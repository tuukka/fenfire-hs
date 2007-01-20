{-# OPTIONS_GHC -fallow-overlapping-instances #-}
module Vobs where

-- Copyright (c) 2006-2007, Benja Fallenstein, Tuukka Hastrup
-- This file is part of Fenfire.
-- 
-- Fenfire is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- Fenfire is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
-- Public License for more details.
-- 
-- You should have received a copy of the GNU General
-- Public License along with Fenfire; if not, write to the Free
-- Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
-- MA  02111-1307  USA

import Utils

import Cairo

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Time

import Control.Monad.Reader
import Control.Monad.Trans (liftIO, MonadIO)

import Graphics.UI.Gtk hiding (Point, Size, Layout, Color, get, fill)
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Matrix (Matrix(Matrix))
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList, insert, empty)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Monoid(mempty, mappend))

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Reader


type Scene k  = Map k (Maybe (Matrix, Size))
data Vob k    = Vob { defaultSize :: Size,
                      vobScene :: RenderContext k -> Scene k,
                      renderVob :: RenderContext k -> Render () }

type Cx k     = MaybeT (Reader (RenderContext k))

runCx :: RenderContext k -> Cx k a -> Maybe a
runCx cx m = runReader (runMaybeT m) cx

data RenderContext k = RenderContext {
    rcRect :: Rect, rcScene :: Scene k, rcFade :: Double,
    rcFgColor :: Color, rcBgColor :: Color, rcFadeColor :: Color }
    
rcMatrix = fst . rcRect; rcSize = snd . rcRect
    
type View s k  = s -> Vob k
type Handler s = Event -> HandlerAction s

type HandlerAction s = StateT s (StateT (Bool, Bool) IO) ()


instance Ord k => Monoid (Vob k) where
    mempty = Vob (0,0) (const Map.empty) (const $ return ())
    mappend (Vob (w1,h1) sc1 r1) (Vob (w2,h2) sc2 r2) = Vob (w,h) sc r where
        (w,h) = (max w1 w2, max h1 h2)
        sc cx = Map.union (sc1 cx) (sc2 cx)
        r cx = r1 cx >> r2 cx

instance Ord k => MonadCx (Cx k) (Vob k) where
    cxAsk = asks rcRect

    cxWrap f (Vob size sc ren) =
        Vob size sc $ \cx -> maybeDo (runCx cx $ f $ ren cx) id
        
instance Ord k => Transform (Cx k) (Vob k) where
    transform f (Vob size sc ren) = Vob size
            (\cx -> let msc = liftM sc (upd cx)
                        sc0 = sc $ cx { rcRect = (rcMatrix cx, size) }
                    in Map.mapWithKey (\k _ -> msc >>= (! k)) sc0)
            (\cx -> maybe (return ()) ren (upd cx))
        where upd cx = flip liftM (runCx cx (f $ return Matrix.identity))
                           (\m -> cx { rcRect = (m * rcMatrix cx, size) })
                        
                
    

defaultWidth  (Vob (w,_) _ _) = w
defaultHeight (Vob (_,h) _ _) = h


setInterp :: Bool -> HandlerAction s
setInterp interp = lift $ modify $ \(_,handled) -> (interp, handled)

unhandledEvent :: HandlerAction s
unhandledEvent = lift $ modify $ \(interp,_) -> (interp, False)


(@@) :: Ord k => Cx k a -> k -> Cx k a   -- pronounce as 'of'
(@@) x key = do cx <- ask
                rect <- maybeReturn =<< Map.lookup key (rcScene cx)
                local (\_ -> cx { rcRect = rect }) x


changeSize :: Ord k => Endo Size -> Endo (Vob k)
changeSize f vob = vob { defaultSize = f $ defaultSize vob }

changeContext :: Ord k => Endo (RenderContext k) -> Endo (Vob k)
changeContext f (Vob s sc r) = Vob s (sc . f) (r . f)

ownSize :: Ord k => Endo (Vob k)
ownSize vob = 
    changeContext (\cx -> cx { rcRect = (rcMatrix cx, defaultSize vob) }) vob
    

comb :: Size -> (RenderContext k -> Vob k) -> Vob k
comb size f = 
    Vob size (\cx -> vobScene (f cx) cx) (\cx -> renderVob (f cx) cx)

renderable :: Ord k => Size -> Render () -> Vob k
renderable size ren = Vob size (const Map.empty) $ \cx -> do
    do C.save; C.transform (rcMatrix cx); ren; C.restore


keyVob :: Ord k => k -> Endo (Vob k)
keyVob key vob = vob { 
    vobScene = \cx -> Map.insert key (Just $ rcRect cx) (vobScene vob cx),
    renderVob = \cx -> 
        maybeDo (maybeReturn =<< (Map.lookup key $ rcScene cx)) $ \rect ->
            renderVob vob $ cx { rcRect = rect } }
        

rectBox :: Ord k => Endo (Vob k)
rectBox vob = useBgColor (fill extents) & clip extents vob & 
              useFgColor (stroke extents)
        

pangoContext :: PangoContext
pangoContext = unsafePerformIO $ do
    context <- cairoCreateContext Nothing
    desc    <- contextGetFontDescription context
    fontDescriptionSetFamily desc "Sans"
    fontDescriptionSetSize   desc (fromInteger 10)
    contextSetFontDescription context desc
    return context
    

label :: Ord k => String -> Vob k
label s = unsafePerformIO $ do 
    layout  <- layoutText pangoContext s
    (PangoRectangle _ _ w h, _) <- layoutGetExtents layout
    return $ renderable (realToFrac w, realToFrac h) $ showLayout layout
    
multiline :: Ord k => Bool -> Int -> String -> Vob k
multiline useTextWidth widthInChars s = unsafePerformIO $ do 
    layout  <- layoutText pangoContext s
    layoutSetWrap layout WrapPartialWords
    desc    <- contextGetFontDescription pangoContext
    lang    <- languageFromString s
    (FontMetrics {approximateCharWidth=cw, ascent=ascent', descent=descent'})
        <- contextGetMetrics pangoContext desc lang
    let w1 = fromIntegral widthInChars * cw
        h1 = ascent' + descent'
    layoutSetWidth layout (Just w1)
    (PangoRectangle _ _ w2 h2, PangoRectangle _ _ w3 h3) 
        <- layoutGetExtents layout
    let w = if useTextWidth then max w2 w3 else w1
        h = maximum [h1, h2, h3]
    return $ renderable (realToFrac w, realToFrac h) $ showLayout layout

                          
fadedColor :: Ord k => Endo (Cx k Color)
fadedColor c = liftM3 interpolate (asks rcFade) (asks rcFadeColor) c

setFgColor :: Ord k => Color -> Endo (Vob k)
setFgColor c = changeContext $ \cx -> cx { rcFgColor = c }

setBgColor :: Ord k => Color -> Endo (Vob k)
setBgColor c = changeContext $ \cx -> cx { rcBgColor = c }

useFgColor :: Ord k => Endo (Vob k)
useFgColor = withColor (fadedColor $ asks rcFgColor)

useBgColor :: Ord k => Endo (Vob k)
useBgColor = withColor (fadedColor $ asks rcBgColor)

useFadeColor :: Ord k => Endo (Vob k)
useFadeColor = withColor (asks rcFadeColor)

fade :: Ord k => Double -> Endo (Vob k)
fade a = changeContext $ \cx -> cx { rcFade = rcFade cx * a }


centerVob :: Ord k => Endo (Vob k)
centerVob vob = translate (-w/2) (-h/2) vob  where (w,h) = defaultSize vob

               
pad4 :: Ord k => Double -> Double -> Double -> Double -> Endo (Vob k)
pad4 x1 x2 y1 y2 vob = resize (x1+w+x2, y1+h+y2) $ translate x1 y1 vob
    where (w,h) = defaultSize vob
    
pad2 :: Ord k => Double -> Double -> Endo (Vob k)
pad2 x y   = pad4 x x y y

pad :: Ord k => Double -> Endo (Vob k)
pad pixels = pad2 pixels pixels


resizeX :: Ord k => Double -> Endo (Vob k)
resizeX w = changeSize $ \(_,h) -> (w,h)

resizeY :: Ord k => Double -> Endo (Vob k)
resizeY h = changeSize $ \(w,_) -> (w,h)

resize :: Ord k => Size -> Endo (Vob k)
resize (w,h) = changeSize $ const (w,h)
    
    
class Interpolate a where
    interpolate :: Double -> Op a
    
instance Interpolate Double where
    interpolate fract x y = (1-fract)*x + fract*y
    
instance Interpolate Color where
    interpolate fract (Color r g b a) (Color r' g' b' a') =
        Color (i r r') (i g g') (i b b') (i a a') where
            i = interpolate fract

instance Interpolate Matrix where
    interpolate fract (Matrix u v w x y z) (Matrix u' v' w' x' y' z') =
        Matrix (i u u') (i v v') (i w w') (i x x') (i y y') (i z z') where
            i = interpolate fract

interpolateScene :: Ord k => Double -> Op (Scene k)
interpolateScene fract sc1 sc2 =
    fromList [(key, liftM2 f (sc1 ! key) (sc2 ! key)) | key <- interpKeys] where
        interpKeys = intersect (keys sc1) (keys sc2)
        f (m1,(w1,h1)) (m2,(w2,h2)) = (i m1 m2, (i w1 w2, i h1 h2))
        i x y = interpolate fract x y
             

isInterpUseful :: Ord k => Scene k -> Scene k -> Bool             
isInterpUseful sc1 sc2 = 
    not $ all same [(sc1 ! key, sc2 ! key) | key <- interpKeys]
    where same (a,b) = all (\d -> abs d < 5) $ zipWith (-) (values a) (values b)
          values (Just (Matrix a b c d e f, (w,h))) = [a,b,c,d,e,f,w,h]
          values Nothing = error "shouldn't happen"
          interpKeys = intersect (getKeys sc1) (getKeys sc2)
          getKeys sc = [k | k <- keys sc, isJust (sc ! k)]
          
instance Show Modifier where
    show Shift = "Shift"
    show Control = "Control"
    show Alt = "Alt"
    show Apple = "Apple"
    show Compose = "Compose"

timeDbg :: MonadIO m => String -> Endo (m ())
timeDbg s act | False     = do out s; act; out s
              | otherwise = act
    where out t = liftIO $ do time <- System.Time.getClockTime
                              putStrLn $ s ++ " " ++ t ++ "\t" ++ show time
       

linearFract :: Double -> (Double, Bool)
linearFract x = if (x<1) then (x,True) else (1,False)

bounceFract :: Double -> (Double, Bool)
bounceFract x = (y,cont) where     -- ported from AbstractUpdateManager.java
    x' = x + x*x
    y = 1 - cos (2 * pi * n * x') * exp (-x' * r)
    cont = -(x + x*x)*r >= log 0.02
    (n,r) = (0.4, 2)



type Anim a = Time -> (Scene a, Bool)  -- bool is whether to re-render

interpAnim :: Ord a => Time -> TimeDiff -> Scene a -> Scene a -> Anim a
interpAnim startTime interpDuration sc1 sc2 time =
    if continue then (interpolateScene fract sc1 sc2, True) else (sc2, False)
    where (fract, continue) = bounceFract ((time-startTime) / interpDuration)
    
noAnim scene = const (scene, False)
    

vobCanvas :: Ord b => IORef a -> View a b -> Handler a -> (a -> IO ()) ->
                      Color -> IO (DrawingArea, Bool -> IO ())
vobCanvas stateRef view handleEvent stateChanged bgColor = do
    canvas <- drawingAreaNew
    
    widgetSetCanFocus canvas True
    
    animRef <- newIORef (mempty, noAnim Map.empty)
    
    let getWH = do (cw, ch) <- drawingAreaGetSize canvas
                   return (fromIntegral cw, fromIntegral ch)
                   
        getVob = do state <- readIORef stateRef
                    return $ useFadeColor paint & view state
                    
        getRenderContext sc = do 
            size <- getWH; return $ RenderContext {
                rcScene=sc, rcRect=(Matrix.identity, size), rcFade=1,
                rcFgColor=black, rcBgColor=white, rcFadeColor=bgColor }
                   
        updateAnim interpolate' = mdo
	    (_,anim) <- readIORef animRef
            vob' <- getVob

            rc' <- getRenderContext scene'
	    let scene' = vobScene vob' rc'
	        
	    time <- scene' `seq` getTime
	    
	    let (scene, _rerender) = anim time
	        anim' = if interpolate' && isInterpUseful scene scene'
                        then interpAnim time 0.5 scene scene'
	                else noAnim scene'

	    writeIORef animRef (vob', anim')
	
	    widgetQueueDraw canvas

    onRealize canvas $ mdo vob <- getVob; rc <- getRenderContext scene
                           let scene = vobScene vob rc
                           writeIORef animRef (vob, noAnim scene)
    
    onConfigure canvas $ \_event -> do updateAnim False; return True

    onKeyPress canvas $ \event -> do
        let Key {eventModifier=mods,eventKeyName=key,eventKeyChar=char} = event
        putStrLn $ show mods++" "++key++" ("++show char++")"

        when (Alt `elem` mods && key == "q") mainQuit
        
        state <- readIORef stateRef
        
        (((), state'), (interpolate', handled)) <- 
            runStateT (runStateT (handleEvent event) state) (False, True)
        
        when handled $ do writeIORef stateRef state'
                          stateChanged state'
                          updateAnim interpolate'    
        return handled

    onButtonPress canvas $ \(Button {}) -> do
        widgetGrabFocus canvas
        return True
    
    onExpose canvas $ \(Expose {}) -> do
        drawable <- drawingAreaGetDrawWindow canvas
        
        (vob, anim) <- readIORef animRef;  time <- getTime
        let (scene, rerender) = anim time
        rc <- getRenderContext scene
        
        renderWithDrawable drawable $ timeDbg "redraw" $ renderVob vob rc
	
	if rerender then widgetQueueDraw canvas else return ()

        return True
        
    return (canvas, updateAnim)
