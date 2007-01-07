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

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Reader
import Control.Monad.Trans (liftIO, MonadIO)

import Graphics.UI.Gtk hiding (Point, Size, Layout, Color, get, fill)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Matrix (Matrix(Matrix))
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList, insert, empty)
import qualified Data.Map as Map
import Data.Monoid hiding (Endo)

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Reader

import qualified System.Time


data Color = Color Double Double Double Double
type Size  = (Double, Double)
type Point = (Double, Double)
type Rect  = (Matrix, Size)

type Time     = Double -- seconds since the epoch
type TimeDiff = Double -- in seconds

type Scene k  = Map k (Matrix, Size)
data Vob k    = Vob    { defaultSize :: Size, layoutVob :: Size -> Layout k }
data Layout k = Layout { layoutScene :: Scene k, 
                         renderLayout :: RenderContext k -> Render () }

type Cx k a   = MaybeT (Reader (Size, RenderContext k)) a
type Render a = Cairo.Render a

data RenderContext k = RenderContext { 
    rcMatrix :: Matrix, rcScene :: Scene k, rcFade :: Double,
    rcColor :: Color, rcBgColor :: Color, rcFadeColor :: Color }
    
newtype Path = Path { renderPath :: Render () }
    
type View s k  = s -> Vob k
type Handler s = Event -> HandlerAction s

type HandlerAction s = StateT s (StateT (Bool, Bool) IO) ()


instance Ord k => Monoid (Vob k) where
    mempty = Vob (0,0) $ \_ -> Layout (Map.empty) $ \_ -> return ()
    mappend (Vob (w1,h1) l1) (Vob (w2,h2) l2) = Vob (w,h) l where
        (w,h) = (max w1 w2, max h1 h2)
        l size = Layout (Map.union sc1 sc2) (\cx -> r1 cx >> r2 cx) where
            (Layout sc1 r1, Layout sc2 r2) = (l1 size, l2 size)

instance Monoid a => Monoid (Cx k a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance Monoid Path where
    mempty      = Path $ return ()
    mappend p q = Path $ renderPath p >> renderPath q


defaultWidth  (Vob (w,_) _) = w
defaultHeight (Vob (_,h) _) = h

[black, gray, lightGray, white] = [Color x x x 1 | x <- [0, 0.5, 0.9, 1]]


setInterp :: Bool -> HandlerAction s
setInterp interp = lift $ modify $ \(_,handled) -> (interp, handled)

unhandledEvent :: HandlerAction s
unhandledEvent = lift $ modify $ \(interp,_) -> (interp, False)


getTime :: IO Time
getTime = do (System.Time.TOD secs picosecs) <- System.Time.getClockTime
             return $ fromInteger secs + fromInteger picosecs / (10**(3*4))
             
             
fromCx :: Cx k a -> Size -> RenderContext k -> Maybe a
fromCx x size cx = runReader (runMaybeT x) (size, cx)

(@@) :: Ord k => Cx k a -> k -> Cx k a   -- pronounce 'of'
(@@) x key = do (_, cx) <- ask; (m,size) <- Map.lookup key (rcScene cx)
                local (\_ -> (size, cx { rcMatrix=m })) x
                      
point :: Ord k => Double -> Double -> Cx k Point
point x y = do cx <- liftM snd ask; return $ transformPt (rcMatrix cx) (x,y)
  where transformPt (Matrix xx xy yx yy x0 y0) (dx,dy) =
          (xx*dx + yx*dy + x0, xy*dx + yy*dy + y0) -- the gtk2hs impl is buggy

anchor :: Ord k => Double -> Double -> Cx k Point
anchor x y = do (w,h) <- liftM fst ask; point (x*w) (y*h)

center :: Ord k => Cx k Point
center = anchor 0.5 0.5

moveTo :: Cx k Point -> Cx k Path
moveTo p = do (x,y) <- p; return $ Path $ do Cairo.moveTo x y

lineTo :: Cx k Point -> Cx k Path
lineTo p = do (x,y) <- p; return $ Path $ do Cairo.lineTo x y

line :: Ord k => Cx k Point -> Cx k Point -> Cx k Path
line p1 p2 = moveTo p1 & lineTo p2

extents :: Ord k => Cx k Path
extents = moveTo (anchor 0 0) & lineTo (anchor 0 1) & lineTo (anchor 1 1)
        & lineTo (anchor 1 0) & lineTo (anchor 0 0)


changeSize :: Ord k => Endo Size -> Endo (Vob k)
changeSize f vob = vob { defaultSize = f $ defaultSize vob }

changeLayout :: Ord k => (Size -> Endo (Layout k)) -> Endo (Vob k)
changeLayout f vob = vob { layoutVob = \s -> f s (layoutVob vob s) }

changeScene :: Ord k => (Size -> Endo (Scene k)) -> Endo (Vob k)
changeScene f = changeLayout $ \s l -> l { layoutScene = f s (layoutScene l) }

changeRender :: Ord k => (Size -> Endo (RenderContext k -> Render ())) 
                      -> Endo (Vob k)
changeRender f = changeLayout $ \s layout -> 
    layout { renderLayout = \cx -> f s (renderLayout layout) cx }
    
changeContext :: Ord k => (Size -> Endo (RenderContext k)) -> Endo (Vob k)
changeContext f = changeRender $ \s ren cx -> ren (f s cx)

ownSize :: Endo (Vob k)
ownSize (Vob size layout) = Vob size (const l') where l' = layout size
    

comb :: Size -> (Size -> Vob k) -> Vob k
comb size f = Vob size $ \size' -> layoutVob (f size') size'

renderVob :: Ord k => Vob k -> RenderContext k -> Render ()
renderVob vob = renderLayout (layoutVob vob (defaultSize vob))

-- | Applies a matrix transformation to a vob.
--
transform :: Ord k => Endo Matrix -> Endo (Vob k)
transform t = ownSize . changeScene (\_ sc -> Map.map (\(m,s) -> (t m,s)) sc)
            . changeContext (\_ cx -> cx { rcMatrix = t' (rcMatrix cx) })
    where t' = (t Matrix.identity *)

renderable :: Ord k => Size -> (Size -> RenderContext k -> Render ()) -> Vob k
renderable size ren = resize size $ decoration $ ask >>= \(s',cx) -> return $
    do Cairo.save; Cairo.transform (rcMatrix cx); ren s' cx; Cairo.restore
    
decoration :: (Cx k (Render ())) -> Vob k
decoration render = Vob (0,0) $ \size -> Layout (Map.empty) $ \cx -> do
    let Color r g b a = interpolate (rcFade cx) (rcFadeColor cx) (rcColor cx)
    maybeDo (fromCx render size cx) $ \render' -> do
        Cairo.save; Cairo.setSourceRGBA r g b a; render'; Cairo.restore


keyVob :: Ord k => k -> Endo (Vob k)
keyVob key vob = flip ($) vob $
    changeScene (\size -> Map.insert key (Matrix.identity, size))
  . changeRender (\_ _ cx -> maybeDo (Map.lookup key $ rcScene cx) $ \(m,s) ->
        renderLayout (layoutVob vob s) $ cx { rcMatrix = m })
        

fill :: Ord k => Cx k Path -> Vob k
fill p = decoration $ do p' <- p; return $ do renderPath p'; Cairo.fill

stroke :: Ord k => Cx k Path -> Vob k
stroke p = decoration $ do p' <- p; return $ do renderPath p'; Cairo.stroke

paint :: Ord k => Vob k
paint = decoration $ return $ Cairo.paint


rectBox :: Ord k => Endo (Vob k)
rectBox vob = useBgColor (fill extents) & clip extents vob & stroke extents
        

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
    return $ renderable (realToFrac w, realToFrac h) 
                        (\_ _ -> showLayout layout)
    
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
    return $ renderable (realToFrac w, realToFrac h) 
                        (\_ _ -> showLayout layout)

between :: Ord k => Cx k Point -> Cx k Point -> Endo (Vob k)
between p1 p2 vob = decoration $ do 
    (x1,y1) <- p1;  (x2,y2) <- p2;  (_,cx) <- ask
    let (x,y) = ((x1+x2)/2, (y1+y2)/2)
        angle = atan2 (y2-y1) (x2-x1)
    return $ do renderVob (translate x y $ rotate angle $ vob) cx

                          
setColor :: Ord k => Color -> Endo (Vob k)
setColor c = changeContext $ \_ cx -> cx { rcColor = c }

setBgColor :: Ord k => Color -> Endo (Vob k)
setBgColor c = changeContext $ \_ cx -> cx { rcBgColor = c }

useBgColor :: Ord k => Endo (Vob k)
useBgColor = changeContext $ \_ cx -> cx { rcColor = rcBgColor cx }

useFadeColor :: Ord k => Endo (Vob k)
useFadeColor = changeContext $ \_ cx -> cx { rcColor = rcFadeColor cx }

fade :: Ord k => Double -> Endo (Vob k)
fade a = changeContext $ \_ cx -> cx { rcFade = rcFade cx * a }


-- | Moves a vob by x and y.
--
translate :: Ord k => Double -> Double -> Endo (Vob k)
translate x y = transform $ Matrix.translate x y

-- | Rotates a vob by angle.
--
rotate :: Ord k => Double -> Endo (Vob k)
rotate angle = transform $ Matrix.rotate angle

-- | Scales a vob by sx and sy.
--
scale :: Ord k => Double -> Double -> Endo (Vob k)
scale sx sy vob = 
    resize (defaultSize vob) $ transform (Matrix.scale sx sy) vob


anchorVob :: Ord k => Double -> Double -> Endo (Vob k)
anchorVob x y vob = translate (-x*w) (-y*h) vob where (w,h) = defaultSize vob
    
centerVob :: Ord k => Endo (Vob k)
centerVob = anchorVob 0.5 0.5

               
pad4 :: Ord k => Double -> Double -> Double -> Double -> Endo (Vob k)
pad4 x1 x2 y1 y2 vob = resize (x1+w+x2, y1+h+y2) $ translate x1 x2 vob
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


clip :: Ord k => Cx k Path -> Endo (Vob k)
clip p = changeRender $ \s render cx -> maybeDo (fromCx p s cx) $ \p' -> do 
    Cairo.save; renderPath p'; Cairo.clip; render cx; Cairo.restore
    
    
class Interpolate a where
    interpolate :: Double -> a -> a -> a
    
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

interpolateScene :: Ord k => Double -> Scene k -> Scene k -> Scene k
interpolateScene fract sc1 sc2 =
    fromList [(key, f (sc1 ! key) (sc2 ! key)) | key <- interpKeys] where
        interpKeys = intersect (keys sc1) (keys sc2)
        f (m1,(w1,h1)) (m2,(w2,h2)) = (i m1 m2, (i w1 w2, i h1 h2))
        i x y = interpolate fract x y
             

isInterpUseful :: Ord k => Scene k -> Scene k -> Bool             
isInterpUseful sc1 sc2 = 
    not $ all same [(sc1 ! key, sc2 ! key) | key <- interpKeys]
    where same (a,b) = all (\d -> abs d < 5) $ zipWith (-) (values a) (values b)
          values (Matrix a b c d e f, (w,h)) = [a,b,c,d,e,f,w,h]
          interpKeys = intersect (keys sc1) (keys sc2)
          
instance Show Modifier where
    show Shift = "Shift"
    show Control = "Control"
    show Alt = "Alt"
    show Apple = "Apple"
    show Compose = "Compose"

timeDbg :: MonadIO m => String -> m () -> m ()
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
    
    animRef <- newIORef (layoutVob mempty (0,0), noAnim Map.empty)
    
    let getWH = do (cw, ch) <- drawingAreaGetSize canvas
                   return (fromIntegral cw, fromIntegral ch)
                   
        getLayout = do (w,h) <- getWH; state <- readIORef stateRef
                       let vob = useFadeColor paint & view state
                       return $ layoutVob vob (w,h)
                   
        updateAnim interpolate' = do
	    (_,anim) <- readIORef animRef
	    layout'  <- getLayout

	    let scene' = layoutScene layout'
	        
	    time <- scene' `seq` getTime
	    
	    let (scene, _rerender) = anim time
	        anim' = if interpolate' && isInterpUseful scene scene'
                        then interpAnim time 0.5 scene scene'
	                else noAnim scene'

	    writeIORef animRef (layout', anim')
	
	    widgetQueueDraw canvas

    onRealize canvas $ do layout <- getLayout
                          let scene = layoutScene layout
                          writeIORef animRef (layout, noAnim scene)
    
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
        
        (layout, anim) <- readIORef animRef;  time <- getTime
        let (scene, rerender) = anim time
        
        renderWithDrawable drawable $ timeDbg "redraw" $
            renderLayout layout $ RenderContext {
                rcMatrix=Matrix.identity, rcScene=scene, rcFade=1,
                rcColor=black, rcBgColor=white, rcFadeColor=bgColor
            }
	
	if rerender then widgetQueueDraw canvas else return ()

        return True
        
    return (canvas, updateAnim)
