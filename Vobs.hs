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

import Control.Monad.Trans (liftIO, MonadIO)

import Graphics.UI.Gtk hiding (Point, Size, Layout, Color, get)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Matrix (Matrix(Matrix))
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList, insert, empty)
import qualified Data.Map as Map

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

type Cx k a = RenderContext k -> Maybe a
type Render a = Cairo.Render a

data RenderContext k = RenderContext { 
    rcMatrix :: Matrix, rcScene :: Scene k, rcFade :: Double,
    rcColor :: Color, rcBgColor :: Color, rcFadeColor :: Color }
    
type View s k  = s -> Vob k
type Handler s = Event -> HandlerAction s

type HandlerAction s = StateT s (StateT (Bool, Bool) IO) ()


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
             
             
anchor :: Ord k => Double -> Double -> k -> Cx k Point
anchor x y key cx = fmap (\(m,(w,h)) -> Matrix.transformPoint m (x*w, y*h))
                         (Map.lookup key $ rcScene cx)
                    
center :: Ord k => k -> Cx k Point
center = anchor 0.5 0.5
             

changeSize :: Ord k => Endo Size -> Endo (Vob k)
changeSize f vob = vob { defaultSize = f $ defaultSize vob }

changeLayout :: Ord k => (Size -> Layout k -> Layout k) -> Endo (Vob k)
changeLayout f vob = vob { layoutVob = \s -> f s (layoutVob vob s) }

changeScene :: Ord k => Endo (Scene k) -> Endo (Layout k)
changeScene f layout = layout { layoutScene = f (layoutScene layout) }

changeRender :: Endo (RenderContext k -> Render ()) -> Endo (Layout k)
changeRender f layout = 
    layout { renderLayout = \cx -> f (renderLayout layout) cx }

changeContext :: Endo (RenderContext k) -> Endo (Layout k)
changeContext f = changeRender $ \ren cx -> ren (f cx)
    

comb :: Size -> (Size -> Vob k) -> Vob k
comb size f = Vob size $ \size' -> layoutVob (f size') size'

-- | Given a matrix transformation, transforms a layout.
-- Suitable transformations, such as Matrix.rotate, multiply from the right.
--
transform :: Ord k => Endo Matrix -> Endo (Layout k)
transform t = changeScene (\sc -> Map.map (\(m,s) -> (t m,s)) sc)
            . changeContext (\cx -> cx { rcMatrix = t' (rcMatrix cx) })
    where t' = (t Matrix.identity *)

renderable :: Size -> (Size -> Render ()) -> Vob k
renderable s ren = Vob s $ \s' -> decoration $ \cx -> do
    Cairo.save; Cairo.transform (rcMatrix cx); ren s'; Cairo.restore
    
decoration :: (RenderContext k -> Render ()) -> Layout k
decoration ren = Layout (Map.empty) $ \cx -> do
    let Color r g b a = interpolate (rcFade cx) (rcFadeColor cx) (rcColor cx)
    Cairo.save; Cairo.setSourceRGBA r g b a; ren cx; Cairo.restore
    
asVob :: Ord k => Layout k -> Vob k
asVob layout = Vob (0,0) (const layout)


keyVob :: Ord k => k -> Endo (Vob k)
keyVob key vob = flip changeLayout vob $ \size ->
    changeScene (Map.insert key (Matrix.identity, size))
  . changeRender (\_ cx -> maybeDo (Map.lookup key $ rcScene cx) $ \(m,s) ->
        renderLayout (layoutVob vob s) $ cx { rcMatrix = m })


nullVob :: Ord k => Vob k
nullVob = renderable (0,0) $ const $ return ()

overlay :: Ord k => [Vob k] -> Vob k
overlay vobs = Vob size layout where
    size = (maximum $ map defaultWidth vobs, 
            maximum $ map defaultHeight vobs)
            
    layout size' = Layout scene ren where
        layouts = map (flip layoutVob size') vobs
        scene = Map.unions $ reverse (map layoutScene layouts)
        ren cx = sequence_ $ map (\l -> renderLayout l cx) layouts
        

drawRect :: Size -> Vob k
drawRect s = renderable s $ \(w,h) -> do 
    Cairo.rectangle 0 0 w h; Cairo.stroke
    
fillRect :: Size -> Vob k
fillRect s = renderable s $ \(w,h) -> do 
    Cairo.rectangle 0 0 w h; Cairo.fill

rectBox :: Ord k => Endo (Vob k)
rectBox vob = overlay [changeLayout (const useBgColor) $ fillRect (0,0), 
                       vob, drawRect (0,0)]
        

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
    return $ renderable (realToFrac w, realToFrac h) (\_ -> showLayout layout)
    
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
    return $ renderable (realToFrac w, realToFrac h) (\_ -> showLayout layout)

line :: Ord k => Cx k Point -> Cx k Point -> Layout k
line p1 p2 = decoration $ \cx ->
    maybeDo (p1 cx) $ \(x1,y1) -> maybeDo (p2 cx) $ \(x2,y2) -> do
        Cairo.moveTo x1 y1; Cairo.lineTo x2 y2; Cairo.stroke
    
between :: Ord k => Cx k Point -> Cx k Point -> Endo (Layout k)
between p1 p2 layout = decoration $ \cx ->
    maybeDo (p1 cx) $ \(x1,y1) -> maybeDo (p2 cx) $ \(x2,y2) -> do
        let (x,y) = ((x1+x2)/2, (y1+y2)/2)
            angle = atan2 (y2-y1) (x2-x1)
        renderLayout (translate x y $ rotate angle $ layout) cx

                          
setColor :: Ord k => Color -> Endo (Layout k)
setColor c = changeContext $ \cx -> cx { rcColor = c }

setBgColor :: Ord k => Color -> Endo (Layout k)
setBgColor c = changeContext $ \cx -> cx { rcBgColor = c }

useBgColor :: Ord k => Endo (Layout k)
useBgColor = changeContext $ \cx -> cx { rcColor = rcBgColor cx }

useFadeColor :: Ord k => Endo (Layout k)
useFadeColor = changeContext $ \cx -> cx { rcColor = rcFadeColor cx }

fade :: Ord k => Double -> Endo (Layout k)
fade a = changeContext $ \cx -> cx { rcFade = rcFade cx * a }


-- | Moves a layout by x and y.
--
translate :: Ord k => Double -> Double -> Endo (Layout k)
translate x y = transform $ Matrix.translate x y

-- | Rotates a layout by angle.
--
rotate :: Ord k => Double -> Endo (Layout k)
rotate angle = transform $ Matrix.rotate angle

-- | Scales a layout by sx and sy.
--
scale :: Ord k => Double -> Double -> Endo (Layout k)
scale sx sy = transform $ Matrix.scale sx sy

scaleVob :: Ord k => Double -> Double -> Endo (Vob k)
scaleVob sx sy = changeLayout (\_ -> scale sx sy)
               . changeSize (\(w,h) -> (sx*w, sy*h))


anchorVob :: Ord k => Double -> Double -> Vob k -> Layout k
anchorVob x y vob = translate (-x*w) (-y*h) $ layoutVob vob (w,h)
    where (w,h) = defaultSize vob
    
centerVob :: Ord k => Vob k -> Layout k
centerVob = anchorVob 0.5 0.5

               
pad4 :: Ord k => Double -> Double -> Double -> Double -> Endo (Vob k)
pad4 x1 x2 y1 y2 = changeLayout (\_size -> translate x1 x2)
                 . changeSize (\(w,h) -> (x1+w+x2, y1+h+y2))
    
pad2 :: Ord k => Double -> Double -> Endo (Vob k)
pad2 x y   = pad4 x x y y

pad :: Ord k => Double -> Endo (Vob k)
pad pixels = pad2 pixels pixels


resizeX :: Ord k => Double -> Endo (Vob k)
resizeX w = changeSize $ \(_,h) -> (w,h)

resizeY :: Ord k => Double -> Endo (Vob k)
resizeY h = changeSize $ \(w,_) -> (w,h)

resize :: Ord k => Double -> Double -> Endo (Vob k)
resize w h = changeSize $ const (w,h)


clipVob :: Ord k => Endo (Vob k)
clipVob = changeLayout $ \(w,h) -> changeRender $ \render cx -> do
    let m = rcMatrix cx
    Cairo.save
    Cairo.save; Cairo.transform m; Cairo.rectangle 0 0 w h; Cairo.restore
    Cairo.clip; render cx
    Cairo.restore
    
    
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
    
    animRef <- newIORef (layoutVob nullVob (0,0), noAnim Map.empty)
    
    let getWH = do (cw, ch) <- drawingAreaGetSize canvas
                   return (fromIntegral cw, fromIntegral ch)
                   
        getLayout = do (w,h) <- getWH; state <- readIORef stateRef
                       let bg = changeLayout (const useFadeColor) $ 
                                    renderable (0,0) $ const Cairo.paint
                           vob = overlay [bg, view state]
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
