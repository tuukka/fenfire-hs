module Vobs where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Trans (liftIO, MonadIO)

import Graphics.UI.Gtk hiding (Size, Layout, Color)
import Graphics.Rendering.Cairo (Render, save, restore)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Matrix
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList, insert, empty)
import qualified Data.Map as Map
import Monad (when)

import qualified System.Time

type Size = (Double, Double)
type Scene k = Map k (Matrix, Size)

data Color = Color Double Double Double Double

data RenderContext k = RenderContext { 
    rcMatrix :: Matrix, rcScene :: Scene k, rcFade :: Double,
    rcColor :: Color, rcBgColor :: Color, rcFadeColor :: Color }

data Vob k    = Vob    { defaultSize :: Size, layoutVob :: Size -> Layout k }
data Layout k = Layout { layoutScene :: Scene k,
                         renderLayout :: RenderContext k -> Render () }

defaultWidth  (Vob (w,_) _) = w
defaultHeight (Vob (_,h) _) = h

[black, gray, lightGray, white] = [Color x x x 1 | x <- [0, 0.5, 0.8, 1]]


type View s k  = s -> Vob k
type Handler s = Event -> s -> Maybe (IO (s, Bool))
    -- bool is whether to interpolate

type Time     = Double -- seconds since the epoch
type TimeDiff = Double -- in seconds

type Anim a = Time -> (Scene a, Bool)  -- bool is whether to re-render

getTime :: IO Time
getTime = do (System.Time.TOD secs pics) <- System.Time.getClockTime
             return $ fromInteger secs + fromInteger pics / (10**(3*4))
             
             
changeSize :: (Size -> Size) -> Vob k -> Vob k
changeSize f vob = Vob (f $ defaultSize vob) (layoutVob vob)

addSize :: Double -> Double -> Vob k -> Vob k
addSize x y = changeSize $ \(w,h) -> (w+x, h+y)

comb :: Size -> (Size -> Vob k) -> Vob k
comb ds f = Vob { defaultSize = ds, layoutVob = \s -> layoutVob (f s) s }

transformVob :: Matrix -> Vob k -> Vob k
transformVob t vob = Vob (defaultSize vob) (\s -> transformLayout $ layoutVob vob s)
    where transformLayout l =
              Layout (transformScene $ layoutScene l)
                     (\cx -> renderLayout l $ cx { rcMatrix=t * rcMatrix cx })
          transformScene scene = Map.map (\(m,s) -> (m*t, s)) scene

setRenderColor :: RenderContext k -> Render ()
setRenderColor cx = Cairo.setSourceRGBA r g b a where
    Color r g b a = interpolate (rcFade cx) (rcFadeColor cx) (rcColor cx)

render :: Size -> (Size -> Render ()) -> Vob k
render s ren = Vob s $ \s' -> Layout Map.empty $ \cx -> do
    save; setRenderColor cx; Cairo.transform (rcMatrix cx); ren s'; restore

decoration :: Ord k => (RenderContext k -> Render ()) -> Vob k
decoration r = Vob (0,0) $ \_ -> Layout Map.empty $ \cx -> do 
    save; setRenderColor cx; r cx; restore
    
changeContext :: (RenderContext k -> RenderContext k) -> Vob k -> Vob k
changeContext f v = Vob (defaultSize v) $ \s -> let l = layoutVob v s in
    Layout (layoutScene l) (\cx -> renderLayout l (f cx))

wrap :: (RenderContext k -> Size -> Render ()) -> Vob k -> Vob k
wrap r v = Vob (defaultSize v) $ \s -> let l = layoutVob v s in
    Layout (layoutScene l) (\cx -> do save; r cx s; renderLayout l cx; restore)
                                        
                                                                       
keyVob :: Ord k => k -> Vob k -> Vob k 
keyVob key vob = Vob (defaultSize vob) layout where
    layout size = Layout scene' ren where
        scene  = layoutScene $ layoutVob vob size
        scene' = Map.insert key (identity,size) scene
        ren cx = case Map.lookup key (rcScene cx) of
            Just (m',size') -> renderLayout (layoutVob vob size') $
                                   cx { rcMatrix=m' }
            Nothing         -> return ()


nullVob :: Ord k => Vob k
nullVob = render (0,0) $ const $ return ()

overlay :: Ord k => [Vob k] -> Vob k
overlay vobs = Vob size layout where
    size = (maximum $ map defaultWidth vobs, 
            maximum $ map defaultHeight vobs)
            
    layout size' = Layout scene ren where
        layouts = map (flip layoutVob size') vobs
        scene = Map.unions (map layoutScene layouts)
        ren cx = sequence_ $ map (\l -> renderLayout l cx) layouts
        

drawRect :: Size -> Vob k
drawRect s = render s $ \(w,h) -> do 
    save; Cairo.rectangle 0 0 w h; Cairo.stroke; restore
    
fillRect :: Size -> Vob k
fillRect s = render s $ \(w,h) -> do 
    save; Cairo.rectangle 0 0 w h; Cairo.fill; restore

rectBox :: Ord k => Vob k -> Vob k   -- XXX don't force white bg?
rectBox v = overlay [useBgColor $ fillRect (0,0), v, drawRect (0,0)]
        

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
    return $ render (realToFrac w, realToFrac h) (\_ -> showLayout layout)
    
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
    return $ render (realToFrac w, realToFrac h) (\_ -> showLayout layout)
    
connection :: Ord k => k -> k -> Vob k
connection k1 k2 = decoration $ \cx -> let sc = rcScene cx in
    if k1 `Map.member` sc && k2 `Map.member` sc then do
        let (m1,(w1,h1)) = sc ! k1
            (m2,(w2,h2)) = sc ! k2
            (x1,y1) = transformPoint m1 (w1/2, h1/2)
            (x2,y2) = transformPoint m2 (w2/2, h2/2)
        save; Cairo.moveTo x1 y1; Cairo.lineTo x2 y2; Cairo.stroke; restore
    else return ()
                          
setColor :: Color -> Vob k -> Vob k
setColor c = changeContext $ \cx -> cx { rcColor = c }

setBgColor :: Color -> Vob k -> Vob k
setBgColor c = changeContext $ \cx -> cx { rcBgColor = c }

useBgColor :: Vob k -> Vob k
useBgColor = changeContext $ \cx -> cx { rcColor = rcBgColor cx }

useFadeColor :: Vob k -> Vob k
useFadeColor = changeContext $ \cx -> cx { rcColor = rcFadeColor cx }

fadeVob :: Double -> Vob k -> Vob k
fadeVob a = changeContext $ \cx -> cx { rcFade = rcFade cx * a }


ownSize :: Vob k -> Vob k
ownSize (Vob size layout) = Vob size (const $ layout size)


translateVob :: Double -> Double -> Vob k -> Vob k
translateVob x y = transformVob (translate x y identity) . ownSize
                  
scaleVob :: Double -> Double -> Vob k -> Vob k
scaleVob sx sy = transformVob (Matrix sx 0 0 sy 0 0) 
               . changeSize (\(w,h) -> (sx*w, sy*h))


centerVob :: Vob k -> Vob k
centerVob vob = translateVob (-w/2) (-h/2) vob where (w,h) = defaultSize vob
               
               
pad4 :: Double -> Double -> Double -> Double -> Vob k -> Vob k
pad4 x1 x2 y1 y2 = translateVob x1 x2 . addSize (x1+x2) (y1+y2)
    
pad2 :: Double -> Double -> Vob k -> Vob k
pad2 x y   = pad4 x x y y

pad :: Double -> Vob k -> Vob k
pad pixels = pad2 pixels pixels


resizeX :: Double -> Vob k -> Vob k
resizeX w = changeSize $ \(_,h) -> (w,h)

resizeY :: Double -> Vob k -> Vob k
resizeY h = changeSize $ \(w,_) -> (w,h)

resize :: Double -> Double -> Vob k -> Vob k
resize w h = changeSize $ const (w,h)


clipVob :: Vob k -> Vob k
clipVob = wrap $ \cx (w,h) -> let m = rcMatrix cx in do
    save; Cairo.transform m; Cairo.rectangle 0 0 w h; restore; Cairo.clip
    
    
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
                       let bg = useFadeColor $ render (0,0) $ const Cairo.paint
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
	
	case handleEvent event state of
          Just action -> do (state', interpolate') <- action
                            writeIORef stateRef state'
                            stateChanged state'
                            updateAnim interpolate'
	                    return True
          Nothing     -> return False
    
    onExpose canvas $ \(Expose {}) -> do
        drawable <- drawingAreaGetDrawWindow canvas
        
        (layout, anim) <- readIORef animRef;  time <- getTime
        let (scene, rerender) = anim time
        
        renderWithDrawable drawable $ timeDbg "redraw" $
            renderLayout layout $ RenderContext {
                rcMatrix=identity, rcScene=scene, rcFade=1,
                rcColor=black, rcBgColor=white, rcFadeColor=bgColor
            }
	
	if rerender then widgetQueueDraw canvas else return ()

        return True
        
    return (canvas, updateAnim)
