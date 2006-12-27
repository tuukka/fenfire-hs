module Vobs where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Trans (liftIO, MonadIO)

import Graphics.UI.Gtk hiding (Size, Layout)
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

data Vob k    = Vob    { defaultSize :: Size, layoutVob :: Size -> Layout k }
data Layout k = Layout { layoutScene :: Scene k,
                         renderLayout :: Matrix -> Scene k -> Render () }

defaultWidth  (Vob (w,_) _) = w
defaultHeight (Vob (_,h) _) = h


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
                     (\m sc -> renderLayout l (m*t) sc)
          transformScene scene = Map.map (\(m,s) -> (t*m, s)) scene

render :: Size -> (Size -> Render ()) -> Vob k
render s r = Vob s $ \s' -> Layout Map.empty $ \m _ -> do
    save; Cairo.transform m; r s'; restore
    
wrap :: (Matrix -> Size -> Render ()) -> Vob k -> Vob k
wrap r v = Vob (defaultSize v) $ \s -> let l = layoutVob v s in
    Layout (layoutScene l) (\m sc -> do save; r m s;
                                        renderLayout l m sc; restore)
                                        
                                                                       
keyVob :: Ord k => k -> Vob k -> Vob k 
keyVob key vob = Vob (defaultSize vob) layout where
    layout size = Layout scene' ren where
        scene  = layoutScene $ layoutVob vob size
        scene' = Map.insert key (identity,size) scene
        ren m sc = renderLayout (layoutVob vob size') m' sc where
            (m', size') = Map.findWithDefault (m, size) key sc


nullVob :: Ord k => Vob k
nullVob = render (0,0) $ const $ return ()

overlay :: Ord k => [Vob k] -> Vob k
overlay vobs = Vob size layout where
    size = (maximum $ map defaultWidth vobs, 
            maximum $ map defaultHeight vobs)
            
    layout size' = Layout scene ren where
        layouts = map (flip layoutVob size') vobs
        scene = Map.unions (map layoutScene layouts)
        ren m sc = sequence_ $ map (\l -> renderLayout l m sc) layouts
        

drawRect :: Size -> Vob k
drawRect s = render s $ \(w,h) -> do 
    save; Cairo.rectangle 0 0 w h; Cairo.stroke; restore

rectBox :: Ord k => Vob k -> Vob k
rectBox v = overlay [v, drawRect (0,0)]
        

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
                          
rgbaColor :: Double -> Double -> Double -> Double -> Vob k -> Vob k
rgbaColor r g b a = wrap $ \_ _ -> Cairo.setSourceRGBA r g b a

rgbColor :: Double -> Double -> Double -> Vob k -> Vob k
rgbColor r g b = rgbaColor r g b 1


ownSize :: Vob k -> Vob k
ownSize (Vob size layout) = Vob size (const $ layout size)


translateVob :: Double -> Double -> Vob k -> Vob k
translateVob x y = transformVob (translate x y identity) . ownSize
                  
scaleVob :: Double -> Double -> Vob k -> Vob k
scaleVob sx sy = transformVob (scale sx sy identity) 
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
clipVob = wrap $ \m (w,h) -> do 
    save; Cairo.transform m; Cairo.rectangle 0 0 w h; restore; Cairo.clip
    
    
    
interpolate :: Ord k => Double -> Scene k -> Scene k -> Scene k
interpolate fract sc1 sc2 = let
      interpKeys = intersect (keys sc1) (keys sc2)
      interp a b = (a*(1-fract)) + (b*fract)   -- interpolate two Doubles
      addMatrix (Matrix u v w x y z) (Matrix u' v' w' x' y' z') =
          Matrix (u+u') (v+v') (w+w') (x+x') (y+y') (z+z')
      interpMatrix a b = scalarMultiply (1-fract) a `addMatrix`
                         scalarMultiply fract b
      f (m1,(w1,h1)) (m2,(w2,h2)) = 
          (interpMatrix m1 m2, (interp w1 w2, interp h1 h2))
   in fromList [(key, f (sc1 ! key) (sc2 ! key)) | key <- interpKeys]
             


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
    if continue then (interpolate fract sc1 sc2, True) else (sc2, False)
    where (fract, continue) = bounceFract ((time-startTime) / interpDuration)
    
noAnim scene = const (scene, False)
    

vobCanvas :: Ord b => IORef a -> View a b -> Handler a -> (a -> IO ()) ->
                      IO (DrawingArea, Bool -> IO ())
vobCanvas stateRef view handleEvent stateChanged = do
    canvas <- drawingAreaNew
    
    widgetSetCanFocus canvas True
    
    animRef <- newIORef (layoutVob nullVob (0,0), noAnim Map.empty)
    
    let getWH = do (cw, ch) <- drawingAreaGetSize canvas
                   return (fromIntegral cw, fromIntegral ch)
                   
        updateAnim interpolate' = do
	    state' <- readIORef stateRef
	    
	    (w,h) <- getWH;  time <- getTime;  (_,anim) <- readIORef animRef

	    let (scene, _rerender) = anim time
	        layout' = layoutVob (view state') (w,h)
	        scene' = layoutScene layout'
	        anim' = if interpolate' && isInterpUseful scene scene'
                        then interpAnim time 0.5 scene scene'
	                else noAnim scene'

	    writeIORef animRef (layout', anim')
	
	    widgetQueueDraw canvas

    onRealize canvas $ do (w,h) <- getWH; state <- readIORef stateRef
                          let layout = layoutVob (view state) (w,h)
                              scene = layoutScene layout
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
            renderLayout layout identity scene
	
	if rerender then widgetQueueDraw canvas else return ()

        return True
        
    return (canvas, updateAnim)
