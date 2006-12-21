module Vobs where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Trans (liftIO, MonadIO)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList, insert, empty)
import Monad (when)

import qualified System.Time

data Vob = Vob { defaultSize :: (Double, Double), 
                 drawVob :: Double -> Double -> Render () }

defaultWidth  (Vob (w,_) _) = w
defaultHeight (Vob (_,h) _) = h


type View a b  = a -> Double -> Double -> Scene b
type Handler a = Event -> a -> (a, Bool)   -- bool is whether to interpolate

type Time     = Double -- seconds since the epoch
type TimeDiff = Double -- in seconds

type Anim a = Time -> (Scene a, Bool)  -- bool is whether to re-render

getTime :: IO Time
getTime = do (System.Time.TOD secs pics) <- System.Time.getClockTime
             return $ fromInteger secs + fromInteger pics / (10**(3*4))

    
hbox :: [Vob] -> Vob
hbox vobs = Vob size draw where
    size = (sum     $ map defaultWidth vobs, 
            maximum $ map defaultHeight vobs)

    draw _w h = do save
                   sequence $ flip map vobs $ \vob -> do
                       let vobW = defaultWidth vob
                       drawVob vob vobW h
                       translate vobW 0
                   restore
                  
overlay :: [Vob] -> Vob
overlay vobs = Vob size draw where
    size = (maximum $ map defaultWidth vobs, 
            maximum $ map defaultHeight vobs)
              
    draw w h = do sequence $ flip map vobs $ \vob -> drawVob vob w h
                  return ()
                  

pangoContext :: PangoContext
pangoContext = unsafePerformIO $ do
    context <- cairoCreateContext Nothing
    desc    <- contextGetFontDescription context
    fontDescriptionSetFamily desc "Sans"
    fontDescriptionSetSize   desc (fromInteger 10)
    contextSetFontDescription context desc
    return context
    

label :: String -> Vob
label s = unsafePerformIO $ do 
    layout  <- layoutText pangoContext s
    (PangoRectangle _ _ w h, _) <- layoutGetExtents layout
    return $ Vob (realToFrac w, realToFrac h) (\_w _h -> showLayout layout)
    
multiline :: Bool -> Int -> String -> Vob
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
    return $ Vob (realToFrac w, realToFrac h) (\_ _ -> showLayout layout)
                          
                          
rgbaColor :: Double -> Double -> Double -> Double -> Vob -> Vob
rgbaColor r g b a (Vob size draw) = Vob size draw' where
    draw' w h = do save; setSourceRGBA r g b a; draw w h; restore

rgbColor :: Double -> Double -> Double -> Vob -> Vob
rgbColor r g b vob = rgbaColor r g b 1 vob

                  
scaleVob :: Double -> Double -> Vob -> Vob
scaleVob sx sy (Vob (w,h) draw) = Vob (sx*w, sy*h) draw' where
    draw' w' h' = do save; scale sx sy; draw (sx*w') (sy*h'); restore
    
    
rectBox :: Vob -> Vob
rectBox (Vob (w,h) draw) = Vob (w+2,h+2) draw' where
    draw' w' h' = do save
                     rectangle 0 0 w' h'; stroke
                     translate 1 1; draw (w'-2) (h'-2)
                     restore
               
               
pad4 :: Double -> Double -> Double -> Double -> Vob -> Vob
pad4 left up right down (Vob (w,h) draw) = Vob size' draw' where
    size'     = (left+w+right, up+h+down)
    draw' w' h' = do save
                     translate left up
                     draw (w'-left-right) (h'-up-down)
                     restore
    
pad2 :: Double -> Double -> Vob -> Vob
pad2 x y   = pad4 x y x y

pad :: Double -> Vob -> Vob
pad pixels = pad2 pixels pixels


resizeX :: Double -> Vob -> Vob
resizeX w (Vob (_,h) draw) = Vob (w,h) draw

resizeY :: Double -> Vob -> Vob
resizeY h (Vob (w,_) draw) = Vob (w,h) draw

resize :: Double -> Double -> Vob -> Vob

resize w h (Vob _size draw) = Vob (w,h) draw


clipVob :: Vob -> Vob
clipVob (Vob size draw) = Vob size draw' where
    draw' w h = do save; rectangle 0 0 w h; clip; draw w h; restore
    
    
    
type Scene a  = Map a (Double, Double, Double, Double, Vob)

drawScene :: Ord a => Scene a -> Render ()
drawScene scene = do
    flip mapM (toList scene) $ \(_, (x, y, w, h, vob)) -> do 
        save; translate (x-w/2) (y-h/2); drawVob vob w h; restore
    return ()
        
        

interpolate :: Ord a => Double -> Scene a -> Scene a -> Scene a
interpolate fract sc1 sc2 = let
      interpKeys = intersect (keys sc1) (keys sc2)
      interp a b = (a*(1-fract)) + (b*fract)   -- interpolate two Doubles
      vob (x1,y1,w1,h1,_vob1) (x2,y2,w2,h2,vob2) = 
          (interp x1 x2, interp y1 y2, interp w1 w2, interp h1 h2, vob2)
    in fromList [(key, vob (sc1 ! key) (sc2 ! key)) | key <- interpKeys]
             


instance Show Modifier where
    show Shift = "Shift"
    show Control = "Control"
    show Alt = "Alt"
    show Apple = "Apple"
    show Compose = "Compose"

timeDbg :: MonadIO m => String -> m ()
timeDbg msg | False     = liftIO $ do time <- System.Time.getClockTime
                                      putStrLn $ msg ++ "\t" ++ show time
            | otherwise = return ()
       

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
    

vobMain :: Ord b => String -> a -> View a b -> Handler a -> IO ()
vobMain title startState view handleEvent = do
    initGUI
    window <- windowNew
    windowSetTitle window title
    windowSetDefaultSize window 700 400
    
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]

    stateRef <- newIORef startState
    animRef  <- newIORef (noAnim $ view startState 700 400)

    onKeyPress window $ \event -> do
        let Key {eventModifier=mods,eventKeyName=key,eventKeyChar=char} = event
        putStrLn $ show mods++" "++key++" ("++show char++")"

        when (Alt `elem` mods && key == "q") mainQuit

        state <- readIORef stateRef
	let (state', interpolate') = handleEvent event state
	writeIORef stateRef state'
	
        (cw, ch) <- drawingAreaGetSize canvas
        let w = fromIntegral cw; h = fromIntegral ch

	time <- getTime
	anim <- readIORef animRef
	let (scene, _) = anim time; scene' = view state' w h
	    anim' = if interpolate' then interpAnim time 0.3 scene scene'
	                            else noAnim scene'
	writeIORef animRef anim'
	
	widgetQueueDraw canvas

	return False
    
    onExpose canvas $ \(Expose {}) -> do
        drawable <- drawingAreaGetDrawWindow canvas
        anim <- readIORef animRef
        time <- getTime

        let (scene, rerender) = anim time
        
        renderWithDrawable drawable $ do
            save
            
	    timeDbg "Starting redraw at"
            drawScene scene
            
            restore
            
	timeDbg "Finished redraw at"
	
	if rerender then widgetQueueDraw canvas else return ()

        return True

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
