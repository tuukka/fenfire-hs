module Vobs where

import Signals

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Trans (liftIO, MonadIO)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList, insert, empty)
import Monad (when)

data Vob = Vob { defaultSize :: (Double, Double), 
                 drawVob :: Double -> Double -> Render () }

defaultWidth v  = do (w,h) <- defaultSize v; return w
defaultHeight v = do (w,h) <- defaultSize v; return h

    
hbox :: [Vob] -> Vob
hbox vobs = Vob size draw where
    size = (sum     $ map defaultWidth vobs, 
            maximum $ map defaultHeight vobs)

    draw w h = do save
                  sequence $ flip map vobs $ \vob -> do
                      vobW <- defaultWidth vob
                      drawVob vob vobW h
                      translate vobW 0
                  restore
                  
overlay :: [Vob] -> Vob
overlay vobs = Vob size draw where
    size = (maximum $ map defaultWidth vobs, 
            maximum $ map defaultHeight vobs)
              
    draw w h = do sequence $ flip map vobs $ \vob -> drawVob vob w h
                  return ()
    
label :: String -> Vob
label s = Vob (unsafePerformIO $ do 
                  context <- cairoCreateContext Nothing
                  layout  <- layoutText context s
                  (PangoRectangle _ _ w h, _) <- layoutGetExtents layout
                  return (fromIntegral w, fromIntegral h))
              (\w h -> do ext <- textExtents s
                          save; moveTo 0 (textExtentsHeight ext)
                          showText s; restore)
                          
                          
rgbColor :: Double -> Double -> Double -> Vob -> Vob
rgbColor r g b (Vob size draw) = Vob size draw' where
    draw' w h = do save; setSourceRGB r g b; draw w h; restore

                  
scaleVob :: Double -> Double -> Vob -> Vob
scaleVob sx sy (Vob (w,h) draw) = Vob (sx*w, sy*h) draw' where
    draw' w h = do save; scale sx sy; draw (sx*w) (sy*h); restore
    
    
rectBox :: Vob -> Vob
rectBox (Vob (w,h) draw) = Vob (w+2,h+2) draw' where
    draw' w h = do save
                   rectangle 0 0 w h; stroke
                   translate 1 1; draw (w-2) (h-2)
                   restore
               
               
pad4 :: Double -> Double -> Double -> Double -> Vob -> Vob
pad4 left up right down (Vob (w,h) draw) = Vob size' draw' where
    size'     = (left+w+right, up+h+down)
    draw' w h = do save
                   translate left up
                   draw (w-left-right) (h-up-down)
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
resize w h (Vob size draw) = Vob (return (w,h)) draw


clipVob :: Vob -> Vob
clipVob (Vob size draw) = Vob size draw' where
    draw' w h = do save; rectangle 0 0 w h; clip; draw w h; restore
    
    
    
type Scene a  = Map a (Double, Double, Double, Double, Vob)

sceneVob :: Ord a => (Double -> Double -> RScene a) -> Vob
sceneVob scene = Vob (0,0) $ \sw sh -> do
    scene' <- scene sw sh
    flip mapM (toList scene') $ \(_, (x, y, w, h, vob)) -> do 
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

timeDbg :: MonadIO m => String -> Time -> m ()
timeDbg msg time | False     = liftIO $ putStrLn $ msg ++ "\t" ++ show time
		 | otherwise = return ()

vobMain :: String -> Signal Vob -> IO ()
vobMain title vobSignal' = do
    initGUI
    window <- windowNew
    windowSetTitle window title
    windowSetDefaultSize window 700 400
    
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]

    state <- newIORef vobSignal'

    onKeyPress window $ \(Key { eventModifier=mods, eventKeyName=key, eventKeyChar=char }) -> do
        liftIO $ putStrLn $ show mods++key++" ("++show char++")"

        when (key=="q") mainQuit

        vobSignal <- readIORef state
	now <- getTimeIO
	writeIORef state $ updateSignal now vobSignal (KeyPress key)
	widgetQueueDraw canvas

	return False
    
    onExpose canvas $ \(Expose {}) -> do
        (cw, ch) <- drawingAreaGetSize canvas
        let w = fromIntegral cw; h = fromIntegral ch
        drawable <- drawingAreaGetDrawWindow canvas

        vobSignal <- readIORef state
        
        renderWithDrawable drawable $ do
            save
            
            time <- liftIO getTimeIO
	    timeDbg "Starting redraw at" time
            let vob = getSignal time vobSignal
            drawVob vob w h
            
            restore
            
        time' <- liftIO getTimeIO
	timeDbg "Finished redraw at" time'
	case getNextChange time' vobSignal of
	    Just t -> do let sleeptime = ceiling ((t-time')*1000)
			 flip timeoutAdd sleeptime
				  (widgetQueueDraw canvas >> return False)
			 timeDbg "Next change at   " t
			 -- liftIO $ putStrLn $ "Sleeping " ++ show sleeptime ++" ms"
			 return ()
            Nothing -> do liftIO $ putStrLn "End of signal, sleeping."
		          return ()

        return True

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
