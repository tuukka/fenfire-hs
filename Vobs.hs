module Vobs where

import qualified System.Time

import Signals

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList, insert, empty)

data Vob = Vob { defaultSize :: Render (Double, Double), 
                 drawVob :: Double -> Double -> Render () }

defaultWidth v  = do (w,h) <- defaultSize v; return w
defaultHeight v = do (w,h) <- defaultSize v; return h

    
hbox :: [Vob] -> Vob
hbox vobs = Vob size draw where
    size = do sizes <- mapM defaultSize vobs
              let (widths, heights) = unzip sizes
              return (sum widths, maximum heights)

    draw w h = do save
                  sequence $ flip map vobs $ \vob -> do
                      vobW <- defaultWidth vob
                      drawVob vob vobW h
                      translate vobW 0
                  restore
                  
overlay :: [Vob] -> Vob
overlay vobs = Vob size draw where
    size = do sizes <- mapM defaultSize vobs
              let (widths, heights) = unzip sizes
              return (maximum widths, maximum heights)
              
    draw w h = do sequence $ flip map vobs $ \vob -> drawVob vob w h
                  return ()
    
label :: String -> Vob
label s = Vob (do ext <- textExtents s
                  return (textExtentsWidth ext, textExtentsHeight ext))
              (\w h -> do ext <- textExtents s
                          save; moveTo 0 (textExtentsHeight ext)
                          showText s; restore)
                          
                          
rgbColor :: Double -> Double -> Double -> Vob -> Vob
rgbColor r g b (Vob size draw) = Vob size draw' where
    draw' w h = do save; setSourceRGB r g b; draw w h; restore

                  
scaleVob :: Double -> Double -> Vob -> Vob
scaleVob sx sy (Vob size draw) = Vob size' draw' where
    size'     = do (w,h) <- size; return (sx*w, sy*h)
    draw' w h = do save; scale sx sy; draw (sx*w) (sy*h); restore
    
    
rectBox :: Vob -> Vob
rectBox (Vob size draw) = Vob size' draw' where
    size'     = do (w,h) <- size; return (w+2, h+2)
    draw' w h = do save
                   rectangle 0 0 w h; stroke
                   translate 1 1; draw (w-2) (h-2)
                   restore
               
               
pad4 :: Double -> Double -> Double -> Double -> Vob -> Vob
pad4 left up right down (Vob size draw) = Vob size' draw' where
    size'     = do (w,h) <- size; return (left+w+right, up+h+down)
    draw' w h = do save
                   translate left up
                   draw (w-left-right) (h-up-down)
                   restore
    
pad2 x y   = pad4 x y x y
pad pixels = pad2 pixels pixels


resizeX :: Double -> Vob -> Vob
resizeX w (Vob size draw) = Vob (do (_,h) <- size; return (w,h)) draw

resizeY :: Double -> Vob -> Vob
resizeY h (Vob size draw) = Vob (do (w,_) <- size; return (w,h)) draw

resize :: Double -> Double -> Vob -> Vob
resize w h (Vob size draw) = Vob (return (w,h)) draw


clipVob :: Vob -> Vob
clipVob (Vob size draw) = Vob size draw' where
    draw' w h = do save; rectangle 0 0 w h; clip; draw w h; restore
    
    
    
type Scene a  = Map a (Double, Double, Double, Double, Vob)
type RScene a = Render (Scene a)

sceneVob :: Ord a => RScene a -> Vob
sceneVob scene = Vob (return (0,0)) $ \_ _ -> do
    scene' <- scene
    flip mapM (toList scene') $ \(key, (x, y, w, h, vob)) -> do 
        save; translate (x-w/2) (y-h/2); drawVob vob w h; restore
    return ()
        
        

interpolate :: Ord a => Double -> Scene a -> Scene a -> Scene a
interpolate fract sc1 sc2 = let
      interpKeys = intersect (keys sc1) (keys sc2)
      interp a b = (a*(1-fract)) + (b*fract)   -- interpolate two Doubles
      vob (x1,y1,w1,h1,vob1) (x2,y2,w2,h2,vob2) = 
          (interp x1 x2, interp y1 y2, interp w1 w2, interp h1 h2, vob2)
    in fromList [(key, vob (sc1 ! key) (sc2 ! key)) | key <- interpKeys]
             


interpSignal :: Ord a => Signal Time -> RScene a -> RScene a -> Signal Vob
interpSignal clock sc1 sc2 = flip fmap clock $ \t -> let
        alpha = sin (t/5*pi)/2+0.5
        isc = do sc1' <- sc1; sc2' <- sc2
                 return (interpolate alpha sc1' sc2')
        [v1, v2, iv] = map sceneVob [sc1, sc2, isc]
        [v1', v2'] = map (rgbColor 0.5 0.5 0.5) [v1, v2]
        iv' = rgbColor 0 0 0 iv
    in overlay [v1, v2, iv]



instance Show Modifier where
    show Shift = "Shift"
    show Control = "Control"
    show Alt = "Alt"
    show Apple = "Apple"
    show Compose = "Compose"
    show _ = "Unknown modifier"

myVob = rectBox $ pad 5 $ label "Hello World!"

myScene1 = do (vw, vh) <- defaultSize myVob
              return $ fromList [("Foo", (50, 50, vw, vh, myVob))]
myScene2 = do (vw, vh) <- defaultSize myVob
              return $ fromList [("Foo", (150, 150, vw+30, vh, myVob))]

myMain = do
    now <- getTimeIO
    vobMain "Example" $ interpSignal (time now 0.01) myScene1 myScene2
    
timeDbg msg time | False     = liftIO $ putStrLn $ msg ++ "\t" ++ show time
		 | otherwise = return ()

vobMain :: String -> Signal Vob -> IO ()
vobMain title vobSignal = do
    initGUI
    window <- windowNew
    windowSetTitle window title
    
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]

    onKeyPress window $ \(Key { eventModifier=mods, eventKeyName=key, eventKeyChar=char }) -> do
        liftIO $ putStrLn $ show mods++key++" ("++show char++")"
	
	return False
    
    onExpose canvas $ \(Expose { eventArea=rect }) -> do
        (cw, ch) <- drawingAreaGetSize canvas
        let w = fromIntegral cw; h = fromIntegral ch
        drawable <- drawingAreaGetDrawWindow canvas
        
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
