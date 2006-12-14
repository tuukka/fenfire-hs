
import qualified System.Time

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
    
label :: String -> Vob
label s = Vob (do ext <- textExtents s
                  return (textExtentsWidth ext, textExtentsHeight ext))
              (\w h -> do ext <- textExtents s
                          save; moveTo 0 (textExtentsHeight ext)
                          showText s; restore)
                  
                  
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
    
    
    
type Scene a = Map a (Double, Double, Double, Double, Vob)

sceneVob :: Ord a => Scene a -> Vob
sceneVob scene = Vob (return (0,0)) $ \_ _ -> do
    flip mapM (toList scene) $ \(key, (x, y, w, h, vob)) -> do 
        save; translate (x-w/2) (y-h/2); drawVob vob w h; restore
    return ()
        
        

interpolate :: Ord a => Double -> Scene a -> Scene a -> Scene a
interpolate fract src dst = let
      ks = intersect (keys src) (keys dst)
      interp a b = (a*(1-fract)) + (b*fract)   -- interpolate two Doubles
      vob (sx,sy,sw,sh,sv) (dx,dy,dw,dh,dv) = 
          (interp sx dx, interp sy dy, interp sw dw, interp sh dh,dv)
    in fromList [(k, vob (src ! k) (dst ! k)) | k <- ks]
             



myVob = rectBox $ pad 5 $
    scaleVob 1 1 (hbox [label "Hello W", label "orld!"])


main = do
    initGUI
    window <- windowNew
    windowSetTitle window "Example"
    
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]
    
    onExpose canvas $ \(Expose { eventArea=rect }) -> do
        (cw, ch) <- drawingAreaGetSize canvas
        let w = fromIntegral cw; h = fromIntegral ch
        drawable <- drawingAreaGetDrawWindow canvas
        
        renderWithDrawable drawable $ do
            save
            
            (vw, vh) <- defaultSize myVob
            
            let myScene1 = fromList [("Foo", (50, 50, vw, vh, myVob))]
                myScene2 = fromList [("Foo", (150, 150, vw+30, vh, myVob))]
            
            System.Time.TOD seconds picoseconds <- liftIO System.Time.getClockTime
            let time = fromInteger seconds+fromInteger picoseconds/(10**(4*3))
                alpha = sin (time/5*pi)/2+0.5

            setSourceRGB 0.5 0.5 0.5
            drawVob (sceneVob myScene1) w h
            drawVob (sceneVob myScene2) w h
            
            setSourceRGB 0 0 0
            drawVob (sceneVob (interpolate alpha myScene1 myScene2)) w h
            
            restore
            
        return True

    flip timeoutAdd (1000 `div` 100) (widgetQueueDraw canvas >> return True)
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
