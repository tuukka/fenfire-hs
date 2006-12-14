
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo

import Data.List (intersect)
import Data.Map (Map, keys, (!), fromList, toList)

data Vob = Vob { vobSize :: Render (Double, Double), drawVob :: Render () }

vobWidth v  = do (w,h) <- vobSize v; return w
vobHeight v = do (w,h) <- vobSize v; return h

    
hbox :: [Vob] -> Vob
hbox vobs = Vob size draw where
    size = do sizes <- mapM vobSize vobs
              let (widths, heights) = unzip sizes
              return (sum widths, maximum heights)

    draw = do save
              sequence $ flip map vobs $ \vob -> do
                  drawVob vob
                  w <- vobWidth vob; translate w 0
              restore
    
label :: String -> Vob
label s = Vob (do ext <- textExtents s
                  return (textExtentsWidth ext, textExtentsHeight ext))
              (do ext <- textExtents s
                  save; moveTo 0 (textExtentsHeight ext); showText s; restore)
                  
                  
scaleVob :: Double -> Double -> Vob -> Vob
scaleVob sx sy (Vob size draw) = Vob size' draw' where
    size' = do (w,h) <- size; return (sx*w, sy*h)
    draw' = do save; scale sx sy; draw; restore
    
    
rectBox :: Vob -> Vob
rectBox (Vob size draw) = Vob size' draw' where
    size' = do (w,h) <- size; return (w+2, h+2)
    draw' = do (w,h) <- size; save
               rectangle 0 0 (w+1) (h+1); stroke
               translate 1 1; draw
               restore
               
               
pad4 :: Double -> Double -> Double -> Double -> Vob -> Vob
pad4 left up right down (Vob size draw) = Vob size' draw' where
    size' = do (w,h) <- size; return (left+w+right, up+h+down)
    draw' = do save; translate left up; draw; restore
    
pad2 x y   = pad4 x y x y
pad pixels = pad2 pixels pixels


clipVob :: Vob -> Vob
clipVob (Vob size draw) = Vob size draw' where
    draw' = do save; (w,h) <- size; rectangle 0 0 w h; clip; draw; restore
    
    
    
type Scene a = Map a (Double, Double, Vob)

sceneVob :: Ord a => Double -> Double -> Scene a -> Vob
sceneVob w h scene = Vob (return (w,h)) $ do
    flip mapM (toList scene) $ \(key, (x, y, vob)) -> do 
        save; translate x y; drawVob vob; restore
    return ()
        
        

interpolate :: Ord a => Double -> Scene a -> Scene a -> Scene a
interpolate fract src dst = let
      ks = intersect (keys src) (keys dst)
      interp a b = (a*(1-fract)) + (b*fract)
      vob (sx,sy,sv) (dx,dy,dv) = (interp sx dx, interp sy dy, dv)
    in fromList [(k, vob (src ! k) (dst ! k)) | k <- ks]
    
    
             



myVob = rectBox $ pad 5 $
    scaleVob 1 1 (hbox [label "Hello W", label "orld!"])
    
myScene1 = fromList [("Foo", (50, 50, myVob))]
myScene2 = fromList [("Foo", (150, 150, myVob))]


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
            
            setSourceRGB 0.5 0.5 0.5
            drawVob (sceneVob w h myScene1)
            drawVob (sceneVob w h myScene2)
            
            setSourceRGB 0 0 0
            drawVob (sceneVob w h (interpolate 0.5 myScene1 myScene2))
            
            restore
            
        return True
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
