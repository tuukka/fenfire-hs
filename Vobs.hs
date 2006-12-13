
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo

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
             



main = vobMain "Example" $ rectBox $ pad 5 $
    scaleVob 1 1 (hbox [label "Hello W", label "orld!"])


vobMain :: String -> Vob -> IO ()
vobMain windowTitle vob = do
    initGUI
    window <- windowNew
    widgetSetAppPaintable window True
    windowSetTitle window windowTitle
    
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]
    
    onExpose canvas $ \(Expose { eventArea=rect }) -> do
        (cw, ch) <- drawingAreaGetSize canvas
        drawable <- drawingAreaGetDrawWindow canvas
        
        renderWithDrawable drawable $ do
            save
            
            (vw, vh) <- vobSize vob
            translate (fromIntegral cw/2 - vw/2) (fromIntegral ch/2 - vh/2)

            setSourceRGB 0 0 0            
            drawVob vob
            
            restore
            
        return True
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
