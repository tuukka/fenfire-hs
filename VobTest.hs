module VobTest where

import Vobs
import Data.Map (fromList)
import Data.IORef
import Graphics.UI.Gtk


myVob = rectBox $ pad 5 $ label "Hello World!"

myScene1 = let (vw, vh) = defaultSize myVob 
           in fromList [("Foo", (50, 50, vw, vh, myVob))]
myScene2 = let (vw, vh) = defaultSize myVob 
           in fromList [("Foo", (150, 150, vw+30, vh, myVob))]


main = do 
    initGUI
    window <- windowNew
    windowSetTitle window "Vob test"
    windowSetDefaultSize window 700 400

    stateRef <- newIORef False

    let view state _w _h    = if state then myScene1 else myScene2
        handle _event state = return (not state, True)

    (canvas, _changeState) <- vobCanvas window stateRef view handle

    set window [ containerChild := canvas ]
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
