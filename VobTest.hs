module VobTest where

import Vobs
import Data.Map (fromList)
import Data.IORef
import Graphics.UI.Gtk


myVob = keyVob () $ rectBox $ pad 5 $ label "Hello World!"

myScene1 :: Vob ()
myScene1 = translateVob 50 50 myVob

myScene2 :: Vob ()
myScene2 = translateVob 150 150 $ addSize 30 0 myVob


main = do 
    initGUI
    window <- windowNew
    windowSetTitle window "Vob test"
    windowSetDefaultSize window 700 400

    stateRef <- newIORef False

    let view state          = if state then myScene1 else myScene2
        handle _event state = Just $ return (not state, True)

    (canvas, _updateCanvas) <- vobCanvas stateRef view handle 
                                         (const $ return ()) lightGray

    set window [ containerChild := canvas ]
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
