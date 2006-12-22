module VobTest where

import Vobs
import Data.Map (fromList)

myVob = rectBox $ pad 5 $ label "Hello World!"

myScene1 = let (vw, vh) = defaultSize myVob 
           in fromList [("Foo", (50, 50, vw, vh, myVob))]
myScene2 = let (vw, vh) = defaultSize myVob 
           in fromList [("Foo", (150, 150, vw+30, vh, myVob))]


main = do vobMain "Example" False 
                  (\state _w _h -> if state then myScene1 else myScene2)
                  (\_event state -> (not state,True))
    
