module VobTest where

-- Copyright (c) 2006-2007, Benja Fallenstein, Tuukka Hastrup
-- This file is part of Fenfire.
-- 
-- Fenfire is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- Fenfire is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
-- Public License for more details.
-- 
-- You should have received a copy of the GNU General
-- Public License along with Fenfire; if not, write to the Free
-- Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
-- MA  02111-1307  USA

import Vobs
import Data.Map (fromList)
import Data.IORef
import Control.Monad.State
import Graphics.UI.Gtk


myVob1 = keyVob 1 $ rectBox $ clipVob $ pad 5 $ multiline False 20 "Hello World!"
myVob2 = keyVob 2 $ rectBox $ label "Foo bar baz"

myScene1 :: Vob Int
myScene1 = overlay [ asVob $ translate 50 100 $ centerVob myVob2
                   , asVob $ translate 50 50 $ centerVob myVob1 ]

myScene2 :: Vob Int
myScene2 = Vob (0,0) $ const $ translate 150 150 $ rotate (-pi/5) $
    scale 1.5 1.5 $ centerVob $ changeSize (\(w,h) -> (w-30, h)) $ myVob1


main = do 
    initGUI
    window <- windowNew
    windowSetTitle window "Vob test"
    windowSetDefaultSize window 700 400

    stateRef <- newIORef False

    let view state    = if state then myScene1 else myScene2
        handle _event = do modify not; setInterp True

    (canvas, _updateCanvas) <- vobCanvas stateRef view handle 
                                         (const $ return ()) lightGray

    set window [ containerChild := canvas ]
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
