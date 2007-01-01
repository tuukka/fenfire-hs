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


myVob = keyVob () $ rectBox $ pad 5 $ label "Hello World!"

myScene1 :: Vob ()
myScene1 = translateVob 50 50 myVob

myScene2 :: Vob ()
myScene2 = translateVob 150 150 $ changeSize (\(w,h) -> (w+30, h)) myVob


main = do 
    initGUI
    window <- windowNew
    windowSetTitle window "Vob test"
    windowSetDefaultSize window 700 400

    stateRef <- newIORef False

    let view state    = if state then myScene1 else myScene2
        handle _event = do modify not; return True

    (canvas, _updateCanvas) <- vobCanvas stateRef view handle 
                                         (const $ return ()) lightGray

    set window [ containerChild := canvas ]
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
