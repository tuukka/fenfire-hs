module Fenfire.GtkFixes where

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

import Foreign (Ptr, FunPtr, Storable(pokeByteOff, peekByteOff), allocaBytes,
                nullPtr, castPtr, freeHaskellFunPtr)
import Foreign.C (CString, castCharToCChar, withCString, peekCString, CFile,
                  CSize, CInt, CUChar, CChar)

import Foreign.ForeignPtr (unsafeForeignPtrToPtr)

import System.Posix.IO (stdOutput)
import System.Posix.Types (Fd)
import System.Environment (getArgs)

import Control.Monad (when, liftM)
import Data.IORef (modifyIORef, readIORef, newIORef)
import Control.Exception (bracket)

import System.Glib.GObject
import System.Glib.FFI
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk
import Graphics.UI.Gtk.Types


-- while Gtk2Hs actionNew needs the label:
actionNew name maybeLabel tooltip stock = do
    item <- maybe (return Nothing) stockLookupItem stock
    let label' = case (maybeLabel, fmap siLabel item) of
            (Just label, _) -> label
            (_, Just label) -> label
            _               -> error "actionNew: no label"
    Graphics.UI.Gtk.actionNew name label' tooltip stock

-- until Gtk2Hs gets another way to create accel groups:
accelGroupNew :: IO AccelGroup
accelGroupNew = uiManagerNew >>= uiManagerGetAccelGroup


-- from Widget.hs generated from Benja's style patch to gtk2hs:
widgetGetStyle :: WidgetClass widget => widget -> IO Style
widgetGetStyle widget = do
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->
    gtk_widget_ensure_style argPtr1) (toWidget widget)
  makeNewGObject mkStyle $ (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->
    gtk_widget_get_style argPtr1) (toWidget widget)

foreign import ccall safe " gtk_widget_ensure_style"
  gtk_widget_ensure_style :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe " gtk_widget_get_style"
  gtk_widget_get_style :: ((Ptr Widget) -> (IO (Ptr Style)))

-- from Structs.hs generated from Benja's style patch to gtk2hs:
styleGetForeground :: Style -> StateType -> IO Color
styleGetForeground st ty = withForeignPtr (unStyle st) $ \stPtr -> do
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 12) stPtr) (fromEnum ty)

styleGetBackground :: Style -> StateType -> IO Color
styleGetBackground st ty = withForeignPtr (unStyle st) $ \stPtr ->
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 72) stPtr) (fromEnum ty)

styleGetLight :: Style -> StateType -> IO Color
styleGetLight st ty = withForeignPtr (unStyle st) $ \stPtr ->
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 132) stPtr) (fromEnum ty)

styleGetMiddle :: Style -> StateType -> IO Color
styleGetMiddle st ty = withForeignPtr (unStyle st) $ \stPtr ->
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 252) stPtr) (fromEnum ty)

styleGetDark :: Style -> StateType -> IO Color
styleGetDark st ty = withForeignPtr (unStyle st) $ \stPtr ->
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 192) stPtr) (fromEnum ty)

styleGetText :: Style -> StateType -> IO Color
styleGetText st ty = withForeignPtr (unStyle st) $ \stPtr ->
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 312) stPtr) (fromEnum ty)

styleGetBase :: Style -> StateType -> IO Color
styleGetBase st ty = withForeignPtr (unStyle st) $ \stPtr ->
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 372) stPtr) (fromEnum ty)

styleGetAntiAliasing :: Style -> StateType -> IO Color
styleGetAntiAliasing st ty = withForeignPtr (unStyle st) $ \stPtr ->
  peek $ advancePtr ((\hsc_ptr -> hsc_ptr `plusPtr` 432) stPtr) (fromEnum ty)
