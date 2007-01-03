module Utils where

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

import Control.Monad
import Control.Monad.Trans


type Endo a = a -> a   -- just what it says, a function from a type to itself


maybeReturn :: MonadPlus m => Maybe a -> m a
maybeReturn = maybe mzero return

returnEach :: MonadPlus m => [a] -> m a
returnEach = msum . map return

maybeDo :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeDo m f = maybe (return ()) f m


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return x = MaybeT $ return (Just x)
    m >>= f  = MaybeT $ do x <- runMaybeT m
                           maybe (return Nothing) (runMaybeT . f) x
    
instance MonadTrans MaybeT where
    lift m = MaybeT $ do x <- m; return (Just x)

instance Monad m => MonadPlus (MaybeT m) where
    mzero = MaybeT $ return Nothing
    mplus m n = MaybeT $ do
        x <- runMaybeT m; maybe (runMaybeT n) (return . Just) x

callMaybeT :: Monad m => MaybeT m a -> MaybeT m (Maybe a)
callMaybeT = lift . runMaybeT
