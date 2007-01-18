-- For (instance MonadReader w m => MonadReader w (MaybeT m)) in GHC 6.6:
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
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
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer (WriterT(..), MonadWriter(..), execWriterT)

import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))

import qualified System.Time


-- just what the rhs says, a function from a type to itself
type Endo a = a -> a

type EndoM m a = a -> m a

type Time     = Double -- seconds since the epoch
type TimeDiff = Double -- in seconds


maybeReturn :: MonadPlus m => Maybe a -> m a
maybeReturn = maybe mzero return

returnEach :: MonadPlus m => [a] -> m a
returnEach = msum . map return

maybeDo :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeDo m f = maybe (return ()) f m


-- XXX newer GHC's have these built in
for   x f = map   f x
forM  x f = mapM  f x
forM_ x f = mapM_ f x
ffor  x f = fmap  f x


getTime :: IO Time
getTime = do (System.Time.TOD secs picosecs) <- System.Time.getClockTime
             return $ fromInteger secs + fromInteger picosecs / (10**(3*4))
             
             
(&) :: Monoid m => m -> m -> m
(&) = mappend


-- XXX newer versions of Data.Monoid have this:
newtype Dual m = Dual { getDual :: m } 

instance Monoid m => Monoid (Dual m) where
    mempty = Dual mempty
    mappend (Dual m) (Dual n) = Dual (n & m)


newtype BreadthT m a = BreadthT { runBreadthT :: WriterT [BreadthT m ()] m a }
    
scheduleBreadthT :: Monad m => BreadthT m a -> BreadthT m ()
scheduleBreadthT m = BreadthT $ tell [m >> return ()]

execBreadthT :: Monad m => BreadthT m a -> m ()
execBreadthT m = do rest <- execWriterT (runBreadthT m)
                    when (not $ null rest) $ execBreadthT (sequence_ $ rest)

instance Monad m => Monad (BreadthT m) where
    return  = BreadthT . return
    m >>= f = BreadthT (runBreadthT m >>= runBreadthT . f)
    
instance MonadTrans BreadthT where
    lift = BreadthT . lift
    
instance MonadState s m => MonadState s (BreadthT m) where
    get = lift $ get
    put = lift . put
    
instance MonadWriter w m => MonadWriter w (BreadthT m) where
    tell = lift . tell
    listen m = BreadthT $ WriterT $ do
        ((x,w),w') <- listen $ runWriterT (runBreadthT m)
        return ((x,w'),w)
    pass m = BreadthT $ WriterT $ pass $ do
        ((x,f),w) <- runWriterT (runBreadthT m)
        return ((x,w),f)


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return x = MaybeT $ return (Just x)
    m >>= f  = MaybeT $ do x <- runMaybeT m
                           maybe (return Nothing) (runMaybeT . f) x
    fail _   = mzero
    
instance MonadTrans MaybeT where
    lift m = MaybeT $ do x <- m; return (Just x)

instance Monad m => MonadPlus (MaybeT m) where
    mzero = MaybeT $ return Nothing
    mplus m n = MaybeT $ do
        x <- runMaybeT m; maybe (runMaybeT n) (return . Just) x
        
instance MonadReader r m => MonadReader r (MaybeT m) where
    ask = lift ask
    local f m = MaybeT $ local f (runMaybeT m)
    
instance MonadWriter w m => MonadWriter w (MaybeT m) where
    tell = lift . tell
    listen m = MaybeT $ do (x,w) <- listen $ runMaybeT m
                           return $ maybe Nothing (\x' -> Just (x',w)) x
    pass m = MaybeT $ pass $ do 
        x <- runMaybeT m; return $ maybe (Nothing,id) (\(y,f) -> (Just y,f)) x

callMaybeT :: Monad m => MaybeT m a -> MaybeT m (Maybe a)
callMaybeT = lift . runMaybeT


instance MonadWriter w m => MonadWriter w (ListT m) where
    tell = lift . tell
    listen m = ListT $ do (xs,w) <- listen $ runListT m
                          return [(x,w) | x <- xs]
    pass m = ListT $ pass $ do -- not ideal impl, but makes 'censor' work
        ps <- runListT m
        return $ if null ps then ([], id) else (map fst ps, snd (head ps))
