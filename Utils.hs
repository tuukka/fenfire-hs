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

import Control.Applicative
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
type Op a      = a -> a -> a

type Time     = Double -- seconds since the epoch
type TimeDiff = Double -- in seconds


avg :: Fractional a => Op a
avg x y = (x+y)/2


maybeReturn :: MonadPlus m => Maybe a -> m a
maybeReturn = maybe mzero return

returnEach :: MonadPlus m => [a] -> m a
returnEach = msum . map return

maybeDo :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeDo m f = maybe (return ()) f m


getTime :: IO Time
getTime = do (System.Time.TOD secs picosecs) <- System.Time.getClockTime
             return $ fromInteger secs + fromInteger picosecs / (10**(3*4))
             
             
(&) :: Monoid m => m -> m -> m
(&) = mappend


funzip :: Functor f => f (a,b) -> (f a, f b)
funzip x = (fmap fst x, fmap snd x)

ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

forA2 :: Applicative f => f a -> f b -> (a -> b -> c) -> f c
forA2 x y f = liftA2 f x y

forA3 :: Applicative f => f a -> f b -> f c -> (a -> b -> c -> d) -> f d
forA3 a b c f = liftA3 f a b c


newtype Comp f g a = Comp { fromComp :: f (g a) }

instance (Functor f, Functor g) => Functor (Comp f g) where
    fmap f (Comp m) = Comp (fmap (fmap f) m)
    
instance (Applicative f, Applicative g) => Applicative (Comp f g) where
    pure = Comp . pure . pure
    Comp f <*> Comp x = Comp $ forA2 f x (<*>)


newtype BreadthT m a = BreadthT { runBreadthT :: WriterT [BreadthT m ()] m a }
    
scheduleBreadthT :: Monad m => BreadthT m a -> BreadthT m ()
scheduleBreadthT m = BreadthT $ tell [m >> return ()]

execBreadthT :: Monad m => BreadthT m a -> m ()
execBreadthT m = do rest <- execWriterT (runBreadthT m)
                    when (not $ null rest) $ execBreadthT (sequence_ rest)

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
