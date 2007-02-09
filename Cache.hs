module Cache where

import Data.Bits
import Data.HashTable (HashTable)
import qualified Data.HashTable as HashTable
import Data.Int
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.Unique

import Control.Monad (when)

import System.IO.Unsafe
import System.Mem.StableName


class Hashable a where
    hash :: a -> Int32
    
instance Hashable String where
    hash s = HashTable.hashString s
    
instance Hashable Int where
    hash i = HashTable.hashInt i
    
instance Hashable Unique where
    hash u = hash (hashUnique u)
    
instance Hashable (StableName a) where
    hash n = hash (hashStableName n)
    
instance (Hashable a, Hashable b) => Hashable (a,b) where
    hash (x,y) = hash x `xor` HashTable.hashInt (fromIntegral $ hash y)
    

type LRU a = (Int, Map Integer a)

newLRU :: Int -> LRU a
newLRU size = (size, Map.empty)

access :: a -> Integer -> LRU a -> (Integer, LRU a)
access x i (size, m) = (i', (size, m')) where
    i' = if Map.null m then 0 else fst (Map.findMax m) + 1
    m' = Map.insert i' x (Map.delete i m)
    
add :: a -> LRU a -> (Integer, Maybe a, LRU a)
add x (size, m) = (i, dropped, (size, m'')) where
    i = if Map.null m then 0 else fst (Map.findMax m) + 1
    doDrop = Map.size m >= size
    (dropK, dropV) = Map.findMin m
    dropped = if doDrop then Just dropV else Nothing
    m' = if doDrop then Map.delete dropK m else m
    m'' = Map.insert i x m'


byAddress :: a -> StableName a
byAddress = unsafePerformIO . makeStableName


type Cache key value = (HashTable key (value, Integer), IORef (LRU key))

newCache :: (Eq key, Hashable key) => Int -> Cache key value
newCache size = unsafePerformIO $ do ht <- HashTable.new (==) hash
                                     ref <- newIORef (newLRU size)
                                     return (ht, ref)

cached :: (Eq k, Hashable k) => k -> Cache k v -> v -> v
cached key (cache, lruRef) val = unsafePerformIO $ do
    lru <- readIORef lruRef
    mval' <- HashTable.lookup cache key
    if isJust mval' then do
        let (val', i) = fromJust mval'
            (i', lru') = access key i lru
        writeIORef lruRef lru'
        HashTable.insert cache key (val', i')
        return val'
      else do
        let (i, dropped, lru') = add key lru
        when (isJust dropped) $ HashTable.delete cache (fromJust dropped)
        HashTable.insert cache key (val, i)
        writeIORef lruRef lru'
        return val
