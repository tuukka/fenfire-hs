module Cache where

import Data.Bits
import Data.HashTable (HashTable)
import qualified Data.HashTable as HashTable
import Data.Int
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Data.Unique

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
    
    
byAddress :: a -> StableName a
byAddress = unsafePerformIO . makeStableName


type Cache key value = HashTable key value

newCache :: (Eq key, Hashable key) => Cache key value
newCache = unsafePerformIO $ HashTable.new (==) hash

cached :: (Eq k, Hashable k) => k -> Cache k v -> v -> v
cached key cache val = unsafePerformIO $ do
    val' <- HashTable.lookup cache key
    if isJust val' then return $ fromJust val' else do
        HashTable.insert cache key val
        return val
