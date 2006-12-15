module Signals where

import System.Time 

data InputEvent = KeyPress String -- XXX

type Time = Double -- seconds since the epoch
type Stream a = [(Time, a)]
data Signal a = Signal a (Stream a) (Time -> InputEvent -> Signal a)

instance Functor Signal where
    fmap f (Signal start chgs upd) = 
        Signal (f start) (flip map chgs $ \(t,v) -> (t, f v))
               (\t e -> fmap f $ upd t e)
    
streamDrop :: Time -> Stream a -> Stream a
streamDrop t ((t',v):xs) | t' < t    = streamDrop t xs
                         | otherwise = (t',v) : xs
streamDrop t []                      = []
    
streamJoin2 :: Time -> Stream a -> Stream a -> Stream a
streamJoin2 t ((t',v):xs) ys | t' < t    = (t',v) : streamJoin2 t xs ys
                             | otherwise = streamDrop t ys
streamJoin2 t []          ys             = streamDrop t ys

streamJoinN :: Stream a -> Stream (Stream a) -> Stream a
streamJoinN start ((t,s):xs) = streamJoin2 t start (streamJoinN s xs)
streamJoinN start []         = start

signalJoin :: Signal (Signal a) -> Signal a
signalJoin (Signal (Signal start str _) signals upd) =
    Signal start (streamJoinN str strs) (\t e -> signalJoin $ upd t e)
        where strs = flip map signals $ \(t, Signal _ str _) -> (t, str)
    
instance Monad Signal where
    return v = Signal v [] (\t e -> return v)
    sig >>= f = signalJoin (fmap f sig)
    

time :: Time -> Double -> Signal Time
time start step = Signal start (s start) (\t _ -> time t step)
     where s t = let t' = t + step in (t', t') : s t'

getTimeIO :: IO Time
getTimeIO = do (TOD secs pics) <- System.Time.getClockTime
               return $ fromInteger secs + fromInteger pics / (10**(3*4))

constSignal :: a -> Signal a
constSignal value = Signal value [] (\t e -> constSignal value)

getSignal :: Time -> Signal a -> a
getSignal time (Signal start evs _) = getSignal' evs start where
    getSignal' ((t,v):evs) def | t > time  = def
                               | otherwise = getSignal' evs v
    getSignal' []          def             = def

getNextChange :: Time -> Signal a -> Maybe Time
getNextChange time (Signal start evs _) = findNext evs where
    findNext ((t,v):evs) | t > time  = Just t
			 | otherwise = findNext evs
    findNext []                      = Nothing
    
updateSignal :: Time -> Signal a -> InputEvent -> Signal a
updateSignal t s@(Signal _ _ upd) ie = upd t ie
