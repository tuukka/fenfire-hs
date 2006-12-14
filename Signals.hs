module Signals where

import System.Time 

type InputEvent = () -- XXX

type Time = Double -- seconds since the epoch
data Stream a = Stream [(Time, a)] (Time -> InputEvent -> Stream a)
data Signal a = Signal a (Stream a)   -- start value + change Streams

instance Functor Stream where
    fmap f (Stream evs upd) = Stream (flip map evs $ \(t,v) -> (t, f v))
                                     (\t e -> fmap f $ upd t e)

instance Functor Signal where
    fmap f (Signal start stream) = Signal (f start) (fmap f stream)

time :: Time -> Double -> Signal Time
time start step = Signal start (stream start)
    where stream start = Stream (s start) (\t _ -> stream t)
          s t = let t' = t + step in (t', t') : s t'

getTimeIO :: IO Time
getTimeIO = do (TOD secs pics) <- System.Time.getClockTime
               return $ fromInteger secs + fromInteger pics / (10**(3*4))

getSignal :: Time -> Signal a -> a
getSignal time (Signal start (Stream evs _)) = getSignal' evs start where
    getSignal' ((t,v):evs) def | t > time  = def
                               | otherwise = getSignal' evs v
    getSignal' []          def             = def

getNextChange :: Time -> Signal a -> Maybe Time
getNextChange time (Signal start (Stream evs _)) = findNext evs where
    findNext ((t,v):evs) | t > time  = Just t
			 | otherwise = findNext evs
    findNext []                      = Nothing
    
updateStream :: Time -> Stream a -> InputEvent -> Stream a
updateStream t (Stream _ upd) ie = upd t ie

updateSignal :: Time -> Signal a -> InputEvent -> Signal a
updateSignal t s@(Signal _ e) ie = Signal (getSignal t s) (updateStream t e ie)
