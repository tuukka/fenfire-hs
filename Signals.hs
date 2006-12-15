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
streamDrop _t []                      = []
    
streamJoin2 :: Time -> Stream a -> Stream a -> Stream a
streamJoin2 t a b = takeWhile (\x -> fst x < t) a 
                 ++ dropWhile (\x -> fst x < t) b

streamJoinN :: Stream a -> Stream (Stream a) -> Stream a
streamJoinN start ((t,s):xs) = streamJoin2 t start (streamJoinN s xs)
streamJoinN start []         = start

signalJoin :: Signal (Signal a) -> Signal a
signalJoin (Signal (Signal start str _) signals upd) =
    Signal start (streamJoinN str strs) (\t e -> signalJoin $ upd t e)
        where strs = flip map signals $ \(t, Signal _ str' _) -> (t, str')
    
instance Monad Signal where
    return v = Signal v [] (\_t _e -> return v)
    sig >>= f = signalJoin (fmap f sig)
    

timeSignal :: Time -> Double -> Signal Time
timeSignal start step = Signal start (s start) (\t _e -> timeSignal t step)
     where s t = let t' = t + step in (t', t') : s t'

getTimeIO :: IO Time
getTimeIO = do (TOD secs pics) <- System.Time.getClockTime
               return $ fromInteger secs + fromInteger pics / (10**(3*4))

constSignal :: a -> Signal a
constSignal value = Signal value [] (\_t _e -> constSignal value)

getSignal :: Time -> Signal a -> a
getSignal time (Signal start evs _) = getSignal' evs start where
    getSignal' ((t,v):xs) def | t > time  = def
                              | otherwise = getSignal' xs v
    getSignal' []          def            = def

getNextChange :: Time -> Signal a -> Maybe Time
getNextChange time (Signal _ evs _) = findNext evs where
    findNext ((t,_v):xs) | t > time  = Just t
			| otherwise = findNext xs
    findNext []                     = Nothing
    
updateSignal :: Time -> Signal a -> InputEvent -> Signal a
updateSignal t (Signal _ _ upd) ie = upd t ie
