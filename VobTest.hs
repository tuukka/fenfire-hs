
import Vobs
import Signals
import Data.Map (fromList)

myVob = rectBox $ pad 5 $ label "Hello World!"

myScene1 = do (vw, vh) <- defaultSize myVob
              return $ fromList [("Foo", (50, 50, vw, vh, myVob))]
myScene2 = do (vw, vh) <- defaultSize myVob
              return $ fromList [("Foo", (150, 150, vw+30, vh, myVob))]


interpSignal :: Ord a => Signal Time -> RScene a -> RScene a -> Signal Vob
interpSignal clock sc1 sc2 = flip fmap clock $ \t -> let
        alpha = sin (t/5*pi)/2+0.5
        isc = do sc1' <- sc1; sc2' <- sc2
                 return (interpolate alpha sc1' sc2')
        [fsc1, fsc2, fisc] = map (\x _ _ -> x) [sc1, sc2, isc]
        [v1, v2, iv] = map sceneVob [fsc1, fsc2, fisc]
        [v1', v2'] = map (rgbColor 0.5 0.5 0.5) [v1, v2]
        iv' = rgbColor 0 0 0 iv
    in overlay [v1', v2', iv']


main = do
    now <- getTimeIO
    vobMain "Example" $ interpSignal (timeSignal now 0.01) myScene1 myScene2
    
