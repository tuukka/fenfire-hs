
module Fenfire where

import Vobs

import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo

import Data.IORef

import qualified Data.Map as Map
import qualified Data.List
import Maybe (fromJust, isJust, isNothing)

data Node = URI String | PlainLiteral String    deriving (Eq, Ord, Show)
data Dir  = Pos | Neg                           deriving (Eq, Ord, Show)

type Graph = [(Node, Node, Node)]

rev Pos = Neg
rev Neg = Pos

mul Pos = id
mul Neg = negate

conns g node Pos = [(prop, obj) | (subj, prop, obj) <- g, subj == node]
conns g node Neg = [(prop, subj) | (subj, prop, obj) <- g, obj  == node]

data Rotation = Rotation Graph Node Int         deriving (Eq, Show)

getRotation :: Graph -> Node -> Node -> Dir -> Node -> Maybe Rotation
getRotation graph node prop dir node' = do
    i <- Data.List.elemIndex (prop, node') (conns graph node dir)
    return (Rotation graph node' i)
    
sheight (Rotation g n _) = 
    max (length $ conns g n Pos) (length $ conns g n Neg) `div` 2

get :: Rotation -> Dir -> Int -> Maybe Rotation
get r@(Rotation graph node rot) dir rot' = result where
    c = conns graph node dir
    index = (length c `div` 2) + rot + rot'
    result = if index >= 0 && index < length c 
             then let (p,n) = c !! index in 
                  getRotation graph node p dir n
             else Nothing
        
-- Some Fenfire views, like the vanishing wheel view, show a conceptually
-- infinitely deep picture, cut off at some depth when actually rendered
-- on the screen. This type is the output of such a view, structured
-- as a list of scenes of increasing depth.
type InfiniteScene = [Scene Node]

combine :: [InfiniteScene] -> InfiniteScene
combine scenes = (Map.unions $ concatMap (take 1) scenes) : combine (map (drop 1) scenes)

nodeView n = rectBox $ clipVob $ resizeY 80 $ pad 5 $ label (show n)


vanishingView :: Int -> Rotation -> Double -> Double -> Scene Node
vanishingView depth start w h = 
    Map.unions $ take depth $ oneNode (w/2, h/2) 0 start where -- XXX
        oneNode :: (Double, Double) -> Double -> Rotation -> InfiniteScene
        oneNode (x,y) angle rot@(Rotation _ node _) = 
            Map.fromList [(node, (x, y, 80, 30, nodeView node))]
                : combine [ connections (x,y) rot 0 angle xdir ydir
                          | xdir <- [Neg, Pos], ydir <- [-1, 1] ]
                
        angleOffs = pi / 21
                
        connections :: (Double, Double) -> Rotation -> Int -> Double -> 
                       Dir -> Int -> InfiniteScene
        connections (x,y) rot offs angle xdir ydir = result where
            rot' = (get rot xdir offs)
            result = if isNothing rot' then [] else
                combine [ oneNode (translate angle (mul xdir 100) (x,y))
                                  angle (fromJust rot'),
                          connections (x,y) rot (offs+ydir) 
                                      (angle+fromIntegral ydir*angleOffs)
                                      xdir ydir ]
                
        translate :: Double -> Double -> (Double, Double) -> (Double, Double)
        translate angle distance (x,y) = 
            (x + distance * sin angle, y + distance * cos angle)



node = PlainLiteral "Home"
node2 = PlainLiteral "A"
node3 = PlainLiteral "B"
prop = PlainLiteral "prop"
graph = [(node,prop,node2),(node,prop,node3)]            
            
main = do
    initGUI
    window <- windowNew
    windowSetTitle window "Example"
    
    canvas <- drawingAreaNew
    set window [ containerChild := canvas ]


    state <- newIORef (Rotation graph node 0)

    onKeyPress window $ \(Key { eventModifier=mods, eventKeyName=key, eventKeyChar=char }) -> do
        liftIO $ putStrLn $ show mods++key++" ("++show char++")"

        (Rotation graph node rotation) <- readIORef state
	writeIORef state (Rotation graph node rotation)
	return False
    
    onExpose canvas $ \(Expose { eventArea=rect }) -> do
        (cw, ch) <- drawingAreaGetSize canvas
        let w = fromIntegral cw; h = fromIntegral ch
        drawable <- drawingAreaGetDrawWindow canvas

        rotation <- readIORef state
        
        renderWithDrawable drawable $ do
            save

	    let scene = vanishingView 3 rotation w h
	    drawVob (sceneVob scene) w h

            restore
            
        return True

    flip timeoutAdd (1000 `div` 100) (widgetQueueDraw canvas >> return True)
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

