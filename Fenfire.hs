
import Vobs

import qualified Data.Map as Map
import qualified Data.List
import Maybe (fromJust, isJust, isNothing)

data Node = URI String | PlainLiteral String    deriving (Eq, Ord)
data Dir  = Pos | Neg                           deriving (Eq, Ord, Show)

instance Show Node where
    show (URI uri)        = "<" ++ uri ++ ">"
    show (PlainLiteral s) = "\"" ++ s ++ "\""

type Graph = [(Node, Node, Node)]

rdfs_label   = URI "http://www.w3.org/2000/01/rdf-schema#label"
rdfs_seeAlso = URI "http://www.w3.org/2000/01/rdf-schema#seeAlso"

fromNode :: Node -> String
fromNode (URI uri)        = uri
fromNode (PlainLiteral s) = s

rev :: Dir -> Dir
rev Pos = Neg
rev Neg = Pos

mul :: Num a => Dir -> a -> a
mul Pos = id
mul Neg = negate

conns :: Graph -> [Node] -> Node -> Dir -> [(Node, Node)]
conns g props node Pos = [(prop, obj)  | (subj, prop, obj) <- g, 
                                         subj == node, prop `elem` props]
conns g props node Neg = [(prop, subj) | (subj, prop, obj) <- g, 
                                         obj == node,  prop `elem` props]

data Rotation = Rotation Graph Node Int         deriving (Eq, Show)

getRotation :: Graph -> [Node] -> Node -> Node -> Dir -> Node -> Maybe Rotation
getRotation graph props node prop dir node' = do
    i <- Data.List.elemIndex (prop, node') (conns graph props node dir)
    return (Rotation graph node (i-length (conns graph props node dir) `div` 2))
    
height :: Rotation -> [Node] -> Int
height (Rotation g n _) props = 
    max (length $ conns g props n Pos) (length $ conns g props n Neg) `div` 2

get :: [Node] -> Rotation -> Dir -> Int -> Maybe Rotation
get props (Rotation graph node rot) dir rot' = result where
    c = conns graph props node dir
    index = (length c `div` 2) + rot + rot'
    result = if index >= 0 && index < length c 
             then let (p,n) = c !! index in 
                  getRotation graph props n p (rev dir) node
             else Nothing
        
-- Some Fenfire views, like the vanishing wheel view, show a conceptually
-- infinitely deep picture, cut off at some depth when actually rendered
-- on the screen. This type is the output of such a view, structured
-- as a list of scenes of increasing depth.
type InfiniteScene = [Scene Node]

combine :: [InfiniteScene] -> InfiniteScene
combine scenes = (Map.unions $ concatMap (take 1) scenes) : combine (map (drop 1) scenes)

nodeView :: Graph -> Node -> Vob
nodeView g n = rectBox $ clipVob $ resizeX 100 $ pad 5 $ label s
    where s = maybe (show n) (\(s,p,o) -> fromNode o)
                    (Data.List.find (\(s,p,o) -> s==n && p==rdfs_label) g)


vanishingView :: [Node] -> Int -> Rotation -> Double -> Double -> Scene Node
vanishingView props depth start w h = 
    Map.unions $ take depth $ oneNode (w/2, h/2) 0 start where -- XXX
        oneNode :: (Double, Double) -> Double -> Rotation -> InfiniteScene
        oneNode (x,y) angle rot@(Rotation graph node rot0) = 
            let vob@(Vob (vw, vh) _) = nodeView graph node in
            Map.fromList [(node, (x-vw/2, y-vh/2, vw, vh, vob))]
                : combine [ connections (x,y) rot (-rot0) (angle-fromIntegral rot0*mul xdir angleOffs) xdir ydir
                          | xdir <- [Neg, Pos], ydir <- [-1, 1] ]
                
        angleOffs = pi / 14
                
        connections :: (Double, Double) -> Rotation -> Int -> Double -> 
                       Dir -> Int -> InfiniteScene
        connections (x,y) rot offs angle xdir ydir = result where
            rot' = (get props rot xdir offs)
            result = if isNothing rot' then [] else
                combine [ oneNode (translate angle (mul xdir 150) (x,y))
                                  angle (fromJust rot'),
                          connections (x,y) rot (offs+ydir) 
                                      (angle+fromIntegral ydir*angleOffs)
                                      xdir ydir ]
                
        translate :: Double -> Double -> (Double, Double) -> (Double, Double)
        translate angle distance (x,y) = 
            (x + distance * cos angle, y + distance * sin angle)


handleKey :: [Node] -> Key -> Rotation -> Rotation
handleKey props key rot@(Rotation graph node rotation) = case key of
    "Up"    -> Rotation graph node $ max (-h) $ min h $ rotation-1
    "Down"  -> Rotation graph node $ max (-h) $ min h $ rotation+1
    "Left"  -> maybe rot id $ get props rot Neg 0
    "Right" -> maybe rot id $ get props rot Pos 0
    _       -> rot
  where h = height rot props

            
main :: IO ()
main = do
    let node = URI "ex:0"
        nodeA = URI "ex:A"
        nodeAA = URI "ex:AA"
        nodeAB = URI "ex:AB"
        nodeB = URI "ex:B"
        prop = rdfs_seeAlso; lbl = rdfs_label
        props = [prop]
        lit = PlainLiteral
        graph = [(node, lbl, lit "Home"),
                 (node, prop, nodeA), (nodeA, lbl, lit "Node A"),
                 (nodeA, prop, nodeAA),
                 (nodeA, prop, nodeAB),
                 (node, prop, nodeB), (nodeB, lbl, lit "Node B")]

    let rot = (Rotation graph node 0)
    vobMain "Fenfire" rot (vanishingView props 3) (handleKey props)
