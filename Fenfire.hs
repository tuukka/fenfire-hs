module Fenfire where

import Vobs

import qualified Data.Map as Map
import qualified Data.List
import Data.IORef
import Maybe (fromJust, isJust, isNothing)

import Graphics.UI.Gtk hiding (get)

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

getText :: Graph -> Node -> Maybe String
getText g n = fmap (\(_s,_p,o) -> fromNode o)
                   (Data.List.find (\(s,p,_o) -> s==n && p==rdfs_label) g)
                    
setText :: Graph -> Node -> String -> Graph
setText g n t = (n, rdfs_label, PlainLiteral t) :
                [(s,p,o) | (s,p,o) <- g, not (s == n && p == rdfs_label)]

nodeView :: Graph -> Node -> Vob
nodeView g n = rectBox $ clipVob $ pad 5 $ multiline False 20 s
    where s = maybe (show n) id (getText g n)


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
                combine [ oneNode (translate angle (mul xdir 200) (x,y))
                                  angle (fromJust rot'),
                          connections (x,y) rot (offs+ydir) 
                                      (angle+fromIntegral ydir*angleOffs)
                                      xdir ydir ]
                
        translate :: Double -> Double -> (Double, Double) -> (Double, Double)
        translate angle distance (x,y) = 
            (x + distance * cos angle, y + distance * sin angle)


handleKey :: [Node] -> Handler Rotation
-- handleKey :: [Node] -> Event -> Rotation -> IO (Rotation, Bool)
handleKey props (Key { eventModifier=_, eventKeyName=key, eventKeyChar=_ })
          rot@(Rotation graph node rotation) = return $ case key of
    "Up"    -> rotate (-1); "i" -> rotate (-1)
    "Down"  -> rotate 1;    "," -> rotate 1
    "Left"  -> move Neg;    "j" -> move Neg
    "Right" -> move Pos;    "l" -> move Pos
    _       -> (rot, False)
  where h = height rot props
        rotate dir = 
            (Rotation graph node $ max (-h) $ min h $ rotation+dir, True)
        move dir = (maybe rot id $ get props rot dir 0, True)

handleKey _ _ rot = return (rot, False)
            
home = URI "ex:0"
nodeA = URI "ex:A"
nodeAA = URI "ex:AA"
nodeAB = URI "ex:AB"
nodeB = URI "ex:B"
testGraph = [(home, lbl, lit "Home"),
             (home, prop, nodeA), (nodeA, lbl, lit "Node A"),
             (nodeA, prop, nodeAA),
             (nodeA, prop, nodeAB),
             (home, prop, nodeB), (nodeB, lbl, lit "Node B")]
    where prop = rdfs_seeAlso
          lbl = rdfs_label
          lit = PlainLiteral

main :: IO ()
main = do
    let props = [rdfs_seeAlso]
        view = vanishingView props 3
        startState = Rotation testGraph home 0

    stateRef <- newIORef startState
    
    initGUI
    window <- windowNew
    windowSetTitle window "Fenfire"
    windowSetDefaultSize window 700 550
    
    textView <- textViewNew
    textViewSetAcceptsTab textView False
    buf <- textViewGetBuffer textView

    let stateChanged (Rotation g n _r) = do
        textBufferSetText buf (maybe "" id $ getText g n)
        
    stateChanged startState
    
    (canvas, updateCanvas) <- 
        vobCanvas stateRef view (handleKey props) stateChanged
    
    afterBufferChanged buf $ do start <- textBufferGetStartIter buf
                                end   <- textBufferGetEndIter buf
                                text  <- textBufferGetText buf start end True
                                Rotation g n r <- readIORef stateRef
                                let g' = setText g n text
                                writeIORef stateRef $ Rotation g' n r
                                updateCanvas True

    paned <- vPanedNew
    panedAdd1 paned canvas
    panedAdd2 paned textView
    
    set paned [ panedPosition := 400 ]

    set window [ containerChild := paned ]
    
    onDestroy window mainQuit
    widgetShowAll window
    widgetGrabFocus canvas
    mainGUI
