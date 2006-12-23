module Fenfire where

import Vobs
import RDF

import qualified Data.Map as Map
import qualified Data.List
import Data.IORef
import Maybe (fromJust, isJust, isNothing)

import Graphics.UI.Gtk hiding (get)

data ViewSettings = ViewSettings { hiddenProps :: [Node] }

data Rotation = Rotation Graph Node Int         deriving (Eq, Show)

getRotation :: ViewSettings -> Graph -> Node -> Node -> Dir -> Node ->
               Maybe Rotation
getRotation vs graph node prop dir node' = do
    i <- Data.List.elemIndex (prop, node') (conns vs graph node dir)
    return (Rotation graph node (i-length (conns vs graph node dir) `div` 2))
    
conns :: ViewSettings -> Graph -> Node -> Dir -> [(Node, Node)]
conns vs g node Pos = [(p,o) | (s,p,o) <- g, s == node,
                               not $ p `elem` hiddenProps vs]
conns vs g node Neg = [(p,s) | (s,p,o) <- g, o == node,
                               not $ p `elem` hiddenProps vs]

rotate :: ViewSettings -> Rotation -> Int -> Maybe Rotation
rotate vs (Rotation g n r) dir = 
    if abs (r+dir) > h then Nothing else Just $ Rotation g n (r+dir)
  where 
    h = max (length $ conns vs g n Pos) (length $ conns vs g n Neg) `div` 2

move :: ViewSettings -> Rotation -> Dir -> Maybe Rotation
move vs (Rotation graph node rot) dir = result where
    c = conns vs graph node dir
    index = (length c `div` 2) + rot
    result = if index >= 0 && index < length c 
             then let (p,n) = c !! index in 
                  getRotation vs graph n p (rev dir) node
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


vanishingView :: ViewSettings -> Int -> Rotation -> Double -> Double ->
                 Scene Node
vanishingView vs depth start w h = 
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
            rot' = do r' <- rotate vs rot offs; move vs r' xdir
            result = if isNothing rot' then [] else
                combine [ oneNode (translate angle (mul xdir 200) (x,y))
                                  angle (fromJust rot'),
                          connections (x,y) rot (offs+ydir) 
                                      (angle+fromIntegral ydir*angleOffs)
                                      xdir ydir ]
                
        translate :: Double -> Double -> (Double, Double) -> (Double, Double)
        translate angle distance (x,y) = 
            (x + distance * cos angle, y + distance * sin angle)


handleKey :: ViewSettings -> Handler Rotation
handleKey vs (Key { eventModifier=_, eventKeyName=key }) rot = case key of
    "Up"    -> m rotate (-1); "i" -> m rotate (-1)
    "Down"  -> m rotate 1;    "," -> m rotate 1
    "Left"  -> m move Neg;    "j" -> m move Neg
    "Right" -> m move Pos;    "l" -> m move Pos
    "q"     -> Just $ do mainQuit; return undefined
    _       -> Nothing
  where m f x = fmap (\rot' -> return (rot', True)) $ f vs rot x

handleKey _ _ _ = Nothing
            
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
    let vs = ViewSettings { hiddenProps=[rdfs_label] }
        view = vanishingView vs 3
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
        vobCanvas stateRef view (handleKey vs) stateChanged
    
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
