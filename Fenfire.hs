module Fenfire where

import Vobs
import RDF

import qualified Data.Map as Map
import qualified Data.List
import Data.IORef
import Maybe (fromJust, isJust, isNothing, maybeToList)

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
vanishingView vs depth startRotation w h = runVanishing depth view where
    view = do moveTo (w/2) (h/2)
              placeNode startRotation
              dir <- choose [Pos, Neg]
              call $ placeConns startRotation dir True
                
    placeConns rotation xdir placeFirst = do
        increaseDepth 1
        if placeFirst then call $ placeConn rotation xdir else return ()
        ydir <- choose [-1, 1]
        call $ placeConns' rotation xdir ydir
        
    placeConns' rotation xdir ydir = do
        increaseDepth 1
        rotation' <- choose $ maybeToList $ rotate vs rotation ydir
        changeAngle (fromIntegral ydir * mul xdir pi / 14)
        call $ placeConn rotation' xdir
        call $ placeConns' rotation' xdir ydir
        
    placeConn rotation dir = do
        rotation' <- choose $ maybeToList $ move vs rotation dir
        movePolar dir 200
        placeNode rotation'
        call $ placeConns rotation' dir True
        call $ placeConns rotation' (rev dir) False
        
    placeNode (Rotation graph node _) = place node $ nodeView graph node
    
    
data VVState = VVState { vvDepth :: Int, vvX :: Double, vvY :: Double,
                         vvAngle :: Double }
                           
newtype VV a = VV (VVState -> ( Scene Node, [(VVState, a)] ))

instance Monad VV where
    return x = choose [x]
    
    (VV f) >>= g = VV h where
        h state = (hScene, hResults) where
            unVV (VV fn) = fn
            (fScene,  fResults) = f state
            (gScenes, gResults) = 
                unzip $ map (\(state', x) -> unVV (g x) state') fResults

            hScene   = Map.unions (fScene : gScenes)
            hResults = concat gResults
    
runVanishing :: Int -> VV a -> Scene Node
runVanishing depth (VV f) = fst $ f (VVState depth 0 0 0)

choose :: [a] -> VV a
choose xs = VV $ \state -> (Map.empty, map (\x -> (state,x)) xs)

call :: VV a -> VV ()   -- get the parameter's vobs without changing the state
call (VV f) = VV g where
    g state = (scene, [(state, ())])  where  (scene, _) = f state

increaseDepth :: Int -> VV ()
increaseDepth n = VV f where
    f state | depth <= 0 = (Map.empty, [])
            | otherwise  = (Map.empty, [(state { vvDepth=depth }, ())])
        where depth = vvDepth state - n

place :: Node -> Vob -> VV ()
place node vob = VV f where
    f state = (Map.fromList [entry], [(state, ())]) where
        entry = (node, (vvX state - w/2, vvY state - h/2, w, h, vob))
        (w,h) = defaultSize vob
        
changeState :: (VVState -> VVState) -> VV ()
changeState f = VV (\state -> (Map.empty, [(f state, ())]))

moveTo :: Double -> Double -> VV ()
moveTo x y = changeState (\s -> s { vvX = vvX s + x, vvY = vvY s + y })

movePolar :: Dir -> Double -> VV ()
movePolar dir distance = changeState result where
    distance' = mul dir distance
    result s = s { vvX = vvX s + distance' * cos (vvAngle s),
                   vvY = vvY s + distance' * sin (vvAngle s) }
                   
changeAngle :: Double -> VV ()
changeAngle delta = changeState result where
    result s = s { vvAngle = vvAngle s + delta }



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
        view = vanishingView vs 8
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
