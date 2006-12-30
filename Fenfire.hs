module Fenfire where

import Vobs
import RDF

import qualified Data.Map as Map
import qualified Data.List
import Data.IORef
import Data.Maybe (fromJust, isJust, isNothing)

import Control.Monad (MonadPlus, mzero, mplus, msum)
import Control.Monad.State (State, StateT, get, gets, modify, put,
                            runState, runStateT,
                            withState, execState, evalState, evalStateT)
import Control.Monad.List  (ListT(ListT), runListT)
import Control.Monad.Trans (lift)

import Graphics.UI.Gtk hiding (get)

data ViewSettings = ViewSettings { hiddenProps :: [Node] }

data Rotation = Rotation Graph Node Int         deriving (Eq, Show)

maybeReturn :: MonadPlus m => Maybe a -> m a
maybeReturn = maybe mzero return

returnEach :: MonadPlus m => [a] -> m a
returnEach = msum . map return

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
    if idx < 0 || idx >= h then Nothing else Just $ Rotation g n (r+dir)
  where 
    h = max (length $ conns vs g n Pos) (length $ conns vs g n Neg)
    idx = r+dir+(h `div` 2)

getConn :: ViewSettings -> Rotation -> Dir -> Maybe (Node, Rotation)
getConn vs (Rotation graph node rot) dir = result where
    c = conns vs graph node dir
    index = (length c `div` 2) + rot
    result = if index >= 0 && index < length c 
             then let (p,n) = c !! index in 
                  fmap (\r -> (p,r)) (getRotation vs graph n p (rev dir) node)
             else Nothing
             
move :: ViewSettings -> Rotation -> Dir -> Maybe Rotation
move vs rot dir = fmap snd (getConn vs rot dir)

getText :: Graph -> Node -> Maybe String
getText g n = fmap (\(_s,_p,o) -> fromNode o)
                   (Data.List.find (\(s,p,_o) -> s==n && p==rdfs_label) g)
                    
setText :: Graph -> Node -> String -> Graph
setText g n t = (n, rdfs_label, PlainLiteral t) :
                [(s,p,o) | (s,p,o) <- g, not (s == n && p == rdfs_label)]

nodeView :: Graph -> Node -> Vob Node
nodeView g n = rectBox $ clipVob $ pad 5 $ multiline False 20 s
    where s = maybe (show n) id (getText g n)
    
propView :: Graph -> Node -> Vob Node
propView g n = overlay [ useFadeColor $ fillRect (0,0),
                         pad 5 $ label $ maybe (show n) id (getText g n) ]



vanishingView :: ViewSettings -> Int -> Rotation -> Vob Node
vanishingView vs depth startRotation = runVanishing depth view where
    view = do placeNode startRotation
              dir <- returnEach [Pos, Neg]
              placeConns startRotation dir True
                
    placeConns rotation xdir placeFirst = call $ do
        increaseDepth 2
        if placeFirst then call $ placeConn rotation xdir else return ()
        ydir <- returnEach [-1, 1]
        placeConns' rotation xdir ydir
        
    placeConns' rotation xdir ydir = call $ do
        increaseDepth 1
        rotation' <- maybeReturn $ rotate vs rotation ydir
        changeAngle (fromIntegral ydir * mul xdir pi / 14)
        placeConn rotation' xdir
        placeConns' rotation' xdir ydir
        
    placeConn rotation@(Rotation graph n1 _) dir = call $ do
        (prop, rotation'@(Rotation _ n2 _))
            <- maybeReturn $ getConn vs rotation dir
        scale <- getScale
        movePolar dir (250 * scale)
        placeNode rotation'
        getFade >>= \fade -> do
            addVob $ onConnection n1 n2 $ fadeVob fade $ 
               scaleVob scale scale $ propView graph prop
            addVob $ fadeVob fade $ connection n1 n2
        placeConns rotation' dir True
        increaseDepth 3
        placeConns rotation' (rev dir) False
        
    placeNode (Rotation graph node _) = call $ do
        scale <- getScale; fadeFactor <- getFade
        placeVob $ scaleVob scale scale $ fadeVob fadeFactor $
            keyVob node $ nodeView graph node
        
    getScale = do d <- gets vvDepth; return (0.97 ** fromIntegral (depth - d))
    getFade  = do d <- gets vvDepth
                  return (fromIntegral d / fromIntegral (depth+2))
    
    
data VVState = VVState { vvDepth :: Int, vvX :: Double, vvY :: Double,
                         vvAngle :: Double }
                         
type VV a = StateT VVState (ListT (State [Vob Node])) a

runVanishing :: Int -> VV () -> Vob Node
runVanishing depth vv = comb (0,0) $ \(w,h) -> overlay $
    execState (runListT $ evalStateT vv $ VVState depth (w/2) (h/2) 0) []
    
call :: VV a -> VV ()   -- get the parameter's vobs without changing the state
call vv = do state <- get; vobs <- lift get
             let vobs' = execState (runListT (evalStateT vv state)) vobs
             lift $ put vobs'

increaseDepth :: Int -> VV ()
increaseDepth n = do state <- get; let depth = (vvDepth state - n)
                     if depth <= 0 then mzero
                                   else modify (\s -> s { vvDepth=depth })

addVob :: Vob Node -> VV ()
addVob vob = lift $ modify $ (vob:)

placeVob :: Vob Node -> VV ()
placeVob vob = do
    state <- get
    addVob $ translateVob (vvX state) (vvY state) $ centerVob vob
        
movePolar :: Dir -> Double -> VV ()
movePolar dir distance = modify result where
    distance' = mul dir distance
    result s = s { vvX = vvX s + distance' * cos (vvAngle s),
                   vvY = vvY s + distance' * sin (vvAngle s) }
                   
changeAngle :: Double -> VV ()
changeAngle delta = modify $ \s -> s { vvAngle = vvAngle s + delta }



handleKey :: ViewSettings -> Handler Rotation
handleKey vs (Key { eventModifier=_, eventKeyName=key }) rot = case key of
    "Up"    -> m rotate' (-1); "i" -> m rotate' (-1)
    "Down"  -> m rotate' 1;    "," -> m rotate' 1
    "Left"  -> m move Neg;    "j" -> m move Neg
    "Right" -> m move Pos;    "l" -> m move Pos
    "q"     -> Just $ do mainQuit; return undefined
    _       -> Nothing
  where m f x = fmap (\rot' -> return (rot', True)) $ f vs rot x
        rotate' vs' rot' x' = rotate vs' rot' x' `mplus` Just rot'

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
        view = vanishingView vs 20
        startState = Rotation testGraph home 0

    stateRef <- newIORef startState
    
    initGUI
    window <- windowNew
    windowSetTitle window "Fenfire"
    windowSetDefaultSize window 800 550
    
    textView <- textViewNew
    textViewSetAcceptsTab textView False
    buf <- textViewGetBuffer textView

    let stateChanged (Rotation g n _r) = do
        textBufferSetText buf (maybe "" id $ getText g n)
        
    stateChanged startState
    
    (canvas, updateCanvas) <- 
        vobCanvas stateRef view (handleKey vs) stateChanged lightGray
    
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
