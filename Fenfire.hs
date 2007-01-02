module Fenfire where

-- Copyright (c) 2006-2007, Benja Fallenstein, Tuukka Hastrup
-- This file is part of Fenfire.
-- 
-- Fenfire is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- Fenfire is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
-- Public License for more details.
-- 
-- You should have received a copy of the GNU General
-- Public License along with Fenfire; if not, write to the Free
-- Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
-- MA  02111-1307  USA

import Vobs
import RDF

import qualified Data.Map as Map
import qualified Data.List
import Data.IORef
import Data.Maybe (fromJust, isJust, isNothing, catMaybes)

import Control.Monad (MonadPlus, mzero, mplus, msum)
import Control.Monad.State (State, StateT, get, gets, modify, put,
                            runState, runStateT,
                            withState, execState, evalState, evalStateT)
import Control.Monad.List  (ListT(ListT), runListT)
import Control.Monad.Trans (lift, liftIO)

import Graphics.UI.Gtk hiding (get, Color, disconnect)

import System.Random (randomIO)
import System (getArgs)

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



vanishingView :: ViewSettings -> Int -> (Rotation, Mark) -> Vob Node
vanishingView vs depth (startRotation, mark) = runVanishing depth view where
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
            let (nl,nr) = if dir==Pos then (n1,n2) else (n2,n1)
            addVob $ onConnection nl nr $ fadeVob fade $ 
               scaleVob scale scale $ propView graph prop
            addVob $ fadeVob fade $ connection nl nr
        placeConns rotation' dir True
        increaseDepth 3
        placeConns rotation' (rev dir) False
        
    placeNode (Rotation graph node _) = call $ do
        scale <- getScale; fadeFactor <- getFade
        let f vob = if Just node /= mark then vob
                        else setBgColor (Color 1 0 0 1) vob
        placeVob $ scaleVob scale scale $ fadeVob fadeFactor $
            keyVob node $ f $ nodeView graph node
        
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



newURI :: IO Node
newURI = do rand <- randomIO
            return $ URI $ "ex:" ++ show (rand :: Int)

newNode :: ViewSettings -> Rotation -> Dir -> IO Rotation
newNode vs (Rotation graph node _) dir = do
    node' <- newURI
    let graph' = (if dir == Pos then (node, rdfs_seeAlso, node')
                                else (node', rdfs_seeAlso, node))
                     : (node', rdfs_label, PlainLiteral "") : graph
    return $ fromJust $ getRotation vs graph' node' rdfs_seeAlso (rev dir) node
    
connect :: ViewSettings -> Rotation -> Dir -> Node -> Rotation
connect vs (Rotation graph node _) dir node' =
    let triple = (if dir == Pos then (node, rdfs_seeAlso, node')
                                else (node', rdfs_seeAlso, node))
        graph' = if triple `elem` graph then graph
                                        else triple:graph
    in fromJust $ getRotation vs graph' node rdfs_seeAlso dir node'

disconnect :: ViewSettings -> Rotation -> Dir -> Maybe Rotation
disconnect vs (Rotation graph node rot) dir = 
    let
        c = conns vs graph node dir
        index = (length c `div` 2) + rot
        (p,n) = c !! index
        triple = case dir of Pos -> (node,p,n)
                             Neg -> (n,p,node)
        graph' = filter (/= triple) graph
        index' = ((length c-1) `div` 2) + rot
        rot' = case index' of x | x == -1                   -> rot+1
                                | x == length c-1 && x /= 0 -> rot-1
                                | otherwise                 -> rot
    in 
        if index >= 0 && index < length c 
        then Just $ Rotation graph' node rot'
        else Nothing


type Mark = Maybe Node

toggleMark :: Node -> Mark -> Mark
toggleMark n Nothing = Just n
toggleMark n (Just n') | n == n'   = Nothing
                       | otherwise = Just n

loadGraph :: ViewSettings -> String -> IO Rotation
loadGraph vs fileName = do
    file <- readFile fileName
    graph <- fromNTriples file >>= return . reverse
    let rots = catMaybes $ flip map graph $
                        \(s,p,o) -> getRotation vs graph s p Pos o
    return $ last rots
                       
openFile :: ViewSettings -> Rotation -> IO Rotation
openFile vs rot0 = do
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen
                                   [("Open", ResponseAccept),
                                    ("Cancel", ResponseCancel)]
    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             loadGraph vs fileName
        _              -> return rot0
        
saveFile :: Rotation -> IO ()
saveFile (Rotation graph _ _) = do
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave
                                   [("Save", ResponseAccept),
                                    ("Cancel", ResponseCancel)]
    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             writeFile fileName $ toNTriples $ reverse graph
        _              -> return ()


handleKey :: ViewSettings -> Handler (Rotation, Mark)
handleKey vs (Key { eventModifier=_, eventKeyName=key }) = do
  (rot@(Rotation _ node _), mk) <- get
  let m f x = maybeDo (f vs rot x) putRotation
      n f x = liftIO (f vs rot x) >>= putRotation
      o f x = maybeDo mk $ \node' -> putState (f vs rot x node') Nothing
  case key of
    "Up"    -> m rotate (-1);    "i" -> m rotate (-1)
    "Down"  -> m rotate 1;       "comma" -> m rotate 1
    "Left"  -> m move Neg;       "j" -> m move Neg
    "Right" -> m move Pos;       "l" -> m move Pos
    "n"     -> n newNode Pos;    "N" -> n newNode Neg
    "c"     -> o connect Pos;    "C" -> o connect Neg
    "b"     -> m disconnect Pos; "B" -> m disconnect Neg
    "m"     -> putMark $ toggleMark node mk
    "O"     -> do rot' <- liftIO $ openFile vs rot
                  put (rot', Nothing)
    "S"     -> liftIO $ saveFile rot
    "q"     -> liftIO $ mainQuit
    _       -> unhandledEvent
  where maybeDo m f     = case m of Just x -> f x; Nothing -> return ()
        putRotation rot = do modify $ \(_,mk)  -> (rot,mk); setInterp True
        putMark mk      = do modify $ \(rot,_) -> (rot,mk)
        putState rot mk = do putMark mk; putRotation rot

handleKey _ _ = unhandledEvent
            
main :: IO ()
main = mdo
    args <- getArgs

    home <- newURI

    let vs = ViewSettings { hiddenProps=[rdfs_label] }
        view = vanishingView vs 20
        graph = [(home, rdfs_label, PlainLiteral "")]
        rot = (Rotation graph home 0)
        startState = (rot, Nothing)

    stateRef <- newIORef startState

    case args of [fileName] -> do rot' <- loadGraph vs fileName
                                  writeIORef stateRef (rot', Nothing)
                 _          -> return ()

    initGUI
    window <- windowNew
    windowSetTitle window "Fenfire"
    windowSetDefaultSize window 800 550
    
    textView <- textViewNew
    textViewSetAcceptsTab textView False

    let stateChanged (Rotation g n _r, _mark) = do
        buf <- textBufferNew Nothing
        textBufferSetText buf (maybe "" id $ getText g n)
        afterBufferChanged buf $ do 
                                start <- textBufferGetStartIter buf
                                end   <- textBufferGetEndIter buf
                                text  <- textBufferGetText buf start end True
                                (Rotation g' n' r', mk') <- readIORef stateRef
                                let g'' = setText g' n' text
                                writeIORef stateRef $ (Rotation g'' n' r', mk')
                                updateCanvas True
        textViewSetBuffer textView buf
        return ()
    
    stateChanged startState
    
    (canvas, updateCanvas) <- 
        vobCanvas stateRef view (handleKey vs) stateChanged lightGray
    
    paned <- vPanedNew
    panedAdd1 paned canvas
    panedAdd2 paned textView
    
    set paned [ panedPosition := 400 ]

    set window [ containerChild := paned ]
    
    onDestroy window mainQuit
    widgetShowAll window
    widgetGrabFocus canvas
    mainGUI
