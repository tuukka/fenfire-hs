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

import Vobs hiding (rotate)
import Utils
import Utils (Dual, Endo)
import RDF

import qualified Raptor (filenameToTriples, triplesToFilename, Identifier(..))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List
import Data.Set (Set)
import Data.IORef
import Data.Maybe (fromJust, isJust, isNothing, catMaybes)
import Data.Monoid(Monoid(mconcat))

import Control.Monad (when, guard)
import Control.Monad.Reader
import Control.Monad.State (StateT, get, gets, modify, put, execStateT)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Writer (Writer, execWriter, tell)

import Graphics.UI.Gtk hiding (Color, get, disconnect, fill)

import System.Environment (getArgs)
import System.Random (randomIO)
import System.Directory (canonicalizePath)

data ViewSettings = ViewSettings { hiddenProps :: [Node] }

data Rotation = Rotation Graph Node Int         deriving (Eq, Show)

getRotation :: ViewSettings -> Graph -> Node -> Node -> Dir -> Node ->
               Maybe Rotation
getRotation vs graph node prop dir node' = do
    i <- Data.List.elemIndex (prop, node') (conns vs graph node dir)
    return (Rotation graph node (i-length (conns vs graph node dir) `div` 2))
    
conns :: ViewSettings -> Graph -> Node -> Dir -> [(Node, Node)]
conns vs g node Pos = sortConns g [(p,o) | (s,p,o) <- g, s == node,
                                   not $ p `elem` hiddenProps vs]
conns vs g node Neg = sortConns g [(p,s) | (s,p,o) <- g, o == node,
                                   not $ p `elem` hiddenProps vs]
                                        
sortConns :: Graph -> [(Node, Node)] -> [(Node, Node)]
sortConns g = Data.List.sortBy cmp'
    where cmp n1 n2 = compare (getText g n1) (getText g n2)
          cmp' (p1,n1) (p2,n2) = catOrds (cmp p1 p2) (cmp n1 n2)
          catOrds EQ o = o; catOrds o  _ = o

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
setText g n t = map update g
    where update (s,p,o) | s == n && p == rdfs_label
                         = (n, rdfs_label, PlainLiteral t)
                         | otherwise = (s,p,o)

nodeView :: Graph -> Node -> Vob Node
nodeView g n = rectBox $ pad 5 $ multiline False 20 s
    where s = maybe (show n) id (getText g n)
    
propView :: Graph -> Node -> Vob Node
propView g n = mconcat [ useFadeColor $ fill extents,
                         pad 5 $ label $ maybe (show n) id (getText g n) ]



vanishingView :: ViewSettings -> Int -> (Rotation, Mark, FilePath) -> Vob Node
vanishingView vs depth (startRotation, mark, _fp) = runVanishing depth view
    where
    -- place the center of the view and all subtrees in both directions
    view = do placeNode startRotation
              let Rotation _ n _ = startRotation in visitNode n $
                  forM_ [Pos, Neg] $ \dir -> do
                      placeConns startRotation dir True
    -- place all subtrees in xdir
    placeConns rotation xdir placeFirst = increaseDepth 1 $ do
        when placeFirst $ placeConn rotation xdir
        forM_ [-1, 1] $ \ydir -> do
            placeConns' rotation xdir ydir
    -- place rest of the subtrees in (xdir, ydir)
    placeConns' rotation xdir ydir = increaseDepth 1 $
        maybeDo (rotate vs rotation ydir) $ \rotation' -> do
            changeAngle (fromIntegral ydir * mul xdir pi / 14) $ do
                placeConn rotation' xdir
                placeConns' rotation' xdir ydir
    -- place one subtree
    placeConn rotation@(Rotation graph n1 _) dir = increaseDepth 1 $
        maybeDo (getConn vs rotation dir) $ \(prop, rotation') ->
          let Rotation _ n2 _ = rotation' in visitNode n2 $ do
            scale' <- getScale
            movePolar dir (280 * (scale'**3)) $ do
                placeNode rotation'
                factor <- getFade
                let (nl,nr) = if dir==Pos then (n1,n2) else (n2,n1)
                addVob $ fade factor $ between (center @@ nl) (center @@ nr) $
                    centerVob $ scale scale' scale' $ propView graph prop
                addVob $ fade factor $ stroke $ 
                    line (center @@ nl) (center @@ nr)
                placeConns rotation' dir True
                increaseDepth 3 $ placeConns rotation' (rev dir) False
    -- place one node view    
    placeNode (Rotation graph node _) = do
        scale' <- getScale; fadeFactor <- getFade
        let f vob = if Just node /= mark then vob
                     else setBgColor (Color 1 0 0 1) vob
        placeVob $ scale scale' scale' $ fade fadeFactor $
                keyVob node $ f $ nodeView graph node
        
    getScale = do d <- asks vvDepth; return (0.97 ** fromIntegral (depth - d))
    getFade  = do d <- asks vvDepth
                  return (fromIntegral d / fromIntegral (depth+2))
    
    
data VVState = VVState { vvDepth :: Int, vvX :: Double, vvY :: Double,
                         vvAngle :: Double }
                         
type VV a = ReaderT VVState (BreadthT (StateT (Set Node) 
                                          (Writer (Dual (Vob Node))))) a

runVanishing :: Int -> VV () -> Vob Node
runVanishing depth vv = comb (0,0) $ \(w,h) -> 
    getDual $ execWriter $ flip execStateT Set.empty $ execBreadthT depth $
        runReaderT vv $ VVState depth (w/2) (h/2) 0
    
-- |Decrease remaining recursion depth by given amount of steps, and
-- cut execution when no depth is left.
--
increaseDepth :: Int -> VV () -> VV ()
increaseDepth n m = do
    state <- ask; let state' = state { vvDepth = vvDepth state - n }
    if vvDepth state' < 0 then return () else
        lift $ scheduleBreadthT $ runReaderT m state'
        
visitNode :: Node -> VV () -> VV ()
visitNode n m = get >>= \visited -> if n `Set.member` visited then return ()
                                       else modify (Set.insert n) >> m

addVob :: Vob Node -> VV ()
addVob vob = tell (Dual vob)

placeVob :: Vob Node -> VV ()
placeVob vob = do
    state <- ask
    addVob $ translate (vvX state) (vvY state) $ centerVob vob
        
movePolar :: Dir -> Double -> VV () -> VV ()
movePolar dir distance = local f where
    distance' = mul dir distance
    f s = s { vvX = vvX s + distance' * cos (vvAngle s),
              vvY = vvY s + distance' * sin (vvAngle s) }
                   
changeAngle :: Double -> VV () -> VV ()
changeAngle delta = local $ \s -> s { vvAngle = vvAngle s + delta }



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

loadGraph :: ViewSettings -> FilePath -> IO Rotation
loadGraph vs fileName = do
    --file <- readFile fileName
    --graph <- fromNTriples file >>= return . reverse-}
    let convert (s,p,o) = (f s, f p, f o)
        f (Raptor.Uri s) = URI s
        f (Raptor.Literal s) = PlainLiteral s
        f (Raptor.Blank s) = URI $ "blank:" ++ s
    graph <- Raptor.filenameToTriples fileName >>= return . map convert
    let rots = catMaybes $ flip map graph $
                        \(s,p,o) -> getRotation vs graph s p Pos o
    return $ last rots

saveGraph :: Graph -> FilePath -> IO ()
saveGraph graph fileName = do
    --writeFile fileName $ toNTriples $ reverse graph
    let convert (s,p,o) = (f s, f p, f o)
        f (URI s) = Raptor.Uri s
        f (PlainLiteral s) = Raptor.Literal s
    Raptor.triplesToFilename (map convert $ reverse graph) fileName

openFile :: ViewSettings -> Rotation -> FilePath -> IO (Rotation,FilePath)
openFile vs rot0 fileName0 = do
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen
                                   [("gtk-cancel", ResponseCancel),
                                    ("gtk-open", ResponseAccept)]
    when (fileName0 /= "") $ fileChooserSetFilename dialog fileName0 >> return ()
    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             rot <- loadGraph vs fileName
                             return (rot, fileName)
        _              -> return (rot0, fileName0)
        
saveFile :: Rotation -> FilePath -> IO FilePath
saveFile (Rotation graph _ _) fileName0 = do
    dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave
                                   [("gtk-cancel", ResponseCancel),
                                    ("gtk-save", ResponseAccept)]
    fileChooserSetDoOverwriteConfirmation dialog True
    dialogSetDefaultResponse dialog ResponseAccept
    when (fileName0 /= "") $ fileChooserSetFilename dialog fileName0 >> return ()
    onConfirmOverwrite dialog $ do Just fileName <- fileChooserGetFilename dialog
                                   if fileName == fileName0
                                       then return FileChooserConfirmationAcceptFilename
                                       else return FileChooserConfirmationConfirm
    response <- dialogRun dialog
    widgetHide dialog
    case response of
        ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                             saveGraph graph fileName
                             return fileName
        _              -> return fileName0


handleKey :: ViewSettings -> Handler (Rotation, Mark, FilePath)
handleKey vs (Key { eventModifier=_, eventKeyName=key }) = do
  (rot@(Rotation _ node _), mk, fileName) <- get
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
    "O"     -> do (rot',fp') <- liftIO $ openFile vs rot fileName
                  put (rot', Nothing, fp')
    "S"     -> liftIO ( saveFile rot fileName ) >>= \fp' -> put (rot, mk, fp')
    "q"     -> liftIO $ mainQuit
    _       -> unhandledEvent
  where putRotation rot = do modify $ \(_,mk,fp)  -> (rot,mk,fp); setInterp True
        putMark mk      = do modify $ \(rot,_,fp) -> (rot,mk,fp)
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
        emptyState = (rot, Nothing, "")

    stateRef <- newIORef emptyState

    case args of [fileName] -> do fn <- canonicalizePath fileName
                                  rot' <- loadGraph vs fn
                                  writeIORef stateRef (rot', Nothing, fn)
                 _          -> return ()

    initGUI
    window <- windowNew
    windowSetTitle window "Fenfire"
    windowSetDefaultSize window 800 550
    
    textView <- textViewNew
    textViewSetAcceptsTab textView False

    let stateChanged (Rotation g n _r, _mark, _fileName) = do
        buf <- textBufferNew Nothing
        textBufferSetText buf (maybe "" id $ getText g n)
        afterBufferChanged buf $ do 
                                start <- textBufferGetStartIter buf
                                end   <- textBufferGetEndIter buf
                                text  <- textBufferGetText buf start end True
                                (Rotation g' n' r', mk', fp') <- readIORef stateRef
                                -- buf corresponds to n, not to n'
                                let g'' = setText g' n text
                                writeIORef stateRef $ (Rotation g'' n' r', mk', fp')
                                updateCanvas True
        textViewSetBuffer textView buf
        return ()
    
    readIORef stateRef >>= stateChanged
    
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
