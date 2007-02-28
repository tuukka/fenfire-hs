-- GENERATED file. Edit the ORIGINAL Fenfire.fhs instead.
{-# LINE 1 "Fenfire.fhs" #-}
{-# OPTIONS_GHC -fth #-} {-# OPTIONS_GHC -fallow-overlapping-instances -fimplicit-params #-}
                         module Fenfire where
{-# LINE 1 "Fenfire.fhs" #-}
import qualified FunctorSugar
{-# LINE 22 "Fenfire.fhs" #-}
import qualified Cache
{-# LINE 23 "Fenfire.fhs" #-}
import Cairo hiding (rotate, Path)
{-# LINE 24 "Fenfire.fhs" #-}
import Vobs
{-# LINE 25 "Fenfire.fhs" #-}
import Utils
{-# LINE 26 "Fenfire.fhs" #-}
import RDF
{-# LINE 28 "Fenfire.fhs" #-}
import qualified Raptor (filenameToTriples, uriToTriples,
                         triplesToFilename, filenameToURI, Identifier(..))
{-# LINE 31 "Fenfire.fhs" #-}
import qualified Data.Map as Map
{-# LINE 32 "Fenfire.fhs" #-}
import qualified Data.Set as Set
{-# LINE 33 "Fenfire.fhs" #-}
import qualified Data.Tree as Tree
{-# LINE 34 "Fenfire.fhs" #-}
import Data.List (intersperse)
{-# LINE 35 "Fenfire.fhs" #-}
import qualified Data.List
{-# LINE 36 "Fenfire.fhs" #-}
import Data.Set (Set)
{-# LINE 37 "Fenfire.fhs" #-}
import Data.IORef
{-# LINE 38 "Fenfire.fhs" #-}
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing,
                   catMaybes)
{-# LINE 39 "Fenfire.fhs" #-}
import Data.Monoid (Monoid(mempty, mconcat), Dual(Dual), getDual)
{-# LINE 41 "Fenfire.fhs" #-}
import Control.Applicative
{-# LINE 42 "Fenfire.fhs" #-}
import qualified Control.Exception
{-# LINE 43 "Fenfire.fhs" #-}
import Control.Monad (when, guard, mplus, msum, liftM, join)
{-# LINE 44 "Fenfire.fhs" #-}
import Control.Monad.Reader (ReaderT, runReaderT, local, ask, asks)
{-# LINE 45 "Fenfire.fhs" #-}
import Control.Monad.State (StateT, get, gets, modify, put,
                            execStateT)
{-# LINE 46 "Fenfire.fhs" #-}
import Control.Monad.Trans (lift, liftIO)
{-# LINE 47 "Fenfire.fhs" #-}
import Control.Monad.Writer (Writer, execWriter, tell)
{-# LINE 49 "Fenfire.fhs" #-}
import GtkFixes
{-# LINE 50 "Fenfire.fhs" #-}
import Graphics.UI.Gtk hiding (Color, get, disconnect, fill,
                               actionNew, widgetGetStyle, styleGetForeground, styleGetBackground,
                               styleGetLight, styleGetMiddle, styleGetDark, styleGetText,
                               styleGetBase, styleGetAntiAliasing)
{-# LINE 58 "Fenfire.fhs" #-}
import Graphics.UI.Gtk.ModelView as New
{-# LINE 60 "Fenfire.fhs" #-}
import qualified Network.URI
{-# LINE 62 "Fenfire.fhs" #-}
import System.Directory (canonicalizePath)
{-# LINE 63 "Fenfire.fhs" #-}
import System.Environment (getArgs, getProgName)
{-# LINE 64 "Fenfire.fhs" #-}
import System.Mem.StableName
{-# LINE 65 "Fenfire.fhs" #-}
import System.Random (randomRIO)
 
{-# LINE 67 "Fenfire.fhs" #-}
data ViewSettings = ViewSettings{hiddenProps :: [Node],
                                 maxCenter :: Int}
 
{-# LINE 68 "Fenfire.fhs" #-}
data FenState = FenState{fsGraph :: Graph, fsPath :: Path,
                         fsMark :: Mark, fsFilePath :: FilePath, fsGraphModified :: Bool,
                         fsHasFocus :: Bool, fsView :: Int, fsProperty :: Node,
                         fsProperties :: Set Node, fsUndo :: [(Graph, Path)],
                         fsRedo :: [(Graph, Path)]}
 
{-# LINE 74 "Fenfire.fhs" #-}
fsNode :: FenState -> Node
{-# LINE 75 "Fenfire.fhs" #-}
fsNode (FenState{fsPath = Path node _}) = node
 
{-# LINE 77 "Fenfire.fhs" #-}
fsRotation ::
             (?vs :: ViewSettings, ?graph :: Graph) => FenState -> Rotation
{-# LINE 78 "Fenfire.fhs" #-}
fsRotation = fromPath . fsPath
 
{-# LINE 80 "Fenfire.fhs" #-}
type Views = [(String, View FenState Node)]
 
{-# LINE 82 "Fenfire.fhs" #-}
data Rotation = Rotation Node Int
              deriving (Eq, Show)
 
{-# LINE 84 "Fenfire.fhs" #-}
fromPath ::
           (?vs :: ViewSettings, ?graph :: Graph) => Path -> Rotation
{-# LINE 85 "Fenfire.fhs" #-}
fromPath path@(Path node (Conn _ dir _ : _))
  = fromMaybe (Rotation node 0) $
      do let {-# LINE 86 "Fenfire.fhs" #-}
             c = conns node dir
         i <- Data.List.elemIndex path c
         return $ Rotation node (i - min (length c `div` 2) (maxCenter ?vs))
{-# LINE 89 "Fenfire.fhs" #-}
fromPath (Path node []) = Rotation node 0
 
{-# LINE 91 "Fenfire.fhs" #-}
toPath ::
         (?vs :: ViewSettings, ?graph :: Graph) =>
         Rotation -> Dir -> Maybe Path
{-# LINE 93 "Fenfire.fhs" #-}
toPath (Rotation node r) dir
  = let {-# LINE 93 "Fenfire.fhs" #-}
        c = conns node dir
      in c !? (min (length c `div` 2) (maxCenter ?vs) + r)
{-# LINE 96 "Fenfire.fhs" #-}
toPath' rot@(Rotation node _)
  = head $
      catMaybes [toPath rot Pos, toPath rot Neg, Just $ Path node []]
 
{-# LINE 99 "Fenfire.fhs" #-}
connsCache :: Cache.Cache (StableName Graph, (Node, Dir)) [Path]
{-# LINE 100 "Fenfire.fhs" #-}
connsCache = Cache.newCache 10000
{-# LINE 102 "Fenfire.fhs" #-}
dc_date = URI "dc:date"
 
{-# LINE 104 "Fenfire.fhs" #-}
conns ::
        (?vs :: ViewSettings, ?graph :: Graph) => Node -> Dir -> [Path]
{-# LINE 105 "Fenfire.fhs" #-}
conns node dir
  = Cache.cached (Cache.byAddress ?graph, (node, dir)) connsCache
      result
  where {-# LINE 107 "Fenfire.fhs" #-}
        result
          = map (\ (prop, node') -> Path node [Conn prop dir node']) sorted
        {-# LINE 108 "Fenfire.fhs" #-}
        sorted = Data.List.sortBy cmp' list
        {-# LINE 109 "Fenfire.fhs" #-}
        list
          = [(p, n) | (p, s) <- Map.toList $ getConns ?graph node dir,
             not (p `elem` hiddenProps ?vs), n <- Set.toList s]
        {-# LINE 111 "Fenfire.fhs" #-}
        cmp n1 n2 | p n1 && p n2 = compare (f n1) (f n2)
          where {-# LINE 112 "Fenfire.fhs" #-}
                p n = hasConn ?graph n dc_date Pos
                {-# LINE 112 "Fenfire.fhs" #-}
                f n = getOne ?graph n dc_date Pos
        {-# LINE 113 "Fenfire.fhs" #-}
        cmp n1 n2 = compare (getText n1) (getText n2)
        {-# LINE 114 "Fenfire.fhs" #-}
        cmp' (p1, n1) (p2, n2) = catOrds (cmp p1 p2) (cmp n1 n2)
        {-# LINE 115 "Fenfire.fhs" #-}
        catOrds (EQ) o = o
        {-# LINE 115 "Fenfire.fhs" #-}
        catOrds o _ = o
 
{-# LINE 117 "Fenfire.fhs" #-}
rotate ::
         (?vs :: ViewSettings, ?graph :: Graph) =>
         Rotation -> Int -> Maybe Rotation
{-# LINE 119 "Fenfire.fhs" #-}
rotate (Rotation n r) dir
  = let {-# LINE 119 "Fenfire.fhs" #-}
        rot = Rotation n (r + dir)
      in
      do guard $ any isJust [toPath rot d | d <- [Pos, Neg]]
         return rot
 
{-# LINE 122 "Fenfire.fhs" #-}
move ::
       (?vs :: ViewSettings, ?graph :: Graph) =>
       Rotation -> Dir -> Maybe Rotation
{-# LINE 124 "Fenfire.fhs" #-}
move rot dir
  = do path <- toPath rot dir
       return $ fromPath (rev path)
 
{-# LINE 127 "Fenfire.fhs" #-}
getText :: (?graph :: Graph) => Node -> Maybe String
{-# LINE 128 "Fenfire.fhs" #-}
getText n = fmap f $ getOne ?graph n rdfs_label Pos
  where {-# LINE 129 "Fenfire.fhs" #-}
        f (PlainLiteral s) = s
        {-# LINE 129 "Fenfire.fhs" #-}
        f _ = error "getText argh"
 
{-# LINE 131 "Fenfire.fhs" #-}
getTextOrURI :: (?graph :: Graph) => Node -> String
{-# LINE 132 "Fenfire.fhs" #-}
getTextOrURI n
  = fromMaybe (showNode (graphNamespaces ?graph) n) (getText n)
 
{-# LINE 134 "Fenfire.fhs" #-}
setText :: Node -> String -> Endo Graph
{-# LINE 135 "Fenfire.fhs" #-}
setText n t = update (n, rdfs_label, PlainLiteral t)
 
{-# LINE 137 "Fenfire.fhs" #-}
nodeView :: (?graph :: Graph) => Node -> Vob Node
{-# LINE 138 "Fenfire.fhs" #-}
nodeView n = useFgColor $ multiline False 20 $ getTextOrURI n
 
{-# LINE 140 "Fenfire.fhs" #-}
propView :: (?graph :: Graph) => Node -> Vob Node
{-# LINE 141 "Fenfire.fhs" #-}
propView n
  = (useFadeColor $ fill extents) &
      (pad 5 $ useFgColor $ label $ getTextOrURI n)
 
{-# LINE 146 "Fenfire.fhs" #-}
presentationView :: (?vs :: ViewSettings) => View FenState Node
{-# LINE 147 "Fenfire.fhs" #-}
presentationView state = let ?graph = fsGraph state in result
  where  
        {-# LINE 148 "Fenfire.fhs" #-}
        result :: (?graph :: Graph) => Vob Node
        {-# LINE 149 "Fenfire.fhs" #-}
        result = cursor & vob
          where {-# LINE 150 "Fenfire.fhs" #-}
                node = fsNode state
                {-# LINE 151 "Fenfire.fhs" #-}
                children = map getPos (conns node Pos)
                {-# LINE 152 "Fenfire.fhs" #-}
                selected = fmap (getSide Pos) (toPath (fsRotation state) Pos)
                {-# LINE 153 "Fenfire.fhs" #-}
                f sc n
                  = keyVob n $ useFgColor $ pad 5 $ scaleVob sc $ multiline True 70 $
                      getTextOrURI n
                {-# LINE 155 "Fenfire.fhs" #-}
                cursor
                  = flip (maybe mempty) selected $
                      \ n ->
                        showAtKey n $ keyVob (PlainLiteral "CURSOR") $ rectBox mempty
                {-# LINE 157 "Fenfire.fhs" #-}
                space = changeSize (const (0, 20)) mempty
                {-# LINE 158 "Fenfire.fhs" #-}
                vob
                  = pad 30 $ vbox $ intersperse space $ f 3 node : map (f 2) children
 
{-# LINE 162 "Fenfire.fhs" #-}
tryMove ::
          (?vs :: ViewSettings, ?graph :: Graph) =>
          Rotation -> Dir -> Maybe Rotation
{-# LINE 164 "Fenfire.fhs" #-}
tryMove rot@(Rotation n r) dir = maybe rot' Just (move rot dir)
  where {-# LINE 165 "Fenfire.fhs" #-}
        rot'
          | r == nearest = Nothing
          | otherwise = Just $ Rotation n nearest
        {-# LINE 167 "Fenfire.fhs" #-}
        nearest
          | r > 0 = len - 1 - min (len `div` 2) (maxCenter ?vs)
          | otherwise = 0 - min (len `div` 2) (maxCenter ?vs)
        {-# LINE 169 "Fenfire.fhs" #-}
        len = (length $ conns n dir)
 
{-# LINE 171 "Fenfire.fhs" #-}
type URIMaker = (String, IORef Integer)
 
{-# LINE 173 "Fenfire.fhs" #-}
newURIMaker :: IO URIMaker
{-# LINE 174 "Fenfire.fhs" #-}
newURIMaker
  = do rand <- sequence [randomRIO (0, 63) | _ <- [1 .. 27 :: Int]]
       let {-# LINE 175 "Fenfire.fhs" #-}
           chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "+-"
       ref <- newIORef 1
       return ("urn:urn-5:" ++ map (chars !!) rand, ref)
 
{-# LINE 179 "Fenfire.fhs" #-}
newURI :: (?uriMaker :: URIMaker) => IO Node
{-# LINE 180 "Fenfire.fhs" #-}
newURI
  = do let {-# LINE 180 "Fenfire.fhs" #-}
           (base, ref) = ?uriMaker
       i <- readIORef ref
       writeIORef ref (i + 1)
       return $ URI (base ++ ":_" ++ show i)
 
{-# LINE 184 "Fenfire.fhs" #-}
newNode ::
          (?vs :: ViewSettings, ?uriMaker :: URIMaker) =>
          Dir -> Node -> EndoM IO (Graph, Rotation)
{-# LINE 186 "Fenfire.fhs" #-}
newNode dir prop (graph, Rotation node _)
  = do node' <- newURI
       let ?graph =
             insert (triple dir (node, prop, node')) $
               insert (node', rdfs_label, PlainLiteral "") graph
         in
         return (?graph, fromPath (Path node' [Conn prop (rev dir) node]))
 
{-# LINE 192 "Fenfire.fhs" #-}
connect :: (?vs :: ViewSettings) => Dir -> Endo FenState
{-# LINE 193 "Fenfire.fhs" #-}
connect _ state | Set.null (fsMark state) = state
{-# LINE 194 "Fenfire.fhs" #-}
connect dir state
  = let {-# LINE 195 "Fenfire.fhs" #-}
        nodes = Set.toList (fsMark state)
        {-# LINE 195 "Fenfire.fhs" #-}
        prop = fsProperty state
      in
      let ?graph =
            foldr (\ n -> insert $ triple dir (fsNode state, prop, n))
              (fsGraph state)
              nodes
        in
        state{fsPath = (Path (fsNode state) [Conn prop dir (head nodes)]),
              fsGraph = ?graph, fsMark = Set.empty, fsGraphModified = True,
              fsUndo = (fsGraph state, fsPath state) : fsUndo state, fsRedo = []}
 
{-# LINE 203 "Fenfire.fhs" #-}
disconnect :: (?vs :: ViewSettings) => Dir -> Endo FenState
{-# LINE 204 "Fenfire.fhs" #-}
disconnect dir state
  = let ?graph = fsGraph state in
      let {-# LINE 205 "Fenfire.fhs" #-}
          rot = fsRotation state
        in
        case toPath rot dir of
            Nothing -> state
            Just path -> let {-# LINE 209 "Fenfire.fhs" #-}
                             path'
                               = fromMaybe (Path (fsNode state) []) $
                                   msum
                                     [flip toPath xdir =<< rotate rot ydir | xdir <- [Neg, Pos],
                                      ydir <- [- 1, 1]]
                             {-# LINE 212 "Fenfire.fhs" #-}
                             triples = pathToTriples path
                             {-# LINE 213 "Fenfire.fhs" #-}
                             graph' = foldr delete (fsGraph state) triples
                           in
                           state{fsGraph = graph', fsPath = path', fsGraphModified = True,
                                 fsUndo = (fsGraph state, fsPath state) : fsUndo state, fsRedo = []}
 
{-# LINE 219 "Fenfire.fhs" #-}
type Mark = Set Node
 
{-# LINE 221 "Fenfire.fhs" #-}
toggleMark :: Node -> Endo Mark
{-# LINE 222 "Fenfire.fhs" #-}
toggleMark n mark
  | n `Set.member` mark = Set.delete n mark
  | otherwise = Set.insert n mark
 
{-# LINE 225 "Fenfire.fhs" #-}
newGraph :: (?uriMaker :: URIMaker) => IO (Graph, Path)
{-# LINE 226 "Fenfire.fhs" #-}
newGraph
  = do home <- newURI
       let {-# LINE 228 "Fenfire.fhs" #-}
           graph = listToGraph [(home, rdfs_label, PlainLiteral "")]
       return (graph, Path home [])
 
{-# LINE 231 "Fenfire.fhs" #-}
findStartPath :: (?vs :: ViewSettings) => Node -> Graph -> Path
{-# LINE 232 "Fenfire.fhs" #-}
findStartPath self g = let ?graph = g in result
  where  
        {-# LINE 233 "Fenfire.fhs" #-}
        result :: (?graph :: Graph) => Path
        {-# LINE 234 "Fenfire.fhs" #-}
        result = head $ catMaybes $ startNode : topic : triples
          where {-# LINE 236 "Fenfire.fhs" #-}
                startNode = fmap getRot' $ getTriple self ffv_startNode
                {-# LINE 237 "Fenfire.fhs" #-}
                topic = fmap getRot' $ getTriple self foaf_primaryTopic
                {-# LINE 238 "Fenfire.fhs" #-}
                triples = map (Just . getRot) $ graphToList g
                {-# LINE 240 "Fenfire.fhs" #-}
                getTriple s p = fmap (\ o -> (s, p, o)) $ getOne g s p Pos
                {-# LINE 241 "Fenfire.fhs" #-}
                getRot (s, p, o) = Path s [Conn p Pos o]
                {-# LINE 242 "Fenfire.fhs" #-}
                getRot' (s, p, o) = Path o [Conn p Neg s]
                {-# LINE 244 "Fenfire.fhs" #-}
                ffv_startNode = URI "http://fenfire.org/rdf-v/2003/05/ff#startNode"
                {-# LINE 245 "Fenfire.fhs" #-}
                foaf_primaryTopic = URI "http://xmlns.com/foaf/0.1/primaryTopic"
 
{-# LINE 247 "Fenfire.fhs" #-}
containsInfoTriples ::
                      (?vs :: ViewSettings) => Node -> Graph -> [Triple]
{-# LINE 248 "Fenfire.fhs" #-}
containsInfoTriples s g = [(s, p, o) | o <- os, o /= s]
  where {-# LINE 249 "Fenfire.fhs" #-}
        p = URI "ex:containsInformationAbout"
        {-# LINE 250 "Fenfire.fhs" #-}
        triples = graphToList g
        {-# LINE 251 "Fenfire.fhs" #-}
        [subjects, objects] = for [subject, object] $ \ f -> map f triples
        {-# LINE 252 "Fenfire.fhs" #-}
        os
          = Set.toAscList $ foldr Set.delete (Set.fromList subjects) objects
 
{-# LINE 254 "Fenfire.fhs" #-}
loadGraph :: FilePath -> IO Graph
{-# LINE 255 "Fenfire.fhs" #-}
loadGraph fileName
  = do let {-# LINE 258 "Fenfire.fhs" #-}
           convert (s, p, o) = (f s, f p, f o)
           {-# LINE 259 "Fenfire.fhs" #-}
           f (Raptor.Uri s) = URI s
           {-# LINE 260 "Fenfire.fhs" #-}
           f (Raptor.Literal s) = PlainLiteral s
           {-# LINE 261 "Fenfire.fhs" #-}
           f (Raptor.Blank s) = URI $ "blank:" ++ s
       (raptorTriples, namespaces) <- if
                                        Data.List.isPrefixOf "http:" fileName then
                                        Raptor.uriToTriples fileName Nothing else
                                        Raptor.filenameToTriples fileName Nothing
       triples <- return $ map convert raptorTriples
       return $
         foldr (uncurry addNamespace) (listToGraph triples) namespaces
 
{-# LINE 268 "Fenfire.fhs" #-}
saveGraph :: Graph -> FilePath -> IO ()
{-# LINE 269 "Fenfire.fhs" #-}
saveGraph graph fileName
  = do uri <- liftM (fromJust . Network.URI.parseURI)
                (Raptor.filenameToURI fileName)
       let {-# LINE 273 "Fenfire.fhs" #-}
           convert (s, p, o) = (f s, f p, f o)
           {-# LINE 274 "Fenfire.fhs" #-}
           f (URI s)
             = Raptor.Uri $ fromMaybe s $
                 do u <- Network.URI.parseURI s
                    return $ show $ Network.URI.relativeFrom u uri
           {-# LINE 277 "Fenfire.fhs" #-}
           f (PlainLiteral s) = Raptor.Literal s
           {-# LINE 278 "Fenfire.fhs" #-}
           triples = graphToList graph
           {-# LINE 279 "Fenfire.fhs" #-}
           namespaces = Map.toAscList $ graphNamespaces graph
       Raptor.triplesToFilename (map convert triples) namespaces fileName
       putStrLn $ "Saved: " ++ fileName
 
{-# LINE 283 "Fenfire.fhs" #-}
newState :: Graph -> Path -> FilePath -> Bool -> FenState
{-# LINE 284 "Fenfire.fhs" #-}
newState graph path fp focus
  = FenState graph path Set.empty fp False focus 0 rdfs_seeAlso ps []
      []
  where {-# LINE 286 "Fenfire.fhs" #-}
        ps
          = Set.insert rdfs_seeAlso $ Set.fromList $ map predicate $ filter f
              $ graphToList graph
        {-# LINE 288 "Fenfire.fhs" #-}
        f (_, _, URI _) = True
        {-# LINE 289 "Fenfire.fhs" #-}
        f _ = False
 
{-# LINE 291 "Fenfire.fhs" #-}
stateReplaceNode :: Node -> Node -> Endo FenState
{-# LINE 292 "Fenfire.fhs" #-}
stateReplaceNode m n s@(FenState{fsPath = Path node cs})
  = FenState{fsGraph = replaceNode m n (fsGraph s),
             fsPath =
               Path (f node) (map (\ (Conn p d n') -> Conn (f p) d (f n')) cs),
             fsMark =
               if m `Set.member` fsMark s then
                 Set.insert n $ Set.delete m $ fsMark s else fsMark s,
             fsProperty = f (fsProperty s),
             fsProperties = Set.map f (fsProperties s), fsGraphModified = True,
             fsFilePath = fsFilePath s, fsHasFocus = fsHasFocus s,
             fsView = fsView s, fsUndo = (fsGraph s, fsPath s) : fsUndo s,
             fsRedo = []}
  where {-# LINE 301 "Fenfire.fhs" #-}
        f x = if x == m then n else x
