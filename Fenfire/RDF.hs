{-# OPTIONS_GHC -fglasgow-exts 
        -fallow-overlapping-instances -fallow-undecidable-instances #-}
module Fenfire.RDF where

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

import Fenfire.Cache
import Fenfire.Utils
import qualified Fenfire.Raptor as Raptor

import Control.Monad.Writer (Writer, WriterT, MonadWriter, tell, forM_,
                             runWriter, runWriterT)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (State, get, put, modify, runState)

import Data.Generics hiding ((:*:))
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust, isJust, listToMaybe)
import qualified Numeric
import Data.Set (Set)
import qualified Data.Set as Set

import HList

import Network.URI hiding (query)

data Node = IRI { nodeStr :: String }
          | BNode { bnodeGraph :: String, nodeStr :: String } 
          | Literal { nodeStr :: String, literalTag :: LiteralTag }
                                                    deriving (Eq, Ord, Show, Read, Typeable, Data)
data LiteralTag = Plain | Lang String | Type String deriving (Eq, Ord, Show, Read, Typeable, Data)
data Dir  = Pos | Neg                               deriving (Eq, Ord, Show)

{-instance Show Node where
    show = showNode defaultNamespaces-}
    

-- This is unfortunately something of a pun because I can't find a good
-- name for it: A 'coin' is something that has two sides...
class CoinClass c a | c -> a where
    getSide :: Dir -> c -> a
    
    getNeg :: c -> a; getNeg = getSide Neg
    getPos :: c -> a; getPos = getSide Pos
    
type Coin a = (a,a)

instance CoinClass (Coin a) a where
    getSide Neg = fst
    getSide Pos = snd


type Triple     = (Node, Node, Node)
type Namespaces = Map String String
data Graph      = Graph {
    graphNamespaces :: Namespaces, graphURI :: String,
    graphSides :: Coin (Map Node (Map Node (Set Node))),
    graphRealTriples :: Set Triple } deriving (Show, Read, Eq, Data, Typeable)
    
data Conn = Conn { connProp :: Node, connDir :: Dir, connTarget :: Node }
            deriving (Eq, Ord, Show)
data Path = Path Node [Conn] deriving (Eq, Ord, Show)

pathToTriples :: Path -> [Triple]
pathToTriples (Path _ [])                 = []
pathToTriples (Path n (Conn p d n' : cs)) = 
    triple d (n,p,n') : pathToTriples (Path n' cs)

instance CoinClass Graph (Map Node (Map Node (Set Node))) where
    getSide dir graph = getSide dir $ graphSides graph
    
instance CoinClass Triple Node where
    getSide Neg = subject
    getSide Pos = object
    
instance CoinClass Path Node where
    getSide Neg (Path node _)     = node
    getSide Pos (Path node [])    = node
    getSide Pos (Path _    conns) = connTarget (last conns)

class Reversible r where
    rev :: Endo r
    
instance Reversible Dir where
    rev Neg = Pos; rev Pos = Neg
    
instance Reversible Path where
    rev (Path node conns) = foldr f (Path node []) (reverse conns) where
        f (Conn p d n') (Path n cs) = Path n' (Conn p (rev d) n : cs)
    
instance Hashable Node where
    hash (IRI s) = hash s
    hash (BNode g s) = hash (g,s)
    hash (Literal s Plain) = hash s
    hash (Literal s (Lang l)) = hash (s,l)
    hash (Literal s (Type t)) = hash (s,t)
    
instance Hashable Dir where
    hash Pos = 0
    hash Neg = 1

rdf          =     "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdf_type     = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
rdf_List     = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"
rdf_first    = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
rdf_next     = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#next"
rdf_nil      = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"

rdfs         =     "http://www.w3.org/2000/01/rdf-schema#"
rdfs_label   = IRI "http://www.w3.org/2000/01/rdf-schema#label"
rdfs_seeAlso = IRI "http://www.w3.org/2000/01/rdf-schema#seeAlso"

defaultNamespaces = Map.fromList [("rdf", rdf), ("rdfs", rdfs)]

showNode :: Namespaces -> Node -> String
showNode ns (IRI uri) = f (Map.toAscList ns) where
    f ((short, long):xs) | take (length long) uri == long =
                               short ++ ":" ++ drop (length long) uri
                         | otherwise = f xs
    f [] = "<" ++ turtle_escaped '>' uri ++ ">"
showNode _  (BNode graph id') = "bnode[" ++ id' ++ " @ " ++ graph ++ "]"
showNode ns (Literal lit tag) = "\"" ++ turtle_escaped '"' lit ++ "\"" ++
    case tag of Plain -> ""; Lang lang  -> "@" ++ lang;
                             Type type' -> "^^" ++ showNode ns (IRI type')

turtle_escaped :: Char -> String -> String
turtle_escaped _        [] = []
turtle_escaped c ('\\':xs) = '\\':'\\':turtle_escaped c xs
turtle_escaped c    (x:xs) | c == x 
                           = '\\':   c:turtle_escaped c xs
turtle_escaped c ('\n':xs) = '\\': 'n':turtle_escaped c xs
turtle_escaped c ('\r':xs) = '\\': 'r':turtle_escaped c xs
turtle_escaped c ('\t':xs) = '\\': 't':turtle_escaped c xs
turtle_escaped c    (x:xs) | i <- fromEnum x, i < 0x20 || i == 0x5C 
                           = '\\':'u':((if i<16 then ('0':) else id) $
                                       Numeric.showHex i (turtle_escaped c xs))
turtle_escaped c (   x:xs) =         x:turtle_escaped c xs

subject :: Triple -> Node
subject (s,_,_) = s

predicate :: Triple -> Node
predicate (_,p,_) = p

object :: Triple -> Node
object (_,_,o) = o

hasConn :: Graph -> Node -> Node -> Dir -> Bool
hasConn g node prop dir = isJust $ do m <- Map.lookup node (getSide dir g)
                                      s <- Map.lookup prop m
                                      if Set.null s then Nothing else Just ()

getOne :: Graph -> Node -> Node -> Dir -> Maybe Node
getOne g node prop dir = if null nodes then Nothing else Just $ head nodes
    where nodes = Set.toList (getAll g node prop dir)
    
getAll :: Graph -> Node -> Node -> Dir -> Set Node
getAll g node prop dir = 
    Map.findWithDefault Set.empty prop $ getConns g node dir

getConns :: Graph -> Node -> Dir -> Map Node (Set Node)
getConns g node dir = Map.findWithDefault Map.empty node $ getSide dir g

emptyGraph :: Graph
emptyGraph = Graph defaultNamespaces "" (Map.empty, Map.empty) Set.empty

listToGraph :: [Triple] -> Graph
listToGraph = foldr insert emptyGraph

graphToList :: Graph -> [Triple]
graphToList = Set.toAscList . graphRealTriples

mergeGraphs :: Op Graph
mergeGraphs real virtual = foldr insertVirtual real (graphToList virtual)

relativizeURI :: String -> Endo String
relativizeURI baseURI s = fromMaybe s $ do
    u <- Network.URI.parseURIReference s; bu <- Network.URI.parseURI baseURI
    return $ show $ Network.URI.relativeFrom u bu
    
absolutizeURI :: String -> Endo String
absolutizeURI baseURI s = fromMaybe s $ do
    u <- Network.URI.parseURIReference s; bu <- Network.URI.parseURI baseURI
    u' <- Network.URI.relativeTo u bu; return $ show u'
    
relativizeNode :: String -> Endo Node
relativizeNode baseURI (IRI s) = IRI $ relativizeURI baseURI s
relativizeNode baseURI (BNode gid s) = BNode (relativizeURI baseURI gid) s
relativizeNode _ node = node

absolutizeNode :: String -> Endo Node
absolutizeNode baseURI (IRI s) = IRI $ absolutizeURI baseURI s
absolutizeNode baseURI (BNode gid s) = BNode (absolutizeURI baseURI gid) s
absolutizeNode _ node = node

changeBaseURI :: String -> String -> Endo Node
changeBaseURI oldBase newBase = absolutizeNode newBase . relativizeNode oldBase

setGraphURI :: String -> Endo Graph
setGraphURI uri g = everywhere (mkT $ changeBaseURI (graphURI g) uri) $ 
                    g { graphURI = uri }

insert :: Triple -> Endo Graph
insert t graph@(Graph { graphRealTriples=ts }) =
    insertVirtual t $ graph { graphRealTriples = Set.insert t ts }

insertVirtual :: Triple -> Endo Graph
insertVirtual (s,p,o) graph@(Graph { graphSides = (neg, pos) }) =
    graph { graphSides = (ins o p s neg, ins s p o pos) } where
    ins a b c = Map.alter (Just . Map.alter (Just . Set.insert c . fromMaybe Set.empty) b . fromMaybe Map.empty) a   -- Gack!!! Need to make more readable
    
delete :: Triple -> Endo Graph
delete (s,p,o) (Graph ns uri (neg, pos) triples) = 
    Graph ns uri (del o p s neg, del s p o pos) $ 
        Set.delete (s,p,o) triples where
    del a b c = Map.adjust (Map.adjust (Set.delete c) b) a
    
deleteAll :: Node -> Node -> Endo Graph
deleteAll s p g = dels s p os g where
    dels s' p' (o':os') g' = dels s' p' os' (delete (s',p',o') g')
    dels _  _  []       g' = g'
    os = Set.toList $ getAll g s p Pos
    
update :: Triple -> Endo Graph
update (s,p,o) g = insert (s,p,o) $ deleteAll s p g

replaceNode :: Node -> Node -> Endo Graph
replaceNode m n graph = Set.fold f graph (graphRealTriples graph) where
    f (s,p,o) = insert (r s, r p, r o) . delete (s,p,o)
    r x = if x == m then n else x

addNamespace :: String -> String -> Endo Graph
addNamespace prefix uri g =
    g { graphNamespaces = Map.insert prefix uri $ graphNamespaces g }
    
triple :: Dir -> (Node,Node,Node) -> Triple
triple Pos (s,p,o) = (s,p,o)
triple Neg (o,p,s) = (s,p,o)

mul :: Num a => Dir -> a -> a
mul Pos = id
mul Neg = negate



--------------------------------------------------------------------------
-- FromRDF and ToRDF
--------------------------------------------------------------------------

updateRDF :: (FromRDF a, ToRDF a) => Endo a -> Node -> Endo Graph
updateRDF f node graph = graph' where
    (x, ts) = runFromRDF $ readRDF graph node
    (_, ts') = runToRDF (graphURI graph) $ toRDF (f x)
    graph' = flip (foldr insert) (Set.toAscList ts') $
             foldr delete graph (Set.toAscList ts)

type FromRdfM = Writer (Set Triple)

class FromRDF a where -- minimal impl: either fromRDF or readRDF
    fromRDF :: Graph -> Node -> a
    fromRDF g n = fst $ runFromRDF (readRDF g n)
    
    -- Return a value read from a graph and the triples
    -- that were used in getting that value. When updating
    -- a value in a graph, these triples will be replaced
    -- by the triples generated by toRDF.
    readRDF :: Graph -> Node -> FromRdfM a
    readRDF g n = return $ fromRDF g n
    
runFromRDF :: FromRdfM a -> (a, Set Triple)
runFromRDF = runWriter

type ToRdfM = WriterT (Set Triple) (State (String, Int))

runToRDF :: String -> ToRdfM a -> (a, Set Triple)
runToRDF gid m = fst $ runState (runWriterT m) (gid, 1)
    
newBNode :: ToRdfM Node
newBNode = do (gid, i) <- get; put (gid, i+1); return $ BNode gid (show i)

tellTs :: (MonadWriter (Set Triple) m) => [Triple] -> m ()
tellTs = tell . Set.fromList

class ToRDF a where
    toRDF :: a -> ToRdfM Node
    
instance FromRDF a => FromRDF [a] where
    readRDF g l | l == rdf_nil = return []
                | otherwise    = do
        let first = fromJust $ getOne g l rdf_first Pos
            rest  = fromJust $ getOne g l rdf_next Pos
        tellTs [ (l, rdf_first, first), (l, rdf_next, rest) ]
        x  <- readRDF g first
        xs <- readRDF g rest
        return (x:xs)
            
instance ToRDF a => ToRDF [a] where
    toRDF []     = return rdf_nil
    toRDF (x:xs) = do l <- newBNode; first <- toRDF x; next <- toRDF xs
                      tellTs [ (l, rdf_first, first)
                             , (l, rdf_next, next) ]
                      return l
                      
instance FromRDF String where
    fromRDF _ (Literal s _) = s
    fromRDF _ n = error $ "Fenfire.RDF.fromRDF(String): can only convert literals, not " ++ show n
    
instance ToRDF String where
    toRDF s = return (Literal s Plain)
    
instance FromRDF Node where
    fromRDF _ n = n
    
instance ToRDF Node where
    toRDF n = return n


--------------------------------------------------------------------------
-- Raptor interface
--------------------------------------------------------------------------

raptorToGraph :: [Raptor.Triple] -> [(String, String)] -> String -> Graph
raptorToGraph raptorTriples namespaces graphURI' = setGraphURI graphURI' $
        foldr (uncurry addNamespace) (listToGraph triples) namespaces where
    triples = map convert raptorTriples
    convert (s,p,o) = (f s, f p, f o)
    f (Raptor.Uri s) = IRI s
    f (Raptor.Literal s) = Literal s Plain
    f (Raptor.Blank s) = BNode graphURI' s
    
graphToRaptor :: Graph -> ([Raptor.Triple], [(String, String)])
graphToRaptor graph = (map convert triples, namespaces) where
    graphURI' = fromJust $ Network.URI.parseURI (graphURI graph)
    convert (s,p,o) = (f s, f p, f o)
    f (IRI s) = Raptor.Uri $ fromMaybe s $ do
                    u <- Network.URI.parseURI s
                    return $ show $ Network.URI.relativeFrom u graphURI'
    f (Literal s _) = Raptor.Literal s
    f (BNode g s) = if g == (graphURI graph) then Raptor.Blank s
                    else error "XXX Cannot save bnode from different graph"
    triples = graphToList graph
    namespaces = Map.toAscList $ graphNamespaces graph


--------------------------------------------------------------------------
-- Writing Turtle
--------------------------------------------------------------------------

writeTurtle :: MonadWriter String m => String -> Graph -> m ()
writeTurtle nl graph = do let graph' = listToGraph $ graphToList graph
                              nss = graphNamespaces graph
                          writeTurtleNamespaces nl nss
                          writeTurtleStmts nl nss $ getPos graph'
                       
writeTurtleNamespaces nl nss = forM_ (Map.toAscList nss) $ \(prefix,iri) -> do
    tell "@prefix "; tell prefix; tell ": <"; tell iri; tell ">."; tell nl
    
writeTurtleStmts nl nss stmts = forM_ (Map.toAscList stmts) $ \(s,pos) -> do
    tell nl; writeTurtleNode nss s; tell nl
    sequence_ $ intersperse (tell ";" >> tell nl) $
        map (writeTurtlePred nl nss) $ Map.toAscList pos
    tell "."; tell nl
    
writeTurtlePred nl nss (p, os) = do
    tell "  "; writeTurtleNode nss p; tell nl
    sequence_ $ intersperse (tell "," >> tell nl) $
        map (writeTurtleObj nss) $ Set.toAscList os
    
writeTurtleObj nss o = do tell "    "; writeTurtleNode nss o

writeTurtleNode nss node = tell $ showNode nss node


--------------------------------------------------------------------------
-- Reimplementation, using HList; this will become the default
-- once it's finished
--------------------------------------------------------------------------

data Any = Any deriving (Eq, Ord, Show)
data X = X     deriving (Eq, Ord, Show)

-- Examples:
-- query (x, rdf_type, X)   :: Graph -> Set Node
-- query (x, rdf_type, X)   :: Graph -> Maybe Node
-- query (x, rdf_type, Any) :: Graph -> Set Triple
-- query (x, rdf_type, Any) :: Graph -> Maybe Triple
-- There are lots of other combinations.
class Show pattern => Pattern pattern result where
    query :: pattern -> Graph' -> result
    
type Quad = (Node, Node, Node, Node)

quad2triple :: Quad -> Triple
quad2triple (s,p,o,_) = (s,p,o)

quadGraph :: Quad -> Node
quadGraph (_,_,_,g) = g

data Graph' = Graph' { defaultGraph :: String
                   , namespaces' :: Map String String
                   , graphViews :: Map (Node, Node, Node, Node) (Set Quad)
                               :*: Map (Node, Node, Node, Any)  (Set Quad)
                               :*: Map (Node, Node, Any,  Node) (Set Quad)
                               :*: Map (Node, Node, Any,  Any)  (Set Quad)
                               :*: Map (Node, Any,  Node, Node) (Set Quad)
                               :*: Map (Node, Any,  Node, Any)  (Set Quad)
                               :*: Map (Node, Any,  Any,  Node) (Set Quad)
                               :*: Map (Node, Any,  Any,  Any)  (Set Quad)
                               :*: Map (Any,  Node, Node, Node) (Set Quad)
                               :*: Map (Any,  Node, Node, Any)  (Set Quad)
                               :*: Map (Any,  Node, Any,  Node) (Set Quad)
                               :*: Map (Any,  Node, Any,  Any)  (Set Quad)
                               :*: Map (Any,  Any,  Node, Node) (Set Quad)
                               :*: Map (Any,  Any,  Node, Any)  (Set Quad)
                               :*: Map (Any,  Any,  Any,  Node) (Set Quad)
                               :*: Map (Any,  Any,  Any,  Any)  (Set Quad)
                               :*: HNil -- use some simple TH for this? :-)
                   }

simpleQuery pattern g = hOccursFst (graphViews g) Map.! pattern

-- We need an instance for each of these because the code GHC *generates*
-- for each of these is different, even though *we* write the same thing
-- for each. Again, we should use Template Haskell to generate these.
instance Pattern (Node, Node, Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Node, Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Node, Node, Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Node, Any,  Any)  (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Any,  Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Any,  Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Any,  Any)  (Set Quad) where query = simpleQuery

instance (Show p, Show o, Show g, Pattern (Any,p,o,g) (Set Quad)) => 
         Pattern (X,p,o,g) (Set Node) where
    query (X,p,o,g) = Set.map (subject . quad2triple) . query (Any,p,o,g)
instance (Show s, Show o, Show g, Pattern (s,Any,o,g) (Set Quad)) => 
         Pattern (s,X,o,g) (Set Node) where
    query (s,X,o,g) = Set.map (predicate . quad2triple) . query (s,Any,o,g)
instance (Show s, Show p, Show g, Pattern (s,p,Any,g) (Set Quad)) => 
         Pattern (s,p,X,g) (Set Node) where
    query (s,p,X,g) = Set.map (object . quad2triple) . query (s,p,Any,g)
instance (Show s, Show p, Show o, Pattern (s,p,o,Any) (Set Quad)) => 
         Pattern (s,p,o,X) (Set Node) where
    query (s,p,o,X) = Set.map quadGraph . query (s,p,o,Any)
    
instance (Show s, Show p, Show o, Pattern (s,p,o,Any) r) => 
         Pattern (s,p,o) r where
    query (s,p,o) = query (s,p,o,Any)

instance Pattern pat (Set Quad) => Pattern pat (Set Triple) where
    query pat = Set.map quad2triple . query pat

instance (Pattern pat (Set r), MonadPlus m) => Pattern pat (m r) where
    query pat = returnEach . Set.toList . query pat

{- less generic versions, in case the above doesn't work out for some reason:
instance (Pattern pat (Set r), MonadPlus m) => Pattern pat (m r) where
    query pat = Set.toList . query pat

instance Pattern pat (Set r) => Pattern pat (Maybe r) where
    query pat g = let s = query pat g in toMaybe (Set.null s) (Set.findMin s)
-}
    
instance Pattern pat (Set r) => Pattern pat r where
    query pat g = let s = query pat g in
                  if Set.null s then error $ "Pattern not found: " ++ show pat
                                else Set.findMin s

instance Pattern pat (Set Quad) => Pattern pat Bool where
    query pat = not . Set.null . (id :: Endo (Set Quad)) . query pat
