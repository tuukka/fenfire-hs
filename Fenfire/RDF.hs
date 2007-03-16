{-# OPTIONS_GHC -fglasgow-exts #-}
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
                             runWriter)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (State, get, put, modify)

import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust, isJust)
import qualified Numeric
import Data.Set (Set)
import qualified Data.Set as Set

import Network.URI

data Node = IRI { nodeStr :: String }
          | BNode { bnodeGraph :: String, nodeStr :: String } 
          | Literal { nodeStr :: String, literalTag :: LiteralTag }
                                                    deriving (Eq, Ord)
data LiteralTag = Plain | Lang String | Type String deriving (Eq, Ord, Show)
data Dir  = Pos | Neg                               deriving (Eq, Ord, Show)

instance Show Node where
    show = showNode defaultNamespaces
    

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
    graphNamespaces :: Namespaces,
    graphSides :: Coin (Map Node (Map Node (Set Node))),
    graphRealTriples :: Set Triple } deriving (Show, Eq)
    
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
emptyGraph = Graph defaultNamespaces (Map.empty, Map.empty) Set.empty

listToGraph :: [Triple] -> Graph
listToGraph = foldr insert emptyGraph

graphToList :: Graph -> [Triple]
graphToList = Set.toAscList . graphRealTriples

mergeGraphs :: Op Graph
mergeGraphs real virtual = foldr insertVirtual real (graphToList virtual)

insert :: Triple -> Endo Graph
insert t graph@(Graph { graphRealTriples=ts }) =
    insertVirtual t $ graph { graphRealTriples = Set.insert t ts }

insertVirtual :: Triple -> Endo Graph
insertVirtual (s,p,o) graph@(Graph { graphSides = (neg, pos) }) =
    graph { graphSides = (ins o p s neg, ins s p o pos) } where
    ins a b c = Map.alter (Just . Map.alter (Just . Set.insert c . fromMaybe Set.empty) b . fromMaybe Map.empty) a   -- Gack!!! Need to make more readable
    
delete :: Triple -> Endo Graph
delete (s,p,o) (Graph ns (neg, pos) triples) = 
    Graph ns (del o p s neg, del s p o pos) $ 
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

type FromRdfM = Writer (Set Triple)

class FromRDF a where
    fromRDF :: Graph -> Node -> a
    fromRDF g n = fst $ runWriter (readRDF g n)
    
    -- Return a value read from a graph and the triples
    -- that were used in getting that value. When updating
    -- a value in a graph, these triples will be replaced
    -- by the triples generated by toRDF.
    readRDF :: Graph -> Node -> FromRdfM a
    
type ToRdfM = WriterT (Set Triple) (State (String, Int))
    
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
                      tellTs [ (l, rdf_type, rdf_List)
                             , (l, rdf_first, first)
                             , (l, rdf_next, next) ]
                      return l


--------------------------------------------------------------------------
-- Raptor interface
--------------------------------------------------------------------------

raptorToGraph :: [Raptor.Triple] -> [(String, String)] -> String -> Graph
raptorToGraph raptorTriples namespaces graphURI =
        foldr (uncurry addNamespace) (listToGraph triples) namespaces where
    triples = map convert raptorTriples
    convert (s,p,o) = (f s, f p, f o)
    f (Raptor.Uri s) = IRI s
    f (Raptor.Literal s) = Literal s Plain
    f (Raptor.Blank s) = BNode graphURI s
    
graphToRaptor :: Graph -> String -> ([Raptor.Triple], [(String, String)])
graphToRaptor graph graphURI = (map convert triples, namespaces) where
    graphURI' = fromJust $ Network.URI.parseURI graphURI
    convert (s,p,o) = (f s, f p, f o)
    f (IRI s) = Raptor.Uri $ fromMaybe s $ do
                    u <- Network.URI.parseURI s
                    return $ show $ Network.URI.relativeFrom u graphURI'
    f (Literal s _) = Raptor.Literal s
    f (BNode g s) = if g == graphURI then Raptor.Blank s
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
