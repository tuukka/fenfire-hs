module RDF where

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

import Cache
import Utils

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

data Node = URI String | PlainLiteral String    deriving (Eq, Ord)
data Dir  = Pos | Neg                           deriving (Eq, Ord, Show)

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
    hash (URI s) = hash s
    hash (PlainLiteral s) = hash s
    
instance Hashable Dir where
    hash Pos = 0
    hash Neg = 1

rdfs         =     "http://www.w3.org/2000/01/rdf-schema#"
rdfs_label   = URI "http://www.w3.org/2000/01/rdf-schema#label"
rdfs_seeAlso = URI "http://www.w3.org/2000/01/rdf-schema#seeAlso"

defaultNamespaces = Map.fromList [("rdfs", rdfs)]

showNode :: Namespaces -> Node -> String
showNode ns (URI uri) = f (Map.toAscList ns) where
    f ((short, long):xs) | take (length long) uri == long =
                               short ++ ":" ++ drop (length long) uri
                         | otherwise = f xs
    f [] = "<" ++ uri ++ ">"
showNode _  (PlainLiteral lit) = show lit

subject :: Triple -> Node
subject (s,_,_) = s

predicate :: Triple -> Node
predicate (_,p,_) = p

object :: Triple -> Node
object (_,_,o) = o

hasConn :: Graph -> Node -> Node -> Dir -> Bool
hasConn g node prop dir = isJust $ do m <- Map.lookup node (getSide dir g)
                                      Map.lookup prop m

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

addNamespace :: String -> String -> Endo Graph
addNamespace prefix uri g =
    g { graphNamespaces = Map.insert prefix uri $ graphNamespaces g }
    
triple :: Dir -> (Node,Node,Node) -> Triple
triple Pos (s,p,o) = (s,p,o)
triple Neg (o,p,s) = (s,p,o)

fromNode :: Node -> String
fromNode (URI uri)        = uri
fromNode (PlainLiteral s) = s

mul :: Num a => Dir -> a -> a
mul Pos = id
mul Neg = negate
