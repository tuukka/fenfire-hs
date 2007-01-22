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

data Node = URI String | PlainLiteral String    deriving (Eq, Ord)
data Dir  = Pos | Neg                           deriving (Eq, Ord, Show)

instance Show Node where
    show (URI uri)        = showURI [("rdfs", rdfs)] uri
    show (PlainLiteral s) = "\"" ++ s ++ "\""

type Triple = (Node, Node, Node)
type Graph  = [Triple]

rdfs         =     "http://www.w3.org/2000/01/rdf-schema#"
rdfs_label   = URI "http://www.w3.org/2000/01/rdf-schema#label"
rdfs_seeAlso = URI "http://www.w3.org/2000/01/rdf-schema#seeAlso"

showURI ((short, long):xs) uri | take (length long) uri == long =
                                     short ++ ":" ++ drop (length long) uri
                               | otherwise = showURI xs uri
showURI [] uri = "<" ++ uri ++ ">"

subject :: Triple -> Node
subject (s,_,_) = s

predicate :: Triple -> Node
predicate (_,p,_) = p

object :: Triple -> Node
object (_,_,o) = o

fromNode :: Node -> String
fromNode (URI uri)        = uri
fromNode (PlainLiteral s) = s

rev :: Dir -> Dir
rev Pos = Neg
rev Neg = Pos

mul :: Num a => Dir -> a -> a
mul Pos = id
mul Neg = negate
