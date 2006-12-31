module RDF where

import Text.ParserCombinators.Parsec

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


toNTriples ((s,p,o):ts) = ntNode s ++ " " ++ ntNode p ++ " " ++ ntNode o ++ ".\n"
                       ++ toNTriples ts
toNTriples []           = ""

ntNode (URI u) = "<" ++ u ++ ">"
ntNode (PlainLiteral s) = "\"" ++ s ++ "\""

fromNTriples :: Monad m => String -> m Graph
fromNTriples s = case parse ntDoc "" s of
    Left err -> fail (show err)
    Right x  -> return x


ntURI = do char '<'; s <- manyTill anyChar (char '>'); return $ URI s
ntLit = do char '"'; s <- manyTill anyChar (char '"'); return $ PlainLiteral s
ntRes = ntURI <|> ntLit

ntTriple = do s <- ntRes; spaces; p <- ntRes; spaces; o <- ntRes; spaces
              char '.'; spaces; return (s,p,o)
              
ntComment = do char '#'; commentChars where
    commentChars = char '\n' <|> do letter; commentChars
    
ntDoc = many ntTriple

