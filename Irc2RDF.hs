
module Irc2RDF where

import System.Time (getClockTime, toUTCTime, CalendarTime(..))
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main = do [root] <- getArgs
          irc <- getContents
          mapM_ (handle root)$ lines irc

handle root line = do clockTime <- getClockTime
                      putStr $ irc2rdf root clockTime line
                      hFlush stdout

irc2rdf root time = uncurry (triples root time) . parse

parse (':':rest) = (Just $ takeWhile (/=' ') rest,
                    parse' "" (tail $ dropWhile (/=' ') rest))
parse      rest  = (Nothing, parse' "" rest)

parse' acc       [] = [reverse acc]
parse' acc   ['\r'] = [reverse acc]
parse'  "" (':':xs) = [reverse . dropWhile (=='\r') $ reverse xs]
parse' acc (' ':xs) = reverse acc : parse' "" xs
parse' acc   (x:xs) = parse' (x:acc) xs

triples root time (Just prefix) ["PRIVMSG","#fenfire",msg] =
    "<irc://freenode/%23fenfire> <"++isContainerOf++"> <"++uri++">.\n"++
    "<"++uri++"> <"++created++"> "++show (day++"T"++second++"Z")++".\n"++
    "<"++uri++"> <"++hasCreator++"> <"++creator++">.\n"++
    "<"++uri++"> <"++hasContent++"> "++show msg++".\n"++
    "<"++uri++"> <"++label++"> "++show ("<"++nick++"> "++msg)++".\n"++
    "<"++creator++"> <"++label++"> "++show nick++".\n"
    where label = "http://www.w3.org/2000/01/rdf-schema#label"
          created = "http://purl.org/dc/terms/created"
          isContainerOf = "http://rdfs.org/sioc/ns#is_container_of"
          hasCreator = "http://rdfs.org/sioc/ns#has_creator"
          hasContent = "http://rdfs.org/sioc/ns#has_content"
          uri = root ++ day ++ "#" ++ second
          nick = takeWhile (/='!') prefix
          creator = "irc://freenode/"++nick++",isuser"
          (CalendarTime y moe d h m s _ps _wd _yd _tzn _tz _isDST) 
              = toUTCTime time
          mo = (fromEnum moe+1)
          p n i = take (n-length (show i)) (repeat '0') ++ show i
          day    = p 4 y ++ '-':p 2 mo ++ '-':p 2 d
          second = p 2 h ++ ':':p 2  m ++ ':':p 2 s
triples _ _ _ _ = ""
