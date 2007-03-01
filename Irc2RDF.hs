
module Irc2RDF where

import System.Time (getClockTime, toUTCTime, CalendarTime(..), ClockTime(..))
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafeInterleaveIO)

main = do [root] <- getArgs
          irc <- getContents
          timestamps <- getTimeStamps
          mapM_ (uncurry $ handle root) $ zip (lines irc) (uniquify timestamps)

getTimeStamps = do ~(TOD secs _picos) <- unsafeInterleaveIO getClockTime
                   xs <- unsafeInterleaveIO getTimeStamps
                   return (TOD secs 0:xs)

uniquify     [] = []
uniquify (x:xs) = (x,Nothing):uniquify' (x,Nothing) xs

uniquify'    _     [] = []
uniquify' prev (x:xs) | fst prev == x = next prev:uniquify' (next prev) xs
                      | otherwise     =   first x:uniquify' (first x) xs
    where next (i,offset) = (i, Just $ maybe (2::Integer) (+1) offset)
          first i         = (i, Nothing)

handle root line (clockTime,offset) = do 
                                putStr $ irc2rdf root (clockTime,offset) line
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

triples root (time,offset) (Just prefix) ["PRIVMSG","#fenfire",msg] =
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
          uri = root ++ day ++ "#" ++ second ++ maybe "" (('.':) . show) offset
          nick = takeWhile (/='!') prefix
          creator = "irc://freenode/"++nick++",isuser"
          (CalendarTime y moe d h m s _ps _wd _yd _tzn _tz _isDST) 
              = toUTCTime time
          mo = (fromEnum moe+1)
          p n i = take (n-length (show i)) (repeat '0') ++ show i
          day    = p 4 y ++ '-':p 2 mo ++ '-':p 2 d
          second = p 2 h ++ ':':p 2  m ++ ':':p 2 s
triples _ _ _ _ = ""
