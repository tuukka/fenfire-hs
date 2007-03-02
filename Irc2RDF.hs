
module Irc2RDF where

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
    "<irc://freenode/%23fenfire> <"++rdftype++"> <"++forum++">.\n"++
    "<"++uri++"> <"++created++"> "++
        show (day++"T"++second++"Z")++"^^<"++date++">.\n"++
    "<"++uri++"> <"++hasCreator++"> <"++creator++">.\n"++
    "<"++uri++"> <"++hasContent++"> "++show msg++".\n"++
    "<"++uri++"> <"++label++"> "++show ("<"++nick++"> "++msg)++".\n"++
    "<"++uri++"> <"++rdftype++"> <"++post++">.\n"++
    "<"++creator++"> <"++label++"> "++show nick++".\n"++
    "<"++creator++"> <"++rdftype++"> <"++user++">.\n"
    where label = "http://www.w3.org/2000/01/rdf-schema#label"
          rdftype = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
          created = "http://purl.org/dc/terms/created"
          isContainerOf = "http://rdfs.org/sioc/ns#is_container_of"
          hasCreator = "http://rdfs.org/sioc/ns#has_creator"
          hasContent = "http://rdfs.org/sioc/ns#has_content"
          date = "http://www.w3.org/2001/XMLSchema#date"
          forum = "http://rdfs.org/sioc/ns#Forum"
          post = "http://rdfs.org/sioc/ns#Post"
          user = "http://rdfs.org/sioc/ns#User"
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
