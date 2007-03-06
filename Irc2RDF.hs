
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

import Data.Char (toUpper, toLower)

main = do [root,filepath] <- getArgs
          'h':'t':'t':'p':':':'/':'/':_ <- return root
          irc <- getContents
          timestamps <- getTimeStamps
          mapM_ (uncurry $ handle root filepath) $ zip (lines irc) 
                                                       (uniquify timestamps)

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

handle :: String -> FilePath -> String -> (ClockTime, Maybe Integer) -> IO ()
handle root filepath line (clockTime,offset) = do 
    let (file,output) = irc2rdf root filepath (clockTime,offset) line
    maybe (return ()) ((flip appendFile) output) file

irc2rdf :: String -> FilePath -> (ClockTime, Maybe Integer) -> String ->
           (Maybe FilePath,String)
irc2rdf root filepath time = uncurry (triples root filepath time) . parse

parse (':':rest) = (Just $ takeWhile (/=' ') rest,
                    parse' "" (tail $ dropWhile (/=' ') rest))
parse      rest  = (Nothing, parse' "" rest)

parse' acc       [] = [reverse acc]
parse' acc   ['\r'] = [reverse acc]
parse'  "" (':':xs) = [reverse . dropWhile (=='\r') $ reverse xs]
parse' acc (' ':xs) = reverse acc : parse' "" xs
parse' acc   (x:xs) = parse' (x:acc) xs

triples :: String -> FilePath -> (ClockTime, Maybe Integer) -> 
           Maybe String -> [String] -> (Maybe FilePath, String)
triples root filepath (time,offset) (Just prefix) [cmd,target,msg] 
    | map toUpper cmd == "PRIVMSG", 
      '#':channel <- map toLower target, channel `elem` ["fenfire","swig"]
    = 
    let file = channel ++ "-" ++ day
        uri = root ++ file ++ "#" ++ second ++ maybe "" (('.':) . show) offset
    in
    (
    Just (filepath++file)
    ,
    "<irc://freenode/%23"++channel++"> <"++isContainerOf++"> <"++uri++">.\n"++
    "<irc://freenode/%23"++channel++"> <"++rdftype++"> <"++forum++">.\n"++
    "<"++uri++"> <"++created++"> "++
        show (day++"T"++second++"Z")++"^^<"++date++">.\n"++
    "<"++uri++"> <"++hasCreator++"> <"++creator++">.\n"++
    "<"++uri++"> <"++hasContent++"> "++show msg++".\n"++
    "<"++uri++"> <"++label++"> "++show ("<"++nick++"> "++msg)++".\n"++
    "<"++uri++"> <"++rdftype++"> <"++post++">.\n"++
    "<"++creator++"> <"++label++"> "++show nick++".\n"++
    "<"++creator++"> <"++rdftype++"> <"++user++">.\n"
    )
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
          nick = takeWhile (/='!') prefix
          creator = "irc://freenode/"++nick++",isuser"
          (CalendarTime y moe d h m s _ps _wd _yd _tzn _tz _isDST) 
              = toUTCTime time
          mo = (fromEnum moe+1)
          p n i = take (n-length (show i)) (repeat '0') ++ show i
          day    = p 4 y ++ '-':p 2 mo ++ '-':p 2 d
          second = p 2 h ++ ':':p 2  m ++ ':':p 2 s
triples _ _ _ _ _ = (Nothing, "")
