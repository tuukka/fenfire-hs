{-# OPTIONS_GHC -fffi #-}
module Fenfire.Irc2RDF where

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
import Data.Char (ord)
import Data.Bits ((.&.))

import qualified Control.Exception

import System.Glib.UTFString (newUTFString, readCString, 
                              peekUTFString)
import System.Glib.FFI (withCString, nullPtr, CString, CInt, Ptr)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "g_utf8_validate" valid :: CString -> CInt -> 
                                                Ptr (CString) -> Bool

-- XXX real toUTF isn't exported from System.Glib.UTFString
toUTF :: String -> String
toUTF s = unsafePerformIO $ newUTFString s >>= readCString

fromUTF :: String -> String
fromUTF s = unsafePerformIO $ Control.Exception.catch 
    (withCString s $ \cstr -> peekUTFString cstr >>= \s' -> 
     if (valid cstr (-1) nullPtr) then return s' -- force any exceptions
                           else return s )
    (\_e -> return s)                 -- if any, keep the local encoding

-- from gutf8.c used in g_utf8_validate
isUnicode c' = let c = ord c' in
     c < 0x110000 &&
     c .&. 0xFFFFF800 /= 0xD800 &&
     (c < 0xFDD0 || c > 0xFDEF) &&
     c .&. 0xFFFE /= 0xFFFE

-- XXX which unicode characters must be escaped?
turtle_escaped :: Char -> String -> String
turtle_escaped _        [] = []
turtle_escaped c ('\\':xs) = '\\':'\\':turtle_escaped c xs
turtle_escaped c    (x:xs) | c == x 
                           = '\\':   c:turtle_escaped c xs
turtle_escaped c ('\n':xs) = '\\': 'n':turtle_escaped c xs
turtle_escaped c ('\r':xs) = '\\': 'r':turtle_escaped c xs
turtle_escaped c ('\t':xs) = '\\': 't':turtle_escaped c xs
turtle_escaped c (   x:xs) =         x:turtle_escaped c xs

main = do [root,filepath,extension] <- getArgs
          'h':'t':'t':'p':':':'/':'/':_ <- return root
          irc <- getContents
          timestamps <- getTimeStamps
          mapM_ (uncurry $ handle root filepath extension) 
                $ zip (map fromUTF$lines irc) (uniquify timestamps)

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

handle :: String -> FilePath -> String -> 
          String -> (ClockTime, Maybe Integer) -> IO ()
handle root filepath extension line (clockTime,offset) = do 
    let (file,output) = irc2rdf root filepath (clockTime,offset) line
    maybe (return ()) ((flip appendFile) (toUTF output).(++extension)) file

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
      '#':channel <- map toLower target, channel `elem` ["fenfire","sioc",
                                                         "swig","haskell"]
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
        t (day++"T"++second++"Z")++"^^<"++date++">.\n"++
    "<"++uri++"> <"++hasCreator++"> <"++creator++">.\n"++
    "<"++uri++"> <"++hasContent++"> "++t msg++".\n"++
    "<"++uri++"> <"++label++"> "++t ("<"++nick++"> "++msg)++".\n"++
    "<"++uri++"> <"++rdftype++"> <"++post++">.\n"++
    "<"++creator++"> <"++label++"> "++t nick++".\n"++
    "<"++creator++"> <"++rdftype++"> <"++user++">.\n"
    )
    where t str = "\"" ++ turtle_escaped '\"' str ++ "\""
          label = "http://www.w3.org/2000/01/rdf-schema#label"
          rdftype = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
          created = "http://purl.org/dc/terms/created"
          isContainerOf = "http://rdfs.org/sioc/ns#container_of"
          hasCreator = "http://rdfs.org/sioc/ns#has_creator"
          hasContent = "http://rdfs.org/sioc/ns#content"
          date = "http://www.w3.org/2001/XMLSchema#dateTime"
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
