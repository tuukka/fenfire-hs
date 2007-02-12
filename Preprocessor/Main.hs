module Main where

import Preprocessor.Hsx

import System.Environment (getArgs)
import Data.List (intersperse, isPrefixOf)

checkParse p = case p of
		  ParseOk m -> m
		  ParseFailed loc s -> error $ "Error at " ++ show loc ++ ":\n" ++ s

transformFile :: String -> String -> String -> IO ()
transformFile origfile infile outfile = do
	f <- readFile infile
	let fm = process origfile f
	writeFile outfile fm

testFile :: String -> IO ()
testFile file = do
	f <- readFile file
	putStrLn $ process file f


testTransform :: String -> IO ()
testTransform file = do
	f <- readFile file
	putStrLn $ show $ transform $ checkParse $ parse file f
	
testPretty :: String -> IO ()
testPretty file = do
	f <- readFile file
	putStrLn $ prettyPrint $ checkParse $ parse file f

testParse :: String -> IO ()
testParse file = do
	f <- readFile file
	putStrLn $ show $ parse file f

main :: IO ()
main = do args <- getArgs
	  case args of
	   [origfile, infile, outfile] -> transformFile origfile infile outfile
	   [infile, outfile] -> transformFile infile infile outfile
	   [infile] -> testFile infile
	   _ -> putStrLn usageString


process :: FilePath -> String -> String
process fp fc = prettyPrintWithMode (defaultMode {linePragmas=True}) $ 
                 transform $ checkParse $ parse fp fc

parse fn fc = parseModuleWithMode (ParseMode fn) fcuc
  where fcuc= unlines $ filter (not . isPrefixOf "#") $ lines fc


usageString = "Usage: trhsx <infile> [<outfile>]"
