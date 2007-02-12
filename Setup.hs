#!/usr/bin/env runhaskell
import Control.Monad (when)
import Distribution.PreProcess
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils (rawSystemVerbose, dieWithLocation)
import System.Cmd (system)
import System.Directory (getModificationTime, doesFileExist)

main = defaultMainWithHooks hooks
hooks = defaultUserHooks { hookedPreProcessors = [trhsx, c2hs] }

trhsx :: PPSuffixHandler
trhsx = ("fhs", f) where
    f buildInfo localBuildInfo inFile outFile verbose = do
        when (verbose > 3) $
            putStrLn ("checking that preprocessor is up-to-date")
        let [pIn, pOut] = ["Preprocessor/Hsx/Parser."++s | s <- ["ly","hs"]]
        exists <- doesFileExist pOut
        runHappy <- if not exists then return True else do
            [tIn, tOut] <- mapM getModificationTime [pIn, pOut]
            return (tIn > tOut)
        when runHappy $ system ("happy "++pIn) >> return ()
        system ("ghc --make Preprocessor/Main.hs -o preprocessor")

        when (verbose > 0) $
            putStrLn ("preprocessing "++inFile++" to "++outFile)
        writeFile outFile ("-- GENERATED file. Edit the ORIGINAL "++inFile++
                           " instead.\n")
        system ("./preprocessor "++inFile++" >> "++outFile)
        
c2hs :: PPSuffixHandler
c2hs = ("chs", f) where
    f buildInfo localBuildInfo inFile outFile verbose = do
        when (verbose > 0) $
            putStrLn $ "preprocess "++inFile++" to "++outFile
        maybe (dieWithLocation inFile Nothing "no c2hs available")
              (\c2hs -> rawSystemVerbose verbose c2hs
                            ["--cppopts", "-D\"__attribute__(A)= \"", 
                             "-o", outFile, inFile])
              (withC2hs localBuildInfo) 
            
                         
