#!/usr/bin/env runhaskell
import Control.Monad (when)
import Distribution.Simple.PreProcess
import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.LocalBuildInfo
import System.Cmd (system)
import System.Directory (getModificationTime, doesFileExist)

main = defaultMainWithHooks hooks
hooks = defaultUserHooks { hookedPreProcessors = [trhsx] }

trhsx :: PPSuffixHandler
trhsx = ("fhs", f) where
    f buildInfo localBuildInfo = PreProcessor True $ mkSimplePreProcessor f'
    f' inFile outFile verbose = do
        when (verbose > normal) $
            putStrLn ("checking that preprocessor is up-to-date")
        let [pIn, pOut] = ["Preprocessor/Hsx/Parser."++s | s <- ["ly","hs"]]
        exists <- doesFileExist pOut
        runHappy <- if not exists then return True else do
            [tIn, tOut] <- mapM getModificationTime [pIn, pOut]
            return (tIn > tOut)
        when runHappy $ system ("happy "++pIn) >> return ()
        system ("ghc --make Preprocessor/Main.hs -o preprocessor")

        when (verbose > silent) $
            putStrLn ("preprocessing "++inFile++" to "++outFile)
        writeFile outFile ("-- GENERATED file. Edit the ORIGINAL "++inFile++
                           " instead.\n")
        system ("./preprocessor "++inFile++" >> "++outFile)
        return ()      
