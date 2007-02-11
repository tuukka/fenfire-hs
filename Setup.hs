#!/usr/bin/env runhaskell
import Control.Monad (when)
import Distribution.PreProcess
import Distribution.Simple
import System.Cmd (system)

main = defaultMainWithHooks hooks
hooks = defaultUserHooks { hookedPreProcessors = [trhsx] }

trhsx :: PPSuffixHandler
trhsx = ("fhs", f) where
    f buildInfo localBuildInfo inFile outFile verbose = do
        when (verbose > 0) $
            putStrLn ("preprocessing "++inFile++" to "++outFile)
        writeFile outFile ("-- GENERATED file. Edit the ORIGINAL "++inFile++
                           " instead.\n")
        system ("trhsx "++inFile++" >> "++outFile)
        
