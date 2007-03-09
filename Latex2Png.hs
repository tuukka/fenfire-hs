
module Latex2Png where

-- Copyright (c) 2007, Benja Fallenstein, Tuukka Hastrup
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

import System.Cmd (rawSystem)
import System.Environment (getArgs)
import System.Directory (getTemporaryDirectory, setCurrentDirectory, 
    createDirectory, getDirectoryContents, removeFile, removeDirectory, 
    doesFileExist)
import System.IO (openTempFile, openFile, hPutStr, hClose, IOMode(..))
import System.Exit (ExitCode(..))

import Control.Monad (when)

latex content = unlines [
    "\\documentclass[12pt]{article}",
    "\\pagestyle{empty}",
    "\\usepackage[utf8]{inputenc}",
    "\\begin{document}",
    content,
    "\\end{document}"
    ]

main = do
    [code,outfile] <- getArgs
    handle <- openFile outfile WriteMode
    tmp <- getTemporaryDirectory
    let dir = tmp ++ "/latex2png" -- FIXME / and predictable name
    createDirectory dir
    setCurrentDirectory dir

    let latexFile = "latex2png-temp"
    writeFile (latexFile++".tex") $ latex code
    -- FIXME set environment variables necessary for security, use rlimit
    ExitSuccess <- rawSystem "latex" ["--interaction=nonstopmode", latexFile++".tex"]
    ExitSuccess <- rawSystem "dvipng" ["-bgTransparent", "-Ttight", "", "--noghostscript", "-l1", latexFile++".dvi"]
    
    png <- readFile $ latexFile++"1.png"

    setCurrentDirectory tmp
    files <- getDirectoryContents dir
    flip mapM_ files $ \filename -> do 
        let file = dir ++ "/" ++ filename -- FIXME /
        exists <- doesFileExist file
        when exists $ removeFile $ file
    removeDirectory dir

    hPutStr handle png
    hClose handle
