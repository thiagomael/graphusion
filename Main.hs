module Main where

import DotParser
import DotPrinter
import FDTMC

import Data.GraphViz.Commands.IO


main = do
    dotGraph <- readDotFile "complete.dot"
    print $ dotToFDTMC dotGraph
    writeDotFile "alalao.dot" $ fdtmcToDot . (`resolve` ['f']) . dotToFDTMC $ dotGraph
    return ()
