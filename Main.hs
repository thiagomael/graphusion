module Main where

import Data.FDTMC
import Data.FDTMC.Parsers.Dot
import Data.FDTMC.Printers.Dot

import Data.GraphViz.Commands.IO


main = do
    dotGraph <- readDotFile "complete.dot"
    print $ dotToFDTMC dotGraph
    writeDotFile "alalao.dot" $ fdtmcToDot . (`resolve` ['f']) . dotToFDTMC $ dotGraph
    return ()
