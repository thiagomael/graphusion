module Main where

import Data.FDTMC
import Data.FDTMC.Parsers.Dot
import Data.FDTMC.Printers.Dot


main = do
    fdtmc <- parseDotFile "complete.dot"
    writeDotFile "alalao.dot" $ pruneUnreachableStates . (`resolve` ['f']) $ fdtmc
    return ()
