module Main where

import Data.FDTMC
import Data.FDTMC.Parsers.Dot
import Data.FDTMC.Printers.Dot


main = do
    fdtmc <- parseDotFile "complete.dot"
    let pruned = pruneUnreachableStates . (`resolve` ['f']) $ fdtmc
    writeDotFile "resolved.dot" pruned
    frag <- parseDotFile "fragment.dot"
    writeDotFile "composed.dot" $ append pruned frag "errorHandling"
    return ()
