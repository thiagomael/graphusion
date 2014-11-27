module Main where


import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Commands.IO

import Data.Text.Internal.Lazy


invertedEdge :: DotEdge String -> DotEdge String
invertedEdge edge = edge { fromNode = toNode edge
                         , toNode = fromNode edge
                         }--, edgeAttributes = Label (StrLabel Empty) : edgeAttributes edge }


transformedStatement :: DotStatement String -> DotStatement String
transformedStatement stmt = case stmt of
    DE edge -> DE (invertedEdge edge)
    otherwise -> stmt


invertedGraph :: DotGraph String -> DotGraph String
invertedGraph original = original {
        graphStatements = fmap transformedStatement $ graphStatements original
    }


ximbas :: String -> String
ximbas s = "alalaô_" ++ s


main :: IO ()
main = do
    leGraph <- readDotFile "simple.dot" :: IO(DotGraph String)
    --print $ graphStatements leGraph
    --print $ fmap ximbas leGraph
    print $ edgeInformation True leGraph
    print $ invertedGraph leGraph
    --alalao <- fmap ximbas leGraph
    --print $ maybe "" (("Alalaô" ++) . show) $ graphID leGraph
    --leName <- graphID leGraph
    --print leName
    writeDotFile "simple_out.dot" $ invertedGraph leGraph
