module Main where


import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Commands.IO

import Data.Text.Lazy (pack)


invertedEdge :: DotEdge String -> DotEdge String
invertedEdge edge = edge { fromNode = toNode edge
                         , toNode = fromNode edge }

labeledEdge :: DotEdge String -> DotEdge String
labeledEdge edge = edge { edgeAttributes = newLabel : filter isLabel (edgeAttributes edge) }
    where
        -- "\\E" é o nome da aresta (origem -> destino)
        newLabel = (Label . StrLabel . pack) $ "\\E"
        isLabel = sameAttribute newLabel


transformedStatement :: DotStatement String -> DotStatement String
transformedStatement stmt = case stmt of
        DE edge -> DE (transformedEdge edge)
        otherwise -> stmt
    where
        transformedEdge = invertedEdge . labeledEdge


invertedGraph :: DotGraph String -> DotGraph String
invertedGraph original = original {
        graphStatements = fmap transformedStatement $ graphStatements original
    }


main :: IO ()
main = do
    graph <- readDotFile "simple.dot" :: IO(DotGraph String)
    writeDotFile "simple_out.dot" $ invertedGraph graph
