module Main where


import Data.GraphViz (dotToGraph)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Commands.IO

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree  -- Instância de Graph

import qualified Data.Text.Lazy as T


invertedEdge :: DotEdge NodeType -> DotEdge NodeType
invertedEdge edge = edge { fromNode = toNode edge
                         , toNode = fromNode edge }

labeledEdge :: DotEdge NodeType -> DotEdge NodeType
labeledEdge edge = edge { edgeAttributes = newLabel : filter isLabel (edgeAttributes edge) }
    where
        -- "\\E" é o nome da aresta (origem -> destino)
        newLabel = (Label . StrLabel . T.pack) $ "\\E"
        isLabel = sameAttribute newLabel


transformedStatement :: DotStatement NodeType -> DotStatement NodeType
transformedStatement stmt = case stmt of
        DE edge -> DE (transformedEdge edge)
        otherwise -> stmt
    where
        transformedEdge = invertedEdge . labeledEdge


invertedGraph :: DotGraph NodeType -> DotGraph NodeType
invertedGraph original = original {
        graphStatements = fmap transformedStatement $ graphStatements original
    }


type NodeType = Node


main :: IO ()
main = do
    --graph <- readDotFile "simple.dot" :: IO(DotGraph NodeType)
    graph <- readDotFile "simple_int.dot" :: IO(DotGraph NodeType)
    print $ (dotToGraph graph :: Gr Attributes Attributes)
    writeDotFile "simple_out.dot" $ invertedGraph graph
