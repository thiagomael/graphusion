module Main where


import Data.GraphViz (dotToGraph,
                      graphToDot,
                      GraphvizParams(..),
                      NodeCluster(..))
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types
import qualified Data.GraphViz.Types.Canonical as Canonical
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Commands.IO

import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)  -- Instância de Graph
import Data.Graph.Inductive.Query.MaxFlow

import qualified Data.Text.Lazy as T


labelFromString :: String -> Attribute
labelFromString = (Label . StrLabel . T.pack)


invertedEdge :: DotEdge n -> DotEdge n
invertedEdge edge = edge { fromNode = toNode edge
                         , toNode = fromNode edge }

labeledEdge :: DotEdge n -> DotEdge n
labeledEdge edge = edge { edgeAttributes = newLabel : filter isLabel (edgeAttributes edge) }
    where
        -- "\\E" é o nome da aresta (origem -> destino)
        newLabel = labelFromString "\\E"
        isLabel = sameAttribute newLabel


transformedStatement :: DotStatement n -> DotStatement n
transformedStatement stmt = case stmt of
        DE edge -> DE (transformedEdge edge)
        otherwise -> stmt
    where
        transformedEdge = invertedEdge . labeledEdge


invertedGraph :: DotGraph n -> DotGraph n
invertedGraph original = original {
        graphStatements = fmap transformedStatement $ graphStatements original
    }


fromDot :: DotGraph Node -> Gr Attributes Attributes
fromDot dotGraph = dotToGraph dotGraph :: Gr Attributes Attributes


-- dotToGraph só aceita DotGraph Node, e graphToDot só retorna Canonical.DotGraph... :(
augmentedGraph :: DotGraph Node -> Canonical.DotGraph Node
augmentedGraph graph = graphToDot params (mixin & (fromDot graph))
    where
        params :: GraphvizParams Node Attributes Attributes () Attributes
        params = Params { isDirected       = True
                        , globalAttributes = []
                        , clusterBy        = N
                        , isDotCluster     = const True
                        , clusterID        = const (Num $ Int 0)
                        , fmtCluster       = const []
                        , fmtNode          = \(_, label) -> label
                        , fmtEdge          = \(_, _, label) -> label
                        }
        mixin :: Context Attributes Attributes
        mixin = ( [( [labelFromString "\\E"]
                   , 1)]
                , 4
                , []
                , [] )


-- Imprime um grafo dummy com os labels das arestas (que precisam ser reais)
-- incrementados com o fluxo passando por ela. A idéia é que numa DTMC toda
-- aresta tem peso no intervalo [0, 1], então estados irrelevantes terão fluxo
-- zero (arestas com peso 0 são "canos entupidos").
printFlow = print $ maxFlowgraph dummyGraph 1 4
    where
        -- Grafo FGL em diamante com uma das arestas com peso 0
        dummyGraph :: Gr String Double
        dummyGraph = mkGraph [(1, "a"), (2, "b"), (3, "c"), (4, "c")]
                             [(1, 2, 1), (1, 3, 0), (2, 4, 1), (3, 4, 1)]


pruneDeadPaths :: DynGraph gr => gr nl (Double, el) -> gr nl (Double, el)
pruneDeadPaths = pruneDeadNodes . pruneDeadEdges

pruneDeadNodes :: DynGraph gr => gr nl (Double, el) -> gr nl (Double, el)
pruneDeadNodes graph = delNodes deadNodes graph
    where
        deadNodes = filter isolated (nodes graph)
        isolated node = outdeg graph node == 0 && indeg graph node == 0

pruneDeadEdges :: DynGraph gr => gr nl (Double, el) -> gr nl (Double, el)
pruneDeadEdges graph = delEdges deadEdges graph
    where
        deadEdges = edges $ elfilter noFlow graph
        noFlow (flow, capacity) = flow == 0


writeFlow :: IO ()
writeFlow = writeDotFile "simple_out.dot" $ graphToDot params (pruneDeadPaths withFlow)
    where
        --withFlow = mf dummyGraph 1 4
        withFlow = maxFlowgraph dummyGraph 1 4  -- As arestas com peso 0 são removidas
        -- Grafo FGL em diamante com uma das arestas com peso 0
        dummyGraph :: Gr String Double
        dummyGraph = mkGraph [(1, "a"), (2, "b"), (3, "c"), (4, "d"), (5, "e")]
                             [(1, 2, 1), (1, 3, 0), (2, 4, 0.5), (3, 4, 1), (2, 5, 0.5)]
        params :: Show edgeLabel => GraphvizParams Node String edgeLabel () String
        params = Params { isDirected       = True
                        , globalAttributes = []
                        , clusterBy        = N
                        , isDotCluster     = const True
                        , clusterID        = const (Num $ Int 0)
                        , fmtCluster       = const []
                        , fmtNode          = \(_, label) -> [labelFromString label]
                        , fmtEdge          = \(_, _, label) -> [labelFromString (show label)]
                        }


addSubgraph :: IO ()
addSubgraph = do
    graph <- readDotFile "simple_int.dot" :: IO(DotGraph Node)
    writeDotFile "simple_out.dot" $ augmentedGraph graph


invertStringGraph :: IO ()
invertStringGraph = do
    graph <- readDotFile "simple.dot" :: IO(DotGraph String)
    writeDotFile "simple_out.dot" $ invertedGraph graph


--main = addSubgraph
--main = invertStringGraph
--main = printFlow
main = writeFlow
