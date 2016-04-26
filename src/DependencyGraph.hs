{-# LANGUAGE OverloadedStrings #-}
-- Takes an input file containing two columns, such that the item in the first
-- column depends on the item in the second column.
module DependencyGraph
    (graphFromFile)
where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import System.IO (readFile)
import System.Environment (getArgs)
import qualified Data.Text.Lazy as T

directedAttrs :: [GlobalAttributes]
directedAttrs = [GraphAttrs [Ratio CompressRatio,
                                     Size $ GSize 16 Nothing True,
                                     Pack DoPack,
                                     PackMode PackGraph]]

undirectedAttrs :: [GlobalAttributes]
undirectedAttrs = [GraphAttrs [Size $ GSize 16 Nothing True,
                               Overlap ScaleXYOverlaps]]

sourceFmt :: [Attribute]
sourceFmt = [style filled, color LightGray]

targetFmt :: [Attribute]
targetFmt = [style filled, color Red]

generateGraph :: [DotNode T.Text] -> [DotNode T.Text] -> [DotEdge T.Text] -> DotGraph T.Text
generateGraph sources targets edges =
    DotGraph {
        directedGraph = False,
        strictGraph = False,
        graphID = Just (Str "G"),
        graphStatements = DotStmts {
            attrStmts = undirectedAttrs,
            subGraphs = [],
            nodeStmts = sources ++ targets,
            edgeStmts = edges
        }
    }

printGraph :: DotGraph T.Text -> IO ()
printGraph = putStrLn . T.unpack . renderDot . toDot

graphFromFile :: String -> IO ()
graphFromFile fname = do
    contents <- readFile fname
    let pairs = map ((\(x:xs) -> (T.pack x, T.pack . head $ xs)) . words) . lines $ contents
    let sources = map (\(s,t) -> DotNode s sourceFmt) pairs
    let targets = map (\(s,t) -> DotNode t targetFmt) pairs
    let edges = map (\(x,y) -> DotEdge x y []) pairs
    printGraph $ generateGraph sources targets edges
