
import System.Process
import System.Environment
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types
import Data.Text
import Text.Printf
-- import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Graph
-- Write PNG image file
-- Neato to PNG first frame
-- Add edge to end of nodes
-- Neato the next step
{-
General flow:
    Read dot graphs given
    Calculate diff of nodes and edges
    Create images of each update
-}

emitFrameFromDotFile :: String -> Int -> IO String
emitFrameFromDotFile dotFileName frameNumber = do
    _ <- readProcess "neato" ["-Ln1", "-Tdot", "-oframe.dot", dotFileName] "" -- TODO use exec
    _ <- readProcess "neato" ["-Ln0", "-Tpng", printf "-oimages/frame%05d.png" frameNumber, "frame.dot"] ""
    readProcess "mv" ["frame.dot", dotFileName] ""

emitFramesFromDotFile :: String -> Int -> IO()
emitFramesFromDotFile dotFileName startFrame = do
    mapM_ (emitFrameFromDotFile dotFileName) [startFrame..startFrame + 10]
{-
addNodes :: oldGraph -> newGraph -> combinedGraph
prune :: oldGraph -> newGraph -> prunedGraph

do:
    addNodes
    saveGraph
    emitFrames
    loadGraph
    prune
    saveGraph
    emitFrames
    loadGraph
-}

main = do
    args <- getArgs
    df <- readDotFile "current.dot" :: IO (DotGraph String)
    writeDotFile "next.dot" (addEdge (args !! 0) (args !! 1) [] df)
    emitFrameFromDotFile "next.dot" 10
