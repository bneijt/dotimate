
import System.Process
import System.Environment
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types
import Data.Text
import Text.Printf
-- import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Graph
import System.Directory
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
    _ <- readProcess "neato" ["-Gmaxiter=5", "-Tdot", "-oframe.dot", dotFileName] ""
    _ <- readProcess "neato" ["-Gmaxiter=1", "-Tpng", printf "-oframes/frame%05d.png" frameNumber, "frame.dot"] ""
    readProcess "mv" ["frame.dot", dotFileName] ""

emitFramesFromDotFile :: String -> Int -> IO()
emitFramesFromDotFile dotFileName startFrame = do
    mapM_ (emitFrameFromDotFile dotFileName) [startFrame..startFrame + 10]

emitGraph dotGraph = do
    writeDotFile "state.dot" dotGraph
    emitFramesFromDotFile "state.dot" 1
    readDotFile "state.dot" :: IO (DotGraph String)

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

createFramesBetween startFileName endFileName = do
    startGraph <- readDotFile startFileName :: IO (DotGraph String)
    endGraph <- readDotFile endFileName :: IO (DotGraph String)
--    emitGraph 
--    writeDotFile "next.dot" (addEdge (args !! 0) (args !! 1) [] df)
    emitFrameFromDotFile "next.dot" 10
    

main = do
    createDirectoryIfMissing False "frames"
    startGraph <- readDotFile "some.dot" :: IO (DotGraph String)
    graph <- emitGraph startGraph
    putStrLn "done"
--    foldl nextDotgraph "initial.dot" args

