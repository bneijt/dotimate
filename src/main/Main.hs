
import System.Process
import System.Environment
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types
import Data.Text
import Text.Printf
import Data.GraphViz.Types.Canonical
-- import Data.GraphViz.Types.Graph
import System.Directory
import Control.Monad
import Data.List
-- Write PNG image file
-- Neato to PNG first frame
-- Add edge to end of nodes
-- Neato the next step
{-
./dotimate a.dot b.dot c.dot d.dot

generate 10 frames from (a.dot) to b.dot
    simulate spring model for 5 iterations (on a.dot), print out image
    add nodes of b.dot to a.dot
    simulate spring model for 5 iterations (on state.dot), print out image
    remove extra nodes and edges not in b.dot from state.dot
    simulate spring model for 5 iterations (on state.dot), print out image    
generate 10 frames from (earlier result) to c.dot
generate 10 frames from (earlier result) to d.dot


-}

emitFrameFromDotFile :: String -> Int -> IO ()
emitFrameFromDotFile dotFileName frameNumber = do
    _ <- readProcess "neato" ["-Gmaxiter=5", "-Tdot", "-oframe.dot", dotFileName] "" -- TODO exec
    _ <- readProcess "neato" ["-Gmaxiter=1", "-Tpng", printf "-oframes/frame%05d.png" frameNumber, "frame.dot"] ""
    System.Directory.renameFile "frame.dot" dotFileName

emitFramesFromDotFile :: String -> Int -> IO()
emitFramesFromDotFile dotFileName startFrame = do
    mapM_ (emitFrameFromDotFile dotFileName) [startFrame..startFrame + 9]

emitGraph :: DotGraph String -> Int -> IO (DotGraph String)
emitGraph graph frameNumber = do
    writeDotFile "state.dot" graph
    emitFramesFromDotFile "state.dot" frameNumber
    readDotFile "state.dot" :: IO (DotGraph String)

nodesOf graph = nodeStmts gs where gs = graphStatements graph

edgesOf graph = edgeStmts gs where gs = graphStatements graph

equalEndpoints :: DotEdge String -> DotEdge String -> Bool
equalEndpoints edgeA edgeB = (fromNode edgeA == fromNode edgeB) && (toNode edgeA == toNode edgeB)

creatUnionGraph graphA graphB = do
    let nodesA = nodesOf graphA
    let nodesB = nodesOf graphB
    let allNodes = union nodesA nodesB
    let edgesA = edgesOf graphA
    let edgesB = edgesOf graphB
    let allEdges = unionBy equalEndpoints edgesA edgesB
    graphA { graphStatements = (graphStatements graphA){nodeStmts = allNodes, edgeStmts = allEdges} }

removeNodesAndEdgesNotIn smallGraph possiblyToLargeGraph = possiblyToLargeGraph

emitFramesBetween :: (DotGraph String, Int) -> (DotGraph String, Int) -> IO (DotGraph String, Int)
emitFramesBetween (currentGraph, aFrameNumber) (nextGraph, bFrameNumber) = do
    afterEmit <- emitGraph currentGraph aFrameNumber
    --Add nodes and edges to the graph
    let unionGraph = creatUnionGraph afterEmit nextGraph
    afterUnionEmit <- emitGraph unionGraph (aFrameNumber + 10)
    --Remove nodes and egdges from the union
    let prunedGraph = removeNodesAndEdgesNotIn nextGraph afterUnionEmit
    lastGraph <- emitGraph prunedGraph (aFrameNumber + 20)
    return (lastGraph, bFrameNumber)
    

main = do
    createDirectoryIfMissing False "frames"
    args <- getArgs
    graphs <- mapM readDotFile args
    let numberedGraphs = Prelude.zip graphs [0,30..]
    foldM_ emitFramesBetween (Prelude.head numberedGraphs) (Prelude.tail numberedGraphs)
    putStrLn "done"
