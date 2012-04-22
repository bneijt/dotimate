
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
    _ <- readProcess "neato" ["-Gmaxiter=2", "-Tdot", "-Gpage=7,7", "-Gbgcolor=lightsteelblue3", "-Gcenter=1", "-oframe.dot", dotFileName] "" -- TODO exec
    _ <- readProcess "neato" ["-Gmaxiter=1", "-Tpng", "-Gpage=7,7", "-Gbgcolor=lightsteelblue3", "-Gcenter=1", printf "-oframes/frame%05d.png" frameNumber, "frame.dot"] ""
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

equalNode nodeA nodeB = nodeID nodeA == nodeID nodeB

creatUnionGraph fromGraph toGraph = do
    let nodesA = nodesOf fromGraph
    let nodesB = nodesOf toGraph
    let allNodes = unionBy equalNode nodesA nodesB
    let edgesA = edgesOf fromGraph
    let edgesB = edgesOf toGraph
    let allEdges = unionBy equalEndpoints edgesA edgesB
    fromGraph { graphStatements = (graphStatements fromGraph){nodeStmts = allNodes, edgeStmts = allEdges} }

-- Remove the elements of the possiblyToLargeGraph which are not in nextGraph
pruneCurrenGraph nextGraph possiblyToLargeGraph = do
    let remainingNodes = intersectBy equalNode (nodesOf possiblyToLargeGraph) (nodesOf nextGraph)
    let remainingEdges = intersectBy equalEndpoints (edgesOf possiblyToLargeGraph) (edgesOf possiblyToLargeGraph)
    possiblyToLargeGraph { graphStatements = (graphStatements possiblyToLargeGraph){nodeStmts = remainingNodes, edgeStmts = remainingEdges} }

emitFramesBetween :: (DotGraph String, Int) -> (DotGraph String, Int) -> IO (DotGraph String, Int)
emitFramesBetween (currentGraph, aFrameNumber) (nextGraph, bFrameNumber) = do
    afterEmit <- emitGraph currentGraph aFrameNumber
    --Add nodes and edges to the graph
    let unionGraph = creatUnionGraph afterEmit nextGraph
    afterUnionEmit <- emitGraph unionGraph (aFrameNumber + 10)
    --Remove nodes and egdges from the union
    let prunedGraph = pruneCurrenGraph nextGraph afterUnionEmit
    lastGraph <- emitGraph prunedGraph (aFrameNumber + 20)
    return (lastGraph, bFrameNumber)
    

main = do
    createDirectoryIfMissing False "frames"
    args <- getArgs
    graphs <- mapM readDotFile args
    let numberedGraphs = Prelude.zip graphs [0,30..]
    foldM_ emitFramesBetween (Prelude.head numberedGraphs) (Prelude.tail numberedGraphs)
    putStrLn "done"
