
import System.Process
import System.Environment
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types
import Text.Printf
import Data.GraphViz.Types.Canonical
import System.Directory
import Control.Monad
import Data.List

baseCmd :: String
baseCmd = "neato"

defaultArguments :: [String]
defaultArguments = ["-Elen=2.0", "-Nfontsize=10", "-Nshape=plaintext", "-Nheight=.1", "-Nwidth=.1", "-Ecolor=#0000555f",  "-Gcenter=1", "-Gsize=12,12!"]

emitFrameFromDotFile :: String -> Int -> IO ()
emitFrameFromDotFile dotFileName frameNumber = do
    _ <- readProcess baseCmd (defaultArguments ++ ["-Gmaxiter=10", "-Tdot", "-oframe.dot", dotFileName]) "" -- TODO exec
    _ <- readProcess baseCmd (defaultArguments ++ ["-Gmaxiter=1", "-Tjpg", printf "-oframes/frame%05d.jpg" frameNumber, "frame.dot"]) ""
    System.Directory.renameFile "frame.dot" dotFileName

emitFramesFromDotFile :: String -> Int -> IO()
emitFramesFromDotFile dotFileName startFrame = do
    mapM_ (emitFrameFromDotFile dotFileName) [startFrame..startFrame + 9]

emitGraph :: DotGraph String -> Int -> IO (DotGraph String)
emitGraph graph frameNumber = do
    writeDotFile "state.dot" graph
    emitFramesFromDotFile "state.dot" frameNumber
    readDotFile "state.dot" :: IO (DotGraph String)

nodesOf :: DotGraph String -> [DotNode String]
nodesOf graph = nodeStmts gs where gs = graphStatements graph

edgesOf :: DotGraph String -> [DotEdge String]
edgesOf graph = edgeStmts gs where gs = graphStatements graph

equalEndpoints :: DotEdge String -> DotEdge String -> Bool
equalEndpoints edgeA edgeB = (fromNode edgeA == fromNode edgeB) && (toNode edgeA == toNode edgeB)

equalNode :: DotNode String -> DotNode String -> Bool
equalNode nodeA nodeB = nodeID nodeA == nodeID nodeB

createUnionGraph :: DotGraph String -> DotGraph String -> DotGraph String
createUnionGraph fromGraph toGraph = do
    let nodesA = nodesOf fromGraph
    let nodesB = nodesOf toGraph
    let allNodes = unionBy equalNode nodesA nodesB
    let edgesA = edgesOf fromGraph
    let edgesB = edgesOf toGraph
    let allEdges = unionBy equalEndpoints edgesA edgesB
    fromGraph { graphStatements = (graphStatements fromGraph){nodeStmts = allNodes, edgeStmts = allEdges} }

-- Remove the elements of the possiblyToLargeGraph which are not in nextGraph
pruneCurrentGraph :: DotGraph String -> DotGraph String -> DotGraph String
pruneCurrentGraph nextGraph possiblyToLargeGraph = do
    let remainingNodes = intersectBy equalNode (nodesOf possiblyToLargeGraph) (nodesOf nextGraph)
    let remainingEdges = intersectBy equalEndpoints (edgesOf possiblyToLargeGraph) (edgesOf possiblyToLargeGraph)
    possiblyToLargeGraph { graphStatements = (graphStatements possiblyToLargeGraph){nodeStmts = remainingNodes, edgeStmts = remainingEdges} }

emitFramesBetween :: (DotGraph String, Int) -> (DotGraph String, Int) -> IO (DotGraph String, Int)
emitFramesBetween (currentGraph, aFrameNumber) (nextGraph, bFrameNumber) = do
    afterEmit <- emitGraph currentGraph aFrameNumber
    --Add nodes and edges to the graph
    let unionGraph = createUnionGraph afterEmit nextGraph
    afterUnionEmit <- emitGraph unionGraph (aFrameNumber + 10)
    --Remove nodes and egdges from the union
    let prunedGraph = pruneCurrentGraph nextGraph afterUnionEmit
    lastGraph <- emitGraph prunedGraph (aFrameNumber + 20)
    return (lastGraph, bFrameNumber)
    
main :: IO ()
main = do
    createDirectoryIfMissing False "frames"
    args <- getArgs
    graphs <- mapM readDotFile args
    let numberedGraphs = Prelude.zip graphs [0,30..]
    foldM_ emitFramesBetween (Prelude.head numberedGraphs) (Prelude.tail numberedGraphs)
    putStrLn "done"
