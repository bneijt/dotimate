
import System.Process
import System.Environment
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types
import Data.Text
import Text.Printf
-- import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Graph
import System.Directory
import Control.Monad
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
    mapM_ (emitFrameFromDotFile dotFileName) [startFrame..startFrame + 10]

emitGraph :: DotGraph String -> Int -> IO (DotGraph String, Int)
emitGraph graph frameNumber = do
    writeDotFile "state.dot" graph
    emitFramesFromDotFile "state.dot" frameNumber
    nextGraph <- readDotFile "state.dot" :: IO (DotGraph String)
    return (nextGraph, (frameNumber + 1))

-- emitFramesBetween :: (DotGraph String, Int) -> (DotGraph String, Int) -> (DotGraph String, Int)
emitFramesBetween (currentGraph, aFrameNumber) (nextGraph, bFrameNumber) = do
    emitGraph currentGraph bFrameNumber
    --Add nodes and edges to the graph
    let unionGraph = currentGraph
    emitGraph unionGraph (bFrameNumber + 10)
    --Remove nodes and egdges from the union
    let endGraph = nextGraph
    emitGraph endGraph (bFrameNumber + 20)

main = do
    createDirectoryIfMissing False "frames"
    args <- getArgs
    graphs <- mapM readDotFile args
    let numberedGraphs = Prelude.zip graphs [0,30..]
    foldM_ emitFramesBetween (Prelude.head numberedGraphs) (Prelude.tail numberedGraphs)
    putStrLn "done"
