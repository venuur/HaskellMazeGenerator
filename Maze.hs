import Data.List
import Data.List.Split
import System.Random
import System.IO

-- |Utility to compose a list of functions and apply to a value.  Can be used to 
-- for example compose list modifications.
compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

-- |The pipe "|" is a north-south corridor, the hyphen "-" is a west-east 
-- corridor, and the plus "+" is an intersection.
data Direction = North | East | South | West deriving (Show, Eq)

flipDirection North = South
flipDirection South = North
flipDirection East = West
flipDirection West = East

data Corridor = 
    NoCorridor |
    DeadEndCorridor Direction |
    PlainCorridor Direction Direction |
    TeeCorridor Direction |
    IntersectionCorridor

-- |Using Unicode box drawing characters (https://en.wikipedia.org/wiki/Box_Drawing)
-- we represetn the maze corridors.  Directions correspond to the direction in which
-- you could lead the corridor segment.
showNoCorridor = "."

showDeadEndCorridor North = "\x2568"
showDeadEndCorridor East = "\x255e"
showDeadEndCorridor South = "\x2565"
showDeadEndCorridor West = "\x2561"

showPlainCorridor North East = "\x255a"
showPlainCorridor North South = "\x2551"
showPlainCorridor North West = "\x255d"
showPlainCorridor East South = "\x2554"
showPlainCorridor East West = "\x2550"
showPlainCorridor South West = "\x2557"
-- |Remaining by symmetry.
showPlainCorridor East North = showPlainCorridor North East
showPlainCorridor South North = showPlainCorridor North South
showPlainCorridor West North = showPlainCorridor North West
showPlainCorridor South East = showPlainCorridor East South
showPlainCorridor West East = showPlainCorridor East West
showPlainCorridor West South = showPlainCorridor South West
showPlainCorridor d1 d2
    | d1 == d2 = show $ DeadEndCorridor d1

showTeeCorridor North = "\x2569"
showTeeCorridor East = "\x2560"
showTeeCorridor South = "\x2566"
showTeeCorridor West = "\x2563"

showIntersectionCorridor = "\x256c"

showCorridor NoCorridor = showNoCorridor
showCorridor (DeadEndCorridor d) = showDeadEndCorridor d
showCorridor (PlainCorridor d1 d2) = showPlainCorridor d1 d2
showCorridor (TeeCorridor d) = showTeeCorridor d
showCorridor IntersectionCorridor = showIntersectionCorridor

instance Show Corridor where
    show c = showCorridor c
    
-- |The first argument is the width (west-east).  Maze lists
-- the maze cells row by row.
data Maze = Maze { mazeWidth :: Int,
                   mazeCorridors :: [Corridor] }
mazeHeight (Maze width corridors) = (length corridors) `quot` width


setCorridorAt :: Location -> Corridor -> Maze -> Maze
setCorridorAt (IndexLocation i) c (Maze n corridors) =
    let (cellsBefore, cellsAfter) = splitAt i corridors
        newCorridors = cellsBefore ++ (c : tail cellsAfter)
    in Maze n newCorridors
setCorridorAt xy@(XYLocation x y) c maze@(Maze n corridors) =
    setCorridorAt (indexFromXY n xy) c maze


data Location = XYLocation Int Int | IndexLocation Int deriving (Eq)

instance Show Location where
    show (XYLocation x y) = "(x=" ++ show x ++ ",y=" ++ show y ++ ")"
    show (IndexLocation i) = "(i=" ++ show i ++ ")"
        
        
indexFromXY width (XYLocation x y) = IndexLocation (x + y*width)
xyFromIndex width (IndexLocation i) = XYLocation x y
    where (y, x) = i `quotRem` width


instance Show Maze where
    show (Maze width corridors) = 
        (intercalate "\n") . (chunksOf width) . concat . (map show) $ corridors


neighbors :: Maze -> Location -> [Location]
neighbors maze@(Maze width corridors) (XYLocation x y) = 
    [XYLocation x' y' | (x', y') <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)], 
     x' >= 0, x' < width, y' >= 0, y' < m]
     where m = mazeHeight maze
neighbors maze@(Maze width corridors) index@(IndexLocation i) = 
    map (indexFromXY width) xyNeighbors
        where xyNeighbors = neighbors maze (xyFromIndex width index)

-- |
data MazeNode = 
    MazeNode {mazeNodeLocation :: Location,
              mazeNodeNeighbors :: [Location]}

instance Show MazeNode where
    show (MazeNode location neighbors) = 
        let locationString = show location
            neighborStrings = map show neighbors
            edgeStrings = intercalate "," neighborStrings
        in locationString ++ "--[" ++ edgeStrings ++ "]"

instance Eq MazeNode where
    x == y = mazeNodeLocation x == mazeNodeLocation y
    

data MazeGraph = MazeGraph {mazeNodeList :: [MazeNode]}

instance Show MazeGraph where
    show (MazeGraph []) = "EmptyGraph"
    show (MazeGraph nodes) = intercalate "\n" (map show nodes)

fullMazeGraph width height = 
    let locations = [XYLocation x y | y <- [0..height-1], x <- [0..width-1]]
        allNeighbors = map (neighbors (emptyMaze width height)) locations
    in MazeGraph (zipWith MazeNode locations allNeighbors)

    
edgeDirection :: Location -> Location -> Direction
edgeDirection (XYLocation x1 y1) (XYLocation x2 y2)
    | x1 == x2 && y1 <  y2 = South
    | x1 == x2 && y1 >  y2 = North
    | x1 <  x2 && y1 == y2 = East
    | x1 >  x2 && y1 == y2 = West


-- |These functions assume all directions are different.
corridorFromDirections :: [Direction] -> Corridor
corridorFromDirections [] = NoCorridor
corridorFromDirections [d] = DeadEndCorridor d
corridorFromDirections [d1, d2] = PlainCorridor d1 d2
corridorFromDirections ds@[d1, d2, d3] = TeeCorridor (flipDirection d4)
    where directions = [North, South, East, West]
          d4 = (directions \\ ds) !! 0
corridorFromDirections [d1, d2, d3, d4] = IntersectionCorridor

corridorFromMazeNode (MazeNode origin neighbors) = 
    corridorFromDirections . map (edgeDirection origin) $ neighbors

corridorsFromGraph (MazeGraph nodes) = map corridorFromMazeNode nodes

emptyMaze width height = Maze width (replicate (width*height) NoCorridor)

mazeFromGraph width height graph@(MazeGraph nodes) = 
    let corridors = corridorsFromGraph graph
        baseMaze = emptyMaze width height
        locations = map mazeNodeLocation nodes
        addCorridors = compose (zipWith setCorridorAt locations corridors)
    in addCorridors baseMaze

randomStep :: MazeNode -> IO Location
randomStep (MazeNode location neighbors) = do
    let m = length neighbors
    i <- randomRIO (0, m-1)
    let randomNeighbor = neighbors !! i
    return randomNeighbor
    
findNode :: Location -> MazeGraph -> MazeNode
findNode location (MazeGraph nodes) = 
    head . filter ((==location) . mazeNodeLocation) $ nodes
   

findRandomNeighbor :: MazeNode -> MazeGraph -> IO MazeNode
findRandomNeighbor node graph = do
    neighborLocation <- randomStep node
    let randomNeigborNode = findNode neighborLocation graph
    return randomNeigborNode

    
randomWalk :: Int -> MazeGraph -> IO [MazeNode]
randomWalk 0 _ = return []
randomWalk n graph@(MazeGraph nodes) = do
    let nNodes = length nodes
    randomIndex <- randomRIO (0, nNodes)
    let randomNode = nodes !! randomIndex
    randomNeigborNode <- findRandomNeighbor randomNode graph
    subsequentNodes <- randomWalkFrom randomNeigborNode (n-1) graph
    return (randomNode : subsequentNodes)
    
    
randomWalkFrom :: MazeNode -> Int -> MazeGraph -> IO [MazeNode]
randomWalkFrom _ 0 _ = return []
randomWalkFrom node n graph = do
    randomNeigborNode <- findRandomNeighbor node graph
    subsequentNodes <- randomWalkFrom randomNeigborNode (n-1) graph
    return (node : subsequentNodes)    
    

replaceNodeInGraph node (MazeGraph nodes) =
    let (before, after) = break (== node) nodes
    in MazeGraph (before ++ [node] ++ (tail after))
    
  
removeNodeInGraph node (MazeGraph nodes) = 
    let (before, after) = break (== node) nodes
    in MazeGraph (before ++ (tail after))
        
        
randomEdge subgraph@(MazeGraph nodes) graph = do    
    randomIndex <- randomRIO (0, length nodes - 1)
    let randomOrigin = nodes !! randomIndex    
    randomNeigborNode <- findRandomNeighbor randomOrigin graph
    return (randomOrigin, randomNeigborNode)

    
pruneNeighbors (MazeNode location neighbors) referenceNodes =
    let referenceLocations = map mazeNodeLocation referenceNodes
        filteredNodes = filter (`notElem` referenceLocations) neighbors
    in MazeNode location filteredNodes
    
   
pruneActiveGraph activeGraph@(MazeGraph nodes) spanningTree@(MazeGraph treeNodes) =
    let prunedNodes = (map (flip pruneNeighbors treeNodes) nodes)
        droppedTrappedNodes = filter ((>0) . length . mazeNodeNeighbors) prunedNodes
    in MazeGraph droppedTrappedNodes
    

updateActiveGraph 
        activeNode 
        neighborNode 
        activeGraph 
        spanningTree@(MazeGraph treeNodes) =
    let activeNodeLocation = mazeNodeLocation activeNode
        neighborLocation = mazeNodeLocation neighborNode
        unvisitedNodes = 
            filter (/= neighborLocation) $ mazeNodeNeighbors activeNode
        updatedActiveNode =
            MazeNode activeNodeLocation unvisitedNodes
        updatedActiveGraph = 
            if length unvisitedNodes > 0
                then replaceNodeInGraph updatedActiveNode activeGraph
                else removeNodeInGraph updatedActiveNode activeGraph
        treeLocations = map mazeNodeLocation treeNodes
        newNeighbors = filter (`notElem` treeLocations) $ mazeNodeNeighbors neighborNode
        newActiveNode =
            MazeNode neighborLocation newNeighbors
        newActiveGraph =
            if length newNeighbors > 0
                then MazeGraph (newActiveNode : mazeNodeList updatedActiveGraph)
                else updatedActiveGraph
    in pruneActiveGraph newActiveGraph spanningTree


updateSpanningTree activeNode neigborNode spanningTree =
    let activeNodeLocation = mazeNodeLocation activeNode
        activeNodeInTree = findNode activeNodeLocation spanningTree
        neighborLocation = mazeNodeLocation neigborNode
        updatedActiveNodeInTree = 
            MazeNode 
                activeNodeLocation
                (neighborLocation : mazeNodeNeighbors activeNodeInTree)
        updatedTree = replaceNodeInGraph updatedActiveNodeInTree spanningTree
        newNeighbor = MazeNode neighborLocation [activeNodeLocation]
        newTree = MazeGraph (newNeighbor : mazeNodeList updatedTree)
    in newTree



randomSpanningTreeFrom :: MazeGraph -> MazeGraph -> MazeGraph -> IO MazeGraph
randomSpanningTreeFrom spanningTree activeGraph@(MazeGraph []) _ = do
    {- DEBUG
    putStrLn "Spanning Tree:"
    putStrLn $ show spanningTree
    putStrLn "Active Graph"
    putStrLn $ show activeGraph
    putStrLn "" -}
    
    return spanningTree    
   
randomSpanningTreeFrom 
        spanningTree@(MazeGraph treeNodes) 
        activeGraph@(MazeGraph activeNodes) 
        graph = do
    {- DEBUG
    putStrLn "Spanning Tree:"
    putStrLn $ show spanningTree
    putStrLn "Active Graph"
    putStrLn $ show activeGraph
    putStrLn "" -}
    
    (randomActiveNode, randomNeighborNode) <- randomEdge activeGraph graph
    
    {- DEBUG
    putStrLn "Random Edge"
    putStrLn $ show randomActiveNode ++ " to " ++ show randomNeighborNode -}
    
    let updatedSpanningTree = 
            updateSpanningTree randomActiveNode randomNeighborNode spanningTree
        updatedActiveGraph = 
            updateActiveGraph randomActiveNode randomNeighborNode activeGraph updatedSpanningTree
        
    randomSpanningTreeFrom updatedSpanningTree updatedActiveGraph graph


main = do
    let width = 24
        height = 12
    spanningTree <- randomSpanningTreeFrom testSpanningTree testActiveGraph $ fullMazeGraph width height
    let spanningTreeMaze = mazeFromGraph width height spanningTree
    putStrLn $ show spanningTreeMaze
    
testEmptyMaze = Maze 4 (replicate 8 NoCorridor)
testLocations = [XYLocation x y | y <- [0..1], x <- [0..3]]
testNeigbors = map (neighbors testMaze) testLocations
testMazeGraph = MazeGraph (zipWith MazeNode testLocations testNeigbors)
testMaze = mazeFromGraph 4 2 testMazeGraph

testSpanningTree = MazeGraph [
    MazeNode (XYLocation 2 0) []]
    
testActiveGraph = MazeGraph [
    MazeNode (XYLocation 2 0) [XYLocation 1 0,XYLocation 3 0,XYLocation 2 1]]
