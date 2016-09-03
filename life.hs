import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Control.Concurrent (threadDelay)


generateRandomMatrix :: Int -> Int -> StdGen -> [[Bool]]
generateRandomMatrix numRows numCols gen
    | numRows == 0 = error "incorrect number of rows! needs to be > 0"
    | numRows == 1 = [fst (generateRandomRow numCols gen)]
    | otherwise    = 
        let (bools, newGen) = (generateRandomRow numCols gen)
        in [bools] ++ (generateRandomMatrix (numRows - 1) numCols newGen)

generateRandomRow :: Int -> StdGen -> ([Bool], StdGen)
generateRandomRow numCols = runState (replicateM numCols (state random))

showMatrix :: [[Bool]] -> IO ()
showMatrix matrix = putStr (flattenMatrix (convertMatrix matrix))

flattenMatrix :: [[String]] -> String
flattenMatrix [] = ""
flattenMatrix (x:xs) = 
    let combinedCols = (combineCols x)
    in combinedCols ++ "\n" ++ (flattenMatrix xs)

convertMatrix :: [[Bool]] -> [[String]]
convertMatrix matrix = [ (convertRow row) | row <- matrix ]

combineCols :: [String] -> String
combineCols [] = ""
combineCols (x:xs) = x ++ (combineCols xs)

convertRow :: [Bool] -> [String]
convertRow row = [ (convertCell cell) | cell <- row ]

convertCell :: Bool -> String 
convertCell cell 
    | cell  = "█"
    | otherwise = " "

swapRow :: [[Bool]] -> Int -> [Bool]
swapRow matrix y =
    let (top, row:bottom) = splitAt y matrix
        rowLength = (length row) - 1
        swappedRow = [ (swapCell cellValue neighbourCount) | x <- [0..rowLength], 
                let neighbourCount = getNeighbourCount matrix x y,
                let cellValue = getCellValue matrix x y ]
    in swappedRow

swapCell :: Bool -> Int -> Bool
swapCell cell neighbourCount
    | cell == True = changeLiveCell neighbourCount
    | cell == False = changeDeadCell neighbourCount 

changeLiveCell neighbourCount
    | neighbourCount < 2    = False
    | neighbourCount == 2   = True
    | neighbourCount == 3   = True
    | otherwise             = False 

changeDeadCell neighbourCount
    | neighbourCount == 3   = True
    | otherwise             = False

runTransition matrix = 
    let matrixLength = (length matrix) - 1
    in [ (swapRow matrix y) | y <- [0..matrixLength] ]

getCellValue :: [[Bool]] -> Int -> Int -> Bool
getCellValue matrix x y = matrix !! y !! x 

-- | Returns 1 if the neighbour is True, 0 if False.
getNeighbour :: [[Bool]] -> Int -> Int -> Int
getNeighbour matrix x y
    | x == -1 = 0
    | y == -1 = 0
    | y == (length matrix)  = 0
    | x == (length (head matrix)) = 0
    | otherwise = 
        let value = matrix !! y !! x
        in (if value then 1 else 0)

{-| Counts the number of neighbours that a cell has from the 8 neighbours 
 - (vertically, horizontally, or diagonally adjacent).
-}
getNeighbourCount :: [[Bool]] -> Int -> Int -> Int
getNeighbourCount matrix x y = 
    let topLeft = getNeighbour matrix (x - 1) (y - 1) 
        top     = getNeighbour matrix x (y - 1) 
        topRight = getNeighbour matrix (x + 1) (y - 1) 
        right   = getNeighbour matrix (x + 1) y 
        bottomRight = getNeighbour matrix (x + 1) (y + 1) 
        bottom  = getNeighbour matrix x (y + 1) 
        bottomLeft = getNeighbour matrix (x - 1) (y + 1) 
        left    = getNeighbour matrix (x - 1) y 
    in topLeft + top 
        + topRight + right
        + bottomRight + bottom
        + bottomLeft + left
         
repeatLoop _ 0 = return ()
repeatLoop matrix n =
    do 
        let nextMatrix = runTransition matrix
        showMatrix matrix
        putStrLn "\n"
        threadDelay 500000
        repeatLoop nextMatrix (n - 1)

main :: IO ()
main = do
    let numRows = 30 
    let numCols = 90 
    gen <- getStdGen
    let result = generateRandomMatrix numRows numCols gen
    putStrLn "\n"
    showMatrix result
    let other = runTransition result
    repeatLoop (result) 100
    putStrLn "\n"
