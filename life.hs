import System.Random
import Control.Monad
import Control.Monad.Trans.State


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
showMatrix matrix = putStr (concat (combineRows matrix))

combineCols :: [String] -> String
combineCols [] = ""
combineCols (x:xs) = x ++ combineCols xs

combineRows :: [[Bool]] -> [String]
combineRows matrix = [ combinedRow ++ "\n" | row <- matrix, 
        let combinedRow = (combineCols (convertRow row)) ]

convertRow :: [Bool] -> [String]
convertRow row = [ (convertCell cell) | cell <- row ]

convertCell :: Bool -> String 
convertCell cell 
    | cell  = "█"
    | otherwise = " "

main :: IO ()
main = do
    let numRows = 60 
    let numCols = 90 
    gen <- getStdGen
    let result = generateRandomMatrix numRows numCols gen
    putStrLn "\n"
    showMatrix result
    putStrLn "\n"
