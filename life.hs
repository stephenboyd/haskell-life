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

swapValue :: [Bool] -> Int -> [Bool]
swapValue row ind = 
    let (front,val:end) = splitAt ind row
    in front ++ (not val):end

-- TODO: remove as transition completed
flipRow :: [Bool] -> [Bool]
flipRow row = [ (not x) | x <- row ]
        
-- TODO: transition needs to update cells
runTransition oldMatrix = [ (flipRow row) | row <- oldMatrix ] 
         
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
