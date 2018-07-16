module Example (main) where
import ParseSudoku
import Data.Foldable
import Solver


main :: IO ()
main = do
    content <- readFile "resources/sudoku17.txt"   
    let step a acc = do
                        r <- solveSudoku a
                        if r then return (acc+1) else return acc
    n <- foldrM step (0::Int) (lines content )
    print n
    return ()

solveSudoku :: String -> IO Bool
solveSudoku line =  do
    m <- parseSudoku line
    solve m
