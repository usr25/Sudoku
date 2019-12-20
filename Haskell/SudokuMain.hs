module Main where

import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Maybe (fromJust)
import Control.Parallel (par, pseq)

import Data.Bits
import Data.Array.Unboxed

import Sudoku

vals_easy = 
        [[0, 0, 3,  0, 2, 9,  6, 0, 8],
         [6, 0, 0,  8, 0, 0,  0, 9, 0],
         [0, 9, 5,  4, 0, 6,  0, 0, 7],
         [1, 6, 0,  9, 3, 0,  8, 0, 4],
         [4, 0, 7,  0, 8, 0,  9, 0, 6],
         [9, 0, 8,  0, 4, 2,  0, 0, 0],
         [3, 0, 0,  2, 0, 4,  0, 6, 0],
         [0, 7, 0,  0, 0, 1,  0, 0, 5],
         [5, 0, 9,  7, 6, 0,  3, 1, 2]]

vals_tree = 
        [[0, 0, 3,  0, 2, 9,  6, 0, 8],
        [6, 0, 0,  8, 0, 0,  0, 9, 0],
        [0, 0, 0,  4, 0, 6,  0, 0, 7],
        [0, 0, 0,  9, 3, 0,  8, 0, 4],
        [4, 0, 7,  0, 8, 0,  9, 0, 6],
        [9, 0, 8,  0, 4, 2,  0, 0, 0],
        [3, 0, 0,  2, 0, 4,  0, 0, 0],
        [0, 7, 0,  0, 0, 1,  0, 0, 5],
        [5, 0, 9,  7, 6, 0,  3, 0, 0]]

vals_final = 
        [[0, 2, 4,  0, 0, 0,  0, 0, 0], 
        [0, 0, 0,  0, 0, 7,  1, 0, 0],
        [0, 9, 0,  0, 0, 0,  0, 0, 0],
        [0, 0, 0,  0, 0, 0,  0, 8, 4],
        [0, 0, 0,  0, 7, 5,  0, 0, 0],
        [6, 0, 0,  0, 3, 0,  0, 0, 0],
        [0, 0, 0,  4, 0, 0,  0, 2, 9],
        [0, 0, 0,  2, 0, 0,  3, 0, 0],
        [1, 0, 0,  0, 0, 0,  0, 0, 0]]

vals_impossible = --It should return sudokuEMPTY and False when checked with isValid
        [[0, 2, 3,  4, 5, 6,  7, 8, 9], 
        [0, 0, 0,  0, 0, 7,  1, 0, 0],
        [0, 9, 0,  0, 0, 0,  0, 0, 0],
        [0, 0, 0,  0, 0, 0,  0, 8, 4],
        [0, 0, 0,  0, 7, 5,  0, 0, 0],
        [6, 0, 0,  0, 3, 0,  0, 0, 0],
        [0, 3, 5,  4, 6, 7,  8, 2, 9],
        [0, 0, 0,  2, 0, 0,  3, 0, 0],
        [0, 0, 0,  0, 0, 0,  0, 0, 0]]

solveParallel :: Sudoku -> (Sudoku, Bool)
solveParallel sud = if isFinished setAll then (setAll, True) else parFall (trySudokus (Sudoku vs newRem rows cols sqrs) rowCounter colCounter possible)
    where 
        setAll :: Sudoku
        setAll@(Sudoku vs rest rows cols sqrs) = setForced sud

        ((rowCounter, colCounter):newRem) = rest
        possible :: Value
        possible = and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter))

        parFall :: [Sudoku] -> (Sudoku, Bool)
        parFall (sudo:sudos) = pair `par` (next `pseq` (if snd pair then pair else next))
            where
                next = parFall sudos
                pair = solveRecursively sudo

        parFall _ = (sudokuEMPTY, False)


main :: IO()
main = do
    f <- getArgs
    let parall = (not $ null f) && (head f `elem` ["p", "parall", "parallel", "threaded"])
    let solve = if parall then solveParallel else solveRecursively
    let r = fst $ solve $ genSudoku vals_final

    startBool <- getCurrentTime
    putStrLn $ toStr $ r
    print $ isValid r
    endBool <- getCurrentTime
    putStrLn $ "Took " ++ show (endBool `diffUTCTime` startBool)

{-
7 2 4  1 6 8  9 3 5  
8 5 6  3 9 7  1 4 2  
3 9 1  5 4 2  7 6 8  

9 7 3  6 2 1  5 8 4  
2 4 8  9 7 5  6 1 3  
6 1 5  8 3 4  2 9 7  

5 6 7  4 1 3  8 2 9  
4 8 9  2 5 6  3 7 1  
1 3 2  7 8 9  4 5 6 
-} 