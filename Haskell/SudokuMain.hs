module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Maybe (fromJust)

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


--Calls setForced while setForced makes changes
setAllForced :: Sudoku -> Sudoku
setAllForced sud = if somethingChanged then setAllForced next else next
    where
        (next, somethingChanged) = setForced sud

--Sets all the forced values for a sudoku. 
--eg.: The only possible value in a tile is 1, it sets the value as 1 
setForced :: Sudoku -> (Sudoku, Bool)
setForced (Sudoku vs rest rows cols sqrs) = (Sudoku (updateVs vs totalChanges 0) finalRemeaning finalR finalC finalS, not (null totalChanges)) 
    where
        helper :: [(Int, Int)] -> Arr -> Arr -> Arr -> ([(Int, Int, Value)], [(Int, Int)], Arr, Arr, Arr)
        helper (tup@(rowCounter, colCounter):rest) rows cols sqrs = if popCount possible == 1 then ((rowCounter, colCounter, possible):changed', newRem', lastR', lastC', lastS') else (changed, tup:newRem, lastR, lastC, lastS) 
            where
                possible = and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter))
                
                (changed, newRem, lastR, lastC, lastS) =  helper rest rows cols sqrs
                (changed', newRem', lastR', lastC', lastS') =  helper rest newR newC newS
                
                (newR, newC, newS) = updateRCS (xor constALL possible) rowCounter colCounter rows cols sqrs
        
        helper _ rows cols sqrs = ([], [], rows, cols, sqrs)
    
        (totalChanges, finalRemeaning, finalR, finalC, finalS) = helper rest rows cols sqrs

--Returns False if an empty tile has no possible values
canBeFinished :: Sudoku -> Bool
canBeFinished (Sudoku _ rest rows cols sqrs) = helper rest
    where
        helper :: [(Int, Int)] -> Bool
        helper ((rowCounter, colCounter):rems) = if and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter)) == 0 then False else helper rems
        helper _ = True


--Generates a list with all the possible values in a tile
trySudokus :: Sudoku -> Int -> Int -> Value -> [Sudoku]
trySudokus _ _ _ 0 = []
trySudokus sud rowCounter colCounter val = result : trySudokus sud rowCounter colCounter (xor val getVal)
    where
        result = genSudokuOneChange sud getVal rowCounter colCounter
        getVal = getNextVal val 1

--Performs a DFS on the possible sudokus, until a valid one is found
fall :: [Sudoku] -> (Sudoku, Bool)
fall (sudo:sudos) = if pos then (res, True) else fall sudos
    where
        (res, pos) = solveRecursively sudo
fall _ = (sudokuEMPTY, False)

--In charge of calling setAllForced / fall in order, starting point of the program
solveRecursively :: Sudoku -> (Sudoku, Bool)
solveRecursively sud = if not (canBeFinished setAll) then (sudokuEMPTY, False) else if isFinished setAll then (setAll, True) else fall (trySudokus (Sudoku vs newRem rows cols sqrs) rowCounter colCounter possible)
    where
        setAll :: Sudoku
        setAll@(Sudoku vs rest rows cols sqrs) = setAllForced sud
        
        ((rowCounter, colCounter):newRem) = rest
        possible = and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter))


main :: IO()
main = do

    let easy = fst $ solveRecursively $ genSudoku vals_easy
    let tree = fst $ solveRecursively $ genSudoku vals_tree
    let r = fst $ solveRecursively $ genSudoku vals_final

    print $ isValid $ easy
    print $ isValid $ tree

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