module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Maybe (fromJust)
import Data.List (splitAt)

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



solve :: Sudoku -> Sudoku
solve (Sudoku vals rows cols sqrs) = if somethingChanged then solve regenerate else regenerate
    where
        (next, somethingChanged) = setAllPossibleB vals rows cols sqrs 0
        regenerate = genSudokuFromPrev next


setAllPossible :: [[Value]] -> UArray Int Value -> UArray Int Value -> UArray Int Value -> Int -> [[Value]]
setAllPossible (v:vs) rows cols sqrs rowCounter = sr : setAllPossible vs rows cols sqrs (rowCounter+1)
    where
        sr = setAllPossibleRow v rowCounter 0

        setAllPossibleRow :: [Value] -> Int -> Int -> [Value]
        setAllPossibleRow (0:vs) rowCounter colCounter = if isPow2 value then value : next else 0 : next
            where
                value = and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter))
                next = setAllPossibleRow vs rowCounter (colCounter+1)

        setAllPossibleRow (v:vs) rowCounter colCounter = v : setAllPossibleRow vs rowCounter (colCounter+1)
        setAllPossibleRow _ _ _ = []

setAllPossible _ _ _ _ _ = []

setAllPossibleB :: [[Value]] -> UArray Int Value -> UArray Int Value -> UArray Int Value -> Int -> ([[Value]], Bool)
setAllPossibleB (v:vs) rows cols sqrs rowCounter = if somethingChangedNow then (sr : faster, True) else (sr : next, somethingChangedFollowing)
    where
        (next, somethingChangedFollowing) = setAllPossibleB vs rows cols sqrs (rowCounter+1)
        (sr, somethingChangedNow) = setAllPossibleRowB v rowCounter 0
        faster = setAllPossible vs rows cols sqrs (rowCounter+1)

        setAllPossibleRowB :: [Value] -> Int -> Int -> ([Value], Bool)
        setAllPossibleRowB (0:vs) rowCounter colCounter = if isPow2 value then (value : next, True) else (0 : next, b)
            where
                (next, b) = setAllPossibleRowB vs rowCounter (colCounter+1)
                value = and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter))

        setAllPossibleRowB (v:vs) rowCounter colCounter = (v : next, b)
            where
                (next, b) = setAllPossibleRowB vs rowCounter (colCounter+1)

        setAllPossibleRowB _ _ _ = ([], False)

setAllPossibleB _ _ _ _ _ = ([], False)


canBeFinished :: Sudoku -> Bool
canBeFinished (Sudoku vs rows cols sqrs) = helper vs 0
    where
        helper (v:vs) rowCounter = helper' v rowCounter 0 && helper vs (rowCounter+1)
        helper _ _ = True

        helper' (0:vs) rowCounter colCounter = and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter)) /= 0 && next
            where
                next = helper' vs rowCounter (colCounter+1)
        helper'(_:vs) rowCounter colCounter = next
            where
                next = helper' vs rowCounter (colCounter+1)
        helper' _ _ _ = True


getNextVal :: Value -> Int -> Value
getNextVal val c = if ((.&.) val c) == c then c else getNextVal val (shift c 1)


findFirstZero :: [[Value]] -> UArray Int Value -> UArray Int Value -> UArray Int Value -> Int -> (Int, Int, Value)
findFirstZero (v:vs) rows cols sqrs rowCounter = if calc == Nothing then findFirstZero vs rows cols sqrs (rowCounter+1) else fromJust calc 
    where
        helper :: [Value] -> Int -> Maybe (Int, Int, Value)
        helper (0:vss) colCounter = Just (rowCounter, colCounter, andAll)
            where
                andAll = and3 (rows ! rowCounter) (cols ! colCounter) (sqrs ! (getSqrIndex rowCounter colCounter))
        helper (_:vss) colCounter = helper vss (colCounter+1)
        helper _ _ = Nothing 
        
        calc = helper v 0


trySudokus :: Sudoku -> Int -> Int -> Value -> [Sudoku]
trySudokus _ _ _ 0 = []
trySudokus sud rowCounter colCounter val = result : trySudokus sud rowCounter colCounter (xor val getVal)
    where
        result = genSudokuOneChange sud getVal rowCounter colCounter
        getVal = getNextVal val 1


fall :: [Sudoku] -> (Sudoku, Bool)
fall (sudo:sudos) = if pos then (res, True) else fall sudos
    where
        (res, pos) = solveRecursively sudo
fall _ = (sudokuEMPTY, False)


solveRecursively :: Sudoku -> (Sudoku, Bool)
solveRecursively sud = if not (canBeFinished setAll) then (sudokuEMPTY, False) else if isFinished setAll then (setAll, isValid setAll) else fall (trySudokus setAll rowCounter colCounter possible)
    where
        setAll@(Sudoku vs rs cs ss) = solve sud
        (rowCounter, colCounter, possible) = findFirstZero vs rs cs ss 0


main :: IO()
main = do
    let r = fst $ solveRecursively $ genSudoku vals_final

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