module Main where

import Data.Bits (xor, (.|.), (.&.), shift)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Maybe (fromJust)
import Data.List (splitAt)

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


general = genSudoku vals
general_tree = genSudoku vals_tree
final = genSudoku vals_final

solve :: Sudoku -> Sudoku
solve (Sudoku vals rows cols sqrs) = if somethingChanged then solve regenerate else regenerate
    where
        (next, somethingChanged) = setAllPossibleB vals rows cols sqrs 0
        regenerate = genSudokuFromPrev next


setAllPossible :: [[Value]] -> [Value] -> [Value] -> [Value] -> Int -> [[Value]]
setAllPossible (v:vs) (rowVal:rs) cols sqrs rowCounter = sr : setAllPossible vs rs cols sqrs (rowCounter+1)
    where
        sr = setAllPossibleRow v rowVal cols sqrs rowCounter 0
setAllPossible _ _ _ _ _ = []

setAllPossibleRow :: [Value] -> Value -> [Value] -> [Value] -> Int -> Int -> [Value]
setAllPossibleRow (0:vs) rowVal (colVal:cs) sqrs rowCounter colCounter = if isPow2 value then value : next else 0 : next
    where
        value = and3 rowVal colVal (sqrs !! (getSqrIndex rowCounter colCounter))
        next = setAllPossibleRow vs rowVal cs sqrs rowCounter (colCounter+1)

setAllPossibleRow (v:vs) rowVal (_:cs) sqrs rowCounter colCounter = v : setAllPossibleRow vs rowVal cs sqrs rowCounter (colCounter+1)
setAllPossibleRow _ _ _ _ _ _ = []

setAllPossibleB :: [[Value]] -> [Value] -> [Value] -> [Value] -> Int -> ([[Value]], Bool)
setAllPossibleB (v:vs) (rowVal:rs) cols sqrs rowCounter = if somethingChangedNow then (sr : faster, True) else (sr : next, somethingChangedFollowing)
    where
        (next, somethingChangedFollowing) = setAllPossibleB vs rs cols sqrs (rowCounter+1)
        (sr, somethingChangedNow) = setAllPossibleRowB v rowVal cols sqrs rowCounter 0
        faster = setAllPossible vs rs cols sqrs (rowCounter+1)
setAllPossibleB _ _ _ _ _ = ([], False)

setAllPossibleRowB :: [Value] -> Value -> [Value] -> [Value] -> Int -> Int -> ([Value], Bool)
setAllPossibleRowB (0:vs) rowVal (colVal:cs) sqrs rowCounter colCounter = if isPow2 value then (value : faster, True) else (0 : next, b)
    where
        (next, b) = setAllPossibleRowB vs rowVal cs sqrs rowCounter (colCounter+1)
        value = and3 rowVal colVal (sqrs !! (getSqrIndex rowCounter colCounter))
        faster = setAllPossibleRow vs rowVal cs sqrs rowCounter (colCounter+1)

setAllPossibleRowB (v:vs) rowVal (_:cs) sqrs rowCounter colCounter = (v : next, b)
    where
        (next, b) = setAllPossibleRowB vs rowVal cs sqrs rowCounter (colCounter+1)

setAllPossibleRowB _ _ _ _ _ _ = ([], False)


canBeFinished :: Sudoku -> Bool
canBeFinished (Sudoku vs rows cols sqrs) = helper vs rows 0
    where
        helper (v:vs) (r:rs) rowCounter = helper' v r cols rowCounter 0 && helper vs rs (rowCounter+1)
        helper _ _ _ = True

        helper' (0:vs) rowAv (colAv:cs) rowCounter colCounter = and3 rowAv colAv (sqrs !! (getSqrIndex rowCounter colCounter)) /= 0 && next
            where
                next = helper' vs rowAv cs rowCounter (colCounter+1)
        helper'(_:vs) rowAv (_:cs) rowCounter colCounter = next
            where
                next = helper' vs rowAv cs rowCounter (colCounter+1)
        helper' _ _ _ _ _ = True


getNextVal :: Value -> Int -> Value
getNextVal val c = if ((.&.) val c) == c then c else getNextVal val (shift c 1)


findFirstZero :: [[Value]] -> [Value] -> [Value] -> [Value] -> Int -> (Int, Int, Value, Bool)
findFirstZero (v:vs) (rowAv:rows) cols sqrs rowCounter = if calc == Nothing then findFirstZero vs rows cols sqrs (rowCounter+1) else fromJust calc 
    where
        helper :: [Value] -> [Value] -> Int -> Maybe (Int, Int, Value, Bool)
        helper (0:vss) (colAv:css) colCounter = Just (rowCounter, colCounter, andAll, andAll /= 0)
            where
                andAll = and3 rowAv colAv (sqrs !! (getSqrIndex rowCounter colCounter))
        helper (_:vss) (_:css) colCounter = helper vss css (colCounter+1)
        helper _ _ _ = Nothing 
        
        calc = helper v cols 0


trySudokus :: [[Value]] -> Int -> Int -> Value -> [[[Value]]]
trySudokus _ _ _ 0 = []
trySudokus vs row col val = result : trySudokus vs row col (xor val getVal)
    where
        result = subIn vs row col getVal
        getVal = getNextVal val 1


fall :: [[[Value]]] -> (Sudoku, Bool)
fall (v:vs) = if pos && isValid res then (res, True) else fall vs
    where
        sudo = genSudokuFromPrev v
        (res, pos) = solveRecursively sudo
fall _ = (Sudoku [] [] [] [], False)


solveRecursively :: Sudoku -> (Sudoku, Bool)
solveRecursively sud = if not (canBeFinished setAll) then (Sudoku [] [] [] [], False) else if isFinished setAll then (setAll, True) else if usable then fall (trySudokus vs row col possible) else (Sudoku [] [] [] [], False)
    where
        setAll@(Sudoku vs rs cs ss) = solve sud
        (row, col, possible, usable) = findFirstZero vs rs cs ss 0


subIn :: [[Value]] -> Int -> Int -> Value -> [[Value]]
subIn vs r c val = before ++ (bf ++ val : af) : after
    where
        (before, row:after) = splitAt r vs
        (bf, _:af) = splitAt c row

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