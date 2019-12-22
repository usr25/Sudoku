module Sudoku where

import Data.Char (chr)
import Data.List (transpose, intersperse, intercalate)

--An improvement would be to use mutable vectors, but thats too far from haskell
import qualified Data.Vector.Unboxed as V
import Data.Bits

type Value = Int
type Vec = (V.Vector Value)

data Sudoku = Sudoku [Value] [Int] Vec Vec Vec

--genSquares only works for 9x9 sudokus

constALL = (shift 1 9) - 1
sudokuEMPTY = Sudoku [] [] V.empty V.empty V.empty

row :: Int -> Int
{-# INLINE row #-}
row i = i `div` 9

col :: Int -> Int
{-# INLINE col #-}
col i = i `rem` 9

getSqrIndex :: Int -> Int -> Int
getSqrIndex row col = 3 * (div row 3) + (div col 3)

getSqrIndex1 :: Int -> Int
{-# INLINE getSqrIndex1 #-}
getSqrIndex1 i = 3 * (div i 27) + (div (col i) 3)

toPow2 :: Value -> Value
{-# INLINE toPow2 #-}
toPow2 x = shift 1 (x - 1)

fromPow2 :: Value -> Value
fromPow2 x = if x == 0 then 0 else 1 + countTrailingZeros x

toChr :: Value -> Char
toChr x = chr $ 48 + fromPow2 x

--Fast, it only checks if there are no values remeaning. If everything works well, there should be no problem
isFinished :: Sudoku -> Bool
{-# INLINE isFinished #-}
isFinished (Sudoku _ rem _ _ _) = null rem

--Slower, more reliable version. It checks if any of the tiles is empty (has a 0)
isFinishedSlow :: Sudoku -> Bool
isFinishedSlow (Sudoku vs _ _ _ _) = not $ any (==0) vs

prettify :: String -> String
prettify str = intercalate "\n" $ map (intersperse ' ' . reverse) $ splitEvery 9 [] str

toStr :: Sudoku -> String
toStr (Sudoku val _ _ _ _) = map toChr val

splitEvery :: Int -> [a] -> [a] -> [[a]]
splitEvery 0 acc xs = acc : splitEvery 9 [] xs
splitEvery c acc (x:xs) = splitEvery (c-1) (x:acc) xs
splitEvery _ _ _ = []

orArray :: [Value] -> Value
{-# INLINE orArray #-}
orArray = foldl1 (.|.)

andArray :: [Value] -> Value
{-# INLINE andArray #-}
andArray = foldl1 (.&.)

--bitwise and & 3 values
and3 :: Value -> Value -> Value -> Value
{-# INLINE and3 #-}
and3 a b c = (.&.) a $ (.&.) b c

--bitwise or | 3 values
or3 :: Value -> Value -> Value -> Value
{-# INLINE or3 #-}
or3 a b c = (.|.) a $ (.|.) b c

--Extracts the smallest power of 2 of a value (9 -> 1, 10 -> 2, 192 -> 64)
isolSmallBit :: Value -> Value
{-# INLINE isolSmallBit#-}
isolSmallBit val = (.&.) val (-val)

--Adds up all the values in a row and ensures they equal constALL
sumRows :: [[Value]] -> Bool
{-# INLINE sumRows #-}
sumRows vs = all (==constALL) (map sum vs)

sumCols :: [[Value]] -> Bool
sumCols = sumRows . transpose

--TODO: Check if any values are repeated
--Checks if a sudoku is valid, the addition of all the values in any row / col / sqr == constALL
isValid :: Sudoku -> Bool
{-# INLINE isValid #-} 
isValid (Sudoku vs _ _ _ _) = sumRows twoD && sumCols twoD && all (==constALL*3) (sumSquares twoD)
    where
        twoD = splitEvery 9 [] vs

        sumSquares :: [[Value]] -> [Value]
        sumSquares (l1:l2:l3:ls) = sumRecurs l1 l2 l3 : sumSquares ls
        sumSquares _ = []

        sumRecurs :: [Value] -> [Value] -> [Value] -> Value
        sumRecurs (x1:x2:x3:xss) (y1:y2:y3:yss) (z1:z2:z3:zss) = x1 + x2 + x3 + y1 + y2 + y3 + z1 + z2 + z3 + sumRecurs xss yss zss
        sumRecurs _ _ _ = 0

--Generate the possible values in each row
genRows :: [[Value]] -> [Value]
genRows (x:xs) = xor constALL (orArray x) : genRows xs
genRows _ = []

--Generate the possible values in each col
genCols :: [[Value]] -> [Value]
genCols = genRows . transpose

--Generate the possible values in each sqr
genSqrs :: [[Value]] -> [Value]
genSqrs (x:y:z:ls) = helper x y z ++ genSqrs ls
    where
        helper :: [Value] -> [Value] -> [Value] -> [Value]
        helper (x0:x1:x2:xs) (y0:y1:y2:ys) (z0:z1:z2:zs) = xor constALL (or3 (or3 x0 x1 x2) (or3 y0 y1 y2) (or3 z0 z1 z2)) : helper xs ys zs
        helper _ _ _ = []
genSqrs _ = []

--Returns a list of tuples with the coords (row, col) of all the 0s
getZeroes :: [Value] -> Int -> [Int]
getZeroes (x:xs) c = 
    if x == 0
        then c : getZeroes xs (c+1)
        else getZeroes xs (c+1)
getZeroes _ _ = []

--Translates the representation from a sudoku into a Sudoku data struct.
--With all the values and possible calculated
genSudoku :: [[Value]] -> Sudoku
genSudoku vals = Sudoku array rest newRows newCols newSqrs
    where
        arrayPow2D = map (map toPow2) vals
        array = foldl (++) [] arrayPow2D
        rest = getZeroes array 0
        newRows = V.fromList (genRows arrayPow2D)
        newCols = V.fromList (genCols arrayPow2D)
        newSqrs = V.fromList (genSqrs arrayPow2D)

--Updates rows / cols / sqrs after one value has been changed
--PRE: valMask is the mask of the new value xor constALL newVal
updateRCS :: Value -> Int -> Int -> Vec -> Vec -> Vec -> (Vec, Vec, Vec)
{-# INLINE updateRCS #-}
updateRCS valMask rowCount colCount oldR oldC oldS = (newR, newC, newS)
    where
        prevR = oldR V.! rowCount
        prevC = oldC V.! colCount
        prevS = oldS V.! sqrCount
        updatedR = (.&.) prevR valMask
        updatedC = (.&.) prevC valMask
        updatedS = (.&.) prevS valMask

        newR = V.update oldR (V.singleton (rowCount, updatedR))
        newC = V.update oldC (V.singleton (colCount, updatedC))
        newS = V.update oldS (V.singleton (sqrCount, updatedS))

        sqrCount = getSqrIndex rowCount colCount

--Regenerates the sudoku after only one tile has been changed
genSudokuOneChange :: Sudoku -> Value -> Int -> Sudoku
{-# INLINE genSudokuOneChange #-}
genSudokuOneChange (Sudoku oldVs newRem oldR oldC oldS) newVal index = Sudoku newVs newRem newR newC newS
    where
        newVs = subIn oldVs index newVal
        (newR, newC, newS) = updateRCS (complement newVal) (row index) (col index) oldR oldC oldS

--Changes a value in the given position
subIn :: [Value] -> Int -> Value -> [Value]
subIn (_:xs) 0 val = val : xs
subIn (x:xs) c val = x : subIn xs (c-1) val

--Updates all the values in the board from an array of (index, newValue)
updateVs :: [Value] -> [(Int, Value)] -> Int -> [Value]
updateVs vs [] _ = vs
updateVs (v:vs) rest@((i,nv):rs) index = 
    if index == i
        then nv : updateVs vs rs (index+1)
        else v : updateVs vs rest (index+1)
updateVs _ _ _ = []

--Returns False if an empty tile has no possible values
cantFinish :: Sudoku -> Bool
cantFinish (Sudoku _ rest rows cols sqrs) = any isNotPossible rest
    where
        isNotPossible :: Int -> Bool
        isNotPossible index = 0 == (and3 (rows V.! (row index)) (cols V.! (col index)) (sqrs V.! (getSqrIndex1 index)))

--Sets all the forced values for a sudoku. 
--eg.: The only possible value in a tile is 1, it sets the value as 1 
setForced :: Sudoku -> Sudoku
setForced (Sudoku vs rest rows cols sqrs) = Sudoku (updateVs vs totalChanges 0) finalRemeaning finalR finalC finalS
    where
        (totalChanges, finalRemeaning, finalR, finalC, finalS) = helper rest rows cols sqrs

        helper :: [Int] -> Vec -> Vec -> Vec -> ([(Int, Value)], [Int], Vec, Vec, Vec)
        helper (index:rest) rows cols sqrs =
            if popCount possible == 1
                then ((index, possible):changed', newRem', lastR', lastC', lastS')
                else (changed, index:newRem, lastR, lastC, lastS)
            where
                possible = and3 (rows V.! (row index)) (cols V.! (col index)) (sqrs V.! (getSqrIndex1 index))

                (changed, newRem, lastR, lastC, lastS) =  helper rest rows cols sqrs
                (changed', newRem', lastR', lastC', lastS') =  helper rest newR newC newS

                (newR, newC, newS) = updateRCS (complement possible) (row index) (col index) rows cols sqrs
        helper _ rows cols sqrs = ([], [], rows, cols, sqrs)

--Generates a list with all the Sudokus generated from the possible values in a tile
trySudokus :: Sudoku -> Int -> Value -> [Sudoku]
trySudokus sud index val = helper val
    where
        helper :: Value -> [Sudoku]
        helper val' =
            if val' == 0
                then []
                else result : trySudokus sud index (xor val getVal)
            where
                result = genSudokuOneChange sud getVal index
                getVal = isolSmallBit val'

--Performs a DFS on the possible sudokus, until a valid one is found
fall :: [Sudoku] -> (Sudoku, Bool)
fall (sudo:sudos) = if pos then (res, True) else fall sudos
    where
        (res, pos) = solveRecursively sudo
fall _ = (sudokuEMPTY, False)

--In charge of calling setForced / fall in order, starting point of the program
solveRecursively :: Sudoku -> (Sudoku, Bool)
solveRecursively sud =
    if cantFinish setAll
        then (sudokuEMPTY, False)
        else
            if isFinished setAll
                then (setAll, True)
                else fall $ trySudokus (Sudoku vs newRem rows cols sqrs) index possible
    where
        setAll :: Sudoku
        setAll@(Sudoku vs rest rows cols sqrs) = setForced sud

        (index:newRem) = rest
        possible :: Value
        possible = and3 (rows V.! (row index)) (cols V.! (col index)) (sqrs V.! (getSqrIndex1 index))
