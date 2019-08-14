module Sudoku where

import Data.Char (chr)

import Data.Array.Unboxed
import Data.Bits

type Value = Int
type Arr = (UArray Int Value)

data Sudoku = Sudoku [[Value]] [(Int, Int)] Arr Arr Arr

--Possible improvements:
--  Use Data.Vectors.Unboxed for the board / remeaning. Probably even worse performance

constR :: Int
constS :: Int
constALL :: Int
sudokuEMPTY :: Sudoku

--genSquares only works for 9x9 sudokus

constR = 3
constS = 9
constALL = (shift 1 constS) - 1
sudokuEMPTY = Sudoku [] [] (array (0, 0) []) (array (0, 0) []) (array (0, 0) [])

getSqrIndex :: Int -> Int -> Int
getSqrIndex row col = constR * (div row constR) + (div col constR)


{-# INLINE constR #-}
{-# INLINE constS #-}
{-# INLINE constALL #-}
{-# INLINE sudokuEMPTY #-}
{-# INLINE getSqrIndex #-}


toPow2 :: Value -> Value
{-# INLINE toPow2 #-}
toPow2 x = shift 1 (x - 1)

fromPow2 :: Value -> Value
fromPow2 x = if x == 0 then 0 else 1 + countTrailingZeros x


--Fast, it only checks if there are no values remeaning. If everything works well, there should be no problem
isFinished :: Sudoku -> Bool
{-# INLINE isFinished #-}
isFinished (Sudoku _ rem _ _ _) = null rem

--Slower, more reliable version. It checks if any of the tiles is empty (has a 0)
isFinishedSlow :: Sudoku -> Bool
isFinishedSlow (Sudoku vs _ _ _ _) = not (or (map (any (==0)) vs))

--Functions in charge of the String representation of the sudoku
prettify :: [Value] -> String
prettify (x:xs) = val : ' ' : prettify xs
    where
        val = if x == 0 then '-' else chr $ (fromPow2 x) + 48
prettify _ = ['\n']

toStr :: Sudoku -> String
toStr (Sudoku val _ _ _ _) = foldl (join) "" val
    where
        join :: String -> [Value] -> String
        join str v = str ++ prettify v


orArray :: [Value] -> Value
{-# INLINE orArray #-}
orArray = foldl (.|.) 0

andArray :: [Value] -> Value
{-# INLINE andArray #-}
andArray = foldl (.&.) constALL

--bitwise and & 3 values
and3 :: Value -> Value -> Value -> Value
{-# INLINE and3 #-}
and3 a b c = (.&.) a ((.&.) b c)

--bitwise or | 3 values
or3 :: Value -> Value -> Value -> Value
{-# INLINE or3 #-}
or3 a b c = (.|.) a ((.|.) b c)

--Generate the possible values in each row
genRows :: [[Value]] -> [Value]
genRows (x:xs) = xor constALL (orArray x) : genRows xs
genRows _ = []

--Generate the possible values in each col
genCols :: [[Value]] -> [Value]
{-# INLINE genCols #-}
genCols = genRows . transpose

--Generate the possible values in each sqr
genSqrs :: [[Value]] -> [Value]
genSqrs (x:y:z:ls) = helper x y z ++ genSqrs ls
    where
        helper :: [Value] -> [Value] -> [Value] -> [Value]
        helper (x0:x1:x2:xs) (y0:y1:y2:ys) (z0:z1:z2:zs) = xor constALL (or3 (or3 x0 x1 x2) (or3 y0 y1 y2) (or3 z0 z1 z2)) : helper xs ys zs
        helper _ _ _ = []
genSqrs _ = []

--Transposes a 2d list. Pretty much the same function from Prelude, optimised a bit
transpose :: [[Value]] -> [[Value]]
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])
transpose (_: xss)   = transpose xss
transpose _ = []

--Returns a list of tuples with the coords (row, col) of all the 0s
getZeroes :: [[Value]] -> Int -> [(Int, Int)]
getZeroes (v:vs) rowCount = getZeroesRow v 0 ++ getZeroes vs (rowCount+1)
    where
        getZeroesRow :: [Value] -> Int -> [(Int, Int)]
        getZeroesRow (0:vss) colCount = (rowCount, colCount) : getZeroesRow vss (colCount+1)
        getZeroesRow (_:vss) colCount = getZeroesRow vss (colCount+1)
        getZeroesRow _ _ = []
getZeroes _ _ = []

--Translates the representation from a sudoku into a Sudoku data struct.
--With all the values and possible calculated
genSudoku :: [[Value]] -> Sudoku
genSudoku vals = Sudoku arrayPow2 rest newRows newCols newSqrs
    where
        arrayPow2 = map (map toPow2) vals
        rest = getZeroes arrayPow2 0
        newRows = listArray (0,constS-1) (genRows arrayPow2)
        newCols = listArray (0,constS-1) (genCols arrayPow2)
        newSqrs = listArray (0,constS-1) (genSqrs arrayPow2)

--Generates a sudoku from the previous, there is no need to get the pow2
--of each element
--PRE: rest has to be updated before, notice that it doesn't change
genSudokuFromPrev :: [[Value]] -> [(Int, Int)] -> Sudoku
genSudokuFromPrev vals rest = Sudoku vals rest newRows newCols newSqrs
    where
        newRows = listArray (0,constS-1) (genRows vals)
        newCols = listArray (0,constS-1) (genCols vals)
        newSqrs = listArray (0,constS-1) (genSqrs vals)

--Updates rows / cols / sqrs after one value has been changed
--PRE: valMask is the mask of the new value xor constALL newVal
updateRCS :: Value -> Int -> Int -> Arr -> Arr -> Arr -> (Arr, Arr, Arr)
{-# INLINE updateRCS #-}
updateRCS valMask rowCount colCount oldR oldC oldS = (newR, newC, newS)
    where
        prevR = oldR ! rowCount
        prevC = oldC ! colCount
        prevS = oldS ! sqrIndex
        updatedR = (.&.) prevR valMask
        updatedC = (.&.) prevC valMask
        updatedS = (.&.) prevS valMask

        newR = listArray (0,constS-1) (bfR ++ updatedR:afR)
        (bfR, _:afR) = splitAt rowCount (elems oldR)

        newC = listArray (0,constS-1) (bfC ++ updatedC:afC)
        (bfC, _:afC) = splitAt colCount (elems oldC)

        newS = listArray (0,constS-1) (bfS ++ updatedS:afS)
        (bfS, _:afS) = splitAt sqrIndex (elems oldS)

        sqrIndex = getSqrIndex rowCount colCount

--Regenerates the sudoku after only one tile has been changed
genSudokuOneChange :: Sudoku -> Value -> Int -> Int -> Sudoku
{-# INLINE genSudokuOneChange #-}
genSudokuOneChange (Sudoku oldVs newRem oldR oldC oldS) newVal rowCount colCount = Sudoku newVs newRem newR newC newS
    where
        newVs = subIn oldVs rowCount colCount newVal
        (newR, newC, newS) = updateRCS (complement newVal) rowCount colCount oldR oldC oldS

--Extracts the smallest power of 2 of a value (9 -> 1, 10 -> 2, 192 -> 64)
isolSmallBit :: Value -> Value
{-# INLINE isolSmallBit#-}
isolSmallBit val = (.&.) val (-val)

--Changes a value in the position (r, c) in a 2d list
subIn :: [[Value]] -> Int -> Int -> Value -> [[Value]]
{-# INLINE subIn #-}
subIn vs r c val = before ++ (bf ++ val : af) : after
    where
        (before, row:after) = splitAt r vs
        (bf, _:af) = splitAt c row


--Updates all the values in the board from an array of (r, c, newValue)
updateVs :: [[Value]] -> [(Int, Int, Value)] -> Int -> [[Value]]
updateVs vs [] _ = vs
updateVs (v:vs) rest rowCount = if fstRow > rowCount --Checks if it is necessary to update any values in this row
    then
        v : updateVs vs rest (rowCount+1)
    else
        changeInRow v nRow 0 : updateVs vs rest' (rowCount+1)
    where
        (fstRow, _, _) = head rest
        splitWhile :: [(Int, Int, Value)] -> ([(Int, Int, Value)], [(Int, Int, Value)])
        splitWhile (x@(rC, _, _):xs)
            | rC == rowCount = (x : a, b)
            | True = ([], x:xs)
            where
                (a, b) = splitWhile xs
        splitWhile _ = ([], [])

        (nRow, rest') = splitWhile rest

        --Changes all the values in a row
        --The list of changes contains only the values in the row 
        changeInRow :: [Value] -> [(Int, Int, Value)] -> Int -> [Value]
        changeInRow vss [] _ = vss
        changeInRow (v':vss) rest@((_, colC, val):restRem) colCount
            | colCount == colC = val : changeInRow vss restRem (colCount+1)
            | True = v' : changeInRow vss rest (colCount+1)
        changeInRow _ _ _= []

updateVs _ _ _ = []

--Adds up all the values in a row and ensures they equal constALL
sumRows :: [[Value]] -> Bool
{-# INLINE sumRows #-}
sumRows vs = all (==constALL) (map sum vs)

sumCols :: [[Value]] -> Bool
{-# INLINE sumCols #-}
sumCols = sumRows . transpose

--TODO: Check if any values are repeated
--Checks if a sudoku is valid, the addition of all the values in any row / col / sqr == constALL
isValid :: Sudoku -> Bool
{-# INLINE isValid #-} 
isValid (Sudoku vs _ _ _ _) = sumRows vs && sumCols vs && (sum (sumSquares vs)) == constS * constALL 
    where
        sumSquares :: [[Value]] -> [Value]
        sumSquares (l1:l2:l3:ls) = sumRecurs l1 l2 l3 : sumSquares ls
        sumSquares _ = []

        sumRecurs :: [Value] -> [Value] -> [Value] -> Value
        sumRecurs (x1:x2:x3:xss) (y1:y2:y3:yss) (z1:z2:z3:zss) = x1 + x2 + x3 + y1 + y2 + y3 + z1 + z2 + z3 + sumRecurs xss yss zss
        sumRecurs _ _ _ = 0
