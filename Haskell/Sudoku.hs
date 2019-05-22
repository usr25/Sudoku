module Sudoku where

import Data.Char (chr)
import Data.List (foldl')

import Data.Array.Unboxed
import Data.Bits

type Value = Int
type Arr = (UArray Int Value)

data Sudoku = Sudoku ![[Value]] ![(Int, Int)] !Arr !Arr !Arr deriving (Eq)

constR :: Int
constS :: Int
constALL :: Int
sudokuEMPTY :: Sudoku

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

--TODO: {-# SPECIALIZE transpose :: [[Value]] -> [[Value]] #-}

prettify :: [Value] -> String
prettify (x:xs) = val : ' ' : prettify xs
    where
        val = if x == 0 then '-' else chr $ (fromPow2 x 0) + 48
prettify _ = ['\n']

isFinished :: Sudoku -> Bool
isFinished (Sudoku _ [] _ _ _) = True
isFinished _ = False

toStr :: Sudoku -> String
toStr (Sudoku val _ _ _ _) = foldl' (join) "" val
    where
        join :: String -> [Value] -> String
        join str v = str ++ prettify v


toPow2 :: Value -> Value
{-# INLINE toPow2 #-}
toPow2 x = shift 1 (x - 1)

fromPow2 :: Value -> Value -> Value
fromPow2 x acc = if x <= 0 then acc else fromPow2 (shift x (-1)) (acc + 1)

orArray :: [Value] -> Value
{-# INLINE orArray #-}
orArray = foldl' (.|.) 0

andArray :: [Value] -> Value
{-# INLINE andArray #-}
andArray = foldl' (.&.) constALL

and3 :: Value -> Value -> Value -> Value
{-# INLINE and3 #-}
and3 a b c = (.&.) a ((.&.) b c)

or3 :: Value -> Value -> Value -> Value
{-# INLINE or3 #-}
or3 a b c = (.|.) a ((.|.) b c)


genRows :: [[Value]] -> [Value]
genRows (x:xs) = xor constALL (orArray x) : genRows xs
genRows _ = []

genCols :: [[Value]] -> [Value]
{-# INLINE genCols #-}
genCols = genRows . transpose

genSqrs :: [[Value]] -> [Value]
genSqrs (x:y:z:ls) = helper x y z ++ genSqrs ls
    where
        helper :: [Value] -> [Value] -> [Value] -> [Value]
        helper (x0:x1:x2:xs) (y0:y1:y2:ys) (z0:z1:z2:zs) = xor constALL (or3 (or3 x0 x1 x2) (or3 y0 y1 y2) (or3 z0 z1 z2)) : helper xs ys zs
        helper _ _ _ = []
genSqrs _ = []

transpose :: [[Value]] -> [[Value]]
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
transpose (_: xss)   = transpose xss
transpose _ = []

getZeroes :: [[Value]] -> Int -> [(Int, Int)]
getZeroes (v:vs) rowCount = getZeroesRow v 0 ++ getZeroes vs (rowCount+1)
    where
        getZeroesRow :: [Value] -> Int -> [(Int, Int)]
        getZeroesRow (0:vss) colCount = (rowCount, colCount) : getZeroesRow vss (colCount+1)
        getZeroesRow (_:vss) colCount = getZeroesRow vss (colCount+1)
        getZeroesRow _ _ = []
getZeroes _ _ = []

genSudoku :: [[Value]] -> Sudoku
genSudoku vals = Sudoku arrayPow2 rest newRows newCols newSqrs
    where
        arrayPow2 = map (map toPow2) vals
        rest = getZeroes arrayPow2 0
        newRows = listArray (0,constS-1) (genRows arrayPow2)
        newCols = listArray (0,constS-1) (genCols arrayPow2)
        newSqrs = listArray (0,constS-1) (genSqrs arrayPow2)


--PRE: rest has to be updated before
genSudokuFromPrev :: [[Value]] -> [(Int, Int)] -> Sudoku
genSudokuFromPrev vals rest = Sudoku vals rest newRows newCols newSqrs
    where
        newRows = listArray (0,constS-1) (genRows vals)
        newCols = listArray (0,constS-1) (genCols vals)
        newSqrs = listArray (0,constS-1) (genSqrs vals)

updateRowsColsSqrs :: Value -> Int -> Int -> Arr -> Arr -> Arr -> (Arr, Arr, Arr)
updateRowsColsSqrs valMask rowCount colCount oldR oldC oldS = (newR, newC, newS)
    where
        
        newR = listArray (0,constS-1) (bfR ++ ((.&.) prevR valMask):afR)
        (bfR, prevR:afR) = splitAt rowCount (elems oldR)

        newC = listArray (0,constS-1) (bfC ++ ((.&.) prevC valMask):afC)
        (bfC, prevC:afC) = splitAt colCount (elems oldC)

        newS = listArray (0,constS-1) (bfS ++ ((.&.) prevS valMask):afS)
        (bfS, prevS:afS) = splitAt (getSqrIndex rowCount colCount) (elems oldS)

genSudokuOneChange :: Sudoku -> Value -> Int -> Int -> Sudoku
genSudokuOneChange (Sudoku oldVs newRem oldR oldC oldS) newVal rowCount colCount = Sudoku newVs newRem newR newC newS
    where
        valMask :: Value
        valMask = xor constALL newVal
        newVs = subIn oldVs rowCount colCount newVal
        
        (newR, newC, newS) = updateRowsColsSqrs valMask rowCount colCount oldR oldC oldS


sumRows :: [[Value]] -> Bool
{-# INLINE sumRows #-}
sumRows vs = all (==constALL) res && sum res == constS * constALL
    where
        res = map sum vs

subIn :: [[Value]] -> Int -> Int -> Value -> [[Value]]
{-# INLINE subIn #-}
subIn vs r c val = before ++ (bf ++ val : af) : after
    where
        (before, row:after) = splitAt r vs
        (bf, _:af) = splitAt c row

{-
splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f (x:xs)
    | f x = (x : a, b)
    | True = ([], x:xs)
    where
        (a, b) = splitWhile f xs
splitWhile _ [] = ([], [])


updateVs :: [[Value]] -> [(Int, Int, Value)] -> [[Value]]
updateVs vs ((rowCount, colCount, val):rest) = updateVs (subIn vs rowCount colCount val) rest
updateVs vs _ = vs
-}

updateVs :: [[Value]] -> [(Int, Int, Value)] -> Int -> [[Value]]
updateVs vs [] _ = vs
updateVs (v:vs) rest rowCount = changeInRow v nRow 0 : updateVs vs rest' (rowCount+1)
    where
        splitWhile :: [(Int, Int, Value)] -> ([(Int, Int, Value)], [(Int, Int, Value)])
        splitWhile (x@(rC, _, _):xs)
            | rC == rowCount = (x : a, b)
            | True = ([], x:xs)
            where
                (a, b) = splitWhile xs
        splitWhile _ = ([], [])

        (nRow, rest') = splitWhile rest

        changeInRow :: [Value] -> [(Int, Int, Value)] -> Int -> [Value]
        changeInRow vss [] _ = vss
        changeInRow (v':vss) rest@((_, colC, val):restRem) colCount
            | colCount == colC = val : changeInRow vss restRem (colCount+1)
            | True = v' : changeInRow vss rest (colCount+1)
        changeInRow _ _ _= []
updateVs _ _ _ = []

isValid :: Sudoku -> Bool
{-# INLINE isValid #-} 
isValid (Sudoku vs _ _ _ _) = sumRows vs && (sumRows . transpose) vs && sum ssqrs == (constALL * constS)
    where
        ssqrs :: [Value]
        {-# INLINE ssqrs #-}
        ssqrs = sumSquares vs 

        sumSquares :: [[Value]] -> [Value]
        sumSquares (l1:l2:l3:ls) = sumRecurs l1 l2 l3 : sumSquares ls
        sumSquares _ = []
        
        sumRecurs :: [Value] -> [Value] -> [Value] -> Value
        sumRecurs (x1:x2:x3:xss) (y1:y2:y3:yss) (z1:z2:z3:zss) = x1 + x2 + x3 + y1 + y2 + y3 + z1 + z2 + z3 + sumRecurs xss yss zss
        sumRecurs _ _ _ = 0

