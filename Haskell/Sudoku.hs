module Sudoku where

import Data.Char (chr)
import Data.List (foldl', transpose)
import Data.Array.ST (STUArray)
import Data.Bits

type Value = Int

data Sudoku = Sudoku ![[Value]] ![Value] ![Value] ![Value] deriving (Eq)

r :: Int
s :: Int
aLL :: Int

r = 3
s = 9
aLL = (shift 1 s) - 1

getSqrIndex :: Int -> Int -> Int
getSqrIndex row col = r * (div row r) + (div col r)

isPow2 :: Value -> Bool
isPow2 v = (((.&.) (v - 1) v) == 0) && v /= 0


{-# INLINE r #-}
{-# INLINE s #-}
{-# INLINE aLL #-}
{-# INLINE getSqrIndex #-}
{-# INLINE isPow2 #-}

--TODO: {-# SPECIALIZE transpose :: [[Value]] -> [[Value]] #-}

prettify :: [Value] -> String
prettify (x:xs) = val : ' ' : prettify xs
    where
        val = if x == 0 then '-' else chr $ (fromPow2 x 0) + 48
prettify _ = ['\n']

isFinished :: Sudoku -> Bool
isFinished (Sudoku v _ _ _) = not (or (map (any (==0)) v))

toStr :: Sudoku -> String
toStr (Sudoku val _ _ _) = foldl' (join) "" val
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
andArray = foldl' (.&.) aLL

orRows :: [[Value]] -> [Value]
orRows (x:xs) = orArray x : orRows xs
orRows _ = []

orCols :: [[Value]] -> [Value]
{-# INLINE orCols #-}
orCols = orRows . transpose

orSqrs :: [[Value]] -> [Value]
orSqrs (x:y:z:xs) = helper x y z ++ orSqrs xs
    where
        helper :: [Value] -> [Value] -> [Value] -> [Value]
        helper (x0:x1:x2:xs) (y0:y1:y2:ys) (z0:z1:z2:zs) = orArray [x0, x1, x2, y0, y1, y2, z0, z1, z2] : helper xs ys zs
        helper _ _ _ = []
orSqrs _ = []

flipList :: [Value] -> [Value]
{-# INLINE flipList #-}
flipList = map (xor (aLL))

genSudoku :: [[Value]] -> Sudoku
genSudoku vals = Sudoku arrayPow2 flipRows flipCols flipSqrs
    where
        arrayPow2 = map (map toPow2) vals
        flipRows = flipList (orRows arrayPow2)
        flipCols = flipList (orCols arrayPow2)
        flipSqrs = flipList (orSqrs arrayPow2)

and3 :: Value -> Value -> Value -> Value
{-# INLINE and3 #-}
and3 a b c = (.&.) a ((.&.) b c)

genSudokuFromPrev :: [[Value]] -> Sudoku
genSudokuFromPrev vals = Sudoku vals flipRows flipCols flipSqrs
    where
        flipRows = flipList (orRows vals)
        flipCols = flipList (orCols vals)
        flipSqrs = flipList (orSqrs vals)


sumRows :: [[Value]] -> Bool
sumRows vs = all (==aLL) res && sum res == s * aLL
    where
        res = map (foldl (+) 0) vs


isValid :: Sudoku -> Bool
{-# INLINE isValid #-} 
isValid (Sudoku vs _ _ _) = sumRows vs && (sumRows . transpose) vs && sum ssqrs == (aLL * s)
    where
    ssqrs = sumSquares vs 

    sumSquares :: [[Value]] -> [Value]
    sumSquares (l1:l2:l3:ls) = sumRecurs l1 l2 l3 : sumSquares ls
    sumSquares _ = []
    
    sumRecurs :: [Value] -> [Value] -> [Value] -> Value
    sumRecurs (x1:x2:x3:xss) (y1:y2:y3:yss) (z1:z2:z3:zss) = x1 + x2 + x3 + y1 + y2 + y3 + z1 + z2 + z3 + sumRecurs xss yss zss
    sumRecurs _ _ _ = 0