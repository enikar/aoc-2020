-- AoC 2020, day 1. Rewritten on January 20, 2025

-- The aim of the puzzle is to find 2 then 3 numbers whose sum is 2020
-- from the list of numbers contained in the file "day1.txt" and compute
-- their products.

module Main  where

import System.IO (readFile')
import qualified Data.Vector.Unboxed as V -- package vector
import Data.Vector.Unboxed
  (Vector
  ,(!)
  )
import qualified Data.IntSet as S  -- package containers
import Data.Functor ((<&>))
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Heap (sort) -- package vector-algorithms
import Control.Monad.Extra (loop) -- package extra


-- The input file "day1.txt" contains one number per line.
-- So we read all lines, and convert each string to an Int.
readDatas :: IO (Vector Int)
readDatas = do
  content <- readFile' "day1.txt"
  let errorRead = error "ReadDatas: not an Int in day1.txt."
      readInt = fromMaybe errorRead . readMaybe
      numbers = map readInt (words content)
      -- Sort the vector makes the part1 and part2 faster.
      vec = runST $ do
        v <- V.unsafeThaw (V.fromList numbers)
        sort v
        V.unsafeFreeze v
  pure vec

main :: IO ()
main = do
  datas <- readDatas
  let combs = combinationsOpt 2 datas
  printSolution "Part1" (part1 combs)
  printSolution "Part2" (part2' datas combs)

printSolution :: String -> [Int] -> IO ()
printSolution part sol =
  putStrLn $ part <> ": numbers: " <> show sol <> ".  Product: " <> show p
      where
        p = product sol

-- Part1 returns a list of 2 Ints whose sum is 2020.
part1 :: [[Int]] -> [Int]
part1 = fromMaybe showError . find  ((==2020) . sum)
  where
    showError = error "Internal error: part1"

-- part2 returns a list of 3 Ints whose sum is 2020.
-- Using foldr to short-circuit. Foldr stops looping when we
-- we don't use acc anymore.
part2 :: Vector Int -> [[Int]] -> [Int]
part2 datas = foldr f []
  where
    numbers = S.fromAscList (V.toList datas)
    f comb acc
      |S.member n numbers = n:comb
      |otherwise          = acc
        where n = 2020 - sum comb

-- With loop from Control.Monad.Extra the short-circuit is more
-- explicit. We leave the loop when when we return a Right, and
-- continue looping as long as we return Left.
part2' :: Vector Int -> [[Int]] -> [Int]
part2' datas = loop search
  where
    numbers = S.fromAscList (V.toList datas)

    search [] = error "Error: part2: no solution found"
    search (comb:combs)
      | S.member n numbers = Right (n:comb)
      | otherwise          = Left combs
        where
          n = 2020 - sum comb

-- combinationsOpt creates a list of all combinations of k Ints from the
-- input vector vec. We proceed by eploring all possibilities (it is
-- a backtracking algorithm).
combinationsOpt :: Int -> Vector Int -> [[Int]]
combinationsOpt k vec
  | k < 0      = error "combinationsOpt: bad arguments"
  | otherwise  = go 0 1 []
    where
      n = V.length vec - 1
      go start depth comb
        | depth == k = [start..n] <&> consComb
        | otherwise  = [start..n] >>= \i -> go (i+1) (depth+1) (consComb i)
               where
                 consComb i = vec ! i : comb
