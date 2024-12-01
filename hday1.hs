
module Main  where

import Data.Vector.Unboxed (fromList, (!), Vector)
import qualified Data.Vector.Unboxed as V
import Data.Functor ((<&>))
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Heap (sort)
import Control.Monad.Extra (loop)

readDatas :: IO (Vector Int)
readDatas = do
  content <- readFile "day1.txt"
  let errRead = error "ReadDatas: not an Int in input.txt"
      readInt = fromMaybe errRead . readMaybe
      numbers = fromList  (map readInt (words content))
      sorted = runST $ do
        v <- V.unsafeThaw numbers
        sort v
        V.unsafeFreeze v
  pure sorted

main :: IO ()
main = do
  datas <- readDatas
  let combs = combinationsOpt 2 datas
  showSolution "Part1" (part1 combs)
  showSolution "Part2" (part2 datas combs)

showSolution :: String -> [Int] -> IO ()
showSolution part sol =
  putStrLn $ part <> ": numbers: " <> show sol <> ".  Product: " <> show p
      where
        p = product sol

part1 :: [[Int]] -> [Int]
part1 = fromMaybe errPart1 . find  ((==2020) . sum)
  where
    errPart1 = error "Part1: no solution found"

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

-- part2 ::  Vector Int -> [[Int]] -> [Int]
-- part2 vec = foldr f []
--   where
--     f comb acc = case binarySearch vec (2020 - sum comb) of
--                    Nothing -> acc
--                    Just v -> v:comb
part2 ::  Vector Int -> [[Int]] -> [Int]
part2 vec = loop search
  where
    search []           = error "Part2: no solution found"
    search (comb:combs) = case binarySearch vec (2020 - sum comb) of
                                Nothing -> Left combs
                                Just e  -> Right (e:comb)

-- v est un Vector Int trié par ordre croissant.
-- e est la valeur à chercher.
-- renvoie (Just e) ou Nothing. Mais pas l'indice dans le Vector !
binarySearch :: Vector Int -> Int -> Maybe Int
binarySearch v e
  | v ! sup0 < e = Nothing
  | otherwise    = go 0 sup0
  where
    sup0 = V.length v - 1

    go inf sup
      | inf >= sup  = if v ! inf == e then Just e else Nothing
      | v ! mid < e = go (mid+1) sup
      | otherwise   = go inf mid
        where mid = (inf+sup) `div` 2

-- binarySearch :: Vector Int -> Int -> Maybe Int
-- binarySearch v e
--   | v ! sup0 < e = Nothing
--   | otherwise    = loop f (0, sup0)
--   where
--     sup0 = V.length v - 1

--     f (inf, sup)
--       | inf >= sup  = Right (if v ! inf == e then Just e else Nothing)
--       | v ! mid < e = Left (mid+1, sup)
--       | otherwise   = Left (inf, mid)
--         where mid = (inf+sup) `div` 2
