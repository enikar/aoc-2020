-- AoC 2020, Day 9

-- This code is quite naive, but it is fast.
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}
import System.IO (readFile')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List (find)
import Data.Vector.Unboxed
  (Vector
  ,(!)
  )
import Data.Vector.Unboxed qualified as V
import Data.Functor ((<&>))

printSolution :: String -> Maybe Int -> IO ()
printSolution part sol = putStrLn (part <> ": " <> sol')
  where
    sol' = maybe "No solution found" show sol

getDatas :: String -> IO (Vector Int)
getDatas filename = toVector <$> readFile' filename
  where
    toVector = V.fromList . map readInt . lines
    readInt s = fromMaybe errReadInt (readMaybe s)
    errReadInt = error "Error: getDatas: not an Int"

main :: IO ()
main = do
  datas <- getDatas "day9.txt"
  let mx = part1 datas
  printSolution "Part1" mx
  printSolution "Part2" (part2 mx datas)

-- For part1 we shift a "window" of 25 consecutive elments in the vector.
-- We check if a combination by pair among these elements can sum to the
-- next element following immediately the 25 consecutive elements.
-- We use indices to acces vector's elments, it is faster.
part1 :: Vector Int -> Maybe Int
part1 nums = go 0
  where
    sup = V.length nums - 25
    go n
      | n > sup = Nothing
      | otherwise =
        let vec = V.slice n 25 nums -- pick up 25 elements from n
            x = nums ! (n + 25) -- get the following element
            pairs = combinationsOpt 2 vec -- compute combinations of pairs
        in case find ((==x) . sum) pairs of
          Nothing -> Just x -- x is not the sum of two elements in the 25 previous elements.
          Just _  -> go (n+1) -- We need to go further

-- alternative version using foldr. This is a bit slower.
part1' :: Vector Int -> Maybe Int
part1' nums = foldr f Nothing [sup, sup-1.. 0]
  where
    sup = V.length nums - 25

    f _ mx@(Just _) = mx
    f n Nothing     =
      let vec = V.slice n 25 nums
          x   = nums ! (n+25)
          pairs = combinationsOpt 2 vec
      in case find ((==x) . sum) pairs of
           Nothing -> Just x
           Just _  -> Nothing

-- We build all combinations of k elements from the vector vec
-- by visiting all possibilities like in a backtracking algorithm.
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


-- Maybe it should be better to use Data.Sequence.
-- We named the consecutive values from index n to index m (n < m)
-- a sequence.
-- For part2 we use a function with these arguments:
--    - the index of the begining of the sequence: named n
--    - the index of the end+1 of the sequence: named m
--    - the value of the sum of elements from begigining to the end:
--      named acc

-- We escape with Nothing when the index of the end becomes greater than the last
-- index of the vector.

-- We try to add the value at index m to the sum:
--   - if the sum is inferior than the searched value, we go further to try
--     with index (m+1)
--   - if the new sum is equal to the searched value, then we reach our goal.
--   - else the sum became greater than the searched value, then we try to start at (n+1)
--     instead, updating the new sum by subtracting the value at index n from the sum
part2 :: Maybe Int -> Vector Int -> Maybe Int
part2 Nothing _       = Nothing
part2 (Just val) nums = go 0 0 0 -- start with an empty sequence (from 0 to -1),
                                 -- the sum is equalt to 0
  where
    sup = V.length nums - 1

    -- n is the first index of the sequence
    -- m-1 is the last index of the sequence
    -- acc is the sum of values of the sequence from n to m-1.
    go :: Int -> Int -> Int -> Maybe Int
    go n m acc
      | m > sup     = Nothing          -- no solution
      | acc' < val  = go n (m+1) acc'  -- go further
      | acc' == val = Just (mini+maxi) -- reach the goal
      | otherwise   = go (n+1) m acc'' -- acc' > val, start at (n+1)
      where
        acc' = acc + nums ! m

        vec = V.slice n (m-n+1) nums
        mini = V.minimum vec
        maxi = V.maximum vec

        acc'' = acc - nums ! n
