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
-- We check if a combination of pair among these elements can sum to the
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
          Nothing -> Just x -- v is not the sum of two elements in the 25 previous elements.
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


-- For part2 we wrote two recursive function to loop.
-- It's not a haskelly version… I wonder how to write it to not
-- have explicit recursives functions and to keep efficiency.
part2 :: Maybe Int -> Vector Int -> Maybe Int
part2 Nothing     _   = Nothing
part2 (Just val) nums = go 0
  where
    sup = V.length nums - 1

    go n
      | n > sup   = Nothing
      | otherwise = case go' n 0 of
                      Nothing -> go (n+1)
                      Just m  -> let vec = V.slice n (m-n+1) nums
                                     mini = V.minimum vec
                                     maxi = V.maximum vec
                                 in Just (mini + maxi)
      where
        go' m acc
          | acc' == val = Just m -- We found the rigth range [n..m]
          | acc' > val  = Nothing -- The solution  was not in range [n..m]
          | otherwise   = go' (m+1) acc' -- We need to go further
          where
            acc' = acc + nums ! m
