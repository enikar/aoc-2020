-- AoC 2020, day 5

{- HLINT ignore "Eta reduce" -}


module Main(main) where

import Data.List
  (foldl'
  ,sort
  ,sortBy
  )

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  ds <- map strToInt . lines <$> readFile "day5.txt"
  printSolution "Part1" (maximum ds)
  printSolution "Part1" (part2' ds)

-- for part2, first we wrote a recursive function to short-circuit
-- when we found a missing number in the places list
-- But we can sort the list in descending order and use a foldr (part2')
part2 :: [Int] -> Int
part2 ds = go inf ds'
  where
    ds' = sort ds
    inf = head ds'

    go _ []   = error "Error: Part2: place not found"
    go v (p:ps)
      | p /= v = v
      | otherwise = go (p+1) ps


part2' :: [Int] -> Int
part2' ds = foldr f inf ds'
  where
    ds' = sortBy (flip compare) ds
    inf = last ds'

    f p acc
      | p /= acc  = acc   -- place is found, we exit
      | otherwise = acc+1 -- else we look at next place

-- The places are encoded in binary
strToInt :: String -> Int
strToInt str = 8 * h + t
  where
    errorStr = error "Error: strToInt"
    h = foldl' f 0 (take 7 str)
    t = foldl' f 0 (drop 7 str)

    f acc c = n + 2 * acc
      where
        n = case c of
              'F' -> 0
              'L' -> 0
              'B' -> 1
              'R' -> 1
              _   -> errorStr
