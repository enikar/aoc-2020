-- AoC 2020, day 5

{- HLINT ignore "Eta reduce" -}


module Main(main) where

import Data.List
  (foldl'
  ,sort
  )

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  ds <- map strToInt . lines <$> readFile "day5.txt"
  printSolution "Part1" (maximum ds)
  printSolution "Part1" (part2 ds)


part2 :: [Int] -> Int
part2 ds = go (zip ds' [inf..sup])
  where
    ds' = sort ds
    inf = head ds'
    sup = last ds'

    go []   = error "Error: Part2: place not found"
    go ((p, v):tl)
      | p /= v = v
      | otherwise = go tl

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
