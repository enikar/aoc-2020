-- AoC 2020, day 6

{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List
  (foldl'
  ,foldl1'
  )
import Data.List.Extra (splitOn)
import Data.Set (Set)
import Data.Set qualified as Set


main :: IO ()
main = do
  datas <- getDatas "day6.txt"
  printSolution "Part1" (part1 datas)
  printSolution "Part2" (part2 datas)


part1 :: [[Set Char]] -> Int
part1 = foldl' f 0
  where
    f acc grps = acc + Set.size (Set.unions grps)

part2 :: [[Set Char]] -> Int
part2 = foldl' f 0
  where
    f acc grps = acc + Set.size (intersections grps)

    intersections = foldl1' Set.intersection

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

getDatas :: String -> IO [[Set Char]]
getDatas filename = do
  grps <- splitOn "\n\n" <$> readFile filename
  let grps' = map (map Set.fromList . lines) grps
  pure grps'
