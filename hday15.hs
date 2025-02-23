-- AoC 2020, day 15

-- This method is quite slow. At least it works without runing out of
-- memory (especialy for part2). We just need to use a sparse
-- representaiton to memoize the last turn we encounter a number. It
-- took 5m19s to solve part1 and part2 (real time).

{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M

-- game is represented as a pair of an IntMap and an pair of Int.
-- The IntMap map a number to its last seen turn.
-- The pair reprensent the last turn (not yet inserted in the Map).
type Game = (IntMap Int, (Int, Int))

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO Game
getDatas filename = parseDatas <$> readFile' filename
  where
    parseDatas str = (nums, (num, turn))
      where
        nums = M.fromList (zip (init ls) [1..])
        ls = map readInt (splitOn "," str)
        turn = length nums + 1
        num = last ls
        readInt = fromMaybe erroReadInt . readMaybe
        erroReadInt = error "Error: getDatas: not an Int"

main :: IO ()
main = do
  game <- getDatas "day15.txt"
  printSolution "Part1" (solve game 2020)
  printSolution "Part2" (solve game 30000000)

solve :: Game -> Int -> Int
solve game@(_, (_, turn)) time = n
  where
    dt = time - turn
    (_, (n, _)) = times dt next game

-- Compute the next turn. Update the IntMap.
next :: Game  -> Game
next (nums, (num, turn)) = (nums', (num',turn+1))
  where
    n = fromMaybe turn (M.lookup  num nums)
    num' = turn - n
    nums' = M.insert num turn nums


-- borrow from: https://github.com/glguy/advent/blob/main/common/src/Advent/Prelude.hs
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x
