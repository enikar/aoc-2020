-- AoC 2020, day 10

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ImportQualifiedPost #-}
{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Foldable (foldl')
import Data.Vector.Unboxed
  (Vector
  ,(!)
  )
import Data.Vector.Unboxed qualified as V
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Heap (sort)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M

import "mtl" Control.Monad.State.Strict
  (State
  ,get
  ,modify'
  ,evalState
  )

type Node = Int
type Neighbours = IntMap (Vector Node)

type Visited = IntMap Int

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO (Vector Int)
getDatas filename = toVector <$> readFile' filename
  where
    readInt s = fromMaybe errReadInt (readMaybe s)
    errReadInt = error "Error: getDatas: not an Int"

    toVector ls =
      let nums = V.fromList (map readInt (lines ls))
      in runST $ do
        v <- V.thaw nums
        sort v
        V.freeze v

-- There is a trap. The chain of adaptaters is between two devices.
-- We need to count the initial difference from the ouput device
-- (at joltage 0) and the last difference with the input device (+3 of
-- the output of the last adaptator).
-- This is the trick to solve part1 and part2
main :: IO ()
main = do
  nums <- getDatas "day10.txt"
  -- add a zero at the beginning and last+3 at the end
  let nums' = V.cons 0 (V.snoc nums (V.last nums + 3))
  printSolution "Part1" (part1 nums')
  printSolution "Part2" (part2 nums')

part1 :: Vector Int -> Int
part1 nums = getResult (foldl' f (0, 0) (zipWith (-) (tail nums') nums'))
  where
    nums' = V.toList nums
    getResult (ones, threes) = ones * threes

    f (ones, threes) diff
      | diff == 1 = (ones+1, threes)
      | diff == 3 = (ones, threes+1)
      | otherwise = (ones, threes) -- not reach

-- For part2 we need to write a dfs. But we need to prepare it
-- with all the next possibilities for each adapter.
-- This is a graph algorithm again.
-- That confirms the correctness of my dfs method but
-- dfs is written in an imperative style.

-- There is at least two better ways to solve part2:
--  - use dynamic programming:
--  https://github.com/mstksg/advent-of-code/blob/main/2020/AOC2020/Day10.hs
--  - use hmatrix like for fibonnacci numbers:
--  https://www.reddit.com/r/adventofcode/comments/kabi91/2020_day_10_closedform_mathematical_solution/

-- nums is a sorted Vector, this is mandatory to compute the successors.
part2 :: Vector Int -> Int
part2 nums = evalState (dfs nghs src dest) M.empty
  where
    src = 0
    dest = V.length nums - 1
    nghs = allNeighbours nums

-- Builds a successors' Map for each number in nums.
-- Nodes are indices in the nums Vector.
allNeighbours :: Vector Int -> Neighbours
allNeighbours nums = foldl' f M.empty [0..sup]
  where
    sup = V.length nums - 1

    f acc i = M.insert i ns acc
      where
        ns = V.fromList (neighboursAt i)

    -- Builds the list of  all successors in nums near position
    -- index.
    neighboursAt index =
      [i
      |let val = nums ! index
      ,i <- [index+1..index+3]
      ,i <= sup
      ,nums ! i - val < 4
      ]

-- In this puzzle, we really need to keep track of visited nodes.
dfs :: Neighbours -> Node -> Node -> State Visited Int
dfs nghs src dest
  |src == dest = pure 1
  |otherwise   = do
     visited <- get
     if src `M.member` visited
     then pure (visited M.! src)
     else V.foldM' next 0 (nghs M.! src)
  where
    next acc node = do
        w <- dfs nghs node dest
        modify' (M.insert node w)
        pure (acc + w)
