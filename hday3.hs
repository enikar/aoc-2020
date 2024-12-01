-- AoC 2020 Day3

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  datas <- readDatas
  let task part = showSolution part (solver part datas)
  task "Part1"
  task "Part2"

showSolution :: String -> Int -> IO ()
showSolution part sol = putStrLn (part <> ": answer: " <> show sol)

type Steps = [(Int, Int)]

mSteps :: [(String, Steps)]
mSteps = [("Part1", [(3, 1)])
         ,("Part2", [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
         ]

solver :: String -> [String] -> Int
solver part = product . solvePart steps
  where
    steps = fromMaybe errorSolver (lookup part mSteps)
    errorSolver = error ("solver: unknown part: " <> part)

countTrees :: [String] -> Int -> Int
countTrees datas rstep = foldl' f 0 (zip datas positions)
  where
    positions = map (`mod` 31) [0, rstep..]
    f count (s, pos) = if s !! pos == '#' then count + 1 else count

solvePart :: Steps -> [String] ->  [Int]
solvePart steps datas = map solve steps
  where
    solve (rstep, dstep) = countTrees datas' rstep
      where datas' = foldr f [] (zip datas [0..])
            f (s, pos) ls
              | pos `mod` dstep == 0 = s:ls
              | otherwise            = ls

readDatas :: IO [String]
readDatas = lines <$> readFile "day3.txt"
