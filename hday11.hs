-- AoC 2020, day 11

-- naive solution, a bit slow

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.Foldable (foldl')
import Data.Array.Unboxed
  (UArray
  ,(!)
  ,(//)
  ,bounds
  ,elems
  ,assocs
  ,array
  )
import Data.Ix (inRange)

type Coord = (Int, Int)
type Board = UArray Coord Char

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO Board
getDatas filename = buildBoard <$> readFile' filename
  where
    buildBoard str = array ((1,1), (xsup, ysup)) ls'
      where
        ls = lines str
        ysup = length ls
        xsup = length (head ls)
        ls' = [((x, y), c)
              |(y, cs) <- zip [1..] ls
              ,(x, c) <- zip [1..] cs
              ]

main :: IO ()
main = do
  board <- getDatas "day11.txt"
  printSolution "Part1" (solve board countAdj 4)
  printSolution "Part2" (solve board countInSight 5)

solve :: Board -> (Coord -> Board -> Int) -> Int  -> Int
solve board0 countNeighs sup = go board0
  where
    go board
      | noUpdatep = countOccupied board'
      | otherwise = go board'
        where
          (noUpdatep, board') = oneRound countNeighs sup board

countOccupied :: Board -> Int
countOccupied board = length (filter (== '#') (elems board))

oneRound :: (Coord -> Board -> Int) -> Int -> Board -> (Bool, Board)
oneRound countNeighs sup board = (null updates, board')
  where
    updates = foldl' f [] (assocs board)
    board' = board // updates

    f upd ((x,y), c)
      |c == 'L'
       && countNeighs (x,y) board == 0   = ((x,y), '#') : upd
      |c == '#'
       && countNeighs (x,y) board >= sup = ((x,y), 'L') : upd
      |otherwise                         = upd

steps :: [Coord]
steps = [(x,y)
         |x <- [-1..1]
         ,y <- [-1..1]
         ,(x,y) /= (0,0)
         ]
-- alternative definition, using applicative
-- steps = filter (\x -> x /= (0,0)) ((,) <$> [-1..1] <*> [-1..1])

countAdj :: Coord -> Board -> Int
countAdj (x0,y0) board = foldl' f 0 steps
  where
    limits = bounds board
    f acc (x, y)
      | match     = acc+1
      | otherwise = acc
        where
          v = (x0+x, y0+y)
          match = inRange limits v && board ! v == '#'

-- From a point we need to see if there is one '#' in one "direction".
-- Here direction means (horizontal, left). (horizontal, rigth) is
-- another direction. If we encounter a 'L' first that counts as 0 '#'
-- In short we need to find the first '#' or 'L' if any.
countInSight :: Coord -> Board -> Int
countInSight (x0,y0) board = foldl' f 0 steps
  where
    limits = bounds board
    f acc step = acc + go (x0,y0) step

    -- we go in one direction until we encounter a '#' or a 'L'
    -- or we go out the board
    go (x,y) step@(sx, sy)
      |not (inRange limits p) = 0
      |c == '#'               = 1
      |c == 'L'               = 0
      |otherwise              = go p step
        where
          p = (x+sx, y+sy)
          c = board ! p
