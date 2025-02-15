-- AoC 2020, day 12

-- Very bad code for a painfull puzzle!

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

data Instruction = N Int
                   |S Int
                   |W Int
                   |E Int
                   |L Int
                   |R Int
                   |F Int deriving (Show)


-- orientation: East=0, South=90, West=180, North=270
data Ship = Ship {orientation :: Int
                 ,position :: (Int, Int)
                 ,waypoint :: (Int,Int) -- (east,north)
                 } deriving (Show)

getDatas :: String -> IO [Instruction]
getDatas filename = parseInstructions <$> readFile' filename
  where
    parseInstructions str = map parseInstr (lines str)
    parseInstr (x:xs) =
      case x of
        'N' -> N (readInt xs)
        'S' -> S (readInt xs)
        'W' -> W (readInt xs)
        'E' -> E (readInt xs)
        'L' -> L (readInt xs)
        'R' -> R (readInt xs)
        'F' -> F (readInt xs)
        _   -> errorParse
    parseInstr _ = errorParse

    errorParse = error "Error: parseInstr: invalid instruction"

    readInt s = fromMaybe errorReadInt (readMaybe s)
    errorReadInt = error "Error: readInt: not an Int"

main :: IO ()
main = do
  instrs <- getDatas "day12.txt"
  printSolution "Part1" (solve move1 instrs)
  printSolution "Part2" (solve move2 instrs)

-- we should remove the orientation because we can code it with
-- waypoint. Also, maybe there is a way to write it to avoid so many
-- cases, but this puzlle is so boring, we leave it as is.
ship0 :: Ship
ship0 = Ship {orientation = 0
             ,position = (0,0)
             ,waypoint = (10, 1)
             }
solve :: (Ship -> Instruction -> Ship) -> [Instruction] -> Int
solve move instrs = abs east + abs north
  where
    (east, north) = position (foldl' move ship0 instrs)

move1 :: Ship -> Instruction -> Ship
move1 ship@(Ship {..}) instr =
  let (ew, ns) = position
      o = orientation
  in case instr of
  (N m) -> ship {position = (ew,ns+m)}
  (S m) -> ship {position = (ew, ns-m)}
  (W m) -> ship {position = (ew-m, ns)}
  (E m) -> ship {position = (ew+m, ns)}
  (L m) -> ship {orientation = (o-m+360) `mod` 360}
  (R m) -> ship {orientation = (o+m) `mod` 360}
  (F m) -> if |o == 0 -> ship {position = (ew+m, ns)}
              |o == 90 -> ship {position = (ew, ns-m)}
              |o == 180 -> ship {position = (ew-m, ns)}
              |o == 270 -> ship {position = (ew, ns+m)}
              |otherwise -> error "Error: move1: bad orientation"

move2 :: Ship -> Instruction -> Ship
move2 ship@(Ship{..}) instr =
  let (ew, ns) = position
      (wayE, wayN) = waypoint
      errorMove2 = error "Error: move2: bad rotation"
  in case instr of
    (N m) -> ship {waypoint = (wayE, wayN+m)}
    (S m) -> ship {waypoint = (wayE, wayN-m)}
    (W m) -> ship {waypoint = (wayE - m, wayN)}
    (E m) -> ship {waypoint = (wayE + m, wayN)}
    (F m) -> ship {position = (ew + m*wayE, ns + m*wayN)}
    (R m) -> if |m == 90  -> ship {waypoint = (wayN, -wayE)}
                |m == 180 -> ship {waypoint = (-wayE, -wayN)}
                |m == 270 -> ship {waypoint = (-wayN, wayE)}
                |otherwise -> errorMove2
    (L m) -> if |m == 90  -> ship {waypoint = (-wayN, wayE)}
                |m == 180 -> ship {waypoint = (-wayE, -wayN)}
                |m == 270 -> ship {waypoint = (wayN, -wayE)}
                |otherwise -> errorMove2
