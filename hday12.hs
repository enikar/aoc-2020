-- AoC 2020, day 12

-- Very bad code for a painfull puzzle!

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

data Instruction = NS Int
                   |EW Int
                   |RL Int
                   |F Int deriving (Show)


-- orientation: East=0, South=90, West=180, North=270
-- In coordinates West, and South are negative, East and North
-- are positive.
data Ship = Ship {orientation :: Int
                 ,position :: (Int, Int) -- (east, north)
                 ,waypoint :: (Int,Int) -- (east,north)
                 } deriving (Show)

getDatas :: String -> IO [Instruction]
getDatas filename = parseInstructions <$> readFile' filename
  where
    parseInstructions str = map parseInstr (lines str)
    parseInstr (x:xs) =
      case x of
        'N' -> NS (readInt xs)
        'S' -> NS (negate (readInt xs))
        'W' -> EW (negate (readInt xs))
        'E' -> EW (readInt xs)
        'L' -> RL (negate (readInt xs))
        'R' -> RL (readInt xs)
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

-- two nested cases… it's ugly
move1 :: Ship -> Instruction -> Ship
move1 ship instr =
  let (ew, ns) = position ship
      o = orientation ship
  in case instr of
  (NS m) -> ship {position = (ew,ns+m)}
  (EW m) -> ship {position = (ew+m, ns)}
  (RL m) -> ship {orientation = (o+m+360) `rem` 360}
  (F m) -> case o of
             0   -> ship {position = (ew+m, ns)}
             90  -> ship {position = (ew, ns-m)}
             180 -> ship {position = (ew-m, ns)}
             270 -> ship {position = (ew, ns+m)}
             _   -> error "Error: move1: bad orientation"

move2 :: Ship -> Instruction -> Ship
move2 ship instr =
  let (ew, ns) = position ship
      (wayE, wayN) = waypoint ship
  in case instr of
    (NS m) -> ship {waypoint = (wayE, wayN+m)}
    (EW m) -> ship {waypoint = (wayE + m, wayN)}
    (F m) -> ship {position = (ew + m*wayE, ns + m*wayN)}
    (RL m) -> case m of
                90   -> ship {waypoint = (wayN, -wayE)}
                180  -> ship {waypoint = (-wayE, -wayN)}
                270  -> ship {waypoint = (-wayN, wayE)}
                -90  -> ship {waypoint = (-wayN, wayE)}
                -180 -> ship {waypoint = (-wayE, -wayN)}
                -270 -> ship {waypoint = (wayN, -wayE)}
                _    -> error "Error: move2: bad rotation"
