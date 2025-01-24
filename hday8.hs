-- AoC 2020, Day 8: https://adventofcode.com/2020/day/8

-- Not a good code but it works.

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Eta reduce" -}


import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.IO (readFile')
import Data.Array
  (Array
  ,(!)
  ,(//)
  )
import Data.Array qualified as Array
import Data.Ix (inRange)

import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Control.Monad.Extra (loop)

data Instr = Nop Int | Acc Int | Jmp Int deriving (Show)
type Instructions = Array Int Instr

-- The machine consist of;
--  bounds of the program
--  the current program counter
--  the value of the accumulator
-- and its state
data Machine = Machine
  {memoryBounds :: (Int, Int)
  ,pc :: Int
  ,accumulator :: Int
  ,state :: MachineState
  }

-- The state of the machine.
data MachineState = End
                  |Loop
                  |Cont
                  |Out deriving (Show, Eq)

getDatas :: String -> IO Instructions
getDatas filename = do
  instrs <- map parseInstr . lines <$> readFile' filename
  pure (Array.listArray (0, length instrs - 1) instrs)

readInt :: String -> Int
readInt s = fromMaybe errReadInt (readMaybe s')
  where
    -- what a shame, read can't parse number prefix with '+'
    s' |head s == '+' = tail s
       |otherwise     = s
    errReadInt = error ("Error: readInt: can't parse an Int: " <> s)

parseInstr :: String -> Instr
parseInstr str = case words str of
  ["nop", i] -> Nop (readInt i)
  ["acc", i] -> Acc (readInt i)
  ["jmp", i] -> Jmp (readInt i)
  [x, _]     -> error ("Error: parseInstr: Unknown instruction: " <> x)
  _          -> error ("Error; parseInstr: malformed instruction: " <> str)

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  instrs <- getDatas "day8.txt"
  let machine = Machine (Array.bounds instrs) 0 0 Cont
  printSolution "Part1" (part1 instrs machine)
  printSolution "Part2" (part2 instrs machine)

-- part1 is straightforward. We step until we encounter
-- an already seen program counter.
part1 :: Instructions -> Machine -> Int
part1 instrs machine = go (S.singleton 0) machine
  where
    go visited m = case state m' of
                 Loop -> accumulator m'
                 Cont -> go (S.insert pc' visited) m'
                 _    -> error "Error: part1: End or Out!" -- should not be reached
        where
          m' = step instrs visited m
          pc' = pc m'

-- part2 is more complex. We need to change one instruction, a Nop to
-- a Jmp or a Jmp to a Nop and test if the program run to the end.
-- We run the program and test the next susbtition each time
-- the program loops or run out of range.
-- This code is quite complex.
part2 :: Instructions -> Machine -> Int
part2 instrs machine = loop f (jmpOrNops instrs)
  where
    subst instr = instrs // [instr]

    f [] = error "Error; part2: no solution"
    f (instr:rest) = go (subst instr) (S.singleton 0) machine
      where
        go instrs' visited m =
          let machine' = step instrs' visited m
              pc' = pc machine'
          in case state machine' of
              End  -> Right (accumulator machine')
              Cont -> go instrs' (S.insert pc' visited) machine'
              _    -> Left rest -- Loop or Out

-- filter all Jmp and Nop, changing Jmp to Nop and vice versa.
jmpOrNops :: Instructions -> [(Int, Instr)]
jmpOrNops instrs = foldr g [] (Array.assocs instrs)
  where
     g (i, instr) acc
           | jmpOrNop instr = (i, instr') : acc
           | otherwise      = acc
             where
               instr' = case instr of
                 Jmp j -> Nop j
                 Nop j -> Jmp j
                 _     -> error "Error: jmpOrNots" -- should not be reached

jmpOrNop :: Instr -> Bool
jmpOrNop = \case
  Jmp _ -> True
  Nop _ -> True
  _     -> False

-- Steps one instruction, calculates the next program counter,
-- updates the accumulator and update the state of the machine:
-- Cont: we can go further
-- End: we reach the end the program: the program counter is
--      exactly one + the maximum of the program
-- Loop: the program counter returns to an already visited value
-- Out: the program counter is out of range of the program
step :: Instructions -> IntSet -> Machine -> Machine
step instrs visited machine =
  let pc' = pc machine
      pcNext = pc'+1
  in case instrs ! pc' of
    Nop _ -> machine {state = machineState visited pcNext machine, pc = pcNext}
    Acc x -> machine {state =machineState visited pcNext machine
                     ,pc = pcNext
                     ,accumulator = accumulator machine + x}
    Jmp x -> let pc'' = pc' + x
             in machine {state = machineState visited pc'' machine
                        ,pc = pc''}

-- Determines the state of the machine.
machineState :: IntSet -> Int-> Machine -> MachineState
machineState visited pc' machine
  | pc' `S.member` visited = Loop
  | pc' == sup + 1 = End
  | not (inRange (inf, sup) pc') = Out
  | otherwise     = Cont
    where
      (inf, sup) = memoryBounds machine
