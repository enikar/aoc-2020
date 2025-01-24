-- AoC 2020, Day 8: https://adventofcode.com/2020/day/8

-- Not a good code but it works.

{-# LANGUAGE ImportQualifiedPost #-}

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

data Instr = Nop Int
           |Acc Int
           |Jmp Int deriving (Show)

type Program = Array Int Instr

-- The machine consist of:
--  bounds of the program
--  the current program counter
--  the value of the accumulator
--  its state
--  the visited position of program counter.
data Machine = Machine
  {memoryBounds :: (Int, Int)
  ,pc :: Int
  ,accumulator :: Int
  ,state :: MachineState
  ,visited :: IntSet
  } deriving (Show)

-- The state of the machine.
data MachineState = End
                  |Loop
                  |Cont
                  |Out deriving (Show)

getDatas :: String -> IO Program
getDatas filename = do
  instrs <- map parseInstr . lines <$> readFile' filename
  pure (Array.listArray (0, length instrs - 1) instrs)

readInt :: String -> Int
readInt s = fromMaybe errReadInt (readMaybe s')
  where
    -- what a shame, read can't parse an Int prefixed with '+'
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
  let machine = Machine (Array.bounds instrs) 0 0 Cont (S.singleton 0)
  printSolution "Part1" (part1 instrs machine)
  printSolution "Part2" (part2 instrs machine)

-- part1 is straightforward. We step until we encounter
-- an already seen program counter.
part1 :: Program -> Machine -> Int
part1 instrs machine = go machine
  where
    go m = case state m' of
             Loop -> accumulator m' -- we reach the goal
             Cont -> go m'' -- we need to go further
             _    -> error "Error: part1: End or Out!" -- should not be reached
      where
        m' = step instrs m
        pc' = pc m'
        m'' = m' {visited = S.insert pc' (visited m')}

-- part2 is more complex. We need to change one instruction, a Nop to
-- a Jmp or a Jmp to a Nop and test if the program run to the end.
-- We run the program and check the next susbtition each time the
-- program loops or runs out of range.
part2 :: Program -> Machine -> Int
part2 instrs machine = loop f (jmpOrNops instrs)
  where
    subst instr = instrs // [instr]

    f [] = error "Error; part2: no solution" -- should not be reached
    f (instr:rest) = go (subst instr) machine
      where
        go instrs' m =
          let machine' = step instrs' m
              pc' = pc machine'
              machine'' = machine' {visited = S.insert pc' (visited machine')}
          in case state machine' of
              End  -> Right (accumulator machine') -- we reach the goal
              Cont -> go instrs' machine''         -- we continue to run the programm
              _    -> Left rest -- Loop or Out, we'are going to test the next substitution

-- Filters all Jmp and Nop, changing Jmp to Nop and vice versa.
jmpOrNops :: Program -> [(Int, Instr)]
jmpOrNops instrs = foldr g [] (Array.assocs instrs)
  where
     g (i, instr) acc =
       case instr of
         Jmp j -> (i, Nop j) : acc
         Nop j -> (i, Jmp j) : acc
         _     -> acc

-- Steps one instruction, calculates the next program counter,
-- updates the accumulator and update the state of the machine:
-- Cont: we can go further
-- End: we reach the end the program: the program counter is
--      exactly one + the maximum of the program counter.
-- Loop: the program counter returns to an already visited value
-- Out: the program counter is out of range of the program
step :: Program -> Machine -> Machine
step instrs machine =
  let pc' = pc machine
      pcNext = pc'+1
  in case instrs ! pc' of
    Nop _ -> machine {state = machineState pcNext machine
                     ,pc = pcNext}
    Acc x -> machine {state = machineState pcNext machine
                     ,pc = pcNext
                     ,accumulator = accumulator machine + x}
    Jmp x -> let pc'' = pc' + x
             in machine {state = machineState pc'' machine
                        ,pc = pc''}

-- Determines the state of the machine.
machineState :: Int-> Machine -> MachineState
machineState pc' machine
  | pc' `S.member` vis = Loop
  | pc' == sup + 1 = End
  | not (inRange (inf, sup) pc') = Out
  | otherwise     = Cont
    where
      vis = visited machine
      (inf, sup) = memoryBounds machine
