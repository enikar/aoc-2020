-- AoC 2020, day 13

-- This puzzle was interesting. It took me a while to understand, I
-- needed to use negative times for delays. The other thing that puzzled
-- me was the error of copy/paste I made when testing the small
-- examples…
-- With the data in the "day13.txt" file, it will take 1.7 billion
-- years to meet the conditions in part2…

{-# LANGUAGE PackageImports #-}

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.List.Extra
  (splitOn
  ,minimumOn
  )
import Data.Foldable (foldlM)

import Data.Maybe
  (fromMaybe
  ,catMaybes
  )
import Text.Read (readMaybe)
import "mtl" Control.Monad.Writer
  (Writer
  ,tell
  ,runWriter
  )

-- a congruence = (residue, modulo)
type Congruence = (Integer, Integer)

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO (Integer, [Maybe Integer])
getDatas filename = parseDatas <$> readFile' filename
  where
    parseDatas :: String -> (Integer, [Maybe Integer])
    parseDatas str =
      case lines str of
        [goal, str'] -> (readInt goal, rules str')
        _            -> error "Error: getDatas: can't parse"

    readInt s = fromMaybe errorReadInt (readMaybe s)
    errorReadInt = error "Error: readInt: not an Integer"

    rules s =  map readMaybe (splitOn "," s)

main :: IO ()
main = do
  (goal, rules) <- getDatas "day13.txt"
  printSolution "Part1" (part1 goal (catMaybes rules))
  printSolution "Part2" (part2 rules)

part1 :: Integer -> [Integer] -> Integer
part1 goal rules = busId * time
  where
    -- compute the waiting time for a bus
    wt bus = (bus, dt)
      where
        dt = (q+1) * bus - goal
        q =  goal `div` bus

    (busId, time) = minimumOn snd (map wt rules)

-- The trick, here, is to understand that buses arrived *after* the
-- first one. So, we need to negate the residues to take account of
-- this. All of this, works with positive moduli (the buses Ids are
-- positive). Anyway, I don't know if negative moduli are used
-- somewhere.
part2 :: [Maybe Integer] -> Integer
part2 rules = residue
  where
    (residue, _modulo) = fst (runWriter (crtnW (buildRules rules)))

-- Builds a list of pair with (zip [0..] rules)
-- then filter out the Nothing with a foldr to just
-- keep buses Id and theirs shifts. We need to negate
-- shifts because they represent a delayed time.
buildRules :: [Maybe Integer] -> [Congruence]
buildRules rules =  foldr f [] (zip [0..] rules)
  where
    f (_, Nothing) acc = acc
    f (n, Just m) acc = (-n, m) : acc

-- -- repeats crt for all (residue,modulo) in the list.
-- crtn :: [Congruence] -> Congruence
-- crtn rules = foldl1' crt rules

-- Rewrited using Writer Monad to see intermediate results in ghci.
crtnW :: [Congruence] -> Writer [String] Congruence
crtnW []  = error "Error: crtnW: empty list!"
crtnW (x:xs) = foldlM reduce x xs
  where
    logW a b c = "crt "
                <> show a
                <> " "
                <> show b
                <> " = "
                <> show c

    reduce acc g = do
      let g' = crt acc g
      tell [logW acc g g']
      pure g'

-- The Extended Euclidian Algorithm
-- (eea x y) returns the triplet (d, u0, v0) that satisfied:
-- d = x * u0 + y * v0, where d = gcd x y
-- There are infinite solutions:
-- u = u0 - y * k
-- v = v0 + x * k
-- where k is an integer
eea :: Integer -> Integer -> (Integer, Integer, Integer)
eea x y = go y (x,1,0) (y, 0, 1)
  where
    go 0 ans _ = ans
    go _ (a0, a1, a2) (b0, b1, b2) = go r (b0, b1, b2) (r, u, v)
      where
        -- moduli are positive, so we can use quotRem
        (q,r) = a0 `quotRem` b0
        u = a1 - q*b1
        v = a2 - q*b2

-- The Chinese Remainder Theorem
-- Adapted from: https://hackage.haskell.org/package/arithmoi-0.13.0.0/docs/Math-NumberTheory-Moduli-Chinese.html
-- We use div and mod instead of quot and rem, so the results are
-- positive.
-- As all moduli are prime numbers, only the first equation where
-- (gcd m1 m2 == 1) is reached.
crt :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
crt (n1, m1) (n2, m2)
  | d == 1
  = ((v * m2 * n1 + u * m1 * n2) `mod` m, m)
  | (n1 - n2) `mod` d == 0 -- not reach
  = ((v * (m2 `div` d) * n1 + u * (m1 `div` d) * n2) `mod` m, m)
  | otherwise -- not reach
  = error "Error: crt: no solution"
  where
    (d, u, v) = eea m1 m2
    m = if d == 1 then m1 * m2 else (m1 `div` d) * m2
