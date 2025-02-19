-- AoC 2020, day 14

-- This puzzle was not very appealing and astonishingly easy. Thanks to
-- the Applicative instance of List. I would have expected the program to
-- take longer and the results to be greater, but they fit into an Int64.
-- However, it's a bit slow. Maybe, we can use ideas from Grabriella
-- Gonzalez's library foldl to reduce the depth of nested loop:
-- https://hackage.haskell.org/package/foldl
-- https://www.haskellforall.com/2013/08/foldl-100-composable-streaming-and.html

{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.List
  (foldl'
  ,unfoldr
  )
import Data.Bits
  ((.&.)
  ,(.|.)
  )
import Data.IntMap (IntMap)
import Data.IntMap qualified as M
import Control.Monad (replicateM)

--  modules for parsing
import Data.Char (isDigit)
import Data.Functor (void)
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,endBy1
  ,munch1
  ,satisfy
  ,many1
  ,char
  ,string
  ,eof
  ,optional
  )

type Addr = Int
data MSet = MSet Addr Int deriving (Show)
type Mask = String
data Instr = IM Mask
             |IS MSet deriving (Show)

-- Although the results fit into a 64 bit integer, we use an Integer.
type Result = Integer
type Memory = IntMap Result

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO [Instr]
getDatas filename = parseDatas <$> readFile' filename

main :: IO ()
main = do
  instrs <- getDatas "day14.txt"
  printSolution "Part1" (solve part1 instrs)
  printSolution "Part2" (solve part2 instrs)

-- part1 and part2 are almost the same, so we merge both.
solve :: (Mask -> Addr -> Int -> Memory -> Memory) -> [Instr] -> Result
solve updateMem instrs = sum memory
  where
    -- we are walking along instructions, applying one of them at each
    -- step
   (_mask, memory) = foldl' f ("", M.empty) instrs

   f (_mask, mem) (IM mask)            = (mask, mem)
   f (mask, mem)  (IS (MSet addr val)) = (mask, updateMem mask addr val mem)

part1 :: Mask -> Addr -> Int -> Memory -> Memory
part1 mask addr val mem = M.insert addr val' mem
  where
    val' = fromIntegral ((val .&. mAnd) .|. mOr) -- apply the mask
    (mAnd, mOr) = buildMask mask

part2 :: Mask -> Addr -> Int -> Memory -> Memory
part2 mask addr val mem = foldl' reduce mem addresses
  where
    addresses = genAddr (applyMask mask addr)
    reduce acc x = M.insert x (fromIntegral val) acc

-- Utility for part1.
-- Builds the two parts of the mask.
buildMask :: Mask -> (Int, Int)
buildMask str = foldl' f (0, 0) str
  where
    f (mAnd, mOr) c
      |c == 'X' = (2*mAnd+1, 2*mOr)
      |c == '1' = (2*mAnd+1, 2*mOr+1)
      |otherwise = (2*mAnd, 2*mOr) -- c == '0'

-- Utilities for part2.
-- Applies the mask for part2. Unlike for part1 we need to use string to
-- manage the floating bits.
applyMask :: Mask -> Addr -> String
applyMask mask addr = zipWith f mask (intToBinStr addr)
  where
    f m a
      |m == '0' = a  -- 0 unchange the bit
      |m == '1' = '1' -- 1 set it to one
      |otherwise = 'X' -- set it to a floating bit

-- We build a list of all possible addresses in taking account
-- the floatting bits marked with a X.
genAddr :: String -> [Addr]
genAddr addr = map f (replicateM n ['0','1'])
  where
    n = length (filter (=='X') addr)

    f ls = binStrToInt (fst (foldr g ("",ls) addr))

    g a (m,[])    = (a:m, [])
    g a (m, l@(x:xs))
      | a /= 'X'  = (a:m, l)
      | otherwise = (x:m, xs)

-- conversions for part2
intToBinStr :: Int -> String
intToBinStr 0  = replicate 36 '0'
intToBinStr n0 = zeros ++ ls
  where
    zeros = replicate (36 - length ls) '0'
    ls = reverse (unfoldr f n0)
    f 0 = Nothing
    f n = Just (r', q)
      where
        (q, r) = n `divMod` 2
        r' = if r == 0 then '0' else '1'

binStrToInt :: String -> Int
binStrToInt str = foldl' f 0 str
  where
    f acc c = acc * 2 + c'
      where
        c' = if c == '0' then 0 else 1

-- Parsing stuff
-- Parses of a positive decimal integer.
positive :: (Read a, Integral a) => ReadP a
positive = read <$> munch1 isDigit

parseDatas :: String -> [Instr]
parseDatas str =
  case readP_to_S parseInstrs str of
    [x] -> fst x
    _   -> error "Error: parseDatas: can't parse"

parseInstrs :: ReadP [Instr]
parseInstrs = do
  instrs <- many1 parseBlock
  optional (char '\n')
  eof
  pure (concat instrs)

parseBlock :: ReadP [Instr]
parseBlock = do
  mask <- IM <$> parseMask
  void (char '\n')
  mems <- endBy1 (IS <$> parseMem) (char '\n')
  pure (mask : mems)

-- Hlint insists with some "improvements" I dislike,
-- so I rewrite these two parsers using Applicative style.
parseMask :: ReadP Mask
parseMask = string "mask = " *> many1 (satisfy (`elem` "01X"))

parseMem :: ReadP MSet
parseMem =
  liftA2 MSet
         (string "mem[" *> positive)
         (string "] = " *> positive)
