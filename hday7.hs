-- AoC 2020, day 7

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')
import Data.Map.Strict
  (Map
  ,(!)
  )
import Data.Map.Strict qualified as M
import Data.Word (Word8)
import Data.Functor
  (void
  ,($>)
  )
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Scanner qualified as S
import Scanner
  (Scanner
  ,anyWord8
  ,scanOnly
  ,skipSpace
  ,lookAhead
  ,decimal
  )

type Bags = Map Bag [Content]
type Bag = ByteString
type Content = (Bag, Int)
type Visited = Map Bag Int

main :: IO ()
main = do
  bags <- getDatas "day7.txt"
  printSolution "Part1" (part1 bags)
  printSolution "Part2" (part2 bags)

needle :: Bag
needle = "shiny gold"

part1 :: Bags -> Int
part1 bags = sum  (M.foldlWithKey' f M.empty bags)
  where
    f  visited bag _
      | bag == needle = visited
      | otherwise     = dfs1 bags bag visited

dfs1 :: Bags -> Bag -> Visited -> Visited
dfs1 bags bag visited
  | bag `M.member` visited = visited -- bag is already seen
    -- we'll walk along current bag's content
    -- Before we insert the current bag in visited as if doesn't contain needle
  | otherwise              = foldr f (M.insert bag 0 visited) (bags ! bag)
    where
      f (bag', _) vis
        | bag' == needle = M.insert bag 1 vis -- the needle is in current bag
        | n == 1         = M.insert bag 1 vis' -- the needle is in bag' or in its children
        | otherwise      = vis' -- the needle wasn't found yet, we'll look for it in next bags
          where
            vis' = dfs1 bags bag' vis -- we are exploring in depth first…
            n = vis' ! bag'

part2 :: Bags -> Int
part2 bags = fst (dfs2 bags needle M.empty) - 1

dfs2 :: Bags -> Bag -> Visited -> (Int, Visited)
dfs2 bags bag visited
  | bag `M.member` visited = (visited ! bag, visited)
  | otherwise              = (m, M.insert bag m visited')
      where
        (m, visited') = foldl' f (1, visited) (bags ! bag)

        f (acc, vis) (bag', n)  = (acc + m'* n, vis')
          where
            (m', vis') = dfs2 bags bag' vis

-- Thanks to a mistake we discovered that part2' is just
-- as effective as part2, though we don't keep a record of
-- all visited bags. This must be due the low number of vertices
-- in the graph.
part2' :: Bags -> Int
part2' bags = dfs2' bags needle - 1

dfs2' :: Bags -> Bag ->  Int
dfs2' bags bag = foldl' f 1 (bags ! bag)
  where
    f acc (bag', n) = acc + m'*n
      where
        m' = dfs2' bags bag'


printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

getDatas :: String -> IO Bags
getDatas filename = parseDatas <$> BC.readFile filename

parseDatas :: ByteString -> Bags
parseDatas str = foldl' f M.empty (BC.lines str)
  where
    f acc s = M.insert b c acc
      where
        (b, c) = either error
                        id
                        (scanOnly parseRecord s)

-- To use word8 instead of Char. It should be faster,
-- although that doesn't matter here.
charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

en, dot, comma, space, es :: Word8
en = charToWord8 'n'
dot = charToWord8 '.'
comma = charToWord8 ','
space = charToWord8 ' '
es = charToWord8 's'

parseRecord :: Scanner (Bag, [Content])
parseRecord = do
  bag <- parseBag
  void (S.take 9) -- skips " contain "
  c <- lookAhead
  case c of
    Just  v |v == en -> void (S.take 14) $> (bag, []) -- skips "no other bags."
    Nothing          -> error "Error: parseRecord" -- should not be reach
    _                -> do content <- parseContain
                           pure (bag, content)

parseContain :: Scanner [Content]
parseContain = do
  c <- lookAhead
  case c of
    Just v
      |v == dot   -> anyWord8 $> [] -- skip '.', closes the list [Content].
      |v == comma -> S.take 2 *> parseContain -- skips ", ", parses remaining charaters
      |otherwise  -> do
         n <- decimal -- parses the number
         skipSpace
         bag <- parseBag -- a bag follows, discard " bag" or " bags"
         rest <- parseContain -- parses the remaining string
         pure ((bag, n) : rest)
    Nothing       -> error "Error: parseContain" -- should not be reached

-- parse a string shaped as "foo bar bag" with an optional 's'
-- at the end.
parseBag :: Scanner ByteString
parseBag = do
  c1 <- S.takeWhile (/= space) -- parses "foo"
  skipSpace
  c2 <- S.takeWhile (/= space) -- parses "bar"
  skipSpace
  void (S.take 3) -- skips "bag"
  c <- lookAhead
  when (c == Just es) (void anyWord8) -- skips 's' if any
  pure (BC.unwords [c1, c2]) -- returns "foo bar" as a Scanner
