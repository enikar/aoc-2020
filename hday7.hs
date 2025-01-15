-- AoC 2020, day 7

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as M

import Data.Functor
  (void
  ,($>)
  )
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Attoparsec.ByteString.Char8
  (Parser
  ,parseOnly
  ,decimal
  ,sepBy1'
  ,string
  ,char
  ,option
  ,notChar
  ,skipSpace
  ,endOfLine
  ,manyTill'
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
part1 bags = (length . M.filter (== 1)) (M.foldlWithKey' f M.empty bags)
  where
    f  visited bag _
      | bag == needle = visited
      | otherwise = dfs bags bag visited

dfs ::  Bags -> Bag -> Visited -> Visited
dfs bags bag visited
  | bag `M.member` visited =  visited -- already seen
  | otherwise = go visited (bags ! bag) -- we walk along current bag's content.
    where
      go :: Visited -> [Content] -> Visited
      go vis []             = M.insert bag 0 vis -- needle wasn't found
      go vis ((bag',_):cts)
        | bag' == needle    = M.insert bag 1 vis -- needle is in current bag
        | n == 1            = M.insert bag 1 vis' -- needle is in bag' or in its children
        | otherwise         = go vis' cts  -- needle wasn't found yet, looking for next bags
          where
            vis' = dfs bags bag' vis -- We explore in depth first…
            n = vis' ! bag'

part2 :: Bags -> Int
part2 bags = fst (dfs' bags needle M.empty) - 1

-- maybe we should use State (and also for dfs).
dfs' :: Bags -> Bag -> Visited -> (Int, Visited)
dfs' bags bag visited
  | bag `M.member` visited = (visited ! bag, visited)
  | otherwise = go (1, visited) (bags ! bag)
    where
      go (m, vis) []                = (m, M.insert bag m vis)
      go (m, vis) ((bag', n) : cts) = go (m+m'*n, vis') cts
        where
          (m', vis') = dfs' bags bag' vis

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

-- Parsing stuff
getDatas :: String -> IO Bags
getDatas filename = parseDatas <$> BC.readFile filename

-- Finally, we found a way to parse using parseOnly only once time.
parseDatas :: ByteString -> Bags
parseDatas str =
  either error
         id
         (parseOnly parseRecords str)

-- The trick is to split on endOfLine, not on ".\n"
parseRecords :: Parser Bags
parseRecords = buildBags <$> sepBy1' parseBag endOfLine

-- Parses a line.
-- Each line is shaped as:
-- shiny gold bags contain 1 foo bar bag, 2 baz qux bags.
-- or:
-- foo bar bags contain no other bags.
-- The constant strings are " bags contain ", " bags, " or " bag, ",
-- " bags." or " bag."
-- A bag can contain no bags, 1 kind of other bag (any amount) or several
-- king of other bags.
-- Each line ends up with a '.' so we parse it.
parseBag :: Parser (Bag, [Content])
parseBag = do
  bag <- manyTill' (notChar '.') (string " bags contain ")
  content <- sepBy1' parseContain (string ", ") <|> parseNoOtherBags
  void (char '.') -- we manage the final '.' here.
  pure (BC.pack bag, content)

-- The other trick is to parse the string "no other bags"
-- explicitly. That way we discard it from the input string.
parseNoOtherBags :: Parser [Content]
parseNoOtherBags = string "no other bags" $> []

-- We use ", " as separator, so we can't use
-- anyChar to parse the bag. Instead we must use
-- (notChar ',') to say to Attoparsec to stop
-- parsing at character ','
parseContain :: Parser Content
parseContain = do
  n <- decimal
  skipSpace
  bag <- manyTill' (notChar ',') (string " bag")
  void (option 's' (char 's')) -- skips an optional 's'
  pure (BC.pack bag, n)

buildBags :: [(Bag, [Content])] -> Bags
buildBags = foldl' f M.empty
  where
    f acc (bag, contained) = M.insert bag contained acc
