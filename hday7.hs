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
import Data.Functor
  (void
  ,($>)
  )
import Control.Applicative
  ((<|>)
  ,optional
  )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Attoparsec.ByteString.Char8
  (Parser
  ,parseOnly
  ,decimal
  ,sepBy1'
  ,string
  ,char
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
part1 bags = sum  (M.foldlWithKey' f M.empty bags)
  where
    f  visited bag _
      | bag == needle = visited
      | otherwise = dfs1 bags bag visited

dfs1 :: Bags -> Bag -> Visited -> Visited
dfs1 bags bag visited
  | bag `M.member` visited = visited -- bag is already seen
    -- we'll walk along current bag's content
    -- Before we insert the current bag in visited as if doesn't contain needle
  | otherwise = foldr f (M.insert bag 0 visited) (bags ! bag)
    where
      f (bag', _) vis
        | bag' == needle = M.insert bag 1 vis -- the needle is in  current bag
        | n == 1         = M.insert bag 1 vis' -- the needle is in bag' or in  its children
        | otherwise      = vis' -- the needle wasn't found yet, we'll look for in next bags
          where
            vis' = dfs1 bags bag' vis -- we are exploring in depth first…
            n = vis' ! bag'

part2 :: Bags -> Int
part2 bags = fst (dfs2 bags needle M.empty) - 1

-- How to write dfs2 using State?
dfs2 :: Bags -> Bag -> Visited -> (Int, Visited)
dfs2 bags bag visited
  | bag `M.member` visited = (visited ! bag, visited)
  | otherwise = foldl' f (1, visited) (bags ! bag)
      where
        f (acc, vis) (bag', n)  = (acc + m'* n, vis')
          where
            (m', vis') = dfs2 bags bag' vis

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
  void (optional (char 's')) -- skips an optional 's'
  pure (BC.pack bag, n)

buildBags :: [(Bag, [Content])] -> Bags
buildBags = foldl' f M.empty
  where
    f acc (bag, contained) = M.insert bag contained acc
