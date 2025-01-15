-- AoC 2020, day 7, part1

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

part1 :: Bags -> Int
part1 bags = (length . M.filter (== 1)) (M.foldlWithKey' f M.empty bags)
  where
    f  visited bag _
      | bag == "shiny gold" = visited
      | otherwise = dfs bags bag visited

dfs ::  Bags -> Bag -> Visited -> Visited
dfs bags bag visited
  | bag `M.member` visited =  visited
  | otherwise = go visited (bags ! bag)
    where
      go :: Visited -> [Content] -> Visited
      go vis []                = M.insert bag 0 vis
      go vis ((bag',_):cts)
        | bag' == "shiny gold" = M.insert bag 1 vis
        | n == 1               = M.insert bag 1 vis'
        | otherwise            = go vis cts
          where
            vis' = dfs bags bag' vis
            n = vis' ! bag'

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
