-- AoC 2020, day 7, part1

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')
import Data.List.Extra (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MapS
import Data.Set (Set)
import Data.Set qualified as Set

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Attoparsec.ByteString.Char8
  (Parser
  ,parseOnly
  ,decimal
  ,sepBy1'
  ,char
  ,string
  ,many1
  ,satisfy
  ,endOfLine
  ,skipSpace
  )

type Bags = Map Bag [Content]
type Bag = ByteString
type Content = (Bag, Int)

main :: IO ()
main = do
  bags <- getDatas "day7.txt"
  printSolution "Part1" (part1 bags)

-- is there a way to accomplish the same thing is one pass?
part1 :: Map Bag [Content] -> Int
part1 = go 0 (Set.singleton "shiny gold")
  where
    go n needles bags
      | null needles = n -- Sets are Foldable
      | otherwise    = go (n+m) needles' bags'
        where
          needles' = search needles bags'
          m = length needles' -- as above

          bags' = Set.foldl' deleteKey bags needles
            where
              deleteKey acc k = MapS.delete k acc


search :: Set Bag -> Bags -> Set Bag
search needles bags = MapS.foldlWithKey' f Set.empty bags
  where
    f acc k vs = Set.union acc (foldl' g Set.empty vs)
      where
        g acc' v = Set.union acc' (Set.foldl' h Set.empty needles)
          where
            h acc'' needle
              | needle == fst v = Set.insert k acc''
              | otherwise       = acc''

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

-- Parsing stuff
getDatas :: String -> IO Bags
getDatas filename = parseDatas <$> BC.readFile filename

-- Here the parsing is tough
-- How to parse properly this style of input file
-- TODO: Try readP instead.
parseDatas :: ByteString -> Bags
parseDatas str =
  either error
         id
         (parseOnly parseRecords str)


parseRecords :: Parser Bags
parseRecords = do
  bags <- sepBy1' parseBag endRecord
  pure (buildBags bags)

endRecord :: Parser ()
endRecord = do
  void (char '.')
  void endOfLine

-- Here the parsing becomes cumbersome
parseBag :: Parser (Bag, [Content])
parseBag = do
  (bag, rest) <- parseBag'
  let bags = fromRight [] -- for "no other bags"
                       (parseOnly (sepBy1' parseContain (string ", "))
                                  rest)
  pure (bag, bags)

-- I can't achieve to split the string on " bags contain "
-- with attoparsec. It should exist a way to do it!
parseBag' :: Parser (Bag, ByteString)
parseBag' = do
  s <- many1 (satisfy (\c -> c /= '.' && c /= '\n'))
  let xs = splitOn " bags contain " s
  pure (case xs of
          [b, r] -> (toBag b, BC.pack r)
          _      -> error "Error: parseBag'")

-- Although, we use ", " as separator, we can't use
-- (many1 anyChar) to parse the bag. Instead we must use
-- (many1 (satisfy (/= ','))). It's odd because at this
-- point it shouldn't exist a character ',' in the string
-- we parse.
parseContain :: Parser Content
parseContain = do
  n <- decimal
  skipSpace
  bag <- many1 (satisfy (/= ','))
  pure (toBag bag, n)


buildBags :: [(Bag, [Content])] -> Bags
buildBags = foldl' f MapS.empty
  where
    f acc (b, contained) = MapS.insert b contained acc

toBag :: String -> Bag
toBag str = fromJust (BC.stripSuffix " bag" str'
                      <|> BC.stripSuffix " bags" str'
                      <|> Just str')
  where
    str' = BC.pack str
