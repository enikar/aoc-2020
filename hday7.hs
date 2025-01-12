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
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Attoparsec.ByteString.Char8
  (Parser
  ,parseOnly
  ,decimal
  ,sepBy1'
  ,string
  ,many1
  ,notChar
  ,skipSpace
  )

type Bags = Map Bag [Content]
type Bag = ByteString
type Content = (Bag, Int)

main :: IO ()
main = do
  bags <- getDatas "day7.txt"
  printSolution "Part1" (part1' bags)


-- We start by seaching all bags that contain "shiny gold" bag.
-- We loop for each such bag, until we there are none left.
-- What about construct a reverse mapping that says what bags
-- contain directly (and how many) another one?
-- We use a recursive function (go).
part1 :: Bags -> Int
part1 = go 0 (Set.singleton "shiny gold")
  where
    go n needles bags
      | null needles = n -- Sets are Foldable
      | otherwise    = go (n+m) needles' bags'
        where
          needles' = search needles bags'
          m = length needles' -- as above
          bags' = Set.foldl' (flip MapS.delete) bags needles

-- An alternative way without writing an explicit recursive
-- function. There is yet another way to do it with until, and
-- many others I haven't thought of.
part1' :: Bags -> Int
part1' bags0 =
  toResult
  . last
  . takeWhile p
  $ iterate  f (0, Set.singleton "shiny gold", bags0)
   where
     toResult (x, _, _) = x
     p (_, x, _) = not (null x)
     f (n, needles, bags) = (n+m, needles', bags')
       where
         needles' = search needles bags'
         m = length needles'
         bags' = Set.foldl' (flip MapS.delete) bags needles

-- searches all bags which contain bags that are in needles.
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

-- Here the parsing is tough, we found a way to
-- parse the input but it's cumbersome.
parseDatas :: ByteString -> Bags
parseDatas str =
  either error
         id
         (parseOnly parseRecords str)

parseRecords :: Parser Bags
parseRecords = buildBags <$> sepBy1' parseBag (string ".\n")

-- First we need to exclude the charater '.', since we split on ".\n"
-- Second, because there is not '.' in the string " contain " we
-- can't use (sepBy1' (notChar '.') (string " contain ")). So
-- as last ressort we split on " contain " with splitOn.
-- Finally we use parseOnly to get the content of the bag.
parseBag :: Parser (Bag, [Content])
parseBag = do
  s <- many1 (notChar '.')
  let xs = splitOn " contain " s
      (bag, rest) = case xs of
                      [b, r] -> (toBag b, BC.pack r)
                      _      -> error "Error: parseBag"
      bags = parseRest rest
  pure (bag, bags)

-- Equals to content of the current bag. If we encounter
-- the string "no other bag", the content is an empty list.
-- We are not managing directly this case, instead parseContain
-- only parse a content of the form: a number, a space and
-- the descrition of a bag (i.e. a string of two words).
-- So when the parsing fails we return an empty list.
parseRest :: ByteString -> [Content]
parseRest str = fromRight  [] (parseOnly parser str)
  where
    parser = sepBy1' parseContain (string ", ")

-- We use ", " as separator, so we can't use
-- (many1 anyChar) to parse the bag. Instead we must use
-- (many1 (notChar ',')) to say to Attoparsec to stop
-- parsing at character ','
parseContain :: Parser Content
parseContain = do
  n <- decimal
  skipSpace
  bag <- many1 (notChar ',')
  pure (toBag bag, n)

buildBags :: [(Bag, [Content])] -> Bags
buildBags = foldl' f MapS.empty
  where
    f acc (b, contained) = MapS.insert b contained acc

-- toBag converts the String to ByteString, and strips
-- one of these suffix " bags" or " bag", if any.
toBag :: String -> Bag
toBag str = fromJust (BC.stripSuffix " bags" str'
                      <|> BC.stripSuffix " bag" str'
                      <|> Just str')
  where
    str' = BC.pack str
