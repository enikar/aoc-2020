-- AoC 2020, day 7, part1

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MapS
import Data.Set (Set)
import Data.Set qualified as Set

import Data.Maybe (catMaybes)
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

-- Finally, we found a way to parse unsing parseOnly only once time.
parseDatas :: ByteString -> Bags
parseDatas str =
  either error
         id
         (parseOnly parseRecords str)

-- Here we must the separator is endOfLine and not (string ".\n")
parseRecords :: Parser Bags
parseRecords = buildBags <$> sepBy1' parseBag endOfLine

-- Parse a line.
-- Each line is shaped as:
-- shiny gold bags contain 1 truc machin bag, 2 bidules cheval bags.
-- or:
-- truc bidule bags contain no other bags.
-- The constant strings are " bags contain ", " bags, " or " bag, ",
-- " bags." or " bag."
-- A bag can contain no bags, 1 kind of other bag (any amount) or several
-- king of other bags.
-- Each line ends up with a '.' so we parse it.
parseBag :: Parser (Bag, [Content])
parseBag = do
  bag <- manyTill' (notChar '.') (string " bags contain ")
  content <- sepBy1' parseContain (string ", ") <|> parseNoOtherBags
  void (char '.')
  pure (BC.pack bag, catMaybes content)

parseNoOtherBags :: Parser [Maybe Content]
parseNoOtherBags = string "no other bags" $> [Nothing]

-- We use ", " as separator, so we can't use
-- anyChar to parse the bag. Instead we must use
-- (notChar ',') to say to Attoparsec to stop
-- parsing at character ','
parseContain :: Parser (Maybe Content)
parseContain = do
  n <- decimal
  skipSpace
  bag <- manyTill' (notChar ',') (string " bag")
  void (option 's' (char 's')) -- when there is "bags"
  pure (Just (BC.pack bag, n))

buildBags :: [(Bag, [Content])] -> Bags
buildBags = foldl' f MapS.empty
  where
    f acc (b, contained) = MapS.insert b contained acc
