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

import Data.Maybe (fromMaybe)
import Data.Functor (void)
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

-- We end up to call parseOnly on each line rather than
-- to call parseOnly only once and let Attoparsec splits
-- on ".\n". Anyway, with the preceding code we also had
-- to call parseOnly on each bag. The parsing is simpler.
parseDatas :: ByteString -> Bags
parseDatas str = foldl' f MapS.empty (BC.lines str)
  where
    f acc s = MapS.insert bag content acc
      where
        s' = fromMaybe s (BC.stripSuffix "." s)
        (bag, content) = parseRecord s'

-- Parse a line.
-- Each line is shaped as:
-- shiny gold bags contain 1 truc machin bag, 2 bidules cheval bags.
-- or:
-- truc bidule bags contain no other bags.
-- The constant strings are " bags contain ", " bags, " or " bag, ",
-- " bags." or " bag."
-- A bag can contain no bags, 1 kind of other bag (any amount) or several
-- king of other bags.
parseRecord :: ByteString -> (Bag, [Content])
parseRecord str = either error
                         id
                         (parseOnly parseBag str)

-- Each line is terminated by '.'. But, as usual, we
-- can't parse (char '.') at the end, if we do the
-- parsing fails. This is why we strip the ended '.'
-- before parsing.
-- We parse until we encounter the
-- string " bags contain ", to get the bag.
-- The remainder is its content if any.
-- We doesn't parse the string "no other bags", instead
-- we try to parse the content if that fails, we return
-- the empty list.
parseBag :: Parser (Bag, [Content])
parseBag = do
  bag <- manyTill' (notChar '.') (string " bags contain ")
  bags <- sepBy1' parseContain (string ", ") <|> pure []
  pure (BC.pack bag, bags)

-- We use ", " as separator, so we can't use
-- anyChar to parse the bag. Instead we must use
-- (notChar ',') to say to Attoparsec to stop
-- parsing at character ','
parseContain :: Parser Content
parseContain = do
  n <- decimal
  skipSpace
  bag <- manyTill' (notChar ',') (string " bag")
  void (option 's' (char 's')) -- when there is "bags"
  pure (BC.pack bag, n)
