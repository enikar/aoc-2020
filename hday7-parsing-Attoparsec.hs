
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}


import Data.List (foldl')
import Data.Map.Strict (Map)
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
  ,endOfLine
  ,notChar
  ,char
  ,decimal
  ,skipSpace
  ,string
  ,sepBy1'
  ,manyTill'
  ,option
  )

type Bags = Map Bag [Content]
type Bag = ByteString
type Content = (Bag, Int)

getDatas :: IO Bags
getDatas = parseDatas <$> BC.readFile "day7.txt"

-- Finally, we a way to parse with this method.
-- Look at parseRecords, parseBag and parseContain.
parseDatas :: ByteString -> Bags
parseDatas str =
  either error
         id
         (parseOnly parseRecords str)

-- The trick is to split on the endOfLine not on
-- the string ".\n". The '.' is parsed later
parseRecords :: Parser Bags
parseRecords = buildBags <$> sepBy1' parseBag endOfLine

-- The other trick is to parse the string "no other bags"
-- explicitly. That way we discard it from the input string.
parseNoOtherBags :: Parser [Content]
parseNoOtherBags = string "no other bags" $> []

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
  bag <- manyTill' (notChar '\n') (string " bags contain ")
  bags <- sepBy1' parseContain (string ", ") <|> parseNoOtherBags
  void (char '.') -- we manage '.' here.
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
  void (option 's' (char 's')) -- skip an optional 's'
  pure (BC.pack bag, n)

buildBags :: [(Bag, [Content])] -> Bags
buildBags = foldl' f M.empty
  where
    f acc (bag, contained) = M.insert bag contained acc
