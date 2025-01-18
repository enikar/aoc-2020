import Data.List (foldl')
import Text.ParserCombinators.ReadP
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Functor (void, ($>))
import Data.Char (isDigit)

type Bag = String
type Content = (Bag, Int)
type Bags = Map Bag [Content]


getDatas :: IO Bags
getDatas = parseDatas <$> readFile "day7.txt"

-- Extracts datas from readP
parseDatas :: String -> Bags
parseDatas str = case readP_to_S parseBags str of
                   []  -> error "Error: parseDatas: can't parse"
                   [x] -> fst x
                   _   -> error "Error: parseDatas: too many possibilities"

-- Each bag is in its line. So we split the string on end of line.
parseBags :: ReadP Bags
parseBags = do
  bags <- buildBags <$> sepBy1 parseBag (char '\n')
  optional (char '\n') -- some file is terminated with a new line, some is not.
  eof
  pure bags

-- a bag record is shaped as:
-- <bag> bags contain <n> <bag> bag[s][, <n> <bag>...].
-- or
-- <bag> bags contain no other bags.
-- where bag = two words ("foo bar")
--       n is a number.
-- So, to get the current bag, we consume all characters until we encounter
-- the string " bags contain ".
-- Next we try try to parse several bags sepatated by ', '
-- or the string "no other bags"
-- Finally we parse the ended '.'
parseBag :: ReadP (Bag, [Content])
parseBag = do
  bag <- manyTill (satisfy (/= '.')) (string " bags contain ")
  bags <- sepBy1 parseContain (string ", ") <++ parseNoOtherBags
  void (char '.')
  pure (bag, bags)

parseNoOtherBags :: ReadP [Content]
parseNoOtherBags = string "no other bags" $> []

-- Parse a bag in the content.
-- This is shaped as: <n>" "<bag>" bag[s]"
-- That is a number, a space, a bag (in two words) and
-- the string " bag" or " bags".
-- So to parse the bag, we consume all characters until we encounter
-- the string " bag", the we parse an optional 's'.
parseContain :: ReadP Content
parseContain = do
  n <- positive
  skipSpaces
  bag <- manyTill (satisfy (/= ',')) (string " bag")
  optional (char 's')
  pure (bag, n)


positive :: ReadP Int
positive = read <$> munch1 isDigit

buildBags :: [(Bag, [Content])] -> Bags
buildBags = foldl' f M.empty
  where
    f acc (b, contained) = M.insert b contained acc
