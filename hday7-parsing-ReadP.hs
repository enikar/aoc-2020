import Data.List (foldl')
import Text.ParserCombinators.ReadP
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Functor (void, ($>))
import Data.Char (isDigit)

type Bag = String
type Content = (Bag, Int)
type Bags = Map Bag [Content]


getDatas :: String -> IO Bags
getDatas filename = parseDatas <$> readFile filename

parse :: ReadP a -> ReadS a
parse = readP_to_S

parseDatas :: String -> Bags
parseDatas str = case readP_to_S parseBags str of
                   []  -> error "Error: parseDatas: can't parse"
                   [x] -> fst x
                   _   -> error "Error: parseDatas: too many possibilities"

parseBags :: ReadP Bags
parseBags = do
  bags <- buildBags <$> sepBy1 parseBag (char '\n')
  optional (char '\n')
  eof
  pure bags

parseBag :: ReadP (Bag, [Content])
parseBag = do
  bag <- manyTill (satisfy (/= '.')) (string " bags contain ")
  bags <- sepBy1 parseContain (string ", ") <++ parseNoOtherBags
  void (char '.')
  pure (bag, bags)

parseNoOtherBags :: ReadP [Content]
parseNoOtherBags = string "no other bags" $> []

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
