-- AoC 2020 Day2

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Control.Monad (void)
import Text.ParserCombinators.ReadP (many1
                                    ,satisfy
                                    ,munch1
                                    ,char
                                    ,get
                                    ,ReadP
                                    ,readP_to_S)

readInt :: String -> Int
readInt s = fromMaybe errorReadInt (readMaybe s)
  where
    errorReadInt = error ("readInt: Not an Int: " <> s)

readDatas :: IO [Rules]
readDatas = do
  datas <- lines <$> readFile "day2.txt"
  pure (map (fst . head . readP_to_S parseRules) datas)

data Rules = Rules
  {first :: Int
  ,second :: Int
  ,theLetter :: Char
  ,password :: String
  } deriving (Show)

skipMinus :: ReadP ()
skipMinus = void (char '-')

digit :: ReadP Char
digit = satisfy isDigit

digits :: ReadP String
digits = many1 digit

skipColon :: ReadP ()
skipColon = void (char ':')

skipSpace :: ReadP ()
skipSpace = void (char ' ')

parseRules :: ReadP Rules
parseRules = do
  n1 <- fmap readInt digits      -- first number
  skipMinus                      -- skip the sign '-'
  n2 <- fmap readInt digits      -- second number
  skipSpace                      -- skip one space
  aLetter <- get                 -- get just next Char
  skipColon                      -- skip the sign ':'
  skipSpace                      -- skip one space
  passwd  <- munch1 (const True) -- get the rest
  pure (Rules n1 n2 aLetter passwd )


choosePart :: String -> Rules -> Bool
choosePart "Part1" = validp
  where validp x = let c = length (filter (theLetter x ==) (password x))
                   in c >= first x && c <= second x

choosePart "Part2" = validp
  where validp x = let  p      = password x
                        coord1 = first x
                        char1  = p !! (coord1 - 1)
                        coord2 = second x
                        char2  = p !! (coord2 - 1)
                        the    = theLetter x
                   in coord1 <= length p
                      && coord2 <= length p
                      && (char1 == the && char2 /= the)
                      || (char1 /= the && char2 == the)

choosePart part = error ("choosePart: unknown part: " <> part)


showSolution :: String -> Int -> IO ()
showSolution part sol = putStrLn (part <> " answer: " <> show sol)

computeSolution :: String -> [Rules] -> Int
computeSolution part datas = length (filter (choosePart part) datas)

main :: IO ()
main = do
  datas <- readDatas
  let task part = showSolution part (computeSolution part datas)
  task "Part1"
  task "Part2"
