-- AoC 2020 day 4

-- a very stupid solution, but it works.
-- Write code for this puzzle was awfull

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')

import Control.Applicative ((<|>))
import Data.Maybe
  (fromMaybe
  ,isJust
  ,fromJust
  )
import Data.Either (fromRight)
import Data.Functor (void)
import Data.Char
  (isSpace
  ,isDigit
  )
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
  ,option
  )

data Height = Inch Int
              |CM Int
              |NoUnit Int
              |None
            deriving (Show, Eq)

data Passeport =
  Passeport {byr :: Maybe Int
            ,iyr :: Maybe Int
            ,eyr :: Maybe Int
            ,hgt :: Height
            ,hcl :: Maybe ByteString
            ,ecl :: Maybe ByteString
            ,pid :: Maybe ByteString
            ,cid :: Maybe ByteString
            ,valid :: Bool
            } deriving (Show)

main :: IO ()
main = do
  pps <- getDatas "day4.txt"
  let pps_checked = filter validP pps
  printSolution "Part1" (length pps_checked)
  printSolution "Part2" (part2 pps_checked)

-- part1 :: [Passeport] -> Int
-- part1 = foldl' f 0
--   where
--     f acc pp
--       |validP pp = acc+1
--       |otherwise = acc

validP :: Passeport -> Bool
validP Passeport {..} = isJust byr
                        && isJust iyr
                        && isJust eyr
                        && hgt /= None
                        && isJust hcl
                        && isJust ecl
                        && isJust pid
                        && valid

ecls :: [ByteString]
ecls = ["amb"
       ,"blu"
       ,"brn"
       ,"gry"
       ,"grn"
       ,"hzl"
       ,"oth"
       ]

hexDigits :: String
hexDigits = "0123456789abcdef"

validP' :: Passeport -> Bool
validP' Passeport {..} = checked
  where
    between inf sup val = val >= inf && val <= sup


    checked = between 1920 2002 (fromJust byr)
              && between 2010 2020 (fromJust iyr)
              && between 2020 2030 (fromJust eyr)
              && fromJust ecl `elem` ecls
              && BC.length pid' == 9
              && BC.all isDigit pid'
              && BC.head hcl' == '#'
              && BC.all (`elem` hexDigits) (BC.tail hcl')
              && checkHeight
      where
        pid' = fromJust pid
        hcl' = fromJust hcl
    checkHeight = case hgt of
                    CM n   -> between 150 193 n
                    Inch n -> between 59 76 n
                    _      -> False


part2 :: [Passeport] -> Int
part2 = foldl' f 0
  where
    f acc pp
      |validP' pp = acc+1
      |otherwise  = acc

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

-- parsing stuff
getDatas :: String -> IO [Passeport]
getDatas filename = parseDatas <$> BC.readFile filename

parseDatas :: ByteString -> [Passeport]
parseDatas str =
    either error
           id
           (parseOnly parsePassports str)

parsePassports :: Parser [Passeport]
parsePassports = sepBy1' parsePassport (string "\n\n")

parsePassport :: Parser Passeport
parsePassport = do
  fields <- sepBy1' parseField (satisfy isSpace)
  pure (buildPasseport fields)

parseField :: Parser (ByteString, ByteString)
parseField = do
  let noSep x = x /= ' ' && x /= '\n' && x /= ':'
  key <- many1 (satisfy noSep)
  void (char ':')
  val <- many1 (satisfy noSep)
  pure (BC.pack key, BC.pack val)


buildPasseport :: [(ByteString, ByteString)] -> Passeport
buildPasseport = foldl' build lilooMultiPass
  where
    lilooMultiPass = Passeport
      {byr = Nothing
      ,iyr = Nothing
      ,eyr = Nothing
      ,hgt = None
      ,hcl = Nothing
      ,ecl = Nothing
      ,pid = Nothing
      ,cid = Nothing
      ,valid = True
      }

    build pp (k, v) =
      case k of
        "byr" -> pp {byr = Just v'} -- Int
        "iyr" -> pp {iyr = Just v'} -- Int
        "eyr" -> pp {eyr = Just v'} -- Int
        "hgt" -> pp {hgt = parseHgt v} -- Mesure (in inch or centimeter)
        "hcl" -> pp {hcl = Just v} -- string
        "ecl" -> pp {ecl = Just v} -- string
        "pid" -> pp {pid = Just v} -- string of nine digits
        "cid" -> pp {cid = Just v}
        _     -> pp {valid = False}
      where
        v' = fst (fromMaybe errorBuild (BC.readInt v))
        errorBuild = error ("Error: buildPasseport: field: "
                            <> show k
                            <> ": "
                            <> show v
                            <> " is not an Int"
                           )

parseHgt :: ByteString -> Height
parseHgt str =
  fromRight None (parseOnly go str)
  where
    go :: Parser Height
    go = do
      n <- decimal
      unit <- option "none" (string "cm" <|> string "in")
      pure (case unit of
              "cm" -> CM n
              "in" -> Inch n
              _    -> NoUnit n)
