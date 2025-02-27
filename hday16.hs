-- AoC 2020, day 16

-- Laborious solution, we use a backtracking algorithm for part2.
-- The use of Vector + ByteString improves substantially the speed:
-- With IntMap + String : 1m22s
-- With Vector + ByteString : 28s
-- So we map field names (what we call also property) to
-- Int from 0 to 19 and then use IntSet and IntMap instead of Set and
-- Map. It's speed up again the program. Now we compute part1 and part2
-- in less than 14s!

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Eta reduce" -}

import Data.List (foldl')
import Data.Maybe
  (listToMaybe
  ,fromMaybe
  )
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Vector
  (Vector
  ,(!)
  ,(//)
  )
import Data.Vector qualified as V
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.Ix (inRange)

--  modules for parsing
import Data.Functor (void)
import Data.ByteString.Char8
  (ByteString
  ,isPrefixOf
  )
import Data.ByteString.Char8 qualified as BC
import Data.Attoparsec.ByteString.Char8
  (Parser
  ,parseOnly
  ,sepBy1
  ,string
  ,char
  ,endOfLine
  ,many1
  ,decimal
  ,notChar
  )

-- Some types defintions
type Range = (Int, Int)
type Field = Int
type Properties = IntMap (Range, Range)

data Game = Game {properties :: Properties
                 ,propMap :: IntMap ByteString
                 ,ourTicket :: [Int]
                 ,nearby :: [[Int]]
                 } deriving (Show)

-- Rules map a position to possible Fields
type Rules = Vector IntSet
-- Goal map Field to the position in the ticket unlike what we're
-- used to i.e. not as in a vector.
type Goal = IntMap Int

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO Game
getDatas filename = parseDatas <$> BC.readFile filename

main :: IO ()
main = do
  game <- getDatas "day16.txt"
  printSolution "Part1" (part1 game)
  printSolution "Part2" (part2 game)


-- Part1 is straightforward. We just collect all fields which doesn't
-- fit into any properties and sum them.
part1 :: Game -> Int
part1 game = foldl' f 0 (concat (nearby game))
  where
    props = properties game

    f acc n
      |inProps props n = acc
      |otherwise       = acc + n

inProps :: Properties -> Int -> Bool
inProps props n = IM.foldr f False props
  where
    f _        True = True
    f (r1, r2) _    = inRange r1 n || inRange r2 n

-- After experimenting a bit, for each tickect there is one field that
-- doesn't match one property. First we build an IntMap for position
-- [0..19] (there are 20 fields by ticket and 20 "properties") that
-- indicates which fields are possible. Then we backtrack to find
-- the first solution.
part2 :: Game -> Int
part2 game = IM.foldlWithKey' selectDeparture 1 sol
  where
    rules = buildRules game -- we prepare the backtracking

    -- The function solutions try to find all possibilities but we
    -- retain only the first one, thanks to listToMaybe.
    sol = fromMaybe errPart2 (listToMaybe (solutions 0 rules IM.empty))
    errPart2 = error "Error: part2: no solution"

    -- When we found a solution, we filter all departure fields and
    -- mutliply all correspondant fields of our ticket.
    ticket = ourTicket game
    selectDeparture acc key n
      |isDeparture key = acc * (ticket !! n)
      |otherwise       = acc

    pmap = propMap game
    isDeparture k = "departure" `isPrefixOf` (pmap IM.! k)


-- The backtracking is written with only two functions,
-- solutions and successors.
solutions :: Int -> Rules ->  Goal -> [Goal]
solutions depth rules goal
  |depth == 20 = [goal]
  |otherwise = concatMap (solutions (depth+1) rules)
                         (successors depth rules goal)

successors :: Int -> Rules ->  Goal -> [Goal]
successors depth rules goal = S.foldl' f [] (S.difference props gprop)
  where
    props = rules ! depth
    gprop = IM.keysSet goal

    f acc prop = IM.insert prop depth goal : acc

-- Preparation of the backtracking.
-- We procede in two passes, but it must exist a way
-- to do it in one pass. But it's not so bad.
buildRules :: Game -> Rules
buildRules game = V.map initialize missings
  where
    props = IM.keysSet (properties game)
    -- First we determine which fields cannot be at
    -- each position
    missings = buildMissings game

    -- Then we compute for all positions which fields
    -- are possible
    initialize ps
      | null ps = props
      | otherwise = S.difference props (S.fromList ps)

buildMissings :: Game -> Vector [Field]
buildMissings game = foldl' f v0 nears
  where
    v0 :: Vector [Field]
    v0 = V.replicate (length props) []
    props = properties game
    -- First we select all valid tickets
    nears = filter (all (inProps props)) (nearby  game)

    -- Then we build an IntMap that represent list of
    -- missing fields at one position for all ticket
    f acc ticket = acc // [(pos, prop : props')]
      where
        props' = acc ! pos
        (prop, pos) = noMatchingField props ticket


-- This function assume there is only one field that doesn't match
-- all property by ticket. So it's not general. Else we would return
-- a [(Field, Int)]
noMatchingField :: Properties -> [Int] -> (Field, Int)
noMatchingField props ticket = fromMaybe errNoMatch field
  where
    errNoMatch = error "Error: noMatchingField: not expected!"

    inProperty n (r1, r2) = inRange r1 n || inRange r2 n

    field = foldr f Nothing (zip ticket [0..])

    f _      acc@(Just _) = acc
    f (n, p) Nothing =
      case IM.assocs (IM.filter (not . inProperty n) props) of
        []        -> Nothing
        [(sp, _)] -> Just (sp, p)
        _         -> Nothing


-- parsing stuff
parseDatas :: ByteString -> Game
parseDatas str =
  either error
         id
         (parseOnly parseGame str)


parseGame :: Parser Game
parseGame = do
  (props, pmaps) <- buildProps <$> parseProps
  endOfLine
  endOfLine
  ticket <- parseTicket
  endOfLine
  endOfLine
  near <- parseNearby
  pure Game {properties = props
            ,propMap = pmaps
            ,ourTicket = ticket
            ,nearby = near
            }

parseProps :: Parser [(ByteString, (Range, Range))]
parseProps = parseProp `sepBy1` endOfLine

parseProp :: Parser (ByteString, (Range, Range))
parseProp = do
  prop <- many1 (notChar ':')
  void (string ": ")
  r1 <- parseRange
  void (string " or ")
  r2 <- parseRange
  pure (BC.pack prop, (r1, r2))

parseRange :: Parser Range
parseRange =
  liftA2 (,)
         decimal
         (char '-' *> decimal)


parseTicket :: Parser [Int]
parseTicket =
  "your ticket:\n"
  *> decimal `sepBy1` char ','

parseNearby :: Parser [[Int]]
parseNearby =
  "nearby tickets:\n"
  *> (decimal `sepBy1` char ',') `sepBy1` endOfLine

buildProps :: [(ByteString, (Range, Range))] -> (Properties, IntMap ByteString)
buildProps ls = foldl' f (IM.empty, IM.empty) (zip [0..] ls)
  where
    f (props, maps) (n, (prop, rs)) = (IM.insert n rs props, IM.insert n prop maps)
