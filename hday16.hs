-- AoC 2020, day 16

-- Laborious solution, we use a backtracking algorithm for part2.
-- It's quite slow: 1m22s (real time).
-- Maybe we should use Vector instead of IntMap and Text instead of
-- String.

{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.List
  (foldl'
  ,isPrefixOf
  )
import Data.Maybe
  (listToMaybe
  ,fromMaybe
  )
import Data.Map (Map)
import Data.Map qualified as M
import Data.IntMap
  (IntMap
  ,(!)
  )
import Data.IntMap qualified as IM
import Data.Set (Set)
import Data.Set qualified as S
import Data.Ix (inRange)

--  modules for parsing
import Data.Char (isDigit)
import Data.Functor (void)
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,sepBy1
  ,munch1
  ,satisfy
  ,many1
  ,char
  ,string
  ,eof
  ,optional
  )

-- Some types defintions
type Range = (Int, Int)
type Properties = Map String (Range, Range)
data Game = Game {properties :: Properties
                 ,ourTicket :: [Int]
                 ,nearby :: [[Int]]
                 } deriving (Show)

type Rules = IntMap (Set String)
type Goal = Map String Int

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO Game
getDatas filename = parseDatas <$> readFile' filename

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
inProps props n = M.foldr f False props
  where
    f _        True = True
    f (r1, r2) _    = inRange r1 n || inRange r2 n

-- After experimenting a bit, for each tickect there is one field that
-- doesn't match one property. First we build an IntMap for position
-- [0..19] (there are 20 fields by ticket and 20 properties) that
-- indicate which fields are possible. Then we backtrack to find
-- the first solution.
part2 :: Game -> Int
part2 game = foldl' f 1 (zip [0..] ticket)
  where
    ticket = ourTicket game
    rules = buildRules game -- we prepare the backtracking

    -- The function solutions try to find all possibilities but we
    -- retain only the first one, thanks to listToMaybe.
    sol = fromMaybe errPart2 (listToMaybe (solutions 0 rules M.empty))
    errPart2 = error "Error: part2: no solution"

    -- When we found a solution, we filter all departure fields and
    -- build a list of their indices in the solution
    sol' = M.foldlWithKey' selectDeparture [] sol
    selectDeparture acc key n
      | "depar" `isPrefixOf` key = n:acc
      | otherwise                = acc

    -- Finally, we multiply all the ticket values that correspond to
    -- the departure fields.
    f acc (p, n)
      |p `elem` sol' = acc * n
      |otherwise     = acc

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
    gprop = S.fromList (M.keys goal)

    f acc prop = M.insert prop depth goal : acc

-- Preparation of the backtracking.
buildRules :: Game -> Rules
buildRules game = foldl' finalize im [0..length props - 1]
  where
    props = M.keysSet (properties game)
    -- first we determine which fields cannot be at
    -- each position
    missings = buildMissings game

    -- Then we determine for all positions which fields
    -- are possible
    im = IM.foldlWithKey' initialize IM.empty missings

    initialize acc key props' = IM.insert key props'' acc
      where
        props'' = S.difference props (S.fromList props')

    -- Finally, we add the positions for which all fields
    -- are possible. (There is just one with our input).
    finalize acc n
      |n `IM.member` acc = acc
      |otherwise         = IM.insert n props acc

buildMissings :: Game -> IntMap [String]
buildMissings game = foldl' f IM.empty nears
  where
    props = properties game
    -- First we select all valid tickets
    nears = filter (all (inProps props)) (nearby  game)

    -- Then we build an IntMap that represent list of
    -- missing fields at one position for all ticket
    f acc ticket = IM.alter (update prop) pos acc
      where
        (prop, pos) = noMatchingField props ticket

    update x Nothing = Just [x]
    update x (Just xs) = Just (x:xs)

-- This function assume there is only one field that doesn't match
-- all property by ticket. So it's not general.
noMatchingField :: Properties -> [Int] -> (String, Int)
noMatchingField props ticket = foldr f ("",0) (zip ticket [0..])
  where
    inProperty n (r1, r2) = inRange r1 n || inRange r2 n

    f (n, p)  acc@(s, _)
      |s /= ""   = acc
      |otherwise = case M.assocs (M.filter (not . inProperty n) props) of
                     [] -> acc
                     [(sp, _)] -> (sp, p)
                     _         -> error "Error: noMatchingField: not expected!"


-- parsing stuff
parseDatas :: String -> Game
parseDatas str =
  case readP_to_S parseGame str of
    [(x, "")] -> x
    _   -> error "Error: parseDatas: can't parse"

parseGame :: ReadP Game
parseGame = do
  props <- buildProps <$> parseProps
  void (string "\n\n")
  ticket <- parseTicket
  void (string "\n\n")
  near <- parseNearby
  optional (char '\n')
  eof
  pure Game {properties = props
            ,ourTicket = ticket
            ,nearby = near
            }

parseProps :: ReadP [(String, (Range, Range))]
parseProps = parseProp `sepBy1` char '\n'

parseProp :: ReadP (String, (Range, Range))
parseProp = do
  prop <- many1 (satisfy (/= ':'))
  void (string ": ")
  r1 <- parseRange
  void (string " or ")
  r2 <- parseRange
  pure (prop, (r1, r2))

parseRange :: ReadP Range
parseRange =
  liftA2 (,)
         positive
         (char '-' *> positive)

positive :: ReadP Int
positive = read <$> munch1 isDigit

parseTicket :: ReadP [Int]
parseTicket = do
  void (string "your ticket:\n")
  positive `sepBy1` char ','

parseNearby :: ReadP [[Int]]
parseNearby = do
  void (string "nearby tickets:\n")
  (positive `sepBy1` char ',') `sepBy1` char '\n'

buildProps :: [(String, (Range, Range))] -> Properties
buildProps ls = foldl' f M.empty ls
  where
    f acc (prop, rs) = M.insert prop rs acc
