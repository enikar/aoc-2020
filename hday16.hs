-- AoC 2020, day 16

-- Laborious solution, we use a backtracking algorithm for part2.
-- The use of Vector + ByteString improves substantially the speed:
-- With IntMap + String : 1m22s
-- With Vector + ByteString : 28s
-- So we map field names to Int from 0 to 19 and then use IntSet and
-- IntMap instead of Set and Map. It's speed up again the program. Now
-- we compute part1 and part2 in less than 14s!

-- The input file is made up of 3 types of data. First, we have fields,
-- which we also call properties, with validity range for each of
-- them. There are 20 such fields.

-- Next comes the description of our ticket, and finally a list of
-- tickets in the vicinity of ours. Each ticket consists of a list of 20
-- numbers.

-- Each of these numbers must correspond to a field.
-- All tickets are constructed in the same way, but we don't know the order
-- of the fields.

-- The aim of the puzzle is to discover this order.

-- There are also invalid tickets that we need to eliminate. These
-- tickets have at least 1 number that doesn't match any of the fields.

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Eta reduce" -}

import Data.List (foldl')
import Data.Maybe
  (listToMaybe
  ,fromMaybe
  )
import Data.Tuple (swap)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Vector
  (Vector
  ,(!)
  )
import Data.Vector qualified as V
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.Ix (inRange)
import Data.Foldable
  (forM_
  ,asum
  )

import Control.Monad.Logic
  (Logic
   ,observe
  )

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
                 ,ourTicket :: Vector Int
                 ,nearby :: [[Int]]
                 } deriving (Show)

-- Rules map a position to possible Fields
type Rules = Vector IntSet
-- Goal map Field to the position in the ticket unlike what we're used
-- to i.e. not as in a vector. It is to ease the conversion to a Set
-- with IntMap.KeysSet and use IntSet.difference at each step of the
-- solution construction in part2 (see successors function)
type Goal = IntMap Int

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

-- To show there is only one solution, and field order in the ticket
printSolutions :: [([Int], [(ByteString, Int)])] -> IO ()
printSolutions sols = do
  putStrLn "Part2:"
  forM_ (zip sols [(1::Int)..]) $ \((sol, fields), n) -> do
    let s = show n <> ") Fields:"
        len = length s
        tab = BC.replicate (len+1) ' '
    putStrLn s
    forM_ fields  $ \(name, value) -> do
      BC.putStrLn (tab <> name <> ": " <> BC.pack (show value))
    putStrLn ("Departure fields values: "
              <> show sol
              <> " Product: "
              <> show (product sol))

getDatas :: String -> IO Game
getDatas filename = parseDatas <$> BC.readFile filename

main :: IO ()
main = do
  game <- getDatas "day16.txt"
  printSolution "Part1" (part1 game)
  printSolution "Part2" (part2 game)
  --printSolutions (part2' game) -- to show there is only one solution

-- Part1 is straightforward. We just collect all fields which doesn't
-- fit into any properties and sum them.
part1 :: Game -> Int
part1 (Game{..}) = foldl' f 0 (concat nearby)
  where
    f acc n
      |inProps properties n = acc
      |otherwise            = acc + n

inProps :: Properties -> Int -> Bool
inProps props n = IM.foldr f False props
  where
    f _        True = True
    f (r1, r2) _    = inRange r1 n || inRange r2 n

-- After experimenting a bit, for each tickect there is one field that
-- doesn't match one property (among the 20). First we build an IntMap
-- for position [0..19] (there are 20 fields by ticket and 20
-- "properties") that indicates which fields are possible. Then we
-- backtrack to find the first solution (and the only one).
part2 :: Game -> Int
part2 game@(Game{..}) = IM.foldlWithKey' selectDeparture 1 sol
  where
    rules = buildRules game -- we prepare the backtracking

    -- The function solutions try to find all possibilities but we
    -- retain only the first one, thanks to listToMaybe.
    sol = fromMaybe errPart2 (listToMaybe (solutions 0 rules IM.empty))
    errPart2 = error "Error: part2: no solution"

    -- When we found a solution, we filter all departure fields and
    -- mutliply all correspondant fields of our ticket.
    selectDeparture acc key n
      |isDeparture key = acc * (ourTicket ! n)
      |otherwise       = acc

    isDeparture k = "departure" `isPrefixOf` (propMap IM.! k)

-- To show there is only one solution and field order in tickets.
part2' :: Game -> [([Int], [(ByteString, Int)])]
part2' game@(Game{..}) = map f sols
  where
    rules = buildRules game
    sols = solutions 0 rules IM.empty

    isDeparture k = "departure" `isPrefixOf` (propMap IM.! k)

    transpose m = IM.fromList (map swap (IM.toList m))

    f sol = (IM.foldlWithKey' selectDeparture [] sol, fields)
      where
        fields = zip (map (propMap IM.!) (IM.elems (transpose sol)))
                     (V.toList ourTicket)

    selectDeparture acc key n
      |isDeparture key = (ourTicket ! n) : acc
      |otherwise       = acc

-- The back-tracking is written with only two functions,
-- solutions and successors.
-- It should be possible to define solutions as a list comprehension,
-- using lazyness as in Dynamic programing.
solutions :: Int -> Rules ->  Goal -> [Goal]
solutions depth rules goal
  |depth == 20 = [goal]
  |otherwise = concatMap (solutions (depth+1) rules)
                         (successors depth rules goal)

-- Alternative way to write solutions.
-- Use the Monad instance of List.
solutions' :: Int -> Rules -> Goal -> [Goal]
solutions' depth rules goal
  |depth == 20 = pure goal
  |otherwise   =
     do goal' <- successors depth rules goal
        solutions' (depth+1) rules goal'

-- Using LogicT
part2Logic :: Game -> Int
part2Logic game@(Game{..}) = IM.foldlWithKey' selectDeparture 1 sol
  where
    rules = buildRules game
    -- `observe' is named `find' in prolog
    sol = observe (solutionsLogic 0 rules IM.empty)

    isDeparture k = "departure" `isPrefixOf` (propMap IM.! k)

    selectDeparture acc key n
      |isDeparture key = (ourTicket ! n) * acc
      |otherwise       = acc

-- solutionsLogic :: MonadPlus m => Int -> Rules -> Goal -> m Goal
-- Use the MonadPlus instance of the Logic type
-- Note: It is almost the same code as solutions'
solutionsLogic :: Int -> Rules -> Goal -> Logic Goal
solutionsLogic depth rules goal
  |depth == 20 = pure goal
  |otherwise   =
     do -- first get the successors of goal
       let goals = successors depth rules goal
       -- then for each successor try to find a solution.
       -- Here asum is used as a chooser.
       -- That works like in the List Monad
       -- but before we map [Goal] -> [Logic Goal]
       -- to be in the Logic monad
       goal' <- asum (map pure goals)
       solutionsLogic (depth+1) rules goal'

-- build a list of successors of goal: the length
-- of current goal is (depth-1), next goals will be
-- of length depth.
-- Note: here we use S.foldl' to build the list instead
-- of map over a list of Fields because it's slightly
-- faster.
successors :: Int -> Rules ->  Goal -> [Goal]
successors depth rules goal = S.foldl' f [] nextFields
  where
    -- gets the possible fields at this position
    props = rules ! depth
    -- removes field already in the goal
    nextFields = S.difference props (IM.keysSet goal)
    -- builds a new goal for each new field
    f acc prop = IM.insert prop depth goal : acc


-- Preparation of the backtracking.
buildRules :: Game -> Rules
buildRules Game{..} = foldl' build v0 nears
  where
    -- First, we select all valid tickets
    nears = filter (all (inProps properties)) nearby

    -- Initial vector with all properties for each index
    v0 = V.replicate (length properties) kprops

    kprops = IM.keysSet properties

    -- We update the vector by removing missing fields at each indices.
    -- Update one element in the vector:
    -- ss is a IntSet, fs is [Field] (Field is an Int)
    update ss fs = S.difference ss (S.fromList fs)

    build acc ticket = V.accum update acc xs
      where
        -- xs in an association list of (pos, [Field]) as expected
        -- by V.accum
        xs = noMatchingFields properties ticket

noMatchingFields :: Properties -> [Int] -> [(Int, [Field])]
noMatchingFields props ticket = foldl' f [] (zip [0..] ticket)
  where
    inProperty n (r1, r2) = inRange r1 n || inRange r2 n

    -- we accumulate all Fields that doesn't match properties
    -- at one index in the ticket.
    f acc (index, value) =
      case IM.assocs (IM.filter (not . inProperty value) props) of
        [] -> acc
        xs -> (index, map fst xs) : acc


-- parsing stuff
parseDatas :: ByteString -> Game
parseDatas str =
  either error
         id
         (parseOnly parseGame str)

parseGame :: Parser Game
parseGame = do
  (properties, propMap) <- buildProps <$> parseProps
  endOfLine
  endOfLine
  ourTicket <- V.fromList <$> parseTicket
  endOfLine
  endOfLine
  nearby <- parseNearby
  pure Game {..}

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
  *> parseList

parseList :: Parser [Int]
parseList = decimal `sepBy1` char ','

parseNearby :: Parser [[Int]]
parseNearby =
  "nearby tickets:\n"
  *> parseList `sepBy1` endOfLine

buildProps :: [(ByteString, (Range, Range))] -> (Properties, IntMap ByteString)
buildProps ls = foldl' f (IM.empty, IM.empty) (zip [0..] ls)
  where
    f (props, maps) (n, (prop, rs)) = (IM.insert n rs props, IM.insert n prop maps)
