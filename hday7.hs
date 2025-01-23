-- AoC 2020, day 7

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List (foldl')
import Data.Map.Strict
  (Map
  ,(!)
  )
import Data.Map.Strict qualified as M
import Data.Word (Word8)
import Data.Functor
  (void
  ,($>)
  )
import Control.Monad
  (when
  ,foldM
  )
import "mtl" Control.Monad.State.Strict
  (State
  ,get
  ,modify'
  ,evalState
  )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Scanner qualified as S
import Scanner
  (Scanner
  ,anyWord8
  ,scanOnly
  ,skipSpace
  ,lookAhead
  ,decimal
  )

type Bags = Map Bag [Content]
type Bag = ByteString
type Content = (Bag, Int)
type Visited = Map Bag Int

main :: IO ()
main = do
  bags <- getDatas "day7.txt"
  printSolution "Part1" (part1 bags)
  printSolution "Part2" (yaPart2 bags)

needle :: Bag
needle = "shiny gold"

part1 :: Bags -> Int
part1 bags = sum  (foldl' f M.empty (M.keys bags))
  where
    f  visited bag
      | bag == needle = visited
      | otherwise     = dfs1 bags bag visited

dfs1 :: Bags -> Bag -> Visited -> Visited
dfs1 bags bag visited
  | bag `M.member` visited = visited -- bag is already seen
    -- we'll walk along current bag's content
    -- Before we insert the current bag in visited as if doesn't contain needle
  | otherwise              = foldr f (M.insert bag 0 visited) (bags ! bag)
    where
      f (sub, _) vis
        | sub == needle = M.insert bag 1 vis -- the needle is in current bag
        | n == 1         = M.insert bag 1 vis' -- the needle is in sub or in its children
        | otherwise      = vis' -- the needle wasn't found yet, we'll look for it in next bags
          where
            vis' = dfs1 bags sub vis -- we are exploring in depth first…
            n = vis' ! sub

-- We wrote four versions for part2, the first is a naive version
-- It is written in an imperative style.
-- In "shiny gold" there are 14177 other bags in, only, 20
-- different bags!
part2 :: Bags -> Int
part2 bags = fst (dfs2 bags needle M.empty) - 1

dfs2 :: Bags -> Bag -> Visited -> (Int, Visited)
dfs2 bags bag visited
  | bag `M.member` visited = (visited ! bag, visited)
  | otherwise              = (m, M.insert bag m visited')
      where
        (m, visited') = foldl' f (1, visited) (bags ! bag)

        f (acc, vis) (sub, n)  = (acc + m'* n, vis')
          where
            (m', vis') = dfs2 bags sub vis

-- The second version use a State. it avoids the need to
-- carry on the Visited bags. It needs some improvements, yet.
-- Probably, we need to write it again from the definition of
-- a depth first search algorithm.
part2State :: Bags -> Int
part2State bags = evalState (dfs2State bags needle) M.empty - 1

-- We could go even further, either by putting Bags in the State or
-- either by using RWS in place of State.
-- We use two mutually recursive functions, dfs2State and dfs2State'
-- dfs2State updates Visited bags.
-- dfs2State' walks through the content of bags in depth.
dfs2State :: Bags -> Bag -> State Visited Int
dfs2State bags bag = do
  w <- dfs2State' bags bag
  modify' (M.insert bag w)
  pure w

-- we name the numbers of bags contained in one bag, its weight.
dfs2State' :: Bags -> Bag -> State Visited Int
dfs2State' bags bag = do
  visited <- get
  if bag `M.member` visited -- we've already seen this bag
  then pure (visited ! bag) -- so we just return how much its weight in bag
  else let next acc (sub, n) = do -- There are n sub in bag
              w <- dfs2State bags sub -- We look for descendant in depth first
              pure (acc + w*n) -- return the partially computed weight of bag

        in -- else we visit bag and its descendants
          foldM next 1 (bags ! bag)

-- The third version is a rewrite of dfs2state that doesn't use
-- two mutually recursive functions but only one recursive function.
yaPart2 :: Bags -> Int
yaPart2 bags = evalState (yaDfs2 bags needle) M.empty - 1

yaDfs2 :: Bags -> Bag -> State Visited Int
yaDfs2 bags bag = do
  w <- yaDfs2' bags bag -- compute the weight for bag
  -- although it's not mandatory in this case
  -- we update Visited with bag and its weight.
  modify' (M.insert bag w)
  pure w -- return weight for bag

-- it isn't sure that method works in another case of
-- the present puzzle.
yaDfs2' :: Bags -> Bag -> State Visited Int
yaDfs2' bags bag = foldM next 1 (bags ! bag)
  where
    next acc (sub, n) = do -- sub is a bag contained in bag
      vis <- get
      if sub `M.member` vis           -- if we've already seen sub
      then pure (acc + (vis ! sub)*n) -- we just return how much it weights in bag
      else do
        w <- yaDfs2' bags sub    -- else we compute the weight of sub
        modify' (M.insert sub w) -- add sub to Visited with its weight
        -- Now we return the partialy computed weight of bag (argument
        -- of yaDfs2').
        -- We don't update the State Visited right away. Instead we do
        -- update it, when we return from yaDfs2 at the end or when we
        -- return from the recursive call in yaDfs2'
        pure (acc + w*n)

-- The fourth version is not a general way to explore a graph.
-- Thanks to a mistake we discovered that part2' is just
-- as effective as part2, though we don't keep a record of
-- all visited bags. It's because the graph is acyclic.
part2' :: Bags -> Int
part2' bags = dfs2' bags needle - 1

dfs2' :: Bags -> Bag ->  Int
dfs2' bags bag = foldl' f 1 (bags ! bag)
  where
    f w (sub, n) = w + w'*n
      where
        w' = dfs2' bags sub


printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

-- Parsing stuff
getDatas :: String -> IO Bags
getDatas filename = parseDatas <$> BC.readFile filename

-- we use scanOnly on each line and build Bags incrementally
parseDatas :: ByteString -> Bags
parseDatas str = foldl' f M.empty (BC.lines str)
  where
    f acc s = M.insert bag content acc
      where
        (bag, content) = either error
                                id
                                (scanOnly parseRecord s)

-- To use word8 instead of Char. It should be faster,
-- although that doesn't matter here.
charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

en, dot, comma, space, es :: Word8
en = charToWord8 'n'
dot = charToWord8 '.'
comma = charToWord8 ','
space = charToWord8 ' '
es = charToWord8 's'

-- Parses a line.
-- Each line is shaped as:
-- shiny gold bags contain 1 foo bar bag, 2 baz qux bags.
-- or:
-- foo bar bags contain no other bags.
-- The constant strings are " bags contain ", " bags, " or " bag, ",
-- " bags." or " bag."
-- A bag can contain no bags, 1 kind of other bag (any amount) or several
-- king of other bags.
parseRecord :: Scanner (Bag, [Content])
parseRecord = do
  bag <- parseBag -- first get the bag name
  void (S.take 9) -- skips " contain "
  c <- lookAhead
  case c of
    Just  v |v == en -> void (S.take 14) $> (bag, []) -- skips "no other bags."
    Nothing          -> error "Error: parseRecord" -- should not be reached
    _                -> do content <- parseContain
                           pure (bag, content)

-- parse a list of "foo bar bag[s]", separated by a ',' and ended with a '.'
parseContain :: Scanner [Content]
parseContain = do
  c <- lookAhead
  case c of
    Just v
      |v == dot   -> anyWord8 $> [] -- skip '.', closes the list [Content].
      |v == comma -> S.take 2 *> parseContain -- skips ", ", parses remaining charaters
      |otherwise  -> do
         n <- decimal -- parses the number
         skipSpace
         bag <- parseBag -- a bag follows, discard " bag" or " bags"
         rest <- parseContain -- parses the remaining string
         pure ((bag, n) : rest)
    Nothing       -> error "Error: parseContain" -- should not be reached

-- parse a string shaped as "foo bar bag" with an optional 's'
-- at the end.
parseBag :: Scanner ByteString
parseBag = do
  c1 <- S.takeWhile (/= space) -- parses "foo"
  skipSpace
  c2 <- S.takeWhile (/= space) -- parses "bar"
  skipSpace
  void (S.take 3) -- skips "bag"
  c <- lookAhead
  when (c == Just es) (void anyWord8) -- skips 's' if any
  pure (BC.unwords [c1, c2]) -- returns "foo bar" as a Scanner
