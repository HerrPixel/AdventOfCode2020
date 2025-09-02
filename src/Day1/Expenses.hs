module Day1.Expenses (part1, part2) where

import Data.List (find)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)

-- Find the first value x in the list, such that (2020 - x) exists, and if so, return x * (2020 -x)
part1 :: IO String
part1 = do
  input <- parseInput
  let (a, b) = fromJust (findPair 2020 input)
  pure (show (a * b))

{-
Find the first value x in the list, such that there is a pair of values in the list summing up to (target -x).
Notice that this is exactly the first part, so we use that function.
Then return the product of x * (product of the pair)
-}
part2 :: IO String
part2 = do
  input <- parseInput
  let (a, b, c) = fromJust (findTriple 2020 input)
  pure (show (a * b * c))

-- fromJust . fmap (\(x, y, z) -> x * y * z) . findTriple 2020 <$> parseInput

parseInput :: IO [Int]
parseInput = do
  file <- readFile "Day1/input.txt"
  return (map read (lines file))

findPair :: Int -> [Int] -> Maybe (Int, Int)
findPair target nums = do
  a <- find (\x -> (target - x) `elem` nums) nums -- find first x such that (target - x) exists in the list
  let b = target - a
  pure (a, b)

findTriple :: Int -> [Int] -> Maybe (Int, Int, Int)
findTriple target nums = do
  (a, b) <- listToMaybe (mapMaybe (\x -> findPair (target - x) nums) nums) -- find first x such that (target - x) can be decomposed into a sum from two other elements from the list
  let c = target - a - b
  pure (a, b, c)
