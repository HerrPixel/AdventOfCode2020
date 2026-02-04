module Day9.Encryption (part1, part2) where

import Data.List (take)
import Data.Sequence as Seq (Seq, fromList, index)

-- For each number in the last 25, we check if (target - number) is also in the last 25 numbers and is not the same as number
-- If that is not the case, we found our invalid number
part1 :: IO String
part1 = do
  input <- parseInput
  return (show (firstInvalid (Data.List.take 25 input) (drop 25 input)))

-- After finding the invalidNumber, we precompute a list of cumulative Sums and find
-- the target interval with a two-pointer method described below in the 'findRange' method.
part2 :: IO String
part2 = do
  input <- parseInput
  let invalidNumber = firstInvalid (reverse (Data.List.take 25 input)) (drop 25 input)
  let cumulativeSums = fromList (scanl (+) 0 input)
  let (lower, upper) = findRange cumulativeSums 0 1 invalidNumber
  return (show (encryptionWeakness (take (upper - lower) (drop lower input))))

parseInput :: IO [Int]
parseInput = do
  file <- readFile "Day9/input.txt"
  return (map read (lines file))

-- The last 25 numbers will always be the first 25 of numberList so we don't have to iterate to the end,
-- We then output the first invalid number
firstInvalid :: [Int] -> [Int] -> Int
firstInvalid _ [] = 0
firstInvalid numberList (x:xs) =
  if not (isValid numberList x)
    then x
    else firstInvalid (x:numberList) xs

-- Check for each of the last (or first in the list) 25 numbers if (target - number) is not the same as number and if that number exists
isValid :: [Int] -> Int -> Bool
isValid numberList number = any (\x -> number - x /= number && number - x `elem` preamble) preamble
  where
    preamble = Data.List.take 25 numberList

-- Finds the range (i,j) such that all numbers from index i to j sum up to target
-- If the numbers from i to j sum up to something greater than target, we reduce it by incrementing i
-- If they sum up to something less, we increase it by incrementing j
-- This way, we save work but still definetly find the range
findRange :: Seq Int -> Int -> Int -> Int -> (Int,Int)
findRange cumulativeSums lower upper target
  | difference < target = findRange cumulativeSums lower (upper + 1) target
  | difference > target = findRange cumulativeSums (lower + 1) upper target
  | otherwise = (lower,upper)
  where
    difference = Seq.index cumulativeSums upper - Seq.index cumulativeSums lower

encryptionWeakness :: [Int] -> Int
encryptionWeakness range = minimum range + maximum range