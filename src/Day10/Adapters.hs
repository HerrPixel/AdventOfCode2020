module Day10.Adapters (part1, part2) where

import Data.List (sort)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet


-- Sort the list and count differences, see `fullChainValue` for an expanded explanation
part1 :: IO String
part1 = do
  show . fullChainValue <$> parseInput

-- It does not matter how the chain looks when it arrives at the same value n,
-- we therefore store for each adapter the nr of combinations to the device joltage starting from that adapter
-- then we can just recursively calculate this and get lower adapter values by summing up the higher following ones.
part2 :: IO String
part2 = do
  show . adapterChains <$> parseInput

parseInput :: IO [Int]
parseInput = do
  file <- readFile "Day10/input.txt"
  return (map read (lines file))

-- Get the value of the chain that uses every adapter
-- Notice that we can build this chain by sorting all adapter values and connecting them that way
-- Then the problem reduces to counting the number of differences that are equal to 1 and 3 and multiplying them
fullChainValue :: [Int] -> Int
fullChainValue list = countOccurences differences 1 * countOccurences differences 3
  where
    adapters = sort list
    joltages = adapters ++ [maximum adapters + 3]
    differences = zipWith (-) joltages (0:joltages)

countOccurences :: [Int] -> Int -> Int
countOccurences haystack needle = length $ filter (==needle) haystack

-- We build a map where at position i we have the number of combinations that lead to the device joltage,
-- We can calculate this value for any n by summing the values for n+1, n+2 and n+3
-- By lazy evaluation we can therefore build this map recursively
adapterChains :: [Int] -> Int
adapterChains list = nrOfCombinations
  where
    adapters = sort list
    device = maximum adapters + 3
    joltages = IntSet.fromList (0:adapters ++ [device])
    combinations = flip IntMap.fromSet joltages $ \n ->
      if n == device
        then 1
        else sum  [ IntMap.findWithDefault 0 (n + difference) combinations | difference <- [1,2,3]]
    nrOfCombinations = combinations IntMap.! 0
