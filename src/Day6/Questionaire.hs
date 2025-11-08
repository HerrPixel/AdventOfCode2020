module Day6.Questionaire (part1, part2) where

import Data.List (nub)

part1 :: IO String
part1 = do
  input <- parseInput
  let count = sum (map (length . getUniqueAnswers) input)
  return (show count)

part2 :: IO String
part2 = do
  input <- parseInput
  let count = sum (map (length . getUnanimousAnswers) input)
  return (show count)

parseInput :: IO [String]
parseInput = do
  file <- readFile "Day6/input.txt"
  return (splitOnBlankLines file)

getUniqueAnswers :: String -> String
getUniqueAnswers s = nub (concat (lines s))

getUnanimousAnswers :: String -> String
getUnanimousAnswers s = filter (\c -> all (\str -> c `elem` str) (lines s)) (getUniqueAnswers s)

splitOnBlankLines :: String -> [String]
splitOnBlankLines = splitter [] [] . lines
  where
    splitter acc group [] = reverse (unlines (reverse group) : acc) -- end of lines, flush the accumulator
    splitter acc group (l : ls)
      | null l = splitter (unlines (reverse group) : acc) [] ls -- hit blank line, flush the accumulator
      | otherwise = splitter acc (l : group) ls -- keep building current group
