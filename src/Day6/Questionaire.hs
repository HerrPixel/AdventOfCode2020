module Day6.Questionaire (part1, part2) where

import Data.List (nub)

-- Split groups on blank lines and reduce a list of answers in a group to uniques and count the length
part1 :: IO String
part1 = do
  input <- parseInput
  let count = sum (map (length . getUniqueAnswers) input)
  return (show count)

-- Same as in Part1 but we additionally filter for only answers that everyone in a group gave
part2 :: IO String
part2 = do
  input <- parseInput
  let count = sum (map (length . getUnanimousAnswers) input)
  return (show count)

parseInput :: IO [String]
parseInput = do
  file <- readFile "Day6/input.txt"
  return (splitOnBlankLines file)

-- Get a list of answers and reduce it to uniques
getUniqueAnswers :: String -> String
getUniqueAnswers s = nub (concat (lines s))

-- For each answer, check if every questionaire contained the answer
getUnanimousAnswers :: String -> String
getUnanimousAnswers s = filter (\c -> all (\str -> c `elem` str) (lines s)) (getUniqueAnswers s)

splitOnBlankLines :: String -> [String]
splitOnBlankLines = splitter [] [] . lines
  where
    splitter acc group [] = reverse (unlines (reverse group) : acc) -- end of lines, flush the accumulator
    splitter acc group (l : ls)
      | null l = splitter (unlines (reverse group) : acc) [] ls -- hit blank line, flush the accumulator
      | otherwise = splitter acc (l : group) ls -- keep building current group
