module Day2.Passwords (part1, part2) where

import Data.Maybe (mapMaybe)

data Entry = Entry {first :: Int, second :: Int, char :: Char, password :: String}

-- Parse the password entries and count the number of occurences of the specified char and check for bounds
part1 :: IO String
part1 = do
  show . count id . map isValid1 <$> parseInput

-- Parse the password entries and XOR the char check in the specified positions
part2 :: IO String
part2 = do
  show . count id . map isValid2 <$> parseInput

parseInput :: IO [Entry]
parseInput = do
  file <- readFile "Day2/input.txt"
  return (mapMaybe parseEntry (lines file))

parseEntry :: String -> Maybe Entry
parseEntry s = do
  [bounds, c : ":", password] <- pure $ words s
  [first, second] <- pure $ splitOn '-' bounds
  pure $ Entry (read first) (read second) c password

isValid1 :: Entry -> Bool
isValid1 Entry {first, second, char, password} = first <= occurences && occurences <= second
  where
    occurences = count (== char) password

isValid2 :: Entry -> Bool
isValid2 Entry {first, second, char, password} = (password !! (first - 1) == char) /= (password !! (second - 1) == char)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

splitOn :: Char -> String -> [String]
splitOn delim = splitter []
  where
    splitter acc [] = [reverse acc] -- end of string, flush the accumulator
    splitter acc (c : cs)
      | c == delim = reverse acc : splitter [] cs -- hit delimiter, flush the accumulator
      | otherwise = splitter (c : acc) cs -- keep building current substring
