module Day4.Passports (part1, part2) where

import Data.Maybe (isJust)

part1 :: IO String
part1 = do
  show . count isValid1 <$> parseInput

part2 :: IO String
part2 = return ""

parseInput :: IO [Passport]
parseInput = do
  file <- readFile "Day4/input.txt"
  let passport_strings = map words (splitOnBlankLines file)
  return (map fillPassport passport_strings)

data Passport = Passport {byr :: Maybe (String), iyr :: Maybe (String), eyr :: Maybe (String), hgt :: Maybe (String), hcl :: Maybe (String), ecl :: Maybe (String), pid :: Maybe (String), cid :: Maybe (String)}

emptyPassPort :: Passport
emptyPassPort = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

fillPassport :: [String] -> Passport
fillPassport = foldl fill emptyPassPort
  where
    fill pass str =
      case str of
        'b' : 'y' : 'r' : ':' : s -> pass {byr = Just s}
        'i' : 'y' : 'r' : ':' : s -> pass {iyr = Just s}
        'e' : 'y' : 'r' : ':' : s -> pass {eyr = Just s}
        'h' : 'g' : 't' : ':' : s -> pass {hgt = Just s}
        'h' : 'c' : 'l' : ':' : s -> pass {hcl = Just s}
        'e' : 'c' : 'l' : ':' : s -> pass {ecl = Just s}
        'p' : 'i' : 'd' : ':' : s -> pass {pid = Just s}
        'c' : 'i' : 'd' : ':' : s -> pass {cid = Just s}
        _ -> pass

isValid1 :: Passport -> Bool
isValid1 pass =
  isJust (byr pass)
    && isJust (iyr pass)
    && isJust (eyr pass)
    && isJust (hgt pass)
    && isJust (hcl pass)
    && isJust (ecl pass)
    && isJust (pid pass)

splitOnBlankLines :: String -> [String]
splitOnBlankLines = splitter [] [] . lines
  where
    splitter acc group [] = reverse (unlines (reverse group) : acc) -- end of lines, flush the accumulator
    splitter acc group (l : ls)
      | null l = splitter (unlines (reverse group) : acc) [] ls -- hit blank line, flush the accumulator
      | otherwise = splitter acc (l : group) ls -- keep building current group

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
