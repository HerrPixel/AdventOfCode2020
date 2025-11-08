module Day4.Passports (part1, part2) where

import Data.Char (isDigit)
import Data.Maybe (isJust)

part1 :: IO String
part1 = show . count isValid1 <$> parseInput

part2 :: IO String
part2 = show . count isValid2 <$> parseInput

parseInput :: IO [Passport]
parseInput = do
  file <- readFile "Day4/input.txt"
  let passport_strings = map words (splitOnBlankLines file)
  return (map fillPassport passport_strings)

data Passport = Passport {byr :: Maybe Int, iyr :: Maybe Int, eyr :: Maybe Int, hgt :: Maybe String, hcl :: Maybe String, ecl :: Maybe String, pid :: Maybe String, cid :: Maybe String}

emptyPassPort :: Passport
emptyPassPort = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

fillPassport :: [String] -> Passport
fillPassport = foldl fill emptyPassPort
  where
    fill pass str =
      case str of
        'b' : 'y' : 'r' : ':' : s -> pass {byr = Just (read s)}
        'i' : 'y' : 'r' : ':' : s -> pass {iyr = Just (read s)}
        'e' : 'y' : 'r' : ':' : s -> pass {eyr = Just (read s)}
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

isValid2 :: Passport -> Bool
isValid2 pass =
  maybe False (\n -> n >= 1920 && n <= 2002) (byr pass)
    && maybe False (\n -> n >= 2010 && n <= 2020) (iyr pass)
    && maybe False (\n -> n >= 2020 && n <= 2030) (eyr pass)
    && maybe False validHeight (hgt pass)
    && maybe False validHairColor (hcl pass)
    && maybe False (\s -> s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) (ecl pass)
    && maybe False (\s -> length s == 9 && all isDigit s) (pid pass)

validHeight :: String -> Bool
validHeight s = case reverse s of
  ('m' : 'c' : rest) -> (\n -> n >= 150 && n <= 193) (read (reverse rest) :: Int)
  ('n' : 'i' : rest) -> (\n -> n >= 59 && n <= 76) (read (reverse rest) :: Int)
  _ -> False

validHairColor :: String -> Bool
validHairColor ('#' : rest) = length rest == 6 && all (`elem` "0123456789abcdef") rest
validHairColor _ = False

splitOnBlankLines :: String -> [String]
splitOnBlankLines = splitter [] [] . lines
  where
    splitter acc group [] = reverse (unlines (reverse group) : acc) -- end of lines, flush the accumulator
    splitter acc group (l : ls)
      | null l = splitter (unlines (reverse group) : acc) [] ls -- hit blank line, flush the accumulator
      | otherwise = splitter acc (l : group) ls -- keep building current group

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
