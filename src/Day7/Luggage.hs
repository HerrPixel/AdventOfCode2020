module Day7.Luggage (part1, part2) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.List (nub)
import Data.Map (Map, fromList)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Text.Regex.TDFA ((=~))

part1 :: IO String
part1 = do
  input <- parseInput
  let count = 0 -- sum (map (length . getUniqueAnswers) input)
  return (show count)

part2 :: IO String
part2 = do
  input <- parseInput
  let count = 5 -- sum (map (length . getUnanimousAnswers) input)
  return (show count)

parseInput :: IO (Map String [(Int, String)])
parseInput = do
  file <- readFile "Day6/input.txt"
  return (fromList (mapMaybe parseLine (lines file)))

-- getValidContainers :: String -> Map String [(Int,String)] -> [String]
-- getValidContainers bag rules =

-- canContainBag :: String -> String -> Map String [(Int,String)] -> Map String Bool -> (Bool,Map String Bool)
-- canContainBag bag targetBag rules memory =

parseLine :: String -> Maybe (String, [(Int, String)])
parseLine line =
  case line =~ "^([a-z]+ [a-z]+) bags contain (.*)$" :: [[String]] of
    [[_, outer, rest]] ->
      let contents = parseContents rest
       in Just (outer, contents)
    _ -> Nothing
  where
    parseContents :: String -> [(Int, String)]
    parseContents "no other bags" = []
    parseContents s = mapMaybe parseContent (splitComma s)

    parseContent :: String -> Maybe (Int, String)
    parseContent s =
      case s =~ "^([0-9]+) ([a-z]+ [a-z]+) bags?$" :: [[String]] of
        [[_, n, name]] -> Just (read n, name)
        _ -> Nothing

strip :: String -> String
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

splitComma :: String -> [String]
splitComma = map strip . splitOn ','

splitOn :: Char -> String -> [String]
splitOn c s =
  case break (== c) s of
    (x, []) -> [x]
    (x, _ : xs) -> x : splitOn c xs
