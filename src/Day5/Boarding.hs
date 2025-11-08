module Day5.Boarding (part1, part2) where

import Data.List (sort)

part1 :: IO String
part1 = do
  input <- parseInput
  let maxID = maximum (map (seatID . findSeat) input)
  return (show maxID)

part2 :: IO String
part2 = do
  input <- parseInput
  let seatIDs = sort (map (seatID . findSeat) input)
  let mySeatID = findMissingSeat seatIDs
  return (show mySeatID)

parseInput :: IO [String]
parseInput = do
  file <- readFile "Day5/input.txt"
  return (lines file)

findSeat :: String -> (Int, Int)
findSeat s = let (row, _, column, _) = foldl divide (0, 127, 0, 7) s in (row, column)

divide :: (Int, Int, Int, Int) -> Char -> (Int, Int, Int, Int)
divide (loR, hiR, loC, hiC) c = case c of
  'F' -> (loR, midR, loC, hiC)
  'B' -> (midR + 1, hiR, loC, hiC)
  'L' -> (loR, loR, loC, midC)
  'R' -> (loR, loR, midC + 1, hiC)
  _ -> (hiR, loR, loC, hiC)
  where
    midR = (loR + hiR) `div` 2
    midC = (loC + hiC) `div` 2

seatID :: (Int, Int) -> Int
seatID (row, column) = 8 * row + column

findMissingSeat :: [Int] -> Int
findMissingSeat (x : y : rest)
  | y == x + 2 = x + 1
  | otherwise = findMissingSeat (y : rest)
findMissingSeat _ = 0
