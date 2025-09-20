module Day3.Airplane (part1, part2) where

import Data.Vector (Vector, fromList, (!))
import Data.Vector qualified as V (head)

part1 :: IO String
part1 = do
  grid <- parseInput
  return $ show (hitTrees grid 3 1)

part2 :: IO String
part2 = do
  grid <- parseInput
  let a = hitTrees grid 1 1
  let b = hitTrees grid 3 1
  let c = hitTrees grid 5 1
  let d = hitTrees grid 7 1
  let e = hitTrees grid 1 2
  return $ show (a * b * c * d * e)

type Grid = Vector (Vector Bool)

parseInput :: IO Grid
parseInput = do
  file <- readFile "Day3/input.txt"
  let v = fromList $ map (fromList . map (== '#')) (lines file)
  return v

type SlopeX = Int

type SlopeY = Int

linePositions :: Grid -> SlopeX -> SlopeY -> [(Int, Int)]
linePositions grid dx dy = descent (0, 0)
  where
    height = length grid
    width = length (V.head grid)

    descent (x, y)
      | y >= height = []
      | otherwise = (x, y) : descent ((x + dx) `mod` width, y + dy)

hitTrees :: Grid -> SlopeX -> SlopeY -> Int
hitTrees grid dx dy = treesHit
  where
    positions = linePositions grid dx dy
    treesHit = length $ filter id $ map (\(x, y) -> grid ! y ! x) positions
