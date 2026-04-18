module Day11.Seats (part1, part2) where

import Data.Vector hiding (map)
import Data.Vector qualified as Vector
import Prelude hiding (length)

-- We simulate the grid as instructed. Due to Haskells immutable structs, this is very easy via maps.
-- At each step, every positition of the grid counts its neighbors and transforms
-- depending on the number of occupied neighbors and its own state.
part1 :: IO String
part1 = do
  input <- parseInput
  let seats = countSeats (stabilize input)
  return $ show seats

-- Same as part1 but we use a different neighbor function as part1 and adjust the bounds from 4 to 5.
part2 :: IO String
part2 = do
  input <- parseInput
  let seats = countSeats (stabilize2 input)
  return $ show seats

data Space = Empty | Occupied | Floor
  deriving (Eq)

type Grid = Vector (Vector Space)

parseInput :: IO Grid
parseInput = do
  file <- readFile "Day11/input.txt"
  let v = fromList $ map (fromList . map parseSpace) (lines file)
  return v
  where
    parseSpace c
      | c == 'L' = Empty
      | c == '#' = Occupied
      | otherwise = Floor

-- Simply counts the number of grid entries with state 'Occupied'
countSeats :: Grid -> Int
countSeats grid = Vector.sum $ Vector.map (Vector.sum . Vector.map (\value -> if value == Occupied then 1 else 0)) grid

-- apply 'simulateStep' until the output does not change
stabilize :: Grid -> Grid
stabilize = go
  where
    go !old
      | old == new = old
      | otherwise = go new
      where
        new = simulateStep old

-- Simulates one step by mapping every space to its new state
simulateStep :: Grid -> Grid
simulateStep grid = Vector.imap (\x row -> Vector.imap (\y value -> changeSeat x y value grid) row) grid

-- Maps a space to its new state by counting the number of occupied neighbors and considering its own state.
changeSeat :: Int -> Int -> Space -> Grid -> Space
changeSeat x y occupancy grid
  | occupancy == Floor = Floor
  | occupancy == Empty && nrOfOccupiedNeighbours == 0 = Occupied
  | occupancy == Occupied && nrOfOccupiedNeighbours >= 4 = Empty
  | otherwise = occupancy
  where
    nrOfOccupiedNeighbours :: Int = Prelude.sum [1 | (r, c) <- neighbors x y grid, grid ! r ! c == Occupied]

-- Get a list of (x,y) coordinates of the neighbors of a space
neighbors :: Int -> Int -> Grid -> [(Int, Int)]
neighbors x y grid =
  [ (x + dx, y + dy)
    | dx <- [-1 .. 1],
      x + dx >= 0,
      x + dx < Vector.length grid, -- number of rows
      dy <- [-1 .. 1],
      y + dy >= 0,
      y + dy < Vector.length (grid ! 0), -- number of columns
      not (dx == 0 && dy == 0) -- the position itself is not a neighbor
  ]

-- We are lazy here and just define functions with suffix '2' denoting part2

-- Same as part1 just with a different simulateStep function
stabilize2 :: Grid -> Grid
stabilize2 = go
  where
    go !old
      | old == new = old
      | otherwise = go new
      where
        new = simulateStep2 old

-- Same as part1 just with a different changeSeat function
simulateStep2 :: Grid -> Grid
simulateStep2 grid = Vector.imap (\x row -> Vector.imap (\y value -> changeSeat2 x y value grid) row) grid

-- Same as part1 but the bound on emptying was changed from 4 to 5 and we use the new neighbor function
changeSeat2 :: Int -> Int -> Space -> Grid -> Space
changeSeat2 x y occupancy grid
  | occupancy == Floor = Floor
  | occupancy == Empty && nrOfOccupiedNeighbours == 0 = Occupied
  | occupancy == Occupied && nrOfOccupiedNeighbours >= 5 = Empty
  | otherwise = occupancy
  where
    nrOfOccupiedNeighbours :: Int = Prelude.sum [1 | (r, c) <- neighbors2 x y grid, grid ! r ! c == Occupied]

-- Instead of the 8 neighbors, we instead get the first seats in each line of sights.
-- We do this by repeatedly addding the offset of the line until we hit a non-floor space.
neighbors2 :: Int -> Int -> Grid -> [(Int, Int)]
neighbors2 x y grid =
  [ (row, col)
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      not (dx == 0 && dy == 0),
      Just (row, col) <- [firstSeatInDirection x y dx dy]
  ]
  where
    width = Vector.length grid
    height = Vector.length (grid ! 0)

    firstSeatInDirection curX curY dx dy =
      go (curX + dx) (curY + dy)
      where
        go nx ny
          | nx < 0 || ny < 0 || nx >= width || ny >= height = Nothing
          | grid ! nx ! ny /= Floor = Just (nx, ny)
          | otherwise = go (nx + dx) (ny + dy)