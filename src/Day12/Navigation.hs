module Day12.Navigation (part1, part2) where

import Prelude hiding (Left, Right, length)

-- Implement the actions as described and folding them onto the starting position + direction
-- Notice that a turn left is just a turn right by 360-angle degrees, so we can save ourselves one case
part1 :: IO String
part1 = do
  input <- parseInput
  let (x, y, _) = foldl doAction1 (0, 0, Right) input
  return (show (abs x + abs y))

-- Similar to part1 but now the direction is replaced by the waypoint parameters.
-- The same observation with left and right turns as in part1 hold,
-- additionally, notice that a right rotation by 90 degrees of (x,y) is just (y,-x) and all rotations are iterated applications of this
part2 :: IO String
part2 = do
  input <- parseInput
  let (x, y, _, _) = foldl doAction2 (0, 0, 10, 1) input
  return (show (abs x + abs y))

data Action = North Int | South Int | East Int | West Int | TurnLeft Int | TurnRight Int | Forward Int

data Direction = Up | Right | Down | Left
  deriving (Enum)

parseInput :: IO [Action]
parseInput = do
  file <- readFile "Day12/input.txt"
  return (map parse (lines file))
  where
    parse s = case s of
      'N' : i -> North (read i)
      'S' : i -> South (read i)
      'E' : i -> East (read i)
      'W' : i -> West (read i)
      'L' : i -> TurnLeft (read i)
      'R' : i -> TurnRight (read i)
      'F' : i -> Forward (read i)
      _ -> Forward 0

doAction1 :: (Int, Int, Direction) -> Action -> (Int, Int, Direction)
doAction1 (x, y, d) f = case f of
  North i -> (x, y + i, d)
  South i -> (x, y - i, d)
  East i -> (x + i, y, d)
  West i -> (x - i, y, d)
  TurnRight i -> (x, y, turn d i)
  TurnLeft i -> (x, y, turn d (360 - i))
  Forward i -> forward1 (x, y, d) i

turn :: Direction -> Int -> Direction
turn d angle = toEnum ((fromEnum d + steps) `mod` 4)
  where
    steps = angle `div` 90 `mod` 4

forward1 :: (Int, Int, Direction) -> Int -> (Int, Int, Direction)
forward1 (x, y, d) i = case d of
  Up -> (x, y + i, d)
  Down -> (x, y - i, d)
  Left -> (x - i, y, d)
  Right -> (x + i, y, d)

doAction2 :: (Int, Int, Int, Int) -> Action -> (Int, Int, Int, Int)
doAction2 (x, y, waypointX, waypointY) f = case f of
  North i -> (x, y, waypointX, waypointY + i)
  South i -> (x, y, waypointX, waypointY - i)
  East i -> (x, y, waypointX + i, waypointY)
  West i -> (x, y, waypointX - i, waypointY)
  TurnRight i -> let (waypointX', waypointY') = rotate (waypointX, waypointY) i in (x, y, waypointX', waypointY')
  TurnLeft i -> let (waypointX', waypointY') = rotate (waypointX, waypointY) (360 - i) in (x, y, waypointX', waypointY')
  Forward i -> (x + i * waypointX, y + i * waypointY, waypointX, waypointY)

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (x, y) angle = iterate rotate90 (x, y) !! steps
  where
    rotate90 (x', y') = (y', -x')
    steps = (angle `div` 90) `mod` 4