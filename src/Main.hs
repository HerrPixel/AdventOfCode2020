module Main where

import Day1.Expenses (part1, part2)
import Day2.Passwords (part1, part2)
import System.Directory (setCurrentDirectory)

solutions :: [(String, IO String, IO String)]
solutions =
  [ ("Day 1: Report Repair", Day1.Expenses.part1, Day1.Expenses.part2),
    ("Day 2: Password Philosophy", Day2.Passwords.part1, Day2.Passwords.part2)
  ]

displaySolution :: (String, IO String, IO String) -> IO ()
displaySolution (str, p1, p2) = do
  part1Result <- p1
  part2Result <- p2
  putStr (str ++ "\n    Part1: " ++ part1Result ++ "\n    Part2: " ++ part2Result ++ "\n\n")

main :: IO [()]
main = do
  setCurrentDirectory "src"
  mapM displaySolution solutions
