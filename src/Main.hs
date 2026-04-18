module Main where

import Day1.Expenses (part1, part2)
-- import Day7.Luggage (part1, part2)

import Day10.Adapters (part1, part2)
import Day11.Seats (part1, part2)
import Day2.Passwords (part1, part2)
import Day3.Airplane (part1, part2)
import Day4.Passports (part1, part2)
import Day5.Boarding (part1, part2)
import Day6.Questionaire (part1, part2)
import Day8.Console (part1, part2)
import Day9.Encryption (part1, part2)
import System.Directory (setCurrentDirectory)

solutions :: [(String, IO String, IO String)]
solutions =
  [ ("Day 1: Report Repair", Day1.Expenses.part1, Day1.Expenses.part2),
    ("Day 2: Password Philosophy", Day2.Passwords.part1, Day2.Passwords.part2),
    ("Day 3: Toboggan Trajectory", Day3.Airplane.part1, Day3.Airplane.part2),
    ("Day 4: Passport Processing", Day4.Passports.part1, Day4.Passports.part2),
    ("Day 5: Binary Boarding", Day5.Boarding.part1, Day5.Boarding.part2),
    ("Day 6: Custom Customs", Day6.Questionaire.part1, Day6.Questionaire.part2),
    -- ("Day 7: Handy Haversacks", Day7.Luggage.part1, Day7.Luggage.part2)
    ("Day 8: Handheld Halting", Day8.Console.part1, Day8.Console.part2),
    ("Day 9: Encoding Error", Day9.Encryption.part1, Day9.Encryption.part2),
    ("Day 10: Adapter Array", Day10.Adapters.part1, Day10.Adapters.part2),
    ("Day 11: Seating System", Day11.Seats.part1, Day11.Seats.part2)
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
