module Day8.Console (part1, part2) where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Sequence as Seq (Seq, adjust, fromList, lookup)
import Data.Set as Set (empty, insert, member)

data State = State {pointer :: Int, accumulator :: Int}
  deriving (Eq, Ord)

data Instructions = NOP Int | ACC Int | JMP Int

part1 :: IO String
part1 = do
  input <- parseInput
  let firstRepeat = firstRepeated input
  return (show (accumulator firstRepeat))

part2 :: IO String
part2 = do
  input <- parseInput
  let corruptedIndex = fromJust (find (isTerminatingAfterDecorruption input) [0 .. (length input)])
  let finalUncorruptedState = firstRepeated (decorrupt input corruptedIndex)
  return (show (accumulator finalUncorruptedState))

parseInput :: IO (Seq Instructions)
parseInput = do
  file <- readFile "Day8/input.txt"
  return (fromList (map parse (lines file)))
  where
    parse s = case s of
      'n' : 'o' : 'p' : ' ' : i -> NOP (readInt i)
      'a' : 'c' : 'c' : ' ' : i -> ACC (readInt i)
      'j' : 'm' : 'p' : ' ' : i -> JMP (readInt i)
      _ -> NOP 0
      where
        readInt i = case i of
          ('+' : rest) -> read rest
          _ -> read i

run :: State -> Seq Instructions -> Maybe State
run state instructions = do
  inst <- Seq.lookup (pointer state) instructions
  return
    ( case inst of
        NOP _ -> state {pointer = pointer state + 1}
        ACC i -> state {accumulator = accumulator state + i, pointer = pointer state + 1}
        JMP i -> state {pointer = pointer state + i}
    )

firstRepeated :: Seq Instructions -> State
firstRepeated instructions = go Set.empty (State 0 0)
  where
    go seenStates state =
      maybe state step (run state instructions)
      where
        step newState
          | pointer newState `Set.member` seenStates = state
          | otherwise = go (pointer newState `Set.insert` seenStates) newState

isTerminatingAfterDecorruption :: Seq Instructions -> Int -> Bool
isTerminatingAfterDecorruption instructions indexToChange = pointer (firstRepeated (decorrupt instructions indexToChange)) >= length instructions

decorrupt :: Seq Instructions -> Int -> Seq Instructions
decorrupt instructions index = adjust flipInstruction index instructions
  where
    flipInstruction instruction = case instruction of
      ACC i -> ACC i
      NOP i -> JMP i
      JMP i -> NOP i
