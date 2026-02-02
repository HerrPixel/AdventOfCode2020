module Day8.Console (part1, part2) where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Sequence as Seq (Seq, adjust, fromList, lookup)
import Data.Set as Set (empty, insert, member)

data State = State {pointer :: Int, accumulator :: Int}
  deriving (Eq, Ord)

data Instructions = NOP Int | ACC Int | JMP Int

-- check previously encountered states by caching them, the moment we hit a cache entry, return the last state
part1 :: IO String
part1 = do
  input <- parseInput
  let firstRepeat = firstRepeated input
  return (show (accumulator firstRepeat))

-- test each instruction inverted if the programm exits and return the last state of the first inversion that exits
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

-- runs one line of instructions given by the pointer value of state
-- returns Nothing if the pointer is outside of the instruction bounds
run :: State -> Seq Instructions -> Maybe State
run state instructions = do
  inst <- Seq.lookup (pointer state) instructions
  return
    ( case inst of
        NOP _ -> state {pointer = pointer state + 1}
        ACC i -> state {accumulator = accumulator state + i, pointer = pointer state + 1}
        JMP i -> state {pointer = pointer state + i}
    )

-- Returns the last state before a repeated instruction or the last state before the programm exists
firstRepeated :: Seq Instructions -> State
firstRepeated instructions = go Set.empty (State 0 0)
  where
    go seenStates state =
      maybe state step (run state instructions) -- Return last state if we ran out of instructions
      where
        step newState -- otherwise, check cache for already encountered states or continue running otherwise
          | pointer newState `Set.member` seenStates = state
          | otherwise = go (pointer newState `Set.insert` seenStates) newState

-- Flips the instruction at the specified index position and tests if the programm exits this way
isTerminatingAfterDecorruption :: Seq Instructions -> Int -> Bool
isTerminatingAfterDecorruption instructions indexToChange = pointer (firstRepeated (decorrupt instructions indexToChange)) >= length instructions

-- Flip the instruction at the specified position
decorrupt :: Seq Instructions -> Int -> Seq Instructions
decorrupt instructions index = adjust flipInstruction index instructions
  where
    flipInstruction instruction = case instruction of
      ACC i -> ACC i
      NOP i -> JMP i
      JMP i -> NOP i
