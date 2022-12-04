import Data.List
import System.Environment

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

data Interval = Interval Integer Integer deriving (Show, Eq)

getInput = fmap (map (parseIntervals . myBreak (== ',')) . lines) . readFile

myBreak with xs = (fst b, tail (snd b))
  where
    b = break with xs

parseIntervals (i1, i2) = (parseInterval i1, parseInterval i2)

parseInterval s = (\(from, to) -> Interval (read from) (read to)) $ myBreak (== '-') s

-- Solution Logic

fullyContains (Interval from to) (Interval from' to') = from <= from' && to >= to'

overlaps (Interval from to) (Interval from' to') = from <= from' && to >= from' || to >= to' && from <= to'

part1 = length . filter (\(i1, i2) -> fullyContains i1 i2 || fullyContains i2 i1)

part2 = length . filter (\(i1, i2) -> overlaps i1 i2 || overlaps i2 i1)