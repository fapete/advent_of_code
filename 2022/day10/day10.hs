import Data.List
import System.Environment

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn "Part 2: "
  putStrLn p2Solution

solve fn = fmap fn . getInput

-- Input Parsing

data Instruction = Add Int | Noop deriving (Show, Read)

getInput :: FilePath -> IO [Instruction]
getInput = fmap (map (read . parseInstruction) . lines) . readFile

parseInstruction ('a' : 'd' : 'd' : 'x' : xs) = "Add " ++ xs
parseInstruction ('n' : xs) = "Noop"
parseInstruction _ = error "Invalid instruction"

-- Solution Logic

execute (Add x) register cycle = [(register, cycle + 1, register), (register, cycle + 2, register + x)]
execute Noop register cycle = [(register, cycle + 1, register)]

relevantCycles = [20, 60 ..]

getCycle (_, cycle, _) = cycle

getRegisterAfter (_, _, value) = value

executeAll = concat . scanl (\result instruction -> execute instruction (getRegisterAfter $ last result) (getCycle $ last result)) [(1, 0, 1)]

signalStrengths [] _ = []
signalStrengths (c : cycles) executionResults = (c * valueDuringCycle) : signalStrengths cycles remainingResults
  where
    remainingResults = dropWhile (\(_, cycleAfter, _) -> cycleAfter < c) executionResults
    (valueDuringCycle, _, _) = head remainingResults

getPixel (value, cycle, _)
  | ((cycle - 1) `mod` 40) `elem` spritePositions = '#'
  | otherwise = ' '
  where
    spritePositions = [value - 1, value, value + 1]

drawScreen :: [(Int, Int, Int)] -> [Char]
drawScreen = foldl (\agg executionResult -> agg ++ [getPixel executionResult]) ""

splitIntoLines [] = []
splitIntoLines line = take 40 line ++ "\n" ++ splitIntoLines (drop 40 line)

part1 = sum . signalStrengths (take 6 relevantCycles) . executeAll

part2 = splitIntoLines . drawScreen . tail . executeAll