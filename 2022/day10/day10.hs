import Data.Bifunctor (Bifunctor)
import Data.Bool (bool)
import Data.List
import Data.List.Split (chunksOf)
import Data.Tuple.All (sel2)
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

getInput = fmap (map (read . parseInstruction) . lines) . readFile

parseInstruction ('a' : 'd' : 'd' : 'x' : xs) = "Add " ++ xs
parseInstruction ('n' : xs) = "Noop"
parseInstruction _ = error "Invalid instruction"

-- Solution Logic

execute (_, cycle, register) (Add x) = [(register, cycle + 1, register), (register, cycle + 2, register + x)]
execute (_, cycle, register) Noop = [(register, cycle + 1, register)]

relevantCycles = [20, 60 .. 240]

executeAll = tail . concat . scanl (execute . last) [(1, 0, 1)]

signalStrengths = map (\(value, cycle, _) -> value * cycle)

isPixel (value, cycle, _) = ((cycle - 1) `mod` 40) `elem` [value - 1, value, value + 1]

drawScreen = concatMap (bool "  " "##" . isPixel)

part1 = sum . signalStrengths . filter (flip elem relevantCycles . sel2) . executeAll

part2 = unlines . chunksOf 80 . drawScreen . executeAll