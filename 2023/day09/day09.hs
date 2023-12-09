import Data.List
import Data.List.Split (splitOn)
import System.Environment

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 getInput1 filename
  p2Solution <- solve part2 getInput2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve solver parser = fmap solver . parser

-- Input Parsing

getInput1 = fmap (map (reverse . parseLine) . lines) . readFile

parseLine :: String -> [Integer]
parseLine = map read . splitOn " "

getInput2 = getInput1

-- Solution Logic

differences :: [Integer] -> [Integer]
differences is = zipWith (-) (init is) (tail is)

computeAllDifferences :: [Integer] -> [[Integer]]
computeAllDifferences = takeWhile (any (/= 0)) . iterate differences

predictNextNumber :: [[Integer]] -> Integer
predictNextNumber = sum . map head

part1 = sum . map (predictNextNumber . computeAllDifferences)

part2 = part1