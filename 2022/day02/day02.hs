import System.Environment
import Data.List
import Data.List.Split (splitOn)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  --p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  --putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

getInput = fmap (lines) . readFile

makeIntegers :: [[String]] -> [[Integer]]
makeIntegers = map (map read)

-- Solution Logic

part1 xs = 4