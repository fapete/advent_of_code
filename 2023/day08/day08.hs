import Data.List
import System.Environment
import qualified Data.Map as M

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

getInput1 = fmap (lines) . readFile

getInput2 = getInput1

-- Solution Logic

part1 xs = 4

part2 = part1