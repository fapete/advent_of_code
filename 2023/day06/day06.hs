
import Data.List
import System.Environment
import Data.List.Split (splitOn)

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
getInput1 = fmap ((\x -> zip (head x) (last x)) . map parseLine . lines) . readFile

getInput2 = fmap ((\x -> [(head x, last x)]) . map parseLine2 . lines) . readFile

parseLine :: String -> [Double]
parseLine = map read . filter (/= "") . splitOn " " . drop 2 . dropWhile (/= ':') 

parseLine2 :: String -> Double
parseLine2 = read . filter (/= ' ') . drop 2 . dropWhile (/= ':') 

-- Solution Logic

buttonBounds :: (Floating a, RealFrac a, Integral b) => a -> a -> (b, b)
buttonBounds time distance = (lower, upper)
  where
    lower = ceiling $ (time - sqrt (time**2 - 4 * (distance + 1)))/2
    upper = floor $ (time + sqrt (time**2 - 4 * (distance + 1)))/2

possibleTimes (lower, upper) = upper - lower + 1

part1 = product . map (possibleTimes . uncurry buttonBounds)

part2 = part1