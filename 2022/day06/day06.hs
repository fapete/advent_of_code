import Data.List
import System.Environment
import Data.Set as S hiding (drop, take)

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

getInput = fmap init . readFile

-- Solution Logic

firstFourDifferent (a:b:c:d:xs)
  | a /= b && a /= c && a /= d && b /= c && b /= d && c /= d = True
  | otherwise = False
firstFourDifferent _ = False

firstFourteenDifferent xs
  | length (S.fromList (take 14 xs)) == 14 = True
  | otherwise = False

dropWhile' fn l@(x:xs)
  | fn l = l
  | otherwise = dropWhile' fn xs
dropWhile' fn [] = []

part1 xs = length xs - length (drop 4 $ dropWhile' firstFourDifferent xs)
part2 xs = length xs - length (drop 14 $ dropWhile' firstFourteenDifferent xs)