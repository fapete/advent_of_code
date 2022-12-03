import Data.Char (isUpper, ord)
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

getInput = fmap (map (map charValue) . lines) . readFile

compartments xs = splitAt half xs
  where
    half = length xs `div` 2

charValue c
  | isUpper c = ord c + 26 - 64
  | otherwise = ord c - 96

-- Solution Logic

findSame (ls, rs) = [x | x <- ls, x `elem` rs]

groupsOfThree xs = case drop 3 xs of
  [] -> [(xs !! 0, xs !! 1, xs !! 2)]
  xs' -> (g !! 0, g !! 1, g !! 2) : groupsOfThree xs'
    where
      g = take 3 xs

findSame' (xs, ys, zs) = findSame (findSame (xs, ys), zs)

part1 = sum . map (head . findSame . compartments)

part2 = sum . map (head . findSame') . groupsOfThree