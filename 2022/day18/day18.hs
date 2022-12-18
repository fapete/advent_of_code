import Data.List
import Data.List.Split (splitOn)
import Data.Set qualified as S
import System.Environment

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  -- p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)

-- putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

type Cube = (Int, Int, Int)

getInput = fmap (S.fromList . map parseCube . lines) . readFile

parseCube = (\xs -> (head xs, xs !! 1, last xs)) . map (\x -> read x :: Int) . splitOn ","

-- Solution Logic

neighbouring (x, y, z) = [(x + 1, y, z), (x - 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

freeSides cube cubes = length $ filter (not . flip S.member cubes) $ neighbouring cube

part1 xs = sum $ map (`freeSides` xs) $ S.elems xs