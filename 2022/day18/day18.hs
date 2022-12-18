import Data.List
import Data.List.Split (splitOn)
import Data.Set qualified as S
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

type Cube = (Int, Int, Int)

getInput = fmap (S.fromList . map parseCube . lines) . readFile

parseCube = (\xs -> (head xs, xs !! 1, last xs)) . map (\x -> read x :: Int) . splitOn ","

-- Solution Logic

neighbouring (x, y, z) = [(x + 1, y, z), (x - 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

freeSides cube cubes = length $ filter (not . flip S.member cubes) $ neighbouring cube

maxDim cubes = (maxX + 1, maxY + 1, maxZ + 1)
  where
    maxX = maximum $ S.map (\(x, _, _) -> x) cubes
    maxY = maximum $ S.map (\(_, x, _) -> x) cubes
    maxZ = maximum $ S.map (\(_, _, x) -> x) cubes

inBounds (x, y, z) (maxX, maxY, maxZ) = x >= (-1) && x <= maxX && y >= (-1) && y <= maxY && z >= (-1) && z <= maxZ

steamTraversal todo done sidesCount dimensions cubes
  | null todo = sidesCount
  | otherwise = steamTraversal todo'' done' sidesCount' dimensions cubes
  where
    (currentPoint, todo') = S.deleteFindMin todo
    done' = S.insert currentPoint done
    unexploredAir = filter (\point -> not (S.member point cubes) && not (S.member point done) && point `inBounds` dimensions) $ neighbouring currentPoint
    neighbouringLava = filter (`S.member` cubes) $ neighbouring currentPoint
    todo'' = foldr S.insert todo' unexploredAir
    sidesCount' = sidesCount + length neighbouringLava

part1 xs = sum $ map (`freeSides` xs) $ S.elems xs

part2 xs = steamTraversal (S.singleton (-1, -1, -1)) S.empty 0 (maxDim xs) xs