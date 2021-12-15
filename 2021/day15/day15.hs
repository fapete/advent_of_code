import System.Environment
import Data.List (minimumBy)
import Data.Bifunctor (bimap)
import qualified Data.Set as Set
import qualified Data.Map as Map

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

getInput = fmap (map parseline . lines) . readFile

parseline :: String -> [Int]
parseline = map (read . (:[]))

extendGrid = extendGridVert . extendGridHor

extendGridHor :: [[Int]] -> [[Int]]
extendGridHor = map extendRow

extendGridVert grid = foldl (\acc by -> acc ++ [map (stupidMod by) row | row <- grid]) grid [1,2,3,4]

extendRow row = foldl (\acc by -> acc ++ map (`stupidMod` by) row) row [1,2,3,4]

-- Works only because we're never adding more than 4, so always (x+y) < 14
stupidMod x y = if x+y > 9 then x + y - 9 else x + y

-- Solution Logic

type Grid = [[Int]]
type Point = (Int, Int)

getWeight grid x y = grid !! y !! x

getAdjacent grid x y = filter (uncurry (isIn grid)) $ zip [x+1, x-1, x, x] [y, y, y+1, y-1]

dim grid = (length $ head grid, length grid)

isIn grid x y = x >= 0 && x < fst (dim grid) && y >= 0 && y < snd (dim grid)

dijkstra :: Grid -> Map.Map Point (Int, Point) -> Map.Map Point (Int, Point) -> Map.Map Point (Int, Point)
dijkstra grid done todo = if null todo then done else dijkstra grid done' todo''
    where
        (nextPoint@(x,y), (weight, prev)) = minimumBy (\x y -> compare (fst $ snd x) (fst $ snd y)) $ Map.assocs todo
        todo' = Map.delete nextPoint todo
        done' = Map.insert nextPoint (weight, prev) done
        adjacentPoints = filter (`Map.notMember` done') $ getAdjacent grid x y
        todo'' = foldl (Map.unionWith minWeight) todo' $ map (\(x',y') -> Map.singleton (x',y') (weight + getWeight grid x' y', nextPoint)) adjacentPoints

minWeight x@(w1, _) y@(w2, _)
    | w1 <= w2 = x
    | otherwise = y

part1 grid = Map.lookup (bimap (+ (-1)) (+ (-1)) $ dim grid) $ dijkstra grid Map.empty (Map.singleton (0,0) (0, (0,0)))

part2 = part1 . extendGrid

getPath p ssp = case Map.lookup p ssp of
    Just (_, prev) -> p : getPath prev ssp
    Nothing -> error "wat"