import Data.List
import System.Environment
import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.Maybe (isJust, catMaybes)

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

type Pos = (Int, Int)

type Grid = M.Map Pos Int

getInput = fmap (parseGrid . lines) . readFile

parseGrid :: [String] -> Grid
parseGrid rows = M.unions $ zipWith parseGridRow rows [0..]

parseGridRow :: String -> Int -> Grid
parseGridRow row rowIdx = foldr (\(c, colIdx) agg -> M.insert (rowIdx, colIdx) (digitToInt c) agg) M.empty $ zip row [0..]

-- Solution Logic

treesInDir pos dirVector grid = catMaybes $ takeWhile isJust [M.lookup x grid | x <- tail $ scanl (\agg _ -> addPos agg dirVector) pos [0..]]

addPos (x,y) (x', y') = (x + x', y + y')

isTreeVisibleFromDir pos dirVector grid = grid M.! pos > safeMaximum (treesInDir pos dirVector grid)
  where
    safeMaximum [] = -1 -- tree on border always visible
    safeMaximum xs = maximum xs


directions = [(0,1), (1,0), (0,-1), (-1,0)]

treeVisible pos grid = any (\direction -> isTreeVisibleFromDir pos direction grid) directions

treesVisibleInDir pos dirVector grid = largeTreeAdjust + length (takeWhile (\x -> grid M.! pos > x) trees)
  where
    trees = treesInDir pos dirVector grid
    largeTreeAdjust = if null $ dropWhile (\x -> grid M.! pos > x) trees then 0 else 1

scenicScore pos grid = product $ map (\direction -> treesVisibleInDir pos direction grid) directions

part1 grid = length $ filter (`treeVisible` grid) $ M.keys grid

part2 grid = maximum $ map (`scenicScore` grid) $ M.keys grid