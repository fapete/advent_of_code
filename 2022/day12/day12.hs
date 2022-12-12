{-# LANGUAGE TupleSections #-}
import Data.List
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (ord, chr)

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

type Grid = M.Map Pos Char

getInput = fmap (flip parseGrid M.empty . lines) . readFile

parseGridRow row rIdx grid = foldl (\agg (c, elevation) -> M.insert (rIdx,c) elevation agg) grid $ zip [0..] row

parseGrid rows grid = foldl (\agg (row, rIdx) -> parseGridRow row rIdx agg) grid $ zip rows [0..]

-- Solution Logic

getStartPos = head . M.keys . M.filter (== 'S')

getNeighbours (x,y) = [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]

getReachableNeighbours pos grid = map fst $ filter (\(_, v) -> fromMaybe '|' v `elem` allowedNeighbours (grid M.! pos)) neighbours
  where
    neighbourPositions = getNeighbours pos
    neighbourValues = map (`M.lookup` grid) neighbourPositions
    neighbours = zip neighbourPositions neighbourValues

allowedNeighbours 'S' = ['a', 'b', 'E', 'S']
allowedNeighbours 'y' = map chr [(ord 'a') .. (ord 'z')] ++ ['E', 'S']
allowedNeighbours 'z' = map chr [(ord 'a') .. (ord 'z')] ++ ['E', 'S']
allowedNeighbours c = map chr [(ord 'a') .. (ord c)] ++ [chr $ ord c + 1, 'S']

bfs :: S.Set (Int, Pos) -> S.Set Pos -> Grid -> Int
bfs positionsToVisit _ _
  | S.null positionsToVisit = 0
bfs positionsToVisit visited grid
  | grid M.! toVisit == 'E' = steps
  | otherwise = bfs newPositionsToVisit (S.insert toVisit visited) grid
    where
      (steps, toVisit) = S.elemAt 0 positionsToVisit
      newPositionsToVisit = foldl (flip S.insert) (S.deleteAt 0 positionsToVisit) $ map (steps + 1,) (filter (not . flip S.member visited) $ getReachableNeighbours toVisit grid)
      
findStartingPositions = M.keys . M.filter (== 'a')

part1 grid = bfs (S.singleton (0, getStartPos grid)) S.empty grid

part2 grid = minimum $ filter (>0) $ [bfs (S.singleton (0, x)) S.empty grid | x <- findStartingPositions grid]