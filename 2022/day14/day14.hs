import Data.List
import System.Environment
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)

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
data Tile = Rock | Air | Sand deriving (Show, Eq)
type Grid = M.Map Pos Tile

getInput = fmap ((`insertAllLinesToGrid` M.empty) . map parseLineDescription . lines) . readFile

parseLineDescription :: String -> [Pos]
parseLineDescription = map (bimap read (read . tail) . break (== ',')) . splitOn " -> "

insertLineToGrid from to grid = M.unions $ map (\pos -> M.insert pos Rock grid) $ linePositions from to

linePositions (x,y) (x',y')
  | x == x' && y < y' = [(x, y'') | y'' <- [y..y']]
  | x == x' && y >= y' = [(x, y'') | y'' <- [y,y-1..y']]
  | x < x' && y == y' = [(x'', y) | x'' <- [x..x']]
  | x >= x' && y == y' = [(x'', y) | x'' <- [x,x-1..x']]
  | otherwise = error "Invalid line coordinates"

insertLinesToGrid :: [Pos] -> Grid -> Grid
insertLinesToGrid ls grid = M.unions $ zipWith (\from to -> insertLineToGrid from to grid) (init ls) (tail ls)

insertAllLinesToGrid :: [[Pos]] -> Grid -> Grid
insertAllLinesToGrid ls grid = M.unions $ map (`insertLinesToGrid` grid) ls

-- Solution Logic

sandStart = (500,0)

below :: Pos -> Grid -> Tile
below (x,y) grid = fromMaybe Air $ M.lookup (x, y+1) grid
belowLeft :: Pos -> Grid -> Tile
belowLeft (x,y) grid = fromMaybe Air $ M.lookup (x-1, y+1) grid
belowRight :: Pos -> Grid -> Tile
belowRight (x,y) grid = fromMaybe Air $ M.lookup (x+1, y+1) grid

getLowestYPos = maximum . map snd . M.keys

moveSandToSettle pos grid = moveSandToSettle' pos grid (getLowestYPos grid) (below pos grid) (belowLeft pos grid) (belowRight pos grid)

moveSandToSettle' pos@(x,y) grid lowestRockYPos belowTile belowLeftTile belowRightTile
  | Air `notElem` [belowTile, belowLeftTile, belowRightTile] = Just pos
  | y > lowestRockYPos = Nothing -- Falling out of the grid
  | belowTile == Air = moveSandToSettle' (x,y+1) grid lowestRockYPos (below (x,y+1) grid) (belowLeft (x,y+1) grid) (belowRight (x,y+1) grid)
  | belowLeftTile == Air = moveSandToSettle' (x-1,y+1) grid lowestRockYPos (below (x-1,y+1) grid) (belowLeft (x-1,y+1) grid) (belowRight (x-1,y+1) grid)
  | belowRightTile == Air = moveSandToSettle' (x+1,y+1) grid lowestRockYPos (below (x+1,y+1) grid) (belowLeft (x+1,y+1) grid) (belowRight (x+1,y+1) grid)
  | otherwise = error "Should be an impossible state"

settleSand (Just sandPos) grid = Just $ M.insert sandPos Sand grid
settleSand Nothing grid = Nothing

pourSand (Just grid) pouredUnits = pourSand (settleSand sandPos grid) (pouredUnits + 1)
  where
    sandPos = moveSandToSettle sandStart grid
pourSand Nothing pouredUnits = pouredUnits

part1 xs = pourSand (Just xs) (-1)

-- Part 2 

moveSandToSettle2 pos lowestRockYPos grid = moveSandToSettle2' pos grid lowestRockYPos (below pos grid) (belowLeft pos grid) (belowRight pos grid)

moveSandToSettle2' pos@(x,y) grid lowestRockYPos belowTile belowLeftTile belowRightTile
  | Air `notElem` [belowTile, belowLeftTile, belowRightTile] = pos
  | y == lowestRockYPos+1 = pos -- Falling on floor
  | belowTile == Air = moveSandToSettle2' (x,y+1) grid lowestRockYPos (below (x,y+1) grid) (belowLeft (x,y+1) grid) (belowRight (x,y+1) grid)
  | belowLeftTile == Air = moveSandToSettle2' (x-1,y+1) grid lowestRockYPos (below (x-1,y+1) grid) (belowLeft (x-1,y+1) grid) (belowRight (x-1,y+1) grid)
  | belowRightTile == Air = moveSandToSettle2' (x+1,y+1) grid lowestRockYPos (below (x+1,y+1) grid) (belowLeft (x+1,y+1) grid) (belowRight (x+1,y+1) grid)
  | otherwise = error "Should be an impossible state"

settleSand2 sandPos grid 
  | sandPos == sandStart = Nothing
  | otherwise = Just $ M.insert sandPos Sand grid

pourSand2 (Just grid) lowestRockYPos pouredUnits = pourSand2 (settleSand2 sandPos grid) lowestRockYPos (pouredUnits + 1)
  where
    sandPos = moveSandToSettle2 sandStart lowestRockYPos grid
pourSand2 Nothing lowestRockYPos pouredUnits = pouredUnits

part2 xs = pourSand2 (Just xs) (getLowestYPos xs) 0