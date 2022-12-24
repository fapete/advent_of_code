import Data.Bool (bool)
import Data.Char (intToDigit)
import Data.List
import Data.Map qualified as M
import Data.Maybe (isNothing, mapMaybe)
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

type Pos = (Int, Int)

data Tile = Start | Target | Blizzard (Int, Int) deriving (Show, Eq, Ord)

type Grid = M.Map Pos [Tile]

getInput :: FilePath -> IO Grid
getInput = fmap (initGrid . M.unions . zipWith parseLine [1 ..] . lines) . readFile

parseLine rowIdx line = M.fromList $ zip [(rowIdx, colIdx) | colIdx <- [1 ..]] $ mapMaybe parseChar line

parseChar '^' = Just [Blizzard (-1, 0)]
parseChar '>' = Just [Blizzard (0, 1)]
parseChar '<' = Just [Blizzard (0, -1)]
parseChar 'v' = Just [Blizzard (1, 0)]
parseChar '.' = Just []
parseChar _ = Nothing

initGrid grid = M.delete (tr + 1, 1) $ M.union (M.fromList [((1, 1), [Start]), ((tr + 1, tc), [Target])]) grid
  where
    (_, (tr, tc)) = getGridDimensions grid

-- Solution Logic

addPos (x, y) (dx, dy) = (x + dx, y + dy)

wrapPos (r, c) dir (mr, mc) = case dir of
  (0, 1) -> (r, 1)
  (0, -1) -> (r, mc)
  (1, 0) -> (2, c)
  (-1, 0) -> (mr, c)
  other -> error "invalid direction"

moveBlizzards grid = M.unionWith (++) (M.unionsWith (++) $ map (`updateBlizzards` grid) blizzards) clearBlizzards
  where
    clearBlizzards = M.map (filter (not . isBlizzard)) grid
    blizzards = M.assocs $ M.filter containsBlizzard grid

updateBlizzards (pos, xs) grid = foldl (\agg (p, b) -> insertBlizzard p b agg) M.empty $ map (\b -> (pos, b) `updateBlizzard` grid) xs

insertBlizzard pos blizzard = M.insertWith (++) pos [blizzard]

updateBlizzard :: (Pos, Tile) -> Grid -> (Pos, Tile)
updateBlizzard (pos, Blizzard dir) grid =
  bool
    (wrapPos pos dir gridMax, Blizzard dir)
    (addPos pos dir, Blizzard dir)
    $ not
    $ isWall
    $ M.lookup (addPos pos dir) grid
  where
    (_, gridMax) = getGridDimensions grid
updateBlizzard wat _ = error $ "Wat: " ++ show wat

getGridDimensions grid = ((1, 1), (tr - 1, tc))
  where
    tr = fst $ maximumBy (\x y -> compare (fst x) (fst y)) $ M.keys grid
    tc = snd $ maximumBy (\x y -> compare (snd x) (snd y)) $ M.keys grid

containsBlizzard ((Blizzard _) : xs) = True
containsBlizzard _ = False

isBlizzard (Blizzard _) = True
isBlizzard _ = False

isWall Nothing = True
isWall (Just _) = False

isFree (Just []) = True
isFree (Just [Target]) = True
isFree (Just [Start]) = True
isFree _ = False

validMoves from grid = filter (isFree . flip M.lookup grid) $ map (addPos from) [(0, 0), (0, -1), (0, 1), (-1, 0), (1, 0)]

bfs :: Tile -> S.Set (Int, Pos, Grid) -> Int
bfs to todo
  | grid M.! elfPos == [to] = steps
  | otherwise = bfs to todo''
  where
    ((steps, elfPos, grid), todo') = S.deleteFindMin todo
    validNext = validMoves elfPos blizzardsMoved
    blizzardsMoved = moveBlizzards grid
    todo'' = S.union todo' $ S.fromList [(steps + 1, newPos, blizzardsMoved) | newPos <- validNext]

moveNSteps 0 grid = grid
moveNSteps n grid = moveNSteps (n - 1) $ moveBlizzards grid

targetPos = fst . head . M.assocs . M.filter (== [Target])

part1 grid = bfs Target $ S.singleton (0, (1, 1), grid)

part2 grid = there + back + there_again
  where
    there = part1 grid
    back = bfs Start $ S.singleton (0, targetPos grid, moveNSteps there grid)
    there_again = bfs Target $ S.singleton (0, (1, 1), moveNSteps (there + back) grid)