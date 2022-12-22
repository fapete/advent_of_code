import Data.List
import System.Environment
import qualified Data.Map as M
import Data.Char (isDigit)
import Data.Bifunctor (bimap)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  --p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  --putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

data Tile = Floor | Wall deriving (Show, Eq)
data Movement = F Int | CW | CCW deriving (Show, Eq)
data Facing = U | D | L | R deriving (Show, Eq)

type Pos = (Int, Int)
type Grid = M.Map Pos Tile

getInput :: FilePath -> IO (Grid, [Movement])
getInput = fmap (bimap (M.unions . zipWith parseGridLine [1..]) (parseInstructions . last) . break ("" ==) . lines) . readFile

parseGridLine row line = M.unions $ zipWith (\tile col -> M.singleton (row, col) (parseTile tile)) gridLine [(initialCol+1)..]
  where
    initialCol = length $ takeWhile (' ' ==) line
    gridLine = takeWhile (' ' /=) $ drop initialCol line

parseTile '.' = Floor
parseTile '#' = Wall
parseTile _ = error "not a tile"

parseInstructions [] = []
parseInstructions ('R':xs) = CW:parseInstructions xs
parseInstructions ('L':xs) = CCW:parseInstructions xs
parseInstructions xs = F (read $ takeWhile isDigit xs):parseInstructions (dropWhile isDigit xs)

-- Solution Logic

moveStep pos facing (F 0) grid = (pos, facing)
moveStep pos facing (F i) grid = case M.lookup nextPos grid of
  Just Floor -> moveStep nextPos facing (F $ i-1) grid
  Just Wall -> (pos, facing)
  Nothing -> case M.lookup nextPosWrap grid of
    Just Floor -> moveStep nextPosWrap facing (F $ i-1) grid
    Just Wall -> (pos, facing)
    Nothing -> error "Wrapped back into Nothing"
  where
    nextPos = posInDir facing pos
    nextPosWrap = getWrapPos facing pos grid

moveStep pos facing turnDir grid = (pos, turn turnDir facing)

posInDir U (r, c) = (r-1, c)
posInDir D (r, c) = (r+1, c)
posInDir R (r, c) = (r, c+1)
posInDir L (r, c) = (r, c-1)

turn CW U = R
turn CW R = D
turn CW D = L
turn CW L = U
turn CCW f = turn CW $ turn CW $ turn CW f
turn (F _) f = error "Not a turn instruction"

getWrapPos U (r, c) = maximumBy (\x y -> compare (fst x) (fst y)) . filter (\(_, c') -> c == c') . M.keys
getWrapPos D (r, c) = minimumBy (\x y -> compare (fst x) (fst y)) . filter (\(_, c') -> c == c') . M.keys
getWrapPos L (r, c) = maximumBy (\x y -> compare (snd x) (snd y)) . filter (\(r', _) -> r == r') . M.keys
getWrapPos R (r, c) = minimumBy (\x y -> compare (snd x) (snd y)) . filter (\(r', _) -> r == r') . M.keys

scoreFacing R = 0
scoreFacing D = 1
scoreFacing L = 2
scoreFacing U = 3

getInitialPos = minimumBy (\x y -> compare (snd x) (snd y)) . filter (\(r, _) -> r == 1) . M.keys

score ((r, c), f) = 1000*r + 4*c + scoreFacing f

part1 (grid, instructions) = score $ foldl (\(pos, facing) instruction -> moveStep pos facing instruction grid) (getInitialPos grid, R) instructions