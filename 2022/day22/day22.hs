import Data.List
import System.Environment
import qualified Data.Map as M
import Data.Char (isDigit)
import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)

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

moveStep pos facing (F 0) wrapping grid = (pos, facing)
moveStep pos facing (F i) wrapping grid = case M.lookup nextPos grid of
  Just Floor -> moveStep nextPos facing (F $ i-1) wrapping grid
  Just Wall -> (pos, facing)
  Nothing -> case M.lookup nextPosWrap grid of
    Just Floor -> moveStep nextPosWrap newFacing (F $ i-1) wrapping grid
    Just Wall -> (pos, facing)
    Nothing -> error "Wrapped back into Nothing"
  where
    nextPos = posInDir facing pos
    (nextPosWrap, newFacing) = wrapping facing pos grid

moveStep pos facing turnDir wrapping grid = (pos, turn turnDir facing)

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

getWrapPos1 U (r, c) grid = (maximumBy (\x y -> compare (fst x) (fst y)) $ filter (\(_, c') -> c == c') $ M.keys grid, U)
getWrapPos1 D (r, c) grid = (minimumBy (\x y -> compare (fst x) (fst y)) $ filter (\(_, c') -> c == c') $ M.keys grid, D)
getWrapPos1 L (r, c) grid = (maximumBy (\x y -> compare (snd x) (snd y)) $ filter (\(r', _) -> r == r') $ M.keys grid, L)
getWrapPos1 R (r, c) grid = (minimumBy (\x y -> compare (snd x) (snd y)) $ filter (\(r', _) -> r == r') $ M.keys grid, R)

scoreFacing R = 0
scoreFacing D = 1
scoreFacing L = 2
scoreFacing U = 3

getInitialPos = minimumBy (\x y -> compare (snd x) (snd y)) . filter (\(r, _) -> r == 1) . M.keys

score ((r, c), f) = 1000*r + 4*c + scoreFacing f

-- Face: (posFrom, posTo, upWrapTo, downWrapTo, rightWrapTo, leftWrapTo)
-- wrapTo: (faceIdx, fromDirection)
testFaces = [
  ((1,9), (4,12), (1, U), (3, U), (5, L), (2, U)),
  ((5,1), (8,4), (0, U), (4, D), (2, L), (5, D)),
  ((5,5), (8,8), (0, L), (4, L), (3, L), (1, R)),
  ((5,9), (8,12), (0, D), (4,U), (5,U), (2, R)),
  ((9,9), (12,12), (3, D), (1, D), (5, L), (2, D)),
  ((9,13), (12,16), (3, R), (1, L), (0, R), (4, R))
  ]
testDim = 3

inputFaces = [
  ((1,51), (50,100), (5, L), (2, U), (1, L), (3, L)),
  ((1, 101), (50, 150), (5, D), (2, R), (4, R), (0, R)),
  ((51, 51), (100,100), (0, D), (4,U), (1, D), (3, U)),
  ((101, 1), (150, 50), (2, L), (5,U), (4, L), (0, L)),
  ((101, 51), (150, 100), (2, D), (5, R), (1, R), (3, R)),
  ((151, 1), (200,50), (3, D), (1, U), (4, D), (0, U))
  ]
inputDim = 49

faces = inputFaces
maxDim = inputDim

getWrapPos2 :: Facing -> Pos -> Grid -> (Pos, Facing)
getWrapPos2 f pos _ = (fromFaceCoord (rotate f fRot $ toFaceCoord pos from) from', opposite fRot)
  where
    (from, to, (u, fu), (d, fd), (r, fr), (l, fl)) = getFace pos faces
    (from', to', _, _, _, _) = faces !! case f of
      U -> u
      D -> d
      R -> r
      L -> l
    fRot = case f of
      U -> fu
      D -> fd
      R -> fr
      L -> fl

toFaceCoord (r,c) (fr, fc) = (abs (fr-r), abs (fc-c))

fromFaceCoord (r,c) (fr, fc) = (fr+r, fc+c)

getFace (r,c) = fromJust . find (\((fr, fc), (tr, tc), _, _, _, _) -> r >= fr && r <= tr && c >= fc && c <= tc)

rotate :: Facing -> Facing -> (Int, Int) -> (Int, Int)
rotate U U (r,c) = (r, maxDim - c)
rotate U D (r,c) = (maxDim, c)
rotate U R (r,c) = (maxDim - c, 0)
rotate U L (r,c) = (c, 0)
rotate D U (r,c) = (0, c)
rotate D D (r,c) = (r, maxDim - c)
rotate D R (r,c) = (c, r)
rotate D L (r,c) = (c, maxDim)
rotate R U (r,c) = (0, maxDim - r)
rotate R D (r,c) = (c, r)
rotate R R (r,c) = (maxDim - r, c)
rotate R L (r,c) = (r, 0)
rotate L U (r,c) = (0, r)
rotate L D (r,c) = (maxDim, maxDim - r)
rotate L R (r,c) = (r, maxDim)
rotate L L (r,c) = (maxDim - r, 0)

opposite U = D
opposite D = U
opposite R = L
opposite L = R

part1 (grid, instructions) = score $ foldl (\(pos, facing) instruction -> moveStep pos facing instruction getWrapPos1 grid) (getInitialPos grid, R) instructions

part2 (grid, instructions) = score $ foldl (\(pos, facing) instruction -> moveStep pos facing instruction getWrapPos2 grid) (getInitialPos grid, R) instructions