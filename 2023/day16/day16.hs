import Data.List
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Set qualified as S
import System.Environment
import System.Posix (GroupEntry (groupID))

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 getInput1 filename
  p2Solution <- solve part2 getInput2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve solver parser = fmap solver . parser

-- Input Parsing

data Direction = L | R | U | D deriving (Show)

data Splits = Vert | Hor deriving (Show)

data Mirrors = LURD | LDRU deriving (Show)

data Tile = Empty | Splitter Splits | Mirror Mirrors deriving (Show)

data Beam = Beam [Position] Direction | Finished [Position] deriving (Show)

type Position = (Int, Int)

type Grid = M.Map Position Tile

getInput1 :: FilePath -> IO Grid
getInput1 = fmap (M.fromList . concat . zipWith (\y line -> parseLine y line) [1 ..] . lines) . readFile

parseLine :: Int -> String -> [(Position, Tile)]
parseLine y line = zipWith (\x char -> ((x, y), parseTile char)) [1 ..] line

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '-' = Splitter Hor
parseTile '|' = Splitter Vert
parseTile '/' = Mirror LURD
parseTile '\\' = Mirror LDRU

getInput2 = getInput1

-- Solution Logic

tupleAdd :: Num a => (a, a) -> (a, a) -> (a, a)
tupleAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

move :: Position -> Direction -> Position
move pos dir = pos `tupleAdd` vector dir

vector :: Direction -> (Int, Int)
vector U = (0, -1)
vector D = (0, 1)
vector R = (1, 0)
vector L = (-1, 0)

applyTile :: Position -> Direction -> Grid -> [Direction]
applyTile pos dir grid = case grid M.!? pos of
  Nothing -> []
  Just Empty -> [dir]
  Just (Mirror m) -> reflect dir m
  Just (Splitter s) -> split dir s

split :: Direction -> Splits -> [Direction]
split U Hor = [L, R]
split D Hor = split U Hor
split L Vert = [U, D]
split R Vert = split L Vert
split U _ = [U]
split D _ = [D]
split L _ = [L]
split R _ = [R]

reflect :: Direction -> Mirrors -> [Direction]
reflect L LDRU = [D]
reflect U LDRU = [R]
reflect R LDRU = [U]
reflect D LDRU = [L]
reflect L LURD = [U]
reflect U LURD = [L]
reflect R LURD = [D]
reflect D LURD = [R]

advanceBeam :: Beam -> Grid -> [Beam]
advanceBeam (Beam (pos : prev) dir) grid
  | M.notMember nextPos grid = [Finished (pos : prev)]
  | otherwise = map (Beam (nextPos : pos : prev)) nextDirs
  where
    nextPos = move pos dir
    nextDirs = applyTile nextPos dir grid
advanceBeam (Finished pos) grid = [Finished pos]

isFinished :: Beam -> Bool
isFinished (Finished _) = True
isFinished _ = False

getPositions :: Beam -> [Position]
getPositions (Beam pos _) = pos
getPositions (Finished pos) = pos

advanceBeams :: [Beam] -> Grid -> [Beam]
advanceBeams beams grid
  | all isFinished beams = beams
  | otherwise = advanceBeams (concatMap (`advanceBeam` grid) beams) grid

illuminatedPositions :: [Beam] -> S.Set Position
illuminatedPositions = S.fromList . concatMap getPositions

part1 = length . illuminatedPositions . advanceBeams [Beam [(1, 1)] R]

part2 = part1

printMap :: Grid -> IO ()
printMap grid = putStrLn $ unlines (transpose $ chunksOf 10 $ map tileToChar $ M.elems grid)

tileToChar :: Tile -> Char
tileToChar Empty = '.'
tileToChar (Splitter Hor) = '-'
tileToChar (Splitter Vert) = '|'
tileToChar (Mirror LURD) = '/'
tileToChar (Mirror LDRU) = '\\'