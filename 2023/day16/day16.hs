import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment

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

data Direction = L | R | U | D deriving (Show, Eq, Ord)

data Splits = Vert | Hor deriving (Show)

data Mirrors = LURD | LDRU deriving (Show)

data Tile = Empty | Splitter Splits | Mirror Mirrors deriving (Show)

data Beam = Beam Position Direction deriving (Show, Eq, Ord)

type Position = (Int, Int)

type Grid = M.Map Position Tile

getInput1 :: FilePath -> IO Grid
getInput1 = fmap (M.fromList . concat . zipWith parseLine [1 ..] . lines) . readFile

parseLine :: Int -> String -> [(Position, Tile)]
parseLine y = zipWith (\x char -> ((x, y), parseTile char)) [1 ..]

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '-' = Splitter Hor
parseTile '|' = Splitter Vert
parseTile '/' = Mirror LURD
parseTile '\\' = Mirror LDRU
parseTile _ = error "Not a tile"

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
reflect L LDRU = [U]
reflect U LDRU = [L]
reflect R LDRU = [D]
reflect D LDRU = [R]
reflect L LURD = [D]
reflect U LURD = [R]
reflect R LURD = [U]
reflect D LURD = [L]

advanceBeam :: Beam -> Grid -> [Beam]
advanceBeam (Beam pos dir) grid
  | M.notMember pos grid = []
  | otherwise = zipWith Beam nextPos nextDirs
  where
    nextDirs = applyTile pos dir grid
    nextPos = map (move pos) nextDirs

beamPosition :: Beam -> Position
beamPosition (Beam pos _) = pos

getPositions :: Grid -> S.Set Beam -> S.Set Position
getPositions grid = S.filter (`M.member` grid) . S.map beamPosition

advanceBeams :: [Beam] -> S.Set Beam -> Grid -> S.Set Beam
advanceBeams [] positions _ = positions
advanceBeams beams positions grid = advanceBeams newBeams (foldr S.insert positions beams) grid
  where
    newBeams = filter (`S.notMember` positions) (concatMap (`advanceBeam` grid) beams)

illuminates :: Beam -> Grid -> Int
illuminates beam grid = length $ getPositions grid $ advanceBeams [beam] S.empty grid

part1 = illuminates (Beam (1,1) R)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)

gridBounds :: Grid -> (Int, Int)
gridBounds = both maximum . unzip . M.keys

startBeams grid =
  [Beam (1,x) R | x <- [1..yMax]] ++
  [Beam (x,1) D | x <- [1..xMax]] ++
  [Beam (xMax,x) L | x <- [1..yMax]] ++
  [Beam (x,yMax) U | x <- [1..xMax]]
  where
    (xMax, yMax) = gridBounds grid

part2 grid = maximum $ map (`illuminates` grid) $ startBeams grid