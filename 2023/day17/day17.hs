import Data.List
import qualified Data.Map as M
import System.Environment
import Data.Char (digitToInt)

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

data Direction = U | D | L | R deriving (Show, Eq, Ord)

data Node = Node Position Int Int Direction deriving (Show, Eq, Ord) -- Position, Cooldown, Available Steps in Direction, Coming from

type Position = (Int, Int)

type Grid = [[Int]]

getInput1 :: FilePath -> IO Grid
getInput1 = fmap (map (map digitToInt) . lines) . readFile

getInput2 = getInput1

-- Solution Logic

vector :: Direction -> (Int, Int)
vector U = (0, -1)
vector D = (0, 1)
vector R = (1, 0)
vector L = (-1, 0)

tupleAdd :: Num a => (a, a) -> (a, a) -> (a, a)
tupleAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

move :: Position -> Direction -> Position
move pos dir = pos `tupleAdd` vector dir

costAt :: Position -> Grid -> Int
costAt (x, y) grid = grid!!y!!x

rightTurns :: Direction -> [Direction]
rightTurns U = [L, R]
rightTurns D = [L, R]
rightTurns R = [U, D]
rightTurns L = [U, D]

isInBounds :: Position -> Grid -> Bool
isInBounds (x,y) grid = x >= 0 && y >= 0 && x < length (head grid) && y < length grid

adjacent :: Node -> Grid -> [Node]
adjacent (Node position cooldown 0 facing) grid =
  map (Node position 0 3) (rightTurns facing)
adjacent (Node position cooldown steps facing) grid =
  Node nextPosition (costAt nextPosition grid) (steps-1) facing:
  map (Node position 0 3) (rightTurns facing)
  where
    nextPosition = move position facing

part1 xs = 4

part2 = part1