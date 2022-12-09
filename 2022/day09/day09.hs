import Data.List
import System.Environment
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Bifunctor as Bifunctor

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

type Pos = (Int, Int)
data Motion = U Int | D Int | R Int | L Int deriving (Show)

getInput = fmap (map (parseMotion . splitOn " ") . lines) . readFile

parseMotion [direction, steps]
  | direction == "U" = U $ read steps
  | direction == "D" = D $ read steps
  | direction == "R" = R $ read steps
  | direction == "L" = L $ read steps
  | otherwise = error "Invalid direction"
parseMotion xs = error "Invalid motion parse"

-- Solution Logic

stepCount (U i) = i
stepCount (D i) = i
stepCount (R i) = i
stepCount (L i) = i

reduceOneStep (U i) = U $ i-1
reduceOneStep (D i) = D $ i-1
reduceOneStep (R i) = R $ i-1
reduceOneStep (L i) = L $ i-1

applyMotion :: (Pos, Pos) -> S.Set Pos -> Motion -> ((Pos, Pos), S.Set Pos)
applyMotion positions tailPositions motion
  | stepCount motion == 0 = (positions, tailPositions)
applyMotion (headPos, tailPos) tailPositions motion = applyMotion (newHeadPos, newTailPos) (S.insert newTailPos tailPositions) (reduceOneStep motion)
  where
    newHeadPos = moveHeadOne headPos motion
    newTailPos = updateTail newHeadPos tailPos
    steps = stepCount motion

moveHeadOne (hx, hy) (U _) = (hx, hy+1)
moveHeadOne (hx, hy) (D _) = (hx, hy-1)
moveHeadOne (hx, hy) (R _) = (hx+1, hy)
moveHeadOne (hx, hy) (L _) = (hx-1, hy)

updateTail :: Pos -> Pos -> Pos
updateTail (hx, hy) (tx, ty)
  | hx == tx && hy == (ty + 2) = (tx, ty+1)
  | hx == tx && hy == (ty - 2) = (tx, ty-1)
  | hx == (tx + 2) && hy == ty = (tx+1, ty)
  | hx == (tx - 2) && hy == ty = (tx-1, ty)
  | hx == (tx + 1) && hy == (ty + 2) || hx == (tx + 2) && hy == (ty + 1) = (tx+1, ty+1)
  | hx == (tx - 1) && hy == (ty + 2) || hx == (tx - 2) && hy == (ty + 1) = (tx-1, ty+1)
  | hx == (tx - 1) && hy == (ty - 2) || hx == (tx - 2) && hy == (ty - 1) = (tx-1, ty-1)
  | hx == (tx + 1) && hy == (ty - 2) || hx == (tx + 2) && hy == (ty - 1) = (tx+1, ty-1)
  | otherwise = (tx, ty)

applyAllMotions = foldl (\(positions, tailPositions) motion -> applyMotion positions tailPositions motion) (((0,0), (0,0)), S.singleton (0,0))

part1 = length . snd . applyAllMotions