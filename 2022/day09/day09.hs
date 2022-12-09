import Data.List
import System.Environment
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as M

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

applyMotion :: [Int] -> M.Map Int Pos -> S.Set Pos -> Motion -> (M.Map Int Pos, S.Set Pos)
applyMotion _ positionMap tailPositions motion
  | stepCount motion == 0 = (positionMap, tailPositions)
applyMotion knots positionMap tailPositions motion =
  applyMotion knots newPositionMap newTailPositions (reduceOneStep motion)
  where
    headPos = positionMap M.! head knots
    newHeadPos = moveHeadOne headPos motion
    tailKnotPositions = map (positionMap M.!) (tail knots)
    newTailKnotPositions = updateTails newHeadPos tailKnotPositions
    newPositionMap = foldl (\agg (key, pos) -> M.insert key pos agg) positionMap (zip knots (newHeadPos:newTailKnotPositions))
    newTailPositions = S.insert (last newTailKnotPositions) tailPositions

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
  | hx == (tx + 1) && hy == (ty + 2) || hx == (tx + 2) && hy == (ty + 1) || hx == (tx + 2) && hy == (ty + 2) = (tx+1, ty+1)
  | hx == (tx - 1) && hy == (ty + 2) || hx == (tx - 2) && hy == (ty + 1) || hx == (tx - 2) && hy == (ty + 2) = (tx-1, ty+1)
  | hx == (tx - 1) && hy == (ty - 2) || hx == (tx - 2) && hy == (ty - 1) || hx == (tx - 2) && hy == (ty - 2) = (tx-1, ty-1)
  | hx == (tx + 1) && hy == (ty - 2) || hx == (tx + 2) && hy == (ty - 1) || hx == (tx + 2) && hy == (ty - 2) = (tx+1, ty-1)
  | otherwise = (tx, ty)

updateTails _ [] = []
updateTails headPos (tailPos:tailPositions) = newTailPos:updateTails newTailPos tailPositions
  where
    newTailPos = updateTail headPos tailPos

applyAllMotions knots = foldl (\(positions, tailPositions) motion -> applyMotion knots positions tailPositions motion) (initialPositions knots, S.empty)

initialPositions knots = M.fromList [(x, (0,0)) | x <- knots]

part1 = length . snd . applyAllMotions [0, 1]

part2 = length . snd . applyAllMotions [0..9]