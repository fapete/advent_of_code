import System.Environment
import Data.Char
import Control.Monad (join)
import Data.Bifunctor (bimap)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  (p1Solution, p2Solution) <- solve parts filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

type Point = (Int, Int)
type Bounds = (Point, Point)

getInput = fmap parse . readFile

parse :: String -> Bounds
parse xs = ((x1, y1), (x2, y2))
    where
        (x1, x2) = (bimap read (read . tail . tail) . break (== '.')) xPart
        (y1, y2) = (bimap read (read . tail . tail) . break (== '.')) yPart
        (xPart, yPart) = bimap (tail . dropWhile (/= '=')) (tail . dropWhile (/= '=')) $ break (== ',') xs

-- Solution Logic

parts bounds = (maximum $ map snd =<< t, length t)
    where t = trajectoriesInBounds bounds

-- Choosing 3000 for upper bound on y on the completely unfounded reasoning that it's probably high enough.
trajectoriesInBounds bounds = filter (any (inBounds bounds)) [trajectory bounds x y (0,0) | x <- [0..(maxX bounds)], y <- [(minY bounds)..3000]]

maxX :: Bounds -> Int
maxX = uncurry max . bimap fst fst

minY :: Bounds -> Int
minY = uncurry min . bimap snd snd

inBounds :: (Ord a1, Ord a2) => ((a1, a2), (a1, a2)) -> (a1, a2) -> Bool
inBounds ((x1, y1), (x2, y2)) (x,y) = x >= x1 && x <= x2 && y >= y1 && y <= y2

trajectory bounds dx dy pos@(x, y)
    | overshot bounds pos = []
    | otherwise = pos : trajectory bounds (if dx > 0 then dx-1 else 0) (dy - 1) (x+dx, y + dy)

overshot :: (Ord a1, Ord a2) => ((a3, a2), (a1, b)) -> (a1, a2) -> Bool
overshot ((_, y1), (x2, _)) (x,y) = x > x2 || y < y1