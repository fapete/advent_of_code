import Data.Char
import Data.List (sort)
import Data.Maybe
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

getInput :: FilePath -> IO [[Int]]
getInput = fmap (map parseLine . lines) . readFile

parseLine = map (read . (: []))

-- Solution Logic

type OctopusState = ([[Int]], Int)

part1 input = snd (compute runStep (input, 0) !! 100)

part2 input = length $ takeWhile (any (any (/= 0)) . fst) $ compute runStep (input, 0)

compute stepFn state = state : compute stepFn (stepFn state)

runStep (energyLevels, numFlashes) = (newEnergyLevels, numFlashes + stepFlashes)
  where
    (newEnergyLevels, stepFlashes) = evalFlash (increaseAll energyLevels, 0)

increaseAll = map (map (+ 1))

evalFlash (levels, count) = (map (map (\x -> if x > 9 then 0 else x)) newLevels, countFlashes newLevels)
  where
    newLevels = head $ dropWhile hasNewFlash $ compute increaseFlashing levels

hasNewFlash = any (elem 10)

increaseFlashing levels = foldl (\acc pos -> increaseAt acc (adjacent pos) pos) levels $ findFlashingPositions levels

adjacent (x, y) = [(x', y') | x' <- [x -1, x, x + 1], y' <- [y -1, y, y + 1], inBounds (x', y')]

inBounds (x, y) = x >= 0 && x < 10 && y >= 0 && y < 10

increaseAt levels ((x, y) : pos) centre = increaseAt updated pos centre
  where
    updated = take y levels ++ changedRow ++ drop (y + 1) levels
    changedRow = [take x (levels !! y) ++ [if (x, y) == centre || levels !! y !! x > 10 then (levels !! y !! x) + 1 else min ((levels !! y !! x) + 1) 10] ++ drop (x + 1) (levels !! y)]
increaseAt levels [] _ = levels

findFlashingPositions levels = filter (\(x, y) -> levels !! y !! x == 10) [(x, y) | x <- [0 .. l], y <- [0 .. l]]
  where
    l = length levels -1

countFlashes levels = length $ do
  row <- levels
  filter (> 9) row