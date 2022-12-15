import Data.List
import System.Environment
import Data.Bifunctor (bimap)
import Data.Bool (bool)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  let lineNum = read $ args !! 1
  p1Solution <- solve (part1 lineNum) filename
  --p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  --putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

type Pos = (Int, Int)
data Sensor  = Sensor Pos Pos Int deriving (Show, Eq, Ord) -- Sensor position, closest beacon position, distance covered

getInput = fmap (map (uncurry parseSensor . parsePositions) . lines) . readFile

parseSensor sensorPos beaconPos = Sensor sensorPos beaconPos $ manhattan sensorPos beaconPos

parsePositions = bimap getPos getPos . break (== ':')

getPos :: String -> Pos
getPos = bimap (read . tail . dropWhile (/= '=')) (read . drop 4) . break (== ',')

-- Solution Logic

manhattan (x,y) (x', y') = abs(x-x') + abs(y-y')

isCovered pos (Sensor sensorPos _ coveredDistance) = manhattan pos sensorPos <= coveredDistance

beaconAt pos (Sensor _ beaconPos _) = pos == beaconPos

coveredMaxDim sensors = (minX - distMax, maxX + distMax)
  where
    (Sensor (minX, _) _ _, Sensor (maxX, _) _ _) = (minimum sensors, maximum sensors)
    distMax = maximum $ map (\(Sensor _ _ d) -> d) sensors
    

part1 :: Int -> [Sensor] -> Int
part1 lineNum sensors = sum $ [(bool 0 1 . \pos -> any (isCovered pos) sensors && not (any (beaconAt pos) sensors) ) (x, lineNum) | x <- [from .. to]]
  where
    (from, to) = coveredMaxDim sensors