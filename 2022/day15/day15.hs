import Data.List
import System.Environment
import Data.Bifunctor (bimap, second)
import Data.Bool (bool)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import Data.List.Split (chunksOf)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  let lineNum = read $ args !! 1
  let maxCoord = read $ args !! 2
  p1Solution <- solve (part1 lineNum) filename
  p2Solution <- solve (part2 maxCoord) filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

type Pos = (Int, Int)
data Sensor  = Sensor Pos Pos Int deriving (Show, Eq, Ord) -- Sensor position, closest beacon position, distance covered

getInput = fmap (map (uncurry parseSensor . parsePositions) . lines) . readFile

parseSensor sensorPos beaconPos = Sensor sensorPos beaconPos $ manhattan sensorPos beaconPos

manhattan (x,y) (x', y') = abs(x-x') + abs(y-y')

parsePositions = bimap getPos getPos . break (== ':')

getPos :: String -> Pos
getPos = bimap (read . tail . dropWhile (/= '=')) (read . drop 4) . break (== ',')

-- Solution Logic

coveredIntervals (Sensor (x,y) _ d) = [(y + d', (x - d + abs d', x + (d - abs d'))) | d' <- [-d..d] ]

intervalIntersect (from, to) (from', to') =
  to >= from' && to <= to' ||
  from >= from' && from <= to' ||
  from <= from' && to >= to' ||
  from' <= from && to' >= to

intervalUnion i@(from, to) i'@(from', to')
  | not $ intervalIntersect i i' = [i,i']
  | otherwise = [(min from from', max to to')]

reduce intervals
  | length intervals == length newIntervals = intervals
  | otherwise = reduce newIntervals
    where
      newIntervals = concatMap (uncurry intervalUnion) $ pairs intervals

pairs xs = map (\xs -> (head xs, last xs)) $ chunksOf 2 xs

intervalsOnLine line = concatMap (map snd . filter (\(y, _) -> y == line))

part1 lineNum sensors = abs $ uncurry (-) $ head $ reduce $ sort $ intervalsOnLine lineNum sensorIntervals
  where
    sensorIntervals = map coveredIntervals sensors

part2 maxCoord sensors = (\(y, (_,x)) -> (x+1) * 4000000 + y)
  $ second head
  $ fromMaybe (0, [(0,0)])
  $ find ((== 2) . length . snd)
  $ map (\line -> (line, reduce $ sort $ intervalsOnLine line sensorIntervals)) [0..maxCoord]
  where
    sensorIntervals = map coveredIntervals sensors