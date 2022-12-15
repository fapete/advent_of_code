import Data.List
import System.Environment
import Data.Bifunctor (bimap, second)
import Data.Bool (bool)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
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

coveredIntervalOnLine line (Sensor (x,y) _ d) 
  | from <= to = Just (from, to)
  | otherwise = Nothing
  where
    (from, to) = (x - d + distFromCenter, x + (d - distFromCenter))
    distFromCenter = abs (y - line)

intervalIntersect :: (Int, Int) -> (Int, Int) -> Bool
intervalIntersect (from, to) (from', to') =
  to >= from' && to <= to' ||
  from >= from' && from <= to' ||
  from <= from' && to >= to' ||
  from' <= from && to' >= to

intervalUnion :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
intervalUnion i@(from, to) i'@(from', to')
  | not $ intervalIntersect i i' = [i,i']
  | otherwise = [(min from from', max to to')]

reduce :: [(Int, Int)] -> [(Int, Int)]
reduce intervals
  | length intervals == length newIntervals = intervals
  | otherwise = reduce newIntervals
    where
      newIntervals = concatMap (uncurry intervalUnion) $ pairs intervals

pairs :: [b] -> [(b, b)]
pairs xs = map (\xs -> (head xs, last xs)) $ chunksOf 2 xs

part1 :: Int -> [Sensor] -> Int
part1 lineNum sensors = abs $ uncurry (-) $ head $ reduce $ sort $ mapMaybe (coveredIntervalOnLine lineNum) sensors

part2 :: Int -> [Sensor] -> Int
part2 maxCoord sensors = (\(y, (_,x)) -> (x+1) * 4000000 + y)
  $ second head
  $ fromMaybe (0, [(0,0)])
  $ find ((== 2) . length . snd)
  $ map (\line -> (line, reduce $ sort $ mapMaybe (coveredIntervalOnLine line) sensors)) [0..maxCoord]