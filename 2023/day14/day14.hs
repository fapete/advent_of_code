import Data.List
import qualified Data.Map as M
import System.Environment
import Data.Bool (bool)
import Data.List.Split (chunksOf)

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

data Tile = Empty | Rolling | Stationary deriving (Show, Eq)
data Direction = North | South | East | West deriving (Show, Eq)
type Dish = M.Map Position Tile
type Position = (Int, Int)

getInput1 :: FilePath -> IO Dish
getInput1 = fmap (M.fromList . concat . zipWith parseLine [1..] . lines) . readFile

parseLine :: Int -> String -> [(Position, Tile)]
parseLine y = zipWith (\x c -> ((x, y), parseTile c)) [1..]

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile 'O' = Rolling
parseTile '#' = Stationary
parseTile _ = error "Not a vaild tile"

getInput2 = getInput1

-- Solution Logic

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

getBounds :: Dish -> (Int, Int)
getBounds dish = both maximum $ unzip $ M.keys dish

getBoundInDir :: Direction -> Dish -> Int
getBoundInDir North dish = 1
getBoundInDir South dish = snd $ getBounds dish
getBoundInDir East dish = fst $ getBounds dish
getBoundInDir West _ = 1

tilt :: Direction -> Dish -> Dish
tilt dir dish = tilt' dir (getInitialPositions dir dish) dish

tilt' :: Direction -> [Int] -> Dish -> Dish
tilt' dir stationaries dish
  | (dir == South || dir == East) && all (>= getBoundInDir dir dish) stationaries = dish
  | (dir == North || dir == West) && all (<= getBoundInDir dir dish) stationaries = dish
  | otherwise = tilt' dir (map (adjust dir) newStationaries) $ rollCountInDir counts dir stationaries newStationaries dish
  where
    counts = rollingCountsInDir dir stationaries newStationaries dish
    newStationaries = findStationaryInDir dir stationaries dish
    adjust North x = x-1
    adjust South x = x+1
    adjust East x = x+1
    adjust West x = x-1

rollCountInDir :: [Int] -> Direction -> [Int] -> [Int] -> Dish -> Dish
rollCountInDir counts dir from to dish = foldr replace dish $ zip (zip (zip [1..] from) to) counts
  where
    replace (((colLine, x), bound), count) dish = case dir of
      North -> foldr (\(pos, c) dish -> if c > 0 then M.insert pos Rolling dish else replaceRolling pos dish) dish $ zip (zip [colLine, colLine..] [x, x-1..bound]) [count, count-1..]
      South -> foldr (\(pos, c) dish -> if c > 0 then M.insert pos Rolling dish else replaceRolling pos dish) dish $ zip (zip [colLine, colLine..] [x..bound]) [count, count-1..]
      East -> foldr (\(pos, c) dish -> if c > 0 then M.insert pos Rolling dish else replaceRolling pos dish) dish $ zip (zip [x..bound] [colLine, colLine..]) [count, count-1..]
      West -> foldr (\(pos, c) dish -> if c > 0 then M.insert pos Rolling dish else replaceRolling pos dish) dish $ zip (zip [x, x-1..bound] [colLine, colLine..]) [count, count-1..]

replaceRolling :: Position -> Dish -> Dish
replaceRolling pos dish = case dish M.! pos of
  Rolling -> M.insert pos Empty dish
  _ -> dish

rollingCountsInDir :: Direction -> [Int] -> [Int] -> Dish -> [Int]
rollingCountsInDir dir from to dish = [countRolling x | x <- zip (zip [1..] from) to]
  where
    countRolling ((colLine, x), bound) = case dir of
      North -> foldr (\pos acc -> if dish M.! pos == Rolling then acc+1 else acc) 0 $ zip [colLine, colLine..] [x, x-1..bound]
      South -> foldr (\pos acc -> if dish M.! pos == Rolling then acc+1 else acc) 0 $ zip [colLine, colLine..] [x..bound]
      East -> foldr (\pos acc -> if dish M.! pos == Rolling then acc+1 else acc) 0 $ zip [x..bound] [colLine, colLine..]
      West -> foldr (\pos acc -> if dish M.! pos == Rolling then acc+1 else acc) 0 $ zip [x, x-1..bound] [colLine, colLine..]

findStationaryInDir :: Direction -> [Int] -> Dish -> [Int]
findStationaryInDir dir from dish = [findNext x | x <- zip [1..] from]
  where
    max = getBoundInDir dir dish
    findNext (colLine, x) = bool (head nextPosition) max (null nextPosition)
      where
        nextPosition = case dir of
          North -> dropWhile (\idx -> Stationary /= dish M.! (colLine, idx)) [x, x-1..max]
          South -> dropWhile (\idx -> Stationary /= dish M.! (colLine, idx)) [x..max]
          East -> dropWhile (\idx -> Stationary /= dish M.! (idx, colLine)) [x..max]
          West -> dropWhile (\idx -> Stationary /= dish M.! (idx, colLine)) [x, x-1..max]

computeLoad :: Dish -> Int
computeLoad dish = sum $ zipWith (*) (map rollingCounts [1..yBound]) [yBound, yBound - 1..1]
  where
    (xBound, yBound) = getBounds dish
    rollingCounts row = foldr (\pos acc -> if dish M.! pos == Rolling then acc + 1 else acc) 0 $ zip [1..xBound] [row,row..]

getInitialPositions :: Direction -> Dish -> [Int]
getInitialPositions South dish = replicate (fst $ getBounds dish) 1
getInitialPositions East dish = getInitialPositions South dish
getInitialPositions North dish = replicate (fst $ getBounds dish) (fst $ getBounds dish)
getInitialPositions West dish = getInitialPositions North dish

part1 dish = computeLoad $ tilt South dish

dishCycle = [South, East, North, West]

doOneCycle :: Dish -> Dish
doOneCycle dish = foldl' (flip tilt) dish dishCycle

doNCycles :: Int -> Dish -> Dish
doNCycles n dish = iterate doOneCycle dish!!n

findCycle :: Dish -> (Int, Int)
findCycle dish = go [] dish
  where
    go dishes dish = case elemIndex dish dishes of
      Just idx -> (length dishes - idx, length dishes - (length dishes - idx))
      Nothing -> go (dish:dishes) $ doOneCycle dish

part2 dish = computeLoad $ doNCycles (startCycle+period+1+remainingCycles) dish
  where
    (startCycle, period) = findCycle dish
    cycles = (1000000000 - startCycle) `div` (period+1)
    remainingCycles = (1000000000 - startCycle) `rem` (period+1)

printDish :: M.Map k Tile -> IO ()
printDish dish = putStrLn $ unlines (transpose $ chunksOf 10 $ map tileToChar $ M.elems dish)

tileToChar :: Tile -> Char
tileToChar Empty = '.'
tileToChar Rolling = 'O'
tileToChar Stationary = '#'