import Data.List (subsequences)
import qualified Data.Set as Set
import System.Environment
import THNames (overlappableDataConKey)

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

type Point = (Int, Int, Int)

type Interval = (Integer, Integer)

data Command = On Cube | Off Cube deriving (Show)

type Cube = (Interval, Interval, Interval)

getInput = fmap (map parseLine . lines) . readFile

parseLine xs = case xs of
  ('o' : 'n' : ys) -> On cube
  ('o' : 'f' : 'f' : ys) -> Off cube
  _ -> error "Invalid line format"
  where
    cube = ((x1, x2), (y1, y2), (z1, z2))
    ((x1, x2), remaining) = parseInterval $ dropWhile (/= ' ') xs
    ((y1, y2), remaining') = parseInterval remaining
    ((z1, z2), _) = parseInterval remaining'

parseInterval xs = ((from, to), remaining)
  where
    (parsed, remaining) = break (== ',') $ tail xs
    interval = break (== '.') parsed
    from = read $ tail $ tail $ fst interval
    to = read $ tail $ tail $ snd interval

-- Solution Logic

getPoints ((x1, x2), (y1, y2), (z1, z2)) = [(x, y, z) | x <- [x1 .. x2], y <- [y1 .. y2], z <- [z1 .. z2]]

within50 (On (i1, i2, i3)) = max50 i1 && max50 i2 && max50 i3
within50 (Off (i1, i2, i3)) = max50 i1 && max50 i2 && max50 i3

max50 (x, y) = x >= -50 && x <= 50 && y >= -50 && y <= 50

applyCommand (On cube) litPoints = Set.union litPoints $ Set.fromList $ getPoints cube
applyCommand (Off cube) litPoints = Set.difference litPoints $ Set.fromList $ getPoints cube

part1 = Set.size . foldl (flip applyCommand) Set.empty . filter within50

sizeCubeUnion cube1 cube2 = cubeSize cube1 + cubeSize cube2 - cubeSize (intersectionCube cube1 cube2)

union = sum . map cubeSize

intersection :: [Cube] -> Integer
intersection = cubeSize . foldl1 intersectionCube

inclusionExclusion :: [Cube] -> Integer
inclusionExclusion = foldl (\acc i -> acc + ((-1) ^ (length i + 1) * intersection i)) 0 . tail . subsequences

sizeCubeDifference cube1 cube2 = cubeSize cube1 - cubeSize (intersectionCube cube1 cube2)

intersectionCube (i1, i2, i3) (i1', i2', i3') = (i1 `intersect` i1', i2 `intersect` i2', i3 `intersect` i3')

intersect (x1, x2) (x1', x2') = (max x1 x1', min x2 x2')

cubeSize :: Cube -> Integer
cubeSize (i1, i2, i3) = intervalSize i1 * intervalSize i2 * intervalSize i3

intervalSize :: Interval -> Integer
intervalSize (x1, x2) = x2 - x1 + 1

isOn (On cube) = True
isOn (Off cube) = False

extractCube (On cube) = cube
extractCube (Off cube) = cube

allIntersections cube = map (intersectionCube cube)

lights numLit processedCommands (comm@(On cube) : remainingCommands) = lights (numLit + newLights) (comm : processedCommands) remainingCommands
  where
    newLights = allIntersections cube (map extractCube)
lights numLit processedCommands (comm@(Off cube) : remainingCommands) = lights (numLit - removedLights) (comm : processedCommands) remainingCommands
lights numLit _ [] = numLit

newPart1 = inclusionExclusion . map extractCube . filter (\c -> isOn c && within50 c)

part1OnlyOn = Set.size . foldl (flip applyCommand) Set.empty . filter (\c -> isOn c && within50 c)

part2OnlyOn = Set.size . foldl (flip applyCommand) Set.empty . filter isOn

part2 xs = 4