import qualified Data.Set as Set
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

type Interval = (Int, Int)

type Point = (Int, Int, Int)

data Command = On Interval Interval Interval | Off Interval Interval Interval deriving (Show)

getInput = fmap (map parseLine . lines) . readFile

parseLine xs = case xs of
  ('o' : 'n' : ys) -> On xInterval yInterval zInterval
  ('o' : 'f' : 'f' : ys) -> Off xInterval yInterval zInterval
  _ -> error "Invalid line format"
  where
    (xInterval, remaining) = parseInterval $ dropWhile (/= ' ') xs
    (yInterval, remaining') = parseInterval remaining
    (zInterval, _) = parseInterval remaining'

parseInterval xs = ((from, to), remaining)
  where
    (parsed, remaining) = break (== ',') $ tail xs
    interval = break (== '.') parsed
    from = read $ tail $ tail $ fst interval
    to = read $ tail $ tail $ snd interval

-- Solution Logic

getPoints :: Interval -> Interval -> Interval -> [Point]
getPoints xInt yInt zInt = [(x, y, z) | x <- interval xInt, y <- interval yInt, z <- interval zInt]

interval (from, to) = [from .. to]

within50 (On xInt yInt zInt) = max50 xInt && max50 yInt && max50 zInt
within50 (Off xInt yInt zInt) = max50 xInt && max50 yInt && max50 zInt

max50 (from, to) = from >= -50 && from <= 50 && to >= -50 && to <= 50

applyCommand (On xInt yInt zInt) litPoints = Set.union litPoints $ Set.fromList $ getPoints xInt yInt zInt
applyCommand (Off xInt yInt zInt) litPoints = Set.difference litPoints $ Set.fromList $ getPoints xInt yInt zInt

part1 = Set.size . foldl (flip applyCommand) Set.empty . filter within50

part2 xs = Set.size $ foldl (flip applyCommand) Set.empty xs