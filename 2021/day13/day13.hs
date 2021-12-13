import qualified Data.Set as Set
import System.Environment
import qualified Data.Bifunctor

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: \n" ++ p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

type Point = (Int, Int)
type Grid = Set.Set Point
data Fold = Hor Int | Vert Int deriving (Show)

getInput = fmap (Data.Bifunctor.bimap parseDots parseFolds . break (== "") . lines) . readFile

parseFolds :: [String] -> [Fold]
parseFolds xs = map (parseFold . split '=') $ tail xs

parseFold [instruction, at] = case last instruction of
    'y' -> Hor (read at)
    'x' -> Vert (read at)
    _ -> error "Not a fold instruction"
parseFold _ = error "Parse error at fold instruction"

parseDots :: [String] -> Grid
parseDots xs = foldl (flip Set.insert) Set.empty (map (parsePoint . split ',') xs)

parsePoint [x, y] = (read x, read y)
parsePoint _ = error "Not a point"

split c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

-- Solution Logic

part1 (dots, folds) = Set.size $ applyFold (head folds) dots

applyFold :: Fold -> Grid -> Grid
applyFold (Hor i) dots = Set.union (Set.filter ((< i) . snd) dots) (Set.map (\(x, y) -> (x, i - (y - i))) $ Set.filter ((> i) . snd) dots)
applyFold (Vert i) dots = Set.union (Set.filter ((< i). fst) dots) (Set.map (\(x, y) -> (i - (x - i), y)) $ Set.filter ((> i) . fst) dots)

part2 (dots, folds) = display $ foldl (flip applyFold) dots folds

display dots = foldl1 (++) [(++ "\n")
   ([if Set.member (x, y) dots then '#' else ' ' |
       x <- [0 .. maxX]]) |
   y <- [0 .. maxY]]
    where
        maxX = maximum $ Set.map fst dots
        maxY = maximum $ Set.map snd dots