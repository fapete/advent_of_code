import Data.List
import System.Environment
import Data.List.Split (splitOn)
import Data.Text.Array (equal)
import Data.Bool (bool)

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

data Tile = Ash | Rock deriving (Show, Eq)
type Map = [[Tile]]

getInput1 = fmap (map parseMap . paragraphs) . readFile

paragraphs :: String -> [[String]]
paragraphs = splitOn [""] . lines

parseMap :: [String] -> Map
parseMap = map (map parseTile)

parseTile :: Char -> Tile
parseTile '.' = Ash
parseTile '#' = Rock
parseTile _ = error "Not a valid tile"

getInput2 = getInput1

-- Solution Logic

pairs :: Int -> [(Int, Int)]
pairs upTo = zip [0..(upTo-1)] [1..upTo]

getBound :: Map -> Int
getBound = flip (-) 1 . length

equalRows :: Map -> (Int, Int) -> Bool
equalRows m (x,y) = m!!x == m!!y

candidates :: Map -> [(Int, Int)]
candidates m = filter (equalRows m) $ pairs $ getBound m

checkCandidate :: Map -> (Int, Int) -> Bool
checkCandidate m (x,y)
  | x < 0 || y > getBound m = True
  | otherwise = equalRows m (x,y) && checkCandidate m (x - 1, y + 1)

findReflection :: Map -> Maybe (Int, Int)
findReflection m = if null reflection then Nothing else Just $ head reflection
  where
    reflection = filter (checkCandidate m) $ candidates m

scoreReflection :: Map -> Int
scoreReflection m = vertical + horizontal
  where
    vertical = 100 * maybe 0 ((1 +) . fst) (findReflection m)
    horizontal = maybe 0 ((1 +) . fst) (findReflection $ transpose m)

part1 = sum . map scoreReflection

part2 = part1