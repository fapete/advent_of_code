import Data.Char (isDigit)
import Data.List
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust)
import Data.Set qualified as S
import System.Environment

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 getInput1 filename
  p2Solution <- solve part2 getInput1 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve solver parser = fmap solver . parser

-- Input Parsing

data EnginePart = Part Position Integer | Symbol Char | None deriving (Show, Eq, Ord)

type Position = (Int, Int)

type EngineSchematic = M.Map Position EnginePart

getInput1 = fmap (parseEngine . lines) . readFile

parseEngine :: [String] -> EngineSchematic
parseEngine lines = M.fromList $ concat [parseLine (lines !! y) y | y <- [0 .. (length lines - 1)]]

parseLine :: String -> Int -> [(Position, EnginePart)]
parseLine = parseLine' 0

parseLine' :: Int -> String -> Int -> [(Position, EnginePart)]
parseLine' _ "" _ = []
parseLine' x line y = parseParts prefix (x, y) ++ parseLine' newX remainder y
  where
    prefix = if isDigit $ head line then takeWhile isDigit line else takeWhile (== head line) line
    remainder = if isDigit $ head line then dropWhile isDigit line else dropWhile (== head line) line
    newX = x + length prefix

parseParts :: String -> Position -> [(Position, EnginePart)]
parseParts parts initPos@(initialX, initialY)
  | head parts == '.' = [((x + initialX, initialY), None) | x <- [0 .. (length parts - 1)]]
  | isDigit $ head parts = [((x + initialX, initialY), Part initPos $ read parts) | x <- [0 .. (length parts - 1)]]
  | otherwise = [((x + initialX, initialY), Symbol $ parts !! x) | x <- [0 .. (length parts - 1)]]

-- Solution Logic

isPart :: EnginePart -> Bool
isPart (Part _ _) = True
isPart _ = False

isSymbol :: EnginePart -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

extractPartNum :: EnginePart -> Integer
extractPartNum (Part _ i) = i
extractPartNum _ = 0

getWithPosition :: Position -> EngineSchematic -> Maybe EnginePart
getWithPosition key map = map M.!? key

adjacentNums :: EngineSchematic -> Position -> S.Set EnginePart
adjacentNums engine (x, y) = S.filter isPart $ S.fromList $ catMaybes [engine M.!? (x + x', y + y') | x' <- [-1 .. 1], y' <- [-1 .. 1]]

symbolPositions :: EngineSchematic -> [Position]
symbolPositions = M.keys . M.filter isSymbol

isGearSymbol :: EnginePart -> Bool
isGearSymbol (Symbol '*') = True
isGearSymbol _ = False

potentialGearPositions :: EngineSchematic -> [Position]
potentialGearPositions = M.keys . M.filter isGearSymbol

extractGearRatios :: EngineSchematic -> Integer
extractGearRatios engine = sum $ map (product . map extractPartNum . S.toList) $ filter (\x -> S.size x == 2) $ map (adjacentNums engine) $ potentialGearPositions engine

part1 engine = sum $ map extractPartNum $ S.toList $ S.fromList $ concatMap (S.toList . adjacentNums engine) $ symbolPositions engine

part2 = extractGearRatios