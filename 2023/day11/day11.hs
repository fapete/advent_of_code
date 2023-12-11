import Data.List
import System.Environment
import qualified Data.IntMap as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Bifunctor (Bifunctor(bimap))

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

type Position = (Int, Int)
type Galaxies = M.IntMap Position

getInput1 = fmap ((\lines -> (parseGalaxies lines, (emptyLines lines, emptyCols lines))) . lines) . readFile

parseGalaxies :: [String] -> Galaxies
parseGalaxies ls = M.fromList $ zip [1..] $ concat $ zipWith parseGalaxiesInLine [0,1..] ls

parseGalaxiesInLine :: Int -> String -> [Position]
parseGalaxiesInLine yPosition line
  | '#' `elem` line = reverse $ drop 1 $ foldl' accumulator [(head xOffsets, yPosition)] $ tail xOffsets
  | otherwise = []
  where
    xOffsets = map length $ splitOn "#" line
    accumulator acc xOff = (xOff + prevXPos + 1, yPosition):acc
      where
        prevXPos = fst $ head acc

emptyLines :: [String] -> S.Set Int
emptyLines = S.fromList . map fst . filter snd . zipWith (\lineIdx line -> (lineIdx, isEmptyLine line)) [0..]

emptyCols :: [String] -> S.Set Int
emptyCols = emptyLines . transpose

isEmptyLine :: String -> Bool
isEmptyLine = notElem '#'

getInput2 = getInput1

-- Solution Logic

manhattan :: Position -> Position -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

spaceExtension :: Position -> Position -> S.Set Int -> S.Set Int -> Int -> Int
spaceExtension (x1, y1) (x2, y2) emptyLines emptyCols by = (S.size colsInBetween * by) + (S.size linesInBetween * by)
  where
    colsInBetween = S.intersection (S.fromList [(min x1 x2)..(max x1 x2)]) emptyCols
    linesInBetween = S.intersection (S.fromList [(min y1 y2)..(max y1 y2)]) emptyLines

extendedDistance :: Position -> Position -> S.Set Int -> S.Set Int -> Int -> Int
extendedDistance p1 p2 el ec by = manhattan p1 p2 + spaceExtension p1 p2 el ec by

galaxyPairs :: Galaxies -> [(Position, Position)]
galaxyPairs gs = map (bimap (gs M.!) (gs M.!)) $ [(x,y) | x <- [1..numGals], y <- [2..numGals], x < y]
  where
    numGals = length $ M.keys gs

part1 (galaxies, (emptyLines, emptyCols)) = sum $ map (\(g1, g2) -> extendedDistance g1 g2 emptyLines emptyCols 1) $ galaxyPairs galaxies

part2 (galaxies, (emptyLines, emptyCols)) = sum $ map (\(g1, g2) -> extendedDistance g1 g2 emptyLines emptyCols 999999) $ galaxyPairs galaxies