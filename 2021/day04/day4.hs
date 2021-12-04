import Data.Char
import System.Environment
import Text.XHtml (cols)

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve fn filename = do
  input <- getInput filename
  return (fn input)

getInput filename = do
  x <- readFile filename
  return (drawnNums $ head $ lines x, boards $ tail $ lines x)

drawnNums xs = map (\num -> read num :: Int) (split ',' xs)

newtype Board = Board [(Int, Bool)] deriving (Show)

parseBoard xs = Board (map (\num -> (read num :: Int, False)) $ words $ unlines (take 5 xs))

boards (x : xs) = parseBoard xs : boards (dropWhile (/= "") xs)
boards [] = []

getRow i (Board xs) = map (\j -> xs !! ((i -1) * 5 + j)) [0, 1, 2, 3, 4]

getCol i (Board xs) = map (\j -> xs !! (j + i)) [-1, 4, 9, 14, 19]

-- Adjust words from prelude to split on any character
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

part1 parsedInput = part1' parsedInput 0

part1' (drawnNums, boards) prevCall
  | hasWin boards = sumUnmarked (getWinningBoard boards) * prevCall
  | otherwise = part1' (tail drawnNums, markBoards (head drawnNums) boards) (head drawnNums)

part2 parsedInput = part2' parsedInput 0

part2' (drawnNums, [finalBoard]) prevCall
  | isWinning finalBoard = sumUnmarked finalBoard * prevCall
  | otherwise = part2' (tail drawnNums, markBoards (head drawnNums) [finalBoard]) (head drawnNums)
part2' (drawnNums, boards) prevCall = part2' (tail drawnNums, filter (not . isWinning) (markBoards (head drawnNums) boards)) (head drawnNums)

sumUnmarked :: Board -> Int
sumUnmarked (Board xs) = sum $ map fst $ filter (not . snd) xs

getWinningBoard boards = head (dropWhile (not . isWinning) boards)

hasWin = any isWinning

isWinning board = any (all snd) rows || any (all snd) cols
  where
    rows = map (`getRow` board) [1, 2, 3, 4, 5]
    cols = map (`getCol` board) [1, 2, 3, 4, 5]

markBoards :: Int -> [Board] -> [Board]
markBoards x ((Board xs) : boards) = Board (markInBoard x xs) : markBoards x boards
markBoards _ [] = []

markInBoard x ((i, m) : xs)
  | i == x = (i, True) : xs
  | otherwise = (i, m) : markInBoard x xs
markInBoard _ [] = []