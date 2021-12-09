import System.Environment
import Data.Char

-- IO Scaffolding

main = do
    args <- getArgs
    let filename = head args
    p1Solution <- solve part1 filename
    --p2Solution <- solve part2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
    --putStrLn ("Part 2: " ++ show p2Solution)

solve :: ([[Int]] -> Int) -> FilePath -> IO Int
solve fn filename = do
    input <- getInput filename
    return (fn input)

-- Input Parsing

getInput :: FilePath -> IO [[Int]]
getInput filename = do
    x <- readFile filename
    let lineLength = length (head $ lines x) + 2
    return $ [take lineLength [10,10..]] ++ map (padInputLine . map (read . (:[]))) (lines x) ++ [take lineLength [10,10..]]

padInputLine xs = [10] ++ xs ++ [10]

-- Solution Logic

part1 input = sum $ map (+1) $ [getPoint input p | p <- findLowPoints input]

findLowPoints xs = filter (isLow xs) ([(x,y) | x <- [1..rows], y <- [1..cols]])
    where rows = length xs - 2
          cols = length (head xs) - 2

isLow xs (x, y) = p < a && p < l && p < r && p < b
    where p = getPoint xs (x, y)
          a = getPoint xs (x, y-1)
          b = getPoint xs (x, y+1)
          r = getPoint xs (x+1, y)
          l = getPoint xs (x-1, y)

getPoint xs (x, y) = (xs!!x)!!y