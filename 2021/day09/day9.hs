{-# LANGUAGE ViewPatterns #-}
import System.Environment
import Data.Char
import Data.List (sort)
import qualified Data.Set as Set

-- IO Scaffolding

main = do
    args <- getArgs
    let filename = head args
    p1Solution <- solve part1 filename
    p2Solution <- solve part2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
    putStrLn ("Part 2: " ++ show p2Solution)

solve :: ([[Int]] -> Int) -> FilePath -> IO Int
solve fn filename = do
    input <- getInput filename
    return (fn input)

-- Input Parsing

getInput :: FilePath -> IO [[Int]]
getInput filename = do
    x <- readFile filename
    let lineLength = length (head $ lines x) + 2
    return $ [take lineLength [9,9..]] ++ map (padInputLine . map (read . (:[]))) (lines x) ++ [take lineLength [9,9..]]

padInputLine xs = [9] ++ xs ++ [9]

-- Solution Logic

part1 input = sum $ map (+1) $ [getPoint input p | p <- findLowPoints input]

part2 input = product $ take 3 $ reverse $ sort $ getBasinSizes input $ findLowPoints input

getBasinSizes xs = map (Set.size . findBasin xs)

findBasin input (x, y) = findBasin' input (Set.singleton (x,y))
findBasin' :: [[Int]] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
findBasin' input visited = if Set.null newPoints then visited else findBasin' input (Set.union newPoints visited)
    where newPoints = newlyAdjacentPoints input visited

newlyAdjacentPoints input visited = Set.filter (\point -> Set.notMember point visited && getPoint input point /= 9) $  foldl1 Set.union $ Set.map (getAdjacent input) visited

getAdjacent :: [[Int]] -> (Int, Int) -> Set.Set (Int, Int)
getAdjacent input (x,y) = Set.fromList [(x, y-1), (x,y+1), (x-1, y), (x+1, y)]

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