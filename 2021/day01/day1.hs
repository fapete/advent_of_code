import System.Environment

diffs :: Num a => [a] -> [a]
diffs (x:y:xs) = y-x:diffs (y:xs)
diffs x = []

sliding (x:y:z:xs) = x+y+z:sliding (y:z:xs)
sliding _ = []

parseInt = map (\y -> read y :: Integer)

part1 intList = length $ filter (> 0) $ diffs intList

part2 intList = part1 $ sliding intList

getInput filename = do
    x <- readFile filename
    return (parseInt $ lines x)

solve1 filename = do
    input <- getInput filename
    return (part1 input)

solve2 filename = do
    input <- getInput filename
    return (part2 input)

main = do
    args <- getArgs 
    let filename = head args
    p1Solution <- solve1 filename
    p2Solution <- solve2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
    putStrLn ("Part 2: " ++ show p2Solution)