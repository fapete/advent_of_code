diffs :: Num a => [a] -> [a]
diffs (x:y:xs) = y-x:diffs (y:xs)
diffs x = []

sliding (x:y:z:xs) = x+y+z:sliding (y:z:xs)
sliding _ = []

parseInt = map (\y -> read y :: Integer)

part1 intList = length $ filter (> 0) $ diffs intList

part2 intList = part1 $ sliding intList

getInput = do
    x <- readFile "./input"
    return (parseInt $ lines x)

solve1 = do
    input <- getInput
    return (part1 input)

solve2 = do
    input <- getInput
    return (part2 input)