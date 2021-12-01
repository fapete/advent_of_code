diffs :: Num a => [a] -> [a]
diffs (x:y:xs) = y-x:diffs (y:xs)
diffs x = []

parseInt = map (\y -> read y :: Integer)

main = do
    x <- readFile "./input"
    let intList = parseInt $ lines x
    let diff = diffs intList
    return (length (filter (> 0) diff))