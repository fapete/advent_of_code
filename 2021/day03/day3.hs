import System.Environment
import Data.Char

main = do
    args <- getArgs
    let filename = head args
    p1Solution <- solve part1 filename
    --p2Solution <- solve part2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
    --putStrLn ("Part 2: " ++ show p2Solution)

solve fn filename = do
    input <- getInput filename
    return (fn input)

getInput filename = do
    x <- readFile filename
    return $ lines x

part1 nums = parseBinary g * parseBinary e
    where g = binGamma nums
          e = complement g

binGamma binNums = map (\onesInCol -> if onesInCol > length binNums `div` 2 then '1' else '0') $ countOnesPerCol binNums

parseBinary s = foldl (\acc (i, n) -> digitToInt i * 2^n + acc) 0 (zip s [length s - 1, length s - 2..0])

complement = map (\x -> if x == '0' then '1' else '0')

countOnesPerCol binNums = foldl (zipWith (+)) [0,0..] (map (map digitToInt) binNums)