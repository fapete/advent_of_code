import System.Environment
import Data.Char

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
    return $ lines x

part1 nums = parseBinary g * parseBinary e
    where g = binGamma nums
          e = complement g

binGamma binNums = map (\onesInCol -> if onesInCol > length binNums `div` 2 then '1' else '0') $ countOnesPerCol binNums

parseBinary = foldl (\acc i -> acc * 2 + digitToInt i) 0

complement = map (\x -> if x == '0' then '1' else '0')

countOnesPerCol binNums = foldl (zipWith (+)) [0,0..] (map (map digitToInt) binNums)

part2 nums = parseBinary oxygen * parseBinary co2Scrubber
    where oxygen = getRating oxygenFilter nums
          co2Scrubber = getRating co2Filter nums

nxor x y = x && y || not x && not y

oxygenFilter binNums col = filter (\num -> (num!!col == '1') `nxor` (oneCount >= zeroCount)) binNums
    where oneCount = countOnesPerCol binNums !! col
          zeroCount = length binNums - oneCount

co2Filter binNums col = filter (\num -> (num!!col == '0') `nxor` (oneCount >= zeroCount)) binNums
    where oneCount = countOnesPerCol binNums !! col
          zeroCount = length binNums - oneCount

getRating filterBy binNums = head $ head $ dropWhile (\xs -> length xs /= 1) (scanl filterBy binNums [0..])