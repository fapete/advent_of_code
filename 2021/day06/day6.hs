import System.Environment
import Data.Char
import Data.List (sort, group)

main = do
    args <- getArgs
    let filename = head args
    let days = read $ head $ tail args
    p1Solution <- solve (part1 days) filename
    --p2Solution <- solve part2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
    --putStrLn ("Part 2: " ++ show p2Solution)

solve fn filename = do
    input <- getInput filename
    return (fn input)

getInput filename = do
    x <- readFile filename
    return $ sort $ map read $ split ',' x :: IO [Int]

split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

part1 i = sum . simulate i . countInitialPhases 0

countInitialPhases :: Int -> [Int] -> [Int]
countInitialPhases 9 input = []
countInitialPhases i input = length (takeWhile (== i) input) : countInitialPhases (i+1) (dropWhile (== i) input)

simulate :: Int -> [Int] -> [Int]
simulate 0 counts = counts 
simulate days [d0, d1, d2, d3, d4, d5, d6, d7, d8] = simulate (days -1) [d1, d2, d3, d4, d5, d6, d7 + d0, d8, d0]
simulate _ _ = error "Too many or too few fish phases"