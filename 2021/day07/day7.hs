import System.Environment
import Data.Char
import Data.List (sort)

-- IO Scaffolding

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

-- Input Parsing

getInput filename = do
    x <- readFile filename
    return $ sort $ map read $ split ',' x :: IO [Int]

split c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

-- Solution logic

part1 input = sum $ map (distance midPoint) input
    where midPoint = input !! (length input `div` 2)

distance x y = abs (x - y)