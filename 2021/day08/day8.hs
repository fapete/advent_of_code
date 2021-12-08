import System.Environment
import Data.Char
import Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- IO Scaffolding

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

-- Input Parsing

getInput :: FilePath -> IO [([String], [String])]
getInput filename = do
    x <- readFile filename
    return $ map ((\splitLine -> (words $ head splitLine, words $ last splitLine)) . split '|') $ lines x

split c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

part1 = sum . map (countUnique . snd)

countUnique = length . filter ((`Set.member` Set.fromList [2,3,4,7]) . length)

