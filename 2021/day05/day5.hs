{-# LANGUAGE TupleSections #-}

import Data.Char
import qualified Data.Map as Map
import System.Environment

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
  return $ map (parseLine . words) $ lines x

type Point = (Int, Int)

type Line = (Point, Point)

split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

parseLine :: [String] -> Line
parseLine [p1, "->", p2] = (makePoint p1, makePoint p2)
parseLine _ = error "Unexpected Input format"

makePoint :: String -> Point
makePoint s = case map (\x -> read x :: Int) $ split ',' s of
  [x, y] -> (x, y)
  _ -> error "Points should have format 'Int,Int'"

part1 :: [Line] -> Int
part1 parsedInput = Map.size $ Map.filter (> 1) $ foldl (flip addLineToMap) Map.empty (filter isHorVert parsedInput)

part2 parsedInput = Map.size $ Map.filter (> 1) $ foldl (flip addLineToMap) Map.empty parsedInput

isHorVert ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

pointsOnLine ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) [min y1 y2 .. max y1 y2]
  | y1 == y2 = map (,y1) [min x1 x2 .. max x1 x2]
  | x1 < x2 && y1 < y2 = zip [x1 .. x2] [y1 .. y2]
  | x1 < x2 && y1 > y2 = zip [x1 .. x2] [y1, (y1 -1) .. y2]
  | x1 > x2 && y1 < y2 = zip [x1, (x1 -1) .. x2] [y1 .. y2]
  | x1 > x2 && y1 > y2 = zip [x1, (x1 -1) .. x2] [y1, (y1 -1) .. y2]
  | otherwise = error "Impossible state"

addLineToMap line map = foldl (\acc p -> Map.insertWith (+) p 1 acc) map (pointsOnLine line)