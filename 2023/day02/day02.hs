import Data.Bifunctor (Bifunctor (bimap))
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import System.Environment

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 getInput1 filename
  p2Solution <- solve part2 getInput1 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve solver parser = fmap solver . parser

-- Input Parsing
data Game = Game Integer Integer Integer deriving (Show) -- Red, Green, Blue

type Games = M.Map Integer [Game]

getInput1 = fmap (M.fromList . map parseGameLine . lines) . readFile

parseGameLine :: String -> (Integer, [Game])
parseGameLine ('G' : 'a' : 'm' : 'e' : ' ' : s) = (gameIndex, games)
  where
    (gameIndex, games) = bimap read (parseGames . drop 2) (break (== ':') s)

parseGames :: String -> [Game]
parseGames s = map (parseGame . trimLeft) (splitOn ";" s)

parseGame :: String -> Game
parseGame s = Game red green blue
  where
    red = maybe 0 (\idx -> read (cubes !! (idx - 1))) (elemIndex "red" cubes)
    green = maybe 0 (\idx -> read (cubes !! (idx - 1))) (elemIndex "green" cubes)
    blue = maybe 0 (\idx -> read (cubes !! (idx - 1))) (elemIndex "blue" cubes)
    cubes = concatMap (splitOn " ") $ splitOn "," s

trimLeft :: String -> String
trimLeft = dropWhile (== ' ')

-- Solution Logic

isPossibleGame :: Game -> Bool
isPossibleGame (Game red green blue) = red < 13 && green < 14 && blue < 15

maxCubes :: [Game] -> (Integer, Integer, Integer)
maxCubes games = maxCubes' games (0, 0, 0)

maxCubes' :: [Game] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
maxCubes' [] maxs = maxs
maxCubes' ((Game red green blue) : games) (curRed, curGreen, curBlue) = maxCubes' games (newRed, newGreen, newBlue)
  where
    newRed = max red curRed
    newGreen = max green curGreen
    newBlue = max blue curBlue

power :: (Integer, Integer, Integer) -> Integer
power (x, y, z) = x * y * z

part1 :: Games -> Integer
part1 = sum . M.keys . M.filter (all isPossibleGame)

part2 :: Games -> Integer
part2 = sum . M.map (power . maxCubes)