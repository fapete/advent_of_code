import System.Environment
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, catMaybes)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  --p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  --putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing


data Shape = Rock | Paper | Scissors deriving (Show)
data Outcome = Win | Draw | Lose deriving (Show)

getInput = fmap (map (map parseShape . splitOn " ") . lines) . readFile

parseShape "A" = parseShape "X"
parseShape "B" = parseShape "Y"
parseShape "C" = parseShape "Z"
parseShape "X" = Just Rock
parseShape "Y" = Just Paper
parseShape "Z" = Just Scissors
parseShape _ = Nothing

makeIntegers :: [[String]] -> [[Integer]]
makeIntegers = map (map read)

-- Solution Logic

gameOutcome Rock Paper = Win
gameOutcome Paper Scissors = Win
gameOutcome Scissors Rock = Win
gameOutcome Paper Rock = Lose
gameOutcome Rock Scissors = Lose
gameOutcome Scissors Paper = Lose
gameOutcome Rock Rock = Draw
gameOutcome Paper Paper = Draw
gameOutcome Scissors Scissors = Draw

shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

score playedShape outcome = shapeScore playedShape + outcomeScore outcome

scoreRound [Just p1Shape, Just p2Shape] = Just (score p2Shape $ gameOutcome p1Shape p2Shape)
scoreRound _ = Nothing

part1 = sum . mapMaybe scoreRound