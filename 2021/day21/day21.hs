import qualified Data.Bifunctor as Bifunctor
import Data.List
import System.Environment
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- IO Scaffolding

main = do
  [p1Start, p2Start, turns] <- fmap (map read) getArgs
  let p1Solution = part1 p1Start p2Start turns
  --p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  --putStrLn ("Part 2: " ++ show p2Solution)

-- No Parsing today

-- Solution Logic

roll player turn = (pos, totalRolls)
    where
        rolls = player * 3 + turn * 6
        totalRolls = rolls + 3
        i = rolls `mod` 100
        pos =  wrongMod10 (i+1 + i+2 + i+3)

score :: Int -> Int -> Int -> Int
score player initPos turn = sum $ tail $ scanl (\acc i -> wrongMod10 $ acc + fst (roll player i)) initPos [0..turn]

wrongMod10 x = if x `mod` 10 == 0 then 10 else x `mod` 10

part1 p1 p2 turns = (scoreP1, scoreP2, rollsTotal)
    where
        scoreP1 = score 0 p1 turns
        scoreP2 = score 1 p2 turns
        rollsTotal = (snd $ roll 0 turns, snd $ roll 1 turns)