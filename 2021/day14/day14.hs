import qualified Data.Map as Map
import System.Environment
import qualified Data.Bifunctor
import Data.Maybe (mapMaybe)
import Data.List (sort)


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

type Dictionary = Map.Map String String

getInput = fmap (Data.Bifunctor.bimap head parseRules . break (== "") . lines) . readFile

parseRules = Map.fromList . map parseRule . tail

parseRule [x, y, ' ','-','>',' ', z] = ([x, y], [x, z, y])
parseRule _ = error "invalid rule format)"

-- Solution Logic

part1 :: (String, Dictionary) -> Int
part1 = (\x -> mostCommon x - leastCommon x) . (!!10) . uncurry applyRules

mostCommon = maximum . map snd . letterCounts Map.empty

leastCommon = minimum . map snd . letterCounts Map.empty

letterCounts counts (x:xs) = letterCounts (Map.insertWith (+) x 1 counts) xs
letterCounts counts [] = Map.toList counts

applyRules xs dict = xs : applyRules grownPolymer dict
    where
      grownPolymer = head (head triples) : foldl1 (++) (map tail triples)
      triples = mapMaybe (`Map.lookup` dict) (getPairs xs)

getPairs (x:y:xs) = [x,y] : getPairs (y:xs)
getPairs _ = []