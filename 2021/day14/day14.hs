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
  p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

getInput = fmap (Data.Bifunctor.bimap (pairCounts . head) parseRules . break (== "") . lines) . readFile

parseRules = Map.fromList . map parseRule . tail

parseRule [x, y, ' ','-','>',' ', z] = ([x, y], ([x,z],[z, y]))
parseRule _ = error "invalid rule format)"

-- Solution Logic

part1 = (`div` 2) . (\x -> mostCommon x - leastCommon x) . (!!10) . uncurry applyRules

part2 = (`div` 2) . (\x -> mostCommon x - leastCommon x) . (!!40) . uncurry applyRules

mostCommon = maximum . map snd . letterCounts Map.empty

leastCommon = minimum . map snd . letterCounts Map.empty

letterCounts :: Map.Map String Int -> Map.Map String Int -> [(String, Int)]
letterCounts letterCounts pairCounts = Map.toList 
  $ foldl1 (Map.unionWith (+)) 
  $ map (\(pair, count) -> Map.insertWith (+) [head pair] count (Map.insertWith (+) [last pair] count letterCounts)) 
  $ Map.assocs pairCounts

pairCounts xs = foldl1 (Map.unionWith (+)) [Map.singleton p 1 | p <- getPairs xs]

applyRules counts dict = counts : applyRules (foldl1 (Map.unionWith (+)) $ getNewCounts (Map.assocs counts) dict) dict

getNewCounts pairs dict = [Map.fromList [(fst (unsafeMaybe $ Map.lookup rule dict), count), (snd (unsafeMaybe $ Map.lookup rule dict), count)] | (rule, count) <- pairs ]

unsafeMaybe (Just i) = i
unsafeMaybe Nothing = error "Tried to unpack Nothing"

getPairs (x:y:xs) = [x,y] : getPairs (y:xs)
getPairs _ = []