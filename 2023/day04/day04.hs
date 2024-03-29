{-# LANGUAGE TupleSections #-}

import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M
import System.Environment
import Data.Bifunctor (Bifunctor(bimap, second))

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
type Scratchcard = (S.Set Integer, S.Set Integer)

getInput1 = fmap (map parseLine . lines) . readFile

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

parseLine :: String -> Scratchcard
parseLine line = (S.fromList $ map read winning, S.fromList $ map read scratched)
  where
    numbers = drop 2 $ dropWhile (/= ':') line
    (winning, scratched) = both (filter (/= "") . splitOn " ") $ second (drop 2) $ break (== '|') numbers


-- Solution Logic

cardPoints :: Floating a => Scratchcard -> a
cardPoints card
  | numWins card == 0 = 0
  | otherwise = 2**(fromIntegral (numWins card) - 1)

initCopies :: [Scratchcard] -> [(Scratchcard, Integer)]
initCopies = map (, 1)

numWins :: Scratchcard -> Int
numWins (winning, scratched) = S.size (S.intersection winning scratched)

makeCopies :: [(Scratchcard, Integer)] -> [(Scratchcard, Integer)]
makeCopies [] = []
makeCopies ((card, copies):cards) = (card, copies): makeCopies (newCopies ++ remainingCards)
  where
    copyCards = take (numWins card) cards
    newCopies = map (second (+copies)) copyCards
    remainingCards = drop (numWins card) cards


part1 = sum . map cardPoints

part2 = sum . map snd . makeCopies . initCopies