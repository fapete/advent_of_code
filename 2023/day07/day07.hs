{-# LANGUAGE InstanceSigs #-}

import Data.List
import System.Environment
import Data.Char (isDigit)
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Map as M

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 getInput1 filename
  p2Solution <- solve part2 getInput2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve solver parser = fmap solver . parser

-- Input Parsing

data Card = Joker | Num Int | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
data HandType = HighCard | Pair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

newtype Hand = Hand [Card] deriving (Show, Eq)

type Game = (Hand, Integer)
type CardCounts = M.Map Card Int

getInput1 :: FilePath -> IO [Game]
getInput1 = fmap (map (bimap parseHand (read . drop 1) . break (== ' ')) . lines) . readFile

getInput2 :: FilePath -> IO [Game]
getInput2 = fmap (map (bimap (parseHand . map jackToJoker) (read . drop 1) . break (== ' ')) . lines) . readFile

jackToJoker :: Char -> Char
jackToJoker 'J' = '*'
jackToJoker x = x

parseHand :: String -> Hand
parseHand = Hand . map (\x -> read [x])

instance Read Card where
  readsPrec d [] = []
  readsPrec d (x:xs)
    | isDigit x = (Num $ read [x], xs):readsPrec d xs
    | otherwise = case x of
      'A' -> (Ace, xs):readsPrec d xs
      'K' -> (King, xs):readsPrec d xs
      'Q' -> (Queen, xs):readsPrec d xs
      'J' -> (Jack, xs):readsPrec d xs
      'T' -> (Num 10, xs):readsPrec d xs
      '*' -> (Joker, xs):readsPrec d xs
      _ -> error "No Parse"

-- Solution Logic

isFiveOfAKind :: CardCounts -> Bool
isFiveOfAKind counts = elem 5 (M.elems counts) || hasWithJoker counts
  where
    hasWithJoker counts = M.findWithDefault 0 Joker counts + maximum (M.elems $ countsWithoutJoker counts) == 5

isFourOfAKind :: CardCounts -> Bool
isFourOfAKind counts = not (isFiveOfAKind counts) && (elem 4 (M.elems counts) || hasWithJoker counts)
  where
    hasWithJoker counts = M.findWithDefault 0 Joker counts + maximum (M.elems $ countsWithoutJoker counts) == 4

isFullHouse :: CardCounts -> Bool
isFullHouse counts = not (isFiveOfAKind counts || isFourOfAKind counts) && ([2,3] == sort (M.elems counts) || hasWithJoker counts)
  where
    hasWithJoker counts = M.findWithDefault 0 Joker counts == 1 && [2, 2] == M.elems (countsWithoutJoker counts)

isThreeOfAKind :: CardCounts -> Bool
isThreeOfAKind counts = not (isFiveOfAKind counts || isFourOfAKind counts || isFullHouse counts) && (elem 3 (M.elems counts) || hasWithJoker counts)
  where
    hasWithJoker counts = M.findWithDefault 0 Joker counts + maximum (M.elems $ countsWithoutJoker counts) == 3

isTwoPairs :: CardCounts -> Bool
isTwoPairs counts = not (isFiveOfAKind counts || isFourOfAKind counts || isFullHouse counts || isThreeOfAKind counts) && [1,2,2] == sort (M.elems counts)
-- Can't have two pairs with hands involving jokers. Once a joker is added you always have either one pair or at least three of a kind

isOnePair :: CardCounts -> Bool
isOnePair counts = not (isFiveOfAKind counts || isFourOfAKind counts || isFullHouse counts || isThreeOfAKind counts || isTwoPairs counts)
  && ([1,1,1,2] == sort (M.elems counts) || hasWithJoker counts)
  where
    hasWithJoker counts = M.findWithDefault 0 Joker counts == 1 -- high card with a joker. Every other case is a better hand type.

isHighCard :: CardCounts -> Bool
isHighCard counts = not (
  isOnePair counts ||
  isTwoPairs counts ||
  isFullHouse counts ||
  isThreeOfAKind counts ||
  isFourOfAKind counts ||
  isFiveOfAKind counts
  )

getHandType :: Hand -> HandType
getHandType (Hand h)
  | isFiveOfAKind counts = FiveOfAKind
  | isFourOfAKind counts = FourOfAKind
  | isFullHouse counts = FullHouse
  | isThreeOfAKind counts = ThreeOfAKind
  | isTwoPairs counts = TwoPairs
  | isOnePair counts = Pair
  | otherwise = HighCard
  where
    counts = countCards h

countCards :: [Card] -> CardCounts
countCards = foldr (\card -> M.insertWith (+) card 1) M.empty

countsWithoutJoker :: CardCounts -> CardCounts
countsWithoutJoker = M.delete Joker

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1@(Hand c1) h2@(Hand c2)
    | h1 == h2 = EQ
    | h1Type == h2Type = compare c1 c2
    | otherwise = compare h1Type h2Type
    where
      h1Type = getHandType h1
      h2Type = getHandType h2

part1 = foldl' (\acc (rank, (_, bid)) -> acc + rank * bid) 0 . zip [1..] . sort

part2 = part1