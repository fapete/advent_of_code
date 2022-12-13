{-# LANGUAGE InstanceSigs #-}
import Data.List
import System.Environment
import Data.Text (replace)
import Data.List.Split (splitOn)
import Data.Bifunctor (second, bimap)
import Data.Char (isDigit)
import Data.Bool (bool)
import Util (unzipWith)

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

data Packet = L [Packet] | I Int deriving (Show, Read, Eq)

getInput :: FilePath -> IO [(Packet, Packet)]
getInput = fmap (map (bimap (read . insertConstructors) (read . insertConstructors) . splitPackets) . paragraphs) . readFile

paragraphs = splitOn "\n\n"

splitPackets = second tail . break (== '\n')

insertConstructors [] = []
insertConstructors ('[':x:line)
  | isDigit x = 'L':' ':'[':'I':' ':insertConstructors (x:line)
  | otherwise = 'L':' ':'[':insertConstructors (x:line)
insertConstructors (',':x:line) 
  | isDigit x = ',':'I':' ':insertConstructors (x:line)
  | otherwise = ',':insertConstructors (x:line)
insertConstructors (x:line) = x:insertConstructors line

-- Solution Logic
instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (L []) (L []) = EQ
  compare (L []) (L p) = LT
  compare (L p) (L []) = GT
  compare (L p) (L p')
    | head p == head p' = compare (L $ tail p) (L $ tail p')
    | otherwise = compare (head p) (head p')
  compare (L p) (I i) = compare (L p) (L [I i])
  compare (I i) (L p) = compare (L [I i]) (L p) 
  compare (I i) (I i') = compare i i'

part1 = sum . zipWith (*) [1..] . map (bool 0 1 . uncurry (<))