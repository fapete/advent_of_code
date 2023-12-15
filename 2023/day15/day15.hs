import Data.List
import System.Environment
import qualified Data.IntMap as M
import Data.List.Split (splitOn)
import Data.Char (ord)

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

getInput1 = fmap (map (filter (/= '\n')) . splitOn ",") . readFile

data Lens = Lens String Int deriving (Show)
data Operation = Remove Lens | Add Lens deriving (Show)
type HASHMap = M.IntMap [Lens]

getInput2 = fmap (map (parseOperation "" . filter (/= '\n')) . splitOn ",") . readFile

parseOperation :: String -> String -> Operation
parseOperation label ('=':xs) = Add $ Lens (reverse label) (read xs)
parseOperation label ('-':xs) = Remove $ Lens (reverse label) 0
parseOperation label (x:xs) = parseOperation (x:label) xs
parseOperation label "" = error "State not possible"

-- Solution Logic

hash :: String -> Int
hash = foldl' (\cur c -> (cur + ord c) * 17 `mod` 256) 0

lensLabel :: Lens -> String
lensLabel (Lens label _) = label

lensFocalLength :: Lens -> Int
lensFocalLength (Lens _ f) = f

lensHash :: Lens -> Int
lensHash = hash . lensLabel

part1 = sum . map hash

instance Eq Lens where
  (==) (Lens label1 _) (Lens label2 _) = label1 == label2

initHashMap :: HASHMap
initHashMap = M.fromList $ zip [0..] (replicate 256 [])

insertLens :: Lens -> HASHMap -> HASHMap
insertLens l m = M.insert hash (insertIntoBucket l lensBucket) m
  where
    hash = lensHash l
    lensBucket = m M.! hash

insertIntoBucket :: Lens -> [Lens] -> [Lens]
insertIntoBucket l ls = before ++ newAfter
  where
    (before, after) = break (== l) ls
    newAfter = if null after then [l] else l:tail after

removeLens :: Lens -> HASHMap -> HASHMap
removeLens l m = M.insert hash (removeFromBucket l lensBucket) m
  where
    hash = lensHash l
    lensBucket = m M.! hash

removeFromBucket :: Lens -> [Lens] -> [Lens]
removeFromBucket l ls = before ++ newAfter
  where
    (before, after) = break (== l) ls
    newAfter = if null after then [] else tail after

focusingPower :: Int -> Int -> Lens -> Int
focusingPower boxNr bucketNr l = (boxNr + 1) * bucketNr * lensFocalLength l

focusingPowers :: Int -> [Lens] -> Int
focusingPowers boxNr = sum . zipWith (focusingPower boxNr) [1..]

applyOperations :: [Operation] -> HASHMap
applyOperations = foldl' (flip applyOperation) initHashMap

applyOperation :: Operation -> HASHMap -> HASHMap
applyOperation (Add lens) = insertLens lens
applyOperation (Remove lens) = removeLens lens

part2 = sum . map (uncurry focusingPowers) . M.toList . applyOperations