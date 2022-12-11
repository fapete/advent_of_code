import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as M
import System.Environment
import Text.Read (readMaybe)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  -- p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)

-- putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

data Monkey = Monkey [Int] (Int -> Int) (Int -> Int) Int -- List of Items held, Worry level increase function, Throw to which monkey function, inspections

getInput = fmap (M.fromList . zip [0 ..] . map parseMonkey . paragraphs) . readFile

paragraphs = splitOn "\n\n"

parseMonkey monkeyLines = Monkey items operation test 0
  where
    ls = tail $ lines monkeyLines
    items = parseItems $ head ls
    operation = parseOperation $ ls !! 1
    test = parseTest $ drop 2 ls

parseItems :: String -> [Int]
parseItems line = items
  where
    (_, itemNums) = second (drop 2) $ break (== ':') line
    items = map read $ splitOn "," itemNums

parseOperation :: String -> (Int -> Int)
parseOperation line
  | head relevant == "+" = (+ (read $ last relevant))
  | head relevant == "*" = multOperation (readMaybe $ last relevant)
  where
    (_, relevant) = second (splitOn " " . drop 2) $ break (== 'd') line
    multOperation (Just i) = (* i)
    multOperation Nothing = \x -> x * x

parseTest :: [String] -> (Int -> Int)
parseTest lines = \x -> bool falseTarget trueTarget $ x `mod` divisor == 0
  where
    divisor = read $ last $ splitOn " " $ head lines
    trueTarget = read $ last $ splitOn " " $ lines !! 1
    falseTarget = read $ last $ splitOn " " $ lines !! 2

-- Solution Logic

getItems (Monkey items _ _ _) = items

applyOperation (Monkey _ operation _ _) = operation

applyTest (Monkey _ _ test _) = test

getInspections (Monkey _ _ _ inspections) = inspections

receiveItem (Monkey items operation test inspections) item = Monkey (items ++ [item]) operation test inspections

resetItems (Monkey items operation test inspections) = Monkey [] operation test (inspections + length items)

turn :: Int -> M.Map Int Monkey -> M.Map Int Monkey
turn i monkeys =
  M.insert i (resetItems monkey) $
    foldl (\agg (item, to) -> M.insert to (receiveItem (agg M.! to) item) agg) monkeys throwTargets
  where
    monkey = monkeys M.! i
    items = getItems monkey
    newWorryLevels = map (\item -> applyOperation monkey item `div` 3) items
    throwTargets = zip newWorryLevels $ map (applyTest monkey) newWorryLevels

computeRound :: M.Map Int Monkey -> M.Map Int Monkey
computeRound monkeys = foldl (flip turn) monkeys $ M.keys monkeys

part1 monkeys = product $ take 2 $ reverse $ sort $ M.elems $ M.map getInspections $ iterate computeRound monkeys !! 20