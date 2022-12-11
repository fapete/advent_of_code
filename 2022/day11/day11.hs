import Data.Bifunctor (first, second)
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
  p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

data Monkey = Monkey [Integer] (Integer -> Integer) (Integer -> Int) Int -- List of Items held, Worry level increase function, Throw to which monkey function, inspections

getInput :: FilePath -> IO (M.Map Int Monkey, [Integer])
getInput = fmap (first (M.fromList . zip [0 ..]) . unzip . map parseMonkey . paragraphs) . readFile

paragraphs = splitOn "\n\n"

parseMonkey monkeyLines = (Monkey items operation test 0, divisor)
  where
    ls = tail $ lines monkeyLines
    items = parseItems $ head ls
    operation = parseOperation $ ls !! 1
    (test, divisor) = parseTest $ drop 2 ls

parseItems line = items
  where
    (_, itemNums) = second (drop 2) $ break (== ':') line
    items = map read $ splitOn "," itemNums

parseOperation line
  | head relevant == "+" = (+ (read $ last relevant))
  | head relevant == "*" = multOperation (readMaybe $ last relevant)
  where
    (_, relevant) = second (splitOn " " . drop 2) $ break (== 'd') line
    multOperation (Just i) = (* i)
    multOperation Nothing = \x -> x * x

parseTest lines = (\x -> bool falseTarget trueTarget $ x `mod` divisor == 0, divisor)
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

turn manageWorry i monkeys =
  M.insert i (resetItems monkey) $
    foldl (\agg (item, to) -> M.insert to (receiveItem (agg M.! to) item) agg) monkeys throwTargets
  where
    monkey = monkeys M.! i
    items = getItems monkey
    newWorryLevels = map (manageWorry . applyOperation monkey) items
    throwTargets = zip newWorryLevels $ map (applyTest monkey) newWorryLevels

computeRound manageWorry monkeys = foldl (flip (turn manageWorry)) monkeys $ M.keys monkeys

part1 (monkeys, _) = product $ take 2 $ reverse $ sort $ M.elems $ M.map getInspections $ iterate (computeRound (`div` 3)) monkeys !! 20

part2 (monkeys, divisors) = product $ take 2 $ reverse $ sort $ M.elems $ M.map getInspections $ iterate (computeRound (`mod` product divisors)) monkeys !! 10000