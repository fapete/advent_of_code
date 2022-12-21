import Data.List
import System.Environment
import Data.Bifunctor (second)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.Maybe (isJust)

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

getInput = fmap (M.unions . map parseMonkey . lines) . readFile

data Operation = Num Int | Add String String | Sub String String | Mul String String | Div String String

parseMonkey line = M.singleton monkeyname $ parseOperation operation
  where
    (monkeyname, operation) = second (drop 2) $ break (== ':') line

parseOperation :: String -> Operation
parseOperation op
  | isJust (readMaybe op :: Maybe Int) = Num $ read op
  | otherwise = case splitOn " " op of
      [n1, "+", n2] -> Add n1 n2
      [n1, "-", n2] -> Sub n1 n2
      [n1, "*", n2] -> Mul n1 n2
      [n1, "/", n2] -> Div n1 n2
      other -> error ("Invalid operation " ++ show op)

-- Solution Logic

computeResult (Num i) monkeys = i
computeResult (Add n n') monkeys = computeResult (monkeys M.! n) monkeys + computeResult (monkeys M.! n') monkeys
computeResult (Sub n n') monkeys = computeResult (monkeys M.! n) monkeys - computeResult (monkeys M.! n') monkeys
computeResult (Mul n n') monkeys = computeResult (monkeys M.! n) monkeys * computeResult (monkeys M.! n') monkeys
computeResult (Div n n') monkeys = computeResult (monkeys M.! n) monkeys `div` computeResult (monkeys M.! n') monkeys

part1 monkeys = computeResult (monkeys M.! "root") monkeys