import Data.List
import System.Environment
import Data.Bifunctor (second)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromMaybe)

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

getInput = fmap (M.unions . map parseMonkey . lines) . readFile

data Operation = Num Int | Add Operation Operation | Sub Operation Operation | Mul Operation Operation | Div Operation Operation | Var String | Eq Operation Operation deriving (Show)

parseMonkey line = M.singleton monkeyname $ parseOperation operation
  where
    (monkeyname, operation) = second (drop 2) $ break (== ':') line

parseOperation :: String -> Operation
parseOperation op
  | isJust (readMaybe op :: Maybe Int) = Num $ read op
  | otherwise = case splitOn " " op of
      [n1, "+", n2] -> Add (Var n1) (Var n2)
      [n1, "-", n2] -> Sub (Var n1) (Var n2)
      [n1, "*", n2] -> Mul (Var n1) (Var n2)
      [n1, "/", n2] -> Div (Var n1) (Var n2)
      other -> error ("Invalid operation " ++ show op)

-- Solution Logic

computeResult (Num i) = Just i
computeResult (Add n n') = opMaybe (+) (computeResult n) (computeResult n')
computeResult (Sub n n') = opMaybe (-) (computeResult n) (computeResult n')
computeResult (Mul n n') = opMaybe (*) (computeResult n) (computeResult n')
computeResult (Div n n') = opMaybe div (computeResult n) (computeResult n')
computeResult (Eq n n') = computeResult $ getVal n n'
computeResult (Var s) = Nothing

opMaybe fn (Just x) (Just y) = Just $ fn x y
opMaybe fn _ _ = Nothing

makeParseTree (Num i) monkeyMap = Num i
makeParseTree (Add n n') monkeyMap = Add (makeParseTree n monkeyMap) (makeParseTree n' monkeyMap)
makeParseTree (Sub n n') monkeyMap = Sub (makeParseTree n monkeyMap) (makeParseTree n' monkeyMap)
makeParseTree (Mul n n') monkeyMap = Mul (makeParseTree n monkeyMap) (makeParseTree n' monkeyMap)
makeParseTree (Div n n') monkeyMap = Div (makeParseTree n monkeyMap) (makeParseTree n' monkeyMap)
makeParseTree (Eq n n') monkeyMap = Eq (makeParseTree n monkeyMap) (makeParseTree n' monkeyMap)
makeParseTree (Var s) monkeyMap = maybe (Var s) (`makeParseTree` monkeyMap) (M.lookup s monkeyMap)

reduce (Num i) = Num i
reduce (Var s) = Var s
reduce (Eq n n') = Eq (reduce n) (reduce n')
reduce op = case computeResult op of
  Just i -> Num i
  Nothing -> case op of
    (Add n n') -> Add (reduce n) (reduce n')
    (Sub n n') -> Sub (reduce n) (reduce n')
    (Mul n n') -> Mul (reduce n) (reduce n')
    (Div n n') -> Div (reduce n) (reduce n')
    other -> error "impossible state"

part1 monkeys = computeResult $ makeParseTree (monkeys M.! "root") monkeys

adjustMonkeys monkeyMap = M.insert "root" (Eq left right) $ M.delete "humn" monkeyMap
  where
    (left, right) = case monkeyMap M.! "root" of 
      (Add n n') -> (n, n')
      (Sub n n') -> (n, n')
      (Mul n n') -> (n, n')
      (Div n n') -> (n, n')
      other -> error "Invalid"

solveForVar y (Var _) = Num y
solveForVar y (Add op (Num i)) = solveForVar (y-i) op
solveForVar y (Sub op (Num i)) = solveForVar (y+i) op
solveForVar y (Mul op (Num i)) = solveForVar (y `div` i) op
solveForVar y (Div op (Num i)) = solveForVar (y * i) op
solveForVar y (Add (Num i) op) = solveForVar (y - i) op
solveForVar y (Sub (Num i) op) = solveForVar (-y + i) op
solveForVar y (Mul (Num i) op) = solveForVar (y `div` i) op
solveForVar y (Div (Num i) op) = solveForVar (i `div` y) op
solveForVar x y = error $ "solveForVar: invalid input" ++ show x ++ "; " ++ show y

getVal op (Num y) = solveForVar y op
getVal (Num y) op = solveForVar y op
getVal op op' = error $ "getVal: invalid input" ++ show op ++ ", " ++ show op'

part2 monkeys = computeResult $ reduce $ makeParseTree (adjustedMonkeys M.! "root") adjustedMonkeys
  where
    adjustedMonkeys = adjustMonkeys monkeys