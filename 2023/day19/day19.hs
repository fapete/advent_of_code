import Data.List
import qualified Data.Map as M
import System.Environment
import Data.Bool (bool)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.Split (splitOn)
import Data.Maybe (isJust, mapMaybe)

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

data Part = Part {
  x :: Int,
  m :: Int,
  a :: Int,
  s :: Int
} deriving (Show, Read)

type Rule = (Part -> Maybe (Either String Bool))

type Workflow = [Rule]

type Workflows = M.Map String Workflow

getInput1 = fmap (bimap (M.fromList . map parseWorkflow) (map parsePart . drop 1) . break (== "") . lines) . readFile

isComparisonRule :: String -> Bool
isComparisonRule (x:y:xs) = y == '<' || y == '>'
isComparisonRule _ = False

parseRule :: String -> Rule
parseRule rule
  | isComparisonRule rule = parseComparisonRule rule
  | otherwise = \_ -> Just $ parseRuleTarget rule

parseComparisonRule :: String -> Rule
parseComparisonRule (n:c:xs) = case n of
  'x' -> bool Nothing (Just result) . (`cmp` cmpVal) . x
  'm' -> bool Nothing (Just result) . (`cmp` cmpVal) . m
  'a' -> bool Nothing (Just result) . (`cmp` cmpVal) . a
  's' -> bool Nothing (Just result) . (`cmp` cmpVal) . s
  _ -> error "Not a valid comparison rule"
  where
    cmp = if c == '<' then (<) else (>)
    result = parseRuleTarget ruleTarget
    (cmpVal, ruleTarget) = bimap read (drop 1) $ break (== ':') xs
parseComparisonRule _ = error "Not a valid comparison rule"

parseRuleTarget :: String -> Either String Bool
parseRuleTarget "A" = Right True
parseRuleTarget "R" = Right False
parseRuleTarget target = Left target

parseWorkflow :: String -> (String, Workflow)
parseWorkflow wf = (name, map parseRule rules)
  where 
    name = takeWhile (/= '{') wf
    rulesString = init $ drop 1 $ dropWhile (/= '{') wf
    rules = splitOn "," rulesString

parsePart :: String -> Part
parsePart p = read ("Part " ++ p)

getInput2 = getInput1

-- Solution Logic

applyWorkflowsToPart :: Workflows -> Part -> Bool
applyWorkflowsToPart wfs p = go "in" p
  where
    go workflow part = case applyRules (wfs M.! workflow) part of
      Left s -> go s p
      Right b -> b

applyRules :: [Rule] -> Part -> Either String Bool
applyRules rs p = head $ mapMaybe (\r -> r p) rs

scorePart :: Part -> Int
scorePart Part { x = x, s = s, m = m, a = a} = x + m + s + a

part1 (wfs, parts) = sum $ map scorePart $ filter (applyWorkflowsToPart wfs) parts

part2 = part1