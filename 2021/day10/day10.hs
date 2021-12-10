import System.Environment
import Data.Char
import Data.Maybe
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

getInput = fmap (map (`parse` []) . lines) . readFile

parse (c:s) stack
    | isOpening c = parse s (c:stack)
    | null stack = (c:s, stack) -- c is not opening, but stack is empty -> parse fails, corrupt
    | matches (head stack) c = parse s (tail stack)
    | otherwise = (c:s, stack) -- c doesn't match bracket on stack -> parse fails, corrupt
parse "" stack = ("", stack) -- Parse completes, but is incomplete (if stack not empty)

isOpening c = c == '(' || c == '[' || c == '{' || c == '<'

matches o c
    | o == '(' = c == ')'
    | o == '[' = c == ']'
    | o == '{' = c == '}'
    | o == '<' = c == '>'
    | otherwise = error $ "First parameter " ++ [c] ++ " is not opening"

-- Solution Logic

part1 = sum . mapMaybe applyScore . extractCorrupt

part2 = (\xs -> xs!!(length xs `div` 2)) . sort . mapMaybe (applyScore2 0) . extractIncomplete

extractCorrupt = map (head . fst) . filter ((/= "") . fst)

extractIncomplete = map snd . filter ((== "") . fst)

applyScore :: Char -> Maybe Int
applyScore ')' = Just 3
applyScore ']' = Just 57
applyScore '}' = Just 1197
applyScore '>' = Just 25137
applyScore _ = Nothing

applyScore2 :: Int -> [Char] -> Maybe Int
applyScore2 i ('(':s) = applyScore2 (i*5+1) s
applyScore2 i ('[':s) = applyScore2 (i*5+2) s
applyScore2 i ('{':s) = applyScore2 (i*5+3) s
applyScore2 i ('<':s) = applyScore2 (i*5+4) s
applyScore2 i "" = Just i
applyScore2 i _ = Nothing