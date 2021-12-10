import System.Environment
import Data.Char
import Data.Maybe
import Data.List (sort)

-- IO Scaffolding

main = do
    args <- getArgs
    let filename = head args
    p1Solution <- solve part1 filename
    --p2Solution <- solve part2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
    --putStrLn ("Part 2: " ++ show p2Solution)

solve fn filename = do
    input <- getInput filename
    return (fn input)

-- Input Parsing

getInput filename = do
    x <- readFile filename
    return $ lines x

-- Solution Logic

part1 = sum . mapMaybe applyScore . mapMaybe extractCorrupt

extractCorrupt :: String -> Maybe Char
extractCorrupt s = case parse s [] of
    (c:s, _) -> Just c
    _ -> Nothing

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

applyScore :: Char -> Maybe Int
applyScore ')' = Just 3
applyScore ']' = Just 57
applyScore '}' = Just 1197
applyScore '>' = Just 25137
applyScore _ = Nothing