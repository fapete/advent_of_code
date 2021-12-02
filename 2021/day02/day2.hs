import System.Environment

main = do
    args <- getArgs
    let filename = head args
    p1Solution <- solve part1 filename
   -- p2Solution <- solve part2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
   -- putStrLn ("Part 2: " ++ show p2Solution)

solve fn filename = do
    input <- getInput filename
    return (fn input)

getInput filename = do
    x <- readFile filename
    return (map parseInputLine (lines x))

data Command = F Int | D Int | U Int

parseInputLine line = case head parts of
    "forward" -> F (read $ last parts :: Int)
    "down" -> D (read $ last parts :: Int)
    "up" -> U (read $ last parts :: Int)
    _ -> error "Unknown Command"
    where parts = words line

part1 commands = finalPosition (moveSub commands (0,0))

finalPosition (pos, depth) = pos * depth

moveSub commands state = foldl (flip moveSubStep) state commands

moveSubStep (F i) (pos, depth) = (pos + i, depth)
moveSubStep (U i) (pos, depth) = (pos, depth - i)
moveSubStep (D i) (pos, depth) = (pos, depth + i)