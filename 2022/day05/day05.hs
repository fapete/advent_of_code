import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (catMaybes)
import System.Environment

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

data Command = Move Int Int Int deriving (Show)

getInput = fmap ((\x -> (getInitialStacks $ map (map parseStackChar . chunksOf 4) $ init $ head x, parseCommands $ last x)) . splitOn [""] . lines) . readFile

parseStackChar ('[' : x : ']' : xs) = Just x
parseStackChar x = Nothing

getInitialStacks = map catMaybes . transpose

parseCommands (x : xs) = Move (read (command !! 1)) (read (command !! 3)) (read (command !! 5)) : parseCommands xs
  where
    command = splitOn [' '] x
parseCommands [] = []

-- Solution Logic

applyCommand :: Command -> [[a]] -> [[a]]
applyCommand (Move 0 from to) stacks = stacks
applyCommand (Move num from to) stacks
  | from < to = applyCommand (Move (num - 1) from to)
      (take from' stacks
        ++ (tail fromStack : take (to' - from' - 1) (drop (from' + 1) stacks)
        ++ ((head fromStack : toStack) : drop (to' + 1) stacks)))
  | from > to = applyCommand (Move (num - 1) from to)
      (take to' stacks
        ++ (head fromStack : toStack) : take (from' - to' - 1) (drop (to' + 1) stacks)
        ++ (tail fromStack : drop (from' + 1) stacks))
  | from == to = error "Can't move to same stack"
  where
    fromStack = stacks !! from'
    toStack = stacks !! to'
    from' = from - 1
    to' = to - 1
applyCommand x s = error "Invalid Command"

applyCommand' :: Command -> [[a]] -> [[a]]
applyCommand' (Move num from to) stacks
  | from < to = take from' stacks
        ++ (drop num fromStack : take (to' - from' - 1) (drop (from' + 1) stacks)
        ++ ((take num fromStack ++ toStack) : drop (to' + 1) stacks))
  | from > to = take to' stacks
        ++ (take num fromStack ++ toStack) : take (from' - to' - 1) (drop (to' + 1) stacks)
        ++ (drop num fromStack : drop (from' + 1) stacks)
  | from == to = error "Can't move to same stack"
  where
    fromStack = stacks !! from'
    toStack = stacks !! to'
    from' = from - 1
    to' = to - 1
applyCommand' x s = error "Invalid command"

part1 (stacks, commands) = head <$> foldl (flip applyCommand) stacks commands
part2 (stacks, commands) = head <$> foldl (flip applyCommand') stacks commands