import Data.List
import System.Environment
import qualified Data.Map as M

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

type Node = String
type Map = M.Map Node (Node, Node)
type Instruction = (Node, Node) -> Node

getInput1 = fmap ((\ls -> (parseInstructions $ head ls, M.fromList $ map parseNode $ drop 2 ls)) . lines) . readFile

getInput2 = getInput1

parseInstructions :: String -> [Instruction]
parseInstructions "" = []
parseInstructions ('R':remaining) = snd:parseInstructions remaining
parseInstructions ('L':remaining) = fst:parseInstructions remaining
parseInstructions _ = error "Invalid input"

parseNode :: String -> (Node, (Node, Node))
parseNode (a:b:c:' ':'=':' ':'(':d:e:f:',':' ':x:y:z:')':_) = ([a,b,c], ([d,e,f], [x,y,z]))
parseNode _ = error "Not a node description"

-- Solution Logic

follow :: (Node, [Instruction], Map) -> (Node, [Instruction], Map)
follow (start, instructions, targets) = (head instructions $ targets M.! start, tail instructions, targets)

ghostInitialNodes :: Map -> [Node]
ghostInitialNodes targets = filter ((==) 'A' . last) $ M.keys targets

cycleLength :: (Node, [Instruction], Map) -> Int
cycleLength = length . takeWhile (\(node, _, _) -> last node /= 'Z') . iterate follow

part1 (instructions, targets) = length 
  $ takeWhile (\(node, _, _) -> node /= "ZZZ") 
  $ iterate follow ("AAA", cycle instructions, targets)

part2 (instructions, targets) = foldl1 lcm $ map (\node -> cycleLength (node, cycle instructions, targets)) $ ghostInitialNodes targets