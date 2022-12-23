import Data.List
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Bool (bool)

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

type Pos = (Int, Int) -- row, col 
type Elves = S.Set Pos
type ProposedMoves = M.Map Pos [Pos] -- Moves to key proposed by value
type ProposedTargets = M.Map Pos Pos

getInput :: FilePath -> IO Elves
getInput = fmap (S.unions . zipWith parseGridLine [0..] . lines) . readFile

parseGridLine :: Int -> String -> Elves
parseGridLine rowIdx line = foldr S.insert S.empty $ mapMaybe (\(c, colIdx) -> bool Nothing (Just (rowIdx, colIdx)) (c == '#')) $ zip line [0..]

-- Solution Logic

data Direction = N | S | W | E deriving (Show, Eq)

considerOrder = [N, S, W, E]

addPos (x,y) (dx, dy) = (x+dx, y+dy)

considerDirs :: Direction -> [Pos]
considerDirs N = [(-1, -1), (-1, 0), (-1, 1)]
considerDirs S = [(1, -1), (1, 0), (1, 1)]
considerDirs W = [(-1, -1), (0, -1), (1, -1)]
considerDirs E = [(-1, 1), (0, 1), (1, 1)]

moveDir :: Direction -> Pos
moveDir d = considerDirs d !! 1

hasNeighborsInDir :: Direction -> Pos -> S.Set (Int, Int) -> Bool
hasNeighborsInDir dir pos elves = any ((`S.member` elves) . addPos pos) (considerDirs dir)

hasAnyNeighbours pos elves = any (\dir -> hasNeighborsInDir dir pos elves) [N, S, W, E]

proposeMoves :: [Direction] -> Elves -> (ProposedMoves, ProposedTargets)
proposeMoves consideredDirections elves = foldl (\(moves, targets) elf -> (updateProposedMoves moves elf, updateProposedTargets targets elf)) (M.empty, M.empty) $ S.elems elves
  where
    directionToMove elf = find (\dir -> not $ hasNeighborsInDir dir elf elves) consideredDirections
    proposedMove elf dir = case dir of
      Just d -> bool Nothing (Just $ addPos elf $ moveDir d) $ hasAnyNeighbours elf elves
      Nothing -> Nothing
    addToProposed map elf (Just pos) = M.insertWith (++) pos [elf] map
    addToProposed map elf Nothing = map
    addToTargets map elf (Just pos) = M.insert elf pos map
    addToTargets map elf Nothing = map
    updateProposedMoves moves elf = addToProposed moves elf $ proposedMove elf $ directionToMove elf 
    updateProposedTargets targets elf = addToTargets targets elf $ proposedMove elf $ directionToMove elf 

moveElves :: Elves -> (ProposedMoves, ProposedTargets) -> Elves
moveElves elves (moves, targets) = S.map (\elf -> if canMove elf then movePos elf else elf ) elves
  where
    canMove elf = case M.lookup elf targets of
      (Just pos) -> case M.lookup pos moves of
        (Just [_]) -> True
        (Just xs) -> False
        Nothing -> False
      Nothing -> False
    movePos elf = targets M.! elf

containingRect elves = ((minx, miny), (maxx, maxy))
  where
    minx = fst $ minimumBy (\e e' -> compare (fst e) (fst e')) elves
    miny = snd $ minimumBy (\e e' -> compare (snd e) (snd e')) elves
    maxx = fst $ maximumBy (\e e' -> compare (fst e) (fst e')) elves
    maxy = snd $ maximumBy (\e e' -> compare (snd e) (snd e')) elves

moveStep elves dirs = moveElves elves $ proposeMoves dirs elves

moveNSteps 0 elves _ = elves
moveNSteps n elves dirs = moveNSteps (n-1) (moveStep elves $ take 4 dirs) $ drop 1 dirs

findFixPoint n elves dirs
  | elves == newElves = n
  | otherwise = findFixPoint (n+1) newElves $ drop 1 dirs
  where
    newElves = moveStep elves $ take 4 dirs

part1 :: Elves -> Int
part1 elves = availableSpaces - length elfPositions
  where
    elfPositions = moveNSteps 10 elves $ cycle considerOrder 
    ((x,y), (x',y')) = containingRect elfPositions
    availableSpaces = (abs (x-x')+1) * (abs (y-y')+1)

part2 elves = findFixPoint 1 elves $ cycle considerOrder