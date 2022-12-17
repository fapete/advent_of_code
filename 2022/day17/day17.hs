import Data.Bifunctor (bimap, first)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Set qualified as S
import System.Environment

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  -- p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)

-- putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

data Shape = HorLine | Cross | MirrorL | VertLine | Block deriving (Show, Eq)

type Grid = S.Set Pos

type Pos = (Int, Int)

data Command = R | L deriving (Show)

getInput = fmap (map parseCommand . init) . readFile

parseCommand '>' = R
parseCommand '<' = L
parseCommand x = error ("Nah " ++ [x])

-- Solution Logic

shapes = cycle [HorLine, Cross, MirrorL, VertLine, Block]

dropShape :: Shape -> Pos -> Int -> Grid -> [Command] -> Maybe (Grid, [Command], Int)
dropShape _ _ _ _ [] = Nothing
dropShape s p oldMax grid (c : cs)
  | isNothing fallPos = Just (settleShape s leftRightPos grid, cs, newMax)
  | otherwise = dropShape s (fromJust fallPos) oldMax grid cs
  where
    leftRightPos = applyCommand c s p grid
    fallPos = fallOne s leftRightPos grid
    newMax = max oldMax $ maximum $ map snd $ occupiedSpaces s leftRightPos

applyCommand :: Command -> Shape -> Pos -> Grid -> Pos
applyCommand c s p gh
  | canMoveInDir c s p gh = move c p
  | otherwise = p

canMoveInDir L s p grid = canMoveLeft s p grid
canMoveInDir R s p grid = canMoveRight s p grid

move R (c, h) = (c + 1, h)
move L (c, h) = (c - 1, h)

fallOne :: Shape -> Pos -> Grid -> Maybe Pos
fallOne s (c, h) grid
  | canMoveDown s (c, h) grid = Just (c, h - 1)
  | otherwise = Nothing

canMoveDown s p grid = not $ any (`S.member` grid) $ positionsOnMove (0, -1) $ occupiedSpaces s p

canMoveLeft s p grid = inBounds spaceAfterMove && not (any (`S.member` grid) spaceAfterMove)
  where
    spaceAfterMove = positionsOnMove (-1, 0) $ occupiedSpaces s p

canMoveRight s p grid = inBounds spaceAfterMove && not (any (`S.member` grid) spaceAfterMove)
  where
    spaceAfterMove = positionsOnMove (1, 0) $ occupiedSpaces s p

inBounds = all (\(c, _) -> c >= 1 && c <= 7)

positionsOnMove (dx, dy) ps = uncurry zip $ bimap (map (+ dx)) (map (+ dy)) $ unzip ps

occupiedSpaces :: Shape -> Pos -> [Pos]
occupiedSpaces HorLine (c, h) = [(c', h) | c' <- [c .. (c + 3)]]
occupiedSpaces MirrorL (c, h) = [(c', h) | c' <- [c .. (c + 2)]] ++ [(c + 2, h + 1), (c + 2, h + 2)]
occupiedSpaces VertLine (c, h) = [(c, h') | h' <- [h .. (h + 3)]]
occupiedSpaces Block (c, h) = [(c, h), (c + 1, h), (c, h + 1), (c + 1, h + 1)]
occupiedSpaces Cross (c, h) = [(c + 1, h), (c, h + 1), (c + 1, h + 1), (c + 2, h + 1), (c + 1, h + 2)]

settleShape s pos grid = foldl (flip S.insert) grid $ occupiedSpaces s pos

initialGrid = S.fromList [(i, 0) | i <- [1 .. 7]]

third (_, _, i) = i

part1 xs = third . foldl (\(grid, commands, newMax) shape -> fromJust $ dropShape shape (3, newMax + 4) newMax grid commands) (initialGrid, cycle xs, 0) $ take 2022 shapes