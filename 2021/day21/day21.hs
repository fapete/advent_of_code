import qualified Data.Bifunctor as Bifunctor
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment

-- IO Scaffolding

main = do
  [p1Start, p2Start] <- fmap (map read) getArgs
  let p2Solution = part2 p1Start p2Start
  putStrLn ("Part 2: " ++ show p2Solution)

-- No Parsing today

-- Solution Logic

roll player turn = (pos, totalRolls)
  where
    rolls = player * 3 + turn * 6
    totalRolls = rolls + 3
    i = rolls `mod` 100
    pos = wrongMod10 (i + 1 + i + 2 + i + 3)

score :: Int -> Int -> Int -> Int
score player initPos turn = sum $ tail $ scanl (\acc i -> wrongMod10 $ acc + fst (roll player i)) initPos [0 .. turn]

wrongMod10 x = if x `mod` 10 == 0 then 10 else x `mod` 10

addScore x y
  | x + y >= 21 = 21
  | otherwise = x + y

part1 p1 p2 turns = (scoreP1, scoreP2, rollsTotal)
  where
    scoreP1 = score 0 p1 turns
    scoreP2 = score 1 p2 turns
    rollsTotal = (snd $ roll 0 turns, snd $ roll 1 turns)

states = [(x, y) | x <- [0 .. 21], y <- [1 .. 10]] -- Possible game states: (score, position)

initial = zip [0, 0 ..] [0, 0 ..] -- Number of gamese in state for Player 1, Player 2

initMap p1pos p2pos =
  ( Map.insertWith (+) (0, p1pos) 1 $ Map.fromList $ zip states [0, 0 ..],
    Map.insertWith (+) (0, p2pos) 1 $ Map.fromList $ zip states [0, 0 ..]
  )

rolls = map (\x -> (fromIntegral $ length x, head x)) $ group $ sort [x + y + z | x <- [1, 2, 3], y <- [1, 2, 3], z <- [1, 2, 3]]

computeOneMove p 21 f = [((21, p), f)]
computeOneMove fromPos fromScore fromFreq =
  map
    ( \(count, r) ->
        ( ( addScore fromScore $ newPos r,
            newPos r
          ),
          fromFreq * count
        )
    )
    rolls
  where
    newPos r = wrongMod10 (fromPos + r)

oneMoveFromEveryState =
  fst
    . Map.mapAccumWithKey
      ( \acc (score, pos) freq ->
          ( Map.unionWith (+) acc $
              Map.fromList $
                computeOneMove pos score freq,
            freq
          )
      )
      Map.empty

wins p1 p2 = sumWinning p1 * sumNotWinning p2

sumWinning m = sum [fromMaybe 0 $ Map.lookup (21, x) m | x <- [1 .. 10]]

sumNotWinning m = sum [fromMaybe 0 $ Map.lookup (y, x) m | x <- [1 .. 10], y <- [0 .. 20]]

filterWinning = Map.mapWithKey (\(score, pos) f -> if score < 21 then f else 0)

hasOpenGames = Map.foldl (\acc freq -> acc || freq > 0) False

iter (p1, p2) (p1Wins, p2Wins)
  | hasOpenGames p1 || hasOpenGames p2 = iter (p1'', p2'') (p1Wins + p1NewWins, p2Wins + p2NewWins)
  | otherwise = (p1Wins, p2Wins)
  where
    (p1', p2') = (oneMoveFromEveryState p1, oneMoveFromEveryState p2)
    p1NewWins = wins p1' p2'
    p2NewWins = wins p2' p1'
    (p1'', p2'') = (filterWinning p1', filterWinning p2')

part2 p1Pos p2Pos = iter (initMap p1Pos p2Pos) (0, 0)

addTuple x = Bifunctor.bimap (fst x +) (snd x +)