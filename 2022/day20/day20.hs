import System.Environment
import Data.Sequence as Seq
import Data.Bifunctor (second)
import Data.Bool (bool)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 filename
  --p2Solution <- solve part2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  --putStrLn ("Part 2: " ++ show p2Solution)

solve fn = fmap fn . getInput

-- Input Parsing

getInput :: FilePath -> IO (Seq (Int, Int))
getInput = fmap (fromList . Prelude.zip [0..] . map read . lines) . readFile

-- Solution Logic

mix :: Int -> Int -> Seq (Int, Int) -> Seq (Int, Int)
mix pos until seq
  | pos == until = seq
  | origIdx /= pos = mix pos until turnCW
  | origIdx == pos = mix (pos + 1) until $ (before |> element) >< after
  | otherwise = error "impossible state"
  where
    turnCCW = (\(f, t) -> t >< f) $ Seq.splitAt 1 seq
    turnCW = (\(i, l) -> l >< i) $ Seq.splitAt (Seq.length seq - 1) seq
    element@(origIdx, num) = index seq pos
    newIndex = (pos + num) `mod` (Seq.length seq - 1)
    (before, after) = Seq.splitAt newIndex $ Seq.deleteAt pos seq

unpackNums = Seq.mapWithIndex (\_ e -> snd e)

turnTo0 seq@((i, num):<|xs)
  | num == 0 = seq
  | otherwise = turnTo0 (xs |> (i,num))
turnTo0 xs = xs

part1 xs = sum [index mixed (1000 `mod` l), index mixed (2000 `mod` l), index mixed (3000 `mod` l)]
  where
    l = Seq.length xs
    mixed = unpackNums $ turnTo0 $ mix 0 l xs