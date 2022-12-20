import System.Environment
import Data.Sequence as Seq
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Maybe (fromJust)

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

getInput :: FilePath -> IO (Seq (Int, Int))
getInput = fmap (fromList . Prelude.zip [0..] . map read . lines) . readFile

-- Solution Logic

mix :: Int -> Int -> Seq (Int, Int) -> Seq (Int, Int)
mix pos until seq
  | pos == until = seq
  | otherwise = mix (pos + 1) until $ (before |> element) >< after
  where
    (curIdx, element@(origIdx, num)) = fromJust $ findWithIndexBy ((==) pos . fst) seq
    newIndex = (curIdx + num) `mod` (Seq.length seq - 1)
    (before, after) = Seq.splitAt newIndex $ Seq.deleteAt curIdx seq

findWithIndexBy = findWithIndexBy' 0

findWithIndexBy' _ _ Seq.Empty = Nothing
findWithIndexBy' idx f (x:<|xs)
  | f x = Just (idx, x)
  | otherwise = findWithIndexBy' (idx+1) f xs

unpackNums = Seq.mapWithIndex (\_ e -> snd e)

turnTo0 seq = i >< t
  where
    zeroIdx = fromJust $ findIndexL ((==) 0 . snd) seq
    (t, i) = Seq.splitAt zeroIdx seq

part1 xs = sum [index mixed (1000 `mod` l), index mixed (2000 `mod` l), index mixed (3000 `mod` l)]
  where
    l = Seq.length xs
    mixed = unpackNums $ turnTo0 $ mix 0 l xs

part2 xs = sum [index mixed (1000 `mod` l), index mixed (2000 `mod` l), index mixed (3000 `mod` l)]
  where
    l = Seq.length xs
    mixed = unpackNums $ turnTo0 $ foldl (\agg _ -> mix 0 l agg) (mapWithIndex (\_ e -> second (* 811589153) e) xs) [1..10]