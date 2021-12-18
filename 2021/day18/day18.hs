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

data SnailNum = SN SnailNum SnailNum | Lit Int deriving (Eq, Show, Read)

parse c
  | c == '[' = "(SN "
  | c == ']' = ")"
  | c == ',' = " "
  | otherwise = "(Lit " ++ c : ")"

getInput :: FilePath -> IO [SnailNum]
getInput = fmap (map (read . (parse =<<)) . lines) . readFile

-- Solution Logic

part1 = magnitude . foldl1 snailAdd

part2 nums = maximum $ [magnitude $ snailAdd x y | x <- nums, y <- nums, x /= y]

magnitude :: SnailNum -> Int
magnitude (SN x y) = 3 * magnitude x + 2 * magnitude y
magnitude (Lit i) = i

snailAdd :: SnailNum -> SnailNum -> SnailNum
snailAdd x y = reduce (SN x y)

reduce :: SnailNum -> SnailNum
reduce n
  | depth n == 4 = reduce $ explode n
  | needsSplit n = reduce $ split n
  | otherwise = n

depth (SN x y) = 1 + max (depth x) (depth y)
depth (Lit i) = -1

needsSplit (SN x y) = needsSplit x || needsSplit y
needsSplit (Lit i) = i >= 10

split (SN x y) = if needsSplit x then SN (split x) y else SN x (split y)
split (Lit i) = SN (Lit (floor (fromIntegral i / 2))) (Lit (ceiling (fromIntegral i / 2)))

explode :: SnailNum -> SnailNum
explode n = addAt r right $ addAt l left $ explodeAt explodePath n
  where
    (explodePath, (l, r)) = getExplodeInfo n
    left = getPathToNextLeft n explodePath
    right = getPathToNextRight n explodePath

addAt i (Just ('L' : p)) (SN x y) = SN (addAt i (Just p) x) y
addAt i (Just ('R' : p)) (SN x y) = SN x (addAt i (Just p) y)
addAt i (Just "") (Lit j) = Lit (i + j)
addAt _ Nothing sn = sn
addAt _ p sn = error $ "Invalid Path to add: " ++ show p ++ " in " ++ show sn

explodeAt ('L' : p) (SN x y) = SN (explodeAt p x) y
explodeAt ('R' : p) (SN x y) = SN x (explodeAt p y)
explodeAt "" (SN (Lit i) (Lit j)) = Lit 0
explodeAt _ _ = error "Invalid Path to explode"

getExplodeInfo (SN (Lit i) (Lit j)) = ([], (i, j))
getExplodeInfo (SN x y)
  | depth x >= depth y = let (p, ij) = getExplodeInfo x in ('L' : p, ij)
  | otherwise = let (p, ij) = getExplodeInfo y in ('R' : p, ij)
getExplodeInfo (Lit _) = error "Can't explode single literal"

getPathToNextLeft sn p
  | all (== 'L') p = Nothing
  | otherwise = Just (descendRightFrom (init (reverse $ dropWhile (== 'L') $ reverse p) ++ "L") sn)

getPathToNextRight sn p
  | all (== 'R') p = Nothing
  | otherwise = Just (descendLeftFrom (init (reverse $ dropWhile (== 'R') $ reverse p) ++ "R") sn)

descendRightFrom ('L' : p) (SN x _) = 'L' : descendRightFrom p x
descendRightFrom ('R' : p) (SN _ y) = 'R' : descendRightFrom p y
descendRightFrom "" (SN _ y) = 'R' : descendRightFrom "" y
descendRightFrom "" (Lit _) = []
descendRightFrom _ _ = error "Invalid path"

descendLeftFrom ('L' : p) (SN x _) = 'L' : descendLeftFrom p x
descendLeftFrom ('R' : p) (SN _ y) = 'R' : descendLeftFrom p y
descendLeftFrom "" (SN x _) = 'L' : descendLeftFrom "" x
descendLeftFrom "" (Lit _) = []
descendLeftFrom _ _ = error "Invalid path"