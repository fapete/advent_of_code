import System.Environment
import Data.Char
import Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- IO Scaffolding

main = do
    args <- getArgs
    let filename = head args
    p1Solution <- solve part1 filename
    p2Solution <- solve part2 filename
    putStrLn ("Part 1: " ++ show p1Solution)
    putStrLn ("Part 2: " ++ show p2Solution)

solve fn filename = do
    input <- getInput filename
    return (fn input)

-- Input Parsing

getInput :: FilePath -> IO [([String], [String])]
getInput filename = do
    x <- readFile filename
    return $ map ((\splitLine -> (words $ head splitLine, words $ last splitLine)) . split '|') $ lines x

split c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

part1 = sum . map (countUnique . snd)

countUnique = length . filter ((`Set.member` Set.fromList [2,3,4,7]) . length)

part2 :: [([String], [String])] -> Int
part2 = sum . map (\(wires, display) -> decode (makeDict wires) display)

canonicalWires = Map.fromList [
    ("abcefg", "0"),
    ("cf", "1"),
    ("acdeg", "2"),
    ("acdfg", "3"),
    ("bcdf", "4"),
    ("abdfg", "5"),
    ("abdefg", "6"),
    ("acf", "7"),
    ("abcdefg", "8"),
    ("abcdfg", "9")
    ]

unsafeMaybe (Just i) = i
unsafeMaybe Nothing = error "Tried to unpack Nothing"

decode :: Map.Map Char String -> [String] -> Int
decode dict = read . foldl1 (++) . map (decodeSingle dict)

decodeSingle dict = unsafeMaybe
    . (`Map.lookup` canonicalWires)
    . sort
    . foldl1 (++)
    . map (unsafeMaybe . flip Map.lookup dict)

makeDict :: [String] -> Map.Map Char String
makeDict xs = Map.fromList [
    (head segmentA, "a"),
    (head segmentB, "b"),
    (head segmentC, "c"),
    (head segmentD, "d"),
    (head segmentE, "e"),
    (head segmentF, "f"),
    (head segmentG, "g")
    ]
    where
        one = head $ filter ((== 2) . length) xs
        four = head $ filter ((== 4) . length) xs
        seven = head $ filter ((== 3) . length) xs
        eight = head $ filter ((== 7) . length) xs
        segmentA = codeMinus seven one
        segmentG = head $ filter ((== 1) . length) $ map (`codeMinus` codePlus four seven) $ filter ((== 6) . length) xs
        nine = codePlus four $ codePlus seven segmentG
        segmentE = codeMinus eight nine
        segmentF = head $ filter ((== 1) . length) $ map (`codeMinus` codePlus (codeMinus (codePlus four seven) one) segmentG ) $ filter ((== 5) . length) xs
        five = codePlus (codeMinus (codePlus four seven) one) (codePlus segmentG segmentF)
        segmentB = head $ filter ((== 1) . length) $ map (`codeMinus` codePlus (codePlus (codeMinus eight five) seven) segmentG ) $ filter ((== 6) . length) xs
        zero = codePlus (codeMinus eight five) (codePlus seven (codePlus segmentG segmentB))
        segmentD = codeMinus four zero
        six = codePlus five segmentE
        segmentC = codeMinus eight six



codeMinus x y = Set.elems $ Set.difference (Set.fromList x) (Set.fromList y)
codePlus x y = Set.elems $ Set.union (Set.fromList x) (Set.fromList y)