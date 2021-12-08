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
    ("adf", "7"),
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

makeDict :: [String] -> Map.Map Char [Char]
makeDict xs = Map.empty