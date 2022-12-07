import Data.List
import System.Environment
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe

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

data FSTree = Dir String [FSTree] | File String Int deriving (Show) -- Dir: Name Children Parent, File: Name Size

data Command = CD String | LS [Contents] deriving (Show)

data Contents = CDir String | CFile String Int deriving (Show, Eq, Ord)

getInput = fmap (buildFSTreeFromMapRoot . (\commands -> buildDirTreeMap commands M.empty []) . parseCommands . lines) . readFile

parseCommands [] = []
parseCommands xs = command : parseCommands remaining
  where
    (command, remaining) = parseCommand xs

parseCommand xs
  | isCd $ head xs = (CD $ last tokens, tail xs)
  | otherwise = (LS $ parseContents (takeWhile isContentLine $ tail xs), dropWhile isContentLine $ tail xs)
    where
      tokens = splitOn " " $ head xs
      isContentLine l = head l /= '$'

isCd line = splitOn " " line !! 1 == "cd"

parseContents = sort . map parseContentLine
  where
    parseContentLine xs
      | isDigit $ head xs = CFile fileName $ read fileSize
      | otherwise = CDir fileName
        where
          fileName = last $ splitOn " " xs
          fileSize = head $ splitOn " " xs

buildDirTreeMap :: [Command] -> M.Map [String] [Contents] -> [String] -> M.Map [String] [Contents]
buildDirTreeMap [] dirs _ = dirs
buildDirTreeMap (CD "..":commands) dirs dirStack = buildDirTreeMap commands dirs $ tail dirStack
buildDirTreeMap (CD name:commands) dirs dirStack = buildDirTreeMap commands dirs $ name:dirStack
buildDirTreeMap (LS contents:commands) dirs dirStack = buildDirTreeMap commands (M.insert dirStack contents dirs) dirStack

buildFSTreeFromMapRoot map = Dir "/" $ buildFSTreeFromMap map ["/"]

buildFSTreeFromMap :: M.Map [String] [Contents] -> [String] -> [FSTree]
buildFSTreeFromMap map dirStack = treeFromContents (map M.! dirStack)
  where
    treeFromContents [] = []
    treeFromContents ((CDir name):contents) = Dir name (buildFSTreeFromMap map (name:dirStack)) : treeFromContents contents
    treeFromContents ((CFile name size):contents) = File name size : treeFromContents contents

-- Solution Logic

computeDirSizes :: FSTree -> [Int]
computeDirSizes (Dir _ children) = filter (>0) $ sum (map computeSize children):concatMap computeDirSizes children
computeDirSizes (File _ s) = [0]

computeSize (File _ s) = s
computeSize dir = head $ computeDirSizes dir

--part1 :: [Command] -> Int
part1 = sum . filter (<= 100000) . computeDirSizes

part2 fsTree = head $ dropWhile (< requiredSize) $ sort dirSizes
  where
    dirSizes = computeDirSizes fsTree
    availableSize = 70000000 - head dirSizes
    requiredSize = 30000000 - availableSize