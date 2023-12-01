import Data.List
import System.Environment
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

-- IO Scaffolding

main = do
  args <- getArgs
  let filename = head args
  p1Solution <- solve part1 getInput1 filename
  p2Solution <- solve part1 getInput2 filename
  putStrLn ("Part 1: " ++ show p1Solution)
  putStrLn ("Part 2: " ++ show p2Solution)

solve solver parser = fmap solver . parser

-- Input Parsing
getInput1 = fmap (map parseLine . lines) . readFile
getInput2 = fmap (map (parseLine . replaceNums) . lines) . readFile

parseLine :: String -> [Integer]
parseLine = mapMaybe (readMaybe . (: [])) 

replaceNums :: String -> String
replaceNums "" = ""
replaceNums l@(c:s)
  | length (replaceNum l) < length l = replaceNums (replaceNum l)
  | otherwise = c : replaceNums s

replaceNum :: String -> String
replaceNum ('o':'n':'e':s) = '1':'e':s
replaceNum ('t':'w':'o':s) = '2':'o':s
replaceNum ('t':'h':'r':'e':'e':s) = '3':'e':s
replaceNum ('f':'o':'u':'r':s) = '4':s
replaceNum ('f':'i':'v':'e':s) = '5':'e':s
replaceNum ('s':'i':'x':s) = '6':s
replaceNum ('s':'e':'v':'e':'n':s) = '7':'n':s
replaceNum ('e':'i':'g':'h':'t':s) = '8':'t':s
replaceNum ('n':'i':'n':'e':s) = '9':'e':s
replaceNum s = s

-- Solution Logic

part1 = sum . map (\x -> head x * 10 + last x)