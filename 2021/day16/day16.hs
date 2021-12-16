import System.Environment
import Data.Char

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

getInput = fmap parse . readFile

hex2bin :: String -> [Integer]
hex2bin ('0':xs) = [0,0,0,0] ++ hex2bin xs
hex2bin ('1':xs) = [0,0,0,1] ++ hex2bin xs
hex2bin ('2':xs) = [0,0,1,0] ++ hex2bin xs
hex2bin ('3':xs) = [0,0,1,1] ++ hex2bin xs
hex2bin ('4':xs) = [0,1,0,0] ++ hex2bin xs
hex2bin ('5':xs) = [0,1,0,1] ++ hex2bin xs
hex2bin ('6':xs) = [0,1,1,0] ++ hex2bin xs
hex2bin ('7':xs) = [0,1,1,1] ++ hex2bin xs
hex2bin ('8':xs) = [1,0,0,0] ++ hex2bin xs
hex2bin ('9':xs) = [1,0,0,1] ++ hex2bin xs
hex2bin ('A':xs) = [1,0,1,0] ++ hex2bin xs
hex2bin ('B':xs) = [1,0,1,1] ++ hex2bin xs
hex2bin ('C':xs) = [1,1,0,0] ++ hex2bin xs
hex2bin ('D':xs) = [1,1,0,1] ++ hex2bin xs
hex2bin ('E':xs) = [1,1,1,0] ++ hex2bin xs
hex2bin ('F':xs) = [1,1,1,1] ++ hex2bin xs
hex2bin _ = []

parseBinary :: [Integer] -> Integer
parseBinary = foldl (\acc i -> acc * 2 + i) 0

data Header = Header Integer Integer deriving (Show) -- version typeid
data Packet = Literal Header Integer | Operator Header [Packet] deriving (Show)

parseHeader xs = (Header version typeid, remaining')
    where
        (version, remaining) = parseVersion xs
        (typeid, remaining') = parseTypeId remaining

parseVersion :: [Integer] -> (Integer, [Integer])
parseVersion xs = (parseBinary $ take 3 xs, drop 3 xs)

parseTypeId xs = (parseBinary $ take 3 xs, drop 3 xs)

parseLiteral header xs = (Just (Literal header parsed), remaining)
    where
        (parsed, remaining) = (parseBinary $ takeNWhile 5 ((== 1) . head) xs, dropNWhile 5 ((== 1) . head) xs)

takeNWhile n predicate xs = tail (take n xs) ++ if predicate xs then takeNWhile n predicate (drop n xs) else []

dropNWhile n predicate xs = if predicate xs then dropNWhile n predicate (drop n xs) else drop n xs

parseOperator header (0:xs) = (Just (Operator header parsedPackets), remaining)
        where
            (parsedPackets, remaining) = (parsePackets $ take (fromIntegral subPacketBitLength) subPacketsRemaining, drop (fromIntegral subPacketBitLength) subPacketsRemaining)
            subPacketBitLength = parseBinary $ take 15 xs
            subPacketsRemaining = drop 15 xs

parseOperator header (1:xs) = (Just (Operator header parsedPackets), remaining)
        where
            parsedPackets = parseNPackets (fromIntegral numSubPackets) subPacketsRemaining
            remaining = dropNPackets (fromIntegral numSubPackets) subPacketsRemaining
            numSubPackets = parseBinary $ take 11 xs
            subPacketsRemaining = drop 11 xs

parseOperator header _ = (Nothing, [])

parsePackets [] = []
parsePackets xs = case parsePacket xs of
    (Just packet, remaining) -> packet : parsePackets remaining
    (Nothing, _) -> []

parseNPackets :: Int -> [Integer] -> [Packet]
parseNPackets 0 xs = []
parseNPackets n xs = packet : parseNPackets (n-1) remaining
    where
        (Just packet, remaining) = parsePacket xs

dropNPackets :: Int -> [Integer] -> [Integer]
dropNPackets 0 xs = xs
dropNPackets n xs = dropNPackets (n-1) remaining
    where
        (_, remaining) = parsePacket xs

parsePacket xs = case parseHeader xs of
    (header@(Header version 4), remaining) -> parseLiteral header remaining
    (header, remaining) -> parseOperator header remaining

parse = parsePackets . hex2bin

-- Solution Logic

versionSum (Operator (Header version _) packets) = version + sum (map versionSum packets)
versionSum (Literal (Header version _) _) = version

part1 = sum . map versionSum

eval (Literal _ i) = i
eval (Operator (Header _ typeid) packets)
    | typeid == 0 = sum recursion
    | typeid == 1 = product recursion
    | typeid == 2 = minimum recursion
    | typeid == 3 = maximum recursion
    | typeid == 5 = if head recursion > last recursion then 1 else 0
    | typeid == 6 = if head recursion < last recursion then 1 else 0
    | typeid == 7 = if head recursion == last recursion then 1 else 0
    | otherwise = error $ "invalid type ID: " ++ show typeid
    where
        recursion = map eval packets

part2 = eval . head