import Data.List
import Data.Maybe (fromJust)
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

data SNAFU = S [Integer] deriving (Show, Eq)

getInput = fmap (map parseLine . lines) . readFile

parseLine = map parseSnafuChar

parseSnafuChar '0' = 0
parseSnafuChar '1' = 1
parseSnafuChar '2' = 2
parseSnafuChar '-' = -1
parseSnafuChar '=' = -2
parseSnafuChar _ = error "Invalid char"

-- Solution Logic

instance Num SNAFU where
  (+) (S xs) (S ys) = S $ add 0 (reverse xs) (reverse ys)
    where
      add carry (x : xs) (y : ys)
        | x + y == -4 = add (-1) xs ys ++ [1]
        | x + y == -3 = add (-1) xs ys ++ [2]
        | x + y == 3 = add 1 xs ys ++ [-2]
        | x + y == 4 = add 1 xs ys ++ [-1]
        | otherwise = add 0 xs ys ++ [x + y]
  (*) = undefined
  abs x
    | signum x == S [-1] = negate x
    | otherwise = x
  signum (S (x : xs))
    | x > 0 = S [1]
    | x == 0 = S [0]
    | x < 0 = S [-1]
  fromInteger = S . decimalToSnafu
  negate (S xs) = S $ map (* (-1)) xs

snafuToDecimal = snafuToDecimal' 0 . reverse

snafuToDecimal' _ [] = 0
snafuToDecimal' exp (x : xs) = 5 ^ exp * x + snafuToDecimal' (exp + 1) xs

decimalToSnafu x = decimalToSnafu' maxExp x
  where
    maxExp = floor $ logBase 5 (fromIntegral x :: Float)

decimalToSnafu' exp _
  | exp < 0 = []
decimalToSnafu' exp 0 = replicate exp 0
decimalToSnafu' 0 1 = [1]
decimalToSnafu' 0 2 = [2]
decimalToSnafu' 0 3 = [1, -2]
decimalToSnafu' 0 4 = [1, -1]
decimalToSnafu' 0 5 = [1, 0]
decimalToSnafu' _ (-2) = [-2]
decimalToSnafu' _ (-1) = [-1]
decimalToSnafu' exp x
  | d == 0 = 0 : decimalToSnafu' (exp - 1) x
  | m == 0 = decimalToSnafu d ++ decimalToSnafu' exp m
  | otherwise = decimalToSnafu d ++ decimalToSnafu' (exp - 1) m
  where
    d = round ((fromIntegral x :: Float) / 5 ^ exp)
    m = x - d * 5 ^ exp

encodeSnafu = map snafuChar

snafuChar 1 = '1'
snafuChar 2 = '2'
snafuChar 0 = '0'
snafuChar (-1) = '-'
snafuChar (-2) = '='
snafuChar _ = error "Incorrect char"

part1 = encodeSnafu . decimalToSnafu . sum . map snafuToDecimal