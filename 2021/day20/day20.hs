import qualified Data.Bifunctor as Bifunctor
import Data.List
import System.Environment
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

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

data Pixel = On | Off deriving (Eq, Show)

type Point = (Int, Int)
type Image = Map.Map Point Pixel

getInput :: FilePath -> IO ([Pixel], Image)
getInput = fmap (Bifunctor.bimap (parseLights . head) (parseInitMap . tail) . break (== "") . lines) . readFile

parseLights ('#':xs) = On:parseLights xs
parseLights ('.':xs) = Off:parseLights xs
parseLights _ = []

parseInitMap rows = foldl1 Map.union $ zipWith parseMapRow [0..length rows -1] rows

parseMapRow :: Int -> String -> Image
parseMapRow i row = Map.fromList $ [((x, i), y) | (x,y) <- zip [0..length row -1] (parseLights row)]

-- Solution Logic

lookupWithDefault def k map = fromMaybe def (Map.lookup k map)

enhance dimFrom dimTo def decoder image = Map.fromList [enhancePixel x y def image decoder | x <- [dimFrom..dimTo], y <- [dimFrom..dimTo]]

enhancePixel x y def image decoder = ((x,y), decoder !! parseBinary (getWindow x y def image))

parseBinary = foldl (\acc i -> acc * 2 + px2bit i) 0

px2bit px = if px == On then 1 else 0

getWindow x y def image = [lookupWithDefault def (x + i, y + j) image | j <- [- 1, 0, 1], i <- [- 1, 0, 1]]

countOnPixels = Map.foldl (+) 0 . Map.map px2bit

part1 (decoder, image) = countOnPixels $ enhance (-4) 104 On decoder $ enhance (-2) 102 Off decoder image

part2 (decoder, image) = countOnPixels $ enhanceIter 50 0 100 On decoder image

enhanceIter 0 dimFrom dimTo prevDef decoder image = image
enhanceIter iters dimFrom dimTo prevDef decoder image = enhanceIter (iters-1) (dimFrom - 2) (dimTo + 2) (comp prevDef) decoder $ enhance (dimFrom - 2) (dimTo + 2) (comp prevDef) decoder image

comp px = if px == On then Off else On