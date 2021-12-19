import qualified Data.Bifunctor as Bifunctor
import Data.List
import qualified Data.Matrix as Matrix
import System.Environment

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

type Point = (Int, Int, Int)

getInput = fmap (map (parsePositions . tail) . split "" . lines) . readFile

parsePositions :: [String] -> [Point]
parsePositions = map (makePoint . map read . split ',')

makePoint [x, y, z] = (x, y, z)
makePoint i = error $ "Can't make a point out of " ++ show i

split c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

-- Solution Logic

rotate (x, y, z) (byX, byY, byZ) =
  makePoint $
    Matrix.toList $
      Matrix.multStd
        ( Matrix.fromList 3 3 $
            map
              round
              [ cos byX * cos byY,
                cos byX * sin byY * sin byZ - sin byX * cos byZ,
                cos byX * sin byY * cos byZ + sin byX * sin byZ,
                sin byX * cos byY,
                sin byX * sin byY * sin byZ + cos byX * cos byZ,
                sin byX * sin byY * cos byZ - cos byX * sin byZ,
                - sin byY,
                cos byY * sin byZ,
                cos byY * cos byZ
              ]
        )
        (Matrix.fromList 3 1 [x, y, z])

rotationAngles :: [(Double, Double, Double)]
rotationAngles =
  [ (0, 0, 0),
    (pi / 2, 0, 0),
    (pi, 0, 0),
    (2 * pi / 2, 0, 0),
    (0, 0, pi / 2),
    (0, pi / 2, pi / 2),
    (0, pi, pi / 2),
    (0, 2 * pi / 2, pi / 2),
    (0, 0, pi),
    (pi / 2, 0, pi),
    (pi, 0, pi),
    (2 * pi / 2, 0, pi),
    (0, 0, 2 * pi / 2),
    (0, pi / 2, 2 * pi / 2),
    (0, pi, 2 * pi / 2),
    (0, 2 * pi / 2, 2 * pi / 2),
    (0, pi / 2, 0),
    (0, pi / 2, pi / 2),
    (0, pi / 2, pi),
    (0, pi / 2, 2 * pi / 2),
    (0, 2 * pi / 2, 0),
    (0, 2 * pi / 2, pi / 2),
    (0, 2 * pi / 2, pi),
    (0, 2 * pi / 2, 2 * pi / 2)
  ]

allRotations ps = [(map (`rotate` r) ps, r) | r <- rotationAngles]

dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

dists s0 s1 = [dist p0 p1 | p0 <- s0, p1 <- s1]

intersects s0 s1 = filter ((>= 12) . fst) $ map (Bifunctor.first (maximum . map length . group . sort . dists s0)) (allRotations s1)

allIntersections xs = map (Bifunctor.first (head . map snd)) $ filter (not . null . fst) $ [(intersects (xs !! x) (xs !! y), (x, y)) | x <- [0 .. length xs -1], y <- [(x + 1) .. length xs -1]]

part1 xs = 3