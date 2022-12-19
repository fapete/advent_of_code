import Data.Bool (bool)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import System.Environment
import Text.Read (readMaybe)

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

getInput :: FilePath -> IO [[Int]]
getInput = fmap (map (mapMaybe readMaybe . splitOn " ") . lines) . readFile

-- Solution Logic

type Resources = (Int, Int, Int, Int)

type Robots = (Int, Int, Int, Int)

buildOreBot :: [Int] -> Resources -> Robots -> (Resources, Robots)
buildOreBot blueprint resources robots =
  ( decrease 1 (blueprint !! 0) resources,
    increase 1 1 robots
  )

canBuildOreBot :: [Int] -> Resources -> Bool
canBuildOreBot blueprint (ore, _, _, _) = ore >= blueprint !! 0

shouldBuildOreBot :: [Int] -> Robots -> Bool
shouldBuildOreBot blueprint (ore, _, _, _) = ore <= maximum [blueprint !! 0, blueprint !! 1, blueprint !! 2, blueprint !! 4]

buildClayBot :: [Int] -> Resources -> Robots -> (Resources, Robots)
buildClayBot blueprint resources robots =
  ( decrease 1 (blueprint !! 1) resources,
    increase 2 1 robots
  )

canBuildClayBot :: [Int] -> Resources -> Bool
canBuildClayBot blueprint (ore, _, _, _) = ore >= blueprint !! 1

shouldBuildClayBot :: [Int] -> Robots -> Bool
shouldBuildClayBot blueprint (_, clay, _, _) = clay <= blueprint !! 3

buildObsidianBot :: [Int] -> Resources -> Robots -> (Resources, Robots)
buildObsidianBot blueprint resources robots =
  ( decrease 2 (blueprint !! 3) $ decrease 1 (blueprint !! 2) resources,
    increase 3 1 robots
  )

canBuildObsidianBot blueprint (ore, clay, _, _) = ore >= blueprint !! 2 && clay >= blueprint !! 3

shouldBuildObsidianBot :: [Int] -> Robots -> Bool
shouldBuildObsidianBot blueprint (_, _, obsidian, _) = obsidian <= blueprint !! 5

buildGeodeBot :: [Int] -> Resources -> Robots -> (Resources, Robots)
buildGeodeBot blueprint resources robots =
  ( decrease 3 (blueprint !! 5) $ decrease 1 (blueprint !! 4) resources,
    increase 4 1 robots
  )

canBuildGeodeBot blueprint (ore, _, obsidian, _) = ore >= blueprint !! 4 && obsidian >= blueprint !! 5

shouldBuildGeodeBot _ _ = True

score (_, _, _, geode) = geode

decrease i by (r1, r2, r3, r4)
  | i == 1 = (r1 - by, r2, r3, r4)
  | i == 2 = (r1, r2 - by, r3, r4)
  | i == 3 = (r1, r2, r3 - by, r4)
  | i == 4 = (r1, r2, r3, r4 - by)
  | otherwise = error "invalid"

increase i by (r1, r2, r3, r4)
  | i == 1 = (r1 + by, r2, r3, r4)
  | i == 2 = (r1, r2 + by, r3, r4)
  | i == 3 = (r1, r2, r3 + by, r4)
  | i == 4 = (r1, r2, r3, r4 + by)
  | otherwise = error "invalid"

get i (r1, r2, r3, r4)
  | i == 1 = r1
  | i == 2 = r2
  | i == 3 = r3
  | i == 4 = r4
  | otherwise = error "invalid"

robotActions resources robots = foldl (\agg idx -> increase idx (get idx robots) agg) resources [1 .. 4]

minute i max blueprint resources robots
  | i == max = score resources
  | otherwise = maximum $ catMaybes buildGeodeIfPossible
  where
    collectedResources = robotActions resources robots
    (oreResources, newOreBot) = buildOreBot blueprint collectedResources robots
    (clayResources, newClayBot) = buildClayBot blueprint collectedResources robots
    (obsidianResources, newObsidianBot) = buildObsidianBot blueprint collectedResources robots
    (geodeResources, newGeodeBot) = buildGeodeBot blueprint collectedResources robots
    tryBuildingAll =
      [ bool Nothing (Just $ minute (i + 1) max blueprint oreResources newOreBot) $ canBuildOreBot blueprint resources && shouldBuildOreBot blueprint robots,
        bool Nothing (Just $ minute (i + 1) max blueprint clayResources newClayBot) $ canBuildClayBot blueprint resources && shouldBuildClayBot blueprint robots,
        bool Nothing (Just $ minute (i + 1) max blueprint obsidianResources newObsidianBot) $ canBuildObsidianBot blueprint resources && shouldBuildObsidianBot blueprint robots,
        bool Nothing (Just $ minute (i + 1) max blueprint geodeResources newGeodeBot) $ canBuildGeodeBot blueprint resources,
        Just $ minute (i + 1) max blueprint collectedResources robots
      ]
    onlyBuildGeode = [Just $ minute (i + 1) max blueprint geodeResources newGeodeBot]
    buildObsidianOrNothing = [Just $ minute (i + 1) max blueprint obsidianResources newObsidianBot, Just $ minute (i + 1) max blueprint collectedResources robots]
    buildGeodeIfPossible = bool buildObsidianIfPossible onlyBuildGeode $ canBuildGeodeBot blueprint resources
    buildObsidianIfPossible = bool tryBuildingAll buildObsidianOrNothing $ canBuildObsidianBot blueprint resources

part1 xs = sum $ zipWith (\blueprint id -> id * minute 0 24 blueprint (0, 0, 0, 0) (1, 0, 0, 0)) xs [1 ..]

part2 = product . map (\blueprint -> minute 0 32 blueprint (0, 0, 0, 0) (1, 0, 0, 0)) . take 3