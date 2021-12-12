import Data.Char (isUpper)
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
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

data Cave = Large String | Small String | SmallWithTime String deriving (Show, Eq, Ord)

type Edge = (Cave, Cave)

getInput = fmap (map parseLine . lines) . readFile

parseLine :: String -> Edge
parseLine e = (parseCave $ head caves, parseCave $ last caves)
  where
    caves = split '-' e

parseCave s
  | isUpper $ head s = Large s
  | otherwise = Small s

split c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : split c s''
    where
      (w, s'') = break (== c) s'

-- Solution Logic

makeAdjacencyLists =
  map (\x -> (fst x, fst x, snd x))
    . Map.assocs
    . foldl (\acc (from, to) -> Map.insertWith (++) to [from] $ Map.insertWith (++) from [to] acc) Map.empty

getGraphFrom = Graph.graphFromEdges . makeAdjacencyLists

part1 = length . validPathsInGraphFrom

part2 edges =
  Set.size $
    foldl1 Set.union $
      map
        (Set.fromList . validPathsInGraphFrom . convertToLarge edges)
        $ getSmallCaves edges

validPathsInGraphFrom edges = map (map (backToSmall . sel1 . vertToAdj)) $ allValidPaths graph (nodeToVert (Small "start")) (nodeToVert (Small "end")) vertToAdj
  where
    (graph, vertToAdj, nodeToVert) = getGraphFrom edges

getSmallCaves edges = Set.toList $ Set.filter (\cave -> isSmallCave cave && isNotStartEnd cave) $ Set.fromList $ edges >>= (\edge -> [fst edge, snd edge])

convertToLarge edges cave = map (\(from, to) -> (convertCaveToLarge from cave, convertCaveToLarge to cave)) edges

convertCaveToLarge (Small i) (Small j)
  | i == j = SmallWithTime i
convertCaveToLarge c c2 = c

backToSmall (SmallWithTime i) = Small i
backToSmall c = c

allValidPaths graph (Just from) (Just to) verToAdj = allValidPaths' from to graph (sel1 . verToAdj) (Set.singleton from) False False
allValidPaths _ _ _ _ = error "Got non-existing Node"

sel1 (a, _, _) = a

allValidPaths' :: Graph.Vertex -> Graph.Vertex -> Graph.Graph -> (Graph.Vertex -> Cave) -> Set.Set Graph.Vertex -> Bool -> Bool -> [[Graph.Vertex]]
allValidPaths' from to graph vert2Node seenSmall seenOnce seenTwice
  | from /= to =
    filter (\p -> head p == from && last p == to) $
      map (from :) $
        foldl (++) [[]] $
          map (\n -> allValidPaths' n to graph vert2Node (if isSmallCave (vert2Node n) then Set.insert n seenSmall else seenSmall) (isSmallButHaveTime (vert2Node n) || seenOnce) (isSmallButHaveTime (vert2Node n) && seenOnce || seenTwice)) $
            filter (\vertex -> (not seenTwice || not (isSmallButHaveTime (vert2Node vertex))) && Set.notMember vertex seenSmall) $ neighbours graph from
  | otherwise = [[to]]

neighbours graph n = map snd $ filter ((== n) . fst) $ Graph.edges graph

isSmallCave (Small _) = True
isSmallCave _ = False

isSmallButHaveTime (SmallWithTime _) = True
isSmallButHaveTime _ = False

isNotStartEnd (Small "start") = False
isNotStartEnd (Small "end") = False
isNotStartEnd _ = True