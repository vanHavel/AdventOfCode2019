module Days.Day18(run) where 

import Util.AStar

import Control.Monad
import Data.Array 
import Data.Char
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence(Seq(..), (><))
import qualified Data.Sequence as Seq
import Data.Set(Set)
import qualified Data.Set as Set

-- this takes a few minutes for each part
run :: String -> String 
run s = 
    let grid1 = parseGrid s
        info1 = costInfos grid1 
        result1 = runAStar info1
        [(yc, xc)] = startPoss grid1
        grid2 = grid1 // zip [(y, x) | y <- [yc-1..yc+1], x <- [xc-1..xc+1]] [Start NW, Wall, Start NE, Wall, Wall, Wall, Start SW, Wall, Start SE]
        info2 = costInfos grid2 
        result2 = runAStar info2
    in show result1 ++ ", " ++ show result2

-- Position, corner and field types
type Position = (Int, Int)
data Corner = NE | SE | NW | SW | C
    deriving (Eq, Ord, Show)

data Field = Start !Corner | Wall | Floor | Key !Char | Door !Char
  deriving (Eq, Ord, Show)

isKey :: Field -> Bool
isKey (Key _) = True
isKey _ = False

isStart :: Field -> Bool 
isStart (Start _) = True 
isStart _ = False

fromChar :: Char -> Field 
fromChar  '@' = Start C
fromChar '#' = Wall
fromChar '.' = Floor 
fromChar c | isLower c = Key c 
           | isUpper c = Door $ toLower c

-- grid utils
type Grid = Array Position Field
startPoss :: Grid -> [Position]
startPoss = map fst . filter (isStart . snd) . assocs

keyPoss :: Grid -> [Position]
keyPoss = map fst . filter (isKey . snd) . assocs

successors :: Position -> [Position]
successors (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

parseGrid :: String -> Grid 
parseGrid s = 
    array ((1, 1), (length $ lines s, length $ head $ lines s)) [
        ((row, col), fromChar char) | (row, line) <- zip [1..] (lines s), 
                                      (col, char) <- zip [1..] line
    ]

-- bfs and utils
data PathInfo = PathInfo {
    cost :: !Int,
    keys :: ![Char]
} deriving (Eq, Ord, Show)

bfs :: Grid -> Seq Position -> Map Position PathInfo -> Map Position PathInfo
bfs grid Empty state = state 
bfs grid (x :<| xs) state = 
    let succs = filter (\pos -> Map.lookup pos state == Nothing && grid ! pos /= Wall) $ successors x 
        newState = foldr updateState state succs
    in bfs grid (xs >< Seq.fromList succs) newState
        where xInfo = state Map.! x 
              updateState pos = Map.insert pos PathInfo {
                cost = succ $ cost xInfo, 
                keys = case grid ! x of 
                    Door c -> c : keys xInfo 
                    _ -> keys xInfo
              }

runBfs :: Grid -> Position -> Map Position PathInfo
runBfs grid pos = bfs grid (pos :<| Empty) $ Map.singleton pos PathInfo {keys=[], cost=0}

keyMap :: Grid -> Position -> Map Position PathInfo -> Map Char PathInfo
keyMap grid pos = kMap . kFilter
    where kMap = Map.mapKeys (\p -> let Key c = grid ! p in c)
          kFilter = Map.filterWithKey $ \p _ -> p /= pos && (isKey $ grid ! p)

costInfos :: Grid -> Map Field (Map Char PathInfo)
costInfos grid = 
    let relevantPositions = startPoss grid ++ keyPoss grid 
    in Map.fromList [(grid ! pos, keyMap grid pos $ runBfs grid pos) | pos <- relevantPositions]

-- AStar setup
data SearchNode = SearchNode {
    heldKeys :: !(Set Char),
    fields :: !(Map Corner Field),
    lastMove :: !Corner
} deriving (Ord, Show)

instance Eq SearchNode where
    (==) n1 n2 = (heldKeys n1 == heldKeys n2 && fields n1 == fields n2)

initial :: [Field] -> SearchNode
initial fs = SearchNode {
    heldKeys = Set.empty, 
    fields = Map.fromList [(corner, f) | f@(Start corner) <- fs], 
    lastMove = C
}

mkSuccFun :: Map Field (Map Char PathInfo) -> SearchNode -> Set (SearchNode, Int)
mkSuccFun costInfos node = Set.fromList $ do 
    corner <- Map.keys $ fields node 
    let relevantInfos = costInfos Map.! (fields node Map.! corner)
    (c, info) <- Map.toList relevantInfos
    guard (all (`elem` (heldKeys node)) (keys info))
    guard (not $ elem c (heldKeys node))
    let newNode = SearchNode {
        heldKeys = Set.insert c $ heldKeys node, 
        fields = Map.insert corner (Key c) $ fields node, 
        lastMove = corner
    }
    return (newNode, cost info)

mkGoalTest :: Int -> SearchNode -> Bool 
mkGoalTest num node = length (heldKeys node) == num

mkHeuristic :: Map Field (Map Char PathInfo) -> SearchNode -> Int 
mkHeuristic info node = 
    let succs = mkSuccFun info node
    in if succs == Set.empty 
        then 0 
        else 
            let updateCosts = \(succInfo, cost) -> Map.insertWith max (lastMove succInfo) cost
                cornerCosts = Set.foldr updateCosts Map.empty succs
            in Map.foldr (+) 0 $ cornerCosts

runAStar :: Map Field (Map Char PathInfo) -> Int 
runAStar info = 
    let n = length $ filter isKey $ Map.keys info
        startingFields = filter isStart $ Map.keys info
    in fst $ genAStarSearch (initial startingFields) (mkHeuristic info) (mkSuccFun info) (mkGoalTest n)