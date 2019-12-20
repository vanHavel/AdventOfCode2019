module Days.Day20(run) where 

import Data.Array 
import Data.Char
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Sequence(Seq(..), (><))
import qualified Data.Sequence as Seq

import Debug.Trace

run :: String -> String 
run s = 
    let grid = parse $ parseRaw s
        distances = bfs grid (startPos grid :<| Seq.Empty) $ Map.singleton (startPos grid) 0
        leveledDistances = leveledBfs grid ((startPos grid, 0) :<| Seq.Empty) $ Map.singleton (startPos grid, 0) 0
    in show (distances Map.! endPos grid) ++ ", " ++ show (leveledDistances Map.! (endPos grid, 0))

-- types and utils
type Position = (Int, Int)
data Direction = N | E | S | W 
    deriving (Eq, Ord, Show)

move :: Position -> Direction -> Position 
move (y, x) dir = case dir of 
    N -> (y - 1, x)
    S -> (y + 1, x)
    E -> (y, x + 1)
    W -> (y, x - 1)

moveMany :: Position -> [Direction] -> Position 
moveMany = foldr (flip move)

data Tile = Wall | Floor | Entrance | InnerWarp Char Char Position | OuterWarp Char Char Position | Exit
    deriving (Eq, Ord, Show)

-- grid utils
parseRaw :: String -> Array Position Char 
parseRaw s = array bounds [((y, x), c) | (y, line) <- zip [1..] $ lines s, (x, c) <- zip [1..] line]
    where bounds = ((1, 1), (length $ lines s, length $ head $ lines s))

parse :: Array Position Char -> Array Position Tile 
parse arr = array (bounds arr) $ map parseTile $ assocs arr
    where parseTile (pos, c) = (,) pos $ case Map.lookup pos warps of 
            Nothing -> case c of 
                '.' -> Floor 
                _ -> Wall 
            Just warp -> warp
          warps = getWarps arr

getWarps :: Array Position Char -> Map Position Tile
getWarps arr = 
    let warpTiles = [pos | pos <- indices arr, (arr ! pos) == '.', maybeGetLabel arr pos /= Nothing]
        warpsWithLabels = [(dejustify $ maybeGetLabel arr pos, pos) | pos <- warpTiles]
        groupedWarpsWithLabels = groupBy ((==) `on` (\(c1, c2, _) -> (c1, c2)) . fst) $ sort warpsWithLabels
    in Map.fromList $ concatMap handleGroup groupedWarpsWithLabels
        where handleGroup [(('A', 'A', True), pos)] = [(pos, Entrance)]
              handleGroup [(('Z', 'Z', True), pos)] = [(pos, Exit)]
              handleGroup [(l1, p1), (l2, p2)] = [(p1, makeWarp l1 p2), (p2, makeWarp l2 p1)]

maybeGetLabel :: Array Position Char -> Position -> Maybe (Char, Char, Bool)
maybeGetLabel arr pos@(y, x) | arr ! pos /= '.' = Nothing
                             | otherwise = 
    let cPos = arr ! pos 
        ((miny, minx), (maxy, maxx)) = bounds arr
        (l1, l2) = (safeLookup arr $ move pos W, safeLookup arr $ moveMany pos [W, W])
        (r1, r2) = (safeLookup arr $ move pos E, safeLookup arr $ moveMany pos [E, E])
        (u1, u2) = (safeLookup arr $ move pos N, safeLookup arr $ moveMany pos [N, N])
        (d1, d2) = (safeLookup arr $ move pos S, safeLookup arr $ moveMany pos [S, S])
    in if isLetter l1 && isLetter l2 
        then Just (l2, l1, x - 2 == minx)
       else if isLetter r1 && isLetter r2 
        then Just (r1, r2, x + 2 == maxx)
       else if isLetter u1 && isLetter u2 
        then Just (u2, u1, y - 2 == miny)
       else if isLetter d1 && isLetter d2
        then Just (d1, d2, y + 2 == maxy)
       else Nothing
     

dejustify :: Maybe a -> a 
dejustify (Just x) = x

makeWarp :: (Char, Char, Bool) -> Position -> Tile 
makeWarp (c1, c2, True) pos = OuterWarp c1 c2 pos
makeWarp (c1, c2, False) pos = InnerWarp c1 c2 pos

startPos :: Array Position Tile -> Position 
startPos = fst . head . filter (snd . fmap (== Entrance)) . assocs

endPos :: Array Position Tile -> Position 
endPos = fst . head . filter (snd . fmap (== Exit)) . assocs

safeLookup :: Array Position Char -> Position -> Char
safeLookup arr (y, x) = 
    let ((miny, minx), (maxy, maxx)) = bounds arr 
        allowed = minx <= x && x <= maxx && miny <= y && y <= maxy 
    in if allowed
        then arr ! (y, x)
        else '#'

-- distance calculations
bfs :: Array Position Tile -> Seq Position -> Map Position Int -> Map Position Int
bfs grid Seq.Empty dists = dists
bfs grid (x :<| xs) dists = 
    let succs = filter (\p -> Map.lookup p dists == Nothing) $ getSuccessors grid x 
        xDist = dists Map.! x
        newDists = foldr (\pos m -> Map.insert pos (succ xDist) m) dists succs
    in bfs grid (xs >< Seq.fromList succs) newDists

leveledBfs :: Array Position Tile -> Seq (Position, Int) -> Map (Position, Int) Int -> Map (Position, Int) Int 
leveledBfs grid (x :<| xs) dists | x == (endPos grid, 0) = dists
                                 | otherwise             =
    let succs = filter (\p -> Map.lookup p dists == Nothing) $ getLeveledSuccessors grid x
        xDist = dists Map.! x
        newDists = foldr (\successor m -> Map.insert successor (succ xDist) m) dists succs
    in leveledBfs grid (xs >< Seq.fromList succs) newDists

getSuccessors :: Array Position Tile -> Position -> [Position]
getSuccessors grid pos = 
    let normalSuccs = filter (\p -> grid ! p /= Wall) [move pos dir | dir <- [N, S, E, W]]
    in case grid ! pos of 
        InnerWarp _ _ other -> other : normalSuccs
        OuterWarp _ _ other -> other : normalSuccs
        _ -> normalSuccs

getLeveledSuccessors :: Array Position Tile -> (Position, Int) -> [(Position, Int)]
getLeveledSuccessors grid (pos, level) = 
    let normalSuccs = map (\p-> (p, level)) $ filter (\p -> grid ! p /= Wall) [move pos dir | dir <- [N, S, E, W]]
    in case grid ! pos of 
        InnerWarp _ _ other -> (other, succ level) : normalSuccs
        OuterWarp _ _ other -> if level == 0 then normalSuccs else (other, pred level) : normalSuccs
        _ -> normalSuccs