module Days.Day24(run) where 

import Data.Array 
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

type Position = (Int, Int)
type Grid = Array Position Bool

run :: String -> String 
run s = 
    let grid = parseGrid s 
        steps = iterate simulate grid 
        steps2 = iterate simulateComplex $ Map.fromList [(i, if i == 0 then grid else emptyGrid) | i <- [-200..200]]
        dup = firstRepeated steps 
        bugCount = sum $ map (length . filter id . elems . snd) $ Map.toList (steps2 !! 200)
    in show (bioDiversity dup) ++ ", " ++ show bugCount

parseGrid :: String -> Grid
parseGrid s = array ((1, 1), (5, 5)) [((row, col), c == '#') | (line, row) <- zip (lines s) [1..5], (c, col) <- zip line [1..5]]

emptyGrid :: Grid 
emptyGrid = array ((1, 1), (5, 5)) [((row, col), False) | row <- [1..5], col <- [1..5]]

simulate :: Grid -> Grid 
simulate grid = array (bounds grid) $ map (change grid) $ assocs $ grid

simulateComplex :: Map Int Grid -> Map Int Grid 
simulateComplex grids = Map.fromList [(level, array (bounds grid) $ map (changeComplex grids level) $ assocs grid) | (level, grid) <- Map.toList grids]

change :: Grid -> (Position, Bool) -> (Position, Bool)
change grid (pos, b) = 
    let nNeighbors = length $ filter id $ map (grid !) $ neighbors grid pos
        val = case b of
            True -> nNeighbors == 1 
            False -> nNeighbors `elem` [1, 2]
    in (pos, val)

changeComplex :: Map Int Grid -> Int -> (Position, Bool) -> (Position, Bool)
changeComplex _ _ center@((3, 3), False) = center
changeComplex grids level (pos, b) = 
    let nNeighbors = length $ filter id $ map (\(l, p) -> (grids Map.! l) ! p) $ neighborsComplex grids level pos 
        val = case b of
            True -> nNeighbors == 1 
            False -> nNeighbors `elem` [1, 2]
    in (pos, val)

neighbors :: Grid -> Position -> [Position]
neighbors grid (y, x) = filter inBounds [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
    where ((miny, minx), (maxy, maxx)) = bounds grid
          inBounds (y, x) = miny <= y && y <= maxy && minx <= x && x <= maxx

neighborsComplex :: Map Int Grid -> Int -> Position -> [(Int, Position)]
neighborsComplex grids level pos = lowerNeighbors ++ levelNeighbors ++ upperNeighbors
    where lowerNeighbors = if level == -200 then [] else 
            zip (repeat $ pred level) $ case pos of 
                (2, 3) -> [(1, i) | i <- [1..5]]
                (4, 3) -> [(5, i) | i <- [1..5]]
                (3, 2) -> [(i, 1) | i <- [1..5]]
                (3, 4) -> [(i, 5) | i <- [1..5]]
                _ -> []
          levelNeighbors = zip (repeat level) $ neighbors (grids Map.! level) pos 
          upperNeighbors = if level == 200 then [] else 
            zip (repeat $ succ level) $ case pos of 
                (1, 1) -> [(2, 3), (3, 2)]
                (1, 5) -> [(2, 3), (3, 4)]
                (5, 1) -> [(4, 3), (3, 2)]
                (5, 5) -> [(4, 3), (3, 4)]
                (1, _) -> [(2, 3)]
                (5, _) -> [(4, 3)]
                (_, 1) -> [(3, 2)]
                (_, 5) -> [(3, 4)]
                _ -> []   

firstRepeated :: [Grid] -> Grid 
firstRepeated grids = go grids Set.empty 
    where go (g:gs) s = 
            if (elems g) `elem` s 
                then g 
                else go gs (Set.insert (elems g) s)

bioDiversity :: Grid -> Int 
bioDiversity grid = sum [p | (b, p) <- zip (elems grid) (iterate (*2) 1), b == True]