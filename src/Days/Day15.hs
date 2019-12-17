module Days.Day15(run, render) where

import Util.Intcode 

import Control.Monad.State.Lazy
import Data.Array 
import Data.List.Split 
import Data.Map(Map)
import qualified Data.Map as Map

type Position = (Int, Int)
data Direction = N | E | S | W 
    deriving (Eq, Show)

toInt :: Direction -> Int 
toInt N = 1
toInt S = 2 
toInt W = 3
toInt E = 4

move :: Position -> Direction -> Position
move (x, y) dir = 
    case dir of 
        N -> (x, y - 1)
        E -> (x + 1, y)
        S -> (x, y + 1)
        W -> (x - 1, y)

turnAround :: Direction -> Direction 
turnAround N = S 
turnAround S = N 
turnAround W = E
turnAround E = W

data SearchState = SearchState {
    position :: Position,
    direction :: Direction,
    grid :: Map Position Char,
    history :: [Direction]
} deriving (Show)

run :: String -> String 
run s = 
    let ints = (map read $ splitOn "," s)
        code = listArray (0, pred $ length ints) ints
        state = explore code 
        dist1 = distanceToGoal $ grid state
        dist2 = farthestDistance $ grid state
    in show dist1 ++ ", " ++ show dist2

explore :: Array Int Int -> SearchState
explore arr = finalState
    where output = runProgram arr (1 : input)
          (input, finalState) = runState (exploreStateful output True) initialState
          initialState = SearchState {position = (0, 0), direction = N, grid = Map.fromList [((0, 0), '.')], history = []}

exploreStateful :: [Int] -> Bool -> State SearchState [Int]
exploreStateful (0 : xs) _ = do 
    modify (\s -> s{grid = Map.insert (move (position s) (direction s)) '#' $ grid s})
    makeMove xs 
exploreStateful (i : xs) new = do 
    modify (\s -> s{position = move (position s) (direction s)})
    modify (\s -> s{history = if new then (direction s) : (history s) else history s})
    modify (\s -> s{grid = Map.insert (position s) (if i == 1 then '.' else 'G') (grid s)})
    makeMove xs

makeMove :: [Int] -> State SearchState [Int]
makeMove xs = do 
    mNextDir <- directionToExplore
    case mNextDir of
        Nothing -> do 
            hist <- history <$> get
            case hist of 
                [] -> return []
                (backDir:rest) -> do
                    modify (\s -> s{direction = turnAround backDir, history = rest})
                    (:) (toInt $ turnAround backDir) <$> exploreStateful xs False
        Just nextDir -> do 
            modify (\s -> s{direction = nextDir})
            (:) (toInt nextDir) <$> exploreStateful xs True

directionToExplore :: State SearchState (Maybe Direction)
directionToExplore = do 
    s <- get 
    let pos = position s 
    let map = grid s 
    let surroundings = [(dir, Map.lookup (move pos dir) map) | dir <- [N, S, E, W]]
    if all isKnown surroundings
        then return Nothing
        else return $ Just $ fst $ head $ filter (not . isKnown) surroundings
            where isKnown = \(dir, mc) -> mc /= Nothing

render :: Map Position Char -> String 
render grid = 
    let keys = map fst $ Map.toList grid 
        minx = minimum $ map fst keys 
        maxx = maximum $ map fst keys 
        miny = minimum $ map snd keys 
        maxy = maximum $ map snd keys 
        chars = [[Map.findWithDefault '?' (x, y) grid | x <- [minx..maxx]] | y <- [miny..maxy]]
    in unlines chars

distanceToGoal :: Map Position Char -> Int 
distanceToGoal grid = (distances grid) Map.! (0, 0)

farthestDistance :: Map Position Char -> Int 
farthestDistance = maximum . map snd . Map.toList . distances

goalPos :: Map Position Char -> Position 
goalPos grid = head [pos | (pos, char) <- Map.toList grid, char == 'G']

distances :: Map Position Char -> Map Position Int 
distances grid = bfs grid [goalPos grid] $ Map.fromList [(goalPos grid, 0)]
    where bfs _ [] seen = seen
          bfs grid (x:queue) seen = 
            let neighbors = [move x dir | dir <- [N, S, E, W], 
                                          Map.lookup (move x dir) grid `elem` [Just '.', Just 'G'], 
                                          Map.lookup (move x dir) seen == Nothing]
                Just xDist = Map.lookup x seen 
                newSeen = foldr (\pos-> Map.insert pos (succ xDist)) seen neighbors
            in bfs grid (queue ++ neighbors) newSeen