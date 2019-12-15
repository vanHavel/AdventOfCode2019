module Days.Day15(run) where

import Util.Intcode 

import Control.Monad.State.Lazy
import Data.Array 
import Data.List.Split 
import Data.Map(Map)
import qualified Data.Map as Map

import Debug.Trace

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
}

run :: String -> String 
run s = 
    let ints = (map read $ splitOn "," s) ++ [0 | _ <- [1..1000]]
        code = listArray (0, pred $ length ints) ints
        state = explore code 
        dist = length $ history state
    in show dist

explore :: Array Int Int -> SearchState
explore arr = finalState
    where output = runProgram arr (1 : input)
          (input, finalState) = runState (exploreStateful output) initialState
          initialState = SearchState {position = (0, 0), direction = N, grid = Map.fromList [((0, 0), '.')], history = []}

exploreStateful :: [Int] -> State SearchState [Int]
exploreStateful (0 : xs) = do 
    modify (\s -> s{grid = Map.insert (move (position s) (direction s)) '#' $ grid s})
    makeMove xs 
exploreStateful (i : xs) = do 
    modify (\s -> s{position = move (position s) (direction s), history = (direction s) : (history s)})
    modify (\s -> s{grid = Map.insert (position s) '.' (grid s)})
    case i of 
        2 -> return [] 
        1 -> makeMove xs

makeMove :: [Int] -> State SearchState [Int]
makeMove xs = do 
    mNextDir <- directionToExplore
    case mNextDir of
        Nothing -> do 
            backDir <- turnAround <$> last <$> history <$> get
            modify (\s -> s{direction = backDir, history = tail $ history s})
            (:) (toInt backDir) <$> exploreStateful xs
        Just nextDir -> do 
            modify (\s -> s{direction = nextDir})
            (:) (toInt nextDir) <$> exploreStateful xs

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