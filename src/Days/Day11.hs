module Days.Day11(run) where

import Util.Intcode

import Control.Monad.State
import Data.Array
import Data.Char
import Data.List.Split
import Data.Map(Map)
import qualified Data.Map as Map

type Position = (Int, Int)
type Grid = Map Position Int
data Direction = N | E | S | W

run :: String -> String
run s = 
    let ints = (map read $ splitOn "," s) ++ [0 | _ <- [1..1000]]
        code = listArray (0, pred $ length ints) ints
        blackGrid = runRobot code 0
        whiteGrid = runRobot code 1
    in show (length $ Map.toList blackGrid) ++ "\n" ++ renderDrawing whiteGrid

runRobot :: Array Int Int -> Int -> Grid 
runRobot code initial = grid 
    where (seen, (grid, _, _)) = runState (vision output) (Map.empty, (0, 0), N)
          output = runProgram code (initial : seen)

vision :: [Int] -> State (Grid, Position, Direction) [Int]
vision [] = return []
vision (color : rotate : xs) = do 
    (grid, pos, dir) <- get 
    let newGrid = paint grid pos color
    let newDir = turn rotate dir
    let newPos = move pos newDir
    put (newGrid, newPos, newDir)
    let seen = Map.findWithDefault 0 newPos newGrid
    (:) seen <$> vision xs

move :: Position -> Direction -> Position
move (x, y) dir = 
    case dir of 
        N -> (x, y - 1)
        E -> (x + 1, y)
        S -> (x, y + 1)
        W -> (x - 1, y)

turn :: Int -> Direction -> Direction 
turn 0 dir = 
    case dir of 
        N -> W 
        W -> S 
        S -> E 
        E -> N 
turn 1 dir = (iterate (turn 0) dir) !! 3

paint :: Grid -> Position -> Int -> Grid 
paint grid pos color = Map.insert pos color grid

renderDrawing :: Grid -> String 
renderDrawing grid = 
    let keys = map fst $ Map.toList grid 
        minx = minimum $ map fst keys 
        maxx = maximum $ map fst keys 
        miny = minimum $ map snd keys 
        maxy = maximum $ map snd keys 
        chars = [[intToDigit $ Map.findWithDefault 0 (x, y) grid | x <- [minx..maxx]] | y <- [miny..maxy]]
    in unlines chars