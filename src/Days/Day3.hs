module Days.Day3(run) where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.List.Split

data Direction = N | S | W | E deriving (Show)
type Position = (Int, Int)
type Command = (Direction, Int)

run :: String -> String
run s = 
    let [cs1, cs2] = map (map parseCommand . splitOn ",") $ lines s
        p1 = passedPoints 0 (0, 0) cs1
        p2 = passedPoints 0 (0, 0) cs2
        intersection = Map.intersectionWith (+) p1 p2
    in 
        show (closest intersection) ++ ", " ++ show (first intersection)

parseCommand :: String -> Command
parseCommand (dir:ls) =
    case dir of
        'D' -> (S, li)
        'U' -> (N, li)
        'R' -> (E, li)
        'L' -> (W, li)
    where li = read ls 

passedPoints :: Int -> Position -> [Command] -> Map Position Int
passedPoints _ _ [] = Map.empty
passedPoints traveled pos (c:cs) = 
    let passedPoss = move c pos 
        zippedPoss = zip passedPoss [traveled + 1 ..]
        recursed = passedPoints (traveled + length passedPoss) (last passedPoss) cs 
    in foldr (\(pos, time) m -> Map.insert pos time m) recursed zippedPoss

move :: Command -> Position -> [Position]
move (dir, dist) (x, y) = 
    case dir of
        N -> zip (repeat x) [y + 1 .. y + dist]
        S -> zip (repeat x) [y - 1, y - 2 .. y - dist]
        E -> zip [x + 1 .. x + dist] (repeat y)
        W -> zip [x - 1, x - 2 .. x - dist] (repeat y)
    
closest :: Map Position Int -> Int 
closest m = minimum [abs x + abs y | ((x, y), _) <- Map.toList m]

first :: Map Position Int -> Int 
first = minimum . map snd . Map.toList