module Days.Day17(run) where

import Util.Intcode

import Data.Array 
import Data.Char
import Data.List.Split 

import Debug.Trace

type Position = (Int, Int)
type Grid = Array Position Char
data Action = L | R | Go Int 
    deriving (Eq, Show)
data Direction = N | S | E | W 
    deriving (Eq, Show)

turnRight :: Direction -> Direction
turnRight N = E
turnRight S = W
turnRight W = N
turnRight E = S

turnLeft :: Direction -> Direction
turnLeft = turnRight . turnRight . turnRight

move :: Position -> Direction -> Position
move (y, x) dir = 
    case dir of 
        N -> (y - 1, x)
        E -> (y, x + 1)
        S -> (y + 1, x)
        W -> (y, x - 1)

fromChar :: Char -> Direction
fromChar '^' = N 
fromChar 'v' = S 
fromChar '>' = E 
fromChar '<' = W

main, a, b, c, feed :: String 
main = "A,C,A,C,B,C,B,A,C,B\n"
a = "R,4,R,10,R,8,R,4\n"
b = "R,4,L,12,R,6,L,12\n"
c = "R,10,R,6,R,4\n"
feed = "n\n"
    
run :: String -> String 
run s = 
    let ints = (map read $ splitOn "," s)
        code = listArray (0, pred $ length ints) ints
        output = runProgram code [] 
        grid = makeGrid output 
        alignment = alignmentScore grid 
        path = getPath grid
        code2 = code // [(0, 2)]
        output2 = runProgram code2 $ map ord $ main ++ a ++ b ++ c ++ feed 
    in show alignment ++ "," ++ (show $ last output2)

makeGrid :: [Int] -> Grid
makeGrid ints = 
    let lines = splitOn [10] $ init $ init ints
        bounds = ((1, 1), (length lines, length $ head lines))
    in array bounds [((row, col), chr i) 
                     |(row, line) <- zip [1..] lines
                     ,(col, i) <- zip [1..] line]

alignmentScore :: Grid -> Int 
alignmentScore = sum . map score . intersections 

score :: Position -> Int 
score (x, y) = (x - 1) * (y - 1)

intersections :: Grid -> [Position]
intersections grid = [pos | (pos@(y, x), char) <- assocs grid
                          ,x > 1, y > 1
                          ,x < (snd $ snd $ bounds grid), y < (fst $ snd $ bounds grid)
                          ,grid ! (y, x) == '#', grid ! (y - 1, x) == '#', grid ! (y + 1, x) == '#'
                          ,grid ! (y, x - 1) == '#', grid ! (y, x + 1) == '#']

getPath :: Grid -> [Action]
getPath grid = compressActions $ navigate startPos startDir grid 
    where startDir = fromChar (grid ! startPos)
          startPos = fst $ head $ filter (\(pos, e) -> not $ e `elem` ".#") $ assocs grid

compressActions :: [Action] -> [Action]
compressActions [] = []
compressActions (Go a : Go b : xs) = compressActions $ Go (a + b) : xs
compressActions (x:xs) = x : compressActions xs

navigate :: Position -> Direction -> Grid -> [Action]
navigate pos dir grid = 
    let moved = move pos dir 
        cell = safeLookup moved grid 
    in case cell of 
        '#' -> Go 1 : navigate moved dir grid 
        '.' -> case (safeLookup (move pos (turnRight dir)) grid, safeLookup (move pos (turnLeft dir)) grid) of 
            ('.', '.') -> []
            ('.', '#') -> L : navigate pos (turnLeft dir) grid 
            ('#', '.') -> R : navigate pos (turnRight dir) grid

safeLookup :: Position -> Grid -> Char 
safeLookup pos grid = 
    let (min, max) = bounds grid 
    in if fst pos >= fst min && snd pos >= snd min && fst pos <= fst max && snd pos <= snd max
        then grid ! pos 
        else '.'