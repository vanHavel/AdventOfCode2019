module Days.Day19(run) where 

import Util.Intcode 

import Data.Array
import Data.List.Split 

run :: String -> String 
run s =
    let ints = (map read $ splitOn "," s)
        code = listArray (0, pred $ length ints) ints
        positions1 = [[x, y] | x <- [0..49], y <-[0..49]]
        outputs1 = [head $ runProgram code pos | pos <- positions1]
        (y, x) = findSquare code (0, 0)
    in show (sum outputs1) ++ ", " ++ show (10000 * x + y)

findSquare :: Array Int Int -> (Int, Int) -> (Int, Int)
findSquare code (y, x) = 
    if x >= 99 && isOk (y, x-99)
        then goLeft code (y, x)
        else findSquare code (goRight code (y + 1, x + 1))
    where goLeft code (y, x) | isOk (y, x-100) = goLeft code (y, x-1)
                             | otherwise       = (y, x-99)
          goRight code (y, x) | head (runProgram code [x+1, y]) == 1 = goRight code (y, x+1)
                              | otherwise                            = (y, x)
          isOk (y, x) = head (runProgram code [x, y]) == 1 
            && head (runProgram code [x, y + 99]) == 1 
            && head (runProgram code [x + 99, y]) == 1 
            && head (runProgram code [x + 99, y + 99]) == 1