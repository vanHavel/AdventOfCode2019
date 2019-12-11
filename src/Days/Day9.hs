module Days.Day9(run) where

import Util.Intcode

import Data.Array
import Data.List.Split

run :: String -> String
run s = 
    let ints = (map read $ splitOn "," s) ++ [0 | _ <- [1..1000]] :: [Int]
        code = listArray (0, pred $ length ints) ints
        output1 = runProgram code [1]
        output2 = runProgram code [2]
    in show output1 ++ ", " ++ show output2