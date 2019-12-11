module Days.Day5(run) where

import Util.Intcode

import Data.Array
import Data.List.Split

run :: String -> String 
run s = 
    let ints = map read $ splitOn "," s :: [Int]
        arr = listArray (0, pred $ length ints) ints
        output1 = runProgram arr [1]
        output2 = runProgram arr [5]
    in show (last output1) ++ ", " ++ show (last output2)