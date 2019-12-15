module Days.Day13(run) where

import Util.Intcode
import Data.Array
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List.Split 

type Position = (Int, Int)

run :: String -> String
run s = 
    let ints = (map read $ splitOn "," s) ++ [0 | _ <- [1..1000]]
        code = listArray (0, pred $ length ints) ints
        (dump, output) = runWithDump code []
        screen = render output
        blockCount = length $ Map.filter (==2) screen
        score = lastScore $ runProgram (hack dump) (repeat 0)
    in show blockCount ++ ", " ++ show score

render :: [Int] -> Map Position Int
render [] = Map.empty
render (x:y:i:xs) = 
    let recursed = render xs 
    in case Map.lookup (x, y) recursed 
        of Nothing -> Map.insert (x, y) i recursed
           _ -> recursed

lastScore :: [Int] -> Int 
lastScore output = firstScore (reverse output)
    where firstScore xs = case xs of 
                            (i:0:(-1):_) -> i 
                            _ -> firstScore $ drop 3 xs
    
hack :: Array Int Int -> Array Int Int 
hack dump = dump // ((0, 2) : [(pos, 1) | pos <- [1652..1693]])