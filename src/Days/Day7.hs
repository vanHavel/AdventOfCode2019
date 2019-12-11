module Days.Day7(run) where

import Util.Intcode

import Data.Array
import Data.List.Split

run :: String -> String 
run s = 
    let ints = map read $ splitOn "," s
        arr = listArray (0, pred $ length ints) ints
        perms1 = permutations [0, 1, 2, 3, 4]
        outputs1 = map (runAmps arr 0) perms1
        perms2 = permutations [5, 6, 7, 8, 9]
        outputs2 = map (runAmpsFeedback arr) perms2
    in show (maximum outputs1) ++ ", " ++ show (maximum outputs2)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = do 
    recursed <- permutations xs 
    (left, right) <- partitions recursed
    return $ left ++ (x:right)

partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions l@(x:xs) = ([], l) : map (\(a, b) -> (x:a, b)) (partitions xs)

runAmps :: Array Int Int -> Int -> [Int] -> Int 
runAmps arr input [] = input
runAmps arr input (signal:signals) = 
    let [output] = runProgram arr [signal, input]
    in runAmps arr output signals

runAmpsFeedback :: Array Int Int -> [Int] -> Int 
runAmpsFeedback arr signals = last amp5Output
    where amp1Output = runProgram arr $ [signals !! 0, 0] ++ amp5Output
          amp2Output = runProgram arr $ [signals !! 1] ++ amp1Output
          amp3Output = runProgram arr $ [signals !! 2] ++ amp2Output
          amp4Output = runProgram arr $ [signals !! 3] ++ amp3Output
          amp5Output = runProgram arr $ [signals !! 4] ++ amp4Output