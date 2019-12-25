module Days.Day25(run) where

import Util.Intcode(readParams, paramKinds)

import Data.Array 
import Data.Char
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

import System.IO.Unsafe

run :: String -> String 
run s = 
    let ints = (map read $ splitOn "," s)
        code = listArray (0, pred $ length ints) ints
    in show $ runProgram code []

runProgram :: Array Int Int -> [Int] -> ()
runProgram code input = unsafePerformIO $ compute (Map.fromList $ assocs code) 0 0 input []

compute :: Map Int Int -> Int -> Int -> [Int] -> [Int] -> IO ()
compute code pos base input output = 
    let op = Map.findWithDefault 0 pos code
        opcode = op `mod` 100
        mods = op `div` 100
        kinds = paramKinds Map.! opcode
        params = readParams code (succ pos) base mods kinds
    in case opcode of 
        1 -> let [p1, p2, target] = params
                 code' = Map.insert target (p1 + p2) code
             in compute code' (pos + 4) base input output
        2 -> let [p1, p2, target] = params
                 code' = Map.insert target (p1 * p2) code
             in compute code' (pos + 4) base input output
        3 -> let [target] = params
                 code' = Map.insert target (head input) code
             in compute code' (pos + 2) base (tail input) output
        4 -> let [value] = params 
             in if chr value == '?' 
                    then do 
                        putStrLn $ map chr $ reverse output
                        cmd <- getLine 
                        compute code (pos + 2) base (map ord $ cmd ++ "\n") output
                    else compute code (pos + 2) base input (value : output)
        5 -> let [condition, jump] = params in 
                if condition > 0
                    then compute code jump base input output
                    else compute code (pos + 3) base input output
        6 -> let [condition, jump] = params in 
                if condition == 0
                    then compute code jump base input output
                    else compute code (pos + 3) base input output
        7 -> let [p1, p2, target] = params 
                 code' = Map.insert target (if p1 < p2 then 1 else 0) code
             in compute code' (pos + 4) base input output
        8 -> let [p1, p2, target] = params 
                 code' = Map.insert target (if p1 == p2 then 1 else 0) code
             in compute code' (pos + 4) base input output
        9 -> let [offset] = params in compute code (pos + 2) (base + offset) input output
        99 -> do 
                putStrLn $ reverse $ map chr output
                return ()