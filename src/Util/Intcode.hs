module Util.Intcode(runProgram, readParams, paramKinds) where

import Data.Array
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map


import Debug.Trace

data Access = Read | Write
paramKinds :: Map Int [Access]
paramKinds = Map.fromList [
        (1, [Read, Read, Write]), 
        (2, [Read, Read, Write]), 
        (3, [Write]), 
        (4, [Read]), 
        (5, [Read, Read]),
        (6, [Read, Read]),
        (7, [Read, Read, Write]),
        (8, [Read, Read, Write]),
        (9, [Read]),
        (99, [])
    ]

runProgram :: Array Int Int -> [Int] -> [Int] 
runProgram code input = compute (Map.fromList $ assocs code) 0 0 input

readParams :: Map Int Int -> Int -> Int -> Int -> [Access] -> [Int]
readParams _ _ _ _ [] = []
readParams code pos base mods (kind:kinds) = 
    let param = case (mods `mod` 10, kind) of
                    (0, Read) -> Map.findWithDefault 0 (Map.findWithDefault 0 pos code) code
                    (2, Read) -> Map.findWithDefault 0 ((Map.findWithDefault 0 pos code) + base) code
                    (2, Write) -> (Map.findWithDefault 0 pos code) + base
                    _ -> Map.findWithDefault 0 pos code
        rest = readParams code (succ pos) base (mods `div` 10) kinds
    in param:rest

compute :: Map Int Int -> Int -> Int -> [Int] -> [Int]
compute code pos base input = 
    let op = Map.findWithDefault 0 pos code
        opcode = op `mod` 100
        mods = op `div` 100
        kinds = paramKinds Map.! opcode
        params = readParams code (succ pos) base mods kinds
    in case opcode of 
        1 -> let [p1, p2, target] = params
                 code' = Map.insert target (p1 + p2) code
             in compute code' (pos + 4) base input
        2 -> let [p1, p2, target] = params
                 code' = Map.insert target (p1 * p2) code
             in compute code' (pos + 4) base input
        3 -> let [target] = params
                 code' = Map.insert target (head input) code
             in compute code' (pos + 2) base (tail input)
        4 -> let [value] = params in (value :) $ compute code (pos + 2) base input
        5 -> let [condition, jump] = params in 
                if condition > 0
                    then compute code jump base input 
                    else compute code (pos + 3) base input
        6 -> let [condition, jump] = params in 
                if condition == 0
                    then compute code jump base input 
                    else compute code (pos + 3) base input
        7 -> let [p1, p2, target] = params 
                 code' = Map.insert target (if p1 < p2 then 1 else 0) code
             in compute code' (pos + 4) base input
        8 -> let [p1, p2, target] = params 
                 code' = Map.insert target (if p1 == p2 then 1 else 0) code
             in compute code' (pos + 4) base input
        9 -> let [offset] = params in compute code (pos + 2) (base + offset) input
        99 -> []

