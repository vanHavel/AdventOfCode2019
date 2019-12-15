module Util.Intcode(runProgram) where

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
runProgram code input = compute code 0 0 input

--runWithDump :: Array Int Int -> [Int] -> (Array Int Int, [Int])
--runWithDump code input = compute code 0 0 input

readParams :: Array Int Int -> Int -> Int -> Int -> [Access] -> [Int]
readParams _ _ _ _ [] = []
readParams code pos base mods (kind:kinds) = 
    let param = case (mods `mod` 10, kind) of
                    (0, Read) -> code ! (code ! pos)
                    (2, Read) -> code ! ((code ! pos) + base)
                    (2, Write) -> (code ! pos) + base
                    _ -> code ! pos
        rest = readParams code (succ pos) base (mods `div` 10) kinds
    in param:rest

compute :: Array Int Int -> Int -> Int -> [Int] -> [Int]
compute code pos base input = 
    let op = code ! pos
        opcode = op `mod` 100
        mods = op `div` 100
        kinds = paramKinds Map.! opcode
        params = readParams code (succ pos) base mods kinds
    in case opcode of 
        1 -> let [p1, p2, target] = params
                 code' = code // [(target, p1 + p2)]
             in compute code' (pos + 4) base input
        2 -> let [p1, p2, target] = params
                 code' = code // [(target, p1 * p2)]
             in compute code' (pos + 4) base input
        3 -> let [target] = params
                 code' = code // [(target, head input)]
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
                 code' = code // [(target, if p1 < p2 then 1 else 0)]
             in compute code' (pos + 4) base input
        8 -> let [p1, p2, target] = params 
                 code' = code // [(target, if p1 == p2 then 1 else 0)]
             in compute code' (pos + 4) base input
        9 -> let [offset] = params in compute code (pos + 2) (base + offset) input
        99 -> []