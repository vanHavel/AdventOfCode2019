module Util.Intcode(runProgram) where

import Control.Monad.ST
import Data.Array
import Data.Array.ST 
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

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
runProgram code input = runST $ do 
    stCode <- thaw code 
    compute stCode 0 0 input

readParams :: STArray s Int Int -> Int -> Int -> Int -> [Access] -> ST s [Int]
readParams _ _ _ _ [] = return []
readParams code pos base mods (kind:kinds) = do 
    param <- case (mods `mod` 10, kind) of
        (0, Read) -> readArray code pos >>= readArray code
        (2, Read) -> readArray code pos >>= readArray code . (+ base)
        (2, Write) -> (+ base) <$> readArray code pos
        _ -> readArray code pos 
    rest <- readParams code (succ pos) base (mods `div` 10) kinds
    return $ param:rest

compute :: STArray s Int Int -> Int -> Int -> [Int] -> ST s [Int]
compute code pos base input = do 
    op <- readArray code pos
    let opcode = op `mod` 100
        mods = op `div` 100
        kinds = paramKinds Map.! opcode
    params <- readParams code (succ pos) base mods kinds
    case opcode of 
        1 -> let [p1, p2, target] = params in do 
            writeArray code target (p1 + p2)
            compute code (pos + 4) base input
        2 -> let [p1, p2, target] = params in do 
            writeArray code target (p1 * p2) 
            compute code (pos + 4) base input
        3 -> let [target] = params in do 
            writeArray code target (head input) 
            compute code (pos + 2) base (tail input)
        4 -> let [value] = params in (:) value <$> compute code (pos + 2) base input
        5 -> let [condition, jump] = params in 
                if condition > 0
                    then compute code jump base input 
                    else compute code (pos + 3) base input
        6 -> let [condition, jump] = params in 
                if condition == 0
                    then compute code jump base input 
                    else compute code (pos + 3) base input
        7 -> let [p1, p2, target] = params in do
                writeArray code target $ if p1 < p2 then 1 else 0
                compute code (pos + 4) base input
        8 -> let [p1, p2, target] = params in do
            writeArray code target $ if p1 == p2 then 1 else 0
            compute code (pos + 4) base input
        9 -> let [offset] = params in compute code (pos + 2) (base + offset) input
        99 -> return []