module Days.Day5 where

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
        (99, [])
    ]

run :: String -> String 
run s = 
    let ints = map read $ splitOn "," s :: [Int]
        arr = listArray (0, pred $ length ints) ints
        (output1, output2) = runST $ do
            code <- thaw arr 
            output1 <- compute code 0 [1]
            code <- thaw arr
            output2 <- compute code 0 [5]
            return (output1, output2)
    in show (last output1) ++ ", " ++ show (last output2)

readParams :: STArray s Int Int -> Int -> Int -> [Access] -> ST s [Int]
readParams _ _ _ [] = return []
readParams code pos mods (kind:kinds) = do 
    param <- case (mods `mod` 10, kind) of
        (0, Read) -> readArray code pos >>= readArray code
        _ -> readArray code pos 
    rest <- readParams code (succ pos) (mods `div` 10) kinds
    return $ param:rest

compute :: STArray s Int Int -> Int -> [Int] -> ST s [Int]
compute code pos input = do 
    op <- readArray code pos
    let opcode = op `mod` 100
        mods = op `div` 100
        kinds = paramKinds Map.! opcode
    params <- readParams code (succ pos) mods kinds
    case opcode of 
        1 -> let [p1, p2, target] = params in do 
            writeArray code target (p1 + p2)
            compute code (pos + 4) input
        2 -> let [p1, p2, target] = params in do 
            writeArray code target (p1 * p2) 
            compute code (pos + 4) input
        3 -> let [target] = params in do 
            writeArray code target (head input) 
            compute code (pos + 2) (tail input)
        4 -> let [value] = params in (:) value <$> compute code (pos + 2) input
        5 -> let [condition, jump] = params in 
                if condition > 0
                    then compute code jump input 
                    else compute code (pos + 3) input
        6 -> let [condition, jump] = params in 
                if condition == 0
                    then compute code jump input 
                    else compute code (pos + 3) input
        7 -> let [p1, p2, target] = params in do
                writeArray code target $ if p1 < p2 then 1 else 0
                compute code (pos + 4) input
        8 -> let [p1, p2, target] = params in do
            writeArray code target $ if p1 == p2 then 1 else 0
            compute code (pos + 4) input
        99 -> return []