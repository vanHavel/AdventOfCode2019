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


runWithDump :: Array Int Int -> [Int] -> (Array Int Int, [Int])
runWithDump code input = computeAndDump code 0 0 input

computeAndDump :: Array Int Int -> Int -> Int -> [Int] -> (Array Int Int, [Int])
computeAndDump code pos base input = 
    let op = code ! pos
        opcode = op `mod` 100
        mods = op `div` 100
        kinds = paramKinds Map.! opcode
        params = readParams code (succ pos) base mods kinds
    in case opcode of 
        1 -> let [p1, p2, target] = params
                 code' = code // [(target, p1 + p2)]
             in computeAndDump code' (pos + 4) base input
        2 -> let [p1, p2, target] = params
                 code' = code // [(target, p1 * p2)]
             in computeAndDump code' (pos + 4) base input
        3 -> let [target] = params
                 code' = code // [(target, head input)]
             in computeAndDump code' (pos + 2) base (tail input)
        4 -> let [value] = params in fmap (value :) $ computeAndDump code (pos + 2) base input
        5 -> let [condition, jump] = params in 
                if condition > 0
                    then computeAndDump code jump base input 
                    else computeAndDump code (pos + 3) base input
        6 -> let [condition, jump] = params in 
                if condition == 0
                    then computeAndDump code jump base input 
                    else computeAndDump code (pos + 3) base input
        7 -> let [p1, p2, target] = params 
                 code' = code // [(target, if p1 < p2 then 1 else 0)]
             in computeAndDump code' (pos + 4) base input
        8 -> let [p1, p2, target] = params 
                 code' = code // [(target, if p1 == p2 then 1 else 0)]
             in computeAndDump code' (pos + 4) base input
        9 -> let [offset] = params in computeAndDump code (pos + 2) (base + offset) input
        99 -> (code, [])

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