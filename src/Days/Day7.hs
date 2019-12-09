module Days.Day7(run) where

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
    let [output] = runST $ do 
            code <- thaw arr
            compute code 0 [signal, input]
    in runAmps arr output signals

runAmpsFeedback :: Array Int Int -> [Int] -> Int 
runAmpsFeedback arr signals = last amp5Output
    where amp1Output = runWithInput $ [signals !! 0, 0] ++ amp5Output
          amp2Output = runWithInput $ [signals !! 1] ++ amp1Output
          amp3Output = runWithInput $ [signals !! 2] ++ amp2Output
          amp4Output = runWithInput $ [signals !! 3] ++ amp3Output
          amp5Output = runWithInput $ [signals !! 4] ++ amp4Output
          runWithInput input = runST $ do 
            code <- thaw arr 
            compute code 0 input

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