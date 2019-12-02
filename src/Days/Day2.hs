module Days.Day2(run) where

import Control.Monad
import Control.Monad.ST 

import Data.List.Split
import Data.Array
import Data.Array.ST

run :: String -> String 
run s = 
    let nums = map read $ splitOn "," s
        n = length nums 
        arr = listArray (0, pred n) nums 
    in runST $ do
        (res1, _, _) <- trySample arr (12, 2)
        results <- forM [(noun, verb) | noun <- [0..99], verb <- [0..99]] (trySample arr)
        let (_, noun, verb) = head . filter (\(x, _, _) -> x == 19690720) $ results
        return $ show res1 ++ ", " ++ show (100 * noun + verb)

compute :: STArray s Int Int -> Int -> ST s Int
compute arr pos = do 
    opcode <- readArray arr pos
    if opcode == 99 then do
        res <- readArray arr 0
        return res
    else do 
        let op = case opcode of 
             1 -> (+)
             2 -> (*) 
        pos1 <- readArray arr (pos + 1)
        pos2 <- readArray arr (pos + 2)
        operand1 <- readArray arr (pos1)
        operand2 <- readArray arr (pos2)
        target <- readArray arr (pos + 3)
        writeArray arr target (op operand1 operand2)
        compute arr (pos + 4)

trySample :: Array Int Int -> (Int, Int) -> ST s (Int, Int, Int)
trySample arr (noun, verb) = do 
    stArr <- thaw arr 
    writeArray stArr 1 noun
    writeArray stArr 2 verb 
    res <- compute stArr 0
    return (res, noun, verb)
        
