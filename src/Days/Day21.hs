module Days.Day21(run) where 

import Util.Intcode

import Data.Array
import Data.Char
import Data.List.Split

import Debug.Trace

runScript = [
        Or 'E' 'J',
        Or 'H' 'J',
        Or 'B' 'T',
        And 'C' 'T',
        Not 'T' 'T',
        And 'T' 'J'
    ] ++ baseScript Run

run :: String -> String 
run s = 
    let ints = (map read $ splitOn "," s)
        code = listArray (0, pred $ length ints) ints
        resultsWalk = [runProgram code $ map ord $ render script | script <- walkScripts]
        damageWalk = last $ head $ filter (any (> 1000)) resultsWalk
        runResult = runProgram code $ map ord $ render runScript
    in show damageWalk ++ ", " ++ show (last runResult)

data Instruction = And Char Char | Or Char Char | Not Char Char | Walk | Run
    deriving (Eq, Ord)
type Script = [Instruction]

instance Show Instruction where 
    show (And c1 c2) = "AND" ++ ' ' : c1 : ' ' : c2 : "\n"
    show (Or c1 c2) = "OR" ++ ' ' : c1 : ' ' : c2 : "\n"
    show (Not c1 c2) = "NOT" ++ ' ' : c1 : ' ' : c2 : "\n"
    show (Walk) = "WALK" ++ "\n"
    show (Run) = "RUN" ++ "\n"

render :: Script -> String 
render = concatMap show

walkScripts :: [Script] 
walkScripts = do 
    btct <- [True, False]
    btcf <- [True, False]
    bfct <- [True, False]
    bfcf <- [True, False]
    return $ funcScript [btct, btcf, bfct, bfcf] ++ baseScript Walk

funcScript :: [Bool] -> Script 
funcScript [False, False, False, False] = []
funcScript [False, False, False, True] = [
        Not 'B' 'T',
        Not 'C' 'J', 
        And 'T' 'J'
    ]
funcScript [False, False, True, False] = [
        Not 'B' 'J',
        And 'C' 'J'
    ]
funcScript [False, True, False, False] = [
        Not 'C' 'J', 
        And 'B' 'J'
    ]
funcScript [True, False, False, False] = [
        Or 'B' 'J',
        And 'C' 'J',
        Not 'J' 'J'
    ]
funcScript [True, True, False, False] = [
        Or 'B' 'J'
    ]
funcScript [True, False, False, True] = [
        Or 'B' 'T',
        And 'C' 'T',
        Or 'B' 'J',
        Or 'C' 'J',
        Not 'J' 'J',
        Or 'T' 'J'
    ]
funcScript [True, False, True, False] = [
        Or 'C' 'J'
    ]
funcScript inputs = funcScript (map not inputs) ++ [Not 'J' 'J']


baseScript :: Instruction -> Script 
baseScript i = [
        And 'D' 'J',
        Not 'A' 'T',
        Or 'T' 'J',
        i
    ]