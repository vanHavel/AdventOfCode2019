module Days.Day23(run) where 

import Util.Intcode 

import Data.Array
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

run :: String -> String 
run s = 
    let ints = (map read $ splitOn "," s)
        code = listArray (0, pred $ length ints) ints
        outputs = runNets code
        nats = natValues outputs
        -- TODO fix the off by two error in there
    in show (head nats) ++ ", " ++ show (pred $ pred $ firstDuplicate nats)

natValues :: [Int] -> [Int]
natValues (255:_:y:xs) = y:natValues xs
natValues (_:_:_:xs) = natValues xs

firstDuplicate :: [Int] -> Int 
firstDuplicate (x:y:xs) = if x == y then x else firstDuplicate $ y:xs

headSeq :: Seq Int -> Int
headSeq (x :<| _) = x
headSeq _ = -1

tailSeq :: Seq Int -> Seq Int
tailSeq (_ :|> x :|> y) = Seq.Empty :|> x :|> y

data Target = None | First Int | Second Int Int
    deriving (Eq, Ord, Show)

changeTarget :: Int -> Target -> Target 
changeTarget i None = First i 
changeTarget v (First i) = Second i v
changeTarget _ (Second _ _) = None

runNets :: Array Int Int -> [Int]
runNets code = 
    let codeMap = Map.fromList $ assocs code 
        codes = Map.fromList $ zip [0..49] $ repeat codeMap
        poss = Map.fromList $ zip [0..49] $ repeat 0 
        bases = poss 
        targets = Map.fromList $ zip [0..49] $ repeat None 
        queues = Map.fromList $ [(i, i :<| Seq.Empty) | i <- 255:[0..49]] 
        idles = Map.fromList $ zip [0..49] $ repeat 0
    in computeNet 0 codes poss bases targets queues idles

computeNet :: Int -> Map Int (Map Int Int) -> Map Int Int -> Map Int Int -> Map Int Target -> Map Int (Seq Int) -> Map Int Int -> [Int]
computeNet 50 codes poss bases targets queues idles = 
    if all (>= 5) idles 
        then computeNet 0 codes poss bases targets (Map.insert 0 (tailSeq (queues Map.! 255)) queues) (Map.fromList $ zip [0..49] $ repeat 0)
        else computeNet 0 codes poss bases targets queues idles
computeNet i codes poss bases targets queues idles = 
    let op = Map.findWithDefault 0 (poss Map.! i) (codes Map.! i)
        opcode = op `mod` 100
        mods = op `div` 100
        kinds = paramKinds Map.! opcode
        params = readParams (codes Map.! i) (succ $ poss Map.! i) (bases Map.! i) mods kinds
    in case opcode of 
        1 -> let [p1, p2, target] = params
             in computeNet (succ i) (Map.adjust (Map.insert target (p1 + p2)) i codes) (Map.adjust (+ 4) i poss) bases targets queues idles
        2 -> let [p1, p2, target] = params
             in computeNet (succ i) (Map.adjust (Map.insert target (p1 * p2)) i codes) (Map.adjust (+ 4) i poss) bases targets queues idles
        3 -> let [target] = params
                 input = headSeq $ queues Map.! i
             in  computeNet (succ i) (Map.adjust (Map.insert target input) i codes) (Map.adjust (+ 2) i poss) bases targets (Map.adjust (Seq.drop 1) i queues) (Map.adjust (\c -> if input == -1 then succ c else 0) i idles)
        4 -> let [value] = params 
                 newQueues = case targets Map.! i of 
                    Second j v -> Map.adjust (\s -> s |> v |> value) j queues
                    _ -> queues
                 outputs = case targets Map.! i of 
                    Second j v -> [j, v, value]
                    _ -> []
             in outputs ++ computeNet (succ i) codes (Map.adjust (+ 2) i poss) bases (Map.adjust (changeTarget value) i targets) newQueues idles
        5 -> let [condition, jump] = params in 
                if condition > 0
                    then computeNet (succ i) codes (Map.insert i jump poss) bases targets queues idles
                    else computeNet (succ i) codes (Map.adjust (+ 3) i poss) bases targets queues idles
        6 -> let [condition, jump] = params in 
                if condition == 0
                    then computeNet (succ i) codes (Map.insert i jump poss) bases targets queues idles
                    else computeNet (succ i) codes (Map.adjust (+ 3) i poss) bases targets queues idles
        7 -> let [p1, p2, target] = params 
                 value = if p1 < p2 then 1 else 0
             in computeNet (succ i) (Map.adjust (Map.insert target value) i codes) (Map.adjust (+ 4) i poss) bases targets queues idles
        8 -> let [p1, p2, target] = params 
                 value = if p1 == p2 then 1 else 0
             in computeNet (succ i) (Map.adjust (Map.insert target value) i codes) (Map.adjust (+ 4) i poss) bases targets queues idles
        9 -> let [offset] = params 
             in computeNet (succ i) codes (Map.adjust (+ 2) i poss) (Map.adjust (+ offset) i bases) targets queues idles

