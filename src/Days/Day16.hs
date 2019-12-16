module Days.Day16(run) where 

import Data.Char 

run :: String -> String 
run s = 
    let digits = map digitToInt s 
        iterations1 = iterate phase digits
        relevantBit = drop (read $ take 7 s) $ concat $ replicate 10000 digits
        iterations2 = iterate sumPhase relevantBit
    in show (take 8 $ iterations1 !! 100) ++ ", " ++ show (take 8 $ iterations2 !! 100)

phase :: [Int] -> [Int]
phase xs = [applyPhase i xs | i <- [1..length xs]]

applyPhase :: Int -> [Int] -> Int 
applyPhase i xs = mod (abs $ sum zipped) 10 
    where zipped = zipWith (*) xs mask 
          mask = tail $ cycle $ replicate i 0 ++ replicate i 1 ++ replicate i 0 ++ replicate i (-1)

partialSums :: [Int] -> [Int]
partialSums [x] = [x]
partialSums (x:xs) = 
    let recursed = partialSums xs 
    in (x + head recursed) : recursed

sumPhase :: [Int] -> [Int]
sumPhase = map (`mod` 10) . partialSums 