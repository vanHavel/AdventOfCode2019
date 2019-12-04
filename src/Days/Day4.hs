module Days.Day4(run) where

import Data.Char
import Data.List
import Data.List.Split

run :: String -> String 
run s = 
    let [min, max] = map read $ splitOn "-" s
        validValues = filter isValid [min..max]
        validValues' = filter isValid' [min..max]
    in show (length validValues) ++ "," ++ show (length validValues')

isValid :: Int -> Bool
isValid i = (hasPair digits) && (nondecreasing digits)
    where digits = map digitToInt $ show i

isValid' :: Int -> Bool
isValid' i = (hasPair' digits) && (nondecreasing digits)
    where digits = map digitToInt $ show i

hasPair :: [Int] -> Bool
hasPair [] = False
hasPair [_] = False
hasPair (x:y:xs) | x == y    = True
                 | otherwise = hasPair (y:xs)

hasPair' :: [Int] -> Bool
hasPair' = (> 0) . length . filter (\x -> length x == 2) . group

nondecreasing :: [Int] -> Bool
nondecreasing [] = True
nondecreasing [_] = True 
nondecreasing (x:y:xs) | x > y     = False
                       | otherwise = nondecreasing (y:xs)
