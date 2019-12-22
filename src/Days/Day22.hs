module Days.Day22(run) where

import Data.Char 
import Data.Map (Map)
import qualified Data.Map as Map

run :: String -> String 
run s = 
    let actions = map parseAction $ lines s 
        n1 = 10007
        n2 = 119315717514047
        result1 = applyUp n1 2019 actions
        linear = downToLinear n2 actions
        powed = fastPow n2 101741582076661 linear 
        result2 = runLinear n2 2020 powed
    in show result1 ++ ", " ++ show result2

data Action = Cut Integer | Reverse | Increment Integer
    deriving (Eq, Ord, Show)

data Linear = Linear {
    factor :: Integer,
    summand :: Integer
} deriving (Eq, Ord, Show)

combine :: Integer -> Linear -> Linear -> Linear 
combine n (Linear f2 s2) (Linear f1 s1) = Linear ((f2 * f1) `mod` n) ((f2 * s1 + s2) `mod` n)

toLinear :: Integer -> Action -> Linear 
toLinear n Reverse = Linear (n - 1) (n - 1)
toLinear n (Cut c) = Linear 1 ((c + n) `mod` n)
toLinear n (Increment i) = Linear (inverse n i) 0

runLinear :: Integer -> Integer -> Linear -> Integer
runLinear n i (Linear f s) = (f * i + s) `mod` n

parseAction :: String -> Action 
parseAction ('c':xs) = Cut $ read $ filter (\c -> isDigit c || c == '-') xs
parseAction xs = case drop 5 xs of 
    'i':_ -> Reverse
    xs    -> Increment $ read $ filter isDigit xs

update :: Integer -> Integer -> Action -> Integer
update n i a = 
    case a of 
        Reverse -> n - 1 - i 
        Cut c -> (i - c + n) `mod` n
        Increment j -> (i * j) `mod` n

inverse :: Integer -> Integer -> Integer 
inverse n i = 
    let (_, y, _) = extGCD n i 
    in y

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (1, 0, a)   
extGCD a b = 
    let (q, r) = a `divMod` b          
        (c, x, y) = extGCD b r 
    in  (x, c-q*x, y) 

applyUp :: Integer -> Integer -> [Action] -> Integer
applyUp n = foldl (update n)

downToLinear :: Integer -> [Action] -> Linear 
downToLinear n = foldr (combine n) (Linear 1 0) . map (toLinear n)

fastPow :: Integer -> Integer -> Linear -> Linear 
fastPow n 0 l = Linear 1 0 
fastPow n 1 l = l 
fastPow n i l | i `mod` 2 == 0 = fastPow n (div i 2) (combine n l l)
              | otherwise      = combine n l $ fastPow n (div i 2) (combine n l l)