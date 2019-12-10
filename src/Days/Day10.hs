module Days.Day10(run) where 

import Data.Function
import Data.List 
import Data.Ord

type Position = (Int, Int)

run :: String -> String
run s = 
    let asteroids = parseAsterioids s 
        (pos, count) = mostVisible asteroids
        (x, y) = (sortByDestruction pos $ filter (/= pos) asteroids) !! 199
    in show (count) ++ ", " ++ show (100 * x + y)

parseAsterioids :: String -> [Position]
parseAsterioids = concatMap parseLine . zip [0..] . lines

parseLine :: (Int, String) -> [Position]
parseLine (row, line) = map ((\x -> (x, row)) . fst) $ filter (\(_, c) -> c == '#') $ zip [0..] line 

difference :: Position -> Position -> Position
difference (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

reduce :: Position -> Position 
reduce (x, 0) = (signum x, 0)
reduce (0, y) = (0, signum y)
reduce (x, y) = (div x g, div y g)
    where g = gcd x y

absSum :: Position -> Int
absSum (x, y) = abs x + abs y

obstructs :: Position -> Position -> Position -> Bool
obstructs a b c = 
    let ab = difference a b 
        ac = difference a c 
    in reduce ab == reduce ac && absSum ab > absSum ac

visible :: Position -> [Position] -> [Position]
visible focus other = [p | p <- other, p /= focus, not $ any (obstructs focus p) [q | q <- other, q /= focus, q /= p]]

mostVisible :: [Position] -> (Position, Int)
mostVisible asteroids = maximumBy (compare `on` snd) $ [(a, length $ visible a asteroids) | a <- asteroids]

angle :: Position -> Double
angle (x, y) = - atan2 (fromIntegral x) (fromIntegral y)

sortByDestruction :: Position -> [Position] -> [Position]
sortByDestruction pos asteroids = 
    let differences = map (reduce . difference pos) asteroids
        statsAndPositions = groupBy ((==) `on` (\(x, _, _) -> x)) $ sort [(angle diff, absSum diff, a) | (diff, a) <- zip differences asteroids]
    in map (\(_, _, x) -> x) $ cycleThrough statsAndPositions

cycleThrough :: (Eq a) => [[a]] -> [a]
cycleThrough [] = []
cycleThrough xss = 
    let heads = map head xss 
    in heads ++ (cycleThrough $ filter (/= []) $ map tail xss)