module Days.Day6(run) where

import Data.List.Split
import Data.Map(Map)
import qualified Data.Map as Map

run :: String -> String
run s =
    let parents = Map.fromList $ (:) ("COM","COM") $ map parseOrbit $ lines s 
        depths = computeDepths parents
        orbits = sum $ map snd $ Map.toList depths
        distanceToSanta = distance parents depths "YOU" "SAN"
    in show orbits ++ ", " ++ show (distanceToSanta - 2)

parseOrbit :: String -> (String, String)
parseOrbit s = (right, left)
    where [left, right] = splitOn ")" s

computeDepths :: Map String String -> Map String Int 
computeDepths parents = depthMap
  where depthMap = Map.fromList [(name, go name) | name <- Map.keys parents]
        go "COM" = 0
        go name = 1 + (depthMap Map.! (parents Map.! name))

distance :: Map String String -> Map String Int -> String -> String -> Int 
distance parents depths x y | x == y                           = 0
                            | depths Map.! x > depths Map.! y  = 1 + distance parents depths (parents Map.! x) y 
                            | depths Map.! x < depths Map.! y  = 1 + distance parents depths x (parents Map.! y)
                            | depths Map.! x == depths Map.! y = 2 + distance parents depths (parents Map.! x) (parents Map.! y)
