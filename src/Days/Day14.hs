module Days.Day14(run) where

import Data.Char
import Data.List.Split
import Data.Map(Map)
import qualified Data.Map as Map 

run :: String -> String 
run s = 
    let rules = Map.fromList $ map parseRule $ lines s
        cost = oreCost rules 1 
        maxFuel = binsearch (\k -> oreCost rules k <= 1000000000000) 1 1000000000
    in show cost ++ ", " ++ show maxFuel

parseRule :: String -> (String, (Int, [(String, Int)]))
parseRule s = 
    let [resources, result] = splitOn ("=>") $ filter (/= ' ') s
        (resultName, resultAmount) = parseAtom result 
        parsedResources = map parseAtom $ splitOn "," resources
    in (resultName, (resultAmount, parsedResources))

parseAtom :: String -> (String, Int)
parseAtom a = (dropWhile isDigit a, read $ takeWhile isDigit a)

binsearch :: (Int -> Bool) -> Int -> Int -> Int 
binsearch p min max | min == max = min 
                    | otherwise =
                        let mid = subtract ((max - min) `div` 2) max
                        in if (p mid) 
                            then binsearch p mid max 
                            else binsearch p min (pred mid)

oreCost :: Map String (Int, [(String, Int)]) -> Int -> Int 
oreCost rules amount = go rules (Map.fromList [("FUEL", amount)])
    where go rules requires | done requires = requires Map.! "ORE"
                            | otherwise = 
                                let (name, needed) = head $ Map.toList $ toProcess requires
                                    (ruleAmount, resources) = rules Map.! name
                                    ruleApplications = (div needed ruleAmount) + (signum $ mod needed ruleAmount)
                                    updates = (name, -ruleApplications * ruleAmount) : [(rName, ruleApplications * rAmount) | (rName, rAmount) <- resources]
                                in go rules (foldr (uncurry $ Map.insertWith (+)) requires updates)
          done requires = toProcess requires == Map.empty
          toProcess = Map.filterWithKey (\k a -> k /= "ORE" && a > 0)
