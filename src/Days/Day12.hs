{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Days.Day12(run) where

import Data.Char 
import Data.Bifunctor
import Data.List.Split
import qualified Data.Map as Map

data Quantity = Position | Velocity
data Vec (a :: Quantity) = Vec (Int, Int, Int)
    deriving (Eq, Show)

sumVec :: [Vec a] -> Vec a 
sumVec = foldr addVec $ Vec (0, 0, 0)

addVec :: Vec a -> Vec a -> Vec a 
addVec (Vec (a, b, c)) (Vec (d, e, f)) = Vec (a + d, b + e, c + f)

diffVec :: Vec a -> Vec a -> Vec a 
diffVec a b = addVec a (negVec b)

negVec :: Vec a -> Vec a 
negVec (Vec (a, b, c)) = Vec (-a, -b, -c)

signumVec :: Vec Position -> Vec Velocity
signumVec (Vec (a, b, c)) = Vec (signum a, signum b, signum c)

sel :: Int -> Vec a -> Int 
sel k (Vec (a, b, c)) = 
    case k of 
        1 -> a
        2 -> b 
        3 -> c

run :: String -> String 
run s = 
    let places = map parseMoon $ lines s 
        speeds = [Vec (0, 0, 0) | _ <- [1..4]]
        moonsLater = iterate simulate $ zip places speeds
        energy1k = sum $ map totalEnergy $ moonsLater !! 1000
        xStates = map (map $ bimap (sel 1) (sel 1)) moonsLater
        yStates = map (map $ bimap (sel 2) (sel 2)) moonsLater
        zStates = map (map $ bimap (sel 3) (sel 3)) moonsLater
        (u1, l1) = getCycle xStates 
        (u2, l2) = getCycle yStates
        (u3, l3) = getCycle zStates
        (u, l) = (maximum [u1, u2, u3], foldr lcm 1 [l1, l2, l3])
    in show (energy1k) ++ ", " ++ show (u + l)

parseMoon :: String -> Vec Position
parseMoon s = let [x, y, z] = map (read . filter (\c -> isDigit c || c == '-')) $ splitOn "," s in Vec (x, y, z)

gravityUpdate :: Vec Position -> [Vec Position] -> Vec Velocity
gravityUpdate moon moons = sumVec [signumVec $ diffVec other moon | other <- moons]

simulate :: [(Vec Position, Vec Velocity)] -> [(Vec Position, Vec Velocity)]
simulate moons = zip newPositions newVelocities 
    where (places, speeds) = unzip moons
          newVelocities = zipWith addVec speeds [gravityUpdate place [other | other <- places, other /= place] | place <- places]
          newPositions = [move pos vel | (pos, vel) <- zip places newVelocities]

move :: Vec Position -> Vec Velocity -> Vec Position
move (Vec (a, b, c)) (Vec (d, e, f)) = Vec ((a + d, b + e, c + f))

totalEnergy :: (Vec Position , Vec Velocity) -> Int 
totalEnergy (pos, vel) = potential pos * kinetic vel 

energy :: Vec a -> Int 
energy (Vec (a, b, c)) = abs a + abs b + abs c 

potential :: Vec Position -> Int
potential = energy

kinetic :: Vec Velocity -> Int
kinetic = energy

getCycle :: [[(Int, Int)]] -> (Int, Int)
getCycle xs = go Map.empty 0 xs 
    where go m i (x:xs) = 
            case Map.lookup x m of 
                Nothing -> go (Map.insert x i m) (succ i) xs 
                Just j -> (j, i - j)