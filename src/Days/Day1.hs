module Days.Day1(run) where

run :: String -> String
run s = let weights = map read (lines s) in
    show (sum . map cost $ weights) ++ ", " ++ show (sum . map trueCost $ weights)

cost :: Int -> Int 
cost w = (div w 3) - 2

trueCost :: Int -> Int 
trueCost w | cost w <= 0 = 0
           | otherwise   = (cost w) + trueCost (cost w)

