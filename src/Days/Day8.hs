module Days.Day8 where 

import Data.Char 
import Data.Function
import Data.List
import Data.List.Split 
import Data.Ord

width :: Int 
width = 25 

height :: Int 
height = 6 

run :: String -> String 
run s = 
    let layers = parseLayers s
        bestLayer = minZeroLayer layers
        rendered = renderImage layers
    in show (checksum bestLayer) ++ "\n" ++ showImage rendered

minZeroLayer :: [[Int]] -> [Int]
minZeroLayer = head . sortBy (compare `on` length . filter (==0))

parseLayers :: String -> [[Int]]
parseLayers = chunksOf (width * height) . map digitToInt 

checksum :: [Int] -> Int 
checksum layer = (length $ filter (==1) layer) * (length $ filter (==2) layer)

renderImage :: [[Int]] -> [Int]
renderImage = map renderPixel . transpose

renderPixel :: [Int] -> Int 
renderPixel (2:xs) = renderPixel xs 
renderPixel (a:xs) = a

showImage :: [Int] -> String 
showImage pixels = 
    let rows = chunksOf width pixels 
    in unlines $ map (concatMap show) rows