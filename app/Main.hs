module Main where

import System.Environment

import Switcher

main :: IO ()
main = do
  day <- head <$> getArgs
  input <- readFile $ "data/day" ++ day ++ ".txt"
  let output = Switcher.runDay (read day) input
  putStrLn output 
