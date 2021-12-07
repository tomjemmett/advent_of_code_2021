module Main where

import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import Days

main :: IO ()
main = do
  runday day01 "inputs/day01.txt"
  runday day02 "inputs/day02.txt"
  runday day03 "inputs/day03.txt"
  runday day04 "inputs/day04.txt"
  runday day05 "inputs/day05.txt"
  runday day06 "inputs/day06.txt"
  runday day07 "inputs/day07.txt"

runday :: (String -> [String]) -> String -> IO()
runday fn file = do
  withFile file ReadMode (\h -> do
    putStrLn $ replicate 80 '-'
    let day = read . take 2 . drop 10 $ file :: Int
    putStrLn $ "Day: " ++ show day

    contents <- hGetContents h
    putStr $ unlines $ fn contents
    putStrLn "")
