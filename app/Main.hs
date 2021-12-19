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
  runday day08 "inputs/day08.txt"
  runday day09 "inputs/day09.txt"
  runday day10 "inputs/day10.txt"
  runday day11 "inputs/day11.txt"
  runday day12 "inputs/day12.txt"
  runday day13 "inputs/day13.txt"
  runday day14 "inputs/day14.txt"
  runday day15 "inputs/day15.txt"
  runday day16 "inputs/day16.txt"
  runday day17 "inputs/day17.txt"
  runday day18 "inputs/day18.txt"
  runday day19 "inputs/day19.txt"
  -- runday day20 "inputs/day20.txt"
  -- runday day21 "inputs/day21.txt"
  -- runday day22 "inputs/day22.txt"
  -- runday day23 "inputs/day23.txt"
  -- runday day24 "inputs/day24.txt"
  -- runday day25 "inputs/day25.txt"

runday :: (String -> [String]) -> String -> IO()
runday fn file = do
  withFile file ReadMode (\h -> do
    putStrLn $ replicate 80 '-'
    let day = read . take 2 . drop 10 $ file :: Int
    putStrLn $ "Day: " ++ show day

    contents <- hGetContents h
    putStr $ unlines $ fn contents
    putStrLn "")
