module Main where

import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

main :: IO ()
main = do
  runday day01 "inputs/day01.txt"
  runday day02 "inputs/day02.txt"

runday :: (String -> String) -> String -> IO()
runday fn file = do
  withFile file ReadMode (\h -> do
    putStrLn $ replicate 80 '-'
    let day = read . take 2 . drop 10 $ file :: Int
    putStrLn $ "Day: " ++ show day

    contents <- hGetContents h
    putStr $ fn contents
    putStrLn "")
