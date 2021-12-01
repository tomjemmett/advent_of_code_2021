module Main where

import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import Day01

main :: IO ()
main = do
  runday day01 "inputs/day01.txt"

runday :: (String -> String) -> String -> IO()
runday fn file = do
  withFile file ReadMode (\h -> do
    contents <- hGetContents h
    putStr $ fn contents)
