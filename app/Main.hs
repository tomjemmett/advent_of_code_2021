module Main where

import System.IO ( hGetContents, withFile, IOMode(ReadMode) )

main :: IO ()
main = undefined

runday :: (String -> String) -> String -> IO()
runday fn file = do
  withFile file ReadMode (\h -> do
    contents <- hGetContents h
    putStr $ fn contents)