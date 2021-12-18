module Day15Spec ( spec ) where

import SpecHelper

testInput = "1163751742\n\
            \1381373672\n\
            \2136511328\n\
            \3694931569\n\
            \7463417111\n\
            \1319128137\n\
            \1359912421\n\
            \3125421639\n\
            \1293138521\n\
            \2311944581"
            
spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    day15 testInput `shouldBe` ["40", "315"]

  it "Actual" $ do
    withFile "inputs/day15.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day15 actualInput `shouldBe` ["687","2957"])