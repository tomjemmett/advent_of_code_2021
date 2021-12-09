module Day09Spec ( spec ) where

import SpecHelper

testInput = "2199943210\n\
            \3987894921\n\
            \9856789892\n\
            \8767896789\n\
            \9899965678"

spec :: Spec
spec = describe "Day 09" $ do
  it "Sample" $ do
    day09 testInput `shouldBe` ["15", "1134"]

  it "Actual" $ do
    withFile "inputs/day09.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day09 actualInput `shouldBe` ["600","987840"])