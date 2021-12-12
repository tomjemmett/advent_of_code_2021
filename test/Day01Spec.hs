module Day01Spec ( spec ) where

import SpecHelper

testInput = "199\n\
            \200\n\
            \208\n\
            \210\n\
            \200\n\
            \207\n\
            \240\n\
            \269\n\
            \260\n\
            \263"

spec :: Spec
spec = describe "Day 01" $ do
  it "Sample" $ do
      day01 testInput `shouldBe` ["7", "5"]
      
  it "Actual" $ do
    withFile "inputs/day01.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day01 actualInput `shouldBe` ["1709","1761"])