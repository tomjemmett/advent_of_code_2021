module Day07Spec ( spec ) where

import SpecHelper

testInput = "16,1,2,0,4,2,7,1,2,14"

spec :: Spec
spec = describe "Day 07" $ do
  it "Sample" $ do
    day07 testInput `shouldBe` ["37", "168"]

  it "Actual" $ do
    withFile "inputs/day07.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day07 actualInput `shouldBe` ["356992","101268110"])