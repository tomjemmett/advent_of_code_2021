module Day05Spec ( spec ) where

import SpecHelper

testInput = "0,9 -> 5,9\n\
            \8,0 -> 0,8\n\
            \9,4 -> 3,4\n\
            \2,2 -> 2,1\n\
            \7,0 -> 7,4\n\
            \6,4 -> 2,0\n\
            \0,9 -> 2,9\n\
            \3,4 -> 1,4\n\
            \0,0 -> 8,8\n\
            \5,5 -> 8,2"

spec :: Spec
spec = describe "Day 05" $ do
  it "Sample" $ do
    day05 testInput `shouldBe` ["5", "12"]

  it "Actual" $ do
    withFile "inputs/day05.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day05 actualInput `shouldBe` ["6856","20666"])
