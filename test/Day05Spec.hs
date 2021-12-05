module Day05Spec ( spec ) where

import SpecHelper

testInput = unlines [
  "0,9 -> 5,9",
  "8,0 -> 0,8",
  "9,4 -> 3,4",
  "2,2 -> 2,1",
  "7,0 -> 7,4",
  "6,4 -> 2,0",
  "0,9 -> 2,9",
  "3,4 -> 1,4",
  "0,0 -> 8,8",
  "5,5 -> 8,2"]

spec :: Spec
spec = describe "Day 05" $ do
  it "Sample" $ do
    day05 testInput `shouldBe` ["5", "12"]

  it "Actual" $ do
    withFile "inputs/day05.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day05 actualInput `shouldBe` ["6856","20666"])
