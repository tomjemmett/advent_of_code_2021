module Day02Spec ( spec ) where

import SpecHelper

testInput = unlines [
  "forward 5",
  "down 5",
  "forward 8",
  "up 3",
  "down 8",
  "forward 2"]

spec :: Spec
spec = describe "Day 02" $ do
  it "Sample" $ do
    day02 testInput `shouldBe` ["150", "900"]

  it "Actual" $ do
    withFile "inputs/day02.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day02 actualInput `shouldBe` ["1484118","1463827010"])