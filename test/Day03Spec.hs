module Day03Spec ( spec ) where

import SpecHelper

testInput = unlines [
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"]

spec :: Spec
spec = describe "Day 03" $ do
  it "Sample" $ do
    day03 testInput `shouldBe` ["198", "230"]

  it "Actual" $ do
    withFile "inputs/day03.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day03 actualInput `shouldBe` ["3309596","2981085"])