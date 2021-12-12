module Day03Spec ( spec ) where

import SpecHelper

testInput = "00100\n\
            \11110\n\
            \10110\n\
            \10111\n\
            \10101\n\
            \01111\n\
            \00111\n\
            \11100\n\
            \10000\n\
            \11001\n\
            \00010\n\
            \01010"

spec :: Spec
spec = describe "Day 03" $ do
  it "Sample" $ do
    day03 testInput `shouldBe` ["198", "230"]

  it "Actual" $ do
    withFile "inputs/day03.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day03 actualInput `shouldBe` ["3309596","2981085"])