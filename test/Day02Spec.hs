module Day02Spec ( spec ) where

import SpecHelper

testInput = "forward 5\n\
            \down 5\n\
            \forward 8\n\
            \up 3\n\
            \down 8\n\
            \forward 2"

spec :: Spec
spec = describe "Day 02" $ do
  it "Sample" $ do
    day02 testInput `shouldBe` ["150", "900"]

  it "Actual" $ do
    withFile "inputs/day02.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day02 actualInput `shouldBe` ["1484118","1463827010"])