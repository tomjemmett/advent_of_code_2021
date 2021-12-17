module Day17Spec ( spec ) where

import SpecHelper

testInput = "target area: x=20..30, y=-10..-5"

spec :: Spec
spec = describe "Day 17" $ do
  it "Sample" $ do
    day17 testInput `shouldBe` ["45", "112"]

  it "Actual" $ do
    withFile "inputs/day17.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day17 actualInput `shouldBe` ["6903","2351"])