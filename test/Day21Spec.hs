module Day21Spec ( spec ) where

import SpecHelper

testInput = "Player 1 starting position: 4\n\
            \Player 2 starting position: 8"

spec :: Spec
spec = describe "Day 21" $ do
  it "Sample" $ do
    day21 testInput `shouldBe` ["739785", "444356092776315"]

  it "Actual" $ do
    withFile "inputs/day21.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day21 actualInput `shouldBe` ["989352","430229563871565"])