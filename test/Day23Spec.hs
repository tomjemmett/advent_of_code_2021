module Day23Spec ( spec ) where

import SpecHelper

testInput = "#############\n\
            \#...........#\n\
            \###B#C#B#D###\n\
            \  #A#D#C#A#  \n\
            \  #########  "

spec :: Spec
spec = describe "Day 23" $ do
  it "Sample" $ do
    day23 testInput `shouldBe` ["12521", "44169"]

  it "Actual" $ do
    withFile "inputs/day23.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day23 actualInput `shouldBe` ["18195","50265"])