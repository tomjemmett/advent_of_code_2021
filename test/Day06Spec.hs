module Day06Spec ( spec ) where

import SpecHelper
import Data.List (sort)

testInput = "3,4,3,1,2"

spec :: Spec
spec = describe "Day 06" $ do
  it "Sample" $ do
    day06 testInput `shouldBe` ["5934", "26984457539"]

  it "Actual" $ do
    withFile "inputs/day06.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day06 actualInput `shouldBe` ["388419","1740449478328"])