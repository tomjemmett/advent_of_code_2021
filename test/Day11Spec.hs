module Day11Spec ( spec ) where

import SpecHelper

testInput = "5483143223\n\
            \2745854711\n\
            \5264556173\n\
            \6141336146\n\
            \6357385478\n\
            \4167524645\n\
            \2176841721\n\
            \6882881134\n\
            \4846848554\n\
            \5283751526"

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    day11 testInput `shouldBe` ["1656", "195"]

  it "Actual" $ do
    withFile "inputs/day11.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day11 actualInput `shouldBe` ["1697","344"])