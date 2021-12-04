module Day01Spec ( spec ) where

import SpecHelper

testInput = unlines ["199", "200","208","210","200","207","240","269","260","263"]

spec :: Spec
spec = describe "Day 01" $ do
  it "Sample" $ do
      day01 testInput `shouldBe` ["7", "5"]
      
  it "Actual" $ do
    withFile "inputs/day01.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day01 actualInput `shouldBe` ["1709","1761"])