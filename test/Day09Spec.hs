module Day09Spec ( spec ) where

import SpecHelper

testInput = unlines []

spec :: Spec
spec = describe "Day 09" $ do
  it "Sample" $ do
    day09 testInput `shouldBe` ["", ""]

{-
  it "Actual" $ do
    withFile "inputs/day09.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day09 actualInput `shouldBe` ["",""])
-}