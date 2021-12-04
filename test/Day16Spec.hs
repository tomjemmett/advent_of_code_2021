module Day16Spec ( spec ) where

import SpecHelper

testInput = unlines []

spec :: Spec
spec = describe "Day 16" $ do
  it "Sample" $ do
    day16 testInput `shouldBe` ["", ""]

{-
  it "Actual" $ do
    withFile "inputs/day16.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day16 actualInput `shouldBe` ["",""])
-}