module Day24Spec ( spec ) where

import SpecHelper

spec :: Spec
spec = describe "Day 24" $ do

  -- no sample for day 24!
  it "Actual" $ do
    withFile "inputs/day24.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day24 actualInput `shouldBe` ["91599994399395","71111591176151"])