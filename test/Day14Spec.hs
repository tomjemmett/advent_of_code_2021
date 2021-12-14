module Day14Spec ( spec ) where

import SpecHelper

testInput = "NNCB\n\
            \\n\
            \CH -> B\n\
            \HH -> N\n\
            \CB -> H\n\
            \NH -> C\n\
            \HB -> C\n\
            \HC -> B\n\
            \HN -> C\n\
            \NN -> C\n\
            \BH -> H\n\
            \NC -> B\n\
            \NB -> B\n\
            \BN -> B\n\
            \BB -> N\n\
            \BC -> B\n\
            \CC -> N\n\
            \CN -> C"

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    day14 testInput `shouldBe` ["1588", "2188189693529"]

  it "Actual" $ do
    withFile "inputs/day14.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day14 actualInput `shouldBe` ["2360","2967977072188"])