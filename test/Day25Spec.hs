module Day25Spec ( spec ) where

import SpecHelper

testInput = "v...>>.vv>\n\
            \.vv>>.vv..\n\
            \>>.>v>...v\n\
            \>>v>>.>.v.\n\
            \v>v.vv.v..\n\
            \>.>>..v...\n\
            \.vv..>.>v.\n\
            \v.v..>>v.v\n\
            \....v..v.>"
            
spec :: Spec
spec = describe "Day 25" $ do
  it "Sample" $ do
    day25 testInput `shouldBe` ["58"]

  it "Actual" $ do
    withFile "inputs/day25.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day25 actualInput `shouldBe` ["400"])