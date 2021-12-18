module Day18Spec ( spec ) where

import SpecHelper

testInput = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
            \[[[5,[2,8]],4],[5,[[9,9],0]]]\n\
            \[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
            \[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
            \[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
            \[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
            \[[[[5,4],[7,7]],8],[[8,3],8]]\n\
            \[[9,3],[[9,9],[6,[4,9]]]]\n\
            \[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
            \[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

spec :: Spec
spec = describe "Day 18" $ do
  it "Sample" $ do
    day18 testInput `shouldBe` ["4140", "3993"]

  it "Actual" $ do
    withFile "inputs/day18.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day18 actualInput `shouldBe` ["3574","4763"])