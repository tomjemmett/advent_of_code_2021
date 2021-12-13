module Day13Spec ( spec ) where

import SpecHelper

testInput = "6,10\n\
            \0,14\n\
            \9,10\n\
            \0,3\n\
            \10,4\n\
            \4,11\n\
            \6,0\n\
            \6,12\n\
            \4,1\n\
            \0,13\n\
            \10,12\n\
            \3,4\n\
            \3,0\n\
            \8,4\n\
            \1,10\n\
            \2,14\n\
            \8,10\n\
            \9,0\n\
            \\n\
            \fold along y=7\n\
            \fold along x=5"

p2TestOutput = "\
\#####\n\
\#   #\n\
\#   #\n\
\#   #\n\
\#####\n"

p2ActualOutput = "\
\ ##  #  #  ##   ##  ###   ##   ##  #  #\n\
\#  # #  # #  # #  # #  # #  # #  # #  #\n\
\#  # #### #    #    #  # #    #  # #  #\n\
\#### #  # # ## #    ###  # ## #### #  #\n\
\#  # #  # #  # #  # #    #  # #  # #  #\n\
\#  # #  #  ###  ##  #     ### #  #  ## \n"

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    day13 testInput `shouldBe` ["17", p2TestOutput]

  it "Actual" $ do
    withFile "inputs/day13.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day13 actualInput `shouldBe` ["850", p2ActualOutput])