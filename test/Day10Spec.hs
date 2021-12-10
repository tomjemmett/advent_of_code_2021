module Day10Spec ( spec ) where

import SpecHelper

testInput = "[({(<(())[]>[[{[]{<()<>>\n\
            \[(()[<>])]({[<{<<[]>>(\n\
            \{([(<{}[<>[]}>{[]{[(<()>\n\
            \(((({<>}<{<{<>}{[]{[]{}\n\
            \[[<[([]))<([[{}[[()]]]\n\
            \[{[{({}]{}}([{[{{{}}([]\n\
            \{<[[]]>}<{[{[{[]{()[[[]\n\
            \[<(<(<(<{}))><([]([]()\n\
            \<{([([[(<>()){}]>(<<{{\n\
            \<{([{{}}[<[[[<>{}]]]>[]]"

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    day10 testInput `shouldBe` ["26397", "288957"]

  it "Actual" $ do
    withFile "inputs/day10.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day10 actualInput `shouldBe` ["265527","3969823589"])