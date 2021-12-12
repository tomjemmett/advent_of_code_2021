module Day12Spec ( spec ) where

import SpecHelper

testInputA = "start-A\n\
             \start-b\n\
             \A-c\n\
             \A-b\n\
             \b-d\n\
             \A-end\n\
             \b-end"

testInputB = "dc-end\n\
             \HN-start\n\
             \start-kj\n\
             \dc-start\n\
             \dc-HN\n\
             \LN-dc\n\
             \HN-end\n\
             \kj-sa\n\
             \kj-HN\n\
             \kj-dc"

testInputC = "fs-end\n\
             \he-DX\n\
             \fs-he\n\
             \start-DX\n\
             \pj-DX\n\
             \end-zg\n\
             \zg-sl\n\
             \zg-pj\n\
             \pj-he\n\
             \RW-he\n\
             \fs-DX\n\
             \pj-RW\n\
             \zg-RW\n\
             \start-pj\n\
             \he-WI\n\
             \zg-he\n\
             \pj-fs\n\
             \start-RW"

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    day12 testInputA `shouldBe` [ "10",   "36"]
    day12 testInputB `shouldBe` [ "19",  "103"]
    day12 testInputC `shouldBe` ["226", "3509"]

  it "Actual" $ do
    withFile "inputs/day12.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day12 actualInput `shouldBe` ["5457", "128506"])