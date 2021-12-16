module Day16Spec ( spec ) where

import SpecHelper

testInputs = [
  "8A004A801A8002F478",
  "620080001611562C8802118E34",
  "C0015000016115A2E0802F182340",
  "A0016C880162017C3686B18A3D4780",
  "C200B40A82",
  "04005AC33890",
  "880086C3E88112",
  "CE00C43D881120",
  "D8005AC2A8F0",
  "F600BC2D8F",
  "9C005AC2F8F0",
  "9C0141080250320F1802104A08"]

spec :: Spec
spec = describe "Day 16" $ do
  it "Sample" $ do
    (map day16 testInputs) `shouldBe` [
      ["16","15"],
      ["12","46"],
      ["23","46"],
      ["31","54"],
      ["14","3"],
      ["8","54"],
      ["15","7"],
      ["11","9"],
      ["13","1"],
      ["19","0"],
      ["16","0"],
      ["20","1"]]

  it "Actual" $ do
    withFile "inputs/day16.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day16 actualInput `shouldBe` ["920","10185143721112"])