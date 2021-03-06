module Day20Spec ( spec ) where

import SpecHelper

testInput = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\
            \\n\
            \#..#.\n\
            \#....\n\
            \##..#\n\
            \..#..\n\
            \..###"

spec :: Spec
spec = describe "Day 20" $ do
  it "Sample" $ do
    day20 testInput `shouldBe` ["35", "3351"]

  it "Actual" $ do
    withFile "inputs/day20.txt" ReadMode (\h -> do
      actualInput <- hGetContents h
      day20 actualInput `shouldBe` ["5786","16757"])