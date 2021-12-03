module Day02 (
  day02
) where

import Common

data Position = Position { forward :: Int, depth :: Int, aim :: Int } deriving (Show)

type PositionUpdate = (Int, Int, Int)

day02 :: AOCSolution
day02 input = map answer [p1, p2]
  where
    answer p = show $ forward p * depth p
    p1 = foldl part1 initPosition i
    p2 = foldl part2 initPosition i
    initPosition = Position 0 0 0
    i = linesWords input

part1 :: Position -> [String] -> Position
part1 p [x, y] = updatePosition p $ case x of
  "forward" -> (z,  0, 0)
  "down"    -> (0,  z, 0)
  "up"      -> (0, -z, 0)
  where z = read y

part2 :: Position -> [String] -> Position
part2 p [x, y] = updatePosition p $ case x of
  "forward" -> (z, z * aim p, 0)
  "down"    -> (0, 0,  z)
  "up"      -> (0, 0, -z)
  where z = read y

updatePosition :: Position -> PositionUpdate -> Position
updatePosition p (x, y, z) = Position f d a
  where
    f = forward p + x
    d = depth p + y
    a = aim p + z