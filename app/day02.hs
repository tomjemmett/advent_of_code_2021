module Day02 (
  day02
) where

data Position = Position { forward :: Int, depth :: Int, aim :: Int } deriving (Show)

type PositionUpdate = (Int, Int, Int)

day02 :: String -> String
day02 input = unlines $ map answer [p1, p2]
  where
      answer p = show $ forward p * depth p
      p1 = foldl part1 initPosition i
      p2 = foldl part2 initPosition i
      initPosition = Position 0 0 0
      i = map words $ lines input

part1 :: Position -> [String] -> Position
part1 p [x, y] = updatePosition p $ case x of
  "forward" -> (y', 0,  0)
  "down"    -> (0,  y', 0)
  "up"      -> (0, -y', 0)
  where y' = read y

part2 :: Position -> [String] -> Position
part2 p [x, y] = case x of
  "forward" -> updatePosition p (y', y' * aim p,   0)
  "down"    -> updatePosition p ( 0,          0,  y')
  "up"      -> updatePosition p ( 0,          0, -y')
  where y' = read y

updatePosition :: Position -> PositionUpdate -> Position
updatePosition p (x, y, z) = Position f d a
  where
    f = forward p + x
    d = depth p + y
    a = aim p + z