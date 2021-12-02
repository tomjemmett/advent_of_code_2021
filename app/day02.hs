module Day02 (
  day02
) where

data Position = Position { forward :: Int, depth :: Int, aim :: Int } deriving (Show)

day02 :: String -> String
day02 input = unlines $ map answer [p1, p2]
  where
      answer p = show $ forward p * depth p
      p1 = foldl parseLineP1 initPosition i
      p2 = foldl parseLineP2 initPosition i
      initPosition = Position 0 0 0
      i = map words $ lines input

parseLineP1 :: Position -> [String] -> Position
parseLineP1 p ["forward", x] = updatePosition p (read x,  0, 0)
parseLineP1 p ["down",    x] = updatePosition p (0,  read x, 0)
parseLineP1 p ["up",      x] = updatePosition p (0, -read x, 0)

parseLineP2 :: Position -> [String] -> Position
parseLineP2 p ["forward", x] = updatePosition p (read x, read x * aim p, 0)
parseLineP2 p ["down",    x] = updatePosition p (0, 0,  read x)
parseLineP2 p ["up",      x] = updatePosition p (0, 0, -read x)

updatePosition :: Position -> (Int, Int, Int) -> Position
updatePosition p (x, y, z) = Position f d a
  where
    f = forward p + x
    d = depth p + y
    a = aim p + z