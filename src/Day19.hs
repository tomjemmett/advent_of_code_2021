-- module Day19 (
--   day19
-- ) where
module Day19 where

import Common
import Control.Arrow (second)
import Control.Monad (guard)
import Data.Function (on)
import Data.List (sort, transpose, sortBy, groupBy, nub)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, isJust, fromJust)
import qualified Data.Map as M

type Scanner = [Point3d]
type DistanceMap = M.Map Point3d (M.Map Int Point3d)
type AlignedPoints = (Point3d, Point3d, Point3d, Point3d)
type Transformation = (Rotation, Point3d)
type Rotation = (Point3d -> Point3d)

day19 :: AOCSolution
day19 input = show <$> [p1, p2]
  where
    i = parseInput input
    r = solve i [] []
    p1 = length $ fst $ r
    p2 = maximum $ [manhattenDistance x y | x <- snd $ r, y <- snd $ r]

parseInput :: String -> [Scanner]
parseInput = map2 (tuplify3 . commaSeparatedInts) . map (tail . lines) . splitOn "\n\n"

manhattenDistance :: Point3d -> Point3d -> Int
manhattenDistance (x0, y0, z0) (x1, y1, z1) = sum $ map abs [x0 - x1, y0 - y1, z0 - z1]

subtractPoint3d ::Point3d -> Point3d -> Point3d
subtractPoint3d (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

addPoint3d :: Point3d -> Point3d -> Point3d
addPoint3d (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

createMap :: Scanner -> DistanceMap
createMap scanner = M.fromList [(s1, M.fromList [ (manhattenDistance s1 s2, s2) | s2 <- scanner, s1 /= s2]) | s1 <- scanner]

search :: Scanner -> Scanner -> [AlignedPoints]
search s0 s1 =
  concatMap snd $
  filter ((>= 12) . length . snd) $
  M.toList $
  M.fromListWith (++) $ do
    (ka, va) <- M.toList m0
    (kb, vb) <- M.toList m1

    as <- M.keys va
    guard $ M.member as vb

    return (ka, [(ka, va M.! as, kb, vb M.! as)])
  where
    m0 = createMap s0
    m1 = createMap s1

find :: Scanner -> Scanner -> Maybe (Scanner, Point3d)
find s0 s1 = go $ search s0 s1
  where
    go :: [AlignedPoints] -> Maybe (Scanner, Point3d)
    go [] = Nothing
    go (x:xs) = if isJust t && countTrue (`elem` s0) s1' >= 12
      then Just (s1', snd t')
      else go xs
      where
        t   = getTransformation x
        t'  = fromJust t
        s1' = map (applyTransformation t') s1

getTransformation :: AlignedPoints -> Maybe Transformation
getTransformation (a, b, c, d) = case whichRot (a, b, c, d) of
  Just r' -> Just (r', subtractPoint3d a (r' c)) 
  Nothing -> Nothing
    
applyTransformation :: Transformation -> Point3d -> Point3d
applyTransformation (r, t) p = addPoint3d (r p) t

whichRot :: AlignedPoints -> Maybe Rotation
whichRot (a, b, c, d)
  | x0 ==  x1 && y0 ==  y1 && z0 ==  z1 = Just (\(x, y, z) -> ( x,  y,  z))
  | x0 == -x1 && y0 ==  y1 && z0 ==  z1 = Just (\(x, y, z) -> (-x,  y,  z))
  | x0 ==  x1 && y0 == -y1 && z0 ==  z1 = Just (\(x, y, z) -> ( x, -y,  z))
  | x0 == -x1 && y0 == -y1 && z0 ==  z1 = Just (\(x, y, z) -> (-x, -y,  z))
  | x0 ==  x1 && y0 ==  y1 && z0 == -z1 = Just (\(x, y, z) -> ( x,  y, -z))
  | x0 == -x1 && y0 ==  y1 && z0 == -z1 = Just (\(x, y, z) -> (-x,  y, -z))
  | x0 ==  x1 && y0 == -y1 && z0 == -z1 = Just (\(x, y, z) -> ( x, -y, -z))
  | x0 == -x1 && y0 == -y1 && z0 == -z1 = Just (\(x, y, z) -> (-x, -y, -z))

  | x0 ==  x1 && y0 ==  z1 && z0 ==  y1 = Just (\(x, y, z) -> ( x,  z,  y))
  | x0 == -x1 && y0 ==  z1 && z0 ==  y1 = Just (\(x, y, z) -> (-x,  z,  y))
  | x0 ==  x1 && y0 == -z1 && z0 ==  y1 = Just (\(x, y, z) -> ( x, -z,  y))
  | x0 == -x1 && y0 == -z1 && z0 ==  y1 = Just (\(x, y, z) -> (-x, -z,  y))
  | x0 ==  x1 && y0 ==  z1 && z0 == -y1 = Just (\(x, y, z) -> ( x,  z, -y))
  | x0 == -x1 && y0 ==  z1 && z0 == -y1 = Just (\(x, y, z) -> (-x,  z, -y))
  | x0 ==  x1 && y0 == -z1 && z0 == -y1 = Just (\(x, y, z) -> ( x, -z, -y))
  | x0 == -x1 && y0 == -z1 && z0 == -y1 = Just (\(x, y, z) -> (-x, -z, -y))

  | x0 ==  y1 && y0 ==  x1 && z0 ==  z1 = Just (\(x, y, z) -> ( y,  x,  z))
  | x0 == -y1 && y0 ==  x1 && z0 ==  z1 = Just (\(x, y, z) -> (-y,  x,  z))
  | x0 ==  y1 && y0 == -x1 && z0 ==  z1 = Just (\(x, y, z) -> ( y, -x,  z))
  | x0 == -y1 && y0 == -x1 && z0 ==  z1 = Just (\(x, y, z) -> (-y, -x,  z))
  | x0 ==  y1 && y0 ==  x1 && z0 == -z1 = Just (\(x, y, z) -> ( y,  x, -z))
  | x0 == -y1 && y0 ==  x1 && z0 == -z1 = Just (\(x, y, z) -> (-y,  x, -z))
  | x0 ==  y1 && y0 == -x1 && z0 == -z1 = Just (\(x, y, z) -> ( y, -x, -z))
  | x0 == -y1 && y0 == -x1 && z0 == -z1 = Just (\(x, y, z) -> (-y, -x, -z))

  | x0 ==  y1 && y0 ==  z1 && z0 ==  x1 = Just (\(x, y, z) -> ( y,  z,  x))
  | x0 == -y1 && y0 ==  z1 && z0 ==  x1 = Just (\(x, y, z) -> (-y,  z,  x))
  | x0 ==  y1 && y0 == -z1 && z0 ==  x1 = Just (\(x, y, z) -> ( y, -z,  x))
  | x0 == -y1 && y0 == -z1 && z0 ==  x1 = Just (\(x, y, z) -> (-y, -z,  x))
  | x0 ==  y1 && y0 ==  z1 && z0 == -x1 = Just (\(x, y, z) -> ( y,  z, -x))
  | x0 == -y1 && y0 ==  z1 && z0 == -x1 = Just (\(x, y, z) -> (-y,  z, -x))
  | x0 ==  y1 && y0 == -z1 && z0 == -x1 = Just (\(x, y, z) -> ( y, -z, -x))
  | x0 == -y1 && y0 == -z1 && z0 == -x1 = Just (\(x, y, z) -> (-y, -z, -x))

  | x0 ==  z1 && y0 ==  x1 && z0 ==  y1 = Just (\(x, y, z) -> ( z,  x,  y))
  | x0 == -z1 && y0 ==  x1 && z0 ==  y1 = Just (\(x, y, z) -> (-z,  x,  y))
  | x0 ==  z1 && y0 == -x1 && z0 ==  y1 = Just (\(x, y, z) -> ( z, -x,  y))
  | x0 == -z1 && y0 == -x1 && z0 ==  y1 = Just (\(x, y, z) -> (-z, -x,  y))
  | x0 ==  z1 && y0 ==  x1 && z0 == -y1 = Just (\(x, y, z) -> ( z,  x, -y))
  | x0 == -z1 && y0 ==  x1 && z0 == -y1 = Just (\(x, y, z) -> (-z,  x, -y))
  | x0 ==  z1 && y0 == -x1 && z0 == -y1 = Just (\(x, y, z) -> ( z, -x, -y))
  | x0 == -z1 && y0 == -x1 && z0 == -y1 = Just (\(x, y, z) -> (-z, -x, -y))

  | x0 ==  z1 && y0 ==  y1 && z0 ==  x1 = Just (\(x, y, z) -> ( z,  y,  x))
  | x0 == -z1 && y0 ==  y1 && z0 ==  x1 = Just (\(x, y, z) -> (-z,  y,  x))
  | x0 ==  z1 && y0 == -y1 && z0 ==  x1 = Just (\(x, y, z) -> ( z, -y,  x))
  | x0 == -z1 && y0 == -y1 && z0 ==  x1 = Just (\(x, y, z) -> (-z, -y,  x))
  | x0 ==  z1 && y0 ==  y1 && z0 == -x1 = Just (\(x, y, z) -> ( z,  y, -x))
  | x0 == -z1 && y0 ==  y1 && z0 == -x1 = Just (\(x, y, z) -> (-z,  y, -x))
  | x0 ==  z1 && y0 == -y1 && z0 == -x1 = Just (\(x, y, z) -> ( z, -y, -x))
  | x0 == -z1 && y0 == -y1 && z0 == -x1 = Just (\(x, y, z) -> (-z, -y, -x))

  | otherwise = Nothing
  where
    (x0, y0, z0) = subtractPoint3d a b
    (x1, y1, z1) = subtractPoint3d c d

solve :: [Scanner] -> [Scanner] -> [Point3d] -> ([Point3d], [Point3d])
solve [x] [] scanners = (x, scanners)
solve (x:y:xs) st scanners = case r of
  Just _  -> solve (x':xs ++ st) [] (t:scanners)
  Nothing -> solve (x:xs) (y:st) scanners
  where
    r  = find x y
    (y', t) = fromJust r
    x' = nub $ x ++ y'