module Day12 (
  day12
) where

import Common
import Data.Char (isLower)
import Data.Function (on)
import Data.Graph
import Data.List (sortBy, groupBy)
import Data.List.Split (splitOn)
import qualified Data.HashSet as HS

type D12Graph = (Graph, Vertex -> ((), String, [String]), String -> Maybe Vertex)

day12 :: AOCSolution
day12 input = go <$> [True, False]
  where
    i = parseInput input
    go b = show $ d12 0 [("start", HS.singleton "start", b)] i

parseInput :: String -> D12Graph
parseInput = graphFromEdges .
  map (\xs -> ((), fst $ head xs, map snd xs)) .
  groupBy ((==) `on` fst) .
  sortBy (compare `on` fst) .
  concatMap (f . splitOn "-") .
  lines
  where
    f [a, b] = filter (\(x, y) -> x /= "end" && y /= "start") [(a, b), (b,a)]

d12 :: Int -> [(String, HS.HashSet String, Bool)] -> D12Graph -> Int
d12 p [] _ = p
d12 p (("end", y, _):xs) (g, v, n) = d12 (succ p) xs (g, v, n)
d12 p ((x, y, z):xs) (g, v, n) = d12 p xs' (g, v, n)
  where
    previouslyVisited = flip HS.member y
    isBigCave = not . all isLower
    (_, _, e) = maybe ((), x, []) v (n x)
    e' = filter (\x -> isBigCave x || not (previouslyVisited x) || not z) e
    z' = map (\x -> z || (not (isBigCave x) && previouslyVisited x)) e
    xs' = foldl (flip (:)) xs (zip3 e' (map (`HS.insert` y) e') z')