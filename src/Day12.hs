module Day12 (
  day12
) where

import Common
import Data.Char (isLower, toUpper)
import Data.Function (on)
import Data.Graph
import Data.List (sortBy, groupBy, intercalate)
import Data.List.Split (splitOn)
import qualified Data.HashSet as HS

type D12Graph = (Graph, Vertex -> ((), String, [String]), String -> Maybe Vertex)

day12 :: AOCSolution
day12 input = [show p1, show p2]
  where
    i = parseInput input
    p1 = mbfs 0 [("start", HS.singleton "start", True)] i
    p2 = mbfs 0 [("start", HS.singleton "start", False)] i

parseInput :: String -> D12Graph
parseInput = graphFromEdges .
  map (\xs -> ((), fst $ head xs, map snd xs)) .
  groupBy ((==) `on` fst) .
  sortBy (compare `on` fst) .
  concatMap (f . splitOn "-") .
  lines
  where
    f [a, b] = filter (\(x, y) -> x /= "end" && y /= "start") [(a, b), (b,a)]

mbfs :: Int -> [(String, HS.HashSet String, Bool)] -> D12Graph -> Int
mbfs p [] _ = p
mbfs p (("end", y, _):xs) (g, v, n) = mbfs (succ p) xs (g, v, n)
mbfs p ((x, y, z):xs) (g, v, n) = mbfs p xs' (g, v, n)
  where
    previouslyVisited = flip HS.member y
    isBigCave = not . all isLower
    (_, _, e) = maybe ((), x, []) v (n x)
    e' = filter (\x -> isBigCave x || not (previouslyVisited x) || not z) e
    z' = map (\x -> z || (not (isBigCave x) && previouslyVisited x)) e
    xs' = foldl (flip (:)) xs (zip3 e' (map (`HS.insert` y) e') z')