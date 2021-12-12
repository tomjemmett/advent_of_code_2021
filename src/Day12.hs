module Day12 (
  day12
) where

import Common
import Data.Char (isLower, toUpper)
import Data.Function (on)
import Data.Graph
import Data.List (sortBy, groupBy, intercalate)
import Data.List.Split (splitOn)

type D12Graph = (Graph, Vertex -> ((), String, [String]), String -> Maybe Vertex)

day12 :: AOCSolution
day12 input = [show p1, show p2]
  where
    i = parseInput input
    p1 = length $ mbfs [] [("start", ["start"], True)] i
    p2 = length $ mbfs [] [("start", ["start"], False)] i

parseInput :: String -> D12Graph
parseInput = graphFromEdges .
  map (\xs -> ((), fst $ head xs, map snd xs)) .
  groupBy ((==) `on` fst) .
  sortBy (compare `on` fst) .
  concatMap (f . splitOn "-") .
  lines
  where
    f [a, b] = filter (\(x, y) -> x /= "end" && y /= "start") [(a, b), (b,a)]

mbfs :: [[String]] -> [(String, [String], Bool)] -> D12Graph -> [String]
mbfs p _ _ | length p == 50000 = map (intercalate "-" . reverse) p
mbfs p [] _ = map (intercalate "-" . reverse) p
mbfs p (("end", y, _):xs) (g, v, n) = mbfs (y:p) xs (g, v, n)
mbfs p ((x, y, z):xs) (g, v, n) = mbfs p xs' (g, v, n)
  where
    previouslyVisited = flip elem y
    isBigCave = not . all isLower
    (_, _, e) = maybe ((), x, []) v (n x)
    e' = filter (\x -> isBigCave x || not (previouslyVisited x) || not z) e
    z' = map (\ee -> z || (not (isBigCave ee) && previouslyVisited ee)) e  
    xs' = xs ++ zip3 e' (map (:y) e') z'
