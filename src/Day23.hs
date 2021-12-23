module Day23 (
  day23
) where

import Common
import Control.Arrow (first, second)
import Control.Monad (guard)
import Data.Char (isLetter)
import Data.List (transpose)
import Data.Maybe (fromJust)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Map as M

type Rooms     = M.Map Int [Char]
type Corridors = M.Map Int Char
type Burrow    = (Rooms, Corridors)

day23 :: AOCSolution
day23 input = show . fromJust . go <$> [p1, p2]
  where
    p1  = parseInput input
    go i = dijkstra g (PQ.singleton 0 (m, M.empty)) M.empty
      where
        m = M.fromList i
        g = goal $ length $ snd $ head i
    p2 = zipWith (\(r, (a:b)) (c:[d]) -> (r, a:c:d:b)) p1 ["DD", "CB", "BA", "AC"]

parseInput :: String -> [(Int, String)]
parseInput = zip [2,4..] .
  transpose .
  map f .
  take 2 .
  drop 2 .
  lines
  where
    g i =  i
    f [] = []
    f (x:xs) = if isLetter x
      then x : f xs
      else f xs

nextStates :: (Int, Burrow) -> [(Int, Burrow)]
nextStates (currentCost, (rooms, corridors)) = map (first (+ currentCost)) $ concat $ roomMoves ++ corridorMoves
  where
    blocks :: Int -> (Int, Int)
    blocks i = (b maximum (-1) (>), b minimum 11 (<))
      where
        b m j c = m $ j : filter (c i) (map fst $ M.toList corridors)

    roomMoves :: [[(Int, Burrow)]]
    roomMoves = do
      (ri, xss@(x:xs)) <- M.toList rooms
      -- check to make sure we aren't in the correct room
      guard $ any (/= x) xs || room x /= ri
      let
        -- work out how many steps to get out of this room
        p = 2 - length xs
        -- what corridor places block to the right?
        (bl, br) = blocks ri
        -- work out next room state
        rooms' = M.insert ri xs rooms
        -- find possible corridor moves
        cs = moveToCorridor (rooms', corridors) x p ri bl br
        -- can we move to our own room?
        mr = moveToRoom (rooms', corridors) x p ri bl br 
      return $ mr ++ cs

    corridorMoves :: [[(Int, Burrow)]]
    corridorMoves = do
      (ci, x) <- M.toList corridors
      let
        (bl, br) = blocks ci
        -- work out next corridor state
        corridors' = M.delete ci corridors
        -- find possible corridor move
        cs = moveToCorridor (rooms, corridors') x 0 ci bl br
        -- can we move to our own room?
        mr = moveToRoom (rooms, corridors') x 0 ci bl br
      return $ mr ++ cs

    moveToCorridor :: Burrow -> Char -> Int -> Int -> Int -> Int -> [(Int, Burrow)]
    moveToCorridor (rooms, corridors) x y i bl br = do
      h <- corridorspots
      guard $ h > bl && h < br && h /= i
      let moveCost     = (abs (i - h) + y) * cost x
          newCorridors = M.insert h x corridors
      return (moveCost, (rooms, newCorridors))

    moveToRoom :: Burrow -> Char -> Int -> Int -> Int -> Int -> [(Int, Burrow)]
    moveToRoom (rooms, corridors) x y i bl br
      | length rs == 2         = []
      | ri < i && bl > ri      = []
      | ri > i && br < ri      = []
      | length rs == 0         = [(moveCost, burrow)]
      | (room $ head rs) /= ri = []
      | otherwise              = [(moveCost, burrow)]
      where
        ri       = room x
        rs       = rooms M.! room x
        p        = 2 - length rs
        moveCost = (abs (i - ri) + p + y) * cost x
        burrow   = (M.insert ri (x:rs) rooms, corridors)

goal :: Int -> Burrow
goal n = (M.fromList $ zip [2,4..] $ map (replicate n) ['A'..'D'], M.empty)

cost :: Char -> Int
cost = \case
  'A' ->    1
  'B' ->   10
  'C' ->  100
  'D' -> 1000

corridorspots :: [Int]
corridorspots = 0 : [1,3..9] ++ [10]

room :: Char -> Int
room = \case
  'A' -> 2
  'B' -> 4
  'C' -> 6
  'D' -> 8

dijkstra :: Burrow -> PQ.MinPQueue Int Burrow -> M.Map Burrow Int -> Maybe Int
dijkstra target pq pc
  | PQ.null pq  = Nothing
  | n == target = Just cost
  | otherwise   = dijkstra target pq'' pc'
  where
    (node@(cost, n), pq') = PQ.deleteFindMin pq
    ns   = filter (\(e, v) -> maybe True (e <) $ M.lookup v pc) $ nextStates node
    pc'  = foldr (uncurry (flip M.insert)) pc ns
    pq'' = foldr (uncurry PQ.insert) pq' ns