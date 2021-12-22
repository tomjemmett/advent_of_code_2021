module Day21 (
  day21
) where

import Common
import Data.Char (digitToInt)
import Control.Monad.State (MonadState(get), modify, evalState, replicateM, State)
import qualified Data.Map as M
import Data.Tuple (swap)

type Position = Int
type Score = Int 
type Player = (Position, Score)
type Game = (Player, Player)

day21 :: AOCSolution
day21 input = show <$> ([p1, p2] <*> i)
  where
    i = pure (parseInput input)
    p1 = part1 0
    p2 = part2

parseInput :: String -> Game
parseInput = tuplify2 . map ((,0) . digitToInt . last) . lines

movePlayer :: Position -> Int -> Position
movePlayer p n = (p - 1 + n) `mod` 10 + 1

part1 :: Int -> Game -> Int
part1 n ((p1, s1), (p2, s2))
  | s1' >= 1000 = s2  * (n + 3)
  | s2' >= 1000 = s1' * (n + 6)
  | otherwise = part1 (n + 6) ((p1', s1'), (p2', s2'))
  where
    ns = map (succ . (`mod` 100) . (+n)) [0..5]
    p1' = movePlayer p1 $ sum (take 3 ns)
    p2' = movePlayer p2 $ sum (drop 3 ns)
    s1' = s1 + p1'
    s2' = s2 + p2'

part2 :: Game -> Int
part2 startGame = max x y
  where
    (x, y) = evalState (part2' startGame) M.empty
    possibleRoles = sum <$> replicateM 3 [1, 2, 3]
    part2' :: Game -> State (M.Map Game (Int, Int)) (Int, Int)
    part2' game@((p1, s1), (p2, s2))
      | s1 >= 21 = return (1, 0)
      | s2 >= 21 = return (0, 1)
      | otherwise = do
        memoisedGames <- get
        case M.lookup game memoisedGames of
          Just r -> return r
          _      -> do
            let allOutcomes = [((p2, s2), (p1', p1' + s1)) | d <- possibleRoles, let p1' = movePlayer p1 d]
            r <- swap .
              foldl1 (\(a, b) (c, d) -> (a + c, b + d)) <$>
              mapM part2' allOutcomes
            modify (M.insert game r)
            return r