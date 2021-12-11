module Day11 (
  day11
) where

import Common
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad.State (State, state, evalState)
import Control.Monad (liftM2)

type D11Grid = M.Map Point2d Int

day11 :: AOCSolution
day11 input = map show $ [p1, p2] <*> pure i
  where
    i = evalState (repeatM step) $ parseInput input
    -- for part 1 we need to take the first 101 iterations, as iterate includes the initial state
    p1 = sum . take 100
    -- for part 2 we simply need to iterate until we find the state where all cells flashed. we can then take the length
    -- of the list to give us the number of iterations perfomed
    p2 = succ . length . takeWhile (< 100)

-- convert the input into a Map of Point2d to Int, e.g. the cartesian coordinate in the grid is the key, the value is
-- the cell's current value
parseInput :: String -> D11Grid
parseInput input = M.fromList $ zip p $ concat i
  where
    i = map2 digitToInt $ lines input
    p = concat $ (zipWith3 . zipWith3) (curry const) (repeat [0..]) (map repeat [0..]) i

-- each step takes a tuple of a grid and a number (which is ignored), and returns the modified grid and the number of
-- flashes that occured on that step
step :: State D11Grid Int
step = state $ \g -> substep (M.map succ g) []

-- if at any substep we have no new values greater than 9 then we stop
-- otherwise, we iterate with the mutated grid and the list of keys appended to already visited keys
substep :: D11Grid -> [Point2d] -> (Int, D11Grid)
substep g v = if null k then (f, g0) else substep g' (v ++ k)
  where
    -- find all of the items in the map which are greater than 9,
    -- but remove the items we have already visited this step
    k  = M.keys (M.filter (> 9) g) \\ v
    -- find the neighbours of those found above...
    n  = concatMap point2dNeighboursDiags k \\ v
    -- ... and increment their values by one
    g' = foldl (flip $ M.adjust succ) g n
    
    -- if we find no keys in this substep, then we are done.
    -- we need to reset any values that are greater than 9 to 0
    g0 = foldl (flip $ M.adjust $ const 0) g v
    -- and count how many zeroes there are, that is how many flashed
    f  = M.size $ M.filter (== 0) g0

repeatM :: (Monad m) => m a -> m [a]
repeatM x = let go = liftM2 (:) x go in go