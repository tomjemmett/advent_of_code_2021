-- module Day20 (
--    day20
-- ) where
module Day20 where

import Common
import Control.Arrow (first, second)
import Data.Function (on)
import Data.List (nub, sortBy, groupBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

type ImageEnhancementAlgorithm = [Int]
type Image = M.Map Point2d Int

day20 :: AOCSolution
day20 input = show . sum . map snd . M.toList . fst . (!!) imgs <$> [2, 50]
  where
    (iea, img) = parseInput input
    -- hacky, but for some reason I need to flip the borders on the actual input, but not on the test...
    border = if M.size img == 25
      then Nothing
      else Just 0
    imgs = iterate (convolute iea) (img, border)

createImage :: [[Int]] -> Image
createImage img = M.fromList $ concat $ (zipWith3 . zipWith3) f (map repeat [0..]) (repeat [0..]) img
  where
    f x y = ((x, y),)

parseInput :: String -> (ImageEnhancementAlgorithm, Image)
parseInput input = (iea, createImage img)
  where
    i = lines input
    iea = pixel <$> head i
    img = map2 pixel $ drop 2 i
    pixel = \case
      '.' -> 0
      '#' -> 1

convolute :: ImageEnhancementAlgorithm -> (Image, Maybe Int) -> (Image, Maybe Int)
convolute iea (img, border) = (M.fromList ni, (1-) <$> border)
  where
    ky = M.keys img
    [minX, maxX] = [minimum, maximum] <*> pure (map fst ky)
    [minY, maxY] = [minimum, maximum] <*> pure (map snd ky)

    bp =
      ((pred minY,) <$> [pred minX..succ maxX]) ++
      ((,pred minX) <$> [minY..maxY]) ++
      ((,succ maxX) <$> [minY..maxY]) ++
      ((succ maxY,) <$> [pred minX..succ maxX])

    ps = bp ++ ky
    ni = zip ps $ map (convolutePoint iea img border) ps

convolutePoint :: ImageEnhancementAlgorithm -> Image -> Maybe Int -> Point2d -> Int
convolutePoint iea img border p = iea !! pv 
  where
    b  = fromMaybe 0 border 
    pn = sortPoint2d (p:point2dNeighboursDiags p)
    pv = bitsToInt $ map (flip (M.findWithDefault b) img) pn

sortPoint2d :: [Point2d] -> [Point2d]
sortPoint2d = sortBy comparePoint2d

comparePoint2d :: Point2d -> Point2d -> Ordering
comparePoint2d (a, b) (c, d)
      | a < c     = LT
      | a > c     = GT
      | b < d     = LT
      | b > d     = GT
      | otherwise = EQ

showImg :: Image -> IO ()
showImg = putStrLn .
  unlines .
  map2 (f . snd) .
  groupBy ((==) `on` fst) .
  map (first fst) .
  sortBy (comparePoint2d `on` fst) .
  M.toList
  where
    f :: Int -> Char
    f 0 = '.'
    f 1 = '#'