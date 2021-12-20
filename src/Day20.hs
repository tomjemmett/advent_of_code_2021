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
    -- if the algorithm has 0 = '#', then every odd iteration flips everything outside of our "image bounds"
    -- we handle this case with Just 0, which we can toggle with (1-) <$> border
    -- otherwise, the infinite points stay as they are, so we always want to use 0, so we can use fromMaybe 0 Nothing
    border = if head iea == 1
      then Just 0
      else Nothing
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
    [minX, minY, maxX, maxY] = [minimum, maximum] <*> (flip map ky <$> [fst, snd])

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
    f 0 = ' '
    f 1 = '#'