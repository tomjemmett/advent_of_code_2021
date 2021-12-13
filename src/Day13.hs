module Day13 (
  day13
) where

import Common
import Data.List.Split (splitOn)
import qualified Data.HashSet as H
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import Data.Bifunctor (first, second)

import Data.List (sortBy, groupBy)

data FoldGrid a = FX a | FY a deriving (Show)
toFoldGrid :: Char -> Int -> FoldGrid Int
toFoldGrid 'x' n = FX n
toFoldGrid 'y' n = FY n

day13 :: AOCSolution
day13 input = [show $ H.size $ x !! 1, showOutput $ last x]
  where
    x = uncurry (scanl foldInstructions) $ parseInput input

foldInstructions :: H.HashSet Point2d -> FoldGrid Int -> H.HashSet Point2d
foldInstructions h = \case
  FX n -> f fst first n
  FY n -> f snd second n
  where
    f a b n = H.union l r
      where
        l = H.filter ((< n) . a) h
        r = H.map (b ((2 * n) -)) $ H.filter ((> n) . a) h

showOutput :: H.HashSet Point2d -> String
showOutput h = unlines [[if H.member (x, y) h then '#' else ' ' | x <- [minx..maxx]] | y <- [miny..maxy]]
  where
    [[minx, maxx], [miny, maxy]] = range <$> (H.map <$> [fst, snd] <*> pure h)
    range x = [minimum, maximum] <*> pure x

parseInput :: String -> (H.HashSet Point2d, [FoldGrid Int])
parseInput input = (a', b')
  where
    [a, b] = map lines $ splitOn "\n\n" input
    a' = H.fromList $ map ((\[x, y] -> (x, y)) . commaSeparatedInts) a
    b' = map (parse' p id) b

    p :: Parser (FoldGrid Int)
    p = do
      P.string "fold along "
      x <- P.oneOf "xy"
      P.char '='
      toFoldGrid x <$> number