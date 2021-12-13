-- module Day13 (
--   day13
-- ) where
module Day13 where

import Common
import Data.List.Split (splitOn)
import qualified Data.HashSet as H
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import Data.List (sortBy, groupBy)

day13 :: AOCSolution
day13 input = [show p1, p2]
  where
    (m, i) = parseInput input
    x = scanl foldInstructions m i
    p1 = H.size (x !! 1)
    p2 = showOutput $ last x

foldInstructions :: H.HashSet Point2d -> (Char, Int) -> H.HashSet Point2d
foldInstructions h ('x', n) = H.union l r
  where
    l = H.filter ((< n) . fst) h
    r = H.map (\(x, y) -> (2 * n - x, y)) $ H.filter ((> n) . fst) h
foldInstructions h ('y', n) = H.union t b
  where
    t = H.filter ((< n) . snd) h
    b = H.map (\(x, y) -> (x, 2 * n - y)) $ H.filter ((> n) . snd) h

showOutput :: H.HashSet Point2d -> String
showOutput h = unlines [[if H.member (x, y) h then '#' else ' ' | x <- [minx..maxx]] | y <- [miny..maxy]]
  where
    [[minx, maxx], [miny, maxy]] = range <$> (H.map <$> [fst, snd] <*> pure h)
    range x = [minimum, maximum] <*> pure x

parseInput :: String -> (H.HashSet Point2d, [(Char, Int)])
parseInput input = (a', b')
  where
    [a, b] = map lines $ splitOn "\n\n" input
    a' = H.fromList $ map ((\[x, y] -> (x, y)) . commaSeparatedInts) a
    b' = map (parse' p id) b

    p :: Parser (Char, Int)
    p = do
      P.string "fold along "
      x <- P.char 'x' <|> P.char 'y'
      P.char '='
      d <- P.many1 P.digit
      return (x, read d)