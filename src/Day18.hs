module Day18 (
  day18
) where

import Common
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

data Snailfish        = Value Int | Pair Snailfish Snailfish deriving (Show, Eq)
data SplitSnailfish   = NoSplit | Split Snailfish
data ExplodeSnailfish = NoExplosion | Exploded | Add (Int, Int) | CarryL Int | CarryR Int 

day18 :: AOCSolution
day18 input = show <$> ([part1, part2] <*> pure i)
  where
    i = map parseInput $ lines input
    part1 = magnitude . foldl1 combine
    part2 xs = maximum [magnitude $ combine a b | a <- xs, b <- xs, a /= b]
    combine :: Snailfish -> Snailfish -> Snailfish
    combine a b = fExplode $ Pair a b
      where
        fExplode sf =
          case explode 0 sf of
            (CarryL _,    s) -> fExplode s
            (CarryR _,    s) -> fExplode s
            (Exploded,    s) -> fExplode s
            (NoExplosion, _) -> fSplit sf
        fSplit sf =
          case split sf of
            Split s -> fExplode s
            NoSplit -> sf
    magnitude :: Snailfish -> Int
    magnitude (Value i) = i
    magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

parseInput :: String -> Snailfish
parseInput = parse' parsePair id
  where
    parsePair = do
      P.char '['
      a <- parseSnailfish
      P.char ','
      b <- parseSnailfish
      P.char ']'
      return $ Pair a b
    parseSnailfish = value <|> pair
      where
        pair = value <|> parsePair
        value = Value <$> number

split :: Snailfish -> SplitSnailfish
split (Value v) = if v > 9
  then Split $ Pair (Value a) (Value b)
  else NoSplit
  where
    a = v `div` 2
    b = v - a
split (Pair a b) = spa
  where
    spa = case split a of
      Split s -> Split (Pair s b)
      NoSplit -> spb
    spb = case split b of
      Split s -> Split (Pair a s)
      NoSplit -> NoSplit

explode :: Int -> Snailfish -> (ExplodeSnailfish, Snailfish)
explode _ (Value v) = (NoExplosion, Value v)
explode 4 (Pair (Value a) (Value b)) = (Add (a,b), Value 0)
explode depth (Pair a b) = explodeL
  where
    explodeL = case explode (depth + 1) a of
      (Add (l,r),   s) -> (CarryL l,    Pair s (aLt r b))
      (CarryL l,    s) -> (CarryL l,    Pair s b)
      (CarryR r,    s) -> (Exploded,    Pair s (aLt r b))
      (Exploded,    s) -> (Exploded,    Pair s b)
      (NoExplosion, _) -> explodeR
    explodeR = case explode (depth + 1) b of
      (Add (l,r),   s) -> (CarryR r,    Pair (aRt a l) s)
      (CarryL l,    s) -> (Exploded,    Pair (aRt a l) s)
      (CarryR r,    s) -> (CarryR r,    Pair a s)
      (Exploded,    s) -> (Exploded,    Pair a s)
      (NoExplosion, _) -> (NoExplosion, Pair a b)
    aLt :: Int -> Snailfish -> Snailfish
    aLt a (Value b) = Value (a+b)
    aLt a (Pair b c) = Pair (aLt a b) c
    aRt :: Snailfish -> Int -> Snailfish
    aRt (Value b) a = Value (a+b)
    aRt (Pair b c) a = Pair b (aRt c a)