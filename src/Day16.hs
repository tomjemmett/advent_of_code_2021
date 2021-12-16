module Day16 (
  day16
) where

import Common

type Version = Int
data Packet = Operator Version Op [Packet] | Literal Version Int deriving (Show)
data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | Equal deriving (Show)

getOp :: Int -> Op
getOp = \case
  0 -> Sum
  1 -> Product
  2 -> Minimum
  3 -> Maximum
  5 -> GreaterThan
  6 -> LessThan
  7 -> Equal

runOperations :: Packet -> Int
runOperations (Operator _ t ps) = case t of
  Sum         -> sum ps'
  Product     -> product ps'
  Minimum     -> minimum ps'
  Maximum     -> maximum ps'
  GreaterThan -> if head ps' >  last ps' then 1 else 0
  LessThan    -> if head ps' <  last ps' then 1 else 0
  Equal       -> if head ps' == last ps' then 1 else 0
  where
    ps' = map runOperations ps
runOperations (Literal _ v) = v

packetVersionSum :: Packet -> Int
packetVersionSum (Operator v _ p) = v + sum (map packetVersionSum p)
packetVersionSum (Literal v _) = v

day16 :: AOCSolution
day16 input = show <$> [p1, p2]
  where
    p = snd $ parsePacket $ parseInput input
    p1 = packetVersionSum p
    p2 = runOperations p

parseInput :: String -> [Int]
parseInput = concatMap hexToBits
  where
    hexToBits :: Char -> [Int]
    hexToBits = \case
      '0' -> [0,0,0,0]
      '1' -> [0,0,0,1]
      '2' -> [0,0,1,0]
      '3' -> [0,0,1,1]
      '4' -> [0,1,0,0]
      '5' -> [0,1,0,1]
      '6' -> [0,1,1,0]
      '7' -> [0,1,1,1]
      '8' -> [1,0,0,0]
      '9' -> [1,0,0,1]
      'A' -> [1,0,1,0]
      'B' -> [1,0,1,1]
      'C' -> [1,1,0,0]
      'D' -> [1,1,0,1]
      'E' -> [1,1,1,0]
      'F' -> [1,1,1,1]

bitsToNumber :: Int -> [Int] -> ([Int], Int)
bitsToNumber n xs = (b, sum $ zipWith (*) powers2 a)
  where
    a = reverse $ take n xs
    b = drop n xs
    powers2 :: [Int]
    powers2 = map (2^) [0..]

-- parsing parts of our input
parsePacket :: [Int] -> ([Int], Packet)
parsePacket xs = if t == 4
    then parseLiteral v 0 xs2
    else parseOperator v (getOp t) xs2
  where
    (xs1, v) = bitsToNumber 3 xs
    (xs2, t) = bitsToNumber 3 xs1

parseLiteral :: Version -> Int -> [Int] -> ([Int], Packet)
parseLiteral v i (x0:xs) = case x0 of
  0 -> (xs', Literal v i')
  1 -> parseLiteral v i' xs'
  where
    (xs', j) = bitsToNumber 4 xs
    i' = i * 16 + j

parseOperator :: Version -> Op -> [Int] -> ([Int], Packet)
parseOperator v o (x0:xs) = case x0 of
  0 -> parseOperator0 v o xs
  1 -> parseOperator1 v o xs

parseOperator0 :: Version -> Op -> [Int] -> ([Int], Packet)
parseOperator0 v o xs = (xs'', Operator v o r)
  where
    (xs', b) = bitsToNumber 15 xs
    xs'' = drop b xs'
    r = go $ take b xs'
    go ys = p : if null ys' then [] else go ys'
      where
        (ys', p) = parsePacket ys

parseOperator1 :: Version -> Op -> [Int] -> ([Int], Packet)
parseOperator1 v o xs = go (bitsToNumber 11 xs) []
  where
    go (ys, 0) c = (ys, Operator v o $ reverse c)
    go (ys, n) c = go (ys', pred n) (p : c)
      where
        (ys', p) = parsePacket ys
