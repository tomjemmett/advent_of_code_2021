module Common (
  countTrue
) where

import Data.Foldable (toList)

countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList