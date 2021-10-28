module Misc where

-- | endpoints are inclusive
clamp :: Ord a => a -> a -> a -> a
clamp a b x = max a (min x b)

data Rect = Rect
  { rectTopLeft :: (Int, Int),
    rectDimensions :: (Int, Int)
  }
  deriving (Show)

rectHeight :: Rect -> Int
rectHeight (Rect _ (_, h)) = h