module Misc where

-- todo reorder args put x last
clamp :: Ord a => a -> a -> a -> a
clamp a b x = max a (min x b)