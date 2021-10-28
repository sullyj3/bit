module TextUtils where

import Data.Text (Text)
import qualified Data.Text as T

-- TODO probably inefficient, especially for long lines
insertChar :: Char -> Int -> Text -> Text
insertChar c i txt = l <> T.singleton c <> r
  where
    (l, r) = T.splitAt i txt

-- does nothing if i ∉ [0, T.length txt)
deleteChar :: Int -> Text -> Text
deleteChar i txt
  | i < 0 = txt
  | i >= T.length txt = txt
  | otherwise = let (l, r) = T.splitAt i txt in l <> T.tail r