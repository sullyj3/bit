{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UnsafeUtils where
import Relude
import qualified Data.Text as T
import Control.Exception (throw)

newtype Unreachable = Unreachable Text
  deriving (Show, Typeable)

instance Exception Unreachable

-- UNSAFE throws on left
leftIsUnreachable :: Show a => Text -> Either a b -> b
leftIsUnreachable errMsg = \case
  Left a -> throw $ Unreachable $ T.pack (show a) <> errMsg 
  Right b -> b

