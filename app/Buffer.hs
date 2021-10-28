{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Buffer where

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((|>))
import Lens.Micro.Platform (makeLenses, (%~), (.~))
import Relude

-- TODO: Is this relative to the window or the buffer? I think it's the buffer
-- regardless, needs to be renamed to make this clearer
data CursorLocation = CursorLocation
  { _cursorColumn :: Int,
    _cursorLine :: Int
  }
  deriving (Show)

makeLenses ''CursorLocation

newtype BufferID = BufferID Int
  deriving (Eq, Show, Ord, Enum)

type BufferContents = Seq Text

data Buffer = Buffer
  { _bufferFilePath :: Maybe FilePath,
    _bufferLines :: BufferContents,
    -- Any function which edits _bufferLines must set this flag
    _bufferChanged :: Bool
  }

makeLenses ''Buffer

newEmptyBuffer :: Buffer
newEmptyBuffer =
  Buffer
    { _bufferFilePath = Nothing,
      _bufferLines = Seq.singleton mempty,
      _bufferChanged = False
    }

bufferLineCount :: Buffer -> Int
bufferLineCount Buffer {_bufferLines} = Seq.length _bufferLines

bufGetLineLength :: Int -> Buffer -> Int
bufGetLineLength line Buffer {_bufferLines} =
  T.length $ Seq.index _bufferLines line

bufEdit ::
  (BufferContents -> BufferContents) ->
  (Buffer -> Buffer)
bufEdit f buf =
  buf
    |> bufferLines %~ f
    |> bufferChanged .~ True

bufInsertChar :: Char -> CursorLocation -> Buffer -> Buffer
bufInsertChar c (CursorLocation col line) =
  bufEdit $ Seq.adjust' (insertChar c col) line

bufInsertNewLine :: CursorLocation -> Buffer -> Buffer
bufInsertNewLine (CursorLocation col line) = bufEdit go
  where
    go :: BufferContents -> BufferContents
    go bufLines =
      -- first element of bottom is the current line, we drop it and replace with
      -- the two halves of the split line
      top <> Seq.fromList [l, r] <> Seq.drop 1 bottom
      where
        theLine = bufLines `Seq.index` line
        (l, r) = T.splitAt col theLine
        (top, bottom) = Seq.splitAt line bufLines

bufDeleteChar :: CursorLocation -> Buffer -> Buffer
bufDeleteChar (CursorLocation col line) =
  bufEdit $ Seq.adjust' (deleteChar col) line

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
