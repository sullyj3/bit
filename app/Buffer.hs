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

import Control.Exception (IOException, catch)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((|>))
import Lens.Micro.Platform (makeLenses, (%~), (.~))
import Relude
import qualified TextUtils as T

data BufferLocation = BufferLocation
  { _cursorColumn :: Int,
    _cursorLine :: Int
  }
  deriving (Show)

makeLenses ''BufferLocation

bufferLocTop :: BufferLocation
bufferLocTop = BufferLocation 0 0

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

empty :: Buffer
empty =
  Buffer
    { _bufferFilePath = Nothing,
      _bufferLines = Seq.singleton mempty,
      _bufferChanged = False
    }

lineCount :: Buffer -> Int
lineCount Buffer {_bufferLines} = Seq.length _bufferLines

lineLength :: Int -> Buffer -> Int
lineLength line Buffer {_bufferLines} =
  T.length $ Seq.index _bufferLines line

edit ::
  (BufferContents -> BufferContents) ->
  (Buffer -> Buffer)
edit f buf =
  buf
    |> bufferLines %~ f
    |> bufferChanged .~ True

insertChar :: Char -> BufferLocation -> Buffer -> Buffer
insertChar c (BufferLocation col line) =
  edit $ Seq.adjust' (T.insertChar c col) line

insertNewLine :: BufferLocation -> Buffer -> Buffer
insertNewLine (BufferLocation col line) = edit go
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

deleteChar :: BufferLocation -> Buffer -> Buffer
deleteChar (BufferLocation col line) =
  edit $ Seq.adjust' (T.deleteChar col) line

--------
-- IO --
--------

-- creates a new empty buffer if there is no existing file at the path
openFile :: FilePath -> IO Buffer
openFile path =
  tryOpenFile `catch` orCreateNewBuffer
  where
    tryOpenFile :: IO Buffer
    tryOpenFile = do
      theLines <- Seq.fromList . lines <$> readFileText path
      pure $ Buffer (Just path) theLines False

    -- If the file isn't present, create an empty buffer
    -- TODO figure out how to check that it's the right IO exception, from memory
    -- I think we may have to resort to string comparison
    orCreateNewBuffer :: IOException -> IO Buffer
    orCreateNewBuffer _ = pure $ Buffer.empty {_bufferFilePath = Just path}

mkInitialBuffer :: Maybe FilePath -> IO Buffer
mkInitialBuffer = \case
  Just fp -> openFile fp
  Nothing -> pure Buffer.empty
