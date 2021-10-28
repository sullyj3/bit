{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Buffer
  ( Buffer (..),
    bufferFilePath,
    bufferLines,
    bufferChanged,
    bufferLocTop,
    BufferLocation (..),
    BufferID (..),
    BufferContents,
    Buffer.empty,
    locCol,
    locLine,
    lineCount,
    lineLength,
    insertChar,
    insertNewLine,
    deleteChar,
    openFile,
    mkInitialBuffer,
    saveContentsToPath,
    getLineRange,
  )
where

import Control.Exception (IOException, catch)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((|>))
import Lens.Micro.Platform (makeLenses, (%~), (.~))
import Relude
import qualified TextUtils as T

data BufferLocation = BufferLocation
  { _locCol :: Int,
    _locLine :: Int
  }
  deriving (Show)

makeLenses ''BufferLocation

bufferLocTop :: BufferLocation
bufferLocTop = BufferLocation 0 0

newtype BufferID = BufferID Int
  deriving (Eq, Show, Ord, Enum)

newtype BufferContents = BufferContents {getBufferContents :: Seq Text}

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
      _bufferLines = BufferContents $ Seq.singleton mempty,
      _bufferChanged = False
    }

lineCount :: BufferContents -> Int
lineCount = Seq.length . getBufferContents

lineLength :: Int -> BufferContents -> Int
lineLength line contents =
  T.length $ Seq.index (getBufferContents contents) line

edit ::
  (BufferContents -> BufferContents) ->
  (Buffer -> Buffer)
edit f buf =
  buf
    |> bufferLines %~ f
    |> bufferChanged .~ True

insertChar :: Char -> BufferLocation -> Buffer -> Buffer
insertChar c (BufferLocation col line) =
  edit $ coerce $ Seq.adjust' (T.insertChar c col) line

insertNewLine :: BufferLocation -> Buffer -> Buffer
insertNewLine (BufferLocation col line) = edit go
  where
    go :: BufferContents -> BufferContents
    go (BufferContents bufLines) =
      -- first element of bottom is the current line, we drop it and replace with
      -- the two halves of the split line
      BufferContents $ top <> Seq.fromList [l, r] <> Seq.drop 1 bottom
      where
        theLine = bufLines `Seq.index` line
        (l, r) = T.splitAt col theLine
        (top, bottom) = Seq.splitAt line bufLines

deleteChar :: BufferLocation -> Buffer -> Buffer
deleteChar (BufferLocation col line) =
  edit $ coerce $ Seq.adjust' (T.deleteChar col) line

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
      pure $ Buffer (Just path) (BufferContents theLines) False

    -- If the file isn't present, create an empty buffer
    -- TODO figure out how to check that it's the right IO exception, from memory
    -- I think we may have to resort to string comparison
    orCreateNewBuffer :: IOException -> IO Buffer
    orCreateNewBuffer _ = pure $ Buffer.empty {_bufferFilePath = Just path}

-- TODO: make more efficient using streamly or something
-- TODO: handle file already exists/prompt overwrite
saveContentsToPath :: FilePath -> BufferContents -> IO ()
saveContentsToPath path (BufferContents theLines) =
  writeFileText path (unlines . toList $ theLines)

mkInitialBuffer :: Maybe FilePath -> IO Buffer
mkInitialBuffer = \case
  Just fp -> openFile fp
  Nothing -> pure Buffer.empty

-- | Warning: representation subject to change
getLineRange :: Int -> Int -> BufferContents -> Seq Text
getLineRange start n =
  Seq.take n . Seq.drop start . getBufferContents