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
    openNewLine,
    deleteChar,
    openFile,
    mkInitialBuffer,
    saveContentsToPath,
    getLineRange,
  )
where

import Control.Exception (IOException, catch)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Flow ((|>))
import Lens.Micro.Platform (makeLenses, (%~), (.~))
import Relude
import qualified TextUtils

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
  Text.length $ Seq.index (getBufferContents contents) line

-- TODO: refactor this to be (Seq Text -> Seq Text) -> Buffer -> Buffer
-- this function is not exported, so this should be fine
edit ::
  (BufferContents -> BufferContents) ->
  (Buffer -> Buffer)
edit f buf =
  buf
    |> bufferLines %~ f
    |> bufferChanged .~ True

insertChar :: Char -> BufferLocation -> Buffer -> Buffer
insertChar c (BufferLocation col line) =
  edit $ coerce $ Seq.adjust' (TextUtils.insertChar c col) line

-- inserts a newline at the position of the cursor, splitting the current line
insertNewLine :: BufferLocation -> Buffer -> Buffer
insertNewLine (BufferLocation col line) = 
  edit \(BufferContents bufLines) ->
    let
      theLine = bufLines `Seq.index` line
      (l, r) = Text.splitAt col theLine
      (top, bottom) = Seq.splitAt line bufLines
    in
      BufferContents $ top <> Seq.fromList [l, r] <> Seq.drop 1 bottom

-- the line at the given index moves down. 
-- examples:
-- - to open a new line at the start of the buffer, the line index will be 0
-- - to open a line before the current line, give the current line
-- - to open a line after the current line, give the current line + 1
openNewLine :: Int -> Buffer -> Buffer
openNewLine line = edit \(BufferContents bufLines) ->
  BufferContents $ Seq.insertAt line Text.empty bufLines

deleteChar :: BufferLocation -> Buffer -> Buffer
deleteChar (BufferLocation col line) =
  edit $ coerce $ Seq.adjust' (TextUtils.deleteChar col) line

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
