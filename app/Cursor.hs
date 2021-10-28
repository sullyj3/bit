{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cursor
  ( CursorMovement (..),
    cursorMovementDone,
    moveCursor,
    moveTop,
    moveBottom,
    moveStartOfLine,
    moveEndOfLine,
    moveRelative,
    clampCursorToBufferHeight,
    clampCursorToViewPort,
  )
where

import Buffer
-- import Flow
import Lens.Micro.Platform (use, (%=), (.=))
import Misc
import Relude

newtype CursorMovement a = CursorMovement {runCursorMovement :: ReaderT BufferContents (State BufferLocation) a}
  deriving (Functor, Applicative, Monad, MonadState BufferLocation, MonadReader BufferContents)

instance Semigroup (CursorMovement ()) where
  a <> b = a *> b

instance Monoid (CursorMovement ()) where
  mempty = pure ()

cursorMovementDone :: CursorMovement ()
cursorMovementDone = pure ()

moveCursor :: BufferContents -> CursorMovement () -> BufferLocation -> BufferLocation
moveCursor contents (CursorMovement movement) = execState . flip runReaderT contents $ movement

moveTop :: CursorMovement ()
moveTop = locLine .= 0

moveBottom :: CursorMovement ()
moveBottom = do
  height <- lineCount <$> ask
  locLine .= height - 1

moveStartOfLine :: CursorMovement ()
moveStartOfLine = locCol .= 0

moveEndOfLine :: CursorMovement ()
moveEndOfLine = do
  width <- widthCurrLine
  locCol .= width - 1

widthCurrLine :: CursorMovement Int
widthCurrLine = do
  line <- use locLine
  lineLength line <$> ask

moveRelative :: (Int, Int) -> CursorMovement ()
moveRelative (dx, dy) = do
  locLine %= (+ dy)
  clampCursorToBufferHeight
  locCol %= (+ dx)
  clampCursorToCurrentLineWidth

clampCursorToBufferHeight :: CursorMovement ()
clampCursorToBufferHeight = do
  height <- lineCount <$> ask
  locLine %= clamp 0 (height -1)

clampCursorToCurrentLineWidth :: CursorMovement ()
clampCursorToCurrentLineWidth = do
  width <- widthCurrLine
  locCol %= clamp 0 (width -1)

clampCursorToViewPort :: Int -> Rect -> CursorMovement ()
clampCursorToViewPort topLine rect = do
  locLine %= clamp topLine (topLine + rectHeight rect - 1)
  clampCursorToCurrentLineWidth