{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AppState where

import Buffer
  ( Buffer,
    BufferContents,
    BufferID,
    CursorLocation (CursorLocation), bufInsertChar, bufferLines, bufDeleteChar, bufGetLineLength, bufInsertNewLine
  )
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((<|), (|>))
import Graphics.Vty (Event)
import Lens.Micro.Platform (Lens', makeLenses, (%~), (.~), (^.))
import Relude
import Safe.Partial (Partial)

-----------
-- State --
-----------

data Rect = Rect
  { rectTopLeft :: (Int, Int),
    rectDimensions :: (Int, Int)
  }
  deriving (Show)

rectHeight :: Rect -> Int
rectHeight (Rect _ (_, h)) = h

-- TODO make a damn decision about whether to allow multiple windows,
-- stick to tabs like Amp, or run as a server and let the WM/multiplexer handle
-- it
data Window = Window
  { _windowBuffer :: BufferID,
    _winTopLine :: Int,
    -- | represents the vertical scroll position of the viewport within the buffer
    _winCursorLocation :: CursorLocation,
    _winRect :: Rect,
    -- How to quit message. Should only be displayed for an empty buffer when bit starts. Disappears after the buffer is modified
    -- TODO: refactor, this flag probably belongs in AppState
    _winShowStartMessage :: Bool
  }

makeLenses ''Window

cursorLocTop :: CursorLocation
cursorLocTop = CursorLocation 0 0

windowFromBufID :: Rect -> BufferID -> Bool -> Window
windowFromBufID rect buf showStartMsg =
  Window
    { _windowBuffer = buf,
      _winTopLine = 0,
      _winCursorLocation = cursorLocTop,
      _winRect = rect,
      _winShowStartMessage = showStartMsg
    }

-- This function is safe - ensures the cursor stays inside the buffer, and doesn't move beyond the end of a line.
-- It scrolls the viewPoint to keep the cursor in view if necessary
moveCursor :: (Int, Int) -> BufferContents -> Window -> Window
moveCursor (dx, dy) bufLines win@Window {..} =
  win
    |> winCursorLocation .~ newCursorLocation
    |> winTopLine .~ topLine'
  where
    CursorLocation currCol currLine = win ^. winCursorLocation

    currLine' = clamp 0 (currLine + dy) (Seq.length bufLines - 1)
    currentLine = bufLines `Seq.index` currLine'

    currCol' = clamp 0 (currCol + dx) (T.length currentLine -1)
    newCursorLocation = CursorLocation currCol' currLine'

    Rect _ (_, winHeight) = win ^. winRect

    topLine' = case compareRange currLine' (_winTopLine, _winTopLine + winHeight) of
      -- new current line is above the viewport, so we move upwards, setting it
      -- to the top line of the viewport
      LT -> currLine'
      -- new current line is within the current viewport, so we keep the top line the same
      EQ -> _winTopLine
      -- new current line is below the viewport, so we move down, and make the
      -- new current line the bottom line in the window
      -- TODO: unit tests, I think this can potentially become negative and it shouldn't
      GT -> currLine' - winHeight + 1

-- check whether an int is less than, within, or greater than a half-open interval
compareRange :: Int -> (Int, Int) -> Ordering
compareRange x (a, b)
  | x < a = LT
  | x < b = EQ
  | otherwise = GT

-- | ensures cursor remains within the viewport
scrollWindow :: Int -> BufferContents -> Window -> Window
scrollWindow n bufLines win@Window {..} =
  win {_winTopLine = newTopLine, _winCursorLocation = cursor'}
  where
    newTopLine = clamp 0 (_winTopLine + n) (Seq.length bufLines)

    CursorLocation curCol curLine = _winCursorLocation
    cursor' = case compareRange curLine (newTopLine, newTopLine + rectHeight _winRect) of
      EQ -> _winCursorLocation
      -- for LT or GT, we've scrolled the cursor out of view, so we clamp the
      -- cursor vertically to the viewport.
      -- now it is on either the top or bottom line of the viewport.
      -- we also clamp the cursor's horizontal position to the length of that line
      _ -> CursorLocation curCol' curLine'
        where
          Rect _ (_, winHeight) = _winRect
          curLine' = clamp newTopLine curLine (newTopLine + winHeight)
          curCol' = clamp 0 curCol lineWidth

          lineWidth :: Int
          lineWidth = T.length <| bufLines `Seq.index` curLine'

clamp :: Ord a => a -> a -> a -> a
clamp a x b = max a (min x b)

data EditorMode = NormalMode | InsertMode

data InputWidgetType = InputWidgetSaveAsPath

data InputWidget = InputWidget
  { _inputWidgetType :: !InputWidgetType,
    _inputWidgetPrompt :: !Text,
    _inputWidgetContents :: !Text
  }

makeLenses ''InputWidget

-- Invariant: every BufferID key maps to a buffer with that bufferID
-- wish I knew how to enforce that with the type system
newtype OpenBuffers = OpenBuffers (NEMap BufferID Buffer)

insertBuffer :: BufferID -> Buffer -> OpenBuffers -> OpenBuffers
insertBuffer bid buf = coerce $ NEMap.insert bid buf

-- Unsafe - attempting to index using a BufferID that doesn't yet exist is a
-- programmer error
getBuffer :: Partial => BufferID -> OpenBuffers -> Buffer
getBuffer bid (OpenBuffers ob) =
  case NEMap.lookup bid ob of
    Just buf -> buf
    Nothing -> error $ "Bug: could not find buffer id: " <> show bid

modifyBuffer :: BufferID -> (Buffer -> Buffer) -> (OpenBuffers -> OpenBuffers)
modifyBuffer bid f = coerce $ NEMap.adjust f bid

data AppState = AppState
  { _stateDimensions :: (Int, Int),
    _stateLastEvent :: Maybe Event,
    _stateWindow :: Window,
    _stateMode :: EditorMode,
    -- a temporary message to be displayed in the status bar. Disappears after input
    _stateStatusMessage :: Maybe Text,
    _stateCurrInputWidget :: Maybe InputWidget,
    _stateOpenBuffers :: OpenBuffers,
    _stateNextBufID :: BufferID
  }

makeLenses ''AppState

-- todo make these into lenses
getCurrentBuffer :: AppState -> Buffer
getCurrentBuffer s = getBuffer (s ^. currentBufferID) (s ^. stateOpenBuffers)

currentBufferID :: Lens' AppState BufferID
currentBufferID = stateWindow . windowBuffer

modifyCurrentBuffer :: (Buffer -> Buffer) -> AppState -> AppState
modifyCurrentBuffer f s = s |> stateOpenBuffers %~ modifyBuffer bid f
  where
    bid = s ^. currentBufferID

modifyCurrentBufferState ::
  MonadState AppState m =>
  (Buffer -> Buffer) ->
  m ()
modifyCurrentBufferState f = modify $ modifyCurrentBuffer f

insertChar :: Char -> AppState -> AppState
insertChar c s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow .~ win'
  where
    -- first modify the buffer
    cursorLoc = s ^. stateWindow . winCursorLocation
    buf' = bufInsertChar c cursorLoc (getCurrentBuffer s)
    -- then move the cursor
    win' = moveCursor (1, 0) (buf' ^. bufferLines) (s ^. stateWindow)

-- TODO consider case where we're deleting the last character in the line
backspace :: AppState -> AppState
backspace s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow .~ win'
  where
    -- first modify the buffer
    CursorLocation col line = s ^. stateWindow . winCursorLocation
    buf' = bufDeleteChar (CursorLocation (col -1) line) (getCurrentBuffer s)
    -- then move the cursor
    win' = moveCursor (-1, 0) (buf' ^. bufferLines) (s ^. stateWindow)

-- TODO consider case where we're deleting the last character in the line
-- delete the character at the cursor. If we were on the last character,
-- move the cursor back one
del :: AppState -> AppState
del s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow .~ win'
  where
    loc@(CursorLocation col line) = s ^. stateWindow . winCursorLocation
    buf' = bufDeleteChar loc (getCurrentBuffer s)

    win = s ^. stateWindow
    lenCurrLine = bufGetLineLength line buf'
    win'
      | col == lenCurrLine = moveCursor (-1, 0) (buf' ^. bufferLines) win
      | otherwise = win

insertNewline :: AppState -> AppState
insertNewline s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow %~ moveCursor (0, 1) (buf' ^. bufferLines)
  where
    loc = s ^. stateWindow . winCursorLocation
    buf' = bufInsertNewLine loc (getCurrentBuffer s)
