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

-- TODO: explicit exports, so I know which functions are implementation details
module AppState where

import Buffer (Buffer (..), BufferContents, BufferID (..), BufferLocation (..), bufferLines)
import qualified Buffer
import Cursor (CursorMovement)
import qualified Cursor
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Flow ((|>))
import Graphics.Vty (Event)
import Lens.Micro.Platform (Lens', makeLenses, (%~), (.~), (^.))
import Misc
import Relude
import Safe.Partial (Partial)

-----------
-- State --
-----------

-- TODO make a damn decision about whether to allow multiple windows,
-- stick to tabs like Amp, or run as a server and let the WM/multiplexer handle
-- it
data Window = Window
  { _windowBuffer :: BufferID,
    _winTopLine :: Int,
    -- | represents the vertical scroll position of the viewport within the buffer
    _winCursorLocation :: BufferLocation,
    _winRect :: Rect,
    -- How to quit message. Should only be displayed for an empty buffer when bit starts. Disappears after the buffer is modified
    -- TODO: refactor, this flag probably belongs in AppState
    _winShowStartMessage :: Bool
  }

makeLenses ''Window

windowFromBufID :: Rect -> BufferID -> Bool -> Window
windowFromBufID rect buf showStartMsg =
  Window
    { _windowBuffer = buf,
      _winTopLine = 0,
      _winCursorLocation = Buffer.bufferLocTop,
      _winRect = rect,
      _winShowStartMessage = showStartMsg
    }

-- Scrolls the viewPoint to keep the cursor in view if necessary
moveCursorWin :: CursorMovement () -> BufferContents -> Window -> Window
moveCursorWin movement bufContents win@Window {_winCursorLocation} =
  scrollViewportToCursor $ win {_winCursorLocation = newCursorLocation}
  where
    newCursorLocation =
      Cursor.moveCursor bufContents movement _winCursorLocation

scrollViewportToCursor :: Window -> Window
scrollViewportToCursor win@Window {..} =
  win {_winTopLine = topLine'}
  where
    Rect _ (_, winHeight) = _winRect
    (BufferLocation _ line) = _winCursorLocation

    topLine' = case compareRange line (_winTopLine, _winTopLine + winHeight) of
      -- new current line is above the viewport, so we move upwards, setting it
      -- to the top line of the viewport
      LT -> line
      -- new current line is within the current viewport, so we keep the top line the same
      EQ -> _winTopLine
      -- new current line is below the viewport, so we move down, and make the
      -- new current line the bottom line in the window
      -- TODO: unit tests, I think this can potentially become negative and it shouldn't
      GT -> line - winHeight + 1

-- check whether an int is less than, within, or greater than a half-open interval
compareRange :: Int -> (Int, Int) -> Ordering
compareRange x (a, b)
  | x < a = LT
  | x < b = EQ
  | otherwise = GT

clampCursor :: BufferContents -> Window -> Window
clampCursor bufContents win@Window {_winTopLine, _winRect} =
  win |> winCursorLocation
    %~ Cursor.moveCursor
      bufContents
      (Cursor.clampCursorToViewPort _winTopLine _winRect >> Cursor.clampCursorToBufferHeight)

-- | ensures cursor remains within the viewport
scrollWindow :: Int -> BufferContents -> Window -> Window
scrollWindow n bufContents win@Window {_winTopLine} =
  win
    |> winTopLine .~ clamp 0 (Buffer.lineCount bufContents - 1) (_winTopLine + n)
    |> clampCursor bufContents

data EditorMode = NormalMode | InsertMode

data InputWidgetType = InputWidgetSaveAsPath

data InputWidget = InputWidget
  { _inputWidgetType :: !InputWidgetType,
    _inputWidgetPrompt :: !Text,
    _inputWidgetContents :: !Text
  }

makeLenses ''InputWidget

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
    buf' = Buffer.insertChar c cursorLoc (getCurrentBuffer s)
    -- then move the cursor
    win' = moveCursorWin (Cursor.moveRelative (1, 0)) (buf' ^. bufferLines) (s ^. stateWindow)

-- TODO consider case where we're deleting the last character in the line
backspace :: AppState -> AppState
backspace s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow .~ win'
  where
    -- first modify the buffer
    BufferLocation col line = s ^. stateWindow . winCursorLocation
    buf' = Buffer.deleteChar (BufferLocation (col -1) line) (getCurrentBuffer s)
    -- then move the cursor
    win' = moveCursorWin (Cursor.moveRelative (-1, 0)) (buf' ^. bufferLines) (s ^. stateWindow)

-- TODO consider case where we're deleting the last character in the line
-- delete the character at the cursor. If we were on the last character,
-- move the cursor back one
del :: AppState -> AppState
del s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow .~ win'
  where
    loc@(BufferLocation col line) = s ^. stateWindow . winCursorLocation
    buf' = Buffer.deleteChar loc (getCurrentBuffer s)

    win = s ^. stateWindow
    lenCurrLine = Buffer.lineLength line $ buf' ^. bufferLines
    win'
      | col == lenCurrLine = moveCursorWin (Cursor.moveRelative (-1, 0)) (buf' ^. bufferLines) win
      | otherwise = win

insertNewline :: AppState -> AppState
insertNewline s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow %~ moveCursorWin (Cursor.moveRelative (0, 1)) (buf' ^. bufferLines)
  where
    loc = s ^. stateWindow . winCursorLocation
    buf' = Buffer.insertNewLine loc (getCurrentBuffer s)

-- Possibly openNewLine and openNewLineAbove should not move the cursor, moving
-- should be left to separate Command.
-- either way, openNewLineAbove certainly still needs a clampCursor, since the 
-- line containing the cursor becomes empty
openNewLine :: AppState -> AppState
openNewLine s =
  s |> modifyCurrentBuffer (const buf')
    |> stateWindow %~ moveCursorWin (Cursor.moveRelative (0, 1)) (buf' ^. bufferLines)
  where
    BufferLocation _col line = s ^. stateWindow . winCursorLocation
    buf' = Buffer.openNewLine (line+1) (getCurrentBuffer s)

openNewLineAbove :: AppState -> AppState
openNewLineAbove s =
  s |> modifyCurrentBuffer (const buf')
    -- cursor vertical position remains the same, since the current line is 
    -- simply moved down. We just need to ensure that the cursor is clamped 
    -- horizontally
    |> stateWindow %~ clampCursor (buf' ^. bufferLines)
  where
    BufferLocation _col line = s ^. stateWindow . winCursorLocation
    buf' = Buffer.openNewLine line (getCurrentBuffer s)
