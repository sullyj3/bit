{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AppState where

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((<|), (|>))
import Graphics.Vty (Event)
import Lens.Micro.Platform (makeLenses, (.~), (^.))
import Relude

-----------
-- State --
-----------

data Buffer = Buffer
  { _bufferFilePath :: Maybe FilePath,
    _bufferLines :: Seq Text,
    _bufferChanged :: Bool
  }

makeLenses ''Buffer

newEmptyBuffer :: Buffer
newEmptyBuffer = Buffer Nothing (Seq.singleton mempty) False

bufferLineCount :: Buffer -> Int
bufferLineCount (Buffer _ bLines _) = Seq.length bLines

data Rect = Rect
  { rectTopLeft :: (Int, Int),
    rectDimensions :: (Int, Int)
  }
  deriving (Show)

data CursorLocation = CursorLocation
  { _cursorColumn :: Int,
    _cursorLine :: Int
  }
  deriving (Show)

makeLenses ''CursorLocation

-- TODO make a damn decision about whether to allow multiple windows,
-- stick to tabs like Amp, or run as a server and let the WM/multiplexer handle
-- it
data Window = Window
  { _windowBuffer :: Buffer,
    _winTopLine :: Int,
    _winCursorLocation :: CursorLocation,
    _winRect :: Rect,
    -- How to quit message. Should only be displayed for an empty buffer when bit starts. Disappears after the buffer is modified
    _winShowStartMessage :: Bool 
  }

makeLenses ''Window

cursorLocTop :: CursorLocation
cursorLocTop = CursorLocation 0 0

windowFromBuf :: Rect -> Buffer -> Bool -> Window
windowFromBuf rect buf showStartMsg = Window buf 0 cursorLocTop rect showStartMsg

-- This function is safe - ensures the cursor stays inside the buffer, and doesn't move beyond the end of a line.
-- It scrolls the viewPoint to keep the cursor in view if necessary
moveCursor :: (Int, Int) -> Window -> Window
moveCursor (dx, dy) win@Window {..} =
  win |> winCursorLocation .~ newCursorLocation
    |> winTopLine .~ topLine'
  where
    bufLines = win ^. windowBuffer . bufferLines
    CursorLocation currCol currLine = win ^. winCursorLocation

    currLine' = clamp 0 (currLine + dy) (bufferLineCount (win ^. windowBuffer) - 1)
    currentLine = bufLines `Seq.index` currLine'

    currCol' = clamp 0 (currCol + dx) (T.length currentLine -1)
    newCursorLocation = CursorLocation currCol' currLine'

    Rect _ (_, winHeight) = win ^. winRect

    topLine' = case lineInViewPort _winTopLine currLine' _winRect of
      LT -> currLine'
      EQ -> _winTopLine
      GT -> currLine' - winHeight + 1

-- | return LT if line is above the viewport, GT if it's below, or Eq if it's within
-- >>> lineInViewPort 1 0 (Rect (0,0) (10,10))
-- LT
--
-- >>> lineInViewPort 0 0 (Rect (0,0) (10,10))
-- EQ
--
-- >>> lineInViewPort 0 9 (Rect (0,0) (10,10))
-- EQ
--
-- >>> lineInViewPort 0 10 (Rect (0,0) (10,10))
-- GT
lineInViewPort :: Int -> Int -> Rect -> Ordering
lineInViewPort topLine lineNumber Rect {rectDimensions = (_, height)}
  = compareRange lineNumber (topLine, topLine+height)


-- check whether an int is less than, within, or greater than a half-open interval
compareRange :: Int -> (Int, Int) -> Ordering
compareRange x (a,b)
  | x < a = LT
  | x < b = EQ
  | otherwise = GT

-- | ensures cursor remains within the viewport
scrollWindow :: Int -> Window -> Window
scrollWindow n (Window buf winTopLine cursor rect ssm) = Window buf newTopLine cursor' rect ssm
  where
    newTopLine = clamp 0 (winTopLine + n) (bufferLineCount buf)

    CursorLocation curCol curLine = cursor
    cursor' = case lineInViewPort newTopLine curLine rect of
      EQ -> cursor
      -- we've scrolled the cursor out of view, so we clamp the cursor to the
      -- viewport, then clamp to the length of the new line.
      _ -> CursorLocation curCol' curLine'
        where
          Rect _ (_, winHeight) = rect
          curLine' = clamp newTopLine curLine (newTopLine + winHeight)
          curCol' = clamp 0 curCol lineWidth

          lineWidth :: Int
          lineWidth = T.length <| _bufferLines buf `Seq.index` curLine'

clamp :: Ord a => a -> a -> a -> a
clamp a x b = max a (min x b)

data EditorMode = NormalMode | InsertMode

data InputWidgetType = InputWidgetSaveAsPath

data InputWidget = InputWidget {
  _inputWidgetType :: InputWidgetType,
  _inputWidgetPrompt :: Text,
  _inputWidgetContents :: Text
}

makeLenses ''InputWidget

data AppState = AppState
  { _stateDimensions :: (Int, Int),
    _stateLastEvent :: Maybe Event,
    _stateWindow :: Window,
    _stateMode :: EditorMode,
    -- a temporary message to be displayed in the status bar. Disappears after input
    _stateStatusMessage :: Maybe Text,
    _stateCurrInputWidget :: Maybe InputWidget
  }

makeLenses ''AppState
