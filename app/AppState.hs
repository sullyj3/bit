{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{-# LANGUAGE NamedFieldPuns #-}
module AppState where

import qualified Data.Map.NonEmpty as NEMap
import           Data.Map.NonEmpty (NEMap)

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((<|), (|>))
import Graphics.Vty (Event)
import Lens.Micro.Platform (makeLenses, (.~), (^.), (.=), (%=), use)
import Relude
import Relude.Unsafe (fromJust)
import Safe.Partial

-----------
-- State --
-----------

newtype BufferID = BufferID Int
  deriving (Eq, Show, Ord, Enum)

type BufferContents = Seq Text

data Buffer = Buffer
  { _bufferID :: BufferID,
    _bufferFilePath :: Maybe FilePath,
    _bufferLines :: BufferContents,
    _bufferChanged :: Bool
  }

makeLenses ''Buffer

newEmptyBuffer :: BufferID -> Buffer
newEmptyBuffer bid = Buffer bid Nothing (Seq.singleton mempty) False

bufferLineCount :: Buffer -> Int
bufferLineCount Buffer {_bufferLines} = Seq.length _bufferLines

data Rect = Rect
  { rectTopLeft :: (Int, Int),
    rectDimensions :: (Int, Int)
  }
  deriving (Show)

rectHeight :: Rect -> Int
rectHeight (Rect _ (_,h)) = h

-- TODO: Is this relative to the window or the buffer? I think it's the buffer
-- regardless, needs to be renamed to make this clearer
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
  { _windowBuffer :: BufferID,
    _winTopLine :: Int,
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
windowFromBufID rect buf showStartMsg = Window buf 0 cursorLocTop rect showStartMsg



-- This function is safe - ensures the cursor stays inside the buffer, and doesn't move beyond the end of a line.
-- It scrolls the viewPoint to keep the cursor in view if necessary
moveCursor :: (Int, Int) -> BufferContents -> Window -> Window
moveCursor (dx, dy) bufLines win@Window {..} =
  win |> winCursorLocation .~ newCursorLocation
      |> winTopLine .~ topLine'
  where
    CursorLocation currCol currLine = win ^. winCursorLocation

    currLine' = clamp 0 (currLine + dy) (Seq.length bufLines - 1)
    currentLine = bufLines `Seq.index` currLine'

    currCol' = clamp 0 (currCol + dx) (T.length currentLine -1)
    newCursorLocation = CursorLocation currCol' currLine'

    Rect _ (_, winHeight) = win ^. winRect

    topLine' = case compareRange currLine' ( _winTopLine, _winTopLine + winHeight ) of
      LT -> currLine'
      EQ -> _winTopLine
      GT -> currLine' - winHeight + 1


-- check whether an int is less than, within, or greater than a half-open interval
compareRange :: Int -> (Int, Int) -> Ordering
compareRange x (a,b)
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

data InputWidget = InputWidget {
  _inputWidgetType :: !InputWidgetType,
  _inputWidgetPrompt :: !Text,
  _inputWidgetContents :: !Text
}

makeLenses ''InputWidget

-- Invariant: every BufferID key maps to a buffer with that bufferID
-- wish I knew how to enforce that with the type system
newtype OpenBuffers = OpenBuffers (NEMap BufferID Buffer)

insertBuffer :: Buffer -> OpenBuffers -> OpenBuffers
insertBuffer buf =
  let bid = buf ^. bufferID
  in  coerce $ NEMap.insert bid buf

-- Unsafe
getBuffer :: Partial => BufferID -> OpenBuffers -> Buffer
getBuffer bid (OpenBuffers ob) =
  case NEMap.lookup bid ob of
    Just buf -> buf
    Nothing -> error $ "Bug: could not find buffer id: " <> show bid

modifyBuffer :: BufferID -> (Buffer -> Buffer) -> (OpenBuffers -> OpenBuffers)
modifyBuffer bid f = coerce $ NEMap.adjust f bid


-- adjust :: Ord k => (a -> a) -> k -> NEMap k a -> NEMap k a
  


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

getCurrentBuffer :: AppState -> Buffer
getCurrentBuffer s = getBuffer bid buffers 
  where bid = s ^. stateWindow . windowBuffer
        buffers = s ^. stateOpenBuffers

modifyCurrentBuffer :: MonadState AppState m 
                    => (Buffer -> Buffer)
                    -> m ()
modifyCurrentBuffer f = do
  bid <- use $ stateWindow . windowBuffer
  stateOpenBuffers %= modifyBuffer bid f
