{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module AppState where
import Relude

import Graphics.Vty ( Event )
import qualified Data.Sequence as Seq

import Lens.Micro.Platform ( makeLenses )

import qualified Data.Text as T


-----------
-- State --
-----------

data Buffer = Buffer { bufferFilePath :: Maybe FilePath
                     , bufferLines :: Seq Text }

newEmptyBuffer :: Buffer
newEmptyBuffer = Buffer Nothing (Seq.singleton mempty)

bufferLineCount :: Buffer -> Int
bufferLineCount (Buffer _ bLines) = Seq.length bLines

data Rect = Rect { rectTopLeft :: (Int, Int), rectDimensions :: (Int, Int) }
  deriving Show

-- left is a window with no buffer

-- TODO make a damn decision about whether to allow multiple windows,
-- stick to tabs like Amp, or run as a server and let the WM/multiplexer handle
-- it
data Window = Window { _windowBuffer :: Buffer
                     , _winTopLine :: Int
                     , _winCursorLocation :: (Int, Int)
                     , _winRect :: Rect 
                     , _winShowStartMessage :: Bool }

makeLenses ''Window

windowFromBuf :: Rect -> Buffer -> Bool -> Window
windowFromBuf rect buf showStartMsg = Window buf 0 (0,0) rect showStartMsg

-- This function is safe at least when called - ensures the cursor stays inside the window, and doesn't move beyond the end of a line.
-- 
-- however if we scroll without moving the cursor, the cursor can end up in an invalid state, focusing on a character that doesn't exist
--
-- TODO: rethink interaction between cursor movement and scrolling.
-- probably the coordinates in the buffer should be stored in the state, rather than the coordinates in the window.
-- That means the cursor would stay on the same character when scrolling automatically
-- TODO should be able to place the cursor after the end of a line
moveCursor :: (Int, Int) -> Window -> Window
moveCursor (dx,dy) Window{ .. } =
  let bufLines = bufferLines _windowBuffer
      Rect _ (_, winHeight) = _winRect
      (x, y) = _winCursorLocation

      y' = clamp 0 (y+dy) (winHeight - 1)

      -- ensure new lineNumber' is a line that exists
      -- 
      -- this is still buggy, this clamp of linenumber is not reflected in the final y'
      -- not worth fixing til after I redo scroll/cursor movement
      lineNumber' = clamp 0 (_winTopLine + y') (Seq.length bufLines - 1)
      currentLine = bufLines `Seq.index` lineNumber'

      x' = clamp 0 (x+dx) (T.length currentLine-1)
      newCursorLocation = (x', y')
   in Window _windowBuffer _winTopLine newCursorLocation _winRect _winShowStartMessage


scrollWindow :: Int -> Window -> Window
scrollWindow n (Window buf winTopLine cursor r ssm) =
  let newTopLine = clamp 0 (winTopLine + n) (bufferLineCount buf)
   in Window buf newTopLine cursor r ssm

clamp :: Ord a => a -> a -> a -> a
clamp a x b = max a (min x b)

data EditorMode = NormalMode | InsertMode

data AppState = AppState
  { _stateDimensions :: (Int, Int)
  , _stateLastEvent :: Maybe Event
  , _stateWindow :: Window
  , _stateMode :: EditorMode
  }

makeLenses ''AppState




