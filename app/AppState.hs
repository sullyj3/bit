{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module AppState where
import Relude

import Graphics.Vty ( Event )
import qualified Data.Sequence as Seq

import Lens.Micro.Platform ( makeLenses )

import qualified Data.Text as T


-----------
-- State --
-----------

newtype Buffer = Buffer { bufferLines :: Seq Text }

newEmptyBuffer = Buffer $ Seq.singleton mempty

bufferLineCount (Buffer bLines) = Seq.length bLines

data Rect = Rect { rectTopLeft :: (Int, Int), rectDimensions :: (Int, Int) }
  deriving Show

-- left is a window with no buffer
type Window = Either Rect BufferWindow

data BufferWindow = BufferWindow { windowBuffer :: Buffer
                                 , winTopLine :: Int
                                 , winCursorLocation :: (Int, Int)
                                 , winRect :: Rect }

windowFromBuf :: Rect -> Buffer -> Window
windowFromBuf r b = Right $ BufferWindow b 0 (0,0) r

-- This function is safe at least when called - ensures the cursor stays inside the window, and doesn't move beyond the end of a line.
-- 
-- however if we scroll without moving the cursor, the cursor can end up in an invalid state, focusing on a character that doesn't exist
--
-- TODO: rethink interaction between cursor movement and scrolling.
-- possibly the coordinates in the buffer should be stored in the state, rather than the coordinates in the window.
-- That means the cursor would stay on the same character when scrolling automatically
moveCursor :: (Int, Int) -> BufferWindow -> BufferWindow
moveCursor (dx,dy) BufferWindow{ .. } = let
  Rect _ (_, winHeight) = winRect
  (x, y) = winCursorLocation
  currentLine :: Text
  currentLine = bufferLines windowBuffer `Seq.index` (winTopLine + y + dy)
  newCursorLocation = (clamp 0 (x+dx) (T.length currentLine-1), clamp 0 (y+dy) winHeight)
  in BufferWindow windowBuffer winTopLine newCursorLocation winRect


scrollWindow :: Int -> BufferWindow -> BufferWindow
scrollWindow n (BufferWindow buf winTopLine cursor r) =
  let newTopLine = clamp 0 (winTopLine + n) (bufferLineCount buf)
   in BufferWindow buf newTopLine cursor r

clamp a x b = max a (min x b)

data EditorMode = NormalMode | InsertMode

data AppState = AppState
  { _stateDimensions :: (Int, Int)
  , _stateLastEvent :: Maybe Event
  , _stateWindow :: Window
  , _stateMode :: EditorMode
  }

makeLenses ''AppState




