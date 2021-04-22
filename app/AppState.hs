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

data Window = Window { windowBuffer :: Buffer
                     , winTopLine :: Int
                     , winCursorLocation :: (Int, Int)
                     , winRect :: Rect 
                     , winShowStartMessage :: Bool }

windowFromBuf :: Rect -> Buffer -> Bool -> Window
windowFromBuf r b showStartMsg = Window b 0 (0,0) r showStartMsg

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
  let bufLines = bufferLines windowBuffer
      Rect _ (_, winHeight) = winRect
      (x, y) = winCursorLocation

      y' = clamp 0 (y+dy) (winHeight - 1)

      -- ensure new lineNumber' is a line that exists
      -- 
      -- this is still buggy, this clamp of linenumber is not reflected in the final y'
      -- not worth fixing til after I redo scroll/cursor movement
      lineNumber' = clamp 0 (winTopLine + y') (Seq.length bufLines - 1)
      currentLine = bufLines `Seq.index` lineNumber'

      x' = clamp 0 (x+dx) (T.length currentLine-1)
      newCursorLocation = (x', y')
   in Window windowBuffer winTopLine newCursorLocation winRect winShowStartMessage


scrollWindow :: Int -> Window -> Window
scrollWindow n (Window buf winTopLine cursor r ssm) =
  let newTopLine = clamp 0 (winTopLine + n) (bufferLineCount buf)
   in Window buf newTopLine cursor r ssm

clamp a x b = max a (min x b)

data EditorMode = NormalMode | InsertMode

data AppState = AppState
  { _stateDimensions :: (Int, Int)
  , _stateLastEvent :: Maybe Event
  , _stateWindow :: Window
  , _stateMode :: EditorMode
  }

makeLenses ''AppState




