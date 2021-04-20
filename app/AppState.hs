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

import           Graphics.Vty hiding (update)
import qualified Data.Sequence as Seq

import Lens.Micro.Platform

import qualified Data.Text as T


-----------
-- State --
-----------

newtype Buffer = Buffer { bufferLines :: Seq Text }

bufferLineCount (Buffer bLines) = Seq.length bLines

data Rect = Rect { rectTopLeft :: (Int, Int), rectDimensions :: (Int, Int) }

data Window =
    EmptyWindow Rect
  | BufferWindow { windowBuffer :: Buffer
                 , winTopLine :: Int
                 , winCursorLocation :: (Int, Int)
                 , winRect :: Rect }

windowFromBuf :: Rect -> Buffer -> Window
windowFromBuf r b = BufferWindow b 0 (0,0) r

moveCursor :: (Int, Int) -> Window -> Window
moveCursor (dx,dy) BufferWindow{ .. } = let
  Rect _ (_, winHeight) = winRect
  (x, y) = winCursorLocation
  currentLine :: Text
  currentLine = bufferLines windowBuffer `Seq.index` (winTopLine + y + dy)
  newCursorLocation = (clamp 0 (x+dx) (T.length currentLine-1), clamp 0 (y+dy) winHeight)
  in BufferWindow windowBuffer winTopLine newCursorLocation winRect
moveCursor _ w@(EmptyWindow _) = w

scrollWindow :: Int -> Window -> Window
scrollWindow _ (EmptyWindow r) = EmptyWindow r
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




