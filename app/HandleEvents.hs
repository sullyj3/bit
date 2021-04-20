{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module HandleEvents where
import Relude

import Control.Monad.RWS.Strict ( RWST )
import           Graphics.Vty hiding (update)
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Lens.Micro.Platform


import AppState

type App a = RWST Vty () AppState IO a

askVty :: App Vty
askVty = ask


------------
-- Events --
------------

data CmdType = NormalModeCmd | InsertModeCmd | AnyModeCmd

data Command a where
  CmdScroll          :: Int -> Command NormalModeCmd
  CmdEnterInsertMode :: Command NormalModeCmd
  CmdQuit            :: Command NormalModeCmd
  CmdOpenFile        :: FilePath -> Command NormalModeCmd
  CmdMoveCursorRelative :: (Int, Int) -> Command NormalModeCmd

  CmdEnterNormalMode :: Command InsertModeCmd
  CmdInsertChar      :: Char -> Command InsertModeCmd
  CmdBackspace       :: Command InsertModeCmd

handleNormalModeCmd :: Command NormalModeCmd -> App ShouldQuit
handleNormalModeCmd = \case
  CmdMoveCursorRelative v -> Continue <$ (stateWindow %= moveCursor v)
  CmdScroll n ->             Continue <$ (stateWindow %= scrollWindow n)
  CmdEnterInsertMode ->      Continue <$ (stateMode .= InsertMode)
  CmdQuit -> pure Quit
  CmdOpenFile fp -> do
    buf <- liftIO $ openFile fp
    dims <- use stateDimensions
    let window = windowFromBuf (rectFullScreen dims) buf
    stateWindow .= window
    pure Continue

rectFullScreen :: (Int, Int) -> Rect
rectFullScreen (w, h) = Rect (0,0) (w, h-1)

handleInsertModeCmd :: Command InsertModeCmd -> App ShouldQuit
handleInsertModeCmd = \case
  CmdEnterNormalMode -> Continue <$ (stateMode .= NormalMode)

  CmdInsertChar c -> do
    -- todo we shouldn't have to pattern match here
    stateWindow %= \case
      EmptyWindow r -> EmptyWindow r
      win@BufferWindow {..} ->
        let Buffer bufLines = windowBuffer
            (x,y) = winCursorLocation
            bufLines' :: Seq Text
            bufLines' = Seq.adjust' (insertChar c x) (winTopLine + y) bufLines
         in win {windowBuffer = Buffer bufLines'}
    stateWindow %= moveCursor (1,0)
    pure Continue

-- TODO probably inefficient, especially for long lines
insertChar :: Char -> Int -> Text -> Text
insertChar c i txt = 
  let (l,r) = T.splitAt i txt
      line' = l <> T.singleton c <> r
   in line'

  

data ShouldQuit = Quit | Continue
  deriving (Show)

-- return whether we should continue
handleEvent :: App ShouldQuit
handleEvent = do
  vty <- askVty

  ev <- liftIO $ nextEvent vty
  stateLastEvent ?= ev

  case ev of
    EvResize w h -> do
      stateDimensions .= (w,h)
      pure Continue
    _ ->
      use stateMode >>= \case
        NormalMode -> maybe (pure Continue) handleNormalModeCmd case ev of
          EvKey (KChar c) [] -> case c of
            'Q' -> Just CmdQuit
            'i' -> Just CmdEnterInsertMode
            'o' -> Just $ CmdOpenFile "app/Main.hs"

            -- todo: cursor movement
            'h' -> Just $ CmdMoveCursorRelative (-1, 0)
            'j' -> Just $ CmdMoveCursorRelative (0, 1)
            'k' -> Just $ CmdMoveCursorRelative (0, -1)
            'l' -> Just $ CmdMoveCursorRelative (1, 0)

            'm' -> Just $ CmdScroll 1
            ',' -> Just $ CmdScroll (-1)

            -- ignore all other chars
            _ -> Nothing
          -- ignore all other keys
          EvKey _ _ -> Nothing
          -- ignore all other events
          _ -> Nothing

        InsertMode -> maybe (pure Continue) handleInsertModeCmd case ev of
          EvKey KEsc [] -> Just CmdEnterNormalMode
          EvKey (KChar '\b') [] -> Just CmdBackspace
          EvKey (KChar c) [] -> Just $ CmdInsertChar c
          _ -> Nothing


openFile :: FilePath -> IO Buffer
openFile path = Buffer . Seq.fromList . lines <$> readFileText path
