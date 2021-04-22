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
  CmdInsertNewline   :: Command InsertModeCmd

handleNormalModeCmd :: Command NormalModeCmd -> App ShouldQuit
handleNormalModeCmd = \case
  CmdMoveCursorRelative v -> Continue <$ (stateWindow %= moveCursor v)
  CmdScroll n ->             Continue <$ (stateWindow %= scrollWindow n)
  CmdEnterInsertMode -> Continue <$ do
    stateMode .= InsertMode
    stateWindow %= (\win -> win {winShowStartMessage = False})
  CmdQuit -> pure Quit
  CmdOpenFile fp -> do
    buf <- liftIO $ openFile fp
    dims <- use stateDimensions
    let window = windowFromBuf (rectFullScreen dims) buf False
    stateWindow .= window
    pure Continue

rectFullScreen :: (Int, Int) -> Rect
rectFullScreen (w, h) = Rect (0,0) (w, h-1)

-- TODO at least for now we can probably get away with handling commands purely, eg
-- handleInsertModeCmd :: Command InsertModeCmd -> AppState -> (AppState, ShouldQuit)

handleInsertModeCmd :: Command InsertModeCmd -> App ShouldQuit
handleInsertModeCmd cmd = do
  win@Window {..} <- use stateWindow

  case cmd of
    CmdEnterNormalMode -> Continue <$ (stateMode .= NormalMode)

    CmdInsertChar c -> do
      let Buffer bufLines = windowBuffer
          (x,y) = winCursorLocation
          bufLines' :: Seq Text
          bufLines' = Seq.adjust' (insertChar c x) (winTopLine + y) bufLines
          win' = win {windowBuffer = Buffer bufLines'}
      assign stateWindow $ moveCursor (1,0) win'
      pure Continue
    CmdBackspace -> do
      let Buffer bufLines = windowBuffer
          (x,y) = winCursorLocation
          -- delete the character before the cursor, and move the cursor back one.
          bufLines' :: Seq Text
          bufLines' = Seq.adjust' (deleteChar $ x-1) (winTopLine + y) bufLines
          win' = win {windowBuffer = Buffer bufLines'}
      assign stateWindow $ moveCursor (-1,0) win'
      pure Continue
    CmdInsertNewline -> do
      let Buffer bufLines = windowBuffer
          (x,y) = winCursorLocation
          currLineNumber = winTopLine + y
          currLine = bufLines `Seq.index` currLineNumber
          (l,r) = T.splitAt x currLine
          (top,bottom) = Seq.splitAt currLineNumber bufLines

          -- first element of bottom is the current line, we drop it and replace with our two new lines
          bufLines' = top <> Seq.fromList [l,r] <> Seq.drop 1 bottom

          win' = win {windowBuffer = Buffer bufLines'}
      assign stateWindow $ moveCursor (0, 1) win'
      pure Continue

-- does nothing if i ∉ [0, T.length txt)
deleteChar :: Int -> Text -> Text
deleteChar i txt | i < 0 = txt
                 | i >= T.length txt = txt
                 | otherwise = let (l,r) = T.splitAt i txt in l <> T.tail r

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
          EvKey (KChar c) [] -> Just $ CmdInsertChar c
          EvKey KBS [] -> Just CmdBackspace
          EvKey KEnter [] -> Just CmdInsertNewline
          _ -> Nothing


openFile :: FilePath -> IO Buffer
openFile path = Buffer . Seq.fromList . lines <$> readFileText path
