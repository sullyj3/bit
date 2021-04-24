{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HandleEvents where

import AppState
import Control.Monad.RWS.Strict (RWST)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((|>))
import Graphics.Vty hiding (update)
import Lens.Micro.Platform
import Relude

type App a = RWST Vty () AppState IO a

askVty :: App Vty
askVty = ask

------------
-- Events --
------------

data CmdType = NormalModeCmd | InsertModeCmd | AnyModeCmd

data Command a where
  CmdScroll :: Int -> Command 'NormalModeCmd
  CmdEnterInsertMode :: Command 'NormalModeCmd
  CmdQuit :: Command 'NormalModeCmd
  CmdOpenFile :: FilePath -> Command 'NormalModeCmd
  CmdMoveCursorRelative :: (Int, Int) -> Command 'NormalModeCmd
  CmdEnterNormalMode :: Command 'InsertModeCmd
  CmdInsertChar :: Char -> Command 'InsertModeCmd
  CmdBackspace :: Command 'InsertModeCmd
  CmdInsertNewline :: Command 'InsertModeCmd
  CmdDel :: Command 'InsertModeCmd

handleNormalModeCmd :: Command 'NormalModeCmd -> App ShouldQuit
handleNormalModeCmd = \case
  CmdMoveCursorRelative v -> Continue <$ (stateWindow %= moveCursor v)
  CmdScroll n -> Continue <$ (stateWindow %= scrollWindow n)
  CmdEnterInsertMode ->
    Continue <$ do
      stateMode .= InsertMode
      stateWindow %= (\win -> win {_winShowStartMessage = False})
  CmdQuit -> pure Quit
  CmdOpenFile fp -> do
    buf <- liftIO $ openFile fp
    dims <- use stateDimensions
    let window = windowFromBuf (rectFullScreen dims) buf False
    stateWindow .= window
    pure Continue

rectFullScreen :: (Int, Int) -> Rect
rectFullScreen (w, h) = Rect (0, 0) (w, h -1)

-- handleNormalModeCmd must be able to do IO (eg for opening files).
-- At least for now, lets see if we can get away with handling insert mode
-- commands purely.
handleInsertModeCmd :: MonadState AppState m => Command 'InsertModeCmd -> m ShouldQuit
handleInsertModeCmd =
  (Continue <$) . \case
    CmdEnterNormalMode -> stateMode .= NormalMode
    CmdInsertChar c -> stateWindow %= winInsertChar c
    CmdBackspace -> stateWindow %= winBackspace
    CmdInsertNewline -> stateWindow %= winInsertNewline
    CmdDel -> stateWindow %= winDel

winInsertChar :: Char -> Window -> Window
winInsertChar c win@Window {_windowBuffer = Buffer fp bufLines, ..} =
  moveCursor (1, 0) win'
  where
    CursorLocation curCol curLine = win ^. winCursorLocation

    bufLines' = Seq.adjust' (insertChar c curCol) curLine bufLines
    win' = win {_windowBuffer = Buffer fp bufLines'}

-- delete the character before the cursor, and move the cursor back one.
winBackspace :: Window -> Window
winBackspace win@Window {_windowBuffer = Buffer fp bufLines} =
  moveCursor (-1, 0) win'
  where
    CursorLocation curCol curLine = win ^. winCursorLocation
    -- if the cursor is at column 0, deleteChar (-1) will be a no-op
    -- TODO allow backspace to span lines
    bufLines' = Seq.adjust' (deleteChar $ curCol - 1) curLine bufLines
    win' = win |> windowBuffer .~ Buffer fp bufLines'

-- delete the character at the cursor. If we were on the last character,
-- move the cursor back one
winDel :: Window -> Window
winDel win@Window {_windowBuffer = Buffer fp bufLines}
  | curCol == lenCurrLine = moveCursor (-1, 0) win'
  | otherwise = win'
  where
    CursorLocation curCol curLine = win ^. winCursorLocation

    bufLines' = Seq.adjust' (deleteChar curCol) curLine bufLines
    lenCurrLine = T.length $ Seq.index bufLines' curLine
    win' = win {_windowBuffer = Buffer fp bufLines'}

winInsertNewline :: Window -> Window
winInsertNewline win@Window {_windowBuffer = Buffer fp bufLines} =
  moveCursor (0, 1) win'
  where
    CursorLocation curCol curLine = win ^. winCursorLocation
    line = bufLines `Seq.index` curLine
    (l, r) = T.splitAt curCol line
    (top, bottom) = Seq.splitAt curLine bufLines
    -- first element of bottom is the current line, we drop it and replace with
    -- the two halves of the split line
    bufLines' = top <> Seq.fromList [l, r] <> Seq.drop 1 bottom
    win' = win {_windowBuffer = Buffer fp bufLines'}

-- does nothing if i ∉ [0, T.length txt)
deleteChar :: Int -> Text -> Text
deleteChar i txt
  | i < 0 = txt
  | i >= T.length txt = txt
  | otherwise = let (l, r) = T.splitAt i txt in l <> T.tail r

-- TODO probably inefficient, especially for long lines
insertChar :: Char -> Int -> Text -> Text
insertChar c i txt = l <> T.singleton c <> r
  where
    (l, r) = T.splitAt i txt

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
      -- TODO need to update window size here, I think.
      stateDimensions .= (w, h)
      pure Continue
    _ ->
      use stateMode >>= \case
        NormalMode -> maybe (pure Continue) handleNormalModeCmd case ev of
          EvKey (KChar c) [] -> case c of
            'Q' -> Just CmdQuit
            'i' -> Just CmdEnterInsertMode
            'o' -> Just $ CmdOpenFile "app/Main.hs"
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
        InsertMode -> maybe
          (pure Continue)
          handleInsertModeCmd
          case ev of
            EvKey KEsc [] -> Just CmdEnterNormalMode
            EvKey (KChar c) [] -> Just $ CmdInsertChar c
            EvKey KBS [] -> Just CmdBackspace
            EvKey KDel [] -> Just CmdDel
            EvKey KEnter [] -> Just CmdInsertNewline
            _ -> Nothing

openFile :: FilePath -> IO Buffer
openFile path = Buffer (Just path) . Seq.fromList . lines <$> readFileText path
