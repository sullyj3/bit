{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module HandleEvents where
import Relude

import Control.Monad.RWS.Strict ( RWST )
import           Graphics.Vty hiding (update)
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Lens.Micro.Platform


import AppState
import Flow ((|>))

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

-- handleNormalModeCmd must be able to do IO (eg for opening files).
-- At least for now, lets see if we can get away with handling insert mode
-- commands purely.
handleInsertModeCmd :: Command InsertModeCmd -> AppState -> (AppState, ShouldQuit)
handleInsertModeCmd cmd state = let
  win@Window {..} = state ^. stateWindow
  (x,y) = winCursorLocation
  Buffer bufLines = windowBuffer
  in case cmd of
    CmdEnterNormalMode -> (state |> stateMode .~ NormalMode, Continue)
    CmdInsertChar c ->
      let bufLines' :: Seq Text
          bufLines' = Seq.adjust' (insertChar c x) (winTopLine + y) bufLines
          win' = win {windowBuffer = Buffer bufLines'}
       in (state |> stateWindow .~ moveCursor (1,0) win', Continue)

    CmdBackspace ->
      let -- delete the character before the cursor, and move the cursor back one.
          bufLines' :: Seq Text
          bufLines' = Seq.adjust' (deleteChar $ x-1) (winTopLine + y) bufLines
          win' = win {windowBuffer = Buffer bufLines'}
       in (state |> stateWindow .~ moveCursor (-1,0) win', Continue)

    CmdInsertNewline -> do
      let currLineNumber = winTopLine + y
          currLine = bufLines `Seq.index` currLineNumber
          (l,r) = T.splitAt x currLine
          (top,bottom) = Seq.splitAt currLineNumber bufLines
          -- first element of bottom is the current line, we drop it and replace with
          -- the two halves of the split line
          bufLines' = top <> Seq.fromList [l,r] <> Seq.drop 1 bottom
          win' = win {windowBuffer = Buffer bufLines'}
       in (state |> stateWindow .~ moveCursor (0, 1) win', Continue)


-- does nothing if i ∉ [0, T.length txt)
deleteChar :: Int -> Text -> Text
deleteChar i txt | i < 0 = txt
                 | i >= T.length txt = txt
                 | otherwise = let (l,r) = T.splitAt i txt in l <> T.tail r

-- TODO probably inefficient, especially for long lines
insertChar :: Char -> Int -> Text -> Text
insertChar c i txt = l <> T.singleton c <> r
  where
    (l,r) = T.splitAt i txt


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

        InsertMode -> maybe
          (pure Continue)
          (\cmd -> do state <- get
                      let (state', shouldContinue) = handleInsertModeCmd cmd state
                      put state'
                      pure shouldContinue)
          case ev of
            EvKey KEsc [] -> Just CmdEnterNormalMode
            EvKey (KChar c) [] -> Just $ CmdInsertChar c
            EvKey KBS [] -> Just CmdBackspace
            EvKey KEnter [] -> Just CmdInsertNewline
            _ -> Nothing


openFile :: FilePath -> IO Buffer
openFile path = Buffer . Seq.fromList . lines <$> readFileText path
