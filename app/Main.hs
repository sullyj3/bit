{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
import Relude

import qualified Graphics.Vty as Vty
import           Graphics.Vty hiding (update)
import Control.Monad.RWS.Strict
import Control.Monad.State.Class
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

import Lens.Micro.Platform
import Control.Exception (bracket)

import Flow
import System.Environment (getArgs)
import qualified Data.Text as T

import View
import AppState

----------
-- Main --
----------

newtype AppArgs = AppArgs { argsFileToOpen :: Maybe FilePath }

parseArgs :: [String] -> Maybe AppArgs
parseArgs args = case args of
  [] -> Just (AppArgs Nothing)
  [fp] -> Just . AppArgs . Just $ fp
  _ -> Nothing

mkInitialState :: Vty -> AppArgs -> IO AppState
mkInitialState vty (AppArgs argsFileToOpen) = do
  bounds <- liftIO $ displayBounds $ outputIface vty
  let (w,h) = bounds
  let winRect = Rect (0,0) (w,h-1)
  initialWindow <- maybe (pure $ EmptyWindow winRect)
                         (\fp -> openFile fp <&> windowFromBuf winRect)
                         argsFileToOpen
  pure $ AppState bounds Nothing initialWindow NormalMode


main = do

  args <- getArgs >>= parseArgs
    .> maybe (die "invalid args") pure

  cfg <- standardIOConfig
  withVty cfg \vty -> do
    initialState <- mkInitialState vty args
    _ <- runRWST loop vty initialState
    putStrLn "bye!"
  where
    loop :: App ()
    loop = presentView >> handleEvent >>= \case
      Continue -> loop
      Quit     -> pure ()

    withVty :: Config -> (Vty -> IO c) -> IO c
    withVty cfg = bracket (mkVty cfg) shutdown

    presentView :: App ()
    presentView = do
      vty <- askVty
      s <- get
      liftIO $ Vty.update vty (viewAppState s)

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
    -- todo store buf in state
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
    -- todo
    pure Continue

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
