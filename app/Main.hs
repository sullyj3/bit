{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AppState
import Control.Exception (bracket)
import Control.Monad.RWS.Strict (RWST (runRWST))
import Flow ((.>))
import Graphics.Vty hiding (update)
import qualified Graphics.Vty as Vty
import HandleEvents (App, ShouldQuit (..), askVty, handleEvent, openFileIO)
import Relude
import System.Environment (getArgs)
import View (viewAppState)
import qualified Data.Map.NonEmpty as NEMap
import Lens.Micro.Platform ((^.))

----------
-- Main --
----------

newtype AppArgs = AppArgs {argsFileToOpen :: Maybe FilePath}

parseArgs :: [String] -> Maybe AppArgs
parseArgs args = case args of
  [] -> Just (AppArgs Nothing)
  [fp] -> Just . AppArgs . Just $ fp
  _ -> Nothing

initialBufferID :: BufferID
initialBufferID = BufferID 0

mkInitialState :: Vty -> AppArgs -> IO AppState
mkInitialState vty AppArgs {..} = do
  bounds <- liftIO $ displayBounds $ outputIface vty
  let (w, h) = bounds
  -- h-1 leaves room for the status bar
  -- TODO this is a bug waiting to happen, figure out something more principled
  initialBuf <- mkInitialBuffer argsFileToOpen
  let windowRect = Rect (0, 0) (w, h -1)
      initialWindow = mkInitialWindow windowRect argsFileToOpen initialBufferID
  pure AppState { _stateDimensions = bounds,
    _stateLastEvent = Nothing,
    _stateWindow = initialWindow,
    _stateMode = NormalMode,
    _stateStatusMessage = Nothing,
    _stateCurrInputWidget = Nothing,
    _stateOpenBuffers = OpenBuffers $ NEMap.singleton (initialBuf ^. bufferID) initialBuf,
    _stateNextBufID = succ (initialBuf ^. bufferID) }

mkInitialWindow :: Rect -> Maybe FilePath -> BufferID -> Window
mkInitialWindow rect fp buf =
  windowFromBufID rect buf case fp of
    Just _ -> False
    Nothing -> True

mkInitialBuffer :: Maybe FilePath -> IO Buffer
mkInitialBuffer = \case
  Just fp -> openFileIO fp initialBufferID
  Nothing -> pure (newEmptyBuffer initialBufferID)

main :: IO ()
main = do
  args <-
    getArgs >>= parseArgs
      .> maybe (die "invalid args") pure

  cfg <- standardIOConfig
  withVty cfg \vty -> do
    initialState <- mkInitialState vty args
    _ <- runRWST loop vty initialState
    putStrLn "bye!"
  where
    loop :: App ()
    loop =
      presentView >> handleEvent >>= \case
        Continue -> loop
        Quit -> pure ()

    withVty :: Config -> (Vty -> IO c) -> IO c
    withVty cfg = bracket (mkVty cfg) shutdown

    presentView :: App ()
    presentView = do
      vty <- askVty
      s <- get
      liftIO $ Vty.update vty (viewAppState s)
