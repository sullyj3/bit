{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AppState
import Buffer (Buffer, BufferID (..))
import qualified Buffer
import Control.Exception (bracket)
import Control.Monad.RWS.Strict (RWST (runRWST))
import qualified Data.Map.NonEmpty as NEMap
import Flow ((.>))
import Graphics.Vty (Vty)
import qualified Graphics.Vty as Vty
import HandleEvents (App, ShouldQuit (..), askVty, handleEvent, openFile)
import Relude
import System.Environment (getArgs)
import View (viewAppState)

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
  bounds@(w, h) <- liftIO $ Vty.displayBounds $ Vty.outputIface vty
  initialBuf <- mkInitialBuffer argsFileToOpen
  -- h-1 leaves room for the status bar
  let windowRect = Rect (0, 0) (w, h -1)
      initialWindow = mkInitialWindow windowRect argsFileToOpen initialBufferID
  pure
    AppState
      { _stateDimensions = bounds,
        _stateLastEvent = Nothing,
        _stateWindow = initialWindow,
        _stateMode = NormalMode,
        _stateStatusMessage = Nothing,
        _stateCurrInputWidget = Nothing,
        _stateOpenBuffers = OpenBuffers $ NEMap.singleton initialBufferID initialBuf,
        _stateNextBufID = succ initialBufferID
      }

mkInitialWindow :: Rect -> Maybe FilePath -> BufferID -> Window
mkInitialWindow rect fp buf =
  windowFromBufID rect buf case fp of
    Just _ -> False
    Nothing -> True

mkInitialBuffer :: Maybe FilePath -> IO Buffer
mkInitialBuffer = \case
  Just fp -> openFile fp
  Nothing -> pure Buffer.empty

main :: IO ()
main = do
  args <-
    getArgs >>= parseArgs
      .> maybe (die "invalid args") pure

  cfg <- Vty.standardIOConfig
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

    withVty :: Vty.Config -> (Vty -> IO c) -> IO c
    withVty cfg = bracket (Vty.mkVty cfg) Vty.shutdown

    presentView :: App ()
    presentView = do
      vty <- askVty
      s <- get
      liftIO $ Vty.update vty (viewAppState s)
