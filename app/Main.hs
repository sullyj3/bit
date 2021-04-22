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
import Control.Monad.RWS.Strict ( RWST(runRWST) )

import Control.Exception (bracket)

import Flow
import System.Environment (getArgs)

import View ( viewAppState )
import AppState
import HandleEvents (handleEvent, ShouldQuit(..), App, openFile, askVty)

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
  initialWindow <- mkInitialWindow winRect argsFileToOpen
  pure $ AppState bounds Nothing initialWindow NormalMode

mkInitialWindow :: Rect -> Maybe FilePath -> IO Window
mkInitialWindow winRect = \case
  Just fp -> do
    buf <- openFile fp
    pure $ windowFromBuf winRect buf False
  Nothing -> pure $ windowFromBuf winRect newEmptyBuffer True


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


