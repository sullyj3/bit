{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
import Relude

import qualified Graphics.Vty as Vty
import           Graphics.Vty hiding (update)
import Control.Monad.RWS.Strict
import Control.Monad.State.Class
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
--import Data.Text
import Flow

main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg

    initialState <- mkInitialState vty
    _ <- runRWST loop vty initialState
    shutdown vty
    putStrLn "bye!"

mkInitialState :: Vty -> IO AppState
mkInitialState vty = do
  bounds <- liftIO $ displayBounds $ outputIface vty
  pure $ initialAppState bounds

initialAppState :: (Int, Int) -> AppState
initialAppState bounds = AppState bounds Nothing EmptyWindow NormalMode

data AppState = AppState
  { stateDimensions :: (Int, Int)
  , stateLastEvent :: Maybe Event
  , stateWindow :: Window
  , stateMode :: EditorMode
  }


type App a = RWST Vty () AppState IO a


view :: App ()
view = do
  vty <- ask
  s <- get
  liftIO $ Vty.update vty (viewAppState s)

viewAppState :: AppState -> Picture
viewAppState state@AppState {..} = let
-- todo store mode in state
  (w,h) = stateDimensions
  bar = statusBar state True
  howToQuit = string defAttr "press Q to exit"

  pic = picForLayers [bar, howToQuit]
    in pic

statusBar :: AppState -> Bool -> Image
statusBar state@AppState {..} showDiagnostics = translate 0 (h-1) $ horizCat
  [ modeWidget, middlePadding, diagnosticsWidget, rightPadding ]
  where
    (w,h) = stateDimensions
    barBgAttr = defAttr `withBackColor` blue
    modeAttr = defAttr `withBackColor` white `withForeColor` blue

    showMode :: EditorMode -> String
    showMode = \case
      NormalMode -> "NORMAL"
      InsertMode -> "INSERT"

    remainingSpace = w - imageWidth modeWidget - imageWidth diagnosticsWidget

    modeWidget = string modeAttr $ " " <> showMode stateMode <> " "
    middlePadding = string barBgAttr $ replicate (remainingSpace - 1) ' '
    diagnosticsWidget | showDiagnostics = diagnostics state
                      | otherwise       = mempty
    rightPadding = char barBgAttr ' '

    -- hardcoded for now - the length of the words NORMAL and INSERT plus 1 
    -- space either side
    modeWidth = 8

diagnostics :: AppState -> Image
diagnostics AppState {..} =
  string (defAttr `withForeColor` white `withBackColor` blue) eventStr
  where
    eventStr = 
      maybe "no events yet" 
            (\e -> "Last event: " ++ show e)
            stateLastEvent


loop :: App ()
loop = do
  view
  shouldExit <- handleEvent
  if not shouldExit
    then loop
    else pure ()


-- return whether we should exit
handleEvent :: App Bool
handleEvent = do
  vty <- ask
  e <- liftIO $ nextEvent vty
  modify (\s -> s {stateLastEvent = Just e})
  case e of

    EvKey (KChar c) [] -> case c of
      'Q' -> pure True
      'o' -> do
        buf <- liftIO $ openFile "app/Main.hs"
        pure False

      'h' -> pure False
      'j' -> pure False
      'k' -> pure False
      'l' -> pure False
      _   -> pure False

    EvResize w h -> do
      modify (\s -> s { stateDimensions = (w,h) })
      pure False
    _ -> pure False


data EditorMode = NormalMode | InsertMode

newtype Buffer = Buffer { bufferLines :: Seq Text }

openFile :: FilePath -> IO Buffer
openFile path = Buffer . Seq.fromList . lines <$> readFileText path

data Window =
    EmptyWindow
  | BufferWindow { windowBuffer :: Buffer, winTopLine :: Int }
