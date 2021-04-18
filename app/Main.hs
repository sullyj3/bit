{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
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
import Control.Exception (bracket)

-----------
-- State --
-----------

data AppState = AppState
  { stateDimensions :: (Int, Int)
  , stateLastEvent :: Maybe Event
  , stateWindow :: Window
  , stateMode :: EditorMode
  }

data EditorMode = NormalMode | InsertMode

newtype Buffer = Buffer { bufferLines :: Seq Text }

data Window =
    EmptyWindow
  | BufferWindow { windowBuffer :: Buffer, winTopLine :: Int }

mkInitialState :: Vty -> IO AppState
mkInitialState vty = do
  bounds <- liftIO $ displayBounds $ outputIface vty
  pure $ initialAppState bounds

initialAppState :: (Int, Int) -> AppState
initialAppState bounds = AppState bounds Nothing EmptyWindow NormalMode

getState :: App AppState
getState = get

getMode :: App EditorMode
getMode = getState <&> stateMode

setEditorMode :: EditorMode -> AppState -> AppState
setEditorMode mode state = state {stateMode = mode}

askVty :: App Vty
askVty = ask

----------
-- Main --
----------

main = do
  cfg <- standardIOConfig
  withVty cfg \vty -> do
    initialState <- mkInitialState vty
    _ <- runRWST loop vty initialState
    putStrLn "bye!"
  where
    loop :: App ()
    loop = view >> handleEvent >>= \case
      Continue -> loop
      Quit     -> pure ()

    withVty :: Config -> (Vty -> IO c) -> IO c
    withVty cfg = bracket (mkVty cfg) shutdown

type App a = RWST Vty () AppState IO a

----------
-- View --
----------

view :: App ()
view = do
  vty <- askVty
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
    diagnosticsWidget | showDiagnostics = viewDiagnostics state
                      | otherwise       = mempty
    rightPadding = char barBgAttr ' '

    -- hardcoded for now - the length of the words NORMAL and INSERT plus 1 
    -- space either side
    modeWidth = 8

viewDiagnostics :: AppState -> Image
viewDiagnostics AppState {..} =
  string (defAttr `withForeColor` white `withBackColor` blue) eventStr
  where
    eventStr =
      maybe "no events yet"
            (\e -> "Last event: " ++ show e)
            stateLastEvent

------------
-- Events --
------------

data NormalModeCmd = CmdEnterInsertMode
                   | CmdQuit
                   | CmdOpenFile FilePath


data InsertModeCmd = CmdEnterNormalMode
                   | CmdInsertChar Char

handleNormalModeCmd :: NormalModeCmd -> App ShouldQuit
handleNormalModeCmd = \case
  CmdEnterInsertMode -> do
    getMode >>= \case
      InsertMode -> pure Continue -- todo handle input
      NormalMode -> do
        modify (setEditorMode InsertMode)
        pure Continue
  CmdQuit -> pure Quit
  CmdOpenFile fp -> do
    -- todo store buf in state
    --"app/Main.hs"
    buf <- liftIO $ openFile fp
    pure Continue

handleInsertModeCmd :: InsertModeCmd -> App ShouldQuit
handleInsertModeCmd = \case
  CmdEnterNormalMode -> do
    getMode >>= \case
      NormalMode -> pure Continue
      InsertMode -> do
        modify (setEditorMode NormalMode)
        pure Continue

  CmdInsertChar c -> do
    -- todo
    pure Continue

data ShouldQuit = Quit | Continue
  deriving (Show)

-- return whether we should continue
handleEvent :: App ShouldQuit
handleEvent = do
  vty <- askVty
  e <- liftIO $ nextEvent vty
  modify (\s -> s {stateLastEvent = Just e})

  case e of
    EvResize w h -> do
      modify (\s -> s { stateDimensions = (w,h) })
      pure Continue
    _ ->
      getMode >>= \case
        NormalMode -> maybe (pure Continue) handleNormalModeCmd case e of
          EvKey (KChar c) [] -> case c of
            'Q' -> Just CmdQuit
            'i' -> Just CmdEnterInsertMode

            'h' -> Nothing
            'j' -> Nothing
            'k' -> Nothing
            'l' -> Nothing
            _ -> Nothing

        InsertMode -> maybe (pure Continue) handleInsertModeCmd case e of
          EvKey KEsc [] -> Just CmdEnterNormalMode
          _ -> Nothing


openFile :: FilePath -> IO Buffer
openFile path = Buffer . Seq.fromList . lines <$> readFileText path
