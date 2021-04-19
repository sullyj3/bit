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

import Lens.Micro
import Lens.Micro.TH
import Control.Exception (bracket)

import Flow
-----------
-- State --
-----------

newtype Buffer = Buffer { bufferLines :: Seq Text }

bufferLineCount (Buffer bLines) = Seq.length bLines

data Window =
    EmptyWindow
  | BufferWindow { windowBuffer :: Buffer, winTopLine :: Int }

windowFromBuf :: Buffer -> Window
windowFromBuf b = BufferWindow b 0

scrollWindow :: Int -> Window -> Window
scrollWindow _ EmptyWindow = EmptyWindow
scrollWindow n (BufferWindow buf winTopLine) =
  let newTopLine = clamp 0 (winTopLine + n) (bufferLineCount buf)
   in BufferWindow buf newTopLine

clamp a x b = max a (min x b)

data EditorMode = NormalMode | InsertMode

data AppState = AppState
  { _stateDimensions :: (Int, Int)
  , _stateLastEvent :: Maybe Event
  , _stateWindow :: Window
  , _stateMode :: EditorMode
  }

makeLenses ''AppState



mkInitialState :: Vty -> IO AppState
mkInitialState vty = do
  bounds <- liftIO $ displayBounds $ outputIface vty
  pure $ initialAppState bounds

initialAppState :: (Int, Int) -> AppState
initialAppState bounds = AppState bounds Nothing EmptyWindow NormalMode

getState :: App AppState
getState = get

getMode :: App EditorMode
getMode = getState <&> (^. stateMode)

setEditorMode :: EditorMode -> AppState -> AppState
setEditorMode mode state = state {_stateMode = mode}

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
viewAppState state = let
-- todo store mode in state
  (w,h) = state ^. stateDimensions
  bar = statusBar state True
  howToQuit = string defAttr "press Q to exit"

  mainWindow = case state ^. stateWindow of
    EmptyWindow -> howToQuit
    BufferWindow (Buffer bufLines) topLineNumber -> let
      linesToDisplay = Seq.take (h-1) $ Seq.drop topLineNumber bufLines

      showLine :: Text -> Image
      showLine = text' defAttr

      in vertCat $ toList $ showLine <$> linesToDisplay

  pic = picForLayers [bar, mainWindow]
    in pic

statusBar :: AppState -> Bool -> Image
statusBar state showDiagnostics = translate 0 (h-1) $ horizCat
  [ modeWidget, middlePadding, diagnosticsWidget, rightPadding ]
  where
    (w,h) = state ^. stateDimensions
    barBgAttr = defAttr `withBackColor` blue
    modeAttr = defAttr `withBackColor` white `withForeColor` blue

    showMode :: EditorMode -> String
    showMode = \case
      NormalMode -> "NORMAL"
      InsertMode -> "INSERT"

    remainingSpace = w - imageWidth modeWidget - imageWidth diagnosticsWidget

    modeWidget = string modeAttr $ " " <> showMode (state ^. stateMode) <> " "
    middlePadding = string barBgAttr $ replicate (remainingSpace - 1) ' '
    diagnosticsWidget | showDiagnostics = viewDiagnostics state
                      | otherwise       = mempty
    rightPadding = char barBgAttr ' '

    -- hardcoded for now - the length of the words NORMAL and INSERT plus 1 
    -- space either side
    modeWidth = 8

viewDiagnostics :: AppState -> Image
viewDiagnostics state =
  string (defAttr `withForeColor` white `withBackColor` blue) eventStr
  where
    eventStr =
      maybe "no events yet"
            (\e -> "Last event: " ++ show e)
            (state ^. stateLastEvent)

------------
-- Events --
------------

data CmdType = NormalModeCmd | InsertModeCmd | AnyModeCmd

data Command a where
  CmdScroll          :: Int -> Command NormalModeCmd
  CmdEnterInsertMode :: Command NormalModeCmd
  CmdQuit            :: Command NormalModeCmd
  CmdOpenFile        :: FilePath -> Command NormalModeCmd

  CmdEnterNormalMode :: Command InsertModeCmd
  CmdInsertChar      :: Char -> Command InsertModeCmd

handleNormalModeCmd :: Command NormalModeCmd -> App ShouldQuit
handleNormalModeCmd = \case
  CmdScroll n -> do
    modify (stateWindow %~ scrollWindow n)
    pure Continue
  CmdEnterInsertMode -> do
    modify (setEditorMode InsertMode)
    pure Continue
  CmdQuit -> pure Quit
  CmdOpenFile fp -> do
    -- todo store buf in state
    buf <- liftIO $ openFile fp
    let window = windowFromBuf buf
    modify (stateWindow .~ window)
    pure Continue


handleInsertModeCmd :: Command InsertModeCmd -> App ShouldQuit
handleInsertModeCmd = \case
  CmdEnterNormalMode -> do
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

  ev <- liftIO $ nextEvent vty
  modify (stateLastEvent ?~ ev)

  case ev of
    EvResize w h -> do
      modify (stateDimensions .~ (w,h))
      pure Continue
    _ ->
      getMode >>= \case
        NormalMode -> maybe (pure Continue) handleNormalModeCmd case ev of
          EvKey (KChar c) [] -> case c of
            'Q' -> Just CmdQuit
            'i' -> Just CmdEnterInsertMode
            'o' -> Just $ CmdOpenFile "app/Main.hs"

            -- todo: cursor movement
            'h' -> Nothing
            'j' -> Nothing
            'k' -> Nothing
            'l' -> Nothing

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
          _ -> Nothing


openFile :: FilePath -> IO Buffer
openFile path = Buffer . Seq.fromList . lines <$> readFileText path
