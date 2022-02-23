{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HandleEvents where

import AppState
  ( AppState,
    EditorMode (InsertMode, NormalMode),
    InputWidget (..),
    InputWidgetType (InputWidgetSaveAsPath),
    Window (_winShowStartMessage),
    backspace,
    del,
    getCurrentBuffer,
    insertBuffer,
    insertChar,
    insertNewline,
    modifyCurrentBufferState,
    moveCursorWin,
    scrollWindow,
    stateCurrInputWidget,
    stateDimensions,
    stateLastEvent,
    stateMode,
    stateNextBufID,
    stateOpenBuffers,
    stateStatusMessage,
    stateWindow,
    windowFromBufID,
  )
import Cursor (CursorMovement)
import Buffer (Buffer (..), BufferContents, BufferID, bufferChanged, bufferFilePath, bufferLines)
import qualified Buffer
import qualified Cursor
import Control.Monad.RWS.Strict (RWST)
import qualified Data.Text as T
import Flow
import Graphics.Vty hiding (update)
import Lens.Micro.Platform
import Misc
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
  CmdMoveCursor :: CursorMovement () -> Command 'NormalModeCmd
  CmdEnterNormalMode :: Command 'InsertModeCmd
  CmdInsertChar :: Char -> Command 'InsertModeCmd
  CmdBackspace :: Command 'InsertModeCmd
  CmdInsertNewline :: Command 'InsertModeCmd
  CmdDel :: Command 'InsertModeCmd
  CmdSave :: Command 'NormalModeCmd
  CmdSaveAs :: Command 'NormalModeCmd
  CmdNewBuffer :: Command 'NormalModeCmd

newBufferID :: MonadState AppState m => m BufferID
newBufferID = do
  bid <- use stateNextBufID
  stateNextBufID %= succ
  pure bid

handleNormalModeCmd :: Command 'NormalModeCmd -> App ShouldQuit
handleNormalModeCmd cmd = do
  currBuf <- getCurrentBuffer <$> get
  let bufLines = _bufferLines currBuf
  case cmd of
    CmdMoveCursor movement -> do
      stateWindow %= moveCursorWin movement bufLines
      pure Continue
    CmdScroll n -> Continue <$ (stateWindow %= scrollWindow n bufLines)
    CmdEnterInsertMode ->
      Continue <$ do
        stateMode .= InsertMode
        stateWindow %= (\win -> win {_winShowStartMessage = False})
    CmdQuit -> pure Quit
    CmdOpenFile fp -> do
      buf <- liftIO $ Buffer.openFile fp
      dims <- use stateDimensions
      bid <- newBufferID
      stateOpenBuffers %= insertBuffer bid buf
      let window = windowFromBufID (rectFullScreen dims) bid False
      stateWindow .= window
      pure Continue
    CmdSave -> do
      Buffer {_bufferFilePath, _bufferLines} <- getCurrentBuffer <$> get
      case _bufferFilePath of
        Nothing -> openSaveAsDialog
        Just path -> do
          saveContentsToPath path _bufferLines
          pure Continue
    CmdSaveAs -> openSaveAsDialog
    CmdNewBuffer -> do
      error "Creating new buffers not yet implemented"

openSaveAsDialog :: App ShouldQuit
openSaveAsDialog = do
  Buffer {_bufferFilePath = currPath} <- getCurrentBuffer <$> get
  let contents :: Text
      contents = maybe mempty fromString currPath
  stateCurrInputWidget .= Just (InputWidget InputWidgetSaveAsPath "path> " contents)
  pure Continue

saveContentsToPath :: FilePath -> BufferContents -> App ()
saveContentsToPath path bufContents = do
  liftIO $ Buffer.saveContentsToPath path bufContents
  modifyCurrentBufferState $
    (bufferFilePath ?~ path)
      .> (bufferChanged .~ False)
  setStatusMessage $ "saved to " <> T.pack path

rectFullScreen :: (Int, Int) -> Rect
rectFullScreen (w, h) = Rect (0, 0) (w, h -1)

-- handleNormalModeCmd must be able to do IO (eg for opening files).
-- At least for now, lets see if we can get away with handling insert mode
-- commands purely.
handleInsertModeCmd :: MonadState AppState m => Command 'InsertModeCmd -> m ShouldQuit
handleInsertModeCmd =
  (Continue <$) . \case
    CmdEnterNormalMode -> stateMode .= NormalMode
    CmdInsertChar c -> modify $ AppState.insertChar c
    CmdBackspace -> modify backspace
    CmdInsertNewline -> modify insertNewline
    CmdDel -> modify del

data ShouldQuit = Quit | Continue
  deriving (Show)

-- Any occurrence of Quit means we should quit, and overrides all continues
instance Semigroup ShouldQuit where
  Continue <> Continue = Continue
  _ <> _ = Quit

-- if nothing happened, we should continue
instance Monoid ShouldQuit where
  mempty = Continue

setStatusMessage :: Text -> App ()
setStatusMessage t = stateStatusMessage .= Just t

-- return whether we should continue
handleEvent :: App ShouldQuit
handleEvent = do
  vty <- askVty

  ev <- liftIO $ nextEvent vty
  stateLastEvent ?= ev
  stateStatusMessage .= Nothing

  case ev of
    EvResize w h -> do
      -- TODO need to update window size here, I think.
      stateDimensions .= (w, h)
      pure Continue
    _ ->
      -- when we have more than two possible input focusses we'll need some
      -- sort of focus system
      use stateCurrInputWidget >>= \case
        Nothing -> handleEventWindow ev
        Just iw -> handleEventInputWidget iw ev

-- TODO refactor - this is currently specialized to saveas widget
handleEventInputWidget :: InputWidget -> Event -> App ShouldQuit
handleEventInputWidget iw@InputWidget {..} ev = case ev of
  EvKey (KChar c) [] -> do
    let contents' = _inputWidgetContents <> T.singleton c
    stateCurrInputWidget .= Just (iw {_inputWidgetContents = contents'})
    pure Continue
  EvKey KBS [] -> do
    let contents' =
          if T.null _inputWidgetContents
            then _inputWidgetContents
            else T.init _inputWidgetContents
    stateCurrInputWidget .= Just (iw {_inputWidgetContents = contents'})
    pure Continue
  EvKey KEnter [] -> do
    bufLines <- (^. bufferLines) . getCurrentBuffer <$> get
    if T.null _inputWidgetContents
      then do setStatusMessage "Can't save to empty path"
      else do
        let path :: FilePath
            path = T.unpack _inputWidgetContents
        saveContentsToPath path bufLines
        setStatusMessage $ "Saved to " <> T.pack path
    stateCurrInputWidget .= Nothing
    pure Continue
  EvKey KEsc [] -> do
    stateCurrInputWidget .= Nothing
    pure Continue
  _ -> pure Continue

  

handleEventWindow :: Event -> App ShouldQuit
handleEventWindow ev =
  use stateMode >>= \case
    NormalMode -> do
      let cmds :: [Command 'NormalModeCmd]
          cmds = case ev of
            EvKey (KChar c) [] -> case c of
              'Q' -> [CmdQuit]
              'i' -> [CmdEnterInsertMode]
              'I' -> [CmdMoveCursor Cursor.moveStartOfLine, CmdEnterInsertMode]
              'a' -> [CmdMoveCursor $ Cursor.moveRelative (1, 0), CmdEnterInsertMode]
              'A' -> [CmdMoveCursor Cursor.moveEndOfLine, CmdEnterInsertMode]
              -- TODO: how to do this? currently have a CmdInsertNewline, but it's an insert mode command. 
              -- A bad decision in retrospect. Possibly even the distinction between normal and insert commands
              -- was bad
              -- Will try the following:
              --  - eliminate the type parameter on Command
              --  - merge handleNormalModeCmd with handleInsertModeCmd
              'o' -> []
              'O' -> []
              'h' -> [CmdMoveCursor $ Cursor.moveRelative (-1, 0)]
              'j' -> [CmdMoveCursor $ Cursor.moveRelative (0, 1)]
              'k' -> [CmdMoveCursor $ Cursor.moveRelative (0, -1)]
              'l' -> [CmdMoveCursor $ Cursor.moveRelative (1, 0)]
              'm' -> [CmdScroll 1]
              ',' -> [CmdScroll (-1)]
              's' -> [CmdSave]
              'S' -> [CmdSaveAs]
              'B' -> [CmdNewBuffer]
              -- ignore all other chars
              _ -> []
            -- ignore all other keys
            EvKey _ _ -> []
            -- ignore all other events
            _ -> []
      shouldQuits <- traverse handleNormalModeCmd cmds
      pure $ mconcat shouldQuits
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
