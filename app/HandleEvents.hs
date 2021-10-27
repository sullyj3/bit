{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ScopedTypeVariables #-}
module HandleEvents where

import AppState
import Control.Monad.RWS.Strict (RWST)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Flow ((|>))
import Graphics.Vty hiding (update)
import Lens.Micro.Platform
import Relude
import Control.Exception (IOException, catch)

import qualified Data.Map.NonEmpty as NEMap
import           Data.Map.NonEmpty (NEMap)
import AppState (bufInsertChar)

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
    CmdMoveCursorRelative v -> Continue <$
      (stateWindow %= moveCursor v bufLines)
    CmdScroll n -> Continue <$ (stateWindow %= scrollWindow n bufLines)
    CmdEnterInsertMode ->
      Continue <$ do
        stateMode .= InsertMode
        stateWindow %= (\win -> win {_winShowStartMessage = False})
    CmdQuit -> pure Quit
    CmdOpenFile fp -> do
      buf <- openFile fp
      dims <- use stateDimensions
      stateOpenBuffers %= insertBuffer buf
      let window = windowFromBufID (rectFullScreen dims) (buf ^. bufferID) False
      stateWindow .= window
      pure Continue
    CmdSave -> do
      Buffer { _bufferFilePath, _bufferLines } <- getCurrentBuffer <$> get
      case _bufferFilePath of
        Nothing -> openSaveAsDialog
        Just path -> do
          saveLinesToPath path _bufferLines
          modifyCurrentBuffer (\buf -> buf {_bufferChanged = False})
          pure Continue
    CmdSaveAs -> openSaveAsDialog
    CmdNewBuffer -> do
      undefined

openSaveAsDialog :: App ShouldQuit
openSaveAsDialog = do
    Buffer { _bufferFilePath = currPath } <- getCurrentBuffer <$> get
    let contents :: Text
        contents = maybe mempty fromString currPath
    stateCurrInputWidget .= Just (InputWidget InputWidgetSaveAsPath "path> " contents)
    pure Continue

-- TODO: make more efficient using streamly or something
saveLinesToPath :: FilePath -> Seq Text -> App ()
saveLinesToPath path theLines = do
  liftIO $ writeFileText path (unlines . toList $ theLines)
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
    CmdInsertChar c -> stateWindow %= winInsertChar c
    CmdBackspace -> stateWindow %= winBackspace
    CmdInsertNewline -> stateWindow %= winInsertNewline
    CmdDel -> stateWindow %= winDel


winInsertChar :: Char -> AppState -> AppState
winInsertChar c s = 
  s |> modifyCurrentBuffer (const buf') 
    |> stateWindow .~ win'
  where -- first modify the buffer
        cursorLoc = s ^. stateWindow . winCursorLocation
        buf' = bufInsertChar c cursorLoc (getCurrentBuffer s)
        -- then move the cursor
        win' = moveCursor (1, 0) (buf' ^. bufferLines) (s ^. stateWindow)

-- delete the character before the cursor, and move the cursor back one.
winBackspace :: Window -> Window
winBackspace win@Window {_windowBuffer = Buffer fp bufLines _} =
  moveCursor (-1, 0) win'
  where
    CursorLocation curCol curLine = win ^. winCursorLocation
    -- if the cursor is at column 0, deleteChar (-1) will be a no-op
    -- TODO allow backspace to span lines
    bufLines' = Seq.adjust' (deleteChar $ curCol - 1) curLine bufLines
    win' = win |> windowBuffer .~ Buffer fp bufLines' True

-- delete the character at the cursor. If we were on the last character,
-- move the cursor back one
winDel :: Window -> Window
winDel win@Window {_windowBuffer = Buffer fp bufLines _}
  | curCol == lenCurrLine = moveCursor (-1, 0) win'
  | otherwise = win'
  where
    CursorLocation curCol curLine = win ^. winCursorLocation

    bufLines' = Seq.adjust' (deleteChar curCol) curLine bufLines
    lenCurrLine = T.length $ Seq.index bufLines' curLine
    win' = win {_windowBuffer = Buffer fp bufLines' True}

winInsertNewline :: Window -> Window
winInsertNewline win@Window {_windowBuffer = Buffer fp bufLines _} =
  moveCursor (0, 1) win'
  where
    CursorLocation curCol curLine = win ^. winCursorLocation
    line = bufLines `Seq.index` curLine
    (l, r) = T.splitAt curCol line
    (top, bottom) = Seq.splitAt curLine bufLines
    -- first element of bottom is the current line, we drop it and replace with
    -- the two halves of the split line
    bufLines' = top <> Seq.fromList [l, r] <> Seq.drop 1 bottom
    win' = win {_windowBuffer = Buffer fp bufLines' True}

-- does nothing if i ∉ [0, T.length txt)
deleteChar :: Int -> Text -> Text
deleteChar i txt
  | i < 0 = txt
  | i >= T.length txt = txt
  | otherwise = let (l, r) = T.splitAt i txt in l <> T.tail r


data ShouldQuit = Quit | Continue
  deriving (Show)

removeTemporaryMessage :: App ()
removeTemporaryMessage = stateStatusMessage .= Nothing

setStatusMessage :: Text -> App ()
setStatusMessage t = stateStatusMessage .= Just t

-- return whether we should continue
handleEvent :: App ShouldQuit
handleEvent = do
  vty <- askVty

  ev <- liftIO $ nextEvent vty
  stateLastEvent ?= ev
  removeTemporaryMessage

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
    let contents' = if T.null _inputWidgetContents
                      then _inputWidgetContents
                      else T.init _inputWidgetContents
    stateCurrInputWidget .= Just (iw {_inputWidgetContents = contents'})
    pure Continue

  EvKey KEnter [] -> do
    bufLines <- (^. bufferLines) . getCurrentBuffer <$> get
    if T.null _inputWidgetContents
      then do setStatusMessage "Can't save to empty path"
      else do let path :: FilePath
                  path = T.unpack _inputWidgetContents
              saveLinesToPath path bufLines
              modifyCurrentBufferState (\buf -> buf { _bufferFilePath = Just path })
              setStatusMessage $ "Saved to " <> T.pack path
    stateCurrInputWidget .= Nothing
    pure Continue
  EvKey KEsc [] -> do
    stateCurrInputWidget .= Nothing
    pure Continue
  _ -> pure Continue


handleEventWindow :: Event -> App ShouldQuit
handleEventWindow ev = use stateMode >>= \case
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
      's' -> Just   CmdSave
      'S' -> Just   CmdSaveAs
      'B' -> Just   CmdNewBuffer
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

-- creates a new empty buffer if there is no existing file at the path
openFile :: FilePath -> App Buffer
openFile path = do
    bid <- newBufferID
    liftIO $ tryOpenFile bid `catch` orCreateNewBuffer bid
  where
    tryOpenFile :: BufferID -> IO Buffer
    tryOpenFile bid = do
      theLines <- Seq.fromList . lines <$> readFileText path
      pure $ Buffer bid (Just path) theLines False

    -- If the file isn't present, create an empty buffer
    -- TODO figure out how to check that it's the right IO exception, from memory
    -- I think we may have to resort to string comparison
    orCreateNewBuffer :: BufferID -> IOException -> IO Buffer
    orCreateNewBuffer bid _ = pure $
      (newEmptyBuffer bid) {_bufferFilePath = Just path}


