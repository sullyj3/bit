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
-----------
-- State --
-----------

newtype Buffer = Buffer { bufferLines :: Seq Text }

bufferLineCount (Buffer bLines) = Seq.length bLines

data Rect = Rect { rectTopLeft :: (Int, Int), rectDimensions :: (Int, Int) }

data Window =
    EmptyWindow Rect
  | BufferWindow { windowBuffer :: Buffer
                 , winTopLine :: Int
                 , winCursorLocation :: (Int, Int)
                 , winRect :: Rect }

windowFromBuf :: Rect -> Buffer -> Window
windowFromBuf r b = BufferWindow b 0 (0,0) r

moveCursor :: (Int, Int) -> Window -> Window
moveCursor (dx,dy) BufferWindow{ .. } = let
  Rect _ (_, winHeight) = winRect
  (x, y) = winCursorLocation
  currentLine :: Text
  currentLine = bufferLines windowBuffer `Seq.index` (winTopLine + y + dy)
  newCursorLocation = (clamp 0 (x+dx) (T.length currentLine-1), clamp 0 (y+dy) winHeight)
  in BufferWindow windowBuffer winTopLine newCursorLocation winRect
moveCursor _ w@(EmptyWindow _) = w

scrollWindow :: Int -> Window -> Window
scrollWindow _ (EmptyWindow r) = EmptyWindow r
scrollWindow n (BufferWindow buf winTopLine cursor r) =
  let newTopLine = clamp 0 (winTopLine + n) (bufferLineCount buf)
   in BufferWindow buf newTopLine cursor r

clamp a x b = max a (min x b)

data EditorMode = NormalMode | InsertMode

data AppState = AppState
  { _stateDimensions :: (Int, Int)
  , _stateLastEvent :: Maybe Event
  , _stateWindow :: Window
  , _stateMode :: EditorMode
  }

makeLenses ''AppState


mkInitialState :: Vty -> AppArgs -> IO AppState
mkInitialState vty (AppArgs argsFileToOpen) = do
  bounds <- liftIO $ displayBounds $ outputIface vty
  let (w,h) = bounds
  let winRect = Rect (0,0) (w,h-1)
  initialWindow <- maybe (pure $ EmptyWindow winRect)
                         (\fp -> openFile fp <&> windowFromBuf winRect)
                         argsFileToOpen
  pure $ AppState bounds Nothing initialWindow NormalMode

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

newtype AppArgs = AppArgs { argsFileToOpen :: Maybe FilePath }

parseArgs :: [String] -> Maybe AppArgs
parseArgs args = case args of
  [] -> Just (AppArgs Nothing)
  [fp] -> Just . AppArgs . Just $ fp
  _ -> Nothing


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

----------
-- View --
----------
type CursorLocation = (Int, Int)



viewAppState :: AppState -> Picture
viewAppState state = let
-- todo store mode in state
  (w,h) = state ^. stateDimensions
  bar = statusBar state True
  howToQuit = string defAttr "press Q to exit"

  mainWindow = case state ^. stateWindow of
    EmptyWindow _ -> howToQuit
    BufferWindow (Buffer bufLines) topLineNumber (cursorX, cursorY) r -> let
      linesToDisplay = Seq.take (h-1) $ Seq.drop topLineNumber bufLines

      cursorAttr :: Attr
      cursorAttr = defAttr `withBackColor` white `withForeColor` black

      showLine :: Maybe Int -> Int -> Text -> Image
      showLine (Just cursorX) lineNumber l = let
          (left, right) = T.splitAt cursorX l
          in horizCat case T.uncons right of
            Just (cursorChar, right') ->
              [ text' defAttr left
              , char cursorAttr cursorChar
              , text' defAttr right' ]
            -- assuming cursorX < length l, we only get nothing if the line is empty
            Nothing -> [ char cursorAttr ' ' ]
      showLine Nothing _ l = text' defAttr l

      in vertCat . toList $
        Seq.mapWithIndex (\i l -> showLine (if i==cursorY then Just cursorX else Nothing)
                                           i
                                           l)
                         linesToDisplay

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
    dims <- getState <&> (^. stateDimensions)
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
      getMode >>= \case
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
