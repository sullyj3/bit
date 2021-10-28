{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module View where

import AppState
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Flow ((|>))
import Graphics.Vty hiding (update)
import Lens.Micro.Platform ((^.))
import Relude

----------
-- View --
----------
type CursorLocation = (Int, Int)

viewMainWindow :: AppState -> Image
viewMainWindow s
  | showStartMsg = howToQuit
  | otherwise = vertCat $ toList theLines <> emptyLines
  where
    Window {..} = s ^. stateWindow
    howToQuit = string defAttr "press Q to exit"
    CursorLocation curCol curLine = _winCursorLocation
    Buffer{_bufferLines} = getCurrentBuffer s
    (winWidth, winHeight) = _winRect |> rectDimensions
    showStartMsg = _winShowStartMessage

    linesToDisplay = Seq.take winHeight $ Seq.drop _winTopLine _bufferLines

    theLines :: Seq Image
    theLines =
      Seq.mapWithIndex
        ( \i l ->
            viewLine
              (if _winTopLine + i == curLine then Just curCol else Nothing)
              winWidth
              i
              l
        )
        linesToDisplay
    emptyHeight = winHeight - Seq.length theLines
    emptyLineAttr = defAttr `withBackColor` brightBlack
    emptyLines =
      replicate
        emptyHeight
        (text emptyLineAttr $ L.fromStrict $ T.replicate winWidth " ")

viewLine :: Maybe Int -> Int -> Int -> Text -> Image
viewLine mCursorX windowWidth _lineNumber l = case mCursorX of
  Just cursorX ->
    let (left, right) = T.splitAt cursorX l
     in horizCat case T.uncons right of
          Just (cursorChar, right') ->
            [ text' currLineAttr left,
              char cursorAttr cursorChar,
              text' currLineAttr right',
              text' currLineAttr (T.replicate remainingWidth " ")
            ]
          -- assuming cursorX < length l, we only get nothing if the line is empty
          Nothing -> [char cursorAttr ' ']
  Nothing -> text' defAttr l
  where
    currLineColor :: Color
    currLineColor = blue

    currLineAttr = defAttr `withBackColor` currLineColor
    cursorAttr = defAttr `withBackColor` black `withForeColor` currLineColor

    remainingWidth = windowWidth - T.length l

viewAppState :: AppState -> Picture
viewAppState appState@AppState {..} = picForLayers [bottomLine, mainWindow]
  where
    (_, h) = _stateDimensions
    bottomLine = translate 0 (h -1) $ case _stateCurrInputWidget of
      Nothing -> statusBar appState True
      Just InputWidget {..} -> case _inputWidgetType of
        InputWidgetSaveAsPath -> text defAttr . fromStrict $ _inputWidgetPrompt <> _inputWidgetContents

    mainWindow = viewMainWindow appState

statusBar :: AppState -> Bool -> Image
statusBar appState _showDiagnostics =
  case appState ^. stateStatusMessage of
    Just msg -> text defAttr (fromStrict msg)
    Nothing -> horizCat
      [modeWidget, middlePadding, currFileWidget, rightPadding]
  where
    (w, _) = appState ^. stateDimensions

    accentColor = case appState ^. stateMode of
      NormalMode -> blue
      InsertMode -> green

    barBgAttr = defAttr `withBackColor` accentColor
    modeAttr = defAttr `withBackColor` white `withForeColor` accentColor

    showMode :: EditorMode -> String
    showMode = \case
      NormalMode -> "NORMAL"
      InsertMode -> "INSERT"

    --remainingSpace = w - imageWidth modeWidget - imageWidth diagnosticsWidget
    remainingSpace = w - imageWidth modeWidget - imageWidth currFileWidget

    modeWidget = string modeAttr $ " " <> showMode (appState ^. stateMode) <> " "
    middlePadding = string barBgAttr $ replicate (remainingSpace - 1) ' '

    --diagnosticsWidget
    --  | showDiagnostics = viewDiagnostics appState accentColor
    --  | otherwise = mempty

    currFileWidget :: Image
    currFileWidget = string defAttr $ path <> modifiedString
      where
        Buffer {..} = getCurrentBuffer appState 

        path = fromMaybe "new file" _bufferFilePath

        modifiedString :: String
        modifiedString | _bufferChanged = "*"
                       | otherwise = ""

    

    rightPadding = char barBgAttr ' '

viewDiagnostics :: AppState -> Color -> Image
viewDiagnostics appState accentColor =
  string (defAttr `withForeColor` white `withBackColor` accentColor) diagnostics
  where
    diagnostics = "winTopLine: " ++ topLine ++ " | " ++ cursorLoc
    Window {_winTopLine, _winCursorLocation} = appState ^. stateWindow
    topLine = show _winTopLine
    cursorLoc = show _winCursorLocation
