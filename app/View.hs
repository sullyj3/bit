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

viewMainWindow :: Window -> Image
viewMainWindow Window {..}
  | showStartMsg = howToQuit
  | otherwise = vertCat $ toList theLines <> emptyLines
  where
    howToQuit = string defAttr "press Q to exit"
    CursorLocation curCol curLine = _winCursorLocation
    (Buffer _ bufLines _) = _windowBuffer
    (winWidth, winHeight) = _winRect |> rectDimensions
    showStartMsg = _winShowStartMessage

    linesToDisplay = Seq.take winHeight $ Seq.drop _winTopLine bufLines

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
viewAppState appState = picForLayers [bar, mainWindow]
  where
    bar = statusBar appState True
    mainWindow = viewMainWindow $ appState ^. stateWindow

statusBar :: AppState -> Bool -> Image
statusBar appState showDiagnostics =
  translate 0 (h -1) $
    horizCat
      [modeWidget, middlePadding, currFileWidget, rightPadding]
  where
    (w, h) = appState ^. stateDimensions

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
    currFileWidget = case appState ^. (stateWindow . windowBuffer) |> bufferFilePath of
      Just bufName -> string defAttr bufName
      Nothing -> string defAttr "new file"

    rightPadding = char barBgAttr ' '

viewDiagnostics :: AppState -> Color -> Image
viewDiagnostics appState accentColor =
  string (defAttr `withForeColor` white `withBackColor` accentColor) diagnostics
  where
    diagnostics = "winTopLine: " ++ topLine ++ " | " ++ cursorLoc
    Window {_winTopLine, _winCursorLocation} = appState ^. stateWindow
    topLine = show _winTopLine
    cursorLoc = show _winCursorLocation
