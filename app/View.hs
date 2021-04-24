{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module View where
import AppState

import Relude
import Flow ( (|>) )

import           Graphics.Vty hiding (update)
import qualified Data.Sequence as Seq

import Lens.Micro.Platform ( (^.) )

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

----------
-- View --
----------
type CursorLocation = (Int, Int)

viewMainWindow :: Window -> Image
viewMainWindow Window{..}
  | showStartMsg = howToQuit
  | otherwise    = vertCat $ toList theLines <> emptyLines
  where
    howToQuit = string defAttr "press Q to exit"
    (cursorX, cursorY) = _winCursorLocation
    -- TODO display filepath somewhere
    (Buffer _ bufLines) = _windowBuffer
    (winWidth, winHeight) = _winRect |> rectDimensions
    showStartMsg = _winShowStartMessage

    linesToDisplay = Seq.take winHeight $ Seq.drop _winTopLine bufLines

    theLines :: Seq Image
    theLines = Seq.mapWithIndex
      (\i l -> viewLine (if i==cursorY then Just cursorX else Nothing)
                        i
                        l)
      linesToDisplay
    emptyHeight = winHeight - Seq.length theLines
    emptyLineAttr = defAttr `withBackColor` brightBlack
    emptyLines = replicate emptyHeight
      (text emptyLineAttr $ L.fromStrict $ T.replicate winWidth " ")

viewLine :: Maybe Int -> Int -> Text -> Image
viewLine mCursorX _lineNumber l = case mCursorX of
  Just cursorX -> let
    (left, right) = T.splitAt cursorX l
    in horizCat case T.uncons right of
      Just (cursorChar, right') ->
        [ text' currLineAttr left
        , char cursorAttr cursorChar
        , text' currLineAttr right' ]
      -- assuming cursorX < length l, we only get nothing if the line is empty
      Nothing -> [ char cursorAttr ' ' ]
  Nothing -> text' defAttr l
  where
    currLineColor :: Color
    currLineColor = blue

    currLineAttr = defAttr `withBackColor` currLineColor
    cursorAttr = defAttr `withBackColor` black `withForeColor` currLineColor




viewAppState :: AppState -> Picture
viewAppState appState = picForLayers [bar, mainWindow]
  where
    bar = statusBar appState True
    mainWindow = viewMainWindow $ appState ^. stateWindow


statusBar :: AppState -> Bool -> Image
statusBar appState showDiagnostics = translate 0 (h-1) $ horizCat
  [ modeWidget, middlePadding, diagnosticsWidget, rightPadding ]
  where
    (w,h) = appState ^. stateDimensions

    accentColor = case appState ^. stateMode of
      NormalMode -> blue
      InsertMode -> green

    barBgAttr = defAttr `withBackColor` accentColor
    modeAttr = defAttr `withBackColor` white `withForeColor` accentColor

    showMode :: EditorMode -> String
    showMode = \case
      NormalMode -> "NORMAL"
      InsertMode -> "INSERT"

    remainingSpace = w - imageWidth modeWidget - imageWidth diagnosticsWidget

    modeWidget = string modeAttr $ " " <> showMode (appState ^. stateMode) <> " "
    middlePadding = string barBgAttr $ replicate (remainingSpace - 1) ' '
    diagnosticsWidget | showDiagnostics = viewDiagnostics appState accentColor
                      | otherwise       = mempty
    rightPadding = char barBgAttr ' '

viewDiagnostics :: AppState -> Color -> Image
viewDiagnostics appState accentColor =
  string (defAttr `withForeColor` white `withBackColor` accentColor) eventStr
  where
    eventStr =
      maybe "no events yet"
            (\e -> "Last event: " ++ show e)
            (appState ^. stateLastEvent)
