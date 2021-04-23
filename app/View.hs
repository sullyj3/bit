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
viewMainWindow Window {..}
  | showStartMsg = howToQuit
  | otherwise    = vertCat $ toList theLines <> emptyLines
  where
    howToQuit = string defAttr "press Q to exit"
    (cursorX, cursorY) = winCursorLocation
    (Buffer bufLines) = windowBuffer
    (winWidth, winHeight) = winRect |> rectDimensions
    showStartMsg = winShowStartMessage

    linesToDisplay = Seq.take winHeight $ Seq.drop winTopLine bufLines

    currLineColor :: Color
    currLineColor = blue

    currLineAttr = defAttr `withBackColor` currLineColor
    cursorAttr = defAttr `withBackColor` black `withForeColor` currLineColor

    viewLine :: Maybe Int -> Int -> Text -> Image
    viewLine (Just cursorX) lineNumber l = let
        (left, right) = T.splitAt cursorX l
        in horizCat case T.uncons right of
          Just (cursorChar, right') ->
            [ text' currLineAttr left
            , char cursorAttr cursorChar
            , text' currLineAttr right' ]
          -- assuming cursorX < length l, we only get nothing if the line is empty
          Nothing -> [ char cursorAttr ' ' ]
    viewLine Nothing _ l = text' defAttr l

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


viewAppState :: AppState -> Picture
viewAppState state = picForLayers [bar, mainWindow]
  where
    bar = statusBar state True
    mainWindow = viewMainWindow $ state ^. stateWindow


statusBar :: AppState -> Bool -> Image
statusBar state showDiagnostics = translate 0 (h-1) $ horizCat
  [ modeWidget, middlePadding, diagnosticsWidget, rightPadding ]
  where
    (w,h) = state ^. stateDimensions

    accentColor = case state ^. stateMode of
      NormalMode -> blue
      InsertMode -> green

    barBgAttr = defAttr `withBackColor` accentColor
    modeAttr = defAttr `withBackColor` white `withForeColor` accentColor

    showMode :: EditorMode -> String
    showMode = \case
      NormalMode -> "NORMAL"
      InsertMode -> "INSERT"

    remainingSpace = w - imageWidth modeWidget - imageWidth diagnosticsWidget

    modeWidget = string modeAttr $ " " <> showMode (state ^. stateMode) <> " "
    middlePadding = string barBgAttr $ replicate (remainingSpace - 1) ' '
    diagnosticsWidget | showDiagnostics = viewDiagnostics state accentColor
                      | otherwise       = mempty
    rightPadding = char barBgAttr ' '

    -- hardcoded for now - the length of the words NORMAL and INSERT plus 1
    -- space either side
    modeWidth = 8

viewDiagnostics :: AppState -> Color -> Image
viewDiagnostics state accentColor =
  string (defAttr `withForeColor` white `withBackColor` accentColor) eventStr
  where
    eventStr =
      maybe "no events yet"
            (\e -> "Last event: " ++ show e)
            (state ^. stateLastEvent)
