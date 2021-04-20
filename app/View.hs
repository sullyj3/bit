{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module View where
import AppState

import Relude

import           Graphics.Vty hiding (update)
import qualified Data.Sequence as Seq

import Lens.Micro.Platform

import qualified Data.Text as T

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
