{-# LANGUAGE OverloadedStrings #-}

module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
import System.Directory
import System.Environment
import System.Exit
import Text.Show.Pretty

tui :: IO ()
tui = do
  arg <- getArgs
  case arg of
    [] -> die "No argument to choose file to edit"
    (fp : _) -> do
      path <- resolveFile' fp
      maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
      let contents = fromMaybe "" maybeContents
      initialState <- buildInitialState contents
      endState <- defaultMain tuiApp initialState
      let contents' = rebuildTextFieldCursor (stateCursor endState)
      unless (contents == contents') $ T.writeFile (fromAbsFile path) contents'

data TuiState = TuiState
  { stateCursor :: TextFieldCursor
  }
  deriving (Show, Eq)

data ResourceName = ResourceName deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap mempty [("text", fg red), ("bg", fg blue)]
    }

buildInitialState :: Text -> IO TuiState
buildInitialState contents = do
  let tfc = makeTextFieldCursor contents
  pure TuiState {stateCursor = tfc}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  [ forceAttr "text" $
      centerLayer $
        border $
          padLeftRight 1 $
            selectedTextFieldCursorWidget ResourceName (stateCursor ts),
    forceAttr "bg" $ fill '@'
  ]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      let mDo :: (TextFieldCursor -> Maybe TextFieldCursor) -> EventM n (Next TuiState)
          mDo func = do
            let tfc = stateCursor s
            let tfc' = fromMaybe tfc $ func tfc
            let s' = s {stateCursor = tfc'}
            continue s'
       in case vtye of
            EvKey (KChar c) [] -> mDo $ textFieldCursorInsertChar c . Just
            EvKey KUp [] -> mDo textFieldCursorSelectPrevLine
            EvKey KDown [] -> mDo textFieldCursorSelectNextLine
            EvKey KLeft [] -> mDo textFieldCursorSelectPrevChar
            EvKey KRight [] -> mDo textFieldCursorSelectNextChar
            EvKey KBS [] -> mDo $ dullMDelete . textFieldCursorRemove
            EvKey KDel [] -> mDo $ dullMDelete . textFieldCursorDelete
            EvKey KEnter [] -> mDo $ Just . textFieldCursorInsertNewline . Just
            EvKey KEsc [] -> halt s
            _ -> continue s
    _ -> continue s
