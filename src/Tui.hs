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
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

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
      appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState = do
  path <- resolveFile' "src/example.txt"
  maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
  let contents = fromMaybe "" maybeContents
  let tfc = makeTextFieldCursor contents
  pure TuiState {stateCursor = tfc}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [selectedTextFieldCursorWidget ResourceName (stateCursor ts)]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s