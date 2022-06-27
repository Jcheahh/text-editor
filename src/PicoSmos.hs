{-# LANGUAGE OverloadedStrings #-}

module PicoSmos where

import Brick
import Brick.Main as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Core as Brick
import Cursor.Brick
import Cursor.Forest
import Cursor.Text
import Cursor.Tree
import Cursor.Types
import qualified Data.ByteString as SB
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tree
import Data.Yaml as Yaml
import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty
import Path
import Path.IO
import System.Directory
import System.Environment
import System.Exit

picoSmosApp :: App TextCursor e Text
picoSmosApp =
  App
    { appDraw = draw,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap Vty.defAttr []
    }

draw :: TextCursor -> [Widget Text]
draw tc =
  [ centerLayer $
      border $
        padAll 1 $
          showCursor "cursor" (Location (textCursorIndex tc, 0)) $
            txtWrap (rebuildTextCursor tc)
  ]

handleEvent :: TextCursor -> BrickEvent Text e -> EventM Text (Next TextCursor)
handleEvent tc e =
  case e of
    VtyEvent ve ->
      case ve of
        EvKey key mods ->
          let mDo func = continue . fromMaybe tc $ func tc
           in case key of
                KChar c -> mDo $ textCursorInsert c
                KLeft -> mDo textCursorSelectPrev
                KRight -> mDo textCursorSelectNext
                KHome -> continue $ textCursorSelectStart tc
                KEnd -> continue $ textCursorSelectEnd tc
                KBS -> mDo $ dullMDelete . textCursorRemove
                KDel -> mDo $ dullMDelete . textCursorDelete
                KEsc -> halt tc
                KEnter -> halt tc
                _ -> continue tc
        _ -> continue tc
    _ -> continue tc
