{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LearnCursor where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.List (listSelectedAttr)
import Control.Monad
import Cursor.Brick.TextField
import Cursor.Forest (CForest, CTree)
import Cursor.Simple.Tree (TreeAbove (TreeAbove))
import qualified Cursor.Simple.Tree as CST
import Cursor.Text
import Cursor.TextField
import qualified Cursor.Tree as CT
import Cursor.Types
import Data.Maybe
import Data.Monoid (Endo)
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import Data.Tree
import qualified Data.Validity as Valid
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
import System.Directory
import System.Environment
import System.Exit
import Test.Hspec
import Text.Show.Pretty

data ListCursor' a = ListCursor' {list :: [a], index :: Int}

makeListCursor' :: [a] -> ListCursor' a
makeListCursor' xs = ListCursor' {list = xs, index = 0}

rebuildListCursor' :: ListCursor' a -> [a]
rebuildListCursor' = list

listCursorPrev' :: ListCursor' a -> Maybe (ListCursor' a)
listCursorPrev' lc
  | index lc <= 0 = Nothing
  | otherwise = Just $ lc {index = index lc - 1}

listCursorNext' :: ListCursor' a -> Maybe (ListCursor' a)
listCursorNext' lc
  | index lc >= length (list lc) = Nothing
  | otherwise = Just $ lc {index = index lc + 1}

-----------------------------------------------------------------------------------

data ListCursor a = ListCursor {prev :: [a], next :: [a]} deriving (Show, Eq, Generic)

makeListCursor :: [a] -> ListCursor a
makeListCursor xs = ListCursor {prev = [], next = xs}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor lc = reverse (prev lc) ++ next lc

listCursorPrev :: ListCursor a -> Maybe (ListCursor a)
listCursorPrev lc = case prev lc of
  [] -> Nothing
  a : as -> Just $ lc {prev = as, next = a : next lc}

listCursorNext :: ListCursor a -> Maybe (ListCursor a)
listCursorNext lc = case next lc of
  [] -> Nothing
  a : as -> Just $ lc {prev = a : prev lc, next = as}

-- newtype TextCursor = TextCursor {unTextCursor :: ListCursor Char} deriving (Show, Eq, Generic)

-- instance Validity TextCursor where
--   validate (TextCursor lc) =
--     mconcat
--       [ genericValidate lc,
--         decorateList (rebuildListCursor lc) $ \c ->
--           declare "The character is not a newline character" $ c /= 'n'
--       ]

-- instance GenUnchecked TextCursor

-- instance GenValid TextCursor

-- makeTextCursor :: T.Text.Text -> Maybe TextCursor
-- makeTextCursor = constructValid . makeListCursor . T.unpack

-- describe :: Spec
-- describe = do
--   describe "textCursorInsert" $
--     it "produces valid text cursors" $
--       producesValidsOnValids2 textCursorInsert

-- textCursorInsert :: Char -> TextCursor -> Maybe TextCursor
-- textCursorInsert '\n' _ = Nothing
-- textCursorInsert c tc = Just (tc & textCursorListCursorL %~ listCursorInsert c)

nanoSmosApp :: App TextFieldCursor e Text.Text
nanoSmosApp =
  App
    { appDraw = draw,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap Vty.defAttr []
    }

draw :: TextFieldCursor -> [Widget Text.Text]
draw tc =
  [ centerLayer $
      border $
        padAll 1 $
          let (y, x) = textFieldCursorSelection tc
           in showCursor "cursor" (Location (x, y)) $
                txtWrap (rebuildTextFieldCursor tc)
  ]

handleEvent :: TextFieldCursor -> BrickEvent Text.Text e -> EventM Text.Text (Next TextFieldCursor)
handleEvent tc e = case e of
  VtyEvent ve -> case ve of
    EvKey key mods ->
      let mDo func = continue . fromMaybe tc $ func tc
       in case key of
            KChar c -> mDo $ textFieldCursorInsertChar c . Just
            KLeft -> mDo textFieldCursorSelectPrevChar
            KRight -> mDo textFieldCursorSelectNextChar
            KUp -> mDo textFieldCursorSelectPrevLine
            KDown -> mDo textFieldCursorSelectNextLine
            KHome -> continue $ textFieldCursorSelectStartOfLine tc
            KEnd -> continue $ textFieldCursorSelectEndOfLine tc
            KBS -> mDo $ dullMDelete . textFieldCursorRemove
            KDel -> mDo $ dullMDelete . textFieldCursorDelete
            KEnter -> mDo $ Just . textFieldCursorInsertNewline . Just
            KEsc -> halt tc
            _ -> continue tc
    _ -> continue tc
  _ -> continue tc

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain nanoSmosApp initialState
  print endState

buildInitialState :: IO TextFieldCursor
buildInitialState = do
  path <- resolveFile' "src/example.txt"
  maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
  let contents = fromMaybe "" maybeContents
  let tfc = makeTextFieldCursor contents
  pure tfc

-- treeCursorSelectNextOnSameLevel :: TreeCursor a -> Maybe (TreeCursor a)
-- treeCursorSelectNextOnSameLevel tc@TreeCursor {..} = do
--   ta <- treeAbove
--   case treeAboveRights ta of
--     [] -> Nothing
--     Node c f : xs ->
--       Just $
--         TreeCursor
--           { treeAbove =
--               Just $
--                 ta
--                   { treeAboveLefts =
--                       Node (treeCurrent tc) (treeBelow tc) :
--                       treeAboveLefts ta,
--                     treeAboveRights = xs
--                   },
--             treeCurrent = n,
--             treeBelow = f
--           }
