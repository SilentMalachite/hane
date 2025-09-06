module Editor.Keymap
  ( KeyAction(..)
  , EditorMode(..)
  , PrefixState(..)
  , keyActionForEvent
  ) where

import qualified Graphics.Vty as V
import Editor.Types (Keymap(..))

-- 編集モード（Vim 風）
data EditorMode = Insert | Normal
  deriving (Eq, Show)

-- プレフィックス状態（Emacs の C-x）
data PrefixState = EmacsC_X
  deriving (Eq, Show)

-- アクション（App 側で解釈）
data KeyAction
  = ActQuit
  | ActSave
  | ActSaveAs
  | ActOpen
  | ActFormatHs
  | ActTogglePreview
  | ActToggleAppMode
  | ActMoveLeft
  | ActMoveRight
  | ActBackspace
  | ActDeleteNext
  | ActInsertNewline
  | ActEnterInsertMode
  | ActEnterNormalMode
  | ActNoop
  deriving (Eq, Show)

-- 宣言的キーマップ：与えられた Keymap/EditorMode/Prefix からイベント→アクションを決定
keyActionForEvent :: Keymap -> EditorMode -> Maybe PrefixState -> V.Event -> (Maybe KeyAction, Maybe PrefixState)
keyActionForEvent keymap em prefix ev = case keymap of
  Emacs  -> emacsMap em prefix ev
  Vim    -> vimMap   em prefix ev

-- Emacs 風: C-x プレフィックス、単発 C-s/C-o、F4/F5、Esc はアプリモード切替
emacsMap :: EditorMode -> Maybe PrefixState -> V.Event -> (Maybe KeyAction, Maybe PrefixState)
emacsMap _ (Just EmacsC_X) ev =
  case ev of
    V.EvKey (V.KChar 's') [V.MCtrl] -> (Just ActSave, Nothing)
    V.EvKey (V.KChar 'f') [V.MCtrl] -> (Just ActOpen, Nothing)
    V.EvKey (V.KChar 'w') [V.MCtrl] -> (Just ActSaveAs, Nothing)
    V.EvKey (V.KChar 'c') [V.MCtrl] -> (Just ActQuit, Nothing)
    _ -> (Just ActNoop, Nothing)
emacsMap _ Nothing ev =
  case ev of
    V.EvKey (V.KChar 'x') [V.MCtrl] -> (Nothing, Just EmacsC_X)
    V.EvKey (V.KChar 's') [V.MCtrl] -> (Just ActSave, Nothing)
    V.EvKey (V.KChar 'o') [V.MCtrl] -> (Just ActOpen, Nothing)
    V.EvKey (V.KChar 's') [V.MCtrl, V.MShift] -> (Just ActSaveAs, Nothing)
    V.EvKey (V.KFun 4) [] -> (Just ActFormatHs, Nothing)
    V.EvKey (V.KFun 5) [] -> (Just ActTogglePreview, Nothing)
    V.EvKey V.KEsc []     -> (Just ActToggleAppMode, Nothing)
    _ -> (Nothing, Nothing)

-- Vim 風: Normal/Insert。Normal: hjlxw、Insert: 文字入力は App 側のデフォルト、Esc は Normal へ
vimMap :: EditorMode -> Maybe PrefixState -> V.Event -> (Maybe KeyAction, Maybe PrefixState)
vimMap Insert _ ev =
  case ev of
    V.EvKey V.KEsc []     -> (Just ActEnterNormalMode, Nothing)
    V.EvKey (V.KFun 4) [] -> (Just ActFormatHs, Nothing)
    V.EvKey (V.KFun 5) [] -> (Just ActTogglePreview, Nothing)
    V.EvKey (V.KChar 's') [V.MCtrl] -> (Just ActSave, Nothing)
    V.EvKey (V.KChar 'o') [V.MCtrl] -> (Just ActOpen, Nothing)
    V.EvKey (V.KChar 's') [V.MCtrl, V.MShift] -> (Just ActSaveAs, Nothing)
    V.EvKey V.KLeft []    -> (Just ActMoveLeft, Nothing)
    V.EvKey V.KRight []   -> (Just ActMoveRight, Nothing)
    V.EvKey V.KBS []      -> (Just ActBackspace, Nothing)
    V.EvKey V.KEnter []   -> (Just ActInsertNewline, Nothing)
    V.EvKey V.KDel []     -> (Just ActDeleteNext, Nothing)
    _ -> (Nothing, Nothing)
vimMap Normal _ ev =
  case ev of
    V.EvKey (V.KChar 'q') [] -> (Just ActQuit, Nothing)
    V.EvKey (V.KChar 'i') [] -> (Just ActEnterInsertMode, Nothing)
    V.EvKey (V.KChar 'h') [] -> (Just ActMoveLeft, Nothing)
    V.EvKey (V.KChar 'l') [] -> (Just ActMoveRight, Nothing)
    V.EvKey (V.KChar 'x') [] -> (Just ActDeleteNext, Nothing)
    V.EvKey (V.KChar 'w') [] -> (Just ActSave, Nothing)
    V.EvKey (V.KFun 4) []    -> (Just ActFormatHs, Nothing)
    V.EvKey (V.KFun 5) []    -> (Just ActTogglePreview, Nothing)
    V.EvKey V.KEsc []        -> (Just ActToggleAppMode, Nothing)
    _ -> (Nothing, Nothing)
