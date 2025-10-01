{-# LANGUAGE OverloadedStrings #-}

module Editor.App.Types (
    Name (..),
    AppMode (..),
    St (..),
) where

import Data.Text (Text)
import Editor.Config (Config)
import Editor.File (FileDialogState)
import Editor.Keymap (EditorMode, PrefixState)

data Name = Name deriving (Ord, Eq, Show)

data AppMode = MarkdownMode | HaskellMode
    deriving (Eq, Show)

data St = St
    { message :: Text
    , mdBuffer :: Text
    , hsBuffer :: Text
    , preview :: Bool
    , currentMode :: AppMode
    , editorMode :: EditorMode
    , mdFilePath :: Maybe FilePath
    , hsFilePath :: Maybe FilePath
    , fileDialog :: Maybe FileDialogState
    , mdCursor :: Int
    , hsCursor :: Int
    , config :: Config
    , prefixState :: Maybe PrefixState
    }
