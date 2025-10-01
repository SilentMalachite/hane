{-# LANGUAGE OverloadedStrings #-}

module Editor.App.State (
    getCurrentBuffer,
    setCurrentBuffer,
    insertChar,
    deleteChar,
    deleteNextChar,
    moveCursorLeft,
    moveCursorRight,
    formatErrorMessage,
    formatSuccessMessage,
    formatInfoMessage,
    prettyFileError,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Editor.App.Types
import Editor.Buffer (graphemes, moveLeft, moveRight)
import Editor.File (FileError (..))
import System.FilePath (takeFileName)

getCurrentBuffer :: St -> (Text, Int)
getCurrentBuffer st = case currentMode st of
    MarkdownMode -> (mdBuffer st, mdCursor st)
    HaskellMode -> (hsBuffer st, hsCursor st)

setCurrentBuffer :: St -> Text -> Int -> St
setCurrentBuffer st content cursor = case currentMode st of
    MarkdownMode -> st{mdBuffer = content, mdCursor = cursor}
    HaskellMode -> st{hsBuffer = content, hsCursor = cursor}

insertChar :: Char -> St -> St
insertChar c st =
    let (buffer, cursor) = getCurrentBuffer st
        graphemes_list = graphemes buffer
        (before, after) = splitAt cursor graphemes_list
        newGraphemes = before ++ [T.singleton c] ++ after
        newBuffer = T.concat newGraphemes
        newCursor = cursor + 1
     in setCurrentBuffer st newBuffer newCursor

deleteChar :: St -> St
deleteChar st =
    let (buffer, cursor) = getCurrentBuffer st
        graphemes_list = graphemes buffer
     in if cursor > 0 && not (null graphemes_list)
            then
                let (before, after) = splitAt (cursor - 1) graphemes_list
                    newGraphemes = before ++ drop 1 after
                    newBuffer = T.concat newGraphemes
                    newCursor = cursor - 1
                 in setCurrentBuffer st newBuffer newCursor
            else st

deleteNextChar :: St -> St
deleteNextChar st =
    let (buffer, cursor) = getCurrentBuffer st
        graphemes_list = graphemes buffer
     in if cursor < length graphemes_list
            then
                let (before, after) = splitAt cursor graphemes_list
                    newGraphemes = before ++ drop 1 after
                    newBuffer = T.concat newGraphemes
                 in setCurrentBuffer st newBuffer cursor
            else st

moveCursorLeft :: St -> St
moveCursorLeft st =
    let (buffer, cursor) = getCurrentBuffer st
        newCursor = moveLeft cursor buffer
     in setCurrentBuffer st buffer newCursor

moveCursorRight :: St -> St
moveCursorRight st =
    let (buffer, cursor) = getCurrentBuffer st
        newCursor = moveRight cursor buffer
     in setCurrentBuffer st buffer newCursor

formatErrorMessage :: String -> String
formatErrorMessage msg = "⚠️  " ++ msg

formatSuccessMessage :: String -> String
formatSuccessMessage msg = "✅ " ++ msg

formatInfoMessage :: String -> String
formatInfoMessage msg = "ℹ️  " ++ msg

prettyFileError :: String -> FileError -> String
prettyFileError ctx fe = case fe of
    FileNotFound p -> ctx ++ ": ファイルが見つかりません — " ++ short p
    FileReadError p e -> ctx ++ ": 読み込みに失敗 — " ++ short p ++ reason e
    FileWriteError p e -> ctx ++ ": 保存に失敗 — " ++ short p ++ reason e
    DirectoryNotFound p -> ctx ++ ": ディレクトリが見つかりません — " ++ p
    InvalidFilePath p -> ctx ++ ": ファイルパスが不正です — " ++ p ++ "（ファイル名を確認してください）"
    PermissionDenied p -> ctx ++ ": 権限がありません — " ++ p ++ "（アクセス権を確認してください）"
  where
    short p = let b = takeFileName p in if null b then p else b
    reason e = if null e then "" else "（詳細: " ++ e ++ ")"
