{-# LANGUAGE OverloadedStrings #-}

module Editor.App.Event (
    handleEvent,
    handleMainEvent,
    handleFileDialogEvent,
    saveCurrentFile,
) where

import Brick
import qualified Brick.Main as M (halt)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Editor.App.State
import Editor.App.Types
import Editor.Config (Config (..))
import Editor.File (FileDialogMode (..), FileDialogState (..), createFileDialog, enterDirectory, goUpDirectory, moveSelection, openFile, saveFile)
import Editor.Haskell (formatBuffer)
import Editor.Keymap (EditorMode (..), KeyAction (..), keyActionForEvent)
import qualified Graphics.Vty as V
import System.Directory (doesDirectoryExist)
import System.FilePath (takeFileName, (</>))

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent ev@(VtyEvent _) = do
    s <- get
    case fileDialog s of
        Just dialog -> handleFileDialogEvent ev dialog
        Nothing -> handleMainEvent ev
handleEvent _ = pure ()

handleMainEvent :: BrickEvent Name e -> EventM Name St ()
handleMainEvent (VtyEvent ev@(V.EvKey _ _)) = do
    s <- get
    let (mAct, newPref) = keyActionForEvent (cfgKeymap (config s)) (editorMode s) (prefixState s) ev
    put s{prefixState = newPref}
    case mAct of
        Just ActQuit -> M.halt
        Just ActSave -> case currentMode s of
            MarkdownMode -> saveCurrentFile (mdFilePath s) (mdBuffer s) "Markdown"
            HaskellMode -> saveCurrentFile (hsFilePath s) (hsBuffer s) "Haskell"
        Just ActSaveAs -> do
            let mode = case currentMode s of
                    MarkdownMode -> SaveAsMarkdown
                    HaskellMode -> SaveAsHaskell
            dialogResult <- liftIO $ createFileDialog mode "."
            case dialogResult of
                Left err -> modify $ \st0 -> st0{message = T.pack $ formatErrorMessage (prettyFileError "ファイルダイアログの初期化" err)}
                Right dialog -> modify $ \st -> st{fileDialog = Just dialog}
        Just ActOpen -> do
            let mode = case currentMode s of
                    MarkdownMode -> OpenMarkdown
                    HaskellMode -> OpenHaskell
            dialogResult <- liftIO $ createFileDialog mode "."
            case dialogResult of
                Left err -> modify $ \st0 -> st0{message = T.pack $ formatErrorMessage (prettyFileError "ファイルダイアログの初期化" err)}
                Right dialog -> modify $ \st -> st{fileDialog = Just dialog}
        Just ActFormatHs -> do
            formatted <- liftIO (formatBuffer (hsBuffer s))
            case formatted of
                Right t -> modify $ \st -> st{hsBuffer = t, message = T.pack $ formatSuccessMessage "fourmolu/ormolu: フォーマット成功"}
                Left e -> modify $ \st -> st{message = T.pack $ formatErrorMessage $ "フォーマット失敗: " ++ T.unpack e}
        Just ActTogglePreview -> modify $ \st -> st{preview = not (preview st)}
        Just ActToggleAppMode -> modify $ \st ->
            st
                { currentMode = case currentMode st of
                    MarkdownMode -> HaskellMode
                    HaskellMode -> MarkdownMode
                }
        Just ActMoveLeft -> modify moveCursorLeft
        Just ActMoveRight -> modify moveCursorRight
        Just ActBackspace -> modify deleteChar
        Just ActDeleteNext -> modify deleteNextChar
        Just ActInsertNewline -> modify (insertChar '\n')
        Just ActEnterInsertMode -> modify $ \st -> st{editorMode = Insert}
        Just ActEnterNormalMode -> modify $ \st -> st{editorMode = Normal}
        Just ActNoop -> pure ()
        Nothing ->
            case ev of
                V.EvKey (V.KChar c) [] | editorMode s == Insert -> modify (insertChar c)
                V.EvKey V.KEnter [] | editorMode s == Insert -> modify (insertChar '\n')
                V.EvKey V.KBS [] | editorMode s == Insert -> modify deleteChar
                V.EvKey V.KDel [] -> modify deleteNextChar
                V.EvKey V.KLeft [] -> modify moveCursorLeft
                V.EvKey V.KRight [] -> modify moveCursorRight
                _ -> pure ()
handleMainEvent _ = pure ()

saveCurrentFile :: Maybe FilePath -> Text -> String -> EventM Name St ()
saveCurrentFile Nothing _ fileType =
    modify $ \st -> st{message = T.pack $ formatErrorMessage $ fileType ++ "ファイルのパスが設定されていません"}
saveCurrentFile (Just path) content fileType = do
    st <- get
    (finalContent, formatMsg) <-
        if cfgFormatOnSave (config st) && fileType == "Haskell"
            then do
                formatted <- liftIO $ formatBuffer content
                case formatted of
                    Right f -> return (f, " (フォーマット済み)")
                    Left _ -> return (content, " (フォーマット失敗、未フォーマットで保存)")
            else return (content, "")
    result <- liftIO $ saveFile path finalContent
    case result of
        Left fe ->
            let ctx = fileType ++ "ファイルの保存"
             in modify $ \st0 -> st0{message = T.pack $ formatErrorMessage (prettyFileError ctx fe)}
        Right _ ->
            modify $ \st0 -> st0{message = T.pack $ formatSuccessMessage $ fileType ++ "ファイルを保存しました: " ++ takeFileName path ++ formatMsg}

handleFileDialogEvent :: BrickEvent Name e -> FileDialogState -> EventM Name St ()
handleFileDialogEvent (VtyEvent (V.EvKey V.KUp [])) dialog =
    modify $ \st -> st{fileDialog = Just $ moveSelection dialog (-1)}
handleFileDialogEvent (VtyEvent (V.EvKey V.KDown [])) dialog =
    modify $ \st -> st{fileDialog = Just $ moveSelection dialog 1}
handleFileDialogEvent (VtyEvent (V.EvKey V.KEnter [])) dialog = do
    let files = fdFiles dialog
        selectedIndex = fdSelectedIndex dialog
    if selectedIndex < length files
        then do
            let selectedFile = files !! selectedIndex
                fullPath = fdCurrentPath dialog </> selectedFile
            isDir <- liftIO $ doesDirectoryExist fullPath
            if isDir
                then do
                    ent <- liftIO $ enterDirectory dialog
                    case ent of
                        Left err -> modify $ \st -> st{message = T.pack $ "ディレクトリに入れません: " ++ show err}
                        Right newDialog -> modify $ \st -> st{fileDialog = Just newDialog}
                else do
                    result <- liftIO $ openFile fullPath
                    case result of
                        Left fe ->
                            modify $ \st0 ->
                                st0
                                    { fileDialog = Nothing
                                    , message = T.pack $ formatErrorMessage (prettyFileError "ファイルの読み込み" fe)
                                    }
                        Right content -> do
                            let (newBuffer, newPath) = case fdMode dialog of
                                    OpenMarkdown -> (content, Just fullPath)
                                    OpenHaskell -> (content, Just fullPath)
                                    _ -> (content, Nothing)
                            modify $ \st ->
                                st
                                    { fileDialog = Nothing
                                    , message = T.pack $ "ファイルを開きました: " ++ selectedFile
                                    , mdBuffer = if fdMode dialog == OpenMarkdown then newBuffer else mdBuffer st
                                    , hsBuffer = if fdMode dialog == OpenHaskell then newBuffer else hsBuffer st
                                    , mdFilePath = if fdMode dialog == OpenMarkdown then newPath else mdFilePath st
                                    , hsFilePath = if fdMode dialog == OpenHaskell then newPath else hsFilePath st
                                    }
        else do
            let selectedFile = if null files then "new_file" else files !! min selectedIndex (length files - 1)
            case fdMode dialog of
                SaveAsMarkdown -> do
                    currentSt <- get
                    let newPath = fdCurrentPath dialog </> selectedFile
                    saveResult <- liftIO $ saveFile newPath (mdBuffer currentSt)
                    case saveResult of
                        Left fe ->
                            modify $ \st0 ->
                                st0
                                    { fileDialog = Nothing
                                    , message = T.pack $ formatErrorMessage (prettyFileError "Markdownファイルの保存" fe)
                                    }
                        Right _ ->
                            modify $ \st0 ->
                                st0
                                    { fileDialog = Nothing
                                    , message = T.pack $ "ファイルを保存しました: " ++ selectedFile
                                    , mdFilePath = Just newPath
                                    }
                SaveAsHaskell -> do
                    currentSt <- get
                    let newPath = fdCurrentPath dialog </> selectedFile
                    saveResult <- liftIO $ saveFile newPath (hsBuffer currentSt)
                    case saveResult of
                        Left fe ->
                            modify $ \st0 ->
                                st0
                                    { fileDialog = Nothing
                                    , message = T.pack $ formatErrorMessage (prettyFileError "Haskellファイルの保存" fe)
                                    }
                        Right _ ->
                            modify $ \st0 ->
                                st0
                                    { fileDialog = Nothing
                                    , message = T.pack $ "ファイルを保存しました: " ++ selectedFile
                                    , hsFilePath = Just newPath
                                    }
                _ -> pure ()
handleFileDialogEvent (VtyEvent (V.EvKey V.KEsc [])) _ =
    modify $ \st -> st{fileDialog = Nothing}
handleFileDialogEvent (VtyEvent (V.EvKey V.KBS [])) dialog = do
    result <- liftIO $ goUpDirectory dialog
    case result of
        Left err -> modify $ \st0 -> st0{message = T.pack $ formatErrorMessage (prettyFileError "ディレクトリ移動" err)}
        Right newDialog -> modify $ \st -> st{fileDialog = Just newDialog}
handleFileDialogEvent _ _ = pure ()
