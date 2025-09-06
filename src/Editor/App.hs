{-# LANGUAGE OverloadedStrings #-}

module Editor.App (run) where

import Brick
import qualified Brick.Main as M (halt, defaultMain, neverShowCursor)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.IO.Class (liftIO)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import qualified Graphics.Vty as V
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)
import Editor.Markdown (renderMarkdownPlain)
import Editor.Haskell (formatBuffer)
import Editor.Render (displayWidth)
import Editor.Buffer (graphemes, moveLeft, moveRight)
import Editor.Types (icuEnabled)
import Editor.Config (Config(..), defaultConfig, loadConfig)
import Editor.File (FileDialogState(..), FileDialogMode(..), FileError(..), openFile, saveFile, createFileDialog, moveSelection, goUpDirectory, enterDirectory)
import System.Directory (doesDirectoryExist)
import Editor.Keymap (EditorMode(..), PrefixState(..), KeyAction(..), keyActionForEvent)

data Name = Name deriving (Ord, Eq, Show)

data AppMode = MarkdownMode | HaskellMode
  deriving (Eq, Show)

data St = St
  { message    :: Text
  , mdBuffer   :: Text
  , hsBuffer   :: Text
  , preview    :: Bool
  , currentMode :: AppMode
  , editorMode :: EditorMode
  , mdFilePath :: Maybe FilePath
  , hsFilePath :: Maybe FilePath
  , fileDialog :: Maybe FileDialogState
  , mdCursor   :: Int  -- Markdownバッファのカーソル位置
  , hsCursor   :: Int  -- Haskellバッファのカーソル位置
  , config     :: Config
  , prefixState  :: Maybe PrefixState
  }

drawUI :: St -> [Widget Name]
drawUI st = case fileDialog st of
  Just dialog -> [drawFileDialog st dialog]
  Nothing -> [drawMainUI st]

drawMainUI :: St -> Widget Name
drawMainUI st = vBox
  [ border $ padAll 1 $ vBox
      [ str ("Hane — Haskell TUI Editor  [ICU: " <> (if icuEnabled then "on" else "off") <> "]" )
      , padTop (Pad 1) $ txt (message st)
      , padTop (Pad 1) $ str ("Mode: " ++ showMode (currentMode st) ++ " | " ++ keyBindings)
      , padTop (Pad 1) $ str (getCursorInfo st)
      , padTop (Pad 1) $ demoLine
      ]
  , hBox
      [ padAll 1 $ border $ vBox
          [ str (if preview st then "Markdown Preview" else "Markdown Raw")
          , padTop (Pad 1) $ if preview st
                              then txt (renderMarkdownPlain (mdBuffer st))
                              else drawBufferWithCursor (mdBuffer st) (mdCursor st) (currentMode st == MarkdownMode)
          ]
      , padAll 1 $ border $ vBox
          [ str "Haskell Buffer"
          , padTop (Pad 1) $ drawBufferWithCursor (hsBuffer st) (hsCursor st) (currentMode st == HaskellMode)
          ]
      ]
  ]
  where
    showMode MarkdownMode = "Markdown"
    showMode HaskellMode = "Haskell"
    keyBindings = case cfgKeymap (config st) of
      Vim   -> "[Vim " ++ show (editorMode st) ++ "] q: quit | i: insert | h/l: move | x: del | F4: format | F5: preview | Esc: app-mode toggle (Normal時)"
      Emacs -> "[Emacs] C-x C-c: quit | C-s/C-x C-s: save | C-o/C-x C-f: open | C-x C-w: save-as | F4: format | F5: preview | Esc: app-mode toggle"

-- カーソル位置を表示するバッファ描画
drawBufferWithCursor :: Text -> Int -> Bool -> Widget Name
drawBufferWithCursor buffer cursor isActive = 
  let graphemes_list = graphemes buffer
      (before, after) = splitAt cursor graphemes_list
      beforeText = T.concat before
      afterText = T.concat after
      cursorChar = if isActive then "█" else "│"
  in if isActive
     then txt (beforeText <> T.pack cursorChar <> afterText)
     else txt buffer

drawFileDialog :: St -> FileDialogState -> Widget Name
drawFileDialog _ dialog = center $ border $ padAll 1 $ vBox
  [ str $ "File Dialog - " ++ show (fdMode dialog)
  , padTop (Pad 1) $ str $ "Path: " ++ fdCurrentPath dialog
  , padTop (Pad 1) $ border $ vBox $ map drawFileItem (zip [0..] (fdFiles dialog))
  , padTop (Pad 1) $ str "↑↓: navigate | Enter: select | Esc: cancel | Backspace: up"
  ]
  where
    drawFileItem (idx, file) = 
      let isSelected = idx == fdSelectedIndex dialog
          prefix = if isSelected then "> " else "  "
      in str (prefix ++ file)

handleEvent :: BrickEvent Name e -> EventM Name St ()

-- ファイルダイアログが開いている場合の処理
handleEvent ev@(VtyEvent _) = do
  s <- get
  case fileDialog s of
    Just dialog -> handleFileDialogEvent ev dialog
    Nothing -> handleMainEvent ev

-- メインUIのイベント処理
handleMainEvent :: BrickEvent Name e -> EventM Name St ()
handleMainEvent (VtyEvent ev@(V.EvKey _ _)) = do
  s <- get
  let (mAct, newPref) = keyActionForEvent (cfgKeymap (config s)) (editorMode s) (prefixState s) ev
  put s { prefixState = newPref }
  case mAct of
    Just ActQuit -> M.halt
    Just ActSave -> case currentMode s of
      MarkdownMode -> saveCurrentFile (mdFilePath s) (mdBuffer s) "Markdown"
      HaskellMode  -> saveCurrentFile (hsFilePath s) (hsBuffer s) "Haskell"
    Just ActSaveAs -> do
      let mode = case currentMode s of
            MarkdownMode -> SaveAsMarkdown
            HaskellMode  -> SaveAsHaskell
      dialogResult <- liftIO $ createFileDialog mode "."
      case dialogResult of
        Left err -> modify $ \st -> st { message = T.pack $ formatErrorMessage $ "ファイルダイアログの初期化に失敗: " ++ show err }
        Right dialog -> modify $ \st -> st { fileDialog = Just dialog }
    Just ActOpen -> do
      let mode = case currentMode s of
            MarkdownMode -> OpenMarkdown
            HaskellMode  -> OpenHaskell
      dialogResult <- liftIO $ createFileDialog mode "."
      case dialogResult of
        Left err -> modify $ \st -> st { message = T.pack $ formatErrorMessage $ "ファイルダイアログの初期化に失敗: " ++ show err }
        Right dialog -> modify $ \st -> st { fileDialog = Just dialog }
    Just ActFormatHs -> do
      formatted <- liftIO (formatBuffer (hsBuffer s))
      case formatted of
        Right t -> modify $ \st -> st { hsBuffer = t, message = T.pack $ formatSuccessMessage "fourmolu/ormolu: フォーマット成功" }
        Left e  -> modify $ \st -> st { message = T.pack $ formatErrorMessage $ "フォーマット失敗: " ++ T.unpack e }
    Just ActTogglePreview -> modify $ \st -> st { preview = not (preview st) }
    Just ActToggleAppMode -> modify $ \st -> st { currentMode = case currentMode st of
                                                  MarkdownMode -> HaskellMode
                                                  HaskellMode  -> MarkdownMode }
    Just ActMoveLeft     -> modify moveCursorLeft
    Just ActMoveRight    -> modify moveCursorRight
    Just ActBackspace    -> modify deleteChar
    Just ActDeleteNext   -> modify deleteNextChar
    Just ActInsertNewline-> modify (insertChar '\n')
    Just ActEnterInsertMode -> modify $ \st -> st { editorMode = Insert }
    Just ActEnterNormalMode -> modify $ \st -> st { editorMode = Normal }
    Just ActNoop -> pure ()
    Nothing ->
      case ev of
        V.EvKey (V.KChar c) [] | editorMode s == Insert -> modify (insertChar c)
        V.EvKey V.KEnter []   | editorMode s == Insert -> modify (insertChar '\n')
        V.EvKey V.KBS []      | editorMode s == Insert -> modify deleteChar
        V.EvKey V.KDel []                              -> modify deleteNextChar
        V.EvKey V.KLeft []                             -> modify moveCursorLeft
        V.EvKey V.KRight []                            -> modify moveCursorRight
        _ -> pure ()
handleMainEvent _ = pure ()

-- ヘルパー関数
saveCurrentFile :: Maybe FilePath -> Text -> String -> EventM Name St ()
saveCurrentFile Nothing _ fileType = 
  modify $ \st -> st { message = T.pack $ formatErrorMessage $ fileType ++ "ファイルのパスが設定されていません" }
saveCurrentFile (Just path) content fileType = do
  st <- get
  finalContent <- if cfgFormatOnSave (config st) && fileType == "Haskell"
                  then do
                    formatted <- liftIO $ formatBuffer content
                    case formatted of
                      Right f -> return f
                      Left _ -> return content
                  else return content
  result <- liftIO $ Editor.File.saveFile path finalContent
  case result of
    Left (FileWriteError _ err) -> 
      modify $ \st -> st { message = T.pack $ formatErrorMessage $ fileType ++ "ファイルの保存に失敗: " ++ err }
    Right _ -> do
      let formatMsg = if cfgFormatOnSave (config st) && fileType == "Haskell"
                     then " (フォーマット済み)"
                     else ""
      modify $ \st -> st { message = T.pack $ formatSuccessMessage $ fileType ++ "ファイルを保存しました: " ++ takeFileName path ++ formatMsg }

-- テキスト編集ヘルパー関数
getCurrentBuffer :: St -> (Text, Int)
getCurrentBuffer st = case currentMode st of
  MarkdownMode -> (mdBuffer st, mdCursor st)
  HaskellMode -> (hsBuffer st, hsCursor st)

setCurrentBuffer :: St -> Text -> Int -> St
setCurrentBuffer st content cursor = case currentMode st of
  MarkdownMode -> st { mdBuffer = content, mdCursor = cursor }
  HaskellMode -> st { hsBuffer = content, hsCursor = cursor }

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
     then let (before, after) = splitAt (cursor - 1) graphemes_list
              newGraphemes = before ++ drop 1 after
              newBuffer = T.concat newGraphemes
              newCursor = cursor - 1
          in setCurrentBuffer st newBuffer newCursor
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

-- 次の 1 グラフェムを削除
deleteNextChar :: St -> St
deleteNextChar st = 
  let (buffer, cursor) = getCurrentBuffer st
      graphemes_list = graphemes buffer
  in if cursor < length graphemes_list
     then let (before, after) = splitAt cursor graphemes_list
              newGraphemes = before ++ drop 1 after
              newBuffer = T.concat newGraphemes
          in setCurrentBuffer st newBuffer cursor
     else st

-- カーソル位置を表示するためのヘルパー
getCursorInfo :: St -> String
getCursorInfo st = 
  let (buffer, cursor) = getCurrentBuffer st
      totalGraphemes = length (graphemes buffer)
      textLines = T.lines buffer
      currentLine = length (takeWhile (< cursor) (scanl (+) 0 (map (length . graphemes) textLines)))
      col = cursor - sum (map (length . graphemes) (take (currentLine - 1) textLines))
  in "Cursor: " ++ show cursor ++ "/" ++ show totalGraphemes ++ " (Line " ++ show currentLine ++ ", Col " ++ show col ++ ")"

-- エラーメッセージの改善
formatErrorMessage :: String -> String
formatErrorMessage msg = "⚠️  " ++ msg

formatSuccessMessage :: String -> String
formatSuccessMessage msg = "✅ " ++ msg

formatInfoMessage :: String -> String
formatInfoMessage msg = "ℹ️  " ++ msg

-- ファイルダイアログのイベント処理
handleFileDialogEvent :: BrickEvent Name e -> FileDialogState -> EventM Name St ()
handleFileDialogEvent (VtyEvent (V.EvKey V.KUp [])) dialog = 
  modify $ \st -> st { fileDialog = Just $ moveSelection dialog (-1) }

handleFileDialogEvent (VtyEvent (V.EvKey V.KDown [])) dialog = 
  modify $ \st -> st { fileDialog = Just $ moveSelection dialog 1 }

handleFileDialogEvent (VtyEvent (V.EvKey V.KEnter [])) dialog = do
  let files = fdFiles dialog
      selectedIndex = fdSelectedIndex dialog
  if selectedIndex < length files
    then do
      let selectedFile = files !! selectedIndex
          fullPath = fdCurrentPath dialog ++ "/" ++ selectedFile
      isDir <- liftIO $ doesDirectoryExist fullPath
      if isDir
        then do
          ent <- liftIO $ enterDirectory dialog
          case ent of
            Left err -> modify $ \st -> st { message = T.pack $ "ディレクトリに入れません: " ++ show err }
            Right newDialog -> modify $ \st -> st { fileDialog = Just newDialog }
        else do
          result <- liftIO $ Editor.File.openFile fullPath
          case result of
            Left (FileReadError _ err) -> 
              modify $ \st -> st { 
                fileDialog = Nothing
              , message = T.pack $ "ファイルの読み込みに失敗: " ++ err 
              }
            Right content -> do
              let (newBuffer, newPath) = case fdMode dialog of
                    OpenMarkdown -> (content, Just fullPath)
                    OpenHaskell -> (content, Just fullPath)
                    _ -> (content, Nothing)
              modify $ \st -> st { 
                fileDialog = Nothing
              , message = T.pack $ "ファイルを開きました: " ++ selectedFile
              , mdBuffer = if fdMode dialog == OpenMarkdown then newBuffer else mdBuffer st
              , hsBuffer = if fdMode dialog == OpenHaskell then newBuffer else hsBuffer st
              , mdFilePath = if fdMode dialog == OpenMarkdown then newPath else mdFilePath st
              , hsFilePath = if fdMode dialog == OpenHaskell then newPath else hsFilePath st
              }
    else do
      -- Save As モードの場合、新しいファイル名を入力
      let selectedFile = if null files then "new_file" else files !! min selectedIndex (length files - 1)
      case fdMode dialog of
        SaveAsMarkdown -> do
          currentSt <- get
          let newPath = fdCurrentPath dialog ++ "/" ++ selectedFile
          saveResult <- liftIO $ Editor.File.saveFile newPath (mdBuffer currentSt)
          case saveResult of
            Left (FileWriteError _ err) -> 
              modify $ \st -> st { 
                fileDialog = Nothing
              , message = T.pack $ "ファイルの保存に失敗: " ++ err 
              }
            Right _ -> 
              modify $ \st -> st { 
                fileDialog = Nothing
              , message = T.pack $ "ファイルを保存しました: " ++ selectedFile
              , mdFilePath = Just newPath
              }
        SaveAsHaskell -> do
          currentSt <- get
          let newPath = fdCurrentPath dialog ++ "/" ++ selectedFile
          saveResult <- liftIO $ Editor.File.saveFile newPath (hsBuffer currentSt)
          case saveResult of
            Left (FileWriteError _ err) -> 
              modify $ \st -> st { 
                fileDialog = Nothing
              , message = T.pack $ "ファイルの保存に失敗: " ++ err 
              }
            Right _ -> 
              modify $ \st -> st { 
                fileDialog = Nothing
              , message = T.pack $ "ファイルを保存しました: " ++ selectedFile
              , hsFilePath = Just newPath
              }
        _ -> pure ()
      
handleFileDialogEvent (VtyEvent (V.EvKey V.KEsc [])) _ = 
  modify $ \st -> st { fileDialog = Nothing }

handleFileDialogEvent (VtyEvent (V.EvKey V.KBS [])) dialog = do
  result <- liftIO $ goUpDirectory dialog
  case result of
    Left err -> modify $ \st -> st { message = T.pack $ "ディレクトリ移動に失敗: " ++ show err }
    Right newDialog -> modify $ \st -> st { fileDialog = Just newDialog }

handleFileDialogEvent _ _ = pure ()

theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App St e Name
app = App
  { appDraw = drawUI
  , appChooseCursor = M.neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const theMap
  }

run :: IO ()
run = do
  configResult <- loadConfig
  let (initialMsg, loadedConfig) = case configResult of
        Left err -> (T.pack $ formatErrorMessage $ "設定読み込みエラー: " ++ show err ++ ", デフォルト設定を使用します", defaultConfig)
        Right cfg -> (T.pack $ formatInfoMessage "設定を読み込みました", cfg)
      
      md = T.unlines
        [ "# 見出し H1"
        , "本文に" <> "**強調**" <> "や" <> "*斜体*" <> "、`code` を含む段落です。"
        , ""
        , "- 箇条書き1"
        , "- 箇条書き2"
        , ""
        , "```haskell"
        , "main = putStrLn \"Hello\""
        , "```"
        ]
      hs = T.unlines
        [ "main =    putStrLn  \"Hello\"" ]
      initial = St
        { message = initialMsg
        , mdBuffer = md
        , hsBuffer = hs
        , preview  = False
        , currentMode = MarkdownMode
        , editorMode = Insert
        , mdFilePath = Nothing
        , hsFilePath = Nothing
        , fileDialog = Nothing
        , mdCursor = 0
        , hsCursor = 0
        , config = loadedConfig
        , prefixState = Nothing
        }
  _ <- M.defaultMain app initial
  pure ()

-- 幅・グラフェムの簡易デモ表示
demoLine :: Widget Name
demoLine =
  let sample = T.pack "あＡ🙂e\x0301"
      w = displayWidth sample
      g = length (graphemes sample)
  in str ("[demo] sample width=" <> show w <> ", graphemes=" <> show g)
