{-# LANGUAGE OverloadedStrings #-}

module Editor.App (run) where

import Brick
import qualified Brick.Main as M (defaultMain, neverShowCursor)
import qualified Data.Text as T
import Editor.App.Event (handleEvent)
import Editor.App.State (formatErrorMessage, formatInfoMessage)
import Editor.App.Types
import Editor.App.UI (drawUI)
import Editor.Config (defaultConfig, loadConfig)
import Editor.Keymap (EditorMode (..))
import qualified Graphics.Vty as V

theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App St e Name
app =
    App
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

        md =
            T.unlines
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
        hs =
            T.unlines
                ["main =    putStrLn  \"Hello\""]
        initial =
            St
                { message = initialMsg
                , mdBuffer = md
                , hsBuffer = hs
                , preview = False
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
