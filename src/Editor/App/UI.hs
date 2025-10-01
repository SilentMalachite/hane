{-# LANGUAGE OverloadedStrings #-}

module Editor.App.UI (
    drawUI,
    drawMainUI,
    drawBufferWithCursor,
    drawFileDialog,
    getCursorInfo,
    demoLine,
) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Data.Text (Text)
import qualified Data.Text as T
import Editor.App.State (getCurrentBuffer)
import Editor.App.Types
import Editor.Buffer (graphemes)
import Editor.Config (Config (..))
import Editor.File (FileDialogState (..))
import Editor.Markdown (renderMarkdownPlain)
import Editor.Render (displayWidth)
import Editor.Types (Keymap (..), icuEnabled)

drawUI :: St -> [Widget Name]
drawUI st = case fileDialog st of
    Just dialog -> [drawFileDialog st dialog]
    Nothing -> [drawMainUI st]

drawMainUI :: St -> Widget Name
drawMainUI st =
    vBox
        [ border $
            padAll 1 $
                vBox
                    [ str ("Hane — Haskell TUI Editor  [ICU: " <> (if icuEnabled then "on" else "off") <> "]")
                    , padTop (Pad 1) $ txt (message st)
                    , padTop (Pad 1) $ str ("Mode: " ++ showMode (currentMode st) ++ " | " ++ keyBindings)
                    , padTop (Pad 1) $ str (getCursorInfo st)
                    , padTop (Pad 1) demoLine
                    ]
        , hBox
            [ padAll 1 $
                border $
                    vBox
                        [ str (if preview st then "Markdown Preview" else "Markdown Raw")
                        , padTop (Pad 1) $
                            if preview st
                                then txt (renderMarkdownPlain (mdBuffer st))
                                else drawBufferWithCursor (mdBuffer st) (mdCursor st) (currentMode st == MarkdownMode)
                        ]
            , padAll 1 $
                border $
                    vBox
                        [ str "Haskell Buffer"
                        , padTop (Pad 1) $ drawBufferWithCursor (hsBuffer st) (hsCursor st) (currentMode st == HaskellMode)
                        ]
            ]
        ]
  where
    showMode MarkdownMode = "Markdown"
    showMode HaskellMode = "Haskell"
    keyBindings = case cfgKeymap (config st) of
        Vim -> "[Vim " ++ show (editorMode st) ++ "] q: quit | i: insert | h/l: move | x: del | F4: format | F5: preview | Esc: app-mode toggle (Normal時)"
        Emacs -> "[Emacs] C-x C-c: quit | C-s/C-x C-s: save | C-o/C-x C-f: open | C-x C-w: save-as | F4: format | F5: preview | Esc: app-mode toggle"

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
drawFileDialog _ dialog =
    center $
        border $
            padAll 1 $
                vBox
                    [ str $ "File Dialog - " ++ show (fdMode dialog)
                    , padTop (Pad 1) $ str $ "Path: " ++ fdCurrentPath dialog
                    , padTop (Pad 1) $ border $ vBox $ map drawFileItem indexedFiles
                    , padTop (Pad 1) $ str "↑↓: navigate | Enter: select | Esc: cancel | Backspace: up"
                    ]
  where
    indexedFiles :: [(Int, FilePath)]
    indexedFiles = zip [0 :: Int ..] (fdFiles dialog)

    drawFileItem :: (Int, FilePath) -> Widget Name
    drawFileItem (idx, file) =
        let isSelected = idx == fdSelectedIndex dialog
            prefix = if isSelected then "> " else "  "
         in str (prefix ++ file)

getCursorInfo :: St -> String
getCursorInfo st =
    let (buffer, cursor) = getCurrentBuffer st
        totalGraphemes = length (graphemes buffer)
        textLines = T.lines buffer
        currentLine = length (takeWhile (< cursor) (scanl (+) 0 (map (length . graphemes) textLines)))
        col = cursor - sum (map (length . graphemes) (take (currentLine - 1) textLines))
     in "Cursor: " ++ show cursor ++ "/" ++ show totalGraphemes ++ " (Line " ++ show currentLine ++ ", Col " ++ show col ++ ")"

demoLine :: Widget Name
demoLine =
    let sample = T.pack "あＡ🙂e\x0301"
        w = displayWidth sample
        g = length (graphemes sample)
     in str ("[demo] sample width=" <> show w <> ", graphemes=" <> show g)
