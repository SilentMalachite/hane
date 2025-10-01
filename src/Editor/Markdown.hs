{-# LANGUAGE OverloadedStrings #-}

module Editor.Markdown (
    renderMarkdownToAnsi,
    renderMarkdownPlain,
) where

import CMarkGFM (
    commonmarkToHtml,
    extStrikethrough,
    extTable,
    extTaskList,
    optSafe,
 )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import System.Console.ANSI (
    Color (..),
    ColorIntensity (..),
    ConsoleIntensity (..),
    ConsoleLayer (..),
    SGR (..),
    Underlining (..),
    setSGRCode,
 )
import qualified Text.HTML.TagSoup as TS

bold, underline, dim, cyan, yellow, reset :: Text
bold = T.pack (setSGRCode [SetConsoleIntensity BoldIntensity])
underline = T.pack (setSGRCode [SetUnderlining SingleUnderline])
dim = T.pack (setSGRCode [SetConsoleIntensity FaintIntensity])
cyan = T.pack (setSGRCode [SetColor Foreground Vivid Cyan])
yellow = T.pack (setSGRCode [SetColor Foreground Vivid Yellow])
reset = T.pack (setSGRCode [Reset])

-- | cmark-gfm → HTML → 簡易 ANSI 文字列
renderMarkdownToAnsi :: Text -> Text
renderMarkdownToAnsi src =
    let html = commonmarkToHtml [optSafe] [extStrikethrough, extTable, extTaskList] src
     in htmlToAnsi html

-- | ターミナル非依存の簡易テキスト（色なし）。UI プレビュー用。
renderMarkdownPlain :: Text -> Text
renderMarkdownPlain src =
    TS.renderTags $ stripTags $ TS.parseTags $ commonmarkToHtml [optSafe] [extStrikethrough, extTable, extTaskList] src

-- 内部: HTML → ANSI テキスト
htmlToAnsi :: Text -> Text
htmlToAnsi html = TL.toStrict . TB.toLazyText $ go mempty 0 (TS.parseTags html)
  where
    go :: TB.Builder -> Int -> [TS.Tag Text] -> TB.Builder
    go b _ [] = b
    go b depth (t : ts) = case t of
        TS.TagOpen name _
            | name `elem` ["h1", "h2", "h3"] ->
                let hdr = TB.fromText (bold <> underline <> cyan)
                 in go (b <> hdr) depth ts
            | name == "strong" -> go (b <> TB.fromText bold) depth ts
            | name == "em" -> go (b <> TB.fromText dim) depth ts
            | name == "code" -> go (b <> TB.fromText yellow) depth ts
            | name == "pre" -> go (b <> TB.fromText (yellow)) (depth + 1) ts
            | name == "ul" -> go b (depth + 1) ts
            | name == "ol" -> go b (depth + 1) ts
            | name == "li" ->
                let indent = TB.fromText (T.replicate (max 0 (depth - 1)) "  ")
                    bullet = TB.fromText "• "
                 in go (b <> indent <> bullet) depth ts
            | name == "p" -> go b depth ts
            | name == "br" -> go (b <> TB.fromText "\n") depth ts
            | otherwise -> go b depth ts
        TS.TagClose name
            | name `elem` ["h1", "h2", "h3", "strong", "em", "code", "pre"] ->
                go (b <> TB.fromText (reset <> "\n")) (if name `elem` ["pre", "ul", "ol"] then depth - 1 else depth) ts
            | name `elem` ["p", "li"] -> go (b <> TB.fromText "\n") depth ts
            | name `elem` ["ul", "ol"] -> go b (depth - 1) ts
            | otherwise -> go b depth ts
        TS.TagText txt -> go (b <> TB.fromText txt) depth ts
        _ -> go b depth ts

-- HTML からテキストのみ抽出（最低限の改行整形）
stripTags :: [TS.Tag Text] -> [TS.Tag Text]
stripTags = concatMap f
  where
    f (TS.TagText t) = [TS.TagText t]
    f (TS.TagOpen n _) | n `elem` ["p", "li", "br"] = [TS.TagText "\n"]
    f (TS.TagClose n) | n `elem` ["h1", "h2", "h3", "p", "li"] = [TS.TagText "\n"]
    f _ = []
