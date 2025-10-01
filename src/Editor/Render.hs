{-# LANGUAGE OverloadedStrings #-}

module Editor.Render (
    renderLine,
    displayWidth,
    displayWidthG,
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V

-- | 行レンダ（現状は恒等）
renderLine :: Text -> Text
renderLine = id

-- | 文字列全体の表示幅（wcswidth 相当、vty の実装に準拠）
displayWidth :: Text -> Int
displayWidth t = V.imageWidth (V.string V.defAttr (T.unpack t))

-- | 1 グラフェムの表示幅（結合文字列含む）
displayWidthG :: Text -> Int
displayWidthG = displayWidth
