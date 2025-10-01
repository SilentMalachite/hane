{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Editor.Buffer (
    Buffer,
    empty,
    fromText,
    toText,
    graphemes,
    graphemesWithLocale,
    moveLeft,
    moveRight,
    selectRange,
) where

import Data.Char (GeneralCategory (..), generalCategory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Normalize as N
#ifdef USE_ICU
import qualified Data.Text.ICU as ICU
#endif
#ifdef USE_ICU
-- ICU 導入済み。将来的に BreakIterator に切替予定だが、現状は安定なフォールバック分割を使用。
#endif

isCombining :: Char -> Bool
isCombining c = case generalCategory c of
    NonSpacingMark -> True
    SpacingCombiningMark -> True
    EnclosingMark -> True
    _ -> False

isVariationSelector :: Char -> Bool
isVariationSelector c =
    (c >= '\xFE00' && c <= '\xFE0F')
        || (c >= '\xE0100' && c <= '\xE01EF') -- VS1..VS16
        -- VS17..VS256 (plane 14)

isZWJ :: Char -> Bool
isZWJ c = c == '\x200D'

isEmojiModifier :: Char -> Bool
isEmojiModifier c = c >= '\x1F3FB' && c <= '\x1F3FF'

-- | テキストを拡張グラフェムクラスターに分割
graphemes :: Text -> [Text]
#ifdef USE_ICU
graphemes t = graphemesWithLocale "ja" t
graphemesWithLocale :: Text -> Text -> [Text]
graphemesWithLocale loc t =
  let tN = N.normalize N.NFC t
      ln = if T.toLower loc == "auto" then ICU.Current else ICU.Locale (T.unpack loc)
      brk = ICU.breakCharacter ln
  in map ICU.brkBreak (ICU.breaks brk tN)
#else
graphemes = go . N.normalize N.NFC
  where
    go txt
      | T.null txt = []
      | otherwise  = let (g, rest) = takeOne txt in g : go rest

graphemesWithLocale :: Text -> Text -> [Text]
graphemesWithLocale _ = graphemes

takeOne :: Text -> (Text, Text)
takeOne s0 =
  case T.uncons s0 of
    Nothing -> (T.empty, T.empty)
    Just (c, s1) ->
      let (cluster, rest) = consumeAfterBase (TB.singleton c) s1
      in (TL.toStrict (TB.toLazyText cluster), rest)

consumeAfterBase :: TB.Builder -> Text -> (TB.Builder, Text)
consumeAfterBase b s =
  case T.uncons s of
    Nothing -> (b, T.empty)
    Just (d, s')
      | isCombining d || isVariationSelector d || isEmojiModifier d -> consumeAfterBase (b <> TB.singleton d) s'
      | isZWJ d ->
          -- ZWJ: 次の基底字とその結合記号も同じクラスターに含める
          case T.uncons s' of
            Nothing -> (b <> TB.singleton d, T.empty)
            Just (e, s'') -> consumeAfterBase (b <> TB.singleton d <> TB.singleton e) s''
      | otherwise -> (b, s)

#endif

-- NOTE: MVP として Text ベース。将来的に rope 実装に差し替え予定。
newtype Buffer = Buffer {unBuffer :: Text}
    deriving (Eq, Show)

empty :: Buffer
empty = Buffer T.empty

fromText :: Text -> Buffer
fromText = Buffer

toText :: Buffer -> Text
toText = unBuffer

-- | 左へ 1 グラフェム移動（範囲外は 0 にクランプ）
moveLeft :: Int -> Text -> Int
moveLeft i _t =
    max 0 (min i (i - 1))

-- | 右へ 1 グラフェム移動（範囲外は 末尾 にクランプ）
moveRight :: Int -> Text -> Int
moveRight i t =
    max 0 (min (i + 1) (length (graphemes t)))

-- | グラフェム単位の選択テキストを取得
selectRange :: Int -> Int -> Text -> Text
selectRange a b t =
    let gs = graphemes t
        lo = max 0 (min a b)
        hi = min (length gs) (max a b)
     in T.concat (take (hi - lo) (drop lo gs))
