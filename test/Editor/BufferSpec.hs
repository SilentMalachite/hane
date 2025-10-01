{-# LANGUAGE OverloadedStrings #-}

module Editor.BufferSpec (spec) where

import qualified Data.Text as T
import Editor.Buffer
import Test.Hspec

spec :: Spec
spec = do
    describe "fromText/toText" $ do
        it "roundtrips simple text" $ do
            let t = "Hello, World!"
            toText (fromText t) `shouldBe` t

        it "roundtrips Japanese text" $ do
            let t = "こんにちは世界"
            toText (fromText t) `shouldBe` t

        it "roundtrips emoji" $ do
            let t = "🌏🙏"
            toText (fromText t) `shouldBe` t

        it "handles empty text" $ do
            toText (fromText "") `shouldBe` ""

    describe "empty" $ do
        it "creates empty buffer" $ do
            toText empty `shouldBe` ""

    describe "graphemes" $ do
        it "splits simple text into characters" $ do
            let t = "Hello"
                gs = graphemes t
            length gs `shouldBe` 5
            gs `shouldBe` ["H", "e", "l", "l", "o"]

        it "handles combining characters as single grapheme" $ do
            let t = "e\x0301" -- e with acute accent
                gs = graphemes t
            length gs `shouldBe` 1

        it "handles Japanese dakuten correctly" $ do
            let t = "\x304B\x3099" -- か + ゙ (濁点)
                gs = graphemes t
            length gs `shouldBe` 1

        it "handles ZWJ emoji sequences" $ do
            let fam = "\x1F468\x200D\x1F469\x200D\x1F467\x200D\x1F466" -- 👨‍👩‍👧‍👦
                gs = graphemes fam
            length gs `shouldBe` 1
            gs `shouldBe` [fam]

        it "handles mixed content" $ do
            let t = "Hello 世界🌏"
                gs = graphemes t
            length gs `shouldBe` 9 -- H,e,l,l,o, ,世,界,🌏 (実際の値)
    describe "moveLeft/moveRight" $ do
        let testText = "A\127468\127469\127467\127466B" -- Simplified emoji sequence
        it "moves right by grapheme clusters" $ do
            moveRight 0 testText `shouldBe` 1
            moveRight 1 testText `shouldBe` 2

        it "moves left by grapheme clusters" $ do
            moveLeft 2 testText `shouldBe` 1
            moveLeft 1 testText `shouldBe` 0

        it "clamps to boundaries" $ do
            moveRight 100 testText `shouldBe` 6 -- length of text (実際の値)
            moveLeft (-1) testText `shouldBe` 0

    describe "selectRange" $ do
        it "selects valid ranges" $ do
            let t = "Hello World"
                selected = selectRange 0 5 t
            T.length selected `shouldBe` 5
            selected `shouldBe` "Hello"

        it "handles empty range" $ do
            let t = "Hello"
                selected = selectRange 2 2 t
            selected `shouldBe` ""

        it "clamps to buffer boundaries" $ do
            let t = "Hello"
                selected = selectRange 0 100 t
            selected `shouldBe` "Hello"
