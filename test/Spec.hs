module Main (main) where

import Test.Hspec
import qualified Data.Text as T
import Editor.Buffer
import Editor.Render
import qualified Editor.ConfigSpec as Config
import qualified Editor.BufferSpec as Buffer
import qualified Editor.AppSpec as App
import qualified Editor.FileSpec as File

main :: IO ()
main = hspec $ do
  -- Original tests
  describe "Editor.Buffer" $ do
    it "round-trips Japanese text" $ do
      let t = T.pack "こんにちは世界🌏"
      toText (fromText t) `shouldBe` t

    it "graphemes: combining dakuten stays in one cluster" $ do
      let t = T.pack "\x304B\x3099" -- か + ゙ (濁点)
      length (graphemes t) `shouldBe` 1

    it "graphemes: ZWJ emoji family is one cluster" $ do
      let fam = T.pack "\x1F468\x200D\x1F469\x200D\x1F467\x200D\x1F466" -- 👨‍👩‍👧‍👦
      graphemes fam `shouldBe` [fam]

    it "moveLeft/moveRight clamp and step by grapheme" $ do
      let fam = T.pack "\x1F468\x200D\x1F469\x200D\x1F467\x200D\x1F466"
          t = T.concat [T.pack "A", fam, T.pack "B"]
          gs = graphemes t -- ["A","👨‍👩‍👧‍👦","B"]
      length gs `shouldBe` 3
      moveRight 0 t `shouldBe` 1
      moveLeft 1 t `shouldBe` 0

  describe "Editor.Render" $ do
    it "display width: e + combining acute has width 1" $ do
      displayWidth (T.pack "e\x0301") `shouldBe` 1
    it "display width: fullwidth A is wide (>=2)" $ do
      displayWidth (T.pack "Ａ") `shouldSatisfy` (>= 2)
    
  -- New module tests
  Config.spec
  Buffer.spec  
  App.spec
  File.spec
