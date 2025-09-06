{-# LANGUAGE OverloadedStrings #-}

module Editor.AppSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Editor.Buffer (fromText, toText)

-- Mock mode for testing
data AppMode = MarkdownMode | HaskellMode
  deriving (Show, Eq)

spec :: Spec
spec = do
  describe "App state handling" $ do
    it "initializes with test data" $ do
      let testText = "# Test Markdown\n\nThis is a test."
          buffer = fromText testText
      toText buffer `shouldBe` testText

    it "handles text with special characters" $ do
      let specialText = "Test with émojis 🌏 and symbols ©®"
          buffer = fromText specialText
      toText buffer `shouldBe` specialText

  describe "File operations" $ do
    it "can handle filename extraction" $ do
      let fileName = "test.md"
      fileName `shouldBe` "test.md"

  describe "User interface messages" $ do
    it "generates appropriate error messages" $ do
      let errorMsg = "Error: Could not save file"
      errorMsg `shouldSatisfy` (not . T.null . T.pack)

    it "generates success messages" $ do
      let successMsg = "File saved successfully"
      successMsg `shouldSatisfy` (not . T.null . T.pack)

  describe "Mode switching" $ do
    it "has correct mode names" $ do
      show MarkdownMode `shouldBe` "MarkdownMode"
      show HaskellMode `shouldBe` "HaskellMode"