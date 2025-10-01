{-# LANGUAGE OverloadedStrings #-}

module Editor.ConfigSpec (spec) where

import qualified Data.Text as T
import Editor.Config (Config (..), ConfigError (..), defaultConfig, parseConfigText, validateConfig)
import Editor.Types (Keymap (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "parseConfigText" $ do
        it "parses empty config as default" $ do
            let result = parseConfigText ""
            result `shouldBe` Right defaultConfig

        it "parses valid keymap config" $ do
            let input = T.unlines ["keymap = vim", "theme = dark"]
                expected = defaultConfig{cfgKeymap = Vim, cfgTheme = "dark"}
            parseConfigText input `shouldBe` Right expected

        it "parses formatOnSave as boolean" $ do
            let input = T.unlines ["formatOnSave = true"]
                expected = defaultConfig{cfgFormatOnSave = True}
            parseConfigText input `shouldBe` Right expected

        it "parses [editor] table with snake_case keys" $ do
            let input =
                    T.unlines
                        [ "[editor]"
                        , "keymap = \"emacs\""
                        , "theme = \"dark\""
                        , "format_on_save = true"
                        , "locale = \"en_US\""
                        ]
                expected =
                    Config
                        { cfgKeymap = Emacs
                        , cfgTheme = "dark"
                        , cfgFormatOnSave = True
                        , cfgLocale = "en_US"
                        }
            parseConfigText input `shouldBe` Right expected

        it "rejects invalid keymap" $ do
            let input = "keymap = invalid"
                result = parseConfigText input
            case result of
                Left (InvalidKeymapError _) -> return ()
                _ -> expectationFailure "Should reject invalid keymap"

        it "handles quoted values" $ do
            let input = T.unlines ["theme = \"custom_theme\"", "locale = \"en_US\""]
                expected = defaultConfig{cfgTheme = "custom_theme", cfgLocale = "en_US"}
            parseConfigText input `shouldBe` Right expected

        it "ignores comments" $ do
            let input = T.unlines ["# This is a comment", "theme = dark # inline comment"]
                expected = defaultConfig{cfgTheme = "dark"}
            parseConfigText input `shouldBe` Right expected

    describe "validateConfig" $ do
        it "accepts valid config" $ do
            let config = defaultConfig
            validateConfig config `shouldBe` Right config

        it "rejects invalid theme" $ do
            let config = defaultConfig{cfgTheme = ""}
            case validateConfig config of
                Left (InvalidThemeError _) -> return ()
                _ -> expectationFailure "Should reject invalid theme"

        it "rejects invalid locale" $ do
            let config = defaultConfig{cfgLocale = "invalid@locale"}
            case validateConfig config of
                Left (InvalidLocaleError _) -> return ()
                _ -> expectationFailure "Should reject invalid locale"
