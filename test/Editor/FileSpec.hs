{-# LANGUAGE OverloadedStrings #-}

module Editor.FileSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeFile, removeDirectory)
import System.FilePath ((</>))
import Control.Exception (catch, try)
import Editor.File

spec :: Spec
spec = do
  describe "File operations" $ do
    it "validates file paths correctly" $ do
      validateFilePath "test.txt" `shouldBe` True
      validateFilePath "/absolute/path.txt" `shouldBe` True
      validateFilePath "" `shouldBe` False
      validateFilePath "/" `shouldBe` False

    it "identifies file types correctly" $ do
      isMarkdownFile "test.md" `shouldBe` True
      isMarkdownFile "test.markdown" `shouldBe` True
      isMarkdownFile "test.txt" `shouldBe` False
      isMarkdownFile "test.MD" `shouldBe` True

      isHaskellFile "test.hs" `shouldBe` True
      isHaskellFile "test.lhs" `shouldBe` True
      isHaskellFile "test.txt" `shouldBe` False
      isHaskellFile "test.HS" `shouldBe` True

    it "handles file operations with proper error handling" $ do
      -- Test file not found
      result <- openFile "nonexistent.txt"
      case result of
        Left (FileNotFound _) -> return ()
        _ -> expectationFailure "Should return FileNotFound for nonexistent file"

      -- Test file read/write cycle
      let testContent = "Hello, World!\nThis is a test file."
          testPath = "test_file.txt"
      
      -- Clean up any existing test file
      _ <- try (removeFile testPath) :: IO (Either IOError ())
      
      -- Write file
      writeResult <- saveFile testPath (T.pack testContent)
      writeResult `shouldBe` Right ()
      
      -- Read file back
      readResult <- openFile testPath
      case readResult of
        Right content -> content `shouldBe` T.pack testContent
        Left err -> expectationFailure $ "Should read file successfully, got: " ++ show err
      
      -- Clean up
      _ <- try (removeFile testPath) :: IO (Either IOError ())
      return ()

  describe "File dialog operations" $ do
    it "creates file dialog with correct mode" $ do
      -- Test markdown dialog creation
      mdResult <- createFileDialog OpenMarkdown "."
      case mdResult of
        Right dialog -> fdMode dialog `shouldBe` OpenMarkdown
        Left _ -> expectationFailure "Should create markdown dialog"
      
      -- Test haskell dialog creation
      hsResult <- createFileDialog OpenHaskell "."
      case hsResult of
        Right dialog -> fdMode dialog `shouldBe` OpenHaskell
        Left _ -> expectationFailure "Should create haskell dialog"

    it "moves selection correctly" $ do
      let dialog = FileDialogState
            { fdCurrentPath = "."
            , fdSelectedIndex = 1
            , fdFiles = ["file1.txt", "file2.txt", "file3.txt"]
            , fdMode = OpenMarkdown
            , fdFilter = const True
            }
      
      let movedUp = moveSelection dialog (-1)
      fdSelectedIndex movedUp `shouldBe` 0
      
      let movedDown = moveSelection dialog 1
      fdSelectedIndex movedDown `shouldBe` 2
      
      -- Test boundary conditions
      let atTop = dialog { fdSelectedIndex = 0 }
      let movedUpFromTop = moveSelection atTop (-1)
      fdSelectedIndex movedUpFromTop `shouldBe` 0
      
      let atBottom = dialog { fdSelectedIndex = 2 }
      let movedDownFromBottom = moveSelection atBottom 1
      fdSelectedIndex movedDownFromBottom `shouldBe` 2

  describe "Error handling" $ do
    it "handles file not found" $ do
      result <- openFile "nonexistent_file.txt"
      case result of
        Left (FileNotFound _) -> return ()
        _ -> expectationFailure "Should return FileNotFound for nonexistent file"

    it "handles permission errors gracefully" $ do
      -- This test might not work on all systems, but it's good to have
      result <- openFile "/root/restricted_file.txt"
      case result of
        Left (FileReadError _ _) -> return ()
        Left (PermissionDenied _) -> return ()
        Left (FileNotFound _) -> return () -- File might not exist
        Right _ -> expectationFailure "Should not be able to read restricted file"