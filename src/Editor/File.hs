{-# LANGUAGE OverloadedStrings #-}

module Editor.File
  ( FileOperation(..)
  , FileDialogState(..)
  , FileDialogMode(..)
  , FileError(..)
  , openFile
  , saveFile
  , getFileExtension
  , isMarkdownFile
  , isHaskellFile
  , validateFilePath
  , createFileDialog
  , moveSelection
  , goUpDirectory
  , enterDirectory
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory
  ( doesFileExist
  , doesDirectoryExist
  , listDirectory
  )
import System.FilePath
  ( takeFileName
  , takeExtension
  , (</>)
  , isAbsolute
  , normalise
  , takeDirectory
  )
import Control.Exception (try, IOException)
import Data.List (sort)

-- | ファイル操作の種類
data FileOperation = OpenFile | SaveFile | SaveAsFile
  deriving (Eq, Show)

-- | ファイルダイアログの状態
data FileDialogState = FileDialogState
  { fdCurrentPath :: FilePath
  , fdSelectedIndex :: Int
  , fdFiles :: [FilePath]
  , fdMode :: FileDialogMode
  , fdFilter :: FilePath -> Bool
  }

instance Eq FileDialogState where
  a == b = fdCurrentPath a == fdCurrentPath b &&
           fdSelectedIndex a == fdSelectedIndex b &&
           fdFiles a == fdFiles b &&
           fdMode a == fdMode b

instance Show FileDialogState where
  show state = "FileDialogState { path = " ++ fdCurrentPath state ++ 
               ", index = " ++ show (fdSelectedIndex state) ++
               ", files = " ++ show (fdFiles state) ++
               ", mode = " ++ show (fdMode state) ++ " }"

-- | ファイルダイアログのモード
data FileDialogMode = OpenMarkdown | OpenHaskell | SaveMarkdown | SaveHaskell | SaveAsMarkdown | SaveAsHaskell
  deriving (Eq, Show)

-- | ファイル操作エラー
data FileError
  = FileNotFound FilePath
  | FileReadError FilePath String
  | FileWriteError FilePath String
  | DirectoryNotFound FilePath
  | InvalidFilePath FilePath
  | PermissionDenied FilePath
  deriving (Show, Eq)

-- | ファイルを開く
openFile :: FilePath -> IO (Either FileError Text)
openFile path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ FileNotFound path
    else do
      result <- try $ TIO.readFile path :: IO (Either IOException Text)
      case result of
        Left e -> return $ Left $ FileReadError path (show e)
        Right content -> return $ Right content

-- | ファイルを保存する
saveFile :: FilePath -> Text -> IO (Either FileError ())
saveFile path content = do
  result <- try $ TIO.writeFile path content :: IO (Either IOException ())
  case result of
    Left e -> return $ Left $ FileWriteError path (show e)
    Right _ -> return $ Right ()

-- | ディレクトリの内容をリストアップ（ソート付き）。
listDirectorySafe :: FilePath -> IO (Either FileError [FilePath])
listDirectorySafe path = do
  exists <- doesDirectoryExist path
  if not exists
    then return $ Left $ DirectoryNotFound path
    else do
      result <- try $ listDirectory path :: IO (Either IOException [FilePath])
      case result of
        Left _ -> return $ Left $ DirectoryNotFound path
        Right files -> return $ Right $ sort files

-- | ファイル拡張子を取得
getFileExtension :: FilePath -> String
getFileExtension = takeExtension

-- | Markdownファイルかどうか判定
isMarkdownFile :: FilePath -> Bool
isMarkdownFile path = 
  let ext = map toLower $ getFileExtension path
  in ext `elem` [".md", ".markdown", ".mdown", ".mkd"]

-- | Haskellファイルかどうか判定
isHaskellFile :: FilePath -> Bool
isHaskellFile path = 
  let ext = map toLower $ getFileExtension path
  in ext `elem` [".hs", ".lhs", ".hsc"]

-- | ファイルパスの妥当性を検証
validateFilePath :: FilePath -> Bool
validateFilePath path =
  let base = takeFileName path
      invalidBase = null base || base == "." || base == ".." || base == "/"
  in  not (null path)
      && not invalidBase
      && (isAbsolute path || not (null $ normalise path))

-- | ファイルダイアログの初期状態を作成
createFileDialog :: FileDialogMode -> FilePath -> IO (Either FileError FileDialogState)
createFileDialog mode initialPath = do
  let currentPath = if null initialPath then "." else initialPath
      filterFunc = case mode of
        OpenMarkdown -> isMarkdownFile
        OpenHaskell -> isHaskellFile
        SaveMarkdown -> const True
        SaveHaskell -> const True
        SaveAsMarkdown -> const True
        SaveAsHaskell -> const True
  
  result <- listDirectorySafe currentPath
  case result of
    Left e -> return $ Left e
    Right files -> do
      -- ディレクトリは常に表示、ファイルはモードのフィルタで絞る
      dirs <- mapM (\f -> doesDirectoryExist (currentPath </> f)) files
      let zipped = zip files dirs
          filteredFiles = [ f | (f, isDir) <- zipped
                              , isDir || filterFunc f ]
      return $ Right $ FileDialogState
        { fdCurrentPath = currentPath
        , fdSelectedIndex = 0
        , fdFiles = filteredFiles
        , fdMode = mode
        , fdFilter = filterFunc
        }

-- | ファイルダイアログで選択を移動
moveSelection :: FileDialogState -> Int -> FileDialogState
moveSelection state delta = 
  let newIndex = max 0 $ min (length (fdFiles state) - 1) (fdSelectedIndex state + delta)
  in state { fdSelectedIndex = newIndex }

-- | ファイルダイアログで親ディレクトリに移動
goUpDirectory :: FileDialogState -> IO (Either FileError FileDialogState)
goUpDirectory state = do
  let currentPath = fdCurrentPath state
      parentPath = if currentPath == "/" then "/" else takeDirectory currentPath
  
  result <- listDirectorySafe parentPath
  case result of
    Left e -> return $ Left e
    Right files -> do
      dirs <- mapM (\f -> doesDirectoryExist (parentPath </> f)) files
      let filteredFiles = [ f | (f, isDir) <- zip files dirs
                              , isDir || fdFilter state f ]
      return $ Right $ state
        { fdCurrentPath = parentPath
        , fdSelectedIndex = 0
        , fdFiles = filteredFiles
        }

-- | ファイルダイアログでディレクトリに入る
enterDirectory :: FileDialogState -> IO (Either FileError FileDialogState)
enterDirectory state = do
  let files = fdFiles state
      selectedIndex = fdSelectedIndex state
  
  if selectedIndex >= length files
    then return $ Left $ DirectoryNotFound "Invalid selection"
    else do
      let selectedFile = files !! selectedIndex
          newPath = fdCurrentPath state </> selectedFile
      
      isDir <- doesDirectoryExist newPath
      if isDir
        then do
          result <- listDirectorySafe newPath
          case result of
            Left e -> return $ Left e
            Right newFiles -> do
              dirs <- mapM (\f -> doesDirectoryExist (newPath </> f)) newFiles
              let filteredFiles = [ f | (f, isDir') <- zip newFiles dirs
                                      , isDir' || fdFilter state f ]
              return $ Right $ state
                { fdCurrentPath = newPath
                , fdSelectedIndex = 0
                , fdFiles = filteredFiles
                }
        else return $ Left $ DirectoryNotFound selectedFile

-- ヘルパー関数
toLower :: Char -> Char
toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- 標準の FilePath 関数を利用するため、自前の実装は撤去
