{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Editor.Config
  ( Config(..)
  , ConfigError(..)
  , defaultConfig
  , loadConfig
  , parseConfigText
  , validateConfig
  ) where

import Data.Text (Text)
import Editor.Types (Keymap(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe, fromMaybe)
import System.Environment (lookupEnv)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (try, IOException, Exception)
import Data.Typeable (Typeable)

data ConfigError = 
    ConfigFileNotFound FilePath
  | ConfigParseError Text String
  | InvalidKeymapError Text
  | InvalidThemeError Text
  | InvalidLocaleError Text
  deriving (Show, Eq, Typeable)

instance Exception ConfigError

data Config = Config
  { cfgKeymap       :: Keymap
  , cfgTheme        :: Text
  , cfgFormatOnSave :: Bool
  , cfgLocale       :: Text
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cfgKeymap       = Vim
  , cfgTheme        = "default"
  , cfgFormatOnSave = False
  , cfgLocale       = "ja"
  }

-- | 設定ファイルを読み込む。失敗した場合は適切なエラーを返す
loadConfig :: IO (Either ConfigError Config)
loadConfig = do
  result <- try loadConfigInternal :: IO (Either IOException (Either ConfigError Config))
  case result of
    Left ioErr -> return $ Left $ ConfigParseError "" (show ioErr)
    Right configResult -> return configResult

loadConfigInternal :: IO (Either ConfigError Config)
loadConfigInternal = do
  envLoc <- lookupEnv "HANE_LOCALE"
  mPathEnv <- lookupEnv "HANE_CONFIG"
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let xdg = fromMaybe (home </> ".config") mXdg
      candidates =
        [ mPathEnv
        , Just (xdg </> "hane" </> "config.toml")
        , Just (home </> ".config" </> "hane" </> "config.toml")
        , Just ("config" </> "config.sample.toml")
        ]
  path <- pickFirstExisting candidates
  case path of
    Nothing -> return $ Right defaultConfig
    Just file -> do
      exists <- doesFileExist file
      if not exists
        then return $ Left $ ConfigFileNotFound file
        else do
          content <- TIO.readFile file
          case parseConfigText content of
            Left err -> return $ Left err
            Right base -> do
              let loc = fromMaybe (cfgLocale base) (T.pack <$> envLoc)
              return $ Right $ base { cfgLocale = loc }

pickFirstExisting :: [Maybe FilePath] -> IO (Maybe FilePath)
pickFirstExisting [] = return Nothing
pickFirstExisting (Nothing:xs) = pickFirstExisting xs
pickFirstExisting (Just p:xs) = do
  e <- doesFileExist p
  if e then return (Just p) else pickFirstExisting xs

-- | TOML風設定をパースする。より堅牢なエラーハンドリングを追加
parseConfigText :: Text -> Either ConfigError Config
parseConfigText txt = do
  -- コメント除去とセクション検出（簡易 TOML 互換: [editor] のみ対応）
  let ls0 = map (T.takeWhile (/= '#')) (T.lines txt)
      ls  = map T.strip ls0
      -- セクション状態を持ちながら k=v を収集
      kvs = collectKVs ls

      -- 正規化: 大文字小文字/表記ゆれを吸収
      normKey :: Text -> Text
      normKey k = case T.toLower k of
        "formatonsave"     -> "formatonsave"
        "format_on_save"   -> "formatonsave"
        "formatonsave"     -> "formatonsave"
        "keymap"           -> "keymap"
        "theme"            -> "theme"
        "locale"           -> "locale"
        _                   -> T.toLower k

      lookupKey :: Text -> Maybe Text
      lookupKey k = lookup (normKey k) kvs

      theme'  = fromMaybe (cfgTheme defaultConfig) (lookupKey "theme")
      locale' = fromMaybe (cfgLocale defaultConfig) (lookupKey "locale")
      fos'    = fromMaybe (cfgFormatOnSave defaultConfig)
                          (lookupKey "formatonsave" >>= parseBool)
      kmText  = fromMaybe (T.toLower $ T.pack $ show $ cfgKeymap defaultConfig)
                          (lookupKey "keymap")

  case parseKeymap kmText of
    Nothing -> Left $ InvalidKeymapError kmText
    Just km' ->
      if not (validateTheme theme')
        then Left $ InvalidThemeError theme'
        else if not (validateLocale locale')
          then Left $ InvalidLocaleError locale'
          else Right $ Config
                { cfgKeymap = km'
                , cfgTheme = theme'
                , cfgFormatOnSave = fos'
                , cfgLocale = locale'
                }
  where
    collectKVs :: [Text] -> [(Text, Text)]
    collectKVs = go False []
      where
        go _ acc [] = reverse acc
        go inEditor acc (l:ls')
          | T.null l = go inEditor acc ls'
          | T.head l == '[' && T.last l == ']' =
              let sect = T.toLower (T.dropEnd 1 (T.drop 1 l))
                  inEd = sect == "editor"
              in go inEd acc ls'
          | otherwise =
              case T.breakOn "=" l of
                (k, v) | not (T.null v) ->
                  let key0 = normalizeKey k
                      key = if inEditor then key0 else key0
                      val = normalizeVal (T.drop 1 v)
                  in go inEditor ((key, val):acc) ls'
                _ -> go inEditor acc ls'

    normalizeKey = T.toLower . T.strip
    normalizeVal = stripQuotes . T.strip

    stripQuotes s
      | T.length s >= 2 && T.head s == '"' && T.last s == '"' = T.dropEnd 1 (T.drop 1 s)
      | otherwise = s

    parseBool :: Text -> Maybe Bool
    parseBool s = case T.toLower s of
      "true"  -> Just True
      "false" -> Just False
      "yes"   -> Just True
      "no"    -> Just False
      "1"     -> Just True
      "0"     -> Just False
      _       -> Nothing

    parseKeymap :: Text -> Maybe Keymap
    parseKeymap s = case T.toLower s of
      "vim"   -> Just Vim
      "emacs" -> Just Emacs
      _       -> Nothing

-- | テーマ名の検証（今後の拡張のために）
validateTheme :: Text -> Bool
validateTheme theme = not (T.null theme)
                   && T.all (`elem` (['_','-'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) theme

-- | ロケールの検証
validateLocale :: Text -> Bool
validateLocale locale = not (T.null locale)
                    && T.all (`elem` (['_','-'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) locale

-- | 設定の完全な検証
validateConfig :: Config -> Either ConfigError Config
validateConfig config = do
  _ <- unless (validateTheme (cfgTheme config)) $ 
    Left $ InvalidThemeError (cfgTheme config)
  _ <- unless (validateLocale (cfgLocale config)) $ 
    Left $ InvalidLocaleError (cfgLocale config)
  Right config
  where
    unless condition action = if condition then Right config else action
