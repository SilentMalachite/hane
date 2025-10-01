{-# LANGUAGE OverloadedStrings #-}

module Editor.Haskell (
    formatBuffer,
    quickCheck,
    startRepl,
) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (findExecutable)
import System.Process.Typed

-- fourmolu / ormolu を優先順で実行
formatBuffer :: Text -> IO (Either Text Text)
formatBuffer input = do
    rFourmolu <- formatWith "fourmolu" ["--stdin-input-file", "InMemory.hs", "-q"] input
    case rFourmolu of
        Right t -> pure (Right t)
        Left _ -> formatWith "ormolu" ["--stdin-input-file", "InMemory.hs"] input

formatWith :: String -> [String] -> Text -> IO (Either Text Text)
formatWith exe args input = do
    mx <- findExecutable exe
    case mx of
        Nothing -> pure $ Left (T.pack (exe <> " が見つかりません"))
        Just _ -> do
            let p =
                    setStdin (byteStringInput (BL.fromStrict (TE.encodeUtf8 input))) $
                        setStdout byteStringOutput $
                            setStderr byteStringOutput $
                                proc exe args
            eres <- try (readProcess p) :: IO (Either SomeException (ExitCode, BL.ByteString, BL.ByteString))
            case eres of
                Left e -> pure $ Left (T.pack (exe <> " 実行エラー: " <> show e))
                Right (ExitSuccess, out, _) ->
                    pure $ Right (TE.decodeUtf8 (BL.toStrict out))
                Right (ExitFailure _, _, err) ->
                    pure $ Left (TE.decodeUtf8 (BL.toStrict err))

-- クイックチェック（将来 ghc -fno-code 連携を予定）
quickCheck :: FilePath -> IO Bool
quickCheck path = do
    -- 1) ghc -fno-code があれば型チェックのみ実行
    mGhc <- findExecutable "ghc"
    case mGhc of
        Just _ -> do
            let p =
                    setStdout byteStringOutput $
                        setStderr byteStringOutput $
                            proc "ghc" ["-fno-code", "-v0", path]
            (_, _, err) <- readProcess p
            pure (BL.null err)
        Nothing -> do
            -- 2) ghci があれば -fno-code で代替
            mGhci <- findExecutable "ghci"
            case mGhci of
                Just _ -> do
                    let p =
                            setStdout byteStringOutput $
                                setStderr byteStringOutput $
                                    proc "ghci" ["-v0", "-ignore-dot-ghci", "-fno-code", path]
                    (ec, _, _) <- readProcess p
                    pure (ec == ExitSuccess)
                Nothing -> pure True -- 環境にコンパイラが無い場合はスキップ（失敗扱いにしない）

-- REPL 起動（将来 cabal repl 連携を予定）
startRepl :: IO ()
startRepl = do
    -- 可能なら cabal repl を、無ければ ghci を、そのどちらも無ければ何もしない
    mCabal <- findExecutable "cabal"
    case mCabal of
        Just _ -> do
            let p =
                    setStdin inherit $
                        setStdout inherit $
                            setStderr inherit $
                                proc "cabal" ["repl"]
            runProcess_ p
        Nothing -> do
            mGhci <- findExecutable "ghci"
            case mGhci of
                Just _ ->
                    runProcess_
                        ( setStdin inherit $
                            setStdout inherit $
                                setStderr inherit $
                                    proc "ghci" []
                        )
                Nothing -> pure ()
