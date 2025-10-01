module Main (main) where

import Data.Version (showVersion)
import Editor.App (run)
import Paths_hane (version)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin, stdout)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> showHelp >> exitSuccess
        ["-h"] -> showHelp >> exitSuccess
        ["--version"] -> showVersionInfo >> exitSuccess
        ["-v"] -> showVersionInfo >> exitSuccess
        _ -> do
            -- Check if running in a proper terminal environment
            stdinTTY <- hIsTerminalDevice stdin
            stdoutTTY <- hIsTerminalDevice stdout
            if stdinTTY && stdoutTTY
                then run
                else do
                    hPutStrLn stderr "⚠️  ターミナル環境で実行してください (TTY必須)"
                    hPutStrLn stderr "Please run in a proper terminal environment (TTY required)"
                    hPutStrLn stderr "Try: make run or run directly in Terminal.app"
                    exitWith (ExitFailure 2)

showHelp :: IO ()
showHelp = do
    putStrLn "Hane - Haskell TUI Editor"
    putStrLn ""
    putStrLn "USAGE:"
    putStrLn "  hane          Start the TUI editor"
    putStrLn "  hane --help   Show this help message"
    putStrLn "  hane --version Show version information"
    putStrLn ""
    putStrLn "REQUIREMENTS:"
    putStrLn "  - Must be run in a proper terminal (TTY required)"
    putStrLn "  - Supports Vim and Emacs keybindings"
    putStrLn "  - Optimized for Markdown and Haskell editing"
    putStrLn ""
    putStrLn "For more information, see README.md"

showVersionInfo :: IO ()
showVersionInfo = do
    putStrLn $ "Hane version " ++ Data.Version.showVersion version
    putStrLn "Terminal-based Haskell/Markdown editor"
