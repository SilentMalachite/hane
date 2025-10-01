{-# LANGUAGE CPP #-}

module Editor.Types (
    Keymap (..),
    icuEnabled,
) where

data Keymap = Vim | Emacs
    deriving (Eq, Show, Read)

icuEnabled :: Bool
#ifdef USE_ICU
icuEnabled = True
#else
icuEnabled = False
#endif
