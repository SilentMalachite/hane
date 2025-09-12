# Config

## 探索順
1. `HANE_CONFIG`
2. `$XDG_CONFIG_HOME/hane/config.toml`
3. `$HOME/.config/hane/config.toml`
4. `config/config.sample.toml`

## 形式（TOML 風）
```toml
[editor]
keymap = "vim"           # "vim" | "emacs"
theme = "default"
format_on_save = false
locale = "ja"            # "auto"/"ja"/"en_US" など
```

備考: キーは大文字小文字を区別せず、表記ゆれを吸収します。
`format_on_save` は `formatOnSave`/`formatonsave` でも解釈されます。

## 環境変数
- `HANE_LOCALE`: ロケールを上書き（例: `en_US`）
