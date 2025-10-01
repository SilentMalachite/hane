# Hane 設定ファイル

## 設定ファイルの探索順序

Haneは以下の順序で設定ファイルを探索します：

1. `HANE_CONFIG` 環境変数で指定されたパス
2. `$XDG_CONFIG_HOME/hane/config.toml`
3. `$HOME/.config/hane/config.toml`
4. プロジェクトルートの `config/config.sample.toml`（デフォルト設定）

## 設定ファイル形式

設定ファイルはTOML形式です：

```toml
[editor]
keymap = "vim"           # キーマップ: "vim" | "emacs"
theme = "default"        # テーマ名: "default" | "dark" | "light"
format_on_save = false   # 保存時の自動整形: true | false
locale = "ja"            # ロケール: "auto" | "ja" | "en_US" など

[keymap]
# カスタムキーバインド（将来の拡張用）
# 現在は組み込みのVim/Emacsキーマップのみサポート
```

## 設定項目の詳細

### keymap
使用するキーマップを指定します。
- `"vim"`: Vim風のキーバインド（Normalモード/Insertモード）
- `"emacs"`: Emacs風のキーバインド

### theme
エディタのテーマを指定します。
- `"default"`: デフォルトテーマ
- `"dark"`: ダークテーマ
- `"light"`: ライトテーマ

### format_on_save
ファイル保存時に自動的に整形を行うかどうかを指定します。
- `true`: 保存時にHaskellコードをfourmolu/ormoluで整形
- `false`: 手動での整形のみ

### locale
エラーメッセージなどの表示言語を指定します。
- `"auto"`: システムのロケールを自動検出
- `"ja"`: 日本語表示
- `"en_US"`: 英語表示
- その他: 指定したロケール文字列

## 環境変数

以下の環境変数で設定を上書きできます：

- `HANE_CONFIG`: 設定ファイルのパスを指定
- `HANE_LOCALE`: ロケール設定を上書き（例: `en_US`, `ja_JP.UTF-8`）

## 設定の注意点

- 設定キーは大文字小文字を区別しません（例: `format_on_save` と `formatOnSave` は同等）
- 設定ファイルの構文エラーは無視され、デフォルト値が使用されます
- テーマ機能は現在開発中で、将来的により多くのカスタマイズが可能になる予定です
