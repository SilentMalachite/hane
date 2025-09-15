# Hane — Haskell TUI Editor

[![CI](https://github.com/SilentMalachite/hane/actions/workflows/ci.yml/badge.svg)](https://github.com/SilentMalachite/hane/actions/workflows/ci.yml)

Hane は、Markdown と Haskell に最適化した端末ベースの軽量エディタです。日本語テキスト（全角・絵文字等）の表示幅やグラフェム単位の安全なカーソル移動を重視します。
ライブラリは CI で常に `-Wall -Werror` による厳格ビルドを行い、警告の混入を防止します。

## クローン

```bash
git clone https://github.com/SilentMalachite/hane.git
cd hane
```

（SSH の場合）

```bash
git clone git@github.com:SilentMalachite/hane.git
cd hane
```

## 機能

### コア機能
- TUI 起動・終了（`q` で終了）
- エディタ基盤（Buffer/Render/Markdown/Haskell/Config/App）
- Markdown → ANSI/Plain プレビュー
- Haskell フォーマット（`fourmolu`→`ormolu` フォールバック）
- 日本語対応（グラフェム分割・全角幅計算）
  - ICU あり: `-f icu` で `text-icu` による正確なグラフェム境界
  - ICU なし: 合成文字/ZWJ/バリアントセレクタを考慮したフォールバック
 - ファイル操作時のわかりやすいエラーメッセージ（権限・存在有無・不正パスなどを日本語で案内）

### ファイル操作
- ファイル保存（`Ctrl+S`）
- ファイルオープン（`Ctrl+O`）
- モード切替（`Esc`でMarkdown/Haskellモード切り替え）
 - Emacs: `C-x C-s`/`C-x C-f`/`C-x C-w`/`C-x C-c` 等
 - Vim: Normal/Insert モード（Normal: `q,i,h,l,x,w` など）

### 設定
- キーマップ（Vim/Emacs）
- テーマ（default/dark/light）
- 自動フォーマット（formatOnSave）
- ロケール設定（locale）

## ビルド & 実行

### 初回セットアップ

```bash
# 依存関係とビルド
cabal update
cabal build

# 開発ツールのインストール（オプション）
make tools
```

### Cabal

```bash
cabal build
cabal run hane
```

### Make

```bash
make setup    # 初期セットアップ
make build    # ビルド
make run      # 実行
make test     # テスト実行
```

### ICU の準備（オプション）

**注意**: ICU サポートは現在オプションです。デフォルトでは無効になっており、プロジェクトは ICU なしでビルドできます。

より正確な日本語テキスト処理のために ICU を有効化したい場合：

- macOS (Homebrew):
  - `brew install icu4c`
  - `export PKG_CONFIG_PATH="$(brew --prefix)/opt/icu4c/lib/pkgconfig:$PKG_CONFIG_PATH"`
- Ubuntu/Debian:
  - `sudo apt-get install -y libicu-dev`
- Fedora/RHEL:
  - `sudo dnf install -y libicu-devel`

その後、ICU 有効でビルド:
```bash
cabal build -f +icu
cabal run -f +icu hane
```

## ディレクトリ構成

```
Hane/
 ├─ app/Main.hs            # エントリポイント
 ├─ src/Editor/*           # コアモジュール
 ├─ test/                 # テスト
 |   ├─ Spec.hs           # テスト実行エントリ
 |   └─ Editor/*.hs       # 各モジュールのテスト
 ├─ config/config.sample.toml
 ├─ Makefile              # ビルドスクリプト
 ├─ hane.cabal            # Cabal 設定
 └─ cabal.project
```

## 設定ファイル

### パスの優先順位
1. `HANE_CONFIG` 環境変数で指定されたパス
2. `$XDG_CONFIG_HOME/hane/config.toml`
3. `$HOME/.config/hane/config.toml`
4. `config/config.sample.toml`（同梱）

### 設定例（TOML 風、[editor] テーブル）
```toml
[editor]
keymap = "vim"           # "vim" | "emacs"
theme = "default"        # テーマ名
format_on_save = false    # 保存時の自動整形
locale = "ja"            # "ja", "en_US" 等（環境変数 HANE_LOCALE でも上書き可）
```

より詳しい設定項目は `docs/CONFIG.md` を参照。

## 開発

### テスト
```bash
make test           # テスト実行
make test-coverage  # カバレッジ付きテスト
```

### コードフォーマット
```bash
make fmt      # コードフォーマット
make fmt-check # フォーマットチェック
```

### リンター
```bash
make lint     # コード品質チェック
```

## CI/CD

```bash
make ci       # CI向けビルドとテスト
```

GitHub Actions は GHC 9.6.5 / 9.6.7 の行列でビルド/テストし、ライブラリは `-Wall -Werror` で厳格ビルドします。

ローカルでは GHC 9.12 系でもビルド確認済みです（依存上限を緩和済み）。

## キーバインド

- Emacs: `docs/KEYMAPS.md#emacs` を参照
- Vim: `docs/KEYMAPS.md#vim` を参照

## 貢献（Contributing）

ガイドラインは `CONTRIBUTING.md` を参照してください。行動規範は `CODE_OF_CONDUCT.md`。脆弱性は `SECURITY.md` を参照のうえご連絡ください。

## リリース

リリース手順は `docs/RELEASE.md` を参照。

## ライセンス
3条項BSDライセンス（詳細はLICENSEファイル参照）
