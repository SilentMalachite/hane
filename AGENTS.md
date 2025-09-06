# Repository Guidelines

## プロジェクト構成とモジュール
- `app/Main.hs`: 実行エントリ（バイナリ `hane`）。
- `src/Editor/*`: コア実装（App/Buffer/Render/Markdown/Haskell/Config/Types/Keymap）。
- `test/`: Hspec テスト（テストスイート `hane-tests`）。
- `config/config.sample.toml`: 設定サンプル（必要に応じてコピー）。
- `hane.cabal`, `cabal.project`: Cabal 設定。`dist-newstyle/` は生成物。

## ビルド・実行・テスト
- Cabal: `cabal build` / `cabal run hane` / `cabal test`。
- Make: `make build` / `make run` / `make test` / `make fmt` / `make fmt-check` / `make lint`。
- ICU 無効ビルド: `cabal build -f -icu`（CI でも使用）。
- macOS の ICU 例: `brew install icu4c` 後、`export PKG_CONFIG_PATH="$(brew --prefix)/opt/icu4c/lib/pkgconfig:$PKG_CONFIG_PATH"`。

## コーディング規約・命名
- 方針: Haskell2010、`-Wall` 警告ゼロを維持。インデント 2 スペース、行長 ~100 目安。
- モジュールは `Editor.*` に配置。型/データは UpperCamelCase、関数/変数は lowerCamelCase。
- フォーマット: `fourmolu` 優先、無ければ `ormolu`。例: `fourmolu -i src/**/*.hs`。
- 静的チェック（任意）: `hlint` で改善提案を確認。

## テスト指針
- フレームワーク: Hspec。
- 命名: `test/Spec.hs` を入口に、各モジュールは `FooSpec.hs`。
- 実行: `cabal test`。カバレッジは任意で `cabal test --enable-coverage`（目安 80%+）。
- 単体テストを優先し、UI 部は副作用を分離して検証可能に。

## コミット & PR ガイド
- Conventional Commits を推奨: `feat:`, `fix:`, `refactor:`, `docs:`, `test:`, `chore:`。
- 1 PR = 1 トピック。変更は小さく、説明は具体的に（目的、主要変更、影響範囲、ロールバック方法）。
- PR には関連 Issue、テスト結果、手動確認手順、TUI のスクリーンショット/録画（例: asciinema）を添付。
- CI（`.github/workflows/ci.yml`）が通過すること（format/lint/build/test）。

## セキュリティ & 設定
- 秘密情報や個人データをコミットしない。`.gitignore` を尊重。
- 設定は `config.sample.toml` を基にユーザー環境で上書き管理。
- ICU が見つからない場合は `PKG_CONFIG_PATH` を適切に設定。

## エージェント向け補足
- 自動修正は最小差分を心掛け、既存の構造/命名に従う。
- 破壊的操作（`rm -rf` など）は実施前にレビュー/承認を得る。
