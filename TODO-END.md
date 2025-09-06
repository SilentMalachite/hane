# TODO-END（進捗サマリ）

作業日: 2025-09-06

## 完了したこと
- ドキュメント
  - `AGENTS.md`（Repository Guidelines）を新規作成。構成/ビルド・テスト/スタイル/テスト指針/コミット&PR/セキュリティ/エージェント補足を記載。
  - `AGENTS.md` に Make/CI 情報を追記。
- 開発体験まわり
  - フォーマッタ設定: `.fourmolu.yaml`（2スペース、インポート整形等）。
  - リンタ設定: `.hlint.yaml`（基本ルールと HTML レポート設定）。
  - `Makefile`: `build/run/test/fmt/fmt-check/lint/ci/clean` を整備。
  - CI: `.github/workflows/ci.yml`（format→lint→build→test、GHC 9.6.5/Cabal 3.10.3.0、`-f -icu`）。
- ソース修正
  - `src/Editor/Haskell.hs`:
    - `quickCheck`: `ghc -fno-code` で型チェック（無ければ `ghci`、無ければスキップ）。
    - `startRepl`: `cabal repl` 優先、次点 `ghci` を継承入出力で起動。
  - `src/Editor/Config.hs`:
    - `loadConfig`: 設定探索（`HANE_CONFIG` → `$XDG_CONFIG_HOME/hane/config.toml` → `$HOME/.config/hane/config.toml` → `config/config.sample.toml`）。
    - `parseConfigText`: 依存追加なしの簡易 TOML 風パーサ（`keymap/theme/formatOnSave/locale`）。
  - `src/Editor/Keymap.hs`: 最小の `KeyAction/KeymapSpec` と `defaultVim/defaultEmacs` を追加。

## 備考/次の候補
- `Editor.App` に `loadConfig`/`KeymapSpec` の反映（UI トグルや formatOnSave）。
- テストの分割（`test/Editor/*.hs`）とカバレッジ有効化。
- pre-commit フックで `fourmolu`/`hlint` を自動化。

