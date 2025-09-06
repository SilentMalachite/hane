# Contributing to Hane

ありがとうございます！Hane への貢献方法をまとめます。

## 開発環境
- GHC 9.6 系（CI は 9.6.5/9.6.7）
- Cabal 3.10 以降
- fourmolu / hlint

## セットアップ
```
make setup     # 依存更新とサンプル設定の作成
make build     # ビルド
make test      # テスト
```

## コーディング規約
- Haskell2010、`-Wall` 警告ゼロ
- フォーマット: `make fmt`（fourmolu）
- Lint: `make lint`（hlint）
- 命名: 型/データ UpperCamelCase、関数/変数 lowerCamelCase

## テスト
- フレームワーク: Hspec
- 実行: `make test`（必要なら `make test-coverage`）
- UI ロジックは副作用を分離してテスト可能に

## コミット/PR
- Conventional Commits 推奨: `feat:`, `fix:`, `refactor:`, `docs:`, `test:`, `chore:`
- 1 PR = 1 トピック。変更は小さく、テストと説明を付与
- PR テンプレートに沿って、目的/影響範囲/テスト結果を記述

## ブランチ
- `main` は保護。PR 経由でマージ
- 名前例: `feature/xxx`, `fix/yyy`, `refactor/zzz`

## 相談
Issue/Discussion を歓迎します。バグは再現手順を、提案は背景と代替案を添えてください。

