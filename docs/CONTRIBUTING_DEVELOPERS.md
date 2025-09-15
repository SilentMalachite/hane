# 開発者向け貢献ガイド

## 開発環境セットアップ

### 必要なツール
- GHC 9.6.x（推奨）または 9.12.x
- Cabal 3.10 以降
- Git

### 初回セットアップ
```bash
git clone https://github.com/SilentMalachite/hane.git
cd hane

# 依存関係のインストールと設定
make setup

# 開発ツールのインストール
make tools

# ビルドとテスト
make build
make test
```

## 開発ワークフロー

### 1. ブランチ作成
```bash
git checkout -b feature/your-feature-name
# または
git checkout -b fix/bug-description
```

### 2. 開発
```bash
# コードの変更後
make build    # ビルド確認
make test     # テスト実行
make fmt      # コードフォーマット
make lint     # リンターチェック
```

### 3. 品質チェック
```bash
# CI と同じ厳格チェック
cabal build lib:hane -f -icu --ghc-options "-Wall -Werror"

# 全てのテストを実行
cabal test -f -icu all
```

### 4. プルリクエスト
- 変更内容を `CHANGELOG.md` の `[Unreleased]` セクションに追記
- コミットメッセージは Conventional Commits 形式を推奨
- プルリクエストテンプレートに従って説明を記入

## コード品質基準

### Haskell コーディング規約
- `-Wall` 警告ゼロを維持
- `fourmolu` でフォーマット（`.fourmolu.yaml` 設定）
- `hlint` 推奨事項に従う（`.hlint.yaml` 設定）
- 型シグネチャを明示的に記述

### テスト要件
- 新機能には対応するテストを追加
- 既存テストの破綻がないことを確認
- エッジケースを考慮したテストケース

### ドキュメント更新
- パブリック API の変更時は対応ドキュメントを更新
- 新機能は `docs/` 内の関連ファイルに追記
- ユーザー向け変更は `README.md` も更新

## デバッグとトラブルシューティング

### よくある問題

#### ICU 関連のビルドエラー
```bash
# ICU なしでビルド（デフォルト）
cabal build -f -icu

# ICU ありでビルド（システムに ICU が必要）
cabal build -f +icu
```

#### 開発ツールが見つからない
```bash
# ツールの再インストール
cabal install fourmolu hlint --overwrite-policy=always

# PATHの確認
echo $PATH | grep cabal
```

#### キャッシュ関連の問題
```bash
# Cabal キャッシュのクリア
cabal clean
rm -rf dist-newstyle
```

## リリースプロセス（メンテナー向け）

### バージョンアップ手順
1. `CHANGELOG.md` の `[Unreleased]` を新バージョンに変更
2. `hane.cabal` の `version:` を更新
3. コミット: `git commit -m "Release vX.Y.Z"`
4. タグ作成: `git tag -a vX.Y.Z -m "vX.Y.Z"`
5. プッシュ: `git push origin main && git push origin vX.Y.Z`
6. GitHub Actions が自動でリリースビルドを実行
7. GitHub Release ページで説明を追加

### 緊急修正（ホットフィックス）
1. `main` からホットフィックスブランチを作成
2. 修正を適用
3. パッチバージョンを上げてリリース
4. `main` にもマージ

## 追加リソース

- [Haskell Style Guide](https://kowainik.github.io/posts/2019-02-06-style-guide)
- [Keep a Changelog](https://keepachangelog.com/)
- [Semantic Versioning](https://semver.org/)
- [Conventional Commits](https://www.conventionalcommits.org/)
