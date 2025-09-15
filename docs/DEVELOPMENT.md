# Development

## 要件
- GHC 9.6.x（CI 対象）/ 9.12 系（ローカル検証済み）
- Cabal 3.10+

## よく使うコマンド
```bash
make setup      # 初期セットアップ
make tools      # 開発ツールインストール
make build      # ビルド
make run        # 実行
make test       # テスト
make fmt        # コードフォーマット
make fmt-check  # フォーマットチェック
make lint       # リンター実行
make ci         # CI チェック
```

## CI
- GitHub Actions: GHC 9.6.5 / 9.6.7 の行列
- ライブラリは `-Wall -Werror` で厳格ビルド
- `fourmolu` と `hlint` のチェック、ICU 無効ビルド/テスト

### 手元で厳格ビルドを再現
```
cabal build lib:hane -f -icu --ghc-options "-Wall -Werror"
```

## スタイル
- `.fourmolu.yaml` / `.hlint.yaml` を参照
