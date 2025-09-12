# Development

## 要件
- GHC 9.6.x（CI 対象）/ 9.12 系（ローカル検証済み）
- Cabal 3.10+

## よく使うコマンド
```
make setup
make build
make run
make test
make fmt fmt-check
make lint
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
