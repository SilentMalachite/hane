# Development

## 要件
- GHC 9.6.x
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
- `fourmolu` と `hlint` のチェック、ICU 無効ビルド/テスト

## スタイル
- `.fourmolu.yaml` / `.hlint.yaml` を参照

