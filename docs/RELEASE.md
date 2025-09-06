# Release Guide

1. 変更を確認
   - `CHANGELOG.md` を更新
   - 動作確認: `make build && make test`
2. バージョン更新
   - `hane.cabal` の `version:` を更新（Semantic Versioning）
3. タグ作成
   - `git tag -a vX.Y.Z -m "vX.Y.Z"`
   - `git push origin vX.Y.Z`
4. GitHub Release 作成（必要ならバイナリ添付）
   - 説明文は `.github/RELEASE_TEMPLATE.md` を雛形として使用
