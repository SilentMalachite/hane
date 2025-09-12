# Changelog

本プロジェクトは Keep a Changelog に準拠し、Semantic Versioning を採用します。

## [Unreleased]
### Added
- 宣言的キーマップ（Vim Normal/Insert、Emacs C-x シーケンス）
- 設定パーサの `[editor]`/snake_case 対応
- CI を GHC 行列化（9.6.5 / 9.6.7）
 - ライブラリを `-Wall -Werror` で厳格ビルド（CI）
 - ファイル操作エラーのユーザーフレンドリ表示（原因と対処のヒント）

### Changed
- 依存の上限制約を緩和しつつ最新 GHC との整合を改善（`brick <3`, `ansi-terminal <2` など）
- Makefile の `run-no-icu` 修正

### Fixed
- `validateFilePath` が `/`・`.`・`..` を許容し得た問題を修正
- `[editor]` テーブルの `format_on_save` が反映されないことがある問題を修正
- FilePath の移植性向上（自前実装撤去）
