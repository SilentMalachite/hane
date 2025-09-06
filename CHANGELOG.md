# Changelog

本プロジェクトは Keep a Changelog に準拠し、Semantic Versioning を採用します。

## [Unreleased]
### Added
- 宣言的キーマップ（Vim Normal/Insert、Emacs C-x シーケンス）
- 設定パーサの `[editor]`/snake_case 対応
- CI を GHC 行列化（9.6.5 / 9.6.7）

### Changed
- 依存の上限制約を追加し再現性を向上
- Makefile の `run-no-icu` 修正

### Fixed
- FilePath の移植性向上（自前実装撤去）

