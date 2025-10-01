# Changelog

本プロジェクトは Keep a Changelog に準拠し、Semantic Versioning を採用します。

## [Unreleased]

## [0.1.1.0] - 2025-01-08
### Fixed
- **Critical**: Save As path join bug using proper FilePath operations (</>) for cross-platform compatibility
- ICU dependency build failures by changing default flag to False
- File path concatenation bugs throughout codebase
- Invalid GHC options in cabal configuration
- Cabal package warnings (moved CHANGELOG.md to extra-doc-files)
- Formatter failure now notifies user instead of silently saving unformatted
- Documentation errors: removed unimplemented undo/redo features from KEYMAPS.md

### Added
- **Refactored App.hs** into modular structure (437 → 497 lines across 5 modules):
  * `Editor.App.Types`: Shared types (Name, AppMode, St)
  * `Editor.App.UI`: UI rendering (115 lines)
  * `Editor.App.State`: State management (90 lines)
  * `Editor.App.Event`: Event handling (193 lines)
  * `Editor.App`: Main entry point (68 lines)
- `docs/KNOWN_ISSUES.md`: Comprehensive documentation of limitations
- `docs/CI_IMPROVEMENTS.md`: CI configuration improvements
- `REFACTOR_SUMMARY.md`: Detailed refactoring documentation
- CI support for GHC 9.8.2 and 9.10.1
- macOS ICU support in CI workflow
- Improved error messages with emoji icons (⚠️ ✅ ℹ️)

### Changed
- CI now tests all GHC versions from `tested-with` field
- CI builds with `-Wall -Werror` for all components (not just library)
- Improved CI cache strategy (includes cabal version)
- Better test output with `--test-show-details=streaming`
- Enhanced configuration file documentation
- Module organization following single responsibility principle

### Improved
- Code maintainability through modular architecture
- Testing capabilities (modules can be tested independently)
- Documentation accuracy (removed false claims)
- Cross-platform compatibility (proper path handling)

## [0.1.0.0] - 2025-09-12
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
