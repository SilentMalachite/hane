# Hane - Haskell TUI Editor Knowledge

## Project Overview
Hane is a terminal-based text editor optimized for Markdown and Haskell, with special focus on Japanese text support (full-width characters, emojis, grapheme clusters).

## Key Features
- TUI editor with Vim/Emacs keybindings
- Markdown and Haskell syntax support
- Japanese text handling (ICU-based grapheme boundaries)
- File operations with clear Japanese error messages
- Auto-formatting with fourmolu/ormolu

## Build System
- **Primary**: Cabal-based build system
- **Alternative**: Makefile wrapper for common tasks
- **CI**: GitHub Actions with `-Wall -Werror` strict builds
- **Testing**: HSpec test suite

## Development Workflow
- Use `make` commands for development tasks
- Format code with `fourmolu` (falls back to `ormolu`)
- Lint with `hlint`
- Test with `cabal test` or `make test`

## ICU Support
- Optional ICU flag for accurate grapheme boundaries
- Fallback implementation without ICU
- Platform-specific ICU installation required

## Configuration
- TOML-based config in `~/.config/hane/config.toml`
- Environment variable `HANE_CONFIG` for custom path
- Support for themes, keymaps, and locale settings

## Module Structure
- `Editor.App`: Main application logic
- `Editor.Buffer`: Text buffer management
- `Editor.Render`: Display rendering
- `Editor.Markdown`/`Editor.Haskell`: Language-specific features
- `Editor.Config`: Configuration handling
- `Editor.File`: File operations
- `Editor.Keymap`: Key binding system

## Development Commands
- `make setup`: Initial project setup
- `make build`: Build the project
- `make test`: Run tests
- `make tools`: Install development tools (fourmolu, hlint)
- `make fmt`: Format code (requires fourmolu)
- `make lint`: Run linter (requires hlint)
- `make ci`: Run CI checks

## Recent Fixes Applied
- Fixed ICU dependency build failures by changing default flag to False
- Fixed file path concatenation bug using proper FilePath operations
- Removed invalid GHC options from library configuration
- Added Makefile guards for missing development tools
- Improved setup documentation
- All tests now pass successfully