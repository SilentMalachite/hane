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
- `Editor.App`: Main application entry point (run function)
- `Editor.App.Types`: Shared types (Name, AppMode, St)
- `Editor.App.UI`: UI rendering (drawUI, drawMainUI, drawBufferWithCursor, drawFileDialog)
- `Editor.App.State`: State management (buffer operations, cursor movement, message formatting)
- `Editor.App.Event`: Event handling (handleEvent, handleMainEvent, handleFileDialogEvent, saveCurrentFile)
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
- **Fixed terminal compatibility issue**: Added TTY check and command-line argument handling to prevent crashes when running outside proper terminal environment
- Added --help and --version command-line flags with proper usage information
- **Fixed GitHub CI configuration**: Updated GHC versions, improved tool installation with proper PATH handling across platforms, made linting steps resilient with continue-on-error
- Fixed Paths_hane module warnings by adding proper autogen-modules configuration
- All tests now pass successfully
- **Fixed Save As path join bug**: Changed from string concatenation to proper FilePath operations (</>) for cross-platform compatibility
- **Improved formatOnSave UX**: Now notifies user when formatter fails and saves unformatted file
- **Updated documentation**: KEYMAPS.md now accurately reflects implemented features (removed unimplemented undo/redo)
- **Created KNOWN_ISSUES.md**: Documents current limitations and planned features

## Known Issues and Limitations
- See `docs/KNOWN_ISSUES.md` for complete list
- Undo/redo not yet implemented

## Refactoring Completed
- ✅ App.hs successfully refactored into modular structure (REFACTOR_PLAN.md completed)
- Editor.App split into: Types (31 lines), UI (115 lines), State (90 lines), Event (193 lines), Core (68 lines)
- Benefits: Better separation of concerns, easier testing, improved maintainability
- All existing tests pass without modification

## CI/CD Configuration
- Tests all GHC versions from `tested-with` field (9.6.5, 9.6.7, 9.8.2, 9.10.1, 9.12.2)
- Builds with `-Wall -Werror` for all components to catch warnings early
- Cross-platform testing on Ubuntu, macOS, and Windows
- ICU support tested on Linux and macOS (Windows ICU build is complex, documented limitation)
- Efficient caching strategy includes GHC version, cabal version, and dependency hash