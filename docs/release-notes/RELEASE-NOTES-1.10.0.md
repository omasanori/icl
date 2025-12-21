# ICL 1.10.0 Release Notes

## Highlights

**ICL is now fully self-contained.** The binary includes all dependencies and can be copied anywhere - no external files needed.

## New Features

### Embedded Slynk

Slynk sources are now embedded in the ICL binary and automatically extracted on first run:

- **Automatic Extraction**: Slynk is extracted to `~/.local/share/icl/slynk-VERSION/` on first run
- **Versioned Directories**: Each ICL version extracts its own slynk version, enabling clean upgrades
- **No External Dependencies**: Works when the binary is moved to any location
- **Pure Lisp Implementation**: Uses the `zip` library for both compression and extraction

### Embedded Browser Assets

Web assets (JavaScript, CSS) for the browser interface are now embedded:

- **Served from Memory**: Assets are loaded at compile time and served directly from memory
- **No Filesystem Extraction**: Unlike slynk, assets don't need to be written to disk
- **Includes**: dockview.min.js, xterm.min.js, xterm-addon-fit.min.js, and their CSS files

### XDG Base Directory Support

Added proper XDG Base Directory Specification support:

- **Data Directory**: `~/.local/share/icl/` (XDG_DATA_HOME) for extracted resources
- **State Directory**: `~/.local/state/icl/` (XDG_STATE_HOME) for history and logs
- **Config Directory**: `~/.config/icl/` (XDG_CONFIG_HOME) for user configuration
- **Windows Support**: Falls back to `%LOCALAPPDATA%` on Windows

## Improvements

- Simplified installer packages (RPM, DEB) - no longer need to install sly or assets directories
- Removed Quicklisp fallback - embedded slynk is always available
- Added WEB-LICENSES file to all installer packages for JS/CSS license compliance
- Windows compatibility for zip extraction with proper path separator handling

## Bug Fixes

- Fixed Makefile default target to properly build `icl` binary

## Breaking Changes

None. The binary is fully backwards compatible.

## Technical Details

- Binary size: ~17MB (compressed, includes ~350KB slynk.zip and ~576KB assets)
- Slynk version: Automatically tracked from ocicl package version
- Build requirement: `zip` and `flexi-streams` libraries (already in ocicl)
