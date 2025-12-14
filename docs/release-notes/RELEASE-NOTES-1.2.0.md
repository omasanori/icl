# ICL 1.2.0 Release Notes

## New Features

### Cross-Platform Support
- **Windows support**: ICL now builds and runs on Windows
  - Native Windows Console API for terminal handling
  - Windows binaries included in releases (`icl-VERSION-windows-amd64.zip`)
- **macOS support**: Pre-built binaries for macOS
  - Apple Silicon (ARM64): `icl-VERSION-macos-arm64.tar.gz`
  - Intel (x64): `icl-VERSION-macos-x64.tar.gz`

### Release Packages
- All binary packages now include:
  - `README.md` - Documentation
  - `LICENSE` - ICL MIT license
  - `THIRD-PARTY-LICENSES.txt` - Collected licenses from all dependencies

## Improvements

### Build System
- Removed linedit dependency (ICL uses its own built-in editor)
- Platform-specific terminal handling via conditional compilation
- Cross-platform CI testing on Linux, macOS, and Windows

### Code Quality
- Fixed format string compatibility for Windows CRLF line endings
- Made osicat dependency conditional (POSIX-only)

## Breaking Changes

None.

## Download

Pre-built binaries are available for:
- Windows (x64)
- macOS (ARM64 and x64)
- Linux (RPM and DEB packages)
