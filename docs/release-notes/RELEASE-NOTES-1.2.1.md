# ICL 1.2.1 Release Notes

## Bug Fixes

### Slynk Serialization
- Fixed crash when evaluating expressions that return unreadable objects
  - Previously, evaluating `*package*` would cause a reader error: `illegal sharp macro character: #\<`
  - Now correctly displays unreadable objects like `#<PACKAGE "COMMON-LISP-USER">`
- Results are now converted to string representations on the backend to avoid serialization issues

### Output Display
- Improved colorization of result values based on content patterns
- Unreadable objects (e.g., `#<...>`) are displayed in dim color
- Numbers, keywords, strings, T, and NIL retain appropriate syntax highlighting

## Download

Pre-built binaries are available for:
- Windows (x64)
- macOS (ARM64 and x64)
- Linux (RPM and DEB packages)
