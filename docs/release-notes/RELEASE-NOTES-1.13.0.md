# ICL 1.13.0 Release Notes

## New Features

### Extended Visualization Types for `,viz`

The `,viz` command now supports additional data types:

#### JSON Strings
- Strings starting with `{` or `[` are recognized as JSON
- Pretty-printed with proper indentation
- Syntax highlighting via highlight.js with automatic language detection
- Theme adapts to light/dark mode

```lisp
(defvar *json* "{\"name\": \"test\", \"values\": [1, 2, 3]}")
,viz *json*
```

#### Image Byte Arrays
- Byte vectors containing image data are automatically detected by magic bytes
- Supported formats: PNG, JPEG, GIF, WebP
- Images are base64-encoded and sent directly over WebSocket as data URLs
- No server-side file storage required

```lisp
(defvar *img* (alexandria:read-file-into-byte-vector #P"photo.png"))
,viz *img*
```

#### SVG Strings
- Strings starting with `<svg` or `<?xml` are rendered as inline SVG graphics

#### HTML Strings
- Strings starting with `<!DOCTYPE` or `<html` are rendered in a sandboxed iframe

### Unified Visualization Refresh

All visualization panels now use a unified refresh system:
- Panels automatically update when REPL evaluations complete
- Type changes are handled seamlessly (e.g., if a variable changes from JSON to hash-table, the panel updates accordingly)
- Refreshes are limited to real REPL activity (from ICL or Emacs/SLY/SLIME), not internal queries

### Emacs Integration

ICL now integrates with Emacs via `icl.el`:
- Start ICL browser with `M-x icl` when you have an active SLY or SLIME connection
- Visualization panels automatically refresh when you evaluate code in SLY/SLIME REPL
- Supports both SLY (`slynk-mrepl`) and SLIME (`swank`) backends
- Auto-stops ICL when the Lisp connection closes (configurable via `icl-auto-stop-on-disconnect`)

```elisp
;; Add to your Emacs config
(load "/path/to/icl/icl.el")

;; Start ICL browser (requires active SLY/SLIME connection)
M-x icl

;; Stop ICL browser
M-x icl-stop
```

## Technical Changes

### ICL Runtime Package

A new `icl-runtime` package is automatically injected into the inferior Lisp on connection. This package provides utility functions needed by ICL's visualization features:
- Base64 encoding for image data (avoids external dependencies)
- Eval generation tracking via wrappers around `slynk-mrepl:mrepl-eval` (SLY) or `swank:listener-eval` (SLIME) to detect real REPL activity

## Breaking Changes

None.
