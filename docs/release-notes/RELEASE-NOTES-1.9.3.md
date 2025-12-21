# ICL 1.9.3 Release Notes

## Bug Fixes

### Browser Inspector
- Fix macro inspection in browser. Macros now use `(macro-function 'sym)` instead of `#'sym` which caused "X is a macro, not a function" errors.
- Fix case sensitivity for mixed-case symbol names (e.g., Java method signatures like `java/lang/Thread`). Symbol names with lowercase letters are now properly escaped with `|...|` to preserve case through the Lisp reader.

## Breaking Changes

None.
