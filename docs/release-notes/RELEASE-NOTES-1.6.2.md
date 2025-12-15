# ICL 1.6.2 Release Notes

## Bug Fixes

### Fixed loading systems with package-qualified features
- Systems like `chipz` that add package-qualified features (e.g., `chipz-system:gray-streams`) to `*features*` can now be loaded without errors
- Previously, Slynk's `:new-features` event would cause a reader error because those packages don't exist in ICL's host process
- The fix auto-creates temporary packages/symbols when reading Slynk messages, then cleans up afterwards

### Suppressed raw debug output
- Raw `:debug` events from Slynk are no longer printed to the terminal
- Errors are handled through ICL's normal error handling instead

### Improved error handling for package changes
- `,cd` to a non-existent package now shows a clean error message instead of raw debugger output

## Breaking Changes

None.
