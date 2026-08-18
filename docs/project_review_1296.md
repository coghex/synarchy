# Project Review Findings: PRs #1296–#1271

This entry records focused evidence from the senior review of merged PRs #1296
through #1271, including the direct first-parent audio-design commit in that
landing interval, for later one-at-a-time disposition. The separately reported
`transfer_order_probe.py` concern was deliberately excluded at the user's
request because its tracker issues were already being drafted.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] PRR-1. Responsive menu tests rewrite the developer's saved keybindings — [#1357]

## 1. Test isolation

### [#1357] PRR-1. Responsive menu tests rewrite the developer's saved keybindings

> **Captured note:** Isolate the `UI.ResponsiveMenus` Settings Defaults cases
> from the developer's real keybinding configuration. PR #1296 says the suite
> cannot modify any `config/*.local.yaml`, but two cases call the production
> Defaults path, which resets and saves keybindings to
> `config/keybinds.local.yaml`.

**Verification:** Verified — a focused `UI.ResponsiveMenus` run logged two
successful keybinding saves and changed the modification time and serialized
contents of the ignored developer file `config/keybinds.local.yaml`.

**Evidence:**

- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:59` — the suite's fixture
  comment explicitly claims that running these specs cannot modify, truncate,
  or regenerate any developer `config/*.local.yaml` file.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:1442` — the scale-change
  fan-out case initializes the real Settings module and calls
  `settingsMenu.onDefaults()`.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:1474` — the unchanged-scale
  case independently calls the same production Defaults path.
- `scripts/settings_menu.lua:155` — `settingsMenu.onDefaults` resets the Settings
  state, then calls `engine.loadDefaultKeybinds()` and
  `engine.saveKeybinds()` before rebuilding the UI.
- `src/Engine/Scripting/Lua/API/Keybinds.hs:216` — the registered save function
  documents and performs persistence to `config/keybinds.local.yaml`.
- Reproduction: `cabal test synarchy-test-headless
  --test-options='--match "UI.ResponsiveMenus"'` reported 90 passing examples
  but printed `Saved keybindings to config/keybinds.local.yaml (10 actions)`
  twice. The file's mtime became `2026-08-16 08:23:23 -0700`; its emitted YAML
  was semantically the factory bindings but bytewise different from the tracked
  default template.

**Handoff context:**

- **Current behavior:** A focused headless UI test run silently replaces the
  developer's saved bindings with factory bindings even though every assertion
  passes. The review did not attempt to restore the file because its previous
  ignored contents were unknowable.
- **Expected behavior:** `UI.ResponsiveMenus` leaves every real developer
  `config/*.local.yaml` byte-identical, including when a pre-existing
  keybinding overlay is non-default; when no local keybinding file exists, the
  suite leaves it absent. The production Settings Defaults action must still
  reset and persist keybindings.
- **Scope and constraints:** Surfaced while reviewing PR #1296 / issue #1266.
  Preserve that change's deterministic 1.0 UI-scale baseline, its explicit
  scale overrides, and both Defaults resize-fan-out assertions. Isolate or
  intercept persistence only within the test boundary; do not weaken the
  player-facing write-through keybinding contract.
- **Verification target:** Run `cabal test synarchy-test-headless
  --test-options='--match "UI.ResponsiveMenus"'` with a test-owned non-default
  keybinding overlay and with the overlay absent, and prove the corresponding
  pre-run state is byte-for-byte unchanged afterward. Retain focused Settings
  or keybinding coverage showing that production Defaults still persists
  factory bindings.
- **Deduplication:** Searches across open and closed issues for ResponsiveMenus,
  headless-test config writes, and local keybinding rewrites found no existing
  tracker item. Closed #638 and #786 define the local-overlay contract but do
  not track this test side effect.
- **Remaining uncertainty:** None at capture time; the write was observed
  directly during the focused suite.
