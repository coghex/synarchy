# Project Review Findings: direct commits `84f6fa2`–`a3f4481`

These entries contain focused evidence from the senior review of 12 direct
first-parent commits, newest-first from `84f6fa27` (2026-02-02) through
`a3f4481d` (2026-02-01), checked against
`master@4c2a26d2e707`.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

The batch introduced the first structured logger and the early UI text/box
rendering paths, followed by logger migration and cleanup commits. Three
non-duplicate current concerns remain. Keys PRR-4 and PRR-5 are preserved from
the earlier broad survivor inventory; PRR-6 is the next unused key.

The dead Lua logging registrar introduced by `b54602cc` is already owned by
issue #1083. Strict UTF-8 decoding retained during the logger migration was
subsequently hardened by `05f39d07` and issue #618. The early UI box/text
geometry was subsequently hardened by issues #747/#749; the focused
`boxTileRects` suite passes all three examples. The focused logger suite passes
all ten examples, although it does not cover the three configuration defects
below.

The next direct-history cursor is `ea2c03dd`.

## Status

- [x] PRR-4. `ENGINE_DEBUG` cannot enable every live log category — [#1915]
- [x] PRR-5. Logger timestamp and thread-ID configuration flags are inert — [#1917]
- [x] PRR-6. Category-specific log thresholds read hidden `Cat`-prefixed variables — [#1918]

## 1. Debug-category environment control

### [#1915] PRR-4. `ENGINE_DEBUG` cannot enable every live log category

> **Captured note:** `LogCategory` is enumerable, but both the text parser and
> the special `ENGINE_DEBUG=all` value maintain separate hand-written subsets.
> `World` and `Unit` cannot be selected by name, while `all` also omits
> `Render`.

**Verification:** Verified by exact source enumeration. The type has 24
constructors. `parseCategory` maps 22 and omits `CatWorld` and `CatUnit`; the
`all` branch inserts 21 and additionally omits `CatRender`. Unknown names are
silently dropped, so `ENGINE_DEBUG=World,Unit` produces an empty enabled map and
`ENGINE_DEBUG=all` leaves all three categories disabled.

**Evidence:**

- `src/Engine/Core/Log/Types.hs:34-60` defines the complete bounded category
  type, including `CatRender`, `CatWorld`, and `CatUnit`.
- `src/Engine/Core/Log/Types.hs:62-86` maps `render` but has no `world` or
  `unit` branch.
- `src/Engine/Core/Log/Env.hs:37-55` silently filters parser misses and
  hand-enumerates `all` without any of the three categories.
- `src/Engine/Core/Log/Env.hs:24-35` already demonstrates complete enumeration
  with `[minBound .. maxBound]` for the separate category-level loader.
- Direct commit `84f6fa27` added `CatRender` without adding it to the parser or
  `all` list. A later change repaired only the named form. Subsequent
  `CatWorld` and `CatUnit` additions repeated the drift.
- Searches across open and closed issues and pending findings reports found no
  current owner.

**Handoff context:**

- **Current behavior:** Render debug logs can be enabled explicitly but not
  through `all`. World and Unit debug logs cannot be enabled through
  `ENGINE_DEBUG` at all.
- **Expected behavior:** Every live category has a stable case-insensitive name,
  and `all` enables the complete category set. Adding a constructor either
  derives the corresponding behavior or causes an exhaustive test to fail.
- **Scope and constraints:** Preserve existing category spellings and the
  comma-separated environment contract. Cover named parsing and `all` with a
  GPU-free completeness test. Coordinate with any planned new category, such
  as `CatAudio`, rather than assigning persisted meaning to constructor order.
- **Remaining uncertainty:** Whether unsupported names should remain silently
  ignored is a separate policy decision. Completeness for valid names is fully
  verified.

## 2. Logger output configuration

### [#1917] PRR-5. Logger timestamp and thread-ID configuration flags are inert

> **Captured note:** `LogConfig` publicly exposes `lcShowTimestamp` and
> `lcShowThreadId`, but initialization discards both values. Handle-formatted
> normal logs always show both fields, and thread logs always show the timestamp
> while suppressing the thread id through an unrelated fixed policy.

**Verification:** Verified by data-flow tracing and focused tests. Each field
occurs only in its declaration and default construction. `LoggerState` retains
only `lcShowLocation`; neither formatter receives the other configuration
values. The focused logging suite passes ten examples, but all relevant
formatting cases exercise only the default values.

**Evidence:**

- `src/Engine/Core/Log/Types.hs:119-149` defines `LoggerState`, `LogConfig`, and
  their defaults. State retains `lsShowLocation` but has no timestamp or
  thread-ID policy.
- `src/Engine/Core/Log.hs:56-79` destructures the configuration and stores
  `lcShowLocation`; it never reads `lcShowTimestamp` or `lcShowThreadId`.
- `src/Engine/Core/Log/Format.hs:17-53` always emits the timestamp.
  `NormalFormat` always emits the thread ID, while `ThreadFormat` always
  suppresses it.
- `test-headless/Test/Headless/Core/LogParity.hs:32-80` pins the default normal
  and thread layouts but never constructs a logger with either flag set to
  false.
- Direct commit `b54602cc` introduced both fields already inert:
  `initLogger` retained only `lcShowLocation`, while formatting
  unconditionally emitted timestamp and thread ID.
- Closed issue #944 unified normal and thread formatting while intentionally
  preserving output. It did not claim these configuration fields.
- Tracker and pending-report searches found no current owner.

**Handoff context:**

- **Current behavior:** Changing either public configuration field has no
  effect. Normal handle output always includes both components; thread output
  always includes the timestamp and never includes the thread ID.
- **Expected behavior:** Public configuration accurately controls its named
  output, or unsupported fields are removed so callers cannot request an
  effect the logger discards.
- **Scope and constraints:** Preserve callback backends' structured `LogEntry`,
  source-location behavior, the normal-versus-thread policy from #944, field
  ordering, and default output. Cover true and false configurations without
  booting the engine.
- **Remaining uncertainty:** No current non-default caller was found, so
  removing the unsupported settings may be preferable to implementing them.
  Retaining inert public fields is the verified problem.

## 3. Category-specific threshold environment control

### [#1918] PRR-6. Category-specific log thresholds read hidden `Cat`-prefixed variables

> **Captured note:** The logger describes category-specific thresholds using
> names such as `ENGINE_LOG_VULKAN`, but derives the actual key from
> `show CatVulkan`. It therefore reads `ENGINE_LOG_CATVULKAN` instead and
> silently ignores the advertised spelling.

**Verification:** Verified with a focused GHCi reproduction against current
master. With `ENGINE_LOG_VULKAN=error`,
`loadCategoryLevelsFromEnv Map.empty` returned no entry for `CatVulkan`.
After replacing it with `ENGINE_LOG_CATVULKAN=error`, the same lookup returned
`Just LevelError`.

**Evidence:**

- `src/Engine/Core/Log/Env.hs:24-35` describes the contract as
  `ENGINE_LOG_<CATEGORY>=<level>`, but constructs the variable with
  `map toUpper (show cat)`. Since `show CatVulkan` is `CatVulkan`, the actual
  key is `ENGINE_LOG_CATVULKAN`.
- `src/Engine/Core/Log/Types.hs:62-86` defines the public category spellings
  without the internal `Cat` constructor prefix.
- `src/Engine/Core/Log/Format.hs:61-62` likewise removes the first three
  characters before displaying a category.
- `src/Engine/Core/Log.hs:56-63` invokes this loader during every logger
  initialization, so the mismatch reaches normal engine startup.
- Direct commit `b54602cc` introduced the defect. Its initialization comment
  explicitly used `ENGINE_LOG_VULKAN=debug` as the example while the adjacent
  implementation generated `ENGINE_LOG_CATVULKAN`.
- Repository search found no test of `loadCategoryLevelsFromEnv` or any
  documented `ENGINE_LOG_CAT*` spelling. Searches across open and closed issues
  and pending reports found no current owner.

**Handoff context:**

- **Current behavior:** Following the stated `ENGINE_LOG_VULKAN=error`
  convention leaves Vulkan at the global threshold, so informational and
  warning messages remain enabled. Only a variable exposing the internal
  constructor prefix changes the threshold.
- **Expected behavior:** Category-specific threshold variables use the same
  stable unprefixed category names as `ENGINE_DEBUG` and formatted log output.
  The documented spelling is covered for every category.
- **Scope and constraints:** Preserve case-insensitive level parsing and the
  existing separation between ordinary level thresholds and debug-category
  enablement. Tests must isolate and restore process environment variables.
- **Remaining uncertainty:** The per-category threshold surface is not
  advertised prominently outside code comments. If it is no longer intended
  to be supported, removing the misleading loader and contract is an
  alternative; the current half-working interface is the verified defect.
