# Code health findings

A running audit of the source tree for stale/incorrect comments, dead code,
oversized modules, misplaced functions, and poor names. Each entry is scoped to
be filed as its own issue. Working order: engine core → engine subsystems →
world → gameplay → Lua → tools → docs.

Status legend: `[ ]` not filed · `[#N]` filed as issue N

> **Methodology note (corrected 2026-07-25).** The "unreferenced export" scans
> in batches 2-11 originally searched `src/`, `app/`, and `test/` only, and
> **omitted `test-headless/` — 124 files, the project's main test suite**. All
> counts below have been corrected, and every individual dead-code claim was
> re-verified against the full corpus. Three findings were wrong and have been
> fixed in place (CH-79, CH-109, and the `ghostTint` case dropped from
> batch 12). Corrected totals:
>
> | Area | First reported | Actual |
> |---|---:|---:|
> | `src/Engine` | 115 | **97** |
> | `src/World` | 230 | **194** |
> | `src/Unit` + `src/Combat` | 25 | **17** |
> | `World/Thread` + `Render` + `ZoomMap` | 14 | **11** |
> | `Sim`/`Power`/`Infection`/`Craft` | 6 | **0** |
> | `Building`/`Structure`/`Location`/`LootTable` | 7 | **3** |
>
> Anything claiming a function is unused has been checked against
> `src/ app/ test/ test-headless/` together.

---

## Batch 1 — `src/Engine/Core/` (swept 2026-07-25)

### [#931] CH-1. `EngineM`'s `ε` type parameter is dead weight on ~295 signatures
`Engine.Core.Monad` declares `newtype EngineM ε σ α` but the body hard-wires
`EngineEnv`:

```haskell
newtype EngineM ε σ α = EngineM
  { unEngineM ∷ EngineEnv → (Either EngineException α → IO σ) → IO σ }
```

`ε` appears in no field, no instance head that constrains it, and no call site
ever instantiates it to anything but a bound variable (the only concrete
mention anywhere is `EngineM' EngineEnv`, 5 places). CLAUDE.md already states
the design decision — "`EngineM` stays hard-wired to `MonadReader EngineEnv`
(no capability typeclass layer)" — so the parameter is not reserved for a
planned feature either.

Cost: every one of ~295 `EngineM ε σ α` / `EngineM' ε α` signatures in the
codebase carries a meaningless type variable, and the module's own haddock
documents it as "ε = environment tag", which is not true.

Fix: drop `ε`, or (if it must stay) document it as vestigial. Mechanical but
wide; good candidate for a single sweeping PR.

### [#932] CH-2. `EngineConfig` carries four fields that nothing reads
`windowWidth`, `windowHeight`, `enableVSync`, `enableDebug` have **zero** read
sites in `src/`, `app/`, or `test/`. They are set once in
`Engine.Core.Defaults.defaultEngineConfig` (800 / 600 / True / CPP-gated) and
never overwritten — `initializeEngineWith` assigns `engineConfig =
defaultEngineConfig` verbatim while the *real* window size comes from
`VideoConfig`/`windowSizeRef`.

This is an active trap: `windowWidth (engineConfig env)` reads as the window
width and always returns 800. `enableDebug` additionally reads as a live dev
toggle when it is a compile-time constant.

Fix: delete all four (and the `#ifdef DEVELOPMENT` block that feeds
`enableDebug`).

### [#933] CH-3. Vulkan reports the application name as "Vulkan Device Test"
`Engine.Core.Defaults.defaultGraphicsConfig` sets
`gcAppName = "Vulkan Device Test"`, and `Engine.Graphics.Vulkan.Instance`
passes it straight to `VkApplicationInfo.applicationName`. The shipped game
identifies itself to the Vulkan driver (and to any driver-side profile,
capture tool, or bug report) as a test scaffold. `defaultWindowConfig` in the
same file correctly uses `"Synarchy"`.

Also here: `gcWidth`/`gcHeight` are a second hardcoded 800x600 that nothing
reconciles with `EngineConfig`'s (see CH-2) or `VideoConfig`'s.

### [#934] CH-4. `EngineEnv.inputThreadActiveRef` carries `gameTimeRef`'s haddock
`Engine/Core/State.hs:308-322`. Two `-- ^` blocks are stacked on
`inputThreadActiveRef`; haddock concatenates them, so the rendered docs for
"has the input thread started" end with:

> Monotonic game-clock in seconds. Advances by real-tick dt only when
> `enginePausedRef` is False. […] Updated by Unit.Thread.unitLoop once per tick.

That paragraph belongs to `gameTimeRef` (declared four lines earlier at
`:304`), which is left completely undocumented. A pure doc-motion fix.

### CH-5. Two record fields share one source line in `GraphicsState`
`Engine/Core/State.hs:456`:

```haskell
  , msaaColorImage     ∷ Maybe (Vk.Image, Vk.DeviceMemory, Vk.ImageView)  , vertexBuffer       ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
```

`vertexBuffer` is invisible when skimming the record and unreachable by a
line-oriented grep for its declaration.

### CH-6. Three of four `LogBackend` constructors are never constructed
`LogToFile`, `LogToCallback`, and `LogMulti` have no construction site
anywhere in `src/`, `app/`, or `test/` — only `LogToHandle` is ever used. They
carry live handling code in `writeLogEntry`, `writeThreadLogEntry`, and
`shutdownLogger`.

That dead code is also **wrong**: `writeThreadLogEntry`'s `LogMulti` branch
recurses into `writeLogEntry`, so a thread-log entry fanned out to multiple
backends would be formatted with the non-thread formatter. Unreachable today,
but it is the kind of defect that ships the moment someone adopts `LogMulti`.

Fix: delete the three unused constructors (preferred), or fix the `LogMulti`
branch and add a test.

### CH-7. Large dead surface in `Engine.Core.Log` / `Engine.Core.Log.Monad`
Exported, documented, zero call sites: `traceLog`, `logException`,
`getEnabledCategories`, `setCategoryLevel`, `logDebugS`, `logWarnS`,
`withTiming`, `withTimingFor`, `logAndThrowFor`.

`withTiming`/`withTimingFor`/`logAndThrowFor` are notable — they are #889
capability-migration primitives that no consumer was ever narrowed onto.

### CH-8. `logMessage` and `logThreadMessage` are duplicated verbatim
`Engine/Core/Log.hs:169-223`. The two functions are identical across 27 lines
except for the final `writeLogEntry` vs `writeThreadLogEntry` call. Same
duplication repeats one layer down in `Log/Format.hs` (`formatLogEntry` /
`formatThreadLogEntry`) and again across the eight
`logDebug`/`logThreadDebug`/… wrappers.

Fix: parameterise on the writer.

### CH-9. `extractCallSite`'s skip-list is an untested, order-sensitive trap
`Engine/Core/Log.hs:134-167`. Source-location attribution depends on a
hand-maintained list of *function name strings*; a rename or a new wrapper
silently misattributes every log line, and nothing tests it.

Worse, the list is internally inconsistent in a way that makes the obvious
"fix" a regression: `logMessage` is listed but `logThreadMessage` is not, and
`logThreadDebug`/`Info`/`Warn`/`Error` are absent. They happen to work today
only because they are the outermost frame (`dropWhile` stops immediately and
returns the same `SrcLoc` as the fallback). Adding `logThreadInfo` to the list
"for symmetry" would drop that frame, land on the unlisted `logThreadMessage`,
and start reporting `Log.hs` as the call site for every threaded log line.

Also: the `-- All internal: use most recent frame` comment on the fallback
branch is wrong. `fallback` is the head of the *reversed* list — the oldest /
outermost frame, which is precisely why it is the correct answer.

### CH-10. Three whole error domains are never constructed
`Engine.Core.Error.Exception` defines 49 constructors across 7 domains. 31 are
never constructed outside their own declaration, including three complete
domains:

| Domain | Tag uses | Dead constructors |
|---|---|---|
| `ResourceError` / `ExResource` | 0 | 6 of 6 |
| `StateError` / `ExState` | 0 | 4 of 4 |
| `LuaError` / `ExLua` | 0 | 8 of 8 |

`LuaError` is the striking one: the Lua subsystem is one of the largest in the
tree and models its errors some other way entirely, leaving an eight-case
taxonomy that reads as the canonical Lua error vocabulary and is inert.

Also dead here: `tryEngine` (0 uses), `TestError` (a test-only constructor in
a production error domain).

### CH-11. `ErrorContext` is exported by field but not by name
`Engine.Core.Error.Exception`'s export list omits the `ErrorContext` type
while exporting its accessor `contextCallStack`, and `EngineException(..)`
exposes `errorContext ∷ ErrorContext`. Downstream code can read the field but
cannot name its type in a signature. Either export the type or make the field
genuinely private.

Minor siblings in the same file: `throwEngineException` and `catchEngine` are
pointless aliases for `throwError`/`catchError` (3 and 2 uses); `AssetError`'s
`AssetFailedCleanup` is the one constructor with no haddock; the
`ExceptionType` constructor comments are misaligned by 1-5 columns.

### CH-12. `Engine.Core.Var` is a production module used only by tests
`src/Engine/Core/Var.hs` exports a thin renaming of `Control.Concurrent.STM`
(`Var = TVar`, `newVar = newTVar`, …). Its only importers are
`test/Test/Engine/Core/Var.hs` (which tests it) and three Vulkan test modules.
No `src/` or `app/` module uses it. `dupVar` has exactly one use — the test
that exercises `dupVar`.

Fix: delete, or move under `test/`. Note the tests are testing STM itself.

### CH-13. `luaQueue` is misnamed relative to its sibling
`EngineEnv` has `luaToEngineQueue` (Lua → engine) and `luaQueue` (engine →
Lua). The direction-neutral name for the directional queue makes call sites
ambiguous. `Engine.Core.Init` already knows the right name — it binds the
local as `engineToLuaQueue` and then assigns `luaQueue = engineToLuaQueue`.

Fix: rename the field to `engineToLuaQueue`.

### CH-14. Capability-record conventions are documented in three places
`Engine/Core/Capability/Core.hs` opens with a 63-line module haddock stating
the conventions **every** capability record must follow (naming, one-way
projection, shared containers, no back-imports, no records ahead of need,
thread-private splits). The same rules are stated in CLAUDE.md's "Capability
records (#889)" paragraph and in `docs/engineenv_capability_inventory.md`,
which the haddock itself names as the authority.

Three copies drift. The rules belong in the inventory doc; `Core.hs` should
document `CoreCapability` and link out.

### CH-15. Cross-cutting: 136 comments cite PR review rounds
`grep -rniE "round [0-9]+ (review|of review)|review round [0-9]+" src app`
returns 136 hits, e.g. `-- ^ #745 review round 12: bumped ONLY by a
route-affecting…`, `-- (round 9 review, issue #763)`. `UI/` is the densest
(`UI/Manager/*`, `UI/Types.hs`, `UI/ControlActivation.hs`).

A review round is not a fact about the code — it is a fact about how the code
came to be, and it is unresolvable without pulling the PR. CLAUDE.md already
made this call for itself ("Deep per-issue history … was trimmed from this
file"); the same principle applies in source. The *invariant* should stay, the
*provenance* should go (an issue number alone is fine).

### CH-16. Cross-cutting: 555 files repeat a global `LANGUAGE` pragma
`UnicodeSyntax` is in `common lang`'s `default-extensions`, imported by all
four cabal components — yet 555 modules re-declare it in a `{-# LANGUAGE #-}`
pragma. `OverloadedStrings` (also global) is re-declared in 66. Pure noise on
the first line of nearly every file; also actively misleading, since it
implies the extension is *not* on elsewhere.

---

## Batch 2 — `Engine/Loop`, `Engine/Input`, `Engine/Asset`, structure (swept 2026-07-25)

### CH-17. `Show Font` drops its closing brace when a cleanup action is present
`Engine/Asset/Types.hs:114`:

```haskell
<> ", fCleanup = " <> if isJust (fCleanup f) then "<present>" else "<absent> }"
```

The `" }"` is inside the *else* string, so any `Font` that has a cleanup
action — i.e. every loaded font — renders as `Font { … fCleanup = <present>`
with no terminating brace. A real (cosmetic) bug in a hand-written `Show`.

### CH-18. `AssetConfig` advertises four features that do not exist
`Engine.Asset.Types.AssetConfig` declares `acMaxTextureAtlases`,
`acMaxShaderPrograms`, `acPreloadAssets`, `acEnableHotReload`. It is
constructed exactly once — positionally, as `AssetConfig 100 100 True True` in
`Engine.Core.Defaults` — stored in `EngineState.assetConfig`, and **never read
anywhere**. So the engine appears to support asset preloading and hot reload
and to enforce atlas/shader caps; it does none of those.

Worse than plain dead code: it is a false capability advertisement, written
positionally so the four bare literals cannot even be matched to their meaning
without opening a second file.

Fix: delete `AssetConfig` and `EngineState.assetConfig`, or implement it.

### CH-19. `TimingState` is five-sixths write-only, and `targetFPS` is a lie
Only `deltaTime` is read outside `Engine/Loop/Timing.hs`. `frameCount`,
`currentTime`, `frameTimeAccum`, `lastFrameTime`, and `targetFPS` have zero
external readers.

`targetFPS = 60.0` (set in `Engine.Core.Defaults`) is read by **nothing** —
the real frame cap comes from `VideoConfig`'s `vcVSync`/`vcFrameLimit`. A
field named `targetFPS` sitting in the engine's timing state is the first
place anyone will go to change the frame rate, and changing it does nothing.

`frameCount` is also misnamed: it resets to 0 every second
(`Engine/Loop/Timing.hs:85`), so it is "frames since the last FPS sample", not
a frame count — and it is typed `Word64` for a value that never exceeds a few
hundred.

Fix: make the five fields local to the timing computation; delete or rename
`targetFPS` and `frameCount`.

### CH-20. `Engine.Input.Thread`'s module haddock describes an API it doesn't expose
The header says the #787 split moved logic into `Dispatch` plus the four
per-domain modules, and concludes: *"Both are re-exported here so the public
API is unchanged."*

The export list is `startInputThread, runInputLoop, processInputs,
processInput`. Only `Dispatch`'s two functions are re-exported;
`Engine.Input.Thread.Keyboard`/`.Char`/`.Mouse`/`.Scroll` are neither imported
nor re-exported. Anyone trusting the header will look for the per-domain entry
points here and not find them.

### CH-21. The module-budget guard has a subdirectory hole, and code already sits in it
`tools/haskell_module_budget.py` guards the #787 input split with the pattern
`src/Engine/Input/Thread/*.hs` at 500 lines. `Path.glob` does not cross
directory separators, so `src/Engine/Input/Thread/Mouse/Activation.hs` is
**not** checked.

That is not hypothetical: `Mouse.hs` is at **exactly 500 lines** — the cap —
and 57 further lines live in the unguarded `Mouse/` subdirectory. The next
overflow has an obvious, silent escape hatch.

Fix: use `**/*.hs` in `BUDGETS`, and add a self-test asserting the pattern
matches nested files.

### CH-22. The 500-line norm guards 6 Lua files while 30 exceed it
`tools/lua_module_budget.py` enforces 500 lines on six historical splits
(`debug`, `unit_resources`, `unit_ai`, `unit_info_v2`, `init`, `ui_manager`).
Meanwhile **30** Lua files exceed 500 lines and are unguarded, including the
largest scripts in the project:

| Lines | File |
|---:|---|
| 1399 | `scripts/ui/dropdown.lua` |
| 1321 | `scripts/hud.lua` |
| 1171 | `scripts/create_world_menu.lua` |
| 1167 | `scripts/build_tool.lua` |
| 1118 | `scripts/shell.lua` |
| 1063 | `scripts/settings_menu.lua` |
| 1058 | `scripts/combat_log.lua` |

The budget currently reads as "these six splits must not regress" rather than
"Lua modules stay under 500 lines", which is how CLAUDE.md presents it. Either
generalise the rule or state plainly that it is a per-split ratchet.

(`scripts/ui/dropdown.lua` at 1399 lines for one widget is a split candidate
in its own right.)

### CH-23. Oversized Haskell modules are now concentrated in `World/Save/`
The 2026-07-07 triage (`docs/history/haskell_large_file_submodule_triage_2026-07.md`)
found 43 modules over 500 lines. That is down to **17** — good progress — but
the remainder is no longer spread evenly:

| Lines | File |
|---:|---|
| 1316 | `src/World/Save/Types.hs` |
| 1139 | `src/World/Save/Component/Entities.hs` |
| 1090 | `src/Engine/Scripting/Lua/API/Save.hs` |
| 860 | `src/World/Save/Envelope.hs` |
| 819 | `src/World/Save/Storage.hs` |
| 660 | `src/World/Save/Component/Page.hs` |
| 616 | `src/World/Save/Component/WorldGen.hs` |

Eight of the seventeen are the persistence subsystem, which was rebuilt by the
#756-#768 overhaul *after* the triage and never split. Note `World/Save/Types.hs`
was on the original list at 726 lines, rated "Low-Med — serialization-sensitive,
split only with care"; it has since grown to 1316. It is now the largest module
in the tree and is growing under an explicit "be careful here" flag, which is
the condition under which files quietly become unmaintainable.

The other five are `Engine/Scripting/Lua/API/*` (`Units/Inventory` 719,
`UI/Property` 645, `Units/Stats` 641, `Blood` 519, `Power` 516) — the same
category the triage rated "High feasibility".

### CH-24. `runGatedByCaptureLock` documents a bug that no longer exists
`Engine/Loop.hs:69-105` — a 37-line haddock in which ~25 lines narrate a
*previous failed attempt*: "The first attempt at this fix only READ
`captureLocked` as a point-in-time pre-check … but this thread was not a real
`SaveOwner` at all, so nothing ever waited for it …".

The durable content is about four lines (this thread is a real `SaveRender`
owner; it acknowledges unconditionally; `acknowledgeCurrent` no-ops when
`SaveRender` isn't in the owner set). The rest describes code that does not
exist, which every future reader must read and then discard. Worst instance of
CH-15.

### CH-25. `tools/` is 122 flat Python files
No subdirectories except `playtest/` and `baselines/`. The 122 files divide
cleanly by role — 74 `*_probe.py`, 10 `*_audit.py`, 6 `*_check.py`, 14
`test_*.py`, 22 reports/utilities — and `tools/README.md` plus
`tools/ci_probes.py` already exist to navigate what a directory layout would
make self-evident.

Proposed: `tools/probes/`, `tools/audits/`, `tools/reports/`, `tools/tests/`,
with the handful of shared helpers (`probelib.py`, `persistence_snapshot.py`)
staying at the top. Note this touches every `python3 tools/x_probe.py`
invocation in CLAUDE.md, CI, and the skills, so it needs to be done in one
sweep.

### CH-26. `CHANGELOG.md` has not been touched in 18 months
Its entire content:

```
## 0.1.0.0 -- 1-22-2025
- First version. Released on an unsuspecting world.
- rendering of multiple textured sprites
- input handling
```

Since then: worldgen, hydrology, geology, combat, crafting, power, farming,
persistence overhaul, UI system, ~900 merged PRs. The file now
*misinforms* — it is the one place a newcomer looks for "what does this do"
and it says "sprites and input".

Fix: delete it, or regenerate from merged PRs and keep it current.

### CH-27. Minor defects worth folding into one cleanup issue
- `Engine/Loop.hs:30` — `_state ← gets graphicsState` in `mainLoop`: an unused
  binding that reads engine state on every tick and discards it.
- `Engine/Loop.hs:127` — `GLFWError "handleEngineRunning: "` — the error
  payload is a bare function-name prefix with a trailing `": "` and no
  message; the real message is passed as a separate argument.
- `Engine/Loop/Timing.hs:24` — `-- Get video config (cache the read)` above a
  plain `readIORef` that caches nothing.
- `Engine/Loop/Timing.hs:55` — `compensatedTarget = targetFrameTime - 0.0012`,
  an unexplained 1.2 ms magic constant in the frame limiter.
- `Engine/Input/Types.hs:18` — `inpPendingUIClick ∷ Map GLFW.MouseButton
  (Text, Text, Double, Double)`; the four components' meanings exist only in
  prose. Should be a named record.
- `Engine/Asset/Types.hs:1-3` — `TypeApplications`, `AllowAmbiguousTypes`, and
  `ScopedTypeVariables` pragmas on a module with no type applications, no
  `forall`, and no ambiguous types (`ScopedTypeVariables` is also implied by
  GHC2024). `AllowAmbiguousTypes` in particular suppresses a useful error for
  no reason.
- 29 modules have no export list, including the four largest engine modules
  (`Engine/Core/State.hs`, `Engine/Input/Types.hs`,
  `Engine/Scripting/Lua/Types.hs`, `Engine/Scene/Render.hs`). Everything is
  public, so nothing can be refactored without a whole-tree grep.

---

## Batch 3 — `src/Engine/Graphics/` (swept 2026-07-25)

No module here exceeds 500 lines; the problems are dead modules, duplicated
constants, and comments that contradict the code beside them.

### CH-28. Five modules are not in `synarchy.cabal` — never compiled, never linted
`synarchy.cabal` uses an explicit module list. These `src/` modules are absent
from it, so GHC never compiles them, `-Wall -Werror` never sees them, and
`cabal sdist` would ship a broken tarball:

| Module | Lines | Status |
|---|---:|---|
| `Engine.Graphics.Vulkan.Types.Core` | 17 | unreferenced |
| `Engine.Graphics.Vulkan.Types.Font` | 16 | unreferenced |
| `World.Log` | 145 | unreferenced |
| `World.Hydrology.Log` | 174 | imported only by `World.Log` |

(`UPrelude` is listed; it was a false positive of the naive check.)

Proof they are not compiled: `Types/Core.hs` imports
`Engine.Graphics.Vulkan.Capability (TextureSystemCapability)` and never uses
it — an unused import that `-Wall -Werror` would reject on sight. Last touched
2026-01-30.

`World.Log` is the substantial one: a 145-line "unified world logger" facade
(`WorldLogger`, `WorldLogDest`, `WorldVerbosity`, `logWorldGen`,
`logGeoTimeline`, `logHydrology`, `logWeather`) that nothing imports, which in
turn is the only importer of the 174-line `World.Hydrology.Log`. 319 lines of
a logging subsystem that has never been part of the build.

Fix: delete all four. Then add a CI check that every `src/**/*.hs` appears in
the cabal module list — this class of rot is invisible otherwise.

### CH-29. Dead types kept alive by other dead types
`Engine.Graphics.Vulkan.Types.SyncObjects` carries this haddock:

> LEGACY: unused by the render loop. […] Type kept only because
> `Engine.Graphics.Types.vsSyncObjects` still references it.

`vsSyncObjects` is a field of `VulkanState` — and `VulkanState` has **zero**
uses anywhere outside its own declaration. So a type documented as retained
for one consumer is retained for a consumer that is itself dead. Deleting
`VulkanState` deletes the only reason `SyncObjects` exists.

Also zero uses in the same two files: `VulkanExtensions`, `VulkanLayers`,
`VulkanDescriptorInfo`. And `Engine.Graphics.Vulkan.Types.Font.FontState` /
`Types.Core.VulkanCore` (CH-28) duplicate field sets that live inline in
`GraphicsState` — they were the abstraction that never landed.

### CH-30. The demo quad vertex buffer is uploaded to the GPU every boot and never drawn
`Engine.Graphics.Vulkan.Vertex.quadVertices` is 12 hardcoded vertices —
"Two side-by-side quads with different atlas IDs … (unused demo geometry)",
per its own comment. `Engine/Graphics/Vulkan/Init.hs:202` calls
`createVertexBuffer` on it at every boot: allocates a staging buffer, maps it,
pokes the 12 vertices, copies to a device-local buffer, and stores the result
in `GraphicsState.vertexBuffer`.

**Nothing ever reads `vertexBuffer`.** The only other references are the field
declaration and `= Nothing` in `Defaults`. So every session carries a
permanent GPU allocation of tutorial geometry that is never bound and never
drawn.

Compounding it: the `vertexBuffer` field is the one crammed onto
`msaaColorImage`'s line (CH-5), so a grep for its declaration doesn't find it.

Fix: delete `quadVertices`, `createVertexBuffer`, the `Init.hs` call, and the
`GraphicsState.vertexBuffer` field.

### CH-31. The bindless texture limit is duplicated in five places with no check
`16384` must agree across:

1. `Bindless.hs:47` — `bcMaxTextures = 16384`
2. `Texture/System.hs:36` — `min 16384 (min maxSlots …)` (bare literal, no comment)
3. `Init.hs:208` — `tscMaxTextures = 16384`
4. `ShaderCode.hs:201` — `uniform sampler2D textures[16384]`
5. `ShaderCode.hs:370` — same, in the UI fragment shader

`65536` (`handleSlotTableSize`) is duplicated across three: `Bindless.hs:68`
and `HANDLE_TABLE_SIZE` in both fragment shaders (`ShaderCode.hs:223, 374`).

The comments say "MUST match shader" / "MUST match `HANDLE_TABLE_SIZE`" — and
nothing enforces it. A mismatch is not a build error; it is out-of-bounds
descriptor indexing at runtime. They agree today.

Fix: one Haskell constant, interpolated into the GLSL (the shaders are
`QuasiQuotes` strings already), or a test asserting the shader source contains
the Haskell values.

### CH-32. `Bindless.hs`'s header claims 64× the real texture limit
Line 1-2:

```haskell
-- | Bindless texture system using UPDATE_AFTER_BIND descriptors
-- This enables up to 1 million texture slots on MoltenVK/Metal
```

45 lines later, in the same file: `bcMaxTextures = 16384 -- Must match shader`.
The header describes the technique's theoretical ceiling as if it were the
system's capacity. Anyone sizing an asset budget off the module header is off
by 64×.

### CH-33. `Texture.System`'s "legacy path" is a throw
Module header: *"Unified texture system that handles both bindless and legacy
paths."* The legacy branch in full:

```haskell
    _ → do
      logInfoM CatTexture "BINDLESS TEXTURES NOT SUPPORTED - LEGACY SYSTEM BROKEN!!!"
      logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
        "Legacy texture system is not implemented."
```

There is no legacy path. Consequences:

- The header is wrong — the module handles exactly one path.
- `TextureSystemConfig.tscForceLegacy` ("Force legacy path (for testing)") is a
  config knob whose only effect is to force a throw. Set `False` at its one
  call site.
- The shouty `!!!` message is logged at *info* and immediately followed by a
  throw carrying a better message — two log lines for one condition.
- `loadTexture` takes a `_filterMode` parameter it ignores (correctly — atlases
  share the global sampler now), still present in the signature.

### CH-34. `destroyBindlessTextureSystem` is exported, never called, and incomplete
It releases the shared sampler and destroys the descriptor pool and layout. It
does **not** touch `btsUndefinedTexture` (image + view + memory),
`btsHandleSlotBuffer`/`btsHandleSlotMemory` (a persistently-mapped 256 KB
storage buffer), or any registered image view.

That is survivable only because those are `allocResource`-managed and freed at
process exit — i.e. the function's name promises a teardown that the codebase
actually performs somewhere else entirely. It has no call sites, so today it is
dead; the day someone wires up device-loss recovery it is a leak.

Fix: delete it, or complete it and document that it must run before the
`allocResource` scope unwinds.

### CH-35. The uniform buffer layout is hand-maintained across five declarations
`UniformBufferObject` is declared in:

- `Vulkan/Types.hs` — the record (14 fields)
- `Vulkan/Types.hs` — a hand-written `Storable` instance with literal byte
  offsets (`+ 4`, `+ 8`, … `+ 32`) and `sizeOf _ = 5 * sizeOf (M44 Float) + 36`
- `Vulkan/Types.hs` — a 20-line comment block listing every field's offset
- `ShaderCode.hs` ×4 — inline GLSL `uniform UniformBufferObject { … }` blocks,
  which are **not identical**: `bindlessVertexShaderCode` declares 14 members,
  the other three declare 12 (they stop before `defaultFaceMapSlot`).

Adding one float means eight coordinated edits — record, `sizeOf`'s magic `36`,
a `peek` line, a `poke` line plus its 14-argument pattern, the offset comment,
and whichever GLSL blocks need it. Inserting a field *in the middle* silently
corrupts every shader that declares the shorter prefix, with no build error.

(Also: `alignment _ = 16` while `sizeOf` is 356, which is not a multiple of 16.
Harmless for a single instance, wrong for an array.)

### CH-36. `fontFragmentShaderCode` is dead, and says so
`-- | Legacy font fragment shader (non-SDF, kept for compatibility)` — zero
call sites. Compatibility with nothing. The SDF shader is the only one wired
up.

### CH-37. `graphicsState` nested-record-update boilerplate, 50×
```haskell
modify $ \s → s { graphicsState = (graphicsState s) { vulkanRenderPass = Just renderPass } }
```
appears **50 times** across 16 modules (18× in `Vulkan/Init.hs` alone, 12× in
`Recreate.hs`). No `modifyGraphicsState ∷ (GraphicsState → GraphicsState) →
EngineM ε σ ()` helper exists. Each site would collapse to one line.

### CH-38. Naming inconsistencies in the graphics records
- **`vc` prefix collision.** `VulkanCore`'s fields are `vcInstance`,
  `vcDevice`, … while `VideoConfig`'s are `vcWidth`, `vcVSync`,
  `vcWindowMode`, … Two records in the same subsystem sharing a prefix, with
  `DuplicateRecordFields` on globally.
- **`FontState.pendingInstanceBuffers`** is the one field in its record with
  no `fs` prefix — and it collides with `GraphicsState.pendingInstanceBuffers`.
- **`SwapchainSupportDetails`** uses bare `capabilities`, `formats`,
  `presentModes` while every other record in `Engine/Graphics/Types.hs` uses a
  prefix. `DevQueues` likewise (`graphicsQueue`, `presentQueue`, …). These are
  maximally generic names in a globally-shared field namespace.
- **`BufferUtils`** — a `*Utils` module name that says nothing about content;
  it sits beside `Buffer.hs`, which does something different.

### CH-39. Minor graphics defects for one cleanup issue
- `Bindless.hs:229` — `allocateBindlessDescriptorSet` takes `_config` and
  ignores it; every caller threads a `config` through for nothing.
- `Bindless.hs:421` — `Nothing → pure ()  -- shouldn't happen` silently keeps
  the wrong sampler if `btsHandleMap` and `btsImageViews` ever desync. Should
  log.
- `Bindless.hs:431` / `System.hs:71` — both return slot `0` to mean "lookup
  failed", which is also the real slot of the undefined texture. Documented in
  neither signature.
- `Vertex.hs:94-131` — six attribute-description comments are written `-- ^ TexCoord`
  inside a list expression. `-- ^` is haddock's "documents the *preceding*
  item"; there is no haddock inside expressions, and the first entry
  (`-- Position`) correctly omits it. Six stray carets.
- `Vertex.hs:81` — `stride = 48` plus offsets `0/8/16/32/36/40/44` must match
  `Vertex`'s `Storable` instance in `Types/Vertex.hs` by hand.
- `Init.hs:234` — `bindlessTexLayout = btsDescriptorLayout texSystem` reads
  from the pre-`createDefaultFaceMap` copy while `texSystemWithFaceMap` is the
  live one. Same value today because the layout is immutable; silently wrong
  the day it isn't.
- 399 lines in `src/` + `app/` have trailing whitespace (369 of them under
  `src/Engine/`).

---

## Batch 4 — `src/Engine/Scripting/` (swept 2026-07-25)

128 modules, 27k lines — the largest subsystem. Its problems are structural
inconsistency rather than dead code: three facade idioms, uncapped function
size, and one comment block that is 22% of the tree's largest file.

### CH-40. `currentSaveVersion` carries a 296-line changelog for a superseded scheme
`src/World/Save/Types.hs` — one `Int` constant with **296 lines of comment**
attached (70 lines of leading haddock + 226 lines of trailing right-hand-side
comment), documenting ~65 historical save versions back to v2. That is **22% of
the 1317-line file**, and the single reason `World/Save/Types.hs` is the
largest module in the tree (CH-23).

```haskell
currentSaveVersion ∷ Int
currentSaveVersion = 91  -- v91 (#761, save-overhaul B3): 'sdLuaModules'
                         -- removed — Lua-owned state no longer rides
                         -- through 'SaveData' at all; …
                         -- v90 (#759, save-overhaul B1): …
                         -- v89: WorldGenParams gains trailing …
                         [… 220 more lines …]
```

The changelog also documents a scheme that no longer governs compatibility.
CLAUDE.md states plainly:

> `currentSaveVersion` now versions only the transitional in-memory load
> bridge (`SaveData`) and is bumped freely — don't trust any number written in
> docs. Component evolution = per-component schema version bumps …

So the file's largest artefact is a growing changelog for a global version
number that was replaced by per-component versioning. `docs/save_compat/` and
`tools/save_compat_audit.py` already exist as the real home.

Fix: move the history to `docs/save_compat/`, leave two lines at the constant.

### CH-41. The 500-line module budget doesn't constrain function size
124 top-level definitions in `src/` exceed 100 lines; 30 exceed 200; 16 exceed
300. Worst offenders:

| Lines | Definition |
|---:|---|
| 450 | `Engine/Input/Thread/Mouse.hs:52` `dispatchMouseEvent` |
| 419 | `World/Generate/Chunk.hs:84` `generateChunk` |
| 402 | `World/Render/CursorQuads.hs:42` `renderWorldCursorQuads` |
| 389 | `World/Render/Quads.hs:41` `renderWorldQuads` |
| 371 | `Combat/Wounds/Tick.hs:73` `tickAllWounds` |
| 369 | `World/Weather/Generate/ClimateBuilder.hs:20` `buildClimateFromOceanSet` |
| 360 | `World/Generate/Timeline.hs:32` `applyTimelineChunk` |
| 327 | `Combat/Resolution.hs:118` `resolveAttack` |
| 318 | `Engine/Scripting/Lua/Thread/Dispatch.hs:49` `processLuaMsg` |
| 311 | `World/Magma/Pool.hs:75` `rimJitter` |

`dispatchMouseEvent` is the sharpest illustration: it lives in a file that sits
at *exactly* the 500-line budget (CH-21), and 450 of those lines are one
function. The module budget is satisfied while the actual reviewability problem
is untouched.

In the Lua tree specifically, `API/Save.hs` is 1090 lines across just 13
top-level definitions — `knownEntitiesFromSaveData` alone is 265 lines and
`callSaveModules0` is 194.

Fix: add a function-length guard alongside the module guard, or file the top
~16 as individual decomposition issues.

### CH-42. Three different facade idioms across nine sibling API domains
Every `API/<Domain>.hs` facade does the same job — re-export its submodules —
in one of three ways:

| Idiom | Domains |
|---|---|
| `module X` re-exports | Buildings, Craft, Forage, UI, World |
| Hand-listed export names | Equipment (10), Items (14), WorldQuery (22), **Units (103)** |
| No facade at all | `Register/` |

The hand-listed variant is a live maintenance hazard: add a function to
`API/Units/Combat.hs` and it is silently absent from the `Units` facade until
someone edits the 103-name list. 149 export names are maintained by hand this
way.

`WorldQuery.hs`'s haddock even documents why it diverged (to keep
`WorldQuery.Lookup` internal) — which is achievable with `module` re-exports
by simply not re-exporting that one.

Fix: pick one idiom (module re-exports, with internal submodules omitted) and
apply it to all nine.

### CH-43. Five Lua API modules are 400-520 lines with no split, while `Save.hs` is 1090
Nine domains were split into facade + subdirectory. These were not:

| Lines | Module |
|---:|---|
| 1090 | `API/Save.hs` |
| 519 | `API/Blood.hs` |
| 516 | `API/Power.hs` |
| 433 | `API/YamlTextures.hs` |
| 407 | `API/Construct.hs` |
| 402 | `API/InputInject.hs` |

`Save.hs` is more than twice the next largest module in the tree and the
obvious next split (it already partitions cleanly: save listing/status, the
save path, the load path, integrity/`KnownEntities`, and the Lua save-module
bridge).

### CH-44. Two `Focus` modules, neither of which says which focus it means
`Engine.Scripting.Lua.API.Focus` and `Engine.Scripting.Lua.API.UI.Focus` are
indistinguishable by name and bind **two genuinely different focus systems**:

- `API/Focus.hs` → `UI.Focus.FocusManager` via `focusManagerRef`, registered
  onto the Lua `engine` table (`engine.registerFocusable`, `requestFocus`,
  `releaseFocus`, `getFocusId`). Its only consumer is `scripts/shell.lua` —
  this is *debug-console/shell text focus*.
- `API/UI/Focus.hs` → `UI.Manager`'s `upmTextFocus`/`upmControlFocus`,
  registered onto the `UI` table. This is the *game UI* focus system CLAUDE.md
  documents at length (#745).

`Engine/Input/Thread/Keyboard.hs:97` already documents the priority order
("1. FocusManager (focusManagerRef) — shell/console text input"), so the
distinction is known — it just isn't reflected in the module names or their
haddocks. `API/Focus.hs` has **no module haddock at all**; `API/UI/Focus.hs`
says "Lua bindings for keyboard/input focus management", which describes both
equally well.

Fix: rename to `API/ShellFocus.hs` (or `API/Focus/Shell.hs`) and document the
two-system split once, in both.

### CH-45. `ScriptFunction` is a dead constructor with a silent-failure handler
`Engine.Scripting.Types.ScriptValue` has six constructors. `ScriptFunction
Dynamic` is **never constructed** anywhere, exists only to force a
`Data.Dynamic` dependency, and its handler silently drops the value:

```haskell
    ScriptFunction _ → Lua.pushnil
```

The doc says "'ScriptFunction' is still nil since 'Dynamic' has no Lua
representation" — "still" implying a deferred fix that has no consumer waiting
for it. If it were ever constructed, the argument would vanish silently rather
than error.

`ScriptBool` (1 use) and `ScriptNil`/`ScriptTable` (2 each) are thin but live.

### CH-46. The Lua API tree holds 57% of the engine's unrestricted-`EngineEnv` surface
The #889-#899 capability epic ratchets modules importing
`Engine.Core.State (EngineEnv(..))`. Of the 49 such modules in all of `src/` +
`app/`, **28 are under `Engine/Scripting/`** — 28 of the tree's 93 modules that
import `Engine.Core.State` at all.

Not a defect on its own, but it locates the epic's remaining work: the Lua API
binding layer is where narrowing pays off most, and it is not currently where
the migration issues point.

### CH-47. `Engine.Core.Log`'s callsite skip-list has a matching hazard here
(Cross-reference to CH-9.) `Engine.Scripting.Lua.API.Internal.registerLuaFunction`
is the single choke point through which every Lua-facing Haskell function is
registered, and it wraps each in a `Catch.catch` handler. Any source-location
reporting through this path inherits CH-9's fragility. Worth checking together.

### CH-48. Minor Lua-tree defects for one cleanup issue
- `Engine/Scripting/Lua/Script.hs:46-47` — two consecutive `-- |` haddock
  openers on `callModuleFunction`; the first (`-- | Call a function on a module
  table`) is a leftover stub that haddock will fold into the real one. Same
  defect class as CH-4.
- `API/Save.hs:417` — `flattenItemInstanceIds'` has a prime suffix with no
  unprimed sibling anywhere; the prime conventionally marks a variant of an
  existing function.
- `API/Save.hs:745` — `callSaveModules0`: a `0` suffix with no
  `callSaveModules`.
- `API.hs` — `registerEngineAPI lst env backendState` is the only one of twelve
  registrars taking the `Lua.State` explicitly, even though all twelve already
  run inside `Lua.runWith lst`.
- `Engine.Scripting.Types` is a 14-line top-level namespace holding one type,
  sitting beside the 272-line `Engine.Scripting.Lua.Types`. The split buys
  nothing — `ScriptValue` is Lua-specific in practice.

### CH-49. Cross-cutting: normalise the enforced Unicode operators (owner decision recorded)
CLAUDE.md publishes an operator table as the codebase convention, but adoption
varies from 99% to 10%. Measured across `src/` + `app/`:

| Operator | ASCII | Unicode | Adoption | Decision |
|---|---:|---:|---:|---|
| **bitwise AND** | **38 `.&.`** | **4 `⌃`** | **10%** | **ENFORCE `⌃`** |
| **bitwise OR** | **55 `.\|.`** | **17 `⌄`** | **24%** | **ENFORCE `⌄`** |
| **bind** | **28 `>>=`** | **34 `⌦`** | **55%** | **ENFORCE `⌦`** |
| **equality** | **42 `==`** | **547 `≡`** | **93%** | **ENFORCE `≡`** |
| inequality | 16 `/=` | 90 `≢` | 85% | (follows equality) |
| logical and | 3 `&&` | 558 `∧` | 99% | already converged |
| logical or | 4 `\|\|` | 315 `∨` | 99% | already converged |
| fmap | 108 `<$>` | 134 `⊚` | 55% | **LEAVE BOTH — deliberate** |

**Owner decision (2026-07-25):** `<$>` and `⊚` are both kept — they read
better in different circumstances, and this is intentional, not drift. Remove
the implication that `⊚` is mandatory from CLAUDE.md's table. Everything else
in bold gets normalised to the Unicode form: `>>=` → `⌦`, `.&.` → `⌃`,
`.|.` → `⌄`, `==` → `≡`.

Planned fix: a Python script that rewrites all `.hs` files in one sweep,
followed by a `make ci` grep guard so the four enforced operators cannot
regress. ~163 sites total.

Where the ASCII forms concentrate:

- **Bitwise** (93 sites) is overwhelmingly Vulkan flag composition —
  `src/Engine/Graphics/Vulkan` (8 files) and `Vulkan/Texture` (3) lead, with a
  long tail through `World/` (Slope, Geology, Fluid, Flora, ZoomMap, SideFace,
  Plate, Render) and `Sim/Fluid`. This is the worst-adopted operator pair in
  the codebase and the main prize.
- **`==`** (42 sites) — notably 8 of them are in `src/Engine/Core/Capability`,
  the *newest* code in the tree (the #889 capability epic), which suggests the
  convention isn't reaching new work.

Care needed in the sweep: `.&.`/`.|.` must not be rewritten inside string
literals, comments, or the embedded GLSL in
`Engine/Graphics/Vulkan/ShaderCode.hs` (GLSL uses `&`/`|`, not these forms, but
the quasiquoted blocks should be excluded anyway), and `==` must not be
rewritten inside Lua/GLSL string payloads or Python-facing text.

---

## Batch 5 — `Engine/Asset`, `Engine/Scene`, `Engine/Loop`, remaining Engine (swept 2026-07-25)

This closes `src/Engine/` coverage.

### CH-50. `Engine.Graphics.Transform` is a fully dead module
67 lines, three exported functions (`createModelMatrix`, `applyTransform`,
`combineTransforms`), listed in `synarchy.cabal` so it compiles on every build,
and **no module imports it**. It operates on `Transform2D`, which lives in
`Engine.Scene.Base` and is used elsewhere — so the type is live and only this
module's operations on it are dead.

Distinct from CH-28: those modules were invisible to the build; this one is
compiled, warning-checked, and still useless.

### CH-51. `Engine.Asset.Manager` is a 470-line abstraction used as an ID generator
23 of its 27 exports have **no external consumer**:

> `cleanupAssetManager`, `loadTextureAtlas`, `loadTextureAtlasWithHandle`,
> `unloadAsset`, `getTextureAtlas`, `getShaderProgram`, `lookupTextureAsset`,
> `lookupFontAsset`, `lookupShaderAsset`, `getAllTextureHandles`,
> `getAllFontHandles`, `getAllShaderHandles`, `getTextureHandleState`,
> `getFontHandleState`, `getShaderHandleState`, `getTextureStateMap`,
> `getFontStateMap`, `getShaderStateMap`, `deleteTextureState`,
> `deleteFontState`, `deleteShaderState`, `updateShaderState`,
> `generateShaderHandle`

Seven modules import it; six import only `generateTextureHandle`,
`generateFontHandle`, `updateTextureState`, or `updateFontState` — the handle
allocators and state setters. The one bare import
(`Lua/Message/Texture.hs`) then calls
`Engine.Graphics.Vulkan.Texture.Bindless.registerTexture` *directly* at line
233, bypassing `Manager.loadTextureAtlas` entirely.

So the real texture path is Lua message → bindless system, and `Manager` keeps
a complete parallel load/lookup/unload API — including its own
`registerTexture` call at line 252 — that nothing invokes. The whole shader
half (`ShaderProgram`, `generateShaderHandle`, `updateShaderState`,
`getShaderStateMap`, …) has no consumer at all.

`cleanupAssetManager` being dead also means the asset-cleanup path never runs.

Fix: reduce to the handle allocators + state setters actually used, and delete
the shader-asset half (or say why it is kept).

### CH-52. 14 verbatim copies of the same YAML loader
Thirteen `Engine/Asset/Yaml*.hs` modules each define a `load<Thing>Yaml` that
is character-identical except for three strings and one field accessor:

```haskell
load<Thing>Yaml ∷ LoggerState → FilePath → IO [<Thing>Def]
load<Thing>Yaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse <noun> YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right f → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (<accessor> f)))
                <> " <plural> from " <> T.pack path
            return (<accessor> f)
```

Each is also preceded by an identical `newtype <Thing>YamlFile` +
`FromJSON` instance wrapping a single list field. 14 copies of the block in
total (Buildings, Equipment, Flora, Infection, Items, Locations, LootTables,
Names, Notifications, Recipes, Substance, Textures ×2, Units).

The whole family collapses to one helper:

```haskell
loadYamlList ∷ FromJSON f ⇒ LoggerState → Text → (f → [a]) → FilePath → IO [a]
```

Signatures also drift where nothing forced them to: `loadLootTableYaml`
returns `IO (Maybe LootTableYamlDef)` and `loadNamePool` returns `IO NamePool`
while every sibling returns `IO [x]` — three different failure conventions for
the same operation.

### CH-53. `Engine.Asset.YamlTextures` loads no textures and holds three unrelated things
Its contents:

1. **Material YAML** — `MaterialDef`, `MaterialFile`, `loadMaterialYaml`,
   `loadMaterialDirectory`, `loadPopulatedMaterialRegistry`
2. **Vegetation YAML** — `VegetationDef`, `VegetationFile`, `loadVegetationYaml`
3. **A runtime texture-name registry** — `TextureNameRegistry`,
   `emptyTextureNameRegistry`, `lookupTextureName`, `registerTextureName`,
   `registryToList`, `registryToWorldCommands` — not YAML at all

Nothing in it loads a texture. The word "Textures" in the name refers only to
item 3, which is a live name→handle map, not a loader. Consequence:
`Engine.Core.State` — the engine's central record — imports a module called
`YamlTextures` to get a runtime registry type.

Compare the sibling naming: `YamlItems` loads items, `YamlFlora` loads flora,
`YamlRecipes` loads recipes. This one loads materials and vegetation.

Fix: split into `YamlMaterials.hs`, `YamlVegetation.hs`, and
`Engine/Asset/TextureNameRegistry.hs`.

### CH-54. 97 exported names in `src/Engine/` have no consumer outside their module
Full inventory captured by scan (qualified uses counted, so no
`import qualified … as GLFW` false positives). It splits into two kinds:

**(a) Genuinely dead API** — nothing uses them anywhere, including inside the
module. Biggest cluster is CH-51's `Asset/Manager.hs` (23). Others:
`Engine/Core/Log.hs` (5, per CH-7), `Core/Log/Monad.hs` (3),
`Graphics/Transform.hs` (3, per CH-50), `Vulkan/Texture/Bindless.hs` (3, per
CH-34), `Core/Error/Exception.hs` (2, per CH-11),
`Asset/YamlUnits.hs` (`defaultUnitYamlBody`,
`defaultUnitYamlNaturalResistance`), `Vulkan/ShaderCode.hs`
(`fontFragmentShaderCode`, per CH-36).

**(b) Over-exported internals** — used only within their own module, so the
export list is simply too wide. Exemplar: `Engine/Loop/Camera.hs` exports 8
names (`zoomMin`, `zoomMax`, `cameraYLimit`, `cameraYLimitChunks`,
`applyLimits`, `applyLimitsChunks`, `stepCameraZoom`,
`cameraGotoBufferChunks`) that only it uses — `Engine.Loop` imports exactly
three functions from it. `Window/GLFW.hs` (11), `Scene/Batch/*` (5),
`Vulkan/Descriptor.hs`, `Vulkan/Device.hs`, `Vulkan/Swapchain.hs`,
`Loop/Frame.hs`, `Loop/Resource.hs` follow the same pattern.

Kind (b) matters because it is what makes the 29 export-list-less modules
(CH-27) hard to fix: with everything public by default and export lists that
don't narrow, no refactor can be reasoned about locally.

Fix: two issues — delete kind (a); shrink the export lists for kind (b).

### CH-55. `Engine.Core.Init`'s three exports have no callers
**Corrected 2026-07-25:** only `initializeEngineWith` is genuinely
unreferenced — `resolveConfigPath` and `migrateLegacyConfig` each have one
consumer in `test-headless/`, which the original scan missed.

`initializeEngineWith` is exported and called by nothing; `initializeEngine`
and `initializeEngineHeadlessWith` reach it internally. A one-line
un-export.

### CH-56. `Engine/Scene` has the `X.hs` + `X/` + `Types/X.hs` triple layout
`Engine/Scene/` contains `Base.hs`, `Graph.hs`, `Render.hs`, `Types.hs`, plus
both `Batch/` and `Types/` subdirectories — so a scene batch type can live in
`Scene/Types.hs`, `Scene/Types/Batch.hs`, or `Scene/Batch/Update.hs`, and the
naming gives no rule for which. Same shape as `Engine/Graphics/Vulkan/`
(`Types.hs` beside `Types/`, `Texture.hs` beside `Texture/`, `Command.hs`
beside `Command/`, `Pipeline.hs` beside `Pipeline/`).

This is a tree-wide convention gap: sometimes `X.hs` is a facade for `X/`
(`Engine/Scripting/Lua/API/Units.hs`), sometimes it is a sibling holding
different content (`Engine/Graphics/Vulkan/Types.hs` vs `Types/`). A reader
cannot tell which without opening the file.

Fix: state the rule — `X.hs` beside `X/` means facade, full stop — and move
non-facade content out.

### CH-57. Minor remaining-Engine defects for one cleanup issue
- `Engine/Loop/Resource.hs` exports `safeVectorHead` and `safeVectorIndex`,
  both unused externally — generic container helpers sitting in a module named
  for the render loop's resources.
- `Engine/Graphics/Font/Util.hs` (24 lines) exists to hold one unused function
  (`calculateTextWidth`).
- `Engine/Scene/Graph.hs` exports `withSceneGraph`/`withSceneGraphM`, both
  unreferenced — a bracket abstraction nothing brackets with.
- `Engine/Input/Thread/Mouse.hs` exports `uiDragThresholdPx` (a tuning
  constant) that only it reads.
- `Engine/Preview/Discovery.hs` exports `isSupportedTextureFile` and
  `sortEntries` with no consumer — notable because CLAUDE.md advertises hspec
  coverage for "the pure discovery/labeling/ordering/containment logic", and
  the ordering helper is not what the tests reach for.

---

## Batch 6 — `app/` (swept 2026-07-25)

1275 lines across 10 modules; all are in the cabal `other-modules` list. The
problems are boot-mode duplication and CLI flags that silently do nothing.

### CH-58. `--seed`, `--worldSize`, and `--plates` are silently ignored outside `--dump`
`Main.hs` parses all three, normalises them (`normalizeWorldSize`,
`normalizePlateCount`, `defaultPlatesFor`), and then hands them to exactly one
dispatch target:

```haskell
runDump      ∷ DumpLayers → Int → Int → Int → (Int,Int,Int,Int) → IO ()
runHeadless  ∷ BootProfile → Maybe Int → IO ()
runGraphical ∷ BootProfile → Maybe Int → IO ()
runOffscreen ∷ BootProfile → Maybe Int → Maybe (Int,Int) → IO ()
runPreview   ∷ (Text, Maybe Text) → Maybe PreviewBrowse → Maybe Int → IO ()
```

So `cabal run synarchy -- --headless --seed 42 --worldSize 256 --plates 5`
accepts every flag, computes with them, and discards all three without a word.
(Headless world generation goes through Lua's `world.init` instead.)

Compounding it, the normalisation warnings are themselves gated on dump mode:

```haskell
when (isJust mDump ∧ worldSize /= rawWorldSize) $ hPutStrLn stderr …
```

— so even in the one mode where a flag *is* honoured the feedback is
conditional, and in the five where it isn't there is no diagnostic at all.

Fix: reject unsupported flags per boot mode with a clear error, or thread them
through. Silently accepting a flag that does nothing is the worst of the three
options.

### CH-59. `allLayers` is not all layers
`App/Cli.hs:38`:

```haskell
allLayers ∷ DumpLayers
allLayers = DumpLayers True True True True True False
```

The `False` is `dlSlope`. Its own haddock opens "**Default** layers (when --dump
has no =value)" and then explains slope is deliberately excluded — so the doc
and the name disagree in the first two words. Rename to `defaultLayers`.

Also: six bare positional booleans, unreadable without counting fields against
the record declaration (the same anti-pattern as `AssetConfig 100 100 True
True`, CH-18). Use field syntax.

### CH-60. The preview category list is duplicated as an error-message string
`App/Cli.classifyPreviewCategory` holds the authoritative lists:

```haskell
    simpleCategories  = ["icons", "items", "ui", "world"]
    groupedCategories = ["units", "flora", "buildings", "structures"]
```

Neither is exported. `Main.hs:92` re-states them as prose:

```haskell
    ⧺ " (expected one of: icons, items, ui, world, units, "
    ⧺ "flora, buildings, structures)"
```

CLAUDE.md pins this as a contract — "the unknown-category error message lists
exactly this set, no compatibility aliases" — and `tools/preview_cli_probe.py`
is CI-eligible, so a drift here fails CI *late* rather than not compiling.
Export the lists and build the message from them.

### CH-61. Five boot modes hand-copy the same error-recovery block
`Graphical`, `Headless`, `Offscreen`, `Dump`, and `Preview` each end with the
identical shape — including this comment reproduced **verbatim in all five**:

```haskell
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
```

Each copy re-lists every worker thread to shut down, in an order that is an
undocumented invariant in some copies and documented in others. `Offscreen`'s
happy path explains it:

> Combat first: wound ticks enqueue UnitKill/UnitCollapse onto the unit queue,
> so the producer has to stop before the consumer …

— but its own error path re-implements the sequence separately, and the other
four copies carry no such note. A missed thread or a wrong order in one of five
hand-maintained copies is invisible.

The `env'` patch is duplicated too — this ten-line block appears in
`Graphical`, `Headless`, and `Offscreen`:

```haskell
  let env' = case mPort of
        Just p  → env { engineConfig = (engineConfig env)
                          { ecDebugPort = p, ecBootProfile = bootProfile } }
        Nothing → env { engineConfig = (engineConfig env)
                          { ecBootProfile = bootProfile } }
```

Both branches differ only in whether one field is set — `maybe id` collapses
it to one line.

Fix: `App.Boot` with `withBootConfig` and `shutdownAllWorkers`, used by all
five.

### CH-62. `shutdownEngine`'s five positional parameters are mutually swappable
```haskell
shutdownEngine ∷ Maybe Window → Maybe ThreadState → Maybe ThreadState
               → ThreadState → ThreadState → EngineM ε σ ()
```

Two adjacent `Maybe ThreadState` (unit, world) and two adjacent `ThreadState`
(input, lua). Swapping either pair compiles silently and tears threads down in
the wrong order — precisely the invariant CH-61 shows the codebase already
knows is load-bearing. The call site reads:

```haskell
shutdownEngine Nothing (Just unitThreadState)
               (Just worldThreadState) inputThreadState luaThreadState
```

`Nothing` for what, at a glance? Fix: a record.

### CH-63. Three separate main loops
`Engine.Loop.mainLoop`, `Engine.Loop.mainLoopOffscreen` (both in `Loop.hs`),
and `Engine.Loop.Headless.headlessLoop`. The first two are already
near-duplicates sharing `runGatedByCaptureLock` (CH-24); the third lives in a
different module. Combined with CH-61's five boot paths, the "start engine,
tick, shut down" story is told six times.

Worth one design issue: one loop parameterised by mode (poll events? present?
pace frames?), or an explicit statement of why three are irreducible.

### CH-64. `--dump` emits three fields that no documentation mentions
Actual output includes `waterTableZ`, `waterTableSummer`, and
`waterTableWinter` under the `terrain` layer. CLAUDE.md's dump field table —
presented as the contract, and what the audit tooling is written against —
lists only:

> \| `terrainZ`, `surfaceZ` \| terrain \| Raw terrain and max(terrain, fluid) \|

No `tools/*.py` reads the three water-table fields. They ride in every default
`--dump` unread and undocumented.

This also puts pressure on `App/Cli.hs`'s claim that a bare `--dump` "stays
byte-identical to historical output" — true for `slope` being opt-in, but the
terrain layer grew three fields since.

Fix: document them in CLAUDE.md's table, or drop them behind an opt-in layer
like `slope`.

### CH-65. `App/Dump.hs` hand-concatenates JSON
`dumpTilesJSON` builds output with ~100 lines of string concatenation —
`",\"terrainZ\":" ⧺ show terrainZ` — plus its own `boolStr`, `fluidTypeStr`,
and `iceModeStr` encoders. The package already depends on `aeson` (every
`Engine/Asset/Yaml*` module uses it).

Nothing escapes strings, which is safe *today* only because every emitted value
is numeric or a closed enum. The first string-valued field (a material name, a
location id) introduces an injection bug in a format the baselines and audit
tools parse.

Also here: `waitForInit` and `waitForChunks` (lines 234-275) are the same
function twice — identical `go n` recursion, identical timeout arithmetic, and
the second's comment literally defers to the first ("see 'waitForInit'"). Only
the readiness predicate and two log strings differ.

### CH-66. Primitive-obsession in the dump signatures
```haskell
runDump      ∷ DumpLayers → Int → Int → Int → (Int,Int,Int,Int) → IO ()
dumpTilesJSON ∷ DumpLayers → MaterialRegistry → Int → ClimateState
              → WorldTileData → Int → Int → Int → Int → BS.ByteString
```

`runDump`'s three bare `Int`s are seed, worldSize, plateCount — swap any two
and it compiles and generates a different world. `dumpTilesJSON` takes the
region as four loose `Int`s even though `parseRegion` already produced it as a
tuple, so the tuple is destructured purely to be re-spread positionally.

### CH-67. `parseRegion` silently substitutes a default for malformed input
```haskell
parseRegion ("--region":s:_) =
    case map reads (splitOn ',' s) of
        [[(cx1,"")],[(cy1,"")],[(cx2,"")],[(cy2,"")]] → (cx1, cy1, cx2, cy2)
        _ → (-8, -8, 8, 8)
```

`--region garbage` dumps the default 16×16 chunk region and reports nothing.
Because the return type is a bare tuple rather than `Maybe`, the caller cannot
distinguish "user asked for the default" from "user's input was rejected" — so
a typo in a long-running dump silently produces the wrong data.

Contrast `parseSize`, which correctly returns `Maybe`, and
`App.ResourceRoot`/`parsePreview`, which explicitly treat a bare flag as an
error rather than an absence. `parseRegion` is the odd one out.

### CH-68. Two module haddocks enumerate the boot modes and both are stale
- `App/Cli.hs:1` — "shared by every boot mode (graphical, headless, dump)"
- `App/Exception.hs:2` — "shared by every `runEngineM` call site (graphical,
  headless, dump)"

There are six: graphical, headless, offscreen, dump, preview, language-report.
`guardNativeExceptions` is in fact used by five of them. Enumerating a list
that grows is the failure mode; say "every boot mode" and stop.

### CH-69. Minor `app/` defects for one cleanup issue
- `Main.hs` — `fromMaybe 8008 port` appears **six times**. Given CLAUDE.md
  warns agents at length that 8008 collides with the user's GUI instance, the
  default port deserves one named constant.
- `Main.hs` — `main` is a single 110-line function: env setup, parsing,
  normalisation, two conditional warnings, and a five-level nested
  `if`/`case`/`case`/`case`/`case` dispatch. A `BootMode` ADT resolved once,
  then dispatched once, would make the precedence rules (`--language-report`
  beats `--dump` beats `--preview` beats `--offscreen` beats `--headless`)
  legible instead of inferred from nesting depth.
- `Main.hs:63,67` — ASCII `/=`, covered by the CH-49 sweep.
- `App/Cli.hs:48` — `drop 7 a` to strip `"--dump="`; a magic length.
- `App/Headless.hs` — the function haddock repeats the module haddock and is
  stale where the module one is right: it says "Starts Lua, world, and unit
  threads" while the code starts five (Lua, world, unit, sim, combat). Delete
  the duplicate.
- `App/Cli.hs:175` — a hand-rolled `splitOn`; fine as the only copy, but it is
  the third string-splitting idiom in the file alongside `break (≡ '/')` and
  `isPrefixOf`+`drop`.

---

## Batch 7 — `src/World/Save`, `src/World/Load`, `src/Engine/Save` (swept 2026-07-25)

9848 lines across 22 modules; all correctly listed in the cabal file. This is
the youngest large subsystem in the tree (the #756-#768 overhaul) and it shows:
the abstractions are sound but under-shared, so the same logic is written
several times.

See also **CH-40** (`currentSaveVersion`'s 296-line changelog) and **CH-23**
(eight of the tree's seventeen oversized modules live here).

### CH-70. The save system's item enumeration is implemented three times
The recursive walk over every item instance in a session — the basis of both
the id-allocator check and load-time integrity validation — exists in three
places:

| Module | Function | Returns |
|---|---|---|
| `World/Save/Snapshot.hs:299` | `flattenItemInstanceIds` | `[Word64]` |
| `Engine/Scripting/Lua/API/Save.hs:417` | `flattenItemInstanceIds'` | `[Word64]` |
| `World/Save/Types.hs:987` | `flattenItemInstances` | `[ItemInstance]` |

The first two are **character-identical apart from the prime**, and so is the
cascade built on them. `Snapshot.hs:308-323` and `API/Save.hs:455-468` are the
same function twice — same `pageItemIds` / `unitItemIds` / `buildingItemIds`
structure, same traversal of ground items, unit
`uisInventory`/`uisEquipped`/`uisAccessories`, and building
`bisMaterialsDelivered`/`bisStorage` — differing only in whether the page
accessors are `pgs*` (`SessionSnapshot`) or `wps*` (`SaveData`).

The third admits the duplication in its own comment: "mirrors
`World.Save.Snapshot.flattenItemInstanceIds`".

This is the highest-consequence duplication found so far. Adding a new item
container to `UnitInstanceSnapshot` requires editing all three; miss one and
integrity validation silently stops seeing those items — no error, no test
failure, corrupt saves accepted.

Fix: one traversal, parameterised over the page accessor (or run after the
`SaveData` to `SessionSnapshot` adaptation so only one shape exists).

### CH-71. `WorldPageId` has no accessor, so ten sites hand-write one
```haskell
newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)
    deriving newtype (Hashable, Serialize)
```

No field label. Consequence — this exact line appears **ten times** across
`World/Save/Types.hs` and `World/Thread/Helpers.hs`:

```haskell
  where unWorldPageId (WorldPageId t) = t
```

Adding `{ unWorldPageId :: Text }` to the newtype deletes all ten and costs
nothing (the `deriving newtype` clauses are unaffected).

### CH-72. Nine near-identical `Missing*Ref` types, misplaced in `Types.hs`
`World/Save/Types.hs:925-1316` — roughly 390 lines, 30% of the file — is nine
repetitions of one shape:

`MissingDefRef`, `MissingItemDefRef`, `MissingRecipeRef`,
`MissingBillOutputItemRef`, `MissingConstructDefRef`, `MissingMaterialRef`,
`MissingFloraRef`, `MissingLocationRef`, `MissingInfectionRef`

Each contributes a `data Missing<X>Ref` (a kind/source `Text`, a
`WorldPageId`, an entity id, an unresolved name), a `renderMissing<X>Ref`
producing the same "&lt;thing&gt; #N on page 'P' references unknown &lt;kind&gt;
'&lt;name&gt;'" sentence, a `missing<X>References` scan, and its own copy of
CH-71's `unWorldPageId`.

Two problems beyond the repetition:

1. **It is in the wrong module.** `World/Save/Integrity.hs` (477 lines) is the
   dedicated integrity module. Reference validation is split across two files
   with no stated rule for which half goes where — and
   `Engine.Scripting.Lua.API.Save` has to import 18 names from `Types.hs`
   (nine types plus nine renderers) to do one job.
2. **It is why `Types.hs` is the largest module in the tree.** 296 lines of
   `currentSaveVersion` comment (CH-40) plus 390 lines of this is over half
   the file.

Fix: one `MissingRef { mrKind, mrSource, mrPage, mrEntity, mrName }` with one
renderer, moved to `Integrity.hs`.

### CH-73. `serializeCodec` cannot express the migration the component system exists for
```haskell
serializeCodec :: S.Serialize d => ComponentId -> Word32 -> Bool -> [ComponentId] -> ...
serializeCodec cid ver req deps toDTO migrate validate = ComponentCodec
    { ccId        = cid
    , ccVersion   = ver
    , ccInputVers = [ver]        -- accepts only its own version
    ...
```

`ccInputVers` is hardcoded to `[ver]`, so a component built with the helper can
never decode an older payload. Per-component schema evolution is the stated
reason the envelope format exists (CLAUDE.md: "Component evolution =
per-component schema version bumps + explicit migrations from frozen vN DTOs")
— so the convenience constructor fails at precisely the case it was built for.

Result: two divergent idioms, split by whether a component has ever needed to
evolve. **7 codecs use `serializeCodec`; 5 hand-roll the full record.** The
workaround is documented twice, in two separate comments:

> `unitSimCodec` … hand-rolled `ComponentCodec` (mirrors
> `craftBillsCodec`/`powerNodesCodec` — `serializeCodec` has no real
> multi-version dispatch) now that this component needs v1 to v2 migration too.

Every component that has actually evolved abandoned the helper; the ones still
using it are one schema change away from doing the same.

The call site is also opaque:

```haskell
buildingsCodec = serializeCodec
    buildingsComponentId 1 True [worldPagesComponentId, coreSessionComponentId]
```

`1` is the version and `True` is "required"; neither is legible without opening
the definition.

Fix: give `serializeCodec` an input-version list (or a `migrate` variant), and
name the version/required arguments.

### CH-74. `Component/Entities.hs` is five components in one 1139-line module
It defines five independent codecs:

| Line | Codec | What it is |
|---:|---|---|
| 243 | `buildingsCodec` | buildings |
| 476 | `unitsCodec` | units |
| 662 | `unitSimCodec` | per-unit sim state |
| 921 | `craftBillsCodec` | craft bills |
| 1110 | `powerNodesCodec` | power nodes |

69 top-level declarations. Craft bills and power nodes are not entities, so the
module name describes three of its five contents. Each codec is a
self-contained group of `XDTO` + `toXDTO` + `fromXDTO` (plus `XDTOv1` +
`migrateXDTOv1` where versioned), so the five-way split is mechanical and
already drawn.

This is the largest single win available against CH-23.

### CH-75. `tshow` is invented four times, while 570 sites don't use it
```haskell
tshow :: Show a => a -> Text
```
is defined independently in `World/Save/Component/Entities.hs:164`,
`World/Save/Component/Page.hs:148`, `World/Save/Component/Session.hs:52`, and
`Sim/Thread.hs:313` — three of them siblings in the same directory.

Meanwhile the un-abstracted `T.pack (show x)` / `T.pack $ show x` appears
**570 times** across `src/` + `app/`.

Fix: one `tshow` in `UPrelude` (which exists to carry exactly this kind of
shared vocabulary), delete the four local copies, and optionally sweep the 570
call sites in the same pass as the CH-49 operator normalisation.

### CH-76. Envelope compat is named after the epic's internal phase letters
`World/Save/Envelope.hs` defines `b1LegacyIds`, `b2Ids`,
`decodeB2SessionMetadata`, and `decodeLegacySessionMetadata`. "B1" and "B2" are
the save-overhaul epic's internal milestone labels (#759 = B1, #761 = B3) —
meaningless to a reader and unresolvable without pulling the epic.

This is CH-15's review-round archaeology promoted into the **API surface**,
which is worse: a comment can be deleted, an exported name has call sites.
None of the four is referenced outside `Envelope.hs`, so renaming is free
(they are also over-exports per CH-54).

Rename to what the formats are — e.g. `preLuaComponentIds`,
`flatSessionComponentIds` — or to the save versions they correspond to.

25 review-round comments also remain in this subsystem, densest in
`Component/Entities.hs` (6), `Save/Types.hs` (4), and `Load/Publish.hs` (4).

### CH-77. `LuaComponentSpec` is a bare 4-tuple
```haskell
type LuaComponentSpec = (Text, Word32, Bool, BS.ByteString)
```

A type alias over an anonymous tuple gives no safety and no documentation: the
components are (presumably) id, version, required, payload, but nothing says
so, and `Word32`/`Bool` are positionally interchangeable with nothing to catch
a swap. It crosses the Haskell/Lua persistence boundary — the place where a
silent field transposition is hardest to detect.

Same defect class as `inpPendingUIClick` (CH-27) and `runDump`'s three bare
`Int`s (CH-66). Should be a record.

### CH-78. `Envelope.hs` is 860 lines beside an `Envelope/` directory
`World/Save/Envelope.hs` (860) sits next to `Envelope/Types.hs` (173) and
`Envelope/Codec.hs` (223) — so the parent module is four times the size of the
subdirectory it appears to head, and is not a facade for it.

Same for `Component.hs` (347) beside `Component/` (2976 lines) and
`Snapshot.hs` (368) beside `Snapshot/Adapter.hs` (163). This is CH-56's
unstated convention gap, and this subsystem is where it bites hardest: given a
save concern, there is no rule saying whether it lives in `X.hs`,
`X/Types.hs`, or `X/Something.hs`.

---

## Batch 8 — the worldgen pipeline (swept 2026-07-25)

`World/Generate` (3926), `World/Geology` (8317), `World/Hydrology` (3276),
`World/Fluid` (4493), `World/Plate` (969), `World/Magma` (1395),
`World/Weather` (1332), `World/ZoomMap` (1018) plus the top-level `World/*.hs`.
The fluid surface is the densest and, as suspected, holds the most gaps.

### CH-79. An abandoned river redesign is still compiled, plus a design doc that reads as current
`src/World/River/Graph.hs` (257 lines) — `RiverGraph`, `RiverRoute`,
`RiverNode`, `buildRiverGraph`, `classifyMouth` — is listed in
`synarchy.cabal` and **imported by no production module**.

**Correction (2026-07-25):** an earlier version of this entry claimed it was
untested. That was wrong — `test-headless/` does exercise `classifyMouth` and
`RiverGraph`, so the "exposed for tests" comment is honest and the module has
real coverage. `buildRiverGraph` has no reference anywhere.

The finding stands in its narrower form: a fully-built, tested model that no
production code path uses, kept compiling indefinitely.

Its design brief, `docs/river_rework.md` (450 lines), opens "This document
describes a new river runtime model for Synarchy" and describes the current
system as the thing to be replaced — with no status marker saying the work
stopped. `docs/history/README.md` explicitly moved it *up* out of the history
folder as "design reference, not a superseded audit", so a reader is told it is
live.

Decide and record: adopt, or archive the doc to `docs/history/` and delete the
module. Right now it is a third river model competing with the four in CH-80.

### CH-80. "River" logic lives in four unrelated namespaces
| Path | Role |
|---|---|
| `World/Fluid/River/` (7 modules, 1245 lines) | identify rivers from terrain |
| `World/Hydrology/River/Carving.hs` (285) | carve river beds |
| `World/Geology/Timeline/River/` + `RiverTrace/` (9 modules, ~1200) | timeline-era river evolution |
| `World/River/Graph.hs` (257) | the abandoned model (CH-79) |

Plus `World/Hydrology/Simulation/Flow.hs` (396) doing flow accumulation. Nothing
names the split, so "where does river X live?" has no answer short of grepping
five trees. Ocean is similarly scattered across `World/Fluid/Ocean.hs`,
`World/Fluid/OceanMask.hs`, `World/Fluid/Lake/Identify/Ocean.hs`, and
`World/Ocean/Types.hs`.

Not necessarily a refactor — but the pipeline's stage boundaries (timeline
evolution → identification → carving → per-chunk composition → sim) should be
stated once, in a doc or module headers, and the directories named for them.

### CH-81. `World.Fluids` and `World.Fluid.*` differ by one letter
`src/World/Fluids.hs` is a 20-line facade re-exporting from `World.Fluid.*`.
`import World.Fluids` and `import World.Fluid.Types` sit one keystroke apart and
resolve to different modules.

The facade is also effectively bypassed: **3 modules import `World.Fluids`** (all
for the same two ocean predicates) against **97 imports of `World.Fluid.*`
directly**. Either promote it to the real entry point or delete it; as a
one-letter-different module used by 3% of consumers it is a trap with no payoff.

### CH-82. The per-tile fluid-surface fold is written five times in one file
`World/Generate/Chunk/Fluid.hs` contains the same "fold this chunk's entries
into a per-tile lowest-wins surface vector" loop five times:

| Lines | Binding | Source table |
|---|---|---|
| 82-98 | `lakeSurfMap` | `gtWorldLakes` |
| 104-116 | `riverSurfMap` | `gtWorldRivers` |
| 123-136 | `lavaSurfMap` | `gtWorldLavaPools` |
| 191-199 | (in `chunkWaterSurfMap`) | `gtWorldLakes` again |
| 200-208 | (in `chunkWaterSurfMap`) | `gtWorldRivers` again |

Each is `VU.create` → `VUM.replicate chunkArea minBound` → `V.forM_
(lakesInChunk …)` → `forM_ [0 .. chunkArea-1]` → read/compare/write, with the
same defensive lowest-wins comment restated three times.

The duplication is load-bearing: `chunkWaterSurfMap` feeds the basalt-cap
decision and its own haddock says the merge "mirrors `composeFluidMap`". If the
two drift, a magma chamber's cap decision disagrees with where water actually
is — lava emitted into a lake.

One `perTileLowestSurface ∷ WorldLakes → ChunkCoord → VU.Vector Int` (plus a
river variant) replaces all five.

### CH-83. The river-flat surface rule is written four times, and its comment overstates its own coverage
`mkSurfaceMap`'s comment names the other copies:

> Same rule lives in Sim/Thread.hs::writeDirtyFluids (sim writeback) and
> World/Edit/Apply.hs::applyEdit (player edits).

Actual sites: `Generate/Chunk/Fluid.hs:520`, `Sim/Thread.hs:388`,
`Edit/Apply.hs:156`, `Edit/Apply.hs:169` — **four**, with `Edit/Apply` holding
two.

More importantly, `applyEdit` applies the rule in only 2 of its 4
fluid-touching branches:

- `WeSetFluidTile` (156) — applies it
- `WeSetFluidSnapshot` (169) — applies it
- `WeAddTile` (134-135) — uses `max newTopZ (fcSurface fc)`; **safe by
  construction**, because the guard above it (`newTopZ ≥ fcSurface fc → Nothing`)
  means the fluid only survives when `newTopZ < fcSurface`, so `max` equals the
  fluid surface anyway
- `WeDeleteTile` (77-78) — uses `max newTopZ (fcSurface fc)` with **no such
  guard**

The dig path is the gap. Digging preserves whatever fluid was there
(`Just _ → curFluid`), so on a River tile whose terrain protrudes above
`fcSurface` — which is precisely the case `mkSurfaceMap` exists to hide —
`newTopZ = oldTopZ - 1` can still exceed the river surface and `max` renders the
protrusion. Generation, sim writeback, and both `WeSetFluid*` paths would render
it flat.

Reachability is narrow (needs a protruding river tile that gets dug), so this is
an inconsistency to confirm-and-fix rather than a known visible bug. The durable
fix is one shared `renderedSurface ∷ Int → Maybe FluidCell → Int` used by all
four.

### CH-84. `floorDivCS` is hand-rolled five times, with an unreachable branch, next to a correct helper
```haskell
floorDivCS a = let (q, r) = a `divMod` chunkSize
               in if r < 0 then q - 1 else q
```

appears verbatim in `World/Generate/Chunk/Fluid.hs:290`, `World/Magma/Pool.hs:171`,
`World/Magma/Field.hs:65`, `World/Magma/Init.hs:272`, and
`World/Magma/Lookup.hs:32`.

Two problems:

1. **The guard can never fire.** Haskell's `divMod` already floors — `div`
   rounds toward negative infinity and `mod` takes the sign of the divisor, so
   with `chunkSize > 0` the remainder is never negative. `divMod (-1) 16 =
   (-1, 15)`. The `r < 0` correction is dead in all five copies; it is the guard
   you would need with `quotRem`, pasted onto `divMod`.
2. **`World.Generate.Coordinates.globalToChunk` already exists** and does this
   correctly (`div` + an explicit `floorMod`). Five modules reimplemented it.

### CH-85. `moSurface` is always empty, its lookup can never succeed, and two comments say it drives lava placement
`MagmaOverlay.moSurface ∷ HM.HashMap (Int, Int) FluidCell` is written at exactly
two sites, both `HM.empty` (`Generate/Chunk/Fluid.hs:327`, `Magma/Init.hs:362`),
and read at one: `World/Magma/Lookup.hs:64`

```haskell
    case HM.lookup (gx, gy) (moSurface overlay) of
```

which therefore always misses. The field's own module acknowledges it —
`Generate/Chunk/Fluid.hs:174`: "the magma overlay's `moSurface` is no longer
populated (caps only)" — while two other comments still describe it as live:

- `World/Chunk/Types.hs:126` — "overlay's `moSurface` map drives lava placement in …"
- `World/Generate/Chunk.hs:301` — "decision: above water → lava cell in `@moSurface@`; sub-sea …"

So the codebase simultaneously documents this field as dead and as the mechanism
for surface lava. Surface lava actually comes from `gtWorldLavaPools`. Delete the
field, its lookup branch, and fix the two comments.

### CH-86. `composeFluidMap`'s haddock documents a parameter it does not have
```haskell
-- The 'waterTableMap' arg is no longer used for surface placement;
-- it stays computed and stored on 'LoadedChunk' so that the
-- subsurface dig path can still ask "is this buried tile saturated?"
composeFluidMap ∷ WorldGenParams → ChunkCoord → VU.Vector Int
                → V.Vector (Maybe FluidCell)
```

Three arguments, none of them a water-table map. The paragraph survived the
signature change that removed it.

### CH-87. 43 modules carry `-fprof-auto`, defeating the cabal's `-fprof-late` profiling strategy
`synarchy.cabal` defines the profiling contract in one place:

```
flag profile
    description: Cost-centre profiling (-fprof-late on top of the prod -O2);
…
    if flag(profile)
      ghc-options: -fprof-late
```

and states the policy explicitly (#635): *"any change to profile options belongs
HERE, not in a component stanza."*

Yet **43 production modules** open with `{-# OPTIONS_GHC -fprof-auto #-}` —
essentially all of `World/Plate/`, `World/Geology/Timeline/`, `World/ZoomMap/`,
`World/Thread/`, plus `Sim/Thread.hs`, `World/Generate/Timeline*`,
`World/Hydrology/River/Carving.hs`, and `World/Fluid/Lake/Graben.hs`.

`-fprof-auto` and `-fprof-late` are not the same tool: `-fprof-auto` inserts
cost centres **before** optimisation (inhibiting it and changing the generated
code), which is exactly what `-fprof-late` exists to avoid. So the 43 modules
the profiling recipe targets — the worldgen hot path — are the only ones that
*don't* get the intended post-optimisation measurement.

They are inert in a non-profiling build (GHC ignores `-fprof-auto` without
`-prof`), so this costs nothing in production; it corrupts the profile, which
`docs/history/worldgen_timeline_profile_2026-07.md` is built on.

Fix: delete all 43 pragmas and let the flag do its job, or document why these
files need pre-optimisation instrumentation.

### CH-88. Four dead bindings that `Strict` actually evaluates
`World/ZoomMap/Cache/BuildPixels.hs:73-80` (a `{-# LANGUAGE Strict #-}` module):

```haskell
_chunkOceanN = isOceanChunk oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
_chunkOceanS = …   _chunkOceanE = …   _chunkOceanW = …
```

All four are unused — the underscore prefix suppresses `-Wunused-binds` rather
than the code being removed. Under `Strict`, let-bindings are forced, so these
run for **every chunk** of every zoom-cache build.

They are not cheap: `hasAnyOceanFluid` scans a 5×5 chunk neighbourhood (25
wrapped `HashSet` lookups), so the four bindings cost up to ~104 lookups per
chunk that are discarded. The zoom cache is rebuilt per world init and load.

### CH-89. Material IDs are a hardcoded Haskell table mirroring `data/materials/*.yaml`
`src/World/Material.hs` hardcodes 74 numeric ids:

```haskell
matGranite = MaterialId 1
matDiorite = MaterialId 2
matGabbro  = MaterialId 3
```

`data/materials/*.yaml` independently declares the same 73 materials with
explicit ids (`- id: 1 / name: granite`). Both tables run to id 255. Nothing
verifies they agree — no test, no audit tool.

They **are** in sync today (the only difference is `matAir = MaterialId 0`,
correctly absent from the content catalogue). The hazard is structural: a
designer renumbering or inserting a YAML material silently repoints every
Haskell constant above it, and **51 of the 74 constants have no Haskell
reference at all**, so a drift in those is invisible to every code path as well.

Same class as CH-31 (the five-way `16384`) but with content authors in the
loop. Fix: generate the constants from the YAML, or add a startup/CI check that
every `mat*` constant matches its YAML `id`/`name` pair.

### CH-90. 194 unreferenced exports in `src/World/`
Same scan as CH-54. Largest concentrations, and what they mean:

| Count | Module | Kind |
|---:|---|---|
| 51 | `World/Material.hs` | unreferenced id constants (see CH-89) |
| 18 | `World/Vegetation.hs` | veg id constants + `selectVegetation` (used internally) |
| 17 | `World/Save/Component/Entities.hs` | DTO `to*`/`from*`/`migrate*` reached only via their codecs |
| 7 | `World/Geology/Volcano.hs` | `applyCaldera`/`applyFissure`/… — dispatched internally by `applyVolcanicFeature`; only it and `perturbDist` need exporting |
| 7 | `World/Magma/Init.hs` | internal geometry helpers (`padBox`, `unionBoxes`, `squareAt`, …) |
| 6 | `World/Fluid/Internal.hs` | 6 of a 98-line module |
| 6 | `World/Generate/Strata.hs` | `applyDelta`, `applyEventDelta`, … |

Most are over-exports (kind (b) in CH-54) rather than dead code — verified for
the volcano and vegetation clusters, which are live internally. The effect is
the same: no module in the pipeline has a meaningful public surface, so nothing
can be refactored without a whole-tree grep.

Genuinely dead here: `World/Log.hs`'s five exports (CH-28 — the module is not
in the cabal at all) and `World/River/Graph.hs`'s three (CH-79).

### CH-91. Minor worldgen defects for one cleanup issue
- **Parallel modules, swapped prefixes.** `Lake/Identify/ChunkIndex.hs` exports
  `buildChunkIndex` + `buildLakeCarveIndex`; the parallel
  `River/Identify/ChunkIndex.hs` exports `buildRiverChunkIndex` +
  `buildCarveDeltaIndex`. Each qualifies the opposite one of the pair.
- `World/Material.hs` (291 lines) sits beside `World/Material/` (one 35-line
  module) — CH-56's facade ambiguity again, as do `World/Slope.hs` (47) beside
  `World/Slope/` (807), `World/Plate.hs` (64) beside `World/Plate/` (969), and
  `World/Fluid/Seabed.hs` (423) beside `World/Fluid/Seabed/Types.hs`.
- `smoothIslandColumns` (`Chunk/Fluid.hs:447`) reads terrain from the immutable
  `terr` while writing to the mutable `mTerr` in the same loop. Safe today
  (smoothed tiles gain fluid and are skipped on later passes), but the mixed
  aliasing is a trap for the next edit.
- `smoothIslandColumns` hand-rolls a frequency count (`countBy`/`uniques`/`hits`)
  over a 4-element list with an O(n²) `foldr` dedup.
- `World/Ocean/Types.hs` (23 lines) is a whole namespace for `OceanMap`,
  `OceanDistMap`, and one accessor, while ocean *logic* lives in three other
  places (CH-80).

---

## Batch 9 — `World/Thread`, `World/Render`, `World/ZoomMap` (swept 2026-07-25)

5226 + 4453 + 1018 lines. **These three trees are the cleanest large area swept
so far**: only 11 unreferenced exports between them (against 194 in the rest of
`World/` and 97 in `Engine/`), `World/Thread.hs` and `World/Thread/Command.hs`
are a tidy loop + flat dispatch, and `World/Render.hs` is a proper two-export
facade. The findings below are correspondingly narrower.

### CH-92. `baseTileW` / `baseTileH` are defined identically in eight modules
```haskell
baseTileW = fromIntegral (gcTilePixelWidth  defaultGridConfig)
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)
```

Verbatim in: `Unit/HitTest.hs`, `Unit/Render.hs`, `Structure/Render.hs`,
`Building/Render.hs`, `Building/HitTest.hs`,
`World/Render/GroundItemQuads.hs`, `World/Render/BloodQuads.hs`,
`World/Render/FloraQuads.hs`.

These are *the* two constants of an isometric renderer — every quad, hit test,
and sprite placement derives from them. Eight private copies means eight places
to check when anything about tile geometry changes, and they are split across
four subsystems that would not obviously be searched together.

`World/Render/FloraQuads.hs:20` additionally annotates the derived values:

```haskell
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)   -- 96
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)  -- 64
```

A hardcoded comment of a config-derived number, in one copy of eight — it goes
stale the moment `defaultGridConfig` changes, and nothing will flag it.

Fix: export them once (`World.Grid` already owns `defaultGridConfig`).

### CH-93. `World.ZoomMap` is a facade that inverts its own dependency direction
```haskell
-- | Thin facade – re-exports the public entry points so that
--   existing call sites ('World.Render') need no import changes.
module World.ZoomMap
    ( generateZoomMapQuads, generateBackgroundQuads
    , buildZoomCache, buildZoomCacheWithPixels ) where

import World.ZoomMap.Cache          (buildZoomCache, buildZoomCacheWithPixels)
import World.Render.Zoom.Quads      (generateZoomMapQuads)
import World.Render.Zoom.Background (generateBackgroundQuads)
```

So `World.Render` imports `World.ZoomMap`, which imports `World.Render.Zoom.*`.
Half of what the `ZoomMap` facade exports is *rendering* code that lives under
`Render`. Its own haddock states the reason plainly: it exists "so that existing
call sites need no import changes" — an import-churn shim, kept permanently,
that now makes the module graph read backwards.

Fix: have `World.Render` import `World.Render.Zoom.*` directly and let
`World.ZoomMap` cover only the cache.

### CH-94. Cross-chunk render lookups don't wrap at the world seam, but the chunk map is keyed wrapped
`World/Render/Quads.hs:82-85`:

```haskell
fluidMapLookup cc = case HM.lookup cc (wtdChunks tileData) of …
terrMapLookup  cc = case HM.lookup cc (wtdChunks tileData) of …
```

Raw `HM.lookup` on whatever coordinate the caller computed. Both callers step
one chunk outward without canonicalising:

- `World/Render/SideDecoQuads.hs:94-101` (`neighborCell`) — `ChunkCoord (cx±1) cy`
- `World/Render/WaterSlope.hs:38-45` (`waterSlopeAt`) — the same construction

But `wtdChunks` is keyed by `lcCoord`, and chunks are stored under the
canonical wrapped coordinate (`World/Thread/ChunkLoading.hs:78-82` applies
`wrapChunkCoordU`). And `World/Render/Quads.hs:394` — in the *same file* —
does wrap before looking up:

```haskell
cc@(ChunkCoord ccx ccy) = wrapChunkCoordU worldSize ccRaw
```

So at the u-axis wrap seam the neighbour lookup misses, both call sites take
their documented "neighbour chunk isn't loaded" conservative branch, and water
side faces and water slope tiles are silently not drawn along the seam. Both
comments describe that branch as covering an *unloaded* neighbour, which is not
the case here.

Needs confirmation at the seam before fixing (the memory note on wrap-seam
topology flags this exact hazard class), but the inconsistency inside one file
is plain either way.

### CH-95. Two zoom namespaces with a real but unstated split
`World/ZoomMap/` (8 modules) builds the zoom **cache**: entries, per-chunk
pixels, the texture atlas, the colour palette, classification, ice noise.
`World/Render/Zoom/` (10 modules) **renders** from it: background, bake, quads,
cursor, icons, climate colours, view bounds.

That data-vs-render split is principled — but nothing states it, and the names
actively work against it: `ZoomMap/Cache/Pixels.hs` generates pixels while
`Render/Zoom/Bake.hs` bakes entries; both read as preparation. Combined with
CH-93's inverted facade, "where does zoom concern X live?" has no answerable
rule.

One sentence in each tree's top module would fix it.

### CH-96. `docs/history/README.md` justifies an archive with a false claim
It marks the 2026-04 fluid audit superseded because it

> References functions that no longer exist (`drainOceanLakes`,
> `waterSideFaceQuads`).

`waterSideFaceQuads` **does** exist and is live —
`World/Render/SideDecoQuads.hs:30`, imported and called from
`World/Render/Quads.hs:234`.

What actually happened is better news: the audit's specific complaint
("`waterSideFaceQuads` only checks in-chunk neighbors… water cliffs at chunk
boundaries don't get side faces rendered") was **fixed** — `neighborCell` now
resolves across chunk boundaries and says so in a comment. So the conclusion
(superseded) is right and the stated reason is wrong.

This matters because the README is the document telling future readers whether
an archived audit's findings are still live. A wrong reason here is how a
still-open finding gets dismissed; note that this same function retains the
separate seam defect in CH-94.

### CH-97. Duplicate module basenames across the render stack
| Basename | Modules |
|---|---|
| `Camera` | `Engine/Graphics/Camera.hs`, `Engine/Loop/Camera.hs`, `World/Render/Camera.hs`, `World/Render/Camera/Types.hs` |
| `Textures` | `World/Render/Textures.hs`, `World/Render/Textures/Types.hs`, `World/Render/Zoom/Textures.hs` |
| `ViewBounds` | `World/Render/ViewBounds.hs`, `World/Render/Zoom/ViewBounds.hs` |
| `Quads` | `World/Render/Quads.hs`, `World/Render/Zoom/Quads.hs` |

Four distinct `Camera` modules across two subsystems. In a codebase where most
imports are unqualified, a stack trace, a grep hit, or an editor tab labelled
`Camera.hs` is ambiguous four ways.

Not all are wrong — `Zoom/ViewBounds` genuinely differs from `ViewBounds` — but
the pattern deserves one pass with a naming rule (prefix by role:
`ZoomViewBounds`, `RenderCamera`, …).

### CH-98. A fifth dead binding in `BuildPixels.hs` (extends CH-88)
`World/ZoomMap/Cache/BuildPixels.hs:72`:

```haskell
_wrapC = wrapChunkCoordU worldSize
```

Unused — the `_chunkOcean*` lines beneath it call `wrapC`, a *different*
binding defined at line 235. So the module carries five underscore-silenced
dead bindings (`_wrapC` plus the four `_chunkOcean*` of CH-88), all forced
under `{-# LANGUAGE Strict #-}`.

The underscore prefix is doing real damage here: it converts "GHC will tell you
this is dead" into "this is deliberate", and five accumulated in one file.

Worth a broader sweep: `grep -rn "^\s*_[a-z]" src --include='*.hs'` for other
underscore-silenced bindings that are dead rather than intentionally ignored.

### CH-99. Minor Thread/Render/ZoomMap defects for one cleanup issue
- Only 11 unreferenced exports across all three trees, but they cluster in the
  zoom render path: `Zoom/Bake.hs` (`bakeEntries`, `zoomQuadWorldUVs`),
  `Zoom/Cursor.hs` (`makeHoverQuad`, `makeSelectQuad`), `Zoom/Quads.hs`
  (`emitQuad`, `makeMapQuads`), `Zoom/Background.hs` (`emitQuadBg`),
  `ZoomMap/ColorPalette.hs` (`emptyColorPalette`, `lookupVegColor`),
  `ZoomMap/ChunkTexture.hs` (`chunkAtlasUVs`). All over-exports (kind (b) in
  CH-54) — the zoom facade already narrows the real surface to four names.
- `World/Thread/ChunkLoading.hs` exports `maxChunksPerTick` (a tuning constant)
  that only it reads; `World/Render/Camera.hs` exports `quadCacheMarginFrac`
  the same way. Both belong in a config or stay private.
- Nine `*Quads.hs` producers (`TileQuads`, `CursorQuads`, `Quads`,
  `BloodQuads`, `GroundItemQuads`, `SideDecoQuads`, `SpoilQuads`,
  `FloraQuads`, `Zoom/Quads`) share no common entry shape — some take
  `EngineEnv → WorldState → Float → IO (V.Vector SortableQuad)`, others take
  eight-plus positional arguments including two lookup functions, a
  `CameraFacing`, `zSlice`/`effectiveDepth`, `tileAlpha`/`xOffset`, and a
  `ViewBounds`. `waterSideFaceQuads` alone takes 14 parameters. A shared
  `QuadContext` record would collapse the repeated tail across all nine.

---

## Batch 10 — `src/Unit`, `src/Combat` (swept 2026-07-25)

6688 + 3069 lines. **The best-documented area swept so far.** Only 17
unreferenced exports and 2 review-round comments between them; the
`Unit.Injury` shared-physics boundary is explicit and correct ("the single
place both damage systems funnel through … used by BOTH `Combat.Resolution`
and `Unit.Fall`"); fall physics constants are defined once and derived, not
copied; and the save-critical enums carry accurate append-only warnings at
every declaration site.

The findings are therefore few but sharp — the first is the most consequential
single defect in the audit so far.

### CH-100. The save-critical enums tell you to bump the wrong version, and CLAUDE.md agrees with them
`Direction`, `Pose`, and `UnitActivity` are positional-by-constructor-tag under
`Generic Serialize`. All three carry a clear, correct append-only warning — and
all three end with the same mitigation instruction:

> `Unit/Direction.hs:22` — "If the geometry ever needs different cardinality
> (16-way etc.), bump `currentSaveVersion` in `World.Save.Types`."
>
> `Unit/Sim/Types.hs:150` — "If the pose set legitimately needs to change,
> bump `currentSaveVersion` in `World.Save.Types`."
>
> `Unit/Sim/Types.hs:181` — "New activities go at the end; replacements bump
> `currentSaveVersion`."

**That instruction no longer does anything.** All three enums are serialized
inside `UnitSimStateDTO` (`simPose`, `simState`, `simFacing`,
`simPostTransition` — `World/Save/Component/Entities.hs:517-526`), which is the
**`unit-sim` component**, versioned by its own `ccVersion = 2` /
`ccInputVers = [1, 2]`. Since the #756-#768 overhaul, compatibility is gated
per component; `currentSaveVersion` versions only the transitional in-memory
`SaveData` bridge and, per CLAUDE.md, "is bumped freely".

So a developer who follows the instruction *correctly* — reorders a `Pose`
constructor and dutifully bumps `currentSaveVersion` — ships a change that
silently remaps every saved unit's pose, because `unitSimCodec` still
advertises version 2 and still accepts version 2 payloads. Nothing rejects the
old data; it is decoded against the new tag order.

**CLAUDE.md contradicts itself on this, 17 lines apart:**

- line 738 (Enum schema policy): "anything beyond appending requires a
  `currentSaveVersion` bump"
- line 755 (Architecture): "Component evolution = per-component schema version
  bumps + explicit migrations from frozen vN DTOs — **NOT a global save-version
  bump**"

Four stale instructions (three source comments plus CLAUDE.md's enum-policy
line), all pointing at a lever that is no longer connected.

Fix: correct all four to name the owning component's `ccVersion`/`ccInputVers`
plus a migration from a frozen DTO. See also CH-101.

### CH-101. Two components store the same enum two different ways; only one is order-safe
In the same file, `World/Save/Component/Entities.hs`:

```haskell
-- units component
    , uidPose           ∷ !Text        -- line 382
-- unit-sim component
    , simPose             ∷ !Pose      -- line 517
```

The `units` component stores pose **by name** — order-independent, immune to
constructor reordering, self-describing in a hex dump. The `unit-sim` component
stores the **enum**, inheriting the full positional hazard of CH-100.

One of the two already solved the problem. Nothing records that the divergence
is deliberate, and a reader comparing the two DTOs would reasonably conclude
either style is fine.

Fix: make `unit-sim` store names too (with a v2→v3 migration), or document why
the sim path must stay positional.

### CH-102. The codebase's only `TODO` is a comment claiming TODOs exist
`src/Unit/Pathing/Cost.hs:30` is the sole occurrence of the string `TODO` in
all of `src/` and `app/` — a genuinely clean discipline. It reads:

```haskell
-- Future extension points (left as TODOs in the code, NOT plumbed yet):
--   * Weather modifier (snow/rain slowing units)
--   * Per-unit modifier (heavy armor slower, light units faster)
--
-- These can be added by widening the function signature; call sites
-- pass placeholder modifiers of 1.0 today.
```

Both factual claims are false:

1. **There are no TODOs in the code.** This comment is the only occurrence.
2. **No call site passes a placeholder modifier.** `stepCost` is
   `PathingConfig → MaterialRegistry → WorldTileData → (Int,Int) → (Int,Int) →
   Maybe Float` — it takes no modifier argument at all, and neither
   `AStar.hs:112` nor `PathAdvance.hs:209` passes one.

A reader looking for the placeholder to fill in will not find it.

### CH-103. `Unit.Types.Combat` holds anatomy, not combat
It defines `BodyPart`, `NaturalWeapon`, `StrikeProfile`, `NaturalResistance` —
the unit's body composition and innate attack data. `Combat.Types` (a separate
tree) defines `AttackMode`, `CombatCommand`, `CombatEvent` — the combat
system's commands and events.

Both are reasonable modules; the name `Unit.Types.Combat` for "unit anatomy"
sends you to the wrong one. `Unit.Types.Body` (or `.Anatomy`) says what it
holds and stops colliding with the `Combat` tree.

Related basename collision, same class as CH-97: `Unit/Fall.hs` (fall *injury*
physics — energy, fractures, concussion) versus
`Unit/Thread/Movement/Fall.hs` (fall *motion* — z-interpolation, duration,
initiation). Genuinely different concerns, identical basename.

### CH-104. The append-only enum policy is unenforced, in a codebase full of enforcement
No tool, test, or CI step checks that `Direction`/`Pose`/`UnitActivity` (or any
other `Generic Serialize` enum) stays append-only. Compare what *is* automated:
the persistence-inventory audit, EngineEnv-capability audit, save-compat audit,
Haskell and Lua module budgets, probe CI-eligibility, and action-outcome
coverage — each with its own self-test.

This is the highest-consequence silent-corruption rule the project has, it is
guarded only by prose, and CH-100 shows the prose has already drifted.

A guard is cheap: parse the named enums' constructor lists, compare against a
checked-in golden file, and fail on any change that is not a pure append —
exactly the shape `tools/engine_env_capability_audit.py` already uses.

### CH-105. Minor Unit/Combat defects for one cleanup issue
- **Shared physics constants live in the injury module.** `gravity` and
  `metresPerZ` are defined in `Unit/Fall.hs` (the *injury* model) and imported
  by `Unit/Thread/Movement/Types.hs` and `Unit/Thread/Movement/Leap.hs` (the
  *motion* system), so every motion module depends on the injury model for
  gravity. The sharing itself is correct and good; the constants belong in a
  neutral module.
- **Test-mode rates threaded through the production tick.**
  `testInfectionBaseRate` / `testInfectionGraceSec`
  (`Combat/Wounds/Infection.hs`) are selected by an env-var check inside
  `tickAllWounds` (`Combat/Wounds/Tick.hs:80`) and threaded as an extra
  positional `Bool` through `tickOneUnit`. Deliberate and documented (#593),
  but the same pattern as `SystemError`'s `TestError` (CH-11): test scaffolding
  on a production hot path.
- **Two `Constants` modules that aren't the only home for constants.**
  `Combat/Resolution/Constants.hs` (24 exports) and
  `Combat/Wounds/Constants.hs` (7) exist, yet `Wounds/Infection.hs` defines 9
  more tuning values inline and `Wounds/Bleed.hs` 10. All are named and
  well-commented — the issue is only that "where is the bleed/infection
  tuning?" has two plausible answers.
- **17 unreferenced exports**, almost all over-exported tuning constants and
  internal helpers: `Unit/Injury.hs` (8 — `bluntAbsorbScale`, `bruiseCap`,
  `cutAbsorbScale`, `layerAbsorb`, `maxInjurySeverity`, `weightedPick`, …),
  `Combat/Resolution/Damage.hs` (3), `Unit/Pathing/Cost.hs` (3 lookup
  helpers), `Combat/Thread.hs` (`combatTickRate`), `Unit/Stats.hs`
  (`boxMuller`).
- `poseDepth` (`Unit/Sim/Types.hs:159`) is a second hand-maintained ordering of
  `Pose`'s constructors, independent of the serialization tag order. Both must
  be updated when a pose is added, for different reasons, and only one of them
  corrupts saves if you get it wrong.

---

## Batch 11 — `src/Sim`, `src/Power`, `src/Infection`, `src/Craft` (swept 2026-07-25)

1050 + 586 + 60 + 597 lines. Small, tidy trees — **zero** unreferenced exports
between them (every export has a production or test consumer), `Power/Network.hs` is thoroughly documented, and the
sim-vs-worldgen fluid boundary is clean (shared `FluidType`/`FluidCell` with
explicit `fluidCellToActive`/`activeToFluidCell` conversions rather than a
parallel type). Three findings reach beyond these trees.

### CH-106. Six worker threads hand-implement one identical lifecycle
`startUnitThread`, `startCombatThread`, `startWorldThread`, `startSimThread`,
`startLuaThread`, `startInputThread` — all `EngineEnv → IO ThreadState`, all
built the same way:

```haskell
startXThread env = do
    logger ← readIORef (ccLoggerRef (toCoreCapability env))
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do logInfo logger CatX "Starting X thread..."
            tid ← forkIO $ xLoop env stateRef `finally` putMVar doneVar ()
            return tid)
        (\(e ∷ SomeException) → do
            logError logger CatX $ "Failed starting X thread: " <> T.pack (show e)
            error "X thread start failure.")
    return $ ThreadState stateRef threadId doneVar
```

and each `xLoop` repeats the same `ThreadStopped` / `ThreadPaused`
(`threadDelay 100000`) / `ThreadRunning` case, the same `captureLocked` save-
barrier gate, the same `acknowledgeCurrent`, and — verbatim in all six — this
comment:

> One guarded tick per iteration; the recursive call lives OUTSIDE the catch —
> inside it, each tick pushes a catch frame that never pops (unbounded stack
> growth).

That comment records a real bug that was fixed once and must now stay fixed in
six places independently. `Engine.Core.Thread` already exists and owns
`ThreadState`, `ThreadControl`, and `shutdownThread` — it owns *shutdown* but
not *startup*, and that asymmetry is the whole finding. A
`runWorkerThread ∷ WorkerSpec → EngineEnv → IO ThreadState` would leave each
thread with only its per-tick body.

Sub-point worth its own line: all six fail with `error "X thread start
failure."` — a bare `error` call in the engine's startup path, six times, in a
codebase that has an `EngineException` hierarchy (CH-10) and a
`guardNativeExceptions` boot wrapper (CH-61).

### CH-107. 22 directories exist solely to hold a single `Types.hs`
A directory `X/` containing nothing but `Types.hs`, with **no sibling `X.hs`**:

| Lines | Path |
|---:|---|
| 330 | `World/Command/Types.hs` |
| 299 | `World/State/Types.hs` |
| 193 | `Unit/Sim/Types.hs` |
| 144 | `World/Chunk/Types.hs` |
| 118 | `World/Cursor/Types.hs` |
| 99 | `World/Tile/Types.hs` |
| 88 | `Unit/Command/Types.hs` |
| 64 | `Sim/State/Types.hs`, `Sim/Command/Types.hs` |
| 60 | `Infection/Types.hs` |
| 57 | `World/Texture/Types.hs` |
| 52 | `World/Region/Types.hs` |
| 51 | `Substance/Types.hs`, `Equipment/Types.hs` |
| 48 | `World/Page/Types.hs` |
| 44 | `World/Plant/Types.hs` |
| 43 | `World/Till/Types.hs`, `World/Chop/Types.hs` |
| 24 | `World/Tool/Types.hs` |
| 23 | `World/Ocean/Types.hs`, `Building/Command/Types.hs` |
| 14 | `Engine/Scripting/Types.hs` |

Every one could be `X.hs` — `World.Tool.Types` (24 lines) and
`Engine.Scripting.Types` (14 lines) each get a whole namespace level for one
small module. `Substance`, `Infection`, and `Equipment` are top-level `src/`
namespaces containing exactly one file.

Contrast the **five legitimate** cases, where `X/Types.hs` sits beside a real
`X.hs`: `Location/Overlay`, `World/Render/Textures`, `World/Render/Camera`,
`World/Fluid/Seabed`, `World/Geology/Ore`.

Because both shapes exist, `X/Types.hs` carries no information — you cannot
tell from the path whether `X.hs` exists. Flattening the 22 makes the
remaining five meaningful (CH-56's rule, applied).

### CH-108. Power hardware is hardcoded in Haskell while 16 other content categories are YAML
`src/Power/Types.hs:88`:

```haskell
powerNodeSpecFor ∷ Text → Maybe (PowerRole, Float)
powerNodeSpecFor "solar_panel"          = Just (PowerSource,  400)
powerNodeSpecFor "high_voltage_battery" = Just (PowerStorage, 5000)
powerNodeSpecFor _                      = Nothing
```

Both buildings already have full YAML definitions —
`data/buildings/solar_panel.yaml` and
`data/buildings/high_voltage_battery.yaml` carry their name, sprite,
description, and build cost. Only their **power role and wattage** live in
Haskell.

So one device's definition is split across two files in two languages, and
adding a wind turbine or a fuel generator requires a source edit and a
recompile — in a project where `data/` holds buildings, items, recipes,
materials, flora, vegetation, units, equipment, substances, infections,
locations, loot tables, structure packs, names, language, and thoughts, all
loaded through the `Engine.Asset.Yaml*` family.

Note the recipe side already got this right: `power_draw` is a **recipe** field
in YAML (#590), and `machine_shop.yaml` comments explain the design. The node
side is the outlier.

Fix: add `power_role` / `power_capacity` fields to the building schema and
delete `powerNodeSpecFor`.

### CH-109. Nineteen lines of reasoning prove two functions are dead, and they are still there
`World/Load/Stage.hs:225-243` is a carefully argued comment concluding that
`Craft.Bills.pruneToStations` and `Power.Types.pruneToBuildings` must **not** be
called during load, because:

> …that scenario is already unreachable by the time staging runs at all: this
> module's caller […] rejects the WHOLE load outright via
> `missingDefReferences` before staging ever starts […] Applying the prune here
> only ever catches the FIRST (tolerated) case, silently discarding bills/nodes
> #763 requires to be restored.

**Correction (2026-07-25):** an earlier version of this entry claimed both
functions were dead. That was wrong — `test-headless/` exercises
`pruneToStations`, `pruneToBuildings`, and `removePowerNode`. They are tested
behaviour, not dead code.

What remains is narrower but still real: **neither function has a production
call site.** Their only production mention is this comment explaining why they
must not be called, and the scenario they were written for ("a station's
building DEFINITION deregistered between sessions") is — by the comment's own
argument — unreachable, because `missingDefReferences` rejects such a load
before staging runs.

So the tree carries two functions plus their tests for a scenario the load path
provably cannot reach, and 19 lines of reasoning that must be re-read by anyone
who wonders why. Either the scenario is reachable by some path the comment
didn't consider (in which case the prune belongs somewhere and the comment is
incomplete), or it is not (in which case functions, tests, and comment can all
go, replaced by one line on `wpsCraftBills`/`wpsPowerNodes` saying
restore-verbatim is deliberate).

That question is the issue worth filing — not the deletion itself.

### CH-110. Minor Sim/Power/Infection/Craft defects for one cleanup issue
- `Sim/Thread.hs` and `World/ZoomMap/*`, `World/Plate/*`, etc. carry
  `{-# OPTIONS_GHC -fprof-auto #-}` — `Sim/Thread.hs` is one of the 43 files in
  CH-87.
- Over-exports (used only internally): `Power/Network.hs`'s `wireComponents`
  and `solarIntensity`, `Craft/Execute.hs`'s `takeItemsByName`.
- `Power/Network.hs` hand-rolls a union-find (`UnionFind`, `ufNew`, `ufFind`,
  `ufUnion`) and a flood-fill (`wireComponents`). Both are correct and
  well-commented, and the union-find is the only one in the tree — but
  connected-component search now exists in ~20 modules across `World/Fluid`,
  `World/Hydrology`, and here, each over a different data structure. Worth one
  survey to see whether a shared `World.Graph` helper is warranted, before a
  21st appears.
- `Craft/Bills.hs` is 399 lines of pure, well-factored bill state with 19
  top-level functions — a good model for what the rest of the tree could look
  like. Noted as a positive reference point, not a defect.

---

## Batch 12 — `src/Building`, `src/Structure`, `src/Location`, `src/LootTable` (swept 2026-07-25)

983 + 626 + 708 + 74 lines. Small and in good shape: **3 unreferenced exports**
across all four trees, and the module boundaries are deliberate
(`Location.Placement`'s haddock explains it was factored out specifically so
`Building.Placement.canPlaceAt` could reuse it purely). Two findings reach
beyond these trees.

### CH-111. `applyFacingF` — the camera rotation — is defined three times, identically
```haskell
applyFacingF ∷ CameraFacing → Float → Float → (Float, Float)
applyFacingF FaceSouth gx gy = ( gx,  gy)
applyFacingF FaceWest  gx gy = ( gy, -gx)
applyFacingF FaceNorth gx gy = (-gx, -gy)
applyFacingF FaceEast  gx gy = (-gy,  gx)
```

Character-for-character in **`World/Grid.hs:177`**, **`Unit/Render.hs:291`**,
and **`Building/Render.hs:330`**.

`World.Grid` is the canonical home — it owns `defaultGridConfig`,
`worldToGrid`, and the matching inverse `unapplyFacingF`. The two renderers
reimplement the forward transform privately rather than importing it. And
`World/Grid.hs`'s `unapplyFacingF` is itself unreferenced (CH-90), so the
canonical module's inverse is unused while its forward transform is
triplicated.

Together with **CH-92** (`baseTileW`/`baseTileH` in eight modules), this is the
pattern: the isometric renderer's core geometry primitives — tile dimensions
and camera rotation — are copy-pasted across `Unit`, `Building`, `Structure`,
and `World/Render` instead of imported from `World.Grid`. Any change to the
projection has to be found in eleven places across four subsystems.

Fix these two together in one pass: export from `World.Grid`, delete the copies.

### CH-112. `validRelBounds` documents a validation it doesn't perform
```haskell
-- | True iff min ≤ max on both axes — the shape every location's
--   authored bounds must satisfy. 'Engine.Asset.YamlLocations' rejects
--   any definition whose bounds fail this at YAML load time.
validRelBounds ∷ RelBounds → Bool
validRelBounds b = rbMinX b ≤ rbMaxX b ∧ rbMinY b ≤ rbMaxY b
```

`Engine.Asset.YamlLocations` **does** reject inverted bounds — but by
hand-inlining the comparison rather than calling this function
(`YamlLocations.hs:162-168`):

```haskell
        when (lybMinX bounds > lybMaxX bounds) $ fail …
        when (lybMinY bounds > lybMaxY bounds) $ fail …
```

`validRelBounds` has **zero references** in `src/`, `app/`, `test/`, or
`test-headless/`. So the canonical predicate is dead, the loader carries its
own copy, and the predicate's haddock asserts a relationship that does not
exist in the code.

The consequence is the usual one: a rule change (adding a Z axis, allowing
degenerate boxes) would be made in the documented "canonical" function and have
no effect, because the only enforcement point reimplements it.

Sibling dead exports in the same tree: `Location/Bounds.hs`'s
`distancePointToBounds` and `distanceBoundsToBounds` (seam-aware Chebyshev
helpers with no callers — discovery uses the `expandBounds` +
`boundsContainsPoint` route instead, which *is* seam-aware, so this is dead
code rather than a seam bug), `Structure/Palette.hs`'s `lookupId`, and
`Location/Overlay/Types.hs`'s `overlayLookup`.

### CH-113. Quad vertex construction is written out longhand in eight places
Every sprite renderer builds its four vertices by hand:

```haskell
v0 = Vertex (Vec2 drawX drawY)                 (Vec2 0 0) tint (fromIntegral slot) fmSlot 0 wuv
v1 = Vertex (Vec2 (drawX + quadW) drawY)       (Vec2 1 0) tint (fromIntegral slot) fmSlot 0 wuv
v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH)) (Vec2 1 1) tint (fromIntegral slot) fmSlot 0 wuv
v3 = Vertex (Vec2 drawX (drawY + quadH))       (Vec2 0 1) tint (fromIntegral slot) fmSlot 0 wuv
```

Eight occurrences across `Unit/Render.hs`, `Structure/Render.hs` (×3),
`Building/Render.hs` (×2), `World/Render/GroundItemQuads.hs`, and
`World/Render/BloodQuads.hs` — identical UV corners, identical
tint/slot/facemap/flags/worldUV tail, differing only in the position
arithmetic.

A `makeQuad ∷ (Float,Float) → (Float,Float) → Vec4 → Int → Float → Word32 →
Word32 → (Vertex,Vertex,Vertex,Vertex)` would reduce each site to one line and
make the UV winding a single fact. Pairs with CH-99's observation that the nine
`*Quads` producers share no common entry shape.

### CH-114. Minor Building/Structure/Location defects for one cleanup issue
- **`Location.Placement` doesn't place anything.** It derives the absolute
  bounds of already-placed locations (`placedLocationBounds`,
  `nearestLocationDistance`) — a *query*, sitting beside `Location.Bounds`
  which holds the bounds *math*. `Building.Placement` (validation for a
  proposed placement) is the honest use of the name; these two modules named
  `Placement` do unrelated things.
- **`Building` splits one concern across two single-file directories** —
  `Building/Thread/Command.hs` (the processor) and
  `Building/Command/Types.hs` (the type). Both are instances of CH-107.
- `Building/Render.hs` and `Structure/Render.hs` both re-derive
  `baseTileW`/`baseTileH` (CH-92) and `Building/Render.hs` also re-derives
  `applyFacingF` (CH-111) — this pair of modules is the densest concentration
  of the copied-geometry problem.
- `Building/Render.hs`'s `ghostTint` is a good example of the *right* pattern
  and worth preserving as-is: a pure function split out of the renderer
  specifically so the decision is testable without a GPU, correctly documented
  as "the one place RGB tinting is allowed by design (see the no-tinting
  rule)", **and actually exercised by `test-headless/`.**

---

## Batch 13 — remaining Haskell: `Item`, `Language`, `Blood`, and the test suites (swept 2026-07-25)

`Item` (555), `Language` (1091), `Blood` (1413), `Equipment`/`Substance` (102)
are healthy — 15 unreferenced exports between them, all over-exported internal
helpers or documented named constants (`Blood/Trail.hs`'s severity ladder,
`Language/Generated/Root.hs`'s `generateRoot`, both used within their own
modules).

The real gap is the test tree: **134 files, 34,293 lines — never audited, and
larger than any production subsystem except `World/`.**

### CH-115. The `synarchy-test-graphical` suite is built by CI but never run
`.github/workflows/ci.yml` and `tools/ci-local.sh` (the `make ci` gate) both do:

```
cabal build synarchy-test-headless
cabal build synarchy-test-graphical     # built…
cabal test  synarchy-test-headless      # …but only headless is RUN
```

So `test/` — 682 lines, 9 modules covering `UPrelude`, `Engine.Core.Queue`,
`Engine.Core.Var`, `Engine.Input.State`, GLFW window creation, and Vulkan
instance/surface/device creation — is a **compile check only**. Its assertions
have never executed in CI or in `make ci`.

That is probably deliberate (the Vulkan/GLFW cases need a display), but nothing
records it: CLAUDE.md describes `test/` as "hspec unit tests (engine core and
Vulkan primitives)" with no hint that they don't run, and the cabal stanza
carries no comment.

Two consequences worth separating:

1. **Three of the nine modules need no GPU at all** — `Test.UPrelude`,
   `Test.Engine.Core.Queue`, and `Test.Engine.Core.Var` contain zero
   GLFW/Vulkan imports. Moved to `test-headless`, they would actually run.
2. **This closes the loop on CH-12.** `Engine.Core.Var` is a production module
   whose only consumer is a test — in the suite that never executes. So the
   module exists to be tested by assertions that never run.

Fix: run the graphical suite where a display exists, or state plainly in
CLAUDE.md and the cabal stanza that it is a build-only target, and move the
GPU-free specs to the suite that runs.

### CH-116. The four largest files in the project are test modules
| Lines | File |
|---:|---|
| 2806 | `test-headless/Test/Headless/UI/ResponsiveGameplay.hs` |
| 1728 | `test-headless/Test/Headless/Lua/SaveModules.hs` |
| 1674 | `test-headless/Test/Headless/UI/ResponsiveMenus.hs` |
| 1448 | `test-headless/Test/Headless/World/Save/Components.hs` |

`ResponsiveGameplay.hs` alone is **more than double** the largest production
module (`World/Save/Types.hs`, 1316 — and 296 lines of *that* is one comment,
CH-40). **15 test-headless files exceed 500 lines.**

The 500-line norm is stated in CLAUDE.md and enforced by
`tools/haskell_module_budget.py` — which covers exactly three production splits
and no test code at all (CH-21). So the convention applies most weakly where
the largest files actually are.

Test code earns some latitude — a spec is a list of cases, not branching logic.
But a 2806-line spec is past the point where a reader can find the case they
broke, and `ResponsiveMenus`/`ResponsiveGameplay` (4480 lines combined) cover
one epic that already has a natural per-screen split.

### CH-117. Seven test modules bypass the shared engine harness
`Test.Headless.Harness` exports `withHeadlessEngine ∷ (EngineEnv → IO α) → IO α`,
and CLAUDE.md states the convention: "one engine, booted in `Spec.hs`".

These seven call `initializeEngineHeadless` directly instead:

`Unit/LineOfSight.hs`, `Core/LogMonad.hs`, `Asset/TextureFallback.hs`,
`World/SelectChunk.hs`, `World/LocationDiscovery.hs`, `World/CursorInfo.hs`,
`Blood/Trail.hs`

Six of the seven do no worldgen, so the cost is small (`initializeEngineHeadless`
allocates IORefs; it is `WorldInit` that costs ~10 s). This is a consistency
problem, not a performance one — but it means the harness is not the single
entry point it is documented to be, and a future change to engine setup has
eight places to reach.

`World/SelectChunk.hs` is the one to check first: it does reference worldgen
(3 sites) while booting privately.

### CH-118. `test/` and `test-headless/` were absent from this audit's own tooling
Recorded here as a finding against the audit, not the code: every
"unreferenced export" scan in batches 2-11 searched `src/ app/ test/` and
omitted `test-headless/` — the suite holding 124 of the project's 134 test
files. Corrected counts and the three wrong findings are listed in the
methodology note at the top of this document.

The general lesson applies to the codebase's own tooling too: any script that
reasons about "is this used?" must enumerate all four source roots. Worth
checking `tools/engine_env_capability_audit.py`,
`tools/persistence_inventory_audit.py`, and `tools/action_outcome_coverage.py`
for the same omission before trusting their coverage claims.

### CH-119. Minor remaining-Haskell defects for one cleanup issue
- `Item`/`Language`/`Blood` over-exports (used only in-module):
  `Blood/Trail.hs`'s `trailModerateVolume`/`trailSevereVolume`/
  `trailCatastrophicVolume`/`trailBloodForVolume`,
  `Language/Generated/Root.hs`'s `generateRoot`/`minNativeWordLength`,
  `Blood/Impact.hs`'s `impactFootprint`/`impactOpacity`,
  `Blood/Types.hs`'s `matchThreshold`/`removeDecalsForTexture`,
  `Item/Types.hs`'s `defaultQualityTiers`.
- `World/Geology` + `Magma` + `Plate` + `Weather` + `Flora` + `Slope` carry 40
  unreferenced exports, dominated by two clusters already noted in CH-90:
  `Geology/Volcano.hs` (7 `apply*` feature builders dispatched internally by
  `applyVolcanicFeature`) and `Magma/Init.hs` (7 internal geometry helpers —
  `padBox`, `unionBoxes`, `squareAt`, `msBBoxFromShapes`, …). Both are
  export-list narrowing, not deletion.
- `Geology/Generate.hs`'s `generateCaldera`/`generateLavaDome`/
  `generateLavaTube`/`generateAndRegister` pair with the `Volcano.hs` cluster —
  worth checking together whether the whole feature-generation surface should
  be one narrowed module.

---

## Batch 14 — `src/UI` (Haskell only; the Lua UI is a later batch) (swept 2026-07-25)

4174 lines, 25 modules, **none over 500**. `UI/Manager.hs` (103) and
`UI/Tooltip.hs` (47) are proper narrow facades over their subdirectories — the
shape CH-56 asks for elsewhere. **11 unreferenced exports, all over-exports;
no dead code.**

Most of this batch is verification. The #742-#750 UI epic's contracts are the
most heavily documented in the codebase, and — unlike the comparable claims in
worldgen (CH-83) and locations (CH-112) — **they hold.**

### CH-120. Five focus modules, and three have no module haddock at all
| Module | Governs | Header? |
|---|---|---|
| `UI/Focus.hs` | `FocusManager` — shell/console text focus | **none** |
| `UI/Manager/Focus.hs` | element/page text focus + control focus | **none** |
| `UI/FocusNavigation.hs` | keyboard control focus, Tab traversal (#745) | yes |
| `Engine/Scripting/Lua/API/Focus.hs` | Lua binding for `FocusManager` | **none** |
| `Engine/Scripting/Lua/API/UI/Focus.hs` | Lua binding for UI focus | vague |

This supersedes and widens **CH-44** (which covered only the two Lua modules).
Three of the five open straight into `module …  where` with no statement of
which focus system they belong to, and their vocabularies overlap directly:

- `UI.Focus` exports `setFocus`, `clearFocus`, `registerFocusTarget`
- `UI.Manager.Focus` exports `setElementFocus`, `clearElementFocus`,
  `setControlFocus`, `clearControlFocus`

A reader who greps `setFocus` lands in the shell-console system while looking
at game UI code, or vice versa. `Engine/Input/Thread/Keyboard.hs:97` already
documents the resolution order ("1. FocusManager (focusManagerRef) —
shell/console text input") — that paragraph is the missing header, and it
belongs at the top of all five.

Fix: one shared sentence per module naming its system, and rename the two
`Focus.hs` files to say which they are (`UI/ShellFocus.hs`,
`Engine/Scripting/Lua/API/ShellFocus.hs`).

### CH-121. `src/UI` is the densest concentration of review-round archaeology
31 comments citing PR review rounds — the highest of any tree, confirming
CH-15's identification:

| Count | Module |
|---:|---|
| 6 | `UI/Manager/Hierarchy.hs` |
| 5 | `UI/Manager/Page.hs` |
| 4 | `UI/Manager/Property.hs` |
| 3 | `UI/Render.hs`, `UI/Manager/Core.hs`, `UI/InputOwnership.hs`, `UI/ControlActivation.hs` |
| 2 | `UI/Types.hs` |
| 1 | `UI/Manager/Query.hs`, `UI/FocusNavigation.hs` |

They cluster on the activation-epoch machinery (`#745 review round 12` /
`round 13` appear repeatedly), where the *invariant* is genuinely subtle and
worth documenting — "bumped ONLY by a route-affecting change", "only bumps when
`visible` actually differs" — but the round number adds nothing a reader can
act on.

This is the best tree to do the CH-15 sweep in first: the invariants are
already well written, so the edit is purely deleting `review round N` and
keeping the sentence.

### CH-122. Verified: the UI tree's "single source of truth" claims are true
Recorded as a positive result, because three comparable claims elsewhere in the
audit turned out to be false (CH-83 `mkSurfaceMap`, CH-112 `validRelBounds`,
CH-85 `moSurface`). Each of these was checked against the live code:

- **`uiLayerBand`** — one definition (`UI/Types.hs:75`), consulted by rendering
  (`UI/Render.hs:56`) and hit-testing (`UI/Manager/Query.hs:194, 322`). ✓
- **`effectiveClip`** — one definition (`UI/Clipping.hs:86`), consulted by
  rendering (`Render.hs:161`), hit-testing (`Query.hs:161`), interactive bounds
  (`InteractiveBounds.hs:182`), and the Lua introspection path
  (`API/UI/Property.hs:281`). ✓
- **`interactiveRect`** — one definition (`InteractiveBounds.hs:139`), and
  hit-testing goes through it (`Query.hs:159`). ✓
- **Paint-order parity** — I initially read `Render.hs:163`
  (`baseLayerId + ueZIndex elem`) as adding only the element's own zIndex while
  `Query.hs:322` accumulates ancestors. That was wrong: `renderElement`
  recurses passing `elemLayerId` as the child's base (`Render.hs:170`), so the
  render path accumulates through the chain exactly as `elementPaintKey` does.
  The two agree. ✓
- **`upmPageEpoch`** — CLAUDE.md says "bumped ONLY by `hidePage`/`showPage`…
  only on a real value change". `bumpPageEpoch` has exactly two call sites,
  both in `UI/Manager/Page.hs` (72, 87), each guarded by an actual-change
  check. ✓

No action needed. Worth keeping in the document so a future reader knows these
were checked rather than assumed.

### CH-123. Minor UI defects for one cleanup issue
- **11 over-exported internals** (no consumer outside their own module):
  `UI/InputOwnership.hs` (`inputBoundaryPage`, `pagesInScope`),
  `UI/TextBuffer.hs` (`clearBuffer`, `submitBuffer`),
  `UI/Tooltip/Layout.hs` (`hintLeadingPx`, `hintPixelWidth`),
  `UI/InteractiveBounds.hs` (`elementRawOverflow`),
  `UI/Manager/Query.hs` (`hitsAtPointBy`), `UI/Render.hs`
  (`uiLayerToLayerId`), `UI/Tooltip/State.hs` (`showTooltip`), `UI/Types.hs`
  (`emptyTooltipState`).
  `submitBuffer` is the one worth a look — a text-submission entry point on a
  documented contract (`UI.TextBuffer`) that nothing calls.
- **`UI.Focus` re-exports `TextBuffer` and `emptyBuffer` from `UI.Types`**, so
  both are importable from two modules. Given CH-120's confusion about which
  focus system is which, the re-export adds a third path to a type that has
  nothing to do with shell focus.
- `UI/Types.hs` (488 lines) is the tree's largest module and holds
  `UIPageManager`, `UIPage`, `UIElement`, `UILayer`, `uiLayerBand`,
  `TextBuffer`, and tooltip state. It is well under budget, but it is the one
  module in this tree with no export list narrowing — worth splitting only if
  it grows.

**Methodology note for this batch:** the tokenizer used for the dead-export
scans treats haddock `'quoted'` names as ending in an apostrophe, so a symbol
mentioned only inside haddock quotes elsewhere can read as unreferenced. Every
name above was re-checked with a direct call-site grep; all 11 are genuine
over-exports (used within their own module), none are dead.

---

## Batch 15 — `scripts/` (Lua) (swept 2026-07-25)

178 files, **59,531 lines** — the largest single-language surface in the
project. The problems here are duplication and layout, not dead code.

### CH-124. `truncateToWidth` has five divergent implementations, and users can see the difference
Five modules each define a private `truncateToWidth`, and unlike the other
duplicates in this batch **they have drifted**:

| Module | Algorithm | Ellipsis | nil/empty guard |
|---|---|---|---|
| `popup.lua:185` | binary search | — | no |
| `event_log.lua:400` | linear | `"..."` | no |
| `unit_info_v2_inventory.lua:42` | linear | `".."` | yes |
| `item_contents_panel.lua:117` | binary search | — | yes |
| `cargo_inventory_panel.lua:397` | drop one char at a time | `".."` | yes |

So a truncated item name ends in `..` in the inventory panel and `...` in the
event log — a visible inconsistency in the same UI, from the same operation.
Two of the five crash on `nil` where three return early.

All five also bound their search with `#text` (bytes) while each file imports
`utf8_safe` for other purposes, so a truncation can cut mid-codepoint —
precisely the class of bug `scripts/ui/utf8_safe.lua` exists to prevent, and
which CLAUDE.md's text contract calls out.

This is the sharpest illustration in the audit of why duplication matters: the
identical copies (below) are merely wasteful; this one already produces
different behaviour in shipped UI.

### CH-125. `clamp` is defined 11 times; `formatGameTimeHMS` 4 times, identically
```lua
local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end
```

Character-for-character in **eleven** modules — `circulation`, `cardio`,
`brain`, `thermo`, `movement_speed`, `salts`, `consumable`, `starvation`,
`thoughts`, `mental_state`, `exhaustion` (one spread over three lines in
`movement_speed`). That is the entire physiology/mental-state family, which has
no shared utility module of its own.

```lua
local function formatGameTimeHMS(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    local ss = secs % 60
    return string.format("%02d:%02d:%02d", hh, mm, ss)
end
```

Seven identical lines in **four** modules — `combat_log`, `injury_log_panel`,
`unit_log`, `thought_log`. These are exactly the four panels that show the
player timestamped events; if one ever drifts, the same event displays a
different time depending on which panel you open.

Also duplicated 3+ times: `wrapText` (3), `worldId` (4), `destroyChrome` (4),
`destroyTransient`/`destroyOwned`/`destroyAll` (3 each), `spawnTab` (3),
`tabPixelWidth` (3), `displayName` (3), `processEvent` (3), `active` (4).

`scripts/lib/` already exists as the shared-library location — it just holds
only persistence code (`save_modules.lua`, `data_codec.lua`). A
`scripts/lib/util.lua` and `scripts/lib/format.lua` would absorb most of the
above.

### CH-126. `shell.wrapText` says "by character" and iterates by byte
`scripts/shell.lua:682`:

```lua
-- Wrap text into multiple lines that fit within maxWidth (by character)
function shell.wrapText(text, maxWidth, font)
    ...
    for i = 1, #text do
        local char = text:sub(i, i)
        local testLine = currentLine .. char
        local width = engine.getTextWidth(font, testLine, fontSize)
```

Three defects in five lines:

1. **The comment is wrong** — `#text` and `text:sub(i, i)` are byte
   operations, not character ones.
2. **Multi-byte UTF-8 splits mid-sequence**, so any non-ASCII in shell output
   wraps into mojibake. The debug console is the most likely place in the game
   to display arbitrary text (Lua return values, engine log lines, error
   messages).
3. **`engine.getTextWidth` is called once per byte** — an O(n) text measurement
   per output line, on the panel that renders streamed log output.

Note `scripts/ui/textbox.lua` gets this right (it uses `utf8Safe.suffix`, and
its only `string.sub` mention is a comment explaining why not to use it) —
`shell.lua` uses the older `FocusManager` path (CH-120) and never adopted
`utf8_safe`. CLAUDE.md's UTF-8 rule is scoped to `UI.TextBuffer` widgets, so
the shell is outside its letter; it should not be.

### CH-127. Four features are split across both a flat file and a same-named directory
`scripts/` has 178 files, **134 of them flat in the root** (43,695 lines), and
uses two different namespacing conventions simultaneously — sometimes for the
same feature:

| Feature | Flat file | Directory |
|---|---|---|
| settings | `settings_menu.lua` (1063) | `scripts/settings/` (4 files) |
| create world | `create_world_menu.lua` (1171) | `scripts/create_world/` (7 files) |
| hud | `hud.lua` (1321) | `scripts/hud/` (1 file) |
| debug | `debug.lua` | `scripts/debug/` (3 files) |

Meanwhile the three *largest* families have no directory at all and are
namespaced by underscore: `unit_ai*` (23 files), `unit_info_v2*` (14),
`unit_resource*` (7) — 16,077 lines of `unit_*` in the flat root. And `ui` is a
directory (27 files) while `ui_manager*` (9 files) is flat beside it.

So the answer to "where does script X live?" depends on which convention its
epic happened to use. Same finding as CH-25 (`tools/` is 122 flat Python
files), one level larger.

### CH-128. Five Lua modules sit at exactly the 500-line cap
`unit_ai_construct.lua`, `unit_info_v2_status.lua`, `init_mouse.lua`,
`unit_info_v2.lua`, `unit_ai_craft.lua` — all **exactly 500**. Four more are
within six lines: `ui_manager_boot.lua` (499), `loading_screen.lua` (499),
`item_contents_panel.lua` (499), `settings/data.lua` (494).

The five at exactly 500 are all inside budget-guarded families, so the guard is
working — but content pinned precisely at a ceiling is being shaped by the
limit rather than by cohesion, the same signal as `Mouse.hs` at exactly 500
(CH-21). Two of the near-misses (`loading_screen.lua`,
`item_contents_panel.lua`) are in no guarded family at all and reached 499
independently.

Extends CH-22: the budget guards 6 named families while 30 files exceed 500,
including the four largest (`ui/dropdown.lua` 1399, `hud.lua` 1321,
`create_world_menu.lua` 1171, `build_tool.lua` 1167).

---

## Batch 16 — `tools/` (Python) (swept 2026-07-25)

130 files, **54,521 lines**. Better shared-infrastructure adoption than
`scripts/` — `probelib` exists and 71 of 72 probes import it — but the library
is routinely imported and then bypassed.

### CH-129. `probelib` is imported by 71 of 72 probes and then reimplemented
`tools/probelib.py` is a real shared library with a documented purpose,
including the debug-console idle-read gotcha its module header explains at
length ("The console keeps the TCP connection open … idle gap and returns the
last non-empty `"> "` result line", `DEFAULT_IDLE = 0.3`).

Probes import it — and then define their own copies of what it provides:

| Helper | probelib has it | Local copies |
|---|---|---:|
| `jget` / `send_json` | `send_json` | **20** |
| `spawn_acolyte` | yes | **6** |
| `poll_until` | yes | **4** |

All 20 `jget` files import `probelib`; **none** references `send_json`. And the
local copy is not equivalent:

```python
# local jget (20 files)                    # probelib.send_json
def jget(port, lua, timeout=10.0):         def send_json(port, lua, timeout=10.0,
    raw = send(port, lua, timeout)                       idle=DEFAULT_IDLE):
    try:                                       raw = send(port, lua, timeout=timeout, idle=idle)
        return json.loads(raw)                 if not raw: return None
    except json.JSONDecodeError:               try: return json.loads(raw)
        return raw.strip('"')                  except (ValueError, TypeError): return raw
```

Three behavioural differences, none documented anywhere:

1. **Empty result** — `send_json` returns `None`; `jget` feeds `""` to
   `json.loads`, catches the error, and returns `""`. A probe checking
   `if result is None` behaves differently depending on which it called.
2. **Parse failure** — `send_json` returns the raw string; `jget` returns
   `raw.strip('"')`.
3. **`idle` is unreachable** — `jget` calls `send` positionally and cannot pass
   `idle`, so the one knob probelib exists to expose for the documented
   console-read gotcha is hardcoded away in 20 probes.

Also duplicated 4+ times across `tools/` without a probelib equivalent:
`bootstrap_defs` (18), `make_isolated_root` (13), `spawn_station` (9),
`as_int` (9), `run_dump` (7), `boot_probe` (5), `count_item` (5), `expect` (5),
`wid` (4), `wait_active` (4), `num` (4), `find_flat_strip` (4), `ai_off` (4).

`make_isolated_root` (13 copies) is the notable one — isolated resource roots
are how `persistence_contract_sweep.py` keeps probes from colliding, and the
setup is written thirteen times.

Fix: move the 13-plus common helpers into `probelib`, then replace the local
`jget` definitions with `send_json` — the semantic differences above have to be
reconciled deliberately, not by blind substitution.

### CH-130. The seven largest files in the project are all tests and tooling
| Lines | File |
|---:|---|
| 3430 | `tools/test_persistence_inventory_audit.py` |
| 2806 | `test-headless/Test/Headless/UI/ResponsiveGameplay.hs` |
| 1951 | `tools/save_compat_audit.py` |
| 1867 | `tools/playtest/critic.py` |
| 1728 | `test-headless/Test/Headless/Lua/SaveModules.hs` |
| 1689 | `tools/persistence_inventory_audit.py` |
| 1674 | `test-headless/Test/Headless/UI/ResponsiveMenus.hs` |

**No production source file appears until rank 8** (`World/Save/Types.hs`,
1316 — of which 296 lines is one comment, CH-40).

`test_persistence_inventory_audit.py` at 3430 lines is the largest file in the
repository, and it is a test for a 1689-line tool — **twice the size of its
subject**. Its two siblings are proportionate (`test_engine_env_capability_audit.py`
1066 vs 1126; `test_save_compat_audit.py` 1308 vs 1951), so this one is an
outlier rather than a house style.

Nothing bounds file size outside the six Lua and three Haskell families the
budget scripts name (CH-21, CH-22, CH-116, CH-128). The three guards cover
neither `tools/` nor any test tree — i.e. none of the seven files above.

### CH-131. `tools/` is 122 flat files that divide cleanly by role
Confirmed and quantified from CH-25: 130 Python files, only `playtest/` and
`baselines/` as subdirectories. The flat 122 partition without ambiguity:

| Count | Kind |
|---:|---|
| 72 | `*_probe.py` |
| 10 | `*_audit.py` |
| 6 | `*_check.py` |
| 14 | `test_*.py` |
| ~20 | reports, generators, shared helpers |

`tools/README.md` and `tools/ci_probes.py --status` exist to navigate what
`tools/probes/`, `tools/audits/`, `tools/reports/`, `tools/tests/` would make
self-evident. The move touches every `python3 tools/x_probe.py` invocation in
CLAUDE.md, CI, and the skills, so it needs one atomic sweep — but the
categories are already unambiguous, which is the hard part.

### CH-132. Minor `tools/` defects for one cleanup issue
- `tools/preview_cli_probe.py` (158 lines) is the only probe of 72 that does
  not import `probelib` — legitimate (it is the CI-eligible no-boot probe and
  never opens a console), but worth a one-line comment saying so, since "every
  probe imports probelib" is otherwise an invariant a reader would assume.
- `tools/__pycache__/` is correctly ignored via `tools/.gitignore` — verified,
  no action.
- The three audit tools with self-tests (`persistence_inventory_audit`,
  `engine_env_capability_audit`, `save_compat_audit`) are the project's model
  for enforcement, and CH-104 recommends a fourth in the same shape for the
  append-only enum rule. Their own "is this used?" logic should be checked for
  the `test-headless/` omission described in CH-118 before their coverage
  claims are relied on.

---

## Sweep complete

Every source tree in the repository has now been audited:

| Batch | Area | Findings |
|---|---|---|
| 1 | `Engine/Core` | CH-1 … CH-16 |
| 2 | `Engine/Loop`, `Input`, `Asset`, structure | CH-17 … CH-27 |
| 3 | `Engine/Graphics` | CH-28 … CH-39 |
| 4 | `Engine/Scripting` | CH-40 … CH-49 |
| 5 | remaining `Engine` | CH-50 … CH-57 |
| 6 | `app/` | CH-58 … CH-69 |
| 7 | `World/Save`, `World/Load`, `Engine/Save` | CH-70 … CH-78 |
| 8 | worldgen pipeline | CH-79 … CH-91 |
| 9 | `World/Thread`, `Render`, `ZoomMap` | CH-92 … CH-99 |
| 10 | `Unit`, `Combat` | CH-100 … CH-105 |
| 11 | `Sim`, `Power`, `Infection`, `Craft` | CH-106 … CH-110 |
| 12 | `Building`, `Structure`, `Location`, `LootTable` | CH-111 … CH-114 |
| 13 | `Item`, `Language`, `Blood`, test suites | CH-115 … CH-119 |
| 14 | `UI` (Haskell) | CH-120 … CH-123 |
| 15 | `scripts/` (Lua) | CH-124 … CH-128 |
| 16 | `tools/` (Python) | CH-129 … CH-132 |

**132 findings.** Recurring themes worth filing as cross-cutting issues rather
than per-site fixes:

- **Copied primitives** — `baseTileW`/`baseTileH` ×8 (CH-92), `applyFacingF` ×3
  (CH-111), `clamp` ×11 and `formatGameTimeHMS` ×4 (CH-125), `tshow` ×4
  (CH-75), `floorDivCS` ×5 (CH-84), `jget` ×20 (CH-129), the fluid surface fold
  ×5 (CH-82), the item walk ×3 (CH-70).
- **Comments that contradict their code** — CH-4, CH-9, CH-32, CH-33, CH-64,
  CH-85, CH-86, CH-96, CH-100, CH-102, CH-112, CH-126.
- **Constants that must agree with nothing enforcing it** — CH-31 (16384 ×5),
  CH-35 (the UBO ×5), CH-89 (material ids), CH-104 (append-only enums).
- **Structure without a stated rule** — CH-56/CH-78/CH-107 (`X.hs` beside
  `X/`), CH-25/CH-127/CH-131 (flat directories), CH-97/CH-103 (duplicate
  basenames).
- **Guards that don't cover what they claim** — CH-21 (glob hole), CH-22/CH-116
  /CH-128 (budget scope), CH-115 (a suite that never runs), CH-118 (this
  audit's own tooling).

---

## Batch 17 — `docs/` markdown (swept 2026-07-25; CLAUDE.md excluded at owner's request)

10 live docs, 5,167 lines (excluding `docs/history/`, which is explicitly
labelled "superseded — context only", and this report).

Every code reference in every live doc was checked against the tree. **The
result is better than expected**: the two authoritative persistence docs
correctly mark deleted modules as deleted, `expedition_gameplay_loop.md`'s
cross-references resolve exactly, and `README.md` is accurate. The problem is
narrower and specific — **two large design docs still describe shipped systems
as unbuilt.**

### CH-133. `player_events.md` (786 lines) is marked "ready to implement" for a system that shipped
Line 7: `Status: design accepted 2026-05-18. Phase 1 ready to implement.`

The player-event system is fully built: `src/Engine/PlayerEvent.hs`,
`src/Engine/PlayerEvent/Emit.hs`,
`src/Engine/Scripting/Lua/API/PlayerEvent.hs` (290 lines),
`data/notification_categories.yaml`, and `engine.getEventLog()` /
`engine.emitEvent` documented in CLAUDE.md as working APIs.

Worse, the doc's own file manifest (lines 667-670) names modules that do not
exist and never did:

```
- `src/Engine/Event.hs` — `Event`/`PopupButton`/`PopupAction` types,
- `src/Engine/Scripting/Lua/API/Event.hs` — Lua API binding
```

Both shipped as `PlayerEvent`, not `Event`. Sections 3-4 are still written in
future tense (`-- src/Engine/Event.hs (new module)`, `(new)`), and the design
table repeatedly qualifies decisions with "in Phase 1" as though Phase 1 were
pending.

So the largest unenforced doc in the tree is out of sync three ways at once:
status, module names, and tense. A reader planning work against it would
rebuild something that exists.

### CH-134. `blood_decals.md` (445 lines) is marked "design draft" for a shipped subsystem
Line 3: `Status: design draft, written 2026-07-07.`

Shipped: `src/Blood/` (5 modules, 1413 lines), `Engine/Scripting/Lua/API/Blood.hs`
(519 lines), and **three** probes (`blood_decal_probe.py`,
`blood_impact_probe.py`, `blood_gpu_lifecycle_probe.py`).

Unlike CH-133 this one is a one-line fix: the doc's module names
(`Blood.Impact`, `Blood.Render`, `Blood.Texture`, `Blood.Types`) match what
shipped, so the design was implemented as designed. Only the status line lies.

(One reference to check while editing: `Blood.Types.BloodStore` does not
resolve against the current `Blood/Types.hs`.)

### CH-135. Status markers are inconsistent, and two of the six that exist are wrong
| Doc | Marker |
|---|---|
| `persistence_contract.md` | **Status:** Authoritative ✓ |
| `persistence_state_inventory.md` | **Status:** Authoritative ✓ |
| `engineenv_capability_inventory.md` | **Status:** Authoritative ✓ |
| `texture_infrastructure.md` | **Status:** Pre-implementation, 2026-05-24 ✓ |
| `player_events.md` | "Phase 1 ready to implement" ✗ (CH-133) |
| `blood_decals.md` | "design draft" ✗ (CH-134) |
| `expedition_gameplay_loop.md` | — none — (has its own status *section*) |
| `asset_generation.md` | — none — |
| `player_manual.md` | — none — |
| `river_rework.md` | — none — (CH-79: abandoned design) |

The project already has the right convention —
`texture_infrastructure.md`'s `**Status:** Pre-implementation, written
2026-05-24` is exactly the marker `river_rework.md` needs to stop reading as a
live plan (CH-79). It just isn't applied uniformly, and where it is applied it
isn't maintained.

Proposal: require one status line on every `docs/*.md` — one of
*Authoritative* / *Pre-implementation* / *Implemented (see §X)* / *Superseded* —
with a date, and check it in the same CI step that already validates the
persistence and capability inventories.

### CH-136. Minor doc defects for one cleanup issue
- **`engineenv_capability_inventory.md:709`** cites
  `test/Test/Headless/Harness.hs`; the file is at
  **`test-headless/`**`/Test/Headless/Harness.hs`. This is the same `test/` vs
  `test-headless/` confusion that produced CH-118 in this audit — in the
  document the capability audit treats as authoritative. (The audit script
  itself keys on module names, not this path, so nothing is broken; the doc is
  simply wrong.)
- **`texture_infrastructure.md`** is honestly labelled pre-implementation, but
  it was written 2026-05-24 and `animations.yaml` still does not exist. Worth
  an explicit decision — still the plan, or move to `docs/history/`?
- **`asset_generation.md`** has no status line and states "Tier 2 subscription
  (~5000 generations/cycle)" as current account state — a fact that goes stale
  silently and is worth dating.
- **`player_manual.md`** has no status line. Its content spot-checks as
  accurate against shipped features, but it is the one player-facing document
  and has no marker saying which build it describes.
- **The blood subsystem has no CLAUDE.md coverage** — 1413 Haskell lines, a
  519-line Lua API, and three probes, with `blood` appearing zero times in
  CLAUDE.md's subsystem contracts. Noted for completeness only; CLAUDE.md was
  excluded from this pass at the owner's request.

### CH-137. Verified: four docs are accurate and worth using as the pattern
Recorded because the reference sweep was expected to find widespread rot and
did not:

- **`expedition_gameplay_loop.md`** — the model. CLAUDE.md gates all current
  discretionary work on "step 9, *Gate the full slice*, of the loop doc"; that
  reference resolves exactly (`### 9. Gate the full slice`, line 327). It
  carries its own `## Implementation status` section naming the shipped issues
  (#777-#782), explicitly reconciles terminology with what shipped ("the
  terminology above matches what shipped: 'remote-start threshold' is the
  `building.remoteCheck` distance gate…"), and states plainly what remains.
- **`persistence_contract.md`** and **`persistence_state_inventory.md`** — both
  name `src/World/Thread/Command/Save/LoadWorld.hs`, which no longer exists,
  and both mark it "(deleted)" / "deleted by" in the same sentence. Correct
  historical referencing, not rot.
- **`README.md`** — accurate on prerequisites, build commands, headless mode,
  the resource-root contract, and testing.

The lesson for CH-133/CH-134: the failure is not that docs drift from code —
these four don't — it is that *design* docs have no lifecycle step that marks
them implemented.
