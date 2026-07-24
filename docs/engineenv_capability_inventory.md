# EngineEnv Capability Inventory

**Status:** Authoritative, Phase 0 of the `EngineEnv` capability-split
epic (issue #537). Written 2026-07-23 against `master@7e5360c2`, issue
#876. This document does not perform any capability split — it
establishes capability ownership, thread access, lifecycle, and the
intended migration boundaries so that later, bounded child issues have
a contract to migrate against. **No runtime or type-boundary change
lands with this document** (issue #876 requirement 11): `EngineEnv`,
`EngineM`, and every call site described below are exactly as they were
at the commit this was written against.

This is a *capability/thread/lifecycle* inventory, not a persistence
inventory. [`docs/persistence_state_inventory.md`](persistence_state_inventory.md)
(issue #756) is the separate, authoritative source of truth for
save/load classification (`Persist exactly` / `Rebuild` / `Exclude` /
...); [`tools/persistence_inventory_audit.py`](../tools/persistence_inventory_audit.py)
guards it. This document is **joinable with that one by exact
`EngineEnv` field name** — every field below uses the identical
backtick-quoted spelling the persistence inventory uses — but records a
different axis of the same fields: not "what does this mean for a
save file" but "who owns this at runtime, which threads may touch it,
and what does its lifecycle look like." Neither document duplicates the
other's classification; read both when a field's full picture is
needed.

## 1. Scope

`src/Engine/Core/State.hs` declares `data EngineEnv = EngineEnv { ... }`
(`:67`) with exactly **81 fields** (`engineConfig` at `:68` through
`popupQueueRef` at `:375`). Every one of the 81 has exactly one row in
§5 below, matching the same field set
[`docs/persistence_state_inventory.md`](persistence_state_inventory.md)
§1 already enumerates and
[`tools/persistence_inventory_audit.py`](../tools/persistence_inventory_audit.py)
already parses via its `ROOT_RECORDS` anchor
(`("EngineEnv", "src/Engine/Core/State.hs", r"^data EngineEnv = EngineEnv\b")`).
[`tools/engine_env_capability_audit.py`](../tools/engine_env_capability_audit.py)
(§8 below) imports and reuses that exact same field-extraction function
(`extract_record_fields`) rather than re-implementing Haskell record
parsing a second time, so the two audits can never drift onto different
notions of "the live field set."

**Out of scope**, per the issue's own boundary (see §6/§7 for how this
is expected to change later, without changing it here):

- `EngineState` (`src/Engine/Core/State.hs:389`, nested under
  `engineStateRef`) is **not** inventoried field-by-field here. It is a
  single, already-documented invariant (§3) — main-render-thread-private
  — and its own fields (`timingState`/`graphicsState`/`assetConfig`/
  `sceneManager`) are pure rendering/timing/scene mechanics, not a
  capability-ownership question distinct from "the main render thread
  owns all of it." `docs/persistence_state_inventory.md` §2 already
  classifies its fields for persistence.
- `WorldManager`/`WorldState`/`UnitManager`/`BuildingManager` internals
  — these are pointed to *from* `EngineEnv` fields
  (`worldManagerRef`/`unitManagerRef`/`buildingManagerRef`), and this
  document inventories those pointer fields, but not each internal
  field of the pointed-to record. That is a separate, later inventory
  (see §7's roadmap) once each capability group's migration issue
  actually needs it.

## 2. Vocabulary

### 2.1 Capability identifiers

Every field in §5 is grouped under exactly one of these eight
identifiers (kebab-case, matched literally by the audit). This splits
requirement 3's minimum bucket list more finely in two places — content
registries are split out from entity managers, since static YAML-backed
registries and live mutable entity managers have entirely different
consumers and mutation patterns — while still covering every named
minimum bucket.

| Identifier | Covers |
|---|---|
| `core-init` | Core initialization and orchestration: boot configuration, the shared logger, the engine lifecycle flag, boot-profile-derived facts. |
| `render-gpu-asset` | Render, window, Vulkan, and asset state: the nested main-thread-private `EngineState`, video/window/framebuffer settings, the bindless texture system, sampler cache, font cache, asset pool, the render camera(s). |
| `input-lua-transport` | Input, keybindings, lifecycle, and Lua message transport: the input event queue, input-barrier tokens, live input device state, key bindings, and the two Lua↔engine message queues. |
| `world-sim-render-handoff` | World, simulation, time, worldgen, and render handoff: the world manager, world/sim command queues, sun angle, worldgen config, pause/game-time, and the render-handoff single-slot uploads (world preview, zoom atlas, world quads, texture palette). |
| `units-buildings-combat` | Units, combat, buildings, and pathing: the unit and building managers and their command queues, unit sim state, stat RNG, combat/injury/thought/action-outcome event streams, pathing tunables. |
| `content-registries` | Items, crafting, equipment, substances, infections, locations, and loot: static, YAML-backed content registries loaded once and queried thereafter. |
| `ui-hud-events` | UI, focus, HUD, selections, events, notifications, and popups: the UI page manager, focus manager, HUD active-page tracking, text-input buffers, the player-event store, notification config, popup queue. |
| `save-load-coordination` | Save/load coordination, provenance, and identity allocation: the save barrier, load status, the staged-load handoff, last-save-time bookkeeping, the item-instance id allocator. |

Generic buckets (`misc`, `shared`, `other`, a blank cell, or any
identifier not in this table) are rejected by the audit — every field
must resolve to exactly one of the eight above.

### 2.2 Thread / execution-role identifiers

These name the concrete execution contexts that exist in this
codebase today (see §4 for which boot profile starts which):

| Identifier | What it is |
|---|---|
| `Boot` | The calling thread of `Engine.Core.Init.initializeEngine(Headless)(With)` and each `app/App/*.hs` boot module, before any worker thread is forked. Effectively single-threaded setup; safe by construction since nothing else is running yet. |
| `MainRender` | The process's original thread running `Engine.Loop.mainLoop` (graphical/offscreen/preview profiles) — Vulkan calls, frame timing, camera update, GLFW window/callback management. Under headless this role does not exist as a *rendering* thread, but the same OS thread still runs the equivalent headless drive loop (`Engine.Loop.Headless`) and remains the sole owner of `EngineState`. Under dump, likewise: `app/App/Dump.hs`'s own `engineAction` (the one-shot drive loop that inits/awaits/queries a world then exits) runs on this same original thread, so its direct field reads/writes are `MainRender` too. |
| `InputThread` | `Engine.Input.Thread` (native GLFW event polling and dispatch). Started only by the graphical, offscreen, and preview boot profiles — never by headless or dump (see §4). |
| `LuaThread` | `Engine.Scripting.Lua.Thread` — script execution, every `engine.*`/`UI.*`/domain Lua API call, and the debug console's line-eval loop (which runs inside this thread, not as a separate OS thread). Started by every boot profile. |
| `WorldThread` | `World.Thread` — world generation, chunk loading, world edits, world time-of-day, and the world-page side of save/load. Not started by the preview profile. |
| `UnitThread` | `Unit.Thread` — unit movement/AI dispatch. Also drains the *building* command queue on the same OS thread (`Unit.Thread` imports `Building.Thread.Command.processAllBuildingCommands`; there is no separate "Building thread"). Not started by the preview profile. |
| `CombatThread` | `Combat.Thread` — combat resolution and wound ticks, at a fixed 60 Hz. Not started by the preview profile. |
| `SimThread` | `Sim.Thread` — fluid/chunk-cell simulation. Not started by the preview profile. |
| `AnyThread` | A field whose access pattern is explicitly documented as thread-agnostic — e.g. a single atomic monotonic counter that is correct to bump from any thread sharing the `EngineEnv` value. Used sparingly, only where the code's own contract says so. |

A Readers/Writers cell in §5 either names one or more of these
identifiers, or is the literal word `None` followed by a parenthetical
justification (e.g. `None (immutable boot configuration)`). Anything
else — a blank cell, a role not in this table, or an unjustified
`None` — is rejected.

**Enforced cell grammar.** Every non-`None` Readers/Writers cell is a
comma-separated list of segments, and each segment must have exactly
this shape:

```
segment := role ("/" role)* (" (" anything ")")?
role     := "`" LETTERS "`"
```

That is: one or more backtick-quoted, slash-joined role names, with
*at most one* trailing parenthetical holding everything else — a
citation, an explanation, a cross-reference — and nothing outside that
parenthetical besides the role name(s) themselves. `` `InputThread`
(drains; `Engine.Input.Thread`) `` is well-formed; `` `InputThread`
drains (`Engine.Input.Thread`) `` is **not** (the bare word "drains"
sits outside the parenthetical, between the role and its own
explanation). `tools/engine_env_capability_audit.py` enforces this
grammar exactly, segment by segment — a segment that doesn't conform
is rejected as malformed regardless of what it says, rather than
guessed at. This grammar is deliberately narrow: it has no
"and"/";"/"plus"-joined alternative form, and needs none, since
anything that isn't a role list wrapped this way is rejected outright
rather than requiring the checker to recognize one more natural-
language joiner.

### 2.3 Lifecycle categories

| Identifier | Meaning |
|---|---|
| `boot-process` | Allocated once during `Engine.Core.Init.initializeEngineWith` (or, for the eight content registries, populated shortly after by the Lua thread's boot-time content-load scripts); lives unchanged in its container for the whole process. No explicit destruction beyond ordinary process exit. The *value inside* may still be read and written throughout the session — that is a Readers/Writers question, not a lifecycle one. |
| `boot-shutdown` | Same boot-time allocation, but the value holds or references an external GPU/OS resource that is explicitly destroyed during `Engine.Loop.Shutdown.shutdownEngine` (or a boot module's own failure branch) before process exit, rather than merely being reclaimed by the garbage collector. |
| `session-replaced` | Wholesale-overwritten by `World.Load.Publish.publishStagedSession` as part of a save/load transaction's atomic publish step — a "new session" boundary that can occur any number of times within one process's lifetime. |
| `transient-handoff` | Allocated at boot, but its meaningful content exists only between one producer's write and one consumer's read/clear within a single short-lived operation (a staged GPU upload, a single in-flight load, a screenshot request). Outside that window the value is a sentinel (`Nothing`/empty), not "the field's actual content." |

### 2.4 Verbs

- **Initialization** — the one-time act of allocating a field's
  container (`newIORef`/`newTVarIO`/`Q.newQueue`/a plain record
  literal) and giving it its first value. For all but a handful of
  fields this happens in exactly one place:
  `Engine.Core.Init.initializeEngineWith` (`src/Engine/Core/Init.hs`),
  called identically by every boot profile (`initializeEngine` for
  graphical/offscreen/preview, `initializeEngineHeadlessWith` for
  headless/dump, which itself calls `initializeEngineWith` and only
  flips `ecHeadless` afterward — see §4). A field's Init cell below
  only names a different site when one genuinely exists (the content
  registries, populated by Lua boot scripts after engine init returns).
- **Orchestration** — coordinating access or lifecycle across more than
  one capability at a single boundary (starting/stopping threads in a
  fixed order, running a save/load transaction, tearing down GPU state
  before threads stop). Orchestration code is exactly the kind of code
  §6 names as a legitimate full-`EngineEnv` exception.
- **Transport** — a `Q.Queue`/`TVar` used purely to hand a message or a
  batch of pending work from one thread to another, with no
  independent "meaning" beyond FIFO delivery (`inputQueue`,
  `worldQueue`, `luaQueue`, ...).
- **Ownership** — the capability responsible for a field's *lifecycle*
  decisions (when it's created, when/whether it's ever explicitly torn
  down, whether it survives a load) — not necessarily the only reader
  or writer.
- **Reading** — any code that dereferences a field's current value
  (`readIORef`/`readTVarIO`/a direct pattern match) without replacing
  it.
- **Writing** — any code that replaces or mutates a field's value
  (`writeIORef`/`atomicModifyIORef'`/`Q.writeQueue`/an STM transaction
  that changes the `TVar`'s content).

## 3. The `EngineState` invariant

`engineStateRef ∷ IORef EngineState` is the one `EngineEnv` field this
document treats specially, per issue #876 requirement 5.

`EngineState` (`src/Engine/Core/State.hs:389-394`) carries the fully
main-render-thread-private mutable state: `TimingState`,
`GraphicsState` (every Vulkan handle, the GLFW window, the scene
render pipeline state), `AssetConfig`, and `SceneManager`. Its own
doc comment states the invariant directly: *"only the main render
thread reads or writes this — worker threads run in plain IO and
cannot reach it. Any state that must cross the thread boundary lives
in `EngineEnv` as an `IORef` instead; never duplicate a field across
the two."*

`engineStateRef` itself — the *pointer* — lives on `EngineEnv` (not
nested inside some worker-thread-owned record) purely so `EngineM`
can carry it through the same immutable Reader environment every other
piece of engine state travels through, instead of needing a second CPS
parameter (see the field's own doc comment,
`src/Engine/Core/State.hs:69-74`). **That placement is a carrying
mechanism, not an ownership signal**: storing the pointer on
`EngineEnv` does not make the `EngineState` it points to a
multi-thread-accessible capability, and this inventory does not
reclassify it as one. In §5's `render-gpu-asset` table,
`engineStateRef`'s Readers and Writers cells are both `MainRender` —
and *only* `MainRender` — specifically because that reflects this
invariant, not because no other thread has *ever* imported
`Engine.Core.State`. A future capability-scoped migration (#537) must
preserve this: `EngineState`'s contents should never move to a
capability record a non-render thread can construct or inspect, even
if the *pointer* field migrates to a narrower render-capability record.

Two fields that logically belong beside `GraphicsState` — the bindless
texture system (`textureSystemRef`) and the default face-map slot
(`defaultFaceMapSlotRef`) — were deliberately moved to `EngineEnv`
*because* worker threads (the world thread's dynamic blood-texture
registration, in particular) need to reach them; `GraphicsState`'s own
doc comment records exactly this (`src/Engine/Core/State.hs:447-449`).
This is the invariant working as intended: state that must cross the
thread boundary is pulled out to `EngineEnv`, rather than the
invariant being silently violated in place.

## 4. Boot profile matrix

Every field is allocated identically for every boot profile — all five
construct their `EngineEnv` via the exact same function,
`Engine.Core.Init.initializeEngineWith`
(graphical/offscreen/preview via `initializeEngine`; headless/dump via
`initializeEngineHeadlessWith`, which calls `initializeEngineWith` and
then only flips `ecHeadless = True` on the returned `EngineConfig` —
`src/Engine/Core/Init.hs:352-357`). What genuinely differs per profile
is **which worker threads get started afterward**, which is what
drives several fields' realistic Reader/Writer role lists in §5 (e.g.
`InputThread` never touches anything under headless/dump; nothing
under `world-sim-render-handoff`/`units-buildings-combat` is ever
written under preview).

| Profile | Module | Input | Lua | World | Unit | Sim | Combat | Window / GPU |
|---|---|---|---|---|---|---|---|---|
| Graphical | `app/App/Graphical.hs` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | window + GPU |
| Offscreen | `app/App/Offscreen.hs` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | GPU, no window |
| Preview | `app/App/Preview.hs` | ✓ | ✓ | — | — | — | — | window + GPU |
| Headless | `app/App/Headless.hs` | — | ✓ | ✓ | ✓ | ✓ | ✓ | none |
| Dump | `app/App/Dump.hs` | — | ✓ | ✓ | ✓ | ✓ | ✓ | none (one-shot, exits after dump) |

(A sixth executable entry point, `app/App/LanguageReport.hs`, performs
no engine initialization at all — by its own module docstring, "no
engine init, no world thread, no Lua, no GPU" — and is correctly
outside this matrix rather than an accidental omission.)

Preview is the one structurally distinct profile: it boots a real
window and GPU (so `render-gpu-asset` fields behave exactly as under
Graphical/Offscreen) but starts no world/unit/sim/combat thread at all
(`app/App/Preview.hs:1-6` docstring). Every field whose only realistic
writers live on those four thread roles is therefore **inert** — never
written, its boot-time value untouched — for the whole lifetime of a
preview-mode process. This is a real, profile-conditional fact about
several fields in `world-sim-render-handoff` and
`units-buildings-combat` below, not a gap in this inventory.

## 5. Field inventory

Column meanings: **Lifecycle** — one of §2.3's four identifiers.
**Readers**/**Writers** — one or more of §2.2's role identifiers (or a
justified `None`), with a representative citation for each. **Sync** —
the field's concurrency primitive and its practical contract. **Init**
— where the field gets its first value. **Shutdown** — what happens
to it (if anything) during engine teardown. **Notes** — migration
dependencies, cross-references, or compatibility-boundary remarks.

### `core-init`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `engineConfig` | boot-process | `AnyThread` (read via the engine environment value itself from every thread — e.g. `ecHeadless` gates GPU-only paths in `Engine.Graphics.Vulkan.Command.Record`) | None (immutable after boot — each `app/App/*.hs` applies one record-update for `ecDebugPort`/`ecBootProfile`/`ecPreviewTarget`/`ecHeadless` from CLI args before any worker thread starts, e.g. `app/App/Graphical.hs:39-48`) | Plain `EngineConfig` record field, no `IORef` — safe from any thread precisely because it is never mutated after boot | `Engine.Core.Init.initializeEngineWith` sets `defaultEngineConfig` (`src/Engine/Core/Init.hs:259`) | None — plain data, reclaimed at process exit | Immutable-boot-configuration carve-out (no writers) is intentional, not a missing decision. |
| `loggerRef` | boot-shutdown | `AnyThread` (every thread logs through it — `Unit.Thread`, `Combat.Thread`, `World.Thread`, `Sim.Thread`, `Engine.Scripting.Lua.Thread`, `Engine.Loop`) | `AnyThread` (`Engine.Core.Log`'s `logInfo`/`logDebug`/`logWarn` write through this ref from any thread) | `IORef LoggerState`; the logger backend batches/flushes internally | `Engine.Core.Init.initializeEngineWith` (`src/Engine/Core/Init.hs:157-158`); backend is `stdout` (graphical/headless) or `stderr` (dump, so stdout stays clean JSON) | Explicitly flushed via `shutdownLogger`, last, after every worker thread has stopped (`src/Engine/Loop/Shutdown.hs:104-107`) and in each `App.*` module's own error branch | Must outlive every other thread's own shutdown log line — hence torn down last, not merely GC'd. |
| `lifecycleRef` | boot-process | `AnyThread` (every worker run-loop polls it each tick — `Unit.Thread`, `Combat.Thread`, `World.Thread`, `Sim.Thread`, `Engine.Scripting.Lua.Thread`, `Engine.Input.Thread`, `Engine.Loop`) | `AnyThread` (the initial-running transition), `MainRender` (sets the final stopped value) | `IORef EngineLifecycle` (`EngineStarting|EngineRunning|CleaningUp|EngineStopped`) | `Engine.Core.Init.initializeEngineWith` seeds `EngineStarting` (`src/Engine/Core/Init.hs:154`) | Set to `EngineStopped` as literally the last step of `Engine.Loop.Shutdown.shutdownEngine`, after every worker thread has stopped and the logger has flushed (`src/Engine/Loop/Shutdown.hs:106-108`) | — |
| `inputThreadActiveRef` | boot-process | `LuaThread` (save-barrier owner-set computation — `API.Save:188`'s `saveWorldFn`, `Thread.Dispatch:408`'s `handleLoadStaged` — consults it to decide whether the input owner slot belongs in a save/load transaction's owner set) | `Boot` (set to true exactly once by `Engine.Input.Thread.startInputThread:49`, on the CALLING thread, immediately BEFORE it forks the actual input thread via `forkIO`; never written again) | `IORef Bool`, write-once | `Engine.Core.Init.initializeEngineWith` seeds `False` (`src/Engine/Core/Init.hs:232`) | None | Boot-profile-derived fact (only Graphical/Offscreen/Preview start an input thread — §4); primary owner is `core-init` even though its principal *consumer* is `save-load-coordination` — see §7. |

### `render-gpu-asset`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `engineStateRef` | boot-shutdown | `MainRender` (only; see §3, the main-thread-private invariant) | `MainRender` (only) | `IORef EngineState`, single-thread-owned; no atomic ops needed since only one thread ever touches it | `Engine.Core.Init.initializeEngineWith` seeds `defaultEngineState` (`src/Engine/Core/Init.hs:257`) | Contents (every Vulkan handle in the nested `GraphicsState`) explicitly destroyed by `Engine.Loop.Shutdown.shutdownEngine` — `deviceWaitIdle`, transient-texture cleanups, `runAllCleanups (vulkanCleanup state)`, explicit sampler/buffer destruction (`src/Engine/Loop/Shutdown.hs:41-84`) — before GLFW/thread teardown; the `IORef` container itself is never destroyed, only overwritten/zeroed | See §3 in full. |
| `videoConfigRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`/`Engine.Graphics.Vulkan.Recreate`, `Engine.Loop.Timing`, and each graphics-capable boot module's own window-creation read — `app/App/Graphical.hs:57`, `app/App/Offscreen.hs:65`, `app/App/Preview.hs:52` — all AFTER `startInputThread`/`startLuaThread` (and, in Graphical/Offscreen, every other worker thread) have already been started, so per §2.2 this is `MainRender`, not `Boot`, in every boot profile), `LuaThread` (`API.Config:36`'s `getVideoConfigFn`, direct query) | `LuaThread` (`Engine.Scripting.Lua.API.Config` — direct settings Apply/Save/Defaults, called synchronously from Lua), `MainRender` (`Engine.Scripting.Lua.Message.Video`'s `handleSetVSync`/`handleSetMSAA`, dispatched via `processLuaMessages` from `Engine.Loop.mainLoop`/`Engine.Loop.Headless`) | `IORef VideoConfig`, multi-writer via `atomicModifyIORef'` | Loaded from `config/video.local.yaml` (or default) (`src/Engine/Core/Init.hs:176-180`) | None | Settings-apply path (#748/#750) reads this on `MainRender` to rebuild the swapchain. Lua-triggered writes split by mechanism: a direct `API.Config` call (e.g. `engine.setVideoConfig`) writes synchronously on `LuaThread`; a call that must also touch the live Vulkan device (VSync/MSAA) instead enqueues onto `luaToEngineQueue` and is applied later, on `MainRender`, when `processLuaMessages` drains it — see `input-lua-transport`'s `luaToEngineQueue`/`luaQueue` rows. |
| `windowSizeRef` | boot-process | `WorldThread` (screen-space quad builders, `World/Render/*Quads.hs`, and `Unit.HitTest`'s hit-testing, reached via `World.Render.CursorQuads`/`updateWorldTiles`), `LuaThread` (`API.Input`, `API.InputInject`, `API.WorldQuery.Pick`, and `Unit.HitTest`/`Building.HitTest` called directly from `API.Buildings.Selection`/`API.Units.Selection`/`API.WorldQuery.Pick`), `MainRender` (`Engine.Loop.Camera`, `Engine.Loop.Frame`), `InputThread` (`Input.Thread.Mouse:56`, `Input.Thread.Scroll:39` — cursor/wheel coordinate conversion) | `InputThread` (native resize callback, `Engine.Input.Thread.Dispatch:117`), `MainRender` (`Engine.Graphics.Window.GLFW:109`, `Engine.Loop.Frame:384`, and `Engine.Scripting.Lua.Message.Video`'s `handleSetResolution`/`handleSetWindowMode` for synthetic/Lua-triggered resolution changes — dispatched via `processLuaMessages`, never the Lua thread itself) | `IORef (Int,Int)`, multi-writer, last-write-wins (no cross-writer ordering guarantee) | Seeded from the loaded `VideoConfig` (`src/Engine/Core/Init.hs:181`) | None | Same multi-writer shape as `framebufferSizeRef`/`windowStateRef` below. |
| `windowStateRef` | boot-process | `MainRender` (only; `Message.Video`, read back when restoring windowed geometry) | `MainRender` (only; `Engine.Scripting.Lua.Message.Video:70` — caches windowed pos/size on fullscreen toggle, dispatched via `processLuaMessages`, never the Lua thread itself) | `IORef WindowState` | `defaultWindowState` (`src/Engine/Core/Init.hs:182`) | None | — |
| `framebufferSizeRef` | boot-process | `WorldThread` (`World/Render.hs`, `World.Render.GroundItemQuads`, `World.Render.BloodQuads`, `World.Render.SpoilQuads`, `World.Render.CursorQuads`), `LuaThread` (`API.UI.Placement`, `API.World.Query`, `API.Input`), `InputThread` (`Input.Thread.Mouse:57`, `Input.Thread.Scroll:40`), `MainRender` (`UI.Tooltip.State:33`, the per-frame tooltip tick called from `Engine.Loop.Frame`) | `InputThread` (`Thread/Dispatch:121`), `MainRender` (`Engine.Scripting.Lua.Message.Video`, dispatched via `processLuaMessages`, and `app/App/Offscreen.hs:72`'s initial-size seed — this runs after `startInputThread`/`startLuaThread`/`startWorldThread`/`startUnitThread`/`startSimThread`/`startCombatThread` have all already been called on the same calling thread, so per §2.2 it is `MainRender`, not `Boot`) | `IORef (Int,Int)`, multi-writer | `Engine.Core.Init.initializeEngineWith` (`src/Engine/Core/Init.hs:183`) | None | — |
| `fpsRef` | boot-process | `LuaThread` (`API.Core`, `engine.getFPS`) | `MainRender` (only, once per frame; `Engine.Loop.Timing:75`) | `IORef Double`, single-writer | `0.0` (`src/Engine/Core/Init.hs:155`) | None | — |
| `brightnessRef` | boot-process | `MainRender` (`Engine.Loop.Frame`, `Engine.Graphics.Vulkan.Init`) | `MainRender` (`Engine.Scripting.Lua.Message.Video:167`'s `handleSetBrightness`, dispatched via `processLuaMessages` — never the Lua thread itself, which only enqueues the request) | `IORef Int` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:184`) | None | — |
| `pixelSnapRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `LuaThread` (`API.Config:223`, direct synchronous `writeIORef`), `MainRender` (`Engine.Scripting.Lua.Message.Video:173`'s `handleSetPixelSnap`, dispatched via `processLuaMessages`, a separate call path from `API.Config`'s) | `IORef Bool` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:185`) | None | — |
| `textureFilterRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Texture.Bindless` — sampler selection) | `LuaThread` (`API.Config:236`, direct synchronous `writeIORef` alongside enqueuing the change), `MainRender` (`Engine.Scripting.Lua.Message.Video:183`'s `handleSetTextureFilter`, dispatched via `processLuaMessages` — this is also where the live GPU sampler swap on `textureSystemRef` happens) | `IORef TextureFilter` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:186`) | None | — |
| `assetPoolRef` | boot-process | `LuaThread` (shared into the Lua backend as `apRef`, `Engine.Scripting.Lua.Thread:51`), `MainRender` (`Message.Texture:84`'s `duplicateCachedTextureHandle`, and `World.Render.BloodQuads:179`'s `uploadOne` -- BOTH dispatched via `processLuaMessages`: `uploadOne` is reached through `uploadBloodTextures`, which `Engine.Scripting.Lua.Message.processLuaMessages` calls directly, not through the world thread's `updateWorldTiles` quad-building path) | `LuaThread`/`Boot`, `MainRender` (`Message.Texture:108`, `atomicModifyIORef'` bumping a cached atlas's refcount, dispatched via `processLuaMessages`) | `IORef AssetPool` | `defaultAssetPool` (`src/Engine/Core/Init.hs:160-161`) | None (any GPU handles it names are registered for teardown where they're created, not on this container) | — |
| `textureNameRegistryRef` | boot-process | `LuaThread` (`Engine.Asset.YamlTextures` name→handle lookups), `WorldThread` (`World.Render.GroundItemQuads:162`'s broken-equipment overlay lookup) | `LuaThread` (registration during content load) | `IORef TextureNameRegistry` | `emptyTextureNameRegistry` (`src/Engine/Core/Init.hs:166`) | None | — |
| `fontCacheRef` | boot-shutdown | `MainRender` (`UI.Render`, `Engine.Scene.Batch.Text`, `Engine.Graphics.Vulkan.Command.Text`), `LuaThread` (`API.Text`) | `MainRender`/`Boot` (`Engine.Graphics.Font.Load` rasterizes on demand) | `IORef FontCache` | `defaultFontCache` (`src/Engine/Core/Init.hs:198`) | Glyph-atlas GPU memory registered via `allocResource` at creation (`Engine.Graphics.Font.Upload`/`Draw`), freed by the generic `vulkanCleanup` sweep in `shutdownEngine` | — |
| `textureSystemRef` | boot-shutdown | `WorldThread` (`src/Unit/Render.hs:131`, via `updateWorldTiles`'s world-thread quad-building pass), `MainRender` (`src/UI/Render.hs`, `Engine.Scripting.Lua.Message.Texture` reads, and `World.Render.BloodQuads:76,161` — the blood-texture upload/dispose functions run via `processLuaMessages`, NOT the world thread's `updateWorldTiles` quad-building path `renderBloodDecalQuads` uses), `LuaThread` (`API.Blood`, direct queries) | `MainRender` (`Engine.Graphics.Vulkan.Init:213` — initial creation on the same thread that runs `Engine.Loop.mainLoop`; also `Engine.Scripting.Lua.Message.Video:189`/`Message.WorldTexture`/`Message.Texture`'s live rebuild/registration handlers, and `World.Render.BloodQuads:84,166` — all dispatched via `processLuaMessages`, never the Lua thread itself and never the world thread) | `IORef (Maybe BindlessTextureSystem)`, all writes confined to `MainRender` (multiple call sites, single writer thread); read by `WorldThread`/`LuaThread` too | `Nothing` at engine boot; populated by `Engine.Graphics.Vulkan.Init:213` on `MainRender` after device creation (graphical/offscreen/preview only — stays `Nothing` under headless/dump) | GPU descriptor/image resources registered via `allocResource` at creation, freed by the `vulkanCleanup` sweep in `shutdownEngine` | Moved to `EngineEnv` specifically because worker threads must be able to READ it (writes stay confined to `MainRender`) — see §3. |
| `samplerCacheRef` | boot-shutdown | `MainRender` (texture/font upload paths acquire samplers by kind) | `MainRender` | `IORef SamplerCache`, refcounted | `emptySamplerCache` (`src/Engine/Core/Init.hs:205`) | Explicitly destroyed via `destroySamplerCache` in `shutdownEngine` (`src/Engine/Loop/Shutdown.hs:62-65`) | — |
| `textureSizeRef` | boot-process | `WorldThread` (`World.Render.GroundItemQuads`, `World.Render.BloodQuads:238`'s `renderBloodDecalQuads` — reached via `updateWorldTiles`, not the upload/dispose path below, `World.Render.Quads`), `MainRender` (`Engine.Scripting.Lua.Message.Texture`, dispatched via `processLuaMessages`), `LuaThread` (`API.Blood`, direct queries) | `MainRender` (`World.Render.BloodQuads:127,188` — per-texture insert/delete as blood textures are created/disposed via the `uploadBloodTextures`/`disposeQueuedBloodTextures` functions `processLuaMessages` calls directly, NOT the world thread's quad-building path, and `Engine.Scripting.Lua.Message.Texture:117,267`, also dispatched via `processLuaMessages` — never the Lua thread itself, never the world thread) | `IORef (HashMap TextureHandle (Int,Int))`, all writes confined to `MainRender` (multiple call sites, single writer thread) | `HM.empty` (`src/Engine/Core/Init.hs:206`) | None — entries are deleted per-texture as their owning textures are disposed, not wholesale at shutdown | — |
| `defaultFaceMapSlotRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `MainRender` | `IORef Word32` | `0` (`src/Engine/Core/Init.hs:207`), reassigned once the default face-map texture binds during Vulkan init | None | — |
| `cameraRef` | session-replaced | `WorldThread` (`World.Render.*`, `World.Thread`, and `Unit.HitTest` reached via `World.Render.CursorQuads`/`updateWorldTiles`), `LuaThread` (`API.Camera:55`'s `cameraGetPositionFn`, and `Unit.HitTest`/`Building.HitTest` called directly from the Lua Selection/Pick API modules), `MainRender` (`Engine.Loop.Frame:243` — the frame loop reads the LIVE camera each frame for the view matrix specifically to avoid the stale-by-tens-of-milliseconds value the world thread's own copy could show) | `WorldThread` (`World/Render.hs:194,212`, `World.Thread.Command.Init:300,356`, load publish `World.Load.Publish:129`), `LuaThread` (`API.Camera:44`), `MainRender` (`Engine.Loop.Camera:178` — WASD camera-pan velocity/position integration runs once per frame on the main thread) | `IORef Camera2D`, multi-writer via `atomicModifyIORef'` | `defaultCamera` (`src/Engine/Core/Init.hs:188`) | None | Session-replaced on load publish; see `docs/persistence_state_inventory.md` §1 `cameraRef`. |
| `uiCameraRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`/`Engine.Graphics.Vulkan.Recreate`) | `MainRender` (window/framebuffer resize path, `Engine.Graphics.Vulkan.Recreate:69`) | `IORef UICamera` | `defaultUICamera`, sized from the loaded `VideoConfig` (`src/Engine/Core/Init.hs:189-190`) | None | — |
| `screenshotRequestQueue` | transient-handoff | `MainRender` (dequeues one per frame in `Engine.Loop.Frame`'s `drawFrame` and replies on the request's own reply channel) | `LuaThread` (`API.Screenshot`) | `Q.Queue ScreenshotRequest` (STM `TQueue`) | `Q.newQueue` (`src/Engine/Core/Init.hs:151`) | None — never drained under headless, the verb refuses before enqueueing | — |
| `nextObjectIdRef` | boot-process | `LuaThread` (only; shared into the Lua backend state as `lbsNextObjectId` — `Engine.Scripting.Lua.Thread:51`, `Engine.Scripting.Lua.Types:43`) | `LuaThread` (only; bumped by every `UI.new*`/scene-object allocation issued from Lua) | `IORef Word32`, monotonic, single-thread-owned (Lua thread only) | `0` (`src/Engine/Core/Init.hs:162`) | None | — |

### `input-lua-transport`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `inputQueue` | boot-process | `InputThread` (drains; `Engine.Input.Thread`, `Thread/Dispatch`), `MainRender` (headless polling path, `Engine.Loop.Headless`) | `Boot`/`MainRender` (GLFW callbacks via `setupCallbacks`), `LuaThread` (`API.InputInject` synthetic injection), `WorldThread` (`World.Load.Publish:210-220`'s `discardStaleQueues`, flushes stale queued input on a load publish) | `Q.Queue InputEvent` (STM `TQueue`), multi-producer/single-consumer FIFO | `Q.newQueue` (`src/Engine/Core/Init.hs:144`) | None; drained/discarded at process exit | — |
| `inputBarrierNextRef` | boot-process | `LuaThread` (`Engine.Input.Inject.newBarrierToken` allocates) | `LuaThread` (only) | `TVar Int`, monotonic allocator | `newTVarIO 0` (`src/Engine/Core/Init.hs:145`) | None | — |
| `inputBarrierRef` | boot-process | `LuaThread` (`Engine.Input.Inject.waitForBarrier`) | `InputThread` (`Engine.Input.Thread.Dispatch.processInput`, strictly after a barrier's turn is fully processed — see the field's own doc comment, `src/Engine/Core/State.hs:89-106`) | `TVar Int` | `newTVarIO 0` (`src/Engine/Core/Init.hs:146`) | None | — |
| `inputStateRef` | session-replaced | `InputThread` (`Thread/Dispatch:46,58`), `LuaThread` (shared as `lbsInputState`), `MainRender` (`Engine.Loop.Camera`) | `InputThread`, `WorldThread` (load publish, `World.Load.Publish:282`, resets to `defaultInputState`) | `IORef InputState` | `defaultInputState` (`src/Engine/Core/Init.hs:168`) | None | Reset on load so stale held-key state can't survive a load. |
| `keyBindingsRef` | boot-process | `InputThread` (`Thread/Keyboard`), `MainRender` (`Engine.Loop.Camera`), `LuaThread` (`API.Input`) | `LuaThread` (only; `API.Keybinds` — settings rebind/reset) | `IORef KeyBindings`, multi-reader/single-writer via `atomicModifyIORef'` | Loaded from `config/keybinds.local.yaml` (or default) (`src/Engine/Core/Init.hs:169-173`) | None | — |
| `currentKeyDownRef` | transient-handoff | `LuaThread` (only) | `LuaThread` (only) | `IORef (Maybe GLFW.Key)` — see the field's own doc comment, `src/Engine/Core/State.hs:123-128` | `Nothing` (`src/Engine/Core/Init.hs:174`) | None | Meaningful only for the duration of one `onKeyDown` broadcast. |
| `luaToEngineQueue` | boot-process | `MainRender` (`Engine.Scripting.Lua.Message`'s `processLuaMessages` — deliberately NOT flushed by `World.Load.Publish`'s `discardStaleQueues` the way `unitQueue`/`buildingQueue`/`combatQueue`/`simQueue`/`inputQueue` are; a stale load-time message is instead left in place and naturally skipped, since `processLuaMessages` itself is gated behind the save barrier's `captureLocked` check — see `World/Load/Publish.hs:77-83`'s own comment on why flushing it from the publish side raced this consumer's drain) | `LuaThread` (`Engine.Scripting.Lua.Thread`, `Thread/Dispatch`) | `Q.Queue LuaToEngineMsg` | `Q.newQueue` (`src/Engine/Core/Init.hs:149`) | None | — |
| `luaQueue` | boot-process | `LuaThread` (drains; `Engine.Scripting.Lua.Thread`, `Engine.Scripting.Lua.Util`) | `WorldThread` (`World.Thread.Command.Init`, `World.Thread.ChunkLoading`, `World.Thread.Command.Save`, `World.Log`, `World.Thread.Helpers`), `MainRender` (`Message.Video`), `UnitThread`/`CombatThread` (notification broadcasts), `InputThread`, `LuaThread` (`API.World.Lifecycle:130`'s `worldOpenArenaFn`, a direct Lua-callable enqueue) | `Q.Queue LuaMsg`, multi-producer/single-consumer | `Q.newQueue`, bound as `engineToLuaQueue` (`src/Engine/Core/Init.hs:150,274`) | None | Engine→Lua direction (the field is literally named `luaQueue` on `EngineEnv` but constructed as `engineToLuaQueue`). |

### `world-sim-render-handoff`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `worldManagerRef` | session-replaced | `UnitThread` (`Unit.Thread.Command.*`, and `Unit.LineOfSight` reached via `Unit.Thread.Command.Lifecycle`), `CombatThread` (`Unit.LineOfSight` reached via `Combat.Resolution`/`Combat.Resolution.Strike`), `WorldThread` (`World.Render*`, `Unit.Render`, and `Unit.HitTest` reached via `World.Render.CursorQuads` — all via `updateWorldTiles`'s world-thread quad-building pass, not `MainRender`, see the `render-gpu-asset` group's BloodQuads note for the distinct upload/dispose exception), `LuaThread` (`API.Units.Spawn:107` direct read, `API.Structure:54`'s `resolveStructurePage` — a direct synchronous read shared by every `structure.*` Lua entry point, and `Unit.HitTest`/`Unit.LineOfSight` called directly from `API.Buildings.Selection`/`API.Units.Selection`/`API.WorldQuery.Pick`/`API.Units.Query`), `MainRender` (`Engine.Loop.Camera:72`'s `getWorldSize`, called from the same module's camera-pan update, and the `Message.Texture` path's `invalidateAllWorldRenderCaches`) | `WorldThread` (world init/load/edit commands, and load publish `World.Load.Publish:158`) | `IORef WorldManager`, multi-writer | `emptyWorldManager` (`src/Engine/Core/Init.hs:192`) | None | Contents classified per-field in `docs/persistence_state_inventory.md` §3/§4; this row covers only the `EngineEnv` pointer, per §1's scope note. |
| `worldQueue` | boot-process | `WorldThread` (drains; `World.Thread`) | `LuaThread` (`API.Construct`, `API.Structure`, `API.Till`, `API.Plant`, `API.World.Lifecycle`, `API.World.Edit`), `SimThread` (`Sim.Thread:392`, enqueues `WorldApplyFluids` once fluid writebacks are ready), `WorldThread` (`World.Thread:154-162`'s `processAuthorizedSave` re-enqueues commands deferred past a captureLocked window), `MainRender` (`app/App/Dump.hs:88,102`, the dump driver's own `WorldInit`/`WorldShow` enqueues) | `Q.Queue WorldCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:147`) | None | — |
| `sunAngleRef` | boot-process | `LuaThread` (`API.World.Clock`, `API.WorldQuery.Climate`), `MainRender` (lighting, `Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `WorldThread` (`World.Thread.Time`, derived via `worldTimeToSunAngle`), `LuaThread` (`API.World.Clock:67`'s `worldSetSunAngleFn`, direct override) | `IORef Float` | `0.25` = noon (`src/Engine/Core/Init.hs:199`) | None | — |
| `worldPreviewRef` | transient-handoff | `MainRender` (consumes for GPU upload) | `WorldThread` (enqueues, and load publish `World.Load.Publish:152`), `MainRender` (`Message.WorldTexture:65`'s `handleWorldPreview`, `atomicModifyIORef'` clearing the slot once dequeued) | `IORef (Maybe (Int,Int,ByteString,Word64))`, single-slot, tagged with a generation | `Nothing` (`src/Engine/Core/Init.hs:200`) | None | Paired with `worldPreviewGenerationRef` to suppress a stale in-flight upload (round 10 review, #763). |
| `worldPreviewGenerationRef` | boot-process | `LuaThread` (`Thread.Dispatch:319`'s `LuaWorldPreviewReady` handler — the generation comparison deliberately happens HERE, at delivery time, not in `Message.WorldTexture.handleWorldPreview`'s upload-completion code, which stopped reading this ref after round 11's review; see that module's own comment explaining why) | `WorldThread` (enqueue bumps it, and load publish `World.Load.Publish:149`) | `IORef Word64`, monotonic, never decreases | `0` (`src/Engine/Core/Init.hs:201`) | None | — |
| `zoomAtlasDataRef` | transient-handoff | `MainRender` (consumes for GPU upload) | `WorldThread` (enqueues, and load publish `World.Load.Publish:137`), `MainRender` (`Message.WorldTexture:186`'s `handleZoomAtlasUpload`, `atomicModifyIORef'` clearing the slot once dequeued) | `IORef (Maybe (Int,Int,ByteString,[WorldState]))`, single-slot | `Nothing` (`src/Engine/Core/Init.hs:202`) | None | Captures the exact `WorldState`s it belongs to at enqueue time (round 9 review, #763). |
| `worldQuadsRef` | boot-process | `MainRender` (frame loop merges + draws) | `WorldThread` (per-tick static/dynamic quad split, #446) | `IORef LayeredQuads` | `emptyLayeredQuads` (`src/Engine/Core/Init.hs:203`) | None | — |
| `bloodDisposeQueue` | transient-handoff | `MainRender` (drains; `World.Render.BloodQuads.disposeQueuedBloodTextures`) | `WorldThread` (page-removal teardown, `World.Blood.Teardown`, `World.Thread.Command.Basic/Init`) | `Q.Queue (IORef BloodTextureHandles)` | `Q.newQueue` (`src/Engine/Core/Init.hs:152`) | None — empty/inert under headless | — |
| `floraCatalogRef` | boot-process | `WorldThread` (`Thread.ChunkLoading`, `Thread.Cursor`, `Thread.Command.Init`, `Command.Cursor.Plant/Chop`, `Command.Edit.Vegetation`, `World.Load.Stage` during staging, and `World.Render.Quads`'s `renderWorldQuads`, reached via `updateWorldTiles`), `LuaThread` (`API.Plant:100`, direct crop/species lookup) | `LuaThread` (content load) | `IORef FloraCatalog` | `emptyFloraCatalog` seed, populated from `data/*.yaml` via Lua content load (`src/Engine/Core/Init.hs:208`) | None | — |
| `materialRegistryRef` | session-replaced | `UnitThread` (`Unit.Thread.Movement`), `WorldThread`, `LuaThread` (`Engine.Scripting.Lua.API.World.Edit:86,211`, `Engine.Scripting.Lua.API.YamlTextures:350`), `MainRender` (`app/App/Dump.hs:152`, direct read while building the dump JSON) | `WorldThread` (populated per-world-init from `data/materials/*.yaml`, `src/World/Thread/Command/Init.hs:111-113`; also load publish, `src/World/Load/Publish.hs:117`), `LuaThread` (`Engine.Scripting.Lua.API.YamlTextures:99` registers each material's physical properties from the same loaded YAML content) | `IORef MaterialRegistry`, multi-writer | `emptyMaterialRegistry` at engine boot (`src/Engine/Core/Init.hs:209`); populated per-world-init from `data/materials/*.yaml` (`src/World/Thread/Command/Init.hs:100-113`) | None | YAML-driven after all (corrected from an earlier draft of this row, per review) — populated once per world init/load, not built into the binary. |
| `worldGenConfigRef` | boot-process | `WorldThread` (`Thread.Command.Init`), `LuaThread` (`API.World.GenConfig`) | `LuaThread` (`API.World.GenConfig`) | `IORef WorldGenConfig` | `loadWorldGenConfig "config/world_gen_default.yaml"` (`src/Engine/Core/Init.hs:224-225`) | None | Global worldgen tunables, distinct from a specific world's `wpsGenParams`. |
| `gameTimeRef` | session-replaced | `InputThread` (`Input.Thread.Char:39`, timestamping a keystroke), `CombatThread` (`Combat.Resolution:120`, timestamping a combat event), `WorldThread` (`Thread.Command.Save.WriteWorld:88`, timestamping a save-triggered event), `UnitThread` (`Unit.Thread.Movement:61`, fall/landing timing), `LuaThread` (`API.Core:146`'s `engine.getGameTime`, direct query) | `UnitThread` (`Unit.Thread.unitLoop`, once per tick when unpaused), `WorldThread` (load publish, `World.Load.Publish:110`) | `IORef Double`, monotonic while unpaused | `0` (`src/Engine/Core/Init.hs:230`) | None | Persisted exactly (`sdGameTime`). Read from essentially every thread for event/log timestamping — enumerated by concrete role rather than `AnyThread`, since that identifier is reserved for a field with an explicitly documented unrestricted-access contract (§2.2), and this field has no such contract, just a wide but ordinary set of readers. |
| `enginePausedRef` | session-replaced | `WorldThread`/`UnitThread`/`SimThread` (skip simulated-state advancement while true), `CombatThread` (`Combat.Thread:87` — sleeps the tick, keeping queued events queued, rather than resolving combat while paused), `MainRender` (keeps rendering/input regardless), `LuaThread` (`API.Core:96`'s `isPausedFn`, direct query) | `LuaThread` (`API.Core`'s `setPausedFn`, `engine.setPaused`), `WorldThread` (load publish `World.Load.Publish:111`, always loads paused) | `IORef Bool` | `False` (`src/Engine/Core/Init.hs:229`) | None | Persisted exactly; authoritative over any Lua-side copy. |
| `simQueue` | boot-process | `SimThread` (drains; `Sim.Thread`) | `WorldThread` (`Thread.ChunkLoading`, `Command.Basic`, `Command.Edit.Sync`, `Command.UI`, and `World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish), `MainRender` (`app/App/Dump.hs:85,135`, the dump driver's own `SimPause`/`SimFastSettleAll` enqueues after worker threads start) | `Q.Queue SimCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:148`) | None | — |
| `texPaletteRef` | session-replaced | `WorldThread` (`Thread.Command.Save.WriteWorld`), `LuaThread` (`API.Structure` — placement interns paths→ids) | `LuaThread`, `WorldThread` (load publish, `World.Load.Publish:118`) | `IORef TexPalette` | `emptyTexPalette` (`src/Engine/Core/Init.hs:215`) | None | Persisted exactly as `sdTexPalette`. |
| `texPaletteHandlesRef` | session-replaced | `WorldThread` (`Structure.Render`'s `renderStructureQuads`, reached via `updateWorldTiles`), `LuaThread` (`Engine.Scripting.Lua.API.Structure:239`'s `structureUnresolvedPaletteIdsFn` — `structure.unresolvedPaletteIds()`, a direct synchronous read) | `LuaThread` (lazy per-palette-path resolution), `WorldThread` (load publish, `World.Load.Publish:121`) | `IORef (HashMap Int TextureHandle)` | `HM.empty` (`src/Engine/Core/Init.hs:216`) | None | Runtime translation table, rebuilt each session — not itself persisted. |

### `units-buildings-combat`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `unitManagerRef` | session-replaced | `UnitThread`, `CombatThread` (`Combat.Wounds.Tick`), `WorldThread` (also `Unit.Render`'s `renderUnitQuads`, reached via `updateWorldTiles` — not `MainRender`), `LuaThread` (`API.Units.Spawn:98`'s `unit.spawn`, direct def-existence check) | `UnitThread` (`Thread.Command.Lifecycle`, `Command.Pose`), `CombatThread` (`Combat.Resolution:323` — wound application, `Combat.Wounds.Tick:91` — periodic wound-tick outcomes, `Combat.Resolution.Wear:68` — weapon-wear mutation, all via `atomicModifyIORef'`), `WorldThread` (load publish, `World.Load.Publish:127`), `LuaThread` (`API.Units.Spawn:141`'s `unit.spawn`, direct unit-id allocation via `atomicModifyIORef'`) | `IORef UnitManager`, multi-writer | `emptyUnitManager` (`src/Engine/Core/Init.hs:210`) | None | — |
| `unitQueue` | boot-process | `UnitThread` (drains; `Unit.Thread.Command`) | `CombatThread` (`Combat.Wounds.Tick`, `Combat.Resolution.Events` — UnitKill/UnitCollapse), `WorldThread` (`Command.Basic`, `Command.Edit.Dig/Terrain`, and `World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish), `LuaThread` (`API.Units.Spawn`) | `Q.Queue UnitCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:211`) | The combat thread (a producer) is shut down **before** the unit thread (its consumer) — `app/App/Graphical.hs:72-76`: "Combat first: wound ticks enqueue UnitKill/UnitCollapse onto the unit queue, so the producer has to stop before the consumer... is torn down" | Deliberate shutdown ordering; see the identical rationale on `combatQueue`. |
| `utsRef` | session-replaced | `UnitThread` (`Unit.Thread`, `Thread.Command`, `Thread.Movement`, `Thread.Command.Spawn/Lifecycle/Pose`), `WorldThread` (`Thread.Command.Save.WriteWorld:101`, save capture), `LuaThread` (`API.Units.List:142`'s `unit.getInfo`, direct query) | `UnitThread`, `WorldThread` (load publish, `World.Load.Publish:128`) | `IORef UnitThreadState`, single-thread-owned by `UnitThread` outside a load publish/save capture (per the field's own doc comment, `src/Engine/Core/State.hs:222-227`) | `emptyUnitThreadState` (`src/Engine/Core/Init.hs:212`) | None | — |
| `statRNGRef` | boot-process | `UnitThread` (`Thread.Command.Spawn`, `Thread.Movement.Climb`), `CombatThread` (`Combat.Resolution`, `Combat.Wounds.Tick`), `WorldThread` (`Thread.Command.Edit.Dig` — dig-yield rolls), `LuaThread` (`API.Forage.Harvest`) | `UnitThread`/`CombatThread`/`WorldThread`/`LuaThread` (the same four roles as Readers; each roll both reads and advances the generator) | `IORef StdGen`, multi-writer, no cross-writer ordering guarantee beyond each individual roll's own atomicity | `Random.newStdGen` (`src/Engine/Core/Init.hs:213`) | None | Explicitly non-deterministic across runs by design — not save-seeded. |
| `buildingManagerRef` | session-replaced | `WorldThread` (`Render.CursorQuads`, `Thread.Power`, `Thread.ItemTemp`), `LuaThread` (`API.Power`, `API.Save`, `API.Buildings.Selection`'s `building.getSelected`), `UnitThread` (`Building.Thread.Command:31`'s `handleBuildingCommand`, drained on the unit thread) | `UnitThread` (via `Building.Thread.Command`, drained on the unit thread), `WorldThread` (load publish, `World.Load.Publish:126`), `LuaThread` (`API.Buildings.Selection:46,59`'s `building.select`/`building.deselect`, direct `atomicModifyIORef'`) | `IORef BuildingManager` | `emptyBuildingManager` (`src/Engine/Core/Init.hs:214`) | None | "Building" is a domain, not a thread — its commands are drained on `UnitThread` (`Unit.Thread` imports `Building.Thread.Command.processAllBuildingCommands`). |
| `buildingQueue` | boot-process | `UnitThread` (drains via `Building.Thread.Command.processAllBuildingCommands`) | `LuaThread` (`API.Power`, `API.Buildings.Spawn`), `WorldThread` (`World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish) | `Q.Queue BuildingCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:217`) | None | See `buildingManagerRef` note. |
| `buildingGhostRef` | session-replaced | `WorldThread` (`Building.Render`'s `renderGhostQuad`, reached via `updateWorldTiles` — placement-preview quad building, not a `MainRender` draw call) | `LuaThread` (the `build_tool` module via `API.Buildings.Spawn`), `WorldThread` (load publish, `World.Load.Publish:281`, always cleared) | `IORef (Maybe BuildingGhost)`, single-slot | `Nothing` (`src/Engine/Core/Init.hs:218`) | None | — |
| `combatQueue` | boot-process | `CombatThread` (drains at 60 Hz, `Combat.Thread.processAllCommands`) | `LuaThread` (`combat.attack` and future combat commands — per the field's own doc comment), `WorldThread` (`World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish) | `Q.Queue Combat.Types.CombatCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:219`) | `CombatThread` (the consumer here, but the *producer* for `unitQueue`) is shut down first — see `unitQueue`'s Shutdown cell | — |
| `combatEventsRef` | session-replaced | `LuaThread` (`combat.drainEvents`, `API.Combat`) | `CombatThread` (`Combat.Wounds.Tick`, `Combat.Resolution.Events`), `LuaThread` (`API.Combat:95`'s `combat.emitDeath`, direct append via `atomicModifyIORef'`), `WorldThread` (load publish, `World.Load.Publish:290`, reset to empty) | `IORef (Seq CombatEvent)` | `Combat.Types.emptyEventQueue` (`src/Engine/Core/Init.hs:220`) | None | Runtime only, never persisted. |
| `injuryEventsRef` | session-replaced | `LuaThread` (`injury.drainEvents`) | `UnitThread` (`Unit.Thread.Movement` — falls), `LuaThread` (`API.Units.Combat`'s `unit.injure`, and `injury.emit`), `WorldThread` (load publish, `World.Load.Publish:291`) | `IORef (Seq CombatEvent)` (reused shape; victim in `target`) | `emptyEventQueue` (`src/Engine/Core/Init.hs:221`) | None | A streaming consumer (the log panel) drains this — don't manually drain it while that panel script is loaded, or you'll race it. |
| `thoughtEventsRef` | session-replaced | `LuaThread` (`thought.drainEvents`) | `LuaThread` (`scripts/thoughts.lua` via `thought.emit`), `WorldThread` (load publish, `World.Load.Publish:292`) | `IORef (Seq CombatEvent)` | `emptyEventQueue` (`src/Engine/Core/Init.hs:222`) | None | — |
| `actionOutcomeRef` | session-replaced | `LuaThread` (`debug.drainActionOutcomes`, the F4 playtest oracle) | `LuaThread` (`debug.recordOutcome`), `WorldThread` (`Thread.Command.Cursor.Common/Plant` — partial-drop counts, and load publish `World.Load.Publish:293`), `InputThread` (`Input.Thread.Keyboard:51`, `Input.Thread.Mouse:98` — `pushActionOutcome` recording key/click routing outcomes) | `IORef (Seq ActionOutcome)` | `emptyActionOutcomeQueue` (`src/Engine/Core/Init.hs:223`) | None | Never surfaced to the player. |
| `pathingConfigRef` | boot-process | `UnitThread` (movement tick re-reads every tick) | None (loaded once at boot; a future settings UI is the field's own stated future intent — `src/Engine/Core/State.hs:280-285`) | `IORef PathingConfig` | `loadPathingConfig logger "config/pathing.yaml"` (`src/Engine/Core/Init.hs:226-227`) | None | "No writers" is valid today per the field's own doc comment's stated rationale. |

### `content-registries`

Every field in this group shares one shape: allocated empty at
`Engine.Core.Init.initializeEngineWith`, then populated exactly once by
the Lua thread's boot-time content-load calls (`scripts/init.lua`'s
`X.loadYaml` sequence, run before gameplay begins) — never written
again afterward.

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `itemManagerRef` | boot-process | `UnitThread` (spawn materializes `starting_inventory`), `LuaThread` (queries), `CombatThread` (`Combat.Resolution:118`'s `resolveAttack`, weapon/item def lookup), `WorldThread` (`World.Thread.Command.Edit.Dig:214`'s `spawnYieldItems`, dig-yield item def lookup) | `LuaThread` (`item.loadYaml`) | `IORef ItemManager` | `emptyItemManager` (`src/Engine/Core/Init.hs:237`), populated from `data/items/*.yaml` | None | — |
| `equipmentClassManagerRef` | boot-process | `LuaThread` (queries; also backs the UI unit-info v2 equipment section's slot layout), `UnitThread` (`Unit.Thread.Command.Spawn:106`, starting-equipment materialization) | `LuaThread` (`equipment.loadYaml`) | `IORef EquipmentClassManager` | `emptyEquipmentClassManager` (`src/Engine/Core/Init.hs:238`) | None | — |
| `substanceManagerRef` | boot-process | `LuaThread` (queries), `CombatThread` (`Combat.Resolution:119`'s `resolveAttack`, weapon-material lookup), `UnitThread` (`Unit.Thread.Movement:132`, physical-property lookup) | `LuaThread` (`substance.loadYaml`) | `IORef SubstanceManager` | `emptySubstanceManager` (`src/Engine/Core/Init.hs:239`) | None | — |
| `infectionManagerRef` | boot-process | `CombatThread`/`UnitThread` (wound tick selects an infection), `LuaThread` (`API.Infection:79`'s `infection.get`, direct query) | `LuaThread` (`infection.loadYaml`) | `IORef InfectionManager` | `emptyInfectionManager` (`src/Engine/Core/Init.hs:240`) | None | — |
| `recipeManagerRef` | boot-process | `LuaThread` (`craft.*`/`repair.*` API — the craft-bill AI itself is Lua code, so it reads this on `LuaThread`, not a Haskell unit thread), `WorldThread` (`World.Thread.Power:50`'s `tickPowerNetworks`, per-tick craft-bill power-draw lookup) | `LuaThread` (`engine.loadRecipeYaml`) | `IORef RecipeManager` | `emptyRecipeManager` (`src/Engine/Core/Init.hs:241`) | None | — |
| `locationDefsRef` | boot-process | `LuaThread` (`locations.*`, `API.Power`, `API.WorldQuery.Location`, `API.Buildings.Spawn`), `WorldThread` (`World.Render.Zoom.Quads:79`, `World.Thread.Discovery:49`) | `LuaThread` (content load) | `IORef LocationRegistry` | `emptyLocationRegistry` (`src/Engine/Core/Init.hs:242`) | None | — |
| `lootTableRegistryRef` | boot-process | `LuaThread` (`loot.roll`) | `LuaThread` (content load) | `IORef LootTableRegistry` | `emptyLootTableRegistry` (`src/Engine/Core/Init.hs:243`) | None | — |

### `ui-hud-events`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `uiManagerRef` | session-replaced | `MainRender` (`UI.Render`), `InputThread` (`Input.Thread.Keyboard:93`'s `validateFocus`, read on every keyboard dispatch), `LuaThread` (`API.UI.TextInput:47`'s `UI.getText`, `API.UI.Hierarchy:104`'s `UI.findElementAt`, and every other direct `UI.*` query) | `LuaThread` (every `UI.*` API module — `API.UI.Focus/Property/Tooltip/Hierarchy`, `API.Config`), `WorldThread` (load publish `World.Load.Publish:286`), `InputThread` (`Input.Thread.Keyboard:93,234`, atomic focus/control-focus validation on every keyboard dispatch — round 4 review notes this races the Lua thread's own concurrent element mutations, hence the atomic transition rather than a separate read/write pair), `MainRender` (`UI.Tooltip.State:50`'s `updateTooltipState`, the per-frame tooltip tick called from `Engine.Loop.Frame`, `atomicModifyIORef'`) | `IORef UIPageManager`, multi-writer via `atomicModifyIORef'` | `emptyUIPageManager` (`src/Engine/Core/Init.hs:191`) | None | Entire UI tree is rebuilt by Lua on load, per `docs/persistence_state_inventory.md`. |
| `focusManagerRef` | session-replaced | `InputThread` (`Thread.Keyboard`/`Thread.Char` — Tab/Shift+Tab control-focus navigation, #745), `LuaThread` (`API.Focus`) | `InputThread`/`LuaThread` (the same two roles as Readers), `WorldThread` (load publish, `src/World/Load/Publish.hs:284`) | `IORef FocusManager` | `createFocusManager` (`src/Engine/Core/Init.hs:196`) | None | — |
| `hudActivePageRef` | session-replaced | `WorldThread` (`Thread.Cursor` — HUD refresh-on-active-world-change, #129) | `WorldThread` (also load publish, `World.Load.Publish:283`, resynced from `wmVisible`) | `IORef (Maybe WorldPageId)` | `Nothing` (`src/Engine/Core/Init.hs:193`) | None | — |
| `textBuffersRef` | boot-process | `LuaThread` (only; `API.Text`, direct queries) | `MainRender` (only; `Engine.Scripting.Lua.Message.Scene`, dispatched via `processLuaMessages` — never the Lua thread itself) | `IORef (Map ObjectId Text)` | `Map.empty` (`src/Engine/Core/Init.hs:197`) | None | Editable-widget text keyed by `ObjectId`, per the UI text-buffer coordinate contract. |
| `eventStoreRef` | session-replaced | `LuaThread` (`API.PlayerEvent:99`'s `readEventLog` — `engine.getEventLog()`, the event-log panel's query) | `WorldThread` (`World.Thread.Discovery`'s `emitEventFullOnPage`, `World.Thread.Command.Save.WriteWorld`'s `emitEvent`, and load publish `World.Load.Publish:294`, reset to empty), `LuaThread` (`API.PlayerEvent`'s `emitEvent`/`emitEventAt`/`emitEventFull` — `engine.emitEvent`/`emitEventAt`/`emitEventForUnit` — and `API.Save`'s save/load-lifecycle emits) | `TVar (Seq PlayerEvent)`, multi-writer STM, ~1000-entry ring | `newTVarIO Seq.empty` (`src/Engine/Core/Init.hs:255`) | None | Explicitly session-only, never serialized. No live `Unit.Thread`/`Combat.Thread` call site emits a player event today, despite `Engine.PlayerEvent.Emit`'s own module comment claiming it's "safe to call from world, unit, and Lua threads concurrently" (a thread-safety guarantee about the STM primitive, not a claim that a unit-thread caller currently exists) — verified by grepping every real `emitEvent*` call site. |
| `notificationCfgRef` | boot-process | `AnyThread` (the `emitEvent` read path) | `LuaThread` (Phase 2 settings tab toggles, per the field's own doc comment) | `IORef NotificationCfg` | `loadNotificationCfg` merges `data/notification_categories.yaml` + `config/notifications.local.yaml` (`src/Engine/Core/Init.hs:251-254`) | None | — |
| `notificationOrder` | boot-process | `LuaThread` (settings tab render order) | None (captured once at boot from the YAML registry order — categories can't be added/removed at runtime, per the field's own doc comment) | Plain `![Text]`, no `IORef` | `loadNotificationCfg`'s second return value (`src/Engine/Core/Init.hs:251-254`) | None | Immutable-boot-configuration carve-out, same shape as `engineConfig`. |
| `popupQueueRef` | session-replaced | None (write-only today — no `readTVar`/`readTVarIO` on this ref exists anywhere in the codebase; live popup delivery goes through a separate `LuaShowPopup` message sent via `luaQueue` at the same emit call site, `Engine.PlayerEvent.Emit:119-121`, not by draining this TVar back out, despite `EngineEnv`'s own field-doc comment at `src/Engine/Core/State.hs:377-378` claiming the Lua side drains this via the LuaShowPopup broadcast — that comment is itself stale/inaccurate. This TVar exists for inspection/debug querying and as a Phase 2 stable source for the notifications panel, per the same comment.) | `WorldThread`/`LuaThread` (same `emitEvent` producers as `eventStoreRef`, filtered to popup-enabled categories, `Engine.PlayerEvent.Emit:118`), `WorldThread` (load publish, `World.Load.Publish:295`, reset to empty) | `TVar (Seq PlayerEvent)` | `newTVarIO Seq.empty` (`src/Engine/Core/Init.hs:256`) | None | — |

### `save-load-coordination`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `loadStatusRef` | boot-process | `WorldThread` (`Thread.Time`, `Thread.Command.Save`), `MainRender` (`Engine.Scripting.Lua.Message.discardLuaMessagesForActiveLoad`, called by the render consumers while `captureLocked`), `LuaThread` (`API.Core`, `Thread.Dispatch`, `API.Save`) | `WorldThread`/`LuaThread` (the same two roles as Readers) | `LoadStatusRef` (opaque, internally synchronized — see `Engine.Load.Status`) | `newLoadStatusRef` (`src/Engine/Core/Init.hs:194`) | None | Diagnostic only, never serialized. |
| `pendingLoadRef` | transient-handoff | `WorldThread`, `LuaThread` (`Thread.Dispatch`) | `WorldThread` (written when a staged-load transaction finishes staging; read and cleared when the matching publish command runs, per the field's own doc comment), `LuaThread` (`Thread.Dispatch:438,460`, cleared on a load-publish failure path before the prepared-but-never-applied Lua load is aborted) | `IORef (Maybe (Int, StagedSession))`, single-slot, keyed by request id defensively | `Nothing` (`src/Engine/Core/Init.hs:195`) | None | Only one load is ever in flight, enforced by `loadStatusRef`. |
| `saveBarrierRef` | boot-process | `UnitThread`, `CombatThread`, `WorldThread` (`Thread.Command.Save`, `Command.Save.WriteWorld`), `MainRender` (`Engine.Loop`), `LuaThread` (`Engine.Scripting.Lua.Thread:227`'s `captureLocked` check, and `Thread.Dispatch:413-455`'s `handleLoadStaged`, which itself DRIVES a whole load-publish transaction via `beginSave`/`acknowledgeSave`/`waitForOwners`/`reachSnapshot`/`failSave`), `InputThread` (`Input.Thread:90`'s `captureLocked` check in the input loop's own per-tick gate), `SimThread` (`Sim.Thread:98`'s `captureLocked` check) | `UnitThread`, `CombatThread`, `WorldThread`, `MainRender`, `LuaThread` (`Thread.Dispatch:413-455` — the Lua thread is the transaction driver for a load publish, not merely one of the acknowledging owners), `InputThread` (`Input.Thread:97`'s `acknowledgeCurrent (saveBarrierRef env) SaveInput`, once per input-loop tick), `SimThread` (`Sim.Thread:130,144`'s `acknowledgeCurrent (saveBarrierRef env) SaveSimulation`, once per sim-loop tick) | `SaveBarrier` (opaque, internally-synchronized coordination record — see `Engine.Save.Barrier`) | `newSaveBarrier` (`src/Engine/Core/Init.hs:231`) | None | Every state-owner thread that must acknowledge a save boundary reads/writes this; diagnostic/coordination only, never serialized. |
| `lastSaveTimeRef` | boot-process | `LuaThread` (only) | `LuaThread` (only; `API.Save.saveWorldFn`, clamps each save strictly past this for monotonic ordering, #98) | `IORef UTCTime` | POSIX epoch (`src/Engine/Core/Init.hs:236`) | None | — |
| `nextItemInstanceIdRef` | session-replaced | `AnyThread` (via `freshItemInstanceId`, `Engine.Core.State` — item rolls/spawns) | `AnyThread` (via `freshItemInstanceId`), `WorldThread` (load publish, `World.Load.Publish:125`) | `IORef Word64`, monotonic allocator, thread-safe atomic bump (`atomicModifyIORef'`) | `1` (`src/Engine/Core/Init.hs:165`) | None | Persisted exactly, restored `max(loaded, current)` — never lowered (#67). |

## 6. Full-`EngineEnv` compatibility boundary

**Live since issue #889 (E1, landed):** 223 files under `src/`/`app/`
import `Engine.Core.State` in some form. Of those, 207 have genuine
unrestricted field-level access: `Engine.Core.State.hs` itself (which
defines `EngineEnv` and therefore imports nothing) plus 206 files that
import it either as an explicit `EngineEnv(..)` (in any combination
with other names on the same import line) or as a **bare**
`import Engine.Core.State` with no explicit list at all — Haskell
grants a bare import full access to everything the target module
exports, `EngineEnv(..)` included, so this is exactly as unrestricted
as the explicit form. Both forms are recognized regardless of
`qualified`/`as`-aliasing or the import spanning multiple lines;
`tools/engine_env_capability_audit.py`'s SS6 ratchet enforces
this exact same two-shape definition against `src/`/`app/` on every
run, verified with:

```
grep -rl "import Engine.Core.State" src app | wc -l                    # 223
# then, per file, whether the import clause is bare or explicitly
# names EngineEnv(..) vs. a strictly narrower list (EngineEnv with no
# (..), a single field accessor, or EngineState instead) — see the
# script logic below; 206 have full access, 17 do not:
#   13 × `Engine.Scripting.Lua.API.Register.*` (`Engine.Scripting.Lua.API`
#        itself plus its 12 `Register.*` submodules; import `(EngineEnv)`,
#        the bare TYPE with no constructor/field access — opaque)
#   1  × `Engine.Core.Resource` (imports only the `loggerRef` accessor)
#   1  × `Engine.Scene.Graph` (imports `EngineState(..)`, not `EngineEnv`)
#   1  × `Engine.Core.Log.Monad` (narrowed by #889 — imports only the
#        bare `EngineEnv` type, deriving everything through
#        `Engine.Core.Capability.Core.toCoreCapability` instead of a
#        direct field accessor)
#   1  × `Engine.Core.Capability.Core` (new by #889 — the `core-init`
#        capability-record projection module itself; imports the bare
#        `EngineEnv` type plus its four `core-init` field accessors,
#        never `EngineEnv(..)`)
```

The remaining 17 files that import `Engine.Core.State` (223 − 206) are
exactly the ones enumerated above — none of them are consumers this
document needs to classify: an opaque `EngineEnv` type import, one or
more individually named field accessors, or an unrelated `EngineState`
import none grant the unrestricted access this section is about.
Adding back `Engine.Core.State.hs` itself (the definer, which imports
nothing and so is outside the 223/206/17 accounting entirely) gives
the 207 total full-access modules this section classifies.

This section names the intended *end state*: what should still
legitimately construct, carry, or inspect the **complete** `EngineEnv`
once the epic's capability split has landed, versus what merely has
full access today because nothing narrower exists yet. It is
deliberately narrow — narrow enough to become the literal allowlist
for #537's final unrestricted-access audit (per requirement 6) — which
means most of today's 207 full-access files are **not** listed as
permanent below; they belong in the temporary section (§6.2), each
assigned individually (no wildcards, no catch-all) to one of §7's
bounded follow-up issues.

### 6.1 Permanent (production)

Requirement 6 asks each exception to state which of three reasons it
is: permanent initialization/orchestration infrastructure, a temporary
compatibility boundary, or the engine-monad carrier itself. Every row
below is one of the first or third — nothing in this permanent section
is the second, by definition of the section.

| Module(s) | Category | Reason |
|---|---|---|
| `Engine.Core.State` | Permanent initialization infrastructure | Defines `EngineEnv`/`EngineState` — the type itself; every other entry in this table depends on this one existing. |
| `Engine.Core.Monad` | The engine-monad carrier itself | `EngineM ε σ α`'s Reader environment *is* `EngineEnv` for the top-level engine monad (`EngineM' EngineEnv`) — this is requirement 6's third category, named explicitly. |
| `Engine.Core.Init` | Permanent initialization infrastructure | Constructs the single `EngineEnv` value; by construction, must name every field once. |
| `Engine.Core.Defaults` | Permanent initialization infrastructure | Provides the default values (`defaultEngineConfig`, `defaultEngineState`, `defaultWindowConfig`, ...) that `Engine.Core.Init` assembles into the single `EngineEnv`/`EngineState` — it never reads a *live* `EngineEnv`'s fields (no `asks`/`gets`/`readIORef env ...` anywhere in the module), it only builds the record values those fields start out holding; the same construction-time role as `Engine.Core.Init` one level down. |
| `Engine.Loop`, `Engine.Loop.Frame`, `Engine.Loop.Headless`, `Engine.Loop.Shutdown`, `Engine.Loop.Camera`, `Engine.Loop.Timing`, `Engine.Loop.Resource` | Permanent orchestration infrastructure | The main loop's job each frame is to coordinate render output with several other capabilities' queues/state (input barrier tokens, world quads, screenshot requests, ...), and `shutdownEngine` performs the one coordinated cross-capability teardown boundary described in §2.4/§3. |
| `app/App/Graphical.hs`, `app/App/Offscreen.hs`, `app/App/Preview.hs`, `app/App/Headless.hs`, `app/App/Dump.hs` | Permanent orchestration infrastructure | Top-level boot/wire-up: each necessarily constructs the engine, starts every thread the profile needs, and wires them together — inherently whole-environment by job description. |
| `Engine.Scripting.Lua.Thread`, `Engine.Scripting.Lua.Thread.Dispatch`, `Engine.Scripting.Lua.Thread.Console` | Permanent orchestration infrastructure | The Lua thread's own dispatch plumbing registers *every* Lua API module against the full environment (`registerLuaAPI`) — this wiring point is inherently cross-capability, even though each individual `Engine.Scripting.Lua.API.*`/`Message.*` module it wires (§6.2) is not. `Thread.Console` is the debug-console command handler living in the same package — TCP debug-server builtins and single-line Lua command execution, both reached from the same core Lua-thread loop as `Dispatch`. |
| `Engine.Scripting.Lua.Message` | Permanent orchestration infrastructure | `processLuaMessages` is the per-frame, `MainRender`-side counterpart to `Thread.Dispatch`: it drains `luaToEngineQueue` and routes every category of Lua-originated message to its per-domain handler (`Message.Video`, `Message.Texture`, `Message.WorldTexture`, `Message.Scene`, §6.2) — inherently cross-capability dispatch infrastructure, not a consumer of any one capability's fields itself. |
| `World.Thread.Command.Save`, `World.Thread.Command.Save.WriteWorld`, `World.Load.Stage`, `World.Load.Publish`, `Engine.Scripting.Lua.API.Save` | Permanent orchestration infrastructure | A save/load transaction is inherently a whole-session boundary: these five modules are the exact, verified set that actually `import Engine.Core.State (EngineEnv(..))` on the save/load path (`grep -rn 'import Engine.Core.State' src/World/Load src/World/Thread/Command/Save* src/Engine/Scripting/Lua/API/Save.hs`) — they must capture or replace every capability's state atomically in one coordinated step (see the persistence contract's snapshot/publish design). Narrowing this to per-capability records would just reconstruct an env-shaped aggregate one level down — this is a permanent exception, not a temporary one awaiting migration. Everything ELSE under `World.Save.*` (`Snapshot`, `Types`, `Component*`, `Envelope*`, `Serialize`, `Storage`, `Integrity`, `Reference`, `Compat*`) is pure data/codec code that never touches `EngineEnv` at all (`World.Save.Snapshot`'s own doc comment states this explicitly) and is correctly outside this list entirely — not a temporary compatibility boundary either, since it was never given full access in the first place. `Engine.Save.Barrier`/`Engine.Load.Status` are the same: opaque coordination types referenced FROM `EngineEnv` (`saveBarrierRef`/`loadStatusRef`), not consumers of it — neither imports `EngineEnv`. |

That's 25 permanent modules (24 importers + `Engine.Core.State` itself,
which imports nothing). The remaining 207 − 25 = 182 full-access
modules are temporary, enumerated exhaustively in §6.2.

Since issue #889, this permanent allowlist and §6.2's temporary
accounting are also enforced live: `tools/engine_env_capability_audit.py`'s
checked-in `PERMANENT_IMPORTERS`/`TEMPORARY_CEILING` constants mirror
this document's §6.1/§6.2 exactly, and the audit fails if the
live-scanned production importer set ever disagrees with either.

### 6.2 Temporary compatibility boundary (production)

Every one of the 182 remaining full-access modules is individually
assigned below to exactly one target capability — **no path-prefix
globs, no "and similar" language, and no catch-all row**: every name
in every cell is a literal, complete Haskell module name. The
assignment method, applied uniformly and mechanically rather than by
directory-name guessing:

1. For each module, scan its source for every occurrence of one of the
   81 `EngineEnv` field names from §5 (`asks`/`gets`/`readIORef env
   ...`/`atomicModifyIORef' ... env`/`writeIORef ... env` patterns, and
   plain field-name references) and tally which capability group (§5's
   heading structure) each hit belongs to.
2. Four fields — `loggerRef`, `lifecycleRef`, `engineConfig`,
   `inputThreadActiveRef` — are read from nearly every module in the
   codebase purely for logging/boot-config boilerplate (§5's own
   `loggerRef` row: "every thread logs through it"). Counting these
   would swamp the real signal, so they're excluded from the tally
   *unless* they are the only hits a module has (in which case the
   module's whole purpose genuinely is `core-init`, e.g.
   `Engine.Core.Log.Monad`).
3. The module is assigned to whichever capability has the most
   remaining tallied hits. Four modules never call `asks`/`gets`/
   `readIORef` on any field at all (`Engine.Graphics.Font.Draw` and
   seven sibling low-level Vulkan modules, `Engine.Input.Callback`,
   `Engine.Scene.Render`, and four `Engine.Scripting.Lua.API.WorldQuery.*`
   query modules) — these take their capability from their own,
   unambiguous package/directory role instead (Vulkan pipeline/
   swapchain/sync internals and font drawing → `render-gpu-asset`;
   a GLFW callback registrar → `input-lua-transport`; scene rendering
   from `EngineState`'s `graphicsState`/`sceneManager`, out of §1's
   scope but unambiguously render-side → `render-gpu-asset`; world-data
   queries alongside their tallied siblings → `world-sim-render-handoff`).
   One module (`UI.Tooltip.State`) is assigned by evident purpose over
   a weak, low-count tally: its central *mutation* is `uiManagerRef`
   (`ui-hud-events`), even though it also *reads* three `render-gpu-asset`
   fields (`fontCacheRef`/`windowSizeRef`/`framebufferSizeRef`) to
   compute tooltip layout geometry — a genuine cross-capability read,
   not a migration blocker, the same shape §7.3 already documents for
   `input-lua-transport`'s writes crossing into World/UI.
4. This process caught and corrected two mistakes from an earlier
   iteration of this document: `World.Log` was previously claimed to
   read `worldManagerRef`/`texPaletteHandlesRef` for diagnostics, but
   it actually only ever touches `luaQueue` (`input-lua-transport`) —
   the earlier claim was an unverified guess; `Structure.Render` was
   previously grouped with `World.Render.*` under
   `world-sim-render-handoff` by directory-name similarity, but its
   actual field tally (`textureSizeRef`+`textureSystemRef`, both
   `render-gpu-asset`, vs. one `texPaletteHandlesRef` hit) puts it in
   `render-gpu-asset` instead — the same capability several of its
   `World.Render.*Quads` siblings land in once measured the same way
   (§6.2's table below no longer treats "`World.Render.*`" as a single
   monolithic family for exactly this reason).

| Target capability | Modules (every current temporary full-`EngineEnv` consumer, individually assigned) | Roadmap entry |
|---|---|---|
| `core-init` | `Engine.Graphics.Vulkan.Command.Record`, `Engine.Scripting.Lua.API.Log` | §7.1 |
| `render-gpu-asset` | `Building.HitTest`, `Building.Render`, `Engine.Asset.Manager`, `Engine.Graphics.Font.Draw`, `Engine.Graphics.Font.Load`, `Engine.Graphics.Font.Upload`, `Engine.Graphics.Vulkan.Command.Sprite`, `Engine.Graphics.Vulkan.Command.Text`, `Engine.Graphics.Vulkan.Framebuffer`, `Engine.Graphics.Vulkan.Init`, `Engine.Graphics.Vulkan.MSAA`, `Engine.Graphics.Vulkan.Offscreen`, `Engine.Graphics.Vulkan.Pipeline`, `Engine.Graphics.Vulkan.Pipeline.Bindless`, `Engine.Graphics.Vulkan.Recreate`, `Engine.Graphics.Vulkan.Swapchain`, `Engine.Graphics.Vulkan.Sync`, `Engine.Graphics.Vulkan.Texture.Bindless`, `Engine.Graphics.Vulkan.Texture.DefaultFaceMap`, `Engine.Graphics.Window.GLFW`, `Engine.Scene.Batch.Text`, `Engine.Scene.Render`, `Engine.Scripting.Lua.API.Camera`, `Engine.Scripting.Lua.API.Config`, `Engine.Scripting.Lua.API.Graphics`, `Engine.Scripting.Lua.API.Input`, `Engine.Scripting.Lua.API.Items.Render`, `Engine.Scripting.Lua.API.Screenshot`, `Engine.Scripting.Lua.API.Text`, `Engine.Scripting.Lua.API.UI.Placement`, `Engine.Scripting.Lua.API.WorldQuery.Pick`, `Engine.Scripting.Lua.API.YamlTextures`, `Engine.Scripting.Lua.Message.Texture`, `Engine.Scripting.Lua.Message.Video`, `Engine.Scripting.Lua.Message.WorldTexture`, `Structure.Render`, `UI.Render`, `Unit.HitTest`, `World.Render`, `World.Render.BloodQuads`, `World.Render.CursorQuads`, `World.Render.GroundItemQuads`, `World.Render.Quads`, `World.Render.SpoilQuads`, `World.Render.Zoom.Quads` | §7.2 |
| `input-lua-transport` | `Engine.Input.Callback`, `Engine.Input.Thread`, `Engine.Input.Thread.Char`, `Engine.Input.Thread.Dispatch`, `Engine.Input.Thread.Keyboard`, `Engine.Input.Thread.Mouse.Activation`, `Engine.Input.Thread.Scroll`, `Engine.Scripting.Lua.API.InputInject`, `Engine.Scripting.Lua.API.Keybinds`, `World.Log`, `World.Thread.Helpers` | §7.3 |
| `world-sim-render-handoff` | `Blood.Impact`, `Engine.Scripting.Lua.API.Blood`, `Engine.Scripting.Lua.API.Chop`, `Engine.Scripting.Lua.API.Construct`, `Engine.Scripting.Lua.API.Core`, `Engine.Scripting.Lua.API.Flora`, `Engine.Scripting.Lua.API.Forage.Crop`, `Engine.Scripting.Lua.API.Forage.Lookup`, `Engine.Scripting.Lua.API.Forage.Query`, `Engine.Scripting.Lua.API.Plant`, `Engine.Scripting.Lua.API.Structure`, `Engine.Scripting.Lua.API.Till`, `Engine.Scripting.Lua.API.World.Clock`, `Engine.Scripting.Lua.API.World.Cursor`, `Engine.Scripting.Lua.API.World.Designation`, `Engine.Scripting.Lua.API.World.Edit`, `Engine.Scripting.Lua.API.World.GenConfig`, `Engine.Scripting.Lua.API.World.Lifecycle`, `Engine.Scripting.Lua.API.World.Query`, `Engine.Scripting.Lua.API.World.Tools`, `Engine.Scripting.Lua.API.WorldQuery.Chunk`, `Engine.Scripting.Lua.API.WorldQuery.Climate`, `Engine.Scripting.Lua.API.WorldQuery.Fluid`, `Engine.Scripting.Lua.API.WorldQuery.Lookup`, `Engine.Scripting.Lua.API.WorldQuery.River`, `Engine.Scripting.Lua.API.WorldQuery.Terrain`, `Sim.Thread`, `Unit.LineOfSight`, `Unit.Render`, `Unit.Thread.Movement.PathAdvance`, `World.Render.Zoom.Background`, `World.Thread`, `World.Thread.ChunkLoading`, `World.Thread.Command`, `World.Thread.Command.Basic`, `World.Thread.Command.Cursor.Chop`, `World.Thread.Command.Cursor.Construct`, `World.Thread.Command.Cursor.Mine`, `World.Thread.Command.Cursor.Plant`, `World.Thread.Command.Cursor.Select`, `World.Thread.Command.Cursor.Till`, `World.Thread.Command.Edit.Fluid`, `World.Thread.Command.Edit.Structure`, `World.Thread.Command.Edit.Sync`, `World.Thread.Command.Edit.Terrain`, `World.Thread.Command.Edit.Vegetation`, `World.Thread.Command.Init`, `World.Thread.Command.Location`, `World.Thread.Command.Texture`, `World.Thread.Command.Time`, `World.Thread.Command.UI`, `World.Thread.Cursor`, `World.Thread.Time` | §7.4 |
| `units-buildings-combat` | `Building.Thread.Command`, `Combat.Resolution`, `Combat.Resolution.Events`, `Combat.Resolution.Wear`, `Combat.Thread`, `Combat.Wounds.Tick`, `Engine.Input.State`, `Engine.Scripting.Lua.API.ActionOutcome`, `Engine.Scripting.Lua.API.Buildings.Materials`, `Engine.Scripting.Lua.API.Buildings.Progress`, `Engine.Scripting.Lua.API.Buildings.Query`, `Engine.Scripting.Lua.API.Buildings.Selection`, `Engine.Scripting.Lua.API.Buildings.Spawn`, `Engine.Scripting.Lua.API.Buildings.Yaml`, `Engine.Scripting.Lua.API.Combat`, `Engine.Scripting.Lua.API.Craft.Bill`, `Engine.Scripting.Lua.API.Craft.Execute`, `Engine.Scripting.Lua.API.Equipment.Accessory`, `Engine.Scripting.Lua.API.Equipment.Render`, `Engine.Scripting.Lua.API.Equipment.Slot`, `Engine.Scripting.Lua.API.Forage.Harvest`, `Engine.Scripting.Lua.API.Items.Ground`, `Engine.Scripting.Lua.API.Power`, `Engine.Scripting.Lua.API.Units.Cargo`, `Engine.Scripting.Lua.API.Units.Combat`, `Engine.Scripting.Lua.API.Units.Equipment`, `Engine.Scripting.Lua.API.Units.Inventory`, `Engine.Scripting.Lua.API.Units.List`, `Engine.Scripting.Lua.API.Units.Medical`, `Engine.Scripting.Lua.API.Units.Query`, `Engine.Scripting.Lua.API.Units.Selection`, `Engine.Scripting.Lua.API.Units.Spawn`, `Engine.Scripting.Lua.API.Units.Stats`, `Engine.Scripting.Lua.API.Units.Survival`, `Engine.Scripting.Lua.API.Units.Yaml`, `Unit.Selection`, `Unit.Thread`, `Unit.Thread.Command`, `Unit.Thread.Command.Lifecycle`, `Unit.Thread.Command.Motion`, `Unit.Thread.Command.Pose`, `Unit.Thread.Command.Spawn`, `Unit.Thread.Movement`, `Unit.Thread.Movement.Climb`, `World.Thread.Command.Cursor.Common`, `World.Thread.Command.Edit.Dig`, `World.Thread.Discovery`, `World.Thread.ItemTemp`, `World.Thread.Power` | §7.5 |
| `content-registries` | `Engine.Scripting.Lua.API.Craft.Recipe`, `Engine.Scripting.Lua.API.Equipment.Class`, `Engine.Scripting.Lua.API.Infection`, `Engine.Scripting.Lua.API.Items.Defs`, `Engine.Scripting.Lua.API.Locations`, `Engine.Scripting.Lua.API.LootTables`, `Engine.Scripting.Lua.API.Repair`, `Engine.Scripting.Lua.API.Substance`, `Engine.Scripting.Lua.API.WorldQuery.Location` | §7.6 |
| `ui-hud-events` | `Engine.Input.Thread.Mouse`, `Engine.PlayerEvent.Emit`, `Engine.Scripting.Lua.API.Focus`, `Engine.Scripting.Lua.API.PlayerEvent`, `Engine.Scripting.Lua.API.UI.Element`, `Engine.Scripting.Lua.API.UI.Focus`, `Engine.Scripting.Lua.API.UI.Hierarchy`, `Engine.Scripting.Lua.API.UI.Page`, `Engine.Scripting.Lua.API.UI.Property`, `Engine.Scripting.Lua.API.UI.TextInput`, `Engine.Scripting.Lua.API.UI.Tooltip`, `Engine.Scripting.Lua.Message.Scene`, `UI.Tooltip.State` | §7.7 |
| `save-load-coordination` | *(none — every module whose dominant field usage is save/load coordination is already a permanent orchestration exception listed in §6.1; `Engine.Scripting.Lua.API.Core` was previously assigned here for its one `loadStatusRef` read, but its dominant usage — `enginePausedRef`/`gameTimeRef`, both read/written more often in the same file — is `world-sim-render-handoff`, so it is listed there instead)* | §7.8 |

Row counts (2 + 45 + 11 + 53 + 49 + 9 + 13 + 0 = 182) match
207 − 25 exactly — every temporary full-access module is accounted for
in exactly one row above.

### 6.3 Test-only exceptions

Test fixtures are listed separately, per requirement 6, since a test's
job is routinely to construct and inspect a **complete**, working
`EngineEnv` — that is not the same "hasn't been migrated yet" gap as
production code, and narrowing test fixture access is not a §7 goal.

| Module(s) | Reason |
|---|---|
| `test/Test/Headless/Harness.hs` and every `Test.Headless.*` module built on it | A headless hspec fixture's entire purpose is booting one working engine environment (`initializeEngineHeadless`/`initializeEngineHeadlessWith`) and sharing or inspecting it across many test cases (see `Test.Headless.UI.ResponsiveGameplay`'s `withSharedFixture` for the canonical example) — broad, whole-environment access here is the intended design, not a compatibility gap. |
| `tools/*_probe.py` (real-engine turnkey harnesses) | Drive a real booted engine over the debug console; not Haskell code and not subject to an import-level allowlist at all, but listed here for completeness since they routinely exercise every capability of a running engine. |

## 7. Migration roadmap

For each capability group: which fields it owns (§5's table for that
group, not repeated here), which module families consume it today
(§6.2's corresponding row), its dependencies on other capability
groups, whether it can migrate independently, and the bounded
follow-up scope a future child issue should have. **This section
defines future scopes; it does not create those issues** (out of
scope, per the issue text).

### 7.1 `core-init`

- **Landed by #889.** `Engine.Core.Capability.Core` introduces
  `CoreCapability` (`ccEngineConfig`, `ccLoggerRef`, `ccLifecycleRef`,
  `ccInputThreadActiveRef`) and its total `toCoreCapability ∷ EngineEnv
  → CoreCapability` projection, establishing the capability-record
  convention E2+ follow. `Engine.Core.Log.Monad` — §6.2's one
  `core-init` module whose migration this issue actually required — no
  longer imports `EngineEnv(..)`/a bare `Engine.Core.State`: its
  capability-scoped primitives (`getLoggerFor`, `logInfoFor`, ...) take
  `CoreCapability` explicitly, and its original `MonadReader EngineEnv`
  names (`logInfoM`, ...) are now thin wrappers over them, so none of
  the ~440 existing production call sites needed to change.
  `Engine.Loop.Shutdown`'s core-only tail (logger flush + lifecycle
  write) is similarly narrowed into a `finalizeCoreShutdown ∷
  CoreCapability → ...` helper, while `shutdownEngine` itself stays a
  permanent, whole-`EngineEnv` orchestration function (§6.1) — the
  rest of it still needs graphics/window/thread capabilities.
  `Engine.Core.Init`'s own helpers already took narrower explicit
  values before this issue and needed no change. §6.2's `core-init` row
  still names `Engine.Graphics.Vulkan.Command.Record` and
  `Engine.Scripting.Lua.API.Log` — this issue did not migrate them;
  they remain live temporary `core-init` consumers for a later child.
- **Dependencies:** None — every other capability group depends on
  this one being available first (the logger and lifecycle flag are
  read from every thread), so this is necessarily the first migration,
  not something that can be deferred.
- **Independent migration:** Yes, and it went first.
- **Follow-up scope (remaining):** Narrow
  `Engine.Graphics.Vulkan.Command.Record`/`Engine.Scripting.Lua.API.Log`
  to `CoreCapability` where feasible. Given how universally
  `loggerRef`/`lifecycleRef` are read, most call sites will still need
  to reach them through a broader carrier for a while yet — this
  migration was about establishing the record and proving the pattern
  on one real consumer, not about shrinking every import immediately.

### 7.2 `render-gpu-asset`

- **Dependencies:** `core-init` (logger, lifecycle).
- **Independent migration:** Yes, for the `MainRender`-only fields
  (`engineStateRef` and everything genuinely single-thread-owned).
  `textureSystemRef`/`textureSizeRef` are the one real complication —
  `WorldThread` reads both (via `Unit.Render`/`World.Render.*`'s
  quad-building pass; see §3's note on why they moved to `EngineEnv`
  in the first place) even though writes stay confined to `MainRender`
  (the `World.Render.BloodQuads` upload/dispose functions run via
  `processLuaMessages`, not the world thread's own quad-building path —
  see their §5 rows), so this capability's record cannot be scoped to
  "things only the render thread touches"; it must be a record the
  world thread can legitimately import for reading, even though it
  never writes through it.
- **Follow-up scope:** One issue narrowing `Engine.Graphics.*`/
  `UI.Render`/the render-adjacent `Engine.Scripting.Lua.Message.*`
  modules (`Video`, `Texture`, `WorldTexture`) to a `RenderCapability`
  record with the 20 fields in §5's `render-gpu-asset` table.

### 7.3 `input-lua-transport`

- **Dependencies:** `core-init`.
- **Independent migration:** Mostly yes. `focusManagerRef` is
  classified under `ui-hud-events` rather than here specifically
  because `Engine.Input.Thread.Keyboard`/`Char` write it directly for
  keyboard control-focus navigation (#745) — this capability's own
  migration and `ui-hud-events`'s will need to land in an order (or
  together) that keeps that direct write working.
- **Follow-up scope:** One issue narrowing `Engine.Input.Thread.*`/
  `Engine.Input.Inject`/`Engine.Input.Callback` and the Lua-transport
  queue producers/consumers to an `InputCapability` record.

### 7.4 `world-sim-render-handoff`

- **Dependencies:** `render-gpu-asset` (the render-handoff fields —
  `worldPreviewRef`/`zoomAtlasDataRef`/`worldQuadsRef` — are read by
  `MainRender`, so this group's record and `render-gpu-asset`'s record
  will need to be importable together at their shared boundary, or
  this group's migration should land after §7.2's), `units-buildings-combat`
  (`materialRegistryRef` is read by `Unit.Thread.Movement`).
- **Independent migration:** Partial. The pure worldgen/world-thread
  side (`worldQueue`, `sunAngleRef`, `floraCatalogRef`,
  `worldGenConfigRef`, `simQueue`) can move on its own; the
  render-handoff fields are the coupled part.
- **Follow-up scope:** Likely two child issues — one for the
  world/sim/worldgen fields proper, one for the render-handoff single-
  slot uploads — rather than one, given the coupling above.

### 7.5 `units-buildings-combat`

- **Dependencies:** `world-sim-render-handoff` (unit/building state
  routinely cross-references world position/material data), `core-init`.
- **Independent migration:** Partial — `statRNGRef` is shared across
  `UnitThread`/`CombatThread`/`WorldThread`/`LuaThread` (dig-yield
  rolls), so it either needs its own tiny shared capability or this
  group's record needs to be importable from `World.Thread.Command.Edit.Dig`
  too.
- **Follow-up scope:** One issue per sub-domain is likely cleanest given
  the size (13 fields, three real consumer families — units, buildings,
  combat) — e.g. units+combat together (they already share
  `unitQueue`/`combatQueue`'s producer/consumer relationship and the
  documented shutdown-ordering dependency between them), buildings
  separately (consumed on `UnitThread` but conceptually its own domain).

### 7.6 `content-registries`

- **Dependencies:** `core-init` only (content is loaded once, read-only
  thereafter — the least coupled group in this inventory).
- **Independent migration:** Yes, cleanly. This is a strong candidate
  for an *early* migration precisely because none of its 7 fields are
  ever written after boot-time content load, so there is no
  write-ordering subtlety to resolve.
- **Follow-up scope:** One issue introducing a `ContentRegistries`
  record for all 7 fields and narrowing every `Engine.Scripting.Lua.API.*`
  content-query module (and the couple of engine-side consumers —
  `Unit.Thread.Command.Spawn`, `Combat.Wounds.Tick`,
  `World`'s location/loot spawn resolution) to it.

### 7.7 `ui-hud-events`

- **Dependencies:** `render-gpu-asset` (`UI.Render` needs both UI state
  and render/GPU handles — a genuine cross-capability read, not a
  migration blocker but something the eventual record boundary must
  accommodate), `input-lua-transport` (`focusManagerRef`, see §7.3).
- **Independent migration:** Partial, for the reasons above.
- **Follow-up scope:** One issue for the UI/focus/HUD fields
  (`uiManagerRef`, `focusManagerRef`, `hudActivePageRef`,
  `textBuffersRef`), one for the event/notification/popup fields
  (`eventStoreRef`, `notificationCfgRef`, `notificationOrder`,
  `popupQueueRef`) — the two halves have almost no consumers in common.

### 7.8 `save-load-coordination`

- **Dependencies:** Every other group, transitively — a save/load
  transaction observes the whole session by design (see §6.1's
  permanent-exception entry for `World.Thread.Command.Save`/
  `World.Load.Stage`/`World.Load.Publish`/`Engine.Scripting.Lua.API.Save`).
  This group's own five fields (the *coordination* state: barrier, load
  status, staged-load handoff, last-save-time, the item-instance
  allocator) are narrower than the transaction machinery itself, but
  migrating them meaningfully still means threading a
  `SaveLoadCapability` record through code that, by its own nature (per
  §6.1), needs to reach everything else too.
- **Independent migration:** No — this should be the **last** group
  migrated, once every other capability record exists for the save/load
  machinery to compose from.
- **Follow-up scope:** Revisit once §7.1-§7.7 have landed; likely folds
  into whatever issue finally narrows those four modules' own internal
  structure, rather than standing alone.
