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
| `MainRender` | The process's original thread running `Engine.Loop.mainLoop` (graphical/offscreen/preview profiles) — Vulkan calls, frame timing, camera update, GLFW window/callback management. Under headless/dump this role does not exist as a *rendering* thread, but the same OS thread still runs the equivalent headless drive loop (`Engine.Loop.Headless`) and remains the sole owner of `EngineState`. |
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
| `loggerRef` | boot-shutdown | `AnyThread` — every thread logs through it (`Unit.Thread`, `Combat.Thread`, `World.Thread`, `Sim.Thread`, `Engine.Scripting.Lua.Thread`, `Engine.Loop`) | `AnyThread` — `Engine.Core.Log`'s `logInfo`/`logDebug`/`logWarn` write through this ref from any thread | `IORef LoggerState`; the logger backend batches/flushes internally | `Engine.Core.Init.initializeEngineWith` (`src/Engine/Core/Init.hs:157-158`); backend is `stdout` (graphical/headless) or `stderr` (dump, so stdout stays clean JSON) | Explicitly flushed via `shutdownLogger`, last, after every worker thread has stopped (`src/Engine/Loop/Shutdown.hs:104-107`) and in each `App.*` module's own error branch | Must outlive every other thread's own shutdown log line — hence torn down last, not merely GC'd. |
| `lifecycleRef` | boot-process | `AnyThread` — every worker run-loop polls it each tick (`Unit.Thread`, `Combat.Thread`, `World.Thread`, `Sim.Thread`, `Engine.Scripting.Lua.Thread`, `Engine.Input.Thread`, `Engine.Loop`) | `AnyThread` for the initial-running transition; `MainRender` sets the final stopped value | `IORef EngineLifecycle` (`EngineStarting|EngineRunning|CleaningUp|EngineStopped`) | `Engine.Core.Init.initializeEngineWith` seeds `EngineStarting` (`src/Engine/Core/Init.hs:154`) | Set to `EngineStopped` as literally the last step of `Engine.Loop.Shutdown.shutdownEngine`, after every worker thread has stopped and the logger has flushed (`src/Engine/Loop/Shutdown.hs:106-108`) | — |
| `inputThreadActiveRef` | boot-process | `LuaThread`/`WorldThread` — save-barrier owner-set computation (`Engine.Scripting.Lua.Thread.Dispatch`, `World.Thread.Command.Save`) consults it to decide whether the input owner slot belongs in a save/load transaction's owner set | `Boot` — set to true exactly once by `Engine.Input.Thread.startInputThread` immediately after it forks; never written again | `IORef Bool`, write-once | `Engine.Core.Init.initializeEngineWith` seeds `False` (`src/Engine/Core/Init.hs:232`) | None | Boot-profile-derived fact (only Graphical/Offscreen/Preview start an input thread — §4); primary owner is `core-init` even though its principal *consumer* is `save-load-coordination` — see §7. |

### `render-gpu-asset`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `engineStateRef` | boot-shutdown | `MainRender` only (see §3, the main-thread-private invariant) | `MainRender` only | `IORef EngineState`, single-thread-owned; no atomic ops needed since only one thread ever touches it | `Engine.Core.Init.initializeEngineWith` seeds `defaultEngineState` (`src/Engine/Core/Init.hs:257`) | Contents (every Vulkan handle in the nested `GraphicsState`) explicitly destroyed by `Engine.Loop.Shutdown.shutdownEngine` — `deviceWaitIdle`, transient-texture cleanups, `runAllCleanups (vulkanCleanup state)`, explicit sampler/buffer destruction (`src/Engine/Loop/Shutdown.hs:41-84`) — before GLFW/thread teardown; the `IORef` container itself is never destroyed, only overwritten/zeroed | See §3 in full. |
| `videoConfigRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`/`Engine.Graphics.Vulkan.Recreate`, `Engine.Loop.Timing`), `Boot` (window creation), `LuaThread` (`API.Config:36`'s `getVideoConfigFn`, direct query) | `LuaThread` (`Engine.Scripting.Lua.API.Config` — direct settings Apply/Save/Defaults, called synchronously from Lua), `MainRender` (`Engine.Scripting.Lua.Message.Video`'s `handleSetVSync`/`handleSetMSAA`, dispatched via `processLuaMessages` from `Engine.Loop.mainLoop`/`Engine.Loop.Headless`) | `IORef VideoConfig`, multi-writer via `atomicModifyIORef'` | Loaded from `config/video.local.yaml` (or default) (`src/Engine/Core/Init.hs:176-180`) | None | Settings-apply path (#748/#750) reads this on `MainRender` to rebuild the swapchain. Lua-triggered writes split by mechanism: a direct `API.Config` call (e.g. `engine.setVideoConfig`) writes synchronously on `LuaThread`; a call that must also touch the live Vulkan device (VSync/MSAA) instead enqueues onto `luaToEngineQueue` and is applied later, on `MainRender`, when `processLuaMessages` drains it — see `input-lua-transport`'s `luaToEngineQueue`/`luaQueue` rows. |
| `windowSizeRef` | boot-process | `WorldThread` (screen-space quad builders, `World/Render/*Quads.hs`), `UnitThread` (`Unit.HitTest`, `Building.HitTest`), `LuaThread` (`API.Input`, `API.InputInject`, `API.WorldQuery.Pick`), `MainRender` (`Engine.Loop.Camera`, `Engine.Loop.Frame`) | `InputThread` (native resize callback, `Engine.Input.Thread.Dispatch:117`), `MainRender` (`Engine.Graphics.Window.GLFW:109`, `Engine.Loop.Frame:384`, and `Engine.Scripting.Lua.Message.Video`'s `handleSetResolution`/`handleSetWindowMode` for synthetic/Lua-triggered resolution changes — dispatched via `processLuaMessages`, never the Lua thread itself) | `IORef (Int,Int)`, multi-writer, last-write-wins (no cross-writer ordering guarantee) | Seeded from the loaded `VideoConfig` (`src/Engine/Core/Init.hs:181`) | None | Same multi-writer shape as `framebufferSizeRef`/`windowStateRef` below. |
| `windowStateRef` | boot-process | `MainRender` only (`Message.Video`, read back when restoring windowed geometry) | `MainRender` only (`Engine.Scripting.Lua.Message.Video:70` — caches windowed pos/size on fullscreen toggle, dispatched via `processLuaMessages`, never the Lua thread itself) | `IORef WindowState` | `defaultWindowState` (`src/Engine/Core/Init.hs:182`) | None | — |
| `framebufferSizeRef` | boot-process | `WorldThread` (`World/Render.hs`, `World.Render.GroundItemQuads`, `World.Render.BloodQuads`, `World.Render.SpoilQuads`, `World.Render.CursorQuads`), `LuaThread` (`API.UI.Placement`, `API.World.Query`, `API.Input`) | `InputThread` (`Thread/Dispatch:121`), `MainRender` (`Engine.Scripting.Lua.Message.Video`, dispatched via `processLuaMessages`), `Boot` (`app/App/Offscreen.hs:71`) | `IORef (Int,Int)`, multi-writer | `Engine.Core.Init.initializeEngineWith` (`src/Engine/Core/Init.hs:183`) | None | — |
| `fpsRef` | boot-process | `LuaThread` (`API.Core`, `engine.getFPS`) | `MainRender` only (once per frame, `Engine.Loop.Timing:75`) | `IORef Double`, single-writer | `0.0` (`src/Engine/Core/Init.hs:155`) | None | — |
| `brightnessRef` | boot-process | `MainRender` (`Engine.Loop.Frame`, `Engine.Graphics.Vulkan.Init`) | `MainRender` (`Engine.Scripting.Lua.Message.Video:167`'s `handleSetBrightness`, dispatched via `processLuaMessages` — never the Lua thread itself, which only enqueues the request) | `IORef Int` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:184`) | None | — |
| `pixelSnapRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `LuaThread` (`API.Config:223`, direct synchronous `writeIORef`), `MainRender` (`Engine.Scripting.Lua.Message.Video:173`'s `handleSetPixelSnap`, dispatched via `processLuaMessages`, a separate call path from `API.Config`'s) | `IORef Bool` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:185`) | None | — |
| `textureFilterRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Texture.Bindless` — sampler selection) | `LuaThread` (`API.Config:236`, direct synchronous `writeIORef` alongside enqueuing the change), `MainRender` (`Engine.Scripting.Lua.Message.Video:183`'s `handleSetTextureFilter`, dispatched via `processLuaMessages` — this is also where the live GPU sampler swap on `textureSystemRef` happens) | `IORef TextureFilter` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:186`) | None | — |
| `assetPoolRef` | boot-process | `LuaThread` (shared into the Lua backend as `apRef`, `Engine.Scripting.Lua.Thread:51`), `MainRender` (`Message.Texture:84`'s `duplicateCachedTextureHandle`, dispatched via `processLuaMessages`), `WorldThread` (`World.Render.BloodQuads:179`'s `uploadOne`, blood-texture generation) | `LuaThread`/`Boot` | `IORef AssetPool` | `defaultAssetPool` (`src/Engine/Core/Init.hs:160-161`) | None (any GPU handles it names are registered for teardown where they're created, not on this container) | — |
| `textureNameRegistryRef` | boot-process | `LuaThread` (`Engine.Asset.YamlTextures` name→handle lookups), `WorldThread` (`World.Render.GroundItemQuads:162`'s broken-equipment overlay lookup) | `LuaThread` (registration during content load) | `IORef TextureNameRegistry` | `emptyTextureNameRegistry` (`src/Engine/Core/Init.hs:166`) | None | — |
| `fontCacheRef` | boot-shutdown | `MainRender` (`UI.Render`, `Engine.Scene.Batch.Text`, `Engine.Graphics.Vulkan.Command.Text`), `LuaThread` (`API.Text`) | `MainRender`/`Boot` (`Engine.Graphics.Font.Load` rasterizes on demand) | `IORef FontCache` | `defaultFontCache` (`src/Engine/Core/Init.hs:198`) | Glyph-atlas GPU memory registered via `allocResource` at creation (`Engine.Graphics.Font.Upload`/`Draw`), freed by the generic `vulkanCleanup` sweep in `shutdownEngine` | — |
| `textureSystemRef` | boot-shutdown | `WorldThread` (`src/World/Render/BloodQuads.hs`, `src/Unit/Render.hs`), `MainRender` (`src/UI/Render.hs`, and `Engine.Scripting.Lua.Message.Texture` reads dispatched via `processLuaMessages`), `LuaThread` (`API.Blood`, direct queries) | `MainRender` (`Engine.Graphics.Vulkan.Init:213` — initial creation on the same thread that runs `Engine.Loop.mainLoop`; also `Engine.Scripting.Lua.Message.Video:189`/`Message.WorldTexture`/`Message.Texture`'s live rebuild/registration handlers, all dispatched via `processLuaMessages` — never the Lua thread itself), `WorldThread` (`src/World/Render/BloodQuads.hs:84,166` — dynamic blood-texture registration) | `IORef (Maybe BindlessTextureSystem)`, multi-writer across `MainRender`+`WorldThread` | `Nothing` at engine boot; populated by `Engine.Graphics.Vulkan.Init:213` on `MainRender` after device creation (graphical/offscreen/preview only — stays `Nothing` under headless/dump) | GPU descriptor/image resources registered via `allocResource` at creation, freed by the `vulkanCleanup` sweep in `shutdownEngine` | Moved to `EngineEnv` specifically because worker threads must reach it — see §3. |
| `samplerCacheRef` | boot-shutdown | `MainRender` (texture/font upload paths acquire samplers by kind) | `MainRender` | `IORef SamplerCache`, refcounted | `emptySamplerCache` (`src/Engine/Core/Init.hs:205`) | Explicitly destroyed via `destroySamplerCache` in `shutdownEngine` (`src/Engine/Loop/Shutdown.hs:62-65`) | — |
| `textureSizeRef` | boot-process | `WorldThread` (`World.Render.GroundItemQuads`, `World.Render.BloodQuads`, `World.Render.Quads`), `MainRender` (`Engine.Scripting.Lua.Message.Texture`, dispatched via `processLuaMessages`), `LuaThread` (`API.Blood`, direct queries) | `WorldThread` (`BloodQuads:127,188` — per-texture insert/delete as blood textures are created/disposed), `MainRender` (`Engine.Scripting.Lua.Message.Texture:117,267`, dispatched via `processLuaMessages` — never the Lua thread itself) | `IORef (HashMap TextureHandle (Int,Int))`, multi-writer | `HM.empty` (`src/Engine/Core/Init.hs:206`) | None — entries are deleted per-texture as their owning textures are disposed, not wholesale at shutdown | — |
| `defaultFaceMapSlotRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `MainRender` | `IORef Word32` | `0` (`src/Engine/Core/Init.hs:207`), reassigned once the default face-map texture binds during Vulkan init | None | — |
| `cameraRef` | session-replaced | `WorldThread` (`World.Render.*`, `World.Thread`), `UnitThread` (`Unit.HitTest`), `MainRender` (`Engine.Loop.Frame:243` — the frame loop reads the LIVE camera each frame for the view matrix specifically to avoid the stale-by-tens-of-milliseconds value the world thread's own copy could show), `LuaThread` (`API.Camera:55`'s `cameraGetPositionFn`, direct query) | `WorldThread` (`World/Render.hs:194,212`, `World.Thread.Command.Init:300,356`, load publish `World.Load.Publish:129`), `LuaThread` (`API.Camera:44`), `MainRender` (`Engine.Loop.Camera:178` — WASD camera-pan velocity/position integration runs once per frame on the main thread) | `IORef Camera2D`, multi-writer via `atomicModifyIORef'` | `defaultCamera` (`src/Engine/Core/Init.hs:188`) | None | Session-replaced on load publish; see `docs/persistence_state_inventory.md` §1 `cameraRef`. |
| `uiCameraRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`/`Engine.Graphics.Vulkan.Recreate`) | `MainRender` (window/framebuffer resize path, `Engine.Graphics.Vulkan.Recreate:69`) | `IORef UICamera` | `defaultUICamera`, sized from the loaded `VideoConfig` (`src/Engine/Core/Init.hs:189-190`) | None | — |
| `screenshotRequestQueue` | transient-handoff | `MainRender` dequeues one per frame in `drawFrame` (`Engine.Loop.Frame`) and replies on the request's own reply channel | `LuaThread` (`API.Screenshot`) | `Q.Queue ScreenshotRequest` (STM `TQueue`) | `Q.newQueue` (`src/Engine/Core/Init.hs:151`) | None — never drained under headless, the verb refuses before enqueueing | — |
| `nextObjectIdRef` | boot-process | `LuaThread` only — shared into the Lua backend state as `lbsNextObjectId` (`Engine.Scripting.Lua.Thread:51`, `Engine.Scripting.Lua.Types:43`) | `LuaThread` only — bumped by every `UI.new*`/scene-object allocation issued from Lua | `IORef Word32`, monotonic, single-thread-owned (Lua thread only) | `0` (`src/Engine/Core/Init.hs:162`) | None | — |

### `input-lua-transport`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `inputQueue` | boot-process | `InputThread` drains (`Engine.Input.Thread`, `Thread/Dispatch`), `MainRender` (headless polling path, `Engine.Loop.Headless`) | `Boot`/`MainRender` (GLFW callbacks via `setupCallbacks`), `LuaThread` (`API.InputInject` synthetic injection) | `Q.Queue InputEvent` (STM `TQueue`), multi-producer/single-consumer FIFO | `Q.newQueue` (`src/Engine/Core/Init.hs:144`) | None; drained/discarded at process exit | Flushed on load publish (`World.Load.Publish`). |
| `inputBarrierNextRef` | boot-process | `LuaThread` (`Engine.Input.Inject.newBarrierToken` allocates) | `LuaThread` only | `TVar Int`, monotonic allocator | `newTVarIO 0` (`src/Engine/Core/Init.hs:145`) | None | — |
| `inputBarrierRef` | boot-process | `LuaThread` (`Engine.Input.Inject.waitForBarrier`) | `InputThread` (`Engine.Input.Thread.Dispatch.processInput`, strictly after a barrier's turn is fully processed — see the field's own doc comment, `src/Engine/Core/State.hs:89-106`) | `TVar Int` | `newTVarIO 0` (`src/Engine/Core/Init.hs:146`) | None | — |
| `inputStateRef` | session-replaced | `InputThread` (`Thread/Dispatch:46,58`), `LuaThread` (shared as `lbsInputState`), `MainRender` (`Engine.Loop.Camera`) | `InputThread`, `WorldThread` (load publish, `World.Load.Publish:282`, resets to `defaultInputState`) | `IORef InputState` | `defaultInputState` (`src/Engine/Core/Init.hs:168`) | None | Reset on load so stale held-key state can't survive a load. |
| `keyBindingsRef` | boot-process | `InputThread` (`Thread/Keyboard`), `MainRender` (`Engine.Loop.Camera`), `LuaThread` (`API.Input`) | `LuaThread` only (`API.Keybinds` — settings rebind/reset) | `IORef KeyBindings`, multi-reader/single-writer via `atomicModifyIORef'` | Loaded from `config/keybinds.local.yaml` (or default) (`src/Engine/Core/Init.hs:169-173`) | None | — |
| `currentKeyDownRef` | transient-handoff | `LuaThread` only | `LuaThread` only | `IORef (Maybe GLFW.Key)` — see the field's own doc comment, `src/Engine/Core/State.hs:123-128` | `Nothing` (`src/Engine/Core/Init.hs:174`) | None | Meaningful only for the duration of one `onKeyDown` broadcast. |
| `luaToEngineQueue` | boot-process | `MainRender` (`Engine.Scripting.Lua.Message`'s `processLuaMessages`; also flushed on a load publish) | `LuaThread` (`Engine.Scripting.Lua.Thread`, `Thread/Dispatch`) | `Q.Queue LuaToEngineMsg` | `Q.newQueue` (`src/Engine/Core/Init.hs:149`) | None | — |
| `luaQueue` | boot-process | `LuaThread` drains (`Engine.Scripting.Lua.Thread`, `Engine.Scripting.Lua.Util`) | `WorldThread` (`World.Thread.Command.Init`, `World.Thread.ChunkLoading`, `World.Thread.Command.Save`, `World.Log`, `World.Thread.Helpers`), `MainRender` (`Message.Video`), `UnitThread`/`CombatThread` (notification broadcasts), `InputThread` | `Q.Queue LuaMsg`, multi-producer/single-consumer | `Q.newQueue`, bound as `engineToLuaQueue` (`src/Engine/Core/Init.hs:150,274`) | None | Engine→Lua direction (the field is literally named `luaQueue` on `EngineEnv` but constructed as `engineToLuaQueue`). |

### `world-sim-render-handoff`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `worldManagerRef` | session-replaced | `UnitThread` (`Unit.Render`, `Unit.HitTest`, `Unit.LineOfSight`, `Unit.Thread.Command.*`), `WorldThread`, `MainRender` (`World.Render*`), `LuaThread` (`API.Units.Spawn:107`, direct read resolving an explicit target page) | `WorldThread` (world init/load/edit commands, and load publish `World.Load.Publish:158`) | `IORef WorldManager`, multi-writer | `emptyWorldManager` (`src/Engine/Core/Init.hs:192`) | None | Contents classified per-field in `docs/persistence_state_inventory.md` §3/§4; this row covers only the `EngineEnv` pointer, per §1's scope note. |
| `worldQueue` | boot-process | `WorldThread` drains (`World.Thread`); `SimThread` observes for coordination | `LuaThread` (`API.Construct`, `API.Structure`, `API.Till`, `API.Plant`, `API.World.Lifecycle`, `API.World.Edit`) | `Q.Queue WorldCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:147`) | None | — |
| `sunAngleRef` | boot-process | `LuaThread` (`API.World.Clock`, `API.WorldQuery.Climate`), `MainRender` (lighting, `Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `WorldThread` (`World.Thread.Time`, derived via `worldTimeToSunAngle`) | `IORef Float` | `0.25` = noon (`src/Engine/Core/Init.hs:199`) | None | — |
| `worldPreviewRef` | transient-handoff | `MainRender` consumes for GPU upload | `WorldThread` enqueues; load publish (`World.Load.Publish:152`) | `IORef (Maybe (Int,Int,ByteString,Word64))`, single-slot, tagged with a generation | `Nothing` (`src/Engine/Core/Init.hs:200`) | None | Paired with `worldPreviewGenerationRef` to suppress a stale in-flight upload (round 10 review, #763). |
| `worldPreviewGenerationRef` | boot-process | `MainRender` (upload handler compares dequeued generation against current) | `WorldThread` (enqueue bumps it, and load publish `World.Load.Publish:149`) | `IORef Word64`, monotonic, never decreases | `0` (`src/Engine/Core/Init.hs:201`) | None | — |
| `zoomAtlasDataRef` | transient-handoff | `MainRender` consumes for GPU upload | `WorldThread` enqueues; load publish (`World.Load.Publish:137`) | `IORef (Maybe (Int,Int,ByteString,[WorldState]))`, single-slot | `Nothing` (`src/Engine/Core/Init.hs:202`) | None | Captures the exact `WorldState`s it belongs to at enqueue time (round 9 review, #763). |
| `worldQuadsRef` | boot-process | `MainRender` (frame loop merges + draws) | `WorldThread` (per-tick static/dynamic quad split, #446) | `IORef LayeredQuads` | `emptyLayeredQuads` (`src/Engine/Core/Init.hs:203`) | None | — |
| `bloodDisposeQueue` | transient-handoff | `MainRender` drains (`World.Render.BloodQuads.disposeQueuedBloodTextures`) | `WorldThread` (page-removal teardown, `World.Blood.Teardown`, `World.Thread.Command.Basic/Init`) | `Q.Queue (IORef BloodTextureHandles)` | `Q.newQueue` (`src/Engine/Core/Init.hs:152`) | None — empty/inert under headless | — |
| `floraCatalogRef` | boot-process | `WorldThread` (`Thread.ChunkLoading`, `Thread.Cursor`, `Thread.Command.Init`, `Command.Cursor.Plant/Chop`, `Command.Edit.Vegetation`, `World.Load.Stage` during staging), `MainRender` (`World.Render.Quads`), `LuaThread` (`API.Plant:100`, direct crop/species lookup) | `LuaThread` (content load) | `IORef FloraCatalog` | `emptyFloraCatalog` seed, populated from `data/*.yaml` via Lua content load (`src/Engine/Core/Init.hs:208`) | None | — |
| `materialRegistryRef` | session-replaced | `UnitThread` (`Unit.Thread.Movement`), `WorldThread`, `LuaThread` (`Engine.Scripting.Lua.API.World.Edit:86,211`, `Engine.Scripting.Lua.API.YamlTextures:350`) | `WorldThread` (populated per-world-init from `data/materials/*.yaml`, `src/World/Thread/Command/Init.hs:111-113`; also load publish, `src/World/Load/Publish.hs:117`), `LuaThread` (`Engine.Scripting.Lua.API.YamlTextures:99` registers each material's physical properties from the same loaded YAML content) | `IORef MaterialRegistry`, multi-writer | `emptyMaterialRegistry` at engine boot (`src/Engine/Core/Init.hs:209`); populated per-world-init from `data/materials/*.yaml` (`src/World/Thread/Command/Init.hs:100-113`) | None | YAML-driven after all (corrected from an earlier draft of this row, per review) — populated once per world init/load, not built into the binary. |
| `worldGenConfigRef` | boot-process | `WorldThread` (`Thread.Command.Init`), `LuaThread` (`API.World.GenConfig`) | `LuaThread` (`API.World.GenConfig`) | `IORef WorldGenConfig` | `loadWorldGenConfig "config/world_gen_default.yaml"` (`src/Engine/Core/Init.hs:224-225`) | None | Global worldgen tunables, distinct from a specific world's `wpsGenParams`. |
| `gameTimeRef` | session-replaced | `AnyThread` (reading pause-aware timestamps, e.g. `UnitThread`) | `UnitThread` (`Unit.Thread.unitLoop`, once per tick when unpaused, and load publish `World.Load.Publish:110`) | `IORef Double`, monotonic while unpaused | `0` (`src/Engine/Core/Init.hs:230`) | None | Persisted exactly (`sdGameTime`). |
| `enginePausedRef` | session-replaced | `WorldThread`/`UnitThread`/`SimThread` (skip simulated-state advancement while true), `CombatThread` (`Combat.Thread:87` — sleeps the tick, keeping queued events queued, rather than resolving combat while paused), `MainRender` (keeps rendering/input regardless), `LuaThread` (`API.Core:96`'s `isPausedFn`, direct query) | `LuaThread` (`API.Core`'s `setPausedFn`, `engine.setPaused`), `WorldThread` (load publish `World.Load.Publish:111`, always loads paused) | `IORef Bool` | `False` (`src/Engine/Core/Init.hs:229`) | None | Persisted exactly; authoritative over any Lua-side copy. |
| `simQueue` | boot-process | `SimThread` drains (`Sim.Thread`) | `WorldThread` (`Thread.ChunkLoading`, `Command.Basic`, `Command.Edit.Sync`, `Command.UI`) | `Q.Queue SimCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:148`) | None | — |
| `texPaletteRef` | session-replaced | `WorldThread` (`Thread.Command.Save.WriteWorld`), `LuaThread` (`API.Structure` — placement interns paths→ids) | `LuaThread`, `WorldThread` (load publish, `World.Load.Publish:118`) | `IORef TexPalette` | `emptyTexPalette` (`src/Engine/Core/Init.hs:215`) | None | Persisted exactly as `sdTexPalette`. |
| `texPaletteHandlesRef` | session-replaced | `MainRender` (`Structure.Render`) | `LuaThread` (lazy per-palette-path resolution), `WorldThread` (load publish, `World.Load.Publish:121`) | `IORef (HashMap Int TextureHandle)` | `HM.empty` (`src/Engine/Core/Init.hs:216`) | None | Runtime translation table, rebuilt each session — not itself persisted. |

### `units-buildings-combat`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `unitManagerRef` | session-replaced | `UnitThread`, `CombatThread` (`Combat.Wounds.Tick`), `MainRender` (`Unit.Render`), `WorldThread`, `LuaThread` (`API.Units.Spawn:98`'s `unit.spawn`, direct def-existence check) | `UnitThread` (`Thread.Command.Lifecycle`, `Command.Pose`, and load publish `World.Load.Publish:127`), `LuaThread` (`API.Units.Spawn:141`'s `unit.spawn`, direct unit-id allocation via `atomicModifyIORef'`) | `IORef UnitManager`, multi-writer | `emptyUnitManager` (`src/Engine/Core/Init.hs:210`) | None | — |
| `unitQueue` | boot-process | `UnitThread` drains (`Unit.Thread.Command`) | `CombatThread` (`Combat.Wounds.Tick`, `Combat.Resolution.Events` — UnitKill/UnitCollapse), `WorldThread` (`Command.Basic`, `Command.Edit.Dig/Terrain`), `LuaThread` (`API.Units.Spawn`) | `Q.Queue UnitCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:211`) | The combat thread (a producer) is shut down **before** the unit thread (its consumer) — `app/App/Graphical.hs:72-76`: "Combat first: wound ticks enqueue UnitKill/UnitCollapse onto the unit queue, so the producer has to stop before the consumer... is torn down" | Deliberate shutdown ordering; see the identical rationale on `combatQueue`. |
| `utsRef` | session-replaced | `UnitThread` only (`Unit.Thread`, `Thread.Command`, `Thread.Movement`, `Thread.Command.Spawn/Lifecycle/Pose`) | `UnitThread` (also load publish, `World.Load.Publish:128`) | `IORef UnitThreadState`, single-thread-owned outside a load publish (per the field's own doc comment, `src/Engine/Core/State.hs:222-227`) | `emptyUnitThreadState` (`src/Engine/Core/Init.hs:212`) | None | — |
| `statRNGRef` | boot-process | `UnitThread` (`Thread.Command.Spawn`, `Thread.Movement.Climb`), `CombatThread` (`Combat.Resolution`, `Combat.Wounds.Tick`), `WorldThread` (`Thread.Command.Edit.Dig` — dig-yield rolls), `LuaThread` (`API.Forage.Harvest`) | `UnitThread`, `CombatThread`, `WorldThread`, `LuaThread` — the same four roles as Readers; each roll both reads and advances the generator | `IORef StdGen`, multi-writer, no cross-writer ordering guarantee beyond each individual roll's own atomicity | `Random.newStdGen` (`src/Engine/Core/Init.hs:213`) | None | Explicitly non-deterministic across runs by design — not save-seeded. |
| `buildingManagerRef` | session-replaced | `WorldThread` (`Render.CursorQuads`, `Thread.Power`, `Thread.ItemTemp`), `LuaThread` (`API.Power`, `API.Save`, `API.Buildings.Selection`'s `building.getSelected`) | `UnitThread` (via `Building.Thread.Command`, drained on the unit thread, and load publish `World.Load.Publish:126`), `LuaThread` (`API.Buildings.Selection:46,59`'s `building.select`/`building.deselect`, direct `atomicModifyIORef'`) | `IORef BuildingManager` | `emptyBuildingManager` (`src/Engine/Core/Init.hs:214`) | None | "Building" is a domain, not a thread — its commands are drained on `UnitThread` (`Unit.Thread` imports `Building.Thread.Command.processAllBuildingCommands`). |
| `buildingQueue` | boot-process | `UnitThread` drains via `Building.Thread.Command.processAllBuildingCommands` | `LuaThread` (`API.Power`, `API.Buildings.Spawn`) | `Q.Queue BuildingCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:217`) | None | See `buildingManagerRef` note. |
| `buildingGhostRef` | session-replaced | `MainRender` (`Building.Render` — placement-preview draw) | `LuaThread` (the `build_tool` module via `API.Buildings.Spawn`), `WorldThread` (load publish, `World.Load.Publish:281`, always cleared) | `IORef (Maybe BuildingGhost)`, single-slot | `Nothing` (`src/Engine/Core/Init.hs:218`) | None | — |
| `combatQueue` | boot-process | `CombatThread` (drains at 60 Hz, `Combat.Thread.processAllCommands`) | `LuaThread` (`combat.attack` and future combat commands — per the field's own doc comment) | `Q.Queue Combat.Types.CombatCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:219`) | `CombatThread` (the consumer here, but the *producer* for `unitQueue`) is shut down first — see `unitQueue`'s Shutdown cell | — |
| `combatEventsRef` | session-replaced | `LuaThread` (`combat.drainEvents`, `API.Combat`) | `CombatThread` (`Combat.Wounds.Tick`, `Combat.Resolution.Events`), `WorldThread` (load publish, `World.Load.Publish:290`, reset to empty) | `IORef (Seq CombatEvent)` | `Combat.Types.emptyEventQueue` (`src/Engine/Core/Init.hs:220`) | None | Runtime only, never persisted. |
| `injuryEventsRef` | session-replaced | `LuaThread` (`injury.drainEvents`) | `UnitThread` (`Unit.Thread.Movement` — falls), `LuaThread` (`API.Units.Combat`'s `unit.injure`, and `injury.emit`), `WorldThread` (load publish, `World.Load.Publish:291`) | `IORef (Seq CombatEvent)` (reused shape; victim in `target`) | `emptyEventQueue` (`src/Engine/Core/Init.hs:221`) | None | A streaming consumer (the log panel) drains this — don't manually drain it while that panel script is loaded, or you'll race it. |
| `thoughtEventsRef` | session-replaced | `LuaThread` (`thought.drainEvents`) | `LuaThread` (`scripts/thoughts.lua` via `thought.emit`), `WorldThread` (load publish, `World.Load.Publish:292`) | `IORef (Seq CombatEvent)` | `emptyEventQueue` (`src/Engine/Core/Init.hs:222`) | None | — |
| `actionOutcomeRef` | session-replaced | `LuaThread` (`debug.drainActionOutcomes`, the F4 playtest oracle) | `LuaThread` (`debug.recordOutcome`), `WorldThread` (`Thread.Command.Cursor.Common/Plant` — partial-drop counts, and load publish `World.Load.Publish:293`) | `IORef (Seq ActionOutcome)` | `emptyActionOutcomeQueue` (`src/Engine/Core/Init.hs:223`) | None | Never surfaced to the player. |
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
| `uiManagerRef` | session-replaced | `MainRender` (`UI.Render`), `InputThread` (`Input.Thread.Keyboard:93`'s `validateFocus`, read on every keyboard dispatch) | `LuaThread` (every `UI.*` API module — `API.UI.Focus/Property/Tooltip/Hierarchy`, `API.Config`), `WorldThread` (load publish `World.Load.Publish:286`), `InputThread` (`Input.Thread.Keyboard:93,234`, atomic focus/control-focus validation on every keyboard dispatch — round 4 review notes this races the Lua thread's own concurrent element mutations, hence the atomic transition rather than a separate read/write pair) | `IORef UIPageManager`, multi-writer via `atomicModifyIORef'` | `emptyUIPageManager` (`src/Engine/Core/Init.hs:191`) | None | Entire UI tree is rebuilt by Lua on load, per `docs/persistence_state_inventory.md`. |
| `focusManagerRef` | session-replaced | `InputThread` (`Thread.Keyboard`/`Thread.Char` — Tab/Shift+Tab control-focus navigation, #745), `LuaThread` (`API.Focus`) | `InputThread`, `LuaThread` — the same two roles as Readers; also `WorldThread` (load publish, `src/World/Load/Publish.hs:284`) | `IORef FocusManager` | `createFocusManager` (`src/Engine/Core/Init.hs:196`) | None | — |
| `hudActivePageRef` | session-replaced | `WorldThread` (`Thread.Cursor` — HUD refresh-on-active-world-change, #129) | `WorldThread` (also load publish, `World.Load.Publish:283`, resynced from `wmVisible`) | `IORef (Maybe WorldPageId)` | `Nothing` (`src/Engine/Core/Init.hs:193`) | None | — |
| `textBuffersRef` | boot-process | `LuaThread` only (`API.Text`, direct queries) | `MainRender` only (`Engine.Scripting.Lua.Message.Scene`, dispatched via `processLuaMessages` — never the Lua thread itself) | `IORef (Map ObjectId Text)` | `Map.empty` (`src/Engine/Core/Init.hs:197`) | None | Editable-widget text keyed by `ObjectId`, per the UI text-buffer coordinate contract. |
| `eventStoreRef` | boot-process | `LuaThread` (event-log panel query) | `WorldThread`, `UnitThread`, `LuaThread` — all push via `Engine.PlayerEvent.emitEvent` (per the field's own doc comment) | `TVar (Seq PlayerEvent)`, multi-writer STM, ~1000-entry ring | `newTVarIO Seq.empty` (`src/Engine/Core/Init.hs:255`) | None | Explicitly session-only, never serialized. |
| `notificationCfgRef` | boot-process | `AnyThread` — the `emitEvent` read path | `LuaThread` (Phase 2 settings tab toggles, per the field's own doc comment) | `IORef NotificationCfg` | `loadNotificationCfg` merges `data/notification_categories.yaml` + `config/notifications.local.yaml` (`src/Engine/Core/Init.hs:251-254`) | None | — |
| `notificationOrder` | boot-process | `LuaThread` (settings tab render order) | None (captured once at boot from the YAML registry order — categories can't be added/removed at runtime, per the field's own doc comment) | Plain `![Text]`, no `IORef` | `loadNotificationCfg`'s second return value (`src/Engine/Core/Init.hs:251-254`) | None | Immutable-boot-configuration carve-out, same shape as `engineConfig`. |
| `popupQueueRef` | boot-process | `LuaThread` (popup module drains via the LuaShowPopup broadcast) | `WorldThread`/`UnitThread`/`LuaThread` (same `emitEvent` producers as `eventStoreRef`, filtered to popup-enabled categories) | `TVar (Seq PlayerEvent)` | `newTVarIO Seq.empty` (`src/Engine/Core/Init.hs:256`) | None | — |

### `save-load-coordination`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `loadStatusRef` | boot-process | `WorldThread` (`Thread.Time`, `Thread.Command.Save`), `MainRender` (`Engine.Scripting.Lua.Message.discardLuaMessagesForActiveLoad`, called by the render consumers while `captureLocked`), `LuaThread` (`API.Core`, `Thread.Dispatch`, `API.Save`) | `WorldThread`, `LuaThread` — the same two roles as Readers | `LoadStatusRef` (opaque, internally synchronized — see `Engine.Load.Status`) | `newLoadStatusRef` (`src/Engine/Core/Init.hs:194`) | None | Diagnostic only, never serialized. |
| `pendingLoadRef` | transient-handoff | `WorldThread`, `LuaThread` (`Thread.Dispatch`) | `WorldThread` — written when a staged-load transaction finishes staging; read and cleared when the matching publish command runs (per the field's own doc comment) | `IORef (Maybe (Int, StagedSession))`, single-slot, keyed by request id defensively | `Nothing` (`src/Engine/Core/Init.hs:195`) | None | Only one load is ever in flight, enforced by `loadStatusRef`. |
| `saveBarrierRef` | boot-process | `UnitThread`, `CombatThread`, `WorldThread` (`Thread.Command.Save`, `Command.Save.WriteWorld`), `MainRender` (`Engine.Loop`), `LuaThread` (`Engine.Scripting.Lua.Thread:227`'s `captureLocked` check, and `Thread.Dispatch:413-455`'s `handleLoadStaged`, which itself DRIVES a whole load-publish transaction via `beginSave`/`acknowledgeSave`/`waitForOwners`/`reachSnapshot`/`failSave`) | `UnitThread`, `CombatThread`, `WorldThread`, `MainRender`, `LuaThread` (`Thread.Dispatch:413-455` — the Lua thread is the transaction driver for a load publish, not merely one of the acknowledging owners) — all the same roles as Readers | `SaveBarrier` (opaque, internally-synchronized coordination record — see `Engine.Save.Barrier`) | `newSaveBarrier` (`src/Engine/Core/Init.hs:231`) | None | Every state-owner thread that must acknowledge a save boundary reads/writes this; diagnostic/coordination only, never serialized. |
| `lastSaveTimeRef` | boot-process | `LuaThread` only | `LuaThread` only (`API.Save.saveWorldFn`, clamps each save strictly past this for monotonic ordering, #98) | `IORef UTCTime` | POSIX epoch (`src/Engine/Core/Init.hs:236`) | None | — |
| `nextItemInstanceIdRef` | session-replaced | `AnyThread` via `freshItemInstanceId` (`Engine.Core.State`) — item rolls/spawns | `AnyThread` via `freshItemInstanceId`; `WorldThread` (load publish, `World.Load.Publish:125`) | `IORef Word64`, monotonic allocator, thread-safe atomic bump (`atomicModifyIORef'`) | `1` (`src/Engine/Core/Init.hs:165`) | None | Persisted exactly, restored `max(loaded, current)` — never lowered (#67). |

## 6. Full-`EngineEnv` compatibility boundary

Today, all 217 files that import `Engine.Core.State` (160 of them
importing `EngineEnv(..)` specifically) have unrestricted access to
every field, because no capability-scoped type exists yet — that is
exactly the gap #537 exists to close. This section names the intended
*end state*: what should still legitimately construct, carry, or
inspect the **complete** `EngineEnv` once the epic's capability split
has landed, versus what merely has full access today because nothing
narrower exists yet. It is deliberately narrow — narrow enough to
become the literal allowlist for #537's final unrestricted-access
audit (per requirement 6) — which means most of today's 160 importers
are **not** listed as permanent below; they belong in the temporary
column, each tied to one of §7's bounded follow-up issues.

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
| `Engine.Loop`, `Engine.Loop.Frame`, `Engine.Loop.Headless`, `Engine.Loop.Shutdown`, `Engine.Loop.Camera`, `Engine.Loop.Timing`, `Engine.Loop.Resource` | Permanent orchestration infrastructure | The main loop's job each frame is to coordinate render output with several other capabilities' queues/state (input barrier tokens, world quads, screenshot requests, ...), and `shutdownEngine` performs the one coordinated cross-capability teardown boundary described in §2.4/§3. |
| `app/App/Graphical.hs`, `app/App/Offscreen.hs`, `app/App/Preview.hs`, `app/App/Headless.hs`, `app/App/Dump.hs` | Permanent orchestration infrastructure | Top-level boot/wire-up: each necessarily constructs the engine, starts every thread the profile needs, and wires them together — inherently whole-environment by job description. |
| `Engine.Scripting.Lua.Thread`, `Engine.Scripting.Lua.Thread.Dispatch` | Permanent orchestration infrastructure | The Lua thread's own dispatch plumbing registers *every* Lua API module against the full environment (`registerLuaAPI`) — this wiring point is inherently cross-capability, even though each individual `Engine.Scripting.Lua.API.*`/`Message.*` module it wires (§6.2) is not. |
| `World.Thread.Command.Save`, `World.Thread.Command.Save.WriteWorld`, `World.Load.Stage`, `World.Load.Publish`, `Engine.Scripting.Lua.API.Save` | Permanent orchestration infrastructure | A save/load transaction is inherently a whole-session boundary: these five modules are the exact, verified set that actually `import Engine.Core.State (EngineEnv(..))` on the save/load path (`grep -rn 'import Engine.Core.State' src/World/Load src/World/Thread/Command/Save* src/Engine/Scripting/Lua/API/Save.hs`) — they must capture or replace every capability's state atomically in one coordinated step (see the persistence contract's snapshot/publish design). Narrowing this to per-capability records would just reconstruct an env-shaped aggregate one level down — this is a permanent exception, not a temporary one awaiting migration. Everything ELSE under `World.Save.*` (`Snapshot`, `Types`, `Component*`, `Envelope*`, `Serialize`, `Storage`, `Integrity`, `Reference`, `Compat*`) is pure data/codec code that never touches `EngineEnv` at all (`World.Save.Snapshot`'s own doc comment states this explicitly) and is correctly outside this list entirely — not a temporary compatibility boundary either, since it was never given full access in the first place. `Engine.Save.Barrier`/`Engine.Load.Status` are the same: opaque coordination types referenced FROM `EngineEnv` (`saveBarrierRef`/`loadStatusRef`), not consumers of it — neither imports `EngineEnv`. |

### 6.2 Temporary compatibility boundary (production)

Everything else that imports `EngineEnv(..)` today is temporary: it
has full access only because no narrower capability-scoped record
exists yet, and is expected to migrate to one of §7's per-capability
records as each bounded follow-up issue lands. Rather than enumerate
all ~150 remaining files individually (a list that would be stale the
moment any one of them is touched), they are grouped by family, each
tied to the capability group whose §7 roadmap entry is expected to
narrow it:

| Family (path prefix) | Target capability | Roadmap entry |
|---|---|---|
| `Engine.Graphics.*`, `UI.Render` | `render-gpu-asset` | §7.2 |
| `Engine.Input.Thread.*`, `Engine.Input.Inject`, `Engine.Input.Callback` | `input-lua-transport` (writes cross into World/UI on dispatch — a genuine cross-capability dependency, not a migration blocker) | §7.3 |
| `World.Thread.*`, `World.Render.*`, `World.Generate.*` | `world-sim-render-handoff` | §7.4 |
| `Unit.Thread.*`, `Unit.Render`, `Unit.HitTest`, `Unit.LineOfSight`, `Building.Thread.Command`, `Building.Render`, `Building.HitTest`, `Combat.Thread`, `Combat.Resolution.*`, `Combat.Wounds.*`, `Sim.Thread` | `units-buildings-combat` | §7.5 |
| `Engine.Scripting.Lua.API.{Units,Buildings,Power,Forage,Craft}.*` and similar content-facing Lua API modules | `content-registries` | §7.6 |
| `UI.Manager.*`, `UI.Focus`, `Engine.Scripting.Lua.API.UI.*`, `Engine.Scripting.Lua.API.Focus`, `Engine.PlayerEvent` | `ui-hud-events` | §7.7 |
| `Engine.Scripting.Lua.API.Core` (the save/load-status-observing parts only — `Engine.Scripting.Lua.API.Save` itself is the permanent orchestration entry point, see §6.1) | `save-load-coordination` | §7.8 |

`World.Load.Stage` is deliberately absent from this table: it is one
of the five permanent exceptions in §6.1, not a temporary family —
listing it here too would contradict that classification.
| Every remaining `Engine.Scripting.Lua.API.*`/`Engine.Scripting.Lua.Message.*` module not named above (e.g. `API.Config`, `API.Camera`, `API.Screenshot`, `Message.Video`, `Message.Texture`, `Message.WorldTexture`, `Message.Scene`) | Whichever capability group its own fields belong to (see §5's per-field Reader/Writer citations for the specific module) | Corresponding §7 entry |

Naming a whole family here, rather than each file, is itself a
temporary convenience: as each capability group's migration issue
narrows its actual imports, this table should shrink to name only what
remains unmigrated, and eventually most of it should be empty.

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

- **Dependencies:** None — every other capability group depends on
  this one being available first (the logger and lifecycle flag are
  read from every thread), so this is necessarily the first migration,
  not something that can be deferred.
- **Independent migration:** Yes, and it should go first.
- **Follow-up scope:** Introduce a `CoreCapability` record
  (`engineConfig`, `loggerRef`, `lifecycleRef`, `inputThreadActiveRef`)
  and narrow `Engine.Core.Init`/`Engine.Loop.Shutdown`'s own internal
  helpers to accept it where they don't also need other capabilities.
  Given how universally `loggerRef`/`lifecycleRef` are read, most call
  sites will still need to reach them through a broader carrier for a
  while — this migration is more about establishing the record and
  proving the pattern than about shrinking many imports immediately.

### 7.2 `render-gpu-asset`

- **Dependencies:** `core-init` (logger, lifecycle).
- **Independent migration:** Yes, for the `MainRender`-only fields
  (`engineStateRef` and everything genuinely single-thread-owned).
  `textureSystemRef`/`textureSizeRef` are the one real complication —
  they are read/written from `WorldThread` too (see §3's note on why
  they moved to `EngineEnv` in the first place), so this capability's
  record cannot be scoped to "things only the render thread touches";
  it must be a record the world thread also legitimately imports.
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
