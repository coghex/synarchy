# EngineEnv Capability Inventory

**Status:** Authoritative. The `EngineEnv` capability split (epic #537)
is **complete**: every one of §2.1's eight capability identifiers has a
capability record or view, §6.2's temporary compatibility boundary is
**empty**, and `tools/engine_env_capability_audit.py` enforces the
permanent-only §6.1 boundary — the epic's "restrict direct
`EngineEnv(..)` access to documented initialization/orchestration
compatibility boundaries" acceptance condition, now machine-enforced.

Originally written 2026-07-23 against `master@7e5360c2` (issue #876) as
Phase 0 — an inventory of capability ownership, thread access,
lifecycle, and intended migration boundaries, landing **no** runtime or
type-boundary change of its own — and then carried forward by each of
the ten migration children (#889–#899). It remains the authoritative
ownership inventory: read it before adding a field to `EngineEnv`,
changing which thread touches one, or changing its lifecycle. §6.4 is
the procedure to follow when you think you need new state.

**What still is not inventoried here:** the internals of `EngineState`,
`WorldManager`, `UnitManager`, and `BuildingManager` (see §1's scope
boundary). The epic deliberately left those for a future, separate
inventory.

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

<!-- engineenv-field-total -->

`src/Engine/Core/State.hs` declares `data EngineEnv = EngineEnv { ... }`
with exactly **89** fields, `engineConfig` through `popupQueueRef`, and
every one of them has exactly one row in §5 below.

<!-- /engineenv-field-total -->

That marked paragraph is the only place this document states a field
total or names the record's first and last field, and it deliberately
carries no source line numbers: the hand-written ones that used to sit
in it had all drifted from the live record, unnoticed, because nothing
checked them (issue #1669). Since that issue,
[`tools/engine_env_capability_audit.py`](../tools/engine_env_capability_audit.py)
re-derives the count and both field names from the live declaration on
every run and rejects the block when either disagrees, so adding or
removing an `EngineEnv` field without amending that paragraph fails the
audit in CI and in `make ci` — even when §5's rows were updated
correctly. Do not restate the count elsewhere in this document, and do
not put a second number (a line anchor above all) inside the block; the
audit rejects both. The block is pinned to where it sits, not merely
to this section: it must be §1's first content, §1 may state no other
number outside it (section and issue references excepted, along with a
source-location span such as `src/Engine/Core/State.hs:446` — but not
a bare number in code font, which is a field total wearing a citation's
clothes), and the whole document is swept for the phrase shape
`<n> EngineEnv fields`. §6.2's first assignment-method
item, which used to repeat the total, is held to the same result
without markers — see the note there.

It is the same field set
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

- `EngineState` (`src/Engine/Core/State.hs:446`, nested under
  `engineStateRef`) is **not** inventoried field-by-field here. It is a
  single, already-documented invariant (§3) — main-render-thread-private
  — and its own fields (`timingState`/`graphicsState`/
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
| `world-sim-render-handoff` | World, simulation, time, worldgen, and render handoff. Two halves (§7.4): the **world/sim** fields — the world manager, the world and sim command queues, sun angle, the flora and material registries, worldgen config, pause flag and game clock (migrated to `WorldSimCapability` by #893, E5a) — and the **coupled render-handoff set** (#894, E5b): the single-slot world-preview and zoom-atlas staging refs plus the preview generation counter, the layered world quads the frame loop merges, the blood-texture dispose queue, and the persistent structure texture palette with its runtime paletteId→handle table. |
| `units-buildings-combat` | Units, combat, buildings, and pathing: the unit and building managers and their command queues, unit sim state, stat RNG, combat/injury/thought/action-outcome event streams, pathing tunables. |
| `content-registries` | Items, crafting, equipment, substances, infections, locations, loot, and the tutorial definition tree: static, YAML-backed content registries loaded once and queried thereafter. |
| `ui-hud-events` | UI, focus, HUD, selections, events, notifications, and popups: the UI page manager, focus manager, HUD active-page tracking, text-input buffers, the player-event store, notification config, popup queue. |
| `save-load-coordination` | Save/load coordination, provenance, and identity allocation: the save barrier, load status, the staged-load handoff, last-save-time bookkeeping, the item-instance id allocator. |

Generic buckets (`misc`, `shared`, `other`, a blank cell, or any
identifier not in this table) are rejected by the audit — every field
must resolve to exactly one of the eight above.

**This vocabulary is closed.** The eight identifiers here and the
`CAPABILITIES` constant in
[`tools/engine_env_capability_audit.py`](../tools/engine_env_capability_audit.py)
are the same list, and a field that fits none of them cannot be
classified at all. Adding a ninth is possible but deliberately hard —
see §6.4(c).

**Eight identifiers, fourteen record/view types.** The record set is
finer-grained than the identifier set, because six capabilities are
deliberately split, for three distinct reasons — four of them by
§3.1's pointer-record visibility rule (a thread-private field forces a
strictly narrower worker-safe view, never a documented restriction on a
wider record), one by consumer coupling, and one by **mutation
authority** (a field's legitimate writer and its many readers need
different *field types*, not different field *sets*):

| Identifier | Record / view type(s) | Landed by |
|---|---|---|
| `core-init` | `Engine.Core.Capability.Core` — `CoreCapability` (4 fields) | #889 (E1) |
| `render-gpu-asset` | `Engine.Core.Capability.Render` — `RenderCapability` (21, `MainRender`-only, carries `engineStateRef`); `Engine.Core.Capability.RenderView` — `RenderViewCapability` (worker-safe, never names `engineStateRef`) | #891 (E3) |
| `input-lua-transport` | `Engine.Core.Capability.Input` — `InputCapability` (8, `LuaThread`-only); `Engine.Core.Capability.InputView` — `InputViewCapability` (worker-safe, carries neither `inputBarrierNextRef` nor `currentKeyDownRef`) | #892 (E4) |
| `world-sim-render-handoff` | `Engine.Core.Capability.WorldSim` — `WorldSimCapability` (9 world/sim fields); `Engine.Core.Capability.RenderHandoff` — `RenderHandoffCapability` (7 coupled handoff fields) | #893 (E5a) / #894 (E5b) |
| `units-buildings-combat` | `Engine.Core.Capability.UnitCombat` — `UnitCombatCapability` (10); `Engine.Core.Capability.Building` — `BuildingCapability` (3) | #895 (E6a) / #896 (E6b) |
| `content-registries` | `Engine.Core.Capability.ContentRegistries` — `ContentRegistriesCapability` (8 registries, the raw WRITER interface); `Engine.Core.Capability.ContentRegistriesView` — `ContentRegistriesViewCapability` (the reader-facing view: 4 registries as `ReadOnlyRef`s + `crvInfectionManagerRef` raw) | #890 (E2) / #1896 (CMA-2) |
| `ui-hud-events` | `Engine.Core.Capability.Ui` — `UiCapability` (4 UI/focus/HUD fields); `Engine.Core.Capability.Events` — `EventsCapability` (4 event/notification/popup fields) | #897 (E7a) / #898 (E7b) |
| `save-load-coordination` | `Engine.Core.Capability.SaveLoad` — `SaveLoadCapability` (5 coordination handles) | #899 (E8) |

The `world-sim-render-handoff` split is the one that is not a §3.1
thread-privacy split: neither half carries a thread-private field, and
it exists because the render-handoff fields' consumers straddle
this group and `render-gpu-asset` (§7.4).

The `content-registries` split is the third kind, and the only one so
far (#1896, CMA-2 of the mutation-authority epic #1890). Both records
carry the same live handles; what differs is what a holder may DO with
four of them. `ContentRegistriesCapability` stays the raw writer
interface — the four `X.loadYaml` populators keep it — while every
other production consumer of those four registries takes
`ContentRegistriesViewCapability`, where they arrive as
`Engine.Core.ReadOnlyRef.ReadOnlyRef`s and a mutation is a compile
error. `crvInfectionManagerRef` rides on the view as the ordinary
`IORef` it is, because `Engine.Scripting.Lua.API.Units.Combat` is the
one module mixing a selected registry with an out-of-scope one and must
not keep the raw record merely to reach infection. Infection, locations,
loot tables and tutorials are deliberately OUTSIDE the pilot's
structural boundary; whether it is worth extending is CMA-3's call.

**The capability-record convention (canonical statement).** Every
record/view in the table above — and any future one — follows exactly
these six rules. This is their one authoritative statement; no other
file in the tree restates it in full, only summarizes and links back
here:

* **Naming and placement.** One module per capability under
  `Engine.Core.Capability.<Name>`, exporting one record named
  `<Name>Capability` with fields prefixed by the record's own
  initials, plus a single total `to<Name>Capability` projection.
* **One-way projection only.** The projection function goes
  `EngineEnv → XCapability` and nothing travels the other direction —
  a capability record is never assembled back into (or used to
  reconstruct) an `EngineEnv`. Each record/view is projected
  independently and totally from `EngineEnv` itself, never derived
  from a wider capability record.
* **Shared live containers, never copied state.** Every field is the
  exact same `IORef`/`TVar`/`Queue` handle (or immutable value)
  `EngineEnv` already carries — the projection aliases live state, it
  does not snapshot or duplicate it.
* **An abstract wrapper narrows AUTHORITY, and must still alias**
  (#1896). A view may present a field through an abstract wrapper
  around the *same live handle* — today exactly one exists,
  `Engine.Core.ReadOnlyRef.ReadOnlyRef`, which denies writes by
  exporting no constructor, no unwrap and no write. Four rules make
  that an extension of the rule above rather than an exception to it:
  the wrapper **aliases**, never copies, so a write through the
  legitimate raw handle is observed through it immediately (the rule
  above, unchanged, and it is what the projection-aliasing hspec
  module must prove for a wrapped field); its **construction is
  public**, because the guarantee is "a module handed only the wrapped
  form cannot write", not unforgeability — the raw handle is what
  confers authority, and a private constructor would merely block the
  view projection and test fixtures; there is **no `.Internal`
  companion**, no unwrap, and no second route back to the handle; and
  the wrapper's own module lives OUTSIDE `Engine.Core.Capability.*`,
  since it is a reference discipline rather than a capability record.
  A consumer narrowed to the wrapped form must not retain another route
  to the raw handle, or the boundary is decorative —
  `tools/engine_env_capability_audit.py` knows the wrapper by name
  (`ALIAS_PRESERVING_WRAPPERS`) so a wrapped field still canonicalizes
  onto its `EngineEnv` field for the §5 write map and the §6.5 residue.
* **No capability module imports its own consumers.** A capability
  module may be imported freely by the modules it narrows access for,
  but must never import back into them.
* **No unused capability records ahead of need.** A capability record —
  or a field within one — is introduced only in the migration that
  actually narrows a real consumer to it, never in anticipation of a
  future one.
* **A thread-private field forces a split, not a comment** (§3.1). A
  capability record is exported as `XCapability(..)` — constructor and
  accessors alike — so any module that can import it can construct and
  inspect every field it carries. When a record would be importable by
  a thread other than the one that privately owns one of its fields
  (`render-gpu-asset`'s `engineStateRef`; `input-lua-transport`'s
  barrier-token allocator and current-key handoff), documenting the
  restriction on that field is not enough: the capability is exposed
  as one main-only/owner-only record plus one or more strictly
  narrower worker-safe views that omit the private field entirely
  (§3.1 has the worked example and the audit's enforcement). A field
  being read or written by only one thread today is not by itself the
  trigger — `save-load-coordination`'s `slLastSaveTimeRef` and two
  `ui-hud-events` fields are single-role with no privileged pointer
  behind them, so documenting the restriction on the field alone is
  sufficient there (`Engine.Core.Capability.SaveLoad`/`.Ui`).

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

`EngineState` (`src/Engine/Core/State.hs:446-450`) carries the fully
main-render-thread-private mutable state: `TimingState`,
`GraphicsState` (every Vulkan handle, the GLFW window, the scene
render pipeline state), and `SceneManager`. Its own
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
`src/Engine/Core/State.hs:70-75`). **That placement is a carrying
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

### 3.1 The pointer-record visibility rule (#891, E3)

**Live since issue #891 (E3, landed).** That last clause — "even if
the *pointer* field migrates to a narrower render-capability record" —
came due when `render-gpu-asset` migrated, and it needed one addition
to stay honest. E1's capability convention exports each record as
`XCapability(..)`: the constructor *and* every accessor. So a
`render-gpu-asset` record that both (a) contains `engineStateRef` and
(b) is importable by worker-thread code would let that code construct
and inspect the main-render-private pointer — the exact thing the
paragraph above forbids — no matter what the field's documentation
said. Documentation is not a boundary.

The rule this document therefore adopts, extending the invariant
above from `EngineState`'s *contents* to the *pointer record* itself:

> **No non-`MainRender` production code gets an interface through
> which it can construct or inspect a record containing
> `engineStateRef`.**

`render-gpu-asset` satisfies it by being exposed as **two** interfaces
rather than one — the first capability in the split to need this, and
the pattern for any later capability with a thread-private field:

| Interface | Fields | Who may import it |
|---|---|---|
| `Engine.Core.Capability.Render` (`RenderCapability`) | all 21 of §5's `render-gpu-asset` fields, `engineStateRef` included | production modules whose execution domain §5 records as `MainRender` only |
| `Engine.Core.Capability.RenderView` (`RenderViewCapability`) | a strict subset — the 14 fields §5 records a `WorldThread`/`LuaThread`/`InputThread` reader or writer for (13 when #891 landed; `fpsRef` joined in #893, §7.2); **never `engineStateRef`** | any consumer, including every worker-thread one |

Both are projections **of `EngineEnv`**, one-way, over the identical
live containers (§7.2) — the narrower view is not derived from the
wider record, and nothing widens a capability record back out.

A **dual-domain** module — one whose functions run on a worker thread
*and* on `MainRender` — satisfies the boundary with the worker-safe
view alone; it does not get the full record for the `MainRender` half.
`World.Render.BloodQuads` is the worked example: `renderBloodDecalQuads`
builds quads on `WorldThread` while `uploadBloodTextures`/
`disposeQueuedBloodTextures` run on `MainRender` via
`processLuaMessages` (see §5's `textureSystemRef`/`textureSizeRef`/
`assetPoolRef` rows). Neither path needs `engineStateRef`, and the
view's handles are the same live containers, so one view serves both.

This is enforced, not merely documented.
`tools/engine_env_capability_audit.py` (CI and `make ci`) fails on any
of: a production module outside its checked-in
`RENDER_MAIN_ONLY_MODULES` importing `Engine.Core.Capability.Render`;
a production module outside `ENGINE_STATE_REF_OWNERS`
(`Engine.Core.State` declares it, `Engine.Core.Init` seeds it,
`Engine.Core.Monad` carries it, `Engine.Core.Capability.Render`
projects it) naming `engineStateRef`/`rcEngineStateRef` at all; or
`Engine.Core.Capability.RenderView` so much as mentioning the field.
Both module sets are checked in *both* directions, like §6's ratchet,
so a stale entry fails too.

Two fields that logically belong beside `GraphicsState` — the bindless
texture system (`textureSystemRef`) and the default face-map slot
(`defaultFaceMapSlotRef`) — were deliberately moved to `EngineEnv`
*because* worker threads (the world thread's dynamic blood-texture
registration, in particular) need to reach them; `EngineState`'s own
doc comment records exactly this (`src/Engine/Core/State.hs:415-420`).
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
`src/Engine/Core/Init.hs:359-364`). What genuinely differs per profile
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

**What the audit checks about these cells.** The Readers and Writers
columns are validated for *grammar* and *citation presence* — every
role is one of §2.2's, and the row cites real source — never for
truth. A role claim here is prose and stays prose. Since #1892 there
is a second, independent mechanism that pins the same ownership
question at a granularity a source scan can actually verify:
`tools/engine_env_capability_audit.py`'s checked-in
`CAPABILITY_WRITER_MODULES` map, which fixes each field's direct
writing **modules** and rejects an undeclared write and a stale entry
alike. Read §6.5 before you conclude that either half proves the
other — they verify different things, and the map deliberately does
not attempt the role claim.

### `core-init`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `engineConfig` | boot-process | `AnyThread` (read via the engine environment value itself from every thread — e.g. `ecHeadless` gates GPU-only paths in `Engine.Graphics.Vulkan.Command.Record`) | None (immutable after boot — each `app/App/*.hs` applies one record-update for `ecDebugPort`/`ecBootProfile`/`ecPreviewTarget`/`ecHeadless` from CLI args before any worker thread starts, e.g. `app/App/Graphical.hs:39-48`) | Plain `EngineConfig` record field, no `IORef` — safe from any thread precisely because it is never mutated after boot | `Engine.Core.Init.initializeEngineWith` sets `defaultEngineConfig` (`src/Engine/Core/Init.hs:265`) | None — plain data, reclaimed at process exit | Immutable-boot-configuration carve-out (no writers) is intentional, not a missing decision. |
| `loggerRef` | boot-shutdown | `AnyThread` (every thread logs through it — `Unit.Thread`, `Combat.Thread`, `World.Thread`, `Sim.Thread`, `Engine.Scripting.Lua.Thread`, `Engine.Loop`) | `AnyThread` (`Engine.Core.Log`'s `logInfo`/`logDebug`/`logWarn` write through this ref from any thread) | `IORef LoggerState`; the logger backend batches/flushes internally | `Engine.Core.Init.initializeEngineWith` (`src/Engine/Core/Init.hs:157-158`); backend is `stdout` (graphical/headless) or `stderr` (dump, so stdout stays clean JSON) | Explicitly flushed via `shutdownLogger`, last, after every worker thread has stopped (`src/Engine/Loop/Shutdown.hs:104-107`) and in each `App.*` module's own error branch | Must outlive every other thread's own shutdown log line — hence torn down last, not merely GC'd. |
| `lifecycleRef` | boot-process | `AnyThread` (every worker run-loop polls it each tick — `Unit.Thread`, `Combat.Thread`, `World.Thread`, `Sim.Thread`, `Engine.Scripting.Lua.Thread`, `Engine.Input.Thread`, `Engine.Loop.Mode`) | `AnyThread` (the initial-running transition), `MainRender` (sets the final stopped value) | `IORef EngineLifecycle` (`EngineStarting|EngineRunning|CleaningUp|EngineStopped`) | `Engine.Core.Init.initializeEngineWith` seeds `EngineStarting` (`src/Engine/Core/Init.hs:154`) | Set to `EngineStopped` as literally the last step of `Engine.Loop.Shutdown.shutdownEngine`, after every worker thread has stopped and the logger has flushed (`src/Engine/Loop/Shutdown.hs:106-108`) | — |
| `inputThreadActiveRef` | boot-process | `LuaThread` (save-barrier owner-set computation — `API.Save:188`'s `saveWorldFn`, `Thread.Dispatch:408`'s `handleLoadStaged` — consults it to decide whether the input owner slot belongs in a save/load transaction's owner set) | `Boot` (set to true exactly once by `Engine.Input.Thread.startInputThread:49`, on the CALLING thread, immediately BEFORE it forks the actual input thread via `forkIO`; never written again) | `IORef Bool`, write-once | `Engine.Core.Init.initializeEngineWith` seeds `False` (`src/Engine/Core/Init.hs:237`) | None | Boot-profile-derived fact (only Graphical/Offscreen/Preview start an input thread — §4); primary owner is `core-init` even though its principal *consumer* is `save-load-coordination` — see §7. |

### `render-gpu-asset`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `engineStateRef` | boot-shutdown | `MainRender` (only; see §3, the main-thread-private invariant) | `MainRender` (only) | `IORef EngineState`, single-thread-owned; no atomic ops needed since only one thread ever touches it | `Engine.Core.Init.initializeEngineWith` seeds `defaultEngineState` (`src/Engine/Core/Init.hs:263`) | Contents (every Vulkan handle in the nested `GraphicsState`) explicitly destroyed by `Engine.Loop.Shutdown.shutdownEngine` — `deviceWaitIdle`, transient-texture cleanups, `runAllCleanups (vulkanCleanup state)`, explicit sampler/buffer destruction (`src/Engine/Loop/Shutdown.hs:41-84`) — before GLFW/thread teardown; the `IORef` container itself is never destroyed, only overwritten/zeroed | See §3 in full. |
| `videoConfigRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`/`Engine.Graphics.Vulkan.Recreate`, `Engine.Loop.Timing`, and each graphics-capable boot module's own window-creation read — `app/App/Graphical.hs:57`, `app/App/Offscreen.hs:65`, `app/App/Preview.hs:52` — all AFTER `startInputThread`/`startLuaThread` (and, in Graphical/Offscreen, every other worker thread) have already been started, so per §2.2 this is `MainRender`, not `Boot`, in every boot profile), `LuaThread` (`API.Config:36`'s `getVideoConfigFn`, direct query) | `LuaThread` (`Engine.Scripting.Lua.API.Config` — direct settings Apply/Save/Defaults, called synchronously from Lua), `MainRender` (`Engine.Scripting.Lua.Message.Video`'s `handleSetVSync`/`handleSetMSAA`, dispatched via `processLuaMessages` from `Engine.Loop.mainLoop`/`Engine.Loop.Headless`) | `IORef VideoConfig`, multi-writer via `atomicModifyIORef'` | Loaded from `config/video.local.yaml` (or default) (`src/Engine/Core/Init.hs:176-180`) | None | Settings-apply path (#748/#750) reads this on `MainRender` to rebuild the swapchain. Lua-triggered writes split by mechanism: a direct `API.Config` call (e.g. `engine.setVideoConfig`) writes synchronously on `LuaThread`; a call that must also touch the live Vulkan device (VSync/MSAA) instead enqueues onto `luaToEngineQueue` and is applied later, on `MainRender`, when `processLuaMessages` drains it — see `input-lua-transport`'s `luaToEngineQueue`/`luaQueue` rows. |
| `windowSizeRef` | boot-process | `WorldThread` (screen-space quad builders, `World/Render/*Quads.hs`, and `Unit.HitTest`'s hit-testing, reached via `World.Render.CursorQuads`/`updateWorldTiles`), `LuaThread` (`API.Input`, `API.InputInject`, `API.WorldQuery.Pick`, and `Unit.HitTest`/`Building.HitTest` called directly from `API.Buildings.Selection`/`API.Units.Selection`/`API.WorldQuery.Pick`), `MainRender` (`Engine.Loop.Camera`, `Engine.Loop.Frame`), `InputThread` (`Input.Thread.Mouse:56`, `Input.Thread.Scroll:39` — cursor/wheel coordinate conversion) | `InputThread` (native resize callback, `Engine.Input.Thread.Dispatch:117`), `MainRender` (`Engine.Graphics.Window.GLFW:109`, `Engine.Loop.Frame:384`, and `Engine.Scripting.Lua.Message.Video`'s `handleSetResolution`/`handleSetWindowMode` for synthetic/Lua-triggered resolution changes — dispatched via `processLuaMessages`, never the Lua thread itself) | `IORef (Int,Int)`, multi-writer, last-write-wins (no cross-writer ordering guarantee) | Seeded from the loaded `VideoConfig` (`src/Engine/Core/Init.hs:181`) | None | Same multi-writer shape as `framebufferSizeRef`/`windowStateRef` below. |
| `windowPosRef` | boot-process | `LuaThread` (`API.Input`'s `getWindowPosFn`, registered as the diagnostic `debug.getWindowPos` — the only reader) | `MainRender` (only; `Engine.Graphics.Window.GLFW`'s `createWindow` and `Engine.Scripting.Lua.Message.Video`'s `handleSetResolution`/`handleSetWindowMode`, dispatched via `processLuaMessages`, never the Lua thread itself) | `IORef (Int,Int)`, single-writer-thread, last-write-wins | `(0,0)`, replaced by the real position at window creation (`src/Engine/Core/Init.hs:182`) | None | Publish-on-change, NOT a live cursor: no GLFW window-position callback is installed, so a user dragging the window leaves this stale until the next publish. It exists because `GLFW.getWindowPos` is main-thread-only, so #907's windowed-geometry restore is otherwise unobservable from a Lua-side check; `tools/video_window_check.py` forces a publish before reading. |
| `windowStateRef` | boot-process | `MainRender` (only; `Message.Video`, read back when restoring windowed geometry) | `MainRender` (only; `Engine.Graphics.Window.GLFW`'s `createWindow` folds what GLFW actually applied through `applyWindowCreation`, and `Engine.Scripting.Lua.Message.Video`'s `handleSetWindowMode` folds each APPLIED mode switch through `applyWindowModeTransition`, caching windowed pos/size only when leaving `wsAppliedMode ≡ Windowed` (#907), dispatched via `processLuaMessages`, never the Lua thread itself) | `IORef WindowState` | `defaultWindowState` (`src/Engine/Core/Init.hs:192`); `wsAppliedMode` is then overwritten at window creation via `applyWindowCreation`, which for a creation that APPLIED a non-windowed mode — borderless (#1731) or fullscreen (#1882) — ALSO seeds the windowed pos/size from the live decorated window sampled before that mutation. Both stay at the `defaultWindowState` values in the window-less boot profiles, and in a boot whose requested mode degraded to the plain decorated window | None | `wsAppliedMode` is deliberately render-thread-owned rather than read back from `vcWindowMode`: the Lua thread publishes the TARGET mode into the video config as soon as it enqueues `LuaSetWindowMode`, so the config already reports the mode being entered by the time the handler runs. Seeding it from the CONFIG would be wrong too: since #1731 `createWindow` applies a persisted `BorderlessWindowed` as well as `Fullscreen`, but EITHER request degrades gracefully to the plain decorated window when no primary monitor or video mode is available, so only the outcome is truthful. The windowed pos/size seed exists for the same reason: applying EITHER non-windowed mode at creation consumes the first-switch caching opportunity `applyWindowModeTransition` otherwise relies on, so both outcomes must seed it (#1731, #1882). |
| `framebufferSizeRef` | boot-process | `WorldThread` (`World/Render.hs`, `World.Render.GroundItemQuads`, `World.Render.BloodQuads`, `World.Render.SpoilQuads`, `World.Render.CursorQuads`), `LuaThread` (`API.UI.Placement`, `API.World.Query`, `API.Input`), `InputThread` (`Input.Thread.Mouse:57`, `Input.Thread.Scroll:40`), `MainRender` (`UI.Tooltip.State:33`, the per-frame tooltip tick called from `Engine.Loop.Frame`) | `InputThread` (`Thread/Dispatch:121`), `MainRender` (`Engine.Scripting.Lua.Message.Video`, dispatched via `processLuaMessages`, and `app/App/Offscreen.hs:72`'s initial-size seed — this runs after `startInputThread`/`startLuaThread`/`startWorldThread`/`startUnitThread`/`startSimThread`/`startCombatThread` have all already been called on the same calling thread, so per §2.2 it is `MainRender`, not `Boot`) | `IORef (Int,Int)`, multi-writer | `Engine.Core.Init.initializeEngineWith` (`src/Engine/Core/Init.hs:188`) | None | — |
| `framebufferMinimizeGenRef` | boot-process | `MainRender` (only; `Engine.Graphics.Vulkan.ResizeRequest`'s `sampleFramebufferState`/`currentMinimizeGeneration`, reached from `Engine.Loop`'s windowed tick and `Engine.Graphics.Vulkan.Recreate`) | `InputThread` (only; `Engine.Input.Thread.Dispatch`'s `FramebufferResize` branch, on a zero-area framebuffer) | `IORef Word64`, single-writer-thread, bumped with `atomicModifyIORef'`; read separately from `framebufferSizeRef` and deliberately not atomic with it (`Engine.Graphics.Vulkan.ResizeRequest` documents why a torn read is conservative) | `0` (`src/Engine/Core/Init.hs:193`) | None | The minimize EDGE (#1693). `framebufferSizeRef` holds a LEVEL, so a minimize and a restore to the pre-minimize size can both be drained by one `processInputs` call and leave that ref unchanged — hiding the fact that the swapchain needs rebuilding. Monotonic and never reset; only the input thread sees every event, which is why this is shared state rather than main-render-private `GraphicsState`. |
| `fpsRef` | boot-process | `LuaThread` (`API.Core`, `engine.getFPS`) | `MainRender` (only, once per frame; `Engine.Loop.Timing:75`) | `IORef Double`, single-writer | `0.0` (`src/Engine/Core/Init.hs:155`) | None | — |
| `brightnessRef` | boot-process | `MainRender` (`Engine.Loop.Frame`, `Engine.Graphics.Vulkan.Init`) | `MainRender` (`Engine.Scripting.Lua.Message.Video:174`'s `handleSetBrightness`, dispatched via `processLuaMessages` — never the Lua thread itself, which only enqueues the request) | `IORef Int` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:189`) | None | — |
| `pixelSnapRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `LuaThread` (`API.Config:223`, direct synchronous `writeIORef`), `MainRender` (`Engine.Scripting.Lua.Message.Video:180`'s `handleSetPixelSnap`, dispatched via `processLuaMessages`, a separate call path from `API.Config`'s) | `IORef Bool` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:190`) | None | — |
| `textureFilterRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Texture.Bindless` — sampler selection) | `LuaThread` (`API.Config:236`, direct synchronous `writeIORef` alongside enqueuing the change), `MainRender` (`Engine.Scripting.Lua.Message.Video:190`'s `handleSetTextureFilter`, dispatched via `processLuaMessages` — this is also where the live GPU sampler swap on `textureSystemRef` happens) | `IORef TextureFilter` | From loaded `VideoConfig` (`src/Engine/Core/Init.hs:191`) | None | — |
| `assetPoolRef` | boot-process | `LuaThread` (shared into the Lua backend as `apRef`, `Engine.Scripting.Lua.Thread:51`), `MainRender` (`Message.Texture:84`'s `duplicateCachedTextureHandle`, and `World.Render.BloodQuads:179`'s `uploadOne` -- BOTH dispatched via `processLuaMessages`: `uploadOne` is reached through `uploadBloodTextures`, which `Engine.Scripting.Lua.Message.processLuaMessages` calls directly, not through the world thread's `updateWorldTiles` quad-building path) | `LuaThread`/`Boot`, `MainRender` (`Message.Texture:108`, `atomicModifyIORef'` bumping a cached atlas's refcount, dispatched via `processLuaMessages`) | `IORef AssetPool` | `defaultAssetPool` (`src/Engine/Core/Init.hs:160-161`) | `Engine.Loop.Shutdown.shutdownEngine` (#1691), which calls `Engine.Asset.Manager.cleanupAssetManager` -- after its `deviceWaitIdle` and before `runAllCleanups`, and only in a boot mode holding both a `vulkanDevice` and its `deviceQueues`, since the drain fails loudly without either. Every atlas loaded through `Engine.Scripting.Lua.Message.Texture:255` stores an explicit `taCleanup` closure (freeing that atlas's image view, image, and device memory) inside `apTextureAtlases`; `cleanupResources` invalidates every stable handle naming a slot it is freeing (`releaseOwnerHandles` + `releaseTextureHandles`, #1281) before running one, then clears `apTextureAtlases` and `apAssetPaths`. A release that fails is logged and contained so the rest of shutdown still runs. `unloadAsset`, the mid-session single-atlas release, still has no caller -- when a texture should be released while the session runs is a separate policy question #1691 left out of scope. Bindless slot unregistration is `unloadAsset`'s own separate step, not part of `taCleanup` | — |
| `textureNameRegistryRef` | boot-process | `LuaThread` (`Engine.Asset.TextureNameRegistry` name→handle lookups), `WorldThread` (`World.Render.GroundItemQuads:162`'s broken-equipment overlay lookup) | `LuaThread` (registration during content load) | `IORef TextureNameRegistry` | `emptyTextureNameRegistry` (`src/Engine/Core/Init.hs:166`) | None | — |
| `fontCacheRef` | boot-shutdown | `MainRender` (`UI.Render`, `Engine.Scene.Batch.Text`, `Engine.Graphics.Vulkan.Command.Text`), `LuaThread` (`API.Text`) | `MainRender`/`Boot` (`Engine.Graphics.Font.Load` rasterizes on demand) | `IORef FontCache` | `defaultFontCache` (`src/Engine/Core/Init.hs:203`) | Glyph-atlas GPU memory registered via `allocResource` at creation (`Engine.Graphics.Font.Upload`/`Draw`), freed by the generic `vulkanCleanup` sweep in `shutdownEngine` | — |
| `textureSystemRef` | boot-shutdown | `WorldThread` (`src/Unit/Render.hs:131`, via `updateWorldTiles`'s world-thread quad-building pass), `MainRender` (`src/UI/Render.hs`, `Engine.Scripting.Lua.Message.Texture` reads, and `World.Render.BloodQuads:76,161` — the blood-texture upload/dispose functions run via `processLuaMessages`, NOT the world thread's `updateWorldTiles` quad-building path `renderBloodDecalQuads` uses), `LuaThread` (`API.Blood`, direct queries) | `MainRender` (`Engine.Graphics.Vulkan.Init:213` — initial creation on the same thread that runs `Engine.Loop.mainLoop`; also `Engine.Scripting.Lua.Message.Video:196`/`Message.WorldTexture`/`Message.Texture`'s live rebuild/registration handlers, and `World.Render.BloodQuads:84,166` — all dispatched via `processLuaMessages`, never the Lua thread itself and never the world thread) | `IORef (Maybe BindlessTextureSystem)`, all writes confined to `MainRender` (multiple call sites, single writer thread); read by `WorldThread`/`LuaThread` too | `Nothing` at engine boot; populated by `Engine.Graphics.Vulkan.Init:213` on `MainRender` after device creation (graphical/offscreen/preview only — stays `Nothing` under headless/dump) | GPU descriptor/image resources registered via `allocResource` at creation, freed by the `vulkanCleanup` sweep in `shutdownEngine` | Moved to `EngineEnv` specifically because worker threads must be able to READ it (writes stay confined to `MainRender`) — see §3. |
| `samplerCacheRef` | boot-shutdown | `MainRender` (texture/font upload paths acquire samplers by kind) | `MainRender` | `IORef SamplerCache`, refcounted | `emptySamplerCache` (`src/Engine/Core/Init.hs:210`) | Explicitly destroyed via `destroySamplerCache` in `shutdownEngine` (`src/Engine/Loop/Shutdown.hs:62-65`) | — |
| `textureSizeRef` | boot-process | `WorldThread` (`World.Render.GroundItemQuads`, `World.Render.BloodQuads:238`'s `renderBloodDecalQuads` — reached via `updateWorldTiles`, not the upload/dispose path below, `World.Render.Quads`), `MainRender` (`Engine.Scripting.Lua.Message.Texture`, dispatched via `processLuaMessages`), `LuaThread` (`API.Blood`, direct queries) | `MainRender` (`World.Render.BloodQuads:127,188` — per-texture insert/delete as blood textures are created/disposed via the `uploadBloodTextures`/`disposeQueuedBloodTextures` functions `processLuaMessages` calls directly, NOT the world thread's quad-building path, and `Engine.Scripting.Lua.Message.Texture:117,267`, also dispatched via `processLuaMessages` — never the Lua thread itself, never the world thread) | `IORef (HashMap TextureHandle (Int,Int))`, all writes confined to `MainRender` (multiple call sites, single writer thread) | `HM.empty` (`src/Engine/Core/Init.hs:211`) | None — entries are deleted per-texture as their owning textures are disposed, not wholesale at shutdown | — |
| `defaultFaceMapSlotRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`) | `MainRender` | `IORef Word32` | `0` (`src/Engine/Core/Init.hs:212`), reassigned once the default face-map texture binds during Vulkan init | None | — |
| `cameraRef` | session-replaced | `WorldThread` (`World.Render.*`, `World.Thread`, and `Unit.HitTest` reached via `World.Render.CursorQuads`/`updateWorldTiles`), `LuaThread` (`API.Camera:55`'s `cameraGetPositionFn`, and `Unit.HitTest`/`Building.HitTest` called directly from the Lua Selection/Pick API modules), `MainRender` (`Engine.Loop.Frame:243` — the frame loop reads the LIVE camera each frame for the view matrix specifically to avoid the stale-by-tens-of-milliseconds value the world thread's own copy could show) | `WorldThread` (`World/Render.hs:194,212`, `World.Thread.Command.Init:300,356`, load publish `World.Load.Publish:129`), `LuaThread` (`API.Camera:44`), `MainRender` (`Engine.Loop.Camera:178` — WASD camera-pan velocity/position integration runs once per frame on the main thread) | `IORef Camera2D`, multi-writer via `atomicModifyIORef'` | `defaultCamera` (`src/Engine/Core/Init.hs:193`) | None | Session-replaced on load publish; see `docs/persistence_state_inventory.md` §1 `cameraRef`. |
| `uiCameraRef` | boot-process | `MainRender` (`Engine.Graphics.Vulkan.Init`/`Engine.Graphics.Vulkan.Recreate`) | `MainRender` (window/framebuffer resize path, `Engine.Graphics.Vulkan.Recreate:69`) | `IORef UICamera` | `defaultUICamera`, sized from the loaded `VideoConfig` (`src/Engine/Core/Init.hs:194-195`) | None | — |
| `screenshotRequestQueue` | transient-handoff | `MainRender` (dequeues one per frame in `Engine.Loop.Frame`'s `drawFrame` and replies on the request's own reply channel) | `LuaThread` (`API.Screenshot`) | `Q.Queue ScreenshotRequest` (STM `TQueue`) | `Q.newQueue` (`src/Engine/Core/Init.hs:151`) | None — never drained under headless, the verb refuses before enqueueing | — |
| `nextObjectIdRef` | boot-process | `LuaThread` (only; shared into the Lua backend state as `lbsNextObjectId` — `Engine.Scripting.Lua.Thread:51`, `Engine.Scripting.Lua.Types:43`) | `LuaThread` (only; bumped by every `UI.new*`/scene-object allocation issued from Lua) | `IORef Word32`, monotonic, single-thread-owned (Lua thread only) | `0` (`src/Engine/Core/Init.hs:162`) | None | Increment-only with no free list and no reset on load, so an id is not reused in any ordinary session — but it is a `Word32` (`Engine.Scripting.Lua.API.Text` bumps it as `n + 1`), so it does WRAP after 2^32 allocations. Nothing may rest on "an id is unique forever": the scene-text cache stays correct because entries are removed with their nodes (#1961), not because a stale entry could never be hit by a later id. |

### `input-lua-transport`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `inputQueue` | boot-process | `InputThread` (drains; `Engine.Input.Thread`, `Thread/Dispatch`), `MainRender` (the startup flush every boot mode's loop shares, `Engine.Loop.Mode.runStartupHandshake`) | `Boot`/`MainRender` (GLFW callbacks via `setupCallbacks`), `LuaThread` (`API.InputInject` synthetic injection), `WorldThread` (`World.Load.Publish:210-220`'s `discardStaleQueues`, flushes stale queued input on a load publish) | `Q.Queue InputEvent` (STM `TQueue`), multi-producer/single-consumer FIFO | `Q.newQueue` (`src/Engine/Core/Init.hs:144`) | None; drained/discarded at process exit | — |
| `inputBarrierNextRef` | boot-process | `LuaThread` (`Engine.Input.Inject.newBarrierToken` allocates) | `LuaThread` (only) | `TVar Int`, monotonic allocator | `newTVarIO 0` (`src/Engine/Core/Init.hs:145`) | None | — |
| `inputBarrierRef` | boot-process | `LuaThread` (`Engine.Input.Inject.waitForBarrier`) | `InputThread` (`Engine.Input.Thread.Dispatch.processInput`, strictly after a barrier's turn is fully processed — see the field's own doc comment, `src/Engine/Core/State.hs:89-106`) | `TVar Int` | `newTVarIO 0` (`src/Engine/Core/Init.hs:146`) | None | — |
| `inputStateRef` | session-replaced | `InputThread` (`Thread/Dispatch:46,58`), `LuaThread` (shared as `lbsInputState`), `MainRender` (`Engine.Loop.Camera`) | `InputThread`, `WorldThread` (load publish, `World.Load.Publish:282`, resets to `defaultInputState`) | `IORef InputState` | `defaultInputState` (`src/Engine/Core/Init.hs:168`) | None | Reset on load so stale held-key state can't survive a load. |
| `keyBindingsRef` | boot-process | `InputThread` (`Thread/Keyboard`), `MainRender` (`Engine.Loop.Camera`), `LuaThread` (`API.Input`) | `LuaThread` (only; `API.Keybinds` — settings rebind/reset) | `IORef KeyBindings`, multi-reader/single-writer via `atomicModifyIORef'` | Loaded from `config/keybinds.local.yaml` (or default) (`src/Engine/Core/Init.hs:169-173`) | None | — |
| `currentKeyDownRef` | transient-handoff | `LuaThread` (only) | `LuaThread` (only) | `IORef (Maybe GLFW.Key)` — see the field's own doc comment, `src/Engine/Core/State.hs:123-128` | `Nothing` (`src/Engine/Core/Init.hs:174`) | None | Meaningful only for the duration of one `onKeyDown` broadcast. |
| `luaToEngineQueue` | boot-process | `MainRender` (`Engine.Scripting.Lua.Message`'s `processLuaMessages` — deliberately NOT flushed by `World.Load.Publish`'s `discardStaleQueues` the way `unitQueue`/`buildingQueue`/`combatQueue`/`simQueue`/`inputQueue` are; a stale load-time message is instead left in place and naturally skipped, since `processLuaMessages` itself is gated behind the save barrier's `captureLocked` check — see `World/Load/Publish.hs:77-83`'s own comment on why flushing it from the publish side raced this consumer's drain) | `LuaThread` (`Engine.Scripting.Lua.Thread`, `Thread/Dispatch`) | `Q.Queue LuaToEngineMsg` | `Q.newQueue` (`src/Engine/Core/Init.hs:149`) | None | — |
| `luaQueue` | boot-process | `LuaThread` (drains; `Engine.Scripting.Lua.Thread`, `Engine.Scripting.Lua.Util`) | `WorldThread` (`World.Thread.Command.Init`, `World.Thread.ChunkLoading`, `World.Thread.Command.Save`, `World.Thread.Helpers`), `MainRender` (`Message.Video`), `UnitThread`/`CombatThread` (notification broadcasts), `InputThread`, `LuaThread` (`API.World.Lifecycle:130`'s `worldOpenArenaFn`, a direct Lua-callable enqueue) | `Q.Queue LuaMsg`, multi-producer/single-consumer | `Q.newQueue`, bound as `engineToLuaQueue` (`src/Engine/Core/Init.hs:150,274`) | None | Engine→Lua direction (the field is literally named `luaQueue` on `EngineEnv` but constructed as `engineToLuaQueue`). |

### `world-sim-render-handoff`

This group migrated in two halves (§7.4), both landed. Since #893 (E5a)
the nine **world/sim** fields — `worldManagerRef`, `worldQueue`,
`sunAngleRef`, `floraCatalogRef`, `materialRegistryRef`,
`worldGenConfigRef`, `gameTimeRef`, `enginePausedRef`, `simQueue` — are
reached through
`Engine.Core.Capability.WorldSim.WorldSimCapability` rather than an
`EngineEnv` field; since #894 (E5b) the rest —
`worldPreviewRef`, `worldPreviewGenerationRef`, `zoomAtlasDataRef`,
`worldQuadsRef`, `bloodDisposeQueue`, `texPaletteRef`,
`texPaletteHandlesRef`, since #1712 `structureWallCatalogRef`, and since
#1921 `sceneStatsRef`, the
**coupled render-handoff** half (the world
thread's staging surface for `MainRender` GPU uploads plus the
structure-palette translation table, the packs' directional wall art
and the scene-assembly telemetry measured while building the published
quads) — are reached through
`Engine.Core.Capability.RenderHandoff.RenderHandoffCapability`. Both
hold for every reader and writer below EXCEPT the §6.1 permanent
orchestration modules, which keep whole-environment access by job
description. Every per-field contract in the table below holds
unchanged either way — neither record grants any new read or write
authority, they only remove the ability to reach fields a consumer has
no business touching, and the render-handoff half in particular changes
no handoff semantics, upload cadence, staleness policy or disposal
ordering.

The clearing contract differs per lifecycle within the render-handoff
half, so the record documents each field's own rather than a blanket
rule: `worldPreviewRef`/`zoomAtlasDataRef` are single slots consumed to
`Nothing` by their `MainRender` upload handler and `bloodDisposeQueue`
is drained by its `MainRender` consumer (`transient-handoff`);
`worldPreviewGenerationRef` is monotonic and never cleared while
`worldQuadsRef` and `sceneStatsRef` stay published until replaced or
explicitly cleared by a world teardown (`boot-process`); `texPaletteRef`/
`texPaletteHandlesRef` follow session replacement (`session-replaced`).

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `worldManagerRef` | session-replaced | `UnitThread` (`Unit.Thread.Command.*`, and `Unit.LineOfSight` reached via `Unit.Thread.Command.Lifecycle`), `CombatThread` (`Unit.LineOfSight` reached via `Combat.Resolution`/`Combat.Resolution.Strike`), `WorldThread` (`World.Render*`, `Unit.Render`, and `Unit.HitTest` reached via `World.Render.CursorQuads` — all via `updateWorldTiles`'s world-thread quad-building pass, not `MainRender`, see the `render-gpu-asset` group's BloodQuads note for the distinct upload/dispose exception), `LuaThread` (`API.Units.Spawn:107` direct read, `API.Structure:54`'s `resolveStructurePage` — a direct synchronous read shared by every `structure.*` Lua entry point, and `Unit.HitTest`/`Unit.LineOfSight` called directly from `API.Buildings.Selection`/`API.Units.Selection`/`API.WorldQuery.Pick`/`API.Units.Query`), `MainRender` (`Engine.Loop.Camera:72`'s `getWorldSize`, called from the same module's camera-pan update, and the `Message.Texture` path's `invalidateAllWorldRenderCaches`) | `WorldThread` (world init/load/edit commands, and load publish `World.Load.Publish:158`) | `IORef WorldManager`, multi-writer | `emptyWorldManager` (`src/Engine/Core/Init.hs:197`) | None | Contents classified per-field in `docs/persistence_state_inventory.md` §3/§4; this row covers only the `EngineEnv` pointer, per §1's scope note. |
| `worldQueue` | boot-process | `WorldThread` (drains; `World.Thread`) | `LuaThread` (`API.Construct`, `API.Structure`, `API.Till`, `API.Plant`, `API.World.Lifecycle`, `API.World.Edit`), `SimThread` (`Sim.Thread:392`, enqueues `WorldApplyFluids` once fluid writebacks are ready), `WorldThread` (`World.Thread:154-162`'s `processAuthorizedSave` re-enqueues commands deferred past a captureLocked window), `MainRender` (`app/App/Dump.hs:88,102`, the dump driver's own `WorldInit`/`WorldShow` enqueues) | `Q.Queue WorldCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:147`) | None | — |
| `sunAngleRef` | boot-process | `LuaThread` (`API.World.Clock`, `API.WorldQuery.Climate`), `MainRender` (lighting, `Engine.Graphics.Vulkan.Init`, `Engine.Loop.Frame`), `WorldThread` (`World.Render`, building the frame's per-page solar table — #1869) | `WorldThread` (`World.Thread.Time`, derived via `worldTimeToSunAngle`), `LuaThread` (`API.World.Clock`'s `worldSetSunAngleFn`, direct override) | `IORef SolarBase` | `publishedSolar 0.25` = noon, not overridden (`src/Engine/Core/Init.hs`) | None | The process-global BASE angle only. Since #1869 each visible page's own angle and circumference travel with the published quads (`Engine.Scene.Types.Batch.lqSolar`), not through this field; the `sbOverridden` flag beside the angle is what makes `world.setSunAngle` reach every rendered page until the next tick republishes. |
| `worldPreviewRef` | transient-handoff | `MainRender` (consumes for GPU upload) | `WorldThread` (enqueues, and load publish `World.Load.Publish:152`), `MainRender` (`Message.WorldTexture:65`'s `handleWorldPreview`, `atomicModifyIORef'` clearing the slot once dequeued) | `IORef (Maybe (Int,Int,ByteString,Word64))`, single-slot, tagged with a generation | `Nothing` (`src/Engine/Core/Init.hs:205`) | None | Paired with `worldPreviewGenerationRef` to suppress a stale in-flight upload (round 10 review, #763). |
| `worldPreviewGenerationRef` | boot-process | `LuaThread` (`Thread.Dispatch:319`'s `LuaWorldPreviewReady` handler — the generation comparison deliberately happens HERE, at delivery time, not in `Message.WorldTexture.handleWorldPreview`'s upload-completion code, which stopped reading this ref after round 11's review; see that module's own comment explaining why) | `WorldThread` (enqueue bumps it, and load publish `World.Load.Publish:149`) | `IORef Word64`, monotonic, never decreases | `0` (`src/Engine/Core/Init.hs:206`) | None | — |
| `zoomAtlasDataRef` | transient-handoff | `MainRender` (consumes for GPU upload) | `WorldThread` (enqueues, and load publish `World.Load.Publish:137`), `MainRender` (`Message.WorldTexture:186`'s `handleZoomAtlasUpload`, `atomicModifyIORef'` clearing the slot once dequeued) | `IORef (Maybe (Int,Int,ByteString,[WorldState]))`, single-slot | `Nothing` (`src/Engine/Core/Init.hs:207`) | None | Captures the exact `WorldState`s it belongs to at enqueue time (round 9 review, #763). |
| `worldQuadsRef` | boot-process | `MainRender` (frame loop merges + draws) | `WorldThread` (per-tick static/dynamic quad split, #446) | `IORef LayeredQuads` | `emptyLayeredQuads` (`src/Engine/Core/Init.hs:208`) | None | — |
| `sceneStatsRef` | boot-process | `LuaThread` (`Engine.Scripting.Lua.API.SceneStats`'s `getSceneStatsFn` — `debug.getSceneStats()`, one direct synchronous read of the whole immutable snapshot) | `WorldThread` (`World.Render.updateWorldTiles` publishes once per completed pass; `World.Thread.Command.Basic`'s `handleWorldDestroyCommand`/`handleWorldDestroyAllCommand` clear it back to `Nothing` beside the `worldQuadsRef` clear) | `IORef (Maybe SceneStats)`, single-writer thread, whole-value replacement | `Nothing` (`src/Engine/Core/Init.hs`) | None | #1921. Scene-assembly telemetry for the pass that produced `worldQuadsRef`'s current value, and PUBLISHED on exactly the same terms — which is why it is cleared at the same two teardown sites: the two describe one world lifecycle, so they must not disagree about whether it ended. `Nothing` is the query's `available = false` state and the next completed pass republishes at sequence 1. Transient session telemetry, never serialized (see `docs/persistence_state_inventory.md`). |
| `bloodDisposeQueue` | transient-handoff | `MainRender` (drains; `World.Render.BloodQuads.disposeQueuedBloodTextures`) | `WorldThread` (page-removal teardown, `World.Blood.Teardown`, `World.Thread.Command.Basic/Init`) | `Q.Queue (IORef BloodTextureHandles)` | `Q.newQueue` (`src/Engine/Core/Init.hs:152`) | None — empty/inert under headless | — |
| `floraCatalogRef` | boot-process | `WorldThread` (`Thread.ChunkLoading`, `Thread.Cursor`, `Thread.Command.Init`, `Command.Cursor.Plant/Chop`, `Command.Edit.Vegetation`, `World.Load.Stage` during staging, and `World.Render.Quads`'s `renderWorldQuads`, reached via `updateWorldTiles`), `LuaThread` (`API.Plant:100`, direct crop/species lookup) | `LuaThread` (content load) | `IORef FloraCatalog` | `emptyFloraCatalog` seed, populated from `data/*.yaml` via Lua content load (`src/Engine/Core/Init.hs:213`) | None | — |
| `materialRegistryRef` | session-replaced | `UnitThread` (`Unit.Thread.Movement`), `WorldThread`, `LuaThread` (`Engine.Scripting.Lua.API.World.Edit:86,211`, `Engine.Scripting.Lua.API.YamlTextures:350`), `MainRender` (`app/App/Dump.hs:152`, direct read while building the dump JSON) | `WorldThread` (populated per-world-init from `data/materials/*.yaml`, `src/World/Thread/Command/Init.hs:111-113`; also load publish, `src/World/Load/Publish.hs:117`), `LuaThread` (`Engine.Scripting.Lua.API.YamlTextures:99` registers each material's physical properties from the same loaded YAML content) | `IORef MaterialRegistry`, multi-writer | `emptyMaterialRegistry` at engine boot (`src/Engine/Core/Init.hs:214`); populated per-world-init from `data/materials/*.yaml` (`src/World/Thread/Command/Init.hs:100-113`) | None | YAML-driven after all (corrected from an earlier draft of this row, per review) — populated once per world init/load, not built into the binary. |
| `worldGenConfigRef` | boot-process | `WorldThread` (`Thread.Command.Init`), `LuaThread` (`API.World.GenConfig`) | `LuaThread` (`API.World.GenConfig`) | `IORef WorldGenConfig` | `loadWorldGenConfig "config/world_gen_default.yaml"` (`src/Engine/Core/Init.hs:229-230`) | None | Global worldgen tunables, distinct from a specific world's `wpsGenParams`. |
| `gameTimeRef` | session-replaced | `InputThread` (`Input.Thread.Char:39`, timestamping a keystroke), `CombatThread` (`Combat.Resolution:120`, timestamping a combat event), `WorldThread` (`Thread.Command.Save.WriteWorld:88`, timestamping a save-triggered event), `UnitThread` (`Unit.Thread.Movement:61`, fall/landing timing), `LuaThread` (`API.Core:146`'s `engine.getGameTime`, direct query) | `UnitThread` (`Unit.Thread.unitLoop`, once per tick when unpaused), `WorldThread` (load publish, `World.Load.Publish:110`) | `IORef Double`, monotonic while unpaused | `0` (`src/Engine/Core/Init.hs:235`) | None | Persisted exactly (`sdGameTime`). Read from essentially every thread for event/log timestamping — enumerated by concrete role rather than `AnyThread`, since that identifier is reserved for a field with an explicitly documented unrestricted-access contract (§2.2), and this field has no such contract, just a wide but ordinary set of readers. |
| `enginePausedRef` | session-replaced | `WorldThread`/`UnitThread`/`SimThread` (skip simulated-state advancement while true), `CombatThread` (`Combat.Thread:87` — sleeps the tick, keeping queued events queued, rather than resolving combat while paused), `MainRender` (keeps rendering/input regardless), `LuaThread` (`API.Core:96`'s `isPausedFn`, direct query) | `LuaThread` (`API.Core`'s `setPausedFn`, `engine.setPaused`), `WorldThread` (load publish `World.Load.Publish:111`, always loads paused) | `IORef Bool` | `False` (`src/Engine/Core/Init.hs:234`) | None | Persisted exactly; authoritative over any Lua-side copy. |
| `playerIntentGenRef` | session-replaced | `WorldThread` (`Thread.Command.Save.WriteWorld`'s `restoreAfterAutosave`, deciding whether an autosave may restore its own pre-request pause/time scale) | `LuaThread` (`Engine.Core.Capability.WorldSim.bumpPlayerIntent`, from `API.Core`'s `setPausedFn` on an APPLIED pause/resume and from `API.World.Clock`'s `worldSetTimeScaleFn` at request time — the only writer) | `MVar Word64`, monotonically increasing; the counter doubles as the MUTEX serializing a player transition against an autosave's compare-then-restore (`Engine.Core.Capability.WorldSim.withPlayerIntent` / `restoreIfPlayerIdle` / `withPlayerIntentHeld`), so a transition can neither slip in unseen after the comparison nor be overwritten by it | `0` (`src/Engine/Core/Init.hs:238`) | None | #913. Deliberately NOT bumped by the engine's own writes to `enginePausedRef`/`wsTimeScaleRef` (auto-pause-on-save, load publish, a `pause`-flagged notification) — it records PLAYER intent, so that an autosave can tell "the player re-paused during my write" from "I paused it myself". Runtime-only, never serialized (see `docs/persistence_state_inventory.md` §1). |
| `enginePauseGenRef` | session-replaced | `WorldThread` (`Thread.Command.Save.WriteWorld`'s `restoreAfterAutosave`, deciding whether an autosave may close a pause epoch a second source still wants open), `LuaThread` (`Engine.Scripting.Lua.API.Save:481`'s `acceptSaveRequest`, snapshotting it into the `AutosaveRequest`) | `WorldThread`/`LuaThread` (both through `World.Pause.imposePause` — a `pause`-flagged notification category at `Engine.PlayerEvent.Emit:215`, an `engine.loadSave` acceptance at `Engine.Scripting.Lua.API.Save:662`) | `IORef Word64`, monotonically increasing; never read or written outside the `playerIntentGenRef` critical section — every epoch transition takes that mutex (`World.Pause.withEpochLock`) and so do both sites above, which is what makes the restore decision linearizable against a pause landing beside it | `0` (`src/Engine/Core/Init.hs:245`) | None | #1730. A pause EPOCH records no owner and `imposePauseHeld` is a complete no-op once the flag is set, so this counter is the only evidence that an engine source other than the running save still wants the game paused. Deliberately NOT bumped by the save's own pause (`World.Pause.reassertSavePause`, `acceptSaveRequest`) — a save may not count itself as a reason to decline its own restore — nor by the player's `engine.setPaused`, which `playerIntentGenRef` above already records, so a declined restore can name the right reason. Runtime-only, never serialized (see `docs/persistence_state_inventory.md` §1). |
| `simQueue` | boot-process | `SimThread` (drains; `Sim.Thread`) | `WorldThread` (`Thread.ChunkLoading`, `Command.Basic`, `Command.Edit.Sync`, `Command.UI`, and `World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish), `MainRender` (`app/App/Dump.hs:85,135`, the dump driver's own `SimPause`/`SimFastSettleAll` enqueues after worker threads start) | `Q.Queue SimCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:148`) | None | — |
| `texPaletteRef` | session-replaced | `WorldThread` (`Thread.Command.Save.WriteWorld`), `LuaThread` (`API.Structure` — placement interns paths→ids) | `LuaThread`, `WorldThread` (load publish, `World.Load.Publish:118`) | `IORef TexPalette` | `emptyTexPalette` (`src/Engine/Core/Init.hs:220`) | None | Persisted exactly as `sdTexPalette`. |
| `texPaletteHandlesRef` | session-replaced | `WorldThread` (`Structure.Render`'s `renderStructureQuads`, reached via `updateWorldTiles`), `LuaThread` (`Engine.Scripting.Lua.API.Structure:239`'s `structureUnresolvedPaletteIdsFn` — `structure.unresolvedPaletteIds()`, a direct synchronous read) | `LuaThread` (lazy per-palette-path resolution), `WorldThread` (load publish, `World.Load.Publish:121`) | `IORef (HashMap Int TextureHandle)` | `HM.empty` (`src/Engine/Core/Init.hs:221`) | None | Runtime translation table, rebuilt each session — not itself persisted. |
| `structureWallCatalogRef` | boot-process | `WorldThread` (`Structure.Render`'s `renderStructureQuads`, reached via `updateWorldTiles` — picks the sprite and cap facemap a wall's edge occupies at the current facing) | `LuaThread` (`Engine.Scripting.Lua.API.Structure`'s `structureRegisterWallFamilyFn` — `structure.registerWallFamily`, one call per structure-pack variant as `scripts/structures.lua` reads `data/structure_packs/*.yaml`) | `IORef StructureWallCatalog`, accumulate-only | `emptyStructureWallCatalog` (`src/Engine/Core/Init.hs:227`) | None | #1712. Content art indexed by texture PATH, so — unlike `texPaletteRef`/`texPaletteHandlesRef` above — it is deliberately NOT session-replaced: a load publish reassigns palette ids but cannot invalidate a path, and re-registering a variant is a no-op. A registration is refused outright unless it covers all four edges and all sixteen cap facemaps, so a partial family can never be stored. |
| `structureArtCatalogRef` | boot-process | `LuaThread` (`Engine.Scripting.Lua.API.StructureArt`'s `structureResolvePieceArtFn` and `structurePackKindBuildableFn` — `structure.resolvePieceArt` / `structure.isPackKindBuildable`, the synchronous unplaced-piece queries; the construction render pass consumes the same catalogue once DTV-11 draws from it) | `LuaThread` (`Engine.Scripting.Lua.API.StructureArt`'s `structureRegisterPackArtFn` — `structure.registerPackArt`, one call per pack as `scripts/structures.lua` and `scripts/wire.lua` read `data/structure_packs/*.yaml`; and `Engine.Scripting.Lua.Thread.Dispatch`'s `LuaAssetFailed` handler, which fails the pack whose registered texture terminally failed to load) | `IORef StructureArtCatalog`, accumulate-only | `emptyStructureArtCatalog` (`src/Engine/Core/Init.hs:229`) | None | #1842. The unplaced-piece counterpart of `structureWallCatalogRef` above, and boot-process for the same reason: it is keyed by pack NAME and holds texture PATHS, neither of which a load publish's palette replacement can invalidate, so one registration per pack covers pieces designated later AND a session reloaded from a save. All or nothing per pack against an explicit declared-kind inventory, so half a pack can never be stored; a terminal texture-load failure makes the whole affected pack resolve nothing, recorded once per (pack, path) so the warning fires once per failed asset rather than per lookup. Registering or resolving interns nothing into `texPaletteRef` — knowing a piece's future art must not make a save carry palette entries for art nobody built (#1675). |

### `units-buildings-combat`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `unitManagerRef` | session-replaced | `UnitThread`, `CombatThread` (`Combat.Wounds.Tick`), `WorldThread` (also `Unit.Render`'s `renderUnitQuads`, reached via `updateWorldTiles` — not `MainRender`), `LuaThread` (`API.Units.Spawn:98`'s `unit.spawn`, direct def-existence check) | `UnitThread` (`Thread.Command.Lifecycle`, `Command.Pose`), `CombatThread` (`Combat.Resolution:323` — wound application, `Combat.Wounds.Tick:91` — periodic wound-tick outcomes, `Combat.Resolution.Wear:68` — weapon-wear mutation, all via `atomicModifyIORef'`), `WorldThread` (load publish, `World.Load.Publish:127`), `LuaThread` (`API.Units.Spawn:141`'s `unit.spawn`, direct unit-id allocation via `atomicModifyIORef'`) | `IORef UnitManager`, multi-writer | `emptyUnitManager` (`src/Engine/Core/Init.hs:215`) | None | — |
| `unitQueue` | boot-process | `UnitThread` (drains; `Unit.Thread.Command`) | `CombatThread` (`Combat.Wounds.Tick`, `Combat.Resolution.Events` — UnitKill/UnitCollapse), `WorldThread` (`Command.Basic`, `Command.Edit.Dig/Terrain`, and `World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish), `LuaThread` (`API.Units.Spawn`) | `Q.Queue UnitCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:216`) | The combat thread (a producer) is shut down **before** the unit thread (its consumer) — `app/App/Graphical.hs:72-76`: "Combat first: wound ticks enqueue UnitKill/UnitCollapse onto the unit queue, so the producer has to stop before the consumer... is torn down" | Deliberate shutdown ordering; see the identical rationale on `combatQueue`. |
| `utsRef` | session-replaced | `UnitThread` (`Unit.Thread`, `Thread.Command`, `Thread.Movement`, `Thread.Command.Spawn/Lifecycle/Pose`), `WorldThread` (`Thread.Command.Save.WriteWorld:101`, save capture), `LuaThread` (`API.Units.List:142`'s `unit.getInfo`, direct query) | `UnitThread`, `WorldThread` (load publish, `World.Load.Publish:128`) | `IORef UnitThreadState`, single-thread-owned by `UnitThread` outside a load publish/save capture (per the field's own doc comment, `src/Engine/Core/State.hs:222-227`) | `emptyUnitThreadState` (`src/Engine/Core/Init.hs:217`) | None | — |
| `statRNGRef` | boot-process | `UnitThread` (`Thread.Command.Spawn`, `Thread.Movement.Climb`), `CombatThread` (`Combat.Resolution`, `Combat.Wounds.Tick`), `WorldThread` (`Thread.Command.Edit.Dig` — dig-yield rolls), `LuaThread` (`API.Forage.Harvest`) | `UnitThread`/`CombatThread`/`WorldThread`/`LuaThread` (the same four roles as Readers; each roll both reads and advances the generator) | `IORef StdGen`, multi-writer, no cross-writer ordering guarantee beyond each individual roll's own atomicity | `Random.newStdGen` (`src/Engine/Core/Init.hs:218`) | None | Explicitly non-deterministic across runs by design — not save-seeded. |
| `buildingManagerRef` | session-replaced | `WorldThread` (`Render.CursorQuads`, `Thread.Power`, `Thread.ItemTemp`), `LuaThread` (`API.Power`, `API.Save`, `API.Buildings.Selection`'s `building.getSelected`), `UnitThread` (`Building.Thread.Command:31`'s `handleBuildingCommand`, drained on the unit thread) | `UnitThread` (via `Building.Thread.Command`, drained on the unit thread), `WorldThread` (load publish, `World.Load.Publish:126`), `LuaThread` (`API.Buildings.Selection:46,59`'s `building.select`/`building.deselect`, direct `atomicModifyIORef'`) | `IORef BuildingManager` | `emptyBuildingManager` (`src/Engine/Core/Init.hs:219`) | None | "Building" is a domain, not a thread — its commands are drained on `UnitThread` (`Unit.Thread` imports `Building.Thread.Command.processAllBuildingCommands`). |
| `buildingQueue` | boot-process | `UnitThread` (drains via `Building.Thread.Command.processAllBuildingCommands`) | `LuaThread` (`API.Power`, `API.Buildings.Spawn`), `WorldThread` (`World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish) | `Q.Queue BuildingCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:222`) | None | See `buildingManagerRef` note. |
| `buildingGhostRef` | session-replaced | `WorldThread` (`Building.Render`'s `renderGhostQuad`, reached via `updateWorldTiles` — placement-preview quad building, not a `MainRender` draw call) | `LuaThread` (the `build_tool` module via `API.Buildings.Spawn`), `WorldThread` (load publish, `World.Load.Publish:281`, always cleared) | `IORef (Maybe BuildingGhost)`, single-slot | `Nothing` (`src/Engine/Core/Init.hs:223`) | None | — |
| `combatQueue` | boot-process | `CombatThread` (drains at 60 Hz, `Combat.Thread.processAllCommands`) | `LuaThread` (`combat.attack` and future combat commands — per the field's own doc comment), `WorldThread` (`World.Load.Publish:210-220`'s `discardStaleQueues` on a load publish) | `Q.Queue Combat.Types.CombatCommand` | `Q.newQueue` (`src/Engine/Core/Init.hs:224`) | `CombatThread` (the consumer here, but the *producer* for `unitQueue`) is shut down first — see `unitQueue`'s Shutdown cell | — |
| `combatEventsRef` | session-replaced | `LuaThread` (`combat.drainEvents`, `API.Combat`) | `CombatThread` (`Combat.Wounds.Tick`, `Combat.Resolution.Events`), `LuaThread` (`API.Combat:95`'s `combat.emitDeath`, direct append via `atomicModifyIORef'`), `WorldThread` (load publish, `World.Load.Publish:290`, reset to empty) | `IORef (Seq CombatEvent)` | `Combat.Types.emptyEventQueue` (`src/Engine/Core/Init.hs:225`) | None | Runtime only, never persisted. |
| `injuryEventsRef` | session-replaced | `LuaThread` (`injury.drainEvents`) | `UnitThread` (`Unit.Thread.Movement` — falls), `LuaThread` (`API.Units.Combat`'s `unit.injure`, and `injury.emit`), `WorldThread` (load publish, `World.Load.Publish:291`) | `IORef (Seq CombatEvent)` (reused shape; victim in `target`) | `emptyEventQueue` (`src/Engine/Core/Init.hs:226`) | None | A streaming consumer (the log panel) drains this — don't manually drain it while that panel script is loaded, or you'll race it. |
| `thoughtEventsRef` | session-replaced | `LuaThread` (`thought.drainEvents`) | `LuaThread` (`scripts/thoughts.lua` via `thought.emit`), `WorldThread` (load publish, `World.Load.Publish:292`) | `IORef (Seq CombatEvent)` | `emptyEventQueue` (`src/Engine/Core/Init.hs:227`) | None | — |
| `actionOutcomeRef` | session-replaced | `LuaThread` (`debug.drainActionOutcomes`, the F4 playtest oracle) | `LuaThread` (`debug.recordOutcome`), `WorldThread` (`Thread.Command.Cursor.Common/Plant` — partial-drop counts, and load publish `World.Load.Publish:293`), `InputThread` (`Input.Thread.Keyboard:51`, `Input.Thread.Mouse:98` — `pushActionOutcome` recording key/click routing outcomes) | `IORef (Seq ActionOutcome)` | `emptyActionOutcomeQueue` (`src/Engine/Core/Init.hs:228`) | None | Never surfaced to the player. |
| `pathingConfigRef` | boot-process | `UnitThread` (movement tick re-reads every tick) | None (loaded once at boot; a future settings UI is the field's own stated future intent — `src/Engine/Core/State.hs:280-285`) | `IORef PathingConfig` | `loadPathingConfig logger "config/pathing.yaml"` (`src/Engine/Core/Init.hs:231-232`) | None | "No writers" is valid today per the field's own doc comment's stated rationale. |

### `content-registries`

Every field in this group shares one shape: allocated empty at
`Engine.Core.Init.initializeEngineWith`, then populated by the Lua
thread's boot-time content-load calls (`scripts/init.lua`'s
`X.loadYaml` sequence, run before gameplay begins) and in practice not
written again afterward. That once-at-boot pattern is the normal
startup shape, **not** an enforced invariant: the `engine.load*Yaml` /
`item.loadYaml` / `equipment.loadYaml` verbs stay publicly callable at
any time (`Engine.Scripting.Lua.API.Register.Engine`) and keep their
insert/replace-by-id behaviour, so nothing here may assume a one-shot
or frozen registry. Since #890 no consumer in this group reaches these
fields through an `EngineEnv` field at all — but since #1896 they no
longer all reach them through the *same record* (§7.6). For
`infectionManagerRef`, `locationDefsRef`, `lootTableRegistryRef` and
`tutorialRegistryRef`, reader and writer alike still take
`Engine.Core.Capability.ContentRegistries.ContentRegistriesCapability`.
For `itemManagerRef`, `equipmentClassManagerRef`, `substanceManagerRef`
and `recipeManagerRef`, that record is now the raw WRITER interface
— held by the one `X.loadYaml` module that legitimately writes each —
while every other non-§6.1 reader takes
`Engine.Core.Capability.ContentRegistriesView.ContentRegistriesViewCapability`,
which presents those four as `Engine.Core.ReadOnlyRef.ReadOnlyRef`s.
§6.1's permanent cohort is outside that boundary by D-4 and still reads
the raw handles (`Engine.Core.Init` allocates them;
`Engine.Scripting.Lua.API.Save` reads the item and recipe registries).
Both records alias the same live containers, so the Readers/Writers
cells below are unchanged: this split narrowed authority, not state.

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `itemManagerRef` | boot-process | `UnitThread` (spawn materializes `starting_inventory`), `LuaThread` (queries), `CombatThread` (`Combat.Resolution:125`'s `resolveAttack`, weapon/item def lookup), `WorldThread` (`World.Thread.Command.Edit.Dig:219`'s `spawnYieldItems`, dig-yield item def lookup) | `LuaThread` (`item.loadYaml`) | `IORef ItemManager` | `emptyItemManager` (`src/Engine/Core/Init.hs:242`), populated from the whole `data/items/` tree (recursive at any depth since #1232) | None | — |
| `equipmentClassManagerRef` | boot-process | `LuaThread` (queries; also backs the UI unit-info v2 equipment section's slot layout), `UnitThread` (`Unit.Thread.Command.Spawn:111`, starting-equipment materialization) | `LuaThread` (`equipment.loadYaml`) | `IORef EquipmentClassManager` | `emptyEquipmentClassManager` (`src/Engine/Core/Init.hs:243`) | None | — |
| `substanceManagerRef` | boot-process | `LuaThread` (queries), `CombatThread` (`Combat.Resolution:126`'s `resolveAttack`, weapon-material lookup), `UnitThread` (`Unit.Thread.Movement:140`, physical-property lookup) | `LuaThread` (`substance.loadYaml`) | `IORef SubstanceManager` | `emptySubstanceManager` (`src/Engine/Core/Init.hs:244`) | None | — |
| `infectionManagerRef` | boot-process | `CombatThread`/`UnitThread` (wound tick selects an infection), `LuaThread` (`API.Infection:90`'s `infection.get`, direct query) | `LuaThread` (`infection.loadYaml`) | `IORef InfectionManager` | `emptyInfectionManager` (`src/Engine/Core/Init.hs:245`) | None | — |
| `recipeManagerRef` | boot-process | `LuaThread` (`craft.*`/`repair.*` API — the craft-bill AI itself is Lua code, so it reads this on `LuaThread`, not a Haskell unit thread), `WorldThread` (`World.Thread.Power:55`'s `tickPowerNetworks`, per-tick craft-bill power-draw lookup) | `LuaThread` (`engine.loadRecipeYaml`) | `IORef RecipeManager` | `emptyRecipeManager` (`src/Engine/Core/Init.hs:246`) | None | — |
| `locationDefsRef` | boot-process | `LuaThread` (`locations.*`, `API.Power`, `API.WorldQuery.Location`, `API.Buildings.Spawn`), `WorldThread` (`World.Render.Zoom.Quads:85`, `World.Thread.Discovery:54`) | `LuaThread` (content load) | `IORef LocationRegistry` | `emptyLocationRegistry` (`src/Engine/Core/Init.hs:247`) | None | — |
| `lootTableRegistryRef` | boot-process | `LuaThread` (`loot.roll`, `loot.rollFor`) | `LuaThread` (content load) | `IORef LootTableRegistry` | `emptyLootTableRegistry` (`src/Engine/Core/Init.hs:248`) | None | `loot.rollFor` (#948) reads this registry alone — its draw is a pure function of the caller's world-seed/instance/entry/roll context, so unlike `loot.roll` it consumes no `statRNGRef`. |
| `tutorialRegistryRef` | boot-process | `LuaThread` (`engine.getTutorialTree`, `Engine.Scripting.Lua.API.Tutorial:88`) | `LuaThread` (content load, `engine.loadTutorialDir`) | `IORef TutorialRegistry` | `emptyTutorialRegistry` (`src/Engine/Core/Init.hs:249`) | None | The one active tutorial definition tree (#957). Unlike its sibling registries this holds at most ONE entry and its loader is a DIRECTORY verb, not a per-file one — `Engine.Asset.YamlTutorials.loadTutorialDir` enumerates `data/tutorials/` so "exactly one tree" is checkable — writing this field exactly once per call: the validated tree, or the empty state on any failure. |

### `ui-hud-events`

Both halves are migrated. The first four fields are the UI/focus/HUD
half #897 (E7a) moved: every production consumer reaches them through
`Engine.Core.Capability.Ui`'s `UiCapability` rather than a field
accessor, apart from the four §7.7 names as deliberate exceptions
(`Engine.Core.State`, `Engine.Core.Init`, the projection module
itself, and §6.1's `World.Load.Publish`). The last four are the
event/notification/popup half #898 (E7b) moved, reached through
`Engine.Core.Capability.Events`'s `EventsCapability` under the same
rule and with the same four kinds of exception. Each record's own
field documentation restates the reader/writer/lifecycle facts below.

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `uiManagerRef` | session-replaced | `MainRender` (`UI.Render`), `InputThread` (`Input.Thread.Keyboard:109`'s `validateFocus`, read on every keyboard dispatch), `LuaThread` (`API.UI.TextInput:48`'s `UI.getText`, `API.UI.Hierarchy:105`'s `UI.findElementAt`, and every other direct `UI.*` query) | `LuaThread` (every `UI.*` API module — `API.UI.Focus/Property/Tooltip/Hierarchy/Presentation`, `API.Config`), `WorldThread` (load publish `World.Load.Publish:286`), `InputThread` (`Input.Thread.Keyboard:109,250`, atomic focus/control-focus validation on every keyboard dispatch — round 4 review notes this races the Lua thread's own concurrent element mutations, hence the atomic transition rather than a separate read/write pair), `MainRender` (`UI.Tooltip.State:64`'s `updateTooltipState`, the per-frame tooltip tick called from `Engine.Loop.Frame`, `atomicModifyIORef'`; and since #2056 `UI.Render`'s own `renderUIPages`, which after rendering a snapshot writes back that SNAPSHOT's armed presentation token — the one field on this record the render thread mutates, and deliberately an `atomicModifyIORef'` at the render site rather than through a helper, so §5's writing-module map records it) | `IORef UIPageManager`, multi-writer via `atomicModifyIORef'` | `emptyUIPageManager` (`src/Engine/Core/Init.hs:196`) | None | Entire UI tree is rebuilt by Lua on load, per `docs/persistence_state_inventory.md`. |
| `focusManagerRef` | session-replaced | `InputThread` (read-only — `Thread.Keyboard:106`/`Thread.Char:119`, one plain `readIORef` per dispatch feeding `getInputMode`, which routes the keystroke to the debug console or lets it fall through), `LuaThread` (`API.ShellFocus:57`'s `getFocusIdFn`) | `LuaThread` (`API.ShellFocus:31,44,51` — `engine.registerFocusable`/`requestFocus`/`releaseFocus`, each an `atomicModifyIORef'`), `WorldThread` (load publish, `src/World/Load/Publish.hs:285`) | `IORef FocusManager` | `createFocusManager` (`src/Engine/Core/Init.hs:204`) | None | Shell/console TEXT focus (`UI.ShellFocus`), NOT either game-UI focus system. The `InputThread` never writes it: #745's Tab/Shift+Tab control-focus traversal transitions `uiManagerRef`, not this ref. |
| `hudActivePageRef` | session-replaced | `WorldThread` (`Thread.Cursor` — HUD refresh-on-active-world-change, #129) | `WorldThread` (also load publish, `World.Load.Publish:283`, resynced from `wmVisible`) | `IORef (Maybe WorldPageId)` | `Nothing` (`src/Engine/Core/Init.hs:198`) | None | — |
| `textBuffersRef` | boot-process | `LuaThread` (only; `API.Text`, direct queries) | `MainRender` (only; `Engine.Scripting.Lua.Message.Scene`, dispatched via `processLuaMessages` — never the Lua thread itself) | `IORef (Map ObjectId Text)` | `Map.empty` (`src/Engine/Core/Init.hs:202`) | None | The SCENE-OBJECT text cache `engine.getText` answers from, keyed by `ObjectId` — not editable-widget state, which is `uiManagerRef`'s `UI.TextBuffer` and a different mechanism entirely. Reset `None` is correct because entries are retired with their scene nodes rather than at a session boundary (#1961): `Engine.Scripting.Lua.Message.Scene` is the map's only writer and only remover, and it maintains one invariant — an entry exists exactly when the scene graph holds a node at that id whose `nodeText` is set, and equals it. The condition is a node BEARING TEXT, not a `TextObject`: `modifySceneNode` does not check `nodeType`, so `engine.setText` against a sprite's id sets and caches that sprite's text, which is preserved behaviour. All four transitions go through its `cacheSceneText`/`forgetSceneText` pair: a text spawn, a `setText` whose `modifySceneNode` actually landed, a destroy, and — easily missed — a SPRITE spawned over a node that bore text, which `addNode`'s `Map.insert` replaces. |
| `eventStoreRef` | session-replaced | `LuaThread` (`API.PlayerEvent`'s `readEventLog` — `engine.getEventLog()`, the event-log panel's query — and `readEventLogProgress` — `engine.getEventLogProgress()`, the #1714 rows-plus-high-water read, one `readTVarIO` so the pair cannot straddle a commit) | `WorldThread` (`World.Thread.Discovery`'s `emitEventFullOnPage`, `World.Thread.Command.Save.WriteWorld`'s `emitEvent`, and load publish `World.Load.Publish.resetTransientState`, rows reset to empty, counter kept), `LuaThread` (`API.PlayerEvent`'s `emitEvent`/`emitEventAt`/`emitEventFull` — `engine.emitEvent`/`emitEventAt`/`emitEventForUnit` — and `API.Save`'s save/load-lifecycle emits) | `TVar EventStore`, multi-writer STM, ~1000-entry ring of `StoredEvent` rows plus the `esNextSequence` mutation counter (#1714) — one ref so a row's sequence is stamped in the same atomic write that commits the row, and so the counter outlives the load-publish row reset (`Engine.PlayerEvent.clearEventStoreRows`) | `newTVarIO emptyEventStore` (`src/Engine/Core/Init.hs:269`) | None | Explicitly session-only, never serialized. No live `Unit.Thread`/`Combat.Thread` call site emits a player event today — verified by grepping every real `emitEvent*` call site. `Engine.PlayerEvent.Emit`'s module comment, `EngineEnv`'s own field doc and `Engine.Core.Init`'s seeding comment all used to read as if unit-thread emitters existed; #898 corrected all three to state the STM primitive's any-thread safety separately from the world-and-Lua-thread call sites that actually exist. |
| `notificationCfgRef` | boot-process | `AnyThread` (the `emitEvent` read path) | `LuaThread` (Phase 2 settings tab toggles, per the field's own doc comment) | `IORef NotificationCfg` | `loadNotificationCfg` merges `data/notification_categories.yaml` + `config/notifications.local.yaml` (`src/Engine/Core/Init.hs:257-260`) | None | — |
| `notificationOrder` | boot-process | `LuaThread` (settings tab render order) | None (captured once at boot from the YAML registry order — categories can't be added/removed at runtime, per the field's own doc comment) | Plain `![Text]`, no `IORef` | `loadNotificationCfg`'s second return value (`src/Engine/Core/Init.hs:257-260`) | None | Immutable-boot-configuration carve-out, same shape as `engineConfig`. |
| `popupQueueRef` | session-replaced | None (write-only today — no `readTVar`/`readTVarIO` on this ref exists anywhere in the codebase; live popup delivery goes through a separate `LuaShowPopup` message sent via `luaQueue` at the same emit call site, `Engine.PlayerEvent.Emit:134-137`, not by draining this TVar back out. `EngineEnv`'s own field doc used to claim the Lua side drains this via the `LuaShowPopup` broadcast; #898 corrected it to state the write-only reality. This TVar exists for inspection/debug querying and as a Phase 2 stable source for the notifications panel, per the same comment.) | `WorldThread`/`LuaThread` (same `emitEvent` producers as `eventStoreRef`, filtered to popup-enabled categories, `Engine.PlayerEvent.Emit:134`), `WorldThread` (load publish, `World.Load.Publish:296`, reset to empty) | `TVar (Seq PlayerEvent)` | `newTVarIO Seq.empty` (`src/Engine/Core/Init.hs:262`) | None | — |

### `save-load-coordination`

| Field | Lifecycle | Readers | Writers | Sync | Init | Shutdown | Notes |
|---|---|---|---|---|---|---|---|
| `loadStatusRef` | boot-process | `WorldThread` (`Thread.Time`, `Thread.Command.Save`), `MainRender` (`Engine.Scripting.Lua.Message.discardLuaMessagesForActiveLoad`, called by the render consumers while `captureLocked`), `LuaThread` (`API.Core`, `Thread.Dispatch`, `API.Save`, `API.LoadGate`) | `WorldThread`/`LuaThread` (the same two roles as Readers) | `LoadStatusRef` (opaque, internally synchronized — see `Engine.Load.Status`) | `newLoadStatusRef` (`src/Engine/Core/Init.hs:199`) | None | Diagnostic only, never serialized. Since #1181 the opaque handle also carries the test-only `StageGate` (armed/read by `LuaThread` through `API.LoadGate`'s `debug.*` verbs, parked on by `WorldThread` in `Thread.Command.Save`) — coordination for a probe, inert unless armed, and never serialized either. |
| `pendingLoadRef` | transient-handoff | `WorldThread`, `LuaThread` (`Thread.Dispatch`) | `WorldThread` (written when a staged-load transaction finishes staging; read and cleared when the matching publish command runs, per the field's own doc comment), `LuaThread` (`Thread.Dispatch:438,460`, cleared on a load-publish failure path before the prepared-but-never-applied Lua load is aborted) | `IORef (Maybe (Int, StagedSession))`, single-slot, keyed by request id defensively | `Nothing` (`src/Engine/Core/Init.hs:200`) | None | Only one load is ever in flight, enforced by `loadStatusRef`. |
| `saveBarrierRef` | boot-process | `UnitThread`, `CombatThread`, `WorldThread` (`Thread.Command.Save`, `Command.Save.WriteWorld`), `MainRender` (`Engine.Loop.Mode`), `LuaThread` (`Engine.Scripting.Lua.Thread:227`'s `captureLocked` check, and `Thread.Dispatch:413-455`'s `handleLoadStaged`, which itself DRIVES a whole load-publish transaction via `beginSave`/`acknowledgeSave`/`waitForOwners`/`reachSnapshot`/`failSave`), `InputThread` (`Input.Thread:90`'s `captureLocked` check in the input loop's own per-tick gate), `SimThread` (`Sim.Thread:98`'s `captureLocked` check) | `UnitThread`, `CombatThread`, `WorldThread`, `MainRender`, `LuaThread` (`Thread.Dispatch:413-455` — the Lua thread is the transaction driver for a load publish, not merely one of the acknowledging owners), `InputThread` (`Input.Thread:97`'s `acknowledgeCurrent (saveBarrierRef env) SaveInput`, once per input-loop tick), `SimThread` (`Sim.Thread:130,144`'s `acknowledgeCurrent (saveBarrierRef env) SaveSimulation`, once per sim-loop tick) | `SaveBarrier` (opaque, internally-synchronized coordination record — see `Engine.Save.Barrier`) | `newSaveBarrier` (`src/Engine/Core/Init.hs:236`) | None | Every state-owner thread that must acknowledge a save boundary reads/writes this; diagnostic/coordination only, never serialized. |
| `lastSaveTimeRef` | boot-process | `LuaThread` (only) | `LuaThread` (only; `API.Save.saveWorldFn`, clamps each save strictly past this for monotonic ordering, #98) | `IORef UTCTime` | POSIX epoch (`src/Engine/Core/Init.hs:241`) | None | — |
| `nextItemInstanceIdRef` | session-replaced | `AnyThread` (via `freshItemInstanceId`, `Engine.Core.State` — item rolls/spawns) | `AnyThread` (via `freshItemInstanceId`), `WorldThread` (load publish, `World.Load.Publish:125`) | `IORef Word64`, monotonic allocator, thread-safe atomic bump (`atomicModifyIORef'`) | `1` (`src/Engine/Core/Init.hs:165`) | None | Persisted exactly; a load ASSIGNS the saved value (`World.Load.Publish:125`), never `max`-ing against the replaced session's (#67, reshaped by #763). Post-load ids still can't collide: `World.Save.Snapshot`'s `itemAllocatorErrors` refuses to capture a session holding an id at or above its own allocator. |

## 6. Full-`EngineEnv` compatibility boundary

**Live since issue #889 (E1, landed); recounted by #890 (E2), #891
(E3), #893 (E5a), #892 (E4), #895 (E6a), #897 (E7a), #896 (E6b), #898
(E7b), #894 (E5b), and closed out by #899 (E8).**
205 files under `src/`/`app/` import `Engine.Core.State` in some form.
Of those, 24 have genuine unrestricted field-level access:
`Engine.Core.State.hs` itself (which defines `EngineEnv` and therefore
imports nothing) plus **exactly the 23 permanent importers of §6.1** —
since #899 there are no others. Those 23 import it either as an
explicit `EngineEnv(..)` (in any combination
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
grep -rl "import Engine.Core.State" src app | wc -l                    # 205
# then, per file, whether the import clause is bare or explicitly
# names EngineEnv(..) vs. a strictly narrower list (EngineEnv with no
# (..), a single field accessor, or EngineState instead) — see the
# script logic below; 23 have full access, 182 do not. The bullets
# below enumerate the narrowing COHORTS issue by issue — which modules
# each capability migration moved off unrestricted access and what
# they still import narrowly — not a partition of today's narrow set:
# a module that never held unrestricted access in the first place was
# never in a §6.2 cohort and so appears in none of them. The live
# totals above are the authoritative figures; the audit reports them
# on every run.
#   13 × `Engine.Scripting.Lua.API.Register.*` (`Engine.Scripting.Lua.API`
#        itself plus its 12 `Register.*` submodules; all import the bare
#        `EngineEnv` TYPE with no constructor access, and two of them
#        — `Register.Craft`/`Register.Item` — additionally name a single
#        field accessor each (`unitManagerRef` / `statRNGRef`) to hand a
#        #890-narrowed callee the one field outside its capability)
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
#   1  × `Engine.Core.Capability.ContentRegistries` (new by #890 — the
#        `content-registries` projection module; bare `EngineEnv` type
#        plus its eight field accessors, never `EngineEnv(..)`)
#   5  × the #890-narrowed content-registry API modules that still need
#        an opaque `EngineEnv` to pass to a helper that takes one:
#        `API.Items.Defs`, `API.Equipment.Class`, `API.Locations`
#        (render-gpu-asset texture helpers), `API.Repair` (the station
#        gate `Craft.Execute.validateStation`, which composes FOUR
#        already-landed capability records and so keeps its `EngineEnv`
#        token past #896 — see that module's header),
#        `API.WorldQuery.Location` (page lookup). The other four
#        of #890's nine (`API.Craft.Recipe`, `API.Infection`,
#        `API.Substance`, `API.LootTables`) import `Engine.Core.State`
#        not at all and so are outside this accounting entirely.
#   2  × `Engine.Core.Capability.Render` / `.RenderView` (new by #891 —
#        the two `render-gpu-asset` projection modules of §3.1; each
#        imports the bare `EngineEnv` type plus only its own field
#        accessors, never `EngineEnv(..)`)
#   40 × the #891-narrowed `render-gpu-asset` modules that still import
#        `Engine.Core.State` narrowly — for `EngineState(..)`/
#        `GraphicsState(..)` (the CPS state σ, not `EngineEnv`), for an
#        opaque `EngineEnv` type to hand to a not-yet-narrowed helper,
#        and/or for individually named accessors (`luaQueue`,
#        `loggerRef`, ...) that a landed capability deliberately left
#        on a pre-existing narrow reader — §7.5's explicit-narrow rule:
#        a module needing exactly one handle takes exactly that handle,
#        not a record it will use one field of. The other 5 of #891's 45
#        (`Vulkan.Command.Text`, `Vulkan.Texture.Bindless`,
#        `Vulkan.Texture.DefaultFaceMap`, `Scene.Batch.Text` and — since
#        #897 took its last accessor, `uiManagerRef` — `UI.Render`) now
#        import `Engine.Core.State` not at all and are outside this
#        accounting.
#   1  × `Engine.Core.Capability.WorldSim` (new by #893 — the world/sim
#        half of the `world-sim-render-handoff` projection; bare
#        `EngineEnv` type plus its nine field accessors, never
#        `EngineEnv(..)`)
#   1  × `Engine.Core.Capability.RenderHandoff` (new by #894 — the
#        coupled render-handoff half of the same projection; bare
#        `EngineEnv` type plus its own field accessors, never
#        `EngineEnv(..)`. One record, not a §3.1-style full/view pair:
#        none of its fields is private to a single thread the way
#        `engineStateRef` is to `MainRender` — every one is a deliberate
#        cross-thread handoff)
#   4  × the #894-narrowed `world-sim-render-handoff` modules — the E5b
#        remainder `Engine.Scripting.Lua.API.Structure`, `World.Thread`,
#        `World.Thread.Command.Basic` and `World.Thread.Command.Init`.
#        All four still import `Engine.Core.State` narrowly: every one
#        needs at least the bare `EngineEnv` type (they still take an
#        `EngineEnv` and project from it), `API.Structure` additionally
#        for the canonical `activeWorldPage`/`activeWorldState` helpers,
#        and `World.Thread` for the bare type alone since #899 (E8)
#        moved its one `saveBarrierRef` accessor onto
#        `Engine.Core.Capability.SaveLoad`)
#   28 × the #893-narrowed `world-sim-render-handoff` modules that still
#        import `Engine.Core.State` narrowly — for the same three
#        reasons #891's 41 do: the canonical `activeWorldStateFrom`/
#        `activeWorldPageFrom`/`resolveActiveWorld` helpers, an opaque
#        `EngineEnv` type to hand to a not-yet-narrowed helper (e.g.
#        `World.Thread.Command` → `Command.Basic`/`Command.Init`, both
#        narrowed by #894 but still taking an `EngineEnv` they project
#        from; the
#        designation `Cursor.*` handlers → `Cursor.Common`'s F4
#        outcome recorders), and/or individually named accessors
#        (`unitManagerRef`, `unitQueue`, `luaQueue`, `loadStatusRef`,
#        `actionOutcomeRef`, `hudActivePageRef`, `saveBarrierRef`) that
#        a landed capability (#892, #894, #895, #897, #899)
#        deliberately left on a pre-existing narrow reader. The other 22 of #893's
#        50 now import `Engine.Core.State` not at all and are outside
#        this accounting.
#   2  × `Engine.Core.Capability.Input` / `.InputView` (new by #892 —
#        the two `input-lua-transport` projection modules of §7.3, in
#        the same LuaThread-only/worker-safe shape §3.1 defines for
#        render; each imports the bare `EngineEnv` type plus only its
#        own field accessors, never `EngineEnv(..)`)
#   1  × `Engine.Core.Capability.UnitCombat` (new by #895 — the
#        units-and-combat half of the `units-buildings-combat`
#        projection; bare `EngineEnv` type plus its ten field
#        accessors, never `EngineEnv(..)`)
#   35 × the #895-narrowed `units-buildings-combat` modules, all of
#        which still import `Engine.Core.State` narrowly — every one
#        needs at least the bare `EngineEnv` type (they still take an
#        `EngineEnv` and project from it), and several additionally
#        name single accessors a landed capability deliberately left
#        narrow (`loggerRef`, `lifecycleRef`, `saveBarrierRef`) or the
#        canonical `activeWorldStateFrom`/`activeWorldPageFrom`/
#        `freshItemInstanceId` helpers. `World.Thread.Command.Edit.Dig`
#        is the one that names NO field accessor at all: §7.5's
#        explicit-narrow rule moved its `statRNGRef`/`unitQueue` reads
#        onto ordinary function parameters supplied by
#        `World.Thread.Command` (which named the two accessors itself,
#        staying narrow)
#   1  × `Engine.Core.Capability.Building` (new by #896 — the buildings
#        half of the `units-buildings-combat` projection; bare
#        `EngineEnv` type plus its three field accessors, never
#        `EngineEnv(..)`)
#   13 × the #896-narrowed `units-buildings-combat` modules that still
#        import `Engine.Core.State` narrowly, in the same three shapes
#        #895's 35 use: the bare `EngineEnv` type they still take and
#        project from, plus — where needed — `loggerRef`/`lifecycleRef`/
#        `saveBarrierRef` or an `activeWorldPageFrom`/
#        `freshItemInstanceId` helper. The 14th, `Building.Thread.Command`,
#        now imports `Engine.Core.State` not at all and is outside this
#        accounting: §7.5's explicit-narrow rule moved its whole
#        parameter list onto `BuildingCapability` + the logger ref +
#        `WorldSimCapability`, supplied by its only caller `Unit.Thread`
#        (which names `loggerRef` itself, staying narrow)
#   11 × the #892-narrowed `input-lua-transport` modules, all of which
#        still import `Engine.Core.State` narrowly: `Engine.Input.Callback`
#        for the `EngineLifecycle(..)` type alone (it holds no `EngineEnv`
#        at all), and the other ten for an opaque `EngineEnv` type to hand
#        to a helper that still takes one (`Engine.Input.State`,
#        `Engine.Input.Thread.Mouse`) and/or individually named accessors
#        (`actionOutcomeRef`, kept a narrow value by #895's own rule,
#        and `saveBarrierRef`, kept narrow by the same rule after #899)
#        — see §7.3's cross-capability surface. #897 took
#        `focusManagerRef`/`uiManagerRef` off that surface.
#   1  × `Engine.Core.Capability.Ui` (new by #897 — the UI/focus/HUD
#        half of the `ui-hud-events` projection; bare `EngineEnv` type
#        plus its four field accessors, never `EngineEnv(..)`)
#   1  × `Engine.Core.Capability.Events` (new by #898 — the
#        event/notification/popup half of the `ui-hud-events`
#        projection; bare `EngineEnv` type plus its four field
#        accessors, never `EngineEnv(..)`)
#   2  × the #898-narrowed `ui-hud-events` modules,
#        `Engine.PlayerEvent.Emit` and
#        `Engine.Scripting.Lua.API.PlayerEvent`: both still take an
#        opaque `EngineEnv` (their callers are unchanged) and project
#        `EventsCapability` from it, alongside the narrower
#        `CoreCapability` (`loggerRef`), `WorldSimCapability`
#        (`gameTimeRef`/`enginePausedRef`) and `InputViewCapability`
#        (`luaQueue`) records the emit path already needed
#   11 × the #897-narrowed `ui-hud-events` modules, all of which still
#        import `Engine.Core.State` narrowly: the seven
#        `Engine.Scripting.Lua.API.UI.*` modules and
#        `Engine.Scripting.Lua.API.ShellFocus` for the bare `EngineEnv`
#        type alone; `Engine.Scripting.Lua.Message.Scene` and
#        `UI.Tooltip.State` for `EngineState(..)`/`TimingState(..)` (the
#        CPS state σ, not `EngineEnv`); and `Engine.Input.Thread.Mouse`
#        for the type plus the one `actionOutcomeRef` accessor §7.5's
#        explicit-narrow rule keeps it on
#   1  × `Engine.Core.Capability.SaveLoad` (new by #899 — the
#        `save-load-coordination` projection module; bare `EngineEnv`
#        type plus its five field accessors, never `EngineEnv(..)`)
#   1  × the #899-narrowed `core-init` module that still imports
#        `Engine.Core.State` narrowly: `Engine.Graphics.Vulkan.Command.Record`,
#        for `EngineState(..)`/`GraphicsState(..)` (the CPS state σ, not
#        `EngineEnv`) — its one `engineConfig` read now goes through
#        `Engine.Core.Capability.Core`. The other of #899's two,
#        `Engine.Scripting.Lua.API.Log`, imports `Engine.Core.State` not
#        at all (its four `log*Fn` entry points take `CoreCapability`)
#        and is outside this accounting
```

None of the 182 narrowly-importing files is a consumer this section
needs to classify: an opaque `EngineEnv` type import, one or more
individually named field accessors, or an unrelated `EngineState`
import — none grants the unrestricted access this section is about.
Adding back `Engine.Core.State.hs` itself (the definer, which imports
nothing and so is outside the 205/23/182 accounting entirely) gives
the **24** total full-access modules this section classifies — which,
since #899, is exactly §6.1's permanent set and nothing else.

This section names the *end state*, and since #899 it **is** the end
state rather than a target: what legitimately constructs, carries, or
inspects the **complete** `EngineEnv` now that the epic's capability
split has landed. It was written deliberately narrow — narrow enough
to become the literal allowlist for #537's final unrestricted-access
audit (per requirement 6) — and that is exactly what it now is. §6.2,
which held the modules that had full access only because nothing
narrower existed yet, is empty; §6.4 governs any future addition here.

### 6.1 Permanent (production)

Requirement 6 asks each exception to state which of three reasons it
is: permanent initialization/orchestration infrastructure, a temporary
compatibility boundary, or the engine-monad carrier itself. Every row
below is one of the first or third — nothing in this permanent section
is the second, by definition of the section.

| Module(s) | Category | Reason |
|---|---|---|
| `Engine.Core.State` | Permanent initialization infrastructure | Defines `EngineEnv`/`EngineState` — the type itself; every other entry in this table depends on this one existing. |
| `Engine.Core.Monad` | The engine-monad carrier itself | `EngineM σ α`'s Reader environment *is* `EngineEnv`, hard-wired in its `MonadReader` instance rather than carried as a type parameter (issue #931 removed the vestigial one) — this is requirement 6's third category, named explicitly. |
| `Engine.Core.Init` | Permanent initialization infrastructure | Constructs the single `EngineEnv` value; by construction, must name every field once. |
| `Engine.Core.Defaults` | Permanent initialization infrastructure | Provides the default values (`defaultEngineConfig`, `defaultEngineState`, `defaultWindowConfig`, ...) that `Engine.Core.Init` assembles into the single `EngineEnv`/`EngineState` — it never reads a *live* `EngineEnv`'s fields (no `asks`/`gets`/`readIORef env ...` anywhere in the module), it only builds the record values those fields start out holding; the same construction-time role as `Engine.Core.Init` one level down. |
| `Engine.Loop`, `Engine.Loop.Frame`, `Engine.Loop.Shutdown`, `Engine.Loop.Camera`, `Engine.Loop.Timing`, `Engine.Loop.Resource` | Permanent orchestration infrastructure | The main loop's job each frame is to coordinate render output with several other capabilities' queues/state (input barrier tokens, world quads, screenshot requests, ...), and `shutdownEngine` performs the one coordinated cross-capability teardown boundary described in §2.4/§3. `Engine.Loop.Headless` left this row in issue #1022: its whole body is now one `Engine.Loop.Mode.LoopMode` value, and `Engine.Loop.Mode` — the shared loop driver that reads `lifecycleRef`/`inputQueue`/`saveBarrierRef` for all three modes — names exactly those three fields in a narrow import, so neither module needs unrestricted access. |
| `app/App/Graphical.hs`, `app/App/Offscreen.hs`, `app/App/Preview.hs`, `app/App/Headless.hs`, `app/App/Dump.hs` | Permanent orchestration infrastructure | Top-level boot/wire-up: each necessarily constructs the engine, starts every thread the profile needs, and wires them together — inherently whole-environment by job description. |
| `Engine.Scripting.Lua.Thread`, `Engine.Scripting.Lua.Thread.Dispatch`, `Engine.Scripting.Lua.Thread.Console` | Permanent orchestration infrastructure | The Lua thread's own dispatch plumbing registers *every* Lua API module against the full environment (`registerLuaAPI`) — this wiring point is inherently cross-capability, even though each individual `Engine.Scripting.Lua.API.*`/`Message.*` module it wires (§6.2) is not. `Thread.Console` is the debug-console command handler living in the same package — TCP debug-server builtins and single-line Lua command execution, both reached from the same core Lua-thread loop as `Dispatch`. |
| `Engine.Scripting.Lua.Message` | Permanent orchestration infrastructure | `processLuaMessages` is the per-frame, `MainRender`-side counterpart to `Thread.Dispatch`: it drains `luaToEngineQueue` and routes every category of Lua-originated message to its per-domain handler (`Message.Video`, `Message.Texture`, `Message.WorldTexture`, `Message.Scene`, §6.2) — inherently cross-capability dispatch infrastructure, not a consumer of any one capability's fields itself. |
| `World.Thread.Command.Save`, `World.Thread.Command.Save.WriteWorld`, `World.Load.Stage`, `World.Load.Publish`, `Engine.Scripting.Lua.API.Save` | Permanent orchestration infrastructure | A save/load transaction is inherently a whole-session boundary: these five modules are the exact, verified set that actually `import Engine.Core.State (EngineEnv(..))` on the save/load path (`grep -rn 'import Engine.Core.State' src/World/Load src/World/Thread/Command/Save* src/Engine/Scripting/Lua/API/Save.hs`) — they must capture or replace every capability's state atomically in one coordinated step (see the persistence contract's snapshot/publish design). Narrowing this to per-capability records would just reconstruct an env-shaped aggregate one level down — this is a permanent exception, not a temporary one awaiting migration. Everything ELSE under `World.Save.*` (`Snapshot`, `Types`, `Component*`, `Envelope*`, `Serialize`, `Storage`, `Integrity`, `Reference`, `Compat*`) is pure data/codec code that never touches `EngineEnv` at all (`World.Save.Snapshot`'s own doc comment states this explicitly) and is correctly outside this list entirely — not a temporary compatibility boundary either, since it was never given full access in the first place. `Engine.Save.Barrier`/`Engine.Load.Status` are the same: opaque coordination types referenced FROM `EngineEnv` (`saveBarrierRef`/`loadStatusRef`), not consumers of it — neither imports `EngineEnv`. |

That's 24 permanent modules (23 importers + `Engine.Core.State` itself,
which imports nothing) — and since issue #899 (E8) that is the WHOLE
full-access set: 24 − 24 = 0 temporary modules remain (§6.2).

Since issue #889, this permanent allowlist and §6.2's temporary
accounting are enforced live: `tools/engine_env_capability_audit.py`'s
checked-in `PERMANENT_IMPORTERS`/`TEMPORARY_CEILING` constants mirror
this document's §6.1/§6.2 exactly, and the audit fails if the
live-scanned production importer set ever disagrees with either — in
BOTH directions, so a stale allowlist entry left behind by a migration
fails too.

Since issue #899 the audit ALSO parses this section's own table
(`audit_permanent_boundary`, reading the **first column only** — the
Reason cells above deliberately cite other module names, e.g.
`World.Save.Snapshot`, `Engine.Save.Barrier`, `Engine.Loop.Mode`, which
are explicitly NOT allowlist entries) and requires the documented set
to equal `PERMANENT_DEFINER` + `PERMANENT_IMPORTERS` exactly, with a
real, non-placeholder Category AND Reason on every row. That closes the
last gap the ratchet alone left open: growing a live importer together
with the Python constant used to pass with this document never
recording why the module is a genuine whole-session boundary.
Documentation alone does not admit a permanent importer, and neither
does a constant change with no written justification — both must move
together, and §6.4(d) governs when that is even permitted.

### 6.2 Temporary compatibility boundary (production)

**Empty since issue #899 (E8).** Every row below is a historical
record of what this section assigned and which issue cleared it; no
production module holds temporary full-`EngineEnv` access any more.

The assignment method each landed migration applied — kept here because
it is what a future reader needs to understand the roadmap entries in
§7, and because §6.4's ninth-capability procedure refers to it — was
applied uniformly and mechanically rather than by directory-name
guessing. Every name in every cell is a literal, complete Haskell
module name: **no path-prefix globs, no "and similar" language, and no
catch-all row**.

The first numbered item below used to repeat §1's field total. It
states no number now, and the audit holds it that way: it locates this
section's first numbered item, requires it to still be that sentence,
and rejects any number in it. The other items count freely — only the
one that carried the second copy of the total is governed.

1. For each module, scan its source for every occurrence of one of the
   `EngineEnv` field names from §5 (`asks`/`gets`/`readIORef env
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
| `core-init` | *(none — migrated by #899 (E8): `Engine.Graphics.Vulkan.Command.Record` reads its one `engineConfig` hit through `Engine.Core.Capability.Core` and keeps a narrow `EngineState(..)`/`GraphicsState(..)` import for the CPS state σ (not `EngineEnv`), and `Engine.Scripting.Lua.API.Log`'s four `log*Fn` entry points take `CoreCapability` directly and so no longer import `Engine.Core.State` at all. Neither holds unrestricted `EngineEnv` access any more, and no module remains whose dominant field usage is this capability)* | §7.1 |
| `render-gpu-asset` | *(none — migrated by #891 (E3): all 45 former entries now reach their render fields through `Engine.Core.Capability.Render` (the `MainRender`-only 21-field record) or `Engine.Core.Capability.RenderView` (the worker-safe view that never carries `engineStateRef` — 13 fields when #891 landed, 14 since #893 added `fpsRef`), per §3.1; none of them holds unrestricted `EngineEnv` access any more, and no module remains whose dominant field usage is this capability)* | §7.2 |
| `input-lua-transport` | *(none — migrated by #892 (E4): all 11 former entries now reach their input fields through `Engine.Core.Capability.Input` (the `LuaThread`-only eight-field record) or `Engine.Core.Capability.InputView` (the worker-safe five-field view that carries neither `inputBarrierNextRef` nor `currentKeyDownRef`), per the §3.1 rule §7.3 applies here; `Engine.Input.Callback` needed no record at all — its API already took the two live handles explicitly, so it merely narrowed its bare import to the `EngineLifecycle` type. None of the 11 holds unrestricted `EngineEnv` access any more, and no module remains whose dominant field usage is this capability)* | §7.3 |
| `world-sim-render-handoff` | *(none — migrated in two halves: #893 (E5a) moved 50 entries onto `Engine.Core.Capability.WorldSim` for the nine world/sim fields, and #894 (E5b) moved the remaining four — `Engine.Scripting.Lua.API.Structure`, `World.Thread`, `World.Thread.Command.Basic`, `World.Thread.Command.Init`, named individually per #893's requirement 2 so nothing was silently dropped between the a/b pair — onto `Engine.Core.Capability.RenderHandoff` for the seven coupled render-handoff fields, composed with the `WorldSim`/`RenderView`/`ContentRegistries`/`InputView`/`UnitCombat`/`Building`/`Core` records their other reads already had. None of the 54 holds unrestricted `EngineEnv` access any more, and no module remains whose dominant field usage is this capability)* | §7.4 |
| `units-buildings-combat` | *(none — migrated by #895 (E6a) and #896 (E6b) together: E6a's 35 entries reach the ten unit/combat fields through `Engine.Core.Capability.UnitCombat`, and E6b's remaining 14 reach the three building fields through `Engine.Core.Capability.Building` — `Building.Thread.Command` by taking that record plus the logger ref and `WorldSimCapability` as explicit parameters and so dropping its `Engine.Core.State` import entirely, the other 13 by projecting from the `EngineEnv` they still take. None of the 49 holds unrestricted `EngineEnv` access any more, and no module remains whose dominant field usage is this capability)* | §7.5 |
| `content-registries` | *(none — migrated by #890 (E2): all nine former entries now reach the eight registries (seven when #890 landed; `tutorialRegistryRef` joined them with #957) through `Engine.Core.Capability.ContentRegistries`, none of them holds unrestricted `EngineEnv` access any more, and no module remains whose dominant field usage is this capability)* | §7.6 |
| `ui-hud-events` | *(none — migrated in two halves: #897 (E7a) moved 11 UI-dominant entries onto `Engine.Core.Capability.Ui`, and #898 (E7b) moved the two event-dominant ones (`Engine.PlayerEvent.Emit`, `Engine.Scripting.Lua.API.PlayerEvent`) onto `Engine.Core.Capability.Events`. None of the 13 holds unrestricted `EngineEnv` access any more, and no module remains whose dominant field usage is this capability)* | §7.7 |
| `save-load-coordination` | *(none — and none ever: every module whose dominant field usage is save/load coordination is a permanent orchestration exception listed in §6.1. #899 (E8) added `Engine.Core.Capability.SaveLoad` for this capability's NON-permanent touchpoints — the per-tick `captureLocked`/`acknowledgeCurrent` sites — and narrowed `World.Thread` onto it. `Engine.Scripting.Lua.API.Core` was previously assigned here for its one `loadStatusRef` read, but its dominant usage — `enginePausedRef`/`gameTimeRef`, both read/written more often in the same file — is `world-sim-render-handoff`, so it is listed there instead)* | §7.8 |

Row counts (0 + 0 + 0 + 0 + 0 + 0 + 0 + 0 = 0) match
24 − 24 exactly — **the temporary boundary is empty**, which is the
epic's end state (#899, E8). All eight rows are retained deliberately:
`tools/engine_env_capability_audit.py`'s `TEMPORARY_CEILING` keeps the
same eight keys mapped to empty sets, and its doc/ceiling cross-check
iterates the union of both key sets — dropping a row here (or a key
there) would silently stop cross-checking that capability, and would
make the end-state self-test's "exactly the eight `CAPABILITIES` keys
with empty module sets" assertion vacuous.

There is consequently **no legal path left for a production module to
take unrestricted `EngineEnv` access.** "Add the field now, narrow it
later" no longer exists: a new full-access module fails the ratchet
even if this section is also edited to document it, and the ceiling is
shrink-only, so it cannot be grown back. §6.4 documents what to do
instead.

### 6.3 Test-only exceptions

Test fixtures are listed separately, per requirement 6, since a test's
job is routinely to construct and inspect a **complete**, working
`EngineEnv` — that is not the same "hasn't been migrated yet" gap as
production code, and narrowing test fixture access is not a §7 goal.

| Module(s) | Reason |
|---|---|
| `test-headless/Test/Headless/Harness.hs` and every `Test.Headless.*` module built on it | A headless hspec fixture's entire purpose is booting one working engine environment (`initializeEngineHeadless`/`initializeEngineHeadlessWith`) and sharing or inspecting it across many test cases (see `Test.Headless.UI.ResponsiveGameplay`'s `withSharedFixture` for the canonical example) — broad, whole-environment access here is the intended design, not a compatibility gap. |
| `tools/*_probe.py` (real-engine turnkey harnesses) | Drive a real booted engine over the debug console; not Haskell code and not subject to an import-level allowlist at all, but listed here for completeness since they routinely exercise every capability of a running engine. |

### 6.4 Post-flip procedure: what to do when you need new state

Since #899 (E8) the boundary is machine-enforced and unforgiving:
§6.2's temporary ceiling is empty, so "add the field now, narrow it
later" is not a thing that can be done. This section is what to do
instead. Work through it in order — (a) resolves the large majority of
cases, and the later steps get progressively harder on purpose.

#### (a) Most new state does not belong on `EngineEnv` at all

Start here, and expect to stop here. `EngineEnv` is the **one shared
record reachable from any thread**; that is a cost, not a service.
State that one subsystem owns belongs with that subsystem:

- **World/gameplay state → `WorldState`** (reached through the active
  page, `wsGenParamsRef` and friends). It is already per-page,
  already persisted through the save components, and already
  session-replaced correctly on load.
- **Manager-owned state → the manager** (`WorldManager`,
  `UnitManager`, `BuildingManager`, `ItemManager`, the content
  registries). `EngineEnv` already carries the pointer; adding a
  sibling field beside that pointer is almost always the wrong shape.
- **Render/timing/scene mechanics → `EngineState`** (§3 —
  main-render-thread-private, and no worker thread may reach it).
- **Purely local state → a function parameter or a local `IORef`.**

**Worked example (#911, placed-location instance identity).** A placed
location needed a stable, persisted, per-page identity: an id
allocated at placement time, its definition id, anchor, resolved
absolute bounds, display name, one-time
content-spawn flag, and lifecycle (#911 also stored a discovery margin;
#1230 removed it when reveal became sight-based). That is a substantial pile of new
mutable, persisted, cross-tick state — and it needed **zero** new
`EngineEnv` fields. The records live in `WorldGenParams` inside
`WorldState`, reached through `wsGenParamsRef`, alongside the page's
other generation state. Being page-scoped is what makes the ids
survive save/load and chunk eviction correctly in the first place; an
`EngineEnv` field would have had to reinvent per-page scoping by hand.
Most of what the expedition arc needs has the same shape.

Only continue past (a) if the state genuinely must be shared across
threads *and* has no owning subsystem to live in.

#### (b) Adding a field to an existing capability

Two audits both gate this, and both run in CI and `make ci`. The
complete set of requirements:

**`tools/persistence_inventory_audit.py`** —
[`docs/persistence_state_inventory.md`](persistence_state_inventory.md):

1. A row under the `### EngineEnv` heading (that exact heading — the
   audit's `ROOT_RECORDS` anchor keys off it).
2. Exactly one of the five classifications in that audit's
   `VALID_CLASSIFICATIONS`: **`Persist exactly`**, **`Persist as
   identity/reference`**, **`Rebuild`**, **`Reset to default`**, or
   **`Exclude`**. Not a phrase of your own.

**`tools/engine_env_capability_audit.py`** — this document's §5:

3. A capability-inventory row, under the `### <capability>` heading
   for the capability the field belongs to.
4. That heading must be one of §2.1's closed eight-identifier set. If
   none of them fits, you are in case (c), not this one.
5. Readers and Writers cells in the **strict grammar** the audit
   enforces: each top-level comma-separated segment is one or more
   backtick-quoted, slash-joined role names optionally followed by a
   single trailing `(...)` parenthetical, or the whole cell is
   `` `None` `` immediately followed by a non-empty parenthetical
   justification. All explanatory prose goes *inside* that
   parenthetical — never bare between the role and the paren, never
   after it closes.
6. Every role named must be one of §2.2's roles.
7. A §2.3 lifecycle category: `boot-process`, `boot-shutdown`,
   `session-replaced`, or `transient-handoff`.
8. Non-empty Sync, Init, Shutdown, and Notes cells. A bare em-dash
   (`—`) is accepted in Notes only — it is a legitimate "nothing
   further to add" there, and nowhere else.
9. Grounding evidence: at least one backtick-quoted `.hs`/`.lua`
   source-location citation somewhere in the row.
10. §1's marked field-total block amended: it states the new live
    count and names the record's real first and last field. The audit
    re-derives all three from the live declaration, so a correct §5 row
    with a stale §1 block still fails (issue #1669).
11. A `CAPABILITY_WRITER_MODULES` entry for the field, mapping it to
    the modules that directly write it — `frozenset()` when nothing
    does, which is the common case for a field only
    `Engine.Core.Init` seeds. The map's keys are checked against the
    live record in both directions, so a new field with no entry
    fails on its own. See §6.5.
12. Whatever else the **live** audit rules require — read the tool, not
    just this list. The §3/§7.3 boundary checks, for instance, will
    reject a new field that a thread privately owns being placed on a
    worker-visible record.

**Adding it to the capability RECORD is a separate decision.** Do that
only when the capability genuinely owns the field *and* a real
consumer needs the narrowed view. E1's no-unused-record rule applies
field-by-field: a record carries what a real consumer already needs,
not everything the capability could plausibly own. If the field is
private to one thread, §3.1 requires a strictly narrower worker-safe
view rather than a comment on a wide record.

#### (c) When none of the eight capabilities fit

A **ninth capability** is permitted only when all three hold:

1. No existing §2.1 capability fits the field's ownership.
2. The state is legitimately shared through `EngineEnv` — i.e. (a)
   has been genuinely worked through and rejected.
3. **The repository maintainer explicitly approves it.**

The bar is intentionally high. Ordinary convenience, legacy coupling,
"it doesn't fit anywhere else", or a wish for a generic
`misc`/`shared`/`other` bucket does **not** qualify — the audit
rejects those identifiers by name precisely so that a field with no
home has to be argued for rather than filed away.

An approved ninth capability requires these changes **in lockstep**:

- §2.1's identifier table, and the record/view table beside it.
- `CAPABILITIES` in `tools/engine_env_capability_audit.py`.
- `tools/test_engine_env_capability_audit.py` — including the
  end-state case, whose "exactly the eight `CAPABILITIES` keys"
  assertion is written against the live constant and will need to
  move with it.
- `TEMPORARY_CEILING` gains the new key mapped to an **empty**
  frozenset, and §6.2 gains its matching `*(none — ...)*` row; the
  ceiling stays empty, and the new capability does not get a
  grandfathering window.
- The persistence-inventory row and the capability-inventory row for
  every field it claims (case (b) above, for each).
- A new `Engine.Core.Capability.<Name>` module following E1's
  convention, listed in `synarchy.cabal`'s explicit library module
  list, with a projection-aliasing hspec module registered in
  `test-headless/Spec.hs` and the test suite's `other-modules`.

And it must have a **real narrowed consumer** in the same change. No
unused record is permitted — that rule is what has kept every one of
the fourteen existing record/view types earning its place.

#### (d) Adding a new module to §6.1

A production module may join the permanent allowlist only
**exceptionally**, with **explicit repository-maintainer approval**,
and only when it is a genuine whole-session orchestration boundary:
one that observes or coordinates the session by design and cannot be
narrowed without destroying that role. The existing 24 are what that
looks like — the env's definer and constructor, the engine-monad
carrier, the boot wire-up per profile, the main loop, the Lua dispatch
plumbing, and the save/load transaction (§7.8).

Not sufficient, individually or together: convenience; an incomplete
migration you intend to finish later; a helper you depend on that
happens to take an opaque `EngineEnv`; or the module being large. If
narrowing is merely inconvenient, narrow it — that is what the other
ten children of #537 each did.

An approved addition requires, in lockstep:

- A §6.1 row with a real Category **and** a real Reason — the audit's
  `audit_permanent_boundary` rejects an empty or placeholder cell, so
  a name-only row will not pass.
- The matching `PERMANENT_IMPORTERS` (or `PERMANENT_DEFINER`) update
  in `tools/engine_env_capability_audit.py`. Neither change admits a
  module on its own: the constants are checked against the live source
  in both directions *and* against this section's documented set.
- Self-test coverage in `tools/test_engine_env_capability_audit.py`,
  including the end-state case's live-set equality assertion.

The same policy governs any other future permanent exception —
including a new §3/§7.3-style thread-private field owner.

### 6.5 Writing-module authority and the pass-on residue (#1892, CMA-1)

`tools/engine_env_capability_audit.py` carries a checked-in
`CAPABILITY_WRITER_MODULES` map: for every live `EngineEnv` field, the
production modules that **directly mutate** it. It is maintained the
same way `RENDER_MAIN_ONLY_MODULES` is, and checked the same two ways —
a write from a module the field's set does not list fails, and a mapped
module that no longer writes the field fails just as loudly, so the map
can never decay into a mere upper bound. Its **keys** are checked
against the live record in both directions too.

Design authority:
[`capability_mutation_authority_design.md`](capability_mutation_authority_design.md)
(CMA-1; decisions D-2, D-2a, D-4, D-5). Read it before widening any
rule below.

**What it proves, and what it does not.** §5 declares thread *roles*; a
source scan yields *modules*; nothing in this repository maps between
them, and at module granularity the mapping is not even well-defined —
`World.Render.BloodQuads` is deliberately dual-domain (§3.1) and writes
`textureSystemRef` from a `MainRender` function while its quad-building
path runs on `WorldThread`, so the role is a property of the *function*.
The map therefore verifies a weaker and different property: **the set of
modules writing this field is what we last declared**, not "§5's role
claim is true". A *new* writer can no longer appear unnoticed; an
already-wrong role cell stays wrong until someone reads it (D-2a).

**Scope: direct `IORef` mutation only.** A write is detected only where
an `IORef` mutation primitive is applied directly to a known accessor
application — `writeIORef (accessor handle) …`, whether through the
field's own `EngineEnv` accessor or through any capability-record
accessor projecting it, prefix or backticked-infix, and whether or not
the expression spans several lines. Accessor *and* primitive are each
recognized qualified (`State.fieldOne`, `Ref.writeIORef`) as readily as
bare. Mutation through a queue, a `TVar`, an `MVar`, an
opaque internally-synchronized handle (`SaveBarrier`, `LoadStatusRef`),
or a helper that was *given* the `IORef` is invisible to a textual scan
and is deliberately out of scope: resolving it means interprocedural
dataflow over Haskell source, written in Python, which this arc rejects
outright (D-5).

**§6.1's cohort is exempt (D-4).** The permanent full-access modules
hold whole-session orchestration authority by job description, and this
boundary does not constrain them: their writes are neither reported as
violations nor admitted into the map. The cohort this map governs is
the capability-narrowed consumers.

**The pass-on residue.** What the scan cannot attribute, it reports.
Every capability-accessor use that is *not* a direct inline handle read
or write — a handle given to a helper, stored into a context record
that mixes several capabilities, handed to a queue/`TVar`/`MVar`
primitive, or composed point-free — is listed on every run as one line
per source occurrence, with its path, line, accessor and canonical
field. It is **non-blocking, counted but never resolved to an
originating module**, and it is printed *before* every blocking check
so a failure elsewhere never costs the measurement. That count is the
evidence CMA-2's pilot and CMA-3's verdict turn on: a small residue
means a textual gate is nearly sufficient, a large one argues for a
mechanism that travels with the handle.

**Two rules keep a textual match honest**, and neither models Haskell's
binding forms:

1. **Import scope, under the exact spelling used** — for the accessor
   *and* for the primitive. A name is only `Data.IORef`'s `writeIORef`
   if that one reaches the module under the spelling written; a
   module-local homonym, or an unrelated module's qualified one, is a
   different function whose argument mutates no `IORef`. Each primitive
   is resolved against **its own** defining module
   (`ACCESS_PRIMITIVE_MODULES`), so `Engine.Core.ReadOnlyRef`'s
   `readReadOnlyRef` (#1896) goes through the identical rule rather than
   a looser second path. It is a READ, and only a read: a
   `ReadOnlyRef` field has no write primitive to recognize, which is the
   whole point of the type. Recognizing it matters for the residue, not
   the map — without it every consumer migrated onto a wrapped view
   would be recounted as a pass-on, and the measurement would report
   the opposite of what the migration did. For the
   accessor the same rule applies: it must
   actually reach that module — imported by name, through a bare
   import, through the wildcard of the record it belongs to
   (`EngineEnv(..)`, and not some other type's `(..)` in the same
   list), or defined by the module itself. Each `import` declaration is kept separate rather than
   merged into one answer per module, because the things that decide
   this are per-declaration language rules — `qualified` removes the
   *unqualified* spelling from scope entirely, an `as` alias
   *replaces* the module name as the qualifier rather than joining it,
   a `hiding` clause takes its own names back out, and one module is
   legitimately imported twice on different terms.
2. **Applied position.** The accessor must head an argument of a
   mutation primitive *and* that argument must be an application —
   `prim (accessor handle) …`. This is a type argument, not a
   heuristic: every accessor projects out of a handle
   (`EngineEnv -> IORef a`, or `EngineEnv -> ReadOnlyRef a` for a
   wrapped view field — §2.1's abstract-wrapper extension, which
   `ALIAS_PRESERVING_WRAPPERS` lets the scan see through so a wrapped
   field still canonicalizes onto its `EngineEnv` field), so it can
   never itself be the handle the primitive takes, and a *bare*
   identifier there always denotes some local binding sharing its
   name. Naming an accessor in a comment, in
   Haddock, or in an import list is not using it either — commentary
   and import declarations are stripped before the scan, and that
   stripping is literal-aware: a `--` inside a string is text, block
   comments nest, and a dash run continuing into a symbol is an
   operator. (It has to be. `Engine.Scripting.Lua.Thread.Dispatch`
   logs a literal `" -- "`, and truncating there also removed the
   string's closing quote, which hid three real mutation sites further
   down the file.)

The primitive must also be in HEAD position. `withLogging writeIORef
(fieldOne env) 1` hands it to `withLogging`, and reading what follows
as its own arguments would invent a write and hide the accessor's
residue entry behind a phantom inline use. What can apply to it is an
identifier or a closing bracket, but a newline ends no application and
layout ends a statement with no token at all, so three things decide
it: a keyword applies to nothing; on the same line an identifier or
bracket is applying; across lines, a continuation is indented past the
line that opened the expression while a sibling statement is not. An
operator SECTION applied prefix (`($) writeIORef …`) is a fourth case
and an unreadable one — `($)` applies its arguments and `(.)` composes
them, with opposite consequences — so it joins the blocking bucket
rather than being modelled operator by operator.

The primitive is held to the same scope test as the accessor: a name is only
`Data.IORef`'s `writeIORef` if that one reaches the module under the
spelling written. A top-level homonym needs no special case — defining
one beside an unqualified `import Data.IORef` is an ambiguous
occurrence at every use site, so it does not compile; `hiding
(writeIORef)` and a local definition is the form that does, and the
import test already decides it.

**Deliberately no lexical scope analysis, and a closed form list
instead.** Those two rules separate every case in this tree; the one
near-miss, `src/Unit/Thread/Movement.hs`'s `utsRef` parameter, is
excluded because that module imports `Engine.Core.State` for the
`EngineEnv` *type* alone. The residue of the rules — a module that
locally binds a name matching an accessor **and applies it to a
handle** — goes on `SHADOW_EXEMPTIONS`, a checked-in
`{(module, field): reason}` list that is **empty**. A locally shadowed
*primitive* is the mirror of that case and goes to the same place: an
exemption suppresses its module/field pair whatever name was shadowed
to produce it. Each entry
suppresses exactly its own pair, must name a live field, must carry a
real reason, and fails once it stops suppressing anything.

The alternative was modelling `let`/`where`/lambda/`<-`/`case` scopes.
Measured against this tree that analysis changed the answer at **none**
of the mutation sites, while its incompleteness was findable
indefinitely — the forms are many, and such an analysis is only ever as
complete as the last one someone thought of.

**Every mutation site must classify, or the gate stops.** That is what
makes the form list above CLOSED rather than aspirational. Each
occurrence of a mutation primitive lands in exactly one bucket: an
applied, in-scope, non-exempt accessor (attributed); a nameable head
that is not this boundary's business — a local `IORef`, an unapplied
accessor, one the module cannot reach, or the primitive used as a value
rather than applied; or a site where an argument is plainly being
formed and the scan cannot read it, which **fails the audit** naming
file and line — `OverloadedRecordDot` access (`env.fieldOne`) is one
such shape, rejected rather than misread as an application of `env`. There are no sites of the third kind today. A
spelling outside the list therefore stops the gate instead of silently
dropping a write — and the fix is to extend the scan and this list
together, never to leave the site unread.

**Maintaining it.** Adding a writing-module entry is a deliberate act,
not a maintenance edit: it declares that a capability-narrowed module now
holds write authority over that field. Removing one is what a narrowing
migration owes the gate. Either direction, the audit names the exact
module and field, so the edit is mechanical once the decision is made —
run `python3 tools/engine_env_capability_audit.py` and read the
violation. Do not silence a violation by widening a rule above; a
surprising write is the finding, not the noise.

## 7. Migration roadmap

For each capability group: which fields it owns (§5's table for that
group, not repeated here), which module families consumed it (§6.2's
corresponding row), its dependencies on other capability groups,
whether it could migrate independently, and what actually landed.

**All eight are now LANDED** (#889–#899). This section is therefore a
record of the completed roadmap rather than a plan: it is where to
look for *why* a capability was split the way it was, which is the
context §6.4 assumes when it sends you to a record or a boundary rule.
The one genuinely open item it still names is out of the epic's own
scope by §1: field-by-field inventories of `EngineState`,
`WorldManager`, `UnitManager`, and `BuildingManager` internals.

### 7.1 `core-init` — **LANDED (#889, E1; completed by #899, E8)**

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
  values before this issue and needed no change. #889 left §6.2's
  `core-init` row naming two modules it did not migrate:
  `Engine.Graphics.Vulkan.Command.Record` and
  `Engine.Scripting.Lua.API.Log`.
- **Completed by #899 (E8).** Those last two are now narrowed, and the
  row is empty. Both turned out to be smaller than #889's caution
  anticipated:
  - `Engine.Graphics.Vulkan.Command.Record`'s only `EngineEnv` use was
    one `engineConfig` read (the preview boot profile's clear colour);
    everything else it touches is the CPS state σ. It now projects
    `CoreCapability` from `ask` for that one read and keeps a narrow
    `Engine.Core.State (EngineState(..), GraphicsState(..))` import for
    the σ — which is not `EngineEnv` access at all (§6's own
    classification counts an `EngineState`-only import as narrow).
  - `Engine.Scripting.Lua.API.Log`'s four `log*Fn` entry points read
    nothing but `loggerRef`, so they now take `CoreCapability`
    directly. Its sole caller,
    `Engine.Scripting.Lua.API.Register.Engine`, already had a
    `toCoreCapability` projection in scope for #890's registry
    loaders and simply passes it. The module no longer imports
    `Engine.Core.State` at all — the same end state
    `Building.Thread.Command` reached under §7.5's explicit-narrow
    rule.
- **Dependencies:** None — every other capability group depends on
  this one being available first (the logger and lifecycle flag are
  read from every thread), so this is necessarily the first migration,
  not something that can be deferred.
- **Independent migration:** Yes, and it went first.
- **Follow-up scope:** None. #889's caveat — that
  `loggerRef`/`lifecycleRef` are read so universally most call sites
  would need a broader carrier for a while yet — held for the ~440
  `logInfoM`-style call sites, which still reach the logger through
  `MonadReader EngineEnv` wrappers, exactly as designed. That is the
  §6.1 carrier boundary working, not a remaining migration: what E1
  set out to narrow was each module's own *field access*, and no
  production module outside §6.1 has unrestricted access to these four
  fields any more.

### 7.2 `render-gpu-asset` — **LANDED (#891, E3)**

- **Dependencies:** `core-init` (logger, lifecycle).
- **Independent migration:** Yes — and done. The complication this
  entry anticipated was real and is what shaped the result:
  `textureSystemRef`/`textureSizeRef` are read by `WorldThread` (via
  `Unit.Render`/`World.Render.*`'s quad-building pass; see §3's note on
  why they moved to `EngineEnv` in the first place) even though writes
  stay confined to `MainRender` (the `World.Render.BloodQuads`
  upload/dispose functions run via `processLuaMessages`, not the world
  thread's own quad-building path — see their §5 rows). So this
  capability could not be exposed as one record the world thread
  imports: that record also carries `engineStateRef`, and §3 forbids
  worker code an interface that can construct or inspect it. **The
  resolution is §3.1's two-interface split** — a `MainRender`-only
  record plus a strictly narrower worker-safe view — not a weakening of
  §3.
- **What landed:** two projection modules, both total, one-way, and
  over the identical live containers `EngineEnv` already carries
  (never a copy, never derived from each other), following §7.1/#889's
  convention:
  - `Engine.Core.Capability.Render` exports `RenderCapability` over
    exactly the 21 fields of §5's `render-gpu-asset` table plus
    `toRenderCapability`. It carries `rcEngineStateRef` and is
    importable only by the 14 `MainRender` modules the audit's
    `RENDER_MAIN_ONLY_MODULES` pins: `Engine.Graphics.Font.Load`/
    `.Upload`, `Engine.Graphics.Vulkan.Command.Sprite`/`.Text`,
    `Vulkan.Init`, `Vulkan.Recreate`, `Vulkan.Texture.Bindless`,
    `Vulkan.Texture.DefaultFaceMap`, `Engine.Graphics.Window.GLFW`,
    `Engine.Scene.Batch.Text`, `Engine.Scripting.Lua.Message.Texture`/
    `.Video`/`.WorldTexture`, and `UI.Render`.
  - `Engine.Core.Capability.RenderView` exports `RenderViewCapability`
    over the worker-visible fields (`videoConfigRef`,
    `windowSizeRef`, `windowPosRef`, `framebufferSizeRef`,
    `framebufferMinimizeGenRef`, `pixelSnapRef`,
    `textureFilterRef`, `assetPoolRef`, `textureNameRegistryRef`,
    `fontCacheRef`, `textureSystemRef`, `textureSizeRef`, `cameraRef`,
    `screenshotRequestQueue` — 13 when #891 landed, plus `fpsRef` added
    by #893, see the next bullet, and `framebufferMinimizeGenRef` added
    by #1693 for the `InputThread` writer that publishes the minimize
    edge) plus `toRenderViewCapability`. It never
    contains `engineStateRef`. Its consumers are the `WorldThread`
    quad/hit-test family (`World.Render`, `.BloodQuads`,
    `.CursorQuads`, `.GroundItemQuads`, `.Quads`, `.SpoilQuads`,
    `.Zoom.Quads`, `Unit.HitTest`, `Building.HitTest`,
    `Building.Render`, `Structure.Render`), the `LuaThread` API modules
    (`API.Camera`, `.Config`, `.Graphics`, `.Input`, `.Items.Render`,
    `.Screenshot`, `.Text`, `.UI.Placement`, `.WorldQuery.Pick`,
    `.YamlTextures`), and the dual-domain `Engine.Asset.Manager` and
    `World.Render.BloodQuads`.
  - Eight low-level modules turned out to dereference no `EngineEnv`
    field at all — they only ever touched the CPS state σ
    (`EngineState`/`GraphicsState`): `Engine.Graphics.Font.Draw`,
    `Vulkan.Framebuffer`, `.MSAA`, `.Offscreen`, `.Pipeline`,
    `.Pipeline.Bindless`, `.Swapchain`, `.Sync`, plus
    `Engine.Scene.Render`. Their unrestricted import was pure excess
    reach and is simply gone.
  - Four of the 45 (`Vulkan.Command.Text`, `Vulkan.Texture.Bindless`,
    `Vulkan.Texture.DefaultFaceMap`, `Scene.Batch.Text`) no longer
    import `Engine.Core.State` at all.
- **Fields deliberately left out of the worker view:** `engineStateRef`
  (§3.1, never), and the `MainRender`-only `windowStateRef`,
  `brightnessRef`, `samplerCacheRef`, `defaultFaceMapSlotRef`,
  `uiCameraRef` (no non-`MainRender` reader in §5). `fpsRef` and
  `nextObjectIdRef` do have `LuaThread` readers, but neither belonged to
  a module this issue migrated (`API.Core` was expected to be §7.4's;
  `nextObjectIdRef`'s only consumer is the permanently-full-access
  `Engine.Scripting.Lua.Thread`), so per #889's "no unused capability
  records ahead of need" — applied field-by-field — a later migration
  adds them when it has a real consumer. **`fpsRef` was added by that
  rule in #893 (E5a)**, which narrowed `API.Core` — `engine.getFPS` is
  the `LuaThread` reader §5's `fpsRef` row names — making the view a
  14-field record. `nextObjectIdRef` still has no consumer to add it
  for.
- **Enforcement:** §3.1's three checks in
  `tools/engine_env_capability_audit.py`, plus the projection-aliasing
  coverage in `Test.Headless.Capability.Render` (all 21 full-record
  fields including `screenshotRequestQueue`, and all 14 view fields,
  each asserted to be the same live container as `EngineEnv`'s).

### 7.3 `input-lua-transport` — **LANDED (#892, E4)**

- **Dependencies:** `core-init`.
- **Outcome:** §6.2's `input-lua-transport` row is now empty. All 11
  assigned modules dropped unrestricted `EngineEnv` access; the
  capability is exposed as **two interfaces**, exactly the §3.1 shape
  `render-gpu-asset` uses:

  | Interface | Fields | Who may hold it |
  |---|---|---|
  | `Engine.Core.Capability.Input` (`InputCapability`) | all 8 of §5's `input-lua-transport` fields | `LuaThread` modules only — `Engine.Scripting.Lua.API.InputInject`, `Engine.Scripting.Lua.API.Keybinds` |
  | `Engine.Core.Capability.InputView` (`InputViewCapability`) | `inputQueue`, `inputBarrierRef`, `inputStateRef`, `keyBindingsRef`, `luaQueue` | everything else — the input thread's dispatch chain and the world thread's Lua-message producers |

  The split is not stylistic. §5 marks two fields `LuaThread`-private:
  `inputBarrierNextRef` (the synthetic-injection barrier-token
  **allocator**) and `currentKeyDownRef` (the transient `onKeyDown`
  current-key handoff). Because E1 exports every capability record as
  `Capability(..)` — constructor *and* accessors — a single eight-field
  record visible to the input thread would hand it a way to allocate
  barrier tokens and to inspect or clobber the Lua thread's in-flight
  key, whatever the Haddock said. The view therefore **contains neither
  field at all**: the input thread gets `inputBarrierRef` (the
  processed **watermark** it publishes) and nothing more. Both records
  are independent one-way projections of `EngineEnv` — the view is
  never derived from the full record.

  `luaToEngineQueue` is deliberately absent from the view (E1's "no
  unused capability records ahead of need", applied field-by-field):
  its only production consumers are the permanently full-access §6.1
  orchestration modules plus two API modules that already import the
  accessor narrowly, so no module this migration narrows needs it.

  `tools/engine_env_capability_audit.py`'s `audit_input_boundary`
  enforces all three parts on every run, both directions, the same way
  `audit_render_boundary` does for §3.1: only an
  `INPUT_LUA_ONLY_MODULES` module may import the full record; only an
  `INPUT_LUA_ONLY_FIELD_OWNERS` module may *name* either private field
  (or its `ic`-prefixed accessor); and the view must not so much as
  mention them. `Test.Headless.Capability.Input` covers projection
  aliasing for both records — including that the two same-typed
  `TVar Int` barrier fields resolve to their correct, distinct live
  containers, and that repeated projection mints nothing fresh.
- **Cross-capability surface (all of it).** These reads/writes cross
  out of `input-lua-transport` and are legitimate; per E1 a narrowed
  module takes its own capability record **plus strictly narrower
  values**, so each rides either an existing capability record or an
  individually named accessor import. None of them pulls a future
  migration into this one's scope:

  | Field(s) | Owning capability | How the input modules reach it |
  |---|---|---|
  | `loggerRef`, `lifecycleRef`, `engineConfig`, `inputThreadActiveRef` | `core-init` | `Engine.Core.Capability.Core` (#889) |
  | `windowSizeRef`, `framebufferSizeRef`, `framebufferMinimizeGenRef` | `render-gpu-asset` | `Engine.Core.Capability.RenderView` (#891; the minimize-generation write added by #1693) — the worker-safe view |
  | `gameTimeRef` | `world-sim-render-handoff` | `Engine.Core.Capability.WorldSim` (#893) |
  | `focusManagerRef`, `uiManagerRef` | `ui-hud-events` | `Engine.Core.Capability.Ui` (#897) — named accessor imports while E4 was the only one landed, swapped for the record when E7a landed |
  | `actionOutcomeRef` | `units-buildings-combat` | named accessor import — #895's `Engine.Core.Capability.UnitCombat` now exists, but §7.5's explicit-narrow rule deliberately keeps these input-thread readers off a record they would otherwise reach unit rosters and combat queues through |
  | `saveBarrierRef` | `save-load-coordination` | named accessor import — §7.8's own row is empty; its modules are permanent §6.1 exceptions |

  On the focus pair specifically: `Engine.Input.Thread.Char` and
  `.Keyboard` **read** `focusManagerRef` (they never write it) and
  perform `atomicModifyIORef'` validate/transition steps on
  `uiManagerRef` for keyboard control-focus navigation (#745). Both
  keep the identical refs and the identical atomicity after this
  migration — the one-atomic-transition discipline §5's `uiManagerRef`
  row records is unchanged, and so is every #745 behavior. This is why
  E4 did not have to wait for #897, and why #897 did not have to
  revisit #745 when it landed: it inherited two named accessor imports,
  not a behavior. #897 (E7a) has since swapped both — in `.Char`,
  `.Keyboard`, `.Scroll` and `.Mouse.Activation` — for
  `Engine.Core.Capability.Ui`'s `uicFocusManagerRef`/`uicUiManagerRef`,
  which alias the identical live containers, so the refs and the
  atomicity are once again unchanged (§7.7).
- **Not this capability's:** `Engine.Input.Thread.Mouse` is assigned to
  `ui-hud-events` (§6.2) — its dominant usage is pointer routing
  through the UI manager — so #897 (E7a) migrated it, and
  `Engine.Input.Thread.Dispatch` still hands it an opaque `EngineEnv`
  to project from. `Engine.Input.Inject` and `Engine.Input.State` were never
  full-access consumers of this capability: `Engine.Input.Inject`
  imports `Engine.Core.State` not at all (its API already takes live
  handles explicitly), and `Engine.Input.State` is a §6.2
  `units-buildings-combat` module.

### 7.4 `world-sim-render-handoff` — **LANDED (E5a #893 + E5b #894)**

- **Dependencies:** `render-gpu-asset` (the render-handoff fields —
  `worldPreviewRef`/`zoomAtlasDataRef`/`worldQuadsRef` — are read by
  `MainRender`, so this group's record and `render-gpu-asset`'s record
  will need to be importable together at their shared boundary, or
  this group's migration should land after §7.2's), `units-buildings-combat`
  (`materialRegistryRef` is read by `Unit.Thread.Movement`). Both were
  satisfied: §7.2 landed first, so E5b could simply compose
  `RenderHandoffCapability` with the already-existing
  `RenderViewCapability` in the two modules that needed both
  (`World.Thread`, `World.Thread.Command.Init`), and with
  `UnitCombatCapability`/`BuildingCapability` in
  `World.Thread.Command.Basic`.
- **Independent migration:** Partial, exactly as this entry predicted.
  The world/sim side moved on its own in E5a; the render-handoff fields
  were the coupled part and moved in E5b.
- **Follow-up scope:** Two child issues, as anticipated — E5a (#893)
  for the world/sim/worldgen fields proper, E5b (#894) for the coupled
  render-handoff set. Both are now landed; this row is closed.

**What landed in E5a (#893):**
`Engine.Core.Capability.WorldSim` exports `WorldSimCapability` over
exactly the nine world/sim fields (`worldManagerRef`, `worldQueue`,
`sunAngleRef`, `floraCatalogRef`, `materialRegistryRef`,
`worldGenConfigRef`, `gameTimeRef`, `enginePausedRef`, `simQueue`) plus
the total one-way projection `toWorldSimCapability`, following
§7.1/#889's convention (same live `IORef`s/`Queue`s, never a copy; no
import of a consumer). It is a pure refactor — no `EngineEnv` field-set
change, no behaviour change.

- **Fully narrowed:** 50 of this row's former 54 §6.2 entries. 22 of
  them no longer import `Engine.Core.State` at all; the other 28 still
  import it narrowly, for the canonical `activeWorldStateFrom`/
  `activeWorldPageFrom`/`resolveActiveWorld` helpers, for an opaque
  `EngineEnv` they only hand to a not-yet-narrowed helper
  (`World.Thread.Command` → `Command.Basic`/`Command.Init`; the four
  designation `Cursor.*` handlers → `Cursor.Common`'s F4 outcome
  recorders, which belong to §7.5), and/or for a named accessor a
  landed capability deliberately left narrow (`unitManagerRef`,
  `unitQueue`, `luaQueue`, `loadStatusRef`, `actionOutcomeRef`,
  `hudActivePageRef`, `saveBarrierRef`).
- **Deferred to E5b (#894) — named individually, nothing silently
  dropped:** `Engine.Scripting.Lua.API.Structure`
  (`texPaletteRef`/`texPaletteHandlesRef`), `World.Thread`
  (`worldQuadsRef`), `World.Thread.Command.Basic` (`worldQuadsRef`,
  `bloodDisposeQueue`) and `World.Thread.Command.Init`
  (`worldPreviewRef`, `worldPreviewGenerationRef`, `zoomAtlasDataRef`,
  `bloodDisposeQueue`). Those four kept their §6.2 entry until E5b
  landed; together, E5a and E5b clear the row completely.
- **Two `Engine.Core.State` helpers added, not moved:**
  `activeWorldPageFrom`/`activeWorldStateFrom` take the live
  `IORef WorldManager` instead of an `EngineEnv`, so a narrowed
  consumer applies the one canonical active-world resolution rule
  (`resolveActiveWorld`) to `wsWorldManagerRef` without reaching for
  the whole environment. The existing `activeWorldPage`/
  `activeWorldState` are now defined in terms of them and behave
  identically.
- **Cross-capability consumers (§7.5/§7.3/§7.7/§7.2):** every
  non-permanent module assigned to ANOTHER capability that reads one of
  the nine now does so through `WorldSimCapability` — the same
  "mixed-capability modules adopt the record" step §7.6 describes for
  #890 — while **keeping its own §6.2 entry** until its own child
  migrates it. That covers the indirect reads too: a call to
  `activeWorldState`/`activeWorldPage` dereferences `worldManagerRef`
  just as much as naming the field does, so those call sites moved to
  `activeWorldStateFrom`/`activeWorldPageFrom` over
  `wsWorldManagerRef` as well (review round 2). That is 55 modules,
  including `Unit.Thread`,
  `Unit.Thread.Movement`, `Combat.Thread`, `Combat.Resolution`, the
  `Engine.Input.Thread.*` timestamping readers, `Engine.PlayerEvent.Emit`,
  the `API.Units.*`/`API.Buildings.*`/`API.Craft.*` families, the
  `World.Render.*` quad builders, `World.Thread.Power` and
  `World.Thread.Command.Edit.Dig`/`Cursor.Common`. §6.1's permanent
  orchestration modules (`Engine.Core.Init`, `Engine.Loop.*`,
  `app/App/Dump.hs`, the save/load family, `Lua.Thread.Dispatch`) are
  deliberately untouched: whole-environment access is their job
  description, and narrowing them early is out of scope.
- **`fpsRef` joined `RenderViewCapability`** under §7.2's own "a later
  migration adds them when it has a real consumer" rule: E5a narrowed
  `Engine.Scripting.Lua.API.Core`, whose `engine.getFPS` is exactly the
  `LuaThread` reader §5's `fpsRef` row names.
- **Enforcement:** the §6 ratchet (`TEMPORARY_CEILING`'s
  `world-sim-render-handoff` set shrunk 54 → 4, checked in both
  directions against the live scan and against §6.2), plus
  projection-aliasing coverage in `Test.Headless.Capability.WorldSim` —
  all nine fields asserted to be the same live container as
  `EngineEnv`'s, plus stability across repeated projection (E5a
  re-projects inline at most call sites) and explicit
  same-shape-swap checks on `worldQueue`/`simQueue` and
  `enginePausedRef`/`gameTimeRef`.

**What landed in E5b (#894):**
`Engine.Core.Capability.RenderHandoff` exports `RenderHandoffCapability`
over exactly the seven coupled render-handoff fields E5b found
(`worldPreviewRef`, `worldPreviewGenerationRef`, `zoomAtlasDataRef`,
`worldQuadsRef`, `bloodDisposeQueue`, `texPaletteRef`,
`texPaletteHandlesRef`; later additions: `structureWallCatalogRef`
(#1712), `structureArtCatalogRef` (#1842) and `sceneStatsRef` (#1921))
plus the total one-way projection
`toRenderHandoffCapability`, following the same §7.1/#889 convention
E5a did (same live `IORef`s/`Queue`, never a copy; no import of a
consumer). It is a pure refactor — no `EngineEnv` field-set change, no
behaviour change: preview staleness handling, zoom-atlas upload,
world-quad publication, texture-palette publication and blood-texture
disposal ordering are all byte-for-byte the same call sequence, reached
through a projected field instead of an `EngineEnv` one.

- **One record, no §3.1-style view split.** `render-gpu-asset` needed a
  full/view pair because `engineStateRef` is `MainRender`-private, and
  `input-lua-transport` needed one because two of its fields are
  `LuaThread`-private. Nothing here is private to a single thread:
  every one of them is a deliberate cross-thread handoff, which is
  the whole point of the group, so a second interface would carve a
  boundary the §5 contracts do not have. `texPaletteHandlesRef` makes
  that concrete — it has `LuaThread` readers
  (`structure.unresolvedPaletteIds`) as well as its `WorldThread` use.
- **Fully narrowed:** all four of this row's remaining §6.2 entries, so
  the row is now empty and §6.2 is down to 2 modules overall (26 − 24).
  Each still imports `Engine.Core.State` narrowly for the bare
  `EngineEnv` type they take and project from — plus, for
  `Engine.Scripting.Lua.API.Structure`, the canonical
  `activeWorldPage`/`activeWorldState` helpers, and — until #899
  (E8) — for `World.Thread`'s one named `saveBarrierRef` accessor,
  which now goes through `Engine.Core.Capability.SaveLoad` (§7.8).
- **Composed with already-landed records rather than widened.** None of
  the four needed a field added to `RenderHandoffCapability` or to any
  other record: their non-handoff reads were already covered, and each
  module simply projects the records it needs.
  `World.Thread.Command.Basic` composes `WorldSim` + `RenderHandoff` +
  `UnitCombat` + `Building`; `World.Thread.Command.Init` composes
  `WorldSim` + `RenderHandoff` + `RenderView` (`cameraRef`) +
  `ContentRegistries` (`locationDefsRef`) + `InputView` (`luaQueue`);
  `World.Thread` composes `WorldSim` + `RenderHandoff` + `RenderView` +
  `Core` (`loggerRef`/`lifecycleRef`); `Engine.Scripting.Lua.API.Structure`
  composes `WorldSim` + `RenderHandoff`. `World.Thread` takes
  `RenderViewCapability`, never `RenderCapability` — it runs on
  `WorldThread`, so §3.1's boundary (enforced by the same audit) keeps
  it off `engineStateRef`.
- **Enforcement:** the §6 ratchet (`TEMPORARY_CEILING`'s
  `world-sim-render-handoff` set shrunk 4 → 0, checked in both
  directions against the live scan and against §6.2), plus
  projection-aliasing coverage in
  `Test.Headless.Capability.RenderHandoff` — all seven fields asserted
  to be the same live container as `EngineEnv`'s, plus stability across
  repeated projection (several call sites re-project inline) and
  explicit non-swap checks on the two single-slot upload handoffs and
  on the palette/handle-table pair. The behaviour this refactor must
  not disturb keeps its own pre-existing gates:
  `Test.Headless.Lua.PreviewGeneration` for the delivery-time preview
  staleness policy and `Test.Headless.Blood.Teardown` for live-ref
  draining, exactly-once cleanup and teardown/FIFO overlap.

### 7.5 `units-buildings-combat` — **LANDED (E6a #895 + E6b #896)**

- **Dependencies:** `world-sim-render-handoff` (unit/building state
  routinely cross-references world position/material data — satisfied
  for the world/sim half since #893: those reads already go through
  `WorldSimCapability`, so this group's own migration only has to
  narrow the fields in §5's `units-buildings-combat` table), `core-init`.
- **Independent migration:** Partial, exactly as this entry predicted.
  The units-and-combat side moved on its own in E6a; the three building
  fields were the separable part and moved in E6b. `statRNGRef` needed
  neither a tiny shared capability nor a `UnitCombatCapability` import
  from `World.Thread.Command.Edit.Dig` — see the explicit-narrow bullet
  below.
- **Follow-up scope:** Two child issues, as anticipated — E6a (#895)
  for units+combat together (they already share
  `unitQueue`/`combatQueue`'s producer/consumer relationship and the
  documented shutdown-ordering dependency between them), E6b (#896) for
  buildings (consumed on `UnitThread` but conceptually its own domain).
  Both have landed; this row is closed.

**What landed in E6a (#895):**
`Engine.Core.Capability.UnitCombat` exports `UnitCombatCapability` over
exactly the ten unit/combat fields (`unitManagerRef`, `unitQueue`,
`utsRef`, `statRNGRef`, `combatQueue`, `combatEventsRef`,
`injuryEventsRef`, `thoughtEventsRef`, `actionOutcomeRef`,
`pathingConfigRef`) plus the total one-way projection
`toUnitCombatCapability`, following §7.1/#889's convention (same live
`IORef`s/`Queue`s, never a copy; no import of a consumer). It is a pure
refactor — no `EngineEnv` field-set change, no behaviour change: unit
AI dispatch, movement, spawning, combat resolution cadence, wound
ticks, the four event streams' drain contracts, and the
combat-before-unit shutdown ordering are all byte-for-byte the same
call sequence over the same containers.

- **Fully narrowed:** 35 of this row's former 49 §6.2 entries. All 35
  still import `Engine.Core.State`, but narrowly: every one needs at
  least the bare `EngineEnv` type (each still takes an `EngineEnv` and
  projects from it), and several additionally name a single accessor
  a landed capability deliberately left narrow (`loggerRef`,
  `lifecycleRef`, `saveBarrierRef`) or one of the canonical
  `activeWorldStateFrom`/`activeWorldPageFrom`/`freshItemInstanceId`
  helpers.
- **`statRNGRef` stayed shared, via the explicit-narrow rule.** This
  entry's open question — whether the four-role `statRNGRef` needed its
  own tiny capability — resolved to "no". A world-side consumer with no
  other unit/combat business takes the live handle as an ordinary
  function parameter instead of adopting the record.
  `World.Thread.Command.Edit.Dig` is the worked example: its
  `handleWorldDigTileCommand` (and the internal `spawnYieldItems` /
  `promoteFullSpoilTiles` it drives) now take the `IORef StdGen` and
  the `Queue UnitCommand` explicitly, supplied by its only caller
  `World.Thread.Command`, which names those two accessors and stays
  narrow itself. That is the same shape `Engine.Input.Callback` used in
  #892 to need no input record at all. The same rule leaves the
  pre-existing narrow readers of these fields exactly where they were —
  `Unit.HitTest`/`Unit.LineOfSight`/`Unit.Render`,
  `Engine.Scripting.Lua.API.Blood`, `API.Register.Craft`/`.Register.Item`,
  `World.Thread.Command.Cursor.Plant`, `World.Thread.Command.Edit.Terrain`
  and the three `Engine.Input.Thread.*` `actionOutcomeRef` readers of
  §7.3 — since none of them holds full access to lose.
- **Deferred to E6b (#896) — named individually, nothing silently
  dropped:** `Building.Thread.Command`,
  `Engine.Scripting.Lua.API.Buildings.Materials`, `.Progress`,
  `.Query`, `.Selection`, `.Spawn` and `.Yaml`,
  `Engine.Scripting.Lua.API.Craft.Bill`,
  `Engine.Scripting.Lua.API.Craft.Execute`,
  `Engine.Scripting.Lua.API.Power`,
  `Engine.Scripting.Lua.API.Units.Cargo`, `Unit.Thread`,
  `World.Thread.ItemTemp` and `World.Thread.Power`. Thirteen of them
  named `buildingManagerRef`, `buildingQueue` or `buildingGhostRef`
  directly; the fourteenth, `Unit.Thread`, handed its whole environment
  to `Building.Thread.Command.processAllBuildingCommands` — there is no
  separate building thread (§2.2), so the unit thread drains the
  building command queue on its own OS thread and was a genuinely mixed
  module until #896. All 14 are now migrated (see below); together, E6a
  and E6b clear the row completely.
- **Mixed consumers adopted the record without leaving their row.**
  Six of the 14 above also touch one of E6a's ten fields
  (`API.Craft.Bill`, `API.Craft.Execute`, `API.Power`,
  `API.Units.Cargo`, `Unit.Thread`, `World.Thread.ItemTemp`); every one
  of those accesses now goes through `UnitCombatCapability`, exactly
  the mixed-consumer step §7.4 and §7.6 describe. A building field is
  therefore the ONLY thing keeping any of the 14 unrestricted, which is
  what makes #896's scope a clean subtraction rather than a re-audit.
  Two modules in OTHER §6.2 rows — both full-access when E6a ran — also
  read one of the ten: `Engine.Input.Thread.Mouse` (`ui-hud-events`,
  #897 — `actionOutcomeRef`) and `World.Thread.Command.Basic` (the E5b
  remainder, #894 — `unitQueue`). Those were deliberately left naming
  the field directly, per this issue's own requirement 3 ("no early
  migration is forced on modules assigned elsewhere"): they are not
  half of this issue's a/b pair, so touching them would pull #894/#897
  scope forward without shrinking this row. `Engine.Input.Thread.Mouse`
  additionally sat exactly at its 500-line #787 module budget, leaving
  no room for another import at the time. #897 (E7a) has since narrowed
  it — inside that same 500 lines, by compacting its import block — and
  kept `actionOutcomeRef` an explicit narrow value under this section's
  own rule rather than adopting `UnitCombatCapability`.
  `World.Thread.Command.Basic` picked its record up when #894 (E5b)
  migrated it, and now reaches `unitQueue` through
  `UnitCombatCapability` too.
- **Cross-capability consumers of OTHER already-landed records:** the
  35 narrowed modules that also read a `content-registries`,
  `render-gpu-asset`, `input-lua-transport` or `world-sim-render-handoff`
  field now reach it through that capability's own record
  (`ContentRegistriesCapability` for the item/equipment/infection/recipe
  registries, `RenderViewCapability` for `cameraRef`,
  `InputViewCapability` for `luaQueue`, `WorldSimCapability` for the
  world/sim fields) rather than a named accessor — the same step E5a
  took in the other direction.
- **Enforcement:** the §6 ratchet (`TEMPORARY_CEILING`'s
  `units-buildings-combat` set shrunk 49 → 14, checked in both
  directions against the live scan and against §6.2), plus
  projection-aliasing coverage in `Test.Headless.Capability.UnitCombat`
  — all ten fields asserted to be the same live container as
  `EngineEnv`'s, stability across repeated projection (E6a re-projects
  inline at most call sites), and explicit non-transposition checks on
  the three identically-typed `IORef (Seq CombatEvent)` streams
  (`combatEventsRef`/`injuryEventsRef`/`thoughtEventsRef`), on the
  `unitQueue`/`combatQueue` producer-consumer pair, and on the
  `unitManagerRef`/`utsRef` pair a load publish swaps together.

**What landed in E6b (#896):**
`Engine.Core.Capability.Building` exports `BuildingCapability` over
exactly the three building fields (`buildingManagerRef`,
`buildingQueue`, `buildingGhostRef`) plus the total one-way projection
`toBuildingCapability`, following §7.1/#889's convention (same live
`IORef`s/`Queue`, never a copy; no import of a consumer). §3.1's
main-only/worker-safe split does not apply — none of the three fields
is confined to one execution role, so one record serves every consumer.
It is a pure refactor — no `EngineEnv` field-set change, no behaviour
change: construction jobs, building spawn (including the portal's
spawn-roster countdown), ghost placement, storage/cargo queries and the
power nodes riding on buildings are the same call sequence over the
same containers.

- **Fully narrowed:** all 14 of this row's remaining §6.2 entries; the
  row and the `TEMPORARY_CEILING` set are now empty. Thirteen still
  import `Engine.Core.State` narrowly, in the same three shapes E6a's
  35 use: the bare `EngineEnv` type they still take and project from,
  plus — where needed — `loggerRef`/`lifecycleRef`/`saveBarrierRef` or
  an `activeWorldPageFrom`/`freshItemInstanceId` helper.
  `Engine.Scripting.Lua.API.Buildings.Materials` additionally moved its
  one `itemManagerRef` read onto the already-landed
  `ContentRegistriesCapability` (§7.6), which is what left it with the
  bare type alone.
- **`Unit.Thread` and the building drain, via the explicit-narrow
  rule.** `Building.Thread.Command.processAllBuildingCommands` no
  longer takes an `EngineEnv` at all: it takes the live
  `IORef LoggerState`, a `WorldSimCapability` and a
  `BuildingCapability`, supplied by its only caller `Unit.Thread` —
  the same shape §7.4's `World.Thread.Command.Edit.Dig` and #892's
  `Engine.Input.Callback` use. That removes the one reason `Unit.Thread`
  was ever on this list (it no longer hands its whole environment
  across the boundary) and drops `Building.Thread.Command` out of §6's
  importer accounting entirely, since it now imports
  `Engine.Core.State` not at all. **The unit tick's scheduling
  boundary is unchanged:** the drain still runs outside the pause-only
  movement block, still inside the save barrier's `unless locked` gate,
  and still before both the `SaveUnit` and `SaveBuilding`
  acknowledgements.
- **Pre-existing narrow readers stayed put.** `Building.Render`,
  `Building.HitTest` and `World.Render.CursorQuads` already named a
  building accessor from a narrow import (they were migrated by #891),
  and the save/load orchestrators in §6.1 (`World.Load.Stage`,
  `World.Load.Publish`, `World.Thread.Command.Save.WriteWorld`,
  `Engine.Scripting.Lua.API.Save`) are permanent whole-session
  exceptions. `World.Thread.Command.Basic`'s one `buildingQueue` write
  belonged to the E5b remainder (#894) and was left naming the accessor
  directly under this section's own rule, exactly as E6a left it naming
  `unitQueue`: touching it would have pulled #894's scope forward
  without shrinking any row. #894 has since narrowed that module, so
  both writes now go through `BuildingCapability`/`UnitCombatCapability`.
- **Enforcement:** the §6 ratchet (`TEMPORARY_CEILING`'s
  `units-buildings-combat` set shrunk 14 → 0, checked in both
  directions against the live scan and against §6.2), plus
  projection-aliasing coverage in `Test.Headless.Capability.Building`
  — all three fields asserted to be the same live container as
  `EngineEnv`'s, and stability across repeated projection (E6b
  re-projects inline at most call sites). There is no transposition
  example, unlike E6a's three identically-typed `IORef (Seq
  CombatEvent)` streams: the three building fields have three distinct
  types, so a swapped binding cannot typecheck, leaving copying as the
  only failure mode the aliasing examples already catch.

### 7.6 `content-registries` — **LANDED (#890, E2)**

- **Dependencies:** `core-init` only (content is loaded at boot and in
  practice only read thereafter — the least coupled group in this
  inventory).
- **Independent migration:** Yes, cleanly — and done. It was the right
  *early* migration precisely because none of its 7 fields is written
  after boot-time content load in normal operation, so there was no
  write-ordering subtlety to resolve. (The loaders themselves stay
  callable at any time and keep insert/replace semantics — see §5's
  `content-registries` preamble; the record carries the write path
  rather than freezing it.)
- **What landed:** `Engine.Core.Capability.ContentRegistries` exports
  `ContentRegistriesCapability` over exactly the 7 fields plus the
  total one-way projection `toContentRegistriesCapability`, following
  §7.1/#889's convention (same live `IORef`s, never a copy; no import
  of a consumer). *(A capability record grows with its group: #957
  added an 8th field, `tutorialRegistryRef`, and its sole consumer
  `Engine.Scripting.Lua.API.Tutorial` reaches it through the record
  from the start — it never held unrestricted access to narrow.)*
  - **Fully narrowed (the nine §6.2 entries, all removed above):**
    `Engine.Scripting.Lua.API.Craft.Recipe`, `.Equipment.Class`,
    `.Infection`, `.Items.Defs`, `.Locations`, `.LootTables`,
    `.Repair`, `.Substance`, `.WorldQuery.Location`. Four of them
    (`Craft.Recipe`, `Infection`, `Substance`, `LootTables`) no longer
    import `Engine.Core.State` at all; the other five still take an
    opaque `EngineEnv` **solely** to hand to a not-yet-narrowed helper
    (`resolveTexturePath`/`loadAndRegister` — §7.2;
    `Craft.Execute.validateStation` — §7.5) or, since #893 narrowed
    §7.4's `worldStateByPage`, to project `toWorldSimCapability` for it
    (`API.WorldQuery.Location`) — and dereference no field themselves.
    The two fields outside this capability that survived the narrowing
    are passed as the bare `IORef`s they are (`statRNGRef` into
    `loot.roll`, `unitManagerRef` into `repair.repairAt`), wired at the
    `Register.*` call sites.
  - **Mixed-capability modules that adopted the record for their
    content lookups only** (each KEEPS its own §6.2 entry until its own
    capability child migrates it): `Combat.Resolution` (weapon item def
    + substance), `Combat.Wounds.Tick` (infection selection),
    `Unit.Thread.Command.Spawn` (starting inventory + equipment class),
    `Unit.Thread.Movement` (fall-injury substance lookup),
    `World.Thread.Command.Edit.Dig` (dig-yield item defs),
    `World.Thread.Power` (per-bill recipe power draw),
    `World.Render.Zoom.Quads` and `World.Thread.Discovery` (location
    defs). That is the complete set of engine-side content readers §5
    names.
- **Follow-up scope:** None — this row is closed. §7.2/§7.4/§7.5 have
  since landed; the `EngineEnv` parameters listed above that survive
  are the ones handed to a §6.1 permanent orchestration boundary or to
  a helper that composes several capability records, which is the
  intended end state, not a remaining migration. Nothing further is
  owed to `content-registries` itself. *(That closure is about the
  #537 VISIBILITY split, and it still holds. The mutation-authority
  arc below is a separate epic (#1890) reaching into the same group,
  not a reopening of this row.)*

**What #1896 added (CMA-2, epic #1890 — mutation authority, not
visibility).** The 8-field record above stayed exactly as it is and
kept every consumer of the four registries outside the pilot. Beside
it now sits `Engine.Core.Capability.ContentRegistriesView`, exporting
`ContentRegistriesViewCapability` plus the total one-way projection
`toContentRegistriesViewCapability`, following the same §2.1
convention (same live handles, never a copy; no import of a consumer)
including that section's abstract-wrapper extension.

- **What differs is the field TYPE, not the field set.**
  `crvItemManagerRef`, `crvEquipmentClassManagerRef`,
  `crvSubstanceManagerRef` and `crvRecipeManagerRef` are
  `Engine.Core.ReadOnlyRef.ReadOnlyRef`s over the very handles
  `ContentRegistriesCapability` exposes raw, so a non-writer's
  mutation is a compile error and the restriction survives the handle
  being passed into a helper or packed into a context record.
- **Who keeps the raw record for a selected field:** its four
  legitimate writers, and nobody else —
  `Engine.Scripting.Lua.API.Items.Defs` (indirectly, through
  `registerItemDefs`), `.Equipment.Class`, `.Craft.Recipe` and
  `.Substance`. They obtain it through the existing projection at
  their `Engine.Scripting.Lua.API.Register.*` call sites, never
  through a fresh raw `EngineEnv` accessor import.
- **Who moved:** all 31 read-only module-field pairs, across 26
  modules — the complete measured accessor surface, in one change.
  Three further modules move with them without naming a selected
  accessor themselves (`Building.Thread.Command`, `Unit.Thread`,
  `Engine.Scripting.Lua.API.Buildings.Progress`), plus
  `World.Thread.Command.BoundSpawn`, because they carry or forward the
  record into `Building.Knowledge.Live.containerObserver`, whose third
  parameter is now the view.
- **And one module the accessor census could not see.**
  `World.Render.GroundItemQuads` read `itemManagerRef` through the raw
  `Engine.Core.State` accessor rather than a capability record, so it
  appears in none of the counts above — while being exactly the kind of
  non-writing consumer this boundary governs. It takes the view too. The
  census measures the capability-accessor surface; the RULE is "every
  non-§6.1 reader of a selected registry", and where the two differ the
  rule wins.
- **The production pass-on** is
  `Building.Knowledge.Live.ContainerObserver.coItems ∷ ReadOnlyRef
  ItemManager` — the arc's load-bearing demonstration, since a
  record-level boundary would have ended the moment the record was
  unpacked. `coGameTime` beside it stays a raw `IORef`: it is a
  `world-sim-render-handoff` field, outside this pilot.
- **`crvInfectionManagerRef` is the one deliberately UNWRAPPED field.**
  `Engine.Scripting.Lua.API.Units.Combat` is the only module in the
  tree mixing a selected registry with an out-of-scope one; keeping
  the raw record merely to reach infection would have handed the raw
  item handle straight back, so infection is supplied on the view as
  the ordinary `IORef` it remains. It is not inside the structural
  boundary, and neither are locations, loot tables or tutorials.
- **Gates:** hspec `--match "ReadOnlyRef"` (the wrapper aliases rather
  than snapshots, both records share one container),
  `tools/test_read_only_ref_compile.py` (manual — real compilations,
  with positive controls, proving the rejections are the boundary and
  not a broken environment), plus this file's own audit, which knows
  the wrapper by name.

### 7.7 `ui-hud-events` — **FULLY LANDED (E7a #897; E7b #898)**

- **Dependencies:** `render-gpu-asset` (`UI.Render` needs both UI state
  and render/GPU handles — a genuine cross-capability read, not a
  migration blocker but something the eventual record boundary must
  accommodate), `input-lua-transport` (`focusManagerRef`, see §7.3).
  Both were already satisfied when E7a ran: #891 and #892 had landed,
  so every cross-read this group makes already had a record to reach
  through.
- **Independent migration:** Partial, exactly as this entry predicted —
  and the split fell where predicted too. The UI/focus/HUD half moved
  on its own in E7a; the event/notification/popup half followed in
  E7b, needing nothing from E7a beyond the shared convention.
- **Follow-up scope:** Two child issues, as anticipated — E7a (#897)
  for the UI/focus/HUD fields, E7b (#898) for the
  event/notification/popup ones. The two halves have almost no
  consumers in common, which is what made the split clean. Both have
  landed; this group's §6.2 row is empty.

**What landed in E7a (#897):**
`Engine.Core.Capability.Ui` exports `UiCapability` over exactly the
four UI/focus/HUD fields (`uiManagerRef`, `focusManagerRef`,
`hudActivePageRef`, `textBuffersRef`) plus the total one-way projection
`toUiCapability`, following §7.1/#889's convention (same live
`IORef`s, never a copy; no import of a consumer). It is a pure
refactor — no `EngineEnv` field-set change, no behaviour change:
pointer routing, modal boundaries, press/release activation, keyboard
control focus, text-input buffers, tooltip dwell/lock and the HUD
active-page tracking are all the same call sequence over the same
containers.

- **One record, no split.** Unlike §3.1's `render-gpu-asset` and
  §7.3's `input-lua-transport`, this capability owns no
  thread-private field: §5 records readers *and* writers on more than
  one thread for `uiManagerRef`/`focusManagerRef`, and the two
  single-role fields (`hudActivePageRef` — `WorldThread`;
  `textBuffersRef` — read on `LuaThread`, written on `MainRender`) are
  ordinary session/boot state, not an allocator or a handoff slot. So
  there is one record and no main-only/worker-safe pair, and the audit
  needs no import boundary for it beyond §6's ratchet.
- **Field prefix `uic`, not `uc`.** The convention appends a `c` to a
  single-word record's initial (`cc`/`rc`/`ic`), which would collide
  with `UnitCombatCapability`'s `uc` (#895). Two capability records
  sharing one prefix would be actively misleading in any module
  holding both, so this record uses `ui` + `c`.
- **Fully narrowed:** all 11 of this row's UI-dominant §6.2 entries —
  `Engine.Input.Thread.Mouse`, `Engine.Scripting.Lua.API.ShellFocus`,
  the seven `Engine.Scripting.Lua.API.UI.*` modules (`Element`, `Focus`,
  `Hierarchy`, `Page`, `Property`, `TextInput`, `Tooltip`),
  `Engine.Scripting.Lua.Message.Scene` and `UI.Tooltip.State`. All 11
  still import `Engine.Core.State`, but narrowly (see §6's accounting
  for exactly what each one still needs).
- **Mixed and already-narrow consumers adopted the record too.** Four
  modules outside §6.2 reached one of these fields by named accessor
  and would otherwise have kept a bare ref alive with no future
  ratchet entry to remove it: `UI.Render` (`uiManagerRef` — now off
  `Engine.Core.State` entirely), `Engine.Scripting.Lua.API.Config`
  (`uiManagerRef`), `Engine.Scripting.Lua.API.Text` (`textBuffersRef`)
  and `World.Thread.Cursor` (`hudActivePageRef`). The three
  input-thread modules §7.3 left on explicit `focusManagerRef`/
  `uiManagerRef` values pending this record —
  `Engine.Input.Thread.Keyboard`, `.Char` and `.Scroll` — plus
  `Engine.Input.Thread.Mouse.Activation` took it as well; #745's
  atomic focus/control-focus transitions are the identical
  `atomicModifyIORef'` calls over the identical container, since a
  projection hands out the live handle. `Engine.Core.Init` (seeds the
  refs), `Engine.Core.State` (declares them), the projection module
  itself and `World.Load.Publish` (§6.1 permanent load orchestration,
  which resets three of the four) stay named-accessor consumers by
  design.
- **`Engine.Input.Thread.Mouse` and its #787 budget.** §7.5 recorded
  this module as the one that could not adopt `UnitCombatCapability`
  because it sat exactly at its 500-line budget
  (`tools/haskell_module_budget.py`). E7a narrowed it anyway, at
  exactly 500 lines: its import block moved to one-line form, which
  paid for the four capability imports it now carries
  (`Core`/`InputView`/`RenderView`/`Ui`). `actionOutcomeRef` is still
  the explicit narrow value §7.5's rule assigns it.
- **Enforcement:** the §6 ratchet (`TEMPORARY_CEILING`'s
  `ui-hud-events` set shrunk 13 → 2, checked in both directions
  against the live scan and against §6.2), plus projection-aliasing
  coverage in `Test.Headless.Capability.Ui` — all four fields asserted
  to be the same live container as `EngineEnv`'s, stability across
  repeated projection (E7a re-projects inline at most call sites,
  several times within a single input event), and an explicit check
  that the two focus-carrying refs are not transposed.
- **Deferred to E7b (#898) — named individually, nothing silently
  dropped:** `Engine.PlayerEvent.Emit` and
  `Engine.Scripting.Lua.API.PlayerEvent`. Both are event-dominant:
  they need `eventStoreRef`, `notificationCfgRef`, `notificationOrder`
  and/or `popupQueueRef`, none of which `UiCapability` carries, and
  neither touches any of E7a's four fields — so #898 was a clean
  subtraction rather than a re-audit.

**What landed in E7b (#898):**
`Engine.Core.Capability.Events` exports `EventsCapability` over
exactly the four event/notification/popup fields (`eventStoreRef`,
`notificationCfgRef`, `notificationOrder`, `popupQueueRef`) plus the
total one-way projection `toEventsCapability`, following §7.1/#889's
convention (same live handles, never a copy; no import of a consumer).
It is a pure refactor — no `EngineEnv` field-set change, no behaviour
change: category gating (`log`/`popup`/`pause`), coalescing, the
~1000-entry ring cap, `engine.getEventLog()`'s payload including the
`uid` tagging, `LuaShowPopup` popup delivery, the notification
registry order and its runtime overrides are all the same call
sequence over the same containers.

- **Field prefix `ec`.** The convention's single-word form (initial +
  `c`) collides with none of the landed records' prefixes (`cc`, `cr`,
  `ic`, `iv`, `rc`, `rv`, `uc`, `uic`, `ws`), so no §7.7-style
  exception was needed here.
- **One record, no split.** This half owns no thread-private field
  either: both `TVar`s are multi-writer STM, `notificationCfgRef` is
  read on `AnyThread` from the emit path, and `notificationOrder` is
  an immutable boot value. So one record, no main-only/worker-safe
  pair, and no §3.1-style import boundary in the audit beyond §6's
  ratchet.
- **Fully narrowed:** both of this row's remaining §6.2 entries —
  `Engine.PlayerEvent.Emit` and
  `Engine.Scripting.Lua.API.PlayerEvent`. Each still imports
  `Engine.Core.State`, but only for the opaque `EngineEnv` type: their
  public signatures are unchanged (`emitEvent env …` and the Lua
  registration functions still take an `EngineEnv`), and every field
  read now goes through a projection. Besides `EventsCapability` they
  project the strictly narrower records the emit path already needed —
  `CoreCapability` (`loggerRef`, for the unknown-category warning),
  `WorldSimCapability` (`gameTimeRef` for the event timestamp,
  `enginePausedRef` for a `pause`-flagged category) and
  `InputViewCapability` (`luaQueue`, for the `LuaShowPopup` message
  that is the real popup delivery path).
- **Producers assigned elsewhere were not force-migrated.**
  `World.Thread.Discovery` (its own `world-sim-render-handoff`
  narrowing) and the save/load emit sites
  (`World.Thread.Command.Save.WriteWorld`,
  `Engine.Scripting.Lua.API.Save` — §6.1 permanent orchestration) call
  the same unchanged `emitEvent*` API and keep their established
  access. `Engine.Core.Init` (seeds the refs), `Engine.Core.State`
  (declares them), the projection module itself and
  `World.Load.Publish` (§6.1, which resets both `TVar`s) stay
  named-accessor consumers by design — the same four kinds of
  exception E7a left.
- **Three steering-text corrections rode along**, each a §5 row this
  document had flagged as stale: `EngineEnv`'s `popupQueueRef` field
  doc no longer claims the Lua side drains the TVar (it is write-only;
  delivery is the separate `LuaShowPopup` message), and
  `Engine.PlayerEvent.Emit`'s module comment plus `Engine.Core.Init`'s
  seeding comment now state the STM primitive's any-thread safety
  separately from the world-and-Lua-thread call sites that actually
  exist — no unit- or combat-thread emitter does.
- **Enforcement:** the §6 ratchet (`TEMPORARY_CEILING`'s
  `ui-hud-events` set shrunk 2 → 0, checked in both directions against
  the live scan and against §6.2), plus projection-aliasing coverage
  in `Test.Headless.Capability.Events` — the three ref-shaped fields
  asserted to be the same live container as `EngineEnv`'s,
  `notificationOrder` asserted by value (it has no identity),
  stability across repeated projection (E7b re-projects inline on
  every emit), and an explicit check that the two event `TVar` fields
  are each pinned to their own named counterpart. That last one was
  written when both were `TVar (Seq PlayerEvent)` and a transposition
  the compiler could not catch; #1714 gave the log ring its own
  `TVar EventStore`, so the swap is now a type error and the check
  stands as the remaining proof that neither is aliased to the other.

### 7.8 `save-load-coordination` — **LANDED (#899, E8)**

- **Dependencies:** Every other group, transitively — a save/load
  transaction observes the whole session by design (see §6.1's
  permanent-exception entry for `World.Thread.Command.Save`/
  `World.Load.Stage`/`World.Load.Publish`/`Engine.Scripting.Lua.API.Save`).
  This group's own five fields (the *coordination* state: barrier, load
  status, staged-load handoff, last-save-time, the item-instance
  allocator) are narrower than the transaction machinery itself.
- **Independent migration:** No — it went **last**, by design, once
  every other capability record existed for the save/load machinery to
  compose from.
- **What landed:** `Engine.Core.Capability.SaveLoad` exports
  `SaveLoadCapability` over exactly the five fields of §5's
  `save-load-coordination` table (`slLoadStatusRef`,
  `slPendingLoadRef`, `slSaveBarrierRef`, `slLastSaveTimeRef`,
  `slNextItemInstanceIdRef`) plus its total, one-way
  `toSaveLoadCapability` projection over the identical live handles.
  One record, not a §3.1-style full/view pair: none of the five is
  private to a single thread the way `engineStateRef` is to
  `MainRender`. The barrier is read AND written by six roles by
  design, the allocator is explicitly `AnyThread`, and the one field
  with a single-role contract (`lastSaveTimeRef`, `LuaThread`-only) is
  a plain monotonic clamp with no privileged pointer behind it —
  §3.1's rule is about a record handing a thread a way to reach state
  another thread privately owns, and there is none here.

  The **real consumer**, per E1's no-unused-record rule, is
  `World.Thread`: #894 (E5b) left it one narrow `saveBarrierRef`
  accessor as its last direct `Engine.Core.State` field import, and
  both of its barrier sites (the per-tick `captureLocked` gate and the
  end-of-tick `acknowledgeCurrent ... SaveWorld`) now go through
  `slSaveBarrierRef`. Its `Engine.Core.State` import names no
  `save-load-coordination` accessor at all any more.

  The other non-permanent barrier consumers — `Unit.Thread`,
  `Combat.Thread`, `Sim.Thread`, `Engine.Input.Thread` and
  `Engine.Loop.Mode` — keep narrow imports naming `saveBarrierRef` as
  an individual accessor (each of them uses exactly that one field of
  this capability, alongside accessors of other capabilities). That is
  deliberate, and is the narrowest API in each case: §7.5's
  explicit-narrow rule says a module needing exactly one handle should
  take exactly that handle, not a five-field record it will use one
  field of. None of them has unrestricted access, so none is a §6
  ratchet concern; the record is for consumers that need more than one
  handle, and is the documented home of the capability.
- **What deliberately did NOT change:** §6.1's five permanent
  save/load orchestration modules (`World.Thread.Command.Save`,
  `.Save.WriteWorld`, `World.Load.Stage`, `World.Load.Publish`,
  `Engine.Scripting.Lua.API.Save`). A save captures, and a load
  replaces, every capability's state atomically in one coordinated
  step; narrowing them would reconstruct an env-shaped aggregate one
  level down while making the transaction harder to read. They may use
  `SaveLoadCapability` internally, but they are not migration targets —
  see §6.1's entry and §6.4(d) for the governing policy. Their internal
  structure remains out of scope, as it was from the start.
- **Follow-up scope:** None. This child also performed the epic's end
  state: §6.2's ceiling is empty, the live unrestricted importer set
  equals §6.1 exactly, and `audit_permanent_boundary` now pins §6.1's
  documented set to the checked-in constants so neither can move alone.
  §6.4 is the procedure that replaces the ceiling.
