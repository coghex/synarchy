# Persistence Contract

**Status:** Authoritative, Phase 1 of the save-overhaul epic (issue #756,
"save-overhaul A1"). Written 2026-07-12.

This document is the contract that every future persistence-overhaul
child (the coordinated snapshot barrier, the new save envelope, per-field
migrations) must implement against. It defines what a save *means*, how
every piece of engine/Lua state is classified, and how the system stays
honest as it grows. The companion document,
[`persistence_state_inventory.md`](persistence_state_inventory.md), is
the actual field-by-field classification this contract's rules produce;
read this document for the *rules*, that one for the *answers*.

**This issue changes no runtime save/load semantics and no save-file
encoding.** Every "new-format" classification below documents *intended*
behavior for the coordinated-barrier/new-envelope children to implement,
not something this PR ships. Where a classification differs from what
v82 actually does today, the inventory says so explicitly (v82 is a
disposable format — see [Format-version policy](#format-version-policy)
— so documenting a target that differs from today's field-for-field
layout is not a contradiction).

## 1. What a save represents (snapshot equivalence)

A save is **one coherent logical boundary across all persistent gameplay
state** — not a per-page or per-subsystem snapshot, and not a replay
log.

- **Session replacement, not merge.** Loading a save replaces the
  entire current game session, including every live world page. It does
  not merge the save's pages into whatever happens to already be in
  memory. **This is a target for a future runtime child, not current
  behavior** — see the divergence callout immediately below.
- **Publish-after-validate.** A successful load publishes the
  replacement session only after restoration and validation of every
  component succeed. A load that fails partway must not leave the live
  session half-replaced (see [Content-integrity
  behavior](#content-integrity-behavior) for what "fails" means).
- **Save leaves the session paused; load starts paused.** Both
  directions land on the same paused state, so a save is always a safe,
  quiescent point to resume from. Unpausing after a load uses the
  **normal default simulation speed** — the speed in effect at the
  moment of save is not part of the contract and is not restored.
- **No deterministic replay.** Two copies of a save resumed
  independently are not required or expected to evolve identically.
  Thread scheduling order, transient gameplay entropy (e.g. stat-roll
  RNG), and random-generator state exist to make *this* session feel
  alive, not to make some *future* session reproducible, and are
  therefore never persisted merely to enable replay.
- **Meaningful seeds are still domain data.** The above does not apply
  to seeds that are deliberately meaningful gameplay facts — a world's
  generation seed is persisted exactly, because it *is* the world's
  identity, not an implementation detail of how randomness happened to
  play out.

"The same state" therefore means: every classified item in the
inventory resolves to an equivalent value by the rule its classification
assigns it (exact bytes, a re-resolved reference, a fresh deterministic
rebuild, or a documented reset) — not bit-for-bit identical process
state.

### Divergence: current loading merges, it does not replace

The "session replacement, not merge" rule above is this contract's
target, mandated by this issue's own spec — it is **not** what
`handleWorldLoadSaveCommand` does today. The current load path
(`src/World/Thread/Command/Save/LoadWorld.hs`, #191/#218) deliberately
**preserves** any live world page that isn't part of the save being
loaded: it computes the set of page ids the load "owns" (the restored
pages, their saved original ids, and whatever a prior load of the same
save registered), filters every building/unit down to that set, and
explicitly keeps everything else — the code's own comment names this
"off-page ... genuinely unrelated live pages, which we keep (#191)".
That is a merge, not a session replacement, and it is intentional,
tested, load-bearing behavior (#191 was itself a bug fix — the prior
behavior of dropping unrelated pages was the defect).

This is therefore a fourth v82-vs-new-format divergence alongside
`wpsTimeScale`, `pause.lua`'s `prevTimeScale`, and `wpsToolMode` (see
`persistence_state_inventory.md`'s summary) — bigger in scope than
those three, because it changes the *load path's* behavior rather than
a single field's disposition. This issue does not implement it: no code
in this PR changes `LoadWorld.hs`. The responsible future child (A2,
which owns the coordinated snapshot barrier and therefore the shape of
"what a load operation does") must decide how whole-session replacement
coexists with the legitimate use case #191 was fixing — a player loading
a *different* save while other pages are open — and implement it, then
extend or replace `tools/multiworld_save_probe.py` with a same-process
load test: the existing probe only exercises save → quit → fresh
restart → load, which starts every run with zero pre-load pages and so
cannot observe merge-vs-replace behavior at all. `wmWorlds`/`wmVisible`
in the inventory (§3) are flagged with this same note.

## 2. Classification taxonomy

Every state item inventoried in `persistence_state_inventory.md` gets
exactly one of five classifications:

| Classification | Meaning |
|---|---|
| **Persist exactly** | The value is serialized and restored verbatim. It is gameplay-domain data with no other source of truth. |
| **Persist as identity/reference** | Only a stable identifier (an id, a name, a page key) is persisted; the referenced content or object is resolved against current definitions/state at load time, not embedded. |
| **Rebuild deterministically** | Never serialized. Reconstructed from persisted domain data plus current content (YAML defs, code) at load time — always the same result for the same inputs, but not itself stored. |
| **Reset to a documented default** | Never serialized (or serialized-but-ignored, see below). Always takes a fixed default value after load, regardless of what it held pre-save. |
| **Exclude (transient runtime state)** | Never serialized, never restored, never given special load-time treatment. Purely a property of the running process. |

For every inventoried item, the inventory records: its **owner**
(file:line of the record/module that holds it), its **scope** (global —
one instance for the whole engine — or per-page), its **restoration
dependency** (what other state or content it needs already loaded
before it can be resolved, if any), its **validation expectation** (what
"this restored correctly" means, if anything beyond type-correct
deserialization), and its **test oracle** (which existing or future
test/probe is expected to catch a regression here).

### What counts as a "root state owner"

For the purposes of this contract and its audit (see [The
persistence-inventory audit](#the-persistence-inventory-audit)), a
**root state owner** is a field on one of these aggregator records:

- `EngineEnv` and `EngineState` (`src/Engine/Core/State.hs`) — every
  IORef/queue/TVar the engine carries hangs off one of these two.
- `WorldManager` and `WorldState` (`src/World/State/Types.hs`) — every
  per-page IORef hangs off `WorldState`; every page hangs off
  `WorldManager`.
- `SaveData`, `WorldPageSave`, `SaveMetadata`, `SaveHeader`
  (`src/World/Save/Types.hs`) — the current serialized envelope, i.e.
  the ground truth of what v82 already persists.
- Registered Lua persistence modules (`scripts/lib/save_modules.lua`'s
  `saveModules.register` call sites).

This is deliberately not "every record in the codebase" — it is exactly
the set of records that a brand-new manager, cache, or subsystem *must*
be wired into to be reachable from a running session at all. A new
top-level manager with no field anywhere on `EngineEnv` or `WorldState`
is unreachable and doesn't exist as engine state; the moment it's wired
in (an `IORef NewManager` field appears on `EngineEnv`, or a
`wsNewThingRef` appears on `WorldState`), it is a new root-owned field
and must be classified. This makes the audit tractable: it doesn't need
to understand Haskell's type graph, only to diff the field lists of a
fixed set of files against the inventory's classification tables.

## 3. Durable gameplay intent vs. transport machinery

State reaches its owner through inter-thread queues
(`Engine.Core.Queue.Queue`, wrapping `TQueue`) and Lua↔engine message
channels. A queue is never itself a persistence unit — but a message
sitting in a queue at the exact instant of a snapshot can represent real
gameplay intent (a designation not yet applied, an attack not yet
resolved), and silently dropping it would lose real player action.

The rule: **for every queue, the snapshot boundary must account for
every message category as either (a) finished before the boundary, (b)
already represented in durable state independent of the queue, or (c)
deliberately canceled/reset.** No future child may add a new queue or
command type without extending this table.

| Queue (`EngineEnv` field) | Message type | Snapshot-boundary rule | Why |
|---|---|---|---|
| `inputQueue` | `InputEvent` | Cancel/reset | Raw device events are not gameplay facts; any in-flight synthetic-input barrier wait is abandoned, not resumed, across a save. |
| `luaToEngineQueue` | `LuaToEngineMsg` | Finish before boundary | Synchronous rendering/window side-effect requests processed within the tick they're issued; nothing gameplay-durable rides here (results land in session-only UI/Lua state, itself Excluded). |
| `luaQueue` | `LuaMsg` | Finish before boundary | Same reasoning, engine→Lua direction. |
| `worldQueue` | `WorldCommand` | **(a)** for the save/load commands themselves — they *are* the boundary. **(b)** for designation/tile-edit/state-mutating commands — the moment such a command is applied it becomes durable data already covered elsewhere in the inventory (`wsEditsRef`, `wsMineDesignationsRef`, etc.); a command still queued but not yet applied at snapshot time is not yet part of any durable state. | The snapshot barrier (A2) must drain `worldQueue` to empty before snapshotting, not race it. This is a requirement on A2, not something this issue changes. |
| `unitQueue` | `UnitCommand` | Same rule as `worldQueue`'s (b) — drain before snapshot; applied results (a completed `moveTo`, a spawned unit) already live in `UnitManager`/`UnitThreadState`, both inventoried and Persisted. | A2 requirement. |
| `buildingQueue` | `BuildingCommand` | Same rule. | A2 requirement. |
| `combatQueue` | `CombatCommand` (`CombatAttack`) | Drain before snapshot. Authoritative combat results (wounds, HP) already live in durable unit/injury state; an attack command not yet resolved must finish or be represented, not vanish leaving a "the attack was issued but never happened" gap. | A2 requirement. |
| `simQueue` | `SimCommand` | Drain before snapshot; the terrain/fluid results these commands cause are durable (`wsEditsRef`, tile data), the queue is pure transport. | A2 requirement. |
| `screenshotRequestQueue` | `ScreenshotRequest` | Cancel before boundary | A pending debug screenshot request has no gameplay meaning. |
| `combatEventsRef` | `CombatEvent` (`TVar`, drained by the combat-log panel) | Cancel/reset — drop whatever's queued at the boundary | These are notification-only fan-out to the UI, not a source of gameplay truth: the wounds/HP/kill facts a combat event *describes* already live durably elsewhere (unit/injury state); the event itself is a one-shot "tell the log panel" message with no independent meaning once dropped. |
| `injuryEventsRef` | `CombatEvent` (fall/injure/death kind, `TVar`) | Cancel/reset | Same reasoning as `combatEventsRef` — the wound/death it describes is durable via unit/injury state; the notification itself is not. |
| `thoughtEventsRef` | thought/mood notification (`TVar`) | Cancel/reset | Notification-only; the mood/psychology state it describes (if any) is durable elsewhere, not in this stream. |
| `actionOutcomeRef` | action-result notification (`TVar`) | Cancel/reset | Notification-only, same reasoning. |
| `eventStoreRef` | player-event ring buffer (`TVar`, multi-writer) | Cancel/reset — the ring buffer starts empty on load | The event LOG panel's history is explicitly session-only (contract §5's "temporary popups" spirit extends to the notification log); nothing durable is derived from a *past* log entry, only from the state changes that produced it, all covered elsewhere. |
| `popupQueueRef` | popup notification (`TVar`, multi-writer) | Cancel/reset | A pending popup has no gameplay meaning; a player who hasn't dismissed a toast by save time simply doesn't see it again after load, same as if they'd dismissed it. |
| `debugQueue` (`TQueue DebugCommand`, `src/Engine/Scripting/Lua/DebugServer.hs`) | queued debug-console command + response channel | Cancel/reset | An open debug-shell session and any command mid-flight have no gameplay meaning (contract §5: exclude debug-shell contents); a client waiting on a response simply never gets one, same as if the engine had been killed. |

`CraftBills` deserve a specific callout because they look queue-shaped
but are not: `cbClaimant`, `cbWorking`, `cbPaused`, and progress accrual
are durable gameplay facts stored directly on the `CraftBill` record
(`wpsCraftBills`, Persist exactly) — a bill is never represented as an
in-flight message, so none of the drain-before-snapshot reasoning above
applies to it.

`wsStructureStageRef` (the Lua write-ahead structure-placement staging
area, explicitly "never saved" per its own module comment) is the other
notable in-flight case: it is Excluded, meaning any in-progress
multi-tile structure placement must complete or be abandoned by the
snapshot boundary — a future child that lets a save land mid-placement
must promote this to a durable classification instead of silently
dropping the in-progress stamp.

## 4. Content-integrity behavior

- A missing **equipment** texture uses the equipment placeholder.
- A missing **terrain** texture uses the terrain placeholder.
- Any other missing visual asset uses the magenta-checkerboard
  fallback.
- Missing visual assets never by themselves invalidate an otherwise
  coherent save — a save is gameplay data, not an asset manifest.
- A missing **gameplay definition** (a referenced unit, item, building,
  material, or recipe definition that does not exist in currently
  loaded content) fails loading clearly, before the live session
  changes. The loader must not invent a substitute definition and must
  not silently drop the affected state (a unit with an unknown species,
  an item with an unknown def) — that would violate publish-after-
  validate (§1) by producing a session that quietly diverges from what
  was saved.
- Producing placeholder textures and changing renderer fallback
  behavior are tracked separately (out of scope here); this contract
  only defines their effect on load *validity*, which is: none, for
  visuals.

## 5. Format-version policy

- `currentSaveVersion` (`src/World/Save/Types.hs`) is 82 as of this
  issue. The new envelope increments it when it lands.
- **No v82→new-format migration is implemented.** Existing local saves
  and obsolete old-format test fixtures may simply be deleted.
- The **first completed new-format save becomes the compatibility
  baseline** — i.e. the format is not required to be stable *before*
  that point, only from it forward.
- From that baseline on, every subsequent format change requires an
  explicit per-component migration: translate changed fields, supply
  deliberate defaults for new fields. No more silent breaks.
- An unsupported or malformed save must fail clearly (a readable error
  naming the expected vs. found version, mirroring the existing
  `currentSaveVersion` mismatch behavior) without partially modifying
  the live session — this is the same publish-after-validate rule from
  §1 applied to the format layer specifically.

## 6. Existing save/load test & probe disposition

No hspec suite exercises save/load — a saved/loaded session spans the
Lua thread, the world thread, the unit/building threads, and an
on-disk file, which is inherently a multi-process, disk-touching
scenario outside hspec's scope. Coverage lives entirely in headless
Python probes (`tools/*.py`).

**Primary save/load probes** — their whole purpose is testing the
persistence envelope itself:

| Probe | Covers | Disposition | Responsible future child |
|---|---|---|---|
| `tools/multiworld_save_probe.py` | Multi-page save/load (#214/#219), world-identity round-trip (#707), the gold-standard save→quit→restart→load pattern | **Rewrite, and extend.** Its assertions (per-page unit/building survival, cross-page isolation, identity round-trip) are exactly what B1's new format must still guarantee, but it currently asserts against v82's field layout and will need updating. It also needs a genuinely new case: every run today starts from a fresh restart with zero pre-load pages, so it cannot observe the merge-vs-replace divergence documented in §1 ("Divergence: current loading merges, it does not replace") — a same-process load with an unrelated live page already present must be added once that divergence is implemented. | B1 (field layout); A2 (session-replacement test case) |
| `tools/save_pause_probe.py` | Pause/timescale invariant across save and load (#42) | **Rewrite.** §1/§5 of this contract retarget `wpsTimeScale` to Exclude and drop `prevTimeScale` persistence from `pause.lua` (see inventory) — both are runtime-semantics changes this issue does *not* make, but the probe's current assertions partly depend on the field being restored, so whichever child changes that behavior must update this probe alongside it. | Whichever child implements the req-2 "no persisted sim speed" behavior (likely B1) |
| `tools/lua_orphan_prune_probe.py` | Post-load reconcile of orphaned per-id Lua AI/spawn state (#195) | **Retain as-is.** Tests a Lua-side invariant (`onSaveLoaded` reconcile) orthogonal to the envelope's wire format; nothing in this contract changes it. | — |

**Secondary probes** — domain probes whose own gate happens to include
a save→quit→restart→load round trip as one assertion among several
(`grep -l "saveWorld\|loadSave" tools/*.py` minus the three primary
probes above — 13 as of this writing). Every one of these is
**retained as-is**: none needs rewriting by THIS issue, and each gates
its own domain's persistence classification, not the save envelope
itself. A future envelope/format change breaking one is a signal that
domain's classification needs re-checking, not a sign this contract's
inventory is wrong.

Every row's "Responsible future child" is **B1** (the new save envelope):
each probe's domain field is a `WorldPageSave`/`SaveData` member, so
only a change to THAT field's wire representation — B1's job, by
definition — could ever require touching these probes. None is A2's
responsibility (the snapshot barrier changes *when*/*how atomically* a
save happens, not what any individual field looks like), and none
needs a domain-specific child of its own — unlike the three primary
probes above, where the specific behavior under test (session
replacement, sim-speed persistence) is itself changing.

| Probe | Domain gated (inventory row) | Disposition | Responsible future child |
|---|---|---|---|
| `tools/chop_probe.py` | `wsChopDesignationsRef`/`wpsChopDesignations` (§3/§4) | Retain — the save round-trip assertion here is "a chop designation survives save→load", unaffected by envelope-level decisions. | B1 (only if `wpsChopDesignations`'s representation changes) |
| `tools/crop_probe.py` | `wsCropPlotsRef`/`wpsCropPlots` (§3/§4) | Retain — same reasoning, crop plots. | B1 (only if `wpsCropPlots` changes) |
| `tools/foraging_probe.py` | `wsFloraHarvestsRef`/`wpsFloraHarvests` (§3/§4) | Retain — same reasoning, foraged-flora harvest state. | B1 (only if `wpsFloraHarvests` changes) |
| `tools/item_instance_probe.py` | `wsGroundItemsRef`/`wpsGroundItems` + `sdNextItemInstanceId` (§1/§3/§4) | Retain — asserts item-instance identity survives save→load; the allocator max-not-lowered rule (#67) it also covers is independent of envelope format. | B1 (only if `wpsGroundItems`/`sdNextItemInstanceId` changes) |
| `tools/flora_growth_probe.py` | `wsFloraHarvestsRef`/`wpsFloraHarvests` + the world date (`wpsDateYear`/`Month`/`Day`, §4) | Retain — asserts the growth clock survives save→load. | B1 (only if those fields change) |
| `tools/farm_ai_probe.py` | `wsCropPlotsRef`/`wpsCropPlots` + `wsPlantDesignationsRef`/`wpsPlantDesignations` (§3/§4) | Retain — same reasoning, farm-AI-driven plot/designation state. | B1 (only if either field changes) |
| `tools/location_overlay_probe.py` | `wsEditsRef`/`wpsEdits` (§3/§4, structure overlay rides the edit log) | Retain — asserts a placed structure's overlay survives save→load. | B1 (only if `wpsEdits` changes) |
| `tools/location_stamp_idempotent_probe.py` | `wsEditsRef`/`wpsEdits` (§3/§4, structure stamps) | Retain — asserts re-stamping is idempotent across a save→load boundary. | B1 (only if `wpsEdits` changes) |
| `tools/location_content_probe.py` | `LocationRegistry.lrDefs` (§9, content) + `wsEditsRef`/`wpsEdits` for any spawned structure | Retain — mostly a content-loading gate; its save/load touch is incidental (spawned-structure persistence), unaffected by envelope decisions. | B1 (only if `wpsEdits` changes; content-loading itself has no format dependency) |
| `tools/item_temp_probe.py` | item-instance temperature, riding the same `wsGroundItemsRef`/`wpsGroundItems`/unit-inventory item data as `item_instance_probe.py` | Retain — same reasoning. | B1 (only if `wpsGroundItems`/unit-inventory item representation changes) |
| `tools/power_probe.py` | `wsPowerNodesRef`/`wpsPowerNodes` (§3/§4) | Retain — asserts power-node state (incl. battery charge, #360) survives save→load. | B1 (only if `wpsPowerNodes` changes) |
| `tools/plant_probe.py` | `wsPlantDesignationsRef`/`wpsPlantDesignations` (§3/§4) | Retain — same reasoning, planting designations. | B1 (only if `wpsPlantDesignations` changes) |
| `tools/till_probe.py` | `wsTillDesignationsRef`/`wpsTillDesignations` (§3/§4) | Retain — same reasoning, till designations. | B1 (only if `wpsTillDesignations` changes) |

## 7. The persistence-inventory audit

`tools/persistence_inventory_audit.py` (req 10) is a static guard, not a
serialization-correctness proof. It:

1. Extracts the current field list of every root-owner record (§2) from
   its source file, via a Haskell record-field parser that strips
   (possibly nested) `{- -}` and `--` comments first — literal-aware in
   BOTH the block-comment pass and the line-comment pass, so a
   `DataKinds`/`GHC.TypeLits` promoted string OR char literal in a
   field's own type (`Proxy "}"`, `Proxy '}'`, `Proxy "--"`, or even
   `Proxy "{-"`/`Proxy "-}"` in two DIFFERENT fields, which would
   otherwise look like a comment opening in one field and closing in a
   LATER one, silently swallowing everything between) can never be
   mistaken for a structural brace or a comment marker; a trailing
   "prime" on an ordinary identifier, e.g. `field'`, is correctly
   distinguished from a char-literal opener by context — so prose in a
   Haddock comment can never desync the brace-depth tracking that finds
   a record's boundary either; splits the record's brace block on
   top-level commas only, likewise literal-aware, so a comma inside a
   field's own type — a tuple, a list-of-tuples — is never mistaken for
   a field separator; understands the codebase's `UnicodeSyntax` (`∷`)
   field separators with the field name and its arrow allowed on
   different physical lines; and handles grouped declarations
   (`{ name1, name2 ∷ Type }`, several names sharing one trailing type
   signature).
2. Extracts every `saveMods.register(...)` call site across `scripts/`,
   covering four Lua access-expression forms for the registry table
   itself (a local named `saveMods`/`saveModules`, bracket-indexed
   `saveMods["register"](...)`/`saveMods['register'](...)`,
   `require("scripts.lib.save_modules").register(...)` chained directly
   off its own `require()` with no local binding at all, and
   `package.loaded["scripts.lib.save_modules"].register(...)` chained
   directly off the exact cache slot `require()` itself reads/writes —
   all ordinary, fully traceable Lua) and all three Lua string-literal
   forms for the module name (`'...'`, `"..."`, and long brackets `[[...]]`/
   `[=[...]=]`/...), fully string-literal-aware in both directions — a
   `--` embedded in a quoted OR long-bracket string is never mistaken
   for a comment start (which would otherwise truncate the line and
   hide a real call after it), and a bare (non-`--`-prefixed)
   long-bracket span is recognized as a string, not code. The extractor
   ALSO discards any call-shaped match whose start falls inside an
   unrelated string-literal span, so a doc string whose text merely
   LOOKS like a registration (`[[example: saveMods.register("x", nil,
   nil)]]`) is never extracted as a live one — only a real call's own
   receiver, which is never itself inside a string, is ever excluded
   from that filter.
3. Parses `persistence_state_inventory.md`'s classification tables for
   the set of item names that have a recorded classification AND that
   classification's own cell text, scoped to the exact `### OwnerName`
   heading each table sits under (locating the "Classification" column
   by its header text, since its position varies by table) — not merely
   the coarser `## N.` section a heading belongs to, since several
   distinct owners can share one numbered section (e.g. `WorldManager`/
   `WorldState` both under "## 3."). A name is only "classified" for the
   owner it's actually documented under, so a same-named field/module
   under a DIFFERENT owner can never mask a missing decision.
4. Fails, naming each offender, if any extracted field or registered
   module name has no matching inventory entry under its own owner
   heading, OR if its classification cell's CORE value (after stripping
   bold markup and a trailing parenthetical aside) doesn't exactly equal
   one of the five taxonomy labels (§2) — a bare placeholder like "—"
   fails this the same as a compound value like "Rebuild + Persist
   (mixed)" would; the contract requires exactly one label per item, not
   zero and not several.
5. Separately fails on any of the four registry-table access forms
   above (dot, bracket, `require(...)`-chained, or `package.loaded[...]`-
   chained) that is NOT itself a direct call (e.g. stored in a local or
   table field and invoked through that alias later) — extraction can
   only trace direct calls, so an alias would otherwise register a module invisibly to the
   audit. Rather than attempting to trace what an alias eventually gets
   called with, this makes the alias itself the failure: the codebase's
   registration convention is direct calls only. String/long-bracket-
   literal-SPAN-aware (not a blanket strip, which would also destroy
   the legitimate `["register"]` bracket key) so a string's own text —
   the registry's own validation error message, which happens to
   contain "saveModules.register", or an unrelated long-bracket string
   literal like `[[saveMods.register]]` — is never mistaken for a live
   reference to the function.
6. Separately fails on any `require("scripts.lib.save_modules")`
   result, any `package.loaded["scripts.lib.save_modules"]` result (the
   identical singleton table under its second legitimate spelling — see
   item 2) — OR the already-canonical bare `saveMods`/`saveModules`
   name — that escapes to something other than the two sanctioned
   patterns above (chained straight into `.register` access, or (re-)
   bound to a variable literally named `saveMods`/`saveModules`) — e.g.
   `local registry = require("scripts.lib.save_modules");
   registry.register(...)`; one hop further,
   `local saveMods = require(...); local registry = saveMods;
   registry.register(...)` (re-aliasing the ALREADY-sanctioned name
   into a second local); without even a `local` keyword at all —
   `registry = saveMods; registry.register(...)` — since Lua's `=` is
   unambiguously assignment (never comparison — Lua has no C-style
   `if (x = y)` confusion, assignment is a statement, not an
   expression), so a bare/global re-assignment is just as live a bypass
   as the `local` form; or re-aliased into a TABLE KEY rather than a
   bare identifier at all — `holder["registry"] = saveMods;
   holder["registry"].register(...)` or its dot-field sibling
   `holder.registry = saveMods; holder.registry.register(...)` — the
   assignment-target grammar covers Lua's full (finite) `name`/
   `name.field`/`name["key"]` chain forms, not just bare identifiers,
   so the escape is caught regardless of which one carries it. This
   grammar broadening required one explicit carve-out:
   `package.loaded["scripts.lib.save_modules"] = saveModules` is Lua's
   own universal require()-caching idiom (used by `save_modules.lua`'s
   own definition, not a bypass attempt), so a target starting with
   `package.loaded` is excluded rather than letting the general
   table-key case flag it. Every `package.loaded["scripts.lib.save_modules"]`
   occurrence gets the SAME escape tracking as `require(...)` (a direct-
   call-only receiver isn't enough on its own — `local registry =
   package.loaded[...]; registry.register(...)` re-aliases the table
   itself, not just its `.register` function, through a spelling the
   `require(...)`-only escape check never looked at): sanctioned if
   chained straight into `.register`/`["register"]` access, bound to a
   local named exactly `saveMods`/`saveModules` (tolerating the real
   definition file's own `local saveModules = package.loaded[...] or
   {}` fallback), or is itself the ASSIGNMENT TARGET of the cache-write
   idiom above (a `package.loaded[...]` occurrence immediately followed
   by `=` is being written to, not read from, so it can't itself be the
   source of a new alias) — anything else is a hard failure, same as
   the `require(...)` case. (The RHS check requires the canonical name
   to be truly BARE, with nothing chained after it at all — not just
   `.register`/`["register"]` — since Haskell/Lua's `\b` word-boundary
   is satisfied by a following `.` too: without this, the registry's
   OWN reload-safety idiom, `saveModules.registry = saveModules.registry
   or {}`, would be misread as "bare `saveModules` aliased into
   `registry`" when it's really a sub-table field assigned to itself.)
   Binding the registry table to an arbitrarily-named variable is a
   genuine data-flow problem no fixed-name regex can trace (Lua allows
   any identifier, and allows aliasing an alias), so rather than trying
   to enumerate every possible name or chase arbitrary aliasing depth,
   the escape itself — the registry table reaching anything other than
   the two sanctioned patterns — is the failure. A THIRD level of
   aliasing (re-aliasing the second variable yet again) is a known,
   accepted limitation of this static, non-interpreting approach — at
   some point, tracing arbitrary-depth data flow through regex matching
   stops being a tractable improvement and starts being a hand-rolled
   Lua interpreter; this audit deliberately stays a static guard, not a
   serialization-correctness proof (see the opening of this section).

Run it directly:

```bash
python3 tools/persistence_inventory_audit.py
```

It is wired into `make ci` (`tools/ci-local.sh`) and
`.github/workflows/ci.yml` alongside `tools/test_audit.py` and
`tools/lua_module_budget.py` — the same "cheap, no-engine, always-on"
gate tier — so it has teeth: a PR that adds a field to `EngineEnv` or
`WorldState`, or registers a new Lua save module, without adding a
matching inventory row fails CI. Its own test suite
(`tools/test_persistence_inventory_audit.py`, mirroring
`tools/test_audit.py`'s pattern of testing `world_audit.py`'s pure
functions against synthetic input) feeds the extraction/parsing
functions synthetic Haskell record text and a synthetic inventory
missing an entry, and asserts the audit reports the gap — this is the
"detects an intentionally introduced unclassified root state owner" test
the acceptance criteria requires.

The audit does not — and cannot, statically — verify that a "Persist
exactly" field is actually wired into `toWorldPageSave`/
`fromWorldPageSave` or the equivalent. That is a serialization-
correctness question for the runtime children (A2, B1) and their own
tests, not this contract's job.

## 8. Out of scope

The following are explicitly not addressed by this issue, and no code
in this PR touches them:

- Implementing the coordinated snapshot barrier (A2).
- Replacing the save-file encoding (B1).
- Implementing per-component migrations.
- Changing runtime load publication.
- Adding autosaves, save slots, cloud storage, a lossy recovery mode, or
  any save/load UI.
- Generating placeholder textures or changing renderer asset fallback
  code (§4 only defines the *effect* on load validity).
- Guaranteeing identical outcomes when two copies of a save are resumed
  independently (§1 explicitly disclaims this).
- Any change to `wpsTimeScale`, `wpsToolMode`, or `pause.lua`'s
  `prevTimeScale` handling — the inventory documents their *target*
  classification for a future child to implement; this issue changes no
  runtime behavior.
- Changing `handleWorldLoadSaveCommand`'s current merge-unrelated-pages
  behavior (`LoadWorld.hs`, #191/#218) to the whole-session-replacement
  target in §1 — see "Divergence: current loading merges, it does not
  replace"; this issue documents the target and the gap, A2 implements
  it.

## Related

- #768 — the parent persistence-overhaul epic this issue is Phase 1 of.
- A2 (coordinated snapshot barrier) and B1 (new save envelope) both
  depend on this contract.
- #569 (splitting `World.Save.Types`) is settled by the format/component
  work that follows this contract, not by this issue.
- #214/#191/#218 — the completed multi-world persistence work whose
  merge-preserving load behavior this contract's session-replacement
  target (§1) deliberately diverges from; see "Divergence: current
  loading merges, it does not replace" above.
