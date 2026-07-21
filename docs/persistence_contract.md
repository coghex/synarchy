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
  memory. **Implemented by issue #763** (save-overhaul C2) — see the
  former-divergence callout immediately below, now resolved.
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

### Resolved divergence: loading used to merge, not replace

Before issue #763 (save-overhaul C2), this contract's "session
replacement, not merge" rule above was aspirational, not current
behavior. The pre-#763 load path
(`src/World/Thread/Command/Save/LoadWorld.hs`, #191/#218 — deleted by
#763) deliberately **preserved** any live world page that wasn't part
of the save being loaded: it computed the set of page ids the load
"owned" (the restored pages, their saved original ids, and whatever a
prior load of the same save registered), filtered every building/unit
down to that set, and explicitly kept everything else — the removed
code's own comment named this "off-page ... genuinely unrelated live
pages, which we keep (#191)". That was a merge, not a session
replacement (#191 was itself a bug fix at the time — the prior behavior
of dropping unrelated pages was the defect it fixed).

Issue #763 replaced that whole load path with a staged, atomically
published whole-session transaction (`World.Load.Stage` +
`World.Load.Publish`): a load now REPLACES the complete session — every
saved page keeps its own id verbatim (no more remap to `main_world`, no
`<id>#N` collision suffix, no `loadProvenanceRef`), and a page that was
live only before the load (never part of the save) does not survive
publication. `wpsTimeScale`/`pause.lua`'s `prevTimeScale`/`wpsToolMode`
remain the three still-current v82-vs-new-format field-level
divergences documented in `persistence_state_inventory.md`'s summary;
this was the fourth, and it is now closed.
`tools/transactional_load_probe.py` is the same-process load test this
section used to call for: it asserts, within one running engine, that a
page live only pre-load does not survive a real published load,
alongside the mutual-exclusion, missing-definition-rejection, and
no-ghost-accumulation-on-repeat guarantees the transaction also
establishes. `tools/multiworld_save_probe.py`'s gold-standard save →
quit → fresh-restart → load round trip (which starts every run with
zero pre-load pages, and so could never itself exercise merge-vs-replace)
is unaffected and still passes unmodified in spirit — only its two
pages' saved ids stopped being remapped to `main_world`.

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
  (`src/World/Save/Types.hs`) — historically the ground truth of what a
  save persists. #759 (save-overhaul B1) made `SaveData`/`WorldPageSave`
  ride as a single transitional "session" component inside the tagged
  `World.Save.Envelope` container; #760 (save-overhaul B2) then RETIRED
  that monolithic component and split gameplay state into independently
  versioned, Haskell-owned components (`World.Save.Component.*`, see
  `persistence_state_inventory.md` §10). `SaveData`/`WorldPageSave` are
  therefore no longer any wire contract — they survive only as a
  transitional IN-MEMORY bridge into the world-thread load path
  (`snapshotToSaveData`). `SaveMetadata` still rides standalone as the
  "metadata" component, and `SaveHeader` still describes the container's
  fixed 16-byte framing header (magic + envelope version + manifest
  length). All four remain root-owner records this contract's audit
  tracks; §10's component owners are audited additionally, against the
  live component registry.
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

- B1 (#759) landed the new tagged envelope (`World.Save.Envelope`) with
  an independent framing version
  (`World.Save.Envelope.currentEnvelopeVersion`, which B1 assigned and
  B2 leaves unchanged — the framing contract did not change). Since B2
  (#760) each GAMEPLAY component carries its OWN independent schema
  version (`World.Save.Component.Types`), so ordinary component
  evolution no longer bumps any global save version. `currentSaveVersion`
  (`src/World/Save/Types.hs`, bumped frequently — don't trust any number
  written down here) now versions only the transitional `SaveData`/
  `WorldPageSave` load bridge, which is not a wire contract; the
  historical global-version meanings (e.g. v83 = #785, v84 = #811) are
  preserved and never reassigned.
- **Pre-B1 (v82 and earlier) positional saves remain unsupported** and
  receive B1's clean-break diagnostic (an envelope-version/magic
  mismatch, no heuristic positional decoding attempted). Migrating THOSE
  is explicitly out of scope for every save-overhaul child, including
  #766 below.
- The **first completed new-format save (the B1 envelope, #759) is the
  compatibility baseline** — i.e. the format was not required to be
  stable *before* that point, only from it forward. Issue #766
  (save-overhaul C4) implements the machinery that promise requires:
  `World.Save.Compat.SessionV90` recognizes and migrates a real B1
  envelope (a single required `"session"` component wrapping the
  then-current `SaveData`/`WorldPageSave` shape) into the current,
  fully-validated `SessionSnapshot` — reusing the SAME per-component
  assembly helpers (`basePageSnapshots`/`applyWorldEdits`/etc.) the
  modern registry-driven path uses, so a migrated B1 session is
  reconstructed by identical code, not a parallel implementation.
  `docs/save_compat/manifest.json` is the machine-readable record of
  every baseline declared supported this way, its tracked fixtures, and
  a frozen-DTO fingerprint guarding against silent drift;
  `tools/save_compat_audit.py` (wired into `make ci`/CI) enforces it, and
  `tools/save_compat_migration_probe.py` proves a tracked fixture
  survives a real load→publish→resave→restart→reload round trip.
  The intervening #760-only transitional shape (Haskell already split
  into components, but Lua state still the pre-#761 opaque blob map) IS
  a declared compatibility baseline (`b2-split-haskell-lua-state`,
  requirement 3): `World.Save.Envelope.decodeB2SessionEnvelope`
  recognizes an envelope shaped exactly `{metadata, every Haskell
  gameplay component, "lua-state"}` (every descriptor required, at its
  genuine historical version — the same exact-shape precision B1's own
  fallback uses) and reuses `assembleSnapshot` UNCHANGED for the
  Haskell side, since it's already the modern per-component registry.
  The opaque `"lua-state"` blob (the documented pre-#761
  `sdLuaModules`/`snapLuaModules` shape, a cereal-encoded `HashMap Text
  Text`) is decoded and validated as that map: a genuinely EMPTY map
  (the common real case) migrates cleanly, defaulting every current Lua
  module the same way a migrated B1 session does; a well-formed but
  NON-empty map, or genuinely malformed bytes, are both refused rather
  than silently discarding or misinterpreting persisted Lua state — the
  pre-#761 Lua deserializer that could honestly interpret non-empty
  state was removed by #761 and never comes back.
- From the B1 baseline on, every subsequent format change requires an
  explicit per-component migration: translate changed fields, supply
  deliberate defaults for new fields. No more silent breaks. The
  raw-to-typed-reference transition (issue #764, save-overhaul C3;
  `craft-bills`/`power-nodes`/`unit-sim` components v1→v2) is the first
  real example of this machinery in the modern, already-split component
  set — `docs/save_compat/manifest.json`'s `c3-raw-reference-v1` baseline
  documents it as already supported without a new migration function
  (the existing `ccInputVers`-dispatched decode already handles it),
  backed by a tracked fixture proving the v1 payloads decode through the
  real envelope/registry path. The first versioned Lua components
  (issue #761, save-overhaul B3) are likewise documented as a
  `b3-lua-versioned` manifest entry, backed by real tracked v1 payload
  fixtures for both `unit_ai` and `building_spawn` (encoded through the
  genuine `scripts/lib/data_codec.lua` via a real HsLua VM, not a
  reimplementation of the wire format) that
  `test-headless/Test/Headless/Lua/SaveModules.hs`'s "tracked v1
  fixtures from disk" gate reads straight off disk and runs through the
  real `saveModules.prepareLoad`/`applyAll` path — on top of, not
  instead of, that same file's existing exhaustive synthesized-payload
  coverage of the full v1/v2/v3 contract.
- An unsupported or malformed save must fail clearly (a readable error
  naming the expected vs. found version, mirroring the existing
  `currentSaveVersion` mismatch behavior) without partially modifying
  the live session — this is the same publish-after-validate rule from
  §1 applied to the format layer specifically.
- **Unknown optional components** (an id present in a save's manifest
  that this build does not recognize, and which the writer itself
  marked optional) must never be silently discarded on the next save to
  the SAME slot. Rather than threading opaque payload bytes through live
  session state, `World.Save.Storage`'s publish transaction reads the
  slot's CURRENT authoritative generation before ever writing a new
  candidate: if it carries any component this build doesn't recognize,
  the whole publish is refused (`PhaseForeignOptionalData`) — the
  original file, and whatever it contains, is simply never overwritten.
  A different save-slot name remains unaffected.

## 6. Existing save/load test & probe disposition

**Correction (#767):** the claim below that "no hspec suite exercises
save/load" was accurate when A1 wrote it, but every subsequent child
(A3, B2, B3, C3, C4) added extensive PURE hspec coverage for the parts
of the pipeline that don't need a real multi-thread engine —
`Test.Headless.Save.Snapshot`/`.Barrier`, `Test.Headless.World.Save.
Components`/`.Compat`/`.Integrity`/`.Storage`, `Test.Headless.Lua.
SaveModules`, and now (#767, save-overhaul D1)
`Test.Headless.World.Save.Contract`'s "persistence contract" describe
group — a representative multi-page session (every designation kind,
nested items, a unit with wounds/skills/equipment, unit-sim state, a
building, a craft bill, a power node, and an identity) round-tripped
through the REAL production codec and compared via `SessionSnapshot`'s
derived `Eq`. What genuinely still can't run under hspec is the actual
multi-thread, multi-process, disk-touching FULL save/load transaction —
that remains headless Python probes (`tools/*.py`), including the two
new ones below.

**Primary save/load probes** — their whole purpose is testing the
persistence envelope itself:

| Probe | Covers | Disposition | Responsible future child |
|---|---|---|---|
| `tools/multiworld_save_probe.py` | Multi-page save/load (#214/#219), world-identity round-trip (#707), the gold-standard save→quit→restart→load pattern | **Retained (updated by #763)**, save-file-shape changes untouched by #759 (B1) needed no changes there. #763 removed the active-page-to-`main_world` remap it exercised, so its two pages now stay under their own saved ids — the probe's assertions were updated to match, not its structure. | Done (#763) |
| `tools/transactional_load_probe.py` | The genuinely new session-replacement-not-merge case, mutual exclusion, missing-def rejection, no-ghost-on-repeat (#763) | **Added by #763.** A same-process load test the multiworld probe structurally cannot be (that probe starts every run with zero pre-load pages): builds a live pre-load page never part of ANY save and proves it does not survive a real published load. | Done (#763) |
| `tools/save_pause_probe.py` | Pause/timescale invariant across save and load (#42) | **Rewrite by A2/#757.** A coordinated save keeps the engine paused and must not restore a prior simulation speed; the positional compatibility field remains decode-compatible until format work. | A2/#757 |
| `tools/lua_orphan_prune_probe.py` | Post-load reconcile of orphaned per-id Lua AI/spawn state (#195) | **Retain as-is.** Tests a Lua-side invariant (`onSaveLoaded` reconcile) orthogonal to the envelope's wire format; nothing in this contract changes it. | — |
| `tools/save_compat_migration_probe.py` | Every tracked complete-session baseline's fixture(s) (B1, the #760-only B2 transitional shape, C3's typed-reference/multi-page/items variants, B3's Lua-versioned session) load→publish→resave→restart→reload round trip (#766) | **Added by #766.** The one thing the pure hspec "save components"/"save compatibility" gates cannot prove: a real fixture on disk survives the normal whole-session transaction and a genuine process restart. | Done (#766) |
| `tools/persistence_contract_probe.py` | The compact, CI-eligible fresh-process end-to-end gate (#767): three real save→load→save cycles (a tiny worldSize-8 page) compared structurally (`SessionSnapshot` `Eq` + `lua.*` payload byte-equality) via `tools/persistence_snapshot.compare_session_files`, plus reset-policy and paused-stability-dwell checks | **Added by #767 (final Phase-4 child).** The final integration gate: proves the ASSEMBLED system (every A1-C4 piece together) honors the player-facing contract, not merely that each piece's own focused test passes. | Done (#767) |
| `tools/persistence_contract_sweep.py` | The broader manual sweep (#767): the SAME three-cycle fresh-process comparison against a real generated-world representative scenario (craft bill, mine designation, identity) | **Added by #767.** Requirement 15's "broader" tier — real (not tiny) worldgen scale; cross-references (does not re-run) the domain-specific probes below and the assembled-failure-contract probes for requirement 11/13 coverage. | Done (#767) |

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
   itself (a local named `saveMods`/`saveModules`, optionally wrapped in
   ANY number of redundant parens — `(saveMods).register(...)`,
   `((saveMods)).register(...)`, arbitrarily deep, are all exactly as
   direct a call as the bare form —, bracket-indexed
   `saveMods["register"](...)`/`saveMods['register'](...)`/`saveMods[
   [[register]] ](...)` (a QUOTED key OR a Lua long-bracket string key
   — `[[register]]`/`[=[register]=]`/... — the same long-bracket string
   form already tolerated for the module-NAME argument, applied to the
   access KEY too),
   `require("scripts.lib.save_modules").register(...)` chained directly
   off its own `require()` with no local binding at all, and
   `package.loaded["scripts.lib.save_modules"].register(...)` (or its
   bracket-indexed sibling `package["loaded"]["scripts.lib.save_modules"]
   .register(...)`, since `loaded` is itself just an ordinary field on
   `package` reachable the same dot-vs-bracket way as `.register`) chained
   directly off the exact cache slot `require()` itself reads/writes —
   all ordinary, fully traceable Lua). The module-PATH string itself
   (`"scripts.lib.save_modules"`, `require()`'s argument and
   `package.loaded[...]`'s index) accepts a long-bracket form too —
   `require([[scripts.lib.save_modules]])`,
   `package.loaded[ [[scripts.lib.save_modules]] ]` — one shared
   fragment used by BOTH `require(...)` and `package.loaded[...]`
   everywhere this literal string appears, so the two can't drift apart
   the way `package.loaded`'s own dot-vs-bracket spellings did before
   they shared a fragment (round 21).

   Both `require(...)` and `.register(...)` also recognize Lua's
   function-call SUGAR: a call whose sole argument is a bare string
   literal needs no parens at all — `require "scripts.lib.save_modules"`
   and `saveMods.register "modname"` are exactly as valid, and exactly
   as live, calls as their parenthesized equivalents (a real, common Lua
   idiom, especially for `require`). Recognized as fully SEPARATE
   compiled patterns from the parenthesized forms (not an optional `(`
   folded into the existing ones) specifically to avoid shifting the
   parenthesized patterns' own group numbering, which extraction
   depends on positionally. No extra "complete argument" check is
   needed for the sugar form the way the parenthesized module-NAME
   argument needs one (item 7): sugar syntax syntactically permits ONLY
   a single string (or table) literal as the entire argument list, so a
   computed/concatenated name isn't even expressible this way.

   Extraction also covers all three Lua string-literal forms for the
   module NAME (the argument to `.register(...)`) — `'...'`, `"..."`,
   and long brackets `[[...]]`/`[=[...]=]`/... — fully
   string-literal-aware in both directions: a
   `--` embedded in a quoted OR long-bracket string is never mistaken
   for a comment start (which would otherwise truncate the line and
   hide a real call after it), and a bare (non-`--`-prefixed)
   long-bracket span is recognized as a string, not code. The extractor
   ALSO discards any call-shaped match whose start falls inside an
   unrelated string-literal span, so a doc string whose text merely
   LOOKS like a registration (`[[example: saveMods.register("x", nil,
   nil)]]`) is never extracted as a live one — only a real call's own
   receiver, which is never itself inside a string, is ever excluded
   from that filter. The literal itself must be the COMPLETE first
   argument — immediately followed by the arg-separating comma or the
   call's closing paren, never concatenated with further expression
   text — so a call like `saveMods.register("unit_ai" ..
   "_untracked", ...)` is never misread as a harmless re-registration
   of the classified literal prefix "unit_ai" when it actually
   registers a different, unclassified runtime name (see item 7).
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
   chained) that is NOT itself a direct call — parenthesized OR
   paren-free sugar, per item 2 (e.g. stored in a local or table field
   and invoked through that alias later) — extraction can
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
   table-key case flag it — recognizing EITHER spelling of `loaded`
   itself (dot-accessed `package.loaded`, or bracket-indexed
   `package["loaded"]`/`package['loaded']`, one shared fragment used
   everywhere `package.loaded` is referenced in this scanner, so the
   two spellings can't drift apart the way earlier fixes did before
   they shared a fragment). Every `package.loaded["scripts.lib.save_modules"]`
   occurrence (either spelling) gets the SAME escape tracking as `require(...)` (a direct-
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
   to be a COMPLETE value — bare or wrapped in redundant parens per the
   table-constructor case below, but with nothing CHAINED after it at
   all — not just `.register`/`["register"]` — since Haskell/Lua's `\b`
   word-boundary
   is satisfied by a following `.` too: without this, the registry's
   OWN reload-safety idiom, `saveModules.registry = saveModules.registry
   or {}`, would be misread as "bare `saveModules` aliased into
   `registry`" when it's really a sub-table field assigned to itself.)
   Also fails on the canonical name hidden as a TABLE CONSTRUCTOR
   field's value — `{ [1] = saveMods }` (explicit bracket key),
   `{ saveMods }` (positional, Lua's implicit-integer-key array-
   constructor form), or `{ registry = saveMods }` (named key) —
   structurally different from a `TARGET = value` assignment statement
   (a `{`/`,`-delimited entry inside a table literal, not a standalone
   statement), so it needs its own pattern keyed off the `{`/`,` that
   opens a value position rather than an `=` that closes an assignment
   target. The canonical name must be the COMPLETE entry (bare or
   parenthesized to any depth — `{ [1] = (saveMods) }` is exactly as
   live as the unparenthesized form, and shares one fragment with the
   plain-assignment RHS check so the two can't drift apart the way they
   initially did — with nothing chained after it, immediately followed
   by the next `,` or the constructor's closing `}`) — so
   `{ saveMods = require(...) }`, where the canonical name is used as a
   KEY whose VALUE is something else entirely, is correctly not
   mistaken for aliasing.
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
   The same limitation covers every OTHER Lua construct capable of
   binding the registry table to a name/location this audit doesn't
   specifically pattern-match: multiple assignment
   (`local a, b = 1, saveMods`), a function-call argument
   (`store(saveMods)`, where `store` does the aliasing internally), a
   for-loop iteration variable (`for _, v in ipairs({saveMods}) do
   v.register(...) end`), a closure upvalue, a coroutine, or a
   metatable `__index` proxy. This audit targets the codebase's ACTUAL
   registration convention (a direct call or one sanctioned local name,
   verified against all 4 real call sites) rather than every
   theoretically expressible Lua aliasing construct; a determined
   author could always find one more syntactic form this audit's
   pattern matching hasn't special-cased. Closing this class of gap
   completely would require actually interpreting Lua, not extending a
   regex — a code-review norm (register through the sanctioned local,
   nothing cleverer) is the intended backstop past this point, not
   further additions to this tool.
7. Separately fails on any direct `.register(...)`/`["register"](...)`
   call (any of the four receiver forms above) whose module-name
   argument is NOT a complete, standalone literal per item 2's
   completeness check — e.g. a concatenation
   (`"unit_ai" .. "_untracked"`), a variable, or any other computed
   expression. `saveModules.register` (the real function) accepts and
   stores whatever string the argument evaluates to at RUNTIME; tracing
   an arbitrary Lua expression to that string is real interpretation
   territory, the same reasoning that makes an untraceable alias itself
   the failure in items 5–6. Rather than silently ignoring such a call
   (which would leave a new, unclassified runtime module invisible to
   the audit) or worse, matching just a literal PREFIX and treating
   that as the whole call, the call itself is the failure — the
   codebase's real registration convention is a plain literal name at
   all four known call sites. Excludes the registry's OWN `function
   saveModules.register(name, serializeFn, deserializeFn)` DEFINITION
   site, which is syntactically indistinguishable from a call to a
   receiver-plus-open-paren matcher (a Lua parameter list looks
   identical) but is a declaration, not a registration.

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
- Changing `handleWorldLoadSaveCommand`'s then-current merge-unrelated-
  pages behavior (`LoadWorld.hs`, #191/#218) to the whole-session-
  replacement target in §1 — see "Resolved divergence: loading used to
  merge, not replace"; this issue (#756) only documented the target and
  the gap. **Implemented since by issue #763** (save-overhaul C2), which
  replaced `LoadWorld.hs` with `World.Load.Stage`/`World.Load.Publish`.
- Exhaustively pattern-matching every Lua construct capable of aliasing
  the save-modules registry table — multiple assignment, function-call
  arguments, for-loop variables, closures, coroutines, and metatable
  proxying are all real gaps in the audit's Lua scanner (§7 item 6),
  left unaddressed by design past the point where closing them would
  mean interpreting Lua rather than pattern-matching it. (Table
  constructor fields — `{ [1] = saveMods }`, `{ saveMods }`,
  `{ registry = saveMods }` — WERE closed, since that specific escape
  route was concretely demonstrated in review; it's listed here only as
  a boundary marker for what's covered vs. not.) The scanner covers the
  codebase's actual registration convention (a direct call or the one
  sanctioned local name); a code-review norm, not further regex, is the
  intended backstop for anything cleverer.

## 9. Typed persistent references and the shared integrity graph (#764, save-overhaul C3)

Every durable cross-component reference now declares its expected
target kind and scope rather than remaining an untyped raw id (§7's
audit enforces this going forward — see below). Two modules carry the
vocabulary and the checks:

- `World.Save.Reference` — the leaf vocabulary (`RefKind`/`ContentKind`/
  `RefScope`) plus `SamePageRef`/`CrossPageRef`, thin wrapper newtypes
  a component DTO field uses to declare "this reference's target must
  live on the same page as the record carrying it" (or an explicitly
  permitted cross-page target) at the TYPE level. Wire-identical to the
  wrapped id — see its module haddock for why a same-page reference
  needs no new bytes on disk, only a new Haskell type.
- `World.Save.Integrity` — `IntegrityError`/`IntegrityReport` (the
  shared structured-diagnostic shape: component, version, data path,
  reference kind/value, expected vs. actual scope, a stable code, and a
  message — requirement 10's deterministic-order, capped-with-omitted-
  count report) and `sessionIntegrityErrors`, the NEW structural checks
  this issue adds over an assembled `SessionSnapshot`: a craft bill's
  station/claimant and a power node's host building are validated for
  wrong-PAGE (a target that resolves on a DIFFERENT page than the
  record referencing it is a hard error) while staying tolerant of
  absence from the whole session (the pre-existing #758 contract).
  `luaReferenceErrors`/`KnownEntities` do the analogous cross-check for
  every reference a Lua save component's `references()` hook reports
  (issue #761) against the load's real entity sets — always a
  non-blocking diagnostic, never load-rejecting, matching the same
  tolerated-dangling-reference precedent.

**Lua's persisted reference fields are typed on the wire too**:
`attackTargetUid` and every other field
`scripts/unit_ai_save_refs.lua`'s `unitAiReferences` declares (and
`scripts/building_spawn.lua`'s `lastUid`) are wrapped to a structured
`{__ref=kind, id=N}` shape at `snapshot()`/`decode()` time and unwrapped
back to a bare number at `apply()` time — both components bumped to
schema v2, with an unambiguous v1→v2 migration (v1 always meant exactly
what the declared field list already says, so there is nothing to
guess). This wrap/unwrap happens ONLY at the save-component boundary in
`unit_ai_save_refs.lua`/`building_spawn.lua`: `aiState`'s LIVE
in-memory shape never changes, so no other module (`unit_ai_combat.lua`,
`unit_ai_deliver.lua`, `scrubStaleRefs`, ...) needed any change.

**Deliberately NOT rewritten onto this vocabulary**: the nine existing
`missingXReferences` content-definition checks
("World.Save.Types"/`Engine.Scripting.Lua.API.Save`'s `continueLoad`)
stay as they are — already working, already tested, each against its
own IO-loaded content registry. They report through the SAME
`continueLoad` load-rejection gate the new checks report through (one
combined rejection message), which is what "the same integrity rules
at both boundaries" cashes out to operationally; their Haskell TYPES
were not unified into `IntegrityError`, a rewrite judged not worth the
regression risk for a vocabulary-only gain.

The persistence-inventory audit (§7,
`tools/persistence_inventory_audit.py`) enforces this going forward
exactly like it already does for root-owner fields and Lua save
modules: a new DTO field typed `SamePageRef`/`CrossPageRef`, or a new
Lua `kind` string reported by a `references()` hook, with no
classification row under `docs/persistence_state_inventory.md`'s
"Typed persistent references" / "Lua reference kinds" headings fails
the audit.

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
- #766 (save-overhaul C4) implements this contract's §5 compatibility
  promise: component migrations, `docs/save_compat/manifest.json`,
  `tools/save_compat_audit.py`, and
  `tools/save_compat_migration_probe.py`.
