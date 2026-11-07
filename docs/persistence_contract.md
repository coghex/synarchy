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
- **A post-publication reconciliation failure is its own disposition
  (#1204).** The step above is not the last one: publication is
  followed by the Lua `onSaveLoaded` reconciliation broadcast, which
  prunes orphaned rows, scrubs typed references, rebuilds derived
  statistics, and rebinds Lua's world/HUD identifiers to the published
  session. Each callback is isolated (one that raises can neither crash
  the Lua thread nor stop the callbacks after it), but a callback that
  partially mutates its own state and *then* raises leaves the
  already-published session incompletely reconciled. That state fits
  neither existing terminal — it is not a success, and it cannot claim
  the unchanged-old-session promise a pre-publication failure makes —
  so it reports as its own terminal phase,
  `Engine.Load.Status.LoadReconciliationFailed`, with a
  `LoadReconciliationIncomplete` outcome aggregating **every** failing
  module paired with its own error text (`engine.getLoadStatus()`
  exposes that as `reconciliationFailures`). `failedAtPhase` stays
  reserved for pre-publication failures, precisely because its presence
  is what says the old session survived. Recovery is loading again;
  nothing rolls the published session back.
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
establishes. Its mutual-exclusion window is ESTABLISHED rather than
raced for (issue #1181): the test-only `debug.armLoadStageGate` parks
the transaction on the world thread immediately before staging, the
probe positively observes that park by request id before checking
anything, and it FAILS if the window cannot be established. Before
#1181 those checks sat behind a timing branch that printed `[SKIP]` and
still exited 0 on a fast machine, so this paragraph described coverage
the probe could silently decline to run. `tools/multiworld_save_probe.py`'s gold-standard save →
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
| **Rebuild deterministically** | Never serialized, or serialized-but-ignored (see below). Reconstructed from persisted domain data plus current content (YAML defs, code) at load time — always the same result for the same inputs, but not itself the authority. |
| **Reset to a documented default** | Never serialized (or serialized-but-ignored, see below). Always takes a fixed default value after load, regardless of what it held pre-save. |
| **Exclude (transient runtime state)** | Never serialized, never restored, never given special load-time treatment. Purely a property of the running process. |

**Per-unit stats (#2761, qualified by #2831).** A unit's Float-valued stats
ride the `uiStats` map, which the units component serializes wholesale, so a
stat added to a script needs no codec change and is classified **Persist
exactly** like the rest of the map (stored zero and partial values come back
verbatim). A save written before the stat existed simply lacks the key, and
the load invents it ONLY where a declared rule says so. The physiology
registry's seeding rule fills the MISSING owned keys of a Simulated system at
load (never overwriting a present one, #2831, design D-6). In addition,
legacy unbodied units (#2893) qualify for absent-only reconstruction when
their loaded definition authors a `body:` block and their saved stat map has
none of `height`, `body_mass`, `lean_mass`, `fat_mass` or `frame_mass`. The
physiology load gate fills `height`, `body_mass`, `frame_mass`, `lean_mass`
and `fat_mass` by applying `seedBodyComposition` to the definition's standard
species means for `height`, `bulk` and `bodyfat`; it also fills absent
`strength_base` from the `strength` stat-template base only if neither
`strength` nor `strength_base` is present, and absent `endurance` from its
stat-template base. `standardStat` supplies each template's non-negative
base/mean, exactly as `SpawnStandard` does. Existing values win, definitions
without a template supply no value, and no other stats, skills or knowledge
are created. One normalization note records the backfilled keys and their
species-average source. A bodied unit with any saved body input but without
`height`, `body_mass` or `lean_mass` is malformed and rejects the load; it is
never completed by the legacy rule. The salt reserve `salt_store`
(`scripts/salts.lua`) is the worked case for physiology seeding: a save with
no reserve gets a full one at load, as a fresh spawn starts, and a stored
value, including 0, is kept. The cases are
`--match "salt reserve (#2761)"`'s `persistence` group and
`--match "persistence of the physiology modes"`.

**Serialized-but-ignored caches (#2831, #2893).** A stat can ride the `uiStats`
payload and still not be durable: it is a CACHE of other state, recomputed
canonically at the physiology load gate before missing physiology seeds are
computed, and again before the first paused frame. It is never read back as
authority. Missing body caches are logged as repairs; present stale cache
values are rewritten silently. This does not change durable body values.
It is classified **Rebuild deterministically**, with the qualifier
*serialized-but-ignored* in the inventory's classification cell: the
strength values the body recompute re-derives and the body-derived maxima.
(Circulation was one until S9, #2836, and consciousness, concentration and
state of mind were until S11a, #2838: they are now derived on demand and never
stored, like `salt_conc`, so a legacy stored copy is scrubbed at load in every
mode rather than ignored, logged once per unit and system. The mind's own keys
are durable: **Persist exactly** while the mind system is Simulated, with
absent-only seeding at load, a BREAK or EUPHORIC episode that lost its end given
`now` so the normal end-of-episode path closes it on the next tick, a BREAK
without a behavior given WANDER, and every present value kept exactly, stale
deadlines included. The blood volume (S10, #2837) is the dedicated
`uiBlood` field: Persist exactly while blood is Simulated, serialized-but-ignored
while Nominal, where the load rewrites it silently to the current canonical
maximum so a Nominal-blood unit saved by current code normalizes zero times.)
The wire is unchanged (the stat map is still serialized wholesale, so no
filter and no codec change), and the load simply does not trust those values:
`unitResources.onSaveLoaded` runs `unit.recomputeBody` as well (it no longer runs
any brain pass: every value that reads physiology is derived on demand, so a
load publishes no stored value of its own and writes no durable integrator,
which would fold an active modifier into a stored base even at dt 0).
Consequently a save taken a tick before
that recompute may hold a stale cache and a save taken after it a canonical
one, and the structural save comparison
(`World.Save.Physiology.withoutRebuiltCaches`, used by the save codec's
`compare`) does not count them. The same comparison does not count the stored
blood volume (`uisBlood`) of a unit whose `blood` override is Nominal (#2837):
the load rewrites it to the current canonical maximum, so a stale saved copy
(a body-mass change after the pin) and its rebuilt successor are one durable
state. A Simulated unit's volume stays compared exactly. The comparison is
registry-free, so it sees a pin and nothing else; the Persistence spec
guards that no shipped definition defaults blood to Nominal. Everything else in the physiology stat set is
durable and compared exactly: a Simulated system's pools and meters, the
override map and the unowned `tissue_recovering` flag are **Persist
exactly**; a Nominal or Unsupported system stores none of its keys; and the
derived `salt_conc` is never stored at all, computed on every read, with a
stored legacy value scrubbed in every mode at load. These are deterministic,
logged-once, idempotent repairs (D-6 (b), (c), (f), (g), (i), (j)), applied
at the load gate before anything is staged, so a unit that cannot be
normalized rejects the whole load with the old session untouched. The inventory's
`UnitInstance (physiology stats)` table names each stat.

**Per-unit physiology intent (#2829).** A unit's explicit per-system
physiology mode override (`uiPhysioOverrides`) is **Persist exactly**
and rides the `units` component from v4. Its presence is the intent: an
override equal to the species default is kept, and an absent one keeps
following the current default. Every older `units` version, and every
unit created before an S4 setter exists, yields an empty map. The
species defaults, the system registry and the profiles are **Rebuild
deterministically** from data. Saved physiology state that cannot be
honoured rejects the load before staging (an override for a system the
registry neither declares nor recognizes as retired or renamed; a
non-finite stored value of a Simulated system; since S4, a Simulated
override for a system the definition no longer supports and a required seed
or upkeep input that is zero, non-finite or missing with no legacy rule);
the cases are `--match "physiology override persistence"` and
`--match "persistence of the physiology modes"`. The setters exist since S4
(#2831): `physiology.setMode` / `clearMode` record and remove the override.

**Combat-policy bookkeeping (#2521).** The Lua combat-policy seam
(`scripts/unit_ai_combat_policy.lua`) keeps, per unit, the time its last
attack goal ended (the local self-defense cutoff) and the live attack goal's
occurrence identity. Both are **Reset to a documented default** (absent):
they live outside `aiState`, keyed by unit id (each occurrence carrying its
own id), never by AI-state table identity. They ride no save payload and no
`unit_ai` version, and `unit_ai_claims`' reset empties them on every
successful load and on session teardown, so a rewound uid inherits nothing. An occurrence's exempt classification is fixed once, from the
provenance its opener passes to `setGoal`: committed order, lash-out or
autonomous. Every ending, cutoff and continuation decision reads only that
stored classification. On a load, the same reset hook then opens a FRESH
occurrence for every restored attack goal and rebuilds its provenance from
the goal's durable saved `mentalLashoutActive` / `committed` fields. It does
this before any cleanup can clear those fields, and it carries no cutoff,
basis or authorization from the saving session. So a restored committed
order or lash-out still stamps no cutoff when it ends, and a restored
autonomous goal still does. With last-attacker memory also reset
on load, the intended
consequence is that a restored NON-HOSTILE self-defense attack goal ends at
its first decision: old damage cannot keep a fight alive across a load. A
hostile fight, a committed player/debug order and a lash-out are unaffected,
and saved withdrawal/hold state keeps its behavior. The case is
`--match "unit AI load reset"`.

A load that FAILS after `unit_ai` applied is unwound by `saveModules`
re-applying the old session's rows. The seam survives that rollback too.
`unit_ai`'s forward apply copies the registry into an in-memory stash, and
its `"rollback"`-phase apply puts the copy back. The stash is never saved,
is overwritten by the next forward apply and is cleared on teardown. So the
restored session keeps its exemptions, cutoffs and occurrences, including
when the load reset had already re-seeded them before a later reset hook
failed. The cases are `--match "survives a load that rolls back"`.

**The component apply phase (#2521).** `saveModules` calls every component's
`apply(data, entities, phase)` with `phase` `"forward"` for the load's own
apply, and `"rollback"` at both rollback call sites:
- the failing component's own restore;
- the unwind of every component applied before it, including after a
  reset-hook failure.

A rollback apply receives the component's own pre-load snapshot and no
entity context. A component with transient state keyed to its rows may use
the phase to restore that state as well; one that ignores it is unaffected.
Reset hooks are still never compensated (#1200). So a load whose envelope
carries no data for a component, and is then abandoned by a later failing
reset hook, cannot restore that component's transient state. The gate is
`--match "the apply phase (#2521)"`.

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
- `UnitManager` (`src/Unit/Types/Manager.hs`), `BuildingManager`
  (`src/Building/Types.hs`) and `UnitThreadState`
  (`src/Unit/Sim/Types.hs`) — the three gameplay managers `EngineEnv`
  reaches through a bare IORef pointer (`unitManagerRef`,
  `buildingManagerRef`, `utsRef`). Those pointer fields are themselves
  root-owned and classified `Rebuild`, delegating the real decisions
  onto these records' own fields, so scanning only the pointer would
  land that delegation in unenforced territory: a field added inside an
  already-reachable manager would pass every gate with no persistence
  decision recorded. Added by #1703; their fields are classified in
  `persistence_state_inventory.md` §5.
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
be wired into to be reachable from a running session at all, plus the
three managers an existing pointer on that set already delegates to. It
is a fixed allowlist, never a transitive closure: records reached
THROUGH a scanned one (`UnitInstance` via `umInstances`,
`BuildingInstance` via `bmInstances`, `UnitSimState` via `utsSimStates`)
are inventoried separately rather than scanned, and the worker-thread
state nothing on `EngineEnv`/`WorldState` reaches at all stays outside
the boundary entirely (`persistence_state_inventory.md` §6). A new
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
| `simQueue` | `SimCommand` | Drain before snapshot; the terrain/fluid results these commands cause are durable (`wsEditsRef`, tile data), the queue is pure transport. Fluid results persist as EXACT `WeSetFluidSnapshot` surfaces, never an integer view; their precision contract, diagnostics and fresh-process gate are in [engine_contracts.md](engine_contracts.md) §Fluid precision: diagnostics and integration contracts (#2535). | A2 requirement. |
| `screenshotRequestQueue` | `ScreenshotRequest` | Cancel before boundary | A pending debug screenshot request has no gameplay meaning. |
| `combatEventsRef` | `CombatEvent` (`TVar`, drained by the combat-log panel) | Cancel/reset — drop whatever's queued at the boundary | These are notification-only fan-out to the UI, not a source of gameplay truth: the wounds/HP/kill facts a combat event *describes* already live durably elsewhere (unit/injury state); the event itself is a one-shot "tell the log panel" message with no independent meaning once dropped. |
| `injuryEventsRef` | `CombatEvent` (fall/injure/death kind, `TVar`) | Cancel/reset | Same reasoning as `combatEventsRef` — the wound/death it describes is durable via unit/injury state; the notification itself is not. |
| `thoughtEventsRef` | thought/mood notification (`TVar`) | Cancel/reset | Notification-only; the mood/psychology state it describes (if any) is durable elsewhere, not in this stream. |
| `actionOutcomeRef` | action-result notification (`TVar`) | Cancel/reset | Notification-only, same reasoning. |
| `eventStoreRef` | player-event ring buffer (`TVar`, multi-writer) | Cancel/reset — the ring buffer starts empty on load, and empty again for the world created after an Exit to Menu (#2291): the rows are dropped at BOTH session boundaries, by `World.Load.Publish.resetTransientState` and by `Unit.Thread.endSessionEpoch`. The `esNextSequence` counter survives both, so no sequence is reissued within one engine process (#1714). | The event LOG panel's history is explicitly session-only (contract §5's "temporary popups" spirit extends to the notification log); nothing durable is derived from a *past* log entry, only from the state changes that produced it, all covered elsewhere. |
| `debugQueue` (`TQueue DebugCommand`, `src/Engine/Scripting/Lua/DebugServer.hs`) | queued debug-console command: Lua text, response channel, and the `DebugCommandState` lifecycle cell | Cancel/reset | An open debug-shell session and any command mid-flight have no gameplay meaning (contract §5: exclude debug-shell contents). "Cancel" here is the literal `cancelDebugCommand` transition (#2282), not merely dropping the queue: a cancelled command is `DebugCommandCancelled` and can never afterwards be claimed, so it cannot execute against the session that replaces this one — which is exactly what the load handoff needs (#763). At the load-transaction boundary the waiting client really is answered rather than left hanging (`REJECTED: …`). At TEARDOWN it is not: both drains stop the console first (#2170), closing and killing every client handler and joining each one that exits within the stop's total bound (#2689; a straggler is logged and left behind, and a command it queues after the drain is never claimed), so a socket client sees its connection close and the `engine shutting down` / `ERROR: Lua thread crashed: …` reply reaches only an in-process waiter — the cancellation, not the reply, is what teardown contributes. |

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
- One deliberate exception, and its boundary (#2305). A saved unit
  references infection definitions in **two** places, and the two are
  handled differently. A wound's `woundInfectionType` names an infection
  the unit is *currently* carrying, and drives its treatment and
  progression, so an unresolved one is a missing gameplay definition in
  the full sense above: `World.Save.Types.missingInfectionReferences`
  inventories it and the load is rejected. A `uiImmunities` key is a
  *memory* of an infection the unit already survived; an unresolved one
  has no surface but the raw id the Status tab would print, and no
  gameplay meaning until the id is reclaimed by different content. It is
  therefore **scrubbed during staging** — dropped from the units the load
  publishes, logged with the count of entries removed and the
  unresolved ids, never a reconciliation failure and never a load
  rejection — on the same terms as the container-knowledge scrub of
  #1087 (`World.Load.Stage`). Entries whose keys resolve are
  restored with their exact levels; a session loaded this way saves back
  without the orphan. The asymmetry is blast radius: inventorying an
  immunity key would make removing one infection definition reject every
  save in which any surviving unit still holds un-decayed immunity to
  it, while the wound case needs a unit that is infected right now.
- A saved **flora condition record** (#2549, first carried by
  `world-activity` v7 and unchanged in v8) names
  its species by authored name, so a record for a species the loading
  build lacks fails the load exactly like any other flora reference
  (`missingFloraReferences`) — including a record for a wild occurrence
  whose chunk is not resident, which is why the record carries the name
  at all. A record whose occurrence cannot currently be resolved (its
  planting is not in the page's log, or its chunk is not resident) is the
  tolerated dangling case, not a failure; a record that contradicts its
  own page's planting, names a tile outside the page's canonical frame, or is
  malformed (reserved key, a tag outside its vocabulary, an expiry before
  its begin day, a successor that contradicts its render context) is a
  hard error naming the page and key.
  The appended occurrence markers `reseeded` and `absent` are valid only
  with their matching wild transient successor; cultivated expiry removes
  its row or plot instead of persisting a successor marker.
- A groundcover plot's **embedded condition** (#2552, `world-activity` v8)
  rides inside the plot value, so its species is converted by the same
  `traverseCropPlotSpecies` step as the plot's own and a condition naming a
  species the loading build lacks fails the load as a `crop plot condition`
  reference. It is judged at the component boundary on the same terms as an
  occurrence's record plus two that only a plot can be asked — the context
  is `cultivated`, and the record's species and tile agree with the plot
  carrying it — each refusal naming the page and the plot's tile as stored
  in the payload key. v6 and v7 keep the frozen three-field crop shape and
  every earlier retained layout its original legacy species form; every
  older plot migrates with no condition.
- Flora retention (#2557) is page-calendar state. Death records snapshot
  their selected policy and absolute whole-day expiry; transient windows,
  including the legacy 60-day default, count calendar days independently
  of plant health. Stored outcomes are not retimed by content changes.
  Wild reseeds retain an age anchor, wild absence retains a tombstone, row
  removal replays through an identity-specific `world-edits` entry, and an
  expired groundcover plot is deleted. The removal constructor is appended
  in `world-edits` v7 with frozen v6 migration; the condition record's
  field layout and `world-activity` version do not change. Compact a marker
  only when its removal preserves current and future presence, age,
  phase/stage, generation, and death boundaries.
- Producing placeholder textures and changing renderer fallback
  behavior are tracked separately (out of scope here); this contract
  only defines their effect on load *validity*, which is: none, for
  visuals.

### Arena pages: the base is reconstructed, and the foundation changed (#1718, #2707)

An arena page's terrain is not persisted. A save stores the page's
generation parameters (seed 0, the empty timeline and the 100000 world-size
sentinel that mark it as an arena) and its ordered edit overlay, so every
column no edit touched is RECONSTRUCTED on load from the page's recorded
seed and the chunk coordinate by `World.Generate.Arena.generateArenaChunk`
(`World.Load.Stage` rebuilds the 5×5 startup footprint; the camera loader
and the queued-demand drain reach the same authority through
`World.Thread.ChunkLoading.generateDemandedChunks` for everything beyond).
Because the base is a function and not data, changing the function changes
what an existing save reconstructs, and no component version or migration is
involved: the wire shapes are untouched.

#2707 made exactly such a change, deliberately and with the owner's
approval (D-55, D-56). Every unedited arena column is now twelve granite
cells under four loam cells, top at sea level, on every path. Before it,
the startup footprint had that stack but the camera loader and eviction
regeneration supplied ONE loam cell per column, so what a save reconstructed
depended on which path had produced each chunk. Consequences for saves made
by earlier builds:

- They load unchanged and reconstruct EVERY chunk on the uniform
  foundation, then replay their persisted edits. There is no legacy
  shallow-expansion profile and no attempt to recover which path a chunk
  once came from.
- Subsurface material appears where the old path had a single cell, and
  excavation outcomes below the old surface can differ. The former base is
  NOT preserved exactly; this is an accepted behaviour change, not a defect.
- The surface-vegetation variant of an untouched column is now drawn from
  a generator derived from the seed and the chunk coordinate alone. Grass
  variants may therefore differ from an earlier build's; that is permitted
  (#2065) and no cross-revision golden pins them.
- Loading, or reconstructing alone, never rewrites the save: the file's
  bytes are identical after a load (`tools/multiworld_save_probe.py --arena`
  hashes it). Explicit saves and autosaves publish as before.

The current behavior is covered by the Hspec group `arena generation
consistency`, which checks the uniform foundation with edits replayed, and
the fresh-process round trip in `tools/multiworld_save_probe.py --arena`.
The pre-release captured fixture is retired with the historical corpus and
remains only in git history.
`currentSaveVersion` was bumped to 107 as the worldgen-output bookkeeping
marker `src/World/CLAUDE.md` requires; it does not gate on-disk
compatibility.

### Finite arena bounds are page state (#2708)

An arena page may carry exact, centred, nonwrapping dimensions
(`world.initArena(pageId, width, height)`; omitted means the expandable
arena above). The size is part of the page's generation state, not of any
scenario: it is `World.Generate.Types.wgpArenaBounds` (`Maybe ArenaBounds`, a
validated width × height), written by `world-pages` v15 as
`pcArenaBounds` beside the page's gen params and restored by the same
component. Nothing else persists it: the rectangle, the partial chunks and
the closed fluid edge are all derived from it on load.

- **Wire and validation.** The wire carries the two dimensions only.
  Decode rebuilds the rectangle through `World.Arena.Bounds.mkArenaBounds`,
  the one validator the Lua binding and the scenario adapters use, so a
  stored rectangle cannot disagree with its size. A size outside the
  dimension domain (each an integer in `[1, 100000]`), or bounds on a page
  that is not an arena, is a `validatePages` failure (`wpBoundsErrors`);
  nothing is clamped or invented.
- **Legacy default.** `PageCoreDTOv14` (the pre-#2708 page core) is frozen
  and `migrateWorldPagesV14` carries every field and leaves the page
  EXPANDABLE. Every older arena save therefore loads as the expandable arena
  it always was, on #2707's corrected foundation. Generated worlds carry no
  bounds, before or after.
- **Restoration order.** Staging installs the bounds on the page's params
  before any chunk is built, builds only the startup chunks that intersect
  the rectangle, replays the overlay (an overlay edit outside the rectangle,
  possible only in a damaged save, is clipped away rather than replayed into
  terrain the map does not have) and publishes tile data that carries the
  rectangle (`wtdBounds`, a derived cache — see the state inventory). Both
  ordinary saves and autosaves capture the live params, so both restore
  identical bounds with no scenario file.
- **Loading never rewrites the file**, exactly as for expandable arenas.

The current behavior is covered by the Hspec group `arena finite bounds`
(real-codec round trips, staged restoration and the movement snapshot of a
hidden page) and the fresh-process finite arena in
`tools/multiworld_save_probe.py --arena`. The pre-release captured fixtures
are retired with the historical corpus and remain only in git history.
`currentSaveVersion` was bumped to 108 as the worldgen-output bookkeeping
marker; the bump does not replace the component migration above.
`currentSaveVersion` was bumped to 109 for #2548's new optional
`relation-causes` component; the bump marks the in-memory `SaveData` bridge
gaining `sdRelationCauses` and, as above, replaces no component migration.
`currentSaveVersion` was bumped to 110 for #2552's `WorldPageSave` layout
change (`wpsCropPlots` values gain the optional condition); as above, the
`world-activity` v7 → v8 migration is the compatibility mechanism.

## 5. Format-version policy

Before the first public release, this repository accepts the current save
format only. Pre-release saves are not a compatibility promise. A persisted
shape or meaning change advances every affected carrier version and disables
its prior-version route. Do not add historical DTOs, migrations, fixture
baselines, or acceptance paths for pre-release saves.

`tools/current_save_schema_audit.py` compiles the candidate and compares its
current schema record with the record in the committed base. The check runs for
every merged code revision, independent of path selection. Pull requests use
their base commit; push builds use the event's `before` commit. Local checks
compare against `origin/master` and include tracked working-tree edits. The
candidate is freshly compiled; the base record comes from the base ref. A
missing base record or an unavailable base fails closed. The tracked record is
`docs/save_compat/current_schema.json`, so changes to it are code-gated and
must not take a docs-only fast path.

The guard checks compiled wire declarations, live versions and accepted
dispatches. It cannot see custom serializers, external encoders, semantic changes, or Lua
payload meaning. When those change, explicitly advance the owning component
version, disable acceptance of the old version, and add current behavioral
coverage. `currentSaveVersion` describes the in-memory load bridge; it does not
protect the on-disk format. A compiled schema audit is not proof that serialization,
validation, staging, publication, or restoration works.

A component with an unsupported version, or a payload incompatible with its
current schema, fails before the incoming session is published. The outgoing
session remains intact and paused. Do not let `.prev` recovery turn an
incompatible generation into a loadable save; recovery remains for corruption.
The same boundary applies to Lua save modules. Keep required/optional component
semantics honest: a newly optional component needs a real default for every
current save that lacks it, and changing a required component to optional is a
format decision requiring behavioral coverage.

The routine current-save gates are:

- `python3 tools/current_save_schema_audit.py --self-test`
- `python3 tools/current_save_schema_audit.py --base-ref origin/master`
- `python3 tools/test_save_compat_audit.py`
- `cabal test synarchy-test-headless --test-options='--match "current save schema"'`
- `persistence_contract` probe, the CI core: three fresh-process save/load
  cycles through the real engine and compiled codec (engines A-D), plus the
  S11a mind-episode engines (an old-default save, its changed-default load
  repair and a second generation that repairs nothing)
- `persistence_system_cycles` probe, **manual-only and in no CI gate** (#2943):
  run it when changing combat engagement, exhaustion/sleep, cardio, blood or
  relation-cause persistence, or their save components
  (`python3 tools/run_probes.py --only persistence_system_cycles --exact`)

CI runs the compiled-schema audit on every code revision, using the actual
committed base revision. The local `tools/ci-local.sh` gate includes the same
check against `origin/master`, plus tracked edits. The selected Hspec group and
the current audit self-tests do not require a live-probe slot, and neither
persistence probe does: they are headless, so they take no probe-slot grant, and
the runner's host-wide resource locks cover concurrency (CLAUDE.md §Testing
tiers).

After #2943 a save-affecting pull request's CI runs the `persistence_contract`
core only (engines A-D plus the mind-episode engines; 265 s in one focused run
on 2026-11-07, against about 686 s for the old combined probe, whose system
cycles now take 642 s in `persistence_system_cycles`). The engagement,
exhaustion/sleep, cardio, blood and relation-cause cycles are no longer in any
CI gate; they live in `persistence_system_cycles` and run on request.

### Public-release checklist

Before the first public release, freeze the first public format as the
supported baseline. Capture real current-format fixtures, record the supported
baseline and any migrations explicitly, and add tests for each accepted older
format. Require the current-schema audit, fixture and migration checks, and
fresh-process restart gates for release changes. Keep historical acceptance
disabled until that release contract is reviewed and adopted. The pre-release
current-only policy does not itself establish public backward compatibility.

## 6. Current save/load test and probe gates

The current-schema guard and codec audit are complementary. The compiled
schema extraction checks declared wire shapes, versions, registrations and
acceptance. `save_compat_audit.py` verifies the retained current fixture with
the compiled codec. Neither replaces behavioral tests of round trips, refusal,
reference integrity, transaction staging or publication.

The focused headless suites are:

- `current save schema` — compiled extraction, custom-boundary coverage and
  current-format/refusal regressions;
- `save components` and `save envelope` — current component and framing
  behavior, including the still-useful runtime DTO conversions;
- `persistence reference integrity`, `atomic save storage`, and `persistence
  contract` — referential validity, generation publication and snapshot
  reconstruction;
- `Lua persistence components` — production Lua module registry and payload
  behavior.

The `persistence_contract` probe remains the required engine-level save/load
gate for persistence changes. Its core saves and loads through three fresh
processes, checks structural equality and Lua payload bytes, and covers reset
policy and paused-session stability; three further engines carry the S11a
mind-episode checks (the pre-S11b registry's old-default save, the load that
normalizes its changed defaults, and a second generation). The focused system
cycles (combat engagement, exhaustion/sleep, cardio, blood, live relation
causes) are the separate manual-only `persistence_system_cycles` probe: no CI
gate runs it, so anyone changing those subsystems' persistence or their save
components runs it. The broader `persistence_contract_sweep` and
feature-specific probes retain their own selection and ownership rules.

No historical fixture corpus, migration replay probe, or historical
audit/reproducibility gate is maintained before public release. Existing
runtime DTO and conversion code can remain where useful, but pre-release shape
changes do not require a new historical decoder, migration, or fixture. The
public-release checklist in §5 defines the future support boundary.

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

Since #2124 that command is a façade over four internal owner modules,
none of them a gate of its own and none run by CI or `tools/ci-local.sh`
directly: `tools/persistence_inventory_audit_haskell.py` owns item 1 and
the typed persistent-reference field discovery (the comment/literal-
aware Haskell record parser; its `extract_record_fields` is the single
canonical parser the EngineEnv capability audit imports too),
`tools/persistence_inventory_audit_lua.py` owns items 2 and 5–7 plus Lua
reference-kind discovery (unchanged in what it recognizes and refuses),
`tools/persistence_inventory_audit_policy.py` owns items 3–4 plus the
save-component registration and test-coverage-map checks (the inventory
document's parser, taxonomy and topology rules), and the data-only
`tools/persistence_inventory_audit_common.py` holds the repository
paths. The façade declares the root owners, reads each repository input
once, composes the owners in the fixed check order, and prints the
report; the scanners never read the repository or the inventory, and no
owner imports the façade. The run and self-test commands are unchanged.

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
  any save/load UI. (Interval autosave landed later, in #913, built
  strictly ON TOP of this contract: it reuses the same barrier, storage
  publication, component registry, and load path unchanged, and adds
  only a durable autosave/manual classification in the `metadata`
  component. Cloud storage and a lossy recovery mode remain out of
  scope.)
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
`unit_ai_deliver.lua`, the post-load `scrubStaleRefs` — since #1589 in
`scripts/unit_ai_reconcile.lua` — ...) needed any change.

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
- #2919 activates the pre-release current-schema policy in §5. Historical
  pre-release fixtures and migration auditing are retired; the public-release
  checklist starts support from the first public save format.
