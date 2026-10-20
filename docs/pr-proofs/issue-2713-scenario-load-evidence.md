# Issue #2713 (SCN-13): evidence for the paused scenario-load service

Validation record for [coghex/synarchy#2713](https://github.com/coghex/synarchy/issues/2713),
branch `issue-2713-scenario-load`. The contract is in
[engine_contracts.md §Scenario load](../engine_contracts.md#scenario-load-the-paused-session-transaction-2713) and
[scenario_format.md](../scenario_format.md#loading-a-scenario-session-scn-13-2713).
Production profile, `cabal-3.16.1.0`, no GPU, no game launched.

## What was decided, and the deviation from "prefer private construction"

Requirement 5 prefers private construction and staging, and D-43 / requirement 7
allow recovery to a fresh default flat arena when preservation would need
disproportionate machinery. The SCN-09..12 adapters commit through LIVE owners
(world-thread install commands with simulation seeding and designation
revalidation, the units' Lua paused-readiness hook, the live unit and building
managers), so a fully private session would need a second engine environment.
The service therefore stages and publishes only an EMPTY arena shell through the
existing transaction (barrier, owner parking, queue cutover, allocator and
incarnation reset, Lua reconciliation) and installs the content afterwards, under
the page-incarnation fence, while the request is still non-terminal. A failure
after the swap recovers to a flat arena through the same path and the request still
ends failed. The design question was posted to the manager before implementation;
no redirect came.

## Hspec groups (final head)

| Group | Examples | Result |
|---|---|---|
| `Scenario.Load` | 50 | pass |
| `Scenario` (all four SCN groups and this one) | 339 | pass |
| `transactional load` | 25 | pass (7 new status-contract examples) |
| `load exception terminalization` | 6 | pass |
| `save snapshot barrier` | 30 | pass |
| `Lua.PauseGate` | 5 | pass (new: `setPaused(false)` refused for the whole life of a SCENARIO load and accepted after; and decided under the pause lock, so a load admitted while an unpause waits still refuses it) |
| `LuaSaveLoaded reconciliation failure disposition` | 2 | pass |
| `load replacement clears transient session surfaces` | 7 | pass |
| `persistence contract` | 8 | pass |
| `autosave`, `Autosave`, `Pause` (autosave and pause gates) | 30, 4, 13 | pass |

`Scenario.Load` covers: an integrated scenario of every family (terrain, fluid,
flora, structure, building, location with its bound item, units with nested
inventory and non-default blood, hydration and calories, a ground item, finite
bounds, camera) read back exactly, paused, with tag queries across tile, plant,
building, location, unit and item categories through the engine's own evaluator;
replacing a populated session including reused runtime ids, an order, a clock that
had moved and a queued stale command; nine preflight refusals (missing,
directory, syntax, not a mapping, missing / malformed / newer / zero version,
failing migration) with the source bytes and modification time unchanged; the
refusal of a live gameplay page and the admission of a pageless direct boot;
supported malformed content (exact warnings, rejected-owner descendants absent,
required-reference cascade, no false whole-file failure); held in-flight requests
refusing a second scenario load, an ordinary load, an ordinary save, an autosave
(`engine.saveWorld(..., {autosave = true})`), a capture claim and an unpause, with
the active status, the generic status, the save barrier, the Lua queue counters
and the pause state unchanged; a save holding the barrier refusing a scenario
load; 300 rounds of six simultaneous claimants admitting exactly one, and every
claimant proven to take the admission lock; held owner acknowledgements (readiness
cannot precede completion; with the barrier held every acknowledged owner is parked
and mutates nothing); 7 pre-swap and 6 post-swap injected faults with the correct
preservation or recovery disposition, a page replaced under the install (the
incarnation fence), recovery failing two ways with both diagnostics kept, two
failing reconciliation callbacks with the third still attempted; stale completions
about an older request; and the ordinary-save integration below.

## Ordinary save, fresh process

The example "a fresh process restores the same session from the saved bytes alone"
loads the integrated scenario from a file, captures the page, writes the ordinary
envelope the real codec produces, deletes the source YAML and then runs the TEST
EXECUTABLE ITSELF as a second OS process, which decodes the envelope through the
real decoder and load stage and captures again. Command and output of a run:

```text
fresh-process command: .../synarchy-test-headless --match Scenario.Load/ordinary-save integration/fresh process child entry  [SYNARCHY_SCENARIO_LOAD_FRESH=<tmp>/synarchy-scenario-load-fresh]
fresh-process exit: ExitSuccess
fresh-process output tail: Scenario.Load / ordinary-save integration / fresh process child entry [✔] / 1 example, 0 failures
```

The child asserts that its capture equals the parent's (structures, buildings,
locations, units, ground items: bounds, tags, scenario identities, physical values
and the adapter projection a later capture needs), that the arena bounds and the
unit scenario references are the saved ones, and that the source file is absent. No
saved component changed, so no migration, frozen type or fixture was added.

## Audits and tool gates (all pass)

`tools/persistence_inventory_audit.py`, `tools/engine_env_capability_audit.py`
(no new `EngineEnv` field, no new full-access module), `tools/save_compat_audit.py`
(46 baselines, 52 fixtures), `tools/test_save_compat_audit.py` (103 members, with
reproducibility), `tools/enum_append_only_audit.py`, `tools/haskell_module_budget.py`,
`tools/lua_module_budget.py`, `tools/unicode_operator_audit.py`,
`tools/test_persistence_inventory_audit.py`, `tools/test_probelib.py` (the
terminal-phase set gained `LoadReplacementFailed`, here and in
`tools/transactional_load_probe.py`), `tools/test_pack_atlas.py` and
`tools/pack_atlas.py --validate-only --strict` (selected by the CI path gates).
`cabal build all synarchy-test-headless synarchy-test-graphical` succeeds. The
save-compat reproducibility run used the compiled `exe:synarchy-save-codec` only: no
engine, no port, no probe slot.

## Review round 1 (six findings, all fixed)

Codex requested changes at `e0ad119d0`. Each finding was reproduced by a new example and the example is killed by reverting its fix (rows
`unpause-checked-before-the-lock` through `paths-renumbered` below):

1. An unpause could overtake a load admitted on another thread: the refusal is now decided inside the player-intent lock (`withPlayerIntentUnless`).
2. A stale `LuaSaveLoaded` ran callbacks and cancelled console commands before being ignored: it is discarded first (`handleSaveLoaded`); the example dispatches one into the live handler and counts the callbacks.
3. Ready preceded the simulation consuming the installed content: a queue-ordered round trip (`awaitSimulation`) now precedes it; the stand-in simulation can be held, and a never-consuming simulation fails the request.
4. Items an installed unit carries were not bindable by a location: they now bind (a significant item as taken, a realized container; a pending carried shell is still refused).
5. A recovery whose staging or barrier failed was reported by the generic status as `LoadFailed`: `failLoad` is publication-aware.
6. Diagnostics were numbered among the survivors: entries are reported at their source paths (`SourcePaths`, returned beside the scenario).

The SCN-12 test glue now composes through `constructScenario` instead of a copy of its order, and all 335 `Scenario` examples still pass.

## Review round 2 (one finding, fixed)

Codex confirmed five of the six round-1 fixes and requested changes for the sixth: source paths covered only top-level entries, so a rejection reported below a rejected nested sibling (a building's second `storage` item, cascaded when the building is rejected) was numbered among the survivors. The decoder now records which source positions of EVERY nested list survived (`spSurvivors`), and `Scenario.Types.translateNested` rewrites each adapter diagnostic to the positions of the document. Covered by an end-to-end example (the reviewer's scenario), by the decoder's survivor record, and by an exhaustive check of every rejection pattern of a nested list up to six items; reverting either the recording or the translation fails them (rows `nested-translation-off`, `nested-survivors-not-recorded`).

## Mutation checks

One source mutation at a time, the `Scenario.Load` group run (the `shared exclusion`
examples for the three admission mutants), the file restored. Every mutant that
compiled was killed by an assertion, by the example named.

| Mutant | Result | First example that failed |
|---|---|---|
| `catalog-without-materials` | KILLED | ready state, loads an integrated scenario of every family and reports ready only with the exact content installed, paused |
| `verdict-always-testing` | KILLED | the session, outside a testing session, refuses to replace an ordinary gameplay session, constructing nothing |
| `camera-ignored` | KILLED | ready state, loads an integrated scenario of every family and reports ready only with the exact content installed, paused |
| `unit-z-fixed` | KILLED | ready state, loads an integrated scenario of every family and reports ready only with the exact content installed, paused |
| `terrain-not-installed` | KILLED | ready state, loads an integrated scenario of every family and reports ready only with the exact content installed, paused |
| `warnings-not-recorded` | KILLED | supported malformed content, loads the usable subset: exact warnings, rejected owners' descendants absent, cascades, no false whole-file failure |
| `ready-despite-failures` | KILLED | failure outcomes, content the page no longer admits, is a failure with recovery, never a ready request with warnings |
| `no-flat-arena-disposition` | KILLED | failure outcomes, after the session is replaced, recovers to a default flat arena, still FAILED, when a fault strikes at PointPublished |
| `reconcile-failure-still-constructs` | KILLED | failure outcomes, post-publication reconciliation, attempts every callback, reports the aggregate failure, and leaves the incompletely initialized replacement paused |
| `pause-not-imposed` | KILLED | shared exclusion, pauses synchronously at acceptance, before any decoding |
| `prepared-lua-load-not-aborted` | KILLED | failure outcomes, before the session is replaced, preserves the previous session when a fault strikes at PointStagingHandoff |
| `publishing-stage-missing` | KILLED | failure outcomes, before the session is replaced, preserves the previous session when a fault strikes at PointBarrier |
| `admission-lock-removed` | KILLED | shared exclusion, makes every claimant take the admission lock, so a check and its claim cannot be separated |
| `save-claim-ignores-load` | KILLED | shared exclusion, rejects every conflicting operation at once while a request is held, changing nothing about it |
| `load-claim-ignores-save` | KILLED | shared exclusion, rejects a scenario load while a save holds the barrier, and leaves the save alone |
| `published-failure-reported-as-preserved` | KILLED | failure outcomes, after the session is replaced, recovers to a default flat arena, still FAILED, when a fault strikes at PointPublished |
| `stale-update-unguarded` | KILLED | the session, stale completions, cannot finish or overwrite a newer request |
| `unpause-checked-before-the-lock` | KILLED | engine.setPaused load-in-flight gate (round 15 review, issue #763), setPaused(false) decides under the pause lock, so a load admitted while it waits still refuses it |
| `stale-reconcile-not-discarded` | KILLED | the session, stale completions, cannot finish or overwrite a newer request |
| `simulation-not-awaited` | KILLED | the simulation, has consumed the installed content before the request is ready |
| `carried-items-not-bound` | KILLED | items an installed unit carries, binds a significant slot to a carried item as taken, with no warning |
| `recovery-failure-reported-as-preserved` | KILLED | failure outcomes, when recovery fails too, reports the generic transaction as replaced, not as preserved, when the recovery's publish barrier fails |
| `paths-renumbered` | KILLED | diagnostic paths, name an entry by the path it had in the document, not by its index among the survivors |

One further mutant (the continuation never claiming a published request) was
dropped: it makes every example wait out its 60 second terminal bound.
