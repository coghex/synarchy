# src/World/Save/ — the save format

Loaded when you work under `src/World/Save/`. Authority:
[`docs/persistence_contract.md`](../../../docs/persistence_contract.md)
(what a save represents, how state is classified) and
[`docs/persistence_state_inventory.md`](../../../docs/persistence_state_inventory.md)
(field-by-field classification, enforced by
`tools/persistence_inventory_audit.py`, which fails when a new root
state owner, Lua save module, component, or typed reference kind lacks a
row). Read the contract before adding state to `EngineEnv`,
`WorldState`, `World.Save.Types`, the three gameplay managers
(`UnitManager`, `BuildingManager`, `UnitThreadState`), or
`scripts/lib/save_modules.lua`'s registry.

## Current-only save schema policy

Before the first public release, the repository accepts only the schema
recorded for the current source tree. Any shape or meaning change to a
persisted Haskell or Lua value advances each affected component's version
and disables its prior-version route. Do not add frozen DTOs, migration
routes, fixture baselines, or compatibility promises for pre-release
saves. Unsupported saves fail during staging, before publication, and leave
the old session paused. A structurally incompatible generation must not be
recovered from `.prev`.

`tools/current_save_schema_audit.py` compares the freshly compiled candidate
schema record with the record in the committed base. It is a compiled-schema
guard, not proof of successful serialization or restoration; the retained
Hspec groups and `persistence_contract` probe provide that behavioral
evidence. It cannot infer custom serializers or Lua payload meaning, so
those changes need explicit version updates and current behavior coverage.
`currentSaveVersion` describes only the in-memory bridge and is not an
on-disk compatibility guard.

See [`docs/persistence_contract.md`](../../../docs/persistence_contract.md)
§5 for the policy and public-release checklist.

## Architecture (epic #756-#768, landed)

- `World.Save.Snapshot.SessionSnapshot` is the immutable, validated
  in-memory capture (pure `captureSessionSnapshot`) — NOT the wire
  format. The save barrier (`Engine.Save.Barrier`) quiesces every
  state-owner thread, releases its capture lock only after the encode is
  forced (`evaluate`), and reports the outcome only after the disk write
  resolves.
- On-disk `world.synworld` is a tagged, checksummed component ENVELOPE
  (`World.Save.Envelope`): FNV-1a-checksummed manifest + independently
  versioned components (`core-session`, `world-pages`, `world-edits`,
  `world-activity`, `buildings`, `units`, `unit-sim`, `craft-bills`,
  `power-nodes`, `texture-palette`, `metadata`, the four OPTIONAL
  `container-knowledge`, `transfer-orders`, `portable-knowledge` and
  `relation-causes`, plus dynamic
  `lua.<module>` components). Registry:
  `World.Save.Component.saveComponentRegistry`. Every gameplay component
  is REQUIRED except those four, each of whose absence has an honest
  default — see `docs/persistence_contract.md` §5 before declaring a
  fifth. `portable-knowledge` (#2512) and `relation-causes` (#2548) are
  the SESSION-scoped optional components: each folds straight onto the
  `SessionSnapshot` rather than onto a per-page slice, because a crate's
  memory is keyed by the crate and a relation cause relates session-wide
  tags. `relation-causes` is captured from the global
  `relationCauseStoreRef` and replaced by the load publish on the
  `texPaletteRef` seam, and Exit to Menu empties it. Before public release, a persisted shape or meaning change advances every
  affected on-disk carrier version, which then accepts only its current version.
  Unchanged carriers retain their accepted versions. Do not add pre-release
  migrations or frozen DTOs. `currentSaveVersion` versions only the transitional
  in-memory load bridge (`SaveData`) and is not on-disk protection. `listSaves` decodes only the `metadata` component.
  Pre-envelope flat saves are a clean break (rejected), and
  `world_gen.yaml` no longer exists.
- Lua-owned state persists via `scripts/lib/save_modules.lua` (see
  `scripts/CLAUDE.md`). A required component's failure aborts the whole
  save/load.
- Disk I/O goes ONLY through `World.Save.Storage.publishGeneration` — a
  write-fsync-revalidate-rotate transaction keeping a
  `world.synworld.prev` recovery generation, with `.prev` fallback on
  corruption (`recovered` in `engine.listSaves()`) but never on an
  INCOMPATIBLE file. Symlinked slot dirs/files are refused.
- `engine.loadSave` is a whole-session TRANSACTION
  (`World.Load.Stage`/`Publish`): stage the entire replacement session
  against fresh values, swap in one quiesced window. A load REPLACES the
  complete session — live pages not in the save do not survive. Save and
  load mutually exclude; a failed load leaves the old session unchanged
  and paused (a one-way ratchet per attempt). The 13-phase lifecycle,
  `LoadReconciliationFailed`'s terminal semantics, and `StoragePhase`
  reporting: `docs/engine_contracts.md` §Save/load transaction.
- Typed persistent references (`World.Save.Reference`:
  `SamePageRef`/`CrossPageRef`; Lua `{__ref=kind, id=N}`) feed the
  shared integrity graph (`World.Save.Integrity`) at both save and load
  boundaries — wrong-PAGE targets are hard errors; DANGLING targets are
  tolerated, non-blocking diagnostics. NB: ground-item ids are
  ZERO-based; every other allocator starts at 1.
- Autosave (#913) rides the SAME transaction — it only adds a
  request-time `AutosaveRequest` plus the durable `smAutosave`
  classification. Contract: `docs/engine_contracts.md` §Autosave.

**What's preserved:** gen-params + camera + time + climate + river
flow, edited tiles (chunks regen + edits replay), buildings (with
spawn-roster countdown), units (stats/modifiers/skills/inventory/sim
state), Lua AI memory, pause state. **Not preserved by design:**
selection, build-tool placement mode, active toolbar tool (HUD resets
via the `onSaveLoaded` broadcast), time scale (always 1), blood decals.
Unsupported versions are rejected at their owning boundary with an
"unsupported save version" diagnostic naming the boundary, found version,
and supported versions. A malformed payload at an accepted version remains
a distinct payload error.

## Gates

Pure hspec — `--match "persistence contract"` (a full representative
session through the real codec, every field via derived `Eq`),
`--match "persistence reference integrity"`,
`--match "Lua persistence components"`, `--match "save envelope"` /
`"save components"` / `"atomic save storage"`.
`python3 tools/current_save_schema_audit.py --self-test`,
`python3 tools/current_save_schema_audit.py --base-ref origin/master`,
and `python3 tools/test_save_compat_audit.py` on every non-docs code
change; the compiled-schema check is not path-selected. Probes —
`persistence_contract_probe.py` (CI-eligible smoke:
three real fresh-process save→load→save cycles compared via
`tools/persistence_snapshot.compare_session_files`, which since #2274
execs the compiled `exe:synarchy-save-codec` rather than a `cabal repl`
of the headless suite),
`persistence_contract_sweep.py`, `save_barrier_probe.py`,
`save_storage_probe.py`, `transactional_load_probe.py`,
`persistence_integrity_probe.py`, `multiworld_save_probe.py`, and
`autosave_probe.py` (manual-only). An arena page is a supported
save-test page. Loading a save containing one used to hang the world
thread — the arena's synthetic gen params wedged the
regenerate-from-params path — and #365 fixed it by rebuilding the page
through the single arena authority (`generateArenaChunk`) from its own
recorded seed, then replaying the edit overlay and the dig/construct
slopes (`World.Load.Stage`'s `isArenaParams` branch, `World.Generate.Arena`;
#1718 settled which seed it rebuilds from, #2707 unified the base: sixteen
cells per unedited column on every path, so older arena saves reconstruct
on the corrected base — `docs/persistence_contract.md` §4). `multiworld_save_probe.py
--arena` is the standing regression for that restore path, including
expansion past the 5×5 startup footprint, which is not an extent. That is
the save/load restore path only, and nothing more, and because
`--arena` swaps only the secondary page, an arena run never stands in for
the default run's two generated worlds or for generated-world persistence
coverage. The headless hspec harness cannot run `engine.saveWorld` end to
end; save round trips are proved by the probes.
