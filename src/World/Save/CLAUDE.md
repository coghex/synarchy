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

## Enum schema policy — append-only

`Direction`, `Pose`, `UnitActivity` (and any enum serialized via
`Generic Serialize`) are positional by constructor tag — **append-only**.
Inserting/reordering silently corrupts saves. A constructor's own FIELDS
are positional too, so reordering them or changing one field's
serialized type corrupts saves the same way while moving no tag (#1270).

Anything beyond appending is a **per-component migration**, never a
`currentSaveVersion` change: find EVERY component storing the enum,
raise its `csVersion`, freeze the outgoing DTO, register the frozen type
in `csOlderVersions` via `atVersion` with an explicit migration. Every
version left in `csOlderVersions` must still DECODE, which today's
frozen DTOs do not fully satisfy — the full procedure and that caveat:
`docs/engine_contracts.md` §Enum append-only audit.

Enforced since #1145 by `tools/enum_append_only_audit.py` (CI +
`make ci`, with its own `--self-test`), which guards **every**
multi-constructor `data` declaration under `src/`/`app/` deriving
`Serialize` through `Generic` — read its module docstring before
adding, moving, or changing one. `docs/save_compat/enum_baseline.json`
is the GENERATED golden constructor list; don't hand-edit it — a pure
append ratchets it with `--update-baseline`, and anything else is a
wire-format break the audit refuses to record.

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
  `power-nodes`, `texture-palette`, `metadata`, the two OPTIONAL
  `container-knowledge` and `transfer-orders`, plus dynamic
  `lua.<module>` components). Registry:
  `World.Save.Component.saveComponentRegistry`. Every gameplay component
  is REQUIRED except those two, each of whose absence has an honest
  default — see `docs/persistence_contract.md` §5 before declaring a
  third. Component evolution = per-component schema version bumps +
  explicit migrations from frozen vN DTOs, NOT a global save-version
  bump. `currentSaveVersion` versions only the transitional in-memory
  load bridge (`SaveData`) and is bumped freely — don't trust any number
  written in docs. `listSaves` decodes only the `metadata` component.
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
Older schema versions are rejected with "expected vN, got vM".

## Gates

Pure hspec — `--match "persistence contract"` (a full representative
session through the real codec, every field via derived `Eq`),
`--match "persistence reference integrity"`,
`--match "Lua persistence components"`, `--match "save envelope"` /
`"save components"` / `"atomic save storage"`.
`tools/test_save_compat_audit.py --only-reproducibility` (~26 s, spawns
a `cabal repl`) when the save
format, the tracked fixture corpus, `save_compat_audit.py`, or a Cabal
path changed — `ci_expensive_gates.py --gate save-compat` is the
authority. Probes — `persistence_contract_probe.py` (CI-eligible smoke:
three real fresh-process save→load→save cycles compared via
`tools/persistence_snapshot.compare_session_files`),
`persistence_contract_sweep.py`, `save_barrier_probe.py`,
`save_storage_probe.py`, `transactional_load_probe.py`,
`persistence_integrity_probe.py`, `multiworld_save_probe.py`, and
`autosave_probe.py` (manual-only). NB #365: a save containing an arena
page hangs the world thread on load — never use arenas as a save-test
page. The headless hspec harness cannot run `engine.saveWorld` end to
end; save round trips are proved by the probes.
