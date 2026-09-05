# Page incarnation teardown and fencing design

A `WorldPageId` is both a page's logical name and, today, its only lifetime
identity. Destroying one page and creating another under the same name is a
shipped lifecycle (new game after a game, arena reset between probe courses),
but the engine never tears down what the old incarnation owned outside its
`WorldState`, and nothing in flight can tell the two incarnations apart. This
design gives a page incarnation an ordered teardown and a fence, so a reused
name can never adopt the previous incarnation's units, buildings, or
simulation output. It benefits players (no ghosts from the previous game in a
fresh world) and every probe or test that recreates a page in one session.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make a reused world-page name a new incarnation with nothing inherited
- [ ] PIN-1. Tear down page-owned units and buildings on single-page destroy and same-id re-init
- [ ] PIN-2. Fence in-flight fluid writebacks with the page's incarnation epoch

## Epic contract

- **Goal:** After a page is destroyed, or re-initialised under the same id,
  no unit, building, selection, unit sim state, destruction effect, or
  simulation writeback that belonged to the previous incarnation is observable
  on the replacement, and the replacement's entities and simulation are
  unaffected by the teardown.
- **Done when:** A headless destroy-then-recreate of the same page id and a
  re-init of a live page id both leave the replacement with zero inherited
  units and buildings; a fluid writeback batch computed against the previous
  incarnation is refused by the replacement; the destroy-all and load paths
  keep their existing behaviour; and the contracts document records the
  incarnation rule.
- **Users and operators:** Players starting a new game after a game; the
  movement, wander, repair, and thought probes that rebuild an arena; anyone
  writing a headless fixture that recreates a page.
- **Arc label:** `bug` fits both slices (each fixes an observable defect).
  None proposed for the umbrella.

## Current state and evidence

Verified on master `67b17a353` (2026-09-02). Origin: holistic audit finding
HPA-26 in `docs/holistic_project_audit_findings.md`.

**Three paths replace or remove a page; only one of them fences entities.**

- Single-page destroy (`handleWorldDestroyCommand`,
  `src/World/Thread/Command/Basic.hs`) sends `SimDropWorld`, hands blood
  textures to the render thread, removes the page from `wmWorlds` and
  `wmVisible`, bumps the selection generation when the head changed (#1602),
  and clears the world quads. It enqueues nothing on the unit or building
  queues.
- Destroy-all (`handleWorldDestroyAllCommand`, same module) does everything
  above for every page and then enqueues `UnitClearAll` and
  `BuildingClearAll`, deliberately via the queues so the clears run in order
  after any pending spawn (#58). That is the ordering pattern this design
  reuses.
- Load publication (`src/World/Load/Publish.hs`) drops every old page's sim
  state first, then replaces the unit manager, building manager, and unit
  thread state wholesale with the staged session. It is already a fenced
  whole-session transaction and is untouched by this arc.

**Two init paths replace a live page under the same id with no teardown.**

- `handleWorldInitCommand` and the arena init in
  `src/World/Thread/Command/Init.hs` both register the new `WorldState` with
  `wmWorlds = (pageId, worldState) : filter ((≢ pageId) . fst) …`, replacing
  an existing entry by design (#58, the "main_world" reuse after Exit to
  Menu). Both bump the selection generation when the head is replaced (#1602)
  and reclaim the old page's blood textures (#788). Neither enqueues a unit or
  building clear.

**What the old incarnation leaves behind.** Transfer orders, power nodes, and
container knowledge live on the page's own `WorldState`
(`wsTransferOrdersRef`, `wsPowerNodesRef`, `wsContainerKnowledgeRef`), so they
go with the page. What survives is in the global managers, keyed only by the
reusable name:

- `UnitManager.umInstances` and `umSelected` — every `UnitInstance` carries
  `uiPage ∷ WorldPageId` (`src/Unit/Types/Instance.hs`) and nothing else
  identifies its incarnation. `unitsOnPage`/`unitsOnPages`
  (`src/Unit/Types/Manager.hs`) and path advance
  (`src/Unit/Thread/Movement/PathAdvance.hs`) resolve by that name against
  whichever `WorldState` currently owns it.
- `UnitThreadState.utsSimStates` (`src/Unit/Sim/Types.hs`) — per-unit sim
  state, keyed by `UnitId`.
- `BuildingManager.bmInstances`, `bmSelected`, and `bmDestructions`
  (`src/Building/Types.hs`) — every `BuildingInstance` carries
  `biPage ∷ WorldPageId`.

`UnitClearAll` wipes `umInstances`, `umSelected`, and `utsSimStates`;
`BuildingClearAll` wipes `bmInstances`, `bmSelected`, `bmDestructions` and
forgets every container. The per-instance removals (`handleUnitDestroyCommand`,
the building demolition handler) additionally retire the instance's transfer
orders, power node, and container record across every page; those are the
`WorldState`-owned rows above.

**Simulation output carries only the name.** `FluidWritebackBatch`
(`src/World/Command/Types.hs`) is `WorldPageId` plus writebacks plus an
optional ack. `handleApplyFluidsCommand` (`src/World/Thread/Command.hs`) drops
a batch whose page is absent, and otherwise applies each writeback whose
`fwEditGen` equals the page's live-edit generation for that chunk, where an
absent entry reads as zero on both sides. A replacement page's never-edited
chunks have no entry, so a batch the sim computed against the previous
incarnation at generation zero passes. The sim enqueues batches onto the world
queue asynchronously from its own tick, so a batch can be in the world queue
when `SimDropWorld` is still in the sim queue. `SimWorldState`
(`src/Sim/State/Types.hs`) is keyed by `WorldPageId` and holds no incarnation
value.

**An incarnation epoch already exists, in one scope.** #2001 (CRS-1, closed)
allocates a process-unique `ChunkGeneration` for every fresh `WorldState`
(`newChunkGeneration`, `src/World/Chunk/Residency.hs`; allocated in
`emptyWorldState`, `src/World/State/Types.hs`), whose haddock states its
purpose: "a page id reused by a reinit, an arena replacement or a load
republish is a DIFFERENT generation." Only chunk requests minted by the
residency owner carry it. `docs/chunk_residency_streaming_design.md` plans for
the world thread to reject candidates "for deleted or replaced pages and
obsolete epochs" in the chunk-residency scope only.

**The lifecycle is shipped, not theoretical.**

- `scripts/create_world/generation.lua` calls `worldManager.destroyWorld()`
  when a world is active and then creates the new one;
  `worldManager.createWorld` defaults the id to `"main_world"`.
- `scripts/movement_arena.lua` `M.reset` destroys and recreates `M.page`.
  Every shipped probe spawns fresh units per course (`tools/movement_probe.py`),
  so nothing relies on entities surviving a reset.
- `scripts/test_arena.lua` destroys its arena page.
- `test-headless/Test/Headless/WorldGen.hs` ("can destroy a world") and
  `test-headless/Test/Headless/Building/PageBinding.hs` (destroy and re-init
  of visible and hidden pages) already drive both paths from hspec, so the
  fixture shape this arc needs exists.

**Tracker.** No open issue owns page-incarnation teardown. #1997 (streaming
epic) never mentions it; #2001 is closed. #1602 fences placement bindings
through the selection generation and is the precedent for "a replaced page is
a change even when the name is not." HPA-44 and HPA-48 in the holistic report
state that they integrate with the fencing this arc lands. HPA-36 owns the
destroy-all Exit-to-Menu path's process-global state and is separate.

## Desired experience

- A player who finishes a game and starts a new one never sees a unit or
  building from the previous game standing in the new terrain, and never sees
  a previous unit's selection, inventory, job, or sim state acted on.
- A probe that resets its arena between courses starts each course with an
  empty page, without despawning units itself.
- Fluid that was settling in the previous incarnation never lands on the
  replacement's tiles.
- Destroy-all, load, hide, and show behave exactly as they do today.
- A late spawn for a destroyed page is still dropped outright, and a spawn
  issued for the replacement after re-init lands on the replacement.

## Scope

### In scope

- Page-scoped teardown of unit and building manager state on single-page
  destroy and on both same-id re-init paths, ordered after pending spawns via
  the unit and building queues.
- Refusing fluid writeback batches from a previous incarnation of a page.
- A contracts entry for the incarnation rule and the teardown order.
- Headless hspec gates for both slices.

### Out of scope

- Process-global session state on Exit to Menu (event store, clocks, popup
  queues): HPA-36.
- Page-addressed bulk chunk work and craft-bill identity domains: HPA-44 and
  HPA-48, which build on this arc's fence.
- Chunk-request epochs and residency eviction: #1997's arc.
- Lua-side state that names units or pages (selection scripts, unit AI job
  tables, HUD). Lua callers that destroy a page own their own bookkeeping, as
  today.
- Persisting any incarnation value. The epoch is runtime-only; saves hold one
  session and load republishes it with fresh `WorldState`s.
- Changing `WorldPageId` itself, the Lua `world.*` verb signatures, or the
  save format.

## Design

### Two independent fences

The defect has two halves with different mechanisms, and they land as two
slices:

1. **Entities are torn down in queue order.** The world thread cannot mutate
   the unit or building managers directly: those threads keep draining their
   queues through a teardown, and a direct clear would race an in-flight spawn
   (#58). Destroy-all therefore enqueues `UnitClearAll` and
   `BuildingClearAll`. D-1 lands a page-scoped sibling of each —
   `UnitClearPage pid` and `BuildingClearPage pid` — enqueued from
   single-page destroy and from both same-id init paths at the point the page
   leaves or is replaced in `wmWorlds`. Queue order then guarantees: spawns
   queued before the teardown insert and are cleared; spawns issued after
   re-init are for the replacement and survive. No entity record needs an
   incarnation token for this half.

2. **In-flight simulation is fenced by an epoch.** The sim produces batches
   asynchronously, so no ordering between the two queues exists to lean on.
   D-3 reuses the `ChunkGeneration` every `WorldState` already mints as the
   page's incarnation epoch: every sim message that carries topology
   (`SimActivateWorld`, `SimChunkLoaded`, `SimChunkEdited`) carries it into
   `SimWorldState`, every
   `FluidWritebackBatch` carries the epoch it was computed under, and
   `handleApplyFluidsCommand` drops a batch whose epoch is not the live page's
   before the per-chunk freshness check. The dump fast-settle ack still fires
   for a dropped batch, as it does today for an absent page.

### Ownership and data flow

- The world thread owns the decision that a page ended or was replaced; it is
  the single point that enqueues the teardown and the single reader of the
  epoch at writeback time.
- The unit and building threads own their managers and apply page-scoped
  clears exactly as they apply the whole-manager clears, filtering by
  `uiPage`/`biPage` and `UnitId` membership for `utsSimStates`.
- The sim thread stores the epoch it was activated with and stamps batches;
  it never compares epochs.
- Blood textures, world quads, scene stats, and the selection generation
  keep their existing per-path handling.

### Failure handling

- A `UnitClearPage` for a page that has no units is a no-op.
- A page-scoped clear arriving after destroy-all's whole-manager clear is a
  no-op.
- A batch for an absent page is dropped as today; a batch with a stale epoch
  is dropped with a debug log naming the page and both epochs.

### Rejected alternatives

- **Incarnation token on every `UnitInstance` and `BuildingInstance`.** It
  would change two runtime records and every query that resolves a page, for
  a guarantee queue ordering already provides. Rejected by D-1.
- **Clearing the managers directly from the world thread.** Rejected by #58:
  it races in-flight spawns.
- **Making single-page destroy call `UnitClearAll`.** Wrong for a hidden
  arena destroyed beside a live main world.

## Decisions

### D-1. Entities are fenced by queue-ordered per-page clear verbs, not by a token

`UnitClearPage pid` and `BuildingClearPage pid` are enqueued on the unit and
building queues, mirroring destroy-all's #58 pattern. Entity records keep
`uiPage`/`biPage` as they are and no resolver compares an incarnation value.
The ordering rule that makes this sufficient: the world thread enqueues both
clears in the same step that removes the page from `wmWorlds` (destroy) or
replaces it there (re-init), and before the replacement is registered. A
spawn for a page absent from `wmWorlds` is already dropped by the spawn
handlers, so every spawn either precedes the clear and is cleared or follows
it and survives. Rationale: it is the mechanism destroy-all already proves
correct, it touches no record or resolver, and no consumer outside placement
(fenced by #1602) captures an id before teardown and acts on it afterwards.
Consequence: if such a consumer ever appears, a token becomes its own later
slice rather than a change to PIN-1. Signed off 2026-09-02 (Q-1). Affects
PIN-1.

### D-2. Re-initialising a live page id stays supported and tears down the incarnation it replaces

Both `handleWorldInitCommand` and the arena init enqueue the D-1 clears for
the page they replace. Replacement remains the #58 contract; `world.init` and
`world.initArena` on a registered id are not refused. Rationale: it is one
enqueue per path, keeps the #1602 replacement-is-a-selection-change logic
live, and leaves the page-binding specs that re-init live pages intact.
Refusal would close no window PIN-1 does not already close. Consequence: a
single-lifecycle refusal, if ever wanted, is a separate later slice with a
logged refusal and lands after PIN-1. Signed off 2026-09-02 (Q-2). Affects
PIN-1 and leaves the Lua world verbs unchanged.

### D-3. The page incarnation epoch is the `ChunkGeneration` every fresh `WorldState` already mints

PIN-2 reuses #2001's per-`WorldState` `ChunkGeneration` as the page's
incarnation epoch rather than minting a second value. It is carried on every
sim message that already carries topology (`SimActivateWorld`,
`SimChunkLoaded`, `SimChunkEdited`), not on activation alone, because
`SimFastSettleAll` emits batches for stored worlds regardless of activation
and a never-activated page must still be stamped. Rename it (for example to
`PageGeneration`) only if the reference count is small; otherwise broaden its
haddock to the page-incarnation meaning. Rationale: its haddock already
defines it as the value distinguishing "the same page, a later generation,"
it is process-unique, and it is allocated in the one constructor every fresh
page passes through. Consequence: the residency epoch and the incarnation
epoch cannot advance independently; the streaming design does not plan for
them to. Signed off 2026-09-02 (Q-3). Affects PIN-2.

## Open questions

### Q-1. Ordered per-page clear verbs, or an incarnation token on entity records?

The proposal is ordered teardown (`UnitClearPage`/`BuildingClearPage`),
mirroring destroy-all's #58 pattern; entity records stay as they are. The
alternative stamps `ChunkGeneration` onto `uiPage`/`biPage` (or beside them)
and has every resolver compare it. The token guards a case ordering does not:
a consumer that captured a unit id before the teardown and acts on it after
the replacement exists. #1602 already fences the one such consumer that
matters (placement bindings) via the selection generation. Affects PIN-1.
Resolved by D-1.

### Q-2. Is re-initialising a live page id a supported lifecycle that tears down, or should it be refused?

The proposal keeps replacement supported (#58 made it deliberate) and has both
init paths enqueue the page-scoped teardown for the incarnation they replace.
The alternative makes `world.init`/`world.initArena` on a registered id a
refused call, requiring an explicit `world.destroy` first. Shipped Lua already
destroys before creating (`generation.lua`, `movement_arena.lua`), so the
alternative would change a contract few callers exercise, but it would also
remove one of the two fenced paths. Affects PIN-1 and the Lua world verbs.
Resolved by D-2.

### Q-3. Reuse `ChunkGeneration` as the page incarnation epoch, or mint a separate value?

The proposal reuses it: its haddock already defines it as the value that
distinguishes "the same page, a later generation," it is process-unique, and
it is allocated in the one constructor every fresh page goes through. The
alternative adds a second per-`WorldState` epoch so the residency owner's
value keeps a chunk-only meaning. Reuse may warrant a rename or a broadened
haddock. Affects PIN-2. Resolved by D-3.

## Verification strategy

- **PIN-1:** an hspec in the headless harness that inits a page, spawns a unit
  and places a building on it, destroys the page, re-inits the same id, and
  asserts the replacement has no units, no buildings, no selection, and no
  unit sim states while a spawn issued after re-init is present. A second
  example re-inits without destroying. A third destroys a hidden page beside
  a visible one and asserts the visible page's entities are untouched.
  Existing fixtures: `test-headless/Test/Headless/WorldGen.hs` ("can destroy
  a world") and `test-headless/Test/Headless/Building/PageBinding.hs`.
  `movement_probe.py` and `wander_hazard_probe.py` exercise the arena reset
  path and must keep passing.
- **PIN-2:** a pure hspec over `handleApplyFluidsCommand`'s freshness decision
  proving a batch stamped with a previous incarnation's epoch is refused even
  when every chunk generation matches, and a harness example that seeds a
  page, captures a batch, destroys and recreates the page, replays the batch,
  and asserts no tile changed. The pure example belongs beside the existing
  freshness spec `test-headless/Test/Headless/Sim/FluidWritebackStaleness.hs`
  (#1596); `test-headless/Test/Headless/Sim/SimPageOwnership.hs` and
  `test-headless/Test/Headless/WorldSim.hs` cover the page-scoped sim
  protocol this slice extends. The `--dump` fast-settle path must keep
  passing, including the ack on a dropped batch.
- **Contracts:** `docs/engine_contracts.md` gains the incarnation rule; the
  `docs/engineenv_capability_inventory.md` and persistence inventory audits
  are unaffected unless a new `EngineEnv` or `WorldState` field is added (the
  proposal adds none).

## Delivery plan

### PIN-1. Tear down page-owned units and buildings on single-page destroy and same-id re-init

- **Outcome:** Single-page destroy and both same-id init paths enqueue a
  page-scoped clear on the unit and building queues, so the replacement
  incarnation inherits no unit, building, selection, unit sim state, or
  destruction effect.
- **Scope:** `UnitClearPage`/`BuildingClearPage` command constructors and
  handlers; enqueueing from `handleWorldDestroyCommand`,
  `handleWorldInitCommand`, and the arena init; the three hspec examples
  above; the contracts entry.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-1, D-2
- **Acceptance signals:** the three examples pass; `movement_probe.py` and
  `wander_hazard_probe.py` pass; destroy-all and load behaviour unchanged.
- **Out of scope:** simulation writebacks (PIN-2); Lua-side bookkeeping;
  Exit-to-Menu process-global state (HPA-36); refusing re-init of a live id.
- **Open questions:** `None`

### PIN-2. Fence in-flight fluid writebacks with the page's incarnation epoch

- **Outcome:** A fluid writeback batch computed against a previous incarnation
  of a page id is refused by the replacement, before the per-chunk freshness
  check, and the fast-settle ack still fires.
- **Scope:** carry the page epoch into `SimWorldState` on activation, stamp
  `FluidWritebackBatch` with it, compare in `handleApplyFluidsCommand`, and
  the two tests above.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** `independent`
- **Relevant decisions:** D-3
- **Acceptance signals:** the pure and harness examples pass; the existing
  `Sim/FluidWritebackStaleness`, `Sim/SimPageOwnership`, and `WorldSim`
  headless specs pass unchanged; a `--dump` run still settles and exits.
- **Out of scope:** chunk-request epochs (#1997); other page-addressed world
  commands (HPA-44, HPA-48).
- **Open questions:** `None`
