# Power-network lifecycle findings

This report records defects in power-item placement, node/building ownership,
and electrical topology across building destruction and chunk streaming. It is
being drafted for later one-at-a-time processing rather than as an issue backlog
or implementation plan.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The power system was selected because one player action crosses several
ownership and lifecycle boundaries: a unit inventory supplies an item, a global
building command is queued, a page-local power node is registered, persistent
structure edits define wire, and network simulation reconstructs topology from
the loaded chunk cache.

The audit traced `power.placeNode` from `scripts/build_tool.lua` through the
unit and building managers; followed building destruction into the per-page
node registry and persistence staging; and compared persistent wire edits with
camera-driven chunk eviction, live network queries, and the world-thread power
tick. Focused power tests and `tools/power_probe.py` were inspected for the
corresponding coverage.

Existing reports were searched for overlap. `docs/code_health_findings.md`
already records hardcoded power-device definitions in CH-108 and the
production-call ambiguity around pruning helpers in CH-109; neither describes
the lifecycle failures below. `docs/unit_item_ownership_findings.md` covers
related page-ownership failures in ground-item and cargo operations, but not
power placement. No GitHub duplicate search was performed; that belongs to
`process-report`.

All three findings were exercised in a safe headless engine on port 9364. The
page-ownership reproduction used the documented sixth argument to spawn a unit
on a hidden page, verified that the unit could not be selected from the active
page, and then invoked the public power API. The lifecycle reproduction
destroyed a placed power building and queried both building and node state.
The streaming reproduction moved the camera until the power hardware's chunk
was genuinely evicted (`world.getChunkInfo(0,0).loaded == false`), then returned
to reload it. The engine was shut down normally afterward.

No graphical or preview process, save file, full test suite, world check, probe
sweep, or `make ci` was run.

## Status

- [x] PWR-1. Power placement can consume an item from a unit on another world page — [#1205]
- [x] PWR-2. Demolishing power hardware leaves an unremovable persisted ghost node — [#1206]
- [x] PWR-3. Chunk eviction temporarily dismantles persistent electrical networks — [#1207]

---

## Placement and ownership lifecycle

### [#1205] PWR-1. Power placement can consume an item from a unit on another world page

`power.placeNode` resolves its destination page independently from the supplied
unit. It then looks up that unit in the global unit manager and removes the
requested item without comparing the unit's `uiPage` with the destination page.

A caller can therefore consume a solar panel or battery from a unit on one
world page and materialize its building and power node on another. The normal
build tool reduces the ordinary exposure by choosing among selected units, but
the public API accepts arbitrary unit IDs and explicit target pages. It also
remains vulnerable if the active page changes between selection and commit.

**Verification:** Verified through the production Lua API with separate active
and hidden arena pages.

**Evidence:**

- `src/Unit/Types/Instance.hs:29` — every unit records its owning page in
  `uiPage`, which is intended to prevent units from leaking between worlds.
- `src/Unit/Selection.hs:31` — selection explicitly requires a unit to belong
  to the active page.
- `src/Engine/Scripting/Lua/API/Power.hs:102` — `power.placeNode` resolves the
  explicit or active destination page before entering the placement
  transaction.
- `src/Engine/Scripting/Lua/API/Power.hs:132` — the transaction independently
  looks up the supplied unit in the global instance map.
- `src/Engine/Scripting/Lua/API/Power.hs:135` — the matching item is removed
  without checking `uiPage == pid`.
- `src/Engine/Scripting/Lua/API/Power.hs:155` — terrain and occupancy are
  correctly validated against the destination page, demonstrating that only
  the inventory owner is left unjoined.
- `src/Engine/Scripting/Lua/API/Power.hs:168` — the building command and power
  node are committed to that independently chosen destination.
- `scripts/build_tool.lua:832` — the shipped build tool searches selected
  units, which normally limits it to the active page.
- `scripts/build_tool.lua:871` — it then calls `power.placeNode` without
  supplying or revalidating a page.
- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:67` — unit spawning already
  documents why explicit ownership must be preserved even when an earlier
  active-page scan appeared sufficient.
- `tools/power_probe.py:31` — existing integration coverage places power items
  only from a selected unit on the same active arena.

In the corrected reproduction, `pwr_evict` was active while the supplying
acolyte belonged to hidden page `pwr_cross2`. `unit.select(uid)` returned
`false`, proving the unit was not on the active page. Nevertheless,
`power.placeNode(uid, "solar_panel", 9, 5, "pwr_evict")` succeeded, reduced the
hidden unit's inventory from one item to zero, and produced a building whose
reported page was `pwr_evict`.

**Handoff context:**

- **Current behavior:** The item source and placed node can have unrelated page
  owners, allowing a finished power item to teleport between worlds.
- **Expected direction:** A placement should commit only when its inventory
  owner belongs to the same page as the destination being validated.
- **Scope and constraints:** Cover both explicit-page calls and the implicit
  active-page form. Preserve exact item identity, original-index rollback,
  placement validation, page-local node allocation, and the existing
  protection against a unit disappearing during rollback. Revalidate ownership
  inside the mutation boundary rather than relying only on the build tool's
  earlier selection scan.
- **Test direction:** Add a two-page regression with the supplier on a hidden
  page. Both explicit and implicit cross-page placement attempts should leave
  inventory, buildings, and power nodes unchanged. Include an active-page
  transition between source selection and commit if the final boundary makes
  that timing controllable.
- **Remaining uncertainty:** The ordinary UI path usually supplies a correctly
  selected unit, so direct API use and page-transition timing are the clearest
  current exposures.

### [#1206] PWR-2. Demolishing power hardware leaves an unremovable persisted ghost node

Power nodes declare the building manager to be their authority for lifetime,
but the building-destruction command only deletes the building instance and
container knowledge. It does not remove the node riding that building.

The orphan disappears from electrical snapshots because its position can no
longer be resolved, but it remains visible through `power.listNodes` and
`power.getNodeForBuilding`. Although a pure `removePowerNode` helper exists,
there is no production caller or Lua removal operation. Save/load deliberately
preserves a node whose building is absent, so the ghost survives indefinitely
rather than being repaired at the persistence boundary.

**Verification:** Verified by destroying a live placed solar-panel building.

**Evidence:**

- `src/Power/Types.hs:3` — the registry declares the building manager to be the
  authority for node position, page, and lifetime.
- `src/Building/Thread/Command.hs:115` — `BuildingDestroy` removes the building
  instance and selection.
- `src/Building/Thread/Command.hs:122` — its only additional ownership cleanup
  concerns container knowledge; no power registry is touched.
- `src/Power/Types.hs:113` — a node-removal transition exists but has no
  production call site.
- `src/Engine/Scripting/Lua/API/Register/Craft.hs:59` — the complete public
  `power` table exposes placement and queries but no remove or cancel
  operation.
- `src/Engine/Scripting/Lua/API/Power.hs:244` — `power.listNodes` enumerates the
  registry directly, including dangling nodes.
- `src/Power/Network.hs:364` — network position resolution silently drops a
  node whose building is missing, making the ghost electrically inert rather
  than removing it.
- `src/World/Load/Stage.hs:243` — load staging explicitly restores dangling
  power nodes verbatim.
- `docs/persistence_state_inventory.md:301` — the persistence contract likewise
  says an absent host building is tolerated.
- `test-headless/Test/Headless/Power/Types.hs:54` — focused tests exercise the
  pure removal helper.
- `test-headless/Test/Headless/Power/Types.hs:82` — they also exercise pruning,
  but do not connect either transition to live building destruction.
- `tools/power_probe.py:434` — integration coverage verifies surviving
  building/node pairs after reload but never demolishes one.

After `building.destroy(bid)` returned true in the live reproduction,
`building.getInfo(bid)` returned nil. `power.getNodeForBuilding(bid)` still
returned the original source node and `power.listNodes()` retained it.

**Handoff context:**

- **Current behavior:** Demolition removes the node's authoritative host while
  leaving an inert, visible, persisted registry row that no public operation
  can clear.
- **Expected direction:** Successfully destroying a power building should also
  retire the node it owns before the resulting state becomes observable or
  saveable.
- **Scope and constraints:** Couple cleanup to the live destruction transaction,
  not to indiscriminate load-time pruning, because compatibility intentionally
  tolerates dangling records from older saves. Preserve page-local node IDs,
  allocator monotonicity, ordinary non-power demolition, queue ordering, and
  whole-page teardown.
- **Test direction:** Place a source and a battery, destroy each host, and prove
  the matching node disappears while unrelated nodes remain. Follow with a
  save/fresh-load round trip and verify that no demolished node is serialized
  or restored.
- **Remaining uncertainty:** There is not yet an ordinary player demolition UI;
  the defect is currently reached through the public `building.destroy` API
  and will affect any later demolition flow unless ownership cleanup is added.

---

## Streaming and network topology

### [#1207] PWR-3. Chunk eviction temporarily dismantles persistent electrical networks

Wire placement is persistent: every piece is recorded in the world's edit log
and replayed when its chunk is regenerated. Power topology, however, ignores
that persistent representation and scans only `lcStructures` in the currently
loaded chunk cache.

When the camera causes a wired chunk to be evicted, the wire disappears from
the electrical topology even though the edit still exists. Nodes attached
through that wire report no network, and `tickPowerNodes` leaves them untouched.
Returning the camera reloads the chunk, replays the same wire, and reconstructs
the network. Electrical membership and battery evolution therefore depend on
camera-driven cache residency.

**Verification:** Verified with a live wired network and real chunk eviction.

**Evidence:**

- `src/World/Tile/Types.hs:47` — chunks are evicted by camera distance when the
  cache exceeds its configured maximum.
- `src/World/Tile/Types.hs:49` — edited chunks intentionally evict because their
  changes survive in the edit log.
- `src/World/Thread/Command/Edit/Structure.hs:26` — wire-compatible structure
  placement updates the loaded overlay and appends a persistent edit for
  eviction replay.
- `src/Power/Network.hs:346` — `wireTilesOn` explicitly limits topology to
  loaded chunks.
- `src/Power/Network.hs:352` — its implementation enumerates only
  `wtdChunks`/`lcStructures`, not persistent structure edits.
- `src/Engine/Scripting/Lua/API/Power.hs:320` — live network queries read that
  same evictable `wsTilesRef`.
- `src/World/Thread/Power.hs:53` — the simulation tick also snapshots
  `wsTilesRef`.
- `src/World/Thread/Power.hs:70` — it derives every current wire tile through
  `wireTilesOn`.
- `src/Power/Network.hs:319` — nodes absent from a reconstructed wire network
  are left untouched instead of charging or discharging.
- `test-headless/Test/Headless/Power/Network.hs:81` — focused connectivity tests
  supply a complete synthetic wire set and do not exercise chunk residency.
- `tools/power_probe.py:43` — the integration probe validates ordinary loaded
  connectivity and save/load replay, but not eviction while the page remains
  live.

Before eviction, `world.getChunkInfo(0,0).loaded` was true and
`power.getNetworkForNode(1)` returned the wired network. After moving the camera,
the same chunk reported `loaded == false` and the network query returned nil.
Returning the camera to the hardware reloaded the chunk and immediately
restored one network containing both source nodes.

**Handoff context:**

- **Current behavior:** Merely viewing a distant area can electrically detach
  persistent hardware, freeze its stored-energy changes, and then reconnect it
  when the player returns.
- **Expected direction:** A persistent wire layout's electrical membership and
  energy evolution should not change solely because its rendering/terrain
  chunk entered or left an eviction cache.
- **Scope and constraints:** Preserve ordered set/clear edit semantics,
  same-page topology, cylindrical coordinate canonicalization, and cheap
  rendering eviction. Avoid introducing a second wire authority that can drift
  from structure placement, clearing, save/load replay, or `clearAll`.
- **Test direction:** Build a source–battery–consumer network, record its
  membership and charge, force the wire chunk to evict, advance game time, and
  verify topology and energy remain continuous. Reload the chunk and ensure
  neither membership nor stored energy jumps. Include a cleared wire so stale
  historical `WeSetStructure` edits cannot resurrect connectivity.
- **Remaining uncertainty:** `wireTilesOn` explicitly documents loaded-only
  behavior, but no player-facing power contract says off-camera infrastructure
  is suspended, and live network queries present the result as actual
  connectivity rather than cache state. If suspension is intentional, it
  needs an explicit gameplay contract and status model instead of silently
  changing topology.
