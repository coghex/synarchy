# Unit/item ownership findings

This report records defects in transitions between ground items, unit inventories,
equipment, and world-page ownership. It is being drafted one concern chapter at
a time so each finding can be discussed before the audit expands.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

Unit/item ownership was selected because item identity crosses several storage
systems: page-local ground collections, unit inventories, equipment slots,
building storage, and explicit transfer transactions. The first pass traced
world-page ownership from `UnitInstance.uiPage` and each
`WorldState.wsGroundItemsRef` through the ground pickup and unit drop APIs. It
then compared those paths with explicit-page spawning and the newer transfer
policy, which treats cross-page endpoints as out of range.

Existing findings reports were searched for overlap. No equivalent concern was
found in `docs/bugs.md`, `docs/save_load_findings.md`, or
`docs/code_health_findings.md`. No GitHub duplicate search was performed; that
belongs to `process-report`.

The defect was reproduced against a real isolated headless engine with two flat
arena pages. `own_a` was kept active while `own_b` was hidden. A steel bar was
spawned explicitly on `own_a`, and an acolyte was spawned explicitly on
`own_b`. Pickup and drop were then invoked directly through the public Lua APIs.
No graphical or preview process, full test suite, probe sweep, world check, or
`make ci` was run.

The follow-up pass traced accessory modifier ownership through equip, unequip,
and repair; then traced cargo reachability from UI target discovery through
the legacy deposit/withdraw APIs and the newer strict transfer contract.
Existing focused probes and tests were inspected for coverage gaps. Both
retained defects were reproduced in one isolated headless arena. No graphical
process, full suite, probe sweep, or `make ci` was run.

## Status

- [x] OWN-1. Ground/inventory moves use the active page instead of the unit’s owning page — [#1208]
- [x] OWN-2. Unequipping one duplicate accessory disables the copy still worn — [#1209]
- [x] OWN-3. Cargo transfers can target remote or off-world storage — [#1013]

---

## Ground and inventory page ownership

### [#1208] OWN-1. Ground/inventory moves use the active page instead of the unit’s owning page

Ground items belong to a particular `WorldState`, while every unit carries an
explicit owning page in `uiPage`. The ground-to-inventory and
inventory-to-ground APIs do not join those two owners. They resolve whichever
world is currently active, then look up the supplied unit ID globally without
checking that the unit belongs to that world.

Consequently, an item can move between world pages without any world traversal:
an off-page unit can pick up an item from the active page, or drop one of its
items onto the active page using coordinates taken from its owning page.

**Verification:** Verified in both directions using two live pages and the
production Lua API.

**Evidence:**

- `src/Unit/Types/Instance.hs:29` — every `UnitInstance` records its owning
  `WorldPageId` in `uiPage`.
- `src/World/State/Types.hs:102` — every `WorldState` owns a separate
  `wsGroundItemsRef`, making ground-item IDs and collections page-local.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:40` — ground-item spawning
  already has an explicit-page resolver capable of targeting hidden pages.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:58` — `item.spawnGround`
  documents that its page argument prevents content from landing on the wrong
  page.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:289` —
  `item.pickupGround(uid, gid)` resolves the active `WorldState`, not the
  unit’s page.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:294` — the ground ID is removed
  from that active page’s collection.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:299` — the supplied unit is then
  looked up globally and receives the item without any `uiPage` comparison.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:42` — the unit inventory
  module’s shared ground resolver always returns the active world.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:317`,
  `src/Engine/Scripting/Lua/API/Units/Inventory.hs:368`, and
  `src/Engine/Scripting/Lua/API/Units/Inventory.hs:414` —
  `dropEquipmentToGround`, `dropItemToGround`, and `dropItemById` all resolve
  that active world before looking up the unit.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:338` — the drop coordinates
  come from the unit instance, despite the destination ground collection
  coming from another page.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:343`,
  `src/Engine/Scripting/Lua/API/Units/Inventory.hs:393`, and
  `src/Engine/Scripting/Lua/API/Units/Inventory.hs:439` — each drop variant
  inserts the removed instance into the active page’s ground collection.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:111` — the same module’s
  temperature query demonstrates the correct lookup pattern: resolve a
  `WorldState` using the unit’s own `uiPage`.
- `src/Unit/Transfer.hs:486` — the newer general transfer policy explicitly
  requires both endpoints to have the same page.
- `test-headless/Test/Headless/Unit/Transfer.hs:577` — its regression suite
  rejects cross-page endpoints even when their coordinates are identical.
- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:67` — unit spawning already
  accepts an explicit owning page and warns that the active page can change
  between an earlier scan and the eventual operation.

The reproduction began with:

- active page: `own_a`
- hidden unit page: `own_b`
- `own_a` ground count: one steel bar
- acolyte endpoint page: `own_b`
- acolyte inventory: no steel bar

Calling `item.pickupGround(uid, gid)` returned `true`, reduced `own_a`’s ground
count to zero, and placed the steel bar in the `own_b` unit’s inventory. With
`own_a` still active, calling `unit.dropItemToGround(uid, "steel_bar")`
returned `true`, removed the bar from that unit, and spawned it on `own_a` at
`(2, 2)`—coordinates read from the unit on `own_b`.

**Handoff context:**

- **Current behavior:** Ground pickup and all three unit drop variants combine
  a page-local ground collection chosen from ambient active state with a
  globally resolved unit. A successful operation can silently teleport an
  exact `ItemInstance` between pages.
- **Expected direction:** Resolve the unit first and operate on the
  `WorldState` identified by its `uiPage`. Pickup should fail if the requested
  ground ID does not exist on that owning page. Any API intended to support a
  separately specified remote page should require that page explicitly and
  validate the ownership relationship.
- **Scope and constraints:** Cover `item.pickupGround`,
  `unit.dropEquipmentToGround`, `unit.dropItemToGround`, and
  `unit.dropItemById` together so their semantics cannot drift. Preserve exact
  item identity, remove-first duplicate prevention, rollback when a unit
  disappears, cursor deselection, and current failure behavior when no valid
  destination page exists. Revalidate page ownership if a unit can change
  pages during the operation.
- **Test direction:** Add a two-page regression with page-local ground
  collections. It should prove an off-page pickup cannot remove a same-numbered
  ground ID from the active page and that every drop variant inserts only into
  the unit’s owning page. Include an active-page change between selection and
  commit if that timing is exposed by the final implementation.
- **Remaining uncertainty:** Normal unit-AI iteration begins with
  `unit.getAllIds()`, which lists active-page units only, reducing the ordinary
  exposure. It does not make the lower-level APIs safe: the active page is
  asynchronous state, the APIs accept arbitrary live unit IDs, and explicit
  hidden-page unit/item creation is supported. The reproduction exercised
  pickup and `dropItemToGround` directly; the equipment and instance-ID drop
  variants were verified structurally but not separately reproduced.

---

## Equipment modifier ownership

### [#1209] OWN-2. Unequipping one duplicate accessory disables the copy still worn

Accessory instances are stored independently, but their stat modifiers use the
item’s display name as a shared source. Equipping another accessory with that
source deliberately replaces the previous modifier, giving the last-equipped
copy control. Unequipping either copy then removes the shared source completely
without rebuilding it from the accessories that remain worn.

The remaining physical accessory continues to appear in the unit’s equipment
list and contributes carried weight, but its stat buff disappears until another
operation—such as repairing an accessory—happens to rebuild accessory
modifiers.

**Verification:** Verified with two worn Technogoggles using the production Lua
API.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Equipment/Accessory.hs:35` —
  `equipment.equipAccessory` accepts an accessory instance and appends it to
  `uiAccessories`; it does not reject another copy with the same modifier
  source.
- `src/Engine/Scripting/Lua/API/Equipment/Accessory.hs:81` — equipping folds the
  new accessory’s buffs into the unit’s modifier map using its display name.
- `src/Unit/Stats.hs:77` — `applyItemBuffs` documents that modifiers with the
  same source replace rather than stack.
- `src/Unit/Stats.hs:100` — the implementation removes the previous same-source
  modifier before inserting the new one, establishing last-equipped-wins
  behavior.
- `src/Engine/Scripting/Lua/API/Equipment/Accessory.hs:133` — unequip constructs
  the list of accessories that will remain worn.
- `src/Engine/Scripting/Lua/API/Equipment/Accessory.hs:139` — despite having that
  remaining list, unequip removes every modifier carrying the target’s display
  name and never reapplies buffs from the remaining accessories.
- `src/Engine/Scripting/Lua/API/Units/Equipment.hs:216` —
  `refreshAccessoryBuffs` already contains the required whole-list derivation
  pattern.
- `src/Engine/Scripting/Lua/API/Units/Equipment.hs:245` — accessory repair uses
  that whole-list refresh so repairing an older duplicate does not take control
  from the newer one.
- `tools/repair_item_probe.py:308` — the existing probe deliberately equips two
  Technogoggles and establishes that duplicate-source accessories are a
  supported live state with the last-equipped copy controlling the buff.
- `tools/repair_item_probe.py:332` — the probe verifies duplicate behavior
  during repair, but does not exercise unequip.
- `scripts/unit_info_v2_context_menu.lua:150` — the ordinary inventory UI lets
  the player equip accessory instances without a duplicate-source restriction.
- `data/items/technogoggles.yaml:15` — Technogoggles provide a condition-scaled
  perception modifier, making the loss directly observable.

The reproduction began with an acolyte’s original Technogoggles at approximately
90.29% condition and equipped a second pair at approximately 89.56%. Perception
was `1.9657`, reflecting the newer pair’s last-equipped buff. Unequipping the
older pair returned `true`; the newer pair remained present in
`equipment.getAccessories`, but perception fell to `1.0701`—a loss of `0.8956`,
exactly the newer pair’s modifier.

**Handoff context:**

- **Current behavior:** Removing any accessory deletes its shared modifier
  source, even when another worn accessory should still own that source.
- **Expected direction:** Modifier state after unequip should be derived from
  the accessories that remain worn, preserving the established ordered
  last-equipped-wins rule.
- **Scope and constraints:** Cover duplicate instances of one definition and
  different definitions that share a display-name source. Preserve accessory
  ordering, condition scaling, non-stacking semantics, unequippable items, and
  modifiers on unrelated stats or from unrelated sources.
- **Test direction:** Extend focused accessory coverage with two
  same-source accessories. Unequipping the older copy should leave the newer
  modifier unchanged; unequipping the newer copy should reactivate the older
  copy at its own condition-scaled value.
- **Remaining uncertainty:** If duplicate-source accessories were intended to
  be prohibited, the equip boundary currently does not enforce that rule and
  the repair probe explicitly defines their ordered behavior. The live
  implementation therefore needs a consistent disposition either way.

---

## Cargo transfer reachability

### [#1013] OWN-3. Cargo transfers can target remote or off-world storage

The player-facing cargo menus treat adjacency as a discovery-time property
rather than a commit-time invariant. They identify an adjacent unit or building
while constructing a context menu, capture its ID, and later invoke
`depositToCargo` or `withdrawFromCargo` without checking whether the endpoints
are still adjacent or even remain on the same world page.

The Store menu has an additional page leak: it parses the global,
page-agnostic `building.list()` instead of using `building.getActiveIds()`.
Consequently, a built cargo hold on another page can be offered as “adjacent”
when its coordinates happen to overlap the selected unit’s coordinates. The
legacy cargo APIs perform no page, distance, or built-state validation, so the
invalid selection can commit.

**Verification:** Remote same-page deposit and withdrawal were reproduced
against a built cargo hold. The off-world menu path is established
structurally by its global building scan and the mutation API’s missing page
check.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Buildings/Query.hs:198` —
  `building.getActiveIds()` is the supported active-page gameplay iterator.
- `src/Engine/Scripting/Lua/API/Buildings/Query.hs:202` — the API explicitly
  instructs scripts to prefer it over parsing the global, page-agnostic
  `building.list()`.
- `scripts/unit_info_v2_context_menu.lua:23` — the Store menu claims to search
  for built storage adjacent to the unit.
- `scripts/unit_info_v2_context_menu.lua:31` — it instead parses
  `building.list()`, admitting buildings from every live world.
- `scripts/unit_info_v2_context_menu.lua:39` — `building.getInfo()` exposes the
  candidate’s page, but the adjacency calculation compares coordinates only.
- `scripts/unit_info_v2_context_menu.lua:226` — the resulting target ID is
  captured in a Store menu entry.
- `scripts/unit_info_v2_context_menu.lua:235` — the later callback invokes
  `depositToCargo` without repeating page, lifecycle, or distance validation.
- `scripts/cargo_inventory_panel.lua:156` — withdrawal similarly chooses an
  adjacent selected unit while constructing its row menu.
- `scripts/cargo_inventory_panel.lua:435` — the callback captures that unit ID
  and later calls `withdrawFromCargo` without revalidating adjacency.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:176` — `depositToCargo` documents
  that it has no adjacency check and delegates the rule to Lua.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:229` — its commit path globally
  resolves the unit and building, moves the item, and never compares
  `uiPage`/`biPage`, endpoint coordinates, or building activity.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:293` —
  `withdrawFromCargo` documents the same caller-owned adjacency rule.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:318` — its reverse commit likewise
  moves directly between the globally resolved owners without spatial or page
  validation.
- `tools/expedition_retrieval_probe.py:1127` — an existing integration probe
  explicitly warns that the engine API would deposit from across the map and
  separately asserts adjacency to keep its test from passing vacuously.
- `src/Unit/Transfer.hs:483` — the newer strict transfer policy validates source
  and receiver eligibility at the transfer boundary.
- `src/Unit/Transfer.hs:486` — that policy rejects endpoints on different pages
  or outside adjacency range.
- `test-headless/Test/Headless/Unit/Transfer.hs:527` — focused strict-transfer
  coverage rejects distant unit/building endpoints.
- `test-headless/Test/Headless/Unit/Transfer.hs:577` — it also rejects
  cross-page endpoints at identical coordinates.

In the live reproduction, the engine was paused with an acolyte and a built
cargo hold at Chebyshev distance five. `unit.depositToCargo` returned `true` and
changed storage from zero to one item. Without moving either endpoint,
`unit.withdrawFromCargo` also returned `true` and changed storage back to zero.

**Handoff context:**

- **Current behavior:** A Store entry can identify off-world cargo as adjacent,
  and either cargo menu can retain a once-adjacent endpoint after it moves away.
  The mutation APIs accept those stale or cross-page decisions.
- **Expected direction:** Same-page ownership, built storage eligibility, and
  footprint adjacency must still hold when the item actually changes owner.
- **Scope and constraints:** Cover both deposit and withdrawal, exact-instance
  targeting, multi-tile building footprints, active-page changes, and context
  menus left open while a unit moves. Preserve cargo-capacity enforcement,
  order-preserving rollback, exact `ItemInstance` identity, and successful
  container-knowledge revelation.
- **Test direction:** Add a two-page menu regression with same-coordinate cargo
  and a stale-action regression that moves the unit after opening the menu but
  before invoking its callback. Both must leave source and destination
  unchanged. Direct mutation-boundary negative tests should accompany this if
  reachability is enforced below Lua.
- **Remaining uncertainty:** The off-world menu action was not driven through a
  rendered UI reproduction; it follows directly from the documented global
  iterator, coordinate-only menu filter, and page-blind commit. Some page
  transitions may rebuild or close particular UI surfaces, but ordinary
  same-page movement is sufficient for the stale-callback failure and the
  remote commit was reproduced.
