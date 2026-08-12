# Project Review Findings: PRs #1182–#1167

These entries record focused evidence from the senior review of merged PRs #1182 through #1167 for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. Item stacks collapse temperature-distinct instances
- [ ] PRR-2. Item-list invalidation omits callback-visible row data
- [ ] PRR-3. Exact-container popup titles discard the instance identity
- [ ] PRR-4. Enum audit cannot see same-arity payload changes
- [ ] PRR-5. World-name suggestion accepts unbounded linear work

## 1. Shared item-list contracts

### PRR-1. Item stacks collapse temperature-distinct instances

> **Captured note:** Treat tracked item temperature as part of raw-item equivalence, or establish another observable way to keep temperature-distinct instances selectable. `unit.getInventory` exports each instance's tracked temperature, but the unit-inventory adapter drops it and the shared stack key omits it even though temperature changes consumable effects.

**Verification:** Partially verified — two otherwise identical held items with different tracked temperatures reach the shared widget as the same raw row and merge behind the first instance, but no current row action directly consumes the selected item.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:607` — `unit.getInventory` exposes `iiTemp` as the per-instance `temp` field while an item is hotter or colder than ambient.
- `scripts/unit_info_v2_inventory_data.lua:21` — the adapter copies the inventory row into a new table without copying `temp`.
- `scripts/ui/item_list.lua:85` — the canonical key compares seven fields and omits temperature, then states that every merged instance is interchangeable and the representative is always safe.
- `scripts/consumable.lua:82` — drinking reads the exact instance's effective temperature and uses it to scale warmth, caffeine, and other effects.
- `test-headless/Test/Headless/UI/ItemList.hs:43` — the direct split-field test covers only the seven specified fields, so it positively accepts the incomplete equivalence relation.

**Handoff context:**

- **Current behavior:** Two coffee pots or other items whose visible stack fields match but whose tracked temperatures differ collapse into one row; only the first instance remains reachable through representative-row actions.
- **Expected behavior:** A grouped row must not claim mechanically distinct instances are interchangeable unless every relevant action can preserve or expose the player's intended instance.
- **Scope and constraints:** Surfaced while reviewing PR #1169 / issue #1088. The PR faithfully implemented the issue's seven-field key, so the processor should judge the specification as well as the extraction. Preserve exact `instanceId` routing and avoid splitting on fields that truly have no behavioral or visible effect.
- **Remaining uncertainty:** The current inventory context menu equips, stores, repairs, or opens contents rather than consuming the row directly. A focused fixture with two co-carried, otherwise identical hot/cold consumables would settle the player-visible impact and desired grouping policy.

### PRR-2. Item-list invalidation omits callback-visible row data

> **Captured note:** Re-check the shared item-list dirty signature against every field consumed by host callbacks. The widget promises complete callback-derived invalidation, but `rowSignature` omits make, material, capacity, weapon details, and buffs while the unit inventory's tooltip callback renders all of them and its presentation key covers only repair state.

**Verification:** Partially verified — the current signatures can return not-stale after tooltip output changes, violating the widget contract statically; the omitted production fields are mostly definition-authored and may not mutate while a panel remains open.

**Evidence:**

- `scripts/ui/item_list.lua:212` — `rowSignature` includes common row fields but no `make`, `material`, `capacity`, nested `weapon`, or `buffs` data.
- `scripts/ui/item_list.lua:236` — the signature contract says callback-produced values must arrive through `presentationKey` when row fields cannot cover them.
- `scripts/unit_info_v2_inventory.lua:62` — the unit host's `presentationKey` contains only unit identity and `repairStatus.cacheKey` values.
- `scripts/unit_info_v2_items.lua:41` — `buildItemHint` renders make, material, container capacity, nested weapon properties, and every buff into the tooltip.
- `test-headless/Test/Headless/UI/ItemList.hs:460` — the invalidation test changes callback presence and an explicit presentation key, but never changes callback-consumed row data while retaining the same key.

**Handoff context:**

- **Current behavior:** Replacing an otherwise identical row with different callback-visible metadata can leave the existing tooltip elements in place because neither the row signature nor the host key changes.
- **Expected behavior:** Any supplied value that changes a rendered label, tooltip, overlay, action, or chrome value must make the widget stale, while genuinely unchanged normalized input must retain its element handles.
- **Scope and constraints:** Surfaced while reviewing PR #1169 / issue #1088 requirement 11. Preserve the no-churn path and keep callback-derived state explicit; do not require the shared widget to know host-specific tooltip semantics.
- **Remaining uncertainty:** The listed fields currently come from item definitions and are generally stable during one panel lifetime. The processor should identify whether another callback-visible live field provides a present production repro or disposition this as an API-contract hardening gap.

### PRR-3. Exact-container popup titles discard the instance identity

> **Captured note:** Carry exact container identity through the item-contents popup title as well as its contents query. `openFor` deliberately targets `instanceId`, but `buildTitle` searches inventory by `defName` and uses the first same-definition item's display name.

**Verification:** Partially verified — contents and lifecycle checks address the clicked instance while the title lookup addresses the first same-definition instance; current authored item-container data does not yet provide two differently named instances that visibly expose the mismatch.

**Evidence:**

- `scripts/item_contents_panel.lua:185` — `buildTitle` loops over `unit.getInventory` and stops at the first row whose `defName` matches, without comparing `s.instanceId`.
- `scripts/item_contents_panel.lua:317` — `openFor` documents exact-container targeting and stores the supplied `instanceId` in popup state.
- `scripts/item_contents_panel.lua:365` — the refresh path continues querying contents with that exact stored identity.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:576` — inventory display names can vary by live instance condition through the `" (broken)"` suffix.
- `data/items/first_aid_kit.yaml:6` — the only currently authored `kind: container` is the first-aid kit and it has no condition spec, so the present data does not reproduce a differing title.

**Handoff context:**

- **Current behavior:** The popup's contents can belong to one exact container while its title is derived from a different same-definition inventory instance.
- **Expected behavior:** Every part of an exact-target popup describes the selected instance, or the title must be definition-only so it cannot imply instance identity.
- **Scope and constraints:** Surfaced while reviewing PR #1169 / issue #1088's representative-instance and responsive-target contracts. Preserve the nil-`instanceId` compatibility path that intentionally falls back to the first definition match.
- **Remaining uncertainty:** No current data-authored container has a per-instance display-name difference, so this is a latent identity-boundary inconsistency rather than a reproduced player-visible defect at capture time.

## 2. Save compatibility enforcement

### PRR-4. Enum audit cannot see same-arity payload changes

> **Captured note:** The append-only enum audit freezes constructor name and arity, but not the ordered types of a constructor's serialized fields. Reordering two fields or changing a field type without changing arity leaves the baseline identical even though Generic `Serialize` changes the payload wire shape.

**Verification:** Verified — the parser deliberately reduces every constructor to `(name, arity)`, and comparison has no remaining representation from which it could detect a same-arity payload mutation.

**Evidence:**

- `tools/enum_append_only_audit.py:4` — the audit explains that Generic cereal writes a positional constructor index followed by positional constructor fields.
- `tools/enum_append_only_audit.py:64` — the documented baseline contract records only the ordered constructor list and each constructor's arity.
- `tools/enum_append_only_audit.py:209` — `Constructor` contains only `name` and `arity`.
- `tools/enum_append_only_audit.py:628` — parsing counts record or positional fields and discards their names, order-specific identities, and types before constructing the model.
- `tools/enum_append_only_audit.py:1159` — compatibility compares only those reduced `Constructor` values.
- `docs/save_compat/enum_baseline.json:105` — guarded on-wire sums such as `NameExpr` have non-nullary alternatives, so the omitted payload-shape dimension is present in the actual guarded set.
- `tools/enum_append_only_audit.py:1666` — the self-test proves an arity change fails but has no same-arity field-type or field-order mutation case.

**Handoff context:**

- **Current behavior:** A guarded constructor can retain its name and field count while changing the meaning or codec of one or more positional payload fields, and this audit still reports an exact baseline match.
- **Expected behavior:** Save-wire enforcement must either detect destructive payload-shape changes in guarded sum alternatives or state and mechanically enforce which other gate owns them, without presenting name-plus-arity as the complete payload contract.
- **Scope and constraints:** Surfaced while reviewing PR #1172 / issue #1145. Single-constructor record field order is explicitly assigned to the frozen-DTO boundary; this concern is the analogous payload inside the multi-constructor sums that this audit does own. Preserve compatible trailing-constructor ratcheting and historical component attribution.
- **Remaining uncertainty:** Existing binary compatibility fixtures may incidentally catch changes to the particular alternatives and values they carry, but the audit's own documentation records that fixture constructor coverage is partial.

## 3. Synchronous scripting APIs

### PRR-5. World-name suggestion accepts unbounded linear work

> **Captured note:** Bound the public `world.suggestName` ordinal contract or make large ordinals complete without replaying every prior head choice. The Lua API accepts an arbitrary integer, and `headIndexAt` walks from ordinal zero to that value synchronously.

**Verification:** Verified — the registered Lua call forwards any nonnegative machine-sized ordinal into a tail-recursive loop whose iteration count is linear in that caller-controlled value.

**Evidence:**

- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:319` — `worldSuggestNameFn` reads the second Lua argument as an integer, clamps only its lower bound, and synchronously resolves that ordinal.
- `src/Language/Suggest.hs:215` — the implementation explicitly describes walking the chain from zero with work per ordinal.
- `src/Language/Suggest.hs:221` — `headIndexAt` increments `k` one at a time until it exceeds the requested ordinal.
- `src/Language/Suggest.hs:252` — `suggestionExprAt` likewise clamps only negative values before invoking that walk.
- `test-headless/Test/Headless/Language/Suggest.hs:118` — reroll coverage exercises ordinals only through 39 and has no public-API bound or large-ordinal timing case.
- `test-headless/Test/Headless/Language/Suggest.hs:226` — the cache contract notes that the dice button runs synchronously on the UI's own thread.

**Handoff context:**

- **Current behavior:** A script or debug-console caller can pass a very large valid integer to `world.suggestName`, monopolizing the Lua/UI thread while the generator replays that many prior head choices.
- **Expected behavior:** Every accepted public ordinal completes within a documented, UI-safe bound; inputs outside the supported reroll domain fail promptly rather than initiating caller-sized synchronous work.
- **Scope and constraints:** Surfaced while reviewing PR #1173 / issue #1106 requirement 8. Normal Create World use increments from zero and must retain deterministic adjacent-meaning changes in one language.
- **Remaining uncertainty:** The unbounded complexity is established statically; no intentionally large ordinal was executed because doing so would stall the shared test process. Normal player sessions are unlikely to reach a harmful ordinal without another script or debug call.
