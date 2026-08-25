# Project Review Findings: PRs #938–#910

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #938, #941, #940, #937, #929, #935, #930, #928, #926, #927, #924, and #910 — plus the five direct first-parent documentation commits in the same window, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #938's non-regular-file animation discovery is already PRR-3 in `docs/project_review_1035-1020.md`; PRs #937 and #927 overlap the open expedition-scenario findings/issues #1212, #1216–#1219, and #1221; and PR #924's temporary scope rule was deliberately retired when the expedition arc shipped. Those concerns are not duplicated here. The intervening direct commits (`2e4993a2`, `23cb7759`, `1bb1a9f6`, `d200fd74`, and `2c2560ee`) only created or dispositioned findings-report bookkeeping and introduced no separate executable defect.

## Status

- [x] PRR-1. Pickup orders inspect the active page but commit against the carrier's page — [#1666]
- [x] PRR-2. An empty location table can restore an allocator below its first valid id — [#1667]
- [ ] PRR-3. Persisted location geometry bypasses component validation
- [ ] PRR-4. The authoritative EngineEnv inventory reports the wrong field total

## 1. Ordered ground-item retrieval

### [#1666] PRR-1. Pickup orders inspect the active page but commit against the carrier's page

> **Captured note:** Make every phase of `unitAi.commandPickup(uid, gid)` resolve the ground id on the carrier's owning page. The #1208 repair moved the atomic pickup verb to that page, but #929's command-time, utility, movement, capacity, label, and event calculations still inspect `item.listGround()` on the active page.

**Verification:** Verified structurally, with narrowed exposure — the Lua order reads an active-page entry three times and ultimately passes the same page-local numeric id to an owning-page mutation. The ordinary context-menu/update path normally keeps the unit and active page aligned, but the reusable command accepts arbitrary live unit ids and does not enforce that precondition.

**Evidence:**

- Issue #920 / PR #929 explicitly require the command-time and arrival-time gates to measure the same live ground instance that is eventually recovered; the PR describes `item.listGround().weight` and `item.pickupGround` as the two halves of that sequence.
- `scripts/unit_ai_pickup.lua:35-40` — `pickupGroundEntry` has only a numeric gid and searches the ambient result of `item.listGround()`; no uid or page participates in the lookup.
- `scripts/unit_ai_pickup.lua:99-131`, `:134-175`, and `:186-198` — the utility phase, execution phase, and public command entry all use that helper for existence, position, stall progress, capacity, item label, and event coordinates. The final execution then calls `item.pickupGround(uid, gid)` with no proof that the inspected entry came from the uid's page.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:129-138` — `item.listGround()` resolves `activeWorldStateFrom` and enumerates only that page's `wsGroundItemsRef`.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:278-305` — since issue #1208 / PR #1236, `item.pickupGround(uid, gid)` deliberately resolves `unitOwningWorldState`; its own contract notes that same-numbered gids on two pages identify different items.
- `src/Engine/Scripting/Lua/API/Units/List.hs:37-49` and `:65-72` — the normal `unitAi.update` loop receives active-page units only. This makes the everyday player path substantially safer, but it is an ambient caller-side coincidence rather than a contract enforced by `commandPickup`.
- The focused `--match "ground item page ownership"` group passed with 16 examples during this review. Those #1208 regressions prove the four Haskell verbs now mutate the owning page; they do not exercise `unit_ai_pickup.lua`'s read-before-commit layer.
- Tracker and findings-report searches found closed #1208 for the original active-page mutation, but no open owner for the remaining read/commit split.

**Handoff context:**

- **Current behavior:** A direct cross-page call can capacity-check and label page B's gid, queue it for a page-A unit, then later retire the order when page A has no such gid or walk to/pick up page A's different same-numbered item. The latter path can emit an event whose name/coordinates came from a different instance than the one committed.
- **Expected behavior:** Command acceptance, progress, movement, arrival capacity, mutation, and success reporting all resolve one `(page, ground id)` identity selected from the carrier's current owning page, or the command refuses a uid/page mismatch before queuing state.
- **Scope and constraints:** Surfaced in PR #929 / issue #920 and exposed by #1208's later ownership repair. Preserve exact item identity, the command-time and arrival-time capacity gates, stall-not-total-trip timing, remove-first atomicity and rollback, active-page UI selection, and quiet retirement when the selected item genuinely disappeared.
- **Remaining uncertainty:** No current player-facing caller was found that supplies an off-active-page uid: selection and `unit.getAllIds()` are both active-page scoped, and inactive units are not ticked until their page becomes active again. The processor should verify whether `unitAi.commandPickup` is intentionally a caller-preconditioned internal surface; if so, an assertion/documented contract may be sufficient. The inconsistency is real at the reusable function boundary, but its present production reachability is narrow.

## 2. Location-instance allocator validation

### [#1667] PRR-2. An empty location table can restore an allocator below its first valid id

> **Captured note:** Validate `lisNextId` itself, even when a saved page has no location instances. The #911 allocator check validates each existing key against the cursor but treats every empty map as well formed, so a decoded cursor of zero or less survives and the next dynamic allocation violates the engine-wide positive-id contract.

**Verification:** Verified directly — evaluating `locationInstanceAllocatorErrors (emptyLocationInstances { lisNextId = 0 })` against the current library returns `[]`, while `allocateLocationInstance` uses that value verbatim as the next `LocationInstanceId`.

**Evidence:**

- Issue #911 / PR #926 require a stable page-local allocator starting at 1. `src/Location/Instance.hs:106-114` names `firstLocationInstanceId = 1` as the convention for every allocator except ground items.
- `src/World/Save/Component/WorldGen.hs:721-736` — the persisted DTO carries `lisdNextId` as an unrestricted `Int`, and decode copies it directly into `lisNextId`.
- `src/Location/Instance.hs:454-475` — `locationInstanceAllocatorErrors` checks the allocator only through comparisons against live map keys and explicitly declares every empty table well formed. It never checks `lisNextId >= firstLocationInstanceId` independently.
- `src/World/Save/Component/Page.hs:825-839` — `validatePages` delegates all location-table validation to that helper, so the malformed empty table reaches the staged page without another allocator guard.
- `src/Location/Instance.hs:359-372` — `allocateLocationInstance` takes `lisNextId` as the new id and advances it; a restored zero cursor therefore allocates id 0, and a negative cursor allocates a negative id.
- `test-headless/Test/Headless/Location/Instance.hs:155-177` — the focused suite proves that an engine-created empty table starts at 1, that live ids must sit below the allocator, and that keys must match records. It has no decoded empty-table case with a malformed cursor.
- The complete `--match "Location instance identity"` group passed with 43 examples during this review; the direct malformed-value evaluation above still returned no errors. Tracker and report searches found no owner for the empty-table allocator floor.

**Handoff context:**

- **Current behavior:** A syntactically decodable world-pages payload with an empty instance map and `nextId <= 0` passes component validation. Any later dynamic location placement uses an id the type's documented allocation domain forbids, and repeated allocations continue forward from the corrupt cursor.
- **Expected behavior:** Every decoded location table requires `lisNextId >= firstLocationInstanceId`, in addition to requiring every live id to be in `[firstLocationInstanceId, lisNextId)` and every key to match its record.
- **Scope and constraints:** Surfaced in PR #926 / issue #911. Preserve deterministic overlay-derived ids, intentional gaps reserved for unregistered definitions, page-local identity, migration of legacy chunk flags, and the rule that a valid empty table starts fresh at 1. Validate current and historical decode paths through their shared component boundary.
- **Remaining uncertainty:** Dynamic post-load placement is still a future-facing seam rather than normal shipped gameplay, so a malformed save may remain inert today. The invalid allocator is nevertheless accepted persistent authority and immediately observable through the exported allocation function.

## 3. Location-instance geometry validation

### PRR-3. Persisted location geometry bypasses component validation

> **Captured note:** Validate the intrinsic shape of stored location-instance geometry before publishing a save. Anchor, absolute bounds, and discovery margin are deliberately durable authority, but the DTO accepts them verbatim and the page validator checks only ids; an inverted footprint, negative halo, or anchor unrelated to its hosting chunk becomes live query, discovery, and placement geometry.

**Verification:** Verified statically — current DTO conversion reconstructs every geometry field without a smart constructor, and `validatePages` reaches only `locationInstanceAllocatorErrors`. The focused geometry tests prove the invariants for engine-created records but never inject a malformed decoded record.

**Evidence:**

- Issue #911 / PR #926 make instance position, resolved tile bounds, and discovery margin stable persisted state. The PR emphasizes that later definition edits must not rederive or reshape those stored values.
- `src/Location/Instance.hs:198-213` — `liChunk`, `liAnchor`, `liBounds`, and `liDiscoveryMargin` are independent record fields and are subsequently treated as the authoritative instance geometry.
- `src/Location/Instance.hs:278-305` — the sole engine constructor derives the anchor from `locationAnchorTile liChunk`, translates definition bounds around it, and copies the definition's validated discovery margin. These relationships establish the live record's intrinsic shape at creation.
- `src/World/Save/Component/WorldGen.hs:670-714` — the current DTO serializes all four values separately and `fromLocationInstanceDTO` copies the decoded anchor, bounds, and margin directly into the live record. No conversion checks ordering, non-negativity, or chunk/anchor coherence.
- `src/World/Save/Component/Page.hs:825-839` and `src/Location/Instance.hs:454-475` — world-pages validation checks duplicate page ids and location allocator/key identity only; it never inspects stored geometry.
- `src/Location/Discovery.hs:166-175` — discovery expands the stored bounds by the stored margin and performs the physical containment test directly. `src/Location/Instance.hs:329-332` likewise supplies stored bounds unchanged to building/location placement exclusion, while Lua queries and map icons read the stored anchor.
- `test-headless/Test/Headless/Location/Instance.hs:181-195` proves valid construction and definition-edit stability, but no save/component test supplies inverted absolute bounds, a negative discovery margin, or an anchor/chunk mismatch.
- Tracker and findings-report searches found validation for authored location-definition bounds and many other save components, but no owner for validating the persisted instance geometry introduced by #911.

**Handoff context:**

- **Current behavior:** Corrupt but decodable instance geometry is published as durable authority. Depending on the fields, discovery can become impossible or occur around the wrong area, map/query coordinates can disagree with the hosting chunk and footprint, and placement exclusion can admit overlaps or reject unrelated terrain.
- **Expected behavior:** Component validation rejects geometry that no current constructor can produce: at minimum inverted absolute bounds and a negative discovery margin, plus any anchor/chunk relationship the format promises to keep invariant. Stored values that pass remain authoritative and are not rederived from today's mutable definition registry.
- **Scope and constraints:** Surfaced in PR #926 / issue #911. Preserve definition-edit stability, seam-aware bounds consumers, footprints that straddle chunks, two instances legitimately sharing a chunk, legacy migration, and clear load diagnostics before staging/publish. This is validation of intrinsic saved shape, not equality against the current YAML definition.
- **Remaining uncertainty:** The exact future contract for relocatable/dynamically placed locations is not yet written. Inverted bounds and negative margins are unconstructible now; whether `liAnchor` must forever equal the original hosting chunk's center should be settled before encoding that relation as a permanent save invariant.

## 4. EngineEnv inventory authority

### PRR-4. The authoritative EngineEnv inventory reports the wrong field total

> **Captured note:** Synchronize or mechanically verify the EngineEnv field total in the capability documentation. Both maintainer entry points say the shared record has exactly 83 fields, while the same checked-in capability audit parses 84 live fields and 84 classified rows.

**Verification:** Verified by the canonical audit — `python3 tools/engine_env_capability_audit.py` reported “84 EngineEnv field(s) all classified” and passed, while both authoritative prose locations still state 83. The focused projection suite also passed, so this is documentation/audit drift rather than a missing capability field.

**Evidence:**

- `CLAUDE.md:144-151` — the repository guidance says `EngineEnv` is one shared record with 83 fields and immediately names the capability inventory/audit as authoritative.
- `docs/engineenv_capability_inventory.md:39-44` — the inventory itself says the record has exactly 83 fields and repeats that all 83 have one row. Its embedded source line anchors (`:68` through `:405`) are stale too; the current declaration begins at `src/Engine/Core/State.hs:69` and `popupQueueRef` is at `:430`.
- Review-time execution of `tools/engine_env_capability_audit.py` succeeded with 84 live fields, 24 permanent full-access modules, and no temporary importer. Thus the row set and enforcement constants are synchronized even though the human-facing total is not.
- The focused `--match "Capability."` binary run passed 117 examples, including all four capability slices in this PR batch: Input (#910), UnitCombat (#930), Ui (#935), and Building (#941). Their projections alias the intended live containers.
- The audit does not reject the stale numeric prose: it verifies the record-to-row set and permanent-importer tables, not the scope section's claimed total. A future field change can therefore repeat this drift while CI remains green.
- Tracker and findings-report searches found no issue owning the 83-versus-84 mismatch.

**Handoff context:**

- **Current behavior:** A maintainer following the designated authority sees a precise but false total in both the repository instructions and inventory. The canonical audit simultaneously reports the correct total and exits successfully, so CI gives no signal that the published scope statement drifted.
- **Expected behavior:** The two maintainer-facing statements match the live audited field set. Either the numeric claim is generated/checked by the audit, or the prose avoids an unguarded exact total while retaining the useful one-row-per-field contract.
- **Scope and constraints:** Surfaced while reviewing PRs #910, #930, #935, and #941 against the capability-split inventory (#876 / epic #537). Preserve the current 84-row classifications, permanent full-access ratchet, thread-private split rules, and passing projections; no capability-record rewrite is indicated.
- **Remaining uncertainty:** This review did not identify which historical edit first made the prose count wrong; the live mismatch and the audit's inability to catch it are settled independently of provenance.
