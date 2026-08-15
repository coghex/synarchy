# Project Review Findings: PRs #398–#348

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #398, #368, #364, #366, #362, #355, #363, #356, #354, #340, #338, and #348 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The color-name correction, z-preserving tile selection, heterogeneous multi-world persistence, jagged-rock slope generation, glacier-rim exposure guard, encumbrance slowdown, canonical seam wrapper, and two asset renames retain their intended core behavior in current source. Focused checks passed for `World.SelectTileZ` (3/3), finite material movement factors (10/10), the quick column-exposure cases (2 passed, one full-tier case pending by design), canonical `wrapChunkCoordU` ownership (3/3), `unit.repairItem`'s finite contract (25/25), encumbrance speed, and the arena variant of the multi-world save/restart/load probe (zero failed checks). The follow-command priority probe passed five of six checks but exposed a randomized-capacity flaw in its pickup fixture; a follow-up run proved the intended pickup arbitration still wins once the command is accepted. No full headless suite, full worldgen tier, graphical session, world check, baseline capture, or `make ci` was run. Four non-duplicate concerns remain, including the lower-severity documentation and probe defects the processor should independently verify before drafting an issue.

## Status

- [ ] PRR-1. `unit.repairItem` turns NaN deltas into total wear
- [ ] PRR-2. Non-finite material movement costs can freeze traversal
- [ ] PRR-3. The follow-command priority probe confounds capacity rejection with arbitration
- [ ] PRR-4. `ItemInstance.iiWeight` still documents the obsolete mass model

## 1. Repair-delta numeric validity

### PRR-1. `unit.repairItem` turns NaN deltas into total wear

> **Captured note:** Reject non-finite `unit.repairItem` deltas before mutating an item. The public Lua primitive accepts `0/0` as a number, and its bare `max`/`min` clamp incidentally maps that NaN to zero, so a malformed repair calculation can break both wear axes rather than failing or leaving the item unchanged.

**Verification:** Verified in a real headless engine and by the current implementation. Calling `unit.repairItem(uid, iid, 0/0, 0/0)` on a held `pick_steel` returned condition/sharpness `0.0,0.0`; an independent `unit.getInventory` read returned the same values. The ordinary repair probe still passes all 25 finite-value checks, so the defect is isolated to the numeric boundary rather than the intended signed-delta, clamping, identity, equipment, or accessory-refresh behavior.

**Evidence:**

- PR #364 / issue #300 introduced the policy-free `unit.repairItem(uid, instanceId, conditionDelta[, sharpnessDelta])` primitive, with positive deltas restoring wear, negative deltas applying wear, and both results bounded to 0..100.
- `src/Engine/Scripting/Lua/API/Units/Equipment.hs:128-143` reads the two deltas with `Lua.tonumber` and converts every returned Lua number directly to `Float`. It checks neither `isNaN` nor `isInfinite`; only a missing/non-number argument falls back to zero.
- `src/Engine/Scripting/Lua/API/Units/Equipment.hs:173-187` computes `cond0 + condD` and `sharp0 + sharpD`, then applies `max 0 (min 100 x)`. For NaN this implementation returns zero as an incidental consequence of `Ord Float`, not as an explicit repair policy.
- The repository already documents this exact trap at `src/Unit/Pathing/Config.hs:85-100`: bare `max`/`min` clamps do not define a safe non-finite policy, `max 0 NaN` silently collapses to `0.0`, and callers should test `isNaN`/`isInfinite` explicitly.
- `src/Item/Types.hs:227-245` specifies both condition and sharpness as 0..100 wear values. The result stays inside that numeric range, so downstream range-only checks cannot distinguish this accidental total breakage from an intentional `-1000` wear operation.
- The live reproduction booted a real engine, loaded the ordinary definitions, created a world, spawned an acolyte and `pick_steel`, invoked the public Lua API with `0/0` for both deltas, and read the item back through a separate API. Both the return table and stored item reported `0.0,0.0`.
- `tools/repair_item_probe.py:173-223` covers ordinary positive/negative deltas, upper/lower saturation, actual-applied amounts, and independent rereads. Its passing 25-check run has no NaN or infinity case, so it does not constrain this boundary.
- Tracker and pending-report searches found no repair-delta numeric-validation issue. Open #1278 covers non-finite values in the separate container-knowledge save component; closed #321 covered Lua-save serialization, not this live item mutation.

**Handoff context:**

- **Current behavior:** A Lua arithmetic error that produces NaN is accepted as a repair delta. Instead of returning `nil`, applying zero, or leaving the item unchanged, the clamp snaps the affected wear axis to zero and reports that destructive result as a successful repair.
- **Expected behavior:** Non-finite deltas have an explicit non-destructive contract, preferably rejection/no mutation consistent with the API's existing `nil` failure shape. Finite positive and negative deltas keep their current additive, saturating behavior and actual-applied reporting.
- **Scope and constraints:** Surfaced from PR #364 / issue #300. Preserve exact-instance lookup across inventory/equipment/accessories, list and slot order, the last-equipped duplicate-accessory buff rule, condition-scaled modifier refresh, signed finite deltas, 0..100 saturation, and the returned result schema. Cover NaN and both infinities at the Lua boundary and in the pure repair core; do not broaden this into unrelated item-field validation unless the processor finds a shared owner.
- **Remaining uncertainty:** The engine's shipped repair policy currently supplies finite authored rates, so the demonstrated path requires a buggy or external Lua caller rather than ordinary gameplay. The processor should decide whether invalid input returns `nil`, raises a Lua error, or succeeds as a no-op; the present silent break-to-zero behavior should not remain accidental.

## 2. Material movement-cost numeric validity

### PRR-2. Non-finite material movement costs can freeze traversal

> **Captured note:** Validate or normalize every material `move_cost` before it reaches the shared registry. `Data.Yaml` accepts an oversized scalar such as `1e999` as positive infinity; route costs cap that value to a finite penalty, but live traversal divides step length by the uncapped infinity, so the planner can return a route that the mover can never advance along.

**Verification:** Partially verified. The parser-to-registry and registry-to-movement paths are direct, and the repository's focused YAML test proves that the same `Data.Yaml`/`Float` combination decodes `1e999` to positive infinity. The finite `surface material movement factor (#312)` suite passes 10/10 but has no malformed material. This review did not alter a material file and boot a unit on it, so the final zero-progress symptom remains a source-proven rather than end-to-end reproduction.

**Evidence:**

- PR #362 / issue #312 introduced `move_cost` as the shared multiplier for both A* route choice and actual traversal speed, with 1.0 as firm ground and larger finite values as progressively softer terrain.
- `src/Engine/Asset/YamlMaterials.hs:25-76` parses `mdMoveCost :: Float` through the ordinary Aeson/YAML `.:?` path and defaults only an omitted field to 1.0. There is no finite, positive, or upper-bound validation.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:104-124` copies `mdMoveCost` unchanged into `MaterialProps`, and `src/World/Material.hs:215-220` stores it as the runtime surface-traversal multiplier.
- `test-headless/Test/Headless/Unit/Pathing/Config.hs:118-127` proves `Data.Yaml` decodes `1e999` into positive infinity for a `Float` field. `src/Unit/Pathing/Config.hs:85-100,131-142` consequently normalizes every pathing-config float with an explicit `isNaN`/`isInfinite` guard, but material loading has no analogous boundary.
- `src/Unit/Pathing/Cost.hs:250-260` returns `max 0.1 mpMoveCost`. That floors finite zero/negative values, incidentally maps NaN and negative infinity to 0.1, but leaves positive infinity unchanged.
- A* consumes the factor through `stepCost` at `src/Unit/Pathing/Cost.hs:84-130`. Its final `clampStepCost` deliberately converts positive infinity to the finite `maxStepCost` of 1e6, so an unavoidable bad tile remains a costly but traversable route edge rather than becoming impassable.
- Live movement separately reads the material under the unit and computes `step = effectiveSpeed * slopeFactor / matSlow * dt` at `src/Unit/Thread/Movement/PathAdvance.hs:102-137`. With `matSlow = +Infinity`, `step` is zero; for any target beyond the small arrival epsilon, the unit stays in the move path without positional progress.
- All shipped explicit values found under `data/materials/` are finite and range from 1.1 to 1.8. The passing 10-case focused suite at `test-headless/Test/Headless/Unit/Pathing/AStar.hs:190-250` exercises finite values 1.05, 1.2, 1.5, 1.6, 1.7, and 12.0 but no NaN/infinity or loader rejection.
- Tracker and pending-report searches found no issue for `move_cost` numeric validity. Open #1278 is specific to restored container knowledge and explicitly does not own a tree-wide float audit; closed #312 is the reviewed feature whose authoring boundary remains.

**Handoff context:**

- **Current behavior:** A syntactically valid oversized YAML scalar can register an infinite movement factor. Routing launders it into a finite high cost and can still select the route, while physical traversal divides by infinity and advances zero tiles per tick. Other non-finite forms receive different accidental clamp behavior.
- **Expected behavior:** Material registration establishes a documented finite positive domain before routing and movement share the value. Invalid authoring either rejects the material with a precise file/material/field error or substitutes a documented safe default; the planner and mover always observe the same finite factor.
- **Scope and constraints:** Surfaced from PR #362 / issue #312. Preserve all valid shipped material output, top-of-column lookup, unknown-material fallback, route-vs-speed parity, soft-ground detour thresholds, the finite step-cost ceiling, and material ids. Add parser/normalization cases for NaN-equivalent construction and oversized positive/negative YAML scalars, plus a small movement case proving progress cannot become zero from content data. Review adjacent material multipliers only if a shared validation helper makes that expansion natural.
- **Remaining uncertainty:** No shipped file currently triggers the defect, and this review did not establish whether the preferred policy is whole-file rejection, per-material rejection, or field fallback. The report processor should verify one temporary malformed fixture before assigning severity and should avoid treating `move_cost: 0` exactly like positive infinity without first deciding whether zero is intended to be invalid or merely floored.

## 3. Follow-command probe staging

### PRR-3. The follow-command priority probe confounds capacity rejection with arbitration

> **Captured note:** Make the pickup-vs-move phase establish that `commandPickup` actually queued an order before it judges AI utility. The probe calls a 10 kg granite chunk “light,” ignores the command's Boolean result, and uses a randomly bodied acolyte; after the command-time capacity gate was added, valid low-capacity rolls now remain in `follow_command` and are reported as a priority-ladder failure.

**Verification:** Verified through the production probe and two isolated headless runs. The unmodified probe failed only its pickup-vs-move check with an action timeline of `follow_command`; the other five checks passed. Repeating the exact staging across twelve fresh acolytes accepted eight pickup commands and rejected four because of their rolled carrying capacities. A final run that first confirmed acceptance selected `pickup_ground` immediately, proving the intended 7.5-over-7.0 arbitration still works when the test's missing precondition holds.

**Evidence:**

- PR #356 / issue #306 introduced `tools/follow_command_priority_probe.py` to prove the re-derived ladder: explicit pickup at 7.5 should beat a pending move at 7.0, while combat/treatment and dire survival remain higher.
- `tools/follow_command_priority_probe.py:137-162` spawns a random acolyte, describes `granite_chunk` as a “light item,” sends `commandMove`, sends `commandPickup`, discards the pickup return value by returning the literal string `"pickup"`, and then fails unless `currentAction` becomes `pickup_ground`.
- `data/items/granite_chunk.yaml:1-8` defines that fixture as a 10 kg block. Acolytes already carry roughly 10.1 or 12.1 kg in the isolated sample, depending on spawn-time shedding.
- `src/Unit/Thread/Command/Body.hs:182-202` derives carrying capacity from randomly rolled lean mass and strength. Its own calibration says weak rolls can be around 11 kg, below the full starting kit, while average rolls are around 23 kg.
- Later #920 made `unitAi.commandPickup`'s command-time capacity check player-facing. `scripts/unit_ai_pickup.lua:178-198` now returns `false` and deliberately leaves `s.pickupOrder` unset when the item would exceed capacity. With no pickup order to score, `follow_command` is the correct arbitration result.
- The production run during this review reported `follow_command` for phase A and failed “pickup_ground wins over a pending move order”; refill-vs-move, move-vs-goal, combat-vs-goal, combat reaching over move, and combat commitment all passed.
- An isolated twelve-spawn sample using the same item and commands produced eight accepted/queued pickup orders and four rejected/unqueued orders. Capacities ranged from about 11.6 to 49.0 kg, making rejection a routine fixture outcome rather than a rare scheduler race.
- A follow-up accepted sample produced the timeline `pickup_ground` on its first poll. The action constants and registration remain correct at `scripts/unit_ai_tunables.lua:20-28,431-443`, `scripts/unit_ai_combat.lua:28-52`, and `scripts/unit_ai.lua:165-193`.
- `tools/ci_probes.py:107-110` classifies this probe flaky only for the separate #724 combat/treatment race. It does not disclose that the first phase can deterministically reject its own command based on a valid body roll. PR #929's later master/branch runs likewise discussed the combat assertions, not this capacity precondition.
- Tracker and pending-report searches found no issue for the stale pickup fixture or ignored `commandPickup` result. Open capacity issues #1212/#1233 concern different provisioning and physical-bulk features; #920's command-time rejection is working as designed.

**Handoff context:**

- **Current behavior:** A correct capacity refusal is misreported as evidence that pickup utility lost to move utility. The probe's pass rate therefore depends on body generation and can hide a real priority regression among unrelated false failures.
- **Expected behavior:** Phase A deterministically stages an admissible pickup, asserts `commandPickup` returned true and both `commandedTask`/`pickupOrder` are live, then judges arbitration. A separate capacity test owns the intentional refusal path.
- **Scope and constraints:** Surfaced from PR #356 / issue #306 and exposed by later #920. Preserve randomized production bodies, spawn-time load shedding, the real command-time capacity gate, 7.5 pickup utility, 7.0 follow utility, all combat/treatment ordering, and the existing manual-only combat-flake disclosure. Prefer a genuinely light target or explicit fixture load/capacity preparation over bypassing the gate; keep an assertion that the target remains present. Update stale “pickup (8.0)” prose in the probe while touching it.
- **Remaining uncertainty:** The independent acceptance run demonstrates no current pickup-utility regression, so this finding is about test trustworthiness rather than gameplay behavior. The processor should decide whether fixing the deterministic setup is worth separating from the broader #724 timing flake or should land as a small probe-maintenance issue.

## 4. Canonical item-mass documentation

### PRR-4. `ItemInstance.iiWeight` still documents the obsolete mass model

> **Captured note:** Correct the load-bearing `ItemInstance.iiWeight` field comment along with the public weight APIs. PR #340 removed the misleading universal “1 L = 1 kg” gloss from four comments after `itemTotalWeight` gained per-container fill density and recursion, but the canonical instance type still says carried weight is only `iiWeight + iiCurrentFill` at 1 kg per litre.

**Verification:** Verified as a current documentation mismatch. The field comment and authoritative helper disagree directly; the public Lua API, building storage, and spawn-time comments now describe the helper correctly. No runtime calculation follows the stale comment, so this is low-severity code-health debt rather than a behavior defect.

**Evidence:**

- Issue #323 / PR #340 corrected `building.getStorageWeight` because its old empty-weight description contradicted `itemTotalWeight`. Review commits then corrected `unit.getCarryingWeight`, removed an inaccurate “1 L = 1 kg” gloss, and updated two spawn-time capacity comments to defer to per-container fill weight.
- `src/Item/Types.hs:229-237` still documents `iiWeight` with “Carried weight = iiWeight + iiCurrentFill (1 L = 1 kg).” This omits both the item's configured `icFillWeight` and all nested `iiContents`.
- The authoritative `itemTotalWeight` at `src/Item/Types.hs:320-338` computes `iiWeight + iiCurrentFill * icFillWeight + sum (map itemTotalWeight iiContents)`. Its documentation explicitly distinguishes fluids at 1 kg/L from discrete fills such as pills around 5e-7 kg each.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:380-405` correctly documents and sums recursive total weight across inventory, equipment, and accessories. `src/Engine/Scripting/Lua/API/Buildings/Materials.hs:166-187` does the same for storage.
- `src/Unit/Thread/Command/Spawn.hs:132-150` now says spawn capacity mirrors `getCarryingWeight`: instance mass, per-container fill weight, nested contents, and worn gear. The stale core field comment is therefore the remaining contradictory statement at the data definition a maintainer is most likely to consult.
- The field-order warning beside `iiWeight` is valid and load-bearing because positional `Generic Serialize` preserves save compatibility. Updating the narrative must not reorder or otherwise alter the record.
- Tracker and pending-report searches found no follow-up. Closed #323 is the documentation repair that missed this site; open #1233 concerns future physical bulk and portable-storage capacity, not the current mass formula.

**Handoff context:**

- **Current behavior:** Runtime callers consistently use recursive, definition-aware `itemTotalWeight`, but the `ItemInstance` field itself advertises a simpler formula that overstates discrete fill mass and omits nested contents. A maintainer can implement a new capacity path from the wrong local contract.
- **Expected behavior:** The field comment states that `iiWeight` is only this instance's empty mass and points to `itemTotalWeight` for carried/contained mass, including configured fill-unit weight and recursive contents.
- **Scope and constraints:** Surfaced from PR #340 / issue #323. This should be a comment-only correction. Preserve field order, serialization/save version, `iiWeight`'s per-instance roll semantics, the no-definition fallback of 1 kg/L, recursive contents, and every current weight calculation. A short reference to `itemTotalWeight` is preferable to another full formula copy that can drift.
- **Remaining uncertainty:** None about the mismatch; only priority is uncertain. The processor may mark it no-issue if one stale internal comment does not justify tracker overhead, but it should not treat the text as an alternate supported mass convention.
