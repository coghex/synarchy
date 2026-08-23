# Project Review Findings: PRs #835–#822

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #835, #832, #833, #831, #830, #829, #828, #827, #826, #824, #825, and #822 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #832's committed building-footprint expansion, #831's tile-Z UI-wiring regression coverage, #830's slider lookup repair, #826's Tiny/Small inland-source extension, and #824's indeterminate input-timeout classification retain their intended contracts in the current tree. No separate current concern was found for those PRs.

## Status

- [x] PRR-1. Soil receivers can credit a high neighbour that cannot shed any soil — [#1591]
- [x] PRR-2. Slot-aware occupancy still stores only one pending construction job per tile — [#1595]
- [ ] PRR-3. The location-content probe shares four fixed temporary fixture paths and never cleans them — [deferred]: NCT-22 unprocessed
- [x] PRR-4. A freshwater tile above four lower neighbours is still forced back to a flat slope — [#1600]
- [x] PRR-5. The save barrier can snapshot Lua before processing causal messages produced by acknowledged owners — [no-issue]
- [x] PRR-6. Negative-infinite pathing cost is clamped to a free step — [#1603]
- [x] PRR-7. The repository-wide lenient UTF-8 invariant has no automated static gate — [#1605]

## 1. Soil donor eligibility

### [#1591] PRR-1. Soil receivers can credit a high neighbour that cannot shed any soil

> **Captured note:** Derive shed credit from a donor state that includes whether the donor actually participates in erosion, or otherwise exclude indestructible neighbour materials. The receiver currently sees elevations only, so it treats every neighbour at least three levels higher as a donor even when that neighbour's own erosion call returns immediately without shedding.

**Verification:** Verified structurally. `applyErosionScalar`'s center-tile hardness guard runs before all last-age soil logic, while `shedCredit` has no neighbour-material or neighbour-hardness input. A receiver next to a high glacier therefore gains a soil layer even though evaluating that glacier as the donor produces `noModification` and removes no soil. The same elevation-only approximation can credit several receivers from one nominal donor, so it is also not a conserved transfer; the indestructible-neighbour case is the direct contradiction.

**Evidence:**

- Issue #812 / PR #835 require soil missing from a steep donor to produce the receiver increase; the PR's central claim is that an uphill neighbour clearing the relief threshold is “guaranteed to expose rock itself.”
- `src/World/Geology/Erosion/Math.hs:112-116` returns `noModification` immediately whenever the center material has `hardness >= 1.0`, explicitly naming glacier and mantle as indestructible. No `exposeRock` or soil-removal branch runs.
- `src/World/Geology/Erosion/Math.hs:256-273` identifies a receiver's donors from the four neighbour elevations alone. The function receives only the center material id/hardness, so it cannot know whether a qualifying high neighbour is glacier, mantle, or another non-eroding material.
- `src/World/Geology/Erosion/Math.hs:280-285` unconditionally adds that inferred `shedCredit` to any eligible receiver's soil depth.
- `data/materials/special.yaml:3-4` defines the real `glacier` material with hardness `1.0`; the scenario is not limited to a synthetic impossible value.
- A cardinal profile with a soft receiver at elevation 50 and a glacier neighbour at 54 is enough: the receiver counts one donor and gains one depth, while the neighbour evaluated with hardness `1.0` takes the early `noModification` path.
- The new `SoilRedistribution` tests vary elevations and use one fixed center hardness (`0.5`). They never supply or model a neighbour material, so they cannot distinguish a real shedding rock face from a high indestructible face.
- Targeted tracker and report searches found closed #812/#225 but no follow-up owning donor material eligibility or transfer conservation.

**Handoff context:**

- **Current behavior:** Eligible lower cells can gain sediment beside a high glacier or other indestructible surface even though no corresponding donor loses a cap. A single high cell can also contribute one credit independently to each lower cardinal neighbour.
- **Expected behavior:** A receiver credit represents soil actually shed by an eligible final-age donor. Indestructible surfaces do not manufacture sediment, and any deliberately non-conservative visual approximation is named and bounded as such.
- **Scope and constraints:** Surfaced in PR #835 / issue #812. Preserve the local, deterministic, one-ring and chunk-order-independent contract; a fix may need the stencil to carry a compact neighbour eligibility signal rather than only elevation.
- **Remaining uncertainty:** This review did not measure how many generated receiver cells border `hardness == 1.0` material at qualifying relief. The pure contradiction exists regardless of prevalence; the broader conservation question may be an accepted visual approximation and should be decided separately.

## 2. Pending construction slot identity

### [#1595] PRR-2. Slot-aware occupancy still stores only one pending construction job per tile

> **Captured note:** Make the structure slot part of pending construction identity, or reject a second compatible-slot designation explicitly instead of reporting it accepted while overwriting the first job. The placed overlay is slot-keyed, but `ConstructDesignations` remains keyed only by `(x,y)`.

**Verification:** Verified structurally. A pending floor at `(x,y)` does not make a wall slot occupied, so PR #829's new slot-specific filter admits a wall designation at the same tile. The subsequent `HM.insert` uses the identical coordinate key and silently replaces the floor job. The probe's “coexistence” phase covers a wall designated beside an already *built* floor, not two simultaneously pending compatible jobs.

**Evidence:**

- Issue #805 requirement 2 says compatible pieces such as a floor and walls may coexist while duplicate instances of the same slot must be refused.
- `src/World/Construct/Types.hs:68-88` stores the target slot only inside `ConstructDesignation.cdTarget`, but defines the whole designation map as `HashMap (Int, Int) ConstructDesignation`.
- `src/World/Thread/Command/Cursor/Construct.hs:90-94` checks only the placed `lcStructures` overlay at `(tile, StructureSlot)`. It does not consult a pending designation occupying the same tile.
- `src/World/Thread/Command/Cursor/Construct.hs:138-150` therefore admits a compatible pending target whenever its *placed* slot is empty.
- `src/World/Thread/Command/Cursor/Construct.hs:151-152` folds every admitted entry into the coordinate-only map with `HM.insert`; a second target at the same tile overwrites the earlier one without rejection, refund, or cancellation outcome.
- Cancellation, status, progress, save DTOs, and Lua query APIs all continue to address a construction job by page plus coordinate, so the missing slot is systemic rather than local to insertion.
- `tools/construction_probe.py`'s PR #829 coexistence case first places a real floor and waits for it to finish, then designates a wall. That proves placed-slot coexistence only and cannot observe pending-floor replacement.
- The comment at `Construct.hs:96-103` currently says compatible slots on one tile coexist, overstating what the pending-job model supports.
- Tracker search found only closed #805; report search found the separate page-identity concern in `project_review_847-834.md`, not same-page slot identity.

**Handoff context:**

- **Current behavior:** Designating floor then wall (or two wall edges) on one still-empty tile retains only the last pending job. The earlier blueprint, claim, progress, and any durable paid marker disappear through an ordinary accepted insertion.
- **Expected behavior:** Compatible slots can remain independently pending and complete on the same tile, or the public designation call rejects the second request before it can erase work. Cancellation and progress address the same slot-qualified job the worker claimed.
- **Scope and constraints:** Surfaced in PR #829 / issue #805. A key-shape change touches persistence compatibility, renderer iteration, Lua job tables, claims, refunds, and construction-slope display; preserve atomic paid-material accounting and canonical world-coordinate handling.
- **Remaining uncertainty:** Current player tooling may make some same-tile sequences awkward, but the public Lua API accepts them and the issue explicitly promises compatible-slot coexistence. No live two-designation probe was needed to establish `HM.insert` replacement.

## 3. Location-probe temporary ownership

### [deferred] PRR-3. The location-content probe shares four fixed temporary fixture paths and never cleans them

> **Deferred:** Open issue #1569 assigns this probe's artifact ownership to NCT-22 in `docs/non_ci_test_audit_findings.md`, so filing this half alone would split one probe's cleanup across two conflicting PRs — clears when NCT-22 is processed, at which point this finding's five `/tmp` fixture paths and its shared log path fold into the `location_content_probe.py` issue that produces.

> **Captured note:** Put all probe-generated location and loot definitions in an invocation-owned temporary directory, pass those unique paths to the engine, and clean them from an outer `finally`. Fixed `/tmp` names are shared across runs and survive every exit path.

**Verification:** Verified structurally. PR #833 added the quinoa location and loot paths beside two existing fixed bogus-content fixtures. All four are opened with truncation before the phase boots, and the only `finally` stops the engine. There is no unlink, directory cleanup, PID/port suffix, or temporary-directory context.

**Evidence:**

- PR #833's deterministic registry scenario added `/tmp/loc_content_probe_quinoa.yaml` and `/tmp/loc_content_probe_quinoa_loot.yaml` as engine-loaded definitions.
- `tools/location_content_probe.py:1010-1035` writes the existing bogus location/loot fixtures to two fixed global paths.
- `tools/location_content_probe.py:1051-1077` writes the quinoa location/loot fixtures to two more fixed global paths.
- `tools/location_content_probe.py:1078-1084` boots only after those writes and asks another process to open the paths, leaving a collision window in which another invocation can truncate or replace a file.
- `tools/location_content_probe.py:1173-1174` cleans up only the engine process. Repository search finds no removal of any `loc_content_probe_*` file.
- The same probe also uses a shared fixed log path, so concurrent invocations can mix warning evidence even if fixture contents happen to match.
- The sixth finding in existing project-review report `project_review_847-834.md` records the identical ownership defect in `flora_growth_probe.py`; no GitHub issue owns the general probe-artifact pattern yet. The processor should consider combining both reports into one scoped audit/fix rather than filing duplicates.

**Handoff context:**

- **Current behavior:** Runs leave four YAML files behind and can read a partial or foreign fixture during parallel execution. A pre-existing path or symlink is overwritten with probe content, and warning checks can read another run's log.
- **Expected behavior:** Every invocation owns unique fixture/log paths for its entire engine lifetime and removes only its own artifacts on success, assertion failure, boot failure, timeout, or interruption.
- **Scope and constraints:** Surfaced in PR #833 / issue #800. Preserve the exact load order and static fixture contents; isolation should not alter the deterministic loot scenario or location-registration order.
- **Remaining uncertainty:** Two normal runs write identical YAML today, so the most likely parallel failure is a truncate/read race or mixed log rather than semantic fixture drift. Future fixture parameterization would make the collision more obvious.

## 4. Four-way freshwater drops

### [#1600] PRR-4. A freshwater tile above four lower neighbours is still forced back to a flat slope

> **Captured note:** Give the all-four-lower case an explicit representable waterfall treatment instead of converting its computed slope mask to zero. PR #828 broadened every neighbour test to recognize multi-level drops, then deliberately erases all four results when they happen together.

**Verification:** Verified as a direct behavior; product correctness is partially verified. `waterSlopeAt` sets all four directional pairs for lower neighbours, producing mask `15`, and immediately returns `0`. The focused test pins this flattening. Issue #816 says a river/lake tile slopes toward every loaded lower cardinal neighbour, but the implementation treats a four-way high point as “not a lip” without evidence that such generated cells are impossible or should visually become horizontal.

**Evidence:**

- Issue #816's first requirement says a rendered river or lake tile must slope toward every loaded cardinal neighbour whose visible surface is lower.
- `src/World/Render/WaterSlope.hs:35-60` now recognizes any lower wet or dry neighbour, in-chunk and cross-chunk, exactly as PR #828 intended.
- `src/World/Render/WaterSlope.hs:67-75` combines all cardinal directions; four lower neighbours produce `raw == 15`.
- `src/World/Render/WaterSlope.hs:76-80` labels that topology an isolated high point and returns `0`, which is the same flat result the exact-one-drop bug produced.
- `test-headless/Test/Headless/World/Render/WaterSlope.hs` explicitly asserts all-four-lower becomes zero. It proves the exception is stable but does not prove the visual result satisfies the waterfall contract.
- A high water cell surrounded by lower water/terrain is precisely a four-sided drop. A flat top may be a useful center surface, but suppressing every directional slope/edge cue makes the returned slope id state that no lower neighbour exists.
- Tracker/report searches found closed #816 and unrelated seam handling, but no follow-up deciding or rendering the four-way topology.

**Handoff context:**

- **Current behavior:** Three lower neighbours produce a directional slope mask; adding a fourth lower neighbour abruptly changes the result to completely flat. A tiny terrain change can therefore remove all slope cues at the most exposed high-water topology.
- **Expected behavior:** The renderer has a documented, tested visual representation for four-way falls that preserves lower-neighbour evidence without requesting an invalid texture mask. If flat-top plus separate side faces is intentionally correct, the test should prove those side faces supply the missing visual contract.
- **Scope and constraints:** Surfaced in PR #828 / issue #816. Preserve the existing masks/direction mapping for one-to-three lower neighbours and cross-chunk parity; coordinate with `SideDecoQuads` so one subsystem does not duplicate another's waterfall sides.
- **Remaining uncertainty:** This review did not render a generated all-four-lower freshwater cell or confirm which slope textures exist for mask 15. The discontinuity and requirement tension are direct; the correct art representation needs visual verification.

## 5. Lua participation in save quiescence

### [no-issue] PRR-5. The save barrier can snapshot Lua before processing causal messages produced by acknowledged owners

> **Disposition:** No issue — the finding's own filing precondition resolves the other way. Enumerating every `luaQueue` writer shows four of the six save owners (`SaveUnit`, `SaveBuilding`, `SaveCombat`, `SaveSimulation`) enqueue nothing at all; the world thread's only non-load site is `LuaStampLocation`, which `World/Thread/ChunkLoading.hs:165-172` documents as re-issued on every chunk load precisely so no queue drain is needed, backed by the persisted `wgpLocationStamped` (`World/Generate/Types.hs:3,178`; `World/Save/Component/WorldGen.hs:1142,1175`) and two independent one-time gates in `scripts/location_stamper.lua:43-56` and `scripts/locations.lua:511-534`; the world's remaining sites belong to the mutually exclusive load transaction; and every `SaveInput` message is player input the finding itself says must stay queued for the resumed session.

> **Captured note:** Treat engine-to-Lua messages produced by pre-boundary worker work as part of Lua's quiescence obligation. `SaveLua` cannot remain permanently acknowledged across passes merely because the interpreter is blocked inside `engine.saveWorld`; blocking the interpreter also prevents it from consuming newly queued causal work.

**Verification:** Partially verified structurally. The world owner performs side-effect-producing chunk work and can enqueue `LuaStampLocation` before acknowledging. The barrier preserves Lua's initial acknowledgement through every later pass, and `saveWorldFn` proceeds directly from worker readiness to `collectLuaComponents` without draining `luaQueue`. The queued callback therefore cannot execute until after the save API returns. A complete disk reproduction proving user-visible loss was not run; location stamping is designed to replay from the persisted overlay, which can mask this specific example after load, but the barrier's “completed causal effects” invariant is not met at capture.

**Evidence:**

- Issue #757 requirement 4 says every persistent mutation accepted before the boundary, including cross-queue causal follow-up work, must be fully reflected or fail; requirement 6 moves registered Lua persistence collection inside that coherent boundary.
- `src/Engine/Scripting/Lua/API/Save.hs:325-358` begins the barrier, acknowledges `SaveLua` once, waits for all owners, enters the snapshot phase, and calls `collectLuaComponents`. There is no intervening `processLuaMsgs` or queue handshake.
- `src/Engine/Save/Barrier.hs:86-96` resets each completed quiescence pass while retaining `SaveLua` in `ssAcknowledged`, on the rationale that the blocked interpreter is already quiescent.
- `src/World/Thread.hs:81-107` drains commands, chunk-init work, clock/chunk updates, and cursor work before its acknowledgement. Those steps are correctly treated as world-owned pre-boundary work.
- `src/World/Thread/ChunkLoading.hs:146-180` can enqueue `LuaStampLocation` while performing that acknowledged chunk work.
- `Engine.Scripting.Lua.Thread.Dispatch.processLuaMsgs` is the only consumer that turns `LuaStampLocation` into the real `onStampLocation` callback; it cannot run concurrently because `saveWorldFn` is executing on that same Lua thread.
- `scripts/location_stamper.lua:43-54` shows the callback can issue persistent location geometry, marker, and content-spawn operations. Those causal effects occur only after Lua resumes, outside the captured transaction, even if the originating chunk load belonged before it.
- Later save-barrier fixes added Input and Render owners and moved the world acknowledgement after side-effect-producing work, but the current tree retains this Lua self-ack design.
- Tracker search found the closed save-overhaul issues and the separate `save_load_findings.md` concerns, but no owner for engine-to-Lua causal work during save capture.

**Handoff context:**

- **Current behavior:** A worker can finish pre-boundary work, enqueue a Lua callback, and acknowledge. The barrier reaches capture while the callback is still pending, so Lua persistence and any Haskell follow-up commands describe an earlier logical moment than the worker state that caused the message.
- **Expected behavior:** The transaction either drains and settles all pre-boundary engine-to-Lua work before collecting components, or establishes a sequence/fence that proves no such message can carry persistent consequences. Messages accepted after the boundary remain queued for the resumed session.
- **Scope and constraints:** Surfaced in PR #827 / issue #757. Avoid recursively processing the save-triggering call or post-boundary input inside `saveWorldFn`; a tokenized queue boundary or a two-phase Lua handshake may be safer than indiscriminate draining.
- **Remaining uncertainty:** Location replay makes the concrete `LuaStampLocation` example recoverable on a later chunk load, and many other `LuaMsg` variants are transient UI notifications. The processor should identify at least one non-replayable persistent callback or formally prove every worker-produced message in this window is transient/idempotently replayable before filing.

## 6. Negative-infinite step costs

### [#1603] PRR-6. Negative-infinite pathing cost is clamped to a free step

> **Captured note:** Classify every non-finite derived cost before sign clamping and map both infinities and NaN to `maxStepCost` (or reject the step). The current order handles NaN and positive infinity but lets negative infinity fall through the ordinary negative-cost branch to zero.

**Verification:** Verified algebraically from the production function. A directly constructed config with `pcClimbFactor = -Infinity` and a positive cliff delta makes the total `-Infinity`; `clampStepCost` sees neither NaN nor `x > maxStepCost`, then returns `0` for `x < 0`. PR #822 explicitly claimed its final guard covers configs that bypass normalization and ensures an extreme step is maximally undesirable, never free.

**Evidence:**

- Issue #815 clarifies that “finite” excludes both positive/negative infinity and NaN, and requires every passable returned total to be finite and non-negative.
- `src/Unit/Pathing/Cost.hs:120-130` derives climb/fall terms from the supplied `PathingConfig` and sends the complete sum to `clampStepCost`.
- `src/Unit/Pathing/Cost.hs:148-166` documents direct construction bypassing `normalizePathingConfig`, including negative or non-finite factors, as a reason the final guard exists.
- `src/Unit/Pathing/Cost.hs:174-179` special-cases NaN and values above the ceiling, then maps every value below zero to `0`. For IEEE `-Infinity`, `isNaN` is false, `x > 1e6` is false, and `x < 0` is true.
- A negative-infinite climb or fluid/combined total therefore becomes a valid zero-cost edge. A* prefers it over ordinary horizontal movement, the inverse of the “extreme step remains maximally undesirable” rationale.
- `normalizePathingConfig` prevents this through the normal YAML path, but tests and public module consumers construct `PathingConfig` records directly; PR #822 intentionally promised defense at this lower boundary.
- The added cost tests cover positive infinity, NaN indirectly, deep default fall overflow, and huge positive finite values. They do not cover `-Infinity` or assert that invalid negative totals are costly rather than free.
- Targeted tracker and report searches found closed #815 but no follow-up for negative-infinite derived totals.

**Handoff context:**

- **Current behavior:** An invalid negative-infinite derived cost is finite after clamping but becomes zero, so it remains passable and can dominate route selection as a free edge.
- **Expected behavior:** Every non-finite total is rejected or assigned the finite maximum penalty consistently, regardless of sign. Ordinary finite negative misconfiguration can follow a separately documented clamp/fallback policy.
- **Scope and constraints:** Surfaced in PR #822 / issue #815. Preserve normal/default costs byte-for-byte and keep the guard at the returned-total boundary; add focused `-Infinity` tests for climb, fall, and the helper's ordering.
- **Remaining uncertainty:** The shipped YAML path normalizes every tunable first, so ordinary content does not currently produce this value. The defect affects the explicit lower-level defense contract and future/direct callers rather than default gameplay configuration.

## 7. UTF-8 sweep enforcement

### [#1605] PRR-7. The repository-wide lenient UTF-8 invariant has no automated static gate

> **Captured note:** Turn the exact strict-decoder search used to accept PR #825 into a small CI audit with a self-test. The manual probe covers two representative calls, not the 55-file tree-wide invariant, so one new strict `TE.decodeUtf8` call can silently reintroduce the failure class anywhere else.

**Verification:** Verified as a missing regression boundary, not a present strict call. The required recursive search is currently clean. Neither CI nor `make ci` runs that search, and `text_encoding_probe.py` remains manual-only/targeted with two API examples. The concern is therefore code-health prevention rather than a current malformed-input crash.

**Evidence:**

- Issue #665 / PR #825 established a repository-wide convention: every direct strict `TE.decodeUtf8` call under `src/Engine/Scripting/Lua/` was replaced (236 expressions across 55 files at merge time).
- The exact current search, `rg -n -P "TE\\.decodeUtf8(?![A-Za-z0-9_'])" src/Engine/Scripting/Lua`, returns no matches.
- `tools/text_encoding_probe.py` exercises malformed input through `engine.setText` and `world.show`; it cannot detect a strict decoder added to any of the hundreds of other Lua boundaries.
- `tools/ci_probes.py --status` classifies `text_encoding` as manual-only `[targeted]`, so normal CI does not run even those two examples.
- Searches across `.github/workflows`, `Makefile`, `tools/ci-local.sh`, and audit tools find no static ban or allowlist for direct strict Lua-tree decoding.
- The repository already uses cheap self-tested static audits for comparable syntactic invariants (Unicode operators, persistence inventory, module budgets, material ids, and report status), so the enforcement shape is established.
- Tracker/report search found #665/#622 history but no issue or finding owning durable enforcement of the completed sweep.

**Handoff context:**

- **Current behavior:** The tree is compliant today, but compliance depends on reviewers remembering an old one-off grep. A valid-looking new Lua API can compile, pass the headless suite, and reintroduce caught `UnicodeException` behavior outside the two manual probe sites.
- **Expected behavior:** Blocking CI rejects a newly introduced direct strict decoder in the scoped tree, with explicit treatment for already non-throwing siblings such as `decodeUtf8'`, `decodeUtf8With`, and `decodeUtf8Lenient`.
- **Scope and constraints:** Surfaced in PR #825 / issue #665. Keep the audit syntax-aware enough not to flag compliant sibling names or comments accidentally, and include a self-test proving both positive and negative cases before adding it to CI/make-ci.
- **Remaining uncertainty:** No current source call violates the convention, so the processor may reasonably classify this as preventative code health rather than a bug. Its value depends on whether maintainers consider the 55-file mechanical sweep a durable policy or a one-time cleanup.
