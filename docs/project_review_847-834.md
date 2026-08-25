# Project Review Findings: PRs #847–#834

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #847, #846, #845, #840, #843, #842, #841, #838, #839, #837, #836, and #834 — plus direct first-parent commit `e067378a` in the same window, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #846's unbound-key outcome, #845's unified wheel routing, #843's remote-portal warning (including the later active-page guard), #838's page-owned LOS/awareness, #837's portal/location exclusion, and #834's calorie-store threshold effects retain their intended contracts in the current tree. No separate current concern was found for those PRs or for direct documentation commit `e067378a`.

## Status

- [x] PRR-1. Deferred mouse outcomes lose their press-time framebuffer coordinate contract — [#1676]
- [x] PRR-2. Construction cancellation and claim state identify jobs by coordinate without their world page — [no-issue]
- [x] PRR-3. A full-file revert can retain a stale `reviewed:approve` label — [#1679]
- [ ] PRR-4. A paused craft bill keeps drawing power forever when its working claimant dies
- [ ] PRR-5. The location-anchor validator and matcher duplicate the vocabulary and retain an unconstrained fallback
- [ ] PRR-6. The flora-growth probe shares fixed fixture paths and never cleans them

## 1. Deferred input-coordinate ownership

### [#1676] PRR-1. Deferred mouse outcomes lose their press-time framebuffer coordinate contract

> **Captured note:** Store a deferred gesture's framebuffer-space press position, or store the window/framebuffer geometry that interpreted the press, and use that durable value on every resolution path. The current tuple retains only raw window coordinates: an ordinary release reconverts them using release-time dimensions, while focus-loss/minimize writes them without any conversion at all.

**Verification:** Verified structurally. The immediate and ordinary-release paths that PR #847 edited convert through `windowToFb`, but the separate focus-loss/minimize resolver was not edited and still publishes the raw stored tuple. The same tuple cannot reproduce the original framebuffer position after a DPI or window/framebuffer-size change between press and release because it carries no press-time scale.

**Evidence:**

- Issue #774 / PR #847 require every Layer-A screen location to share the framebuffer-pixel space used by screenshots, injected actions, and widget bounds, including swallowed/no-op and click-versus-drag outcomes.
- `src/Engine/Input/Types.hs:18-26` defines `inpPendingUIClick` as `(kind, callback, press-x, press-y)`. The coordinates are GLFW window pixels; no framebuffer position, scale, or viewport snapshot is retained.
- `src/Engine/Input/Thread/Mouse.hs:56-74` reads window/framebuffer dimensions once for the event currently being dispatched and builds `toFb` from those live values.
- `src/Engine/Input/Thread/Mouse.hs:439-457` retrieves the earlier press coordinates on release and converts the selected press/release location with that release event's `toFb`. A monitor-DPI or resize change during the hold therefore reinterprets the old window coordinate under the new ratio.
- `src/Engine/Input/State.hs:99-125` is the focus-loss/minimize resolver for pending clicks. It writes `px`/`py` directly to `aoWhereX`/`aoWhereY`; it neither reads framebuffer geometry nor calls `windowToFb`. At 2× scale a press delivered at window `(75,32)` is recorded as `(75,32)`, not its framebuffer point `(150,64)`.
- The new #847 Hspec case at `test-headless/Test/Headless/Input/LayerA.hs:498-528` covers a normal release at a constant 2× ratio. The existing focus-loss case immediately above it runs at the fixture's 1× dimensions, so both the missed conversion and a mid-gesture ratio change remain invisible.
- `scripts/unit_drag_select.lua:301-311` has the analogous delayed conversion: `recordDeferredClick` calls `toFbCoords` only when resolving the saved raw press. A scale change between `deferClick` and resolution has the same press-location reinterpretation on the Lua route.
- Targeted tracker and findings-report searches found closed #774 but no follow-up owning the unresolved deferred/focus-loss paths.

**Handoff context:**

- **Current behavior:** On a scaled display, a focus loss or minimize during a pending UI/camera gesture records window pixels in a framebuffer-space ledger. On a normal release after a resize or DPI transition, the original press is converted with the wrong ratio. Either result breaks the F4-to-screenshot/widget join #774 was meant to establish.
- **Expected behavior:** Every resolution path emits the framebuffer position that corresponded to the chosen event when that event occurred. Focus-loss, normal release, Lua cancellation, and view-transition cancellation must use the same coordinate contract.
- **Scope and constraints:** Surfaced in PR #847 / issue #774. Preserve window-space drag-threshold comparisons and Lua/UI routing; only the diagnostic `where` representation changes. A small typed pending record is preferable to adding more tuple positions without names.
- **Remaining uncertainty:** The focus-loss mismatch is direct. The live-ratio case needs a synthetic press/resize/release regression to pin the desired resize semantics, but reinterpreting a past GLFW coordinate with future dimensions cannot preserve its original visual point.

## 2. Construction job page identity

### [no-issue] PRR-2. Construction cancellation and claim state identify jobs by coordinate without their world page

> **Disposition:** No issue — fixed after this report was captured. Commit `9d946e2a` (PR #1346 / issue #1329) replaced the bare `"x,y"` construct-claim key with `scripts/unit_ai_claims.lua`'s page-qualified `key(wid, x, y)`, and gave `abandonClaim`, `releaseConstructJob` and `sweepConstructClaims` an explicit `wid`; `scripts/build_tool.lua:1207,1219` now passes the same page id to both the engine cancellation and the claimant interrupt. Covered by `test-headless/Test/Headless/Lua/UnitAiLoadReset.hs:349` ("keeps the same coordinate on two pages as two distinct claims").

> **Captured note:** Include `WorldPageId` in construction claim keys and cached jobs, and pass it through cancellation/abandon/release operations. The engine-side cancellation is correctly page-scoped, but the Lua-side claimant interruption immediately drops that page identity and can clear a worker belonging to a different page at the same `(x,y)`.

**Verification:** Partially verified structurally. Every identity-bearing engine call accepts or derives a page, while the Lua claim registry and `s.constructJob` cache do not. Two pages can legally contain designations at the same coordinates, and the active-page switch leaves the module-global registry alive. A full two-page runtime reproduction was not run; the collision follows directly from the key and call signatures.

**Evidence:**

- PR #840 / issue #799 added `cancelDesignationForRefund(pageId, gx, gy)` as an atomic page-owned pop and then added `abandonClaim(gx, gy)` to stop the live Lua claimant from finishing a canceled job.
- `scripts/build_tool.lua:1124-1147` captures the HUD world id and passes it to `cancelDesignationForRefund`, but calls `constructAi.abandonClaim(gx, gy)` without that same id immediately afterward.
- `scripts/unit_ai_construct.lua:45-47` declares one process-global `constructClaims` table keyed only as the string `"x,y"`.
- `scripts/unit_ai_construct.lua:98-107` removes that coordinate key and clears any referenced unit state whose cached job has matching `x`/`y`. Neither comparison checks a page.
- The broader claim lifecycle has the same omission: `releaseConstructJob` at `scripts/unit_ai_construct.lua:76-90` deletes an `x,y` key and sends status to whatever `world.getActiveWorldId()` returns at release time; `sweepConstructClaims` at `:115-128` looks up each active page's jobs in the same page-less table.
- `findConstructJob` and `constructExecute` resolve the active world afresh (`scripts/unit_ai_construct.lua:157-167,297-302`), while the cached candidate/job created at `:306-350` stores coordinates, pack, and phase but not the page it was claimed from.
- `unitAi.update` enumerates active-page units only (`scripts/unit_ai.lua:467-490`), so changing pages pauses the old worker's ticks but does not clear `constructClaims`. A same-coordinate job on the new page can collide with that retained key; canceling it can clear the old page's cached claimant, while timeout/adoption later repairs only the durable status.
- Existing construction-probe phases cover claimant death, save/load, rapid same-tile cancellation, and payment/refund races on one page. Repository searches found no same-coordinate/two-page claim or cancellation case and no tracker/report owner for it.

**Handoff context:**

- **Current behavior:** A claim on page A can temporarily block a same-coordinate job on page B. Canceling page B's designation can delete page A's claim entry and clear page A's worker's local job even though page A's durable designation was not canceled. Release and stale-sweep paths can likewise write status against the page active at the later callback rather than the page originally claimed.
- **Expected behavior:** The page is part of a construction job's identity from discovery through claim, payment, interruption, completion, and cancellation. Operations on `(page B,x,y)` never inspect or mutate `(page A,x,y)`.
- **Scope and constraints:** Surfaced through PR #840's new claimant-interrupt path, with the root identity omission shared by the older construction claim registry. Preserve the synchronous atomic engine pop and durable `cdMaterialsPaid` accounting; do not regress the same-page rapid-cancel fixes.
- **Remaining uncertainty:** The immediate wrong-claim clear assumes a retained page-A claim and a page-B cancellation at the same coordinates, a legal multi-world state not exercised live during this review. Whether the user can reach it through current page-switch UI affects severity, not the identity collision in the public/module APIs.

## 3. Review-gate revert handling

### [#1679] PRR-3. A full-file revert can retain a stale `reviewed:approve` label

> **Captured note:** Decide overlap against the approved revision's file set as well as the new revision's, or compare the reviewed patch identities directly. `gh pr diff --name-only` runs after the synchronize push, so a commit that completely removes one approved file from the PR also removes the evidence that the changed path belonged to the approved patch.

**Verification:** Verified from the workflow logic and with a deterministic shell reproduction of its `comm -12` decision. A push diff containing only `fully-reverted.lua` intersected an after-push PR file list containing only `still-owned.lua` as empty, exactly selecting the workflow's keep-label branch. An ordinary edit to `still-owned.lua` intersected and selected stripping as expected.

**Evidence:**

- PR #842's goal is to preserve approval only for a branch-update push that introduces no change to the PR's own content; real PR-content changes must still strip the label.
- `.github/workflows/review-gate.yml:96-100` computes `drifted_files` from the synchronize event's before/after SHAs, correctly identifying paths changed by the new push.
- `.github/workflows/review-gate.yml:106-111` fetches `pr_files` with `gh pr diff --name-only` after checkout of the new head and intersects only that current set with `drifted_files`.
- If approved revision R1 modifies files A and B, then R2 completely reverts A to the base branch while retaining B, `drifted_files={A}` but the new three-dot PR diff reports `pr_files={B}`. The intersection is empty and lines `:115-116` keep `reviewed:approve`, although the approved patch lost all of A's changes.
- The same false negative occurs when the last remaining change is reverted: depending on `gh pr diff` output, the workflow can reach either the empty-PR-file result or its guarded command path without a positive overlap proving staleness.
- The review added failure guards for `git`/`gh` command errors, but there is no before-push PR file snapshot, stored approved file set, or patch/head identity in the label decision.
- No workflow self-test covers file addition/removal/reversion matrices. Targeted tracker and report searches found PR #842 itself but no follow-up owning the after-push file-set false negative.

**Handoff context:**

- **Current behavior:** An approved PR can push a commit that removes a whole approved file change and retain `reviewed:approve`. The required review check can therefore pass a materially different patch without rerunning the reviewer.
- **Expected behavior:** Only ancestry-only/base-update changes preserve approval. Adding, editing, deleting, or fully reverting any content in the approved PR patch invalidates it, including paths absent from the new net diff precisely because the push removed them.
- **Scope and constraints:** Surfaced in PR #842. Preserve the desired no-op branch-update behavior and fail-closed handling for unavailable SHAs/API results. A robust fix needs an approved-head or approved-patch identity; relying solely on the new head's path list is insufficient.
- **Remaining uncertainty:** GitHub event/checkout availability may shape the implementation, but the set-theory counterexample is deterministic and independent of API timing.

## 4. Paused craft claimant death

### PRR-4. A paused craft bill keeps drawing power forever when its working claimant dies

> **Captured note:** Reconcile dead/stale claimants independently of fresh-claim eligibility. A paused bill should remain unavailable for new work, but its dead holder must be cleared and `cbWorking` must become false so the station stops drawing recipe power and the persisted bill returns to an honest idle state.

**Verification:** Verified structurally. The state transition has no autonomous cleanup: pausing preserves `cbWorking`, a dead worker cannot execute the Lua completion/release path, and `claimAvailable` deliberately prevents another worker from taking a paused dead claim. The power fold accepts the stale claimant/working pair without an alive check.

**Evidence:**

- Issue #796 / PR #841 require paused work to stop after the current cycle, `drainW` to return to zero and stay there, and stale/dead claimant takeover plus power accounting to remain correct.
- `Craft.Bills.setBillPaused` at `src/Craft/Bills.hs:352-362` changes only `cbPaused`; it intentionally leaves claimant, progress, and working state untouched so a live current cycle can finish.
- `claimAvailable` at `src/Craft/Bills.hs:248-254` permits only the same holder while paused. The dead/stale takeover branches are nested under `not (cbPaused bill)`.
- The focused spec explicitly locks in this state at `test-headless/Test/Headless/Craft/Bills.hs:178-183`: a different worker may not clear/take a paused bill even when the alive predicate reports the current claimant dead.
- `scripts/unit_ai_craft.lua:366-376` clears a paused not-yet-working job when its living holder next executes. A worker that has been destroyed has no next tick, and no separate sweep calls `releaseBill` for it.
- `Power.Network.activeCraftConsumersOn` at `src/Power/Network.hs:478-494` takes no `UnitManager`/alive predicate. It counts any bill with `cbClaimant /= Nothing` and `cbWorking=True`, including the paused bill's absent claimant, and continues charging the full recipe wattage.
- `cbClaimant` and `cbWorking` are persisted verbatim. Load integrity tolerates a claimant absent from the whole session (`src/World/Save/Component/Entities.hs:885-893`), so save/load does not guarantee this state is repaired.
- Existing #841 runtime probes cover pause during fetch/walk/live work and subsequent unpause, not destruction of the working claimant while the bill is paused. Tracker/report searches found no follow-up beyond closed #796.

**Handoff context:**

- **Current behavior:** Destroying the worker during the permitted final cycle leaves the paused bill visibly working, permanently claimed by a missing unit, and continuously draining its recipe's power until the player unpauses it and another claim attempt happens to replace the stale holder. It can survive save/load in that state.
- **Expected behavior:** Claim liveness cleanup clears claimant and working flags even while pause continues to block fresh claims. Progress and remaining count stay queued; unpausing later permits a fresh worker to resume from the retained progress.
- **Scope and constraints:** Surfaced in PR #841 / issue #796. Do not let a replacement start while paused and do not discard partial progress or fetched inventory. Separate “clean stale ownership” from “is eligible for a new claim.”
- **Remaining uncertainty:** A runtime probe should confirm the exact destroy/death API path used by normal gameplay, but every downstream power/state predicate required for the stuck state is present and there is no cleanup owner in the current code.

## 5. Location anchor authority

### PRR-5. The location-anchor validator and matcher duplicate the vocabulary and retain an unconstrained fallback

> **Captured note:** Parse anchor tags into one shared closed type or central lookup that owns both validation and semantics. The current loader list and placement pattern match are manually duplicated, and the matcher's wildcard still returns `True`; adding a newly “valid” tag on only the loader side silently recreates the arbitrary-placement bug #801 was meant to eliminate.

**Verification:** Verified structurally as a drift-prone correctness gap. Current shipped YAML tags are handled, so no present data file is mis-placed. The implementation nevertheless contradicts the issue's one-authoritative-vocabulary requirement and preserves the exact dangerous fallback for any future accepted/directly-constructed tag.

**Evidence:**

- Issue #801 / PR #839 require one authoritative anchor vocabulary and require unknown or unimplemented tags never to become unconstrained matches.
- `src/Engine/Asset/YamlLocations.hs:97-111` defines `validAnchorTags` as a private `[Text]` in a zero-local-dependency asset module.
- `src/Engine/Asset/YamlLocations.hs:204-208` validates YAML membership in that list but does not attach semantics or convert to a closed runtime representation; `LocationDef.ldAnchor` remains `[Text]` (`src/Location/Types.hs:68-76`).
- `src/Location/Overlay.hs:353-374` separately spells the supported tags in `anchorOk`. Its wildcard branch remains `_ -> True`, the exact unconstrained behavior that made #801 necessary.
- The PR body explicitly says the vocabulary was duplicated to avoid an import. That means adding a tag to `validAnchorTags` without adding the corresponding `anchorOk` branch makes normal YAML accept it and placement treat it as always satisfied.
- A `LocationDef` built directly in Haskell bypasses loader validation immediately; the PR deliberately retained `True` for that case. Even if current production definitions all originate in YAML, the runtime type advertises no invariant that its text has been validated.
- Tests enumerate the current eight tags and rejection cases, but no test derives matcher coverage from the validator's accepted set or proves that every accepted tag has non-wildcard semantics. Two manually updated lists can make the suite green while drifting later.
- Targeted searches found only closed #801 and no report or tracker entry owning consolidation of the validation/semantic authority.

**Handoff context:**

- **Current behavior:** Today's eight YAML tags work, but the next vocabulary extension can be accepted by the loader and silently impose no constraint if the separate matcher is missed. Programmatic runtime definitions with arbitrary text already receive the unconstrained result.
- **Expected behavior:** It is impossible to construct an accepted anchor without selecting explicit semantics. Unknown values fail closed with the authored definition/tag diagnostic; deliberate no-constraint modifiers such as `waterside` are explicit constructors/entries rather than a wildcard effect.
- **Scope and constraints:** Surfaced in PR #839 / issue #801. Preserve `Engine.Asset.YamlLocations`'s Base-style dependency boundary by moving the closed vocabulary/decoder to an appropriate dependency-free module if needed, rather than importing the YAML layer into placement.
- **Remaining uncertainty:** This is a latent extension/direct-construction defect, not a claim that current `data/locations/*.yaml` contains an unsupported accepted tag. The processor may choose code-health priority if production construction is guaranteed YAML-only.

## 6. Flora probe fixture ownership

### PRR-6. The flora-growth probe shares fixed fixture paths and never cleans them

> **Captured note:** Give each flora-growth probe invocation an isolated temporary resource/fixture directory and remove it in an outer `finally`. `probe_berry.yaml` and the new `probe_clover.yaml` are written to fixed shared `/tmp` names, so concurrent runs overwrite one another's inputs and every success/failure leaves stale fixtures behind.

**Verification:** Verified structurally. The probe writes both definitions unconditionally at fixed process-global paths, loads them by those names, and its sole `finally` shuts down the engine without unlinking either file. The PR body explicitly treats the absence of cleanup as satisfying “preserve fixture cleanup,” which is the opposite of owning the new fixture safely.

**Evidence:**

- Issue #798 / PR #836 require a probe-owned no-fruiting fixture and call out preserving temporary-fixture cleanup behavior.
- `tools/flora_growth_probe.py:37` sets the shared root to `/tmp` for all invocations.
- `tools/flora_growth_probe.py:159-170` writes `/tmp/probe_berry.yaml` and `/tmp/probe_clover.yaml` with ordinary truncating `open(...,"w")`, then tells the running engine to load those paths.
- Neither path contains a PID, random suffix, port, seed, or per-run directory. Two probes running concurrently can overwrite the same file between one process's write and engine-side read, and unrelated users/processes can pre-create those names.
- `tools/flora_growth_probe.py:250-384` has one `try/finally`; the `finally` calls only `quit_engine`. Repository searches find no `remove`, `unlink`, `rmtree`, or temporary-directory context for either fixture.
- PR #836 introduced the second fixed path and states in its “Spec note” that cleanup is “satisfied as-is” because the earlier berry fixture was also never deleted. That preserves an existing leak and doubles the shared collision surface rather than preserving an actual cleanup contract.
- Nearby probes demonstrate the established safe shape: `chop_probe.py`, `construction_probe.py`, `plant_probe.py`, `power_probe.py`, and others allocate `tempfile.mkdtemp(prefix=...)` and remove it in `finally`.
- The probe also shares `/tmp/flora_growth_probe_engine.log`; fixing fixture ownership is the minimum finding, while folding the log into the same per-run directory would close the analogous collision.
- Targeted tracker and findings-report searches found closed #798 but no follow-up owning flora-probe artifact isolation/cleanup.

**Handoff context:**

- **Current behavior:** Every run mutates two globally predictable YAML files and leaves them behind. Concurrent or interrupted runs can load a fixture another run just replaced, producing nondeterministic placement/test results and stale diagnostic artifacts.
- **Expected behavior:** The invocation owns unique fixture and log paths from creation through engine shutdown and removes what it created on success, failure, timeout, or early return. Cleanup never deletes a pre-existing path owned by someone else.
- **Scope and constraints:** Surfaced in PR #836 / issue #798. Preserve registration order (`real flora -> probe_berry -> probe_clover`) because placement hashes depend on it; isolation must not change the logical load order or fixture contents.
- **Remaining uncertainty:** A live collision was not orchestrated because the fixed shared files and overwrite are direct. If retaining fixtures on failure is desired for diagnosis, make it an explicit opt-in under the invocation-owned directory rather than the default behavior.
