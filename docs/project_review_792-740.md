# Project Review Findings: PRs #792–#740

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #792, #791, #769, #770, #765, #755, #754, #753, #752, #751, #737, and #740 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #792's probe reclassification has since been superseded accurately where later fixes changed the evidence; PR #791's final-climate rebuild, #770/#765's element/page input-ownership contracts, #755/#751/#740's geology splits, and #753/#752's preview/content art retain their intended behavior in the current tree. PR #754's unsupported movement-probe promotion was already corrected by PR #772 and is accurately documented in the current registry. No separate current concern was found for those PRs.

## Status

- [x] PRR-1. Persistence inventory CI stops at manager pointer fields — [#1703]
- [x] PRR-2. Catatonia can freeze a standing unit in mid-air — [#1709]
- [ ] PRR-3. The lunge landing strike is unreachable through the AI dispatcher

## 1. Persistence inventory audit depth

### [#1703] PRR-1. Persistence inventory CI stops at manager pointer fields

> **Captured note:** Extend the persistence-inventory drift gate through the state-bearing records behind `unitManagerRef`, `buildingManagerRef`, and `utsRef`. Classifying only each pointer as `Rebuild` does not force a persistence decision when an existing manager or unit-sim record gains a new field.

**Verification:** Partially verified structurally. The authoritative inventory explicitly classifies `UnitManager` and `BuildingManager` field-by-field and says their decisions are the interesting state behind the two `EngineEnv` pointers. The audit explicitly excludes both records, however, and treats the pointer rows as the complete mechanically guarded boundary. A new field on either existing manager therefore changes none of the eight records in `ROOT_RECORDS` and can leave the inventory stale while the audit passes. `UnitThreadState` has the same shape behind `utsRef`: its per-unit map is persisted through the `unit-sim` component, but its record is not a scanned owner. This is a verified coverage gap; whether it violates issue #756's deliberately narrow definition of a newly introduced "root state owner" is a contract-scope judgment for the processor.

**Evidence:**

- Issue #756 requires a field-by-field inventory of the unit/building managers and worker-thread simulation state, plus an audit that prevents state from being added without an explicit persistence decision.
- `src/Engine/Core/State.hs:233-245` stores `unitManagerRef`, `utsRef`, and `buildingManagerRef` on `EngineEnv`. The `utsRef` comment says the referenced `UnitThreadState` is the sole authority for movement/timed state and exists there specifically so save/load can snapshot and restore it.
- `docs/persistence_state_inventory.md:90-94` classifies all three pointer fields as `Rebuild`. The unit/building rows defer the "interesting classification decisions" to their pointed-to records in §5; the unit-sim row defers to `wpsUnitSimStates`.
- `docs/persistence_state_inventory.md:328-355` records `UnitManager.umDefs`/`umInstances`/`umSelected`/`umNextId` and the four `BuildingManager` fields individually, but explicitly says the audit does not scan either manager.
- `src/Unit/Types/Manager.hs:37-42` and `src/Building/Types.hs:182-190` are ordinary extensible records. Adding a fifth field changes neither `unitManagerRef` nor `buildingManagerRef`.
- `src/Unit/Sim/Types.hs:186-188` defines `UnitThreadState` as the record behind `utsRef`; its current `utsSimStates` payload is persisted through the inventory's `unit-sim` component, but `UnitThreadState` is not listed under a scanned owner heading.
- `tools/persistence_inventory_audit.py:50-63` hard-codes only `EngineEnv`, `EngineState`, `WorldManager`, `WorldState`, and the four transitional/envelope save records in `ROOT_RECORDS`. Neither manager nor `UnitThreadState` can contribute a missing field to the audit.
- The current persistence inventory audit and its self-test pass; that establishes that the concern is missing coverage rather than an already-stale row in one of the owners the tool does scan.
- Full tracker and findings-report searches found issue #756 and its downstream persistence work, but no follow-up owning drift detection for fields added inside these existing state-bearing records.

**Handoff context:**

- **Current behavior:** CI requires a classification when `EngineEnv.unitManagerRef` itself is added or renamed, but not when durable or transient state is added inside the already-reachable `UnitManager`; the same is true for `BuildingManager` and `UnitThreadState`.
- **Expected behavior:** Every state field the authoritative inventory promises to classify has a mechanical owner/table agreement check, or the docs clearly downgrade the unscanned manager/sim tables from authoritative drift protection and state the manual-only boundary.
- **Scope and constraints:** Surfaced in PR #769 / issue #756. Preserve the audit's exact per-owner heading match, five-label taxonomy, Haskell comment/string handling, Lua registration scan, and component-registry checks. The existing `### UnitManager` and `### BuildingManager` tables already provide natural owner headings; `UnitThreadState` may need one if it is brought into scope.
- **Remaining uncertainty:** Issue #756 defines its audit root narrowly as fields on fixed aggregator records, so the implementation meets that literal definition. The gap matters because the same document separately claims the pointed-to manager fields are authoritative and field-by-field; the processor should decide whether that promise warrants widening the gate or whether a documented manual-only boundary is intentional.

## 2. Catatonia during a leap

### [#1709] PRR-2. Catatonia can freeze a standing unit in mid-air

> **Captured note:** Give mental-break entry a transition-safe way to stop locomotion. Turning a live jump into `Idle` while preserving its interpolated `realZ` and jump endpoints makes catatonia hold a nominally standing unit visibly above the ground for the whole episode.

**Verification:** Verified in a focused real-engine arena reproduction and by the state transitions. An acolyte was allowed to enter a one-tile leap, then the deterministic debug hook forced catatonia while `activity = "transitioning"` and `realZ` was above `gridZ`. Break entry queued `unit.stop`; the resulting state reported `activity = "idle"`, `pose = "standing"`, `gridZ = 0`, and `realZ = 0.598275...`. The same elevated value remained unchanged 1.5 seconds later because catatonia deliberately issues no replacement movement. Ending the forced episode allowed a later walk to normalize `realZ`; the verified player-visible defect is suspension for the episode, not necessarily permanent corruption after the unit next moves.

**Evidence:**

- Issue #717 requires catatonia to stop movement already in flight, keep the unit in place without replacement actions, and leave it standing and physiologically lucid.
- `scripts/mental_state.lua:168-177` enters every break by unconditionally calling `unit.stop`, with a comment that assumes the in-flight operation is a walk.
- `src/Unit/Thread/Movement/Leap.hs:91-117` represents a jump as `TransitioningTo Falling` plus a deadline, from/to positions, and `usJumpApex`; `tickFallZ` interpolates both horizontal position and `usRealZ` along that state.
- `src/Unit/Thread/Command/Motion.hs:142-158` implements `UnitStop` by setting `usState = Idle` and clearing `usTransitionUntil`, but does not snap `usRealX`/`usRealY`/`usRealZ`, reconcile `usGridZ`, or clear `usFallFromTile`/`usFallToTile`/`usJumpApex`.
- `src/Unit/Thread/Movement/Fall.hs:22-52` advances or lands the arc only when the state still matches `TransitioningTo Falling` and the deadline remains present. After `UnitStop`, no transition path can finish the leap while catatonia suppresses new movement.
- The focused arena run observed: before break, `{activity="transitioning", pose="standing", gridZ=0, realZ=0.608938...}`; immediately after the stop, `{activity="idle", pose="standing", gridZ=0, realZ=0.598275...}`; after 1.5 seconds, the second tuple was unchanged.
- `tools/mental_state_probe.py:415-455` covers catatonia only after an ordinary walking command. It checks x/y displacement, the string pose, and the activity label, but never starts a jump/fall and never compares `realZ` with `gridZ`, so this state satisfies the current oracle.
- Full tracker and findings-report searches found the closed source issue #717 but no issue or report entry for transition-safe break entry, mid-leap catatonia, or `UnitStop` preserving an interpolated height.

**Handoff context:**

- **Current behavior:** A catatonic break that begins during the short airborne window cancels the transition machinery at its current interpolated point. The renderer receives a standing/idle unit whose continuous height remains above its grid layer until some later operation happens to normalize it.
- **Expected behavior:** Catatonia reaches a coherent grounded standing state, or safely lets a non-cancelable physical transition finish before holding position. Activity, pose, grid position, continuous position, and transition metadata agree throughout the episode.
- **Scope and constraints:** Surfaced in PR #737 / issue #717. Preserve immediate preemption of ordinary walking/work/combat, physiological lucidity, fixed-duration cooldown behavior, genuine fall/landing injury physics, and the public `unit.stop` semantics expected by other callers. A transition-aware stop primitive or a mental-entry locomotion-only rule are possible directions, not prescribed fixes.
- **Remaining uncertainty:** The deterministic force hook made the timing reliable; a natural break must happen during a roughly one-second leap, so player frequency was not measured. The desired policy for an already-airborne body — complete the arc versus snap safely to a valid tile — also needs an explicit gameplay decision.

## 3. Lash-out and shared lunge completion

### PRR-3. The lunge landing strike is unreachable through the AI dispatcher

> **Captured note:** Let the combat lunge state machine observe its airborne phase independently of the dispatcher's general "do not run AI while transitioning" guard. Otherwise short-reach lash-out units leap, time out, and discard the reach/impact strike the lunge exists to deliver.

**Verification:** Verified in the current control flow and a focused real-engine lash-out run with a red squirrel. The lunge started and the unit traversed a real arc, but `lungeSawAir` remained false for every airborne sample. The dispatcher suppresses `attackTargetExecute` for every `transitioning` tick, while `tryLunge` can set `lungeSawAir` only from inside that suppressed execute path. Once the unit was standing/idle again, the code had never observed `pose == "falling"`, so it waited until `LUNGE_TIMEOUT_SEC`, cleared the phase, and resumed ordinary pursuit. A later ordinary close-range attack landed; the lunge's stored reach and impact-speed strike never did.

**Evidence:**

- Issue #717 routes lash-out through episode-owned real combat behavior and requires its probe to prove an actual attack. `scripts/unit_ai_mental.lua:114-153` directly invokes the shared `attackTargetExecute` for that purpose.
- `scripts/unit_ai.lua:235-247` returns from `tickOne` whenever `unit.getActivity(uid) == "transitioning"`, before mental short-circuit or any combat action executes.
- `scripts/unit_ai_combat_attack.lua:193-220` is the only code that advances phase `"air"`. It sets `lungeSawAir = true` only when an execute tick observes `unit.getPose(uid) == "falling"`, then requires that flag plus a later standing/non-transitioning observation before firing the reach/impact `combat.attack`.
- `scripts/unit_ai_combat_attack.lua:257-271` initializes `lungeSawAir = false` immediately before enqueueing the jump. There is no unit-thread callback or other path that flips it during the transition.
- The timeout branch at `scripts/unit_ai_combat_attack.lua:199-201` clears only `lungePhase`; it does not deliver the landing strike and leaves the other lunge bookkeeping to be overwritten by some future attempt.
- In the focused arena run, a lash-out red squirrel with reported jump reach `4.3528` and attack range `0.1193` entered phase `"air"`. Samples observed `realZ` rise above 1.25 and later `pose="falling"`, but all such samples also had `activity="transitioning"` and the Lua state retained `saw=false`. After landing, `pose="standing"`/`activity="idle"` still had `phase="air", saw=false`; the phase disappeared only on timeout. The target's `lastAttacker` changed later, after ordinary close-range pursuit resumed.
- `tools/mental_state_probe.py:483-595` proves lash-out's real attack using acolytes, whose normal melee reach does not require the short-reach lunge path. It has no assertion that a lunge applies its stored reach/impact strike, and repository search finds no dedicated lunge behavior probe.
- `scripts/unit_ai_combat_attack.lua`'s lunge implementation predates PR #737, but that PR made it the shared combat path for lash-out without adding a transition-compatible advancement path or short-reach species coverage.
- Full tracker and findings-report searches found closed issue #307 for a different lunge stamina calculation, but no issue for the dispatcher making `lungeSawAir` unreachable or for lash-out's short-reach lunge timing out.

**Handoff context:**

- **Current behavior:** A lunge can animate and reposition the actor, but the AI never observes the airborne pose and therefore never executes the landing strike with `s.lungeReach`/`s.lungeImpactSpeed`. After three seconds it silently falls back to ordinary pursuit; species capable of eventually closing to sub-tile range may still attack, masking the missing lunge effect.
- **Expected behavior:** A successfully launched lunge records that it became airborne, resolves exactly once on a valid landing/target, applies its reach and impact contribution, and clears all phase bookkeeping on success, invalidation, interruption, or timeout. Lash-out gets the same correct behavior as ordinary attack targeting.
- **Scope and constraints:** Surfaced while reviewing PR #737 / issue #717, but the root state machine is shared general combat code and likely deserves a subsystem-wide fix. Preserve the dispatcher's protection against arbitrary AI commands clobbering pose transitions, target invalidation/retaliation rules, lunge timeout, stamina/cooldown gates, and save-reference scrubbing for `lungeTarget`.
- **Remaining uncertainty:** The reproduced squirrel eventually landed an ordinary attack after the failed lunge timed out, so this does not prove lash-out can never damage a target. It proves the distinct lunge landing strike — including its extra vertical reach and impact energy — is skipped. The processor should decide whether to file this under the psychology integration or the older general combat/lunge subsystem.
