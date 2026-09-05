# Simulation consistency bug findings

Recorded during the owner's plain-language architecture audit on 2026-09-05, initially against `da96202c863b7d563f4968d34cb685d2e622e73c`; follow-up findings name their own reviewed revision. This file contains concrete bugs; broader design discussion and audit coverage live in [the conversation audit](architecture_conversation_audit_2026-09-05.md) and [the gameplay timing audit](gameplay_timing_audit_2026-09-05.md).

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` deliberately not filed · `[deferred]` blocked on a stated precondition.

## Status

- [ ] CH-1. Lua stance recovery can overwrite a concurrent combat charge
- [ ] CH-2. Lua stamina updates can erase combat costs and bypass exhaustion checks
- [x] CH-3. Timed Lua updates can starve behind sustained message traffic — [#2415]
- [ ] CH-4. The world calendar discards fractional minutes and cannot advance at default speed
- [ ] CH-5. Movement discards remaining elapsed time when reaching a path waypoint

## Shared unit resource mutations

### CH-1. Lua stance recovery can overwrite a concurrent combat charge

**Severity:** Medium

`tickStance` reads current stance, reads dexterity and agility, computes recovery, and calls `unit.setStat` with an absolute replacement. Combat can commit a stance charge between that initial read and the replacement. The replacement is individually atomic but writes a value computed from stale state, erasing the intervening charge.

**Evidence:**

- `scripts/unit_resource_injury.lua:20-28` — the shipped read/calculate/overwrite operation.
- `scripts/unit_resources.lua:81-104` — the ordinary physiology loop invokes recovery for living units.
- `src/Engine/Scripting/Lua/API/Units/Stats.hs:343-362` — `unit.setStat` atomically inserts the supplied absolute value, without checking the value originally read.
- `src/Combat/Resolution/Constants.hs:178-180` — a quick strike costs 0.25 stance; a heavy strike costs 0.5.
- `src/Combat/Resolution/Wear.hs:164-174,183-195` — combat spends stance against the current instance as part of the strike.
- `src/Combat/Resolution/Admission.hs:182-206` — the strike admission and application share one atomic modification, but a subsequent stale Lua write can overwrite the result.
- `src/Combat/Thread.hs` and `src/Engine/Scripting/Lua/Thread.hs` — those writers execute on separate workers.

**Reproduction:**

Run `lua docs/audit_evidence/2026-09-05/stance_interleaving.lua /path/to/reviewed/synarchy` from the docs worktree. The script loads the actual target checkout's Lua module and stubs unrelated imports. It injects the quick-strike debit into the dexterity read, after recovery has already read stance.

Observed: initial stance 0.600; concurrent charge leaves 0.350; recovery writes 0.659. With dexterity and agility both 1 and `dt = 0.1`, recovery is 0.059, so a result preserving both operations is 0.409. Neither serial ordering of those operations yields 0.659. Assertions passed on the reviewed revision.

**Handoff context:**

- **Current behavior:** Resource recovery can restore stance already spent by combat, affecting attack readiness and combat balance.
- **Expected direction:** Recovery applies its bounded change against current authoritative state in one mutation operation. Audit the analogous stamina resource update, but do not claim it was independently reproduced here.
- **Scope and constraints:** Preserve recovery rate and cap, combat admission semantics, and initialization/debug setter behavior. Do not change AI/player transfer policies or require a general engine rewrite. Fixing only combat's commit cannot close a later stale recovery write.
- **Verification:** Retain a controlled interleaving regression that proves the debit and recovery both survive. A fix should produce 0.409 in this scenario, retain the ordinary no-interleaving recovery result, and respect the maximum of 1.
- **Deduplication:** Searched primary and docs-worktree findings/designs and the live tracker for stance and shared-stat overwrite concerns. Closed #2328 covers atomic strike admission/cost, not this subsequent Lua overwrite. #1890 is related mutation-authority infrastructure. No matching open issue was identified; no issue was filed.
- **Remaining uncertainty:** This is a deterministic reproduction of the actual Lua calculation with a simulated engine-boundary interleaving. It does not exercise the full combat resolver or establish occurrence frequency under live thread scheduling. Other resource fields need separate verification.

### CH-2. Lua stamina updates can erase combat costs and bypass exhaustion checks

**Severity:** High — the reproduced interleaving affects resource conservation and the existing exhaustion-death rule.

Verified 2026-09-05 against `13fa8126ad8d0126abd04eeaf77705aa1809a087`, as the next bounded candidate after stance recovery. This is a separate caller and needs its own dynamic-capacity and consequence handling; it is explicitly excluded from the stance design.

`tickResource` snapshots stamina, computes a resource delta, clamps a proposed absolute value, and writes it with `unit.setStat`. Combat can spend stamina between the snapshot and write. Both the write and the following exhaustion checks then use the old calculation. An attack cost can disappear, including one that reduced live stamina to zero.

**Evidence:**

- `scripts/unit_resource_tick.lua:37-46` — maximum/current lookup and unconditional first-observation initialization.
- `scripts/unit_resource_tick.lua:146-155` — absolute next-value calculation, clamping, and overwrite.
- `scripts/unit_resource_tick.lua:179-205` — death and collapse use the earlier `current` and calculated `next`, not values returned from a shared-state commit.
- `scripts/unit_resource_config.lua:18-62` — shipped acolyte stamina uses an endurance-derived maximum, idle recovery factor 0.5, collapse threshold 0.1, and `kill_on_zero = true`.
- `src/Combat/Resolution/Wear.hs:142-174` — heavy strikes spend 0.25 of effective maximum stamina, flooring at zero; exhaustion consequences are delegated to the resource tick.
- `src/Combat/Resolution/Common.hs:88` and `scripts/unit_stats.lua:85` — current maximum semantics already have matching Haskell/Lua implementations, including effective modifiers and explicit maximum precedence on the Haskell side.
- `src/Engine/Scripting/Lua/API/Units/Stats.hs:343` — the setter atomically replaces one stat with the supplied absolute value.

**Reproduction:**

Run `lua docs/audit_evidence/2026-09-05/stamina_interleaving.lua /path/to/reviewed/synarchy` from the docs worktree. It loads the real resource tick and acolyte resource configuration. Engine API stubs hold a maximum of 10, endurance of 1, no organ failure, and no caffeine. During the endurance read after the initial stamina lookup, inject a 2.5 debit, matching the current heavy-strike cost for that maximum.

| Scenario | Observed result |
| --- | --- |
| Start at 6; debit 2.5 during calculation; recover 0.05 | Stamina becomes 6.05 instead of 3.55 |
| Start at 2; debit to zero during calculation; recover 0.05 | Stamina becomes 2.05; no kill is requested |
| Control: start at zero, no interleaved debit | Recovery writes 0.05, but the existing pre-recovery-zero rule requests a kill |

All assertions passed. The zero control distinguishes the failure from an intentional rule allowing ordinary recovery to rescue a resource tick that observes zero.

**Handoff context:**

- **Current behavior:** Physiology can refund a committed attack and miss exhaustion consequences in the same update.
- **Expected direction:** Commit a signed stamina adjustment against current stored stamina; enforce its current effective maximum; return the authoritative before/after values and maximum for the existing threshold checks. First-observation initialization must only fill a still-absent pool at commit, never refill a pool combat created in the meantime.
- **Scope and constraints:** One stamina-specific repair, including initialization, ordinary drain/recovery, zero-delta ticks, and threshold checks. Preserve existing rates, kill-before-collapse ordering, maximum/modifier semantics, and the cross-resource revive gate. No broad resource migration, new simulation clock, page-policy change, or save schema change is needed merely to repair this path. Decide threshold inputs from the committed result; changing only the final setter is insufficient.
- **Verification:** Exercise the real Lua caller and engine mutation boundary with controlled production spending. Pin both serial orders, dynamic maxima, initialization races, zero-delta/zero-stamina behavior, and death/collapse checks. A cost arriving after the resource operation's commit may be handled on the next ordinary tick, as today; this proposal does not promise a latched event for every transient zero between ticks.
- **Deduplication:** Searched both current findings/designs and tracker results for stamina. Closed #1735 owns effective combat maximum resolution and #2328 owns strike admission/commit; neither owns this later physiology overwrite. No matching open issue identified and no tracker issue created. `stance_recovery_design.md` intentionally excludes stamina; do not file this as completion of its SR-1 slice.
- **Remaining uncertainty:** The actual Lua function/configuration were executed, but combat spending was simulated at an engine API boundary. This is not a live-thread frequency measurement or a full resolver reproduction. The authoritative engine operation and its exact API contract remain to be designed; the issue must account for thresholds and initialization rather than assuming the stance primitive can be reused unchanged.

## Lua scheduling fairness

### [#2415] CH-3. Timed Lua updates can starve behind sustained message traffic

**Severity:** Medium–High, conditional on sustained queued work. No ordinary-play occurrence rate was measured.

Source verified on 2026-09-05 at `13fa8126ad8d0126abd04eeaf77705aa1809a087`. This is a bounded scheduling defect within the audit's workload-dependent simulation concern. It is separate from deciding the future fixed-game-tick architecture.

The production Lua loop calls `runDueScripts` only when its blocking engine-message read returns `Nothing` (timeout). If a message arrives, it dispatches and drains messages, then returns without running due scripts. Before reaching that wait, it also drains engine messages and debug commands recursively until empty. There is no scheduling boundary at which a due timed script must receive service while ordinary queued traffic remains continuously available.

**Evidence:**

- `src/Engine/Scripting/Lua/Thread.hs:395-452` — initial engine-message drain, save-owner recheck, debug drain, and timeout read; only the `Nothing` branch runs `runDueScripts`.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:46-58` — `processLuaMsgs` repeatedly consumes until empty, including new arrivals during dispatch.
- `src/Engine/Scripting/Lua/Thread/Console.hs:46-56` — `processDebugCommands` also drains until empty.
- `src/Engine/Scripting/Lua/TickPolicy.hs:163,215` — due status is known independently of message availability, but making the timeout small does not force the loop to take its timeout branch.
- `scripts/init_loader.lua` registers both AI and physiology as timed scripts; `scripts/unit_ai.lua:461` and `scripts/unit_resources.lua:77` perform gameplay work in their `update` callbacks.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:325-395` — `LuaSaveLoaded` reconciles the replacement session and cancels stale console commands before ordinary work may resume. A fairness change must preserve that boundary.

**Admitted failure schedules (source trace, not executed reproductions):**

1. A due script exists. Each wait returns an engine message before timing out; each subsequent drain finishes. Every loop takes the message branch, so the due script never runs even though the worker repeatedly returns to its outer loop.
2. A message handler finishes but more engine messages are always queued. The initial recursive drain never returns to the outer scheduler, so changing only the post-wakeup branch would not prevent starvation.
3. The debug-command drain likewise never yields while commands remain available. A complete ordinary-work fairness repair must cover that entry path too.

**Handoff context:**

- **Current behavior:** AI, physiology, and other timed callbacks can be deferred indefinitely by sustained queued work even when individual handlers terminate. Movement on another worker can continue.
- **Expected direction:** Give due timers service independently of an idle queue. Process finite batches of ordinary engine messages and console commands, recheck timer deadlines and lifecycle gates between batches, and retain prompt message wakeups when timers are not due. Preserve FIFO within each queue and leave unprocessed work queued rather than dropping it.
- **Scope and constraints:** One coordinated change to the Lua owner loop and its ordinary drain entry points, with deterministic scheduling tests. Preserve pause/event-only semantics, callback rescheduling (#2205), save-owner parking (#2221), load reconciliation before scripts/console resume (#763), command cancellation, and stop behavior. Do not run timers merely because a batch ended if the load transaction still requires reconciliation. The narrow guarantee assumes individual callbacks/commands terminate; callback preemption, arbitrary long-running debug commands, queue admission/memory bounds, and scheduling every other engine worker are outside this repair.
- **Verification:** Exercise the real scheduling path through a controlled clock and message source: repeated message wakeups with due timers, a continually replenished engine queue, a replenished debug queue, FIFO/no-loss, no timer execution during owner parking or pending load reconciliation, and normal idle behavior. Observe timer progress within the stated batch bound without waiting for a queue to become empty. Timing values and the batch policy should be chosen and documented in the issue; this report does not invent a measured latency target.
- **Deduplication and disposition:** EA-2 already noted generic queue-starvation risk. Its closed issue #1910 added telemetry and explicitly retained all drain termination conditions; it did not repair this Lua scheduling path. Closed #1695, #2204, and #2205 cover interval validity, clock sampling, and rescheduling respectively. Refreshed tracker searches found no matching Lua timer-fairness issue. The owner approved the draft, filed as [#2415](https://github.com/coghex/synarchy/issues/2415) on 2026-09-05. This marks the finding handed off, not fixed.
- **Remaining uncertainty:** Established by direct production control-flow inspection, not a running flood experiment. The amount of ordinary input/engine traffic needed to trigger noticeable delays remains unmeasured. This fix provides scheduling opportunities; it does not solve the configured-interval `dt` disagreement or guarantee constant wall-clock simulation speed under overload.

## Calendar accumulation

### CH-4. The world calendar discards fractional minutes and cannot advance at default speed

**Severity:** High — autonomous day/night and calendar progression are blocked at the default speed.

Verified on 2026-09-05 at `2922bb476be795c9fd3d33eb65962b7eccca39ed` by source tracing and execution of the real `advanceWorldClock` function.

`WorldTime` stores only integer hours and minutes. Every update adds `timeScale * dt`, floors to minutes, and immediately stores that integer result; no fractional remainder survives. The normal world worker caps `dt` at 0.25 seconds. With the default scale of 1 game-minute per elapsed second, even the largest admitted update is rounded away. This is permanent loss on every update, not merely coarse display precision.

**Evidence:**

- `src/World/Time/Types.hs:36-41,80-104` — integer representation and floor after adding one update's increment.
- `src/World/Thread/Time.hs:35-54` — caller reads the integer clock, advances it, then stores the result without another accumulator.
- `src/World/State/Types.hs:490` — default scale 1.
- `src/World/Thread.hs:75-101` and `src/Engine/Core/Clock.hs:55-82` — monotonic elapsed input is capped at 0.25 seconds.
- `src/World/Thread/Time.hs:63-77` — flora regrowth, tracked-item temperatures and batteries consume fractional `dt * scale * 60` despite the frozen calendar.
- `src/World/Thread/Power.hs:50-88` — energy integration reads solar angle from the same frozen time of day.
- `scripts/circadian.lua:63-72` and `scripts/unit_ai_sleep.lua:182-205` — world sun angle contributes to sleep urge/utility.

**Reproduction:**

Run the [retained GHCi script](audit_evidence/2026-09-05/gameplay_timing.ghci) through `cabal repl lib:synarchy --repl-options=-v0` in the reviewed checkout. Starting day 1 at 10:00, 240 calls of 0.25 seconds at scale 1 still return day 1 at 10:00. The pure function given the same total 60 seconds in one call returns 11:00; that large call is a comparison, not a value the normal worker admits. At scale 60, splitting the same elapsed minute into 600 × 0.1 seconds yields day 3 at 22:00, while 480 × 0.125 seconds yields day 3 at 18:00. [Recorded outputs](audit_evidence/2026-09-05/gameplay_timing_results.md).

**Handoff context:**

- **Current behavior:** The calendar freezes at default speed; higher rates are quantized according to worker update size. Calendar-derived solar/circadian/seasonal behavior diverges from world-process countdowns and energy integration.
- **Expected direction:** Retain fractional calendar progress across updates and round only for queries/presentation requiring whole minutes. Equal admitted elapsed time should advance the same calendar duration within a documented numerical tolerance, regardless of partition. Preserve pause, time-scale validation, rollover and per-page ownership.
- **Scope and constraints:** A bounded calendar repair, separate from synchronizing all gameplay clocks. Decide the authoritative precise representation and its save behavior explicitly. Current saves contain only integer time; a new persisted remainder needs the applicable component version/migration and inventory changes. Initialize old-save fractions honestly to zero. Manual set-time, load, new page and teardown must not inherit stale fractional progress.
- **Verification:** Repeated sub-minute steps at default scale; partition comparisons; minute/midnight/month/year rollover; paused ticks; changed scale; manual set/load/reset boundaries; corresponding solar-angle progression. Existing monotonic tests deliberately use whole-minute increments and can pass with this bug (`test-headless/Test/Headless/Core/MonotonicClock.hs:137-143,349-372`).
- **Deduplication:** Refreshed open tracker and clock/fraction searches. Closed #2204 handles monotonic input/capping, #2280 handles scale validity/totality, and #2339 canonicalizes date ingress. None retains fractional progress. No matching open issue identified; no issue filed.
- **Remaining uncertainty:** Production arithmetic executed directly, not a running world-worker experiment. Runtime occurrence frequency does not affect the default-scale proof. Seasonal and circadian consequences are source-traced, not separately exercised end to end.

## Movement time consumption

### CH-5. Movement discards remaining elapsed time when reaching a path waypoint

**Severity:** Medium — movement rate depends on update partition when a path waypoint is reached; larger updates lose more available motion.

Verified on 2026-09-05 at `2922bb476be795c9fd3d33eb65962b7eccca39ed` through the real pure `tickUnit` entry point. This is independent of the 0.25-second cap: every reproduction step is at or below it.

When the next waypoint falls within the current movement budget, `stepTowardSubGoal` snaps to it and calls `arriveAtSubGoal`. That function pops the waypoint and returns, leaving further movement to another tick. The unused duration is never consumed on the next segment. The arrival epsilon also allows an early snap on short steps, producing another partition-sensitive effect around the boundary.

**Evidence:**

- `src/Unit/Thread/Movement/PathAdvance.hs:181-218` — displacement derives from speed and elapsed time.
- `src/Unit/Thread/Movement/PathAdvance.hs:259-267` — reached-waypoint branch returns the arrival operation instead of consuming a remaining budget.
- `src/Unit/Thread/Movement/PathAdvance.hs:301-333` — snap/pop, with continuation deferred until the next tick.
- `src/Unit/Thread/Movement/PathAdvance.hs:269-293` — separate protected-step ceiling; deliberately not involved in the reproduction and must not be removed casually.

**Reproduction:**

Run [movement_timing.ghci](audit_evidence/2026-09-05/movement_timing.ghci) through `cabal repl lib:synarchy --repl-options=-v0`. Start at `(0.4, 0.5)`, speed 1 tile/second, with path waypoints `(0.5, 0.5)`, `(1.5, 0.5)`, `(2.5, 0.5)` and target `(2.5, 0.5)`. One elapsed second split into 4 × 0.25 yields final x **1.25** (0.85 tiles travelled). The same second as 10 × 0.1 yields x **1.4000002** (about 1 tile). With 20 × 0.05, arrival tolerance yields x **1.4499997** (about 1.05 tiles). The path is supplied directly, with no terrain and `FallPermitted`; this isolates the production movement function and does not assert that a particular generated route appears in ordinary play. [Recorded outputs](audit_evidence/2026-09-05/gameplay_timing_results.md).

**Handoff context:**

- **Current behavior:** Reaching a waypoint can consume a whole tick regardless of how little of its elapsed budget was needed. Repeated boundaries can slow movement; arrival tolerance can also advance short-step movement early.
- **Expected direction:** Account explicitly for time spent reaching a waypoint and carry usable remaining time across subsequent path segments, or use a bounded internal stepping policy with an explicit distance/tolerance contract. Test equal admitted durations against the real mover.
- **Scope and constraints:** Preserve final arrival, material/slope speed, fall/climb transitions, pose restrictions and the protected-movement hazard checks. Bound traversal and validate every crossed terrain boundary. Do not solve it by removing `maxProtectedStep` or allowing unchecked multi-tile jumps. The appropriate numerical/arrival tolerance should be specified before demanding bit-identical partition results.
- **Verification:** Isolate waypoint residual time and arrival epsilon; then cover multiple adjacent waypoints, final target, changed terrain speed, blocked/hazardous crossing and transition entry. Repeat equal-duration schedules below the existing elapsed cap.
- **Deduplication:** Tracker waypoint search found related closed #1217 (ambient hazard protection) and expedition scenarios, but no matching open waypoint-budget repair. No issue filed.
- **Remaining uncertainty:** The pure production mover was executed with controlled state, not a generated-world/live-worker workload. Frequency and cumulative impact on shipped pathing have not been measured. This finding does not claim all movement paths lose the same amount or that ordinary straight-line motion always has this defect.
