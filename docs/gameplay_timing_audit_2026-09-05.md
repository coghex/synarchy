# Gameplay timing: where the game disagrees with itself

Draft for discussion, 2026-09-05. Reviewed source: `2922bb476be795c9fd3d33eb65962b7eccca39ed`. No implementation or new tracker issue was created. The existing Lua scheduling issue is [#2415](https://github.com/coghex/synarchy/issues/2415).

## The main conclusion

The game does not have one agreed measure of how much gameplay has happened. It has several individually plausible rules: elapsed time, elapsed time with discarded gaps, an allowance per Lua callback, a combat-loop counter, fluid iterations, and a separate calendar. Those rules disagree as soon as their workers experience different workloads.

This can change game balance, not just smoothness. A character can cover distance while spending less stamina, a wound can age toward an infection deadline while receiving less bleeding/healing integration, and a powered job can consume energy on a different schedule from its work progress. The direction of the advantage depends on which worker is delayed.

**There are also two concrete bugs:** fractional minutes disappear on every world update, and movement discards its remaining time when it reaches a path waypoint. At the default speed, the calendar cannot advance at all through the normal bounded update path. These are recorded as CH-4 and CH-5 in [the canonical bug report](simulation_consistency_findings.md). The calendar bug is the first repair priority.

The owner's requirement is compatible with a fixed-step simulation. Exact replay and identical ordering between independent units are unnecessary. What matters is that the game accounts for the same accepted duration across coupled systems, and that overload slows that duration coherently.

## What was checked

This pass traced the production world, unit, combat, fluid, and Lua worker loops; their clocks and pause gates; the gameplay consumers below them; the relevant save representations; and existing timing tests. Separate bounded Sol and Terra reviews covered Lua and engine gameplay paths. Their findings were checked against the source and combined here.

Verification includes executions of the actual Haskell calendar, elapsed-time and movement functions, the actual Lua stamina calculation with shipped configuration, and the actual AI work-interval policy. These use controlled inputs. They are **not** measurements of how often ordinary play stalls, nor an end-to-end overloaded engine experiment.

## The current time map

Here, *session seconds* means `engine.gameTime()`. *Calendar seconds* means the world clock's minutes converted to seconds. They currently have different rates and different owners.

| System | What advances it today | What workload changes |
| --- | --- | --- |
| Movement and session clock | Unit worker's measured monotonic elapsed seconds; each sample capped at 0.25 s; paused time excluded | Long unit-worker stalls discard time. Movement also has per-step limits described below. |
| Unit action/animation deadlines, modifiers, AI decisions and many cooldowns | Absolute session seconds, read when the relevant worker gets service | A late consumer observes an aged deadline; it does not necessarily perform the missed intervening work. |
| Physiology and stance recovery | Configured Lua interval, normally 0.1 s, once per completed update | Missed updates lose their entire allowance, even if session time advanced. |
| Building-instance construction | Configured Lua interval, normally 0.2 s, multiplied by current worker contribution | Also loses progress when callbacks are delayed or omitted. This is separate from designation construction handled by unit AI. |
| Craft, designation construction, harvest work | Differences between session timestamps; interrupted or greater-than-5-second gaps credit zero | Can catch up short gaps unlike physiology, but deliberately refuses longer gaps. |
| Dig, chop, till and plant work | Session-time differences capped at 2 s per action update | A different amount of a long gap is credited than for craft/construct/harvest. |
| Wounds: bleed, clot, heal, infection and immunity integration | Fixed 0.1 s after every six completed combat-worker iterations | Combat processing time reduces the amount integrated per session second. Wound age also reads session time. |
| Fluid movement | One discrete active-fluid iteration per worker pass, followed by a 100 ms sleep | Command and simulation work reduce iterations per session second. There is no elapsed-time input. |
| World date/time | World worker's independently capped elapsed seconds × page time scale, rounded down to whole minutes every call | Loses fractions even during light load. Different step sizes give different calendar progress. |
| Flora regrowth, tracked-item temperature, battery charge/discharge | World worker's elapsed seconds × page time scale × 60 | These consume fractional time even when the calendar does not; world-worker stalls also discard elapsed time independently. |
| Circadian urge, calendar-derived plant growth/fruiting and solar angle | Queries of world time/date, often localized to a unit or page | Follow the stalled/truncated calendar, not the elapsed world-process counters. |

Source entry points: `Unit/Thread.hs:109-132`; `Engine/Scripting/Lua/Thread.hs:494-504`; `scripts/init_loader.lua:104-125`; `scripts/unit_resources.lua:77-142`; `scripts/building_spawn.lua:639-674`; `scripts/unit_ai_stall.lua:290-316`; `Combat/Thread.hs:107-140`; `Sim/Thread.hs:79-140`; `World/Thread/Time.hs:21-77`. Haskell paths in this report are relative to `src/` unless written otherwise.

### Physiology has a broad footprint

The 0.1-second resource update feeds circulation, temperature, salts, brain state, thoughts, mental-state drift, failure meters, resource drain/recovery, digestion, and starvation as well as stance. Some calculations are instantaneous derived values, but many integrate the supplied `dt`. Changing only stamina would leave the timing disagreement throughout this update tree. The separate approved resource-mutation proposals address stale writes; they do not repair time accounting.

Mental episodes and thoughts also use session-time deadlines (`scripts/mental_state.lua:30-43,214-233`; `scripts/thoughts.lua:136-152`). A system can therefore mix nominal callback integration and elapsed deadlines internally.

### Work progress already has several policies

The recent bounded-work repair (#2332), and commanded-order stall accounting (#1291), correctly prevent a job from claiming time while it was suspended. Their guards should be preserved until replaced with an explicit definition of eligible simulated work.

A Lua scheduling gap is currently indistinguishable from some eligibility interruptions unless another path announces the interruption. A three-second gap can credit craft three seconds, dig two seconds, and one physiology callback only 0.1 seconds. A six-second gap credits craft zero. Simply replacing all these rules with `now - last` would reintroduce work across suspension and page changes.

Evidence: `scripts/unit_ai_stall.lua:100,198-210,290-316`; `unit_ai_craft.lua:386`; `unit_ai_construct.lua:440`; `unit_ai_harvest.lua:268`; `unit_ai_dig.lua:300-315`; `unit_ai_chop.lua:287-298`; `unit_ai_farm.lua:189-198,390-398` (the latter paths are under `scripts/`).

## The concrete failures

### 1. The calendar throws away time

`WorldTime` stores integer hours and minutes. `advanceWorldClock` adds this update's fractional minutes, floors the result, and returns no remainder (`World/Time/Types.hs:36-41,80-104`). `tickWorldTime` immediately stores that result. There is no other accumulator in this path.

The default scale is 1 game-minute per elapsed second (`World/State/Types.hs:490`), and the maximum admitted world update is 0.25 seconds. Every default update therefore adds at most 0.25 minutes and then discards it. This holds even with the largest admitted step; a faster worker cannot fix it.

Executed results, starting at day 1, 10:00:

| Production-function input | Calendar result |
| --- | --- |
| 240 updates × 0.25 s, scale 1: sixty elapsed seconds | Day 1, 10:00 — no advance |
| One 60 s call, scale 1 | Day 1, 11:00 — the pure function can represent the total, but the normal worker caps such a sample |
| 600 updates × 0.1 s, scale 60 | Day 3, 22:00 |
| 480 updates × 0.125 s, scale 60 | Day 3, 18:00 — four game-hours less for the same elapsed minute |

Meanwhile flora regrowth, item cooling and batteries consume `dt × scale × 60` from the same world pass. Solar power consumes elapsed energy time using a sun angle that can remain fixed. Circadian sleep pressure can accumulate while the day/night cue never cycles. Calendar-derived flora age and seasonal stages remain at the stored date.

The existing monotonic-clock tests deliberately choose integer-minute advances (`test-headless/Test/Headless/Core/MonotonicClock.hs:137-143,349-372`). They verify sample sanitization and rollover but cannot expose fractional loss. Static circadian probes set time explicitly, which also masks autonomous-clock failure.

### 2. Lua progress depends on how many callbacks finish

`runDueScripts` passes `scriptTickRate`, and `advanceTick` drops missed executions after sufficient lateness. Fixing #2415 gives callbacks service under queue traffic; it does not restore the time those callbacks missed.

The controlled reproduction executes the shipped acolyte stamina calculation with maximum 10, endurance 1, idle recovery and starting stamina 6. Over a stipulated one-second observation window:

- Ten nominal 0.1-second callbacks yield **6.50**.
- Two nominal 0.1-second callbacks yield **6.10**.

No concurrent stat write was injected. This is a separate mechanism from the earlier stale-write bug. If the unit worker continues normally through that window, movement and session deadlines can advance a second while physiology integrates only 0.2 seconds. The example uses recovery for clarity; movement drain and other integrated needs lose time by the same mechanism.

### 3. Combat workload changes wound progression

The combat loop drains commands, advances a counter, occasionally runs wounds with a fixed 0.1-second argument, and then sleeps. It does not subtract command execution time from its sleep or derive wound duration from accepted session time (`Combat/Thread.hs:107-157`).

For example, six passes each taking 50 ms of work plus roughly 16.7 ms of sleep occupy about 0.4 seconds, but apply only 0.1 seconds of wound integration. This is an illustrative schedule derived from the loop, not a measured workload.

The wound subsystem also reads the unit-owned game clock for age/timestamps (`Combat/Wounds/Tick.hs:77-125`). Infection age gates can advance while healing, blood loss and immunity receive less duration (`:181-185,223-243`). A fixed wound interval is reasonable; tying its execution count to completed combat-drain iterations is the disagreement.

### 4. Fluids have an iteration budget without a gameplay-time definition

The fluid algorithm is already discrete and volume-conserving. That is a useful foundation. The problem is its owner: one pass of work plus a full 100 ms sleep, with no accounting for elapsed or accepted game time. A 50 ms work cost produces roughly 6.7 passes per second instead of 10. More active chunks can therefore make water move more slowly relative to units.

`simulateActiveTick` also counts 200 unchanged iterations before deactivation (`Sim/Fluid/Active.hs:23-49`). This is an algorithmic settling threshold; it need not become a gameplay-duration promise. The design must distinguish solver work, initialization/fast-settle work, and the advancement of gameplay-visible water. It should not multiply a discrete fluid transfer by an arbitrary large `dt`.

### 5. A shared monotonic source is not yet shared simulation time

The monotonic-clock repair (#2204) correctly removed wall-clock jumps and capped host interruptions. Preserve that protection. However, each owner independently decides how much of an interval to discard.

The actual sanitiser admits four samples of 0.25 seconds as a total of 1 second, but one sample of 1 second as only 0.25 seconds. If the unit worker gets the first schedule and the world worker the second, they permanently account for different durations. Ordinary command backlogs can cause the same shape as a host interruption. World and unit command drains are recursive and precede gameplay work; elapsed-time sampling alone cannot guarantee progress through an indefinitely replenished queue.

This does not mean the game should simulate an hour of host sleep. It means the policy for accepting or dropping time must be coordinated at the simulation level instead of independently changing the balance of each subsystem.

### 6. Movement has assumptions about update size even below the clock cap

Movement generally integrates elapsed time and shares its clock with action/animation deadlines. That is better aligned than the other domains, but does not guarantee equal distance for equal elapsed time.

`stepTowardSubGoal` snaps to a reached waypoint; `arriveAtSubGoal` pops it and returns without applying the remaining time to the next segment (`Unit/Thread/Movement/PathAdvance.hs:259-267,301-333`). The arrival epsilon can also snap a short distance before the step budget reaches the waypoint.

A controlled execution of the real `tickUnit` starts at `(0.4, 0.5)`, speed 1 tile/second, with tile-center waypoints `(0.5, 0.5)`, `(1.5, 0.5)`, `(2.5, 0.5)`:

| Partition of one elapsed second | Final x | Distance travelled |
| --- | --- | --- |
| 4 × 0.25 s | 1.25 | 0.85 tiles |
| 10 × 0.1 s | 1.4000002 | About 1.00 tile |
| 20 × 0.05 s | 1.4499997 | About 1.05 tiles |

All samples are within the clock cap. The fixture supplies a path directly, uses `FallPermitted`, and has no terrain; it isolates the movement function rather than proving a generated route or a live-unit workload. The 0.25-second case discards the 0.15 seconds left after the first waypoint. The smaller-step overshoot also exposes the arrival tolerance's effect.

Protected movement has a separate deliberate 0.9-tile-per-step ceiling to prevent skipping dangerous terrain (`:269-293`). That safety rule must survive a repair. Consuming movement budget across waypoints requires bounded traversal/substeps with each terrain crossing checked; simply removing the ceiling or jumping straight to the destination would undo the hazard contract. Fixed simulation steps reduce workload-dependent partition changes, but the movement algorithm's distance/tolerance behavior still deserves explicit tests.

## What should stay separate

**Calendar speed and gameplay speed currently mean different things.** `world.setTimeScale` is explicitly a calendar-rate control. Movement, combat, session deadlines and much of physiology ignore it, while world processes use it. Metabolism and hydration are tuned to the default conversion: one game-day is treated as 1,440 ordinary seconds (`scripts/unit_stats.lua:33-37`; `unit_resource_config.lua:70-76`). Increasing calendar scale changes those relationships.

That is an existing semantic split, not evidence that the API violates its present contract. Before exposing general fast-forward, distinguish the day-length/calendar conversion from a simulation-speed multiplier. A faster sun and a faster whole game are different controls. This report recommends one gameplay timebase with an explicit calendar conversion; the owner's desired fast-forward behavior remains a design decision.

**Wall time still has legitimate uses.** Keep it for autosave intervals, human-facing timestamps, popup coalescing, preview playback and key repeat, transport timeouts, logging and worker pacing. Render/camera responsiveness should not wait for an overloaded colony tick. In-world animation and action completion already use session time (`Unit/Render.hs:65-115,172-175`; `Unit/Thread/Movement/Timers.hs:30-76`); that coupling should remain. Blood presentation is transient by design, and its trail/pool timing already uses session-time/displacement accounting rather than the calendar (`Blood/Trail.hs:98-141`; `Blood/Pool.hs:182-190`).

**Background simulation is a separate eligibility decision.** World time/power/temperature currently iterate visible pages; movement resolves each unit's own page, wounds inspect all loaded-page climates, and fluid state has explicit per-page activation. The owner wants the home colony to continue during expeditions. A new clock must support that requirement, but replacing the clock alone cannot repair these different membership rules. Residency work (#1997) remains a dependency to coordinate, not a substitute for simulation membership.

## The shape of the larger repair

This is epic-sized. A clock helper or a global search-and-replace of `dt` is insufficient. The useful contract is:

> An eligible system advances once for each duration the simulation accepts. Systems may run at different cadences, but they cannot silently claim or discard different amounts of that duration.

A practical direction is a simulation coordinator that issues fixed steps, tracks completed work, and owns pause, speed and overload accounting. Movement may run each step while physiology, wounds and fluids run every few steps. Their credited duration comes from those steps. Rendering and asynchronous preparation remain independently paced.

The coordinator need not impose identical ordering between independent units or make the game replay-deterministic. It does need clear completion boundaries: merely publishing a tick number while workers fall arbitrarily far behind would reproduce today's problem. Coupled work such as movement/exertion, combat/wounds, and crafting/power needs a bounded phase relationship. Existing atomic mutation and command-admission policies still matter.

Under sustained overload, cap the catch-up work and slow accepted simulation time coherently. Do not ask one slow Lua callback to integrate a huge duration in one call: threshold crossings, AI decisions, pathing, resource depletion and fluid transfers are not generally interchangeable with many small steps. Nor should the scheduler blindly replay entire callbacks that mix UI, commands and gameplay effects. Separate those responsibilities where the timing migration reaches them.

The base step size should be chosen after measuring representative work. This audit establishes the correctness problem; it does not justify a particular Hz target or a new performance guarantee.

## Save/load and lifecycle are part of the design

The existing save barrier, owner parking, and post-load Lua reconciliation must remain authoritative. A clock migration must define which logical step a snapshot represents and prevent work from a discarded session from applying after load. The new scheduler must also rebase its wall-clock pacing after pause/load so an old sample cannot become catch-up debt.

Current durable time-bearing state includes:

- Session `gameTimeRef`/`sdGameTime`, installed by `World/Load/Publish.hs:120` and reset at the documented new-session boundary (`Engine/Core/SessionEpoch.hs`).
- World hours/minutes/date and flora regrowth countdowns; there is currently no persisted fractional calendar remainder.
- Unit modifier expiry, wound timestamps, movement/action deadlines and other unit simulation state.
- Lua AI timestamps/eligible stall counters, building spawn's `lastSpawnedAt`, and mental/thought deadlines stored in unit stats.

Authority: `docs/persistence_state_inventory.md` entries for `gameTimeRef`, `wsTimeRef`, `wsDateRef`, `wsFloraHarvestsRef`, `utsSimStates`, and Lua save modules. `scripts/unit_ai_save.lua:50-90,183-215,347-352` deliberately strips some timing scratch and migrates older stall state; `scripts/building_spawn.lua:482-499` persists spawn timing.

Retaining session seconds as the public/persisted unit can reduce migration scope even if an internal integer step count drives them. Reinterpreting stored seconds as ticks, changing the epoch, or adding persistent calendar precision requires explicit component compatibility work. Older saves have no fractional minutes to recover; a migration can initialize that remainder to zero, not reconstruct time already lost. Do not reset or rebase every deadline indiscriminately.

## Verification needed before calling the redesign complete

The missing proof is cross-system consistency under controlled scheduling, not another collection of sleeps until an outcome appears.

| Scenario | Required evidence |
| --- | --- |
| Calendar partitioning | Many small steps and fewer larger steps representing the same accepted duration agree, including fractional minutes, midnight and save/load. |
| Path movement across waypoints | Equal accepted duration preserves intended distance within an explicit arrival tolerance, while checking every hazardous crossing and keeping traversal bounded. |
| Moving survivor under delayed Lua service | Distance and physiological cost remain tied to accepted simulation duration; no free travel from missed resource callbacks. |
| Wounded unit under combat queue pressure | Bleeding/healing/infection age and session time account for the same accepted interval. |
| Powered work under asymmetric world/Lua load | Progress and electrical consumption retain their intended relationship. |
| Fluids under different command/active-chunk workloads | Solver advancement for the same accepted duration is stable, within the explicitly chosen numerical policy. |
| Pause, host sleep, overload, save/load and exit-to-menu | No hidden catch-up, duplicate step, lost completed step, stale-session work, or timer reset beyond the documented policy. |
| Two retained gameplay locations | The same clock contract applies regardless of the viewed page, once background membership is implemented. |

Existing useful gates include `monotonic elapsed-time contract`, `Lua tick-interval policy`, `Lua scheduler reentrancy`, `bounded work clocks`, `world.setTimeScale domain`, and `pause preserves the chosen world speed`. Wound tests exercise formulas with supplied durations; fluid tests exercise conservation and admission. These are valuable pieces, but their current oracles do not establish the cross-system guarantees above. The `#2415` scheduling tests should remain a separate fairness guarantee.

## Evidence and continuation

Retained controlled inputs: [Haskell clock reproduction](audit_evidence/2026-09-05/gameplay_timing.ghci), [Lua consumer reproduction](audit_evidence/2026-09-05/gameplay_timing.lua), and [movement reproduction](audit_evidence/2026-09-05/movement_timing.ghci). All ran against the revision above. [Recorded results](audit_evidence/2026-09-05/gameplay_timing_results.md) distinguish executions from illustrative schedules.

Deduplication checked the open tracker and related closed clock/work/waypoint issues. #2204 owns monotonic samples and the deliberate interruption cap; #2280 owns safe time-scale values; #2332 and #1291 own suspended-work/stall accounting; #1217 owns protected movement's hazard policy; #2415 owns Lua queue fairness. No matching open calendar-accumulation or waypoint-time-budget issue was identified, and none of those related fixes establishes shared gameplay advancement. No new issue was filed in this pass.

Next: repair calendar accumulation as a bounded bug, retain the waypoint-budget finding for its own movement repair, and turn the shared-step contract into a design document with migration slices. First settle simulation-speed versus calendar-rate semantics and how participating owners acknowledge a step; then choose cadence and overload parameters with a representative workload. Preserve the earlier stance/stamina drafts and chunk-residency discussion as separate linked work.

This pass covers the identified timing owners and major gameplay consumer families. It does not measure production stall frequency, benchmark every workload, prove numerical stability of every integrator, audit all AI job lifecycles, or settle the chunk-residency policy. Those remain explicit follow-ups rather than implied guarantees of this report.
