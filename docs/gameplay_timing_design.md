# Shared gameplay timing design

Synarchy should advance movement, survival, combat, work and the world by an agreed amount of gameplay time. A busy worker may make the game run more slowly, but must not give a unit cheaper travel, slower bleeding or a different amount of work per simulated second.

Design state: `ready for issue processing`

**Ready for issue processing following the owner's 2026-09-05 readiness/publication instruction.** D-1 through D-11 record established direction. Whole-simulation fast-forward, 5–50 controlled units, a tentative 10 × 10-chunk base, a four-core/8-GB minimum laptop class, and responsive input/UI during even severe simulation slowdown are established. Ordinary movement prepares its required chunks before starting; rare forced movement into unknown terrain uses best-effort estimates, with the unit phased out of world interaction until reconciliation. Cooperative presentation service within long gameplay steps remains the recommended mechanism. Q-1 is resolved in direction. Q-2 through Q-4 are deliberately open engineering decisions with explicit slice gates below; readiness does not approve numerical budgets or an unspecified reconciliation algorithm.

Created 2026-09-05. Evidence comes from the [timing audit](gameplay_timing_audit_2026-09-05.md), verified at `2922bb476be795c9fd3d33eb65962b7eccca39ed`. Rechecked against `ece7dc6dd88ccb62b70bfd9716135089611e38de`: intervening changes only affect `tools/lunge_probe.py`, not the audited timing paths. The owner expanded publication to the pending documentation batch, including this design, the supplemental audit, findings and retained reproductions. No issue or label is created by publishing them.

Follow-up source check at `7242a132a7332affdc2d2590a15724a303c7830d`: subsequent changes affect construction-probe/CI wiring, not these runtime paths. Current Lua resource/AI entry points still loop over the unit roster in one callback, and `usLocalPath` still stores x/y waypoints rather than a precomputed terrain/stat timeline.

Final editorial read on 2026-09-05 recorded the clarified footprint and phased-unit isolation throughout the design and reviewed all delivery slices. Since the input-worker check at `7f135c0ed78ee6c30cae9bd4ab2242b55f21de3b`, changes through `5c33fc7d7422cdcc64f88fab065a8d4b9529fe14` affect only `test-headless/Test/Headless/Item/ContentsSignature.hs`. This is source-drift and document validation, not a new runtime performance measurement or tracker-readiness review.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]` deliberately not tracked separately · `[deferred]` blocked on a concrete precondition.

## Processing status

- [ ] EPIC. Advance coupled gameplay through one coordinated simulation clock
- [ ] GT-1. Define the step protocol and pure clock accounting
- [ ] GT-2. Add lifecycle and transaction coordination at completed-step boundaries
- [ ] GT-3. Bound unit and combat command service for granted work
- [ ] GT-4. Separate world and fluid control work from timed advancement
- [ ] GT-5. Separate Lua gameplay callbacks from ordinary script scheduling
- [ ] GT-5A. Add resumable gameplay batches and responsive presentation service
- [ ] GT-6. Assemble the coordinated runner behind an internal test mode
- [ ] GT-7. Move unit timing and motion publication onto granted steps
- [ ] GT-8. Advance combat wounds and deadlines through step context
- [ ] GT-9. Advance physiology through credited simulation intervals
- [ ] GT-10. Separate AI work accounting from decision frequency
- [ ] GT-11. Separate building construction progress from spawn polling
- [ ] GT-12. Advance calendar and world processes from the shared clock
- [ ] GT-13. Schedule and acknowledge fluid advancement by simulation step
- [ ] GT-14. Persist clock continuity and complete save/load integration
- [ ] GT-15. Verify cross-system consistency and select the shipping cadence
- [ ] GT-16. Activate coordinated timing and retire independent gameplay clocks

The ledger describes delivery slices in dependency order, each scoped to one reviewable PR. GT-1 supplies the detailed owner/access map that later issue drafts must use. If that evidence requires a material split, return the affected scope to design before drafting that child; do not silently expand it. Linking an issue is not proof its implementation merged. Production activation requires the actual prerequisite implementations and checks, not merely terminal processing markers.

## Epic contract

- **Goal:** Equal accepted simulation duration gives coupled systems equal time credit regardless of worker workload, while controls and rendering remain responsive.
- **Done when:** The production engine uses one advancement authority; every shipped continuous gameplay owner consumes granted intervals rather than its own wall samples or loop counts; periodic systems have explicit simulation-time cadence; save/load and pause respect completed boundaries; controlled asymmetric-load scenarios preserve the intended gameplay relationships; input/UI feedback meets an independently agreed service target during simulation overload; and workload/cadence behavior is measured on the target machine class. Sustaining normal-speed simulation under every stress case is not required.
- **Users and operators:** Players managing a colony and expeditions; maintainers adding gameplay systems or investigating slowdown.
- **Arc label:** None proposed.
- **Not a performance guarantee:** An overloaded computer cannot sustain unlimited simulation at normal wall-clock speed. The contract is coherent slowdown and bounded outstanding work.
- **Tracker relationship:** [#2415](https://github.com/coghex/synarchy/issues/2415) supplies ordinary Lua queue fairness. [#1997](https://github.com/coghex/synarchy/issues/1997) concerns residency. Neither owns this clock architecture. The readiness recheck on 2026-09-05 found no matching open shared-timing epic. Adjacent [#1995](https://github.com/coghex/synarchy/issues/1995) owns Lua ABI registration contracts and [#1890](https://github.com/coghex/synarchy/issues/1890) owns capability mutation-authority checks; reuse their landed interfaces without absorbing their arcs. Repeat deduplication for each artifact during issue processing.

## Current state and evidence

| Current owner | Time input | Important consequence |
| --- | --- | --- |
| `Unit.Thread` | Measured monotonic elapsed, individually capped at 0.25 s | Writes `gameTimeRef` and advances movement together; its own stalls discard elapsed time. |
| Lua resources/AI | `update(0.1)` on a wall-scheduled callback | Resources integrate nominal duration; AI uses session deadlines and separate work-gap policies. |
| Lua building spawn/construction | `update(0.2)` | Construction time credit depends on callback count while spawn deadlines use session time. |
| `Combat.Thread` | Every six loop iterations, wounds receive 0.1 s | Combat command work slows integration relative to the unit-owned clock. |
| `Sim.Thread` | One fluid iteration, then 100 ms sleep | Solver rate depends on command and fluid work time. |
| `World.Thread` | Its own capped elapsed, multiplied by page calendar scale | Calendar and world processes can discard different time from units. Integer-minute storage discards fractions on every call. |

Source boundaries: `src/Unit/Thread.hs:109-132`; `src/Combat/Thread.hs:107-157`; `src/Sim/Thread.hs:79-140`; `src/World/Thread/Time.hs:21-77`; `src/Engine/Scripting/Lua/Thread.hs:494-504`; `scripts/init_loader.lua:104-125`. The audit contains downstream consumers, controlled reproductions and uncertainty limits.

Preserve existing protections: monotonic sampling and host-interruption handling (#2204), Lua callback reentrancy (#2205), owner parking (#2221), whole-session load replacement/reconciliation (#763), command cancellation (#2282), and eligible work/stall accounting (#1291/#2332). They solve real problems. The migration replaces their timing inputs where necessary rather than removing their guarantees.

Two bugs remain separate work: CH-4 calendar fraction loss and CH-5 waypoint movement-budget loss in [simulation_consistency_findings.md](simulation_consistency_findings.md). CH-4 needs retained fractional calendar progress instead of flooring each update; CH-5 needs unused movement duration carried across waypoint arrivals while preserving terrain safety. The stance/stamina mutation proposals are separate too: apply relative updates to authoritative stored resources, preserving clamps, initialization and exhaustion/death consequences. This epic must integrate their final contracts, not silently create competing implementations. Before drafting the dependent child, locate the corresponding draft or current tracker artifact and verify its implementation status.

## Desired experience

A sprint of ten simulated seconds has the same movement and survival accounting whether those seconds take ten or fifteen real seconds to compute. A battle cannot reduce bleeding merely by filling the combat queue. A workshop cannot keep consuming simulation-time energy while its work clock loses callbacks.

Pause freezes gameplay at a coherent boundary. Loading restores the saved time and outstanding gameplay state without replaying the real time spent on disk. A short CPU interruption does not create a movement jump or a storm of overdue AI actions. Under sustained overload, simulation seconds take longer; the camera and menus can still respond.

Looking away from the colony does not remove its *time eligibility*. The residency work must separately keep required terrain available. Neither this design nor a tick number creates missing chunk data.

## Scope

### In scope

- Authoritative simulation duration, step completion and workload accounting.
- Clock context for Haskell gameplay and Lua APIs; migration of the identified continuous and periodic consumers.
- Finite command-service boundaries and explicit inter-phase handoffs.
- Resumable shipped gameplay batches and safe input/presentation service during a long, incomplete step.
- Pause, failure, shutdown, save/load and session replacement integration.
- View-independent timing membership for already prepared gameplay participants, with a defined interface to residency/readiness.
- Compatibility of persisted time, cadence phase and existing deadlines.
- Focused observability, cross-domain tests and cadence measurement before activation.

### Out of scope

- Replay determinism, networking, lockstep multiplayer and changing RNG ownership.
- Moving all Lua gameplay into Haskell, parallel Lua states, or arbitrary callback preemption.
- Rewriting every gameplay formula, retuning difficulty by default, or redesigning the job/claim system.
- World generation, chunk cache algorithms, hibernation, and travel UI. Forced-movement prediction/reconciliation implementation belongs to a separately scoped streaming/movement design; this document records the accepted direction and required timing interface.
- Atomic rollback of a partially mutated gameplay step or recovery beyond the last valid save.
- Making every rendered pixel a transactionally consistent snapshot of the entire world.
- New art or player-facing speed controls. A future control can use the chosen speed contract without being delivered here.

## Decisions: established owner requirements

### D-1. Gameplay uses simulation time

Gameplay-affecting progression should use game ticks. Movement and other coupled systems must not acquire different rates simply because worker workloads differ. Wall time remains necessary to pace the simulation and non-gameplay services.

### D-2. Exact execution-order determinism is unnecessary

Small ordering fluctuations are acceptable. Sequential, evenly credited script execution matters; identical ordering between independent actors does not. This permits practical parallelism where ownership and dependencies allow it.

### D-3. The home colony continues during expeditions

Home and other gameplay-critical locations must continue simulating while the player acts elsewhere. Retained chunks support this; camera visibility is not a sufficient simulation-membership policy.

### D-4. Shared-resource mutations preserve committed work

The owner favors relative operations applied against authoritative engine state. Clock coordination does not replace atomic admission/update policies or authorize stale Lua overwrites.

### D-5. Design precedes tracker decomposition

The owner requested this document for discussion and later `process-design-doc` use. No implementation, epic creation, publication or readiness signoff is implied by requesting the draft.

### D-6. The owner approves the architectural baseline

After reading the draft, the owner said: "approved, i like it". Treat the coordinated-step approach, existing-owner arrangement and staged migration as the accepted direction; do not ask a later session to approve that direction again without new evidence or a material change. Keep the explicitly unresolved behavior choices and measurement gates visible. This approval supplies no missing colony-size/hardware target and does not choose between policies the draft deliberately left open. No tracker creation or documentation publication was requested in this approval turn.

### D-7. Fast-forward advances the whole simulation

The owner endorses fast-forward and proposes running without deliberate delay until the precomputed target is reached. Interpret this as executing the intervening fixed steps, not jumping the clock or scaling each owner's integration independently. Catch-up and requested fast-forward share the executor; the latter raises the target rate. The owner's stronger responsiveness priority in D-9 means this cannot be an uninterruptible run-to-target loop. Bounded catch-up/service policy is specified in P-6.

### D-8. Target a small colony on a modest laptop, with room for unfinished systems

The typical colony is **5–50 player-controlled units**. The owner clarified the estimated base footprint as **10 × 10 chunks, or 100 chunks total**. This is a provisional planning estimate, not a measured typical colony or a hard size limit: colony building is not yet available to establish what actual bases will look like. Use it as an initial workload point and revisit it as playable colonies become available. The minimum target class is a **midrange laptop with four CPU cores and 8 GB total RAM**. This is whole-machine memory, not an 8-GB allowance for the game heap.

An exact battle size is not yet specified. In this document that means simultaneous combatants, including hostiles and relevant NPCs, not just controlled units. Measure increasing hostile/pathfinding/wound loads rather than inventing a supported enemy cap. The owner explicitly notes that substantial future memory/work is not implemented; benchmarks describe current scope and headroom, not proof of final-product capacity.

### D-9. Preserve interaction and legibility before simulation wall-clock speed

The owner accepts simulation slowing even to a crawl under strain, with slow 2D animation remaining readable, as in the Dwarf Fortress inspiration. Input and UI must not become comparably sluggish. Assess simulation throughput and input-to-visible-feedback separately. No fixed normal-speed throughput target across all stress loads should be inferred from the hardware target.

This supersedes the draft's earlier allowance that UI could simply wait for a whole slow Lua gameplay callback or complete simulation step. Retain coherent step completion, but add safe resumable work and presentation opportunities within long steps. Numerical response-time targets remain proposals under Q-3.

### D-10. Prepare ordinary routes; estimate exceptional forced movement

The owner explicitly accepts loading the required chunks before allowing an ordinary move to begin. This supersedes the earlier ambiguity about whether every pre-movement wait was unacceptable. Retain those chunks for the admitted movement. The finite movement extent covered by one admission still needs definition; do not infer permission to load an arbitrarily long cross-world route into memory.

The exceptional case is displacement by another force: for example, a unit thrown at high speed into an unloaded chunk. The owner wants the unit's motion and relevant state tracked using best estimates, then reconciled when terrain arrives. Future elevation and stat-cost prediction do not exist yet and are part of that future feature. Approximation is accepted for this rare case; replay-perfect simulation is unnecessary. This is now an accepted fallback direction, not merely an optional idea for ordinary walking.

The owner additionally specifies that the provisional unit is **phased out of reality until reconciliation** and **must not affect anything else**. Its estimated trajectory is private accounting, not an interacting world position. It cannot damage or displace others, block movement, perform work, transfer items, or reveal terrain through its provisional presence. Reconciliation resolves its own state and reintroduces it at a valid boundary; it does not replay missed interactions against other actors. Rendering, selection, incoming targeting and reinsertion details belong to the separate movement design and must respect that isolation.

Q-4 retains the estimate horizon, collision/consequence rules, persistence and last-resort behavior. General tolerance for approximation does not select a teleport distance, refund policy or death reversal rule. An unknown-terrain trajectory is not an ordinary pathfinding route and cannot assume a prevalidated path exists.

### D-11. Slow gameplay progression while continuing interaction service

The owner wants all load-bearing gameplay either in scripts or called from scripts, with gameplay looping slowing under load while input and UI keep getting service. Native calculations need not be rewritten in Lua, but they must participate in the same controlled advancement and cannot independently run ahead when gameplay scripts fall behind. Input responsiveness is the minimum requirement; Lua-based UI responsiveness remains the desired outcome.

Do not implement this by adding delay to the entire Lua owner: that would delay UI too. Separate gameplay admission from interaction service and yield long gameplay work cooperatively. Q-2 must reconcile the requested script-driven entry points with the previously approved unit-worker coordinator; the coordinator can retain clock/permit ownership while native workers execute explicit phase work. The exact orchestration boundary remains a proposal, not permission to silently relocate all engine ownership into Lua.

## Proposed architecture

### P-1. One time authority, retaining public seconds

Use a fixed base interval `h` and an integer completed-step ordinal. Compute session seconds from an origin and the ordinal rather than repeatedly adding floating-point `dt`:

```text
committedTime = originSeconds + completedStep / baseHz
next grant   = { sessionEpoch, stepId, startTime, endTime, dt=h }
```

`sessionEpoch` rejects work from a replaced runtime session; it is distinct from persisted time and from world-page identity. A successful load installs a fresh runtime epoch while preserving saved gameplay time. Additional world pages do not reset the clock.

Retain `engine.gameTime()` and persisted deadline units as **session seconds**. Do not reinterpret old seconds as tick counts. Inside a granted gameplay phase, clock-dependent APIs receive that phase's `endTime`; outside a gameplay phase, queries receive the latest completed time. Relative integrators receive `dt` explicitly. The phase context must reach modifier-expiry, combat-admission and other engine helpers as well as Lua's top-level getter; changing only `engine.gameTime()` would leave a second clock inside the same operation.

`gameTimeRef` becomes the compatibility publication of completed time, with one ordinary advancement writer; the existing quiesced load/session-reset paths may install a replacement origin. It is not advanced speculatively before owners finish. Rendering that consumes it must use a matching completed motion snapshot, described under P-8.

Preserve the current fresh-session epoch contract (#2291): boot and Exit to Menu reset to zero; a load installs saved time; creating another page does not reset time. Unpaused menu time currently accrues even with no page. Empty participant sets may complete empty steps to retain that behavior; stopping the clock merely because the menu is visible would be a separate policy change, not an incidental optimization.

### P-2. Coordinate existing owners before adding another worker

**Recommended first implementation:** host the coordinator in the existing unit worker, which already owns the session clock. Keep the world, combat, fluid and Lua workers as execution owners. The coordinator executes its own unit stages directly and grants bounded work to the other owners. There is no request followed by a wait on itself.

D-11 adds a script-driven orchestration requirement to this baseline. GT-1 must explicitly map which gameplay entry points are Lua routines and which invoke native work, including currently independent movement, wounds and world/fluid advancement. A possible arrangement keeps pacing and completion accounting in the unit worker while a resumable Lua gameplay driver requests the native phases under that permit. This is not yet the agreed concrete graph: native requests must yield control rather than block Lua while an owner waits for another Lua callback. GT-1 may investigate and test this protocol without migrating workers; settle the resulting mapping under Q-2 before drafting GT-2 through GT-6 as implementation-ready issues. An engine-owned grant loop alone does not establish that every load-bearing operation is called from scripts.

Start with an explicit phase order. Independent units within an owner may retain their current ordering. Permit overlap only when a declared read/write dependency analysis and tests show it safe. A shared IORef is not evidence of independence merely because each write is atomic.

This sacrifices some current overlap to establish correct boundaries. It is a recommendation, not a measured speed improvement. GT-15 must measure its cost before default activation. D-9 allows lower simulation throughput under strain, but not unacceptable input/presentation delay. If serial phases or individual work segments violate that service target, revise batching or phase grouping under Q-2; do not hide the failure by skipping one owner's time. Waiting for a phase acknowledgement must not occupy CPU in a busy loop.

Keep scheduling debt, in-flight grants and acknowledgements in worker-local runtime state. Construct shared transport handles during startup and pass them to worker owners. Use existing managers/capabilities for domain state. Do not scatter clock refs over `EngineEnv` or invent a new capability/role without following `docs/engineenv_capability_inventory.md` §6.4. GT-1 must map the actual state/transport placement before infrastructure issues are ready to implement.

### P-3. A step is completed work, not a timer notification

Only one gameplay step may be in flight. Each grant identifies its session, step and phase. The recipient validates identity and its last completed phase before doing work; a duplicate/stale grant cannot apply effects twice. An acknowledgement is sent only after that phase's authoritative effects and required handoffs finish.

A phase may suspend at declared batch boundaries without acknowledging completion. Retain its captured membership, cursor, interval context and pending effects; service safe input/presentation, then resume the same phase. No second gameplay step or next dependent phase starts during that suspension. This is cooperative scheduling, not rollback, partial time publication or rerunning an already processed unit.

Proposed initial phase order:

| Phase | Work and dependency rule |
| --- | --- |
| Admission | Apply the finite accepted prefix of external gameplay commands and ready asynchronous publications. Fix participant membership and interval-start work/activity facts. Transaction requests take the boundary path instead of entering a step. |
| Motion and direct actions | Apply movement/action transitions for `h`, then direct combat consequences at their documented phase boundary. Movement exposure and other interval facts are retained for the time just consumed. |
| Continuous consequences | Advance physiology and wounds for `h`, in a documented order; settle resulting death/collapse/stop commands before later work phases inspect eligibility. Preserve atomic spending and current death-before-collapse precedence. |
| Productive work and environment | Resolve eligible work intent and per-recipe electrical demand, compute power admission/energy for this interval, credit only authorized work, advance precise calendar/regrowth/item thermal state, and run due fluid work. Order causal dependencies explicitly rather than relying on queue timing. |
| Decisions and scheduled events | Run due AI decisions and spawn polling once for their scheduled opportunity. Their commands normally target the next step. Advance no additional continuous time here. |
| Completion | Finish required world-owned writebacks, publish completed time/motion, record acknowledgements and offer a control/UI service turn before another step. |

These are semantic stages, not necessarily six modules or six messages. Work intent/power admission may require splitting today's world/Lua routines. GT-1 records the concrete acyclic phase graph; Q-2 must be resolved before treating this table as a fixed implementation order.

For every cross-owner effect, classify it as **current-step forward handoff** or **next-step command**. A forward handoff must be applied or explicitly refused before its dependent phase acknowledges. An effect emitted toward an already completed phase belongs to the next step, unless the graph has an explicit bounded settlement stage. Never drain queues until a cross-system cycle happens to become quiet.

Example: physiology queues a death today. In the new graph, that consequence must reach the unit owner before a later work phase credits the dead unit. By contrast, an AI decision to begin a new walk takes effect next step and cannot retroactively earn travel or work for the preceding interval.

### P-4. Separate continuous integration from periodic decisions

**Initial correctness baseline:** move the coupled continuous accumulators onto the base step. A candidate is **30 Hz**, matching today's movement target. This means physiology, wound integration and continuous construction/work receive `h`, even if their former parent script ran at 10 or 5 Hz. AI decision scans can remain 10 Hz and spawn polling 5 Hz; individual `nextActionAt` deadlines and jitter still govern actual decisions. Active fluids may remain a discrete 10 Hz system, scheduled every third completed base step.

This would increase some integration call counts. It is deliberately a baseline to measure, not a promise that every existing physiology scan should ship at 30 Hz. Check per-callback probabilities, small-write thresholds and iterative convergence as well as `rate * dt`: more calls must not triple an event probability or discard newly smaller increments. If the cost is unacceptable, lower the common base rate or introduce measured multirate integration with the obligations below. Do not choose different elapsed-time loss policies for expensive subsystems.

| Consumer | Proposed time contract |
| --- | --- |
| Motion, combat/action deadlines | Granted phase time and fixed substeps; no worker elapsed samples. Preserve waypoint/terrain safety. |
| Stamina, other continuous physiology, wounds | `h` for each accepted base interval; no missed-callback allowance. Preserve update ordering inside physiology and mutation semantics. |
| Productive work | Credit `h` only while the unit/job is eligible for that interval. Decision polling frequency does not determine progress. |
| AI decisions, spawn checks, notifications based on game deadlines | Scheduled using session time; execute once when due, without inventing a decision for every historical missed opportunity. |
| Fluids | One defined solver iteration per scheduled simulation interval; work duration affects wall pacing, not how many intervals the solver is credited. |
| Calendar and elapsed world processes | Granted duration converted through the explicit calendar rate, with fractional progress retained. |

Periodic decision skipping is different from skipping continuous simulation. It is acceptable to deliberate once at a due boundary; it is not acceptable to skip the intervening hunger or work accounting.

If a later optimization batches a continuous system, it must retain the relevant interval inputs and consume exactly the credited duration. It cannot use the latest pose to classify all previous movement, or apply one large Euler step across resource thresholds. Batching that delays an admission-affecting consequence across combat/work phases needs a revised phase contract, not just a larger `dt`. Pending continuous intervals must be settled at a completed boundary or explicitly persisted; the initial every-base-step baseline avoids that debt.

A discrete fluid cadence may leave a fractional interval until its next scheduled iteration. That is its defined sample cadence, not forgotten elapsed time; preserve cadence phase across save/load. Terrain edits between solver iterations are admitted according to that discrete policy. Initialization/fast-settle iterations remain preparation work, not secretly elapsed gameplay.

### P-5. Preserve eligibility and distinguish it from lack of CPU service

Separate lightweight `advanceWork(context)` from expensive `decide(context)` in unit AI. Work/physiology do not wait for `nextActionAt`. Retain present eligibility checks: hold position, player orders, valid claims, adjacency, building lifecycle, power, required material and active action state.

Use interval-start facts and explicit transition fractions where meaningful. Movement should report time/distance spent walking and arrival within the step, so an arrival does not classify the entire interval as idle. A job selected at the end receives no earlier work credit. A cancellation known at admission earns no work for the following interval. Numerical/phase ordering can quantize instantaneous events to a boundary; that bounded error must be documented and tested, not caused by variable worker delay.

Keep commanded-order stall budgets in **eligible simulated time**. A player pause, blocked terrain preparation, disabled unit or suspended order must not consume its stall budget merely because another location continues. The current 2-second caps and 5-second gap heuristic are migration guards, not the final definition of eligibility. Remove them only after every interruption currently covered has an explicit timing disposition and regression test.

Electricity and work need a shared interval decision: declare demand, determine admission, then debit energy and credit the matching work quantum. If power is available for only part of an interval, either credit that same fraction or use an explicitly conservative full-quantum refusal; Q-2's phase contract must choose one policy. Do not preserve the current race by checking a power snapshot before a later unrelated battery debit.

This is an accounting contract, not permission to remove the separate strict player-transfer and lax AI paths or rewrite claim ownership.

### P-6. Wall time requests steps; it does not grant independent time to owners

Only the coordinator samples monotonic wall time for gameplay pacing. Render, transport and other non-gameplay clocks remain independent.

Proposed pacing policy:

1. Sample the raw clock; sanitize invalid/backward values and cap host interruptions using the existing 0.25-second policy. Replace the raw sample even when its excess is discarded.
2. Convert admitted pacing time through the chosen global simulation-speed multiplier and accumulate a bounded amount of requested duration.
3. Grant complete fixed steps while sufficient duration is available, subject to a maximum burst count and a wall-time service budget. Do not add a pacing sleep while behind the target, but yield input/presentation service between bounded batches inside long phases and full boundary work between completed steps. When ahead of the target, sleep or wait interruptibly.
4. Bound outstanding pacing debt in simulation seconds. Discard excess **unstarted demand**, record it, and slow simulation relative to the wall clock. Never discard an already admitted step or a particular owner's remaining phase.

For example, at a 30-Hz base, a 0.2-second shortfall means six fixed steps. They may run back-to-back with respect to simulation pacing, but still contain presentation service opportunities. A 4× speed request asks for four times the simulation duration per wall second; if the machine cannot supply it, realized speed is lower. An ever-receding wall target must never monopolize the UI or create unlimited debt.

No exact values for debt cap, burst count or service budget are approved. GT-15 measures them against Q-3's envelope; initial test values are fixture parameters, not shipping defaults. A numeric time-scale ceiling is not a substitute for this work budget.

On ordinary pause, load or a global infrastructure hold, stop admitting steps and rebase wall pacing; do not accumulate a debt to replay on resume. A single non-yielding handler can still block its owner: that limits what cooperative scheduling can guarantee, and an offending shipped path must be bounded or split before passing the UI-service gate. No timeout may claim the phase succeeded or run the next step concurrently. A watchdog reports the blocking owner; a crashed owner takes the fail-stop path below.

### P-7. Gameplay steps and transaction/control work have different permits

The coordinator has explicit modes: `Boundary`, `RunningStep`, `Paused`, `Transaction`, and `Faulted`. Ordinary gameplay work requires a step grant. Queue service and authorized save/load work must remain possible without one.

**Pause:** a pause request immediately prevents new admission. A running step finishes its already granted phases using its immutable running context; reading the newly set global pause flag halfway through and skipping the remaining physiology would produce a half-step. Migrated gameplay entry points use the grant's admission state instead of their current `pause.isPaused()`/global-flag early returns. Public control/UI queries may report the pending pause immediately; they must not be reused to revoke part of an admitted interval. Pause becomes a settled gameplay boundary when that step completes. Expose requested versus settled state to diagnostics; preserve the existing rejection and pause-epoch semantics. Finishing the admitted step is accepted under D-6; its concrete service/wait graph remains subject to Q-2's verification.

**Save/load:** acquire a completed gameplay boundary first, then use the existing multi-pass save barrier and its per-owner parking protocol. Do not begin parking individual save owners while another phase of the same gameplay step is still owed. Quiescence drains still operate; save/load remains mutually exclusive; publication and post-load `LuaSaveLoaded` reconciliation retain their current ordering and failure dispositions. Do not reuse step acknowledgements as save acknowledgements: the two protocols have different authorized work and lifecycle.

**Lua deadlock prevention:** `engine.saveWorld` currently synchronously waits for owner acknowledgements. It may run in a boundary/control callback while the coordinator admits no gameplay step and owners still service transaction work. It must not wait inside a gameplay callback whose phase the coordinator is awaiting. Current save callers are UI/world-view/autosave paths, not the three periodic gameplay owners. Classify these explicitly. A prohibited blocking API invoked from gameplay context returns a clear refusal before mutation; provide a separate request-at-next-boundary mechanism if a future gameplay system needs it. Do not silently report a deferred save as completed or launch a hidden reentrant coordinator loop.

`engine.loadSave` keeps its synchronous-acceptance/asynchronous-status contract. Player-initiated lifecycle calls run through the boundary lane. Pause requests caused by gameplay events latch a stop after the current step; they do not block the caller. Ordinary debug Lua and synchronous input settlement execute at boundaries, with their existing results and cancellation rules. Built-in waits that already run on client threads stay there.

**Faults:** a failure after a phase has mutated state cannot be rolled back by this design. Latch the engine fault, stop further steps, keep the last completed visual/time publication, and allow diagnostics or explicit restoration from a valid save. Do not publish the failed step, retry its whole callback, allow ordinary gameplay to resume, or save partially applied state as a coherent session. The current worker fail-stop contract is retained; recovery automation is outside scope.

**Shutdown/session teardown:** stop admission, finish or fault the in-flight step, then run owner shutdown/teardown and reply cancellation. A new session receives new runtime grant identities. It cannot inherit old grants, command prefixes, cadence cursors or pacing debt.

### P-8. Keep the Lua owner single and rendering independent

Add explicit script scheduling classes: **gameplay**, **ordinary/UI**, and **event-only/control**. The migrated gameplay registrations receive step context through a dedicated entry point. Ordinary `update(dt)` retains its existing scheduling and reentrancy contracts; never globally reinterpret every script's `dt` as game time.

The Lua state remains single-threaded. Its owner services granted gameplay at the prescribed phase and safe presentation callbacks at cooperative batch boundaries, including while that phase is incomplete. Ordinary callbacks with unrestricted gameplay/lifecycle access still require a complete-step boundary. #2415 supplies finite ordinary message/console batches, but the grant/control/presentation lanes also need explicit finite service policies. Neither gameplay grants nor interaction feedback may starve behind replenished work. Save reconciliation still takes precedence over starting normal gameplay.

The current native input worker already publishes input state separately (`src/Engine/Input/Thread.hs`, `src/Engine/Input/Thread/Dispatch.hs`, checked at `7f135c0ed78ee6c30cae9bd4ab2242b55f21de3b`). Event handling also queues Lua messages, so prompt capture alone does not prove prompt visible response. Preserve event ordering, modifier-release fences and transaction gating when introducing priority service; do not pull selected messages ahead of their prerequisites. Gameplay slowdown means fewer completed fixed steps per wall second, not less frequent service of the whole Lua thread or larger gameplay deltas.

Current `unitResources.update` and `unitAi.update` each loop over the entire roster (`scripts/unit_resources.lua:77-95`, `scripts/unit_ai.lua:461-492`). Convert migrated loops to resumable batches, retaining per-step membership and a cursor. A batch completes an internally consistent unit operation before yielding; do not interrupt a unit halfway through circulation/temperature/consciousness updates. A wall budget alone cannot stop one oversized operation, so profile and split such operations at real safe boundaries. Do not add arbitrary Lua instruction interruption or execute another Lua state against shared gameplay globals.

Presentation/input service has a restricted contract: use completed read views for gameplay-derived display, mutate UI-local state, and queue gameplay intents for their normal admission point. It may capture a move click or latch a pause request and show feedback promptly; it may not call the suspended AI's mutation functions, edit live unit state, run unrestricted debug Lua or start save/load mid-phase. Revalidate queued intents at authoritative admission. Existing synchronous APIs stay in the boundary lane unless explicitly adapted with tested result semantics; a script cannot gain permission merely by being labelled UI. Non-ticking awareness/reconciliation work must be classified too, not smuggled through presentation service because it runs while paused today.

Measure **input capture**, **visible acknowledgement**, and **gameplay effect** separately. Camera motion, selection feedback and the pressed pause control should respond on wall time; a movement order's world effect still waits for its eligible simulation boundary. A pending pause can receive immediate feedback even though an already admitted step has to settle before save-safe pause is reached. This is not a claim that a whole-system GC pause, blocking driver or arbitrary debug loop can be preempted by the application scheduler.

Presentation polls, camera input, logging, autosave wall intervals, preview animation and popup coalescing remain independent of simulation progress, subject to their required access lane. Autosave eligibility can be noticed promptly, while its transaction still waits for a completed boundary. Keep presentation/GC/allocation work within measured budgets too; cooperative Lua yields do not by themselves guarantee a responsive whole process.

Publish an immutable unit-motion/time view after a completed step. In-world animation selection uses its matching simulation time. Optional interpolation may smooth positions between completed samples, bounded to those samples; wall time must not advance an attack, fall, death or other gameplay timeline beyond the last completed state. The first implementation can hold the last sample. It must not interpolate through a pause, load or teleport. This arc does not require a transactional snapshot of every render cache.

### P-9. Calendar conversion and residency membership are explicit inputs

Keep three distinct concepts: **session seconds**, **calendar conversion** (minutes per session second), and **simulation speed** (how quickly session seconds are requested relative to wall time). At normal speed, retaining the present calendar conversion of one minute per session second avoids a wholesale content retune.

Q-1's direction is resolved by D-7: whole-game fast-forward changes step pacing for every gameplay owner. The existing `world.setTimeScale` remains a calendar-rate API for compatibility; it must not silently become a multiplier of movement or combat. At speed 1, it retains its documented numerical meaning. With separate simulation speed, apply calendar rate to simulation seconds rather than multiplying by speed twice. Loading resumes with default speed under the existing reset policy. The underlying pacing contract is included; no speed UI is included.

Reuse the calendar fraction repair's precise per-page representation. Derive calendar deltas and world-process duration from the same granted interval and page conversion. Keep time-of-day/date setters as explicit control commands, distinct from gameplay clock progression; they must not reset session deadlines.

The timing coordinator consumes a prepared participant set. Membership is independent of the viewed page and fixed for a step. This arc can advance multiple already prepared pages and their units in tests; making those pages remain prepared during real travel belongs to residency work.

Existing gameplay-critical participants must not be silently dropped when data or budget is unavailable. In ordinary movement, prepare and reserve the required footprint before admitting motion, as accepted in D-10. A pending move does not remove its unit from physiology, combat or the rest of simulation, and infrastructure preparation must not consume an order's eligible-work stall budget. D-10 separately requires an explicit fallback for unexpected forced movement beyond prepared terrain. An admitted verified footprint disappearing remains a reservation/lifecycle violation, not an automatic switch to prediction.

**Recommended first defense: load ahead of motion.** Prioritize data along the unit's intended corridor before it approaches the edge, using speed and a conservative observed preparation-latency margin to size lookahead. Raise preparation urgency as time-to-boundary shrinks; rebase demand on reroutes. Retain current/near-route gameplay data independently of camera visibility. During fast-forward, account for increased wall-time travel rate and bound admitted lookahead by the memory policy. This needs measured tail latency and bounded demand; no finite buffer guarantees success under unlimited speed or I/O delay.

Distinguish missing gameplay/navigation data from missing rendering assets. If collision/elevation/material and relevant edits are already known, a unit can advance correctly before distant visual data finishes loading. A compact retained navigation corridor could support that without retaining every render artifact. This is a proposed residency interface, not an assertion that the present chunk pipeline already separates those products. Inspect whether the actual delay is generation, I/O, publication or graphics work before designing a new cache tier.

**Forced-movement fallback:** when a throw or other impulse exceeds the prepared footprint, prioritize preparation along the projected trajectory and retain a bounded provisional record for the affected unit. The owner's requested fallback applies even to genuinely unknown terrain; a previously validated corridor is useful where available but is not a prerequisite that excludes the motivating case. Current `usLocalPath` contains only x/y waypoints (`src/Unit/Sim/Types.hs:39`), not future z/material/exertion data, and a throw needs its own motion model rather than walking pathfinding.

Proposed record: last confirmed position and velocity/impulse, session/page identity, start step, elapsed simulated duration, bounded trajectory segments, assumptions and estimated costs already charged. Advance estimates with the same granted simulation intervals as other units, never the duration of the disk read. Reconcile at a declared step boundary when authoritative data is ready. Where loaded geometry shows a wall partway along the swept trajectory, resolve the first relevant impact instead of checking only the estimated final position. Bounded history and swept collision handling belong to the future movement feature; neither is claimed to exist today.

**Isolation is decided under D-10:** the provisional unit is phased out and cannot affect the surrounding world. Keep its estimated motion, physiology and costs in its own accounting; suppress ordinary world-interaction execution until reinsertion. Reconciliation must not replay attacks, displacement, item transfers, discovery or other missed external effects along the provisional trajectory. The unit's own collision/injury correction remains possible; changing other actors' histories does not.

Provisional mode spans completed simulation steps; it is not one indefinitely incomplete phase. A step acknowledges the unit's bounded estimate/accounting work without waiting for terrain I/O, so the home colony and other units continue. Ready terrain permits reconciliation at a subsequent boundary. Distinguish this gameplay mode from a worker that failed to finish its granted work. The exact horizon and failure policy must be chosen by the movement design rather than allowing unbounded trajectory storage or a hidden global pause.

Q-4 retains self-damage correction timing, the estimate horizon, persistence and exhausted-horizon behavior. A short provisional record limits repair scope; it does not by itself decide how later physiology or death should respond to a newly discovered earlier collision. Reconcile accounted costs exactly once and never replay already credited metabolism for the elapsed interval. If this mode exists when saving, its unresolved trajectory is gameplay state, not disposable pacing debt or an in-flight grant; its implementing design must provide an explicit save contract. This timing arc defines the interval/mode interface; the separate streaming/movement design owns implementation and consequence policy.

The local [chunk residency design's hibernation decision](chunk_residency_streaming_design.md#d-20-disk-backed-hibernation-is-withdrawn-from-the-delivery-plan) withdraws disk-backed hibernation pending measurement, while the older #1997 body still describes it. This document follows the local design's boundary and does not reintroduce hibernation. Its per-page budget assumption must be revisited by the residency arc when simultaneous gameplay pages ship. This timing design claims neither that reservations are implemented nor that a multi-page memory budget is solved.

### P-10. Preserve time continuity in saves and migrate without mixed clocks

Keep existing absolute seconds and resource/work values meaningful. Add required timing metadata through the appropriate existing component rather than inventing another optional component: clock origin, completed ordinal/cadence phase, and the versioned cadence profile needed to interpret them. Runtime epoch, in-flight grants, transport queues, wall samples and unstarted pacing debt are not saved.

For older saves, preserve the exact saved session seconds as the new origin and start the new ordinal/cadence phase at its documented migration default. The old save has no common cadence phase to recover. All stored modifier/wound/action/AI/spawn deadlines remain in seconds. New saves must preserve cadence phase so repeated save/load cannot indefinitely postpone a fluid iteration or other periodic event.

`sdGameTime` remains the compatible published value, derived consistently from timing metadata; validate any mirrored fields at decode. A change of shipping cadence after this format lands needs a compatibility rule that preserves time and periodic phase, not reinterpretation of an old ordinal at a new Hz. Freeze DTOs, migrate per component, retain fixtures and update both persistence inventories under the repository contract.

Saving occurs only at a completed base boundary, with no owed continuous integration. The save protocol may apply accepted quiescence commands without advancing time, exactly as its contract permits. The snapshot records the resulting state at that time; it is not claimed to be the immutable render snapshot. Fluid preparation may remain unfinished under its separate save/activity contract; a save must not force future simulated time merely to settle water.

On successful load, install saved timing with the replacement session while owners are parked, discard old-session work at the existing publication cutover, finish Lua reconciliation, and reset wall pacing. Failed staging keeps the old session unchanged and paused; reconciliation failure retains its distinct published-but-failed state. An additional page does not start a new session clock.

Build the coordinated path dormant first. Every production owner stays on the legacy clock until all migrated owners, lifecycle support, persistence and cross-domain checks are ready. Tests may select a complete isolated coordinated backend, but there is no supported production mode where units use granted steps while physiology or the world still free-run. Never “shadow” gameplay by mutating the live state twice. The final activation removes legacy advancement and the test-only selection switch.

## Alternatives considered

- **Just share a tick counter:** rejected as sufficient architecture; it records desired time but does not establish completed work or bound worker drift.
- **Pass wall elapsed time into every Lua callback:** unsuitable as the general fix; large gaps cross thresholds and activity changes, and cannot safely replay commands or discrete fluid transitions.
- **Run all simulation on the render thread:** rejected as the baseline because rendering and controls should remain independently paced. It also changes established ownership unnecessarily.
- **Immediately move all gameplay into one new thread:** still a possible simplification, but entails moving Lua/world/unit ownership and save behavior at once. The proposed existing-owner coordinator keeps those boundaries explicit; Q-2 remains open if its coordination cost is excessive.
- **Independent fixed-step accumulators per worker:** better local integration, but without a common completion/backlog rule they can still advance different amounts. Not sufficient for D-1.
- **Unbounded catch-up or skipping the expensive owner:** rejected; the former can prevent control service, the latter recreates balance changes.
- **Require every current detail to be replay deterministic:** unnecessary for the owner's goal and not a condition of completion.

## Open questions

These questions are deliberately open at document readiness. They are not delegated permission to invent a product policy during implementation. The following gates allow the epic and the first bounded protocol slice to be processed while preventing dependent work from assuming missing answers.

| Question | Work that resolves it | Stop before proceeding |
| --- | --- | --- |
| Q-2: script/native phase, wait and presentation-access graph | GT-1 maps the actual call sites, owner transitions, safe yields and required read views, and tests the pure protocol. No worker migration occurs in that slice. | Do not draft GT-2 through GT-6 as implementation-ready until that concrete graph is recorded and reviewed. If satisfying D-11 requires changing the accepted ownership baseline, return that material change to the owner. |
| Q-2: partial-interval power/work policy | GT-1 records the alternatives and affected handoffs; settle the choice before the productive-work adapters are specified. | Ask the owner to choose or approve the recommended behavior before GT-10/GT-12 are implementation-ready. Do not silently select partial credit or full-quantum refusal. |
| Q-3: service target and shipping parameters | Early pure/isolated tests use explicit fixture values. GT-15 measures the full migrated backend on the stated workload and machine class. | Agree the numerical responsiveness target with the owner before treating GT-15 results as pass/fail. A failure or material cadence change returns to design; GT-16 stays blocked. |
| Q-4: currently reachable missing-terrain paths | GT-1 inventories reachable displacement/readiness paths and records the timing-state interface. Relevant adapters preserve their supported behavior until an explicit disposition exists. | Do not remove a protective guard or activate an unhandled reachable path. Ask for any unresolved current-path behavior before the affected child; future throwing details stay with the separate movement design. |

GT-1 also assigns each presentation read view and AI action family to its delivery owner. GT-5A supplies the shared suspension/read-view/intent mechanism with a real fixture; the domain adapters supply their domain-specific publication and yielding. It does not absorb a rewrite of every UI panel. If the access map requires separately changing existing panels, or one AI family needs a separate migration, add a bounded stable slice through design before processing that work. This is a scope-change gate, not authorization for an oversized catch-all PR.

### Q-1. What should simulation speed mean?

Resolved in direction by D-7: support whole-simulation fast-forward by executing fixed steps toward the requested target. Keep calendar-rate compatibility and leave speed UI outside this arc. The no-pacing-delay catch-up path still yields input/presentation service and bounds unstarted debt under D-9. Exact multipliers, debt and service budgets are engineering parameters under Q-3, not grounds to reopen this behavioral choice.

### Q-2. Is the proposed coordinator, phase graph and boundary behavior the accepted baseline?

Baseline resolved by D-6: unit-worker coordinator, one in-flight step, initially ordered owner phases, explicit forward handoffs, finish an admitted step before settling pause, and boundary-only blocking lifecycle APIs. Remaining specification work: choose the work/power partial-interval policy and complete the concrete dependency/wait graph. This affects nearly every adapter and the save deadlock proof. GT-1 must map owner read/write dependencies and every synchronous wait; if a cycle or unsupported ownership transfer remains, revise the affected design before implementation issue readiness. Do not reopen the accepted baseline merely because its final graph still needs that verification.

D-9 additionally requires safe presentation/input service during an incomplete phase. P-8/GT-5A propose cooperative batches and restricted read/intent access. Complete that access map and safe suspension protocol as part of the graph review; arbitrary UI callbacks cannot be allowed to mutate a suspended gameplay phase's state.

D-11 supplies new steering on script-driven orchestration. Preserve the accepted clock/worker ownership where possible, but map the Lua driver/native phase entry points in P-2 and prove their wait graph before treating that arrangement as resolved. This is a focused consequence of new owner input, not a request to reapprove the entire baseline.

### Q-3. What workload and responsiveness envelope should select the cadence?

Partly resolved by D-8/D-9: typical 5–50 controlled units; a tentative 10 × 10-chunk base (100 chunks); four-core/8-GB minimum laptop class; readable simulation slowdown is acceptable, but UI/input service has priority. The footprint is a planning guess pending playable colony building, not a capacity promise. No fixed battle ceiling or shipping cadence is yet declared.

Candidate cadence remains 30 Hz continuous, 10 Hz AI/fluid opportunities and 5 Hz spawn polling, subject to cost and numerical tests. Suggested measurement matrix: 5/25/50 controlled units exercising currently implemented work across a 10 × 10-chunk prepared footprint, then increasing NPC/combat/pathfinding and streaming pressure. State which colony systems are represented and which are absent; this is a constructed workload, not evidence of an established colony size. Label the stress increments as experimental loads, not promised supported battle sizes. Compare quiet, ordinary-work and overload cases; an empty arena of idle units is not a colony capacity test.

Starting proposal for discussion: aim for input-to-visible acknowledgement at or below **100 ms at the 95th percentile** on the target-class workload, recording worst observed stalls separately. Measure camera/selection/menu/pause feedback even while a gameplay step is deliberately delayed. This is a proposed usability threshold, not an owner-approved number or measured result. Gameplay command execution latency is a separate simulation-time metric. Aim for normal simulation speed in ordinary conditions, but report actual throughput rather than pretending the owner required it at every stress level.

Record the actual CPU/GPU/storage/power mode and whole-process resident/peak memory on an appropriate machine; core count alone does not identify laptop speed. Leave room within 8 GB for the OS, graphics/shared memory and future systems. A large workstation with an 8-GB heap cap is not proof of behavior on an 8-GB laptop. Benchmark current implemented scope and retained headroom; revisit as future systems land. Avoid paging that undermines the very UI target being tested.

GT-15 measures phase costs, continuous batch cost, control service, memory and realized simulation speed against the independently chosen envelope. Select budgets within that envelope. A degraded UI target or materially different cadence/multirate contract comes back for a design decision; measurement must not choose its own pass threshold.

GT-1's tests are parameterized, and GT-6 through GT-14 remain dormant while this is open. GT-16 cannot activate an unmeasured or failed configuration.

### Q-4. How should a unit waiting for future terrain affect time eligibility?

Partly resolved by D-10: ordinary moves wait for explicit chunk preparation before starting; rare forced displacement beyond ready terrain uses estimated motion/stat changes and later reconciliation. Do not reopen whether approximation is acceptable. Loading readiness applies to a finite admitted movement extent; determine its size under the residency budget so a distant destination does not require retaining an entire world-spanning route.

Interaction policy is resolved by D-10: the unit is phased out until reconciliation and cannot affect anything else. Missed external encounters are not replayed afterward. Remaining choices are the finite forecast horizon, the unit's own impact/damage correction timing, reinsertion, persistence and last-resort behavior if data remains unavailable. Exact replay is unnecessary, but the implementation still needs a rule when a newly revealed wall precedes the estimated endpoint or when accumulated injury changes survival. Those choices must preserve other units' progress and the isolation rule.

Handle these detailed forced-movement policies in the separate streaming/movement design. They need not expand this timing epic into an unimplemented throwing system. GT-1 records the time/accounting interface and inventories whether any current displacement path already reaches the exceptional case. Any currently reachable path needs an explicit disposition before GT-16; future feature work must resolve its remaining policies before activation.

Coordinate this with the residency design's caller-specific reservation policy. GT-1 defines distinct readiness/prediction states; GT-10 must not remove old interruption guards until the supported outcome is explicit. Preserve home simulation and avoid double-charging or silently granting free survival. GT-16 needs one honest implemented disposition for every reachable path; a future approximate mode must not become an implicit missing-terrain fallback in the meantime.

## Verification strategy

Use a headless driver of the real coordinated entry points with scripted pacing and completion scheduling. Compare equal **completed simulation duration**, not equal wall sleep. One schedule supplies prompt acknowledgements; another delays Lua, combat, world or fluids and supplies finite queue replenishment. Continuous state and intended conservation relationships must agree within declared integration tolerances; control responsiveness is measured separately.

Required scenarios:

1. Move a survivor through a fixed prepared path while tracking stamina/exertion and calendar progression. Include waypoint arrival and a mid-step movement stop.
2. Resolve a fixed attack sequence and wound progression under delayed combat service. Control RNG streams/inputs for assertions without turning determinism into a product requirement.
3. Run powered crafting/construction with finite battery energy, admission failure, interruption and completion; compare credited work with charged energy.
4. Advance a fixed fluid fixture through equal scheduled iterations while altering worker delivery; preserve volume, edit-generation rejection and acknowledged world publication.
5. Exercise pause requested from input and from a gameplay callback, synchronous save from boundary Lua, prohibited in-step blocking calls, load failure/publication/reconciliation failure, shutdown and fresh-session identity.
6. Save/load at different cadence phases, with expiring modifiers, wound timestamps, AI eligible counters, building spawn cooldown and fractional calendar time. Repeated loads cannot erase owed cadence or move deadlines.
7. Saturate each ordinary queue with finite replenishment beyond its batch budget; gameplay grants and controls still make progress. Deliberately stretch a roster phase across multiple presentation turns and assert camera/selection/pause feedback, stable completed read views, queued intent revalidation and exact-once cursor resumption. Arbitrary callback preemption is not promised; oversized shipped operations must be split or fail the service gate.
8. Advance two fully prepared gameplay pages while switching view. Their eligible time is unchanged; a not-ready participant produces the declared Q-4 outcome.

At the timing-interface level, model a provisional unit across several completed steps: its private elapsed accounting advances once per granted interval, it emits no world effects, and it does not block other participants awaiting terrain. This does not require implementing throws in the timing epic. The future movement implementation additionally verifies swept-terrain correction, isolated reinsertion, no replay of external encounters, and its chosen persistence/horizon policies.

Pure controller properties cover no duplicate acceptance, no acknowledgement of the wrong epoch/phase, bounded unstarted debt, no time advance on failed/incomplete steps, and nonnegative time. Domain probes remain necessary where pure fixtures cannot establish the behavior. Do not run worldgen/full CI by default; issue processing selects focused gates for changed inputs.

Expose read-only diagnostics: completed step/time, current owner/phase and its elapsed wall duration, batch service/yield cost, outstanding pacing debt, discarded unstarted demand, simulation-seconds per wall-second, input-to-feedback latency and gameplay-command latency separately, plus bounded memory/queue high-water summaries. Use bounded aggregates, not an unbounded per-step log. Fault diagnostics identify the last completed boundary and the incomplete phase. No new asset is required.

## Delivery plan

Phase A establishes dormant infrastructure; Phase B migrates owners under the isolated test runner; Phase C validates and activates the complete path. Each slice keeps production legacy timing active until GT-16.

### GT-1. Define the step protocol and pure clock accounting

- **Outcome:** A tested clock/phase model with explicit state placement, execution-time context, cadence and readiness contracts.
- **Scope:** Ordinals/epochs, fixed intervals, debt policy, phase graph and read/write/wait inventory; no running worker migration.
- **Phase:** A.
- **Depends on:** none.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3, D-10, D-11; proposals P-1 through P-7.
- **Acceptance signals:** Pure state-machine tests; no duplicate/partial completion; documented acyclic handoffs, safe presentation access and all state owners; parameterized cadence. Record the Q-2/Q-4 dispositions or the exact owner decision still needed before dependent implementation issues can proceed.
- **Out of scope:** Shipping parameter selection and live world updates.
- **Open questions:** Q-2/Q-4 are deliberately investigated here under the gate table; Q-3 values remain parameterized. Q-1's behavioral direction is resolved by D-7.

### GT-2. Add lifecycle and transaction coordination at completed-step boundaries

- **Outcome:** A boundary/control permit protocol that cannot deadlock on Lua save or park halfway through an admitted step.
- **Scope:** Pause intent versus settlement, owner control service, transaction entry, fault/shutdown modes and in-step blocking-call refusal. Existing production save path remains active.
- **Phase:** A.
- **Depends on:** GT-1.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-5; P-7.
- **Acceptance signals:** Controlled pause/save/load/failure schedules, including Lua holding a callback; existing save-owner park and failure contracts remain intact.
- **Out of scope:** New save DTOs or successful production activation.
- **Open questions:** Q-2.

### GT-3. Bound unit and combat command service for granted work

- **Outcome:** Unit/combat owner adapters can service finite accepted prefixes and acknowledge applied consequences without draining indefinitely.
- **Scope:** Separate ordinary/control commands from granted phase work, preserve FIFO/admission and settle same-step death/stop effects through explicit handoffs.
- **Phase:** A.
- **Depends on:** GT-1, GT-2.
- **Ordering:** critical path.
- **Relevant decisions:** D-2, D-4; P-3, P-7.
- **Acceptance signals:** Replenished queues, same-step consequences, next-step commands, duplicate/old-epoch grants and shutdown replies.
- **Out of scope:** Changing combat formulas or elapsed integration.
- **Open questions:** Q-2 phase assignment.

### GT-4. Separate world and fluid control work from timed advancement

- **Outcome:** World/fluid adapters keep preparation and authorized transaction service alive while gameplay waits for a grant.
- **Scope:** Finite command/publication prefixes, explicit fluid-to-world completion acknowledgements, and readiness admission with existing generation fences.
- **Phase:** A.
- **Depends on:** GT-1, GT-2.
- **Ordering:** independent of GT-3; required before GT-6.
- **Relevant decisions:** D-3, D-4; P-3, P-9.
- **Acceptance signals:** Delayed publications cannot acknowledge early; paused transaction/preparation work progresses; stale fluid results are rejected.
- **Out of scope:** Residency enforcement, async worldgen redesign, new fluid algorithms.
- **Open questions:** Q-2, Q-4.

### GT-5. Separate Lua gameplay callbacks from ordinary script scheduling

- **Outcome:** Explicit gameplay registration/context and a finite grant/control service lane on the single Lua owner.
- **Scope:** Keep ordinary scheduling/reentrancy and input settlement intact; contextual game-time APIs and lifecycle-call restrictions.
- **Phase:** A.
- **Depends on:** GT-1, GT-2.
- **Ordering:** independent of GT-3/GT-4; required before GT-6.
- **Relevant decisions:** D-1, D-2; P-1, P-7, P-8.
- **Acceptance signals:** Gameplay receives only grants; UI remains ordinary; replenished queues cannot starve grants; save reconciliation wins over normal work.
- **Out of scope:** Parallel Lua, callback preemption and physiology formula changes.
- **Open questions:** Q-2. Reconcile the actual implementation of #2415 before drafting; reuse its ordinary fairness helpers rather than duplicate them.

### GT-5A. Add resumable gameplay batches and responsive presentation service

- **Outcome:** A gameplay phase can yield to safe presentation/input work and resume exactly where it stopped, without publishing partial simulation or admitting another step.
- **Scope:** Captured-roster cursor/context protocol, bounded batch service, shared completed-read-view interface, presentation access restrictions, queued gameplay intents and pause-request feedback. Supply a small real roster fixture; domain migrations implement their domain-specific read publication and yielding through this interface later.
- **Phase:** A.
- **Depends on:** GT-1, GT-2, GT-5.
- **Ordering:** critical path before GT-6; independent of GT-3/GT-4 implementation.
- **Relevant decisions:** D-1, D-2, D-9, D-11; P-3, P-6, P-7, P-8.
- **Acceptance signals:** A delayed phase spans several input/presentation service turns without duplicate/skipped actors; reads use the completed view; UI actions cannot mutate suspended gameplay state; intents revalidate when admitted. Long atomic work is identified rather than hidden by a nominal batch-count limit.
- **Out of scope:** Arbitrary Lua preemption, a second Lua state, unrestricted console execution mid-phase, converting every gameplay family or rewriting existing UI panels in this infrastructure PR.
- **Open questions:** Q-2's access/suspension graph and Q-3's numerical feedback target. Split read-view/API coverage into a follow-up stable slice before issue readiness if its verified surface cannot fit this PR.

### GT-6. Assemble the coordinated runner behind an internal test mode

- **Outcome:** One-step-at-a-time coordinator drives the real owner adapters and exposes bounded diagnostics in an isolated headless backend.
- **Scope:** Startup/owner rendezvous, local unit stages, grant/ack orchestration and incomplete-step fault handling. Use minimal real domain fixtures.
- **Phase:** A.
- **Depends on:** GT-3, GT-4, GT-5, GT-5A.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2; P-2, P-3, P-6, P-7.
- **Acceptance signals:** Injected owner delays never advance the published clock early or create a second in-flight step; presentation/input is serviced while a phase remains incomplete as well as at complete boundaries.
- **Out of scope:** Selecting coordinated timing in ordinary gameplay or mutating a shadow live session.
- **Open questions:** Q-2; Q-3 shipping parameters remain open.

### GT-7. Move unit timing and motion publication onto granted steps

- **Outcome:** Movement, action/animation deadlines and the completed motion/time view use one granted interval.
- **Scope:** Explicit context through unit commands/helpers, removal of independent elapsed integration in the test backend, read/publish pairing for rendering.
- **Phase:** B.
- **Depends on:** GT-6.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-4; P-1, P-4, P-8.
- **Acceptance signals:** Equal-duration movement/deadline tests, pause/load publication, hazardous crossings and waypoint budget regression.
- **Out of scope:** Frame interpolation enhancements and unrelated path planning.
- **Open questions:** None beyond Q-2's resolved protocol. CH-5's movement repair is an external prerequisite if still unimplemented; link/reuse it during processing.

### GT-8. Advance combat wounds and deadlines through step context

- **Outcome:** Wounds cease counting combat-worker iterations; combat admission and wound age use the same interval context.
- **Scope:** Grant-driven continuous consequences, RNG ownership preservation, and unit consequence acknowledgements.
- **Phase:** B.
- **Depends on:** GT-6, GT-7.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-4; P-3, P-4.
- **Acceptance signals:** Fixed attacks/wounds under delayed combat service preserve integrated duration, admission and death/collapse ordering.
- **Out of scope:** Combat rebalance or deterministic random draws across arbitrary schedules.
- **Open questions:** None beyond resolved Q-2; integration cadence remains the test profile until GT-15.

### GT-9. Advance physiology through credited simulation intervals

- **Outcome:** Resource/physiology time credit no longer depends on ordinary Lua callback count.
- **Scope:** Resumable gameplay entry point using GT-5A's roster cursor, movement/activity interval inputs, integration-order review, and small-delta/threshold behavior at the candidate base cadence. Yield between consistent unit updates, never through one unit's dependent physiology chain.
- **Phase:** B.
- **Depends on:** GT-6, GT-7, GT-8.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-4; P-4, P-5.
- **Acceptance signals:** Travel/exertion/resource comparisons under delayed Lua service; stationary recovery, death thresholds and no lost tiny increments from cadence changes.
- **Out of scope:** New physiology mechanics or a generic stat API.
- **Open questions:** None beyond Q-3 cadence/cost. Integrate the stance/stamina authoritative-mutation repairs as external prerequisites, deduplicating their current tracker state before drafting.

### GT-10. Separate AI work accounting from decision frequency

- **Outcome:** Active job progress and eligible stall budgets use granted duration; periodic decisions retain their own cadence.
- **Scope:** Craft, designation construction, harvest, dig, chop, till/plant, suspension and hold/order paths. Classify timing scratch, use resumable decision/work batches, and integrate work/power intent interfaces. Keep actual gameplay mutation outside presentation-only service.
- **Phase:** B.
- **Depends on:** GT-6, GT-7, GT-9.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-3; P-3, P-5.
- **Acceptance signals:** No work during ineligibility; no time loss under a delayed worker; no retrospective work from a new decision; existing bounded-work/stall/hold semantics remain covered.
- **Out of scope:** Job selection rebalance, claims redesign, transfer policy changes.
- **Open questions:** Q-2 power quantum and Q-4 readiness. Split by action family before issue readiness if the verified diff exceeds a plausible single PR.

### GT-11. Separate building construction progress from spawn polling

- **Outcome:** Continuous building progress uses granted duration while roster sequencing remains a periodic check of session deadlines.
- **Scope:** `building_spawn` entry-point split, worker contribution timing and saved spawn deadline semantics.
- **Phase:** B.
- **Depends on:** GT-6, GT-9.
- **Ordering:** independent of GT-10; both required before GT-14.
- **Relevant decisions:** D-1, D-3; P-4, P-5.
- **Acceptance signals:** Equal worker-seconds under delayed Lua, no spawn before the saved deadline, no duplicate roster entries.
- **Out of scope:** New spawn behavior or construction costs.
- **Open questions:** None beyond Q-3 polling cadence.

### GT-12. Advance calendar and world processes from the shared clock

- **Outcome:** Precise calendar, flora regrowth, tracked-item temperature and power share granted world duration; productive work is paired with power accounting.
- **Scope:** Page conversion, view-independent prepared-page iteration and the work-demand/admission handoff defined under Q-2.
- **Phase:** B.
- **Depends on:** GT-6, GT-10.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-3; P-3, P-5, P-9.
- **Acceptance signals:** Calendar partition/rollover, paired work/energy, paused stability and two prepared pages unaffected by view changes.
- **Out of scope:** Cache retention, calendar UI or changing worldgen output.
- **Open questions:** Q-1, Q-2. CH-4's precise-calendar repair is an external prerequisite; retain its schema rather than creating another remainder representation.

### GT-13. Schedule and acknowledge fluid advancement by simulation step

- **Outcome:** Active fluid iterations are issued at simulation cadence and their accepted writebacks finish before step completion.
- **Scope:** Cadence cursor, generation-aware commit/refusal, solver/preparation distinction and activity pause behavior.
- **Phase:** B.
- **Depends on:** GT-6, GT-12.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-3, D-4; P-3, P-4, P-9.
- **Acceptance signals:** Equal scheduled iterations under delayed service, conservation, stale writeback rejection and no forced future-time settlement on pause/save.
- **Out of scope:** New fluid model, hibernation, reservation implementation.
- **Open questions:** Q-3 discrete cadence; Q-4 currently reachable readiness outcomes.

### GT-14. Persist clock continuity and complete save/load integration

- **Outcome:** Coordinated sessions save and restore completed time, cadence phase and existing deadline meaning through the real transaction path.
- **Scope:** Required component evolution, frozen DTO/migration/fixtures, inventories, cadence interpretation and epoch/pacing reset at publication.
- **Phase:** C.
- **Depends on:** GT-7, GT-8, GT-9, GT-10, GT-11, GT-12, GT-13.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-3, D-5; P-7, P-10.
- **Acceptance signals:** Fresh-process round trips at several phases, unchanged failed loads, reconciliation-failure disposition, no stale acknowledgements or periodic-work postponement by repeated saves.
- **Out of scope:** New save storage protocol or automatic recovery of partial steps.
- **Open questions:** Q-1/Q-2 resolved before implementation. Q-3's cadence metadata is explicit and versioned even while the shipping profile is still under measurement.

### GT-15. Verify cross-system consistency and select the shipping cadence

- **Outcome:** The complete coordinated backend satisfies cross-domain correctness and the agreed interaction target; simulation throughput and memory are measured honestly across ordinary and overload cases.
- **Scope:** Controlled completion delays, finite queue pressure, conservation/workload comparisons, phase/batch/control-service measurements, memory headroom on the four-core/8-GB class, and a recorded parameter decision for 5–50 controlled units across the provisional 10 × 10-chunk footprint. Label absent colony systems and revisit the estimate when playable colonies supply evidence.
- **Phase:** C.
- **Depends on:** GT-14.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3, D-7, D-8, D-9; P-4, P-6, P-8 and the verification strategy.
- **Acceptance signals:** Required scenarios pass; debt/storage remain bounded; no skipped-owner shortcut; input/presentation meets Q-3's predeclared target even under admitted simulation slowdown. Report ordinary/stress simulation speed and future-scope limitations separately.
- **Out of scope:** Broad unrelated optimization or redefining the target to match a failing result.
- **Open questions:** Q-3 explicitly resolved by measurement plus owner decision where needed. Reopen the design for material phase/cadence changes.

### GT-16. Activate coordinated timing and retire independent gameplay clocks

- **Outcome:** One production timing path with all migrated owners enabled together and updated engine contracts.
- **Scope:** Startup activation, removal of legacy advances/test selection, time-writer audit, focused production-path smoke and rollout documentation.
- **Phase:** C.
- **Depends on:** GT-15.
- **Ordering:** critical path.
- **Relevant decisions:** D-1 through D-5; P-10.
- **Acceptance signals:** No production gameplay owner samples wall elapsed or counts worker loops for time credit; ordinary/UI timers remain independent; save compatibility and completed-step diagnostics work in the normal boot path.
- **Out of scope:** Shipping expedition UI or declaring the residency epic complete.
- **Open questions:** None. Q-1 through Q-4 must be resolved or have an explicit implemented disposition for every currently supported path.

## Handoff and next discussion

The design is ready for issue processing under the owner's 2026-09-05 instruction to mark it ready if it passes review and publish it. D-6 approves the architectural baseline; D-7 through D-11 settle fast-forward, the tentative 100-chunk footprint and hardware class, interaction priority, phased-unit isolation and script-driven progression. The remaining Q-2/Q-3/Q-4 choices have explicit resolving slices and stop conditions in the gate table. Do not ask the owner to restate the footprint or isolation decision; the suggested 100-ms target remains a proposal. There are 17 delivery slices, including GT-5A.

Readiness assessment: the epic has observable outcomes, explicit non-goals, dependency-ordered PR slices, persistence/migration requirements and focused validation. The protocol/access investigation is a bounded first delivery, not an unstated prerequisite to every possible tracker artifact. Implementation-specific policy choices remain gated rather than presumed. Detailed future throwing mechanics do not block this timing design unless an equivalent exceptional path is already supported and lacks a defined disposition. Readiness does not certify production activation or the one-PR size of an unforeseen scope expansion.

`process-design-doc` can process EPIC first and then one child per invocation using the stable GT identifiers, with separate approval for each tracker artifact. It must enforce the gate table before drafting a dependent child as implementation-ready. Refresh source/tracker evidence, match external bug/resource prerequisites to their current artifacts, and reconcile the older #1997 tracker text with its newer residency design before relying on it. Material behavior or delivery-scope changes return the affected design to exploration; routine issue links and processing annotations do not.

This document is the timing-architecture continuation point. The report remains evidence; unresolved calendar/waypoint findings and the residency design remain separate records. No report status has been changed by drafting this design.
