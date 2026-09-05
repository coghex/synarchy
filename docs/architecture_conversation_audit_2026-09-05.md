# Architecture conversation, first pass — 2026-09-05

This is a human-readable architecture review and continuation record, requested by the owner. It is deliberately separate from the findings-report processing workflow. Recommendations here are discussion candidates, not approved epics or implementation instructions.

Reviewed source revision: `da96202c863b7d563f4968d34cb685d2e622e73c`. One agent, inline investigation; no subagents, production edits, engine launches, full CI, or performance measurements. Repository documentation and live GitHub issues were checked for prior work. The only executed behavioral reproduction was the controlled Lua interleaving linked below.

## My first assessment

The project has many useful boundaries already: pure world-generation and gameplay calculations, a real save transaction, compiled unit artwork, page-qualified entity access, and substantial executable checks. I would preserve those investments. The Haskell/Lua combination is not itself the problem I found.

The most consequential weakness in this pass is that a single simulated character is governed by several independent schedules and mutation paths. Each subsystem can obey its local contract while their combination produces inconsistent gameplay. As the game adds more coupled mechanics, local fixes alone become increasingly expensive.

The recommendation is a bounded simulation-consistency effort: establish what advances together, under which clock, and who commits changes to shared resources. This does not require rewriting the game or moving all Lua into Haskell.

## How the project fits together

The application boots the runtime and selects graphical, offscreen, headless, dump, preview, or language-report behavior. The main thread owns graphics; separate workers handle input, world work, unit movement/building commands, combat/wounds, active fluids, and Lua. Queues carry requests, while shared references expose managers and published state. Lua runs both gameplay orchestration and UI callbacks on one scripting worker.

World generation builds the geological and climatic foundation. Detailed chunks are materialized for local use; terrain edits and other gameplay changes must survive regeneration and saves. World rendering builds cached terrain and dynamic quads, and the main thread assembles and submits rendering work. The existing residency and world-map designs address important capacity questions; they should not be rediscovered as new audit findings.

Units span a movement-state manager, the shared unit-instance manager, Lua physiology and AI, and combat/wound processing. Crafting, construction, transfers, and equipment connect these owners to items, buildings, and page-local state. YAML defines content, but much of the behavior and coordination lives in Lua and its Haskell API implementations.

Persistence captures a coordinated session, encodes independently versioned components, stages a replacement session, and publishes it while owners are quiescent. This is a valuable transaction boundary. Ordinary gameplay has a more fragmented coordination model.

For scale only: the checkout contains 835 Haskell files under `src/` (188,360 lines), 227 Lua files under `scripts/` (80,656 lines), 417 headless Haskell files (179,536 lines), and 487 Python files under `tools/` (229,823 lines). Counts include comments and fixtures and are not quality judgments. Import sampling shows substantial two-way dependencies between Engine and game domains. That makes this one integrated game/runtime in practice; it is not evidence that extracting a reusable engine should be a priority.

## 1. Workload can change the relative speed of game systems

Movement advances by measured elapsed time. Lua physiology receives the script's configured interval even when its callback runs late. Wounds advance by a fixed amount every sixth combat-worker iteration. The calendar measures elapsed time on the world worker, which also does chunk loading and scene preparation.

For example, suppose movement keeps up but a physiology callback configured for 0.1 seconds runs only five times in one second. The physiology code accounts for 0.5 seconds while movement can account for the full second. A moving unit can therefore cover distance while paying less than the intended resource cost. This is the consequence of the current arithmetic, not a measured claim that normal colonies already run at that rate.

This matters before further balancing: changing UI cost, colony size, or worker load can alter resource depletion relative to movement and combat. Optimizing a subsystem can change gameplay rates as well as responsiveness.

Evidence:

- `src/Unit/Thread.hs:107`: `unitTickWith` samples elapsed time and uses it for both game time and movement.
- `src/Engine/Scripting/Lua/Thread.hs:494`: `runDueScripts` passes `scriptTickRate` to `update`, rather than elapsed simulation time.
- `src/Engine/Scripting/Lua/TickPolicy.hs:189`: a sufficiently overdue script advances its deadline to `now + interval`; it does not replay every missed tick.
- `scripts/unit_resources.lua:77`, `scripts/unit_resource_tick.lua:146`: physiology consumes the callback's `dt` and applies resource rates over it.
- `src/Combat/Thread.hs:109`: wound progression uses six loop iterations times the configured combat interval, followed by a post-work sleep.
- `src/World/Thread.hs:74`, `src/World/Thread/Time.hs:21`: the world has its own elapsed-time sample and page clock advancement.
- `src/Engine/Core/Clock.hs`: the shared monotonic source and 0.25-second cap are already implemented. That prevents invalid elapsed steps; it does not make independent consumers account for the same simulation time.

The direction I recommend is a simulation timeline shared by coupled gameplay systems, with explicit conversion to calendar time. UI animation and interaction can keep independent presentation timing. Different update frequencies are fine; each gameplay consumer should know how much simulation time it is advancing. Decide explicitly what happens under overload and across suspension: bounded catch-up, coherent slow motion, or a shared dropped interval. Avoid sending an arbitrarily large delta into every existing formula.

First useful validation: run the same movement/resource scenario under different Lua callback frequencies and combat-worker delays. Compare resource cost per distance and wound progression per simulation second. Exact deterministic replay is not a prerequisite; control the randomness relevant to the experiment.

Existing work: #2204 fixed monotonic sampling and bounded elapsed steps; #2205 fixed callback rescheduling. Neither changes the configured-interval `dt` contract. Revising that contract requires deliberate migration of gameplay callbacks, not a one-line scheduler substitution.

## 2. Looking at another page changes which parts of life continue

There are several distinct meanings of “active” in the current engine:

| System | Current selection rule |
| --- | --- |
| Lua AI and physiology | Units on the resolved active page |
| Movement | All entries in unit simulation state, with terrain from each unit's owning page |
| Wounds, bleeding, infection, healing | The unit-manager roster, with page-owned climate |
| Calendar, power, tracked item temperatures, harvest regrowth | Visible pages |
| Location discovery and craft-claim reconciliation | Loaded pages, with their documented pause/load rules |
| Active fluid work | Sim-world activation state, changed by world show/hide commands |

Here “active page” is generally the first visible page, with a fallback to the first registered page when none is visible. A lone hidden page is therefore not an adequate test of background behavior. Use two pages, and distinguish changing page selection from merely panning the camera within one page.

A wounded unit left on another loaded page can keep undergoing wound processing while its ordinary AI and physiology callbacks stop. An existing movement order can continue without the normal Lua resource and decision updates. These are consequences of the selected rosters in the source; this pass did not run a two-page gameplay experiment.

This is a product decision embedded in infrastructure. Decide what should happen at home during an expedition: continued detailed simulation, complete suspension, or an explicitly defined reduced simulation. Any can be valid. The current partial suspension needs to become one deliberate rule, because survival, work, power, and time depend on each other.

I would separate viewing a page from deciding its simulation eligibility. Every coupled system should consume the latter decision. A fully suspended page should suspend its action timers and resource spending together; a simulated page should have the terrain residency it needs even when it is not being viewed. A reduced simulation is additional design work, not an automatic shortcut.

Evidence: `src/Engine/Scripting/Lua/API/Units/List.hs:45`, `scripts/unit_ai.lua:461`, `scripts/unit_resources.lua:77`, `src/Unit/Thread/Movement.hs:77`, `src/Combat/Wounds/Tick.hs:77`, `src/World/Thread/Time.hs:21`, `src/World/Thread/Command/UI.hs`, `src/World/Thread/ChunkLoading.hs:59`.

Existing work: HPA-51 and closed #2332 identified and bounded inappropriate craft/construct progress across interruptions. #2332 explicitly excludes a background-simulation policy. I am not reporting that old progress burst as an unfixed new bug. #1997's residency design must be coordinated with this decision; memory residency and simulation eligibility are related but distinct. The pending page-incarnation design concerns identity/lifecycle, not this policy.

First useful validation: a two-page scenario with one moving unit, one wounded unit, and one powered job. Switch the viewed page while holding the chosen simulation policy constant. Assert that the selected rules apply to the whole scenario.

## 3. Shared resources need operations that preserve other systems' work

Several systems can mutate the same unit record. Individual Haskell updates are often atomic, which protects a single modification. Lua frequently performs a longer operation: read a stat, calculate a new value, then overwrite the stat. Another worker can change that stat between the read and the write.

I reproduced this with the shipped stance-recovery function:

1. Recovery reads stance `0.600`.
2. A quick-strike charge reduces live stance by `0.250`, to `0.350`.
3. Recovery computes `+0.059` from its old value and writes `0.659`.
4. Preserving both changes would yield `0.409`. The entire attack cost has disappeared.

The reproduction runs the actual Lua function with an injected engine-API interleaving. It proves the overwrite mechanism; it does not measure the frequency of that interleaving in a running game. See the [canonical bug record](simulation_consistency_findings.md) and [reproducer](audit_evidence/2026-09-05/stance_interleaving.lua).

Recent combat work (#2328) correctly rechecks admission and commits strike costs atomically. That transaction cannot stop a later writer from replacing its result with a stale calculation. This is why “both callers use atomic writes” does not settle ownership of the whole gameplay operation.

The immediate repair can be bounded: recovery should apply a clamped adjustment against current state at the owning mutation boundary. The broader direction is to classify shared resource writers and expose meaningful operations such as recover, spend, and apply damage. Keep raw setters for explicit initialization/debug uses where appropriate. Physiology calculations that depend on several changing inputs need a coherent snapshot-and-commit or an owner-controlled step; adding an atomic increment everywhere is not sufficient.

Start with stance and stamina, where there is a concrete collision. Do not migrate every manager merely for consistency of style. The existing #1890 capability arc and ContentRegistries read-only pilot address access control; a permitted writer can still perform a stale update, so this proposal is related but not identical.

## What I would do next

Discuss the background-page behavior first, because that determines what the timeline and residency work must support. Then design a bounded first implementation around movement, stance/stamina recovery, and wound processing. Preserve current rates under an unloaded scenario, expose divergence under delayed scheduling, and expand only after that slice demonstrates a coherent rule.

The stance bug can be fixed independently. No critical crash or data-loss bug was newly confirmed in this pass. The canonical bug record is medium severity and is retained because it is concrete evidence for the ownership concern, not because the entire project needs to stop for it.

## Continuation record

This was a broad map followed by focused examination. It is not an exhaustive audit of 835 production modules.

| Area | Coverage this pass | Still to examine |
| --- | --- | --- |
| Runtime topology and core state | Worker loops, clocks, queues at call sites, capability shapes, shared managers | Full shutdown/error paths; queue budgets and measured contention |
| Unit simulation and combat | Movement/resource/wound timing and scope; stance writer paths; current strike commit | Stat derivation ownership, all cross-writer fields, targeting/pathfinding architecture, AI cancellation/reservations |
| Pages and streaming | Visibility versus simulation rosters; camera chunk-demand entry point; existing designs | Controlled two-page run, actual residency limits, authoritative edits on nonresident terrain, page lifecycle end to end |
| Save/load | Component registry, publication boundary, DTO/enum constraints | Migration isolation, content changes across versions, practical save/load cost, failure recovery review |
| World generation | Namespace and orchestration map; chunk facade; existing hydrology/streaming context | Geological/hydrological algorithms, climate feedback, numerical stability, recomputation dependencies |
| Rendering and assets | World scene construction, assembly entry points, asset lifecycle interfaces and atlas contracts | GPU lifetime/fences, frame consistency, culling/scaling, texture residency; no visual or GPU validation |
| UI and Lua interfaces | Boot/load topology, callback scheduler, API samples, UI contracts | Widget lifecycle, rebuild cost, input/focus behavior, gameplay/presentation separation beyond scheduling |
| Items/economy/construction | Craft station admission and inventory commit sampled; transfer policy references | Whole transaction/claim model, economy/content extensibility, nested inventory and equipment invariants |
| Validation and workflow | Cabal/CI structure, repository size/import sampling, existing audit and issue overlap | Test value versus maintenance cost; realistic vertical scenarios; no full test suite run |
| Language, naming, audio, tutorials, art quality | Inventory/context only | Unaudited |

Resume by reading this document and checking its cited source files against the current revision. The highest-value follow-up is the two-page scenario and inventory of time/resource owners; then inspect AI job/claim lifecycle and cross-manager transactions. Keep world-generation numerical review and GPU correctness as separate later passes. Do not repeat the historical HPA/EA findings without verifying whether their fixes have landed.

All new artifacts are uncommitted in the branch-resolved docs worktree. Existing worktree changes were left alone; no tracker issue was created and no documentation was published.

## Owner direction and next component — follow-up, 2026-09-05

This section supersedes the earlier suggestion to resolve background-page behavior first.

The owner confirmed that the home colony must continue simulating during expeditions. Gameplay-critical chunks should remain resident while unneeded chunks can be evicted; keeping an entire giant world resident is not the objective. Multi-world/page switching is known unfinished work and is not the immediate repair priority.

The owner also confirmed that exact determinism is unnecessary. Small ordering differences are acceptable; movement and other gameplay must not acquire different rates under different workloads. The owner favors game ticks for gameplay and wants the timing disagreement corrected in a later bounded effort.

For shared resources, the owner proposed sending relative adjustments to the engine so the operation applies against its current value, and asked to discuss solutions and choose the quickest component first. The selected next discussion is **stance recovery**, followed by stamina once the operation's semantics have been demonstrated. This follow-up specifies a recommended first repair; the public API name is illustrative and production code is unchanged.

Technical recommendation for that first component:

- Send the intended recovery amount (`rate * dt`), not an absolute replacement and not a difference derived from a stale, already-clamped target.
- Read current stance, add the recovery, clamp to `[0, 1]`, and publish inside one existing `atomicModifyIORef'` operation on the unit manager. No new owner thread, queue, or EngineEnv field is needed for this narrow change.
- Prefer a small stance-specific entry point for the pilot (conceptually `unit.recoverStance(uid, amount)`) backed by a reusable pure adjustment helper. A general stat API needs an explicit choice of stored/base versus effective values and maximum ownership: current `getStat` includes modifiers while `setStat` writes the base. Do not silently generalize that mismatch to every resource.
- Preserve the absent-stance convention (implicitly full) and missing-unit behavior; reject invalid/non-finite adjustment inputs. Return the committed result if callers need it.
- Recovery amounts may use slightly older dexterity/agility readings under the owner's accepted ordering tolerance. The accumulated resource value itself must be read at commit. Operations requiring exact admission, such as spending only when enough remains, must check and spend in the same commit.
- Verify recovery before and after a debit, a debit between Lua calculation and commit, saturation at full stance, implicit-full stance, and a vanished unit. The existing 0.600/0.250/0.059 case must preserve both changes and yield 0.409. Near a cap, different serial orderings may legitimately yield different values.

Alternatives considered: version-check-and-retry for calculations that require a consistent set of inputs, and a single owner applying queued resource operations. Both can be useful later; neither is necessary for an additive stance recovery. Never retry a whole Lua callback that may already have emitted other effects.

Notes retained for later design: chunk retention should follow explicit live gameplay demand, not biome labels; residency protects terrain dependencies but does not by itself decide simulation membership. Fixed gameplay steps still need a real-time scheduler and a bounded catch-up/overload policy. An overloaded simulation cannot guarantee unchanged wall-clock speed indefinitely; the important requirement is that coupled gameplay slows coherently rather than diverging by subsystem. Exact replay or globally deterministic script order is not required.

## Drafting progress — 2026-09-05

The owner has now requested three bounded repair drafts: [stance recovery](stance_recovery_design.md), [stamina commits and exhaustion](stamina_update_issue_draft.md), and [Lua scheduler fairness](lua_scheduler_fairness_issue_draft.md). Following explicit owner approval, Lua scheduler fairness was filed as [#2415](https://github.com/coghex/synarchy/issues/2415); CH-3 is marked handed off in the canonical report. Stance and stamina remain local drafts, with CH-1 and CH-2 undispositioned. Nothing has been implemented by this audit, and these local documents have not been published.

The fairness draft proposes ordinary batches of 32 engine messages and 8 console dequeue attempts, with timer service independent of an empty queue. These are initial policy choices, not measured performance optima. Preserve synchronous input settlement's exhaustive drain and the pending-load reconciliation boundary when implementing it. CH-3 is established by source inspection; deterministic production-path regression tests remain implementation work.

Resume after these drafts with the shared gameplay-clock design and its overload policy, informed by an inventory of time owners and the two-page scenario above. Background simulation and residency remain a larger coordinated design. The coverage table still records the areas not audited deeply; AI job/claim lifecycle, cross-manager transactions, world-generation numerical correctness, and GPU correctness have not received a full pass here.

## Gameplay timing follow-up — 2026-09-05

The owner requested a comprehensive timing check and report, authorizing bounded Sol/Terra/Luna assistance. Two bounded reviews (Sol for Lua, Terra for engine gameplay) supported the [gameplay timing audit](gameplay_timing_audit_2026-09-05.md), verified at `2922bb476be795c9fd3d33eb65962b7eccca39ed`. It inventories elapsed session time, nominal Lua allowances, AI work/deadline policies, combat iteration timing, fluid iteration timing, calendar time and scaled world-process durations, including persistence and pause/load constraints.

Direct production-function executions confirmed two additional bugs: fractional minutes disappear on every calendar update (default-speed calendar advancement is impossible), and waypoint arrival discards unused movement duration. They are recorded as CH-4 and CH-5 in the canonical report, with retained inputs/results. The timing report distinguishes these executions from source-derived wound/fluid workload examples and does not claim a live saturation benchmark.

Next discussion: calendar accumulation first, waypoint time consumption as another bounded repair, then the shared gameplay-step design and migration plan. Resolve gameplay fast-forward versus calendar-rate semantics and step completion/overload accounting before selecting Hz. Background page eligibility/chunk retention remains separate coordinated work. Deep AI job/claim lifecycle, cross-manager transaction, worldgen numerical and GPU correctness audits remain unfinished; the earlier broad coverage table is historical and this follow-up supersedes its timing coverage.
