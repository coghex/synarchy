# Coordinated simulation step protocol

GT-1, issue #2482, under epic #2478. Investigative protocol and dormant pure
model; production workers still use their existing clocks. Source evidence
below is pinned to base commit `7680c9ef6e73822c4e9bca1fa2016b90cf0f82b9`.
Paths and line numbers refer to that commit, not to future adapters.

**Owner review pending.** The unit-worker coordinator and one in-flight step
are already accepted in the parent design. This document proposes their
concrete handoffs. Acceptance of this graph is a separate gate before GT-2
through GT-6; it does not select shipping rates or the power policy. The
Q-4 classification is settled: deliberate combat leaps wait for prepared
terrain before launch, as confirmed by the owner on 2026-09-07. No new runtime field, capability or save codec is added.

## Model and numeric domain

`Engine.Core.StepProtocol` is pure and has no production consumer. The public
constructor validates positive runtime epoch and rational base rate,
nonnegative rational origin, a debt cap large enough for one interval, and a
nonempty ordered plan containing all six semantic stages. Rational inputs
exclude NaN and infinity. The wall sampler accepts `Double`, uses the existing
`Engine.Core.Clock.sanitiseElapsed`, and retains each raw sample independently
of whether it admitted time. It does not change that existing contract.

For completed ordinal `n`, committed seconds are exactly `origin + n / rate`.
An adapter may round once when publishing a `Double`; tests compare exact
rationals, including a nonzero origin and rate 3, rather than treating rounded
public seconds as an exact oracle. Ordinals and epochs are unbounded integers.

A grant carries epoch, ordinal, phase position, stage, owner, start, end and
duration. Every accept, batch, yield, resume and acknowledgement validates the
complete grant. Only the enabled phase may accept; acceptance itself prevents
duplicate execution. Acknowledgement requires the captured membership to be
fully processed. Only the last acknowledgement publishes completed time.

The model represents roster work as ordered member IDs and an applied-member
ledger. This proves protocol exact-once behavior, not domain mutation or
snapshot correctness. Empty rosters still traverse and acknowledge each phase.
Suspension retains membership, accepted command prefix, interval and next
member cursor. The model cannot begin another step or apply a batch while
suspended. A fault retains the failed phase for diagnosis and prevents ordinary
resume, work and transaction admission; explicit replacement is the escape.

Pacing caps and counts discarded **unstarted** demand separately from admitted
work. A pause immediately closes admission and clears pacing baseline/debt,
but does not revoke the current grant. It becomes settled after completion.
Paused wall samples create no replay debt; resume rebases its first sample.
Transaction mode can start only at a completed boundary and finishes paused.
These are pure transitions, not an implementation of the GT-2 save permit.

Replacement requires a strictly newer runtime epoch, installs the supplied
origin, and clears the in-flight step, prefixes, wall baseline, demand and
diagnostics. `Scoped` values demonstrate rejection of old command prefixes and
cadence cursors. Saved cadence progress may be rewrapped under the fresh epoch;
resetting runtime identity does not discard durable progress. Actual cadence
serialization, phase restoration and component migration remain GT-14.

## Concrete proposed phase graph

The unit worker owns wall pacing, permit admission and completion accounting.
The single Lua owner runs a resumable gameplay driver. Before each native
phase, that driver requests the named phase under the current permit and
**returns to its event loop**. It never synchronously waits for a unit callback.
The coordinator validates the request, executes unit-owned work locally or
grants another owner its work, collects completion, then resumes the driver.
Lua cannot authorize a different step, choose elapsed time, or publish time.
The coordinator does not block its own unit command service awaiting itself.

These are future entry points; no such driver or transport exists in GT-1.
The request/return/acknowledge rendezvous is the proposed D-11 arrangement for
owner review, preserving the accepted clock and worker ownership.

| Order | Stage | Execution owner | Work and mandatory successor handoff |
| --- | --- | --- | --- |
| A1 | Admission | Lua | Capture the finite external-intent prefix and driver context; return native admission requests. Lifecycle requests instead take the boundary lane. |
| A2 | Admission | Unit | Apply admitted unit/building commands and fix participant/activity facts. |
| A3 | Admission | World | Apply admitted world commands and ready preparation publications; fix world membership and hand ready sim seeds forward. |
| A4 | Admission | Fluid | Admit ready seeds/edits into sim state, without crediting a fluid iteration. |
| M1 | Motion | Unit | Advance movement, transitions and landing effects for the interval; preserve exposure facts and collect direct consequences. |
| M2 | Motion | Lua | Continue admitted direct-action episodes using the interval context; emit their finite combat prefix. Utility selection remains D1. |
| M3 | Motion | Combat | Resolve that prefix with authoritative admission checks; emit finite death/stop consequences. |
| M4 | Motion | Unit | Apply direct-combat/landing consequences before continuous eligibility is read. |
| C1 | Continuous consequences | Combat | Integrate wounds once for the interval, reading each unit's own page climate. |
| C2 | Continuous consequences | Unit | Settle wound death/collapse consequences before physiology inspects pose. |
| C3 | Continuous consequences | Lua | Integrate physiology in its internal dependency order; emit death/collapse/revive/stop consequences. |
| C4 | Continuous consequences | Unit | Settle those consequences before productive eligibility is read. Death has precedence over collapse. |
| P1 | Productive work/environment | Lua | Capture eligible work intent, elapsed exposure and per-recipe demand; do not credit work yet. |
| P2 | Productive work/environment | World | Compute power admission and interval energy against current nodes/topology; return explicit authorized work duration. |
| P3 | Productive work/environment | Lua | Credit only authorized productive work, construction and eligible work-stall time; emit finite world/unit/building completion requests. |
| P4 | Productive work/environment | World | Commit world-owned work results, advance calendar/regrowth/item temperature, and emit resulting sim edits. Native Lua-called inline mutations stay in P3 and its declared state inventory. |
| P5 | Productive work/environment | Unit | Commit finite unit/building completion consequences. No second movement interval is granted. |
| P6 | Productive work/environment | Fluid | Apply received terrain edits and run a due simulation iteration, emitting the finite writeback set. A not-due opportunity explicitly completes with no iteration. |
| P7 | Productive work/environment | World | Apply or explicitly refuse every P6 writeback before acknowledging. Resulting sim reseeds target the next step. |
| D1 | Decisions/events | Lua | Run due AI utility decisions, encounter decisions and spawn polling once. New movement, attacks, roster spawns and world mutations target next admission. No second continuous interval is credited. |
| F1 | Completion | World | Finish this step's permitted awareness/housekeeping and publish completed world read views; queued effects toward already-completed owners target next admission. |
| F2 | Completion | Unit | Publish matching completed motion/time and the consolidated completion; offer boundary/control service before another step. |

**Acyclic argument:** each current-step edge goes to a strictly larger order
index in this table. Repeated Unit/World/Lua stages are distinct finite phases,
not recursive queue drains. Each native request adds a Lua-driver rendezvous
before its phase and a return after it; the driver suspends between the two,
so that rendezvous is not a blocking reverse edge. Writes directed back to an
earlier phase become a new command for the next step. No owner drains until a
cross-system cycle happens to become quiet. Captured prefixes and member
lists bound each phase; a callback too large to yield must be split by its
domain adapter before the responsiveness gate can pass.

P2 uses interval-start calendar/solar facts for power, while P4 publishes the
advanced calendar. This is an explicit causal ordering proposal, not a claim
that the current interleaved world/Lua ticks already provide this alignment.

## Current effects and their proposed handoffs

| Current source evidence | Proposed classification and required settlement |
| --- | --- |
| `src/Unit/Thread.hs:133`, `src/Unit/Thread/Movement/PathAdvance.hs:124`: movement and transition expiry | M1 interval work; landing consequences must settle by M4. Preserve the consumed interval's motion/exertion facts for C/P. |
| `src/Combat/Thread.hs:134`, `src/Combat/Wounds/Tick.hs:73`: wound cadence and queued pose consequences | C1 → C2, current-step forward handoff. C3 reads committed survival/pose state. |
| `scripts/unit_resources.lua:96`, `scripts/unit_resource_injury.lua:101`, `scripts/unit_resource_tick.lua:181`: ordered physiology and kill/collapse | C3 → C4, current-step handoff. Do not credit P1 work for a unit whose death is merely queued. |
| `scripts/unit_ai_combat_lunge.lua:245`, `src/Combat/Thread.hs:155`: attacks and combat drain | M2 → M3 → M4 for an already-admitted action. A newly selected D1 action targets the next step. Preserve native combat's commit-time revalidation. |
| `scripts/unit_ai_craft.lua:377`, `scripts/unit_ai_craft.lua:386`, `src/World/Thread/Power.hs:51`: independent powered query, work credit, battery integration | P1 → P2 → P3 → P4/P5. Demand and awarded work duration must describe the same interval. No Lua query of instantaneous power can substitute for the P2 allowance. |
| `scripts/building_spawn.lua:639`, `scripts/building_spawn.lua:660`: construction and roster polling share one callback | Continuous construction is P3/P5; due polling is D1, with new spawns admitted next step. Preserve saved roster deadlines. |
| `src/Sim/Thread.hs:127`, `src/World/Command/Types.hs:88`: runtime writebacks are currently fire-and-forget | P6 → P7, current-step acknowledged apply/refuse. Existing edit-generation checks remain; a completed refusal is distinct from a worker failure. |
| `scripts/unit_ai.lua:249`, `scripts/unit_ai.lua:461`: eligibility, decisions and execute share one roster traversal | Split admitted episode integration into M2/P1/P3, utility decisions into D1, and finite result handoffs into their declared successor. Do not run the old combined callback twice. |
| `src/World/Thread/Time.hs:53`, `src/World/Thread/Time.hs:120`, `src/World/Thread/Time.hs:136`: calendar, discovery/bill housekeeping and solar publication | Calendar is P4; read-view publication/awareness is F1. Discovery currently works while paused: retain equivalent boundary reconciliation, not arbitrary live-state mutation during a suspended phase. |

Effects emitted at F1/D1 toward a prior owner are retained as next-step
commands with fresh admission checks. This includes spawn-induced discoveries
and sim reseeds caused by a writeback. Boundary save drains retain their own
multi-pass rules; they are not a license for gameplay phases to drain globally.

## Synchronous waits and lane classification

| Existing edge and evidence | Lane and cycle-prevention disposition |
| --- | --- |
| Lua → save owners: `src/Engine/Scripting/Lua/API/Save.hs:401` calls `waitForOwners` after Lua self-ack | Boundary/transaction only. Acquire a completed step before entering the existing barrier. Calling this during a Lua phase would deadlock with the unit coordinator awaiting Lua completion. |
| Lua → load-publication owners: `src/Engine/Scripting/Lua/Thread/Dispatch.hs:522` | Boundary/transaction only, including conditional input and required render participation. Staging notifications cannot interrupt a gameplay phase with the blocking publication driver. |
| Unit/building, world, combat and fluid owner parking: `src/Unit/Thread.hs:125`, `src/World/Thread.hs:91`, `src/Combat/Thread.hs:123`, `src/Sim/Thread.hs:94` | Per-owner save gates after completed-step admission. Parking gates work rather than blocking the shared unit/building acknowledgements. Authorized world save/load commands still run. Never park one owner while another owes an admitted phase. |
| Fluid → world fast-settle replies: `src/Sim/Thread.hs:327` | Existing explicit dump/control operation with a shared deadline. Keep world reply service available; never make the world owner await fluid completion while withholding those replies. Ordinary P6/P7 uses yielded handoff and later world acknowledgement, not this blocking control loop. |
| Lua → world readiness polls: `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:804`, `src/Engine/Scripting/Lua/API/WorldQuery/Chunk.hs:167` | Boundary/control only when called inline; they block Lua by polling. Exact console forms are intercepted outside Lua (`src/Engine/Scripting/Lua/Thread/Console.hs:98`). Prepared gameplay admission must not call these waits. |
| Ordinary world commands and result notifications: `src/World/Command/Types.hs:98`, `src/World/Thread.hs:164` | Queue service and Lua notifications, not a generic blocking world-command RPC. Replies required for current-step work become explicit forward acknowledgements. Snapshot queries currently read shared state directly; adapting them is part of the read-view inventory below. |
| Debug client → Lua response: `src/Engine/Scripting/Lua/DebugServer/Client.hs:173` | Boundary/control. Unrestricted debug Lua never runs during a suspended phase. Preserve accepted-command cancellation and response semantics. Client waiting does not authorize a second Lua executor. |
| Lua → input barrier → inline Lua drain: `src/Engine/Scripting/Lua/API/InputInject.hs:147` | Boundary/control. Input settlement waits for the native input worker and then drains Lua messages inline; it must not reenter suspended gameplay or reorder release fences. Native input capture stays independent. |
| Lua → render screenshot reply: `src/Engine/Scripting/Lua/API/Screenshot.hs:75` | Boundary/control; rendering remains available to answer. Screenshots and synchronous debug tools are not safe arbitrary presentation callbacks. |
| Worker/listener joins: `src/Engine/Core/Thread.hs:489`, `src/Engine/Scripting/Lua/DebugServer/Listener.hs:179` | Shutdown/control after admission stops and the running step completes or faults. Never join an owner while waiting for it to execute an admitted phase. |

No accepted baseline change is needed to break these proposed cycles: restrict
blocking calls to completed boundaries and use returned, resumable requests for
gameplay handoffs. GT-2 through GT-6 must prove this with real owner fixtures;
the pure model and this graph do not prove that existing worker loops already
obey these restrictions.

## Read/write and script/native inventory

| Execution owner | Current reads/writes and evidence | Future entry and delivery owner |
| --- | --- | --- |
| Unit, including building drain | Reads pause, queues, unit stats, per-owner-page terrain and simulation records; writes `utsSimStates`, unit render mirrors, building manager and session time (`src/Unit/Thread.hs:111`, `src/Unit/Thread.hs:152`, `src/Unit/Thread/Movement/PathAdvance.hs:78`). | Lua driver requests native admission, motion, consequence settlement and publication under a unit-owned permit. GT-3 owns finite command service, GT-7 motion/time and its completed view. |
| Combat | Reads unit stats/equipment/body state, current time and own-page climate; atomically writes wounds/blood and emits unit commands/events (`src/Combat/Thread.hs:155`, `src/Combat/Wounds/Tick.hs:78`). Shared unit state makes overlap with physiology unsafe. | Native finite attack resolution and wound integration requested by the driver; GT-3 service, GT-8 continuous wounds and injury view. |
| Fluid | Reads admitted activation/seeds/edits and private `SimState`; writes private chunks/dirty sets, then publishes world batches (`src/Sim/Thread.hs:85`, `src/Sim/Thread.hs:127`). | Native preparation and interval iteration requests; GT-4 control/readiness separation, GT-13 cadence/writeback completion. |
| World | Reads page calendar/config, tiles/edits, unit/building state, bills/recipe definitions and nodes; writes calendar/date, harvest timers, item temperatures, power-node charge, terrain/writeback state, discovery and quads (`src/World/Thread/Time.hs:21`, `src/World/Thread/Power.hs:51`, `src/World/Thread.hs:125`). | Separate native admission, power admission, world integration, writeback and completed-view requests. GT-4 service, GT-12 integration/views, GT-13 fluid handoff. |
| Lua physiology | Reads unit info/pose, body/stat/resource values, environment and inventory; writes stats, psychology/thought state, alert state and native kill/collapse commands (`scripts/unit_resources.lua:77`, `scripts/unit_resource_tick.lua:146`). | Granted resumable C3 roster; GT-9 owns interval integration, atomic field policies and its completed physiology view. |
| Lua AI and building gameplay | Reads and writes `aiState`, claims, per-job clocks, bills, inventories and native command queues; `building_spawn` writes its roster/deadline state and progress (`scripts/unit_ai.lua:249`, `scripts/unit_ai_craft.lua:225`, `scripts/building_spawn.lua:639`). | GT-10 splits intent/work/utility phases; GT-11 splits construction/polling. GT-5 supplies contextual entry points; GT-5A supplies retained membership, safe yields and read/intent access. |

Existing native helpers invoked synchronously from Lua (stat writes, craft and
transfer operations, building progress) execute on the Lua owner today. They
remain explicitly classified Lua-phase mutations unless a future adapter moves
them. A namespace name does not establish an execution thread. Cross-owner
atomic updates remain required; serial scheduling alone does not repair stale
snapshot writes such as the independently tracked #2468/#2470 repairs.

## Presentation reads and AI delivery coverage

All safe mid-phase presentation uses **completed** gameplay read views,
UI-local mutations and queued/revalidated intents. These are future interfaces;
current APIs often read live refs. `scripts/init_loader.lua:86` through `:279`
is the concrete ordinary-script inventory. The following assigns its gameplay
read families without placing every panel rewrite inside GT-5A.

| Read family / current consumers | Delivery owner |
| --- | --- |
| Unit identity/position/pose/selection, movement overlays, name plates; `unit_manager`, `unit_drag_select`, both unit-info UIs | GT-7 publishes motion/time/identity; GT-5A supplies access/intent routing; GT-3 authoritative selection/order admission. |
| Wounds/blood/injury and combat information; injury panel/log, unit-info body views | GT-8 completed combat/injury view; GT-9 derived physiology/mental values. |
| Stats/resources/needs/thoughts and mental state; unit-info V2, thought/unit log annotations | GT-9 produces the completed values; GT-5A routes safe access. Drained log events remain single-consumer streams, not copied or repeatedly drained fixtures. |
| Inventories, ground items, bills, work progress and transfer eligibility; cargo/item/contents/crafting panels, transfer-session UI | GT-10 maps action-owned values/intents; GT-12 world/item/bill read views; GT-5A preserves nested-window/revalidation behavior. Item identities and remembered container observations keep their existing semantics. |
| Building progress/rosters and spawn deadlines; building-info and build-tool preview | GT-11 completed building gameplay view, GT-3 placement/teardown admission; static preview animation remains wall-paced. |
| Calendar/solar/weather, flora/plant choices, terrain/cursor/map/locations and power | GT-12 owns world read publication; GT-4 owns safe preparation/cursor service; GT-13 supplies completed fluid results. Static authored definitions need no per-step copy. |
| Tutorial and expedition/location progress, etymology and discovery | GT-10 owns action facts, GT-12 world facts, GT-5A presents their completed versions and boundary reconciliation. No new tutorial content is part of GT-1. |
| Event/combat/injury/thought logs, popup coalescing, shell, menus, autosave eligibility and pause feedback | GT-5/GT-5A classify access. Wall scheduling stays; unrestricted shell and actual save/load take boundary permits. Pending pause has immediate UI acknowledgement distinct from settled gameplay pause. |
| World render quads/solar tables and native unit animation | GT-7 matching motion/time publication, GT-12 completed world view, GT-13 accepted fluid terrain. Camera interpolation never advances gameplay beyond completed samples. |

**Scope gate:** implementing shared adapters rather than changing existing
panel behavior is the starting proposal. GT-5A must enumerate each affected
API consumer before its issue is ready. Any panel requiring a separate
behavioral rewrite needs a new bounded design slice; none is silently assigned
to an unlimited GT-5A PR. These assignments are ownership, not evidence that
every live UI API is already safe during suspension.

AI's registered families are enumerated at `scripts/unit_ai.lua:145`, `:184`,
`:229`, `scripts/bear_ai.lua:299`, `scripts/red_squirrel_ai.lua:226`, and
`scripts/unit_ai_encounter.lua:400`:

| Families | Migration owner and phase split |
| --- | --- |
| `retreat`, `engage`, `attack_target`, `follow_command`, ruin guard/memory/engage | GT-10 decisions/admitted-action split; GT-7 movement; GT-8 native attack/wound context. New decisions target next step. |
| idle/wander and bear/squirrel rest/alert/flee/wander/idle | GT-10 utility cadence; elapsed rest/episode accounting uses granted duration; GT-9 physiology remains independent. |
| drink/eat/forage/refill/search/source-drink, sleep, treatment, notify allies | GT-10 continuous episode costs/progress and due decision split; GT-9 resulting physiology, GT-8 wound treatment. |
| build-nearby, delivery, construct, craft, store-materials, dig, chop, till, plant, harvest, repair | GT-10 P1/P3 work and eligible stalls; GT-11 building construction; GT-12 energy/calendar/world commits. Power alternative below remains open. |
| pickup-ground, transfer-order, escort-transfer, hold-for-transfer and position hold | GT-10 action/eligibility and queued intents, GT-3/GT-7 order application and movement. Preserve strict player transfer policy, page checks, holds and remembered observations. |

`unit_ai_save`, reconciliation, claims, page filters, fetch helpers and location
knowledge are supporting state/lifecycle owners, not additional time grants.
They follow their owning family and boundary reconciliation. Newly discovered
families or a GT-10 scope too large for one PR return to design for a split.

## State and transport placement

GT-1's `Protocol` is an ordinary pure function value, unreachable from any
running engine root. It is not a new inventoried session field. This uses
capability inventory §6.4(a); no §6.4(c)/(d) exception is requested.

Later Phase A transport proposal: construct bounded request/grant/ack handles
at startup and pass them to the owning worker closures, including Lua's driver
context. Coordinator epoch/ordinal/debt/mode are unit-worker-local; each worker
holds its last accepted phase and resumable cursor locally. Completed read-view
handles are shared immutable publications passed to consumers, with one stated
publisher per domain. No global clock fields are scattered into `EngineEnv`.
GT-2/GT-5A/GT-6 must recheck concrete constructor plumbing and classify any
actually introduced root-owned state before wiring it into runtime.

Transport, raw wall samples and unstarted debt are transient. Completed time
and periodic progress that affect gameplay require GT-14's persistence design.
An estimated provisional unit's trajectory is gameplay state, not disposable
transport; its future movement/residency owner must define the save contract.

## Q-4: current displacement and terrain-readiness inventory

| Reachable path and base evidence | Current guard and timing disposition |
| --- | --- |
| Fall-prohibited ordinary movement: `src/Unit/Thread/Movement/PathAdvance.hs:143`, `:371` | Abandons an unverified own-page request; terrain cost checks refuse unreadable crossings. Keep this guard until a prepared-motion adapter replaces it explicitly. |
| Fall-permitted ordinary movement: same `:371`, and fallback elevation at `:509` | Can move without a terrain snapshot; it is not protected by the prior row's gate. D-10 says ordinary movement should prepare required terrain before starting. Preserve today's production behavior in GT-1; GT-7 must make the future prepared-admission change explicit rather than accidentally treating this path as already guarded. |
| Deliberate leap/lunge: `src/Engine/Scripting/Lua/API/Units/Spawn.hs:452`, `src/Unit/Thread/Command/Motion.hs:123`, `scripts/unit_ai_combat_lunge.lua:254` | Checks standing pose, transition state and reach; no terrain-readiness check at native launch. `Leap.hs:97` captures a same-height endpoint and `Fall.hs:25` interpolates it. Reachability includes combat scripts. **Owner decision (2026-09-07):** deliberate combat leaps are prepared motion and wait for terrain before launch. GT-7 must add the prepared-admission guard; these leaps do not enter the forced-displacement fallback. |
| Cliff fall and climb: `src/Unit/Thread/Movement/PathAdvance.hs:398`, `:409` | Start only after source/destination terrain elevations were read. `Fall.hs:87` stores anchors; later interpolation does not revalidate readiness. Loss of an admitted reserved footprint is a residency/lifecycle violation, not automatic permission to predict. |
| Slipped climb: `src/Unit/Thread/Movement/Climb.hs:119` | A scheduled slip converts the climb into a fall back to the captured origin anchor, with no new terrain read. Preserve the known-footprint reservation requirement and classify its interval under M1; it does not request another interval. |
| Re-ground after a terrain edit: `src/Unit/Thread/Command/Lifecycle.hs:123`, `src/World/Thread/Command/Edit/Dig.hs:414` | Snaps only idle units on the edited page/tile and only when the page's surface can be read; missing surface does nothing. P4 → P5 is a forward handoff. Transitioning units are deliberately excluded. This is an edit consequence, not a newly simulated fall. |
| Administrative teleport: `src/Unit/Thread/Command/Lifecycle.hs:69` | Validates finite coordinates, accepts explicit Z or falls back to Z=0 when its own-page surface is unavailable; clears target/transition state and mirrors the new position. Keep it in boundary/control admission, consuming no movement interval and resetting interpolation. It does not reset the whole session epoch. |
| Spawn/load/reset: `src/Unit/Thread/Command/Spawn.hs:206`, `src/World/Load/Publish.hs:108`, `src/Unit/Thread.hs:183` | Establishes state rather than integrating elapsed motion. Only whole-session replacement/reset installs a fresh runtime epoch; an additional page or unit spawn does not reset time. |

No throwing/impulse transport is added here. A future provisional unit needs
session/page identity, last verified anchors/velocity, start step, private
elapsed accounting and bounded assumptions/costs. Each granted interval credits
it once privately, with no attacks, transfers, discovery or external effects;
it acknowledges without waiting for terrain so other participants continue.
Ready terrain permits reinsertion at a later boundary, never replay of external
encounters. Horizon, self-damage correction and persistent trajectory policy
remain with the future movement/residency design.

**Owner decision (2026-09-07):** combat leaps wait for prepared terrain before
launch. This classifies deliberate leaps as prepared motion; provisional
estimation remains reserved for forced displacement under D-10. GT-7 must
implement this admission rule. GT-1 records the decision and changes no launch
guard or runtime missing-terrain behavior.

## Power/work alternatives: owner decision still open

1. **Powered-fraction credit.** P2 computes an authorized fraction/duration;
   P3 credits the matching worker-seconds and P4 records the corresponding
   energy debit. Completion thresholds, resource costs and recipe results must
   all use the same allowance. This preserves useful partial work but requires
   a precise allocation rule when several bills share limited storage.
2. **Full-quantum refusal.** P2 admits a full work interval only if its complete
   energy requirement is available; otherwise P3 credits zero and its work
   energy debit is zero. Generation/passive node evolution still proceeds.
   This is simpler but can strand energy smaller than one work quantum.

Both retain P1 → P2 → P3 → P4 ordering and authoritative admission. Neither is
selected here. The owner must choose before GT-10/GT-12 become ready. Shipping
base rate, debt cap, service/burst limits and responsiveness threshold are also
unselected; fixture parameters are not recommendations for runtime defaults.

## Validation and owner handoff

Run the issue's focused `coordinated simulation step protocol` and unchanged
`monotonic elapsed-time contract` groups, warning-clean builds, and Cabal,
Unicode, Haddock, module-budget and capability audit/self-test pairs.
Mutation evidence must name the changed rule and the example that detects it;
pure protocol tests do not claim real-worker or rendered-UI coverage.

### Executed validation (2026-09-07)

- `cabal build all` and `cabal build synarchy-test-headless`: passed.
- `cabal test synarchy-test-headless --test-options='--match "coordinated simulation step protocol"'`: 15 examples, 0 failures.
- `cabal test synarchy-test-headless --test-options='--match "monotonic elapsed-time contract"'`: 19 examples, 0 failures.
- Required Cabal, Unicode, Haddock, Haskell-budget and capability audit/self-test pairs: passed without baseline or inventory edits.

The mutation experiment compiled the exact new test module against temporary
copies of the real protocol source using GHC 9.12.2, `-XGHC2024`,
`-XNoImplicitPrelude`, `-XUnicodeSyntax`, `-XOverloadedStrings`, and the existing
Cabal package environment (`cabal exec -- ghc`). It first ran all 15 examples
unmutated, then compiled every mutation successfully and ran the named example
with Hspec `--match`. Every row below produced exactly one failing example;
compile errors, empty matches and timeouts were not counted as detections.
The production source was never mutated by this experiment.

Protocol source SHA-256: `8eccea421ca92f023ec27bf224bcbcbf22dd6a52aa3db0317ab3f4ade70b1965`.
The retained foreground-session artifacts include the exact old/new source
strings, compilation commands, per-mutant build/test logs and result JSON.
These are local verification artifacts, not a new recurring CI gate.

| Mutated rule | Detecting example | Result |
| --- | --- | --- |
| `duplicate-accept` | rejects duplicate grants and acknowledged phases without applying effects | Detected |
| `wrong-epoch` | rejects wrong epochs and step ordinals on grants and acknowledgements | Detected |
| `wrong-step` | rejects wrong epochs and step ordinals on grants and acknowledgements | Detected |
| `wrong-owner` | rejects the wrong owner and not-yet-enabled phase without effects | Detected |
| `future-phase` | rejects the wrong owner and not-yet-enabled phase without effects | Detected |
| `second-step` | admits at most one in-flight step | Detected |
| `incomplete-ack` | publishes no time for an incomplete, unacknowledged or failed phase | Detected |
| `unaccepted-ack` | publishes no time for an incomplete, unacknowledged or failed phase | Detected |
| `early-publication` | publishes no time for an incomplete, unacknowledged or failed phase | Detected |
| `rounded-clock` | keeps exact origin plus N over rate with no floating-point drift | Detected |
| `invalid-rate` | validates numeric and phase-plan domains so time cannot be negative | Detected |
| `negative-origin` | validates numeric and phase-plan domains so time cannot be negative | Detected |
| `uncapped-demand` | caps and counts only unstarted demand while preserving admitted work | Detected |
| `discard-counter` | caps and counts only unstarted demand while preserving admitted work | Detected |
| `discard-admitted-work` | caps and counts only unstarted demand while preserving admitted work | Detected |
| `pause-not-visible` | exposes requested pause immediately but settles only after completion | Detected |
| `pause-prematurely-settled` | exposes requested pause immediately but settles only after completion | Detected |
| `pause-keeps-demand` | rebases pause pacing without replay or changing a suspended interval | Detected |
| `paused-wall-replay` | rebases pause pacing without replay or changing a suspended interval | Detected |
| `resume-with-old-baseline` | rebases pause pacing without replay or changing a suspended interval | Detected |
| `lost-suspension-cursor` | resumes the same membership cursor prefix and interval exactly once | Detected |
| `work-while-suspended` | resumes the same membership cursor prefix and interval exactly once | Detected |
| `fault-not-latched` | latches faults and refuses every ordinary continuation | Detected |
| `old-prefix-cadence` | invalidates old epoch grants prefixes cadence cursors and pacing debt | Detected |
| `reused-epoch` | invalidates old epoch grants prefixes cadence cursors and pacing debt | Detected |
| `transaction-mid-step` | keeps transaction and empty-roster completion at whole-step boundaries | Detected |
| `unsanitised-wall` | sanitises wall samples without admitting invalid demand | Detected |

Before dependent issue drafting, record the owner's graph acceptance and Q-4
classification in the parent design through its canonical design workflow.
GT-1 does not update that document's processing markers or close its later
decision gates merely by writing this protocol.
