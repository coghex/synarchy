# Shared page time and background gameplay design

Design state: `exploring`

Owner: `coghex/synarchy`. Publication target: `master`.
Local draft authorized on 2026-09-24; no GitHub epic or child issues created.
This document stages alongside the river design; its eventual implementation
and required contracts/evidence belong together in its own delivery lane.

Status legend: `[ ]` unprocessed · `[#N]` linked issue · `[no-issue]` deliberately
not tracked separately · `[deferred]` blocked on a concrete precondition.

## Processing status

- [ ] EPIC. Keep existing hidden pages coherent as gameplay time advances

No child slices allocated. Consumer inventory and catch-up policies precede
delivery decomposition and readiness signoff.

## Goal and observable outcome

Returning to an existing page reveals water, calendar, and time-dependent
gameplay that agree about elapsed gameplay time. Hiding a page must not produce
a drained river in a calendar that stayed frozen. Pause freezes progression;
render visibility is not the authority for elapsed time.

Completion requires approved policies for each time consumer, bounded work and
memory, coherent save/load, and evidence that page visibility does not introduce
unaccounted elapsed time or duplicate effects. It does not require every system
to run at visible-page frequency or every chunk to stay loaded.

## Approved decisions

### D-1. Advance shared time on existing hidden pages

The owner approved shared page-calendar advancement with coherent handling by
time-dependent gameplay. This carries forward the
[river design's shared-time decision](river_runtime_design.md#d-20-advance-shared-time-on-existing-hidden-pages).
It does not authorize generating additional pages or advancing time while the
game is paused or closed.

### D-2. Deliver this as a separate design arc

The owner approved a separate local draft. Its integrated behavior gates
all-page river rollout; counted hydraulic time, the fluid kernel, and controlled
single-page river experiments can proceed independently. Tracker creation still
requires its own approval.

### D-3. Reuse coordinated timing and scope only residual background work

The owner approved reuse of #2478 in the
[river timing decision](river_runtime_design.md#d-33-reuse-the-existing-coordinated-gameplay-timing-arc). This draft does
not own another clock, calendar adapter, or duplicate gameplay-consumer migration.
First audit GT coverage; retain only additional all-existing-page eligibility,
compact-state catch-up, and cost requirements. No separate tracker umbrella is
justified until that residual scope is established.

## Timing evidence and ownership

Checked `master`/`origin/master` at
`e3c781c77bf3534e5ab1204f6e5ab073f53c5d37` on 2026-09-24.

`src/World/Thread/Time.hs:tickWorldTime` advances fractional clock/date only for
`wmVisible` pages, respecting the shared pause flag and per-page speed. Flora
regrowth, item temperatures, and power updates occur within that visible-page
loop. Location discovery and craft-bill ownership reconciliation separately run
over existing pages under their load-transaction gate. Thus some hidden-page
work already runs: this is not a blanket change from no simulation to simulation.

Unit needs, schedules, farming, climate, work execution, and their Lua/native
timing boundaries still need a consumer-by-consumer audit. Do not infer their
behavior solely from the world-time loop. An initial open-issue keyword search
did not establish a matching umbrella; full tracker deduplication remains a
readiness gate, including overlaps with flora and residency work.

## Proposals

Slicing-time ownership check: existing epic
[#2478](https://github.com/coghex/synarchy/issues/2478) and
`docs/gameplay_timing_design.md` already own coordinated timing and consumer
migrations. The [river ownership question](river_runtime_design.md#q-23-how-does-this-arc-reuse-the-existing-coordinated-timing-work)
is resolved in favor of reuse. Narrow this draft to
residual all-page membership/catch-up behavior; it is not authority to introduce
a competing publisher or duplicate timing slices.

### P-1. Share elapsed time and classify consumers

Consume GT grants and their `PageGameplayProgress` projection described in
[river timing integration](river_runtime_design.md#p-6-gameplay-clock-and-representation-handoff).
The unit-worker coordinator owns permits/completion; the world thread publishes
the GT-12 page/calendar projection. This arc addresses residual hidden-page
eligibility and consumer coverage; it does not introduce another accumulator.
Calendar setters must
not silently create elapsed simulation time.
Inventory each consumer's clock, persistent progress, effects, dependencies,
and resident-data needs. Propose continuous bounded stepping, event scheduling,
or exact/approved catch-up per consumer; no blanket analytical catch-up claim.

Until this integration is available, the interim river rollout freezes hidden
page water with its calendar in river-only test modes, after completing already
admitted intervals. No hidden wall-time debt is added on return. Do not freeze
GT-prepared hidden gameplay participants by camera visibility: integrated
production must satisfy their timing eligibility and river progression together.
Final all-page behavior remains gated on GT plus any residual work identified
here; the interim policy is not completion of D-1.

### P-2. Preserve causality and durable obligations

Catch-up cannot consume materials twice, skip needs without policy, award work
through unavailable prerequisites, or apply seasonal water forcing from the
wrong date. Coordinate dependent consumers at agreed boundaries. If processing
lags, retain the obligation and define query/admission behavior rather than
reporting unapplied time as completed. Save one coherent session boundary with
progress and accepted effects; restore without applying elapsed time twice.

### P-3. Measure the whole-game cost

The river design's initial reference is game speed 1.0 on Apple M3 Max, 64 GiB,
with pause supported. Its accepted initial hydraulic budget excludes non-water hidden
gameplay. Propose that hardware as this arc's reference too, but establish a
separate whole-game budget and workloads before promising throughput. Measure
page count, units, crops, jobs, reservations, backlog, and revisit latency.

## Open questions and delivery gates

### Q-1. Which consumers advance continuously versus catch up?

Requires the consumer inventory and explicit behavior approval, especially for
unit needs, hazards, crops, and jobs whose consequences interact. This blocks
implementation slicing; it does not block river-only experiments.

### Q-2. How are per-page speeds and overload handled?

Define speed changes while hidden, pending work, admission and query consistency,
and bounded processing without quietly dropping elapsed time. Shared elapsed
time does not itself decide whether pages share a speed setting.

### Q-3. What existing arcs supply lifecycle and persistence support?

Coordinate with river hydraulic progress/component ownership and chunk
residency reservations. CRS specifications are pending work, not implemented
services. Keep one authoritative clock and one owner for each gameplay effect.

## Verification and delivery order

First audit GT coverage and consumers, then agree only residual policies;
integrate GT's shared progress contract and consumer persistence boundaries;
then allocate
small consumer migrations and integrated
all-page tests. No one-PR scope is asserted yet.

Compare visible and hidden schedules at the same elapsed gameplay time using
approved consumer tolerances. Include midnight/season boundaries, pause, speed
changes, page hide/show, creation/destruction, edits during catch-up, and fresh
process save/load. Test existing hidden-page discovery/reconciliation contracts
as well as new progression. Run targeted subsystem gates from
`docs/engine_contracts.md` and persistence behavioral probes as their owners
change. Headless correctness and rendered revisit behavior are separate gates.
