# River runtime and partial-world simulation design

Design state: `ready for issue processing`

Owner approved the 29-slice plan and internal-test-backend-first rollout on
2026-09-24. Readiness retains each slice's explicit parameter, art, numerical,
migration and external-integration gates; it does not approve implementation
or create tracker artifacts.

Owner: `coghex/synarchy`. Publication target: `master`.
Code, this design, contracts, and retained evidence belong in the same delivery
lane. The current isolated worktree is a staging location, not an expansion of
#2533's specification. No tracker or production-code change is made by this revision.

Status legend: `[ ]` unprocessed · `[#N]` linked issue · `[no-issue]` deliberately
not tracked separately · `[deferred]` blocked on a concrete precondition.

## Processing status

- [ ] EPIC. Make river behavior independent of detailed chunk residency
- [ ] RVR-01. Build the controlled hydraulic experiment harness
- [ ] RVR-02. Select and verify the detailed flow law
- [ ] RVR-03. Define the durable hydraulic component and transaction model
- [ ] RVR-04. Implement and measure eager legacy fluid import
- [ ] RVR-05. Implement finite aquifer accounting and excavation transfers
- [ ] RVR-06. Make wet terrain edits conservative and acknowledged
- [ ] RVR-07. Implement the selected all-fluid kernel
- [ ] RVR-08. Add the accounted fixed-sea-level boundary
- [ ] RVR-09. Wire coherent hydraulic checkpoints into save and reconstruction
- [ ] RVR-10. Integrate hydraulic advancement with GT grants
- [ ] RVR-11. Implement compact finite storage and routed flow
- [ ] RVR-12. Implement basin merge trees and additive storage curves
- [ ] RVR-13. Apply terrain changes to compact topology
- [ ] RVR-14. Implement shared compact/detail interfaces
- [ ] RVR-15. Reconstruct and promote current compact water
- [ ] RVR-16. Demote steady and moving water safely
- [ ] RVR-17. Publish compact surfaces and promote affected edits
- [ ] RVR-18. Prove the end-to-end arena river milestone
- [ ] RVR-19. Implement the solid-fill job and material transaction
- [ ] RVR-20. Expose and visually validate the fill build order
- [ ] RVR-21. Define climate units and persist contributing-area summaries
- [ ] RVR-22. Apply climate budgets to the current water owner
- [ ] RVR-23. Build generated river and basin summaries
- [ ] RVR-24. Admit compact routes that encounter lava
- [ ] RVR-25. Discover off-network spills and update edited basins
- [ ] RVR-26. Initialize generated worlds and legacy graphs coherently
- [ ] RVR-27. Measure the supported hydraulic workload and error envelope
- [ ] RVR-28. Integrate all-existing-page water progression
- [ ] RVR-29. Activate the river backend and retire obsolete fluid paths

The umbrella and child boundaries below are approved for issue processing; no
new tracker artifacts have been created. The ledger matches Appendix C. Open
parameter and integration gates are assigned to their affected slices; they do
not imply approval to invent defaults or silently expand a PR.

## Goal and observable outcome

A dam or diversion changes river flow downstream, including in unloaded chunks
and on existing world pages the player is not viewing. Compact storage and
connections simulate those consequences; bounded detailed regions handle local
interactions. Loading midway through drainage reveals current water without
refilling, deleting, or duplicating it or introducing a conspicuous surge.

Water follows gameplay speed and pause. New worlds avoid numerical startup
surges; climate-unsustainable lakes may gradually dry during play (D-21).
Otherwise near-equilibrium initialization remains the aim. Lakes have finite storage, with rainfall/evaporation-driven
supply and desired glacier/ice runoff. Initial work covers surface lake-fed
rivers; mountain ravines that combine downstream remain required broader behavior.

Arc completion requires accounted quantities, stable sustained flow, acceptable
coarse/detail error and transition behavior, save/restart continuity, and measured
memory/time bounds. D-32 accepts the initial numerical targets; transient
profile bounds remain provisional and no checks are claimed passed.
Underground flow, erosion, waves/tides, adaptive skipped-step
interpolation, riverbed infiltration/seepage, dynamic winter freezing, and
seasonal snowpack storage are outside the initial slice (D-23/D-24).

## Approved decisions

All decisions below were made by the owner on 2026-09-24. Candidate mechanisms
in the design sections are not additional approvals.

### D-1. Small level areas at bends and junctions are acceptable

Shared level areas are acceptable if measured and bounded; this does not approve
flattening long reaches.

### D-2. Review the complete river system while developing the change

Review and substantial refactoring/replacement are authorized, with explanations
and evidence. Preserve useful existing work.

### D-3. Couple compact river state to bounded detailed simulation

Represent reaches, junctions, and water bodies outside detailed residency.
Loading an entire river is not the correctness mechanism.

### D-4. Dams and diversions affect unloaded downstream reaches

Later-loaded reaches reflect changed supply, including dry beds after drainage.
Tributaries, lakes, and retained pools can prevent complete drying.

### D-5. Water exchanges have one owner and durable accounting

One owner per water quantity and exchange. Representation changes cannot
create/delete water; dams and stored water survive unloading. Sources, sinks,
and reactions are explicit.

### D-6. Review the existing solver before selecting a replacement

Retain useful exact-volume, reaction, and save-safety behavior. Prove small
examples; do not assume the current detailed solver is a physical reference.

### D-7. Defer automatic erosion

Rerouting and flooding over existing terrain and player-dug channels remain in
scope. Water-driven excavation, sediment, and riverbed migration are deferred.

### D-8. Begin controlled arena experiments without awaiting scenario tooling

Use authored terrain and the existing console. Consume the parallel scenario
system when available; do not redesign it. Arena success alone does not prove
generated-world integration.

### D-9. Supply changes propagate with approximate travel and drainage delay

Use simulation time, including outside detailed residency. No particular wave
equations or numerical rates are approved.

### D-10. Loading during drainage must reveal the current partial state

Camera movement cannot reset hydraulic history or finish drainage early.
Reconstruction needs sufficient spatial information for partly drained reaches.

### D-11. Simulate segment storage and measure reconstruction error

Segments hold actual water and exchange it. Detailed simulation feeds accepted
changes back. Begin with a small dam/diversion example; measure height, wet
extent, and timing while holding accounting and barrier connectivity strict.

### D-12. Require underground diversion later, outside the first slice

Underground filling and remote outlets are required future behavior. Surface-only
experiments can exclude cave entrances; broader rollout needs a supported
interface rather than an unaccounted drain.

### D-13. Permit lake-fed rivers initially; preserve distributed mountain origins

Not every river should start at a special spring/lake. One-tile mountain ravines
must eventually combine into larger channels. Lake-fed rivers are acceptable
initially; do not remove existing non-lake rivers to enforce this staging choice.

### D-14. Keep fixed sea level initially; defer waves and tides

Initial ocean boundaries stay at sea level. Waves and tides are planned later.
External ocean exchanges remain accounted.

### D-15. Require consistent perceived behavior and speed; defer step skipping

Chunk-loading patterns must not materially change visible outcomes or speed.
Exact session replay is unnecessary. Adaptive skipped iterations with temporal
interpolation are future optimization, not an initial dependency. Load-dependent
slowdown is not an approved solution to insufficient hydraulic capacity.

### D-16. Use finite lake storage and climate-derived replenishment

Regional rainfall and evaporation determine replenishment; glacier/ice runoff
is desired where feasible. Lakes can rise/fall and are not restored automatically
to generated levels. Ice availability, routing, and unit conversion need evidence.

### D-17. Water follows game speed and pause

Explicitly approved during review follow-up. Use the same gameplay-time basis as
climate, not an independent fixed wall-clock cadence.

### D-18. New worlds start near equilibrium

Explicitly approved during review follow-up. Avoid a large startup flood/drainage
under baseline climate. Compare calibration and bounded pre-start settling;
the owner has not chosen the initialization algorithm. D-21 subsequently
permits climate-driven drying of unsustainable lakes; that behavior is not a
startup solver defect to eliminate by refilling or inventing sources.

### D-19. Continue on all existing world pages

Explicitly approved during review follow-up. Compact water evolution continues
on existing pages even when not viewed. This does not require creating unseen
pages or ticking every detailed chunk. Shared hidden-page time is subsequently
settled by D-20.

### D-20. Advance shared time on existing hidden pages

Owner explicitly selected "Advance shared page time" in the second review
follow-up. The shared calendar advances and time-dependent gameplay must handle
elapsed time coherently; water does not run against a separate clock while the
page's season remains frozen. This introduces a shared timing/catch-up
prerequisite beyond rivers. Bounded background simulation or explicit catch-up
may implement individual consumers, but their mechanisms and acceptable latency
still need design. Approval does not require every detailed subsystem to tick
at full frequency or create otherwise nonexistent pages.

### D-21. Let climate-unsustainable lakes dry during play; defer springs

Owner explicitly accepted lake drying in the second review follow-up and
reserved springs for later work. Keep the generated lake's initial water and
let the accounted rainfall/evaporation, inflow, and outlet balance lower it
over simulation time. Do not automatically shrink/omit it during generation
solely to impose climate equilibrium, or add a prescribed spring to sustain it.
This qualifies D-18 for such lakes and their changing downstream supply;
numerical initialization pulses and load-induced surges remain unacceptable.
Drying rate follows the calibrated climate/storage model, not an arbitrary
countdown. Existing-save restoration still follows its migration contract.

### D-22. Target game speed 1.0 on the owner's M3 Max initially

Owner explicitly selected "1.0 on this M3 Max" for the first performance gate:
Apple M3 Max, 64 GiB RAM, game speed 1.0 with pause supported. Measure faster
rates separately; this does not authorize silently clamping valid API speeds.
The timing budget is accepted under D-32; supported workload capacity still
needs benchmark evidence. Hardware selection is not a performance result.
The proposed hydraulic budget measures water work only; non-water hidden-page
cost and the whole-game budget belong to the separate background arc (D-27).

### D-23. Defer riverbed infiltration and seepage

The first slice retains the existing climate-based water table and excludes
dynamic infiltration/seepage between surface water and groundwater. No implicit
groundwater loss is added to river or lake budgets. Future exchange must have
explicit ownership and accounted sources/sinks.

### D-24. Defer dynamic winter freezing and seasonal snowpack

The first slice uses baseline climate supply without a new dynamic winter
freezing or seasonal snowpack-storage model. Existing ice presentation does not
establish simulated under-ice flow, evaporation, or liquid/ice mass transfer.
Glacier/ice-runoff accounting remains a separate design item under D-16/Q-10;
this deferral does not authorize an unbounded melt source or duplicate snowfall
and melt credits. Future seasonal integration must define finite stores,
precipitation partition, melt timing, and ice-covered evaporation behavior.

### D-25. The river arc owns the durable hydraulic component and migration

Introduce the durable hydraulic component and migrate the old fluid edit log
once within this arc. CRS-5/CRS-12 must reuse that authority and migration;
their bundle work remains deferred. Align the streaming design before delivery,
without creating a second fluid balance or waiting for the Arc B bundle gate.

### D-26. Build initial playable dams with ordinary solid fill

Allow ordinary fill in water, preserving its quantity, raising the local surface
as the bed rises, and letting the solver redistribute it outward. Supply a
gameplay placement path; debug terrain commands alone do not fulfill this.
This approves the displacement behavior, not a dedicated floodgate feature or
the still-proposed wet-edit job acknowledgment protocol.

### D-27. Separate shared background simulation from the early river work

Shared hidden-page time and coherent gameplay catch-up get a separate design
arc. It gates all-page river rollout, not the hydraulic clock, kernel, or
controlled single-page tests. The owner authorized a local design draft, not
creation of a GitHub epic. See [background simulation](background_simulation_design.md).

### D-28. Dug-up groundwater draws from a finite regional budget

The owner selected finite regional aquifer storage with no refill initially.
Digging transfers an accepted quantity from that store into surface water;
it is not an external source minted per dig. Repeated digging may draw further
water only while the regional budget remains. Depleted stores and drained wells
do not recharge in this slice. Chunk regeneration, edits, and save/load must
preserve remaining storage. D-23's climate-derived water table still supplies
the eligibility reference; this adds finite accounting, not groundwater flow
or dynamic seepage. Initial budgets and legacy initialization remain design gates.

### D-29. Provide a dedicated solid-fill build order

The owner selected a dedicated player build order with material costs and
wet-tile support for D-26's solid-fill dams. Keep existing spoil-disposal rules.
The feature must integrate placement legality, jobs, material accounting, and
the hydraulic commit protocol; debug fill does not satisfy this decision.

### D-30. Digging wet or dry columns can draw finite groundwater

At an admitted dig, first preserve existing surface-water quantity against the
new bed, then top up toward the climate-derived water table from D-28's remaining
regional budget. This applies to dry pits, already-wet wells, and river/lake beds.
Never lower an existing higher surface to the water table. Extraction is capped
by the deficit, available aquifer storage, and admitted surface capacity, with
one atomic debit/credit. Dredging can therefore drain the finite aquifer into a
river. No continuous top-up or recharge follows between dig commits. In exact
volume units, requested top-up is `max(0, volumeToWaterTable(newBed) - existingV)`;
the committed amount is limited by the remaining budget and admitted capacity.

### D-31. Initial fill recipe and height rules

Each added tile-column z-level costs four `granite_chunk` items and becomes
granite terrain. Support wet and dry ground, one committed layer at a time,
toward a fixed designated height. There is no water-relative height cap;
terrain representation limits and safe worker access still apply. Reuse
existing granite art where suitable. This is the accepted initial gameplay
recipe, not a physical density conversion or completed art signoff.

### D-32. Accept initial numerical gates; measure transient bounds first

The owner accepted the acceptance table as initial gates, including steady
profile, timing, load-pulse and hydraulic-budget targets. Transient-profile
bounds remain provisional until measured and separately accepted; strict
accounting/barrier rules always apply. Acceptance does not establish feasibility,
passing results, or approval to loosen a failed gate. The numerical method and
its stability/convergence evidence still require evaluation.

### D-33. Reuse the existing coordinated gameplay timing arc

The owner selected reuse of #2478, keeping river experiments independent.
GT owns the shared clock, calendar/fluid scheduling adapters, and clock
persistence. River code consumes that contract; it does not introduce a second
elapsed-time authority. The separate background draft covers only residual
all-page membership/catch-up requirements not already owned by GT. Production
integration waits for the actual required timing implementations and gates.

### D-34. Approve the delivery plan for issue processing

The owner approved RVR-01 through RVR-29 and the internal-test-backend-first
rollout. Unresolved parameter, art and integration choices remain explicit
gates on their affected slices. Begin tracker processing with the epic and
then one child at a time, with separate approval for each artifact. This is
design readiness, not authorization to create issues or implement the plan.

Current-code evidence and retained characterization results are in
[Appendix A](#appendix-a-current-code-and-retained-evidence).

## Candidate design

The following is a concrete strawman for review and experiments. It is not a
selected numerical method, a wire schema, or a claim of measured performance.

### P-1. Compact state, links, interfaces, and accounting

Use storage cells rather than one aggregate per whole river. A long reach has
several segments; a pool, branch, barrier, or significant geometry change can
introduce a boundary. A lake can contain several hydraulic storage regions while
retaining one water-body identity.

| Candidate record | Meaning and minimum fields |
|---|---|
| `PageGameplayProgress` | Page/incarnation, conversion of GT-granted duration, and publication revision. A world-thread-published projection of #2478's time, not an independent accumulator; exact representation is reconciled with GT-12/GT-14. |
| `HydraulicProgress` | Page identity, logical time consumed, shared target revision, fixed-step remainder, numerical-method version. Its target is read from `PageGameplayProgress`, not independently accumulated. |
| `StorageRegion` | Stable region ID, body ID, spatial/vertical domain, geometry revision, exact volume, volume-to-level relation, reconstruction data, authority revision. |
| `Segment` | Channel specialization of a storage region: length, cross-section summaries, adjacent link IDs, roughness/damping parameters. |
| `Link` | Stable ID, endpoint region/port IDs, connection geometry, sill/barrier, width and length, fluid transport policy, previous accepted discharge, quantization remainder. |
| `FaceFlowState` | Detailed face identity, signed accepted discharge, fractional conversion remainder, fluid policy, topology/ownership revision; shared faces have one stored value. |
| `Interface` | Port geometry, owner on each side, ownership epoch, shared step/interval, head data, proposed and accepted exchange. |
| `LedgerEntry` | Transaction ID, page/interval, from/to owner or external source/sink, signed exact quantity, reason, topology/ownership revisions, commit status. |
| `CatchmentSummary` | Stable contributing-area identity, climate weights, drainage destination, rainfall/loss terms, optional ice contributor, geometry revision. |
| `AquiferBudget` | Page and stable regional identity, initialization version, remaining exact quantity and extraction transaction watermark; durable finite storage under D-28, with no initial recharge. |
| `TopologyDelta` | Revision, split/retired/new IDs, geometry changes, volume/link-state remapping, edit provenance. |

These are semantic records. Use checked wide volume arithmetic for compact
stores; a lake must not inherit a single cell's `Word16` capacity. One volume
unit is the existing one-tile-column eighth-z quantity. Geometry conversion must
account for area; one eighth of height across a lake is many volume units.

Identify a domain by page, region/port ID, horizontal extent, and a vertical
compartment reference, initially `Surface`. Never identify all future water by
`(x,y)` alone. This reduces avoidable assumptions but cannot promise that later
stacked-span storage/rendering will need no migration.

The ledger is an exactly-once transaction mechanism, not an unbounded historical
log. After acknowledged commit and a safe snapshot boundary, compact old entries
into balances and deduplication watermarks. Proposed transfers are not water in
transit: until commit, the donor still owns them. If an algorithm explicitly
models transit storage, name it as a separate conserved owner.

#### Flow state and its persistence classification

The proposed inertial kernel adds gameplay state; today's `FluidMap` and
`afcFlowDir` bitmask cannot encode it. Plan approximately two unique face values
per detailed column plus per-face conversion remainders; measure their memory
and save cost. Do not bake flow away on deactivation.

| State/lifecycle | Proposed authority and classification |
|---|---|
| Active detailed faces | Sim thread owns `FaceFlowState` alongside volume arrays. Accepted discharge and conversion remainder: **Persist exactly**, via the coordinated durable snapshot, not blanket `Rebuild` scratch. |
| Compact-owned region | Persist compact link discharge, remainder, and any retained subregion/residual flow needed by projection. Coarsening may approximate the fine flow field under the demotion error contract; it may not reset all flow to zero. |
| Dormant/baked detailed checkpoint | Keep a sparse durable flow checkpoint in the component selected by Q-19; CRS-12 does not provide it today. `FluidMap` is only the published surface projection. Rebuild from persisted compact state/profile, never volume alone. |
| Geometry tables, lookup slots, render data | **Rebuild deterministically** from authoritative geometry/IDs and hydraulic state; process handles/scheduling deadlines remain **Exclude**. |
| Save/load | Snapshot the owning representation's flow and volume at the same interval, including residuals and accepted obligations. Reconstruct active arrays from that state; no new surge from zero-initialized discharge. |

The sim thread is the sole evolving hydraulic owner across existing pages.
The world thread is the sole publisher to `wsTilesRef` and holds the proposed
durable checkpoint mirror supplied by D-25. Checkpoints are versioned mirrors, not
a second spendable simulation. Use acknowledged revisions through the existing
save barrier. Revise the current `Rebuild` classification for Sim state in
`docs/persistence_state_inventory.md` section 6 and add nested flow-state,
remainder, and compact-projection rows when implementing this schema.

The standalone kernel change therefore needs its own component migration even
before river coupling. Freeze the old wire layout; missing historical flow
cannot be recovered exactly from surfaces. Propose documented legacy
initialization and test its transition separately from exact round trips of
new saves. A zero-flow legacy fallback is not automatically acceptable.

Proposed face key: page, vertical domain, canonical tile at the negative-axis
endpoint, and positive X/Y direction. Wrap the endpoint through the page's
actual topology; never choose the owner by sorting wrapped coordinates. Store
one value in that endpoint's chunk checkpoint while both sides are detailed.
At a compact/detail boundary, transfer that value, orientation, and remainder
into the Interface/Link in the same ownership transaction, even if the former
storage chunk is absent. Reverse this transfer on promotion. Test ordinary and
wrapped faces through save/load and both neighbor promotion orders.

### P-2. Partial residency without whole-segment loading

Recommend partitioning storage ownership at the actual detailed footprint:
temporary subregions cover loaded portions of a reach/lake, and compact regions
retain the remainder. Logical body identity survives; owned footprints never
overlap. A representation split is distinct from a terrain/topology split.

A partly loaded lake needs head-aware exchange and finite shared storage.
Each compact subregion owns its quantity; detailed cells own theirs. Derive
boundary heads from the appropriate storage/geometry and use one bidirectional
exchange, not a prescribed river discharge or an infinite fixed-level lake.
No separate whole-lake owner may also hold the sum as spendable water.
Short-term head differences can exist; basin-wide instantaneous equalization
is not assumed across a long lake or a new dam.

For surface water, precompute a level-to-volume curve per chunk **and connected
basin compartment**, with geometry revisions. Represent it as compressed
elevation/count data plus prefix sums. Curves add for disjoint areas connected
at a common head; subtract loaded members from cached basin aggregates to obtain
the unloaded remainder without scanning absent terrain. Camera moves update
membership aggregates, not the whole watershed. Separate disconnected pools
and regions with distinct heads instead of summing them into one lake curve.
An edit invalidates the affected curves and connectivity ancestors; compute
replacement summaries from the final composed terrain before accepting the
new partition. Storage precision must match the column model, including
eighth-level thresholds, without rounding each partition independently.

Connectivity depends on level: the basin hierarchy is a merge tree with sill
thresholds, not a fixed list of connected leaves. Below an internal sill, retain
separate pool quantities/heads; do not drain them through an aggregate curve.
Above it, allow exchange without assuming immediate common head. Geometry
summaries encode these thresholds independently of camera membership.

A renderable loaded chunk need not be hydraulically detailed forever. A compact
owner can publish reconstructed surfaces and discharge-driven visual data while
terrain remains loaded. Do not pin every segment/lake member to support one view.

#### Editing and querying compact-owned loaded terrain

Loading terrain and promoting hydraulic ownership are separate operations.
The proposed hydraulic edit protocol applies only to edits affecting wet cells,
hydraulic interfaces/barriers, or compact-owned storage/connectivity (including
a dry bank opened beside water). Unrelated dry terrain and cosmetic edits keep
their existing completion path. Use current authority, not a stale displayed
fluid cell, to decide eligibility.

Candidate: reserve the affected footprint and checkpoint headroom, reconcile
the latest water at a hydraulic boundary, then commit geometry, displacement,
topology, and ownership together. Already-loaded terrain needs no reload.
Until acknowledgment the affected job remains pending: no completion receipt,
duplicate material consumption, or committed pathing update. Rejection/cancel
uses its existing rollback/refund contract; final publication triggers ordinary
revalidation and regrounding. The job/API integration must be specified and
tested before adoption (Q-20). An alternative immediate-terrain commit needs a
durable displacement obligation, coherent query semantics, and protection from
further edits/flows using old geometry; merely delaying water accounting is not
a complete protocol. Preserve page/edit fences in either approach.

Reserve the edit's write footprint and geometry/interface dependencies through
commit or rejection, including neighboring faces whose transfers could conflict.
Serialize overlapping jobs by admitted command identity; disjoint work may
continue. Acquire the complete bounded footprint atomically or retry without
holding a subset. Revalidate revisions after waiting. Proposed terrain stays
out of the committed tile map, but pending reservations are visible to job
admission and final movement checks: conflicting jobs wait, and a unit cannot
enter the affected occupancy footprint across commit without revalidation.
Keep unrelated movement and jobs running. Cancellation releases reservations
only after the transaction is definitively canceled or acknowledged committed.
CRS-6 specifies related work but is not implemented; reuse one reservation and
acknowledgment contract, supplying the needed subset before playable wet fill.

Ordinary compact evolution must also emit surface updates for loaded terrain.
The world thread applies revisioned publications to `wsTilesRef` and invalidates
the corresponding render/path observations. A rendered compact surface cannot
be newer than what gameplay queries see. Candidate normal-load edit target:
visible hydraulic response within 200 ms of an admitted edit at reference speed;
also report request-to-admission delay and refusals rather than hiding backlog.

Verified consumers include `scripts/unit_ai_water.lua` via
`WorldQuery.Fluid` (water search/drinking/standing-on-fluid checks),
`Unit.Pathing.Cost` (river/lake penalties, ocean/lava blocking),
`World.Thread.Command.Cursor.Till`, `World.Spoil.Logic`, flora/soil gates, cursor
queries, and fluid/ground-item/zoom rendering. Check source discovery and cached
observations after flooding/drying. No swimming implementation was found in the
searched `src`/`scripts`; do not list it as an existing consumer without evidence.
Extend the consumer inventory before implementation as changed query paths demand.

Fixed-level ocean boundary detail: expose the sea head only at geometrically
connected ocean ports/cells. Compute or enforce its exchange under the same
interval ownership rules and record any volume added/removed as external ocean
supply/sink. A clamped cell is a boundary region, not an ordinary finite cell
also receiving a duplicate compact reservoir transfer. Dammed-off coastal
water is finite. Never clamp every cell labeled Ocean regardless of connectivity.

### P-3. Sustained flow and partition-independent updates are prerequisites

The old redistribution rule is not the baseline for production coupling.
First demonstrate rated sustained throughput, overtopping, wetting/drying,
lake-at-rest balance, and acceptable travel time at representative depths,
widths, and eighth-level slopes. A one-unit same-bed gradient lies in the old
wet-neighbor dead band, but this does not prove every generated sloping-bed river
is immobile. The review's 137-z estimate is illustrative, not a validated limit.

The kernel is shared by rivers, lakes, ponds, player-placed water, and lava.
This is an independently gated **all-fluid kernel replacement**, not a
river-only edit. Introduce an explicit transport policy per compatible fluid
family: water types may share parameters; lava has separately reviewed
resistance and momentum-memory behavior and must not inherit water's tuning.
Unlike contact still follows accounted reaction/solidification rules.
Kernel approval includes these other fluids and its flow-state migration.
It also requires conservative edits against the latest simulation state before
the kernel ships: preserve pre-edit volume for bed changes, account explicit
sources/sinks/reactions once, and never reseed a whole chunk from stale
`wsTilesRef`. Reconcile neighboring transfers through the edit boundary before
changing revisions. Compact promotion later extends this same protocol; it
cannot be the first point at which edits become conservative. Q-20 gates the
displacement and gameplay completion policy; isolated kernel tests may precede it.

Candidate experiment: a depth-aware inertial link rule with damped discharge.
For a face use connected bed/barrier sill `z_s`, heads `H`, wet area `A`,
distance `L`, previous accepted discharge `q`, and damping time `tau`:

```text
wetHead  = maximum free-surface head among sides that actually contain water
wetDepth = max(0, wetHead - sill)            # zero if neither side is wet
A        = width * wetDepth                 # rectangular prototype only
qTrial   = (qOld + gEffective*A*dt*(Ha-Hb)/L) / (1 + dt/tau)
dVTrial  = qTrial * dt
```

A closed barrier or zero overtopping depth permits no transfer and clears
inapplicable momentum across that face. A wet donor **can** flow into a dry
receiver. A dry cell's high bed is not a water source; after determining the
trial direction, require actual donor storage and cap accepted outflow.
With inertia, trial direction can differ from the instantaneous head gradient,
so "the higher side is always the donor" is not a sufficient rule.
Test a dry high bank, overtopped sill, advancing front, reversal, and an emptied
donor with nonzero prior discharge.

This is a game-tuned local-inertial/virtual-pipe candidate with linear damping,
not an implementation of LISFLOOD-FP. Compare it with a depth-dependent friction
variant before selection; for example replace the denominator with
`1 + gEffective*n^2*dt*abs(qOld)/(A*R^(4/3))` for positive wet area, where `R`
is hydraulic radius and `n` is a consistently scaled roughness parameter.
This formula is a candidate adaptation, not a validation claim.

[Bates, Horritt & Fewtrell (2010)](https://doi.org/10.1016/j.jhydrol.2010.03.027)
is the local-inertial reference.
[De Almeida et al. (2012)](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2011WR011570)
analyzes low-friction instability and stabilized alternatives. Compare
wet/dry-front behavior and low-friction cases; a CFL bound is necessary but not
a complete stability argument. Do not transplant coefficients or assume the
papers validate our integer-volume, reaction, and mixed-resolution adaptations.

`gEffective` is explicitly a calibrated game-space/game-time coefficient.
At scale 1.0 a 100-ms wall interval represents six game-seconds. Neither `9.81`
nor a six-second explicit step is valid by assertion: physical gravity would
require a defined spatial conversion and suitable substeps. Time compression
alone does not mathematically prohibit physical units, but it changes how much
numerical work a wall interval must cover.

Use the same constitutive family and consistent geometry at both resolutions.
This does not make them agree by construction: coarse cross-sections, distance,
and omitted within-segment storage still change the result. Compare resolution
refinement and coarse/detail interfaces before accepting the model.

Proposed update protocol for every substep:

1. Apply admitted edits and explicit environmental quantities at the defined
   step boundary; freeze heads, volumes, link state, and topology.
2. Enumerate every physical face once, including interior, seams, and
   compact/detail ports; compute requests from that same snapshot.
3. For each donor, proportionally scale its full outgoing request set to
   available water. Convert shares to integer units with largest-remainder
   allocation, ties in its local N/E/S/W order (compact ports use a documented
   relative geometric order). Never sort absolute wrapped coordinates.
   Retain fractional conversion remainders, not denied-flow obligations.
   Check proposed receiver totals with wide arithmetic. Normal supported
   geometry should not hit the storage ceiling; prove that bound. If an input
   exceeds it, reject/limit incoming amounts by the same local proportional
   rule, leaving rejected water with donors. Do not redistribute rejected
   shares again this step. Never narrow unchecked or assert-and-lose water.
4. Commit accepted transfers once with equal donor debit/receiver credit.
   Preserve reaction outcomes and topology changes atomically; incompatible
   fluids route through the reaction path described below.
5. Update accepted discharge/state, ledger, and render observations. Rejected
   quantities remain with the donor.

A snapshot alone is insufficient: allocation, rounding, reaction conflicts, and
update scheduling must also be independent of chunk placement. Local compass
rounding preserves translation/wrap behavior, not rotational symmetry; test both
ordinary and cylindrical seams, multi-donor destinations, and reaction conflicts. Determine a
stability bound for `dt`; compare halved steps and shorter segments. Positivity
limiting is not proof of stability. Necessary numerical substeps are distinct
from the owner's deferred skipping/interpolation optimization.

### P-4. Geometry edits and stable topology

Keep generated beds as geography; changing hydraulic state must not erase them.
A dam in the middle of a segment can separate two storage regions. Prepare a
topology delta from composed terrain plus durable edits, split its geometry and
water by connected volume, invalidate affected links/catchment routes, and
publish the topology and ownership change together.

Removing a dam reconnects regions. It need not immediately merge their IDs:
physical reconnection and optional representation coalescing are different.
Only merge when the compact representation can preserve quantities, pools,
and flow information. Avoid ID churn for every camera move.

Use a page-local monotonic ID allocator and revisioned mappings; never reuse an
ID while a queued exchange can reference it. New child IDs, retired mappings,
allocation state, and persistent geometry edits must be restored coherently.
At retirement, detail must export topology/distribution changes, not just one
water total. Ledger entries against stale topology are rejected/reconciled
without silently losing their donor water.

### P-5. Flooding, route discovery, and incompatible fluids

Candidate route representation: a hierarchy of connected basins and spill
points built from deterministic terrain summaries. Store storage curves, spill
elevations, adjacent basins, and conservative bounds identifying where detail
is needed. Rising water fills a basin and crosses its spill connections into
other regions, including off-channel terrain.

Per-chunk storage summaries from P-2 contribute to this merge tree; its active
connected compartments change as levels cross internal sills. They support
partial-lake materialization without absent-terrain reads only where shared-head
connectivity holds. Test draining through a sill during camera movement and
save/load, preserving both resulting pools and their separate ownership.

The summaries must match final composed terrain/carves and durable edits.
A local edit can change a remote watershed; local recomputation is an
optimization, not a guarantee. Invalidate dependent ancestors/connections and
measure affected-region rebuild cost. Do not assume an absent summary means flat
terrain or an open drain.

Bound resident detailed patches, graph nodes, refinement depth, queued route
work, and basin-summary memory separately. Growing a reservoir activates basin
records without allocating all its detailed chunks. Unknown geometry cannot be
used to publish invented water; the overload/admission behavior is a rollout
gate, not permission to erase storage at a hard cap.

Underground water needs a separate detailed representation for multiple
connected vertical spans, plus rendering, edits, and save changes. A vertical
port in this graph is preparation for that arc, not its implementation.

For lava, query available generated pool/terrain summaries before admitting a
new path. Route contact through a bounded detailed reaction patch with the
existing accounted reaction/solidification semantics. A pending patch does not
grant permission to push unreacted water into lava. Admission failure is an
explicit unresolved route/work dependency; broad rollout is blocked until it
has acceptable timing behavior. Surface-only water fixtures can exclude lava
and caves, but production cannot assume those contacts are impossible.

### P-6. Gameplay clock and representation handoff

Implement hydraulic time coordination before production coupling. The previous
document's claim of an existing coordinated fluid clock was incorrect.

Under D-33, #2478 defines the shared grant/clock authority: the unit worker owns
pacing, permits, and completion; its resumable Lua driver requests finite native
phases. GT-12 publishes calendar/page conversion, GT-13 owns fluid cadence and
acknowledgment, and GT-14 owns clock persistence. `PageGameplayProgress` is a
projection of those grants, not a competing world-thread clock. River adapters
reconcile hydraulic substeps/remainders with that contract and complete every
accepted exchange before acknowledging the granted phase. The background arc
handles only residual all-page eligibility/catch-up gaps. Sim consumes intervals
with counted fixed-size steps and a retained remainder. Completion records both
logical progress and accepted exchanges; sleeping after work must not define
elapsed hydraulic time. Calendar editing must not replay or rewind physical
history: use elapsed simulation progress, not differences of mutable dates.

Persist hydraulic progress/remainders and any genuinely accepted pending work
at the existing save barrier. Rebuild process timing/deadline state on load;
paused wall time adds no hydraulic debt. Thread scheduling and renderer time
remain transient. Use capabilities and the existing save/load publication fence.

D-19/D-20 require an explicit page-lifecycle change: hiding a page stops detail,
not its shared calendar or compact water progression. Coordinate publication
with the world-time owner and inventory calendar/time consumers, including
climate, crops/flora, unit needs and schedules, and ongoing work. Assign each a
bounded simulation or catch-up policy before exposing hidden-page progression;
changing the visible-only calendar loop alone is insufficient. Handle page
creation/destruction, pause, speed changes, save/load, and incarnation
replacement. River-only clock/kernel experiments need not implement every
consumer, but integrated all-page rollout depends on this shared timing work.

Interim integration rule: until that background support lands, hidden pages
accrue no new calendar or hydraulic elapsed time in river-only test modes.
Complete only already-admitted intervals, checkpoint/demote detail, and freeze
water with page time. Returning does not add hidden wall time as catch-up debt.
Do not disable an existing GT-prepared hidden gameplay participant merely
because the camera moves: production membership must satisfy GT and D-19/D-20
together before enabling this backend for those pages. This limited
rollout does not satisfy final D-19/D-20; all-page acceptance stays blocked until
shared progression is enabled coherently. Test hide/show mid-interval and pause
to prove no water-only clock or duplicated interval exists.

Proposed loading protocol:

1. Prepare terrain from base plus terrain-only edits while the compact owner
   continues. Do not replay fluid placements/snapshots or terrain-edit water
   side effects; attach current hydraulic state after preparation.
2. At a common completed hydraulic interval, validate edit/topology/ownership
   revisions and reconstruct from the latest state, not load-request time.
3. Reconstruct both storage and discharge/profile information. A flat lake at
   rest must stay at rest; a steady river must carry its current boundary flow.
   Volume matching alone can cause a startup pulse.
4. Reconcile all interface transactions through that interval, then atomically
   transfer ownership and publish detail. Retry stale preparations.
5. On retirement, project accepted detail, flow, and topology back into compact
   state; acknowledge final writeback before releasing reservations.

No visible load-induced surge is an explicit criterion, with numerical bounds
below. Do not force a genuinely draining reach into equilibrium to hide a pulse.
If a coarse state cannot reconstruct acceptably, retain richer reconstruction
data/refine it rather than discarding the error.

A steady-flow demotion certificate uses near-stable storage/head, balanced
inflow/outflow, reconstructible distribution, and no unresolved edits/reactions.
It does not require zero discharge. Transfer to compact authority through
an acknowledged deactivation path satisfying CRS-5's intended contract. Q-19
must supply that path and its durable checkpoint owner before production use;
the pending CRS specifications are not existing implementations.

#### Demotion while water is moving

Page hide and eviction cannot wait for the steady-flow certificate. Provide
a second, conservative projection path at a completed hydraulic interval:
partition by connected compartments, transfer exact volumes and pending accepted
obligations, project face discharge into compact link/profile state, and retain
subregion residuals for pools/fronts the coarse representation cannot express.

This projection may lose small-scale velocity detail and within-region wave
shape, **not** volume, barriers, fluid identity, or disconnected pool membership.
A subsequent load may have the separately bounded transient proposed below.
If that error bound cannot be met, keep a sparse geometry/flow residual instead
of silently discarding detail. Ordinary ponds need this path too; they need not
already belong to a generated river graph.

Reserve projection/checkpoint capacity when promoting so memory-pressure
retirement cannot depend on an allocation it is guaranteed to lack. Page
visibility changes need not wait for physical equilibrium; retirement work may
finish asynchronously while its owned state is retained. Release detailed
terrain reservations only after acknowledged projection, and measure the
temporary overlap, completion latency, residual growth, and all-page evolution
cost. There is no claim that arbitrary unresolved detail can always be reduced
to constant space; unsupported cases block rollout under Q-6.

### P-7. Initial state, experiments, and cost control

D-18/D-21 require coherent initial flow without numerical startup surges, while
allowing climate-unsustainable generated lakes and their downstream supply to
decline during play. Preserve those lakes' generated initial storage. Compare
calibration of conveyance/initial discharge with bounded compact settling of
hydraulic profiles; neither may consume the lake's intended drying history
before publication or alter rainfall to hold its level fixed. A global spin-up
to climate equilibrium is therefore not the default for these lakes.

If spin-up changes water at new-world publication, it is a worldgen-output
change: run the full worldgen tier, regenerate baselines, and implement the
required save/component versioning and migrations. The same applies to
calibration that changes published generated water.

Spin-up must have convergence criteria, time/work bounds, and an honest failure
result; it must not generate every detailed river chunk. Initialize link flow
as well as volume. Persistent runtime saves resume their state without spin-up;
legacy initialization is a separate migration problem.

Start with fully known lake/channel/dam/diversion terrain and explicit climate
fixtures. Prove the clock and flow kernel independently, then coupling, edits,
partial lakes, persistence, and generated-page startup. Reuse the retained
characterization evidence as regression motivation, not the physical oracle.

Use unboxed structure-of-arrays storage for production compact stepping:
volumes, heads, endpoint indices, geometry coefficients, and flow/remainder
arrays. Stable-ID lookup maps may exist at edit/serialization boundaries, not
as a mandatory boxed HashMap traversal for every link. A scalar reference model
can remain simple. Measure allocation/GC, dirty publication, geometry rebuild,
save/projection size, and link-update count as well as kernel time.

First measure compact cost across graph size, disturbance size, and page count.
A future event-driven scheduler for exactly unchanged states is conceptually
different from skipping dynamic steps and interpolating them. It is not yet
adopted: a steady segment with throughflow still owes transfers, and all sources,
climate changes, backwater, and edits need wake/advancement rules. If measurements
show it is necessary, propose it explicitly rather than quietly overriding D-15.
Also evaluate conservative multirate stepping as a separate candidate: coarse
regions may use longer stable intervals while detailed faces substep. Keep
bounded interface flux registers, reserve donor budget across substeps, and
reconcile at shared boundaries so both sides apply exactly the same integrated
exchange once. Edits and promotion/demotion synchronize through those boundaries;
persist unfinished accepted interval obligations. A long segment is not
automatically safe: depth, conductance, topology, and coupled forcing set limits.
Derive and test the bound (the local-inertial scale `L/sqrt(g*h)` is only a
starting estimate). No skipped unaccounted evolution or visual interpolation
substitutes for this integration. Selection needs evidence; it is not yet an
approved change to the initial single-rate prototype.

Do not claim 512+ worlds or arbitrary accepted game speeds are supported before
measurement. The clock's representation-safe speed limit is not a performance
budget.

Before rollout, report generated-lake time-to-empty and associated river
time-to-dry across canonical seeds at speed 1.0, using calibrated climate and
initial storage. Include distributions, lake/river identities, measurement
horizon, and never-dry/not-yet-dry cases; distinguish temporary dry intervals
from permanent loss of supply. Report game time and wall-time equivalents to
the owner. This is an experience metric, not an invented pass/fail threshold
or authority to tune rainfall to prolong lakes.

### P-8. Climate, catchments, and ice

Proposed interval balance:

```text
surface storage change = routed inflow + local catchment runoff + direct lake rain
                       + connected meltwater + accepted aquifer extraction
                       - lake evaporation - accepted outflow
```

Terms are quantities over one gameplay interval. Upstream runoff is counted
where it enters, then routed; do not count it again in a downstream lake's
catchment. Separate land losses from evaporation over current lake wet area.
Cap losses at available storage and preserve conversion remainders.
Allow negative net climate balance; do not clamp every lake to positive filling.
Aquifer extraction is D-30's dig transaction, not climate recharge; its debit
cancels the surface credit in the combined aquifer-plus-surface ledger.

One environmental-budget owner computes unique source/sink transactions and
dispatches them to the current compact or detailed owner of each footprint.
Apply them once per interval. A half-loaded lake must not receive rainfall twice
or cease evaporating on its detailed side. Rejected sinks debit only accepted
water; disposition of a source exceeding local representation capacity requires
storage/spill handling, not silent truncation.

Build catchment summaries during generation while routing arrays exist.
Recommend persisting the compact authoritative summaries and edit revisions,
with exact schema classification to be reviewed. Raw direction/accumulation
arrays are not carried by the current river table. Regeneration from seed and
edits is an alternative whose time/memory cost and algorithm-version behavior
must be measured; it is not free or already implemented.

Climate regions are not drainage basins. Weight climate by contributing area;
nearby ice across a divide contributes nothing. `rcEvaporation` is available but
not exposed by the current local lookup. The generator's scaled formulas and
unit comments do not establish conversion to runtime eighth-volume units.

For ice, distinguish actual finite snow/ice storage from a prescribed external
melt input. The former must be debited; the latter must be named honestly and
approved as an approximation. Do not double-credit snow as rainfall and melt.
Investigate existing glacier geometry/temperature before enlarging the first
slice into a glacier simulation.

### P-9. Dedicated solid-fill build order

D-29/D-31 define a four-`granite_chunk` recipe per tile-column z-level on wet
or dry ground. Build contiguous layers upward from solid terrain toward a fixed
designated height; water changes must not move that target or create extra work.
Each acknowledged layer charges once. Revalidate access, occupancy, and hydraulic
revisions before commit; do not presume underwater workers or unlimited reach.
Specify exact worker stance/reach and work cost before filing this feature slice.

Persist target, material/payment state, and command identity under the existing
job/save contracts. Concurrent edits revalidate remaining layers; cancellation
does not remove committed fill and refunds only eligible uncommitted material.
Preview, quoted cost, and actual completion must agree about remaining height.

Art inventory for the accepted granite recipe:

| Presentation | Existing candidate or delivery gate |
|---|---|
| Material/picker icon | `assets/textures/items/material/granite_chunk.png`; `scripts/build_tool.lua` already reuses content textures. Verify picker legibility. |
| Completed terrain | `assets/textures/world/granite/granite.png`, plus existing granite zoom/background textures in `data/materials/igneous_intrusive.yaml`; use terrain rendering, not a structure overlay. |
| Cursor/designation preview | Adapt the target-art ghost convention to the actual proposed terrain height. `docs/designation_tools_design.md` specifies runtime opacity factors; translucency alone does not require a new PNG. Wet occlusion, depth order, invalid-state feedback, and pending-job visibility need rendered evidence. |
| Paid/in-progress work | Specify a visible progress presentation using approved existing UI/art or inventory missing assets before delivery. Structure construction-frame behavior is not automatically a terrain-fill implementation. |

These are reuse candidates, not completed visual signoff. Missing icons, preview
art, or progress assets block the fill feature until the owner chooses supply;
new assets require their own issues/PRs and owner signoff under repository rules.
Do not use placeholders or silently omit a needed presentation. Existing engine
ghost support does not prove correct wet-terrain rendering.

## Verification strategy

[Appendix B](#appendix-b-acceptance-targets) records the initial targets accepted
under D-32 and identifies the transient-profile limits still awaiting measurement.

Prototype cadence candidate: a logical interval equal to 100 ms at the declared
reference game speed, with its game-time duration recorded explicitly. Page
scales use game-minutes per real-second; do not confuse a 100-ms service interval
with 0.1 game-seconds. Run half-step/refined-segment comparisons before selecting
a production step, allowing bounded numerical substeps where stability requires.

Required examples: lake-fed straight reach, shallow gradient, raised sill,
dam closure/removal, tributary, side diversion, trapped pool, wrapped seam,
partial lake, ocean-connected outlet, and lava contact. Record exact quantities,
heads, candidate/accepted exchanges, owner revisions, and refusal reasons.

Add dam-crest displacement/spill in both directions and filling the last wet
cell of a pond, including a thin film spreading onto formerly dry banks. Check
exact conservation, representation limits, and subsequent drainage. Include
both in rendered signoff: fill intentionally lifts the water under D-26 and
can send some downstream; it must not look like duplicated or teleporting water.

Vary camera/load order, terrain-preparation delay, speed, page visibility,
edits during handoff, and save/restart during drainage/topology changes.
No always-detailed reference is trusted until its own conservation, rest-state,
throughflow, and partition checks pass. Rendered signoff remains required for
surges and water appearance; headless results alone do not prove them.

For an external numerical reference,
[Clawpack's shallow-water solver documentation](https://www.clawpack.org/v5.10.x/riemann/Shallow_water_Riemann_solvers.html)
distinguishes bathymetry balance and dry-state support. These are separate
properties to test; the virtual-pipe candidate above is not a Clawpack method.

### Existing validation affected by the kernel

The independently gated kernel/migration slice must run targeted groups
`Sim.Fluid.Seam`, `Sim.Fluid.Conservation`, `Sim.Fluid.Exact`,
`unlike-fluid reaction`, `solidification`, `sim chunk admission`, and the
fluid writeback staleness/incarnation and persistence-contract examples.
These cover #2481 reactions/events and #2520 exact activation/bake semantics.
Use the real group names in `test-headless/Spec.hs`; do not assume a file name
is an hspec selector.

Integration evidence includes `tools/fluid_reaction_probe.py`,
`tools/fluid_reaction_visual_probe.py`, `tools/canteen_instance_probe.py`,
and affected save/barrier/round-trip probes. Surface publication changes also
exercise `World.Render.FluidLevels`, `World.Slope.FaceMaps`,
`World.Spoil`, and `tools/test_tillable_fluid_filter.py`, plus pathing/water-AI
fixtures at newly wet/dry tiles.

Separate old **rate-specific** expectations from invariants before updating
tests. Exact quantity, no overflow, reaction ratios/stone commits, epoch fences,
and save continuity remain strict; do not rebaseline them away. Replace
quarter-difference timing expectations only against the approved candidate law,
retain before/after observations, and add face-flow bake/save/demotion tests.
A changed generated publication additionally triggers the full worldgen tier;
do not run that tier merely for documentation changes.

## Persistence and integration constraints

Snapshot one coherent state: volumes, accepted link-flow state, physical
topology/allocator revisions, hydraulic progress, catchment/climate remainders,
regional aquifer balances/extraction watermarks, and any pending accepted
transfer intent. Rebuild render caches and process
timers. Do not persist queues as substitutes for their durable obligations.

Map the candidate state onto the existing owner/capability inventory before
implementation. Q-19 selects the prerequisite owner and sequencing. Align with
CRS-5/CRS-12's intended retirement and sparse-state contracts, with one migration
and one authority, not parallel hydraulic and streaming balances.

### Separate replayed terrain from evolving fluid

Today's durable water is in the ordered `world-edits` log, not a separate
hydraulic component. Migration must evaluate the supported legacy replay
semantics in order against the correct generated base: terrain changes,
`WeSetFluidTile` replacement, exact set snapshots, and explicit clear snapshots.
Do not simply collect all fluid entries after applying final terrain: ordering
and bed changes affect the result. Preserve the final legacy surface/type,
exact volume over its corresponding bed, and required sub-terrain metadata,
including snapshots for nonresident chunks. Historical flow remains unknown;
do not fabricate a history of source quantities from replacement snapshots.

Publish the migrated hydraulic checkpoint and a terrain-only replay stream
atomically with a migration version/watermark. Preserve frozen legacy codecs;
removing operations from the new replay stream does not mean deleting old enum
constructors. New terrain replay must exclude automatic fluid changes in
`WeAddTile`/`WeDeleteTile`, not only the three fluid-specific constructors.
New-world generated water seeds hydraulic authority once; chunk regeneration
thereafter attaches current authority rather than generated water or old edits.

Propose admitting future fluid placement as a command-ID-deduplicated source
transaction, not a replayed instruction. Specify quantity and unlike-fluid
reaction policy before replacing today's one-level *replacement* semantics;
any deliberate replacement/removal needs an explicit accounted sink too.
Future dig-exposed groundwater debits D-28's regional store in the same
transaction that credits surface water. Persist the balance under D-25's
component and classify it **Persist exactly** in the inventory. Legacy logs do
not establish historical aquifer depletion: preserve their existing surface
water and explicitly select initial remaining groundwater under Q-22, rather
than claiming it can be recovered from snapshots or silently refilling it.

The selected component must cover off-cache modified chunks, volume/flow,
pending accepted obligations, and migration completion independently of cache
membership. Stop `appendFluidSnapshot` as the runtime save producer when that
owner becomes authoritative. Candidate migration is eager before hydraulic
publication: enumerate every affected legacy chunk, regenerate its required
base, fold its ordered edits, and seed the compact/checkpoint authority before
any page advances. Include chunks with terrain edits that changed water even
without an explicit fluid record. Process in bounded batches and discard scratch
terrain after each contribution; do not require simultaneous residency.
Camera-triggered lazy migration is excluded: unexplored saved drains and
placements must already affect remote flow.

Before accepting migration for rollout, inventory affected-chunk counts and
edit counts in the largest available real saves, and measure full migration
time, peak memory, and compact-seeding cost on the reference machine. Report
each page and the entire session, including hidden pages: total touched chunks
and work add across pages. Measure actual session peak memory, including retained
migrated state and load-barrier overlap, not the sum of isolated page peaks.
No page advances before the entire replacement session is seeded. Include
synthetic scaling cases when the real corpus is small. Report the corpus and
results to the owner and agree a load-time/work envelope; none is measured yet.
If the gate fails, revise migration or explicitly withhold compatibility for
review rather than starting with incomplete compact state. Migration failure
leaves the old session and save intact under the existing load transaction.

Required fixtures: interleaved fill/dig/place/set/clear edits; partial eighth
surfaces; absent chunks with saved snapshots; repeated regeneration and saves
after drainage; and failed/retried migration. Compare the first converted state
with the real supported legacy replay, then prove old entries never resurrect
water. Race edits with queued writebacks and cross-chunk transfers: quantity
and flow must survive, rather than merely rejecting stale publications.

Follow `docs/persistence_contract.md`, `docs/persistence_state_inventory.md`,
`docs/engineenv_capability_inventory.md` section 6.4, and `src/World/Save/CLAUDE.md`.
Use explicit component migrations and frozen legacy shapes, not just a version
bump. Save/load preserves state, not deterministic future replay. Existing
time-scale reset behavior after load remains unless separately revised.

Worldgen-output changes need the full worldgen tier, baselines, and migration
checks from `src/World/CLAUDE.md`. Preserve reaction commit guarantees, canonical
wrapped coordinates, page-incarnation fences, and edit-generation fences.
Code, contracts, evidence, and migration fixtures travel together in each PR.

## Open questions and prerequisite gates

### Q-1. Must a dam or diversion affect unloaded reaches?

Resolved by D-4; page scope is subsequently resolved by D-19.

### Q-2. Does the first implementation include erosion?

Resolved by D-7: deferred.

### Q-3. How should a supply change propagate?

Resolved at behavior level by D-9/D-10; numerical rates are measured under P-3/P-7.

### Q-4. Does this arc include remote underground diversion?

Resolved by D-12. Multiple-span representation, renderer/save changes, and
cave-interface handling remain a future integration gate, not a small graph add-on.

### Q-5. What feeds rivers and which reservoirs are finite?

Resolved by D-13/D-14/D-16. Catchment and melt mechanics remain Q-10.

### Q-6. What happens when work exceeds budget?

D-15 sets the experience; no silent slowdown or invented completed evolution.
Measure P-7's envelope and propose explicit admission/degradation behavior before
broad rollout. A bounded reaction/discovery queue does not by itself solve this.

### Q-7. How do legacy saves enter the new model?

Audit ordered set/clear fluid snapshots and placements in `world-edits`, plus
their interleaved terrain edits, before deriving an initial graph. The migration
above must preserve their final legacy result for resident and absent chunks.
Do not refill all lakes or spin up an already dynamic save. Document unavoidable
unknown history and obtain owner acceptance of any visible migration adjustment.
Preserve all supported legacy decoding. Blocks production migration.

### Q-8. Which candidate method and errors are acceptable?

D-32 accepts the initial numeric gates, except provisional transient-profile
bounds. Retain P-3 candidate comparisons, errors, stability/cost results, and
visual evidence. Accept the method and measured transient limits before
coupling becomes production behavior; initial target approval is not that proof.

### Q-9. How do existing streaming and fluid arcs integrate?

Refresh CRS-5/CRS-12, eighth-level work, and current owners before allocating
child PRs. Agree any required revisions to their contracts. Continuous flow
requires an acknowledged projection/deactivation path, not physical quiescence.
CRS-5 is unprocessed and CRS-12 deferred, not usable services; D-25 resolves
the delivery dependency and the overlapping fluid-log migration ownership.

### Q-10. How are climate and ice inputs converted?

Set catchment membership, units, sampling cadence, and snow/melt accounting.
Choose persisted compact summaries versus measured regeneration. Desired ice
support cannot be silently omitted or claimed implemented by geological events.

### Q-11. Should water follow game speed?

Resolved by D-17. The new counted hydraulic clock is a prerequisite; it does
not exist merely because a calendar or save barrier exists.

### Q-12. Should new worlds start near equilibrium?

Resolved by D-18 as qualified by D-21. Select coherent hydraulic initialization
from evidence; preserve the intended in-play drying of climate-unsustainable
lakes instead of spinning that history away before publication.

### Q-13. Do hidden pages continue?

Resolved by D-19. Coordinate page time/climate publication and lifecycle;
measure all-page compact cost before claiming the performance target.

### Q-14. What happens to #2533 and where does this work ship?

Recommend a separate runtime arc, retaining #2533's worldgen-only specification.
Do not fold runtime work into its acceptance criteria or close it with an
unrelated implementation. The owner has authorized this isolated worktree for
experimentation; its branch name does not define the final issue/PR scope.
Recommend landing #2533's specified eighth-level generation change before
final generated-world equilibrium calibration/spin-up baselines. Clock/kernel
and authored-arena experiments can proceed independently. If the generation
baseline changes afterward, rerun calibration and its full validation rather
than assuming compatibility. This sequencing is proposed, not a tracker action.
Before publication, choose #2533's disposition and the runtime delivery branch.
Keep the evidence/harness with their owning runtime work. No assignment,
tracker, branch, or worktree move has been made by this review revision.

### Q-15. What does elapsed time mean on hidden pages?

Behavior resolved by D-20: advance shared page time and handle time-dependent
gameplay coherently. Inventory consumers and agree bounded background/catch-up
mechanisms as a shared prerequisite; calendar advance alone does not execute
them. This blocks integrated all-page rollout, not river-only fixtures.
Q-21/D-27 establish the separate background-simulation design and delivery arc.

### Q-16. What happens to climate-unsustainable generated lakes?

Resolved by D-21: let them dry during play under the climate balance; springs
are future work. The earlier proposal to shrink/omit these lakes to force
initial climate equilibrium is superseded. Existing saves remain subject to
their separately reviewed migration policy, not automatic erasure/refill.

### Q-17. What speed and reference hardware define the performance gate?

Resolved by D-22: game speed 1.0 and pause on Apple M3 Max, 64 GiB RAM.
Higher console/test speeds exist even though no fast-forward UI was found in
the reviewed pause flow. Measure them separately until their supported policy
is chosen; do not silently clamp speed or declare all valid API values performant.
Record OS/build profile, detailed workload, page/link counts, thermal conditions,
and measurement procedure with the results. Hardware identification is not a
benchmark result.
The accepted hydraulic budget excludes non-water hidden-page gameplay costs;
shared catch-up needs its own measurements and an integrated whole-game budget.
That exclusion is not evidence that all-page gameplay meets either budget.

### Q-18. Which groundwater and winter interactions belong in the first model?

Resolved by D-23/D-24: defer riverbed infiltration/seepage, dynamic winter
freezing, and seasonal snowpack storage. Retain the existing climate-based water
table and use baseline climate supply for the first slice. Glacier/ice-runoff
accounting remains Q-10. Future seasonal integration requires explicit storage,
timing, and exchange rules; fixed-climate hydraulic tests can proceed without it.

### Q-19. Who supplies durable hydraulic state before the kernel swap?

Resolved by D-25: this arc supplies one durable hydraulic component and legacy
fluid-log migration, with CRS-5/CRS-12 later reusing it.
Update their specifications together before implementation; no duplicate
migration or competing checkpoint authority. Waiting for Arc B or pulling its
bundle prerequisites forward is not the selected approach. This gates
production persistence/kernel delivery, not pure probes.

### Q-20. How are dams built and wet terrain edits committed?

Dam mechanic resolved by D-26: ordinary fill with conserved displacement. The
spoil wet-tile restriction is not a universal construction prohibition, and
debug fill is not a playable dam mechanic.
The player action is resolved by D-29: a dedicated solid-fill build order with
material costs and wet-tile support. Existing spoil-disposal rules stay unchanged.

Ordinary fill preserves the latest water quantity, initially raising its surface
with the bed, then redistributing via the solver. No implicit deletion at
complete fill. Representation overflow or newly disconnected storage needs
accounted projection/admission handling, not clipping. D-30 resolves excavation:
preserve existing quantity, then account any finite aquifer top-up toward the
water table, including wet beds. Dedicated controllable barriers
remain later work with height/overtopping/open-state semantics to define.
Specify the chosen gameplay path and the narrowly scoped job acknowledgment
protocol in P-2 before rollout. Groundwater supply is finite under D-28/Q-22;
a source named once per dig is not an acceptable substitute.
Future fluid-placement amount/replacement behavior also needs approval; D-23
does not by itself settle it.

### Q-21. Should shared hidden-page catch-up have a separate design arc?

Resolved by D-27, preserving D-20's behavior. The separate
[background-simulation draft](background_simulation_design.md) covers shared
calendar, consumer catch-up, and whole-game cost. It gates all-page river
rollout, not the counted clock, kernel, or controlled single-page coupling.
No GitHub epic has been created.

### Q-22. What bounds dug-up groundwater and does it refill initially?

Behavior resolved by D-28/D-30: finite regional storage, dig-triggered top-up
for wet and dry columns, and no recharge initially;
the owner accepted that drained wells do not refill. Specify initial budget,
stable region membership, extraction amount, and legacy initialization before
implementation. Region IDs must not reset with camera movement or river-topology
edits. This is storage accounting, not a groundwater-flow/seepage model.

Extraction must atomically debit only the amount actually credited to the
surface, capped by the water-table deficit, remaining budget and admitted capacity.
Test deeper digging in already-wet wells and river/lake beds below the water
table, with both sufficient and exhausted aquifers. Also test surfaces already
above the water table (no top-up or drain), no refill while idle, and multiple
columns drawing from one region, exhaustion, dig/drain/dig and fill/dig cycles,
retried/canceled commands, chunk reload, and save/load. All preserve the combined
aquifer-plus-surface quantity except separately declared sources/sinks; no idle
refill or budget reset. These parameter/migration gates precede shipping the
kernel's groundwater edit integration.

### Q-23. How does this arc reuse the existing coordinated-timing work?

The slicing-time tracker audit found [#2478](https://github.com/coghex/synarchy/issues/2478),
whose authority is `docs/gameplay_timing_design.md` and
`docs/gameplay_timing_protocol.md`. Its GT-1 pure accounting/protocol (#2482)
has landed; production scheduling is still legacy. GT-4/GT-12/GT-13/GT-14 own
world/fluid service, calendar, fluid acknowledgment, and clock persistence.
The timing design records owner acceptance of its phase graph on 2026-09-16;
the epic body's older graph-acceptance warning is not the latest design state.

Resolved by D-33: reuse those contracts and implementations. `PageGameplayProgress`
is a page projection of granted shared time, not a second wall-time authority.
Keep isolated river experiments independent; gate production clock integration
on the relevant timing adapters and activation. Audit the background draft for
residual all-page membership/catch-up requirements, not duplicate consumer
migrations. The earlier river-owned clock proposal is superseded.

## Delivery plan

The approved plan contains **29 delivery slices**, grouped into five milestones.
The detailed scopes, dependencies, acceptance signals and stop/ask gates are in
[Appendix C](#appendix-c-delivery-slices).

| Milestone | Slices | Observable result |
|---|---|---|
| A. Trustworthy local water | RVR-01–RVR-10 | Measured solver, durable state/import, conservative edits, and a GT integration adapter. |
| B. First complete river experiment | RVR-11–RVR-18 | Close/divert/reopen an authored river, unload/reload mid-drain, and save/restart without creating or deleting water. |
| C. Player-built dams | RVR-19–RVR-20 | Build wet/dry solid fill with paid materials and reviewed visuals. |
| D. Generated-world hydrology | RVR-21–RVR-26 | Catchments, finite climate budgets, basin routing, lava admission, and coherent generated/legacy initialization. |
| E. Integrated production | RVR-27–RVR-29 | Measured workload/error envelope, coherent all-page progression, and one activated backend. |

Start with RVR-01 and RVR-02. Do not make those experiments wait for the timing,
background or streaming epics. RVR-10 is a separate integration branch:
the controlled river milestone can use explicit-duration isolated fixtures
without it. A live coordinated-gameplay demonstration requires that adapter.

**Approved delivery policy:** keep new runtime behavior in a clearly identified
internal test backend until RVR-29. Each earlier PR has real tests and durable
format/version rules where applicable; an internal switch does not excuse
broken saves or running two authorities on the same water. Test fixtures may
supply controlled grants/reservations, but that is not evidence that production
GT/CRS integration works. Existing gameplay remains on its current backend.

**External owners:** #2478 supplies GT timing and activation; #1997 supplies
residency/reservation capabilities. D-25 assigns hydraulic durability/migration
here for CRS reuse; coordinate companion specifications before their related
children are filed. #2533 retains its worldgen-only scope and precedes final
generated calibration. #2535 owns existing exact-level diagnostics/compatibility,
and #2698 owns reusable arena scenarios/tags. Reuse them rather than duplicate
their features; neither diagnostic nor scenario completion blocks pure solver
experiments. The background draft supplies only residual requirements after
GT coverage is audited.

Each slice's code, required contracts, migration fixtures, measurements and owner
evidence ship together. Production-path proofs accompany each integration;
RVR-29 is final activation, not the first conservation/save test. A slice that
cannot fit a reviewable PR must return here for a named split before filing.

The M3 Max hydraulic gate does not replace GT/CRS minimum-machine and whole-game
budgets. No river-only slowdown or skipped admitted work is allowed; reconcile
overload with GT's shared progression contract and the measured Q-6 envelope.
The owner accepted these delivery boundaries with the stated later-resolution
gates under D-34. A material change to scope or boundaries returns the affected
design to review before further issue processing.

Readiness tracker check on 2026-09-24 found no duplicate open river-runtime
umbrella. Adjacent #2478, #1997, #2514/#2533/#2535 and #2698 retain the ownership
boundaries above. Repeat targeted deduplication when drafting each artifact.

## Review history

Five external review passes shaped the clock, flow/persistence ownership,
conservative edit protocol, and migration gates above. Current decisions and
open questions are authoritative; earlier disposition tables are retired.
The original solver characterization and evidence remain unchanged. No runtime
or save-format implementation is claimed by these design revisions.

## Appendix A. Current code and retained evidence

Evidence is pinned to `8caf3c99b6acc947468be63fcca68f40e1aa583a`. Refresh affected
owners against the implementation base before filing issues or writing code.
The third-review save/edit/construction and CRS audit also checked local
`master`/`origin/master` at `e3c781c77bf3534e5ab1204f6e5ab073f53c5d37`.

| Concern | Verified code and implication |
|---|---|
| Fluid clock | `Sim.Thread.simTick` performs work then `threadDelay ssTickRate`; default delay is 100000 microseconds. `SimSetTickRate` has a constructor/handler but no sender found under `src`/`scripts`/`test-headless`. No hydraulic progress counter exists in `SimState`. Fluid observes pause but not page speed. |
| Existing world time | `World.Thread.Time.tickWorldTime` advances fractional page time/date using game-minutes per real-second, currently only for `wmVisible` pages. There is a world clock; there is no coordinated clock feeding fluid. Hidden calendar behavior must be reconciled with D-19. |
| Transfer law/order | `Sim.Fluid.Active` runs per-chunk gravity, lateral, waterfall, dry-out phases, then a live, sorted seam pass. Interior snapshots differ by phase; seam requests see later states. Changing only the formula cannot remove partition effects. |
| Capacity/dead band | Equal-bed wet neighbors need a difference greater than one eighth-unit and request roughly a quarter of that difference. Dry interior neighbors have a further volume threshold. Bed-downhill rules differ. No sustained-flow capacity is established. |
| Representation | `World.Fluid.Internal.FluidMap` is one optional cell per column; `SimChunkState` has one terrain top per column. Passive sub-terrain cells can be preserved, but the active solver cannot represent independently moving stacked water spans. |
| Activation | New chunks start inactive. Settle countdown alone does not activate volume simulation. Missing/inactive neighbors do not exchange water. `equilThreshold = 200` counts ticks with no transfers reported; throughflow generally prevents it. |
| Ocean/reactions | Ocean water is ordinary finite volume in the active solver, not a fixed-level reservoir. Existing `Sim.Fluid.Reaction` handles lava/water locally. `World.Magma.Pool` generates surface lava pools; original water barriers do not rule out later diverted contact. |
| Generation | `World.Fluid.River.Identify` builds direction/accumulation arrays locally. `WorldRivers` retains river metadata, chunk coverage/surfaces, and carve deltas, not those routing arrays. `GeoTimeline` retains generated fluid tables, not a reusable catchment graph. |
| Climate | `RegionClimate` stores populated precipitation, evaporation, temperature, and snow fraction. `LocalClimate` omits evaporation; river generation uses a different temperature/humidity proxy. `ClimateBuilder` initializes `csSurface = HM.empty` despite declared runoff/snowpack fields. |
| Ice/lakes | Geological `Glacier.Evolution`/`Spawn` are not runtime meltwater accounting. `Lake` holds generated surface/floor/area/bounds, not finite runtime storage or a full volume-level curve. |
| Persistence/residency | `docs/chunk_residency_streaming_design.md` specifies future work: CRS-5 is unprocessed; CRS-12 is deferred behind Arc B's memory gate and depends on CRS-5/CRS-11. Neither is an implemented checkpoint service. Migration ownership/sequencing is Q-19. |
| Fluid durability today | `World.Thread.Command.Save.WriteWorld.appendFluidSnapshot` replaces resident chunks' prior snapshots with exact set/clear entries, retaining absent chunks' snapshots. `World.Edit.Apply` replays these and `WeSetFluidTile` with terrain edits. They must seed migration once, never override subsequent hydraulic state. |
| Live edit accounting | `WeAddTile` retains a surviving surface or deletes its fluid; either can lose quantity. `WeDeleteTile` retains an existing surface as the bed falls (adding volume) or reveals climate-derived groundwater. `Edit.Sync.syncEditToSim`/`Sim.Chunk.applyChunkEdit` reseed from published tiles, potentially discarding newer sim transfers. Fences prevent stale writes, not that rollback. |
| Dam construction | `World.Spoil.Logic.spoilTileOk` forbids wet spoil destinations. Debug `handleWorldAddTileCommand` has no wet-column prohibition. Ordinary `World.Construct.Plan` places structure overlays using the rendered surface; this does not establish solid fill or hydraulic barriers. Playable dams need the explicit mechanic in Q-20. |

Completed experiments remain under `docs/evidence/river-runtime/`. The real-library
harness `tools/river_runtime/Characterize.hs` used ten fixed ticks; every observed
tick conserved exact initial volume.

| Example | Initial units | After 10 ticks |
|---|---:|---:|
| Raised sill, interior or seam | 24 / 0 | 24 / 0 |
| Downhill control | 24 / 0 | 8 / 16 |
| One-level water, interior | 8 / 0 | 8 / 0 |
| Same physical example, seam | 8 / 0 | 4 / 4 |
| Inactive or absent neighboring chunk | 24 / 0 | 24 / 0 |
| Active neighbor control | 24 / 0 | 12 / 12 |

Two headless arena runs retain recipes, observations, manifests, and socket
transcripts in `arena-v1`/`arena-v2`. In v2 the diversion wetted five then eight
cells after opening; the former dam tile wetted after removal but farther
downstream stayed dry during the three-second observation. These finite-charge,
all-resident tests do not demonstrate sustained flow, accurate tick timing,
coarse coupling, or rendered quality. The baseline engine built successfully;
no production candidate solver has been implemented.

## Appendix B. Acceptance targets

The owner accepted these initial targets in D-32 except the explicitly
provisional transient-profile bounds. These are gates, not passed measurements
or thresholds to tune after failures.

| Check | Accepted initial target unless marked provisional |
|---|---|
| Water accounting | Zero unexplained integer-unit discrepancy every committed step and handoff, including explicit climate/ocean/reaction terms. |
| Barrier and ownership | Zero flow through a closed impermeable barrier; zero duplicate ownership/transactions; zero negative/overflowed storage. |
| Partition translation | Identical detailed state and accepted exchanges for translated fixtures after canonical coordinate normalization, including junction allocation/reactions. |
| Lake at rest | No accepted transfer or surface drift in a closed, equal-head lake without forcing. |
| Steady river capacity | Keep widths 1/3/8, depths 1/8/32 eighth-units, flows 1/4/8 units per reference interval as small diagnostic cases. After climate conversion, derive rated flows from generated catchments, including the measured maximum discharge and a 2x maximum stress case at corresponding geometry. Supported cases need no secular pileup and mean output within 5% of input. Report the corpus/rate envelope; small fixtures alone cannot pass rollout. |
| Steady profile | At matched measurement times over the union of both wet extents: 95th-percentile equivalent surface error at most 1/8 z, maximum 1/4 z; wet boundaries within one tile. Use terrain elevation as the zero-depth comparison on a dry side; report wet/dry disagreement separately. |
| Transient profile | Initial proposal: 95th percentile at most 1/2 z, maximum 1 z, wet-front distance at most two tiles over the same union. Measure draining/wave cases before approving these provisional bounds; retained disconnected pools/barrier errors are never excused by a percentile. |
| Timing | Arrival and drainage milestones within max(10% of reference duration, one reference interval); record both gameplay and real-time measurements at the same speed. |
| Load transient | Steady fixtures: at most 1/8 z added peak deviation versus continuous control. Moving-state/forced-demotion fixtures: provisional transient-profile bounds and accepted timing bound above, with transition-only excess reported separately from accumulated coarse error. No reset to equilibrium or zero flow. |
| Repeated transitions | 100 load/unload cycles; no volume drift, growing pulse, forgotten barrier, or duplicated climate/ocean exchange. |
| Runtime budget | Accepted initial target: aggregate hydraulic work at most 5 ms p95 / 10 ms p99 per 100-ms service interval at the approved game speed 1.0 on Apple M3 Max / 64 GiB (D-22); no growing logical-time backlog over ten minutes. Test 1k/10k/100k **total** links distributed over 1/4/16 pages, plus a separate stress matrix up to 100k links **per page**. Report detailed-cell count and substeps; no 1.6M-link guarantee is implied. |
| New-world publication | Sustainable steady fixtures meet steady-flow/profile targets over 100 subsequent fixed-climate reference intervals. D-21 lakes retain generated initial storage and show only accounted drying under the transient targets, not a numerical startup pulse. Report init time and memory separately. |

## Appendix C. Delivery slices

The scope boundaries below are approved for issue processing under D-34, not
filed issues or implementation authorization. RVR IDs are stable; process the
epic and then one child at a time with separate tracker-creation approvals.
Appendix B's accepted
invariants apply throughout; unresolved limits remain explicit stop gates.

### RVR-01. Build the controlled hydraulic experiment harness

- **Outcome and scope:** Retain the characterization evidence and add authored channels, reservoirs, edits, explicit-duration stepping, exact ledger/flow diagnostics, and reference comparisons. Reuse scenario tooling when available; keep the existing client/nc path usable without it.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** none. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-6, D-8, D-11.
- **Acceptance signals:** Reproduce the current seam/dead-band results; prove fixture quantities and barriers independently of the candidate solver. Archive reproducible inputs and outputs.
- **Out of scope:** Production scheduling, new public diagnostics already owned by #2535, and a duplicate scenario framework.
- **Open questions / stop gate:** None; explicit grants in isolated fixtures do not choose a production clock.

### RVR-02. Select and verify the detailed flow law

- **Outcome and scope:** Compare P-3 candidates in the harness, including per-fluid policies, dry fronts, donor allocation, wrapped faces, reactions, stability and rate scaling. Retain method/parameter evidence and owner verdict in this PR.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-01. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-6, D-15, D-32.
- **Acceptance signals:** Appendix B rest-state, conservation, translation and diagnostic-throughflow cases pass; document convergence and water/lava behavior. Rated generated discharge is verified later in RVR-26.
- **Out of scope:** Runtime activation, compact coupling and generated-world calibration.
- **Open questions / stop gate:** Q-8: stop before dependent solver implementation if no candidate passes or method approval is absent.

### RVR-03. Define the durable hydraulic component and transaction model

- **Outcome and scope:** Implement versioned volume/face-flow/remainder checkpoints, IDs/revisions, transfer deduplication and exact accounting as a dormant codec/model. Classify state and specify future compact extensions. Align CRS-5/CRS-12 contracts so this is their sole durable owner.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-02. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-25.
- **Acceptance signals:** Real-codec round trips preserve exact state and rejected/duplicate transaction behavior; frozen DTOs and inventory checks pass.
- **Out of scope:** Changing normal saves or introducing another clock; graph geometry arrives later.
- **Open questions / stop gate:** Q-9: resolve companion CRS ownership before filing; unknown legacy flow policy is gated in RVR-04.

### RVR-04. Implement and measure eager legacy fluid import

- **Outcome and scope:** Fold supported ordered terrain/fluid logs into staged checkpoints, preserve absent-chunk and sub-terrain data, and construct terrain-only replay. Measure affected chunks, load time and memory per page and whole session. Aquifer initialization consumes RVR-05's policy at publication.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-01, RVR-03. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-10, D-25.
- **Acceptance signals:** Legacy replay and staged results agree; retries/failures preserve old saves. Owner accepts missing-flow initialization and measured migration envelope before this import is enabled.
- **Out of scope:** Publishing an incomplete migrated session, camera-triggered lazy migration, and reinitializing existing surface water.
- **Open questions / stop gate:** Q-7: historical flow/profile fallback and cost envelope must be accepted in this slice; publication also waits for RVR-05.

### RVR-05. Implement finite aquifer accounting and excavation transfers

- **Outcome and scope:** Specify stable region membership, initial quantities and legacy initialization, then implement D-30 top-up as an exact transfer. Use authored budgets first and version the selected generated/legacy policy.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-03. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-23, D-28, D-30.
- **Acceptance signals:** Wet wells and river/lake dredging draw only the available deficit; exhaustion, multiple columns, fill/dig cycles and codec round trips never recreate budget.
- **Out of scope:** Recharge, groundwater flow and seepage; climate-derived water-table eligibility remains.
- **Open questions / stop gate:** Q-22: region/budget initialization requires owner acceptance before this implementation's issue is approved; no invented default.

### RVR-06. Make wet terrain edits conservative and acknowledged

- **Outcome and scope:** Implement scoped edit command identity, latest-state reconciliation, fill displacement, excavation extraction and explicit fluid-placement accounting in the test backend. Serialize conflicting footprints and commit/refuse once without stale chunk reseeding.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-03, RVR-05. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-26, D-28, D-30.
- **Acceptance signals:** Race edits with transfers/writebacks/cancellation; combined water and aquifer totals remain exact. Unrelated dry/cosmetic edits retain their ordinary path.
- **Out of scope:** New player build UI, blanket job delays, and duplication of CRS-6 reservations.
- **Open questions / stop gate:** Q-20: settle wet-edit job acknowledgment and fluid-placement semantics before issue approval. Reuse CRS-6's real reservation contract or jointly scope its required prerequisite; fixtures may use an explicit test reservation provider.

### RVR-07. Implement the selected all-fluid kernel

- **Outcome and scope:** Build the detailed snapshot/request/commit kernel with unique face flow, fractional remainders, per-fluid policy, reaction integration and ordinary/wrapped seams. Expose it only to controlled tests initially.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-02, RVR-03. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-6, D-32.
- **Acceptance signals:** Existing exact/conservation/reaction invariants and the selected-method fixtures pass; rate-specific changes have justified baselines. Bake/restore does not reset flow.
- **Out of scope:** Default backend switch and coarse river ownership.
- **Open questions / stop gate:** Q-8 method gate from RVR-02; failure of a strict invariant blocks integration rather than rebaselining it.

### RVR-08. Add the accounted fixed-sea-level boundary

- **Outcome and scope:** Implement connected ocean ports with external supply/sink entries, preserving isolated coastal storage and preventing duplicate clamping/exchange.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-03, RVR-07. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-14.
- **Acceptance signals:** Inflow, a coastal hole, and closing/reopening a barrier hold the connected head while every exchange is accounted; isolated water stays finite.
- **Out of scope:** Tides, waves and arbitrary clamping by fluid label.
- **Open questions / stop gate:** None beyond selected kernel/ledger contracts.

### RVR-09. Wire coherent hydraulic checkpoints into save and reconstruction

- **Outcome and scope:** Connect the test backend's live owner, world-thread checkpoint mirror, staged load, terrain-only reconstruction and fresh-process saves. Replace edit-log fluid snapshots only for sessions using this versioned backend.
- **Phase / ordering:** A; dependency-driven within milestone A.
- **Depends on:** RVR-04, RVR-05, RVR-06, RVR-07, RVR-08. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-10, D-25.
- **Acceptance signals:** Save/restart preserves volume, discharge, aquifers and pending accepted obligations; migrated multi-page saves seed fully before advancement. Legacy normal sessions remain on their existing path until activation.
- **Out of scope:** Unacknowledged eviction, whole-river loading and default production activation.
- **Open questions / stop gate:** Q-7/Q-22 policies must be resolved; new-backend test saves are explicitly identified and cannot be silently read by the legacy backend.

### RVR-10. Integrate hydraulic advancement with GT grants

- **Outcome and scope:** Connect the river backend to GT's existing grant, page-time conversion, fluid completion and persistent cadence interfaces. Implement only river-specific adaptation; reconcile the P-2 edit commit with GT's forward phase ordering.
- **Phase / ordering:** A; parallel integration branch; not a prerequisite for isolated RVR-18 experiments.
- **Depends on:** RVR-09. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-17, D-20, D-33.
- **Acceptance signals:** Delayed/duplicate grants, pause, save/load, edits and refused writebacks never advance water twice or acknowledge early. Fluid substeps consume exactly the granted page duration.
- **Out of scope:** Another coordinator, wall-time accumulator, or reimplementation of GT adapters.
- **Open questions / stop gate:** External GT-4/GT-6/GT-12/GT-13/GT-14 implementations are required for integration. If GT-13 already delivers this exact adapter, disposition this slice against that artifact rather than file a duplicate. GT-16 is a production gate.

### RVR-11. Implement compact finite storage and routed flow

- **Outcome and scope:** Implement stable reach/link storage, the selected flow law, checked wide quantities, saved flow/remainders and unboxed stepping arrays over authored graphs.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-03, RVR-07. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-3, D-4, D-9, D-11.
- **Acceptance signals:** Finite lake-fed reaches, tributaries, backwater and delayed drainage conserve water and match refined reference cases within accepted or explicitly provisional targets.
- **Out of scope:** Generated catchments, dynamic topology and detailed ownership transfer.
- **Open questions / stop gate:** Q-8: compare compact resolution/substeps; obtain transient-bound acceptance before broad coupling acceptance.

### RVR-12. Implement basin merge trees and additive storage curves

- **Outcome and scope:** Build level-dependent compartments, spill thresholds and per-chunk volume curves from authored terrain. Support remainder aggregation without reading absent detailed chunks.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-03, RVR-11. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-3, D-5, D-10.
- **Acceptance signals:** A partly loaded basin drains through an internal sill into separate pools with exact aggregate quantities; member insertion/removal and wrap translations preserve geometry.
- **Out of scope:** Whole-world summary generation and live terrain editing.
- **Open questions / stop gate:** None; terrain summaries are fully known in this slice.

### RVR-13. Apply terrain changes to compact topology

- **Outcome and scope:** Turn admitted edits into stable split/reconnect mappings, geometry revisions and conservative storage/flow remapping on known terrain. Persist allocators and retired-reference handling.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-06, RVR-11, RVR-12. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-4, D-5, D-10.
- **Acceptance signals:** Mid-reach dam/diversion/removal, disconnected pools and save/restore preserve quantities and pending transfers; removing a dam need not merge IDs.
- **Out of scope:** Unbounded off-network discovery and underground routing.
- **Open questions / stop gate:** Q-6: retain explicit refusal for work outside the authored fixture envelope; generated discovery is RVR-25.

### RVR-14. Implement shared compact/detail interfaces

- **Outcome and scope:** Create ownership cuts at actual detailed footprints, finite head-aware partial-lake exchange, interface epochs, and face-to-link flow handoff.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-09, RVR-11, RVR-12, RVR-13. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-3, D-5, D-10.
- **Acceptance signals:** One face has one owner/value through both promotion orders, wrapped seams and save/restart; donor reservation and exchange settlement never duplicate water.
- **Out of scope:** Visible reconstruction, autonomous eviction and climate forcing.
- **Open questions / stop gate:** Q-8: unresolved transient errors block general-use acceptance, not strict conservation tests.

### RVR-15. Reconstruct and promote current compact water

- **Outcome and scope:** Prepare terrain-only chunks, reconcile at a completed interval, reconstruct quantity plus flow/profile, and atomically transfer ownership with stale-preparation retries.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-14. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-9, D-10, D-11, D-32.
- **Acceptance signals:** Loading during drainage shows current partial water; steady loads meet the accepted pulse/profile gates; edits during preparation cannot resurrect old water.
- **Out of scope:** Forced demotion and player build orders.
- **Open questions / stop gate:** Q-8: retain/refine insufficient profiles; do not force equilibrium to pass a load test.

### RVR-16. Demote steady and moving water safely

- **Outcome and scope:** Implement steady certificates and forced conservative projection, sparse residuals and checkpoint headroom. Integrate acknowledged release through the real residency API.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-15. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-3, D-5, D-10.
- **Acceptance signals:** Steady throughflow and a moving pond can retire without volume/flow reset; pressure, cancellation and save during retirement preserve ownership. Measure residual growth and overlap memory.
- **Out of scope:** Changing residency hard ceilings or waiting for physical equilibrium.
- **Open questions / stop gate:** External CRS-3/CRS-5 capabilities and Q-6 admission/space policy gate real eviction; jointly agree any prerequisite extraction instead of claiming pending CRS code exists.

### RVR-17. Publish compact surfaces and promote affected edits

- **Outcome and scope:** Publish loaded compact-owned surfaces through the world thread to render and gameplay queries. Connect scoped edit promotion, reservations and final revalidation.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-06, RVR-15, RVR-16. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-10, D-15, D-26.
- **Acceptance signals:** Water AI/pathing/tilling see the same wetness as rendering; overlapping edits serialize, disjoint work proceeds, and admission/response delays are measured.
- **Out of scope:** New swimming behavior, blanket delays for all edits and new fill UI.
- **Open questions / stop gate:** Q-20 and actual CRS-6 edit reservations must be resolved; test-only providers are not production evidence.

### RVR-18. Prove the end-to-end arena river milestone

- **Outcome and scope:** Assemble an authored lake/reach/dam/side-diversion experiment with partial loading, forced retirement, edit races and fresh-process persistence. Add rendered comparisons and retain quantitative evidence.
- **Phase / ordering:** B; dependency-driven within milestone B.
- **Depends on:** RVR-17. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-8, D-9, D-10, D-11, D-32.
- **Acceptance signals:** Closing/diverting/restoring flow and moving the camera mid-drain meet accounting, timing and steady-load targets; owner reviews measured transient limits and visuals.
- **Out of scope:** Generated worlds, gameplay fill payment and default activation.
- **Open questions / stop gate:** May use explicit-duration isolated fixtures independently of GT. Live coordinated mode additionally needs RVR-10. Reuse #2698 scenario capabilities if landed; do not duplicate them.

### RVR-19. Implement the solid-fill job and material transaction

- **Outcome and scope:** Add the durable fixed-height order, granite recipe, per-layer work/commit, stance/access validation, payment reservation, cancellation/refund and acknowledgment. Expose initially through test APIs.
- **Phase / ordering:** C; dependency-driven within milestone C.
- **Depends on:** RVR-06, RVR-17, RVR-18. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-26, D-29, D-31.
- **Acceptance signals:** Four granite chunks buy exactly one committed tile-z; retries do not double-pay, water changes do not move target height, and a blocked next layer does not erase completed fill.
- **Out of scope:** Picker/preview artwork, dedicated floodgates and wet spoil disposal.
- **Open questions / stop gate:** Q-20/P-9: work cost, stance/reach, interrupted-job semantics and existing construction interfaces must be specified before issue approval; reuse GT work-credit contracts.

### RVR-20. Expose and visually validate the fill build order

- **Outcome and scope:** Integrate picker, fixed-height selection, cost/validity feedback, target preview and paid-work presentation using P-9's approved art inventory. Include existing cancellation/order UI behavior.
- **Phase / ordering:** C; dependency-driven within milestone C.
- **Depends on:** RVR-19. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-26, D-29, D-31.
- **Acceptance signals:** Player completes wet and dry dams; crest spill and last-cell fill look coherent; quoted and consumed costs agree. Owner visual evidence lands in this PR.
- **Out of scope:** New assets without their own approved supply/asset issues and signoff.
- **Open questions / stop gate:** Missing preview/progress art blocks this slice, not RVR-18. Confirm all required asset contracts and minimum supported UI sizes before filing.

### RVR-21. Define climate units and persist contributing-area summaries

- **Outcome and scope:** Calibrate rainfall/evaporation conversion and catchment membership; persist authoritative summaries while generation's routing data exists, using actual source units and geometry revisions.
- **Phase / ordering:** D; independent of player-fill UI once listed prerequisites pass.
- **Depends on:** RVR-11. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-13, D-16, D-23, D-24.
- **Acceptance signals:** Catchment areas/weights explain rated discharges and independent tributary supply without rainfall double counting; codec and regeneration/version behavior are explicit.
- **Out of scope:** Surface-flow activation, dynamic snowpack, recharge and infinite lake sources.
- **Open questions / stop gate:** Q-10: approve unit conversion, summary persistence and initial ice-runoff disposition before issue approval; worldgen-output changes require the full tier.

### RVR-22. Apply climate budgets to the current water owner

- **Outcome and scope:** Generate unique interval source/sink transactions for baseline lake rain, catchment runoff and wet-area evaporation. Dispatch to compact/detail ownership, including partial lakes and any explicitly approved melt input.
- **Phase / ordering:** D; dependency-driven within milestone D.
- **Depends on:** RVR-14, RVR-21. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-16, D-21, D-24.
- **Acceptance signals:** Moving ownership boundaries neither duplicate nor omit forcing; finite lakes can dry; rejected sinks preserve remainders and accepted-source capacity is handled.
- **Out of scope:** New winter mechanics, undocumented melt sources and springs.
- **Open questions / stop gate:** Q-10 glacier/ice accounting must be explicitly resolved or separately deferred by the owner before this slice is approved.

### RVR-23. Build generated river and basin summaries

- **Outcome and scope:** Adapt the authored summary builders to deterministic final terrain, existing river/lake identities, chunk geometry and off-channel basin membership; persist/version required data.
- **Phase / ordering:** D; dependency-driven within milestone D.
- **Depends on:** RVR-12, RVR-13, RVR-21. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-3, D-13, D-16.
- **Acceptance signals:** Representative generated rivers/lakes map to finite storage and valid interfaces without loading their whole detailed footprint. Measure build size/time and preserve terrain contracts.
- **Out of scope:** Live unknown-route discovery and equilibrium tuning.
- **Open questions / stop gate:** Q-14 final #2533 terrain/surface baseline must be known; full worldgen/migration tier when published output changes.

### RVR-24. Admit compact routes that encounter lava

- **Outcome and scope:** Use generated magma/contact summaries to require bounded detailed reaction admission, carrying accepted quantities and reaction outcomes back into compact topology.
- **Phase / ordering:** D; dependency-driven within milestone D.
- **Depends on:** RVR-07, RVR-16, RVR-23. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-5, D-6, D-12.
- **Acceptance signals:** An unloaded diversion into lava cannot advance as water-only flow; admission failures retain water and work identity; reaction/solidification contracts still pass.
- **Out of scope:** Underground spans, new reaction chemistry and silently ignoring magma.
- **Open questions / stop gate:** Q-6 reaction-patch capacity/refusal policy must be accepted before rollout; coordinate existing reaction transaction owners.

### RVR-25. Discover off-network spills and update edited basins

- **Outcome and scope:** Integrate fill-and-spill route activation, downstream dependency invalidation and bounded discovery/refinement queues after terrain edits or rising reservoirs.
- **Phase / ordering:** D; dependency-driven within milestone D.
- **Depends on:** RVR-13, RVR-17, RVR-23, RVR-24. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-4, D-7, D-12.
- **Acceptance signals:** Large dam backwater finds valid off-channel routes and later draining pools; unknown geometry does not become a free drain. Report rebuild cost, node growth and refusal behavior.
- **Out of scope:** Erosion, underground flow and unbounded detail promotion.
- **Open questions / stop gate:** Q-6: set measured admission/space limits and obtain owner policy acceptance before broad generated-world use.

### RVR-26. Initialize generated worlds and legacy graphs coherently

- **Outcome and scope:** Seed generated compact state and legacy-import graphs with coherent flow/profile. Compare bounded initialization methods while preserving D-21 lakes' intended in-play drying; integrate the final #2533 baseline.
- **Phase / ordering:** D; dependency-driven within milestone D.
- **Depends on:** RVR-04, RVR-05, RVR-18, RVR-22, RVR-25. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-10, D-18, D-21, D-25.
- **Acceptance signals:** Canonical-seed startup has no numerical pulse; report lake/river drying distributions and per-page/session migration cost. Save restarts resume rather than spin up.
- **Out of scope:** Invented springs, deletion of unsustainable lakes and replaying saved drainage history.
- **Open questions / stop gate:** Q-7/Q-10/Q-14/Q-22 initialization policies and #2533 delivery disposition resolved before approval; published worldgen changes require full gates.

### RVR-27. Measure the supported hydraulic workload and error envelope

- **Outcome and scope:** Run rated generated-flow, camera/edit/save and 1/4/16-page benchmark matrices; include allocation, residual growth, publication and ledger costs. Compare single-rate and proposed multirate only as evidence demands.
- **Phase / ordering:** E; dependency-driven within milestone E.
- **Depends on:** RVR-18, RVR-20, RVR-26. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-15, D-22, D-32.
- **Acceptance signals:** Accepted steady/timing/budget gates pass at speed 1.0 on the M3 Max; report stress limits, transient evidence and whole-session memory. Owner accepts final transient limits and supported workload.
- **Out of scope:** Quietly loosening gates, guaranteeing 1.6M links, or claiming the repository minimum machine from M3 measurements.
- **Open questions / stop gate:** Q-6/Q-8: failed capacity/error results return to design before activation; material scheduling changes require explicit approval and their own bounded implementation work.

### RVR-28. Integrate all-existing-page water progression

- **Outcome and scope:** Audit GT-prepared pages versus all existing pages and deliver only river-specific registration/publication changes after residual background policies are implemented. Honor common pause/speed/lifecycle behavior.
- **Phase / ordering:** E; dependency-driven within milestone E.
- **Depends on:** RVR-10, RVR-27. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-19, D-20, D-27, D-33.
- **Acceptance signals:** Hidden water and calendar advance coherently; hide/show during drainage, page lifecycle and multiworld saves preserve progress without duplicate catch-up. Measure aggregate water and separate whole-game cost.
- **Out of scope:** Duplicate GT consumer migrations and a water-only hidden-page clock.
- **Open questions / stop gate:** External GT production eligibility plus accepted/implemented residual background work are required; do not treat an open design as an implementation dependency satisfied.

### RVR-29. Activate the river backend and retire obsolete fluid paths

- **Outcome and scope:** Enable the approved backend in ordinary sessions, select the durable migration path, and remove conflicting legacy reseeding/snapshot/stepping paths. Refresh engine/persistence/hydrology contracts and rollout evidence together.
- **Phase / ordering:** E; dependency-driven within milestone E.
- **Depends on:** RVR-09, RVR-10, RVR-20, RVR-26, RVR-27, RVR-28. External capability gates are stated below; they mean landed behavior, not tracker status.
- **Relevant decisions:** D-2, D-5, D-25, D-33.
- **Acceptance signals:** Normal boot, edit, fluid reactions, save/restart, generated worlds and all-page gates use one owner/clock. Targeted production-path checks and owner evidence pass before review.
- **Out of scope:** New algorithm development, unresolved assets or leaving required rollout docs for later publication.
- **Open questions / stop gate:** External GT-16 and real CRS reservation/retirement capabilities must be active. Every relevant open gate above must be closed or explicitly scoped to a separately approved future feature.
