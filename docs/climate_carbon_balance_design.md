# Geological carbon balance and emergent climate design

Restore credible cold and temperate biomes by making geological carbon sources
and sinks balance through simulated processes. The owner wants temperature to
emerge naturally and accepts a longer implementation; a forced final temperature
or quick restoration profile is not the chosen direction.

Design state: `ready for issue processing`

Readiness reviewed on 2026-09-10 under the owner's authorization to mark the
document ready if the review passes. The observable outcome, approved scope,
compatibility and verification contracts are recorded. The twelve delivery
slices have distinct reviewable outcomes, follow their dependencies and match
the processing ledger. Deliberately open scientific and measurement contracts
have named owners and downstream stop conditions below. Readiness permits issue
processing; it does not claim that reference data, calibration or implementation
have already been validated. Material scope or slice-boundary changes require
renewed design review.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Derive world climate from a balanced geological carbon cycle
- [ ] CARBON-1. Account for existing carbon sources and geological elapsed time
- [ ] CARBON-8. Specify and package physical reference closures
- [ ] CARBON-9. Implement the reduced carbonate chemistry solver
- [ ] CARBON-10. Define geological source and reaction-rate kernels
- [ ] CARBON-2. Implement and verify a carbon-balance model in isolation
- [ ] CARBON-7. Couple surface albedo and greenhouse forcing in a thermal model
- [ ] CARBON-6. Model primordial cooling and ocean formation in isolation
- [ ] CARBON-11. Extract geological climate inputs and reactive-material supply
- [ ] CARBON-5. Exercise candidate carbon and climate through real world generation
- [ ] CARBON-4. Calibrate generated-world outcomes and gate biome regressions
- [ ] CARBON-12. Preserve legacy climate and persist the new model identity
- [ ] CARBON-3. Activate the calibrated model with verified save compatibility

## Epic contract

- **Goal:** CO2 and temperature respond to geological activity, weatherability,
  water availability, and elapsed time, with understandable feedbacks.
- **Done when:** carbon has an auditable budget; integration is insensitive to
  arbitrary numerical step subdivisions; ordinary-world outcomes satisfy the
  agreed climate policy; controlled extremes have explainable behavior; and
  regression tests cover the mechanism, final climate, and visible ice/snow.
- **Users and operators:** players generating worlds and developers tuning geology.
- **Arc label:** None proposed.

## Current state and evidence

Investigated on 2026-09-09 against master `83535fb525d0a00312ff6a19efad8fb6202cb643`.
The owner's running worktree was `9f92dc0b291784dd6404dba8a21c57e981895cb2`;
the inspected climate/ice/CO2 loop code matched master.

- The running world used seed **1334661219**, size **64**, and configuration
  plate count **10**, volcanic activity **1.25**, and the standard timeline
  shape (one Eon, two Eras, Period/Epoch/Age counts each in 1–3).
  Read-only console sampling of all 256 climate-region centers found annual
  means **27.9666–80.2050°C**, snow fraction **0 everywhere**, and sampled
  altitude-corrected ambient temperatures no lower than **24.7166°C**.
  A separate fresh headless generation on master reproduced these measurements.
- The current regional formula implies CO2 **8.66109 times baseline**, adding
  **45.96654°C**. This CO2 value was inferred from the temperature/seasonality
  formula, not read directly from the internal field. Recalculating the same
  sampled regional climate with baseline forcing gives **−17.9999–34.2385°C**,
  71 snow-eligible centers and 25 additional tundra-eligible centers. Those
  counts are climate eligibility, not generated terrain or rendered pixel counts.
- Seed 42 retains cold regions at size 64/3 plates and 128/10 plates. At
  256/17 plates it has 12.6374–66.3310°C regional means and no snow/tundra
  eligible sampled centers. The defect is not universal across inputs.
- Commit `f70bc5cfb` (2026-07-12, issue #785 / PR #791) changed final regional
  climate from baseline CO2 to the timeline's evolved CO2. This exposed the
  excessive warming while correctly fixing disagreement between the regional
  grid and its summary. Keep that consistency requirement.
- `src/World/Weather/Generate/ClimateBuilder.hs:131` uses
  `(globalCO2 - 1) * 6`, despite its comment describing warming per doubling.
  `solarConst` is stored but does not enter this builder's temperature equation.
- Albedo follow-up checked on 2026-09-10: `ClimateBuilder.hs:291–292` computes
  land/ocean reflectivity plus a snow-fraction adjustment and stores `rcAlbedo`.
  A source-wide consumer search found display/serialization uses but no feedback
  into generated temperature. `cpAlbedoFeedback` is declared/configured/persisted
  without a corresponding climate calculation; `csSurface` is built empty.
  Existing fields and surface-type comments are not evidence of active albedo
  feedback. The builder's `snowFrac` is also stored as precipitation snow fraction,
  so it cannot automatically stand in for a physically modeled snow-covered area.
- `src/World/Geology/Timeline/Loop.hs:239` adds a fixed CO2 quantity for each
  successful per-Age eruption roll. Eruption probability is per Age, not per
  million years; Age durations range approximately 1–15 Myr.
- `src/World/Geology/Timeline/Volcanism.hs:64` adds 0.01 per newly registered
  feature, including hydrothermal vents, rather than accounting for a defined
  quantity of degassed carbon. Birth and subsequent activity have separate
  additions whose physical meanings need documenting before changing amounts.
- `src/World/Geology/Hash.hs:79` scales feature counts by integer world-area
  ratio above size 128, with a minimum count below that size. The present global
  CO2 increment has no corresponding atmospheric-capacity scaling. This is a
  verified scaling mismatch, not proof that every change in world size should
  preserve climate: generated geography and source distributions also change.
- `src/World/Hydrology/Simulation/Flow.hs:141` floors each land cell's water
  contribution to one before river accumulation. That value cannot serve as
  physical liquid-water availability for weathering in dry or frozen regions.
- `ElevGrid` carries elevation, coordinates, and land flags, but no material or
  reactive-rock inventory. Supply-limited weathering therefore needs a named
  additional data source. `MaterialProps` likewise has no weathering reactivity
  field; mining hardness must not silently become chemical reactivity.
- `Loop.hs:321–327` removes CO2 using Age duration, global mean temperature,
  and average precipitation, then adds `0.2 * sin(currentDate * 0.4 * 2*pi)`
  directly to the carbon stock and applies a floor of 0.4. That sine addition
  is not a source/sink budget or a time-integrated orbital forcing.
- The clock also advances 500 Myr for bombardment, 100 per Era, 50 per volcanic
  formation Period, and 30 for emitted evolution Periods. Those paths do not
  run the Age weathering update. Resolve whether these are physical intervals
  or abstraction boundaries; do not silently integrate overlapping durations.
- The Age CO2 code contains a 0.15 supervolcano branch, but the filter requires
  an Age eruption profile while supervolcano profiles are Period-scale
  (`src/World/Geology/Types.hs`). The accounting inventory must cover actual
  reachable sources, not assume every written branch contributes.
- `src/World/Geology/Timeline.hs:146` builds ice levels from the timeline climate;
  `src/World/Thread/Command/Init.hs:259` later refines regional climate. Ice,
  vegetation, zoom output, and glacier evolution need a consumer audit. In
  particular, some glacier choices use raw CO2 as a temperature proxy
  (`Timeline/Helpers.hs:293` and the glacier evolution family).
- `src/World/Fluid/Ice.hs:67` forces ice at the world boundary independently of
  temperature, explaining the remaining ice walls.
- `test-headless/Test/Headless/World/Climate.hs` covers synthetic temperatures,
  finiteness, forcing response, and final wiring. It does not require a
  generated world's carbon balance or cold-biome outcomes to be credible.
- First tracker check: open-issue searches for climate and CO2 found no clearly
  overlapping carbon-cycle arc; results #2017 and #2480 concern map scaling
  and lava/water reactions.
- Readiness tracker check on 2026-09-10: open-title searches for `climate` and
  `carbon` returned no matches; the open `epic` title inventory contained no
  clearly overlapping umbrella. Per-child deduplication remains part of issue
  processing.

Reproduction evidence was captured under `/tmp/tundra-live-climate.json`,
`/tmp/tundra-counterfactual.json`, and `/tmp/tundra-reproduction-results.log`.
These are disposable local captures; the measurements and inputs above are the
durable record. No game state or implementation was changed in the investigation.

## Decisions

### D-1. Temperature must emerge from the model

The owner rejected a forced present-day temperature profile and chose to
rebalance geological CO2 accumulation. No per-world temperature normalization,
guaranteed polar override, or hidden reset to baseline as the restoration method.

### D-2. Prefer a durable redesign over a quick fix

The owner accepts the longer route. Establish the time/units contract, explain
the mechanism, and measure it before choosing final coefficients.

### D-3. Protect the outcome with tests

The owner requires regression protection after the correction. Proposed delivery
adds mechanism tests with the model and end-to-end outcome tests with integration
and calibration, so verification is not postponed until all implementation lands.

### D-4. Allow naturally extreme worlds

The owner accepted unusual greenhouse and heavily frozen worlds, with ordinary
settings usually producing habitable climates. Do not require tundra on every
seed, reroll an inconvenient result, or alter final temperature to pass a biome
check. D-8 and D-12 define the frequency target and initial land criterion.
D-29 narrows ordinary worlds to defaults with size/seed variation and accepts
the allowed outcome misses without additional severity tests.
Q-4 retains numerical limits and corpus details.

### D-5. Use the feedback-based redesign

The owner accepted consistent carbon/time accounting, geography-dependent
weathering, a logarithmic CO2 temperature response, removal of the unbudgeted
per-Age sine addition, and mechanism tests delivered alongside the model.
Detailed reservoir, compatibility, integration, and calibration choices remain
subject to the contracts and questions below. This is design-direction approval,
not permission to mark the design ready or publish tracker artifacts.

### D-6. Track atmospheric and ocean carbon separately

The owner explicitly chose two reservoirs and an exchange function, because
ocean uptake, release and transient atmospheric changes matter to the desired
simulation. This supersedes the proposed one-effective-reservoir approximation.
Track two carbon inventories in compatible units; atmospheric CO2 drives the
greenhouse response. Ocean chemistry determines the ocean-side exchange
potential. This does not yet choose surface/deep-ocean structure, carbonate
chemistry detail, or gameplay-time simulation; those are separate contracts.

### D-7. Preserve existing saves

The owner agreed with the recommendation to preserve existing worlds using
their stored climate and history wherever possible. Do not silently rebalance
them on load. New model fields require deliberate migration/default semantics
and reconstruction evidence; technical treatment is recorded under Q-3.

### D-8. Aim for approximately 90% ordinary-world habitability

The owner agreed with the recommendation that ordinary settings produce about
90% climatically habitable worlds. D-29 clarifies that ordinary worlds use default
settings with only size and seed varying. The owner assumes the allowed misses
are acceptable and explicitly declines additional tests of their severity.
This is an ensemble calibration target, never a per-seed correction. D-12/D-21
define the land criterion; corpus and statistical acceptance remain Q-4.

### D-9. Use carbonate chemistry and an explicit alkalinity budget

The owner approved a compact carbonate-chemistry calculation, including
alkalinity, so weathering changes the ocean's ability to hold carbon. Track
ocean dissolved inorganic carbon and total alkalinity inventories separately;
derive their concentrations from the chosen ocean volume. Alkalinity is chemical
state, not an additional carbon reservoir. A fixed-buffer or fixed-alkalinity
approximation is superseded by this decision.

Compute ocean-surface pCO2 from that chemical state and the specified temperature
and salinity inputs. Reaction stoichiometry, the chemical solver's domain,
initialization, temperature/salinity ownership and ocean-box interpretation still
need explicit contracts. Approval of this calculation does not implicitly approve
a full ocean ecosystem, more carbon boxes, or gameplay-time carbon evolution.

### D-10. Use one global ocean carbon reservoir

The owner approved one global ocean box, with a finite exchange rate and an
effective mixing approximation. There are exactly two carbon reservoirs in this
arc: atmosphere and ocean. Ocean DIC and alkalinity are global inventories;
concentrations use the specified ocean volume. Surface temperature, salinity
and exposed area still enter gas-exchange calculations, but separate surface
and deep-ocean carbon, alkalinity and transport are outside scope.

Calibrate the effective transfer rate against the behavior this approximation
can represent. Do not claim that it resolves a separate surface/deep mixing lag
or silently add a third reservoir to obtain one.

### D-11. Evolve the carbon cycle during world generation only

The owner approved geological-generation-only evolution for this arc. The
two-reservoir carbon/alkalinity state evolves across the geological timeline;
completed worlds publish their resulting climate. Gameplay ticks do not advance
this carbon cycle or produce ongoing atmospheric/ocean climate changes.

Transient eruption and exchange responses are observable in generation traces
and model tests. This decision does not disable existing gameplay systems such
as time, seasonal queries or runtime fluids; it adds no new live carbon owner.

### D-12. Require 20% suitable land for the initial habitability criterion

The owner approved the climate-suitability definition with an initial threshold
of **20%**, replacing the proposed 10%. At least 20% of non-boundary exposed
land must simultaneously have annual ambient temperature 0–30°C, a thawed
summer, and the precipitation criterion finalized by D-21. This measures climate suitability, not
guaranteed food, resources or a safe starting location. The ensemble target
remains approximately 90% of ordinary worlds (D-8).

The owner allows revisiting the land fraction later, including 10% as a possible
adjustment. Twenty percent is the current acceptance criterion; 10% is not an
automatic fallback when calibration fails. Record any later change explicitly
and rerun the declared calibration/validation corpus under the revised criterion.
D-21 resolves the former "usable water" ambiguity as annual precipitation.

### D-13. Keep world size climate-neutral by design and verify it empirically

The owner approved proportional scaling of carbon sources, reservoir capacity,
and weathering area as the design intent. World size must not select a different
temperature target, climate coefficient set, or corrective CO2 adjustment.
Geometry still enters through area, volume, exposure, and geological activity;
generated worlds at different sizes can therefore produce different climates.

Identical climates across generated sizes are not promised. The minimum product
requirement is roughly the same temperature scale, with systematic differences
due to map size small enough to go unnoticed in play. Establish this through
predeclared multi-size calibration and held-out validation, not an assumed
scaling proof. A controlled replicated-geography fixture can verify proportional
accounting, but cannot establish the outcome for independently generated worlds.
D-40/D-43 define practical validation: the bounded sample is reported by size,
with controlled accounting tests and no added statistical size-difference gate.

### D-14. Begin with primordial conditions and simulate their equilibration

The owner chose primordial starting conditions, including the earlier hot
molten/steam phase, rather than initializing after a liquid ocean already exists.
The transition toward equilibrium must arise from modeled processes. The
reference-start and post-condensation-start proposals are superseded.

This expands scope to an explicit approximation of early thermal evolution and
ocean formation. Initial heat and volatile inventories require justified values;
neither a scripted cooling curve nor resetting carbon to a habitable state meets
the decision. Keep the two carbon reservoirs in D-6/D-10; supporting thermal and
water-phase state is not an additional carbon reservoir. Interior degassing can
remain an explicitly budgeted external input rather than silently adding a third
carbon stock. D-35 defines the reduced atmospheric-composition scope.

Q-8 retains the reduced physical model, detailed timeline mapping and transition
conditions; D-15 resolves how equilibration fits the timeline. D-4's allowance for valid extreme worlds remains;
this decision does not guarantee that every world becomes temperate. The existing
liquid-ocean chemistry and logarithmic warming proposal must not be extrapolated
into primordial conditions outside their validated domains.

### D-15. Evolve primordial conditions inside the existing geological history

The owner clarified that climate state is carried and updated throughout the
existing geological timeline loop. There is no separate preliminary settling
run, no pause in geology while climate equilibrates, and no extension of world
history until a stability condition passes. Early cooling and later carbon-cycle
evolution belong to one continuous history, with state inherited at every step.

For the owner's example of a 20–40-period history, the first roughly two or three
periods should show the sharp decline from primordial conditions toward the
atmosphere that develops naturally, followed by continued geological/climate
evolution. This is an expected trajectory to calibrate and test for ordinary
worlds, not a temperature schedule or a branch on the period index. The example
does not change the configured timeline count. Rates use elapsed physical time;
subdividing identical history into more periods must not change its trajectory.

Tests must retain early and later climate/carbon snapshots, not only final
outputs. D-16 places bombardment before atmospheric initialization. Precise
early-transition acceptance bands and alignment of later Era and Period durations
remain Q-8/Q-4 work. Later events can perturb climate;
equilibration does not imply a permanent frozen climate or guaranteed temperate
outcomes for every world. The separate-settling interpretation is rejected.

### D-16. Complete pre-atmosphere bombardment before primordial initialization

The owner clarified that bombardment precedes the atmosphere in this world's
generation sequence. Complete that geological interval first, then initialize
primordial atmospheric/climate state once and carry it through subsequent
geological periods under D-15. The bombardment interval still contributes to
geological age, but its 500 Myr must not be integrated as atmospheric cooling,
air-sea exchange or atmosphere-dependent weathering time. This is the chosen
game-model chronology, not a claim that real impacts require an absent atmosphere.

Record the initialization boundary in the trace and distinguish total geological
age from elapsed time since atmospheric initialization. The owner further
specified that bombardment must have no noticeable effect on the starting
atmosphere. Do not derive its initial composition, pressure or thermal condition
from impact history. Bombardment-generated terrain remains available to later
geological/climate processes; this restriction concerns atmospheric initialization,
not a guarantee that different terrain produces identical subsequent climates.
Bombardment cannot implicitly consume the early atmospheric transition.
The expected first two or three cooling periods are after this
boundary. This is a one-time initialization, not an Eon-boundary climate reset.

### D-17. Route volcanic carbon by the source's current environment

The owner approved released carbon entering the atmosphere when the source is
exposed to air and entering ocean DIC when submerged in the liquid ocean.
Before liquid oceans exist, surface emissions enter the atmosphere. Determine
the destination from conditions at the source as geography changes, rather than
a permanent volcanic feature label. Each emission is counted once; subsequent
ocean-to-atmosphere movement occurs through the finite exchange function.

This is the agreed reduced routing model, not a simulation of underwater bubbles
or plume bypass. D-33 resolves lake-covered sources: credit their emissions to
the atmosphere, without treating lakes as part of the connected ocean reservoir.

### D-18. Transfer weathered carbon to the ocean and remove it through burial

The owner approved the weathering-to-ocean-to-sediment pathway. Land weathering
consumes atmospheric CO2 and delivers dissolved carbon and alkalinity to the
ocean; ocean chemistry determines partitioning and availability for exchange.
Carbonate formation and burial remove carbon into sediments outside the two
active reservoirs. Geological inputs and burial are recorded external fluxes;
this arc does not add an evolving sediment or mantle carbon reservoir.

Burial responds to chemistry and available material rather than deleting a fixed
fraction each Age or being set equal to emissions. Specify carbon and alkalinity
stoichiometry, and distinguish atmospheric carbon from any carbon supplied by
weathered carbonate rock. Weathering transfer and burial must not be counted as
two removals of the same carbon from the combined atmosphere-ocean budget.
Q-5 retains the reaction details, seafloor pathway and quantitative rate laws.

### D-19. Target about 66% precipitation coverage on ordinary worlds

The owner selected precipitation as the observable and revised the coverage
target from about 60% to about **66%** of the world exceeding a minimum
precipitation level. The revised target is motivated by the owner's approximate
desert/tundra comparison with Earth; it is a game-design choice, not a verified
equivalence between biome coverage and a precipitation cutoff.
In the ongoing land-suitability discussion, interpret this
as non-boundary land area, not ocean-inclusive map area; make that interpretation
explicit in the discussion. It is an approximate ordinary-world calibration
target, not an instruction to adjust each generated world to a quota.

The chosen minimum is **250 mm/year of total precipitation in liquid-water-
equivalent units**. The owner confirmed that snowfall counts, and raised the
distinction between snow depth and water content. Use the predicate annual
precipitation `>= 250 mm/year`. D-20 sets the acceptable coverage band;
do not use a per-world percentile to make exactly 66% pass by definition.
This is not a sufficient farming or
settlement-water criterion on its own. [USGS's desert classification overview](https://pubs.usgs.gov/gip/deserts/what/)
supports 250 mm/year as a commonly used arid/semiarid boundary, while noting that
other classifications use additional climate factors.
Measure area-weighted annual precipitation, counting rain and snow in
liquid-water-equivalent units. The numerical conversion and aggregation of the
existing seasonal fields still need a precise contract. This coverage target does not replace D-12's 20%
combined climate-suitability criterion or require access to lakes/rivers.
D-21 applies the same precipitation threshold to D-12's combined climate test.

Keep precipitation water quantity distinct from physical snow depth. A 10:1
snow-to-liquid ratio means 10 mm of fresh snow contains 1 mm of water, not 10 mm
of precipitation for this metric. Ratios vary with atmospheric conditions and
snow density; 10:1 is an illustration, not an approved universal conversion.
The owner accepted retaining water-equivalent quantities for climate and water
budgets, with physical snow depth derived separately if a consumer needs it.
Snowmelt transfers existing water and must not be counted as new precipitation.
Reference: [NWS snow-ratio explanation](https://www.weather.gov/arx/why_snowratios).

The Earth comparison is approximate: drylands are a land-area classification
based on precipitation relative to evaporative demand, not a universal rainfall
cutoff. See [UNCCD's Global Drylands report](https://www.unccd.int/sites/default/files/inline-files/Global_Drylands_Full_Report.pdf).
Use it as motivation for the game target, not as evidence for an exact threshold.

### D-20. Allow 56–76% precipitation coverage in ordinary test worlds

The owner accepted the proposed plus/minus 10 percentage-point tolerance around
66%. At least 90% of the predeclared ordinary-world test corpus should have
56–76% of non-boundary land receiving at least 250 mm/year of total precipitation
in water-equivalent units. The endpoints are inclusive. This is an acceptance
policy, not a measured result or a per-world generation correction.

Rarer worlds outside this band are reported without rerolls or post-hoc
exclusions; D-29 accepts the allowed misses without a second outcome gate or
additional testing of them. This precipitation pass rate is separate from D-8's
habitability frequency. Size-stratified reporting,
seed selection and precise treatment of the 66% central target remain Q-4 work.

### D-21. Define the 20% requirement solely through climate factors

The owner approved applying the same 250 mm/year precipitation threshold to
the combined criterion and explicitly excluded arable land and drinking water.
At least 20% of non-boundary exposed land must simultaneously have:

- annual mean ambient temperature in the inclusive range 0–30°C;
- summer temperature strictly above 0°C;
- annual total precipitation at least 250 mm in liquid-water-equivalent units.

The same locations must meet all three. No agricultural suitability, soil
fertility, freshwater proximity, potability or year-round liquid-water guarantee
is part of this acceptance test. This replaces the vague "usable water" wording;
do not add a second evaporation/runoff/soil-moisture threshold to the criterion.
Physical weathering and hydrology may still use their own liquid-water inputs.
D-8's approximately 90% ordinary-world frequency and D-12's 20% fraction remain.

### D-22. Keep stellar output constant and defer stellar evolution

The owner wants stellar evolution as a future feature but explicitly excludes
it from this slice. Hold stellar output constant over the geological timeline,
retaining latitude and seasonal effects. Initial planetary heat, atmospheric
carbon, oceans and geological activity still evolve and determine temperature.
Fixed stellar output does not impose a fixed climate or final temperature.

Keep the incoming-energy input explicit so a future feature can supply a
time-dependent value without replacing carbon accounting. Do not implement an
unused stellar-evolution subsystem or expose inert controls in this work.

### D-23. Include reduced surface-albedo temperature feedback

The owner approved albedo-temperature feedback in this slice alongside the
already approved CO2 greenhouse design. Keep albedo and greenhouse contributions
conceptually separate and independently testable while coupling both to one
thermal calculation. Include ocean/basic ground reflectivity and changing
snow/ice coverage. The owner also approved deferring new detailed ice-flow
physics, snow-grain aging, evolving vegetation-albedo feedback and dynamic cloud
feedback; preserve existing glacier and cloud behavior within the declared model.

The desired result is more coherent polar ice and ocean/sea-ice patterns, with
less unexplained patchiness. This is a validation goal, not an established
consequence of albedo alone: reflection reduces absorbed sunlight and can
reinforce existing ice, including isolated patches. It does not redistribute
heat or mechanically smooth ice flow. Diagnose and test spatial coherence with
the existing heat-distribution approximation, snow accumulation/melt, terrain
and water continuity. Do not delete small patches merely to make a map uniform;
local conditions can justify them. Artificial boundary walls remain excluded.

Q-9's inclusion choice is resolved. Q-8/Q-9 retain the numerical and spatial
closure, coupled stability tests and explicit domains. CARBON-7 separates the
thermal/albedo mechanism from carbon accounting for review; both remain in this
design's delivery scope, with primordial coupling in CARBON-6 and worldgen
integration in CARBON-5.

### D-24. Seek natural variation and judge the combined result visually

The owner clarified that subtle interactions such as albedo matter because their
combined effect should produce climate that feels varied and natural rather than
uniform or visibly formulaic. Coherent polar/ocean behavior under D-23 does not
mean smoothing every boundary or eliminating local variation. Geography,
geological history and interacting feedbacks should produce explainable large
patterns with local and world-to-world differences.

Automated tests cannot establish this aesthetic outcome. Use them for budgets,
isolated response directions, numerical convergence, determinism and broad
climate/biome regression criteria. Evaluate the combined appearance through
representative offscreen maps and regional transects across the predeclared seed
and size corpus, including borderline and unusual worlds. Record the owner's
assessment of repetitive patterns, abrupt artificial transitions and convincing
local variety before production activation. Do not turn a desired visual pattern
into a rule that overwrites simulated climate.

"Chaotic mix" describes interacting complexity here; it does not establish
mathematical chaos as a requirement or relax reproducibility. This qualitative
goal applies to generated climate and resulting terrain in the current scope;
D-11's generation-only carbon evolution is unchanged.

### D-25. Scale ongoing and event emissions by volcanic type, size and activity

The owner approved ongoing outgassing over source lifetimes plus occasional
explicit eruption pulses, and added that volcanic type and geological size must
affect carbon output. Give each source a documented type-specific emission
profile, a geological magnitude and an activity history. Larger sources have
larger emission capacity/rates when type and activity are otherwise comparable;
this is not a claim that every large dormant feature outgasses more than every
small active one. Integrate rates over elapsed time and apply each pulse once.
Formation, intensification, dormancy and extinction update the source history;
do not count a release both in the ongoing budget and as an eruption pulse.

Recommend an effective magma-supply/released-volume proxy multiplied by a
type-specific carbon yield and release fraction. This need not introduce a
fully simulated magma reservoir. Existing geometric dimensions can inform a
declared proxy but are not inherently carbon quantities, and eruption style alone
does not establish a real-world emission ranking. Calibrate the profiles rather
than inventing universal per-type factors. Source magnitude must obey D-13's
area/capacity convention, independent of rendering resolution or subdivision of
one source into multiple feature records.

Specify a carbon policy for every volcanic feature kind, including vents,
collapsed calderas and passive conduits. No lava-eruption profile does not itself
prove zero gas release. D-17 determines the receiving reservoir. Keep the volcanic
activity setting's influence explicit and avoid applying it twice through feature
count and emission magnitude unintentionally. Exact source mappings, pulse
budgets and dormant/extinct policies remain technical contracts under Q-5.

Evidence: `Timeline/Feature.hs` provides per-type dimensions and persistent
activity/formation/last-active/parent metadata. `Geology/Types.hs` supplies
eruption radius, deposited-tile volume, material and timeline-scale profiles;
none alone defines a CO2 yield. Research distinguishes eruptive and non-eruptive
degassing and relates gas properties to magma composition
([Fischer et al., 2019](https://www.usgs.gov/publications/emissions-co2-and-other-volatiles-worlds-subaerial-volcanoes)).

### D-26. Track regional weatherable-rock depletion and replenishment

The owner approved a regional weatherable-rock budget carried through the
geological timeline. Land-weathering rate depends on rock reactivity, temperature,
liquid water, exposed area and available fresh reactive material. Reactions
consume that capacity; erosion, uplift/exposure and new volcanic material can
replenish it through explicitly budgeted processes. The capacity persists across
steps and is not reset at every Age or solver substep.

This is supporting mineral-reactivity state, not a third active carbon reservoir.
The D-18 carbon/alkalinity transfers still account for the corresponding reactions.
Define capacity units and the mapping from material quantities to reaction limits.
New exposure is not synonymous with net elevation increase: identify the actual
geological inputs or a justified reduced replenishment model. Do not replenish
capacity merely to obtain a desired climate. Classify generation-owned state and
its reconstruction/persistence requirements before implementation.

Test exhaustion, zero supply, replenishment, lithology differences and elapsed-
time subdivision. Under fixed conditions, two regions with different material
or exposure histories can legitimately have different weathering fluxes.
Q-5 retains quantitative reaction/supply laws and seafloor-specific contracts;
D-27 resolves terrain-material transformation scope.

### D-27. Defer terrain-material transformation as a desired future feature

The owner explicitly wants chemical weathering to transform actual terrain
materials in a future feature and approved keeping it separate from this slice.
Here, depletion and replenishment affect the regional reactive-rock budget and
its carbon/alkalinity fluxes. Existing geological erosion/deposition continues;
this work does not add conversion of individual rock tiles into soil or other
weathering products, or change mineable material identities merely because a
regional capacity is exhausted.

Preserve the future feature in this design's follow-up notes; it is deferred,
not rejected. Keep material/exposure inputs and reaction accounting explicit so
future material transformations can be integrated without counting reactions
twice. No issue or implementation for that future feature is created here.

### D-28. Scale air-sea carbon exchange by ice-free ocean area

The owner approved exchange proportional to the ice-free fraction of connected
ocean surface. With other conditions fixed, 50% ice coverage halves that region's
exchange, and complete coverage blocks direct exchange in this reduced model.
Detailed gas transport through ice and unresolved cracks is deferred; do not add
a minimum flux solely to force equilibration.

Surface freezing preserves the ocean carbon inventory. Underwater volcanic
inputs and other eligible ocean processes continue; direct exchange resumes
when open water returns. Albedo and gas exchange use the same declared coverage
state with distinct response laws. Artificial boundary walls are excluded.
This does not imply the whole ocean volume freezes when its surface is covered;
Q-8/Q-5 retain actual liquid-volume and salt/carbon partition contracts.

### D-29. Test ordinary worlds at defaults, varying only size and seed

The owner defines ordinary worlds as worlds generated with default settings;
only world size and random seed vary in the ordinary acceptance corpus. Keep
plate count, volcanism, geological-history controls and every other setting at
their declared defaults. Randomly generated geography and history still vary
through the seed. Do not manually adjust plate count or history to suit a size.
Record the full resolved configuration and model version so defaults are
reproducible. The current shipped volcanic-activity default is 1.25.

Moderate as well as extreme custom settings are outside the ordinary 90% target:
the owner expects those controls to change world quality substantially. Retain
custom diagnostic/stress cases, including the original regression inputs, but
do not mix them into the ordinary denominator or silently relabel them as defaults.
All configurations retain the applicable accounting and numerical-validity gates.

The owner assumes that the allowed ordinary-world misses are acceptable and
explicitly rejects additional severity bounds or tests of those misses. The
proposed 15% climate-suitable-land floor and 51–81% precipitation outer band are
rejected, not deferred. The ordinary outcome gates remain the agreed pass rates;
this assumption is not a claim that those rates mathematically bound outliers.
Retain each sampled world's result for honest pass counting, but do not trigger
extra generation, retries, expanded sampling or follow-up tests because it misses
an outcome criterion. Accounting and numerical-validity tests remain distinct.

CI inclusion of the ensemble test is undecided. Predeclare a bounded sample and
generation budget if it is included; do not sample until a desired number of
passing worlds is reached. No per-world forcing, rerolls or seed exclusion are
authorized; failure of a calibration gate means revisiting the model.

### D-30. Share primordial starting conditions, scaling inventories with world area

The owner approved a shared primordial starting point: initial carbon, water
and stored heat per unit area are fixed for the model, and their total amounts
scale with world area under D-13. Initial atmospheric capacity and thermal
capacity must scale consistently so a larger world does not start hotter or
with a different carbon concentration merely because it is larger.

World size is the only source of variation in these initial totals. Seed,
bombardment history, generated geography and other generation settings do not
randomize or retune primordial atmospheric composition, temperature or the
reference inventories. Those inputs can affect subsequent evolution through
the approved geological and climate processes. Initialization still happens
once after bombardment under D-16; no later reset or forced equilibrium is added.

This approves the initialization policy, not numerical inventory values or a
primordial radiation/phase model. Q-8/Q-5 retain those technical specifications
and the bookkeeping of carbon, water and alkalinity as liquid ocean forms.

### D-31. Use a reduced global primordial energy/water model

The owner approved a simplified global model of stored heat, atmospheric water
vapour and liquid water during the primordial phase, coupled to carbon's effect
on heat loss. Cooling and condensation follow the modeled energy balance. This
slice does not simulate individual magma flows or regional crust solidification.

The model advances inside the existing post-bombardment geological timeline,
with no separate settling run. Its connection to regional climate preserves
heat, water and carbon accounting through ocean formation without a temperature
reset or a prescribed cooling-completion time. The early sharp drop remains an
expected ordinary-world outcome under D-15, not a scheduled state change.

This settles the approximation's scope. Q-8 still requires the actual heat
capacities, radiation and phase-change laws, units and validity ranges, plus the
coupling to regional temperatures and ocean geography. Q-5 retains liquid-ocean
chemistry and transfer bookkeeping. No full spatial mantle model is required.

### D-32. Keep fixed sea level and approximate the ocean footprint

The owner approved retaining the existing reference sea level in this slice.
Track effective liquid-ocean volume for carbon concentrations and thermal/water
accounting, with an explicitly approximate mapping to the terrain-defined ocean
footprint. Liquid-ocean processes require liquid water to exist. The conserved
model water inventory is not asserted to equal the water implied by rendered
tile depths.

This arc does not derive changing sea level or coastlines from condensed water
volume. Terrain evolution can still change coastlines at the fixed reference
level. A future sea-level feature may use a simple global offset; a new spatial
fluid simulation is not required merely to represent such an offset. Q-8 retains
the reduced volume/footprint coupling during condensation and freezing, without
reopening the approved sea-level scope.

### D-33. Route lake-covered volcanic emissions to the atmosphere

The owner approved crediting carbon released by lake-covered volcanic sources
to the atmosphere once. Temporary lake storage and delayed release are
approximated away at the geological timescale; this is not a claim that lakes
cannot retain dissolved carbon. This slice adds neither a lake carbon reservoir
nor detailed lake chemistry.

Sources submerged in the connected liquid ocean retain D-17's ocean-DIC routing.
Use the current environment at the source, so a change between lake, exposed
land and connected ocean changes the destination of subsequent emissions without
duplicating earlier emissions or creating a carbon transfer from an unmodeled
lake inventory.

### D-34. Initial fixed-salinity interpretation, clarified by D-38

The earlier approval was recorded as keeping reference salinity fixed throughout
the slice. D-38 corrects that interpretation: the owner intended a temporary
reference while developing the salinity formula, not a final exclusion of
evolving salinity. Preserve this history without treating the earlier exclusion
as binding. Ocean chemistry still requires liquid water and a valid property domain.

### D-35. Evolve atmospheric CO2 and water vapour with fixed background gas

The owner approved explicitly evolving CO2 through the carbon cycle and water
vapour through the thermal/water model, alongside a fixed background gas
inventory scaled with world area under D-30. The background inventory does not
evolve with the seed or geological history. Its exact composition and amount
remain technical specifications under Q-8.

A fixed background inventory does not fix total atmospheric pressure or
temperature. Changes in CO2 and water vapour still enter the declared pressure
and radiation calculations. Preserve the two active carbon reservoirs; water
and background gas are supporting atmospheric state, not extra carbon stocks.

Methane chemistry, evolving oxygen/biology and volcanic sulfur-aerosol cooling
are outside this slice. This is a reduced game model, not a reconstruction of
every primordial gas mixture. Existing cloud fields retain D-23's scope. Q-8
still requires radiation laws and validity ranges spanning the approved
primordial and mature regimes.

### D-36. Retain a liquid deep-ocean approximation after ocean formation

The owner approved retaining liquid deep-ocean water while surface ice grows
and melts. Complete freezing or evaporation of the formed ocean is outside this
slice. Keep one global ocean carbon/alkalinity reservoir under D-10; a bulk
thermal approximation does not add a second ocean carbon stock.

The owner cited roughly 1000 m depth and 4°C as the physical rationale. Record
that as context, not a universal depth-triggered temperature rule: 4°C is the
maximum-density temperature of fresh water, whereas seawater continues becoming
denser down to its lower freezing point
([NOAA](https://oceanservice.noaa.gov/facts/oceanfreeze.html)). Ocean depth does not
thermostatically force water to 4°C. The numerical model must retain D-1's
energy-accounted temperatures; the approved scope is a liquid deep-ocean
approximation, not an instruction to clamp temperature or add heat without a
budget. Effective depth/capacity and the supported thermal domain belong to Q-8,
consistent with D-32's distinction between model volume and rendered bathymetry.

### D-37. Defer long-term orbital changes

The owner approved deferring geological evolution of orbital parameters and
axial tilt. Retain the existing latitude/seasonal inputs within the new thermal
calculation. The removed unbudgeted CO2 sine wave is not replaced by a synthetic
orbital forcing term. Stellar evolution remains separately deferred under D-22.

### D-38. Treat fixed salinity as temporary while its formula is designed

The owner clarified that fixed salinity was intended as a temporary reference
until the salinity formula is established. It is not the completed model's
fixed-salinity requirement, and the earlier D-34 exclusion is superseded.

Pass current salinity explicitly to carbonate chemistry, gas solubility, mineral
saturation and the seawater freezing-point function. A reference value can be
used in isolated fixtures and preliminary model work; do not hard-code it into
those consumers or present a constant-salinity run as the final designed behavior.

Q-15 owns the remaining salinity law, inputs and state. This clarification does
not by itself approve a full regional salt circulation model, extra carbon
reservoirs, or particular salt-source coefficients. The interfaces remain useful
while the formula is settled. Reference-data domains and the simplified mineral
background must be reviewed against the chosen salinity range.

### D-39. Evolve one global dissolved-salt budget

The owner approved the evolving-budget recommendation under Q-15. Track global
dissolved-salt mass and derive salinity from liquid solution mass. Declared
geological dissolution adds salt; mineral deposition/removal exports it. These
transfers must agree with the carbon/alkalinity reaction accounting rather than
duplicating material or treating salt as an unrelated tuning factor.

Use the proposed salt-free ice/evaporation approximation: water leaves the liquid
phase without salt, and melting/condensation returns water that dilutes it.
Salt remains in the liquid ocean; detailed brine retention and regional salt
circulation are outside this reduced model. Keep exactly two active carbon
reservoirs. All ocean chemistry/freezing consumers use the resulting salinity.

The fixed-total-salt alternative was not selected. Initialization, reaction-to-
salt mass conversions, source/removal coefficients and the composition convention
remain technical contracts under Q-15, not a reason to reopen the approved budget
structure. Shared primordial initialization and world-area scaling still apply.

### D-40. Use a practical 14-world verification matrix

The owner prioritizes practical generation cost alongside world-size neutrality
and specifies this bounded matrix at default settings:

| World size | Number of distinct seeded worlds |
|---|---:|
| 32 | 10 |
| 64 | 3 |
| 128 | 1 |
| Total | 14 |

Sizes 256, 512 and 1024 are excluded from this matrix because their generation
cost is impractical. This supersedes both earlier all-size sample proposals.
Retain D-13's size-neutral accounting design, but do not claim generated-world
validation of the omitted sizes. Controlled scaling fixtures need no expensive
world generation and remain useful.

Use the existing 90% criterion across the 14-world sample: `ceil(0.9*14) = 13`
successes for each of the two separate outcome criteria. This pooling is the
technical application of the approved frequency to the owner's small unequal
sample, not a new per-size pass requirement. Report each size's results; do not
turn one size-128 or three size-64 observations into a separate 90% gate. Allowed
misses still trigger no additional tests, retries or expanded samples under D-29.

The matrix is 14 generations per scheduled verification run, not 14 for each
metric, capture or automatically duplicated calibration/validation phase. Reuse
each generated world's outputs for every applicable check. Keep tuning evidence
and held-out validation identities distinct without silently adding another
matrix; any separate generation workload needs an explicit practical budget.
CI inclusion remains undecided. Preserve large-world diagnostic history without
making rerunning those historical cases a prerequisite to this bounded matrix.

### D-41. Reuse one sampled world for the cold-biome regression

The owner approved designating one of D-40's 14 worlds as a cold reference.
Its final output must contain interior snow/ice, cold regions in both hemispheres
and warmer land elsewhere. Artificial border ice cannot satisfy the check.
Select the reference identity before tuning and reuse its generation; do not
add a fifteenth world, require every sampled world to have ice, or select a
different reference after seeing a failure.

Use actual final climate/ice consumers alongside the pure mechanism tests.
Relying solely on mechanism tests plus visual review was not selected. Exact
sampling/patch predicates are an implementation contract for CARBON-4, grounded
in representative output rather than copied from new baselines. This predeclared
regression is separate from the two 90% frequency gates and is not follow-up
testing of a tolerated miss. Visual review remains necessary for appearance.

### D-42. Preserve geological durations and expose early cooling through snapshots

The owner approved keeping existing geological durations and capturing
intermediate climate snapshots during the early intervals. A physically rapid
transition can therefore be visible within the first interval instead of being
stretched to fill two or three period boundaries. Shortening early intervals to
change timeline pacing was not selected. D-15's early sharp decline remains an
expected emergent trajectory, not a prescribed cooling schedule.

Choose bounded diagnostic checkpoints tied to physical elapsed time and relevant
phase transitions; they observe the integration and add no physical time, climate
reset or extra world generation. No new playback UI is implicitly required.

### D-43. Report sampled size differences without a new statistical pass gate

The owner approved reporting temperatures and climate coverage by size for the
D-40 matrix, alongside controlled scaling tests. Do not impose a new statistical
temperature-difference gate on the small unequal sample. The earlier 3°C proposal
is withdrawn. D-13's proportional accounting and size-neutral design remain;
generation evidence covers only the tested sizes and does not prove neutrality
for the excluded expensive worlds.

## Proposed design

The direction in D-1 through D-43, including D-38's correction of D-34, is accepted. Detailed choices below remain
proposals until explicitly resolved; distinguish implementation choices from
owner-facing behavior and from coefficients that require measurement.

### Carbon stock and fluxes

Track atmospheric carbon `A` and ocean dissolved inorganic carbon `O` separately,
in compatible carbon-amount units. Ocean carbon includes dissolved CO2 and its
carbonate-system forms; it is not all freely exchangeable dissolved CO2.
Derive atmospheric partial pressure from `A` and atmospheric capacity, and
ocean-surface CO2 partial pressure from the ocean carbon concentration and the
selected chemistry/buffering model. Document volumes, reference concentrations,
area normalization, initial partition, temperature inputs and time units.

For a signed air-to-ocean exchange flux `E` and separately identified external
source/removal terms, the accounting skeleton is:

```
dA/dt = atmosphericSources - atmosphericRemoval - E - weatheringTransfer
dO/dt = oceanSources - oceanRemoval + E + weatheringTransfer
d(A+O)/dt = totalExternalSources - totalExternalRemoval
```

`weatheringTransfer` represents atmospheric carbon delivered to the ocean by
the selected land-weathering pathway. Its exact chemistry and any accompanying
rock-derived carbon/alkalinity must be explicit. Sediment burial removes carbon
from the modeled atmosphere–ocean system; exchange and transfer do not. Do not
subtract the old lumped weathering sink and also count the same carbon as a
transfer plus burial. Seafloor-weathering reactions need their own explicit
carbon/alkalinity interpretation rather than an assumed air-to-ocean transfer.

`change in carbon = integrated degassing − integrated net geological removal`

This last equation applies to the combined stock, with any other declared
external inputs included. Maintain separate ledgers for each reservoir as well.

Maintain a budget ledger including any explicit pulses and boundary exchanges.
Do not claim a closed whole-planet carbon budget unless mantle and sediment
reservoirs are modeled too. Never count the same release as both background
degassing and an eruption pulse.

D-25 approves persistent volcanic outgassing as a rate with active lifetime and
type/size/activity-dependent magnitude; discrete pulses have carbon quantities
and event times.
Relate rates to geology rather than raw visual-feature counts. Decide how world
size scales atmospheric capacity, source magnitudes, and weatherable area
together. Doubling numerical samples must not double planetary emissions.

Source routing is approved by D-17: released carbon from a source
exposed to air enters the atmosphere; released carbon from a source submerged
in the connected liquid ocean enters ocean DIC. Before a liquid ocean exists,
surface-released carbon enters the atmosphere. Determine routing from current
conditions at the source, not a permanent feature label; lake-covered sources
emit into the atmosphere under D-33. Account for each
release once, with subsequent air-sea exchange using the existing transfer flux.
This is a reduced model: shallow-water plume bypass and detailed bubble dynamics
are not implicitly included. [GEOCLIM7 (2025), section 3.4.4](https://gmd.copernicus.org/articles/18/6367/2025/index.html#section3.4.4)
provides a structural precedent for distinguishing atmospheric volcanic inputs
from mid-ocean-ridge inputs to ocean carbon; it does not validate every routing
approximation proposed here.

### Air–sea exchange

Use a signed flux driven by the difference between atmospheric and ocean-surface
CO2 partial pressures, with a nonnegative exchange coefficient carrying the
necessary area, solubility and unit conversions. Schematically:

`E = K * (pCO2_air - pCO2_ocean_surface)`

Positive `E` moves carbon from air to ocean; negative `E` outgasses. Compute one
transfer amount and apply it with opposite signs to the inventories. Equilibrium
means equal exchange potentials, not equal carbon inventories. Exchange has a
finite rate; do not equilibrate both reservoirs instantaneously after each Age.

Ocean temperature, salinity and carbonate buffering affect the ocean-side
potential; total dissolved carbon alone cannot determine it. D-9 selects a
carbonate-chemistry closure with an explicit alkalinity budget; Q-5 retains its
implementation contracts. Alkalinity is supporting chemical state without
being a third carbon reservoir. Ocean volume affects concentration/capacity;
exposed ocean area affects transfer rate. Sea-ice coverage must have a documented
effect on exchange area, with the artificial border excluded. Zero ocean area
means zero air–sea exchange, with no division by zero.

D-28 approves the exchange-area rule: multiply local exchange by
the fraction of connected ocean surface that is ice-free, then sum with area
weights. With all other inputs held fixed, 50% surface ice halves that region's
exchange. Fully covered surface has zero direct exchange in this reduced model;
do not introduce an unbudgeted minimum flux to force equilibration. Real ice
can exchange gas and contain leads, so any future permeability/leakage term needs
an explicit physical interpretation and calibration. Open-water fraction scaling
is used in research models, with differing treatments of residual exchange
([Sea ice controls net ocean uptake of carbon dioxide, 2025](https://www.nature.com/articles/s43247-025-02395-x)).

Surface ice does not erase ocean carbon or stop an underwater volcanic input;
the two inventories continue evolving through their permitted fluxes, and
exchange resumes as open surface returns. Use the same declared coverage state
for albedo and exchange, with their separate response laws. Actual liquid-volume
changes and carbon/alkalinity partition during freezing remain Q-8/Q-5 contracts;
salinity evolves through D-39's salt budget and salt-free phase-transfer approximation, and
surface coverage alone must not be treated as the entire ocean freezing solid.

Fast exchange and slower geological sources/sinks require a solver that resolves
or stably integrates both timescales. A numerical timestep must not erase the
transient atmospheric response the owner chose two reservoirs to represent.
Add a pulse/relaxation fixture and compare transient peaks and recovery as well
as final inventories. Gameplay-time carbon evolution is outside this arc under
D-11.

### Weathering feedback

D-26 approves regional supply-limited weathering. Estimate net long-term
silicate-weathering removal over exposed land, using
regional temperature, liquid water/runoff, reactive lithology, and supply of
fresh rock. A proposed kinetic term increases with CO2, temperature, and runoff;
its realizable rate is limited by available reactive material. Dry, frozen, or
nonreactive areas must not be assigned a fictional unrestricted sink.

Warming can increase removal; cooling can slow it, allowing continuing degassing
to rebuild CO2. Stable equilibrium arises when the rates balance. The model must
not assume every forcing/land configuration admits a habitable equilibrium.
Retain land and seafloor weathering in the design direction previously accepted
with the recommendations. Resolve their chemistry and transfers under Q-5;
neither is an unaccounted generic sink used to force equilibrium.

### Temperature response

Replace the linear concentration multiplier with a logarithmic CO2 forcing
response in the mature domain. `warming = S * log2(pCO2_air / reference_pCO2)`
describes the intended approximate equilibrium sensitivity, not a temperature
offset to add after solving the TS-5 energy balance. The selected radiation
closure owns greenhouse forcing once. Choose sensitivity through model
calibration; neither the current value 6 nor a real-Earth estimate automatically
becomes the game constant. Keep latitude, maritime influence, altitude and
seasonality as explicit physical inputs rather than prescribing final regional
temperatures with the legacy latitude formula.

Fixed reference energy/reaction constants are unavoidable model parameters;
they do not imply resetting every world's computed temperature to a target.
Audit literal CO2-as-temperature consumers before changing the concentration
range. Ensure the final grid and summary retain #785's consistency.

Remove the unbudgeted per-Age sine addition to carbon. D-37 defers orbital cycles
to separate work on insolation/seasonality;
an orbital cycle is not itself a carbon source. D-23 includes simple surface
albedo feedback alongside greenhouse forcing; validate the coupled
feedback and its stability before calibration or production activation.

### Geological time and integration

Define non-overlapping physical intervals. Under D-16, bombardment is
pre-atmosphere; initialize primordial state afterward, then advance continuous
atmospheric/ocean sources and sinks over subsequent physical intervals. Resets
such as atmospheric loss during a cataclysm, if
chosen, must be explicit budgeted transitions; current Eon prose alone does not
establish that behavior.

Use deterministic internal integration steps or an appropriate stable solver,
independent of how many narrative Ages the caller uses. Recompute the inexpensive
carbon/climate feedback as necessary; do not rebuild terrain for each substep.
Preserve positivity numerically and surface invalid states. A numerical minimum
must not conceal mass creation or function as the model's climate regulator.

For the same fixed external forcing/event history, refined time steps should
converge to the same trajectory. Changing the randomly generated geological
history is a different experiment and need not yield the same climate.

### Technical contracts proposed for readiness

These recommendations resolve implementation ambiguities without pretending
that unmeasured coefficients are known. D-6 fixes the two-reservoir structure;
Q-3/Q-4 retain technical and acceptance details, and Q-5 covers the exchange and
chemistry approximation. Acceptance of the complete design includes these contracts.

| Concern | Recommended contract |
|---|---|
| Runtime scope | Approved by D-11: the carbon cycle runs during geological generation only. Gameplay ticks do not evolve atmospheric or ocean carbon in this arc. |
| Reservoir interpretation | Two carbon stocks, atmosphere and ocean (D-6), with finite bidirectional conservative exchange. Atmospheric partial pressure drives warming. Ocean DIC, volume, temperature and the chosen carbonate-buffering closure determine its exchange potential; never compare the raw inventories to decide flow direction. |
| Planet size | Approved direction in D-13: aim for proportional scaling of sources, capacities, and weathering area, with no size-specific climate target or correction. Test replicated geography for accounting invariance and report independently generated size cohorts under D-40/D-43. Numerical resampling must not create emissions; generated worlds need not match exactly. No additional statistical cross-size gate is required. |
| Primordial phase | D-14/D-15 require primordial evolution inside the same geological loop, carrying state through early cooling and all later periods. D-42 preserves existing durations and captures intermediate snapshots when rapid cooling occurs within an interval; do not stretch the transition across two or three period boundaries. Q-8 retains the physical reference and initialization contracts. |
| Geological clock | D-16 places bombardment before atmospheric initialization: it advances geological age but not atmospheric evolution. The proposed later clock preserves additive Era, formation/evolution Period and Age durations; structural Epochs consume none. Integrate carbon through these post-initialization physical intervals without also integrating parent totals over children. Record both geological age and time since initialization. |
| Cataclysms | Eons continue the inherited carbon state. This arc introduces no atmospheric stripping or baseline reset; correct reset-like prose. A future atmospheric-loss event would need its own explicit carbon transfer. |
| Event ordering | Materialize interval-start geological events, apply any explicitly budgeted carbon pulse once, derive that interval's forcing snapshot, then advance carbon through its duration. Stamp trace entries with start/end time and stable event IDs. Define zero-duration events as pulses only. Preserve existing random feature/event identities where possible. |
| Volcanic source | Prefer duration-based mean degassing for persistent activity at Myr scale. Assign each geological source one magnitude and activity lifetime, independent of render instance count. A source's formation is not automatically a second carbon pulse. Document dormant/extinct multipliers and every excluded feature kind. Explicit exceptional pulses require one quantity and timestamp, not a second per-Age chance roll. |
| Volcanic controls | Apply the existing volcanic-activity setting at documented source-generation/activity boundaries. Audit its existing effects on feature count and chance so the new carbon adapter does not accidentally multiply it again. Zero volcanic activity must give zero volcanic carbon input even if minimum-count terrain features remain. |
| Land weathering inputs | Build a bounded, area-weighted regional weatherability snapshot from the current geological material/exposure state. Use explicit material reactivity groups with coverage checks. Do not infer reactivity from hardness or pretend the existing ElevGrid stores lithology. |
| Supply limit | D-26 approves a regional reactive-rock budget carried across elapsed time. Reactions deplete it and declared geological exposure/deposition processes replenish it; reaction throughput cannot exceed available capacity. A fresh capacity reset on every numerical substep is forbidden. Do not equate net elevation change with erosion flux without evidence. D-27 defers new chemical transformation of terrain materials. |
| Liquid water | Derive a continuous, nonnegative liquid-water estimate from climate precipitation, seasonal thaw and an explicit evaporation/runoff approximation. Do not reuse the river generator's minimum-one floor. Keep water units distinct from integer accumulated river flow. |
| Seafloor sink | Account for seafloor weathering separately from land weathering and attach it to ocean area and reactive oceanic crust. Q-5 defines reaction/alkalinity effects and eventual burial. No unconditional world-wide sink is substituted for missing land. Its coefficients and cold-world response are calibrated independently. |
| Coupling cost | Refresh expensive geography/material inputs at geological event boundaries. Carbon solver substeps update cheap climate-response and regional-flux calculations only; they do not rerun erosion, river identification or full terrain generation. |
| Solar and orbital scope | D-22 approves constant stellar output and defers stellar evolution. D-23 includes reduced surface-albedo feedback alongside greenhouse forcing. Keep existing latitude/maritime/seasonal inputs and remove synthetic CO2 oscillation. D-37 defers long-term orbital changes; Q-9 retains albedo implementation contracts. No new inert tuning controls are exposed. |
| Glacier behavior | Replace raw CO2-as-temperature classifications in generation, evolution and branch-cap logic with one documented local climate criterion. Include accumulation/thaw evidence and altitude. The criterion and sampling location must be shared across those consumers, not separately tuned tables. |
| Final publication | After the last geological interval, evaluate final regional climate at the final stocks and thermal state with zero added geological time; use that same final climate for final ice classification/levels and subsequent chunk/zoom/vegetation consumers. Do not silently relax carbon or temperature for extra time after the last interval or recursively regenerate terrain until a desired climate appears. Resolve ocean/terrain dependency ordering explicitly. |
| Boundary walls | Keep the forced glacier border as the existing gameplay boundary. Exclude it and beyond-world tiles from physical climate area, cold-biome coverage, and weathering accounting. It must never satisfy the interior-ice regression by itself. |
| Failure | Invalid units, non-finite state, failed numerical convergence, or model-domain violations stop generation with a diagnostic containing seed/config and the last valid budget state. Do not clamp the final climate or secretly retry another seed. A valid extreme climate is not itself a failure. |
| Determinism | Fix iteration order, sampling weights, event ordering and numerical policy. Same revision/config/seed must preserve the repository's deterministic worldgen contract; timestep-convergence tests use tolerances rather than requiring bit equality across different numerical policies. |
| Persistence | Keep diagnostic traces local and generation-only. Classify any added in-memory stock/supply state and persist only what is needed for the chosen save/load/reconstruction contract. Preserve historical DTO shapes and follow per-component migration rules. |
| Cache freshness | New model parameters that affect persisted/reconstructed output enter the appropriate parameter encoding and cache identity. The current zoom artifact already hashes production source and encoded parameters; verify invalidation rather than deleting caches to manufacture a pass. |

### Calibration and numerical acceptance are explicit work products

The design must distinguish model structure from coefficient selection. Assigning
unmeasured source strengths or reaction times here would only replace one guess
with another. Before production activation, the calibration slice must produce
a checked-in parameter specification listing each coefficient's meaning, units,
source or rationale, initial search range, selected value, and sensitivity.
Parameters are global for a declared model version; no per-seed fitting.

Predeclare ordinary/stress configurations and separate calibration and validation
seeds before tuning. Under D-29, ordinary configurations use resolved defaults
with only world size and seed varying; all other custom configurations are
reported separately. Keep the original failing inputs in the diagnostic corpus.
Freeze the validation criteria before inspecting candidate outcomes. Report
every case, including valid extremes, failures and exclusions; do not replace a
seed because it fails. Frequency measured on a finite corpus is an engineering
acceptance measure, not proof of the distribution over all seeds.

Under D-13/D-40, stratify the approved ordinary corpus by tested world size and report
temperature distributions, cold-region coverage, and climate-suitable land per
size. Surface systematic size-related differences in the report. D-40 excludes
expensive larger worlds and does not impose per-size frequency gates. Revisit
practical cross-size comparisons under Q-4 for the unequal small sample; controlled
scaling invariance and generated-world comparability are separate gates.

For pure-model tests, proposed numerical acceptance is:

- Budget residual no larger than `1e-8 * max(1, initialStock, totalThroughput)`
  in normalized carbon units, with source/sink throughput accumulated separately
  from the state change. A self-derived identity is not an independent test.
- Under a fixed external history, step refinement changes final CO2 by at most
  `1e-4` relative and regional temperatures by at most `0.01°C` in the declared
  convergence fixtures. The method must also demonstrate a decreasing-error
  trend against an analytic or more accurate reference where available.
- Numerical positivity preserves the budget. No positive floor silently adds
  carbon. The solver may reject/refine a step; exhaustion of its documented
  work limit returns a failure rather than hanging or accepting an invalid step.
- Exclude genuinely unstable or supply-exhausted cases from stable-equilibrium
  assertions by their input contract, not by inspecting whether they passed.

These tolerances are proposed engineering requirements, not measurements of an
implemented solver. Solver selection, step ceilings and a generation-time budget
must be justified by convergence and runtime measurements before activation.
The existing game will retain its current model until the candidate passes
calibration, integration, compatibility and visual gates together.

### Approved habitability definition and remaining measurement details

D-12/D-21 approve measuring **climate suitability**: at least 20%
of non-boundary exposed land has annual ambient temperature in `0–30°C`, summer
temperature above freezing, and annual precipitation at least 250 mm water
equivalent. Qualifying
land must satisfy all three conditions together. Worlds with no exposed land
fail this land-climate measure. Arable land and drinking-water access are
explicitly excluded; the metric must not be presented as proof of either.

Snow/tundra diversity is a separate ensemble measure: climate suitability
does not imply ice sheets, and ice-free habitable worlds remain valid. Set
interior polar coverage criteria for declared cold-reference fixtures and
report snow/tundra/ice fractions across the entire ensemble. These separate
cold-biome coverage thresholds still need definition; the combined climate
criterion itself is approved by D-12/D-21.

## Technical specification pass (draft for review)

This pass makes the mechanism and integration contracts concrete within
D-1–D-43, including the salinity correction in D-38/D-39. It proposes engineering choices; it does not silently promote them
to owner decisions or claim calibrated coefficients, executable tests or a
validated primordial model. D-36/D-37 resolve the Q-12/Q-14 scope questions;
Q-13 was checked against the existing decision and does not require reapproval.
Q-15 records D-39's evolving salt budget and its remaining coefficient/composition
contracts; fixed salinity is only a temporary reference under D-38.
Unresolved scientific reference data are called out separately below.

### TS-1. Units, area and ownership

Use `Double` for the candidate model and explicit unit wrappers at its boundary.
Carbon inventories are mol C, alkalinity is mol charge equivalents, water is kg,
energy is J, pressure is Pa and temperature is K internally. Convert to Celsius
only for the existing climate interface and outcome predicates. Rates use SI
seconds internally; one geological Myr is `1e6 * 365.25 * 86400` seconds,
independent of the player's configurable calendar. A seasonal climatology year
is a fixed model year, not millions of individually simulated weather years.

Each canonical, non-boundary surface tile contributes the same effective area.
Use `A_world = A_ref * (worldSize / 128)^2` with `A_ref` a declared physical-scale
parameter, and distribute it by represented tile counts. Do not add a second
cos(latitude) weight to tile counts: this is the game's equal-area approximation,
not a literal latitude/longitude mesh on a sphere. Latitude affects insolation.
Boundary walls and aliases contribute no extra area. Regional land/ocean fractions
come from a fixed deterministic sampling rule, shared by budget and coverage
calculations. Resampling must preserve represented area.

Gravity and the background composition remain model constants across sizes.
World size scales extensive amounts and capacities; it does not select new
gravity, reference temperature, climate sensitivity or equilibrium targets.
Store the area rule/version with the parameter specification.

Proposed generation-local records:

| Record | Contents | Lifetime |
|---|---|---|
| CarbonState | Atmospheric `A`, ocean `O`, ocean alkalinity `B` | Carried through generation; final diagnostic values retained only if a declared consumer needs them |
| OceanSaltState | Dissolved-salt mass and external addition/removal ledger, with salinity derived from liquid solution mass | Generation-local under D-39; persist resulting salinity and any reconstruction-required quantities |
| ThermalWaterState | Interior enthalpy, surface/atmosphere enthalpy, vapour/liquid/snow/ice masses and regional thermal state | Generation-local coupled solver state; formed deep ocean remains liquid under D-36 |
| GeoClimateInputs | Represented areas, reference sea-level footprint, material groups, source identities/activity, supply increments and interval duration | Immutable snapshot per geological boundary |
| ReactiveSupply | Available regional silicate/carbonate reaction capacity and separately declared seafloor capacity | Carried and depleted during generation; no tile-material conversion |
| ModelParameters | Versioned units, reference inventories, coefficients, reference-data hashes and solver policy | Immutable; reconstruction-relevant subset persisted with generated world |
| FluxLedger | Accepted transfers, external additions/exports, energy and water residuals | Generation diagnostic; transient, bounded trace |

No new `EngineEnv` field or capability is required: pass pure state through
`TimelineBuildState`/candidate functions. Classify final persisted fields and
transient state in the existing persistence inventory when implemented.

### TS-2. Atmospheric composition and ocean chemistry

Initialize once after the first pre-atmosphere bombardment: `A = a0*A_world`,
`O = 0`, `B = 0`; all initial water is in the primordial water/steam model.
This zero-ocean initialization avoids inventing dissolved carbon or alkalinity
when condensation begins. A fixed background N2 approximation is proposed;
its reference mass, initial thermal enthalpy and `a0` require the parameter
specification. They are the same per unit area for every seed under D-30.
Weathering supplies the first alkalinity; finite exchange and eligible geological
sources supply ocean carbon as liquid becomes available.

For the well-mixed atmospheric approximation, compute total pressure from
atmospheric mass and gravity, then partial pressures from mole fractions:

```
nCO2 = A
nH2O = atmosphericWaterKg / molarMassH2O
nN2  = fixedBackgroundKg / molarMassN2
pTotal = gravity * totalAtmosphericMassKg / A_world
pCO2 = pTotal * nCO2 / (nCO2 + nH2O + nN2)
```

Do not use CO2 mass fraction as mole fraction or apply the dilute-atmosphere
conversion unchanged to a CO2-dominated atmosphere. Non-ideal fugacity corrections
belong to the selected high-pressure reference closure. The mature diagnostic
CO2 multiplier is `pCO2 / referencePCO2`; it is not the stored carbon amount.

Use dissolved molality `D = O / liquidOceanKg`, `TA = B / liquidOceanKg` and
current salinity `S(t)` from D-39 for a reduced carbonate-plus-water system. Until
the salt kernel is implemented, a fixed reference may be supplied for isolated
work only. With hydrogen
molality `h`, all constants on the same declared pH/activity scale:

```
denom = h*h + K1*h + K1*K2
CO2aq = D * h*h / denom
HCO3  = D * K1*h / denom
CO3   = D * K1*K2 / denom
TA = HCO3 + 2*CO3 + Kw/h - h
fCO2ocean = CO2aq / K0
```

Solve the alkalinity residual in log(h) by bracketed iteration. Carry a previous
root as a guess, not as authority to skip the residual check. Include an exact
zero-DIC case and acid-rich, high-alkalinity and donor-exhaustion fixtures. This
reduced species set omits borate/nutrients; reference comparisons must use the
same omission and activity conventions. It must not be described as full seawater
chemistry. The carbonate and water equilibria are documented in
[PyCO2SYS's model paper](https://gmd.copernicus.org/articles/15/15/2022/).

Model-domain validation precedes logarithms or divisions. Include zero atmospheric
CO2 as a radiative boundary case with no CO2 absorber, not an artificial positive
carbon floor; land reactions requiring atmospheric carbon then have no donor.
Pressure, water mass and chemical constants must satisfy their declared domains.
Use full-model reference functions, not `log(0)`, outside the dilute logarithmic
forcing approximation.

**Reference-data prerequisite:** select and pin `K0/K1/K2/Kw`, pressure corrections
and their actual valid temperature/salinity/pressure ranges. Modern-ocean fits
alone do not establish validity for a newly condensed hot ocean. CARBON-8 below
must supply a reviewed hot-to-mature closure or an explicit, reviewed approximation
before implementation proceeds into that domain. Clamping the input temperature
to a library's limit, suspending eligible exchange until it becomes convenient,
or silently extrapolating is not an acceptable substitute. Reference salinity
35 is a candidate consistent with today's code, not a measured primordial value.

Evaluate local surface chemistry from the same global DIC/alkalinity molalities
and each ocean region's surface temperature; do not create local carbon stocks.
For region `r`, exchange is
`E_r = area_r * liquidOceanFraction_r * (1-iceCoverage_r) * k_r * K0_r
       * (fCO2air - fCO2ocean_r)`.
Here `k_r` has units kg water/(m² s), and `K0` mol/(kg Pa). Use the same fugacity
convention on both sides. A fixed positive transfer coefficient is the initial
reduced transport model; add wind dependence only if its input units and
additional calibration are justified. Sum signed regional fluxes before applying
one equal/opposite atmosphere/ocean transfer. Finite transfer remains resolved.

### TS-3. Reaction stoichiometry and finite supply

Define land silicate weathering `Ws`, carbonate-rock weathering `Wc`, seafloor
silicate weathering `Wf` and carbonate burial `Jb` in mol reaction/s. The reduced
divalent-mineral stoichiometry is:

| Process, per mol reaction | Atmospheric C | Ocean C | Ocean alkalinity | External C |
|---|---:|---:|---:|---:|
| Land silicate weathering | -2 | +2 | +2 | 0 |
| Land carbonate-rock weathering | -1 | +2 | +2 | +1 from rock |
| Seafloor silicate weathering | 0 | 0 | +2 | 0 |
| Ocean carbonate burial | 0 | -1 | -2 | -1 to sediment |

Seafloor weathering changes dissolved speciation/alkalinity; subsequent burial
exports carbon. It is not an immediate second carbon sink. Accordingly:

```
dA/dt = volcanicAir - E - 2*Ws - Wc
dO/dt = volcanicOcean + E + 2*Ws + 2*Wc - Jb
dB/dt = 2*Ws + 2*Wc + 2*Wf - 2*Jb
d(A+O)/dt = volcanicAir + volcanicOcean + Wc - Jb
```

The distinction between atmospheric and carbonate-rock carbon follows the
weathering accounting described by
[CLIMBER-X](https://gmd.copernicus.org/articles/16/3501/2023/index.html);
the particular reduced rates and seafloor closure here are game-model proposals.
No organic-carbon, methane or sediment carbon stock is introduced.

For material group `j`, propose kinetic reaction rate
`k_j * exposedArea_j * (pCO2/pRef)^beta_j
 * exp[-Ea_j/R * (1/T - 1/Tref)] * (runoff/runoffRef)^gamma_j`.
Evaluate warm/cold seasons separately and average rates by duration. Zero
liquid runoff or zero available material gives zero land reaction. Material
groups are reactive silicate, carbonate-bearing and effectively inert, with
subgroups only where authored materials justify different coefficients.
The material-to-group table must cover every encountered substrate explicitly;
hardness, color and a default fallback group are not chemical evidence.

Track capacity in mol reaction, not carbon mass. An accepted reaction depletes
its matching regional capacity once. New deposition contributes its declared
volume times reactive capacity density; exposure contributes only independently
measured newly exposed material. A terrain snapshot is not replenishment.
Submergence pauses the land pathway and transfers remaining capacity to the
matching submerged-material accounting where applicable, without resetting it.
Positive/negative net elevation alone cannot establish gross supply; CARBON-11
must instrument event/material turnover or specify a separately budgeted exposure
proxy. No hidden unlimited source of fresh rock is permitted.

Seafloor kinetics use liquid-ocean temperature and the ocean-side dissolved
CO2/acidity from TS-2, not atmospheric CO2 or land runoff. Propose a separate
Arrhenius/material-capacity law, with area restricted to eligible reactive
seafloor. Surface sea ice alone does not shut down this submerged pathway.
Its rate coefficients are calibrated independently of land weathering.

Burial uses `Jb = k_b * eligibleOceanArea * max(0, Omega-1)^n`, where
`Omega = CO3 / CO3_saturation(T,S(t))` is an explicitly reduced mineral-saturation
closure. The saturation threshold embeds a fixed background divalent-cation
approximation; its consistency with Q-15's salinity law needs review, and no
full calcium/salt budget is yet specified. Limit accepted burial by
available DIC and alkalinity. All processes competing for a stock share the same
coupled donor constraint; independent clipping followed by sequential subtraction
is insufficient. After burial, there is no automatic sediment re-dissolution in
this two-active-reservoir model.

### TS-4. Geological emissions and spatial scaling

One persistent geological source owns one degassing budget. Define a type-specific
intrinsic magma-supply proxy from dimensions in reference geological units, then
`q_source = yield_type * magmaSupplyProxy * activityFactor * representationWeight`.
Keep duration-based degassing separate from explicitly identified event pulses.
An eruption's pulse allocation is not also included in the same interval's mean
emission estimate. No independent carbon-only eruption chance roll is added.

| Feature | Proposed geometric input and identity policy |
|---|---|
| Shield, cinder cone, lava dome | Intrinsic base area and height define an effective edifice/magma-volume proxy; each type has its own supply-timescale and carbon yield |
| Fissure | Intrinsic length, width and ridge-height proxy; use the source path, not the number of rendered segments |
| Supervolcano | Declared magma-volume proxy from caldera/ejecta dimensions; exceptional eruption pulses use identified events |
| Caldera | Retain the parent magmatic source where one exists; collapse geometry does not manufacture a new independent source |
| Hydrothermal vent | A separately declared hydrothermal throughput proxy; if fed by a modeled parent, allocate from that parent's budget rather than duplicate it |
| Lava tube | A conduit, with no independent magma supply; any vented fraction is an allocation from its parent source |

Propose active multiplier 1, extinct multiplier 0, and a calibrated dormant
multiplier in [0,1]. The mapping must distinguish a stand-alone hydrothermal source
from a passive conduit; absence of a lava eruption profile is not a zero-carbon
rule. D-17/D-33 determine the receiving reservoir from the current environment.

Current `World.Geology.Hash.scaleCount` uses an area ratio with integer flooring
and a minimum base count; `World.Scale` separately scales rendered distances and
elevations against size 512. Do not combine those raw fields as if they were
unscaled physical source dimensions. For each source family document whether its
stored dimensions are intrinsic or scaled, undo presentation scaling exactly once,
and record the effective area represented by one generated source.

Where the count rule is `baseCount * max(1,floor(areaRatio))`, the reference
representation weight can be `areaRatio / max(1,floor(areaRatio))`. This corrects
the declared count discretization, not observed CO2 or the realized number of
volcanoes. Use the actual family's rule; never normalize by the observed emissions,
seed-specific volcano count or desired final temperature. A single source still
gains influence with greater intrinsic geological magnitude. D-13's replicated
geography tests and independently generated size cohorts remain distinct.

### TS-5. Thermal equations, phase change and regional coupling

Use a conservative enthalpy formulation. During the global primordial regime:

```
dHinterior/dt = internalHeatProduction - interiorToSurface
dHsurfaceAtmosphere/dt = interiorToSurface
    + A_world * (absorbedSolar - outgoingLongwave)
```

Temperature and phase fractions are derived from the enthalpy/water equation of
state, including solidification, condensation and melting latent heats. No
separate latent-heat correction may count the same transfer twice. Water moves
between phase inventories with equal/opposite mass changes. Fixed initial water
is not supplemented by unbudgeted rain; atmospheric escape and new volcanic water
input are omitted in this proposed reduced closure. Carbon outgassing remains
the separately budgeted external source already approved.

Use state-dependent reference radiative functions
`OLR(T,pCO2,pH2O,pBackground)` and absorbed solar
`ASR(insolation, gasState, surfaceAlbedo)`. Generate compact, versioned interpolation
tables or a checked analytic surrogate offline; no external atmosphere solver
runs during game generation. Cover the primordial and mature domains with a
common closure or overlapping flux-matched closures. Match radiative flux and
enthalpy at the handoff, not a target surface temperature. Do not extrapolate
the mature logarithmic CO2 approximation into a dense steam atmosphere.

This choice is informed by radiative/energy-balance coupling in
[EOS-ESTM](https://arxiv.org/abs/2206.05151) and the explicit H2O/CO2 primordial
calculations of [Pluriel et al.](https://arxiv.org/abs/1809.02036). Their detailed
cloud or mantle physics is not implicitly included. **CARBON-8 must first deliver
the selected radiative reference, usable data/formulas, license/provenance,
parameter domain and interpolation-error fixtures.** Research citations alone
are not an implementable radiation law. If a proposed clear-sky/fixed-cloud
approximation cannot cover the required range, revise that proposal before
coding the coupled model; do not add an arbitrary cooling schedule.

In the regional regime, retain one global interior state and use
`dH_r/dt = area_r*(ASR_r - OLR_r + interiorFlux_r) + sum_j G_rj*(T_j-T_r)`.
Choose symmetric nonnegative conductances so lateral heat transfer cancels
globally. U wraps; polar boundaries have no outward exchange. Derive conductances
from normalized world geometry and represented area so changing world size alone
does not change the reference transport strength. Ocean thermal capacity scales
with effective water mass; land capacity follows material/area. Elevation affects
the declared surface/ambient-temperature conversion, not a duplicated global
warming term. Retain latitude and seasonal insolation; remove the old fixed
equator temperature plus `52°C` pole-drop temperature prescription from the
candidate energy solver. Legacy reconstruction retains the legacy calculation.

Initialize regional enthalpies by an area/capacity-conserving distribution of the
global state when the declared material/phase domain permits regional physics.
Do not throw away remaining interior heat at this boundary. Once regional, keep
that representation; the handoff is not repeated at every Eon or warm excursion.

Snow and surface ice use water-equivalent mass and energy-limited phase changes.
Proposed salinity-aware phase rule: use one shared
`seawaterFreezingPoint(currentSalinity, surfacePressure)` function at the
ocean/ice interface. Under D-38 the fixed reference is temporary; the final
salinity law supplies the changing input. Surface pressure is
already available from TS-2, so any supported pressure correction belongs in
the property function rather than a new pressure simulation. Do not substitute
deep seafloor pressure when evaluating surface sea ice. At ordinary surface
pressure and typical ocean salinity the reference value is approximately -1.9°C.
Use about 0°C for freshwater lake/river ice and terrestrial snow in the reduced
surface model. Keep D-21's summer-above-0°C land criterion unchanged.

The physical property reference may use
[TEOS-10's seawater freezing calculation](https://www.teos-10.org/pubs/gsw/html/gsw_t_freezing.html)
or a pinned fit/table checked against it. Its salinity is Absolute Salinity in
g/kg and pressure is sea pressure in dbar: convert explicitly from the model's
chosen salinity convention and absolute Pa, including the atmospheric reference
offset. Do not feed an ambiguous existing `35.0` field into different chemistry
conventions without conversion. A reference dissolved-air convention is a fixed
property approximation, not a newly simulated gas inventory.

Use local water/interface temperature and available energy for freezing/melting;
air temperature alone is not the water's phase criterion. Growth and retreat
at the seawater/ice interface use the same salinity-aware boundary, with latent
heat accounted once. Snow lying above sea ice uses its freshwater phase rule.
D-39 approves salt-free ice and evaporation: salt remains in liquid water, and
melting/condensation dilutes it. Detailed brine retention and internal sea-ice
salt profiles are excluded. Q-15 retains reaction-to-salt mass conversions.
Add small reference-property fixtures (freshwater
near 0°C, reference seawater near -1.9°C, and prescribed pressure cases) and
controlled below/above-boundary energy tests to CT-07, without new generated worlds.

Proposed coverage is a bounded monotone mass-to-area mapping with a stated
representative cover thickness; coverage is not snowfall fraction. Snow over ice
has precedence in area-weighted surface albedo. The same sea-ice coverage enters
gas exchange. Melting is limited by available frozen mass and energy; its latent
cost enters the same enthalpy ledger. Glacier transfers redistribute water already
counted as ice, not an independent precipitation source. D-36 retains a liquid
deep-ocean approximation after formation and excludes complete ocean loss. Specify
its effective mass, thermal coupling and supported phase domain under Q-8; no
depth-triggered 4°C clamp, unaccounted heat source or hidden numerical water floor
implements this approximation. Surface ice can close exchange while liquid bulk
water and its carbon/alkalinity remain present.

### TS-6. Precipitation and local water availability

The current precipitation values are dimensionless scores: `ClimateBuilder`
constructs roughly `0.10 + 0.50*moisture` with pressure/season modifiers, while
`Weather.Lookup`, hydrology, erosion and zoom consume them on that scale. The
candidate must not simply replace those values with numbers in mm/year.

Keep a named legacy score adapter for consumers expecting the old scale. Define
new physical seasonal precipitation as `P_s = P_unit * score_s`, with `P_unit`
in mm/model-year per score unit, one global documented calibration coefficient.
Use the existing spatial moisture/pressure pattern initially; do not choose
`P_unit` from a world's percentiles. A suggested initial search spans
500–3000 mm/year per unit score; this is a game-model search interval, not an
empirical conversion measured from the existing field. Add physical output under
an explicit candidate/final-climate model version, not a silent reinterpretation
of saved `rcPrecipitation` values. Persist the conversion needed by consumers.

Summer/winter values represent annualized rates over two equal-duration
climatological seasons: `Pannual = (Psummer + Pwinter)/2`, not their sum. Rain and
snow partition each rate once. The 250 mm/year predicate uses their combined
water equivalent. Use energy/temperature-based snowmelt for weathering's liquid
input, subtract a separately dimensioned potential-evaporation estimate, and
bound runoff below by zero. Keep precipitation coverage independent of runoff.

The spatial precipitation approximation is a climatological throughput, not a
new atmospheric water stock. If those fluxes advance snow/liquid stocks, debit
their water donor and credit the receiver in the same annual budget, including
the replenishing evaporation/return flow. A requested flux without a donor cannot
create water. Diagnose a mismatch between the moisture proxy and the available
cycle rather than compensating with a hidden positive water floor. This does
not add river-by-river chemical storage or a full moisture circulation solver.

### TS-7. Clock, ordering and bounded numerical integration

Use actual published interval durations, converted once to physical time:
initial bombardment 500 Myr excluded from atmospheric evolution; subsequent Era
events 100 Myr; formation/evolution Period entries 50/30 Myr; Ages use the same
rounded 1–15 Myr duration as `gpDuration` and `gsDate`; Epochs consume zero time.
Do not retain the current mixture of rounded calendar time and unrounded carbon
rate time. Parent structural grouping contributes no additional elapsed time.

Current code calls the bombardment builder at every Eon. Proposed reconciliation
with D-15/D-16: only the first 500 Myr is pre-atmosphere. Later Eon impact-terrain
intervals retain the inherited atmosphere and advance its ordinary source/sink
and thermal evolution for their elapsed duration, without impact-derived
atmospheric reinitialization or direct atmospheric heat injection. Preserve
terrain-event identity. This interpretation needs explicit review if multiple
Eons are intended to mean something different; defaults use one Eon.

At each boundary, apply instantaneous geological/source events once, publish a
single forcing/material snapshot, and integrate the following duration. Final
erosion/terrain changes become the next snapshot; do not alternate terrain and
climate until a chosen result appears. Final climate evaluation uses the final
terrain and stocks with **zero additional geological time**, so it cannot draw
down extra carbon after the timeline ends. If a transient prevents a static
final-climate refinement, publish the actual final thermal state rather than
quietly running an extra settling interval.

Propose a stiff implicit solver with deterministic step refinement, event-aligned
steps and joint positivity constraints. Candidate first method: backward Euler
with one-full-step versus two-half-step error estimation, using a bracketed
carbonate solve within the nonlinear residual. Advance ledgers only for accepted
steps. It must resolve atmosphere/ocean pulse relaxation, not merely agree on a
late endpoint. Tolerances use state-specific units; compare conservation residuals
to accumulated external throughput as specified above.

Do not simulate every season of every geological year. Use bounded representative
seasonal-cycle solves for averaged slow fluxes once the fast system's transient
is resolved. Retain warm/cold branch history under albedo feedback. Reuse a
quasi-steady seasonal solution only when measured residual and timescale/error
criteria justify it; any accelerated interval still advances carbon, water and
energy ledgers over its full elapsed time. Event transients disable that shortcut.
Declare maximum nonlinear iterations, refinements and evaluations per interval
from measured performance before production activation. Exhaustion is a reported
numerical failure, not a seed reroll or an automatic sampling expansion.

### TS-8. Observable metrics and bounded validation

Use the same area-weighted regional samples for temperature, precipitation and
the simultaneous climate-suitable-land predicate. Border/ocean samples never
enter the land denominator. Test exact threshold boundaries on authored fixtures.
Keep the two 90% gates separate under D-20/Q-13; use `ceil(0.9*N)` successes for a
fixed sample of N. An implementation crash is not a tolerated climate miss.
No severity bounds or extra runs are triggered by an allowed outcome miss.

Corpus size is approved by D-40: 10 distinct seeds at size 32, three at size 64,
and one at size 128, all otherwise at defaults. That is 14 generations per
verification run, reused across metrics. Sizes 256/512/1024 and the earlier
60-world and 30-world matrices are excluded. No automatic second matrix is added
for calibration, captures or follow-up checks. Seed manifests identify tuning
versus validation use; any further generation workload needs a separate explicit
budget. CI inclusion remains undecided. Do not run worlds during this docs task.

Apply each approved 90% criterion to the pooled sample: 13/14 must pass, with
separate counts for climate-suitable land and precipitation coverage. Report
results by size without new per-size pass-rate assertions. Retain 66% as the
descriptive center of the existing 56–76% band, not a new mean gate.
The earlier 3°C median/percentile comparison against a size-128 cohort is
withdrawn: a single reference world cannot support the proposed distributional
comparison. D-43 approves size reporting plus controlled scaling fixtures without
adding a replacement statistical pass gate.

For the original regression, D-41 approves one predeclared cold reference within
the 14-world sample: interior snow/ice, cold regions in both hemispheres and
warmer land elsewhere. Use actual final climate/ice consumers; border ice cannot
satisfy the predicate. The earlier two-adjacent-cell patch-size detail remains an
engineering candidate, not an additional owner-approved threshold. Specify the
precise classifier/area sampling before calibration, preserving the qualitative
contract. The known baseline diagnostic was regional, so it does not establish
exact tile fractions or ice-sheet area. Keep visual assessment separate from the
automated predicate and reuse the same generated world.

### TS-9. Persistence, interfaces and evidence gates

Use pure module boundaries for chemistry/flux integration, radiation/phase
properties, regional thermal evaluation, and geological adapters. Suggested
operations are `initializeModel`, `applyGeoBoundary`, `advanceInterval`,
`evaluateFinalClimate` and `summarizeBudget`, each consuming explicit immutable
inputs. No UI-config read, random draw or filesystem access occurs inside a
numerical residual. All stochastic inputs are fixed before the solver runs.

Persist final climate, reconstruction-required snow/ice outputs and a model
identity with reference-data/parameter hashes in the appropriate world-pages
schema. Preserve old DTOs and migrate their identity to a legacy recipe. A loaded
legacy world uses its saved climate and legacy chunk/zoom consumers; it never
reruns this geological solver. Do not create fictional ocean carbon or alkalinity
to retrofit an old save whose completed climate never had those inventories.
Numerical work arrays, traces, intermediate rock budgets and integration counters
are excluded unless an explicit persisted consumer is identified. Compatibility
proof requires actual chunk/zoom regeneration after load, not only successful
DTO decoding. CARBON-12 owns this boundary before activation.

Reference-data and coefficient acceptance are separate. CARBON-8 supplies
versioned, reproducible reference functions/tables and independent fixtures;
CARBON-4 selects globally shared tunable values from declared ranges and records
sensitivity. Neither artifact may be generated from the desired per-seed final
temperature. Existing worldgen-output, save-compatibility, inventory and cache
gates still apply at implementation; no implementation or engine testing is
performed by this documentation pass.

### TS-10. Remaining review items after this pass

D-36/D-37 resolve Q-12 (complete ocean loss) and Q-14 (orbital scope).
Q-13 is resolved by the existing D-20 decision. D-40 settles the practical
14-world sample. D-41 approves the cold-reference regression; D-43 settles
size-comparison reporting without a new statistical gate. D-42 preserves existing
durations and exposes early cooling through intermediate snapshots. One top-level
Period contains two 50/30 Myr entries plus nested Ages, and an Era contributes
100 Myr before them; the integration/checkpoint implementation must respect that
clock without stretching a fast transient to fill three entries.

D-39 resolves salinity's evolving budget and phase-transfer structure; Q-15 retains
its technical initialization/composition/rate contracts. The temporary reference
input must not be promoted to the final model contract.

Technical evidence still required before affected implementation: the hot/mature
radiation and carbonate constant domains; actual material-to-reactivity mapping
and exposure fluxes; initial inventory/enthalpy values within those domains;
and reference data licensing/reproducibility. These are assigned engineering
research deliverables, not questions asking the owner to invent chemistry constants.
The remaining listed work is assigned technical specification, reference research
and validation. The 2026-09-10 readiness review checked those prerequisite/stop
contracts, delivery boundaries and tracker overlap. They permit issue processing
while preserving the downstream implementation prerequisites.

## Open questions

### Remaining design work after the technical pass

TS-1–TS-10 above are the completed first technical-specification pass against
this checklist. They define proposed equations, units, interfaces and delivery
owners. D-40–D-43 close the remaining sampling, cold-regression and clock choices.
The revised plan contains twelve delivery slices. The following technical
prerequisites are deliberately open; they do not require the owner to invent
physical constants or expand the approved feature scope.

| Prerequisite | Required artifact | Ownership and timing |
|---|---|---|
| Physical references (Q-8/Q-9) | Versioned radiation/phase reference functions or data, validity domains, provenance and independent fixtures; justified initial inventories and enthalpy. | CARBON-8 supplies references before dependent mechanisms are implemented. |
| Chemistry and geological rates (Q-5/Q-15) | Carbonate constants, consistent carbon/alkalinity/salt stoichiometry, initial salt composition and reviewed rate laws. | CARBON-9/10 resolve these before CARBON-2 consumes them. D-39 requires evolving salinity. |
| Geological supply (Q-5) | Material-to-reactivity mapping, intrinsic source magnitudes and actual gross exposure/deposition inputs or a reviewed budgeted approximation. | CARBON-11 resolves adapter evidence before CARBON-5 integration. |
| Coupling and clock diagnostics (Q-8/Q-9) | Conservative solver/handoff interfaces, phase budgets and bounded physical-time/phase checkpoints under D-42. | CARBON-2/7/6 validate their mechanisms before CARBON-5 integration; preserve geological durations. |
| Measurement and calibration (Q-4) | Physical precipitation conversion, declared seed identities and exact cold-reference sampling/predicate before tuning; globally shared coefficients and size reports. | CARBON-4 uses D-40's fourteen worlds and D-41's reused reference. D-43 adds no size gate; 66% is descriptive. No follow-up tests of tolerated misses. CI inclusion remains undecided and is not implicitly activated. |
| Compatibility (Q-3) | Versioned DTO/reconstruction ownership and behavioral proof that supported legacy worlds retain their output. | CARBON-12 establishes TS-9's preservation boundary before activation. |

An affected downstream slice must stop at an unavailable upstream reference or
unreviewed closure rather than invent a law to finish implementation. Return to
the owner if evidence requires changing approved behavior or scope. Check each
slice against the resulting interfaces before drafting it and split any outcome
that cannot fit one reviewable PR. Measured coefficients and proof that the model
passes are delivery work, not claims made by this document. The design is ready
for issue processing with these deliberately open prerequisites; the approved
behavior choices do not need another round of individual approval.

### Q-1. Which climate extremes should ordinary settings permit?

Resolved by D-4. Naturally extreme worlds are allowed; ordinary settings should
usually be habitable. Q-4 refines that policy into a measurable target.

### Q-2. What minimum reservoir and geological-time model is sufficient?

Reservoir choice resolved by D-6: separate atmosphere and ocean, with an exchange
function. The one-effective-reservoir proposal was rejected because the owner
values ocean uptake/release and transient atmospheric effects. Land and seafloor
weathering remain in scope. The technical-contract table proposes successive
additive physical intervals and inherited carbon across Eons; record exact units
and integration policy with the model. D-16 excludes pre-atmosphere bombardment
from atmospheric integration. Q-5 now owns chemistry/box interpretation.

### Q-3. How should existing saves behave?

Policy resolved by D-7: corrected newly generated worlds, with no silent
rewriting of saved climate or terrain. Verify the technical strategy before
CARBON-3: chunks regenerate from saved parameters, so keeping only a saved CO2
field may be insufficient when generation formulas change.

The question offered a clear incompatibility, preservation, or explicit
migration. Further repository inspection favors preservation where possible:
`World.Load.Stage` restores saved `WorldGenParams`, including climate and
geological history, instead of replaying the entire geological timeline.
Isolating new equations to timeline construction may preserve old outputs without
forking the entire old generator. Prove that by reconstructing old chunks and
zoom pixels; retain a legacy recipe only for changed reconstruction consumers.

`docs/persistence_contract.md` §5 explicitly promises compatibility from the B1
baseline. An intentional break would therefore require an explicit owner change
to that promise and corresponding contract/fixture updates; a global save-version
bump alone does not implement a clear wire-format refusal. Do not discard
supported fixtures to make an accidental incompatibility pass.

### Q-4. What does usually habitable mean operationally?

Policy resolved by D-8, D-12 and D-21: approximately 90% of ordinary worlds should
have at least 20% of non-boundary exposed land simultaneously at annual ambient
temperature 0–30°C, with a thawed summer and annual precipitation at least
250 mm water equivalent. Ten percent
remains a possible explicit later adjustment, not the initial criterion.

Remaining measurement work: fix the precipitation conversion, exact seed
identities and spatial area weighting before tuning. D-40 specifies 10 size-32,
three size-64 and one size-128 worlds, with each 90% criterion applied to the pooled
sample (13/14). Ordinary configuration scope is resolved by D-29: defaults,
varying only size and seed. D-43 resolves cross-size verification through reporting
and controlled scaling tests, without a new statistical difference gate. D-41
defines the cold-reference behavior; record its exact sampling predicate before tuning. These are
ensemble calibration and validation criteria, never per-world corrections or
rerolls. CARBON-4 depends on these details; it does not need the owner to reapprove
the already selected 20% land fraction or 90% frequency direction.

Treatment of allowed misses is resolved by D-29: accept them without additional
severity limits or follow-up testing. The owner rejected the proposed outer
coverage bands. D-40 fixes the bounded sample; whether to include it in CI remains
undecided. Do not expand work in
response to sampled worlds missing the outcome criteria.

Observable direction resolved by D-19: precipitation coverage of about 66%, not
wet terrain tiles or lake counts. The earlier sustained-season question was
premature and has not been approved. The threshold is 250 mm/year of total
precipitation, including snowfall in water-equivalent units. Set the conversion
and annual aggregation of seasonal fields before tuning; D-20 resolves the
coverage band and ordinary-world pass rate. Evaporation adjustment is not implicitly approved as
part of this precipitation metric. Weathering still needs its separately defined
liquid-water input. D-21 resolves the precipitation threshold's role in D-12's
combined 20% criterion; it does not measure surface freshwater access or
year-round liquid water.

Coverage-tolerance choice resolved by D-20: at least 90% of ordinary test worlds
have 56–76% qualifying land. Treat 66% as the descriptive center, with no new
corpus-mean gate. Statistical interpretation respects D-40's small unequal sample. Report size
cohorts without per-size 90% assertions or requiring expensive larger-world runs.

Combined-criterion definition resolved by D-21. Its three climate thresholds
apply at the same locations, with arable land and drinking water excluded.

Unit evidence checked on 2026-09-10: `src/World/Weather/Types.hs` describes
`rcPrecipitation` as mm/year equivalent, but its default is 0.5 in both seasons;
`Generate/ClimateBuilder.hs` builds seasonal values from `0.10 + 0.50 * totalMoisture`
and modifiers. Audit the physical conversion and consumers before applying
the proposed real-world mm/year cutoff; the field comment alone does not
establish that raw values can be compared directly with such a threshold.

### Q-5. How should the ocean box and carbonate chemistry be represented?

Chemistry approach resolved by D-9: a compact carbonate-equilibrium calculation
with an explicit alkalinity budget. The two-carbon-reservoir choice remains D-6.
Ocean-box interpretation and runtime scope are resolved by D-10/D-11. Before CARBON-2,
settle the remaining chemistry contracts:

- The mapping from ocean DIC, temperature, salinity and alkalinity/buffering to
  surface pCO2: included chemical species, equilibrium constants, units, valid
  temperature/salinity/concentration range and solver convergence criteria.
  Independently checked reference states must establish the scale; computing
  expected values with the same solver is insufficient.
- Initial inventories/chemistry, atmospheric capacity, ocean volume,
  weathering delivery and burial
  stoichiometry. A starting equilibrium is an initial condition, not a repeated
  reset. Track any carbonate-rock carbon entering the two-box system separately.
- D-25's per-feature source contracts: physical size/magma-supply proxy, type
  yields, activity-state rates, pulse magnitudes and exclusion of double-counted
  releases. Map existing source/parent identities and dimensions without making
  render count or timestep count an emissions multiplier.
- D-26's regional reactive-capacity units, lithology mapping, exposure and
  replenishment inputs, depletion/reaction stoichiometry, and generation-state
  classification. Specify how this relates to the separate seafloor pathway.
- Exchange timescale and surface-temperature inputs; D-39 supplies the salinity
  budget, with Q-15's remaining composition/rate contracts. D-28 resolves the sea-ice
  area rule, with liquid-volume/carbon/alkalinity partition effects still to specify. Resolve
  ocean mixing and atmospheric pulse responses in controlled transient tests;
  choose an integration scheme that stays accurate across the fast/slow scales.

The pathway is approved by D-18: land weathering transfers consumed
atmospheric carbon into ocean DIC and supplies alkalinity according to explicit
reactions; carbonate burial removes carbon and the corresponding alkalinity from
the ocean. Account for rock-derived carbon separately. Treat geological inputs
and burial as recorded external fluxes to/from the two active carbon reservoirs,
without adding an evolving sediment or mantle carbon reservoir in this arc.
Burial responds to the chosen chemistry and material constraints, rather than
removing an arbitrary fraction each Age or balancing emissions by construction.
The full weathering/burial stoichiometry and seafloor pathway still need contracts;
approval of this high-level pathway does not settle their numerical coefficients.

Salinity evolves through the D-39 global dissolved-salt budget. A fixed reference
is a temporary input for preliminary work, not the final ocean-chemistry requirement.
Q-15 retains initialization, mass/composition conversion and source/removal rates,
including their chemical validity range through ocean formation. Current code evidence:
`src/World/Weather/Generate/ClimateBuilder.hs`
assigns `baseSalinity = 35.0` to ocean cells, and `World.Weather.Types` labels
`ocSalinity` in parts per thousand. Reusing that exact number is not yet a
validated primordial chemistry choice.

Volcanic-source routing is resolved by D-17/D-33: exposed and lake-covered
sources emit into the atmosphere; connected-liquid-ocean sources emit into ocean
DIC. Temporary lake storage and delayed release are approximated away. No lake
carbon stock or detailed lake chemistry is added in this slice.

Initialization direction is resolved by D-14: begin with primordial conditions
and simulate their evolution. The simpler reference start and the later
post-condensation start were not accepted. Q-8 owns the primordial model and its
handoff; Q-5 owns the resulting liquid-ocean chemistry, including how initial
alkalinity and dissolved carbon are obtained without creating inventories at
the handoff. Air-sea equilibrium and CO2 abundance remain distinct concepts,
but no initial exchange equilibrium is imposed by D-14. Initialization occurs
once, never as a reset between Eons. Exact initial values remain to be justified.

Research context: [Miyazaki and Korenaga (2022)](https://www.nature.com/articles/s41586-021-04371-9)
model a transition from harsh post-magma-ocean conditions to a habitable Hadean
surface through greenhouse-gas removal. The timing and mechanisms remain
uncertain. [Krissansen-Totton et al. (2018)](https://doi.org/10.1073/pnas.1721296115)
predict temperate Precambrian climates with carbon-cycle feedbacks. These support
distinguishing the formation phase from subsequent geological history; they do
not supply an exact initial CO2 value or validate this game's climate formula
at primordial pressures.

These reference and closure contracts are deliberately assigned to CARBON-8/9/10
before downstream implementation. Recheck dependent slice size once those
artifacts are concrete; changes to approved behavior return to the owner.

### Q-6. What does the ocean carbon reservoir represent?

Resolved by D-10: one global ocean box with finite exchange and an effective
mixing approximation. Separate surface/deep-ocean inventories and circulation
are out of scope. Keep the distinction between bulk inventory/volume and
the water properties used for surface gas exchange explicit.

### Q-7. Does atmospheric/ocean carbon continue evolving during gameplay?

Resolved by D-11: generation only. Gameplay-time carbon evolution would require
a separate explicitly designed runtime feature; it is not deferred work inside
this arc.

### Q-8. How do primordial conditions evolve into the geological carbon model?

Starting-phase choice resolved by D-14; continuous evolution in the existing
timeline, without a separate equilibration run, is resolved by D-15.
D-16 resolves bombardment ordering: it completes before primordial atmospheric
initialization and contributes no elapsed atmospheric evolution time.
Approximation scope is resolved by D-31: a global reduced energy/water model
coupled to carbon, without individual magma flows or regional crust
solidification. This approximation still needs explicit heat capacity,
solidification/condensation energy, absorbed and outgoing radiation, and declared
internal heat inputs consistent with D-16's bombardment independence. Specify
the D-35 gas model's radiation/pressure laws and validity rather than assuming
a modern CO2 response covers steam conditions.
Water and energy need their own auditable budgets. No ocean chemistry operates
on nonexistent liquid volume; carbon and alkalinity at ocean formation must
come from declared transfers and sources. Define phase transitions and subsequent
coupling so that they conserve stocks and do not introduce a temperature jump.

Timing interpretation is resolved by D-42: preserve post-bombardment Era/Period
durations and expose rapid cooling through intermediate snapshots, without
double-counting time or adding a separate settling phase. CARBON-6/5 choose
bounded physical-time/phase checkpoints from the reference model; no forced
two-or-three-boundary schedule applies. CARBON-8's physical references and the
CARBON-2/7 coupling interfaces remain prerequisites for CARBON-6 and candidate
integration, including conservative ocean formation and regional handoff.

Atmospheric-composition scope is resolved by D-35: evolving CO2 and water vapour
with a fixed background gas inventory scaled by area. Methane chemistry,
evolving oxygen/biology and volcanic sulfur-aerosol cooling are excluded. Specify
the background composition/amount and pressure/radiation approximation with the
physical model; total pressure still responds to CO2 and water vapour. Existing
cloud fields remain under D-23's scope.

Stellar-evolution scope resolved by D-22: stellar output is constant in this
slice, and stellar evolution is a desired future feature. Primordial thermal
calculations still need an actual energy-input contract even if the later
regional climate adapter keeps its existing insolation approximation.

Atmospheric independence from bombardment is resolved by D-16; deriving the
starting atmosphere from impact history is rejected. Initial-condition variation
is resolved by D-30: shared reference carbon, water and heat inventories per
unit area, with totals and capacities scaled consistently under D-13. There is
no seed-dependent primordial variation or retuning by other generation settings.
Subsequent geological histories create variation. Exact inventory values and
the thermal/water initialization model still need technical specification.

### Q-9. Should changing snow/ice coverage feed back into temperature in this slice?

Scope resolved by D-23: include reduced albedo-temperature feedback, with
greenhouse forcing already in scope. Snow and ice reflect sunlight; changing
their coverage can amplify warming or cooling
([NASA Snow science overview](https://snow.nasa.gov/science)). More uniform ice
is an intended outcome to investigate, not a guarantee made by this feedback.

Accepted direction: include a reduced surface-albedo feedback in this slice,
with separate coefficients for ocean and a small set of supported ground types,
plus snow/ice coverage that changes with the modeled accumulation and melt.
Feed this into absorbed solar energy, together with a documented atmospheric
reflection/transmission approximation; surface albedo is not identical to total
planetary albedo. Keep CO2-dependent greenhouse response in outgoing heat/thermal
balance and explicitly handle water vapour in the primordial regime under Q-8.
Avoid adding a second CO2 warming term if that effect is already represented in
the heat-loss approximation. Modern logarithmic forcing must not be extrapolated
unchanged across the primordial domain.

Defer new detailed ice-flow, snow-grain aging, evolving vegetation-albedo and
dynamic cloud-feedback models. This does not remove existing glacier behavior,
cloud fields or sea-ice effects on carbon exchange. Derive snow-covered area
from a declared coverage/accumulation approximation, not merely the fraction of
precipitation falling as snow. Refresh it as climate evolves, using consistent
time integration and water/energy accounting; do not force a selected equilibrium.

The earlier recommendation to defer all dynamic surface-albedo feedback is
superseded by D-23. As part of the coupled model,
test more reflection causing less absorbed solar energy, greenhouse response,
conservation, timestep convergence and coupled freezing/melting transitions.
Valid frozen states are not numerical failures. Specify the coverage sampling,
regional heat-distribution approximation and temporal coupling before CARBON-7;
do not treat the repository's stored but unused albedo values as a working
feedback implementation. Q-8 owns the shared radiation approximation and its
primordial domain. Check local/regional stability and artificial patchiness with
fixed terrain and forcing fixtures as well as generated maps.

### Q-10. Does chemical weathering also transform terrain materials in this slice?

Resolved by D-27: represent chemical depletion in the D-26 regional budget.
Terrain-material transformation is explicitly wanted as a separate future
feature. Existing geological erosion/deposition continues to shape terrain;
regional chemical depletion does not itself change mineable material identities.
CARBON-5's material/exposure adapter must respect this boundary.

### Q-11. Does ocean volume determine sea level and coastlines during generation?

Resolved by D-32: keep the existing reference sea level and use an explicitly
approximate mapping between effective liquid-ocean volume and ocean footprint.
Liquid-ocean processes require liquid water; model water conservation is not a
claim of exact agreement with rendered tile-depth volume. Q-8 retains the
technical volume/footprint coupling through condensation and freezing.

Current code fixes `seaLevel = 0` in `src/World/Constants.hs`; ocean identification
compares terrain with that level. River/coast and lake consumers also depend on
sea level. The earlier proposal to derive a changing level from water volume
would align those quantities, but the owner accepted the simpler approximation
for this slice. A future changing level can be represented by a global offset;
no new sea-level delivery slice is approved here.

### Q-12. Can a formed ocean completely freeze or evaporate in this slice?

Resolved by D-36: retain a liquid deep-ocean approximation after formation;
complete ocean freezing/evaporation is outside scope. Surface snow/ice still
evolves and blocks exchange under D-28. Q-8 retains effective depth/mass, thermal
coupling and domain specification. The owner's 4°C/depth explanation is corrected
in D-36; do not replace that technical work with a fixed-temperature thermostat.

### Q-13. Do the same 90% of ordinary worlds need to pass both outcome criteria?

Resolved by D-20: the precipitation and climate-suitability pass rates are
separate. This was unnecessarily asked again during the technical pass; no
reapproval is needed. A joint 90% rate would be a stronger new requirement and
must not be introduced implicitly. Retain D-29's fixed bounded sample and
acceptance of allowed misses without follow-up tests.

### Q-14. Are long-term orbital changes excluded?

Resolved by D-37: retain existing latitude/seasonal inputs and defer geological
changes in orbit or tilt. Stellar evolution is separately excluded by D-22.
Removing the unbudgeted CO2 sine wave does not require a replacement orbital cycle.

### Q-15. How is salinity calculated and carried through geological history?

Budget/phase structure is resolved by D-39: one evolving global dissolved-salt
inventory, with salt-free ice/evaporation and dilution by returning water.
D-38 limits fixed salinity to a temporary reference. Specify initialization,
reaction-to-salt mass conversion, geological input/removal coefficients,
and use of effective water mass under
D-32/D-36. State the mass/concentration convention and physical units. A formula
that depends on water quantity must distinguish pure-water mass from solution
mass and avoid dividing by nonexistent liquid during primordial conditions.

Keep current salinity as an explicit input to chemistry, solubility, saturation
and freezing. Any supporting salt state is not another carbon reservoir, but
needs its own ownership and conservation/approximation contract. The existing
mineral-saturation background in TS-3 requires review for consistency, rather
than adding salt from weathering and also treating its chemical effect as an
unrelated fixed background. Numerical coefficients and detailed regional salt
circulation have not been chosen. Resolve the model before finalizing affected
CARBON-8/9/10/6 interfaces and the delivery plan. The budget structure is approved;
these remaining technical contracts do not reopen that choice.

Options considered; D-39 selects the evolving budget:

| Option | Behavior | Tradeoff |
|---|---|---|
| Approved: one evolving global dissolved-salt budget | Derive ocean salinity from dissolved-salt mass and liquid solution mass. Declared rock/seafloor dissolution supplies salt; declared mineral deposition/burial removes it. Water-phase transfers change concentration. | Gives geological history a direct effect on salinity; requires explicit salt sources/removals consistent with the existing carbon/alkalinity reactions. |
| Alternative: a fixed total available-salt budget | Declare one initial salt supply per world area, account for its dissolution as the ocean forms, then retain the dissolved budget while water quantities change. No later geological salt input/removal. | Salinity still evolves with dilution/concentration, but later weathering and mineral removal do not change the salt budget. Simpler and less coupled to geological history. |

Both options keep one global ocean salinity rather than regional surface/deep
salt circulation, and feed that salinity into the existing chemical/freezing
interfaces. Neither adds a carbon reservoir or forces a target salinity. For a
minimal partition rule, D-39 approves salt-free ice/evaporation: salt remains in
liquid water; melting/condensation returns water and dilutes it. This approximates
real sea ice, which expels much of its salt but can retain brine. Geological salt
sources are supported by [NOAA's overview](https://oceanservice.noaa.gov/facts/whysalty.html),
and the ice-growth/melt distinction by
[NSIDC](https://nsidc.org/learn/parts-cryosphere/sea-ice/why-sea-ice-matters).

The initialization must identify where soluble material resides before liquid
exists and how it enters solution; do not materialize a full ocean salt inventory
in an arbitrarily tiny first condensate or assert the real primordial ocean was
fresh. The evolving-budget option can use the reactive geological input model;
the fixed-budget alternative needs an explicit finite initial dissolution source.
No input/sink coefficient, exact initial salt mass or mineral-composition mapping
is selected by presenting these options. Salt/carbon/alkalinity cross-accounting
and independent mass-budget tests remain part of either chosen specification.

## Scope

In scope: primordial thermal evolution and ocean formation (D-14),
two-reservoir geological carbon balance, finite air–sea exchange,
the ocean chemistry needed to define it, elapsed-time accounting, climate coupling
including reduced albedo-temperature feedback (D-23),
affected glacier/ice/vegetation/zoom consumers, diagnostic measurements,
determinism, compatibility, and meaningful tests. No missing art is identified.

Approved exclusions from D-10/D-11/D-22: separate surface/deep-ocean carbon
reservoirs, gameplay-time carbon evolution, and stellar evolution. Stellar
evolution is explicitly desired as a future feature, not rejected permanently.
Approved exclusion from D-32: deriving sea-level change from the water inventory;
retain fixed reference sea level with an explicit effective-volume approximation.
D-38 supersedes D-34's earlier salinity exclusion; D-39 includes evolving global
dissolved-salt accounting with salt-free ice/evaporation, excluding regional salt
circulation and detailed brine retention. Q-15 retains technical rate/composition work.
Approved exclusions from D-35: methane chemistry, evolving oxygen/biology and
volcanic sulfur-aerosol cooling; evolve CO2 and water vapour with fixed background gas.
Approved exclusions from D-36/D-37: complete freezing/evaporation of a formed
ocean and long-term orbital changes; retain liquid deep water and seasonal inputs.
Approved exclusions from D-23: new detailed ice-flow physics, snow-grain aging,
evolving vegetation-albedo feedback and dynamic cloud feedback.
Approved exclusion from D-27: new chemical transformation of terrain materials
into soil or other weathering products, explicitly desired as a future feature.
Approved exclusion from D-28: detailed gas transport through sea ice and
unresolved cracks; use the declared open-water fraction in this slice.

Other proposed exclusions: a full circulation model, a comprehensive ocean ecosystem
or full three-dimensional chemistry model, a live
seasonal weather simulation and automatic repair of existing worlds.

## Verification strategy

- **Albedo/thermal coupling:** with other inputs fixed, greater surface
  reflectivity reduces absorbed sunlight; validate CO2 response separately,
  then test the coupled energy budget, timestep convergence, freezing/melting
  transitions and snow-cover fractions. Verify no artificial border contribution
  and no double-counted CO2 warming. Controlled spatial fixtures and offscreen
  transects assess whether ice/ocean boundaries follow local conditions rather
  than numerical flicker or classification inconsistencies. Albedo alone is not
  evidence of smoother ice flow or elimination of all isolated ice.
- **Primordial evolution:** independently check energy and water budgets,
  cooling/phase-change reference cases, and conservation through ocean formation.
  Verify the carbon/alkalinity handoff and the valid domains of both climate
  approximations. Under D-15, inspect the full trajectory through the actual
  geological loop, including the sharp early decline and later response to events.
  Assert the D-16 ordering: no atmospheric integration during bombardment,
  one initialization afterward, and continuous state across later intervals.
  With atmospheric configuration and area/capacity convention held fixed,
  changing bombardment history must not alter initialized atmospheric conditions;
  later differences caused by resulting terrain are permitted.
  Test elapsed-time invariance under subdivision; no checkpoint overwrites state
  to meet a temperature or equilibrium target.
- **Budget:** per-reservoir and combined stock changes equal declared sources
  minus sinks/transfers within a documented tolerance. Internal exchange cancels
  from the combined budget; no hidden sine injections or floor-created carbon.
- **Weathering/burial:** isolated land-weathering transfer preserves combined
  carbon except for separately recorded rock-carbon inputs. Burial reduces
  ocean and combined carbon by the recorded export and changes alkalinity by
  the declared reaction stoichiometry. Verify that these processes do not
  double-count removal or balance emissions by construction.
- **Reactive supply:** declared reactions consume regional capacity; exhaustion
  limits further weathering and a declared replenishment restores available
  capacity. Verify material-specific limits, zero supply, history dependence,
  and invariance under subdivision of unchanged geological history. Replenishing
  capacity must not itself inject atmospheric/ocean carbon without a declared
  carbon-bearing transfer.
- **Exchange:** test air-to-ocean uptake, ocean-to-air release, zero net flux at
  equal partial pressures despite unequal inventories, zero ocean area, and
  donor nonnegativity. Pure exchange conserves `A+O`. An atmospheric pulse has
  a resolved transient peak and recovery; instantaneous equilibration must fail
  that fixture. Test chemistry closure and the alkalinity budget independently;
  include fixed reference states, reaction stoichiometry, concentration/volume
  conversions, domain failures and convergence across exchange timescales.
- **Sea-ice exchange:** with the same chemical potentials and per-area transfer
  coefficient, half the open area gives half the flux and zero open area gives
  zero direct flux. Freezing coverage alone preserves ocean carbon; underwater
  inputs remain budgeted, and exchange resumes with thawed surface. Verify
  shared coverage inputs with albedo and exclusion of artificial border ice.
- **Volcanic routing:** an equal release enters only the correct reservoir for
  exposed and ocean-submerged sources; routing follows a change in source
  submergence. Before liquid oceans exist, surface emissions enter atmosphere.
  The combined stock increases by exactly the release, regardless of destination.
- **Volcanic magnitude/time:** for a fixed type/activity/history, a larger
  declared geological source magnitude produces more carbon; changing activity
  or type uses the documented profile. Partitioning the same elapsed interval
  preserves integrated emissions, and an exceptional pulse appears exactly once.
  Cover all feature kinds and ensure resampling/feature subdivision does not
  manufacture carbon. Test zero volcanic input under the zero-activity setting.
- **Units/time:** equivalent elapsed time and fixed forcing converge under
  subdivision; independent changes to spatial resolution preserve area-scaled
  flux. Never require identical outcomes for different random event histories.
- **Feedback:** in a controlled fixture with one independently established
  stable equilibrium and no additional attractors, high and low initial carbon
  approach that equilibrium and a pulse relaxes toward it. More sustained
  degassing shifts that fixture's equilibrium upward; increased weatherability
  shifts it downward with other conditions fixed. Do not demand convergence to
  one common state for arbitrary coupled albedo histories: multiple stable
  states or hysteresis must not be mistaken for a numerical failure.
- **Limits:** dry/frozen land and exhausted rock supply reduce land removal;
  ocean-only and extreme forcing cases are explicitly classified; zero-duration
  steps, event boundaries, and numerical positivity do not corrupt the budget.
- **Climate:** finite fields, equal warming per doubling within the selected
  parameterization, correct mean/grid agreement, and consistent final forcing
  across all consumers. Changing these expectations is a model decision.
- **World outcomes:** retain seed 1334661219/64/10 and seed 42/256/17 as regression
  evidence, with a small declared ordinary and stress-case seed/config matrix.
  Set numerical outcome bands from D-4/Q-4 before tuning, not by re-blessing
  whatever the new code generates. Track CO2, sources/sinks, region temperatures,
  snow/tundra eligibility, interior ice coverage, and ice modes.
- **Visual evidence:** offscreen captures of polar transects at terrain and map
  zooms, checking the tundra transition, snow, sea ice, and basin/drape surfaces.
  Eligibility counts and a headless success do not prove rendered appearance.
  Under D-24, review regional and world-to-world variety as well as coherence;
  preserve believable patchiness and irregular boundaries. Automated gates do
  not prove natural appearance. Record owner visual assessment of the declared
  corpus before activation, without selecting only the most attractive outputs.
- **Repository gates:** targeted Hspec/tool tests during iteration. Changes to
  worldgen output require the full tier, captured baselines, world checks, and
  save-version treatment in `src/World/CLAUDE.md`. Baseline agreement alone is
  not evidence of a healthy climate. Run `make ci` only if explicitly requested.
- **Persistence:** classify any new inventoried state; use the persistence
  contract and save owner rules. Positional DTO changes require component
  migrations and frozen historical wire types, not only a global version bump.

### Concrete test specification and implementation prerequisites

This is a test design, not a report of executable tests passing. The candidate
two-reservoir, reactive-supply and albedo mechanisms are not implemented yet.
Tests must exercise production candidate functions when their owning slice lands;
do not create a duplicate toy implementation solely to make these assertions pass.

Current coverage was checked on 2026-09-10. The existing headless group
`Final climate refinement` in `test-headless/Test/Headless/World/Climate.hs`
checks five areas: CO2 sensitivity, global/regional agreement, finite output,
independently pinned synthetic temperatures, and final-forcing wiring through a
shared generated world. Its exact 3°C rise for a CO2 change from 1.0 to 1.5 pins
the current linear formula. Preserve those legacy expectations while the old
model is the production default; give the candidate independent fixtures and
replace/version affected expectations deliberately during activation. Re-running
that group alone cannot demonstrate any new carbon budget or ice-exchange rule.

The following stable case IDs describe intended behavior, not existing test names:

| Case | Controlled fixture and independent expectation | Delivery owner |
|---|---|---|
| CT-01 Carbon transfer | Start with atmosphere 100 and ocean 900 in declared test carbon units. Applying a prescribed air-to-ocean transfer of 7 gives 93 and 907, total 1000. The reverse transfer restores the stocks. Test source inputs and burial separately against externally specified quantities, including donor exhaustion. | CARBON-2 |
| CT-02 Exchange potential | Reference carbonate states from an independent calculation exercise uptake, release and equal-potential zero flux despite unequal stocks. Check finite relaxation after a pulse against an independent reference solution, not an instantaneous equilibrium reset. CARBON-8 supplies the Q-5 reference chemistry. | CARBON-9 chemistry; CARBON-2 finite exchange |
| CT-03 Sea-ice area | Hold both chemical potentials, exposed-ocean geometry and the per-area coefficient fixed. The instantaneous exchange at ice fractions 0, 0.5 and 1 is respectively F, F/2 and 0. Compare instantaneous flux, not a full evolving trajectory whose potentials change. Closing and reopening surface preserves inventories and restores eligible exchange; underwater input still increases ocean carbon while covered. | CARBON-2; shared coverage wiring in CARBON-5 |
| CT-04 Source/time budget | An injected source rate of 3 carbon units/Myr over 2 Myr contributes 6, whether integrated as one or several intervals. Add a separately specified pulse exactly once, including at an event boundary. Verify type/activity profiles and monotonic geological-magnitude scaling with other inputs fixed; zero-activity input emits zero. Small routing fixtures send exposed/lake-covered sources to atmosphere and connected-ocean sources to ocean DIC; changed environment affects only subsequent emissions without duplication. Production source proxies wait for Q-5. | CARBON-2 and CARBON-5 |
| CT-05 Weathering supply | Use known initial reactive capacity and declared reaction stoichiometry. Consumption cannot exceed capacity; zero supply halts that reaction; a specified fresh-material addition permits the independently calculable additional reaction. Repeated substeps cannot refill capacity. Verify carbon/alkalinity destinations, including separately declared rock carbon. | CARBON-2; geological replenishment adapter in CARBON-5 |
| CT-06 Spatial scaling | Replicate an identical forcing/geography fixture and scale sources, area and reservoir capacities together; concentration histories agree within the numerical tolerance. Separately resample the same physical source without adding emissions. Real generated sizes use outcome comparisons, not an identical-climate assertion. | CARBON-2 and CARBON-5 |
| CT-07 Thermal mechanisms | Under fixed incoming radiation and declared atmospheric transmission, higher surface reflectivity reduces absorbed energy by the independently derived amount. Test greenhouse response separately and check the joint energy ledger without duplicated CO2 warming. Validate snow/ice coverage transitions and timestep convergence; no assertion that albedo must remove all small ice patches. | CARBON-7 |
| CT-08 Primordial chronology | Trace shows bombardment before the single atmosphere initialization, no atmospheric integration during that interval, and inherited state through all later periods. Under D-30, changed seed, impact history or other generation settings leave primordial reference conditions unchanged; size scales inventories and capacities with area, preserving initial intensive conditions. Exercise this with small initialization fixtures, not an additional generated-world corpus. Verify energy/water/carbon conservation through ocean formation. Under D-42, preserve geological durations and capture rapid cooling at bounded intermediate checkpoints using Q-8's reference configuration and physical time/temperature bands; no forced two-or-three-period schedule. | CARBON-6 and CARBON-5 |
| CT-09 Climate predicate | Hand-authored climate samples test inclusive annual 0°C/30°C bounds, strict summer >0°C, and precipitation 249/250/251 mm/year. Qualifying fractions use the intersection at the same locations; construct disjoint individually qualifying sets that fail the combined 20% threshold. Exclude ocean/artificial borders and use area weights; a world without exposed land fails. Arability and drinking-water data are not inputs. | CARBON-4 |
| CT-10 Precipitation metric | Known annual rain and snow-water inputs sum once; physical snow-depth changes at fixed water mass leave precipitation unchanged, and later snowmelt adds no precipitation. Test seasonal duration weighting/conversion once its units contract is settled. Hand-authored cohort results exercise inclusive 56%/76% coverage bounds and at-least-90% pass counting, including non-multiples of ten. | CARBON-4 |
| CT-11 Generated regression | Retain failing seed/config 1334661219/64/10 and historical diagnostic 42/256/17 as evidence. Reuse an eligible size-64 slot for the original seed when its resolved settings match D-29; do not automatically add a custom-config run. D-40 excludes rerunning size 256 from the bounded matrix. Check independent budgets, final forcing/consumer consistency and declared cold-reference outcomes on eligible existing fixtures. Border-only ice must fail an interior-ice criterion. Define cold coverage before candidate tuning; never copy new output into expectations as the sole justification. | CARBON-5 and CARBON-4 |
| CT-12 Corpus and appearance | Use D-40's 14-world matrix (32×10, 64×3, 128×1), with 13/14 passing each separate D-20/D-21 criterion and per-size reporting only. Keep seed manifests explicit about tuning versus held-out evidence without automatically duplicating the matrix. Accept misses without severity checks, retries, extra samples or follow-up tests. Reuse generated outputs for metrics and representative visual evidence. Larger worlds are outside this practical matrix; custom runs need their own budget and never enter its denominator. CI inclusion remains undecided. Automated results do not certify aesthetic quality. | CARBON-4 |
| CT-13 Compatibility | Load supported legacy fixtures and reconstruct chunks/zoom through the actual load path; verify preserved output and fresh-world candidate identity. This complements existing persistence gates and cannot be replaced by a serialization-inventory audit. | CARBON-12; activation verification in CARBON-3 |

Numerical tolerances remain those proposed in the calibration section until
validated against the selected solver and arithmetic. No physical chemistry
constants, time/temperature windows, or cold-biome coverage thresholds are
silently chosen by this test table. Those prerequisites remain Q-4/Q-5/Q-8/Q-9.

Keep CT-01–10 fixtures small and primarily GPU-free. Reuse shared worlds for
applicable integration checks, and declare the generation cost of added seed/size
cases before attaching them to routine CI. The larger validation corpus and
offscreen evidence belong to measured calibration/integration runs; repository
worldgen-output gates still apply when output changes. A documentation-only
change to this specification does not require running engine tests.

## Delivery plan

### CARBON-1. Account for existing carbon sources and geological elapsed time

- **Outcome:** a reproducible trace explains the failing world's CO2 budget.
- **Scope:** diagnostic source/sink/clock accounting with no output change;
  pin the live-world reproduction and inspect event/lifetime/area assumptions.
- **Phase:** evidence and units contract.
- **Depends on:** `none`.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3.
- **Acceptance signals:** trace reconciles initial/final CO2 and every actual
  addition/removal; elapsed intervals and unchecked branches are identified.
- **Out of scope:** changing coefficients or forcing final temperatures.
- **Open questions:** supplies accounting evidence for Q-5 and model calibration.

### CARBON-8. Specify and package physical reference closures

- **Outcome:** versioned, implementable radiation/phase-property and chemical-constant references with declared domains and independent fixtures.
- **Scope:** TS-2/5 reference selection, reproducible table/formula provenance and licenses, error budgets, hot/mature overlap and required initial-state ranges; no coupled world simulation.
- **Phase:** physical reference contract.
- **Depends on:** CARBON-1.
- **Ordering:** critical path.
- **Relevant decisions:** D-9, D-14, D-31, D-34, D-35.
- **Acceptance signals:** consumers can evaluate specified inputs without inventing missing formulas; independent reference points and domain failures are explicit; dense-steam radiation and hot-water chemistry have justified coverage.
- **Out of scope:** new gases/cloud physics, final climate tuning, running a research atmosphere solver in the game.
- **Open questions:** Q-8 physical closure and supported phase domain under D-36. If the reference artifacts cannot remain one reviewable package, split by radiation versus aqueous chemistry before processing.

### CARBON-9. Implement the reduced carbonate chemistry solver

- **Outcome:** a pure DIC/alkalinity-to-speciation/fugacity evaluator with checked units and domain handling.
- **Scope:** TS-2 chemistry with explicit salinity input, bracketed hydrogen solve, reference comparisons and finite valid boundary states.
- **Phase:** chemistry kernel.
- **Depends on:** CARBON-8.
- **Ordering:** critical path.
- **Relevant decisions:** D-6, D-9, D-10, D-34.
- **Acceptance signals:** independent reference and limiting-case agreement; pH/activity conventions consistent; invalid-domain inputs return diagnostics.
- **Out of scope:** source/sink integration and world generation.
- **Open questions:** Q-5 numerical reference contract must be supplied by CARBON-8.

### CARBON-10. Define geological source and reaction-rate kernels

- **Outcome:** pure emission/weathering/burial functions return dimensioned rates and stoichiometric transfers.
- **Scope:** TS-3/4 material/feature tables, intrinsic magnitude and count-representation rules, activity/pulse ownership, reaction-capacity depletion rules and independent reaction ledgers; D-39 salt inputs/removals consistent with the carbon/alkalinity reactions.
- **Phase:** geological rate model.
- **Depends on:** CARBON-9.
- **Ordering:** critical path.
- **Relevant decisions:** D-13, D-17, D-18, D-25, D-26, D-27, D-33.
- **Acceptance signals:** source identity avoids duplicate emissions, larger comparable magnitude increases capacity, zero supply halts the affected reaction, and stoichiometry reconciles external rock carbon and burial.
- **Out of scope:** real terrain exposure extraction, hydrology mutation and final coefficient tuning.
- **Open questions:** Q-5 intrinsic feature dimensions and reactive-material table require code-grounded mapping; no silent fallback coefficients.

### CARBON-2. Implement and verify a carbon-balance model in isolation

- **Outcome:** a small pure model with documented units and meaningful tests.
- **Scope:** integrate two carbon inventories, explicit alkalinity, D-39 dissolved salt and finite exchange using CARBON-9/10 kernels; atmospheric concentration interface, joint donor constraints, deterministic fast/slow integration and carbon/salt budget diagnostics.
- **Phase:** mechanism.
- **Depends on:** CARBON-9, CARBON-10.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3, D-6, D-9, D-10, D-11, D-34.
- **Acceptance signals:** budget, exchange direction, transient response,
  subdivision, feedback and limiting-case tests pass without a per-world
  temperature target. Alkalinity has its own balance tests and independently
  validated chemistry reference fixtures.
- **Out of scope:** enabling the model in production world generation.
- **Open questions:** Q-2 reservoir choice and Q-5 chemistry approach are resolved
  by D-6/D-9; Q-6/Q-7 are resolved by D-10/D-11. Q-5 solver tolerances and
  bounded-work policy remain to justify; chemistry and rate kernels have separate owners.

### CARBON-7. Couple surface albedo and greenhouse forcing in a thermal model

- **Outcome:** independently tested albedo and greenhouse contributions feed one
  consistent reduced thermal calculation.
- **Scope:** supported surface reflectivities, fractional snow/ice coverage,
  incoming/reflected/outgoing energy accounting and coupling to CARBON-2's CO2;
  pure spatial/temporal fixtures in the declared mature-climate domain.
- **Phase:** thermal mechanism.
- **Depends on:** CARBON-2.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-3, D-13, D-22, D-23.
- **Acceptance signals:** directional and budget tests, independent reference
  fixtures and timestep convergence pass; no duplicated CO2 warming or imposed
  final temperature; controlled coverage transitions avoid numerical artifacts.
- **Out of scope:** production activation and D-23's deferred detailed physics.
- **Open questions:** Q-8/Q-9 thermal/spatial contracts must be settled before
  implementation; split this provisional slice if it cannot fit one PR.

### CARBON-6. Model primordial cooling and ocean formation in isolation

- **Outcome:** a tested reduced primordial model supplies a physically accounted
  transition into the liquid-ocean carbon model.
- **Scope:** D-31's global energy/water approximation, Q-8's physical laws, phase transitions, and
  conservative coupling to CARBON-2/7 across the primordial-to-mature domains;
  pure diagnostics and reference fixtures.
- **Phase:** primordial mechanism.
- **Depends on:** CARBON-2, CARBON-7.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-3, D-6, D-10, D-13, D-14, D-15, D-30, D-31, D-32, D-35, D-42.
- **Acceptance signals:** declared energy, water and carbon budgets reconcile;
  numerical refinement and independent limiting cases pass; ocean formation
  does not create carbon or impose a final temperature.
- **Out of scope:** default activation, individual magma flows or regional crust solidification.
- **Open questions:** Q-8 blocks implementation; Q-5 must cover chemistry at
  the handoff. Split this provisional slice if the chosen model is not one-PR sized.

### CARBON-11. Extract geological climate inputs and reactive-material supply

- **Outcome:** immutable, dimensioned source/material/area snapshots and measured supply increments come from actual geological events.
- **Scope:** TS-1/3/4 adapters, current source environment, deterministic area weights, intrinsic feature dimensions, reactive substrate identity and gross exposure/deposition accounting; diagnostics without activating the candidate.
- **Phase:** geological input integration.
- **Depends on:** CARBON-10.
- **Ordering:** critical path before integrated generation.
- **Relevant decisions:** D-13, D-17, D-25, D-26, D-27, D-33.
- **Acceptance signals:** representative real events match hand-accounted material/source quantities, resampling does not add carbon or capacity, and no unsupported lithology or parent identity is guessed.
- **Out of scope:** temperature solver, chemical terrain conversion and production activation.
- **Open questions:** Q-5 requires explicit gross exposure data or a reviewed budgeted approximation where the current grid lacks it.

### CARBON-5. Exercise candidate carbon and climate through real world generation

- **Outcome:** the candidate runs the actual geology/climate/ice pipeline in
  explicit diagnostic/test generation while ordinary generation keeps the old
  behavior until CARBON-3.
- **Scope:** wire CARBON-11's geological-source and material/supply adapters, continuous-time
  advancement including the primordial phase and its handoff, glacier/ice
  consumers, final climate ordering, candidate trace and
  offscreen capture entry points. Select the candidate through an explicit test
  construction policy; do not add a permanent player toggle as a shortcut.
- **Phase:** candidate integration.
- **Depends on:** CARBON-2, CARBON-7, CARBON-6, CARBON-11.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3, D-5, D-17, D-33, D-42.
- **Acceptance signals:** the actual failing-seed trace is explained by the new
  budget; candidate geography responds to candidate history; climate/ice ordering
  is coherent; early and late period snapshots demonstrate D-15's continuous
  evolution; tests prove the production default has not switched early.
- **Out of scope:** changing the default model or exposing candidate-generated
  saves as ordinary compatible saves. Diagnostic output needs distinct model
  identity; temporary candidate worlds cannot be mistaken for legacy worlds.
- **Open questions:** Q-2/Q-6/Q-7 resolved; Q-5 and Q-8 must be settled. Scope splitting must be revisited if adapters
  and consumers cannot remain one reviewable PR; do not bundle a large migration.

### CARBON-4. Calibrate generated-world outcomes and gate biome regressions

- **Outcome:** approved ordinary/extreme outcomes and durable regression gates.
- **Scope:** a declared seed/config experiment, parameter sensitivity analysis,
  climate/biome acceptance bands, offscreen comparison and documentation.
- **Phase:** calibration and visual verification.
- **Depends on:** CARBON-5.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-8, D-12, D-13, D-19, D-20, D-21, D-23, D-24, D-29, D-40, D-41, D-43.
- **Acceptance signals:** the agreed distribution and mechanistic trends hold;
  tests catch the original warming mechanism and missing cold-biome outcomes
  where those outcomes are required; the parameter specification, validation
  corpus, final compatibility plan and runtime measurements are reviewable
  before activation. D-24's visual assessment records whether the coupled result
  has convincing natural variation. Candidate output gates run after tuning.
- **Out of scope:** selecting only favorable seeds or requiring tundra on every
  extreme world unless that is an explicit product decision.
- **Open questions:** Q-4's remaining measurement details must be fixed before
  tuning; the 20% land criterion is approved by D-12. Q-3's technical preservation
  strategy must be verified before the activation handoff.

### CARBON-12. Preserve legacy climate and persist the new model identity

- **Outcome:** final candidate worlds save/reconstruct consistently, and supported legacy worlds retain their previous climate/terrain consumers.
- **Scope:** TS-9 persisted model identity, required final outputs/parameters, per-component DTO migrations, legacy recipe dispatch and inventory/cache identity updates.
- **Phase:** compatibility integration.
- **Depends on:** CARBON-4.
- **Ordering:** critical path before activation.
- **Relevant decisions:** D-7, D-11, D-32, D-34, D-35.
- **Acceptance signals:** actual legacy load/chunk/zoom reconstruction preserves output; new-model save/reload preserves final climate and conversion semantics; no geology rerun or fabricated historical carbon inventory.
- **Out of scope:** changing climate coefficients, repairing old worlds or activating the candidate as default.
- **Open questions:** Q-3 requires the exact persisted/reconstruction field inventory and frozen DTO boundary before editing schemas.

### CARBON-3. Activate the calibrated model with verified save compatibility

- **Outcome:** ordinary newly generated worlds use the approved model, and
  existing saves follow the explicitly selected compatibility policy.
- **Scope:** default activation, versioned model/parameter identity as needed,
  final production wiring using CARBON-12's save treatment, updated documentation, and all required
  worldgen baselines/gates. Most model behavior is already implemented and tested
  by CARBON-2/5; this slice cannot become their deferred implementation dump.
- **Phase:** activation.
- **Depends on:** CARBON-4, CARBON-12.
- **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5.
- **Acceptance signals:** production matches the approved candidate; budget and
  biome gates run through the real default path; fresh-world and existing-save
  behavior both match their contracts; full worldgen tier, generated baselines,
  world checks and relevant compatibility gates pass.
- **Out of scope:** orbital forcing, new carbon physics, further tuning without
  repeating calibration gates, or unrequested old-world repair.
- **Open questions:** Q-3's technical preservation strategy must be resolved;
  D-7 already settles the owner-facing policy. If the selected compatibility strategy
  needs substantial new machinery, add a separate stable slice before activation
  during design refinement, rather than leaving that work implicit here.

## Scientific reference notes

- [Bower et al. (2022)](https://arxiv.org/abs/2110.08029)
  models magma-ocean/atmosphere volatile exchange. Water retention, carbon gas
  speciation and the evolving atmosphere depend on interior chemistry and
  solidification. This motivates an explicit primordial approximation; it does
  not establish that a steam/CO2-only atmosphere is universally accurate.
- [Krissansen-Totton and Catling (2017)](https://www.nature.com/articles/ncomms15423)
  develops a geological carbon-cycle model with continental and seafloor
  weathering. It supports the feedback-based direction while documenting
  uncertainty in weathering sensitivity and ocean-chemistry simplifications.
- [CLIMBER-X carbon cycle (2023)](https://gmd.copernicus.org/articles/16/3501/2023/index.html)
  makes carbon-stock evolution explicit through source and sink fluxes. It is
  a structural reference, not a proposed dependency or complexity target.

The design above is a proposed game-scale approximation informed by these
sources; its numerical coefficients and world-behavior policy are not implied by
the papers and remain to be measured and selected.
