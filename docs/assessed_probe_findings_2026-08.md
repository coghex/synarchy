# Assessed probe findings — harness validity and gameplay continuity

This report records seventeen current concerns from an approved coordinated-test assessment. Fifteen concern probe validity or infrastructure; two establish product behavior requiring further investigation.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source was approved assessment
`20260826T133352Z-ui-language-etymology-construction-porta-298686`, which
correlated 23 observations from coordinated manual-probe runs at assessed
commit `8070b82c0e4ea3a51ea00d36fbd2e2709d338238`.

The assessment read the complete source reports and retained primary logs,
inspected available engine logs, screenshots, and disposable instrumentation,
traced relevant behavior from each tested revision to the assessed commit, and
reviewed tracker coverage. For this report, the owning implementation, probe
code, focused data, and existing findings-report corpus were rechecked at
`9bd44d0febd1f65f6506622e35469ab4b2f1f111`. Among the implicated files, only
`tools/run_probes.py` changed after the assessment; that change added an
unrelated probe registration and did not alter the persistence sweep's
900-second timeout or buffered child-output behavior.

Seventeen actionable current findings are retained. Three observations whose
probe defects were already fixed are omitted. The broad craft-bill observation
is supporting evidence for the focused item-identity finding rather than an
independent concern. The expedition water-memory observation remains
inconclusive and is omitted until the approved assessment's focused follow-up
test captures page, facing, light, visible tiles, fluid identity, action, and
memory together.

No scenario was rerun while drafting this report. Missing or overwritten
transient artifacts were not treated as current evidence. Tracker
deduplication and issue disposition remain intentionally deferred to
`process-report`. No implementation, test, tracker item, or remote state was
changed.

## Status

- [ ] AP-1. Thermo comparison drops the live world's plate configuration
- [ ] AP-2. Power brownout timeout loses the state needed to classify it
- [ ] AP-3. Thought probe accepts any environmental thought as evidence of cold
- [ ] AP-4. Farm capstone requires harvested yield to remain transiently grounded
- [ ] AP-5. State-of-mind guard never samples a value that distinguishes the gates
- [ ] AP-6. Plant probe does not assert a nonzero preferred-soil baseline
- [ ] AP-7. Multi-boot probes overwrite earlier engine logs
- [ ] AP-8. Offscreen type-icon check can be satisfied by the discovery popup
- [ ] AP-9. Foraging's default fixture can contain no harvestable target
- [ ] AP-10. Pickup-priority fixture fails its carrying-capacity precondition
- [ ] AP-11. Repair-priority oracle samples the competing item too late
- [ ] AP-12. Persistence sweep can exhaust 900 seconds without phase diagnostics
- [ ] AP-13. An accepted expedition return order is silently abandoned
- [ ] AP-14. Construction stake phase stops before portal roster delivery
- [ ] AP-15. Location embark continues after exact-unit selection fails
- [ ] AP-16. Tutorial pre-latched branch lacks deterministic sight controls
- [ ] AP-17. Crafting removes an unrelated same-definition inventory item

---

## Behavioral oracle validity

### AP-1. Thermo comparison drops the live world's plate configuration

The thermo-altitude probe's ice-agreement phase compares positions generated
with different tectonic-plate counts. Its live console world uses five plates,
while the separate dump process receives seed and world size but no
`--plates`, causing the dump to use the size-derived default of nine. A passing
temperature comparison therefore relates ice positions from one world to
ambient values from another.

**Evidence:**

- `tools/thermo_altitude_probe.py:125-141` — constructs the dump command with
  `--seed`, `--worldSize`, and `--region`, but no plate count.
- `tools/thermo_altitude_probe.py:235` — initializes the live world with five
  plates.
- `.git/codex-test/reports/20260822T032634Z-probe-thermo-altitude-946174.test-result.md:50-58`
  — records five plates in the console log, nine in the dump log, and a passing
  eight-tile comparison.

**Handoff context:**

- **Current behavior:** The phase can pass while comparing two distinct
  generated worlds.
- **Expected direction:** Both launches should use the same complete
  world-generation parameters and report those parameters with the result.
- **Scope and constraints:** Preserve the separate dump launch, sampled-region
  design, reporter-selected logs, and arena-safety checks.
- **Remaining uncertainty:** The observation does not indicate any defect in
  ice generation or ambient-temperature calculation.

### AP-2. Power brownout timeout loses the state needed to classify it

The powered-workshop probe polls only the bill's `working` flag while waiting
for the AI to reach the browned-out station. When that poll expires, the
artifact does not preserve the unit's active action, craft job, position, bill
claim transition, or synchronized network state. The resulting timeout cannot
distinguish power policy, job planning, pathing, or sampling failures.

**Evidence:**

- `tools/power_workshop_probe.py:591-607` — separately polls the claimant and
  `bill.working`, then checks drain without recording the worker's action,
  craft-job state, or position.
- `tools/power_workshop_probe.py:609-622` — later checks zero progress and
  powered completion, but does not explain why the earlier working phase was
  not observed.
- `.git/codex-test/reports/20260822T032824Z-probe-power-workshop-6b0391.test-result.md:50-58`
  — records that the bill was claimed, `working` was not observed, drain stayed
  at zero, and the same bill completed after noon power became available.

**Handoff context:**

- **Current behavior:** A timeout reports the bill flag and network drain but
  not the AI state that should have produced them.
- **Expected direction:** Timeout evidence should atomically or monotonically
  capture bill state, supplied power, active action/job, and unit position.
- **Scope and constraints:** Preserve the real AI, midnight/noon transition,
  claimed-versus-working distinction, active demand, and battery checks.
- **Remaining uncertainty:** The retained run does not determine whether any
  product subsystem caused the missed working state.

### AP-3. Thought probe accepts any environmental thought as evidence of cold

The arctic-ambient phase stops at the first thought whose display category is
`environmental`. That category also contains daylight, heat, and night
thoughts, whose independent triggers do not establish that the cold predicate
was eligible or selected. The observed run passed on the daylight thought.

**Evidence:**

- `tools/thought_probe.py:103-137` — `roll_until` accepts the
  `environmental|` prefix after monkey-patching ambient temperature.
- `data/thoughts.yaml:98-105` — defines `cold_bite` as an environmental thought
  with the `cold` trigger.
- `data/thoughts.yaml:120-126` — defines the independently triggered
  environmental `day_easier` thought that satisfied the recorded run.
- `.git/codex-test/reports/20260822T033237Z-probe-thought-d6fc6f.test-result.md:50-58`
  — records the daylight text as the passing event.

**Handoff context:**

- **Current behavior:** The phase can pass without ever emitting the
  cold-triggered thought.
- **Expected direction:** The oracle should assert the exact intended thought
  identity and include the complete sampled thought set on failure.
- **Scope and constraints:** Retain weighted thought selection and the rule that
  eligible environmental thoughts continue competing with random entries.
- **Remaining uncertainty:** None material about the false-positive path; the
  run does not judge whether cold thoughts work in production.

### AP-4. Farm capstone requires harvested yield to remain transiently grounded

The farm-AI capstone advances the world at 50,000x while AI remains active,
polling crop ripeness once per second. Afterward it requires both a cleared plot
and wheat grain still present on the ground. A worker can pass through
ripeness, harvest the crop, and move its yield between those observations, so
successful autonomous handling can fail the asserted ground-state snapshot.

**Evidence:**

- `tools/farm_ai_probe.py:552-578` — advances time at 50,000x and samples the
  harvestable state once per second.
- `tools/farm_ai_probe.py:580-612` — leaves AI active, waits for the plot to
  clear, and then requires `wheat_grain` in `item.listGround()`.
- `.git/codex-test/reports/20260822T033732Z-probe-farm-ai-bdbe14.test-result.md:50-58`
  — records no sampled ripe state, a cleared plot, no grounded grain, and
  increased farming XP.

**Handoff context:**

- **Current behavior:** The capstone conflates successful harvest with one
  transient location for its output and can miss the ripe state entirely.
- **Expected direction:** It should follow durable yield identity across ground
  and inventory while recording crop-state and worker-action transitions.
- **Scope and constraints:** Preserve real game-clock growth, autonomous target
  discovery, wild-forage isolation, crop clearing, and farming-XP coverage.
- **Remaining uncertainty:** The retained artifact does not say where the
  missing grain went; that question is separate from the oracle defect.

### AP-5. State-of-mind guard never samples a value that distinguishes the gates

The regression guard is intended to prove that psychological
`state_of_mind` does not replace consciousness in collapse, delirium,
confusion, and alert-state decisions. It only requires state of mind to be
below consciousness. The recorded psychological value was approximately
0.933, above every relevant gate, so substituting it for consciousness would
still produce the same standing and alert result.

**Evidence:**

- `tools/state_of_mind_probe.py:176-195` — accepts any
  `stateOfMind < consciousness` while requiring all physiological gates to
  remain off.
- `scripts/brain.lua:58-61` — defines confusion, delirium, and unconscious
  thresholds at 0.70, 0.40, and 0.15.
- `scripts/brain.lua:263-283` — production correctly bases those predicates on
  consciousness, which is the dependency the probe claims to protect.
- `.git/codex-test/reports/20260822T033817Z-probe-state-of-mind-8c66a4.test-result.md:50-58`
  — records the passing state-of-mind value as 0.933.

**Handoff context:**

- **Current behavior:** The guard passes at a value where the intended and
  mistaken gate inputs agree.
- **Expected direction:** Exercise at least one psychological value below each
  discriminating threshold while consciousness remains high.
- **Scope and constraints:** Preserve the current production rule that
  physiological state depends only on consciousness; this is probe coverage,
  not a request to retune thresholds.
- **Remaining uncertainty:** None material about the non-discriminating fixture.

### AP-6. Plant probe does not assert a nonzero preferred-soil baseline

The plant probe claims to prove that preferred soil permits a nonzero overall
suitability score and non-preferred soil reduces that same crop to zero. Its
preferred-loam phase verifies soil fit and generic score bounds, but does not
require any selected crop's overall score to be positive. It only makes the
granite zero score mandatory.

**Evidence:**

- `tools/plant_probe.py:202-233` — forces loam, checks wheat's soil fit, and
  accepts every overall score in the full zero-to-one range.
- `tools/plant_probe.py:235-246` — requires both crop scores to be zero on
  granite.
- `.git/codex-test/reports/20260822T045108Z-probe-plant-3dae84.test-result.md:70-98`
  — records wheat at zero even on loam and tomato transitioning from a positive
  loam score to zero on granite.

**Handoff context:**

- **Current behavior:** The probe could stay green if every preferred-soil
  overall score regressed to zero.
- **Expected direction:** Select a crop/tile whose non-soil factors are
  nonzero, assert positive overall score plus soil fit 1.0, then assert the
  same crop reaches overall score and soil fit zero on granite.
- **Scope and constraints:** Preserve the six-factor inventory, ordering,
  designation, cancellation, replacement, and save/load coverage.
- **Remaining uncertainty:** The recorded product result was correct; the
  concern is the missing positive-side assertion.

---

## Execution artifacts and isolation

### AP-7. Multi-boot probes overwrite earlier engine logs

The preview and offscreen probes launch multiple fresh engine processes but
reuse log paths. The shared launcher opens every selected path in truncating
mode. Preview therefore retains only its final target boot, while offscreen's
restart on the original port overwrites the much longer worldgen/gameplay
session.

**Evidence:**

- `tools/preview_probe.py:117` — defines one fixed engine-log path.
- `tools/preview_probe.py:248,437,693,1380` — representative list, item, unit,
  and dispatch-sweep boots all reuse that path; several additional call sites
  do the same.
- `tools/offscreen_probe.py:740-743` and
  `tools/offscreen_probe.py:835-845` — start the initial and restart engines on
  the same port without unique log paths.
- `tools/probelib.py:197-224` — derives the default path solely from the port
  and opens it with mode `"w"`.
- `.git/codex-test/reports/20260822T050310Z-probe-preview-b42d4c.test-result.md:50-58`
  — records that the retained preview artifact contained only the final boot.

**Handoff context:**

- **Current behavior:** Earlier warnings, crashes, and lifecycle output
  disappear whenever a later process reuses the path.
- **Expected direction:** Retain one uniquely named log per boot, with each path
  identified in the final probe artifact.
- **Scope and constraints:** Preserve per-port defaults for simple one-boot
  probes and retain preview/offscreen process isolation.
- **Remaining uncertainty:** Existing passing assertions remain valid; the lost
  data affects retrospective inspection and failure diagnosis.

### AP-8. Offscreen type-icon check can be satisfied by the discovery popup

The offscreen location-icon phase compares full screenshots before and after a
ruin becomes discovered. Discovery opens a popup over the same central region
as the icon, but the probe neither dismisses that popup nor restricts its diff
to the marker. The popup therefore supplies more than enough changed pixels
while obscuring the type icon the assertion names.

**Evidence:**

- `tools/offscreen_probe.py:94-104` — accepts a frame when at least 0.1% of all
  pixels differ.
- `tools/offscreen_probe.py:481-508` — spawns the discovering unit and compares
  the complete pre-discovery and post-discovery frames without dismissing
  notifications or cropping to the marker.
- `.git/codex-test/reports/20260822T051226Z-probe-offscreen-7f7b98.test-result.md:50-59`
  — records that the popup covered the icon and changed 77,418 pixels, or 8.4%
  of the frame.

**Handoff context:**

- **Current behavior:** The visual check can pass without showing that the
  unknown marker became the ruin type icon.
- **Expected direction:** Remove the popup before capture and compare a stable
  icon-sized region or a semantic expected-icon signature.
- **Scope and constraints:** Preserve the state assertions, unseen control,
  loaded-terrain baseline, alternate zoom, rotation, and persistence checks.
- **Remaining uncertainty:** The retained state evidence proves discovery, but
  the run does not prove or disprove the rendered type icon itself.

---

## Fixtures and asynchronous state

### AP-9. Foraging's default fixture can contain no harvestable target

The foraging probe depends on natural raspberry or clover generated by its
default seed and region. It scans for a target and exits when none exists, so
the API, regrowth, persistence, and autonomous-foraging assertions can all be
skipped by a deterministic fixture that lacks the required subject.

**Evidence:**

- `tools/foraging_probe.py:162-170` — samples the loaded region and returns
  `None` when no harvestable flora is found.
- `tools/foraging_probe.py:173-179` — defaults to seed 42, size 64, and three
  plates.
- `tools/foraging_probe.py:206-220` — initializes that world and aborts the
  behavioral path when the target search fails.
- `tools/foraging_probe.py:274-317` — later AI coverage depends on a second
  successful target search.

**Handoff context:**

- **Current behavior:** The registered default invocation can terminate before
  exercising any target-dependent foraging behavior.
- **Expected direction:** Use or construct a deterministic target fixture and
  make its presence an explicit preflight contract.
- **Scope and constraints:** Preserve real harvesting, regrowth timing,
  save/restart/load, ground identity, and hungry-unit AI coverage.
- **Remaining uncertainty:** Target absence does not indicate a defect in
  foraging queries or AI.

### AP-10. Pickup-priority fixture fails its carrying-capacity precondition

The follow-command-priority probe stages a 10 kg granite chunk, invokes
`commandPickup`, ignores the returned boolean, and then waits for
`pickup_ground` to outrank the pending move. Production refuses an
over-capacity pickup before it stores any order, so the intended priority
contest may never exist.

**Evidence:**

- `tools/follow_command_priority_probe.py:137-162` — stages
  `granite_chunk`, discards the `commandPickup` result, and polls only the
  action name.
- `data/items/granite_chunk.yaml:1-9` — gives the staged item a 10 kg weight.
- `scripts/unit_ai_pickup.lua:241-264` — returns `false` before assigning
  `pickupOrder` whenever the capacity check fails.
- `.git/codex-test/reports/20260825T164538Z-probe-follow-command-priority-ab4e07.test-result.md`
  — records an already-over-capacity starting load and an action timeline
  containing only `follow_command`.

**Handoff context:**

- **Current behavior:** A rejected pickup is graded as though an accepted
  pickup lost AI arbitration.
- **Expected direction:** Stage an item below verified remaining capacity and
  require `commandPickup` to return true before polling priority.
- **Scope and constraints:** Preserve the separate move, refill, routine-goal,
  and combat priority stages.
- **Remaining uncertainty:** The run does not test whether an accepted pickup
  correctly outranks a move.

### AP-11. Repair-priority oracle samples the competing item too late

The repair-priority phase waits until the prioritized gambeson reaches full
condition and only then performs a separate console request for the broken
axe. The live AI remains active between those reads and may legitimately start
or complete the axe repair after finishing the gambeson. The resulting failure
cannot distinguish correct ordering followed by fast subsequent work from
incorrect ordering.

**Evidence:**

- `tools/repair_ai_probe.py:560-588` — flags the gambeson and waits until it is
  fully repaired.
- `tools/repair_ai_probe.py:590-600` — reads the axe only afterward, then waits
  for its eventual completion.
- `.git/codex-test/reports/20260825T165932Z-probe-repair-ai-c29e3c.test-result.md`
  — records the gambeson completing, its flag clearing, the intermediate axe
  assertion failing, and the axe later completing.

**Handoff context:**

- **Current behavior:** Two sequential snapshots can span the transition into
  the next valid repair job.
- **Expected direction:** Record the claimed repair-job instance before
  completion, or capture the job and both item states in one transition
  timeline.
- **Scope and constraints:** Preserve real asynchronous AI, priority
  self-clearing, normal severity ordering afterward, and all other repair
  phases.
- **Remaining uncertainty:** The retained run cannot establish which item was
  repaired first.

### AP-12. Persistence sweep can exhaust 900 seconds without phase diagnostics

The broad persistence sweep is registered under the aggregate runner's
900-second default. The runner captures its stdout in a pipe and waits for
process completion before returning output. When the child is terminated at
the timeout, the retained artifact can contain only the aggregate timeout
summary, with no indication of which engine cycle, comparison, or nested probe
was advancing or stalled.

**Evidence:**

- `tools/run_probes.py:847-850` — sets the default per-probe timeout to 900
  seconds.
- `tools/run_probes.py:1214-1249` — captures child output through
  `stdout=PIPE` and waits through `communicate(timeout=timeout)`.
- `tools/persistence_contract_sweep.py:563-593` — begins a four-engine,
  three-cycle generated-world persistence scenario.
- `tools/persistence_contract_sweep.py:687-713` — performs structural
  comparisons and then launches cross-referenced probes.
- `.git/codex-test/reports/20260825T170942Z-probe-persistence-contract-sweep-0076ea.test-result.md`
  — records a 900-second timeout with no surviving child phase lines.

**Handoff context:**

- **Current behavior:** The default registered invocation can consume fifteen
  minutes and still provide no persistence verdict or stalled-phase identity.
- **Expected direction:** Emit flushed phase progress and apply bounded
  phase-level timing, or split the sweep into independently attributable
  stages.
- **Scope and constraints:** Preserve isolated roots, real world-size-64
  generation, three fresh-process cycles, structural comparison, and the
  maintained cross-probe set.
- **Remaining uncertainty:** The run cannot distinguish an undersized total
  budget from one unusually slow or stalled phase.

---

## Integrated gameplay scenarios

### AP-13. An accepted expedition return order is silently abandoned

A carrier successfully traversed the outbound route, picked up the exact
retrieval item, accepted a return command, and retained that command while
eating preempted it. The carrier then wandered near the ruin instead of
resuming the return. After save and reload, the item remained on the same
carrier but the return intent was gone, with no explicit cancellation or
surfaced failure.

**Evidence:**

- `tools/expedition_retrieval_probe.py:782-840` — issues the return order,
  proves it starts, triggers a real eating interruption, and verifies the
  command remains pending during that interruption.
- `tools/expedition_retrieval_probe.py:849-889` — requires the carrier to resume
  `follow_command`, close on home, and retain the recovered instance.
- `tools/expedition_retrieval_probe.py:1056-1073` — saves while the return
  journey is unfinished and the item remains carried.
- `.git/codex-test/logs/20260825T173049Z-probe-expedition-retrieval-e754cf.log:34-65`
  — records accepted return, pending intent during eating, subsequent
  wandering, absent intent after load, and failure to reach storage.
- `.git/codex-test/logs/20260825T173049Z-probe-expedition-retrieval-e754cf.log:24-32`
  — establishes that the same carrier completed the comparable outbound route
  and acquired the exact item.

**Handoff context:**

- **Current behavior:** A valid player return command can disappear without
  completing and without reporting why.
- **Expected direction:** Accepted return intent should remain durable through
  valid interruptions and save/load until completion, explicit replacement, or
  an observable failure.
- **Scope and constraints:** Preserve survival-needs preemption, ordinary
  multi-tick movement, exact item/carrier identity, command supersession, and
  persistence boundaries.
- **Remaining uncertainty:** The evidence does not yet isolate pathfinding,
  terrain stamping, task cancellation, persistence, or another transition as
  the implementation cause.

### AP-14. Construction stake phase stops before portal roster delivery

The focused construction stake phase proves that an `acolyte_portal`
designation becomes an active building and then immediately destroys the
builder. It does not wait for the portal's spawn sequencer or assert that any
positive unit-spawn result produces a roster member. It therefore cannot
validate the portal-delivery integration behavior its selected scenario was
intended to cover.

**Evidence:**

- `tools/construction_probe.py:466-487` — asserts designation and active
  building creation, then destroys the builder and returns.
- `tools/construction_probe.py:850-863` — registers `stake` as a standalone
  selectable phase.
- `.git/codex-test/reports/20260825T180652Z-probe-construction-stake-185c4e.test-result.md`
  — records exactly two passing construction checks and no roster-delivery
  event before shutdown.

**Handoff context:**

- **Current behavior:** The phase can pass after portal construction without
  observing any portal-spawn result.
- **Expected direction:** A roster-integration run should wait for and identify
  at least one expected portal-delivered unit.
- **Scope and constraints:** Preserve the inexpensive standalone construction
  phase; roster delivery may instead belong in a separate focused phase or the
  existing expedition integration apparatus.
- **Remaining uncertainty:** The run neither confirms nor contradicts current
  portal roster behavior.

### AP-15. Location embark continues after exact-unit selection fails

The location-embark scenario checks whether a real click selected one chosen
portal-roster unit but treats a false result as an ordinary failed assertion
and continues. Right-click movement then targets whatever units are actually
selected, while later visibility polls remain pinned to the originally chosen
UID. Discovery attribution separately accepts any unit from the roster,
allowing one selection miss to mix several unit identities across the phase.

**Evidence:**

- `tools/location_embark_probe.py:568-593` — returns false when five click
  attempts do not select the requested UID.
- `tools/location_embark_probe.py:624-642` — right-clicks the current selection
  rather than accepting an explicit UID.
- `tools/location_embark_probe.py:895-939` — chooses one roster UID, records
  exact-selection failure as a check, and issues the move anyway.
- `tools/location_embark_probe.py:948-965` — permits discovery attribution to
  any portal-roster unit.
- `tools/location_embark_probe.py:1025-1047` — continues watching the original
  UID for the out-and-back movement.
- `.git/codex-test/reports/20260825T181319Z-probe-location-embark-d48529.test-result.md`
  — records the failed exact click, successful discovery by some roster unit,
  and failure of the original UID to return within sight.

**Handoff context:**

- **Current behavior:** Movement and visibility assertions can describe
  different units after selection fails.
- **Expected direction:** Exact selection should be a prerequisite, or the
  harness should explicitly adopt and consistently track a verified selected
  roster UID.
- **Scope and constraints:** Preserve real mouse input, portal roster
  concurrency, discovery attribution, zoom-map icons, and save/reload coverage.
- **Remaining uncertainty:** The original click miss may be overlap, movement,
  hit testing, or another input condition; this artifact cannot classify it.

### AP-16. Tutorial pre-latched branch lacks deterministic sight controls

The tutorial's main water-discovery phase pins daylight, controls unit
positions, and checks whether the recipient can see water. Its third-boot
pre-latched branch only queues `unit.setPos`, unpauses, and polls
`knownWaterSources`. It does not verify the applied page and position, facing,
visible tiles, daylight, or active AI action even though production visibility
depends on all of those conditions.

**Evidence:**

- `tools/tutorial_probe.py:459-469` — documents and implements daylight pinning
  for deterministic sight.
- `tools/tutorial_probe.py:602-629` — the main discovery path pins daylight,
  controls positions, checks visibility, and then polls discovery.
- `tools/tutorial_probe.py:802-811` — the pre-latched path only sets position,
  unpauses, and waits for nonempty water memory.
- `tools/tutorial_probe.py:937-969` — constructs the third boot and reaches the
  uncontrolled reveal leg.
- `src/Unit/LineOfSight.hs:90-129` — computes visibility from page-local time,
  position, facing, perception, radius, and occlusion.
- `src/Unit/LineOfSight.hs:303-313` — restricts ordinary visibility to a
  120-degree facing cone.
- `.git/codex-test/reports/20260825T184754Z-probe-tutorial-bd179d.test-result.md`
  — records an empty water memory after 60 seconds without the missing
  discriminator state.

**Handoff context:**

- **Current behavior:** Failure to discover water cannot be separated from an
  unapplied position, wrong page, darkness, facing, occlusion, or AI movement.
- **Expected direction:** Establish a deterministic visible-water setup and
  preserve all sight and action inputs with the discovery result.
- **Scope and constraints:** Preserve the real tutorial evaluator, third-boot
  ordering, pre-latched composite, and subsequent reversal assertions.
- **Remaining uncertainty:** The run does not establish a tutorial or
  water-discovery product defect.

---

## Item identity

### AP-17. Crafting removes an unrelated same-definition inventory item

A focused identity trace followed a pre-existing granite instance through two
real AI craft cycles whose recipe consumes steel and produces granite. The old
instance disappeared after the first cycle. `craft.executeAt` returned only
fresh output IDs, the AI called `dropItemById` only for those fresh IDs, and all
four fresh outputs remained on the ground. The unrelated old instance therefore
vanished outside the observed exact-output drop path.

**Evidence:**

- `tools/craft_bill_probe.py:93-102` — defines the probe recipe as consuming
  `steel_bar` and producing two `granite_chunk` instances.
- `tools/craft_bill_probe.py:429-474` — stages one carried same-definition item
  and requires it to remain while fresh outputs are grounded.
- `scripts/unit_ai_craft.lua:367-382` — calls `craft.executeAt` and drops only
  the returned fresh instance IDs.
- `src/Craft/Execute.hs:20-33` and `src/Craft/Execute.hs:59-70` — consume only
  inventory instances matching recipe demand names.
- `src/Engine/Scripting/Lua/API/Craft/Execute.hs:206-230` — appends fresh
  outputs to the post-consumption inventory and returns their IDs.
- `.git/codex-test/logs/20260825T200450Z-probe-craft-output-identity-followup-ea285a.log:1-18`
  — records old ID 22, fresh IDs 24/25 and 27/28, exact successful drops of
  only the fresh IDs, an empty resulting inventory, and four grounded outputs.

**Handoff context:**

- **Current behavior:** A completed craft can remove an unrelated inventory
  instance sharing the output definition even though the exact-output drop
  calls do not target it.
- **Expected direction:** Every inventory mutation during crafting should
  preserve unrelated instances, including items whose definition matches an
  output.
- **Scope and constraints:** Preserve atomic ingredient consumption, fresh
  instance minting, exact-ID output placement, quality calculation, bill
  completion, and legitimate capacity handling.
- **Remaining uncertainty:** The trace rules out the observed output-drop calls
  but does not yet identify whether ingredient consumption, inventory
  replacement, capacity handling, fetching, or another mutation removes the
  old instance.
