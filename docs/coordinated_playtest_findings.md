# Coordinated playtest findings

This report records seven independent concerns confirmed while assessing
coordinated local playtests: commanded-move continuity, remote-settlement
modal layout, and five weaknesses in the bleeding-trail, combat-animation,
and action-outcome probes.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The sources were three approved local assessments:

- `20260813T011157Z-ui-transfer-combat-blood-gameplay-expedi-5fa13c`,
  covering five observations from four playtest runs against `origin/master`
  commit `293717e60370ab1452dd674870602733b5037178`;
- `20260813T020840Z-combat-animation-bddd95`, covering two combat-animation
  observations against commit
  `4c2a26d2e707e05355ca637a5b3717b2d7ddc0f4`; and
- `20260813T021424Z-ux-action-feedback-52da40`, covering two action-outcome
  observations against the same `4c2a26d2e707e05355ca637a5b3717b2d7ddc0f4`
  revision.

Together they correlated nine observations from six playtest runs. This report
retains the seven confirmed independent concerns. The fixed transfer-probe
localization observation and inconclusive recovered-radio consumer observation
remain excluded because they are not confirmed current concerns.

The drafting passes read the owning implementation, focused probes, shared
harness helpers, probe classifications, supporting comments, persistence and
arena paths, animation and pose mappings, captured logs, and relevant history.
The current implementation was rechecked at
`4c2a26d2e707e05355ca637a5b3717b2d7ddc0f4`.

Existing findings reports and `docs/bugs.md` were searched for equivalent
entries. The two action-outcome concerns overlap the combined, still-unprocessed
PRR-2 entry in `docs/project_review_715-694.md`; they remain separate here
because the approved assessment established two independently repairable root
causes. A later `process-report` invocation should coordinate their disposition
with PRR-2. No other overlap was found. This drafting pass did not perform a new
GitHub duplicate search or choose tracker dispositions.

No scenario was rerun while drafting. The evidence includes the supplied
remote-modal screenshot, expedition and combat-animation timelines, bleeding
probe engine warnings, action-outcome results, and current-code tracing. No
implementation file, test, tracker item, or remote state was changed.

## Status

- [x] PT-1. Survival interruptions consume a commanded move’s stall budget — [#1291]
- [x] PT-2. The remote-settlement title renders across the modal’s top border — [#1394]
- [x] PT-3. Final bleeding-pool cases spawn outside the loaded arena — [#1395]
- [x] PT-4. Combat-animation setup does not reliably establish combat — [#1396]
- [x] PT-5. The combat-animation probe does not gate its death-animation contract — [#1397]
- [x] PT-6. The default chop fixture cannot reach the promised partial-result oracle — [#1398]
- [x] PT-7. The portal outcome check still asserts the pre-confirmation contract — [#1399]

---

## Gameplay command continuity

### [#1291] PT-1. Survival interruptions consume a commanded move’s stall budget

A commanded return remained pending when an intentional eating interruption
began, but disappeared before the unit could resume walking. The unit then
selected ambient wander, moved away from home, and reached the save boundary
without a return intent to persist.

The stall clock measures elapsed time since the last closest-approach progress.
Eating short-circuits the AI tick before task maintenance runs, so all time
spent eating is later charged as movement stall time. If the interruption lasts
longer than the 60-second budget, the first post-eating maintenance pass deletes
the command even though the unit was not free to make progress during that
interval.

**Evidence:**

- `scripts/unit_ai.lua:243-251` — `tickOne` returns immediately while the unit’s
  activity is `eating` or `drinking`, before calling `core.maintainTask`.
- `scripts/unit_ai_core.lua:65-70` — commanded movement has a 60-second stall
  budget and a 0.5-tile closest-approach progress threshold.
- `scripts/unit_ai_core.lua:267-275` — the timeout subtracts the task’s old
  `progressAt` from current game time and deletes the task when that elapsed
  duration exceeds the budget.
- `scripts/unit_ai_core.lua:281-291` — `commandMove` is explicitly intended to
  survive higher-priority interruptions and stores no interruption-aware clock
  state.
- `scripts/unit_ai_combat.lua:35-46` — the utility contract says eating and
  other urgent actions preempt a commanded move temporarily, after which the
  move resumes.
- `tools/expedition_retrieval_probe.py:39-48` — the probe defines survival
  interruption, subsequent resumption, and save/load of the pending return
  intent as one end-to-end contract.
- `tools/expedition_retrieval_probe.py:813-855` — the focused check observes
  eating, verifies the recovered item remains held and the command is initially
  pending, then waits for `follow_command` to resume and close on home.
- `tools/expedition_retrieval_probe.py:1041-1104` — the following save/load
  stage expects the unfinished return intent to remain pending across a fresh
  process.
- `scripts/unit_ai_save.lua:252-258` — `commandedTask` is persisted with the
  rest of AI state when it still exists; persistence cannot restore a task
  already cleared before capture.

**Handoff context:**

- **Current behavior:** Time spent in an intentional higher-priority activity
  counts as failure to progress. A sufficiently long meal can silently turn a
  valid player move request into wander before the unit gets another chance to
  follow it.
- **Expected direction:** A deliberate survival or emergency interruption
  should suspend or otherwise exclude its duration from movement-stall
  accounting. Once the unit is again eligible to move, a genuinely unreachable
  or non-progressing command must still expire normally.
- **Scope and constraints:** Preserve arrival clearing, closest-approach
  progress resets, unreachable-target timeout, the existing utility priority
  ladder, adaptive commanded-move pacing, and save compatibility. The fix
  should cover all activities that intentionally suspend a pending move without
  converting ordinary pathing failure into an immortal order.
- **Test direction:** Tighten the expedition retrieval probe or add focused
  coverage with an interruption lasting beyond the normal stall budget. Assert
  that the task remains pending throughout the interruption, resumes closing
  on its target afterward, survives a mid-return save/load, and still times out
  when the unit is free to move but makes no closest-approach progress.
- **Remaining uncertainty:** The correct interruption taxonomy and whether the
  clock should be explicitly suspended or have its progress timestamp shifted
  remain implementation decisions.

---

## Modal layout and visual coverage

### [#1394] PT-2. The remote-settlement title renders across the modal’s top border

The supplied offscreen screenshot shows most of “Establish Colony Remotely?”
behind or above the modal’s top border while the message and both actions remain
readable.

The modal places the title’s text element at the content area’s top edge.
However, text positions are baselines and the visible glyph mass extends upward
from that coordinate. The title is therefore centered horizontally but placed
vertically as though its coordinate described the top of its visible bounds.

**Evidence:**

- `scripts/build_tool_remote_warning.lua:200-215` — the warning panel defines a
  padded content area inside the decorated box.
- `scripts/build_tool_remote_warning.lua:218-237` — the title is placed with
  `y = 0` and `origin = "top-center"`, putting its text baseline at the content
  area’s top edge.
- `scripts/ui/panel.lua:194-221` — `panel.place` resolves `y = 0` from
  `contentY` and attaches the element at that result after applying its origin
  offset.
- `scripts/ui/label.lua:57-63` — label text coordinates are explicitly
  documented as baselines whose visible glyph mass sits above them.
- `scripts/ui/label.lua:240-267` — widget introspection reconstructs a label’s
  visual bounds by subtracting its font size from the reported baseline.
- `tools/location_embark_probe.py:586-590` — the offscreen expedition flow
  confirms the modal opens and saves `remote_modal.png`, but only asserts that
  the screenshot operation succeeded; it does not validate the title’s visual
  bounds against the panel.

**Handoff context:**

- **Current behavior:** The remote-warning modal’s title intersects or escapes
  the decorated top border despite the panel having sufficient vertical
  padding.
- **Expected direction:** The title’s visible glyph bounds should remain inside
  the panel’s content area at every supported framebuffer and UI-scale
  combination, while retaining horizontal centering and readable fitted text.
- **Scope and constraints:** Preserve the responsive width fitting, framebuffer
  caps, message and button layout, exclusive modal-input behavior, cancellation
  and confirmation paths, and the supported responsive envelope. Avoid
  treating the text baseline as its visual top edge.
- **Test direction:** Extend the existing offscreen location-embark coverage to
  compare the title’s `ui.dumpWidgets` visual bounds with the panel’s usable
  bounds, or add an equivalent deterministic geometry assertion. Retain the
  screenshot as a human-readable artifact and sample representative supported
  scale/size combinations.
- **Remaining uncertainty:** The visual defect was observed in one supplied
  offscreen configuration; its precise severity across the rest of the
  responsive envelope has not been sampled.

---

## Probe fixture integrity

### [#1395] PT-3. Final bleeding-pool cases spawn outside the loaded arena

The bleeding-trail probe reports success even though its final collapsed-unit,
adjacent-bleeder, and time-scale pooling cases spawn units outside the flat
arena’s loaded chunk footprint. The engine emits five warnings and falls back
to elevation zero: one collapsed-unit spawn, two adjacent bleeders, and one
time-scale unit for each of the two scale runs.

The fallback elevation happens to equal the arena’s nominal surface elevation,
so the accumulator assertions can still pass. Those cases nevertheless do not
exercise ordinary unit placement on a loaded terrain surface and therefore
weaken the probe’s claimed arena-level coverage.

**Evidence:**

- `src/World/Generate/Arena.hs:45-47` — the arena radius is two chunks, producing
  a five-by-five loaded footprint centered on the origin.
- `src/World/Generate/Arena.hs:112-115` — only chunk coordinates `-2` through
  `2` on each axis are generated.
- `src/World/Chunk/Types.hs:87-95` — each chunk is 16 by 16 tiles, so the eager
  arena covers global tile coordinates `-32` through `47` on each axis.
- `tools/bleeding_trail_probe.py:145-162` — `spawn_fresh` delegates directly to
  `spawn_acolyte` and does not verify that its requested tile has a loaded
  surface.
- `tools/bleeding_trail_probe.py:807-809` — the collapsed-unit case spawns at
  `(30, 50)`, outside the arena on the y axis.
- `tools/bleeding_trail_probe.py:864-867` — the adjacent-bleeder case spawns at
  `(20, 60)` and `(22, 60)`, also outside the arena.
- `tools/bleeding_trail_probe.py:906-915` — both time-scale iterations spawn at
  `(40, 60)`, accounting for the remaining two fallback warnings.
- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:143-155` — when
  `surfaceZInWorld` cannot resolve the requested tile, `unit.spawn` logs
  `chunk not loaded ... defaulting Z=0` and continues.
- `tools/README.md:290` — the probe is documented as the end-to-end arena gate
  for moving trails and stationary/collapsed pooling.
- `tools/ci_probes.py:194-197` — `bleeding_trail` is classified as targeted
  ongoing-bleeding regression coverage, so false-positive fixture setup
  weakens an automated signal rather than only an ad hoc manual scenario.

**Handoff context:**

- **Current behavior:** Five unit spawns silently use the unloaded-chunk
  fallback, but the probe treats the resulting pooling assertions as ordinary
  arena coverage and exits successfully.
- **Expected direction:** Every probe scenario should establish that its unit
  stands on a loaded, resolvable arena surface before applying wounds or
  interpreting blood behavior. A fallback spawn should be a setup failure.
- **Scope and constraints:** Keep the production fallback behavior out of scope
  unless independently reconsidered. Preserve per-case unit isolation,
  deterministic constitution/body-mass pinning, distinct adjacent-cluster
  anchors, decal clearing, death cleanup, and the two-scale comparison. Use
  loaded coordinates or explicitly provision the required chunks without
  allowing state from one scenario to contaminate another.
- **Test direction:** Make the shared spawn helper verify
  `world.getSurfaceAt` before spawning and fail immediately when the tile is
  unresolved. Relocate the final fixtures inside the existing arena or load
  their chunks deliberately, and ensure the engine log contains no
  `unit.spawn` fallback warnings for an accepted run.
- **Remaining uncertainty:** The observation does not establish an error in the
  blood accumulator itself. The final assertions passed, but their behavior on
  ordinary loaded terrain remains unproven by this run.

### [#1396] PT-4. Combat-animation setup does not reliably establish combat

The combat-animation probe can fail before either combatant attacks. In the
captured run, the acolyte progressed through running, walking, climbing,
falling, landing, and collapsed recovery transitions while the bear only
walked. Neither unit produced a swing.

The fixture verifies only a dry, equal-height three-tile strip. It does not
bound the combatants’ surrounding movement or neutralize the acolyte’s standing
`find_water` goal before issuing the attack command. The repository already
classifies this exact approach-and-fall outcome as a recurring probe flake.

**Evidence:**

- `tools/combat_anim_probe.py:58-79` — fixture discovery checks only three
  adjacent equal-height, dry tiles and establishes no safe surrounding arena.
- `tools/combat_anim_probe.py:100-118` — the probe raw-spawns both combatants,
  waits briefly, and issues `commandAttack` without clearing existing AI goals
  or proving that the units remain close and engaged.
- `tools/probelib.py:345-356` — the shared harness documents that a freshly
  spawned acolyte’s `find_water` goal can outrank the tested behavior and walk
  the unit off a cliff.
- `tools/probelib.py:359-369` — the canonical `spawn_acolyte` helper clears that
  goal by default and fails setup when AI state cannot be prepared.
- `tools/ci_probes.py:104-106` — `combat_anim` is manual-only because the
  attacker occasionally falls while approaching the target and never swings,
  observed in one of three classifier runs.

**Handoff context:**

- **Current behavior:** A known fixture and AI-isolation failure produces the
  same exit result as a missing swing animation, so a red probe does not
  reliably distinguish setup failure from product animation failure.
- **Expected direction:** The probe should establish a bounded safe arena,
  neutralize unrelated standing goals, and positively verify proximity and
  combat engagement before interpreting animation samples.
- **Scope and constraints:** Preserve real engine startup, shipped unit and
  animation definitions, real `commandAttack` routing, and live
  `currentAnim` sampling. Do not make a genuine no-swing result pass once the
  combat preconditions have been established.
- **Test direction:** Use the shared AI-isolating spawn path or equivalent
  setup, provision enough safe terrain for approach and combat, and fail with a
  distinct fixture diagnosis if proximity or combat activity is not reached
  before sampling begins.
- **Remaining uncertainty:** The replacement may use a deliberately
  provisioned arena or a stronger generated-world search. The assessment does
  not establish which fixture best preserves the intended realism.

---

## Probe oracle integrity

### [#1397] PT-5. The combat-animation probe does not gate its death-animation contract

The probe advertises two checks: a recognizable swing and, if a combatant dies,
a settled death animation. Only the swing affects its exit result.

In the captured run, the probe printed
`acolyte settled on a death animation: collapsed-to-climbing`. That state is a
recoverable collapse transition rather than death. The helper intended to
recognize death is defined but never called, and strings containing `collapse`
are accepted for display without changing the result.

**Evidence:**

- `tools/combat_anim_probe.py:20-21` — the documented contract promises a
  conditional death-animation check in addition to the swing check.
- `tools/combat_anim_probe.py:134-151` — `died` is defined but unused, `ok`
  depends only on whether either timeline contains `attack`, and terminal names
  containing `collapse` are printed as death animations.
- `src/Unit/Sim/Types.hs:145-164` — `Collapsed` and terminal `Dead` are distinct
  poses; only `Dead` represents actual death.
- `src/Unit/Thread/Movement/Timers.hs:18-34` — non-lethal fall collapse has an
  explicit timed recovery back to `Standing`.
- `data/units/acolyte.yaml:486-499` — actual healthy and injured death rendering
  resolves through the `dead-idle` mapping, including `injured_death`, rather
  than arbitrary collapse-transition names.

**Handoff context:**

- **Current behavior:** Misleading output can label recovery as death, while an
  incorrect death animation cannot independently fail the probe.
- **Expected direction:** A conditional oracle should first establish actual
  death, then verify the legitimate mapped terminal animation and include that
  result in the exit status.
- **Scope and constraints:** Do not require every sampled fight to kill a
  combatant. Preserve removal as an acceptable terminal outcome if that remains
  the engine contract, distinguish ordinary collapse from death, and retain the
  independent swing assertion.
- **Test direction:** Sample authoritative pose or death state alongside
  animation. When death occurs, require a valid death animation or accepted
  removal; when no death occurs, report the death branch as not exercised
  without claiming success for a collapse transition.
- **Remaining uncertainty:** The exact authoritative query for units removed
  immediately after death, and whether such removal should count as sufficient
  evidence without observing one rendered death frame, remain repair-time
  decisions.

---

## Action-outcome behavior coverage

### [#1398] PT-6. The default chop fixture cannot reach the promised partial-result oracle

The action-outcome probe’s default seed-42 run spends approximately eight
minutes exercising its broader contract but cannot find a tree at the sparse
coordinates used by its chop fixture. It consequently exits red without
testing the promised 5×5 partial-chop accounting regression.

The probe fails loudly rather than silently skipping the check, but its default
invocation remains a known failure and supplies no evidence about whether the
chop outcome reports 25 requested tiles as applied plus dropped tiles.

**Evidence:**

- `tools/action_outcome_probe.py:96-120` — `find_chop_mixed_box` samples every
  fourth coordinate in a fixed region and accepts a fixture only after that
  sampled tile produces a genuine 5×5 partial chop result.
- `tools/action_outcome_probe.py:279-298` — failure to find such a sampled tree
  marks the entire probe failed; only the successful branch asserts
  `requested == 25` and `requested == applied + dropped`.
- `tools/chop_probe.py:85-89` — another real-engine probe uses the authoritative
  `world.findHarvestableFlora(..., "wood")` query to locate a choppable tree
  rather than relying on sparse `getFloraAt` sampling.
- `tools/ci_probes.py:245-248` — `action_outcome` is manual-only because it
  needs a generated world, a mixed tillable area, and a real tree for the chop
  partial path. Manual classification does not make a failing default fixture
  an accepted result.

**Handoff context:**

- **Current behavior:** The advertised default invocation produces a known red
  result after an expensive run and never reaches the chop-partial assertion.
- **Expected direction:** Fixture discovery should deterministically establish
  suitable wood-bearing flora, or fail as an explicit setup error before the
  behavior result can be confused with a chop regression.
- **Scope and constraints:** Preserve the public `chop.designate` route, real
  action-outcome draining, at least one applied and one dropped tile, the full
  25-tile requested count, and the identity
  `requested == applied + dropped`. Do not replace the behavior check with a
  synthetic outcome record.
- **Test direction:** Use an authoritative flora query, provision deterministic
  wood flora, or pin a demonstrated fixture. Assert the fixture precondition
  separately before executing and validating the partial designation.
- **Remaining uncertainty:** Whether generated-world realism, a deterministic
  arena fixture, or a pinned seed and coordinate offers the most stable
  long-term contract remains an implementation decision.

### [#1399] PT-7. The portal outcome check still asserts the pre-confirmation contract

The action-outcome probe creates an empty arena, arms starting-portal
placement, and clicks a valid tile. It then expects an immediate accepted
placement outcome and one new building.

Current gameplay intentionally classifies a page with no existing locations as
remote. The first click therefore presents the remote-settlement warning,
leaves placement armed, and does not spawn a portal. The probe interprets that
correct intermediate state as failure and never drives the confirmation action
needed to test successful placement.

**Evidence:**

- `tools/action_outcome_probe.py:327-388` — the probe creates an empty
  `portal_probe` arena, calls the real `buildTool.handleMouseDown` path once,
  then immediately requires `buildTool.commitPlacement=accepted` and a building
  count increase.
- `scripts/build_tool.lua:1003-1022` — a starting portal with no nearby placed
  location, including a page with no locations, opens the remote warning
  without exiting placement or committing the building.
- `scripts/build_tool.lua:910-941` — direct and confirmed placement share
  `commitStartingPlacement`, which spawns exactly once and records the accepted
  action outcome.
- `scripts/build_tool_remote_warning.lua:337-350` — opening the warning records
  `buildTool.remoteWarning=presented`.
- `scripts/build_tool_remote_warning.lua:379-418` — `establishHere` records
  confirmation, revalidates the saved tile and active world, then either calls
  the shared commit path or records `revalidationRejected`.
- Git history places the probe’s immediate-placement assertion before issue
  #779 introduced the remote-settlement confirmation boundary.

**Handoff context:**

- **Current behavior:** Correct remote-warning behavior makes the probe fail
  deterministically in its empty arena, while the post-confirmation accepted
  outcome remains unverified.
- **Expected direction:** The probe should recognize presentation of the warning
  as the first successful outcome, confirm that no building appeared early,
  then drive confirmation and verify the confirmed and accepted outcome
  sequence plus exactly one spawned portal.
- **Scope and constraints:** Preserve the real player-facing click path,
  confirmation boundary, placement-mode behavior, current-tile revalidation,
  accepted placement without a rejection reason, and exactly-once spawning.
  Do not bypass the warning while claiming to test the complete remote flow.
- **Test direction:** Drain and assert the `presented` outcome after the click,
  invoke the real confirmation action, then assert `confirmed` followed by the
  accepted commit and a one-building increase. Retain or add focused coverage
  for cancellation and revalidation rejection where appropriate.
- **Remaining uncertainty:** The processor should decide whether the complete
  remote-warning outcome matrix belongs in this slow behavior probe or should
  share responsibility with a smaller focused harness.
