# Expedition survival mechanics findings

This report extracts the remaining actionable mechanics and scenario-quality
concerns from the 2026-07-25 expedition survival calibration, whose run record
is archived verbatim as
[`docs/history/expedition_survival_calibration_2026-07.md`](history/expedition_survival_calibration_2026-07.md)
— that archive, not this ledger, carries observations E1-E7 and the
fall-calibration section. This report is intended for one-at-a-time
re-verification and disposition through `process-report`, not as a new balance
verdict.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source calibration exercised a deterministic 48-tile expedition and a
first-aid fall scenario using the real starting party, inventories, survival
resources, movement orders, and medical APIs. The project owner reviewed that
run and approved no immediate water, food, or shared wound-system tuning.

This conversion re-verified the source observations against the current unit
definitions, spawn path, transfer policies, utility AI, pathing costs,
consumption code, wound state machine, medical behavior, focused tests, and
scenario runner. No engine scenario was rerun during conversion; randomized
outcomes that require a fresh runtime observation are identified explicitly.

The original shallow-fall trauma discrepancy is not retained as an open
finding. The current `Unit.Fall` model and deterministic profile tests implement
the subsequent #998 correction and verify the restored 2-z injury and fracture
contract. Tracker duplication was not evaluated here; that belongs to
`process-report`.

## Status

- [x] SURV-1. Low-capacity acolytes can lack expedition-supply headroom — [no-issue]
- [x] SURV-2. The calibration transfer helper can overshoot carrying capacity — [#1212]
- [x] SURV-3. Spawn-time capacity checks ignore innate capacity modifiers — [#1213]
- [x] SURV-4. Completed move orders immediately release units back to wander — [#1216]
- [x] SURV-5. Ambient wander can select routes across damaging drops — [#1217]
- [x] SURV-6. The first-aid scenario races live AI before its baseline — [#1218]
- [x] SURV-7. A discrete meal can consume and waste an entire food reserve — [#1219]
- [x] SURV-8. Canteen drinking can drain a different instance than it selected — [#1220]
- [x] SURV-9. Imminent blood loss does not constrain locomotion above the collapse threshold — [no-issue]
- [x] SURV-10. Post-fix multi-wound treatment lacks a stabilization observation — [#1221]

---

## Expedition load and provisioning

### [no-issue] SURV-1. Low-capacity acolytes can lack expedition-supply headroom

> **Disposition:** No issue — the ~11 kg weak-roll tail sitting just under the
> ~12 kg full kit is an explicitly recorded design decision
> (`Body.hs:185-194`: shed pick/shovel "instead of flooring the formula"),
> with mule dependence as the documented logistics answer
> (`acolyte.yaml` starting_inventory, the `unit_ai_fetch.lua` ladder). The
> remaining concern — identifying low-capacity units during party
> preparation — is existing scope of open epic #1013, whose Mode A panels
> show weight/capacity and whose landed strict policy (`Unit/Transfer.hs`)
> refuses over-capacity per item, so provisioning can neither overload a
> weak acolyte nor fail invisibly.

The body-derived carrying-capacity range intentionally permits weak acolytes
whose capacity is below the complete starting loadout. Spawn shedding removes
the pick and shovel, but the remaining mandatory equipment, canteen, food,
axe, and radio can leave little or no room for mission-specific supplies such
as a first-aid kit or additional rations. A strict transfer surface will refuse
those supplies; a lax transfer surface can instead overload the unit and impose
a severe travel penalty.

> **Source note:** “A small acolyte cannot carry its own kit.”

**Evidence:**

- `src/Unit/Thread/Command/Body.hs:185` — the capacity calibration explicitly
  places the weakest rolls near 11 kg against a roughly 12 kg full starting
  loadout.
- `data/units/acolyte.yaml:369` — the mandatory inventory includes a full
  canteen, food, tools, axe, and radio; only the pick and shovel have shedding
  priorities.
- `src/Unit/Thread/Command/Spawn.hs:122` — spawn preserves armor, weapons, and
  survival items while shedding only prioritized inventory.

**Handoff context:**

- **Current behavior:** Some generated acolytes begin at or near their effective
  carrying limit after spawn-time shedding.
- **Expected direction:** Establish whether every starting acolyte should have a
  minimum amount of expedition-supply headroom, or whether low-capacity units
  are intentionally dependent on the technomule and must be identified clearly
  during party preparation.
- **Scope and constraints:** Preserve meaningful body diversity, encumbrance,
  equipment weight, and the technomule’s logistics role. Measure the complete
  mandatory load, including worn equipment and container fill.
- **Remaining uncertainty:** The current distribution of post-shedding headroom
  across randomized acolyte rolls has not been recomputed.

### [#1212] SURV-2. The calibration transfer helper can overshoot carrying capacity

The calibration runner describes its legacy unit-to-unit provisioning as
capacity-gated, but its check only asks whether the receiver is already at
capacity before moving the next item. It does not include the prospective
item’s full instance weight. The final transfer can therefore push the receiver
over capacity and contaminate the route measurement with encumbrance.

This does not prove the intentionally lax legacy transfer API should be changed:
current code documents that AI fetch, repair, and medic callers depend on that
contract. It does show that the scenario’s local guard is insufficient.

**Evidence:**

- `tools/gameplay_scenarios_support.py` (`transfer`) — the runner acknowledges that
  `unit.transferItemToUnit` performs no capacity check.
- `tools/gameplay_scenarios_support.py` (`transfer`) — the local guard checks `w >= cap` before
  transfer rather than `w + itemWeight > cap`.
- `src/Unit/Transfer.hs:29` — the newer transfer policy deliberately leaves the
  old unit-to-unit verb lax while applying strict capacity rules to the new
  player-managed path.

**Handoff context:**

- **Current behavior:** A receiver just below capacity can accept one more
  indivisible item and finish over capacity.
- **Expected direction:** Scenario provisioning should use the strict transfer
  policy or perform an exact prospective check against the selected item
  instance.
- **Scope and constraints:** Do not silently tighten the legacy API without
  auditing AI callers that deliberately preflight their own transfers.
- **Remaining uncertainty:** No fresh scenario run measured how often the
  current provisioning set crosses capacity on its final transfer.

### [#1213] SURV-3. Spawn-time capacity checks ignore innate capacity modifiers

Spawn-time shedding compares load against `carrying_capacity` in the base
`initialStats` map. Innate and accessory modifiers are not attached until the
`UnitInstance` is constructed afterward. The technomule therefore has its
starting inventory judged against its unmodified body-derived capacity even
though its permanent cybernetic modifier raises the live capacity by 50%.

Because the mule’s starting inventory has no shedding priorities, a load that
fits its live capacity but exceeds its base capacity produces an over-capacity
spawn warning with nothing eligible to remove.

**Evidence:**

- `src/Unit/Thread/Command/Spawn.hs:132` — shedding reads capacity directly from
  `initialStats`.
- `src/Unit/Thread/Command/Spawn.hs:155` — innate and accessory modifiers are
  attached only after the shedding decision.
- `data/units/technomule.yaml:40` — the mule declares a permanent 50% modifier
  to `carrying_capacity`.
- `data/units/technomule.yaml:50` — the starting cargo is explicitly calibrated
  against the modifier-raised capacity.

**Handoff context:**

- **Current behavior:** Spawn validation and live gameplay can disagree about
  the mule’s capacity.
- **Expected direction:** Spawn-time load validation should use the same
  effective-stat calculation as later pickup and transfer policies.
- **Scope and constraints:** Preserve deterministic modifier application,
  accessory effects, item fill and nested-content weight, and existing shedding
  priority.
- **Remaining uncertainty:** The current randomized mule’s exact base capacity
  and initial load were not sampled in a fresh engine run.

---

## Movement and scenario control

### [#1216] SURV-4. Completed move orders immediately release units back to wander

> **Decision (2026-08-10):** hold position — an arrived unit stands at its
> commanded destination (wander and autonomous work suppressed; survival
> interrupts stay live and return to the anchor) until a new command or an
> explicit release. Filed as #1216.

A commanded task is deleted as soon as the unit enters the arrival radius.
Nothing records a post-arrival hold or formation intent. On the next AI
decision, ambient wander becomes eligible and can move the unit away from the
destination. This remains important to expedition return, regrouping, and
coherent multi-unit observations.

> **Source note:** “A completed move order does not hold position.”

**Evidence:**

- `scripts/unit_ai_core.lua:260` — arrival clears `commandedTask` immediately.
- `scripts/unit_ai_needs.lua:26` — wander remains an ordinary positive-utility
  candidate for a rested acolyte.
- `scripts/unit_ai_needs.lua:46` — wander issues a new random nearby move.
- `tools/README.md:312` — the integrated expedition probe still compensates by
  pinning arrivals and taking paused, simultaneous observations.

**Handoff context:**

- **Resolved by #1216:** a completed PLAYER move order now leaves the unit
  holding its destination (`scripts/unit_ai_hold.lua`), at `follow_command`'s
  own utility — so every survival/combat/treatment interrupt that outranked
  the order still preempts the hold and returns to the anchor afterwards,
  while wander and work entry stay suppressed until an accepted player
  command or `unitAi.releaseHold`. The source note below quotes the original
  calibration and is retained as history.
- **Current behavior (as of the source calibration):** Reaching a commanded
  point restores autonomous wander, so units do not reliably remain mustered
  or at camp.
- **Expected direction:** Make the post-command behavior explicit and
  player-legible, whether that is hold, remain within a formation radius, or
  intentionally resume autonomous work.
- **Scope and constraints:** Do not globally disable useful autonomous jobs or
  survival interrupts. Distinguish one-shot movement from persistent stance or
  formation orders.
- **Remaining uncertainty:** Whether immediate return to wander is still the
  desired default is a product decision.

### [#1217] SURV-5. Ambient wander can select routes across damaging drops

Ambient wander chooses an arbitrary radial destination without inspecting
terrain or hazard. Pathing assigns an exponential cost to falls, but a fall is
not prohibited; if the random target or available route requires a drop, the
unit may still take it. The original first-aid setup observed an acolyte walking
off the ridge before receiving the commanded descent.

**Evidence:**

- `scripts/unit_ai_needs.lua:46` — wander selects a random angle and distance
  with no terrain query.
- `scripts/unit_ai_needs.lua:58` — that target is passed directly to
  `unit.moveTo`.
- `src/Unit/Pathing/Cost.hs:124` — damaging descents are discouraged through
  cost rather than rejected as impassable.
- `tools/gameplay_scenarios_support.py` (`spawn_roster`) — the runner already documents that an
  autonomous water-search goal can walk a scout off a cliff.

**Handoff context:**

- **Current behavior:** Ambient movement may traverse a real fall when no safer
  route satisfies the randomly selected target.
- **Expected direction:** Give non-emergency autonomous movement an explicit
  hazard policy so aimless activity does not choose preventable injury.
- **Scope and constraints:** Preserve deliberate player movement, forced
  retreats, panic or delirium behavior, legitimate climbing, and cases where a
  risky route is intentionally allowed.
- **Remaining uncertainty:** A fresh run has not measured the current frequency
  of hazardous wander outside the artificial ridge scenario.

### [#1218] SURV-6. The first-aid scenario races live AI before its baseline

The scenario spawns the party onto a prepared ridge, waits 1.5 seconds for
materialization, clears one standing goal, transfers the kit, and reads the
“before the fall” checkpoint while the simulation and ambient AI remain live.
It does not first pause, hold, or otherwise establish that the scout is still
healthy at the intended start tile. A pre-command fall can therefore invalidate
the baseline while the scenario continues.

**Evidence:**

- `tools/gameplay_scenarios_support.py` (`spawn_roster`) — `spawn_roster` creates live AI-controlled
  units and waits before clearing their initial water goal.
- `tools/gameplay_scenarios_support.py` (`spawn_roster`) — the materialization wait gives ambient AI
  time to act.
- `tools/gameplay_scenarios_first_aid.py` (`run_first_aid`) — the first-aid scenario immediately
  provisions the live roster.
- `tools/gameplay_scenarios_first_aid.py` (`run_first_aid`) — the “before the fall” checkpoint is
  recorded without an atomic health/position precondition.

**Handoff context:**

- **Current behavior:** Setup movement can injure the scout before the intended
  descent, making before/after treatment observations ambiguous.
- **Expected direction:** Establish the roster and pre-fall checkpoint under a
  paused or otherwise controlled simulation, then release only the intended
  command.
- **Scope and constraints:** Keep this runner observational and manual-only; the
  fix should not convert balance outcomes into CI pass/fail thresholds.
- **Remaining uncertainty:** The current runner has not been rerun to determine
  whether the race still reproduces frequently.

---

## Food and container consumption

### [#1219] SURV-7. A discrete meal can consume and waste an entire food reserve

> **Decision (2026-08-10):** stop before waste — a meal still feeds to full
> from bulk food, but another DISCRETE item is opened only when the stomach
> can hold at least a tunable fraction of its calories. Filed as #1219.

Once the stomach falls below the eating threshold, `eatExecute` repeatedly
consumes food until the stomach reaches 99% or inventory runs out. Discrete
foods are removed whole and excess calories are deliberately discarded at the
stomach cap. Because stomach capacity scales with body mass while rations are
fixed at 250 kcal, a large acolyte can consume several or all carried rations in
one decision, even though some of the final ration is wasted.

> **Source note:** “A meal empties the pack.”

**Evidence:**

- `scripts/unit_ai_needs.lua:164` — one eating action loops through as many as
  ten food items.
- `scripts/unit_ai_needs.lua:175` — eating continues until the stomach is at
  least 99% full.
- `scripts/unit_ai_needs.lua:180` — discrete-food overflow is clamped and wasted
  by design.
- `data/items/rations.yaml:14` — each ration contributes an indivisible 250 kcal.

**Handoff context:**

- **Current behavior:** A single meal can erase the expedition’s remaining
  discrete reserve and discard part of the final item.
- **Expected direction:** Make reserve expenditure and discrete overflow an
  intentional, legible policy rather than an accidental consequence of
  feed-to-full iteration.
- **Scope and constraints:** Preserve the established “meal, not bite” behavior,
  salt delivery, bulk-food partial consumption, and the distinction between
  stomach contents and the calorie store.
- **Remaining uncertainty:** The desired tradeoff among meal fullness, ration
  indivisibility, reserve protection, and calorie overflow is a design decision.

### [#1220] SURV-8. Canteen drinking can drain a different instance than it selected

> **Verification note (2026-08-10):** the refill path
> (`unit_ai_water.lua`, `refillExecute`) has the same wrong-instance
> defect — headroom measured on the selected canteen, fill applied to the
> first same-def match. #1220 covers both call sites.

The drinking action locates a specific filled canteen and calculates the sip
from that instance. It then mutates inventory by definition name, which drains
the first matching canteen rather than the selected instance. If an earlier
same-definition canteen is empty or has a different fill level, hydration can be
credited from one instance while another is drained—or no water is drained at
all.

The exact-instance mutation API already exists and is used by the general
consumable path for precisely this reason.

**Evidence:**

- `scripts/unit_ai_needs.lua:72` — canteen lookup returns a specific inventory
  record.
- `scripts/unit_ai_needs.lua:100` — drinking calculates its sip from that
  selected canteen’s fill.
- `scripts/unit_ai_needs.lua:121` — the mutation nevertheless calls
  `modifyItemFill` by definition name.
- `src/Engine/Scripting/Lua/API/Units/Equipment.hs:25` — name-based mutation
  explicitly adjusts the first matching instance.
- `scripts/consumable.lua:110` — the consumable implementation documents the
  same wrong-instance failure and uses `modifyItemFillById`.

**Handoff context:**

- **Current behavior:** Multiple steel canteens in one inventory can desynchronize
  water consumption from hydration credit.
- **Expected direction:** Mutate the exact instance selected for the sip and
  verify multiple empty/partial/full same-definition containers.
- **Scope and constraints:** Preserve fill clamping, hydration-per-litre,
  animation timing, inventory order, and the existing name-based API for callers
  that intentionally want first-match semantics.
- **Remaining uncertainty:** No focused canteen test or fresh engine reproduction
  of the multi-instance case was found during drafting.

---

## Injury behavior and verification

### [no-issue] SURV-9. Imminent blood loss does not constrain locomotion above the collapse threshold

> **Disposition:** No issue — project-owner decision (2026-08-10): the
> straight cliff (full capability above the 30% blood threshold, collapse
> below it) is the intended behavior for now. Graduated pre-collapse
> weakness is planned as part of separate, not-yet-implemented work and is
> deliberately out of this finding's scope. Verified state: collapse is
> `unconsciousFraction = 0.30` on current blood (`Combat/Wounds/Tick.hs`),
> injury knockout is concussion-only, crawl is leg-damage-only, and
> `brain.lua`'s consciousness reads core temp / blood oxygen / salt — no
> blood-volume or bleed-rate input anywhere.

Blood loss collapses a unit only after current blood falls below 30% of maximum.
The injury locomotion state separately considers concussion and disabling
leg/foot wounds. Neither path considers aggregate bleed rate or projected time
to unconsciousness. A conscious unit above 30% blood with usable legs can
therefore stand, wander, or follow a command even while active bleeding predicts
collapse or death within seconds.

The #998 fall correction reduced the shallow-fall case substantially, but the
underlying rule still applies to severe combat or multi-wound bleeding.

> **Source note:** “A critically bled unit stands up and walks.”

**Evidence:**

- `src/Combat/Wounds/Constants.hs:37` — blood-driven unconsciousness begins at
  30% of maximum blood.
- `src/Combat/Wounds/Tick.hs:429` — the outcome checks current blood thresholds,
  not bleed-rate prognosis.
- `scripts/injuries.lua:480` — injury unconsciousness is based on concussion.
- `scripts/injuries.lua:512` — locomotor incapacity is based on severe leg and
  foot damage.
- `scripts/unit_resource_injury.lua:121` — the pose state machine combines those
  present-state signals after fall knockdown expires.

**Handoff context:**

- **Current behavior:** A unit with enough blood at the current instant and
  functional legs can resume purposeful movement despite imminent
  exsanguination.
- **Expected direction:** Decide explicitly whether projected blood loss, pain,
  shock, or a medical-emergency state should constrain pose, speed, command
  execution, or AI priorities before the absolute collapse threshold.
- **Scope and constraints:** Keep this separate from fall-trauma generation and
  shared bleed-rate tuning. Preserve conscious crawling and emergency behavior
  where movement remains physically possible.
- **Remaining uncertainty:** Whether this is desired dramatic behavior or a
  missing shock/triage mechanic requires product judgment.

### [#1221] SURV-10. Post-fix multi-wound treatment lacks a stabilization observation

The corrected fall tests verify wound count, injury kinds, aggregate bleed, and
fracture thresholds, but not whether the starting party’s medic can stabilize a
representative patient using the shipped kit. `treatBleeding` dresses one
worst-bleeding wound per call, and the medic AI repeats it on later idle
decisions. The manual first-aid scenario stops observing as soon as it sees the
medic claim the patient, rather than following treatment to controlled bleeding,
resource exhaustion, collapse, or death.

The legacy post-fix samples showed materially lower bleeding, but they did not
establish end-to-end treatment throughput.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/Medical.hs:53` — each treatment call selects
  and dresses one worst-bleeding wound.
- `scripts/unit_ai_medic.lua:313` — the medic repeats treatment on subsequent
  idle ticks until bleeding stops or supplies fail.
- `tools/gameplay_scenarios_first_aid.py` (`follow_treatment`) — the scenario watches for a treatment claim
  and exits that observation loop immediately once the claim appears.
- `tools/gameplay_scenarios_first_aid.py` (`run_first_aid`) — final success evidence requires only that
  some wound is dressed.
- `test-headless/Test/Headless/Unit/Fall.hs:145` — current regression tests cover
  fall output and bleed projections, not medic stabilization.

**Handoff context:**

- **Current behavior:** Injury generation is calibrated, but the full
  fall→triage→repeated treatment→stable-or-terminal outcome is not measured.
- **Expected direction:** Add an observational treatment-throughput scenario that
  follows a deterministic representative wound set until bleeding is controlled,
  supplies are exhausted, the patient dies, or a bounded timeout is reached.
- **Scope and constraints:** Keep balance outcomes observational until the project
  owner approves explicit thresholds. Use the real medical API, AI claim path,
  kit contents, blood tick, and clotting behavior.
- **Remaining uncertainty:** The current medic may already stabilize the
  post-#998 wound set reliably; the missing observation is the concern.
