# repair_ai coverage map (#2939, PRB-5)

`tools/repair_ai_probe.py` (#302) was retired by #2939 after the owner's
2026-11-07 decision to replace it with focused coverage (the same shape as
`unified_transfer`, #2935). It drove the `repair_job` action in
`scripts/unit_ai.lua` with the AI live on a flat arena and was classified
FLAKY (#724). This map accounts for **all 57 checks** it printed, phase by
phase, keyed by the check name exactly as the probe printed it. The probe's
last form is `tools/repair_ai_probe.py` at `47240d0fd` (`git show
47240d0fd:tools/repair_ai_probe.py`).

Phases 3 and 5 both print `acolyte claimed the mule-held item`; they are
told apart below by phase.

**Homes.**

- `S` = `Test.Headless.Lua.UnitAiRepairSourcing`, describe `repair sourcing`
  (new in #2939). Cases are cited by their describe group and the start of
  the `it` text.
- `G` = `Test.Headless.Lua.UnitAiRepairGround`, describe `repair ground
  target` (existing, unchanged in behaviour; its fixtures moved to
  `Test.Headless.Lua.UnitAiRepairFixture`).
- `repair_probe` / `repair_item_probe` = the CI probes `repair` and
  `repair_item`, unaffected by #2939.

"Already covered" means the named case fails if the behaviour breaks.
"Unique" means the probe was the only coverage; the check moved to `S`.
"Not worth keeping" carries its reason.

**Tally.** 45 unique (moved to `S`), 7 already covered (all phase 10, in
`G`), 5 not worth keeping. 45 + 7 + 5 = 57.

## Phase 1 `own_inventory`

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 1 | broken weapon (condition 0) repaired first | unique | S `claim order by severity` / *claims the broken weapon first and the degraded armour only after it*: the broken axe is the scored pick and holds the claim (`claimantOf 801 = 1`, `802 = nil`) before any tick, then reaches 100. The inventory lists the armour first, so a pick by list position fails. |
| 2 | armor untouched while the higher-severity weapon job was active | not worth keeping | A read of "still at 5" after the fact is destroyed by correct follow-on work (the same defect #1736 and #1767 fixed in phase 8): once the weapon job ends the AI may legitimately repair the armour at once. The intent, that the armour is not engaged while the weapon is, is asserted positively in the S case above as `claimantOf 802 = nil` right after the weapon claim, before the job advances. |
| 3 | lower-severity armor repaired afterward | unique | S `claim order by severity` / *claims the broken weapon first…*: after the weapon job ends the armour is the next pick, is claimed, and reaches 100; nothing is left (`none`). |
| 4 | both ground lignite_chunk fetched and consumed | unique (fetch); consumption is another gate's | S *claims the broken weapon first…*: the ground lignite goes 2 → 1 on the first job (worker holds it at the `walking` tick) and 1 → 0 on the second, `groundDefs` empty at the end. What `repair.repairAt` spends is its own contract, graded by `repair_probe` `repair_condition at furnace: fuel consumed, condition->100, sharpness untouched`; the S stub spends the ingredient only so a sequence of jobs can run, and no S case asserts consumption. |
| 5 | smithing work-XP granted (#265 smith role's first work action) | unique | S `claim order by severity` / *grants smithing work experience for a repair it completes*: skill set to 20, one completed job, skill strictly higher. |

## Phase 2 `equipped_ground`

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 6 | ground whetstone present before repair | not worth keeping | Fixture precondition, not behaviour. S `an equipped weapon with a ground consumable` asserts it before acting, so the case cannot pass vacuously. |
| 7 | weapon equipped into right_hand slot | not worth keeping | Setup through `equipment.equip`; that verb's contract is `repair_item_probe` `target is now equipped`. The S fixture places the axe in `right_hand` directly. |
| 8 | axe sits in equipment loadout, not inventory | not worth keeping | Setup precondition. S asserts `equipped:right_hand` before the job. |
| 9 | equipped weapon sharpened to 100 via ground-sourced whetstone | unique | S `an equipped weapon with a ground consumable` / *sharpens the weapon in its slot with a whetstone fetched off the ground*: scored as `axe_steel\|820\|sharpness\|own` (the loadout scan), whetstone fetched off the ground, sharpness 100. |
| 10 | ground whetstone consumed | unique (fetch); consumption is another gate's | Same S case: the ground whetstone is gone and the worker holds it at the `walking` tick. Spending it is `repair_probe` `repair_sharpness at workbench: whetstone consumed, sharpness->100, condition untouched`. |
| 11 | axe still equipped (not left loose in inventory) after repair | unique | Same S case: `equipped:right_hand` after the job and 820 absent from the loose inventory. The engine's own in-place repair of a wielded item is `repair_item_probe` `repairItem finds the WIELDED instance`. |

## Phase 3 `mule_spare_gear`

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 12 | acolyte claimed the mule-held item | unique | S `spare gear held by the technomule` / *fetches the item and its consumable off the mule…*: scored `axe_steel\|830\|condition\|mule`, `fetch_item`, claimant is the worker. |
| 13 | mule-sourced weapon repaired to full condition | unique | Same case: the instance is at 100/100 when the job ends. |
| 14 | repaired weapon returned to the mule | unique | Same case: 830 is in the mule's inventory afterwards. |
| 15 | acolyte no longer holds the (returned) weapon | unique | Same case: 830 is not held by the worker, whose inventory is empty. |
| 16 | lignite_chunk fetched from the mule and consumed | unique (fetch); consumption is another gate's | Same case: at the `walking` tick the worker holds `[830, 831]` and the mule `[]`, so the consumable came off the mule. Spending it is `repair_probe`'s. |

## Phase 4 `dead_claimant_release`

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 17 | first acolyte claimed the mule-held item | unique | S `a dead claimant on a mule-held item` / *holds the claim while the claimant lives, releases it when the claimant dies, and lets a second worker finish the repair*: the rival claims (`fetch_item`, `claimantOf 830 = 2`); a live claimant keeps the item from a second worker (`scoreWorker = none`). |
| 18 | item still on the mule after the claimant died | unique | Same case: after `killUnit`, the mule still holds `[830, 831]`. |
| 19 | second acolyte picked up the released claim and finished the repair | unique | Same case: the worker scores the item again, claims it (`claimantOf 830 = 1`), and the job ends with 830 at 100/100 on the mule. G's `self-heals a claim whose claimant is gone…` is the same rule on the ground rung only. |

## Phase 5 `abort_returns_item`

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 20 | acolyte claimed the mule-held item | unique | S `aborting after the item was fetched` / *returns the fetched instance to the mule while the worker is still alive…*: `fetch_item` claim. |
| 21 | item fetched off the mule into the acolyte's own inventory | unique | Same case: after the `fetch_item` tick the worker holds `[830]`, the mule `[831]`. |
| 22 | acolyte's job cached this station before it's destroyed | unique | Same case: `repairJob.bid` is the station `repairUtility` vetted (`900`) before the station is destroyed. |
| 23 | aborted job returns the fetched item to the mule (not leaked) | unique | Same case: with `building.getInfo` answering nothing for the cached id, the next tick releases the job and the mule holds `[830, 831]`; 830 is still 5/100 (hand-back, not a repair). |
| 24 | acolyte no longer holds the item after the abort | unique | Same case, read on the **live** worker, before any teardown. The probe destroyed the worker first and recounted the mule (the old oracle at `repair_ai_probe.py:750-753`), which could not tell a returned item from a vanished one. |

## Phase 6 `own_item_collision`

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 25 | acolyte claimed the mule-held (degraded) item, not its own | unique | S `the worker's own same-def item` / *repairs and returns the flagged mule instance, never the worker's own axe*: with a healthy own axe in hand, the pick is `axe_steel\|841\|condition\|mule`. |
| 26 | the FLAGGED mule instance reaches full condition | unique | Same case: 841 ends 100/100. |
| 27 | acolyte's OWN axe is untouched (same instance, still healthy) | unique | Same case, and the abort case *hands back the flagged instance, not the worker's own axe, when the job aborts*: 840 stays 100/100 in the worker's inventory. |
| 28 | the repaired FLAGGED instance is the one back on the mule (not the acolyte's own) | unique | Same two cases (completion and abort), plus *fetches the flagged instance off a mule that carries another copy of the def listed ahead of it* for the fetch half. The own axe lists first, so a defName-only transfer would pop it. |
| 29 | acolyte carries exactly its own one axe_steel afterward | unique | The completion case: `countDef workerUid "axe_steel" = 1`. |

## Phase 7 `role_weight` (pure Lua)

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 30 | repair_job mapped to the 'craft' family | unique | S `role weighting of repair_job` / *maps repair_job to the craft family and boosts a smith on it*. |
| 31 | smith gets ON_ROLE (1.4) on repair_job | unique | Same case. |
| 32 | miner gets OFF_ROLE (0.7) on repair_job | unique | Same case. |
| 33 | laborer stays neutral (1.0) on repair_job | unique | Same case. |
| 34 | smith is now OFF_ROLE (0.7) on dig_designation (craft family now has actions) | unique | S *damps a smith's other routine work and keeps a miner's own boosted*. |
| 35 | miner stays ON_ROLE (1.4) on its own dig_designation | unique | Same case. (`tools/role_probe.py` steers a miner onto `dig_designation` under the same constants but never weighs `repair_job` or a smith, so it is not credited.) |

## Phase 8 `player_priority`

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 36 | item starts unflagged | unique | S `player repair priority` / *claims the flagged lower-severity armour before the broken weapon, clears the flag on repair, and then takes the weapon*: `isRepairPriority(802)` is false first. |
| 37 | unitAi.setRepairPriority flags the armor instance | unique | Same case: true after the call. |
| 38 | player-prioritized armor (lower severity) repaired | unique | Same case: the armour ends 100/100. |
| 39 | the flagged armor was claimed/repaired before any work on the higher-severity weapon | unique | Same case: without the flag the weapon is the pick, with it the armour is, and the armour holds the claim (`802 = 1`, `801 = nil`) before the job advances. This is the ordering the probe judged from one live timeline (#1767); here the claim is observed before the follow-on work can begin. G `lets a player-prioritized ground instance beat a more severe unprioritized one…` is the ground rung's counterpart. |
| 40 | priority flag self-clears once the item is actually repaired | unique | Same case: the flag is false once the armour is repaired. G `puts the exact instance back on the worker's own tile…` covers the ground rung's. |
| 41 | un-prioritized weapon repaired afterward | unique | Same case: the weapon is then the pick and ends 100/100. |

## Phase 9 `priority_gating` (pure Lua)

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 42 | itemNeedsRepair is false for a healthy item (90% > threshold) | unique | S `priority is offered only for an item that needs repair` / *judges a healthy item above the threshold and a degraded one below*. |
| 43 | itemNeedsRepair is true for a degraded item (5% < threshold) | unique | Same case. |
| 44 | menuItem offers nothing for a flagged-but-healthy item | unique | S *offers, shows and hints nothing for a flagged-but-healthy item, and still offers a degraded one*. |
| 45 | suffix shows nothing for a flagged-but-healthy item | unique | Same case; the suffix is asserted as the empty string (`[]`), not merely falsy. |
| 46 | hintLine shows nothing for a flagged-but-healthy item | unique | Same case. |
| 47 | menuItem offers 'Prioritize Repair' for a degraded item | unique | Same case. |

## Phase 10 `ground_target` (#1737)

The probe called this phase supporting evidence only; `G` is its primary
gate. `G`'s `repair.repairAt` stub records calls without restoring
condition, so the two "repaired" outcomes (rows 54 and 56) are **not**
credited to `G`; they moved to `S`, whose stub restores in place.

| # | Check as printed | Class | Home or reason |
|---|---|---|---|
| 48 | ground row carries instanceId, sharpness and kind (#1737) | already covered | G `the row both ground readers build` / *gives item.listGround and item.getGroundForUnit the same instanceId, sharpness and kind for every item on the page*; and *always carries sharpness and kind, even where qualityTier is absent…*. |
| 49 | the ground target is degraded below the repair threshold | not worth keeping | Fixture precondition. G's scoring cases select the same rows as candidates, which fails if the row is not degraded. |
| 50 | acolyte claimed the GROUND instance, not a held or mule one | already covered | G `choosing a ground target` / *picks a condition-axis ground target* (source `ground`), *prefers a ground instance over a more severely damaged one on the technomule*, and `taking a ground target` / *takes the EXACT instance…* (`fromGround = true`, phase `fetch_ground`). |
| 51 | the EXACT instance was taken into inventory (item.pickupGround preserves it; nothing was re-created) | already covered | G *takes the EXACT instance, with its quality, fill, condition and sharpness intact, and removes it from the page it lay on*. |
| 52 | its quality and sharpness survived the pickup untouched | already covered | The same G case (`(721, "axe_steel", 3, 17, 88, 0)`). |
| 53 | it is no longer lying on the ground while carried | already covered | The same G case (`groundRows` is empty). |
| 54 | the ground-sourced weapon was repaired to full condition | unique | S `a ground-sourced target that is repaired` / *goes back to the ground at full condition with its quality unchanged…* (`REPAIRED[1].instanceId = 851`, then 100). |
| 55 | the target was RETURNED to the ground when the job ended (#1737 requirement 8), never left in the worker's inventory | already covered | G `returning a ground target` / *puts the exact instance back on the worker's own tile and page when the job completes* and *returns it to the ground when the job aborts after pickup, never handing it to a technomule*. |
| 56 | it went back at full condition and unchanged quality | unique | The S case of row 54: back on the ground at the worker's tile with condition 100 and quality 88. |
| 57 | the worker is not still holding it | already covered | G *puts the exact instance back…* (`invOf` excludes 731), and the S case of row 54. |

## Only the live tick could show

None of the 57 checks graded the live `unit_ai` arbitration for its own
sake; each is a property of `repairUtility` / `repairExecute` and the
state they leave, which `S` and `G` drive directly. What the live tick
added was noise: the probe's own comments describe the interactions it had
to design around (an idle Materials item being auto-deposited by
`store_materials`; a `unit_warning` pausing the engine; the spawn loadout
settling late), and the recorded flake is exactly that timing (#724). No
small replacement probe is added: nothing that remains has a deterministic
oracle that does not race claim, fetch and work timing.

Accepted loss, stated so it is not mistaken for coverage: nothing now
exercises `repair_job`'s competition with other actions in the real
`unit_ai` dispatch (its entry utility against wander, the 6.0 lock utility
against `store_materials`). That arbitration was never one of the 57
checks, and the probe could not grade it reliably.

## Negative controls

Run at branch head `026619218` (after the rebase onto #2958). Each mutation below was applied to the production Lua alone (no test
changed), `cabal test synarchy-test-headless --test-options='--match
"repair sourcing"'` was run, and the file was restored. All 15 `S` examples
pass unmutated.

| Mutation | `S` failures |
|---|---|
| **Ordering**: in `unit_ai_repair_target.lua` `rank`, prefer the LESS severe candidate | 2 of 15: `claims the broken weapon first…`, and the priority case (whose last step expects the weapon second). |
| **Abort hand-back**: remove `unit.transferItemToUnit(uid, mule.uid, job.defName, job.instanceId)` from `abortRepairJob` | 5 of 15: the mule case, the dead-claimant case, the abort case, and both own-item cases. The abort case fails on the mule inventory read taken from the live worker. |
| **instanceId on hand-back**: the same call without `job.instanceId` | 2 of 15: both own-item cases (completion and abort). |
| **instanceId on fetch**: `fetch_item`'s `transferItemToUnit(mule.uid, uid, job.defName, job.instanceId)` without the id | 1 of 15: `fetches the flagged instance off a mule that carries another copy…`. |
| **Priority**: `isPriority` always false | 1 of 15: the priority case. |
| **Self-clear**: drop `repairPriority[job.instanceId] = nil` on repair | 1 of 15: the priority case. |
| **Dead claimant**: drop `not unit.exists(c.uid)` from `repairClaimedByOther` | 1 of 15: the dead-claimant case. |

A first ordering mutation (`sev < bestSev` with the `best == nil` seed left
out) made every selection fail and was discarded as uninformative; the one
tabled keeps the first candidate and prefers the less severe afterwards, so
single-candidate cases correctly still pass.
