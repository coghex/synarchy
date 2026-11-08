# #2937: the AI-planted row crop missing after save/load in `farm_ai_probe`

Verdict: **the game was at fault**, not the probe. Chunk admission killed every
planted row crop, on every load. Introduced by `e79b7bfac` ("Reconcile flora
conditions when chunks load", 2026-11-01), fixed in the pull request that closes
#2937. Source report: `docs/probe_value_findings.md` PRB-3.

## What was established

### 1. The instance was in the saved record

Decoding the kept run-2 save (`farm_ai_run2_root/root/saves/farm_ai_v79_check_c40c6fb3/world.synworld`)
with `exe:synarchy-save-codec summary` gives, for the page's flora edit log
(`scenarioFlora`):

```
planting -64 -48 tomato_plant 4611686018427387905 0   6.0   (site C)
planting -64 -44 tomato_plant 4611686018427387906 364 6.0   (site D)
planting -64 -48 tomato_plant 4611686018427387907 614 6.0   (site C again)
planting -64 -56 tomato_plant 4611686018427387908 614 6.0   (site B, the AI's)
planting -64 -40 tomato_plant 4611686018427387909 693 6.0   (site E)
remove   -64 -48 4611686018427387905                        (only removal)
cursor 6
```

Site B is `(-64, -56)` (`site A=(-64,-60) ... B=(-64,-56)` in the run's log). Its
planting, id `…908`, is present and has **no** `WeRemoveFlora`. The page's
`floraConditions` holds only two wild dandelion records, nothing for `…908`. So
the instance existed at capture time and the save lost nothing.

### 2. It disappears on load, deterministically

Booting the current engine on a copy of the kept resource root and loading that
save reproduces the failure: after `LoadSucceeded` and `loadChunksInRegion`
/`waitForChunks`, the chunk is resident (`getSlopeAt`/`getSurfaceAt` answer, and
27 wild instances around it are listed), but `world.getFloraGrowthAt` and
`world.getFloraAt` are `nil` at B, D(`-64,-44`) and E(`-64,-40`) alike. The age-0
planting at E rules out a seasonal-rot explanation.

A fresh world reproduces it without the acolyte, the AI or any time dwell: set the
date, till a tile, `world.plantRowCropAt`, read it back (present), `save`, `load`:

| planted on | after one save/load |
|---|---|
| year 1 day 5 (absolute day 4) | `dead=True`, age 360, retention to day 60 (a corpse) |
| year 1 day 200 | gone |
| year 2 day 5 (absolute day 364) | gone |
| the default start date (day 0) | **survives** |

### 3. Cause

`reconcileLoadedOccurrences` (`src/World/Thread/Command/Flora/Condition.hs`) runs
for every chunk the load admits. For an instance with no explicit age anchor it
evaluated the growth state at **day 0**. A planting's age baseline is
`fiAge = -plantedDay` (`World.Edit.Apply`), so at day 0 its total age is negative:
generation -1, inside the previous generation's dead window. `initialDeathDay`
then returned day 0, which is not after the current day, so the plant was marked
dead at once; once its retention window (60 days) had passed, `RetainAwaitReplanting`
ran `removeRowOccurrence`, which appends `WeRemoveFlora` and drops the instance.
A plant planted on day 0 (age baseline 0) is the only one that escaped, which is
why runs that planted early looked fine.

### 4. Why the failing check moved between runs

Two different causes, so a single cause is **ruled out**:

* The **save/load** check (direct runs 1 and 2, 2026-11-07, master `c42ea0a1f`) is
  this defect. It passed in lab runs A, B (2026-10-04/05) and C (`5578e845`, revision
  `acc77479e`, 2026-10-19) because the admission reconcile did not exist yet:
  `git merge-base --is-ancestor e79b7bfac acc77479e` is false, and `e79b7bfac` is dated
  2026-11-01. Run C's "save/load passed with a tomato at `stage: senescing`" is the
  pre-defect behaviour.
* The **re-designation** check failure in lab run C (`new=[] stale_plot=None`) cannot
  be this defect: it happens before the probe's only save/load, on a revision that
  predates the cause. Run C's artifacts were not kept, so its own cause was not
  reproduced. The check, as written, accepted the designation disappearing as
  completion and then read the tile once, with a failed query indistinguishable from
  an absent plant; it also never established that the worker held the wheat job or
  that the swap landed before that job finished. Those boundaries are now explicit
  (below), so a recurrence names which one failed instead of reading as a stale-crop
  failure.

## What changed

* **Game fix**: `initialDeathDay` evaluates an unanchored instance at the first day
  its total age is non-negative. That is day 0 for generated flora (unchanged), and
  the planting day for a planting, so its first generation starts when it was planted.
  No persisted shape or meaning changed, so no component version moved.
* **Regression test** (`test-headless/Test/Headless/World/FloraCondition.hs`):
  "admits a planted annual row crop on its later calendar day alive, and kills it only
  once its lifespan has passed". It admits an annual row crop planted on day 200 on
  its planting day and on day 359 (alive, no record, no removal edit) and on day 560
  (dead, corpse retained). It fails without the fix: expected the instance resident,
  got none and a `WeRemoveFlora` edit.
* **Probe** (`tools/farm_ai_probe.py`):
  * check 11 follows two AI plantings (B and the re-designation's E) by `instanceId`:
    a read right before the save with the simulation paused, then a read after the
    load with the session resumed and the chunks admitted. A first attempt that read
    while the load left the session paused passed on the broken engine, because the
    reconcile runs once the world thread admits the chunk, so the post-load read
    follows the resume, as a player's load does.
  * `growth_read` keeps a failed query apart from "nothing grows here".
  * check 10 holds the worker instead of racing it: the simulation is paused the
    moment the `wheat` claim is seen; under the pause the probe reads the job
    (progress below 1), no plot and no flora on the tile, makes the swap, and polls
    until the world thread has committed the tomato designation (`plant.designate`
    only queues it), re-reads that the wheat job is still unfinished, then resumes. It
    completes on the tomato instance appearing, with no wheat plot and the
    designation cleared.

## Evidence kept

Under `~/.local/state/synarchy-2937/` (outside the repository):

* `probe_unfixed_run1.log`: revised probe on the unfixed engine, `SOME FAILED`: both
  pre-save reads pass (`…908` age 79 and `…909` age 0, `read_failure=None`), both
  post-load reads are `[]`.
* `probe_fixed_run1.log`: same probe on the fixed engine, `ALL FARM AI CHECKS PASSED`,
  the same two instance ids present after the load with the same ages.
* `probe_fixed_run2_hold.log`: the final probe (check 10 holding the worker) on the
  fixed engine, `ALL FARM AI CHECKS PASSED`; the swap was committed with
  `plantJob='-64,-40,wheat,0'`, no plot, no flora.
* `persistence_contract_run1.log`: `run_probes.py --only persistence_contract --exact`
  on the fixed tree, PASS (262.4 s).
* `probe_fixed_paused_read_run0.log`: the first draft (post-load read while paused)
  passing on the fixed engine; it is not evidence about the unfixed one.

Earlier evidence: `~/.local/state/synarchy-probe-value-20261107/` (runs 1 and 2).
