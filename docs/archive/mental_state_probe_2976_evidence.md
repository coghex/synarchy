# mental_state probe: residual flakes (#2976)

Follow-up to `docs/archive/mental_state_probe_2975_evidence.md`. Scope: phase 4a/4b
(wander and flee breaks), 9c2's staged-hit setup, 9k, 9h, 9b's landed-attack check
and (owner ruling 2026-11-09, "Fix 9g too") 9g's fixture leak. Every change is in
`tools/mental_state_probe.py`; no engine, Lua or content file changed.

## Cause of each flake: no gameplay defect

| check | cause | class |
|---|---|---|
| 4a wander break | graded a fixed 0.9 tiles of net displacement; a slow unit cannot walk that far in a six-second break | probe pace assumption |
| 4b flee break | graded a fixed 2.0 tiles farther from the mule's REGISTRATION position; a slow unit cannot, and the technomule walks by itself so the stale reference also misjudges | probe pace assumption + stale reference |
| 9c2 setup, "10 never landed a hit on 8" | the neutered attacker's stamina pool is tiny, the committed chase exhausts it, it collapses and recovers before it can swing; the hit latency has a 18-25 s tail in 9 of 45 trials against a 35 s wait | fixture stamina |
| 9c2 setup, "subject could not act" | the subject was already dead at the break (before batch runs 3 and 6; once in the after batch's final-head run 8 and once in a 50-trial harness): one neutered head strike (strength_base 0.02, neuter read back) inflicted brain trauma 1.6 (and a jaw fracture 1.07 in another trace); the old code waited 15 s for a dead unit | ungradable setup (restage on fresh units) |
| 9k "expected only 30, saw 26" | not reproduced in any measured batch. From the kept #2975 transcripts (`setup: 29 never targeted 30`, then seven samples all `26`) and the code: by uid order 26 is 9i's target, which 9i teleports to (-19, 25) and leaves alive, six tiles from 9k's subject spot (-25, 25), inside lash-out's eight-tile range; wandering it can become the nearest eligible unit, and a full-strength strike on the adjacent target can take it out of the fight | fixture isolation (from evidence; not reproduced) |
| 9h "expected Chebyshev-nearer 26, got 27" | the decoy was staged at exactly (+8, 0), ON the eight-tile eligibility boundary, and the units meander from spawn: a traced run read it at dx 8.96 (ineligible); later traces show 2-3 tiles of drift within seconds | fixture geometry |
| 9b landed attack | seen once in the development batches (dev5 run 3): the SUBJECT was dead (`lpose: dead`, target still set, the attacker's record never named it) before it could swing, the same fatal neutered head hit as 9c2; not seen in the before batch. The predicate was also unattributable (it matched the attacker record's uid with no time check); it now requires a hit inside the lash-out episode, and a dead subject restages 9a/9b | ungradable setup (dead subject) + predicate hardened |
| 9g "targeted — target 19" (item 6) | 19 is 9f's lash-out subject, left alive; it wandered into 9g's isolated subject's range | fixture isolation |

### 4a/4b: what the flee does (engine contracts, mental states)

`scripts/unit_ai_mental.lua` `fleeExecute`: nearest other live unit strictly within 15
tiles, an eight-tile destination directly away with angular jitter, at
`movement_speed.ordered`; it only issues a move when the unit is not already walking.
Replayed 18 trials of phases 1-4b with a 0.2 s sampler (`ENGINE_DEBUG=Lua`):

- the flee ran at EXACTLY the unit's ordered speed (`moveSpeed` equal to `ordered` on every
  active sample), in the direction away from the mule, and the mule was the nearest unit;
- ordered speed varies per unit from 0.47 to 0.84 tiles/s (endurance is drawn with range
  1.0; the band modifiers scale it), and the gain at the end of the six-second break was
  0.82-0.92 x ordered x 6 s, so a unit under ~0.4 tiles/s cannot reach the old fixed 2.0;
- pinning endurance at 0.4 reproduced the old failures: 4a failed 3 of 4 and 4b 2 of 4
  (one run both), with the unit fleeing exactly per contract;
- the technomule moves: the live distance to it differed from the distance to its
  registration position by up to 2.7 tiles during the break, which is why a distance gain
  from a fixed reference (the old check) and from the live mule (a first rewrite) both
  mis-grade.

### What the probe now proves

- **4a** `wander_break_verdict`: the unit is stopped in the same console chunk that forces
  the break; while the wander break is active it walks at least
  `min(0.3, 0.4 x meander x active seconds)` tiles of path (9g's walked-path predicate on
  this break's own flag; a normal acolyte is held to the full 0.3 tiles). Movement after the
  break ends never counts.
- **4b** `flee_break_verdict`: the technomule is proven registered AND is the unit the
  production search would flee from; the unit is stopped at the break; while the break is
  active its median walking speed is at least 0.8 x ordered, it moves directly away from the
  mule's CURRENT position (summed step by step) by at least 0.6 x ordered x active seconds,
  and at least 0.7 of its path is away; fewer than 3 active seconds observed is a named
  setup failure.
- **9b** `landed_in_episode`: the attacker's last-attacker record names the subject AND its
  game time lies within the subject's lash-out episode (mental_since to mental_until), the
  break still running. A record from before the episode, a hit after expiry, an attempted
  swing and a missing record all fail. #2975's dressing guard is untouched.
- **9c2**: both fighters neutered with the neuter read back; the hit wait tops both
  fighters' stamina up each second (never 0, 0.9 of max); a dead subject discards the
  cluster and restages on fresh units (bounded by the existing three attempts, naming each
  discard and printing the wounds); committed staging orders, the new-hit proof, subject
  readiness and the observer are unchanged.
- **9h** `chebyshev_ranking_phase`: candidates at (5, 5) and (6, 0) (Chebyshev 5 vs 6,
  squared Euclidean 50 vs 36) inside the range, snapped next to the subject with arrival
  confirmed; graded only when, at the decision, exactly the two candidates are eligible by
  the production predicate and the geometry still separates the metrics; a setup that missed
  that is discarded and restaged (3 attempts), a wrong target on a held setup is a policy
  failure and never restaged.
- **9k**: fighters neutered with the neuter read back; exactly the intended target eligible
  by the production predicate (stray units are a named setup failure); 9h's, 9i's, 9j's and
  9k's own units are destroyed after use.
- **9g**: before 9g stages its subject every unit but the main subject is destroyed and
  the removal CONFIRMED by polling `unit.getAllIds` (a queued destroy proves nothing);
  unconfirmed cleanup, or any eligible unit beside the subject, is a named setup failure.
  9g's rejection of a target and of movement outside the active episode is unchanged.

## Validation

- `python3 tools/mental_state_probe.py --self-test`: all pass. New cases cover every changed
  predicate with positive and negative cases (stationary, post-episode-only movement, invalid
  setup, genuine movement, a slow unit that passes only per its own pace, a mule walking
  toward a fleeing unit, sideways and toward-the-mule runs, stale and post-expiry hits,
  unreadable windows, ungradable geometry, stray and unconfirmed cleanup). Each new threshold
  clause was mutation-checked: removing it fails a case.
- `--lashout-case stale`: exit 0. `--lashout-case ineligible`: MET 3/3, exit 1.

## Before and after (`run_probes --only mental_state --exact`, alone, sequential)

Per-check results come from the probe's own transcript tee
(`MENTAL_STATE_PROBE_TRANSCRIPT`); a check with no line is NOT OBSERVED and never a pass.
Engine: the one binary built from master `97df16466` in the issue worktree for every
batch (the change is probe-only). Local evidence (transcripts, run logs, the probe under
test, replay JSON) is under `~/.local/state/synarchy/evidence-mental-state-2976/`.

### Before: master `97df16466`, probe sha `71053997` (15 runs)

12 PASS, 3 FAIL: 9c2 twice (the subject dead at the break), 4a and 4b together once.

| run | overall | 4a wander | 4b flee | 9b dressing | 9b landed | 9c2 | 9g wander | 9h Chebyshev | 9j end pursuit | 9k technomule | other failing |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 2 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 3 | FAIL | pass | pass | pass | pass | FAIL | pass | pass | pass | pass | - |
| 4 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 5 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 6 | FAIL | pass | pass | pass | pass | FAIL | pass | pass | pass | pass | - |
| 7 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 8 | FAIL | FAIL | FAIL | pass | pass | pass | pass | pass | pass | pass | - |
| 9 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 10 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 11 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 12 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 13 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 14 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 15 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |

runs=15 overall-fail=3
  4a wander: pass=14 FAIL=1 not-observed=0
  4b flee: pass=14 FAIL=1 not-observed=0
  9b dressing: pass=15 FAIL=0 not-observed=0
  9b landed: pass=15 FAIL=0 not-observed=0
  9c2: pass=13 FAIL=2 not-observed=0
  9g wander: pass=15 FAIL=0 not-observed=0
  9h Chebyshev: pass=15 FAIL=0 not-observed=0
  9j end pursuit: pass=15 FAIL=0 not-observed=0
  9k technomule: pass=15 FAIL=0 not-observed=0

### Development batches (failed ones included)

Each is a run of the probe as committed at the head named; none is the acceptance batch.

| batch | head / probe sha | runs | overall | what failed, and what it taught |
|---|---|---|---|---|
| dev1 | `cf5400daa` / `d2b157d5` | 15 | 11 PASS | 4b three times (runs 6, 14, 15): the first rewrite measured distance gain from the LIVE mule, which walks toward the fleeing unit (the unit ran 2.9-4.1 tiles directly away at ordered speed); 9h once: the decoy drifted out of range (dx 8.96), caught by the new guard as ungradable |
| dev2 | `bf46b8452` / `da2a05f04` | 8 (aborted) | 6 PASS | 9h twice, geometry drifted 2-3 tiles inside seconds even at 4.5/5.8 offsets (guard restaged 3 of 3, then named a setup failure); 9g once: 9f's surviving subject entered 9g's range (item 6) |
| dev3 | `e1df148f5` / `47c4b6ab` | 1 (aborted) | 0 PASS | 9g isolation read: the console encodes an empty Lua table as `{}`, so "nothing eligible" read as unreadable |
| dev4 | `0b6805b20` / `7f2df271` | 15 | 14 PASS | 9c2 once: "10 never landed a hit on 8", attacker `falling`/`transitioning` at 35 s; a 90-trial staging harness showed 9 of 45 hits taking 18-25 s because the neutered attacker exhausts its stamina pool and collapses |
| dev5 | `94f40015e` / `d810fc4a` | 3 (aborted) | 2 PASS | 9b landed once: the subject died before it could swing |

### After: head `6222348c1`, probe sha `b3eefdf7` (15 runs)

| run | overall | 4a wander | 4b flee | 9b dressing | 9b landed | 9c2 | 9g wander | 9h Chebyshev | 9j end pursuit | 9k technomule | other failing |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 2 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 3 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 4 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 5 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 6 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 7 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 8 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 9 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 10 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 11 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 12 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 13 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 14 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |
| 15 | PASS | pass | pass | pass | pass | pass | pass | pass | pass | pass | - |

runs=15 overall-fail=0
  4a wander: pass=15 FAIL=0 not-observed=0
  4b flee: pass=15 FAIL=0 not-observed=0
  9b dressing: pass=15 FAIL=0 not-observed=0
  9b landed: pass=15 FAIL=0 not-observed=0
  9c2: pass=15 FAIL=0 not-observed=0
  9g wander: pass=15 FAIL=0 not-observed=0
  9h Chebyshev: pass=15 FAIL=0 not-observed=0
  9j end pursuit: pass=15 FAIL=0 not-observed=0
  9k technomule: pass=15 FAIL=0 not-observed=0

Observed restages in this batch: 9c2 once (run 8: the subject died, brain trauma 1.6, a fresh
cluster passed) and 9j once (run 8, the existing #2975 restage). No other check discarded
a setup.

## Observations outside this issue's scope

- A fixture unit with `strength_base` 0.02 (neuter read back) killed its target with a single
  head strike (jaw fracture 1.07 plus brain trauma 1.6, 'fatal brain trauma' tier) in
  roughly 1 of 50 staged hits. The probe now treats a dead subject as ungradable setup. Whether
  a near-zero-strength attacker should be able to inflict that is a combat damage-model
  question this change does not touch.
- The 4b technomule is not stationary: it moved up to 2.7 tiles during a six-second break.

