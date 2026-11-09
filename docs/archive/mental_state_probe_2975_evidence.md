# mental_state probe: three residual timing flakes (#2975)

Follow-up to `docs/archive/mental_state_probe_2973_evidence.md` §Residual flakes.
Scope (owner ruling 2026-11-08, "Keep narrow, split rest"): the 9b dressing guard,
the 9g final agitated wander and the 9j episode-end pursuit. Phase 4a/4b and the 9c2
staged-hit setup are #2976 and are reported here, not fixed.

## Cause of each flake: probe timing assumption, no gameplay defect

All three were established by instrumented runs against the engine (20-40 trials each,
own engine and port, scratch scripts not kept in the repository) in addition to the
kept failure evidence, which carries no movement or combat detail beyond the engine log.

### 9b dressing guard — legitimate retaliation advanced the record

`dress_staged_wound` read the subject's attacker record, dressed the wound, read it
again and required the two to be equal. The subject is mid lash-out episode, so its
swing lands on the staged attacker, which then retaliates through self-defense after a
fresh hit (`engage: 4 engages 3`); `Combat.Resolution` stamps the record
(`uiLastAttackerAt`) on every landed hit. A hit landing between the two reads advances
the record for a reason that has nothing to do with the treatment. In all 12 unexcluded
traces (8 s after the dressing) the record advanced, and in 14 trials with the new
exclusion a retaliation strike was dropped in 8 of them while the record stayed
unchanged in all 14.

Evidence that a hit cannot be proven after the fact, which is why the guard excludes
rather than explains: a neutered fighter's wound is below
`woundCleanupThreshold` and is gone within a second, so the wound list does not show
the hit; and the `combat.drainEvents()` hit stream is also drained by `combat_log.lua`
(loaded by `scripts/init_loader.lua`, 0.1 s tick), so a probe-side read misses events
(records advanced with no event in every trial inspected).

Fix: for the dressing window only, `combat.attack` onto the subject from any other unit
is dropped (every production strike, including the lunge, goes through that global; the
combat thread resolves a queued command within a 60 Hz tick, and the install waits 0.3 s
before the first read). The record must then be unchanged, so a treatment-induced change
is still a failure. The subject's own swings are untouched, so 9b's real landed-attack
check is unaffected. The stale demonstration's separate dressing chunk
(`stale_dress_chunk`, `stale_dressing_problems`) is unchanged.

### 9g final wander — net displacement of a slow random walk

The check required more than 0.8 tiles of net displacement within 15 s of forcing a
break. Acolytes meander at 0.15-0.34 tiles/s (`movement_speed.meander`); a wander leg is
uniform on [0, 3) tiles in a random direction; the episode lasts six game seconds and the
poll also counted movement after the episode had ended (ordinary AI). Over 20 trials the
unit walked in every one but one never exceeded 0.8 tiles of net displacement in 22 s, and
most trials crossed it only after four to six seconds.

Fix: wait on observed walking while the lash-out episode is active (`mentalLashoutActive`
set, `isBreaking`, no attack target), summing path length over consecutive active samples
(at least 0.3 tiles). Movement outside the active episode never counts; the failure path
is bounded (episode ended, or a 45 s wall-clock hang ceiling).

### 9j episode-end pursuit — baseline taken before the delirium and the boundary

`closed_gap < 2.0` was measured from a position read before delirium began, over a
window that ended 1.5 s after the episode end. It therefore charged the unit for
pursuit it was entitled to run until the episode ended: over 20 trials that pre-end
travel was 1.0-1.5 tiles, and the old measure read 0.53-1.71 (the kept failure read
2.01). After the observed cleanup tick (0.3 s grace) the unit never exceeded its meander speed in
60 of 60 traced trials, while the live pursuit ran at least 3.8 times the meander (0.98-1.43
tiles/s against a meander of 0.15-0.34), i.e. the in-flight leg is cancelled and delirium's own wander follows.

Fix: grade from the observed boundary, the first sample on which `mentalLashoutActive`
is clear after it was set. Required: on the sample just before the boundary the unit is
pursuing the target at more than twice the meander speed under an active episode with
delirium active, and delirium also on the boundary sample; no target, attack goal or
committed order from the boundary on; and, judging every interval from the boundary
sample on (so a leg that finishes between two samples is seen), no movement toward the old
target beyond the pursuit pace for the 0.3 s grace plus 1.5x the meander afterwards, and no
closing on it beyond what a meander explains over 1.5 s. Fast movement that is not toward
the old target is not a leaked leg. Pre-end travel is never counted. A pair whose pursuit
had already ended when the episode was ended (the unit arrived and fell in one run) is not
gradable: 9j stages at 7 tiles, reads the gaits and target position before the break,
holds delirium from the sample chunk itself, and restages up to three times on a
`no_pursuit`, `no_overlap` or `setup` verdict, naming each discarded attempt; a real
failure never restages.

## Validation

- `python3 tools/mental_state_probe.py --self-test`: all pass. New cases: the dressing
  guard (unchanged passes; a time, uid, vanished, appeared, backwards or unreadable record
  fails) and the exclusion chunk run against a stub `combat.attack` under `lua` (drops
  strikes onto the subject, passes the subject's own swing, other targets and extra
  arguments, restore, idempotent install); 9g (walking while active passes; walking out
  and back passes; stationary until the episode ended, movement only after it ended, a
  non-lash break, a target and unreadable positions fail); 9j (clean stop and delirious
  wander pass; big pre-end travel is not charged; a leg still running past the grace, a
  closing creep, no live pursuit, no delirium overlap and uncleared combat state fail;
  not-yet-covered windows wait).
- `--lashout-case stale`: exit 0, stale discard then a valid passing restage.
  `--lashout-case ineligible`: MET 3/3, exit 1 (2 of 3 runs; the first run hit the
  known hit-staging flake in attempt 1 and its fixture then aborted, #2976 class).

## Before and after (`run_probes --only mental_state --exact`, alone, sequential)

Probe versions are the sha256 prefix of `tools/mental_state_probe.py`. Each batch is
`python3 tools/run_probes.py --only mental_state --exact` run alone, one at a time, from the
issue worktree; `MENTAL_STATE_PROBE_TRANSCRIPT=<file>` makes the probe tee its own output,
because `run_probes` prints nothing for a passing probe. A check with no matching line is
"NOT OBSERVED" and never counts as a pass. Local evidence (logs, transcripts, kept failure
directories, the diagnostics' JSON) is under `~/.local/state/synarchy/evidence-mental-state-2975/`.

### Before: master `0af4acfde` (15 runs, `run_probes`, overall outcome only)

11 PASS, 4 FAIL. Run 2: 9b dressing guard, 4a wander, 4b flee. Run 4: 9c2 staged-hit setup.
Run 7: 9b dressing guard. Run 13: 4a wander, 4b flee. A passing run's per-check lines were not
captured in this batch (see the next one).

### Before: master + transcript hook only (15 runs, per-check outcomes)

| run | overall | 9b dressing guard | 9g final wander | 9j episode-end pursuit | other failing checks |
|---|---|---|---|---|---|
| 1 | PASS | pass | pass | pass | - |
| 2 | PASS | pass | pass | pass | - |
| 3 | FAIL | pass | pass | pass | [FAIL] wander break didn't move: state={'since': 30.085474014282, 'stateOfMind': 0.9998354 |
| 4 | FAIL | pass | NOT OBSERVED | NOT OBSERVED | [FAIL] setup: 10 never landed a hit on 8 — can't test the retaliation-swap exclusion (atte |
| 5 | PASS | pass | pass | pass | - |
| 6 | PASS | pass | pass | pass | - |
| 7 | PASS | pass | pass | pass | - |
| 8 | PASS | pass | pass | pass | - |
| 9 | PASS | pass | pass | pass | - |
| 10 | FAIL | pass | pass | pass | [FAIL] wander break didn't move: state={'since': 29.905460357666, 'stateOfMind': 0.9997789 |
| 11 | FAIL | pass | pass | pass | [FAIL] wander break didn't move: state={'state': 'break', 'since': 30.027490615845, 'state |
| 12 | PASS | pass | pass | pass | - |
| 13 | PASS | pass | pass | pass | - |
| 14 | PASS | pass | pass | pass | - |
| 15 | PASS | pass | pass | pass | - |

runs=15 overall-fail=4 9b dressing guard: not-pass=0 9g final wander: not-pass=1 9j episode-end pursuit: not-pass=1

The three targeted checks passed wherever they were observed in this batch (9g and 9j were
not observed in run 4, which stopped at the 9c2 setup). Across the two before batches the dressing
guard failed in 2 of 30 runs; 4a/4b and 9c2 failed in 5 of 30 and 2 of 30.

### After: final probe (sha `f6482e75`, 15 runs then 2 extra)

Run 15 stopped at the out-of-scope 9c2 setup before 9g and 9j, and the first extra run
reached them, so there are 15 fully observed runs among the 17.

| run | overall | 9b dressing guard | 9g final wander | 9j episode-end pursuit | other failing checks |
|---|---|---|---|---|---|
| 1 | PASS | pass | pass | pass | - |
| 2 | PASS | pass | pass | pass | - |
| 3 | PASS | pass | pass | pass | - |
| 4 | PASS | pass | pass | pass | - |
| 5 | PASS | pass | pass | pass | - |
| 6 | PASS | pass | pass | pass | - |
| 7 | PASS | pass | pass | pass | - |
| 8 | FAIL | pass | pass | pass | [FAIL] expected only 30, saw: ['26', '26', '26', '26', '26', '26', '26']; [FAIL] setup: 29 never targeted 30 |
| 9 | FAIL | pass | pass | pass | [FAIL] lash-out never landed an attack on 7 |
| 10 | PASS | pass | pass | pass | - |
| 11 | PASS | pass | pass | pass | - |
| 12 | FAIL | pass | pass | pass | [FAIL] expected Chebyshev-nearer 26, got 27 (Euclidean-nearer would be 27) |
| 13 | PASS | pass | pass | pass | - |
| 14 | PASS | pass | pass | pass | - |
| 15 | FAIL | pass | NOT OBSERVED | NOT OBSERVED | [FAIL] setup: 10 never landed a hit on 8 — can't test the retaliation-swap exclusion (atte; [FAIL] wander break didn't move: state={'state': 'break', 'endsAt': 36.150032043457, 'stat |

runs=15 overall-fail=4 9b dressing guard: not-pass=0 9g final wander: not-pass=1 9j episode-end pursuit: not-pass=1

| run | overall | 9b dressing guard | 9g final wander | 9j episode-end pursuit | other failing checks |
|---|---|---|---|---|---|
| 1 | FAIL | pass | pass | pass | [FAIL] lash-out never landed an attack on 4 |
| 2 | PASS | pass | pass | pass | - |

runs=2 overall-fail=1 9b dressing guard: not-pass=0 9g final wander: not-pass=0 9j episode-end pursuit: not-pass=0

Result on the final probe: 17 runs; 9b, 9g and 9j were each observed passing in 16 and
never failed (run 15 did not reach 9g/9j). The overall probe still exits nonzero when an
excluded check fails, as before. Failures at other checks, reported and not touched (#2976):
the 9c2 staged-hit setup (run 15), 9k `expected only 30, saw 26` (run 8; unit 26 appears to be
9i's target, teleported to 6 tiles from the 9k subject and left alive), 9b's separate
landed-attack check `lash-out never landed an attack on 7`/`4` (run 9 and extra run 1; the
dressing guard had excluded 0 strikes in both), 9h `expected Chebyshev-nearer 26, got 27`
(run 12) and the 4a wander break (run 15). The overall failure rate, 5 of 17, matches the
before batches (8 of 30).

### History of the fix (every targeted-check failure seen while building it)

- First 9b guard (explain the changed record by a new wound at its time): after-run 4
  failed it. Wounds of a neutered fighter vanish within a second, so the explanation was
  unreliable; replaced by the exclusion above (14 of 14 in-engine trials unchanged).
- First 9j verdict (wall-clock cap 15 s): one run reported `no_boundary — window not
  covered` with 24 samples in 15 s; the same run's engine log carries `Audio output underruns`
  for the whole stage and the run took 193 s against 155-175 s. Game time had fallen behind the
  wall clock, so the 15 s hang ceiling was raised to 60 s (9g: 45 s); a normal run leaves the
  loop within seconds. 100 further traced trials never delayed the cleanup boundary.
- 9j leak test on pace alone (1.5x meander): one run read a second leg at 0.52 tiles/s
  against a 0.23 meander 1.3 s after the cleanup (the cleanup itself had stopped the pursuit,
  speed 1.1 to 0.23). The leg's origin is not established (no instance in 100 traced trials),
  so the leak test now also requires the movement to be toward the old target, which is what
  a leaked pursuit is; a leg at a tired pace toward the target still fails (self-test). The
  trace now prints pose, activity, stamina, live meander and position for any 9j failure.
  None of the 17 runs on the final probe failed 9j.
- Review round 1 (two valid 9j gaps, both reproduced): pairing only post-grace samples
  hid a leg that finished between two samples, and the live pursuit and the delirium were
  accepted from any earlier samples. Both fixed with mutation-checked regression cases (the
  previous verdict returned `ok` for both traces). The stricter precondition then failed one
  run (`no_pursuit`): at 6 tiles the unit had arrived and fallen (`pose=falling`) before the
  episode was ended, so nothing was in flight. That is the staging race the restaging above
  closes; the final probe's 17 runs carry one restage (extra attempt printed
  `9j attempt 1 discarded ... restaging`) and no 9j failure.
