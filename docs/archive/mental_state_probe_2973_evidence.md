# mental_state probe: staged hits stopped landing (#2973)

## Cause (bisected)

- `53331a66c` (parent of #2521's FTS-5 commit): `run_probes --only mental_state --exact` passes (168 s).
- `15f300475` (#2521, "Port alliance, hostility, combat and AI consumers to the directed relation authority"): fails with the issue's exact signature (staged attacks never land; lash-out preference ungradable).
- `455fa82a9` (master at solve time): same failure, identical lines.

It is a **stale probe staging assumption**, not a gameplay regression. Every acolyte spawns tag-less, so the staged attacker and the lash-out subject are uncontrolled allies. Since FTS-5, an autonomous attack on an ally with no hostility or fresh self-defense ends at its first decision (`scripts/unit_ai_combat_attack.lua`, `policy.mayContinue`), before any swing. `retaliation_swap_probe.py` was corrected the same way in `5d24ee8da` (#2521); `mental_state_probe.py` was missed. The fix issues the two staging `commandAttack` orders as committed, the player stand-in, and leaves the policy untouched.

## Consequences found while verifying

1. `--self-test`'s Lua swap harness ran the real `unit_ai_combat_policy` with no lash-out occurrence, so the governed-goal rule ended the goal and the harness read no result (`KeyError: 'post'`, on master too). The harness now opens the exempt `lash_out` occurrence the way production does.
2. 9c2's "a NEW hit" test compared against a hit time that had passed through JSON at 14 significant digits, so the same hit could read as new on a re-stage (hit age 16 s, outside the 3 s swap window). A hit now has to be newer by `NEW_HIT_MARGIN_SEC`. Self-test cases added.
3. A committed staging order leaves `s.committed = true` after the probe's stand-down; production end-of-attack paths clear it. The stand-down now clears it.

## Validation

- `python3 tools/mental_state_probe.py --self-test`: all pass.
- `--lashout-case stale`: exit 0, stale discard then a valid passing restage. `--lashout-case ineligible`: MET 3/3, exit 1 as designed.
- `run_probes --only mental_state --exact`, run alone, on the final code: four consecutive passes (runs 5-8 of an 8-run sample) and, in an earlier sample, four consecutive passes before a fifth failed at the episode-end pursuit check below.

## Residual flakes (not the staging)

On the final code about 3 runs in 13 failed, always at a check unrelated to hit staging:

- `dressing 3's staged wound changed its attacker record`: lash-out's swing lands on the attacker, which then retaliates legitimately (`engage: 4 engages 3`, self-defense after a fresh hit), so the subject's attacker record advances before the dressing guard reads it. This race predates the gate. Evidence: `~/.local/state/synarchy/evidence-mental-state-2973/20261108T225048Z-f5ddc1b46-11d0dc/`.
- `expected wander w/ no target: moved=False` (final phase). Evidence: `.../20261108T224214Z-f5ddc1b46-0bed3e/`.
- `lash-out state or pursuit leaked while delirium overlapped episode end ... closed_gap=2.01`: a distance threshold of 2.0 tiles measured over a timing-dependent window. Evidence: `.../20261108T223816Z-455fa82a9-e7ea0b/`.

Pre-fix baseline and bisect logs are in the same directory (`run0.log`, `bisect_pre2.log`, `bisect_gate.log`).
