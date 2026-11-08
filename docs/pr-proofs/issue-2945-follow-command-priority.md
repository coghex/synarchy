# #2945 evidence: follow_command_priority treat_ally flake

Probe: `tools/follow_command_priority_probe.py` (suite `probe:follow-command-priority`, run through
the quruntul lab's exit-suite route, `quruntul flake --target probe:follow-command-priority --ref <commit> --trials 20`,
`SYNARCHY_PROBE_FAILURE_KEEP=60` so no failing trial's full stdout was pruned). Batch sizes were fixed at 20 and 20
before the first run. Full stdout of every failing trial is in the subdirectories of this write-up's folder
(`docs/pr-proofs/issue-2945-follow-command-priority/`). Lab runs: before `f378cab3-c36c-45a3-9549-84adb41a38bd`,
intermediate `b64d2fbc-7011-497c-8aac-cdee9017813d`, final `2664d89c-164f-4de2-b74d-8067b817a7a5`.
(A first before-batch attempt, `75555fee-...`, was `blocked` at engine preflight: my shell had put cabal 3.18 first
on PATH and bindings-GLFW would not build; no trial ran and it was rerun unchanged on cabal 3.16.)

## Batches

Protocol batches (sizes 20 and 20, fixed in advance):

| Batch | Commit | What the commit contains | Trials | Exit 0 | Exit 1 | Other |
|---|---|---|---|---|---|---|
| Before | `ef2a42682` | requirement 2 evidence capture only; E's move kind, fixture and every assertion unchanged | 20 | 0 | 20 | 0 |
| After | `c1bc96116` (final probe file) | narrowed E, committed-attack staging, attacker placed at order time, E's move kept pending and verified before the first hit, pair isolation on evidence, hand-back counted only while the fight is on | 20 | 20 | 0 | 0 |

Development batches (NOT part of the protocol; each ran at a non-final commit while a defect was found and fixed; all
reported, none hidden; the review of round 1 asked for this to be explicit and an owner exception was requested in
the channel):

| Commit | Trials | Exit 0 | Exit 1 | What it showed |
|---|---|---|---|---|
| `0bdd3b56f` | 20 | 18 | 2 | both failures: check D, no hit landed (attacker chased a walking victim); fixed by placing the attacker at order time |
| `25d9b1f4f` | 20 | 20 | 0 | clean, but measured before the review's findings and before the leftover-patient effect below was exposed (the pair teardown it contained was later removed on review as speculative) |
| `e9da94adc` | 20 | 14 | 6 | teardown removed, staging repaired: every failure had `treat_ally` selected (see below) |

The final PR commit differs from `c1bc96116` only in `tools/ci_probes.py` (the FLAKY reason text); the probe file is
byte-identical (`git diff c1bc96116 HEAD -- tools/follow_command_priority_probe.py` is empty).

## Before batch: per-check tally (20 trials, from each trial's own complete stdout)

| Check | Pass | Graded fail | Not graded |
|---|---|---|---|
| A pickup_ground wins over a pending move | 20 | 0 | 0 |
| B refill_canteen (dry) wins | 20 | 0 | 0 |
| C follow_command beats a routine goal | 20 | 0 | 0 |
| D a struck goal-bound unit reacts with combat | 0 | 20 | 0 |
| E combat reached over a pending move (player move) | 0 | 20 | 0 |
| E combat does NOT hand back to the stale move | 20 | 0 | 0 (vacuous: combat was never entered) |
| F over-capacity commandPickup returns false | 20 | 0 | 0 |
| F refused pickup stores no pickupOrder | 20 | 0 | 0 |
| F refused pickup leaves the move selected | 20 | 0 | 0 |

Per-trial outcome classes: all-passed (exit 0) 0; graded failure (exit 1) 20; setup stop (exit 2) 0; other 0.
Of the 20 graded failures, all 20 are "E setup failure" by requirement 3's definition. The before commit has no in-probe
split, so this class was assigned from the printed qualifying-hit evidence (`hitByStaged:false`, no `lastAttacker`, in
every D and E snapshot of every trial).

## Verdict (requirement 5): the probe is at fault

- D's victim timeline was `search_for_water` in 20/20 trials. E's was `follow_command -> hold_position` (14) or
  `hold_position` (6): the pending PLAYER move arrived and the unit held position, never entering combat.
- In none of 20 trials did the staged attacker land a hit on D's or E's victim (no `lastAttacker`, `incomingHitScores`
  and `attackGoalScores` both false), and no unit anywhere on the arena had a wound (`wounded` empty at every sample).
  `treat_ally` appeared in 0 of 20 trials. So there was no treatable patient and no arbitration to fault.
- Cause: the probe ordered the attack with `commandAttack(atk, victim)` and no `committed` flag. That opens an
  autonomous, non-exempt attack occurrence, which #2521's combat policy ends before any swing for two same-faction
  acolytes (not hostile, no last attacker, no fixture authorization), as the issue review predicted. The old
  `treat_ally` reports (runs A and C, 2026-10-04 and 10-19) are not reproduced by THIS fixture on the evidence commit (no wounded patient
  existed to treat), but were reproduced once the staging repair let hits land and the combat pairs were left on the arena (below).
- E asserted the superseded #306 rule against a player move (#916 scores it 9.0 and lets the player withdraw), a
  second probe-side defect. Narrowed per the owner's 2026-11-07 decision.
- A game defect is not indicated by the before batch (no `treat_ally` at all). Later development batches reproduced
  `treat_ally`; every such trial is a leftover-patient fixture fault (below), E and F alike, with the same cause.

## Development batch `0bdd3b56f` (2 failures)

Trials 3 and 17 (`intermediate-0bdd3b56f/`): check D failed, `D staged attacker landed a qualifying hit: False`, victim
timeline `search_for_water`; every other check passed and E's hit landed. The attacker was alive and standing but ended
the poll window several tiles behind the walking victim. Fixed by placing the attacker beside the victim in the same
console transaction as the committed attack order.

## Development batch `e9da94adc` (6 failures, all `treat_ally`): the original flake, reproduced

Per-check tally over 20 trials: A, B, C, D, F-false, F-no-order pass 20/20; E combat reached pass 18, fail 2; E no
hand-back pass 19, fail 1; F refused-pickup-keeps-move pass 15, fail 5. Full stdout of the 6 failing trials is in
`dev-e9da94adc/`. With D's and E's combat pairs left on the arena (as in the original probe), the same-transaction
snapshots show the cause:

- Trials `c761fb` and `8650c5` (E reached fails): E's subject was already `treat_ally` **before** its attack was
  ordered (`[E pre-attack] ... action=treat_ally`), claiming uid 5, D's attacker, whose wounds were left over from check D
  3.4 tiles away; a treatment claim scores 8.0 over its pending 7.0 move, the documented ladder. The subject then
  collapsed (and in `c761fb` died) after one hit, so the AI was suspended on a stale `treat_ally`.
- `62730d`, `faaab3`, `296d6c`, `e8070c` (F keeps-move fails, 5 in all): F's subject claimed a wounded D/E survivor
  1.3-7 tiles away (`treatClaim` 5/6/7, phases D, D-attacker, E, E-attacker) and treated it over the 7.0 move.
- Trial `faaab3` also failed E's no-hand-back: the staged attacker died (`attackerPose: dead`) mid-tail, the fight was
  over, and the pending move resumed. A sample after the opponent has died is not "while the fight is on".

Verdict for this batch: probe-side. A medic with a wounded ally in reach treating it at 8.0 over a 7.0 move is the
documented ladder, not a defect; the probe left patients in reach that the check did not intend. Eligible combat was
never out-scored: where combat was eligible and the unit was alive, it won.

## Final batch (`c1bc96116`)

20/20 exit 0 (lab run `20674d0a-c24b-472f-940c-75e7ba1667b2`): every check PASS and no E setup failure (exit 3) is the
only path to exit 0, and a passing run prints nothing through `run_probes`. Smoke runs printed the evidence lines; for
example D `search_for_water -> attack_target` with `incomingHitBasis: self_defense`, E `follow_command -> engage ->
attack_target` with `movePending: true, movePlayer: false` at the order.

Reading: the probe went from 0/20 to 20/20 clean. This is one machine; the CI runner's slower, variable timing is not
measured, so the probe stays FLAKY and manual-only and no reclassification is made here.
