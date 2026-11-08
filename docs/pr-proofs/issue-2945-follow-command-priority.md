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

| Batch | Commit | What the commit contains | Trials | Exit 0 | Exit 1 | Other |
|---|---|---|---|---|---|---|
| Before | `ef2a42682` | requirement 2 evidence capture only; E's move kind, fixture and every assertion unchanged | 20 | 0 | 20 | 0 |
| Intermediate | `0bdd3b56f` | + narrowed E, committed-attack staging, E setup/arbitration split, pair teardown | 20 | 18 | 2 | 0 |
| After (final) | `25d9b1f4f` | + attacker placed beside the victim at order time, E's move kept pending | 20 | 20 | 0 | 0 |

The final PR commit differs from `25d9b1f4f` only in `tools/ci_probes.py` (the FLAKY reason text); the probe file is
byte-identical (`git diff 25d9b1f4f HEAD -- tools/follow_command_priority_probe.py` is empty). The intermediate batch is
reported because the 2 failures it found were a real second staging defect that the first fix did not cover; the final
batch is the one the issue calls "after".

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
  `treat_ally` reports (runs A and C, 2026-10-04 and 10-19) predate #2521 and are not reproduced on current master.
- E asserted the superseded #306 rule against a player move (#916 scores it 9.0 and lets the player withdraw), a
  second probe-side defect. Narrowed per the owner's 2026-11-07 decision.
- A game defect is not indicated: no `treat_ally` over eligible combat or a non-player move was ever observed. No
  speculative game change was made. E and F did not fail with `treat_ally` in any measured trial, so no separate cause
  is claimed for them.

## Intermediate batch (`0bdd3b56f`), the 2 failures

Trials 3 and 17 (`intermediate-0bdd3b56f/20261108T023218Z.stdout.txt`, `...T024816Z.stdout.txt`): check D failed, `D staged
attacker landed a qualifying hit: False`, victim timeline `search_for_water`; every other check passed and E's hit landed
(`True`). The attacker was alive, standing and chasing but ended the poll window several tiles behind the walking victim.
Fix: place the attacker beside the victim in the same console transaction as the committed attack order
(`stage_committed_attack`), spawn E's subject nine tiles from its move target (it was two, and a smoke run showed the
move finishing before the attack) and report a missing pending move as an E setup failure.

## Final batch (`25d9b1f4f`)

20/20 exit 0. A passing run prints nothing through `run_probes`, so a clean trial is evidenced by its exit status: every
check PASS and no E setup failure (exit 3) is the only path to exit 0. Smoke runs printed the evidence lines; for
example D `search_for_water -> attack_target` with `incomingHitBasis: self_defense`, E `follow_command -> engage ->
attack_target` with `movePending: true, movePlayer: false` at the order.

Reading: the probe went from 0/20 to 20/20 clean. This is one machine; the CI runner's slower, variable timing is not
measured, so the probe stays FLAKY and manual-only and no reclassification is made here.
