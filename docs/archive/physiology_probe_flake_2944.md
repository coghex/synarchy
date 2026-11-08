# physiology_probe flake: diagnosis and verdict (#2944)

Verdict: **the probe is at fault, not the product.** `temperate (22C/0.5): circ min >= 0.75`
is a threshold at the floor of the population the probe itself rolls. The circulation rule
behaves as `docs/engine_contracts.md` §Cardio and circulation specifies, and the one
reproduced failure is explained to 6 decimals by that rule. The squirrel salt failure of
run B no longer exists. Changing the probe is owner-gated (#2761, homeostasis rule), so
this document recommends and changes nothing in the population, thresholds, stimuli or
checks.

## Method

Two separately identified batches. Neither changes the probe's population, stimuli,
thresholds, checks, RNG or timing; the only probe change is observation-only per-unit
diagnostics (`[DIAG]` records: unit id, species, six system modes, every effective numeric
stat, circulation and its inputs, unrounded, taken in the same console call as the
assertion aggregate; baseline and final for the five acolytes of every scenario and for
the three squirrels).

- **Batch R (primary), via quruntul's legacy-probe route.** Revision `cc8f031b0`.
  Planned **12**; started 12; completed and evaluable **12**; interrupted 0; missing 0.
  Each attempt: `python3 tools/run_probes.py --only physiology --exact --jobs 1
  --port 9171 --tail 100000` (the command `.quruntul/adapter.py` runs), with
  `SYNARCHY_PHYSIOLOGY_DIAG_FILE` set per attempt so passing attempts keep their per-unit
  records too, and `SYNARCHY_PROBE_FAILURE_ROOT` set so the five failing attempts' evidence
  is not rotated away. Retained under `~/.local/state/synarchy/issue-2944/viarunner/`:
  `run-NN.log` (runner output), `run-NN.diag.jsonl` (every `[DIAG]` record),
  `failures/physiology/<stamp>-cc8f031b0-*/` (kept attempt directories), `revision.txt`.
- **Batch D (supplementary), direct invocation.** Revision `a1ce85bb6` (acolyte
  diagnostics only; no squirrel baselines). Planned 12; completed and evaluable 12.
  `python3 tools/physiology_probe.py --port 9171` per attempt, outside the runner (so no
  `probe-failures/` directory). Retained under
  `~/.local/state/synarchy/issue-2944/baseline/` (`run-NN.log`, `run-NN.engine.log`,
  `diag_all.json`).
- No post-fix batch exists because no product change was made.
- The issue's historical runs A, B and C are not reproducible (their revisions and logs
  are gone); they are cited only as the issue records them.

## Per-check failure rates

| Check | Batch R (12) | Batch D (12) | Pooled (24) |
|---|---|---|---|
| `temperate (22C/0.5)` (incl. circ min) | **1** | 0 | 1/24 |
| `small creature temperate` (incl. salt 0.7-1.3) | 0 | 0 | 0/24 |
| `squirrel simple profile (salt Nominal)` | 0 | 0 | 0/24 |
| `humid heat (46C/0.95)` rise > 0.1 C | 2 | 1 | 3/24 |
| `fatigued unit sprints slower` | 2 | 1 | 3/24 |
| `calorie store drains faster in combat` | 0 | 1 | 1/24 |
| every other check | 0 | 0 | 0/24 |

Whole-probe result: batch R 7 green, 5 red (runs 01, 02 humid heat; 04 temperate circ;
06, 07 fatigued sprint); batch D 9 green, 3 red. Pooled 24 runs: 16 green, 8 red; every
red run failed exactly one check.

Batch R failing values: run 01 humid heat +0.0918 C; run 02 +0.0860 C; run 04 circ min
0.7411; run 06 fatigued sprint rested 0.80 vs fatigued 0.87 tiles/s; run 07 rested 0.27
vs fatigued 0.64 tiles/s. Batch D: humid heat +0.0955 C (run 01), fatigued sprint 1.06 vs
0.96 (run 05), calorie drain idle -22.3 vs combat -28.5 kcal (run 06). The humid-heat
rise over the 24 runs ranges 0.086 to 0.184 C against a `> 0.1` threshold.

## The reproduced circulation failure (batch R run 04)

Per-unit record behind `circ min 0.74` (all five acolytes, sepsis 0, blood fraction
1.000000, core 37.0 C, cardio Simulated):

| Unit | constitution | fat / body (kg) | fat fraction | circulation |
|---|---|---|---|---|
| 27 | 1.3181 | 19.50 / 73.53 | 0.265 | 1.00000 |
| 28 | 0.7943 | 4.53 / 55.33 | 0.082 | 0.89715 |
| 29 | 0.7814 | 20.60 / 79.34 | 0.260 | 0.85357 |
| 30 | 1.2293 | 21.09 / 74.60 | 0.283 | 1.00000 |
| **31** | **0.6807** | 29.63 / 80.37 | **0.369** | **0.74111** |

Unit 31 is the failing unit: heart rate 70.17 (no factor), so
`(0.5 + 0.5 * 0.6807) * (1 - 0.7 * (0.3687 - 0.20)) = 0.74111` against the engine's
0.74111 (both 0.741107). A below-average constitution combined with high body fat is
exactly the effect the circulation rule is specified to have. Fault: the probe's
expectation, not the rule.

## Why circ min sits on the edge

For a healthy rolled acolyte (blood fraction 1.0, heart rate 70, core 37, sepsis 0) the
rule in `src/Unit/Physiology/Circulation.hs` reduces to

    circulation = (0.5 + 0.5 * constitution) * (1 - 0.7 * max(0, fat_mass/body_mass - 0.20))

All 120 temperate acolytes of both batches reproduce the engine's value with this formula
(batch D: max error 5.5e-8). `constitution` rolls `N(1, 0.25)` clamped to `[0.5, 1.5]`
(`data/units/acolyte.yaml`, `Unit.Stats.rollStat`). So:

- A unit at the constitution floor (0.5) with fat fraction <= 0.20 reads **exactly 0.75**:
  the check passes on float equality (`< 0.75`). Batch D run 01, unit 27: constitution
  0.5000, fat 3.5/103.9 kg, circulation 0.75.
- A unit whose fitness product drops under 0.75 (low constitution combined with a fat
  fraction above 0.20) fails; the lowest possible rolled value is
  `0.75 * (1 - 0.7 * 0.18) = 0.656` (constitution floor, fat fraction 0.38). These are the
  contract's deliberate effects, not a defect.
- Monte Carlo of that formula with the measured fat fractions (mean 0.197, sd 0.082) and
  the rolled constitution: P(unit < 0.75) = 2.3%, i.e. **about 11% of five-acolyte runs**
  fail, plus a further 1.2% of units sitting exactly on 0.75. A bodyfat `N(0.2, 0.09)`
  model gives 2.6% per unit, 12% per run. Observed: 1/24 runs (4%) and 1/120 units
  (0.8%) below 0.75, within chance of the model (P(at most 1 failing run in 24) about
  21%); with the issue's 2 earlier failures in 3 runs, the pooled 3/27 matches it.
- Lowest values: batch R 0.7411 (c 0.681, fat 0.369), 0.7520 (c 0.721, fat 0.380), 0.7807
  (c 0.759, fat 0.361); batch D 0.7500 (c 0.500, fat 0.034), 0.7631 (c 0.722, fat 0.363).

The historical failures (0.70, 0.73, 0.74) are the same mechanism.

## Squirrel salt

Run B's `small creature temperate: salt 0.69-1.00` predates #2833 (51f27738f), which
changed red_squirrel to the `simple` profile. In batch R all three squirrels, at baseline
and at the final snapshot, read salt mode Nominal and salt_conc exactly 1.0 (12/12 runs);
batch D's final snapshots agree (12/12), thermo Simulated (core 36.4 to 37.5 C). The
failure cannot occur on the shipped profile; this is confirmed, not assumed, and nothing
about simulated salt was restored.

## Recommendation (needs the owner; goes to a follow-up issue, not this PR)

The probe's temperate circulation check should stop depending on an unseeded roll of a
constitution/fat population. Options, in order of preference:

1. **Assert the contract instead of an edge**: for each acolyte, compare the engine's
   circulation with `circulationRule`'s value for that unit's own recorded inputs
   (constitution, fat, body mass, core, blood fraction) within a small tolerance, and keep
   an absolute floor at the rule's guaranteed minimum (0.65) for the intended population.
2. **Lower the floor to 0.65** (the minimum the rolled population can reach with this
   rule) and keep everything else.
3. **Control the variable**: spawn the temperate batch with fixed constitution and body
   fat (or the test-only standard profile, noting `spawn_batch` calls `unit.spawn`
   directly, so `setTestStandardProfile` alone would not reach it).

Options 1 to 3 change the tested population or a threshold and are therefore owner
decisions. The same edge-of-variation shape applies to `humid heat` (rise threshold 0.1 C
against measured 0.086 to 0.184 C, 3/24 failing), which the owner may want in the same
follow-up. `fatigued unit sprints slower` (3/24) and `calorie store drains faster in
combat` (1/24) are not part of #2944's two named checks; their values are above.
