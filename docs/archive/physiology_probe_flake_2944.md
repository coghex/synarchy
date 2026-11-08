# physiology_probe flake: diagnosis and verdict (#2944)

Verdict: **the probe is at fault, not the product.** `temperate (22C/0.5): circ min >= 0.75`
is a threshold at the floor of the population the probe itself rolls. The circulation rule
behaves as `docs/engine_contracts.md` §Cardio and circulation specifies. The squirrel salt
failure of run B no longer exists. Changing the probe is owner-gated (#2761, homeostasis
rule), so this document recommends and changes nothing in the population, thresholds,
stimuli or checks.

## Method

- Revision `a1ce85bb6` (master `19fe2bd86` plus observation-only per-unit diagnostics in
  `tools/physiology_probe.py`: one extra Lua read in the same console call as the existing
  aggregate; no stimulus, population, threshold, check, RNG or timing change).
- Planned **12** complete default runs (`python3 tools/physiology_probe.py --port 9171`,
  `--seconds 30 --count 5`, all sections in their shipped order), sequential on one
  machine. 12 started, **12 completed and evaluable**, 0 interrupted, 0 missing.
- Raw logs, including every `[DIAG]` line (per-unit id, pose, six system modes, every
  effective numeric stat, circulation and its inputs, unrounded) and the per-run engine
  logs: `~/.local/state/synarchy/issue-2944/baseline/run-NN.log`,
  `run-NN.engine.log`, `diag_all.json`, `revision.txt`. These were not produced through
  `run_probes.py`, so nothing is under `probe-failures/`; the directory above is the
  retained evidence.
- No post-fix batch exists because no product change was made.
- Historical runs A, B and C of the issue are not reproducible (their revisions and
  logs are gone); they are cited only as the issue records them.

## Per-check failure rates (baseline, n = 12 evaluable runs)

| Check | Failed | Value when failed |
|---|---|---|
| `temperate (22C/0.5)` (incl. circ min) | 0/12 | lowest run minimum 0.7500 (run 01) |
| `small creature temperate` (incl. salt 0.7-1.3) | 0/12 | squirrel salt_conc exactly 1.0 in 12/12 |
| `squirrel simple profile (salt Nominal)` | 0/12 | |
| `humid heat (46C/0.95)` rise > 0.1 C | 1/12 | run 01: +0.0955 C (others 0.1045 to 0.1711) |
| `fatigued unit sprints slower` | 1/12 | run 05: rested 1.06 vs fatigued 0.96 tiles/s |
| `calorie store drains faster in combat` | 1/12 | run 06: idle -22.3 vs combat -28.5 kcal |
| every other check (23 of 26) | 0/12 | |

Whole-probe result: 9 of 12 runs green, 3 red, each red for one of the three non-temperate
checks above. Both temperate checks of the issue did not fail in this batch.

## Why circ min sits on the edge

For a healthy rolled acolyte (blood fraction 1.0, heart rate 70, core 37, sepsis 0) the
rule in `src/Unit/Physiology/Circulation.hs` reduces to

    circulation = (0.5 + 0.5 * constitution) * (1 - 0.7 * max(0, fat_mass/body_mass - 0.20))

The recorded inputs reproduce the engine's value for all 60 temperate acolytes to within
5.5e-8 (float rounding). `constitution` is rolled `N(1, 0.25)` clamped to `[0.5, 1.5]`
(`data/units/acolyte.yaml`, `Unit.Stats.rollStat`). So:

- A unit at the constitution floor (0.5) with fat fraction <= 0.20 reads **exactly 0.75**:
  the check passes on float equality (`< 0.75`). Run 01, unit 27: constitution 0.5000,
  fat 3.5/103.9 kg, circulation 0.75.
- Any unit whose fitness product drops under 0.75 (low constitution combined with a fat
  fraction above 0.20) fails the check; the lowest possible rolled value is `0.75 * (1 - 0.7 * 0.18) = 0.656`
  (constitution floor, fat fraction 0.38). Those are the contract's deliberate effects
  (constitution, body composition), not a defect.
- Monte Carlo of that formula with the measured fat fractions (60 units: mean 0.197, sd
  0.082) and the rolled constitution: P(unit < 0.75) = 2.3%, i.e. **about 11% of
  five-acolyte runs** fail, plus a further 1.2% of units sitting exactly on 0.75. A
  bodyfat `N(0.2, 0.09)` model gives 2.6% per unit, 12% per run. 0/12 observed is
  expected 21% of the time at that rate; with the issue's earlier 2 failures in 3 runs the
  pooled 2/15 matches the model.
- The lowest values in this batch: 0.7500 (c 0.500, fat 0.034), 0.7631 (c 0.722, fat
  0.363), 0.7736 (c 0.547, fat 0.143), 0.7922 (c 0.584, fat 0.197), 0.7931 (c 0.706,
  fat 0.300). Run 12's 0.7631 is a constitution-0.72 unit with 36% body fat.

The historical failures (0.70, 0.73, 0.74) are the same mechanism: a low-constitution or
high-fat acolyte, not a bug.

## Squirrel salt

Run B's `small creature temperate: salt 0.69-1.00` predates #2833 (51f27738f), which
changed red_squirrel to the `simple` profile. All 12 runs read the three squirrels with
salt mode Nominal, salt_conc exactly 1.0, thermo Simulated (core 36.4 to 37.5 C). The
failure cannot occur on the shipped profile; this is confirmed, not assumed, and nothing
about simulated salt was restored.

## Recommendation (needs the owner; goes to a follow-up issue, not this PR)

The probe's temperate circulation check should stop depending on an unseeded roll of a
constitution/fat population. Options, in order of preference:

1. **Assert the contract instead of an edge**: for each acolyte, compare the engine's
   circulation with `circulationRule`'s value for that unit's own recorded inputs
   (constitution, fat, body mass, core, blood fraction) within a small tolerance, and keep
   an absolute floor at the rule's guaranteed minimum (0.65) for the intended population.
   This checks the rule and the plumbing without failing on natural variation.
2. **Lower the floor to 0.65** (the minimum the rolled population can reach with this
   rule) and keep everything else.
3. **Control the variable**: spawn the temperate batch with fixed constitution and body
   fat (or the test-only standard profile, noting `spawn_batch` calls `unit.spawn`
   directly, so `setTestStandardProfile` alone would not reach it).

Options 1 to 3 change the tested population or a threshold and are therefore owner
decisions. The same edge-of-variation shape applies to `humid heat` (rise threshold 0.1 C
against measured 0.0955 to 0.1711 C), which the owner may want in the same follow-up.
`fatigued unit sprints slower` and `calorie store drains faster in combat` flaked once each
at 1/12 and are not part of #2944's two named checks; their values are above.
