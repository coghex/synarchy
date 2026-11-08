# physiology_probe: per-unit expected values (#2960) - evidence

Issue #2960, PR #2968. Follows #2944 (`docs/archive/physiology_probe_flake_2944.md`), which found the probe, not the product, at fault for two edge-of-variation checks. Owner ruling (2026-11-08): "Per-unit expected values" with an absolute floor of 0.65, and "Pure engine read" for the heat balance.

## What changed

- **Circulation (temperate).** Each of the five acolytes' `unit.readPhysio(u,'circulation')` must equal the circulation rule evaluated on that unit's own recorded inputs (blood fraction, constitution, fat/body mass, sepsis, core, heart rate under a Simulated cardio) within `1e-4`, and be at least `0.65`. The expectation is a Python port of `circulationRule` (`tools/physiology_expectations.py`), held to the Haskell rule by a shared 29-case fixture (`tools/fixtures/physiology_circulation_parity.json`, checked by `PhysiologyCardioCirculationModes.Rule` and by the Python self-test) and to 600 real engine readings from the 24 #2944 runs (`physiology_circulation_recordings.json`; the port is within 1.1e-7 of the engine on all of them; the 120 temperate acolytes include the unit that failed `>= 0.75`). Incomplete observations (no modes, no stats, unknown mode word, missing heart rate/core) are refused, not defaulted.
- **Humid heat.** Each unit's core rise must equal `T * (rate_base + rate_final) / 2`, where `rate` is the unclamped core-temperature rate of `thermo.readHeatBalance` (the pure read of the rule `thermo.tick` itself runs) at the probe's two snapshots and `T` is the simulated seconds of thermo steps the engine actually executed between them (a counter-only ledger over `thermo.tick`'s step receipts; wall-clock sleep is not simulated time). Tolerance `0.002 C + 2 %` of the expected rise. Each unit must also heat on its own (rise >= 0.02 C or hyperthermia); no unit is excused by another.
- **The read never changes a unit.** `thermo.heatInputs` gathers through a reader: the tick keeps `unit.getStat`'s initialization; the read uses the new `unit.peekStat`, which never rolls, caches or advances the stat RNG, and reports a unit with an unrolled input as unobservable. The physiology access audit enforces it (check 5).
- Unchanged: scenario climates, durations, stimuli, the acolyte population and every other check.

## Post-change batch

Declared before execution: **24 complete runs**, serial (`run_probes.py --only physiology --exact --jobs 1`), at the frozen probe commit `613cad5c6f944a7006f1b357fc3f0fbb489689ea`, `SYNARCHY_PHYSIOLOGY_DIAG_FILE` and `SYNARCHY_PROBE_FAILURE_ROOT` set per attempt. Evidence: `~/.local/state/synarchy/issue-2960/batch/` (`run-NN.log`, `run-NN.diag.jsonl`, `run-NN.start`/`.end`, `failures/`). Each run took 431-432 s of probe time.

| Attempts | Count |
|---|---|
| Completed (probe ran to its end) | 24 |
| Interrupted | 0 (an earlier batch at 52213de4c was stopped after one interrupted run when review round 1 superseded that head; it is not part of these numbers) |
| Unevaluable (bad measurement, spawn short of `--count`, missing observation) | 0 |

Per-check results over the 24 runs:

| Check | Failures | #2944 (24 runs, old rule) |
|---|---|---|
| `temperate (22C/0.5)` (circulation per unit, floor 0.65, plus the unchanged checks) | **0 / 24 runs** (120 units; every unit within `1e-4` of its rule; lowest circulation 0.6725) | 1 |
| `humid heat (46C/0.95)` (per-unit rise vs heat-balance rule, plus heating) | **0 / 24 runs** (120 units; largest deviation 4.6e-4 C = 0.64 % of the predicted rise, against the allowed `0.002 C + 2 %`; rises 0.060-0.325 C) | 3 |
| every other check of the probe | 2 failures, both outside #2960 (below) | fatigued sprint 3, calorie drain 1 |

Remaining failures, each explained:

1. Run 15: `fatigued unit sprints slower: rested 0.88 vs fatigued 0.85 tiles/s` - the known out-of-scope flake (#2944 saw 3 of 24). Evidence `failures/physiology/20261108T135546Z-613cad5c6-508f90`.
2. Run 20: `calorie store drains faster in combat: idle -28.9 kcal vs combat -35.1 kcal` - 1.21x against the required 1.3x, the known out-of-scope flake (#2944 saw 1 of 24). Evidence `failures/physiology/20261108T143955Z-613cad5c6-b208c2`.

In both failing runs every scenario check (temperate, humid heat, small creature, squirrel profile) passed. Whole-probe reliability is not established, so `physiology` stays FLAKY / manual-only in `tools/ci_probes.py`, whose reason now states this picture. Those two checks may get their own issues if the owner wants them.

## Reproduce

```
python3 tools/test_physiology_probe.py
python3 tools/physiology_access_audit.py --self-test
SYNARCHY_PHYSIOLOGY_DIAG_FILE=run-NN.diag.jsonl python3 tools/run_probes.py --only physiology --exact --jobs 1
python3 tools/record_physiology_heat_fixture.py --port 9173 --repeats 12   # regenerates the heat fixture
```
