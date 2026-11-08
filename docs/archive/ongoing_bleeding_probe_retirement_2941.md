# Retiring the end-to-end ongoing-bleeding probe (#2941, PRB-6)

The `tools/bleeding_trail_probe.py` probe (#882, #883, #2837; 1,213 lines, manual-only,
289 s / 287 s / 379 s in the three quruntul shakedown runs) is retired. The owner decided
on 2026-11-07 that Hspec grades the arithmetic it exercised and accepted the loss of
live-tick wiring coverage. Source report: `docs/probe_value_findings.md` PRB-6.

What replaces it: the new Hspec group `ongoing bleeding through the production tick`
(`test-headless/Test/Headless/Blood/OngoingBleed.hs`), which runs the real wound tick
(`Combat.Wounds.Tick.tickAllWounds`) and the real movement tick
(`Unit.Thread.Movement.tickAllMovement`) in lockstep against a real headless engine
environment and its real `BloodStore`. Run it with

```
cabal test synarchy-test-headless --test-options='--match "ongoing bleeding through the production tick"'
```

It finishes in about 0.02 s of test time (the examples are in-process; the cost is the
per-example engine environment).

## Coverage map: all 17 checks

Classes: **covered** = an existing example fails if the behaviour breaks; **moved** =
unique to the probe, now stated in the new group; **dropped** = not worth keeping. "Blood.Trail"
is `test-headless/Test/Headless/Blood/Trail.hs`; "WoundFold" is
`test-headless/Test/Headless/Unit/PhysiologyBloodModes/WoundFold.hs`; "New" is the new
group. Compound checks are split into their assertions.

| # | Probe check | Assertion | Class | Where it is stated now |
|---|---|---|---|---|
| 1 | `moving_trail` | marks lie along the route, not at one point | moved (wiring) | New: *a walking bleeder's trail marks are spread along the route, at least the documented distance apart, and bounded by it* (x span greater than `ttMinDistance`) |
| | | count within `[1, route/min_distance + 2]` | covered + moved | Blood.Trail *DISTANCE-limited path (cadence alone would allow more marks…)*, *pops exactly one mark once BOTH gates are cleared*; New same example (count bound, consecutive marks at least `ttMinDistance` apart) |
| | | a real `unit.injure` + `unit.moveTo` on the live unit thread | **accepted loss** | see below |
| 2 | `scaled_trail` | trail density unchanged under a different `world.setTimeScale` | **moved** | New: *a walking bleeder lays the same trail marks, stamped with game time, under a 1x and a 5x world calendar* |
| 3 | `internal_wound_dry` | an internal-only wound lays no marks | covered + moved | Blood.Trail *Combat.Wounds.Bleed.isExternallyBleedingKind excludes internal, fracture, and concussion*; WoundFold *credits an internal-only wound no external flow, though it bleeds, and drains no Nominal volume*; New *an internal-only wound lays no marks while the unit walks, though the unit is bleeding internally* (through both ticks, Simulated blood) |
| | | blood visibly drains | moved | same New example (`uiBlood` below the starting maximum) |
| 4 | `death_stops_trail` | the death tick clears the accumulator | covered | Blood.Trail *a NON-EMPTY pool cluster resets on load, clears on death, and disappears on destroy* (part 3: a fatal tick yields `Nothing`, the same unit with blood to spare keeps its cluster) |
| | | a corpse's wound tick never recreates it | covered | Blood.Trail *a dead unit's wound tick never recreates the trail accumulator…* |
| | | the kill command clears it and a corpse that keeps being moved lays nothing | moved | New: *the kill command ends the trail…* |
| 5 | `clot_stops_trail` | natural clotting zeroes the external bleed, clears the accumulator, no further marks while the unit keeps moving | **moved** | New: *stops a walking bleeder's trail the moment the external bleed reaches zero…* |
| 6 | `stationary_pool` | layers clustered within the jitter radius of the anchor | covered | Blood.Trail *every layer lands within ptJitterRadius of the anchor*, *anchors where the unit stands on the first tick, and dwells*; New *two adjacent bleeders…* (every layer within the jitter radius of its unit) |
| | | count grows with game time | moved | New clock-source pool example (layer count tracks `floor(dwell / ptMinCadence)`) |
| | | every mark carries the unit's own `sourceUnit` | moved | New *two adjacent bleeders…* (`bdeSourceUnit`) |
| | | pool/drops style family, small footprint | covered | Blood.Trail *stays within the pool/drops vocabulary*, *footprint is ALWAYS small* |
| | | growth is strictly additive | covered | Blood.Trail *layering never mutates an existing BloodDecal* (the probe already deferred the whole-record claim to this example) |
| 7 | `pool_bound` | layering stops at exactly `ptMaxLayers`, count flat while the wound is still bleeding | covered + moved | Blood.Trail *saturates at EXACTLY ptMaxLayers and then adds nothing more*, *at the bound the pending blood stays BANKED and the gates stay put*; New *two adjacent bleeders…* reaches the bound through both ticks (peak 12 layers, exactly 12 decals each) |
| 8 | `clot_stops_pool` | clot ends layering before the bound; the clot, not the bound, stops it | **moved** | New: *stops a stationary bleeder's pool below the layer bound when the external bleed reaches zero…* (peak at least 1 and below `ptMaxLayers`) |
| 9 | `walk_then_pool` | one accumulator: trail along the route, then a pool at the stop | covered + moved | Blood.Trail *a walk-then-stop pools UNDER the stopped unit*, *walk-then-stop-then-walk conserves every drop across the seam*; New *a walk-then-stop hands ONE accumulator…* (through both ticks: route marks, then a cluster whose anchor and layers sit within the jitter radius of the stopped unit) |
| 10 | `collapsed_pool_and_death` | a collapsed unit pools | moved | New *two adjacent bleeders, one of them collapsed…* (the collapsed unit reaches the bound; `uiPose` is set by the test, the movement tick has no pose input other than "dead") |
| | | death drops the cluster state | covered | Blood.Trail *a NON-EMPTY pool cluster … clears on death* |
| | | the marks outlive the source | covered + moved | Blood.Trail *…disappears on destroy* (decals stay queryable); New *the kill command ends the trail…* (decal count unchanged after the kill) |
| | | the marks keep aging | covered | Blood.Types `wetnessAt`: *dries linearly toward 0 as age approaches bloodDryDuration* (age is derived from game time at read time) |
| 11 | `independent_pools` | two bleeders, two separate bounded clusters | moved | New *two adjacent bleeders…* (each exactly 12 layers, each within the jitter radius of its own unit, `length ds == 2 * bound`, units farther apart than `ptClusterRadius`) |
| | | the two spirals are not in lockstep | covered | Blood.Trail *two adjacent bleeders' clusters are not in lockstep (the seed…)* |
| 12 | `pool_time_scale` | pool density unchanged under `world.setTimeScale` | **moved** | New: *a stationary bleeder lays the same pool layers, stamped with game time, under a 1x and a 5x world calendar* |
| 13 | `nominal_moving_trail` | severe slash under Nominal blood: marks, alive, volume untouched, live bleed rate | covered | WoundFold *twin*: *leaves a severe external slash's volume at the stale stored value, raises no collapse or exsanguination…*, *flow*: *credits the accumulator the wound-derived external flow under Nominal…*; *marks and pools*: *turns a Nominal accumulator into trail marks…* |
| 14 | `nominal_stationary_pool` | Nominal stationary bleeder pools, volume untouched | covered | WoundFold *marks and pools* (stationary half of the same example), *keeps the explicit exception…* |
| 15 | `nominal_dressing` | a sealing dressing clears the accumulator; a competent one keeps a reduced flow | covered | WoundFold *zero-bleed cleanup*: *a fully sealing dressing (seep 0) clears…* and *a competent dressing (seep 0.05) keeps a REDUCED positive flow…* (both modes) |
| 16 | `nominal_internal_dry` | internal-only wound under Nominal: no marks, volume full, still bleeding | covered | WoundFold *visual flow*: *credits an internal-only wound no external flow, though it bleeds, and drains no Nominal volume* |
| 17 | `loaded_spawn_surfaces` | no spawn took the unloaded-chunk Z=0 fallback | **dropped** | Guards the probe's own `unit.spawn` fixture; meaningless once the probe is gone |

## Why the clock-source and clot examples are strong

The clock-source examples build two complete scenes that differ **only** in the world
calendar: the same wound, the same positions each step, the same elapsed `gameTimeRef`,
but `wsTimeScaleRef` 1 versus 5 and `wsTimeRef` advanced five times faster. The test first
asserts the calendars did run apart (so the comparison is not vacuous), then asserts equal
layer or mark counts, a count near `floor(elapsed / cadence)`, and that every decal's
`bdeCreatedAt` lies inside the game-time window and matches between scenes. The boundary
under test is the real one: `tickAllMovement` reading `wsGameTimeRef`
(`src/Unit/Thread/Movement.hs`), with the wound tick reading the same ref in `tickAllWounds`.

The clot examples start from an unclotted wound, assert emission and a positive external
bleed first, then walk (or dwell) until `externalBleedRateFor` reads exactly zero, assert
the accumulator is `Nothing` and the decal count is frozen, and keep moving (a measured
patrol distance of more than 15 tiles) or dwelling for ten more game-seconds. The pool case
asserts a peak of at least one and below `ptMaxLayers`, so saturation cannot explain the
stop.

### How the examples were checked to fail when the behaviour breaks

Each mutation was applied to the production code alone, the new group was run, and the
change was reverted.

| Mutation | Result |
|---|---|
| `tickAllMovement` takes `now` from the game clock multiplied by the page's `wsTimeScaleRef` (a calendar-driven clock) | both clock-source examples fail; the rest pass |
| `clotBaseRate = 0.0` (clotting never progresses) | both clot examples fail (the wound never clots shut); the rest pass |
| zero-external-bleed cleanup removed from **both** the wound tick (`newTrailState`) and the movement tick (`extRate <= 0`) | both clot examples fail; the rest pass |

The two cleanups are redundant by design, so removing only one is correctly not a failure:
the contract is "no emission after the external bleed reaches zero", not which tick clears.

## Accepted loss (owner decision, 2026-11-07)

After this change nothing runs a real wounded unit under the live `unit_ai` / unit-thread
tick end to end and reads the resulting decals through `blood.listDecals`. Specifically
not covered by any gate:

- the scheduling of the combat thread's wound tick (10 Hz) against the unit thread's
  movement tick (the new group calls the two in lockstep and mirrors positions into the
  unit manager by hand, as `publishToRender` does);
- the Lua verbs the probe drove (`unit.injure`, `unit.moveTo`, `unit.collapse`,
  `unit.kill`, `unit.getBlood`) and the command queue behind them; the new group calls
  `handleUnitKillCommand` directly and sets `uiPose` itself;
- the `blood.listDecals`, `blood.getTrailState` and `blood.listTextures` debug surface as
  read from a live engine;
- agreement of the accumulator with real wall-clock seconds under the live tick at several
  time scales (the new group fixes the game clock and varies only the calendar).

A wiring break between the wound tick and the accumulator that the pure and the new
production-tick groups do not reach would no longer be caught.

## Retired in the same change

`tools/bleeding_trail_probe.py`; its `tools/probe_runner_registry.py` entry; its
`tools/ci_probes.py` reason; the `tools/probe_flake.py` `PROTOCOL_PROBES` registration,
`tools/probe_flake_tests/migration_bleeding_trail.py` and its `tools/test_probe_flake.py`
entry; the `docs/probe_census.json` row (through the locked writer,
`probe_census_storage.retire_probe`); the `tools/README.md` row and migrated-probe
counts; the live mentions in `docs/engine_contracts.md`, `docs/persistence_state_inventory.md`,
`src/Blood/Pool.hs` and `tools/lunge_probe.py`. `tools/probe_select.py` and
`tools/test_probe_select.py` used the key as a fixture whose registration order and
lexicographic order disagree; they now use `autosave` (registered after
`blood_gpu_lifecycle`, sorts before it), which states the same thing. The `.quruntul`
adapter names no probe key, and `python3 .quruntul/checks.py` passes.

Not changed: `docs/history/`, `docs/archive/` history, `tech_debt.md`,
`docs/code_health_findings.md:3850` (historical disposition) and the findings report.

## Validation run

`cabal test synarchy-test-headless --test-options='--match "ongoing bleeding through the
production tick"'` (9 examples), `--match "Blood.Trail"` (56) and `--match "unit
simulation page ownership"` (14, the group whose fixtures the new one reuses); and
`python3` `tools/test_probe_flake.py`, `tools/test_probe_select.py`,
`tools/test_run_probes.py`, `tools/ci_probes.py --self-test`, `.quruntul/checks.py`,
`tools/test_probe_census.py`, `tools/test_readme_registry_count.py`,
`tools/test_persistence_contract_sweep.py`, `tools/test_persistence_inventory_audit.py`.
No engine probe was run.
