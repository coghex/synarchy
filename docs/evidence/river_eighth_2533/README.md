# Generated eighth-level rivers (#2533)

The final indexed river table uses signed eighth-z surfaces. Selection, widths,
river identities, rainfall/flow accumulation and lake planes are unchanged.
Ordinary compatible cross-sections remain flat; intersecting sections at different
historical planes become downhill junctions instead of flattening a whole bend.

## Owner-approved specification amendment

The owner approved “Revise overlap handling to avoid large cuts” and “Yes, resolve
downhill junctions” after the strict original contract required cuts up to 100 z.
Accordingly, this PR supersedes equality of **every** historical width claim and
monotonicity along overwritten uphill centre edges. It preserves equality of
compatible claims and monotonicity along resolved surface connections. These
connections are generation scratch; stored identities and hydrological flow data
retain their original meaning. The policy is specified in
[the hydrology contract](../../hydrology_pipeline.md#exact-river-surface-finalization-2533).

## Measurements

Base: `5a44b615bfaceb34687760f807b74f281a03be65`. Base and candidate use identical
seeds, world sizes, plate counts, tracked defaults and resource data.
[inputs.json](inputs.json) records explicit dump paths and hashes; the adjacent
report files retain the complete diagnostic output. These reports are observations,
not pass/fail tools, and their integer compatibility views cannot prove exact steps.

The seed-42, size-64, three-plate inland dump covers 786,432 tiles. Its terrain and
fluid classifications are unchanged. Both sides have 9,160 rendered river tiles,
85 river–lake cliff pairs, 241 mouth-gap locations (77 fillable, 164 blocked), and
775 water-above-bank pairs. The full indexed world has 14,535 fractional river
surfaces out of 16,654, with a largest plane reduction of 10 eighth-units (1.25 z).
See [surface_measurement.json](surface_measurement.json).

Nine size-32/four-plate regional comparisons include seeds 7, 42, 314, 2718, 4096,
4567, 5050, 8080 and 13579. All have **zero new or worsened water-above-bank pairs**
and no newly reported water-to-water cliff locations. Bed repairs are at most 2 z
in those regions; every changed terrain tile previously held river or ocean water.
No dry-bank or lake-bed terrain changes. The former 100-z-cut witness in seed 4567
keeps its bed. Its whole indexed river field lowers by at most 15 eighth-units.

[baseline_review.json](baseline_review.json) retains counts, new diagnostic
locations and their exact before/after neighbourhoods. Eight category additions
need interpretation rather than being hidden by the rebaseline:

- Six `MID_RIVER_CLIFF` reports (five in seed 314, one in 4567) concern existing
  two-z water steps. The step remains two z; a one-z positive-depth bed repair
  makes the bed flat enough to enter this diagnostic's narrower category.
- Seed 42 gains one `ISLAND_1TILE` report: the dry tile is unchanged, but its
  neighbours now all classify as Ocean under the existing priority rule.
- Seed 4567 gains one `DESERT_SOIL_ON_SLOPE` report beside a repaired wet bed.
  Its neighbourhood is retained for review; no extra terrain smoothing was added.

Ocean overrides latent river-table entries during composition. Treating every
such entry as another sea-level sink caused unnecessary inland cuts and new lake
outlet drops; that candidate was rejected. The final solver anchors only routed
breakthrough endpoints. Chunk generation repairs zero-depth ocean beds locally,
using the same priority rule, without propagating those cuts into adjacent banks.

## Reproduce

For each binary, run the following from its matching resource tree:

```sh
ENGINE --dump=terrain,fluid --seed 42 --worldSize 64 --plates 3 \
  --region -32,-32,31,31 +RTS -N2 > SIDE.json
python3 tools/river_lake_gaps_report.py SIDE.json
python3 tools/river_mouth_gap_report.py SIDE.json
python3 tools/water_above_land_report.py SIDE.json
python3 tools/river_eighth_capture.py --engine /absolute/ENGINE \
  --out /absolute/NEW_CAPTURE_DIRECTORY --port 9534
```

The nine regional dumps use `--dump --worldSize 32 --plates 4 --region -4,-4,4,4`.
All comparison paths, hashes, counts and changed neighbourhoods are retained.
The base and candidate directories contain offscreen captures with matching
camera coordinates, zoom and facings. `captures.json` records the binary hashes
and recipe. The gentle reach shows fractional water steps; steep terrain obscures
some other facings and cut-plane clipping remains visible. These are supplemental
visual evidence, not a claim that all existing river artifacts are fixed.

## Persistence and verification

`world-pages` v13 freezes the outgoing v12 page and worldgen DTO shapes. Every
supported v1–v12 decoder scales real river heights once, preserves `minBound`,
and disables the new bed-repair policy for historical worlds. Existing stored
carves and terrain remain unchanged, including after resaving. A persisted policy
bit enables the repair for newly generated worlds. `GeoTimeline` itself keeps its
wire shape. The tracked `z5-exact-generated-rivers` fixture contains real fractional
river surfaces and was produced through the engine's normal save transaction.

`WorldGen.ExactRiver` covers synthetic descent, overlapping claims, centre/wing
junctions, confluences, wrapped adjacency, preserved branches, closed pools,
Ocean and magma interaction, and old/current save conversion. The generated-world
group independently reconstructs historical selection for `(42,32,4)`, `(42,64,3)`,
`(4567,32,4)` and `(13579,32,4)`, checks unchanged metadata and surface bounds,
and generates every indexed river chunk to verify positive depth and zoom/detail
agreement. It also limits smoothing to seven eighth-units below the sink baseline,
and total plane lowering to fifteen eighth-units in these worlds.

Validation:

- Production `cabal build all` and the full headless tier with
  `SYNARCHY_FULL_TESTS=1`: **10,466 examples, zero failures**. The final full
  run used the built test executable while the independent world check ran,
  avoiding concurrent Cabal configuration. The first full run exposed two stale
  save expectations; the corrected 177-example save-components group and the
  final full run both pass.
- All 25 exact-river examples, including four generated worlds; map-page goldens.
- `world_baseline.py`: 21 worlds × three identical runs; `world_check.py`:
  **21/21 pass**; `world_determinism.py --seed 42 --runs 3`: identical output.
- Save manifest audit: 33 baselines/39 fixtures; save-audit tests:
  370 assertions; reproducibility: four assertions.
- Persistence inventory and its self-tests (260 assertions), enum and Unicode
  audits; capture-tool syntax check and real offscreen execution; diff checks.

[baseline_counts.json](baseline_counts.json) records the complete changed category
totals against the base.

Fresh-process migration coverage is recorded in
[migration_validation.json](migration_validation.json): all **33 historical
complete-session fixtures** pass the canonical sweep. Its final current-format
case produced two stale-expectation failures because the sweep had cached that
fixture's expected summary before its refresh: actual `gameTime`
`2.2158049999998184`, old expected `2.232239999997546`. The actual value matches
the final tracked expectation. A fresh run of the same canonical
`run_one_fixture` function for `z5-exact-generated-rivers` passed every check,
including initial load, resave, fresh-process reload and second resave, with zero
failures or skips. Thus all 34 final fixtures have passing behavioral coverage;
the original all-fixture command itself exited 1 and is not reported as green.

The focused rerun loaded fresh declarations and used the canonical runner (no
alternate comparison or changed probe assertions):

```python
import sys
sys.path.insert(0, "tools")
import save_compat_migration_probe as probe
assert not (probe.verify_bootstrap_plan() + probe.verify_lua_error_marker())
fixtures = [f for f in probe.declared_complete_session_fixtures()
            if f["fixture_id"] == "z5-exact-generated-rivers"]
assert len(fixtures) == 1
checks = probe.Checks()
probe.run_one_fixture(checks, 9537, fixtures[0])
assert checks.failed == 0 and checks.skipped == 0
```

Both runs used the final built engine and save-codec executables through
`SYNARCHY_PROBE_ENGINE_EXE` and `SYNARCHY_SAVE_CODEC_EXE`, with
`DYLD_LIBRARY_PATH=/usr/local/lib`.
