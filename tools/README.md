# Development tools

Python scripts for auditing/regression-testing world generation, and for
driving/verifying engine and game-logic behavior against a real headless
engine instance.

## Pre-push gate: `ci-local.sh`

`make ci` (repo root) runs `tools/ci-local.sh`, which mirrors the CI gate
(`.github/workflows/ci.yml`) locally: a warning-clean (`-Werror`) build of
the library/exe + both test suites, the headless hspec suite,
`test_audit.py`, and `world_check.py --quick`. A green `make ci` predicts a
green CI run. It applies `-Werror` the same scoped way CI does and restores
any pre-existing `cabal.project.local` on exit.

## World generation tools

Scripts for auditing, checking determinism, and regression-testing the
world generation pipeline (dump-only — no TCP, no interaction).

### `world_audit.py`
Runs the `synarchy --dump` command (or reads a pre-generated dump) and
categorizes anomalies in the tile data. Output is structured JSON.

```bash
# Run audit on seed 42
python3 tools/world_audit.py --seed 42 --worldSize 32 --region -4,-4,4,4

# Audit a saved dump
python3 tools/world_audit.py --input dump.json --format text
```

Checks for: dry-below-sea tiles, ocean-on-land (cascade bug), fluid-under-
terrain, floating fluid, terrain spikes/pits, river chunk gaps, river mouth
drops, isolated islands/fluids, minBound leaks, surface inconsistencies.

### `world_determinism.py`
Runs the dump multiple times for the same seed and verifies the output is
content-identical across runs. Reports which tiles differ if the pipeline is
non-deterministic.

```bash
python3 tools/world_determinism.py --seed 137 --verbose
```

`--runs` defaults to 3; pass a higher count (e.g. `--runs 10`) only when
chasing a suspected race.

### `world_baseline.py`
Captures baseline outputs for every seed in `baselines/_seeds.json`. Records
determinism status, fluid stat envelopes, and issue count envelopes.

```bash
# Capture all baselines
python3 tools/world_baseline.py

# Capture a single seed
python3 tools/world_baseline.py --seed 42
```

Writes `baselines/seed{N}_size{N}_region_{X1}_{Y1}_{X2}_{Y2}.json` per seed.
`--runs` defaults to 3; pass a higher count only when chasing a suspected race.

### `world_check.py`
Runs the regression suite: for every seed in `_seeds.json`, dumps N times,
audits each dump, and compares to the stored baseline envelope.

```bash
# Run full check (pre-commit gate)
python3 tools/world_check.py

# Verbose output
python3 tools/world_check.py --verbose

# Check a single seed
python3 tools/world_check.py --seed 42
```

Exit 0 on pass/improvement, 1 on failure, 2 on bad invocation.

### `test_audit.py`
Unit tests for the audit script. Constructs synthetic tile grids to verify
each check correctly identifies the issue it's meant to catch.

```bash
python3 tools/test_audit.py
```

### `lua_module_budget.py`
Cheap, no-engine guard (#545) for Lua files that were split into a shell
plus small per-domain modules with an agreed physical-line budget, such
as `scripts/debug.lua` + `scripts/debug/*.lua` or
`scripts/unit_resources.lua` + `scripts/unit_resource*.lua`. Fails if
any budgeted file grows back past its limit.

```bash
python3 tools/lua_module_budget.py
```

### Workflow

Before committing a change:
```bash
python3 tools/test_audit.py               # unit tests pass
python3 tools/lua_module_budget.py        # Lua module line budgets pass
python3 tools/world_check.py              # regression suite passes
```

After an intentional change that improves (or legitimately alters) world
generation output, re-capture baselines:
```bash
python3 tools/world_baseline.py
```

Baselines are **tracked in git** (#421): `tools/baselines/` is committed, so
a fresh clone or worktree can run `world_check.py` immediately, and an
intentional worldgen-output change ships its re-captured baselines in the
same PR — the baseline diff is how reviewers see the intended drift. Never
edit the baseline JSON by hand; always regenerate with `world_baseline.py`.

CI (`.github/workflows/ci.yml`) runs `world_check.py --quick` as a blocking
gate on every PR. Worldgen output is bit-identical across macOS/aarch64
(where baselines are typically captured) and Linux/x86_64 (where CI runs),
so there is one set of baselines for all platforms — a worldgen-output PR
that forgets to rebaseline fails CI.

## Behavior probes (headless engine)

Unlike the worldgen tools above (which shell out to `--dump`, no TCP), these
scripts boot a **real headless engine** (`--headless --port NNNN`), drive it
over the debug-console TCP protocol (see the repo-root `CLAUDE.md` "Headless
Mode & Debug Console" section for the protocol itself), and assert on the
result. They're first-class regression harnesses — each one is the gate for
a specific system or bug, referenced from `CLAUDE.md` and PR descriptions —
but because they boot a full engine (and some generate a real world on top
of that), they're **much slower** than the dump-only tools above: a few
seconds of engine boot at minimum, tens of seconds to a couple of minutes
when a scenario needs actual world generation. They are not part of the
default test tiers; run the ones relevant to what you changed.

Each probe is self-contained (own `main()`, own engine boot/teardown, own
default port chosen to avoid the user's GUI on 8008) and prints PASS/FAIL
plus `sys.exit(0 or 1)`. Most take `--port` to avoid colliding with another
running instance; one (`cargo_capacity_probe.py`) takes no flags at all and
hardcodes its port.

**Gotcha:** not every probe uses `argparse` — the flagless one above has no
`--help` handling either, so passing `--help` doesn't print usage and exit,
it silently runs the *actual probe* (which boots a real engine and can hang
for minutes if you weren't expecting that). Check the header docstring
instead of reaching for `--help` when in doubt.

"Boot" below is `arena` (flat synthetic terrain via
`scripts/movement_arena.lua`, no world generation — fast) or `worldgen`
(generates a real world at a given seed/size — slower, scales with size).

| Probe | Gates | Boot | Purpose |
|-------|-------|------|---------|
| `cargo_capacity_probe.py` | #189 | arena | `depositToCargo` weighs the actual `ItemInstance` (fill + nested contents), not the item def's base weight. |
| `chop_probe.py` | #97 | worldgen | Chop-designation layer + chop AI + `wood_log` yield, end to end. |
| `collapse_crawl_probe.py` | #304 | arena | Collapse↔crawl pose hysteresis in `tickInjuries`. |
| `combat_anim_probe.py` | general combat/animation guard | worldgen | Drives a real fight headless; samples `currentAnim` to verify swing and death animations actually play. |
| `concussion_revive_probe.py` | #304 | arena (shares boot helpers with `collapse_crawl_probe.py`) | `checkRevive` concussion-band hysteresis (companion to `collapse_crawl_probe.py`). |
| `construction_probe.py` | #96 | arena | `construct_job` AI end-to-end: claim, material sourcing, progress accrual, piece placement, staking, dead-claimant release. |
| `consumable_effects_probe.py` | #347 | arena | `scripts/consumable.lua`'s drink mechanism: hydration/caffeine/mood/warmth scaled by a coffee_pot instance's quality (#343) and effective temperature (#344); the caffeine meter's decay + concentration boost and stamina fatigue-offset (`brain.lua`/`unit_resources.lua`). |
| `cooking_probe.py` | #346 | arena | Kitchen workshop + cooking skill/`basic_cuisine` knowledge + `basic_food.yaml` coffee recipe: content shape, all-or-nothing consumption, crafter-derived quality (#343), 100 °C output temperature (#344). |
| `craft_probe.py` | #325, #326, #343, #327 | arena | `craft.*` API: catalogue, execute, work stations, crafter-derived quality, smelting. |
| `craft_bill_probe.py` | #329 | arena | Craft-bill backend (`craft.addBill`/claim/progress/complete verbs) + `craft_job` AI: claim a bill, source inputs from the ground and from cargo storage, work the built station, the fresh output instances laid down at the station (a carried same-def item stays carried), knowledge gate. |
| `crop_probe.py` | #334 | worldgen | Row-crop natural placement (`tomato_plant`) + groundcover `world.plantCropAt` (`wheat`) into a `CropPlot`, growth under the real clock, harvest, refusal for a row_crop species, save/load round-trip. |
| `disarm_probe.py` | #193 | arena | Disabled-hand auto-drop must re-fire. |
| `flora_growth_probe.py` | #332 | worldgen | Derived flora growth/age/phase under the advancing calendar; fruiting-window gating; survives save/load. |
| `follow_command_priority_probe.py` | #306 | arena | Follow-command priority against other AI goals. |
| `foraging_probe.py` | #94 | worldgen | Foraging AI + harvestable-flora gating. |
| `infection_probe.py` | #593 | arena | Infection growth / antiseptic prevention / antibiotic cure / sepsis meter, end-to-end. Boots its own engine with `SYNARCHY_INFECTION_TEST_MODE=1` (test-tuned rate/grace, scoped to that one process) so growth is observable in seconds without touching production gameplay. |
| `injury_log_probe.py` | logging arc (general) | arena | Injury-log stream roundtrip: `injury.emit`/`drainEvents`, `unit.injure`, `emitEventForUnit` tagging. |
| `item_instance_probe.py` | #67 | worldgen | Per-instance item identity. |
| `item_temp_probe.py` | #344 | worldgen | Item temperature model. |
| `location_content_probe.py` | #90, #91 | worldgen + arena | Location content spawning + ruin probe. |
| `location_overlay_probe.py` | #89 | worldgen + arena | World-gen location-overlay placement. |
| `location_stamp_idempotent_probe.py` | #424 | worldgen | Geometry-stamp idempotency survives clearing the anchor floor + save/restart/reload; a never-visited location still stamps on first load. |
| `lua_orphan_prune_probe.py` | #195 | worldgen | Lua per-id AI state is pruned (not inherited by id reuse) after a save load. |
| `medic_coord_probe.py` | squad-medic coordination (general) | arena | `bestMedicFor`/`medicAvailable` distance-discounted selection fix. |
| `movement_probe.py` | movement arc (general, closed) | arena | Obstacle-course movement (pathing/climbs/falls/ramps) via `movement_arena.lua` courses; `--list` shows courses. |
| `multiworld_save_probe.py` | #214, #219 | worldgen + arena | Multi-world save → quit → restart → load; cross-page entity survival. |
| `physiology_probe.py` | homeostasis (general) | arena | Thermoregulation/circulation sanity across controlled environments (temperate/arctic/humid-heat). |
| `plant_probe.py` | #335 | worldgen | Plant-designation layer: `world.getPlantSuitability` lists both shipped crops sorted best-first, designation refused on an untilled tile / for an unregistered crop name, succeeds on a tilled tile (row_crop and groundcover_crop names both accepted), replace-on-redesignate semantics, save/load. |
| `power_probe.py` | #358 | arena | Build-tool-routed power-node placement: `buildTool.commitPlacement` consumes an item off the selected unit for `power.*`-placeable defs, role/parameter reporting, save → quit → restart → load reconnects nodes to buildings. |
| `power_workshop_probe.py` | #361 | arena | `requires_power` workshop consumer: unpowered `craft.executeAt` refusal, wired-but-uncharged still unpowered, noon flip powers it, `craft_job` AI stalls at 0 progress while browned out and resumes once powered, battery `storedWh` rises/falls over a simulated day/night with the consumer's drain folded into the balance. |
| `repair_item_probe.py` | #300 | worldgen | `unit.repairItem` primitive. |
| `repair_probe.py` | #301 | arena | Repair policy layer (station-gated repair on top of #300). |
| `repair_ai_probe.py` | #302 | arena | `repair_job` AI end-to-end: claim, own/equipped/mule-held sourcing, station routing, dead-claimant release, `smith` role weighting. |
| `role_probe.py` | #265 | worldgen | Derived unit-role hysteresis/demotion/work-XP growth. |
| `save_pause_probe.py` | #42 | worldgen | Save/load pause-semantics regression. |
| `thermo_altitude_probe.py` | #308 | worldgen (size 128) | Altitude-lapse thermal effect. |
| `till_probe.py` | #333 | worldgen | Till-designation layer + till AI end to end: designate/cancel, fluid-tile exclusion, save/load, autonomous tilling (`world.getVegAt` confirms the flip), idempotent re-sweep. |
| `wire_probe.py` | #359 | arena | Wire structure piece: connection-aware autotile shape derivation (adjacency → isolated/end/straight/corner/tee/cross) and the `construct_job` AI placing a real wire tile from a designation. |

Invocation is bare `python3 tools/<name>.py` for sane defaults; most accept
`--port`/`--seed`/`--size` overrides and a handful have scenario-specific
flags (`--course`, `--phase`, `--attacker`/`--target`, ...) — see the
script's header docstring for its exact flag set.

### `run_probes.py` — opt-in aggregate runner

Runs a selection of the probes above and prints a per-probe PASS/FAIL
summary, exiting non-zero if any failed. `python3 tools/run_probes.py
--list` is the authoritative count and listing of registered probes — it's
grown over time (currently in the low 30s) and this doc doesn't try to
track the exact number.

```bash
# Run everything, sequentially (slow — low tens of minutes)
python3 tools/run_probes.py

# Run up to 4 probes concurrently, each its own engine on its own port (#531)
python3 tools/run_probes.py --jobs 4

# Run a subset, matched by substring against the probe key/filename
python3 tools/run_probes.py --only combat,movement

# Match --only against exact probe KEYS instead of substrings — 'craft'
# won't also pull in 'craft_bill'
python3 tools/run_probes.py --only craft --exact

# List known probes and exit
python3 tools/run_probes.py --list

# Override --port on probes that support it (the one flagless probe keeps
# its own hardcoded port regardless)
python3 tools/run_probes.py --port 9500
```

Each selected probe still shells out to its own subprocess and boots its
own headless engine — probes canNOT share one running engine (several
neutralise the global `unit_ai.update`, load defs engine-wide, reuse the
same world/page names, or restart the engine mid-run), so there's no clean
per-scenario isolation on a shared instance. What `--jobs` changes is
whether those independent engines run one after another or several at
once:

- **Default (`--jobs 1`), sequential:** one engine at a time; total cost is
  roughly the sum of each probe's own boot + scenario time. This is the
  mode CI's selective gate (`tools/ci_probes.py`, #530) relies on.
- **`--jobs N`, concurrent:** up to `N` probes run at once, each its own
  engine on a unique port (#531), cutting wall-time to roughly
  `total / N`, bounded by the slowest single probe. Concurrency raises
  engine-boot and port contention, so failures are more likely to be
  flakes than with `--jobs 1`. Cap `N` at (cores − 1) or so — each probe
  is a full engine process.

A full run (any `--jobs`) is slow; it is *not* part of any default test
tier — see `CLAUDE.md` Testing Tiers. Prefer `--only` for day-to-day use.

`--retries N` re-runs a failed probe SOLO (never concurrently, regardless
of `--jobs`) up to `N` more times before it's counted as failed — a probe
that passes on any attempt counts as PASS. This absorbs the contention
flakes a back-to-back or concurrent run can introduce; it does not paper
over a probe that's genuinely broken. `--tail N` prints the last `N` lines
of a failing probe's captured output for a quicker look without re-running
it by hand.

`--list` shows the full probe registry but not CI status. For that, see
`tools/ci_probes.py --status` below.

### `ci_probes.py` — CI probe selection + eligibility (#530, #540)

Computes which probes CI should run for a given set of changed files (see
`.github/workflows/ci.yml` and the CLAUDE.md "Testing Tiers" section for
the gate this feeds). It also owns `CI_ELIGIBLE` — the curated,
small smoke subset of the full registry that's actually allowed to run in
the blocking CI gate. Deterministic probes can still be manual-only when
they are too narrow or too expensive for every matching PR, and paths
covered only by manual-only probes select no behavior probe by default.

```bash
# What would CI run for these changed files?
python3 tools/ci_probes.py --changed src/Power/Network.hs

# Validate the mapping (no engine) — also a blocking CI step
python3 tools/ci_probes.py --self-test

# Every registered probe's CI status: CI-eligible, or manual-only with a
# reason category (flaky / base-failing / slow/worldgen-heavy /
# scenario-heavy / targeted / unclassified)
python3 tools/ci_probes.py --status
```

## Directory layout
```
tools/
├── README.md               (this file)
├── world_audit.py          (audit a single dump)
├── world_determinism.py    (detect race conditions)
├── world_baseline.py       (capture reference outputs)
├── world_check.py          (regression suite runner)
├── test_audit.py           (unit tests)
├── lua_module_budget.py    (Lua module split line-budget guard)
├── run_probes.py           (opt-in aggregate behavior-probe runner)
├── *_probe.py              (headless behavior probes — see above)
└── baselines/
    ├── _seeds.json         (seed list config)
    └── seed*.json          (per-seed baseline data)
```
