# Development tools

Python scripts for auditing/regression-testing world generation, and for
driving/verifying engine and game-logic behavior against a real headless
engine instance.

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

### Workflow

Before committing a change:
```bash
python3 tools/test_audit.py     # unit tests pass
python3 tools/world_check.py    # regression suite passes
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
running instance; two (`cargo_capacity_probe.py`, `disarm_probe.py`) take no
flags at all and hardcode their port.

**Gotcha:** not every probe uses `argparse` — the two flagless ones above
have no `--help` handling either, so passing `--help` doesn't print usage
and exit, it silently runs the *actual probe* (which boots a real engine and
can hang for minutes if you weren't expecting that). Check the header
docstring instead of reaching for `--help` when in doubt.

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
| `craft_probe.py` | #325, #326, #343, #327 | arena | `craft.*` API: catalogue, execute, work stations, crafter-derived quality, smelting. |
| `craft_bill_probe.py` | #329 | arena | Craft-bill backend (`craft.addBill`/claim/progress/complete verbs) + `craft_job` AI: claim a bill, source inputs from the ground and from cargo storage, work the built station, the fresh output instances laid down at the station (a carried same-def item stays carried), knowledge gate. |
| `disarm_probe.py` | #193 | worldgen | Disabled-hand auto-drop must re-fire. |
| `flora_growth_probe.py` | #332 | worldgen | Derived flora growth/age/phase under the advancing calendar; fruiting-window gating; survives save/load. |
| `follow_command_priority_probe.py` | #306 | arena | Follow-command priority against other AI goals. |
| `foraging_probe.py` | #94 | worldgen | Foraging AI + harvestable-flora gating. |
| `infection_probe.py` | general infection-system guard | arena | Infection growth / antiseptic prevention / antibiotic cure / sepsis meter, end-to-end. |
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
| `repair_item_probe.py` | #300 | worldgen | `unit.repairItem` primitive. |
| `repair_probe.py` | #301 | arena | Repair policy layer (station-gated repair on top of #300). |
| `repair_ai_probe.py` | #302 | arena | `repair_job` AI end-to-end: claim, own/equipped/mule-held sourcing, station routing, dead-claimant release, `smith` role weighting. |
| `role_probe.py` | #265 | worldgen | Derived unit-role hysteresis/demotion/work-XP growth. |
| `save_pause_probe.py` | #42 | worldgen | Save/load pause-semantics regression. |
| `thermo_altitude_probe.py` | #308 | worldgen (size 128) | Altitude-lapse thermal effect. |

Invocation is bare `python3 tools/<name>.py` for sane defaults; most accept
`--port`/`--seed`/`--size` overrides and a handful have scenario-specific
flags (`--course`, `--phase`, `--attacker`/`--target`, ...) — see the
script's header docstring for its exact flag set.

### `run_probes.py` — opt-in aggregate runner

Runs a selection of the probes above back-to-back and prints a per-probe
PASS/FAIL summary, exiting non-zero if any failed.

```bash
# Run everything (slow — boots ~27 engines in sequence)
python3 tools/run_probes.py

# Run a subset, matched by substring against the probe name
python3 tools/run_probes.py --only combat,movement

# List known probes and exit
python3 tools/run_probes.py --list

# Override --port on probes that support it (the two flagless ones keep
# their own hardcoded port regardless)
python3 tools/run_probes.py --port 9500
```

It shells out to each probe as its own subprocess and runs them
**sequentially, one engine at a time** — it does not attempt to make probes
share a single running engine (they're independent scripts with their own
boot/teardown, not built around an injectable engine handle), so the total
cost is roughly the sum of each probe's own boot + scenario time. That
makes a full run slow (easily 15+ minutes); it is *not* part of any default
test tier — see `CLAUDE.md` Testing Tiers.

## Directory layout
```
tools/
├── README.md               (this file)
├── world_audit.py          (audit a single dump)
├── world_determinism.py    (detect race conditions)
├── world_baseline.py       (capture reference outputs)
├── world_check.py          (regression suite runner)
├── test_audit.py           (unit tests)
├── run_probes.py           (opt-in aggregate behavior-probe runner)
├── *_probe.py              (headless behavior probes — see above)
└── baselines/
    ├── _seeds.json         (seed list config)
    └── seed*.json          (per-seed baseline data)
```
