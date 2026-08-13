# Development tools

Python scripts for auditing/regression-testing world generation, and for
driving/verifying engine and game-logic behavior against a real headless
engine instance.

## Pre-push gate: `ci-local.sh`

`make ci` (repo root) runs `tools/ci-local.sh`, which runs the complete local
CI gate: a warning-clean (`-Werror`) build of
the library/exe + both test suites, the headless hspec suite,
`test_audit.py`, and `world_check.py --quick`. PR CI is path-selective for
the graphical test-suite build and quick worldgen check, while pushes to
master run both; a green `make ci` remains a conservative CI prediction.
`-Werror` itself lives in `synarchy.cabal`'s checked-in warning policy, so
every build already carries it; `ci-local.sh` only scopes a temporary
`-fforce-recomp` via `cabal.project.local` (forcing a genuine recheck of
every module instead of trusting a possibly-stale warm build), restoring
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

### `action_outcome_coverage.py`
Self-audit (#646) for the F4 action-outcome oracle: greps each registered
commit-boundary verb's own source for its `debug.recordOutcome` /
`pushActionOutcome` call site and reports instrumented yes/no, mirroring
`ci_probes.py --status`'s "make the gap visible" style. Not a blocking
gate — Tier 2/3 verbs are deliberate fast-follows, not regressions.
Verbs that share a file (e.g. `unitAi.commandMove`/`commandAttack`,
`craft.execute`/`executeAt`) are checked within their OWN function body,
not file-wide, so instrumenting one sibling can't false-positive the
other. `--self-test` proves that scoping actually discriminates.

```bash
python3 tools/action_outcome_coverage.py
python3 tools/action_outcome_coverage.py --self-test
```

### `location_placement_sweep.py`
One-off MEASUREMENT tool (#997), not a gate: generates a fixed set of 21
DISTINCT worlds at the GUI/default configuration (10 plates, unchanged
`config/world_gen_default.yaml`) and counts how many place zero
locations. Each world gets its own engine process, so any single run is
reproducible on its own from the printed tuple. The recorded result
lives in `docs/location_placement_sweep.md`; the standing placement
gates are the `Location overlay (#89)` hspec group and
`location_overlay_probe.py`'s phase-9 matrix, which stay small
deliberately. Re-run this only when you want a fresh frequency number.

```bash
python3 tools/location_placement_sweep.py
python3 tools/location_placement_sweep.py --only 64 --json /tmp/sweep.json
python3 tools/location_placement_sweep.py --single --seed 7 --size 128
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
gate for worldgen-output PRs and every push to master. Worldgen output is bit-identical across macOS/aarch64
(where baselines are typically captured) and Linux/x86_64 (where CI runs),
so there is one set of baselines for all platforms — a worldgen-output PR
that forgets to rebaseline fails CI.

## Language report

### `language_report.py`
Report/check tool (#710, #1094, #1095, #1096) for the generated-language
native-name renderer (`Language.Generated.*`) — not a `*_probe.py`, so it
needs no `tools/ci_probes.py` registration. Drives the production Haskell
generator through the engine's `--language-report` boot mode (pure
computation, no graphical engine, headless simulation, or world
generation) over a seed range and reports profile diversity, canonical
native-name renderings alongside their #709 English glosses, root
collisions, duplicate-name counts, output-length distribution,
ASCII/length/capitalization/punctuation contract violations, #1094's
per-profile admissible two-consonant onset sets and `y` roles, #1095's
triple-letter runs, doubled-letter rate, and per-language boundary-repair
rules, and #1096's per-language bound morphemes (each selected concept's
free and bound form, both collision totals, and every selected concept
rendered bare and in each dependent slot).

```bash
# Human-readable report
python3 tools/language_report.py --seeds 0:255

# Check mode: the enforced quality gate (#1094 requirement 10, #1095/#1096 acceptance)
python3 tools/language_report.py --seeds 0:255 --check

# Detector self-test: boots no generator, proves the gates can fire
python3 tools/language_report.py --self-test
```

The tool reimplements no generation logic. That is why #1096's
admissibility verdict arrives as a Haskell-computed per-record boolean
rather than being recomputed in Python — the admissibility relation *is*
generation logic — while the prefix rule and both collision totals are
checked here directly from the exposed strings and counts.

#1096's bound-slot renderings are accumulated separately from the
canonical `renderings` array and never enter the distinct-name,
profile-signature, or pinned length-distribution populations, so added
sample volume cannot move a gate's denominator. They are still subject to
every zero-gated structural check.

`--check` splits into two kinds of assertion:

- **Structural gates**, enforced for any seed range: zero root
  collisions, zero output-contract violations, no name duplicated
  within a single language, every version-2 profile's admissible-onset
  count inside the inclusive 25-45% band of its own `n*(n-1)` ordered
  pairs (integer arithmetic, never a rounded percentage), no empty
  version-2 relation, every word-initial two-consonant onset accepted
  by that profile's own exported relation, no identical-consonant
  onset, zero triple-letter runs, a real boundary rule on every
  profile at or above the boundary-phonology generator version, a
  3-character minimum name length, a 27-character structural maximum,
  profiles whose stamped version matches the report header, and #1096's
  bound-form rules — at most eight per language, every stored form a
  nonempty strictly-shorter prefix retaining a visible letter, zero
  forms rejected by their own profile's admissibility relation, zero
  bound-related collisions, every `Bare` rendering equal to its
  concept's free form, no bound form at all below the version that
  introduced them, and at least one visible free-to-bound shortening
  reaching completed output.
- **Pinned regression gates**, measured from the current generator at
  the canonical `--seeds 0:255` sample and skipped (loudly) for any
  other range: exact distinct-signature, total-name and distinct-name
  counts (the latter two also floored at the historical 240/256 and
  95% ratios), the exact maximum name length, the average within ±0.5,
  the cross-seed shared-pair onset-diversity rule, the presence of
  consonant-only, vowel-only, and dual-role `y` profiles, and the
  presence of both compound and both genitive ordering directions
  (without which #1096's slot matrix is exercised one-sided). These are
  pins to update deliberately alongside a generator change, not
  invariants — nothing forbids two independently generated languages
  from coincidentally sharing a short string.

Doubled letters are **reported, never gated**. #1095 requires them to
stay legal at a comparable rate rather than to clear a threshold, so the
enforceable form of that lives in the hspec suite (a fixture whose
in-morpheme double survives every join) instead of as an arbitrary
population percentage here. A triple-letter run is three contiguous
ASCII letters that are the same letter ignoring case — punctuation
interrupts a run, so a hyphen join's `a-a` and an apostrophe affix's
`h'h` are not runs, while a capitalized `Aaa` is.

Word-initial onset scoping: rendered roots are flat text with no
per-character slot provenance, so the onset gates look only at the
name's first two glyphs and the first two after each `-` join, and only
when both are consonant-capable and neither is vowel-capable in that
profile (a dual-role `y` in a vowel slot would otherwise look like a
cluster member). Interior adjacencies come from syllable and compound
concatenation, which #1094 assigned to L1c — #1095 mediates them through
that same exported relation, so a boundary repair and a syllable onset
cannot disagree about what a given language allows.

Exit codes: 0 pass, 1 check failure, 2 bad invocation.

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
for most world-generation scenarios, and up to ten-plus minutes for the
heaviest AI-driven ones (`till_probe.py` ~7 min, `farm_ai_probe.py`
~11.5 min — both do many synchronous TCP round-trips per site over real
terrain). They are not part of the default test tiers; run the ones
relevant to what you changed.

Each probe is self-contained (own `main()`, own engine boot/teardown, own
default port chosen to avoid the user's GUI on 8008) and prints PASS/FAIL
plus `sys.exit(0 or 1)`. Every probe registered in `run_probes.py` (the
table below) takes `--port` to avoid colliding with another running
instance, defaulting to its own historical fixed port when unset (#723).

"Boot" below is `arena` (flat synthetic terrain via
`scripts/movement_arena.lua`, no world generation — fast) or `worldgen`
(generates a real world at a given seed/size — slower, scales with size).

| Probe | Gates | Boot | Purpose |
|-------|-------|------|---------|
| `action_outcome_probe.py` | #646 | worldgen | F4 action-outcome oracle through the real Lua contract: `debug.recordOutcome` requires kind+outcome, a full record round-trips through `debug.drainActionOutcomes` with every field intact, the ring drains destructively (second drain empty), a mixed tillable/non-tillable sweep reports `partial` with `requested == applied + dropped`, and an unloaded-anchor sweep reports `rejected`. |
| `bleeding_trail_probe.py` | #882, #883 | arena | Ongoing bleeding, both halves off one accumulator. Trails (`Blood.Trail`): a moving, externally-bled unit leaves distance/cadence-gated marks along its route within documented bounds, invariant to `world.setTimeScale`; clot progression and an internal-only wound stop/suppress marks; death mid-route stops the trail cleanly. Pooling (`Blood.Pool`): a stationary or collapsed bleeder instead grows a clustered pool of layered additive spawns that saturates at the documented per-cluster bound, stops early on clot, survives walk↔stop transitions, drops its cluster at death while the marks persist and age, stays independent between adjacent bleeders, and keeps the same density under `world.setTimeScale` (`blood.getTrailState`, `blood.listTextures`). |
| `blood_decal_probe.py` | #604, #606 | arena | Blood decal model + procedural texture generation: descriptor reuse/eviction, `blood.getRenderQuads()` render records, wetness-tint aging. |
| `blood_impact_probe.py` | #607 | arena | Wound-kind/severity -> impact-blood mapping (`Blood.Impact`) driven through the debug `unit.injure` path. |
| `canteen_instance_probe.py` | #1220 | arena | The two water AI actions mutate the canteen INSTANCE they selected, not the first same-def match: `drinkExecute` drains the chosen full canteen (leaving an earlier empty one alone, credit == water removed), `refillExecute` fills the chosen empty canteen (leaving an earlier full one alone). Opposite inventory orderings — each reproduces one bug and would mask the other. |
| `cargo_capacity_probe.py` | #189 | arena | `depositToCargo` weighs the actual `ItemInstance` (fill + nested contents), not the item def's base weight. |
| `chop_probe.py` | #97 | worldgen (isolated resource root) | Chop-designation layer + chop AI + `wood_log` yield, end to end. |
| `circadian_probe.py` | #611 | arena | Sleep pressure + circadian urge signals: `getCircadianUrge` peaks near dusk, `sleep_pressure` drains monotonically and never regens idle. |
| `circadian_species_probe.py` | #613 | arena | Species-specific circadian phase (bear_brown dawn-peak vs acolyte dusk-peak) from the raw urge signal through `sleepUtility` to the real `go_to_sleep` AI and pose chain. |
| `collapse_crawl_probe.py` | #304 | arena | Collapse↔crawl pose hysteresis in `tickInjuries`. |
| `combat_anim_probe.py` | general combat/animation guard | worldgen | Drives a real fight headless; samples `currentAnim` to verify swing and death animations actually play. |
| `concussion_revive_probe.py` | #304 | arena (shares boot helpers with `collapse_crawl_probe.py`) | `checkRevive` concussion-band hysteresis (companion to `collapse_crawl_probe.py`). |
| `config_migration_probe.py` | #786 | arena (no worldgen) | Pre-#661 legacy config (`config/video.yaml`, `config/keybinds.yaml`, `config/notifications.yaml`) upgrading to the `*.local.yaml` runtime paths: a legacy file with distinct values migrates on boot, a second boot is idempotent, an existing local file wins outright over legacy, and a malformed legacy file falls back safely without ever touching a valid local file. |
| `config_state_probe.py` | #638 | arena (no worldgen) | Local runtime config (`config/video.local.yaml`, `config/keybinds.local.yaml`, `config/notifications.local.yaml`) vs. versioned `_default.yaml` templates: a simulated fresh clone boots on the templates, the settings UI's save paths write the local files, and none of it dirties `git status` for `config/` + `.gitignore`. |
| `construction_probe.py` | #96 | arena (isolated resource root) | `construct_job` AI end-to-end: claim, material sourcing, progress accrual, piece placement, staking, dead-claimant release. |
| `content_registry_probe.py` | #890 | worldgen | The `content-registries` capability's seven registries end-to-end: each written through its public `load*Yaml` verb and read back through its public query (substance, item, equipment class, infection, recipe/repair, location def, loot table incl. `loot.roll`), post-boot re-loading replacing by id rather than freezing or duplicating, and `world.listPlacedLocations` joining real placements against the location-def registry. |
| `consumable_effects_probe.py` | #347 | arena | `scripts/consumable.lua`'s drink mechanism: hydration/caffeine/mood/warmth scaled by a coffee_pot instance's quality (#343) and effective temperature (#344); the caffeine meter's decay + concentration boost and stamina fatigue-offset (`brain.lua`/`unit_resources.lua`). |
| `cooking_probe.py` | #346 | arena | Kitchen workshop + cooking skill/`basic_cuisine` knowledge + `basic_food.yaml` coffee recipe: content shape, all-or-nothing consumption, crafter-derived quality (#343), 100 °C output temperature (#344). |
| `craft_probe.py` | #325, #326, #343, #327 | arena | `craft.*` API: catalogue, execute, work stations, crafter-derived quality, smelting. |
| `craft_bill_probe.py` | #329 | arena | Craft-bill backend (`craft.addBill`/claim/progress/complete verbs) + `craft_job` AI: claim a bill, source inputs from the ground and from cargo storage, work the built station, the fresh output instances laid down at the station (a carried same-def item stays carried), knowledge gate. |
| `crop_probe.py` | #334 | worldgen (isolated resource root) | Row-crop natural placement (`tomato_plant`) + groundcover `world.plantCropAt` (`wheat`) into a `CropPlot`, growth under the real clock, harvest, refusal for a row_crop species, save/load round-trip. |
| `debug_console_boot_probe.py` | #1190 | none (every failing boot dies before the engine action — no world, no GPU, ~13 s) | Required-debug-console boot contract for the two windowless modes. `--headless`/`--offscreen` must FAIL when their only interactive control surface never comes up: port 0 (issue #46's "no TCP listener" sentinel, which belongs to `--dump` alone) is refused before a socket is touched, and a real `Left` from the listener — both an invalid service (`--port -1`) and a genuine `EADDRINUSE` against a port the probe itself holds — aborts the boot. Each case must exit non-zero on its own inside a bounded timeout, print NO `READY` marker on stdout, name the selected mode / effective port / specific cause on stderr, and leave cleanup EVIDENCE rather than a bare vanished process: the pre-thread Lua state's close and the exact worker count torn down (0 for headless, whose first worker is Lua; 1 for offscreen, which starts the input thread first) are each announced by the step that performs them, and offscreen must never reach its engine action, so Vulkan is never initialized. Also pins the two unchanged behaviours: `--dump` still exits 0 with valid JSON and its `READY port=0` marker, and a successful headless bind still reaches `engine.quit()` over the console and exits 0. |
| `disarm_probe.py` | #193 | arena | Disabled-hand auto-drop must re-fire. |
| `etymology_probe.py` | #1104 | worldgen (size 16, one offscreen boot) | Name etymology through the REAL in-game UI, windowless: a world named through the genuine `world.suggestName` -> `world.init` path (so its stored name really was rendered from the expression stored beside it), then all three entry points — the world's own name, a discovered location, and a river reached by selecting one of its visible segments through `world.getRiverAt`'s stable-identity resolution — opening ONE panel, retargeted rather than duplicated. Every control is located through the name plate's and the panel's own `dump()` oracles and clicked with `input.click` at its real interactive bounds, never a hardcoded coordinate. Asserts populated content (stored name, whole gloss, morpheme rows carrying concept/role/realized spelling/canonical free spelling/English lemma) and re-derives #1104 requirement 3 ITSELF — the reported surface tokens must concatenate back to the stored name — rather than trusting the engine's own claim that they do; that a bound form reports its free spelling as ONE morpheme; that a recurrence entry leaks nothing beyond an entity kind and an already-visible name; the honest unavailable state for a CUSTOM-named world (stored name still shown, reason `custom`, no invented morphemes); and that a resize keeps the panel valid and pointed at the same entity while close leaves no rows or stale viewport handle. |
| `expedition_loop_probe.py` | #923 | worldgen (two real engine boots, isolated resource root) | **The expedition arc's final integrated gate** — the whole first expedition as ONE session, from an empty world to a reloaded save: `prepare -> travel -> discover -> extract -> return -> invest`. Eight independently-reported stages (`setup`, `prepare`, `travel`, `extract`, `return`, `save`, `load`, `control`), so a failure names which part of the loop broke. A real `acolyte_portal` placed through `building.canPlaceAt` delivers its OWN six-unit roster (`scripts/building_spawn.lua`); one acolyte secures water by its own `getVisibleTiles` FOV scan, completing the shipped `first_session` tree to its exact expected latched set; a traveller is provisioned off the technomule through `unit.transferItemToUnit`. Two travellers then share ONE identical ~30-tile leg — mustered to a single staging tile and held there by the pause (`unit.setFrozen` is render-only and would report stale positions while the sim kept walking them), then the same verb (`commandMove`) to the same destination, issued in one paused window from the same seeded hunger deficit, both verified inside their carrying capacity — and are measured together once BOTH are inside the ruin's halo in **one coherent snapshot** (a single paired read, revalidated with the simulation stopped, with the control's metrics taken inside that same stopped window — two separate `unit.getInfo` round trips would let a pair that was never inside together satisfy the test), differing ONLY in the food they carried (the canteen is left FULL on both: a dry one puts `refill_canteen` at its 7.5 peak, above `follow_command`, so an unwatered traveller correctly abandons the leg and walks to the lake the scout radioed about — a behavioural difference rather than the supply being measured). Five things that would otherwise leak into the comparison are levelled deliberately: the origin (a shared destination is not a shared journey — hunger drains with time on the road, and an early run departed from 36.4 vs 31.5 tiles out — and a shared *distance* is not enough either, since a radial band is satisfied anywhere on a circle, so the departure check asserts how far apart the two stand as well as how far each must walk; each is pinned on arrival, because a completed move order does not hold position and waiting for both to be near a tile at once can never come true), the verb (`commandMove` walks at `ordered` = comfort x 1.15 while `pickup_ground` walks at `comfort`, so the prepared traveller's retrieval order is issued only AFTER the measurement), encumbrance (an over-encumbered acolyte crawls and its order stall-times-out — calibration observation E1), the observation point (simultaneous containment, since a unit that finishes its move reverts to wander and can drift back out while the other is still walking), and the control's own loot target (see below). The prepared one eats en route through the ordinary AI — watched live as a real `eat_from_inventory` action, so the delta is attributed to a mechanism rather than inferred from a number — and the **unprepared control is measurably worse off at the same observation point** (a predetermined adverse delta in stomach fraction — the metric `docs/expedition_survival_calibration.md` measured actually goes live on a trip this length; water is reported as evidence rather than gated, see the module docstring), so the gate proves preparation matters rather than proving a walk succeeds. The control carries no retrieval target of its own — handing it the ruin's second loot roll would put the loot TABLE inside the experiment, since a ruin can roll food and a control that eats what it finds destroys the measurement. Approach promotes the location instance to `discovered` exactly once with one player event, and per-unit knowledge only for the units that went (the never-went-there control is eligibility-based rather than held: a stay-at-home colonist counts only if it was never observed inside the halo during the leg and is outside it at check time, so one that genuinely wanders to the ruin is excluded rather than read as a leak); the carrier picks up the ruin's OWN seed-stable loot roll (nothing is staged), walks home, and banks it in colony storage from an adjacent tile. A FRESH process then reloads and re-checks every durable identity — the same `(page, instance id)` still `discovered` with `contents_spawned`, the traveller's location knowledge, the exact completed objective-ID set, and the recovered item's instance id / definition / mutable properties / storage ownership — before a different colonist withdraws that exact instance, proving the recovered loot is ordinary colony stock. Never calls `world.setLocationLifecycle` (the expected end state is `discovered`; nothing in the shipped game drives an instance past it) and never stages an item. Prints a `FINGERPRINT` line (ruin instance, anchor, rolled loot, target, colony and water tiles, objective set, per-stage outcomes) so two consecutive fixed-seed runs can be diffed for identity AND result, not just compared on exit status; sampled measurements are printed separately and kept out of it. An unexpected operational failure (dead engine, socket timeout) is recorded against the stage it interrupted, so no run can traceback its way past a PASS summary. |
| `expedition_retrieval_probe.py` | #920 | worldgen (two real engine boots, isolated resource root) | Player-driven remote retrieval and return, end to end against a `radio` the probe stages on the ground inside a real placed `ruin_small` (#921 made ruin contents weighted loot-table draws, so no ruin guarantees a specific item to target), using only the direct-RTS verbs a player already has (`commandPickup` / `commandMove` / adjacent `depositToCargo`) — no caravan or one-click retrieval interface. Capacity is legible BEFORE the trip (an over-capacity order is refused with a player-visible warning naming carrier and item, starts no journey, and the identical order is accepted once one ballast item is shed — so the gate is the live `getCarryingWeight` + `listGround().weight` sum); the carrier then travels tens of tiles over many ticks (no teleport), picks the item up through the real `pickup_ground` action, and the pickup lands a player-facing event naming the item and its carrier; a forced survival need (eating — hunger, not thirst: hydration feeds the consciousness model, so a thirst deep enough to outrank the order can knock the carrier out before it ever drinks) preempts the pending return order and the carrier keeps the item and resumes; the session is saved mid-inbound-leg, a FRESH process loads it with the same instance on the same carrier and the return intent still pending, finishes the walk, and deposits into colony storage; finally a different colonist withdraws that exact instance and drives an existing provenance-blind consumer with it (`notify_allies`' radio branch). Guards the two stall bugs it found: `pickup_timeout` and `maintainTask`'s `TASK_TIMEOUT_SEC` were total-trip budgets that abandoned still-progressing orders at ~21 and ~42 tiles. Both are STALL timers now, reset on a new closest approach and — since #1291 — charged only for time the unit was FREE to pursue the order, so the interruption above costs it nothing however long it lasts (`scripts/unit_ai_stall.lua`; hspec `--match "commanded order stall budget"`). |
| `farm_ai_probe.py` | #336 | worldgen | Farm AI capstone: till -> plant -> grow -> auto-harvest end to end through the real acolyte AI stack, plus `world.plantRowCropAt` and the `findHarvestableFlora` CropPlot scan. |
| `flora_growth_probe.py` | #332 | worldgen | Derived flora growth/age/phase under the advancing calendar; fruiting-window gating; survives save/load. |
| `follow_command_priority_probe.py` | #306 | arena | Follow-command priority against other AI goals. |
| `foraging_probe.py` | #94 | worldgen | Foraging AI + harvestable-flora gating. |
| `infection_probe.py` | #593 | arena | Infection growth / antiseptic prevention / antibiotic cure / sepsis meter, end-to-end. Boots its own engine with `SYNARCHY_INFECTION_TEST_MODE=1` (test-tuned rate/grace, scoped to that one process) so growth is observable in seconds without touching production gameplay. |
| `injury_log_probe.py` | logging arc (general) | arena | Injury-log stream roundtrip: `injury.emit`/`drainEvents`, `unit.injure`, `emitEventForUnit` tagging. |
| `item_instance_probe.py` | #67 | worldgen | Per-instance item identity. |
| `item_temp_probe.py` | #344 | worldgen | Item temperature model. |
| `location_content_probe.py` | #90, #91, #915, #1101 | worldgen + arena | Location content spawning + ruin probe; also the player-wide discovery layer, the per-unit location-knowledge layer beside it, and (#1101) each placed location's name rendered in its world's own generated language — generated name + English gloss on a provenance-bearing world, the `ldLabel` fallback with no gloss on the same seed without one, both surviving save/load and reproduced by regenerating the same seed + language in a fresh process. |
| `location_overlay_probe.py` | #89 | worldgen + arena | World-gen location-overlay placement. |
| `location_stamp_idempotent_probe.py` | #424 | worldgen | Geometry-stamp idempotency survives clearing the anchor floor + save/restart/reload; a never-visited location still stamps on first load. |
| `lua_orphan_prune_probe.py` | #195 | worldgen | Lua per-id AI state is pruned (not inherited by id reuse) after a save load. |
| `lua_strict_msg_probe.py` | #622 | none (no world/scripts needed) | A Haskell exception embedded, unevaluated, in a `LuaToEngineMsg`/`LuaMsg` field must not escape to the consuming thread and crash the whole engine — `engine.setText` with malformed UTF-8 must degrade to a caught Lua error instead. |
| `machine_shop_probe.py` | #591 | arena | Electric furnace `smelt_steel_electric` recipe + the new `machine_shop` building's `machine_wiring`/`machine_electric_motor` recipes, real shipped content built on #590's power-draw mechanism. |
| `medic_coord_probe.py` | squad-medic coordination (general) | arena | `bestMedicFor`/`medicAvailable` distance-discounted selection fix. |
| `mental_efficiency_probe.py` | #353 | arena | Combat/craft mental-effectiveness plumbing end to end: `unit.getMentalEffectiveness` reads the documented 0.75..1.10 values off a real `UnitInstance`; real `craft.addBillProgress` scales by it; a real `craft.executeAt` applies the #353 quality delta on top of #343's skill × knowledge base, clamped; and mean landed-hit damage energy (`combat.drainEvents`) isn't shifted by it (a sanity bound on top of the hspec suite's deterministic proof). |
| `mental_state_probe.py` | #352 | arena | Mental-state threshold ladder over `state_of_mind`: stable/stressed hysteresis, deterministic break episodes (wander/flee forced behaviours), cooldown. |
| `movement_probe.py` | movement arc (general, closed) | arena | Obstacle-course movement (pathing/climbs/falls/ramps) via `movement_arena.lua` courses; `--list` shows courses. |
| `multiworld_save_probe.py` | #214, #219 | worldgen + arena | Multi-world save → quit → restart → load; cross-page entity survival. |
| `offscreen_probe.py` | #650 | offscreen (needs a GPU) | `--offscreen` render mode end to end: windowless Vulkan boot + real UI flow, non-blank screenshot capture, F2 input injection driving the UI, parallel instances on separate ports, and (unless `--skip-worldgen`) a full click-to-generate-world path to the in-game HUD. |
| `persistence_contract_probe.py` | #767 | worldgen (size 8, four real engine boots, isolated resource root) | Compact fresh-process persistence contract smoke: three real fresh-process save → load → save cycles (four engine boots -- one to create the initial save, three more each loading the prior generation and saving the next), each save/load tied to its own `engine.getSaveStatus()`/`getLoadStatus()` request id reaching its terminal phase (not just the file appearing on disk), compared structurally (`SessionSnapshot` `Eq` + `lua.*` payload byte-equality, via `tools/persistence_snapshot.compare_session_files`) through the real production codec, plus reset-policy (seeded with a real unit/building/tile selection + non-default tool mode beforehand, each verified immediately after it's set), a pre-load-only throwaway page proving a load replaces rather than merges, and paused-stability-dwell checks. |
| `persistence_contract_sweep.py` | #767 | worldgen (size 64, four real engine boots, isolated resource root) | Broader persistence contract sweep: the SAME three-cycle/four-boot fresh-process structural comparison against a real generated-world representative scenario (a built craft station running a bill, an acolyte_portal roster + a real unit_ai attack for non-vacuous Lua state, a mine designation, a world identity, a SECOND real saved page with its own identity/visibility — round-6 review — and a pre-load-only throwaway page proving a load replaces rather than merges — round-5 review); actually RUNS (via `run_probes.py --only ... --exact --retries 1`) a DEFAULT set of all 12 cross-referenced probes except `craft_bill` (independently flaky per `ci_probes.py --status`, opt in explicitly) and propagates any failure. All 12 are isolated (round-6 review retrofitted `--resource-root` support into `chop`/`till`/`crop`/`plant`/`construction`/`power`/`transactional_load`/`save_barrier`, which previously wrote into this repo's real `saves/` directory), so `--cross-probe-keys` only ever accepts one of the 12 registered keys. |
| `persistence_integrity_probe.py` | #764 | worldgen (isolated resource root) | Shared save/load integrity graph: a unit's `attackTargetUid` pointing at a destroyed unit survives a real save → quit → restart → load round trip as a non-blocking diagnostic naming the component/kind/id, and a truncated save is rejected with `LoadFailed` while leaving the already-loaded live session's active page/unit state and paused status completely unchanged. |
| `physiology_probe.py` | homeostasis (general) | arena | Thermoregulation/circulation sanity across controlled environments (temperate/arctic/humid-heat). |
| `plant_probe.py` | #335 | worldgen (isolated resource root) | Plant-designation layer: `world.getPlantSuitability` lists both shipped crops sorted best-first, designation refused on an untilled tile / for an unregistered crop name, succeeds on a tilled tile (row_crop and groundcover_crop names both accepted), replace-on-redesignate semantics, save/load. |
| `power_probe.py` | #358 | arena (isolated resource root) | Build-tool-routed power-node placement: `buildTool.commitPlacement` consumes an item off the selected unit for `power.*`-placeable defs, role/parameter reporting, `building.destroy` retires the host's node live (#1206), save → quit → restart → load reconnects the surviving nodes and restores none of the retired one. |
| `power_workshop_probe.py` | #361 | arena | `requires_power` workshop consumer: unpowered `craft.executeAt` refusal, wired-but-uncharged still unpowered, noon flip powers it, `craft_job` AI stalls at 0 progress while browned out and resumes once powered, battery `storedWh` rises/falls over a simulated day/night with the consumer's drain folded into the balance. |
| `preview_cli_probe.py` | #886, #887, #888, #1012, #1191 | none (pre-boot only, no window/engine thread) | `--preview` CLI contract: every explicitly unexposed category name (`equipment`/`hud`/`facemap`/`utility`/`vegetation`) is an ordinary unknown-category error listing exactly the canonical set; every grouped category (`units`/`flora`/`buildings`/`structures`) with no item prints the "select a specific ..." guidance and exits 0; a bare `--preview` errors without falling through to a real boot; a nonexistent/directory/path-escaping simple-category item all reject before ever creating a window; and (#887) an unknown unit, a `units/<name>` carrying path structure or `.`/`..`/absolute traversal, and a unit directory with no `animations/` subtree all reject the same pre-boot way. And (#888) the remaining grouped categories reject the identical pre-boot way: an unknown `flora`/`buildings`/`structures` item, a name carrying path structure or `.`/`..`/absolute traversal, a symlinked item directory, and a FILE where a browsable item directory was expected (`flora/unknown_flora.png`). And (#1012/CH-58) one case per row of `incompatibleFlagTable`: a flag given to a boot mode that does not honour it exits 1 naming both the flag and the selected mode. And (#1191) present-but-malformed VALUES in a mode that DOES honour the flag: every affected spelling (`--seed`/`--worldSize`/`--plates`/`--ages`/`--port`), a flag with no operand at all, empty and unknown `--dump=` layer selections plus empty segments, and malformed and non-positive `--size` each exit 1 pre-boot naming the flag and the offending token, with nothing on stdout — plus the two orderings the fix must preserve (validation runs ahead of mode-specific early exits and regardless of consumption; mode-compatibility rejection still outranks it) and the requirement that omitting a flag still keeps its default. |
| `preview_probe.py` | #886, #887, #888 | real window (no offscreen variant) | `--preview` real-boot browser (a window per target, ~15 boots): simple-category list mode reports boot profile `"preview"` and the parsed target, its discovered entries (`require("scripts.preview_manager").dump()`) match an independently-computed filesystem expectation, the first entry auto-selects and resolves, clicking a different row (located from the dump's own row bounds) changes selection, wheel input changes the reported scroll offset, and a grow/shrink resize reflows without overflowing; focused item mode has no list while its texture resolves; every requested texture path stays under the browsed category's root (#886). Units viewer (#887): the animation list matches a filesystem-derived expectation exactly and in order, the default selection is `idle`/south, effective fps/loop match `data/units/<name>.yaml`, all eight direction cells appear in the game's order with the western three reporting their real mirror source, the frame index advances over wall time, a dump-located row click switches clips and a dump-located mirrored-cell click enlarges that direction, a resize preserves animation/direction/scroll, and a YAML-less unit (`tiller`) falls back to fps=8/loop=true with inferred mirroring. Buildings viewer (#888): the mixed animation-directory + loose-static entry list matches a filesystem+YAML expectation exactly and in order with each row's own static/animation identity, the default selection is the DIRECTORY holding `state_animations.built`'s declared frames (`idle`, not the YAML's `portal-idle` name), its fps/loop come from that entry, the frame index advances, a resize preserves selection/scroll, and a dump-located static-row click selects it and exposes no playback; a building with no `built` state falls back to its `sprite` and still recognizes a YAML-less `demolish/` folder by the numbered-frame convention at fps=8/loop=false; a building with no YAML at all (`dungeon_1`) surfaces its `damaged/` subtree as ordinary statics and defaults to its first entry; `flora/<name>` and `structures/wire` dispatch into the shared simple browser rooted at the item folder; and a final sweep proves every canonical category dispatches with no `placeholder` mode left anywhere. |
| `remote_warning_page_guard_probe.py` | #844 | arena (no worldgen) | Remote-settlement confirmation cross-page guard: `establishHere()` rejects a stale confirmation when the active world page changed while the modal was open (no spawn, `revalidationRejected` with reason `"active world changed"`), while the same-page happy path and `chooseAnotherSite()` cancel remain unaffected. |
| `repair_item_probe.py` | #300 | worldgen | `unit.repairItem` primitive. |
| `repair_probe.py` | #301 | arena | Repair policy layer (station-gated repair on top of #300). |
| `repair_ai_probe.py` | #302 | arena | `repair_job` AI end-to-end: claim, own/equipped/mule-held sourcing, station routing, dead-claimant release, `smith` role weighting. |
| `resource_root_probe.py` | #636 | worldgen (size 64, one dump) | Resource-root launch contract: the built binary run from a temp directory OUTSIDE the repo fails with an actionable error when no root is given, and works (`--dump` JSON via `--resource-root`, `--headless` READY/console/clean quit via `SYNARCHY_ROOT`) when pointed at the checkout. |
| `river_naming_probe.py` | #1102 | worldgen (size 16, three boots) | River identity + naming through the real Lua table: every river carries its `GeoFeatureId` as `id`, a second `world.getRivers()` returns the identical id→geometry association, a language-bearing world names every river with a non-empty `name`/`gloss` whose head morpheme recurs, the same seed with a CUSTOM name leaves both keys absent, and every id/name/gloss survives save → fresh process → load and is reproduced by regenerating the same seed + language. |
| `role_probe.py` | #265 | worldgen | Derived unit-role hysteresis/demotion/work-XP growth. |
| `save_compat_migration_probe.py` | #766 | none (a tracked legacy fixture is placed directly on disk, isolated resource root) | Fresh-process save-compatibility migration: a tracked pre-#760 B1 fixture (`docs/save_compat/manifest.json`'s `b1-initial-session` baseline) loads and publishes through the normal whole-session transaction, the migrated session begins paused and a dwell advances no gameplay date, re-saving under a new slot produces a genuine current-format re-encode (not a copy of the legacy bytes), and a FRESH engine process loads that re-saved file and reaches the same active page — proving the migration survives a real restart, not merely an in-memory decode. |
| `autosave_probe.py` | #913 | worldgen (size 32, isolated resource root with a COPIED `config/`) | Interval autosave end to end in one boot: the shipped default-off config produces neither a request nor a slot across a dwell longer than one configured interval; enabled, a REAL one-minute interval fires and hands an unpaused world back at its exact prior fast-forward time scale, while one that began paused stays paused and zero-scaled; a player pause/resume during the request window suppresses restoration even though the final pause boolean is unchanged (the time scale is the discriminator); an accepted autosave whose storage write fails stays paused and zero-scaled with `engine.getSaveStatus()` carrying the rendered `StoragePhase`; a deadline outside `uiManager.isGameplayView()` skips silently (no request, no failure event, cadence uninterrupted); a `save_load` category configured to pause wins over the restoration; a pre-existing MANUAL save on an `autosave-<n>` name — as a slot directory OR a pre-#762 legacy flat file a published directory would shadow — fails the attempt through `save_load` with nothing overwritten or partially rotated; rotation keeps `autosave-1` newest across generations that all stay classified autosave, a failed write against a FULL family discards and renumbers nothing (publish-then-rotate), a rotation that fails part-way leaves every generation on disk and retries cleanly (retire-by-rename, delete last), and one interrupted AFTER a partial shift resumes without ageing out a second generation; and reducing `rotation_depth` or disabling autosave retains every excess generation untouched. |
| `save_pause_probe.py` | #42 | worldgen | Save/load pause-semantics regression. |
| `save_barrier_probe.py` | #757 | worldgen (isolated resource root) | Coordinated save-owner acknowledgement and paused reload smoke test. |
| `save_storage_probe.py` | #762 | worldgen (size 64, isolated resource root) | Atomic save-storage transaction: a first save publishes with no previous generation, a second save to the same slot retains the first as the previous generation; restart-and-select across constructed on-disk states (missing/truncated/bad-framing/checksum-corrupt authoritative, a stray leftover temp file) always recovers the correct complete generation via the live camera position, never a hybrid; neither generation valid rejects the load outright; `engine.listSaves()` reports a recovered slot's machine-readable status; a real disk-level write failure (directory path pre-occupied) names its storage phase via `engine.getSaveStatus()` and the barrier recovers for a follow-up save. |
| `sleep_probe.py` | #612 | arena | The "go to sleep" AI goal + Sleeping pose end to end: multi-hop lie-down/wake pose chain, `go_to_sleep` goal selection, sleep-pressure regen while asleep, and all three wake conditions. |
| `state_of_mind_probe.py` | #350 | arena | Unified consciousness/mood model (`brain.lua`): fresh-unit baseline, pain-driven concentration/mood/emotional_pain drift, no-hunger-config species fallback, the locomotor-collapse regression guard, and the awareness/perception drift input. |
| `text_encoding_probe.py` | #618, #665 | none (no world/scripts needed) | `TE.decodeUtf8Lenient` sweep across `Engine.Scripting.Lua`: `engine.setText` with a truncated multi-byte UTF-8 sequence (`"caf\195"`) no longer raises a Lua error and the malformed text round-trips through `engine.getText` (#618 Text API), plus the same malformed sequence through the representative non-Text-API `world.show` boundary (#665) proceeds to its normal semantic no-op instead of erroring, plus well-formed control cases and a liveness/responsiveness check. |
| `thermo_altitude_probe.py` | #308 | worldgen (size 128) | Altitude-lapse thermal effect. |
| `thought_probe.py` | #351 | arena | Thought event stream (`thought.emit`/`drainEvents`), STATE/ENVIRONMENTAL thought triggers, state-of-mind-biased selection (mood-weighted valence), and the thought-log data path. |
| `till_probe.py` | #333 | worldgen (isolated resource root) | Till-designation layer + till AI end to end: designate/cancel, fluid-tile exclusion, save/load, autonomous tilling (`world.getVegAt` confirms the flip), idempotent re-sweep. |
| `tutorial_probe.py` | #922 | worldgen (two engine boots, real save/load) | First-session tutorial integration gate: the shipped `data/tutorials/first_session.yaml` branch driven end to end from real gameplay state — a fresh session revealing only the root row, a placed `acolyte_portal` completing the portal objective, one acolyte discovering generated water by FOV scan and radio-sharing it with a second acolyte held immobile with no water anywhere in its own `getVisibleTiles` field of view (so a received source cannot be a second independent discovery, and must be one of the finder's own), the live water/food subobjectives checking one at a time as supplies are restored stepwise, the composite latching when a single acolyte holds both, that latch surviving the supplies being stripped again, and a fresh-process save/load round trip preserving every completed objective while the HUD returns collapsed and the live subobjectives recompute from the loaded world. Injects no tree and stubs no predicate. |
| `tutorial_hud_probe.py` | #960 | worldgen, `--offscreen` (needs-gpu, manual-only) | Tutorial checklist HUD rendered over a real world: collapsed at session entry, real toggle clicks opening/closing the list (located through the module's own `dump()`, never a hardcoded coordinate), a transparent overlay that leaves terrain visible inside the list's rect, a 41-row injected tree scrolling under a real wheel event, and a real click landing on a row still selecting the terrain tile beneath it. |
| `transactional_load_probe.py` | #763 | worldgen (three real engine boots, isolated resource root) | Whole-session load transaction: several deliberately invalid loads (missing save, corrupt save, missing gameplay definition) each leave the current session unchanged and paused, reporting `LoadFailed` via `engine.getLoadStatus()`; mutual exclusion rejects a save mid-load (creating no save, and starting no save transaction), rejects a second concurrent load, keeps the original request authoritative and non-terminal across both, and makes `scripts.pause.set(false)` a complete no-op — all against an in-flight window ESTABLISHED and positively observed by request id via the test-only `debug.armLoadStageGate` staging gate (#1181), never raced for, and failing rather than skipping if that window cannot be established; a successful load REPLACES the complete session (a page live only pre-load, never part of the save, does not survive publication) rather than merging; Haskell and Lua state agree immediately post-publication; a paused dwell advances no gameplay state and unpausing lands on the default time scale; repeated loads accumulate no ghost pages. |
| `transfer_context_menu_probe.py` | #1014, #1085 | worldgen, `--offscreen` (needs-gpu, manual-only) | The "Transfer" context-menu entry end to end: a real right-click on a real built storage building, a real technomule and (since #1085's faction-based widening) a second real player acolyte, with the "Transfer" row located through `ui.dumpWidgets()` by its visible label (never a hardcoded coordinate), the existing "Contents"/"Info" rows still present alongside it, `debug.drainActionOutcomes()` confirming the click routed through the context-menu handler and never fell through to a move order, a real click on the row producing a `scripts.transfer_session` session naming the exact NAMED source/destination endpoints (and no operation field), and the two exclusions that must survive the widening — self-transfer and a non-player-commandable wildlife target both offer no row. |
| `wire_probe.py` | #359 | arena | Wire structure piece: connection-aware autotile shape derivation (adjacency → isolated/end/straight/corner/tee/cross) and the `construct_job` AI placing a real wire tile from a designation. |

Invocation is bare `python3 tools/<name>.py` for sane defaults; most accept
`--port`/`--seed`/`--size` overrides and a handful have scenario-specific
flags (`--course`, `--phase`, `--attacker`/`--target`, ...) — see the
script's header docstring for its exact flag set.

### `run_probes.py` — opt-in aggregate runner

Runs a selection of the probes above and prints a per-probe PASS/FAIL
summary, exiting non-zero if any failed. `python3 tools/run_probes.py
--list` is the authoritative count and listing of registered probes — it's
grown over time (currently in the mid-50s) and this doc doesn't try to
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

# Override --port uniformly across every registered probe
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
# scenario-heavy / targeted / needs-gpu / unclassified)
python3 tools/ci_probes.py --status
```

### `ci_expensive_gates.py` — CI worldgen/graphical selection

Selects the two expensive CI gates that are conditional on pull requests:
quick worldgen-output regression checking and graphical test-suite
compilation. Both run unconditionally after a merge to master. The mapping is
intentionally explicit; add a relevant glob when introducing a new worldgen
output or graphics entry point.

```bash
python3 tools/ci_expensive_gates.py --changed src/World/Geology/Timeline.hs --gate worldgen
python3 tools/ci_expensive_gates.py --self-test
```

## Manual gameplay scenarios (`gameplay_scenarios.py`, #925)

`tools/gameplay_scenarios.py` is a small on-demand runner for *watching*
first-expedition gameplay, not for gating it. **It is deliberately outside
CI**: it is not registered in `run_probes.py`, not classified in
`ci_probes.py`, not invoked by any workflow, and deliberately not named
`*_probe.py` — the probe registry and its classification self-test key off
registered probe names only, so it cannot be selected by the blocking probe
gate. Its **exit status reports setup/runtime failure only, never a
gameplay-balance verdict**: a unit that dies, starves, never reaches a
waypoint or goes untreated is reported as an observation and still exits 0.
Survival-pressure tuning on top of these observations is #919's job.

```bash
python3 tools/gameplay_scenarios.py --list
python3 tools/gameplay_scenarios.py --test expedition
python3 tools/gameplay_scenarios.py --test first-aid
```

Both scenarios boot their own headless engine (default port 9925, `--port`
overrides; never 8008), spawn the real starting party — five acolytes plus
one technomule, player faction, with each def's real `starting_inventory` —
tear the engine down in a `finally`, and save nothing.

- **`expedition`** (~5 min) generates a deterministic world (seed 42, size
  64, 3 plates), derives a base camp and a fixed out-and-back route from
  that world, provisions two acolytes off the STATIONARY mule through
  `unit.transferItemToUnit` (capacity-gated the way the fetch AI gates it),
  then walks the pair 3 × 8 tiles out and straight back under the real
  player move order (`unitAi.commandMove`). It reports, at every waypoint:
  inventory, carrying weight vs capacity, hunger/calories/hydration/
  exhaustion/stamina, blood and bleed rate, pain, wounds and their dressing
  state, the AI's current action/role/treatment claim, and position —
  plus an observations list for anything that ended a trip early.
- **`first-aid`** (~2 min) builds a wide arena ridge, issues the mule's
  pre-stocked `first_aid_kit` to the selected expedition acolyte via the
  same transfer path, walks that acolyte off the ridge for a real fall,
  administers first aid through `unit.treatBleeding` the moment the injury
  lands, and reports the injury, the treatment call's full result
  (part/kind/method/bandages used/attempts/residual seep/message), the
  kit's remaining contents and holder, whether the medic AI claimed the
  patient on its own, and the final unit state.

  Its **pre-fall baseline is captured under a stopped simulation** (#1218).
  `engine.setPaused(true)` goes on before the first spawn and stays on
  through roster materialization, the kit transfer, the baseline read and
  the descent order; `engine.setPaused(false)` is issued immediately after
  that order and everything from there on — the fall, the treatment and the
  medic-AI observations — is live and observational as before. Under the
  hold the AI never ticks, so each acolyte's standing `find_water` goal is
  retired directly against `unit_ai_core`'s own state rather than through
  probelib's `clear_find_water` (which polls for a tick that cannot come).
  The baseline read is bracketed by `engine.isPaused()` checks and asserts
  its own preconditions — the scout exists, sits within `ARRIVAL_TILES` of
  its staging tile, carries zero wounds and holds the issued kit — aborting
  with a `ScenarioError` naming what drifted instead of continuing into an
  ambiguous before/after comparison. The `kit issued, before the fall`
  checkpoint header carries the `engine.isPaused()` value it was recorded
  under, so a run demonstrates the hold rather than implying it. The
  `expedition` scenario's setup is unchanged and still runs live.

An unclassified `tools/*.py` path makes CI's path-selective probe gate fall
back to its full CI-eligible probe set for that PR — that is `ci_probes.py`'s
pre-existing conservative default for unknown paths, not this script running
in CI.

## Playtest harness (`playtest/`)

`tools/playtest/` is the naive-player UX playtest harness (H1, #647 —
epic #641): a lockstep runner that drives a **windowed** instance, hands
each frame to a Codex `gpt-5.6-luna`/medium naive player (screenshot-only,
persona-driven, oracle-blind), injects its chosen `input.*` action, and
records a replayable session trace for the critic (H2). Unlike everything else in
tools/ it deliberately launches a graphical instance (focus-stealing —
run unattended); `--selftest` is its offline CI-safe check and `--smoke`
a scripted no-LLM session. See `tools/playtest/README.md`.

## GUI-attached checks (need a windowed instance)

### `screenshot_check.py`

The #643 gate for `debug.captureScreenshot(path)` — the swapchain PNG
capture verb the UX playtest harness (#641) perceives through. The verb
copies the swapchain image, so it CANNOT work under the GPU-less
`--headless` mode (where it deliberately returns a clear error — that
path is covered by the always-on hspec suite instead). This check
therefore does NOT boot an engine: it **attaches to an already-running
graphical instance** (launch the game normally first) and is human-run
only — never part of CI or `run_probes.py`.

```bash
# with the game running (its console listens on 8008):
python3 tools/screenshot_check.py
python3 tools/screenshot_check.py --port 9008
```

Asserts the reply shape (`{path, width, height}`), PNG validity, IHDR
dims matching the reply, pixels not one uniform color (unfiltered per
the PNG spec, RGB only — the capture forces alpha opaque, so an
all-black frame must still be caught), a clean `{error=...}` on an
unwritable path, and that the instance stays responsive. Colors and
orientation still deserve a human eyeball against the live window; the
pure swizzle/row-order contract is pinned by
`Test.Headless.Graphics.Screenshot` in the hspec suite.

### `input_check.py`

The #644 gate for the `input.*` synthetic-input verbs — the playtest
harness's (#641) actor-output channel. Like `screenshot_check.py` it
**attaches to an already-running graphical instance** (the verbs refuse
under the GPU-less `--headless` mode, where no input thread runs) and
is human-run only. Run it from the main menu for the cleanest routing
assertions:

```bash
python3 tools/input_check.py             # attach to port 8008
python3 tools/input_check.py --port 9008
```

Loads `scripts/input_check_fixture.lua` (UI elements at known
framebuffer coordinates that record every input broadcast) and asserts
end to end: the framebuffer→window DPI conversion via
`engine.getMousePosition`, click-at-pixel activates the element drawn
there, a `{"shift"}` mods click is observed as held shift inside the
click callback, key hold/release state + broadcasts, `input.type` into
a focused text field with Backspace/Enter editing, UI-vs-game scroll
routing, and a full drag with `"game"` down/up route pairing. The
fixture tears itself down afterwards.

### `action_outcome_layer_a_check.py`

The F4 (#646) Layer A gate: `Engine.Input.Thread`'s `ClickRoute`
decision and `scripts/init_mouse.lua`'s tool/selection/deadclick chain
only run on a real GLFW-backed instance, same reason `input_check.py`
is GUI-attached. Reuses `input_check_fixture.lua`'s button rather than
building a second fixture:

```bash
python3 tools/action_outcome_layer_a_check.py             # attach to port 8008
python3 tools/action_outcome_layer_a_check.py --port 9008
```

Injects a click on the fixture button and asserts
`debug.drainActionOutcomes()` drains EXACTLY ONE `"accepted"` record
with `handler == "onInputCheckClick"` — the fixture's actual registered
callback, not just any non-empty handler (a wrong consumer would
otherwise still pass) — then a RIGHT-click on that same left-click-only
button (deterministic: it never registers a right-click handler) and
asserts the same exact callback, proving the no-right-click-handler
route preserves the real consumer's identity instead of a generic
placeholder. Then forces the instance to the main menu
(`uiManager.showMenu("main")` — switches away from whatever the
instance was doing, so run it when that's fine) so an empty-space click
is unambiguously a phantom affordance rather than a legitimate
gameplay deselect, and asserts exactly one `"deadclick"` record. If a
gameplay world is already active on attach, also best-effort exercises
the off-world no-selection right-click deadclick route (a miss here —
the corner happened to show world geometry — is informational, not a
failure, since this script doesn't control camera framing).

### `video_window_check.py`

The #891 gate for the video/window settings path — the modules the
`render-gpu-asset` capability migration narrowed that no automated tier
reaches: `Engine.Graphics.Window.GLFW`, `Engine.Graphics.Vulkan.Swapchain`,
`Engine.Graphics.Vulkan.Recreate` and `Engine.Scripting.Lua.Message.Video`.
`--headless` has no GLFW and no swapchain; `--offscreen` has a GPU but
still no window and no swapchain (see `offscreen_probe.py`'s own
header), so neither tier executes these paths. Like the other checks
here it **attaches to an already-running graphical instance** and is
human-run only — never part of CI or `run_probes.py`.

```bash
python3 tools/video_window_check.py             # attach to port 8008
python3 tools/video_window_check.py --port 9008
```

Asserts `engine.getVideoConfig()`/`getWindowSize()`/`getFramebufferSize()`
read live values; that `engine.setResolution` round-trips through
`Message.Video`'s GLFW write path into both size refs; that toggling
VSync and MSAA rebuilds the swapchain with the instance still
responsive and reporting a sane framebuffer afterwards; that
brightness / pixel-snap / texture-filter each apply cleanly; and that a
real window-mode TRANSITION runs through `handleSetWindowMode` and
back, with `rcWindowSizeRef`/`rcFramebufferSizeRef` live and sane
through every branch. `fullscreen` is never chosen as the transition
target — it switches the monitor's video mode; `borderless` reaches the
same code shape without disrupting the desktop.

From a `windowed` start it is also the #907 regression gate: the round
trip must land the window back on its exact pre-transition SIZE and
POSITION. That bug had `handleSetWindowMode` decide whether to cache the
windowed geometry by reading a `vcWindowMode` that `setWindowModeFn` had
already overwritten with the target mode, so leaving `windowed` skipped
the cache and coming back restored the borderless monitor geometry; the
decision now keys off `wsAppliedMode`, the mode the render thread last
actually applied. Position is read through `debug.getWindowPos()`, the
narrow diagnostic seam added with that fix — `GLFW.getWindowPos` is
main-thread-only, so the Lua thread reads a ref the render thread
publishes, and the script forces a publish (a no-op `setResolution`)
before sampling. From a `borderless` or `fullscreen` start the geometry
is reported but not asserted: `defaultWindowConfig` only applies
`fullscreen` at window creation, so a borderless-configured boot's
reported mode and real window state disagree.

Every setting it touches is captured from the LIVE config first and
restored at the end — never to a hardcoded default, since a user's
persisted `config/video.local.yaml` holds the real values. The
resolution needs two captures, not one: `engine.setResolution` writes
`vcWidth`/`vcHeight` *and* enqueues the GLFW resize, whereas dragging a
window edge moves only the window, so the config dimensions and the
physical window size can legitimately disagree on entry. The script
restores the window with `setResolution` and the config with
`engine.setVideoConfig` (a config-only write), and asserts both — it
cannot pass while having replaced a saved resolution with a transient
window size. Whether the picture still looks right after a swapchain
rebuild is the human eyeball this check exists to prompt.

## Directory layout
```
tools/
├── README.md               (this file)
├── world_audit.py          (audit a single dump)
├── world_determinism.py    (detect race conditions)
├── world_baseline.py       (capture reference outputs)
├── world_check.py          (regression suite runner)
├── test_audit.py           (unit tests)
├── ci_expensive_gates.py   (path selector for CI's worldgen/graphical gates)
├── lua_module_budget.py    (Lua module split line-budget guard)
├── action_outcome_coverage.py (F4 action-outcome verb instrumentation self-audit)
├── language_report.py      (generated-language native-name report/check, #710/#1094/#1095/#1096)
├── run_probes.py           (opt-in aggregate behavior-probe runner)
├── gameplay_scenarios.py   (manual first-expedition scenarios, #925 — outside CI)
├── screenshot_check.py     (GUI-attached debug.captureScreenshot check — see above)
├── video_window_check.py   (GUI-attached video/window settings check, #891 — see above)
├── playtest/               (naive-player UX playtest harness — see above)
├── input_check.py          (GUI-attached input.* injection check — see above)
├── action_outcome_layer_a_check.py (GUI-attached F4 Layer A check — see above)
├── *_probe.py              (headless behavior probes — see above; includes action_outcome_probe.py, #646)
└── baselines/
    ├── _seeds.json         (seed list config)
    └── seed*.json          (per-seed baseline data)
```
