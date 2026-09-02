# Development tools

Python scripts for auditing/regression-testing world generation, and for
driving/verifying engine and game-logic behavior against a real headless
engine instance.

## Pre-push gate: `ci-local.sh`

`make ci` (repo root) runs `tools/ci-local.sh`, which runs the complete local
CI gate: a warning-clean (`-Werror`) build of the library/exe + both test
suites, the headless hspec suite, `test_audit.py`, the unit-asset inventory
gate (`test_pack_atlas.py` + `pack_atlas.py --validate-only --strict`),
`world_check.py --quick`, and the probe-runner self-tests. PR CI is
path-selective for the graphical test-suite build, the quick worldgen check,
and the unit-asset gate, while pushes to master run all three; a green
`make ci` remains a conservative CI prediction.

That "same gate set" claim is now enforced rather than maintained by hand:
`ci_parity_audit.py` (#1355) compares this file's `python3 tools/*.py`
invocations against those of `.github/workflows/ci.yml`'s
`test-and-audits` worker,
at command-and-arguments granularity and in both directions, and fails on
any difference outside a hard-coded exemption list carrying a reason per
entry (CI's path selectors, which `make ci` has nothing to select for). It
runs last in both files and has its own `--self-test`.

The PR-only `behavior-probes` job is deliberately outside that parity
contract: it owns `ci_probes.py --stdin` and `run_probes.py`, restores the
same build caches on an independent runner, and runs in parallel with
`test-and-audits`. The stable `build-test` context aggregates both workers;
that is the single CI verdict consumed by the admin-bypass PR drainer, while
branch protection also requires the probe context directly. The parity
audit structurally pins the aggregate dependencies and both probe commands.
Behavior probes remain opt-in locally.

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

# Accept the reduced coverage of a seed whose baseline is missing
python3 tools/world_check.py --quick --allow-missing-baselines
```

A selected seed whose baseline file is absent is reported `SKIP` and, by
default, fails the run: that seed is never generated or audited, and
baselines are tracked in git, so a missing one means the file was deleted
or a seed was added without capturing it. `--allow-missing-baselines`
accepts that reduced coverage for local exploratory runs — it narrows
nothing else, so an ordinary failure still exits 1. Neither
`.github/workflows/ci.yml` nor `tools/ci-local.sh` passes it.

Exit 0 on pass/improvement, 1 on failure or a missing baseline, 2 on bad
invocation.

### `test_audit.py`
The shared self-test for `world_audit.py`, `world_check.py` and
`world_baseline.py`. Synthetic tile grids and dumps verify that each audit
check identifies the issue it's meant to catch, that the regression
summary, determinism status, content-hash gate and missing-baseline exit
policy decide as documented, and that strict baseline capture refuses what
it must. Sub-second, engine-free, and it never writes under
`tools/baselines/`.

Since #2070 the file is a façade: it composes and runs the ordered group
inventories of six owner modules — `test_audit_categories.py` (the
emitted-category inventory derived from `world_audit.py`'s source),
`test_audit_world_audit.py`, `test_audit_world_check.py`,
`test_audit_content_hash.py`, `test_audit_strict_capture.py` and
`test_audit_missing_baseline.py` — over the shared fixtures and assertion
facility in `test_audit_support.py`. The owners expose no command line, and
the aggregate refuses to run if any owner declares fewer groups than it has
always carried. The command is unchanged:

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

### `pack_atlas.py`
The authoritative unit-animation asset inventory (#1257). `--validate-only`
walks every PNG under
`assets/textures/units/<unit>/animations/<animation>/<direction>/` —
FILESYSTEM-FIRST, so it covers trees no YAML mentions — and checks that each
is owned by exactly one animation-frame declaration in `data/units/*.yaml`,
under either the gameplay `units:` key or the asset-only `asset_units:` key.
The asset-only form is supported for shipped art that should remain outside
the gameplay registry while still being validated and previewable.
Also enforces identifier safety, exact per-animation/per-direction
containment (cross-unit, cross-animation and cross-direction references are
each named), strict three-digit `frame_NNN.png` naming, contiguous
numbering from zero in the declared order, the five-vs-eight direction rule
against `flip`, and the scalar types of `fps` and `loop`.
Reuse of an animation frame as `sprite`,
`directional_sprites` or `portrait` is legal and never reported as a
duplicate. A `--unit` naming neither a declaration nor an asset tree exits
non-zero rather than reporting an empty success.

Validation also opens and decodes every declared frame (#1311). Three checks,
because each covers ground the others cannot: a full decode covers the
compressed pixel stream — truncation, corrupt deflate data, a non-image, and a
valid image of another format renamed `.png`; Pillow's `verify()` CRCs the
chunks, the only thing that catches an intact payload under a wrong checksum;
and `locate_png_stream_end` covers the terminal IEND chunk, which `verify()`
breaks on without checksumming and the decoder never reads, catching both a
tampered terminal checksum and data appended past the image (a second canonical
IEND included, which is why it locates the stream's end rather than comparing
the file's last bytes). Every frame of one animation must then decode to the
same pixel size; frame COUNTS may still differ per direction. Any legitimate
PNG colour type passes, including paletted, greyscale, 16-bit and interlaced.
Content findings are errors with or without `--strict`. Non-animation
textures (`sprite`, `directional_sprites`, `portrait`) stay existence-checked
only — the inventory's scope is `animations/`.

`--compile` (#1258) is the other half: it packs the declared frames into one
lossless PNG atlas per ANIMATION under
`assets/textures/units/<unit>/atlas/`, beside a generated `index.json`
recording rows in a fixed direction order, real per-direction frame counts,
integer cell geometry, mirroring, storage paths, a per-animation source
digest and an atlas content digest, plus separate schema and tool versions.
Rows shorter than the animation's longest are padded with transparent cells
that no frame count can address. Compilation refuses to run on an inventory
that does not validate, writes only artifacts whose content actually changed
(so editing one animation cannot rewrite an unrelated atlas), and removes
obsolete atlases from the unit's own output directory and nowhere else.
Where an index exists, `--validate-only` regenerates it from the sources and
reports staleness, hand edits, missing atlases and tampered pixels. A unit
with no index is valid to THIS tool — an uncompiled tree is a legitimate
working-copy state — but not to the engine, which since #1261 refuses to
register a unit that declares animations and ships no compiled artifacts.
Every shipped declaration, gameplay or asset-only, is compiled and tracked.

PyYAML and Pillow are the dependencies, pinned in
`tools/requirements-assets.txt`. Both are required by validation as well as
compilation: an absent Pillow is a loud error naming the install command,
never a silent skip of the content checks. The whole strict gate over the
4,620-frame corpus runs in about 1.5 s, of which the content pass is about
half a second.

```bash
python3 tools/pack_atlas.py --validate-only --strict
python3 tools/pack_atlas.py --validate-only --unit acolyte
python3 tools/pack_atlas.py --validate-only --root <alternative tree>
python3 tools/pack_atlas.py --compile --unit acolyte
python3 tools/pack_atlas.py --compile --check      # report, write nothing
```

### `test_pack_atlas.py`
Fixture self-test for `pack_atlas.py`. Every case builds a complete isolated
unit tree in a temp directory and runs the real tool against it via `--root`,
so nothing reads or mutates the shipped assets. Each negative case asserts
BOTH a nonzero exit and a diagnostic naming the actual problem; each compiler
SCENARIO drives a real compile and asserts on the emitted pixels, the index
document, or which files a second run actually wrote.

```bash
python3 tools/test_pack_atlas.py
```

Both run unconditionally in `make ci` and post-merge master CI, and
path-selectively on PRs (`ci_expensive_gates.py --gate unit-assets`).

### `action_outcome_coverage.py`
Self-audit (#646) for the F4 action-outcome oracle: greps each registered
commit-boundary verb's own source for its `debug.recordOutcome` /
`pushActionOutcome` call site and reports instrumented yes/no, mirroring
`ci_probes.py --status`'s "make the gap visible" style. The plain report
is not a blocking gate — it always exits 0, because Tier 2/3 verbs are
deliberate fast-follows, not regressions. Verbs that share a file (e.g.
`unitAi.commandMove`/`commandAttack`, `craft.execute`/`executeAt`) are
checked within their OWN function body, not file-wide, so instrumenting
one sibling can't false-positive the other. `--self-test` proves that
scoping actually discriminates, against constructed source strings.

`--verify-tier1` (#1704) is the blocking half, and it is the only one
that reads the real tree: it evaluates the Tier 1 (Layer A) areas ONLY
and exits non-zero when a mapped source file is absent — a producer
renamed or moved out from under the checker — or when a mapped file is
present but a required producer pattern is missing. Each verb declares
the files its check reads, which is what lets the gate tell a stranded
MAPPING (re-point the checker) from deleted INSTRUMENTATION (restore
it); the plain report cannot, and prints `gap` with status 0 for both.
That is how #787's input-thread split left all five Layer A areas
reporting as gaps while every producer was present and passing its own
hspec suite. Run by CI and `make ci`.

```bash
python3 tools/action_outcome_coverage.py
python3 tools/action_outcome_coverage.py --self-test
python3 tools/action_outcome_coverage.py --verify-tier1
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
python3 tools/test_audit.py               # world_audit/check/baseline self-test passes
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
plus `sys.exit(0 or 1)`. Every probe registered in
`probe_runner_registry.PROBES` (the table below) takes `--port` to avoid colliding with another running
instance, defaulting to its own historical fixed port when unset (#723).

"Boot" below is `arena` (flat synthetic terrain via
`scripts/movement_arena.lua`, no world generation — fast) or `worldgen`
(generates a real world at a given seed/size — slower, scales with size).

| Probe | Gates | Boot | Purpose |
|-------|-------|------|---------|
| `action_outcome_probe.py` | #646 | worldgen | F4 action-outcome oracle through the real Lua contract: `debug.recordOutcome` requires kind+outcome, a full record round-trips through `debug.drainActionOutcomes` with every field intact, the ring drains destructively (second drain empty), a mixed tillable/non-tillable sweep reports `partial` with `requested == applied + dropped`, and an unloaded-anchor sweep reports `rejected`. The chop fixture is located with the authoritative `world.findHarvestableFlora(..., 'wood')` query from origins covering the loaded region, and exits **2** (not 1) when no wood-bearing flora is found at all, so an unestablished fixture reads differently from a broken contract (#1398). Also drives starting-portal placement through the real remote-settlement confirmation (#779/#1399): the click records `buildTool.remoteWarning`/`presented` and spawns nothing, and only `establishHere()` records `confirmed` then `buildTool.commitPlacement`/`accepted` with exactly one new building id. |
| `bleeding_trail_probe.py` | #882, #883 | arena | Ongoing bleeding, both halves off one accumulator. Trails (`Blood.Trail`): a moving, externally-bled unit leaves distance/cadence-gated marks along its route within documented bounds, invariant to `world.setTimeScale`; clot progression and an internal-only wound stop/suppress marks; death mid-route stops the trail cleanly. Pooling (`Blood.Pool`): a stationary or collapsed bleeder instead grows a clustered pool of layered additive spawns that saturates at the documented per-cluster bound, stops early on clot, survives walk↔stop transitions, drops its cluster at death while the marks persist and age, stays independent between adjacent bleeders, and keeps the same density under `world.setTimeScale` (`blood.getTrailState`, `blood.listTextures`). |
| `blood_decal_probe.py` | #604, #606 | arena | Blood decal model + procedural texture generation: descriptor reuse/eviction, `blood.getRenderQuads()` render records, wetness-tint aging. |
| `blood_impact_probe.py` | #607 | arena | Wound-kind/severity -> impact-blood mapping (`Blood.Impact`) driven through the debug `unit.injure` path. |
| `canteen_instance_probe.py` | #1220 | arena | The two water AI actions mutate the canteen INSTANCE they selected, not the first same-def match: `drinkExecute` drains the chosen full canteen (leaving an earlier empty one alone, credit == water removed), `refillExecute` fills the chosen empty canteen (leaving an earlier full one alone). Opposite inventory orderings — each reproduces one bug and would mask the other. |
| `cargo_capacity_probe.py` | #189 | arena | `depositToCargo` weighs the actual `ItemInstance` (fill + nested contents), not the item def's base weight. |
| `chop_probe.py` | #97 | worldgen (isolated resource root) | Chop-designation layer + chop AI + `wood_log` yield, end to end. |
| `circadian_probe.py` | #611 | arena | Sleep pressure + circadian urge signals: `getCircadianUrge` peaks near dusk, `sleep_pressure` drains monotonically and never regens idle. |
| `circadian_species_probe.py` | #613 | arena | Species-specific circadian phase (bear_brown dawn-peak vs acolyte dusk-peak) from the raw urge signal through `sleepUtility` to the real `go_to_sleep` AI and pose chain. |
| `collapse_crawl_probe.py` | #304 | arena | Collapse↔crawl pose hysteresis in `tickInjuries`. |
| `combat_anim_probe.py` | general combat/animation guard, #1261, #1396, #1397 | arena | Roster-wide unit-animation STORAGE plus a real fight. Storage (#1261, also runnable alone via `--roster-only`): every `data/units/*.yaml` registers, and for each one the texture-NAME registry (`engine.getTextureHandle` — not `engine.getLoadedTexturePaths()`, which is written only inside the device branch and so is empty headless) shows one `unit_<u>_<anim>_atlas` per animation in its generated index, the direct sprite / directional sprites / portrait it declares, and NOT ONE `unit_<u>_<anim>_<dir>_<i>` for any of the 4,620 declared frames. Then it drives a real fight, samples `currentAnim` to verify a swing animation actually plays, judges the DEATH contract on `unit.getPose` rather than on the animation's name (#1397 — a combatant that ends in the authoritative `dead` pose must show an animation its own declaration maps for that pose under `state_animations`; a removed unit is an accepted outcome, any other pose reports the branch as not exercised, and a violated contract fails the run), reads a live `unit.getFrameSample` off each spawned unit (a real atlas cell: a texture, cell dimensions, and a sub-rect that is not the whole image), and checks every `unit.getAnimDuration` against the index's own real per-direction frame counts / fps — the padded column count would disagree. The fight is gated on two preconditions first (#1396): every tile of the arena footprint the combatants can traverse is flat, dry and loaded, and one coherent live observation shows both alive, the attacker holding the `attack` goal and `attack_target` action on the commanded target, and their Chebyshev separation within its live `unit.getAttackRange`. Failing either exits **2** naming the fixture cause, so a broken fixture can no longer read as the missing swing it is there to catch; `--self-test` exercises those decisions and their exit codes with no engine at all. |
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
| `debug_console_boot_probe.py` | #1190, #1365 | none (no world and no GPU in any boot: every failing boot dies before the engine action, and the four normal boots quit as soon as they have answered — seconds in total) | Required-debug-console boot contract for the two windowless modes. `--headless`/`--offscreen` must FAIL when their only interactive control surface never comes up: port 0 (issue #46's "no TCP listener" sentinel, which belongs to `--dump` alone) is refused before a socket is touched, and a real `Left` from the listener — both an invalid service (`--port -1`) and a genuine `EADDRINUSE` against a port the probe itself holds — aborts the boot. Each case must exit non-zero on its own inside a bounded timeout, print NO `READY` marker on stdout, name the selected mode / effective port / specific cause on stderr, and leave cleanup EVIDENCE rather than a bare vanished process: the pre-thread Lua state's close and the exact worker count torn down (0 for headless, whose first worker is Lua; 1 for offscreen, which starts the input thread first) are each announced by the step that performs them, and offscreen must never reach its engine action, so Vulkan is never initialized. Also pins the two unchanged behaviours: `--dump` still exits 0 with valid JSON and its `READY port=0` marker, and a successful headless bind still reaches `engine.quit()` over the console and exits 0 — its FIRST post-READY command staying that quit (#1283), which is why the widget check below boots separately rather than querying it first. Since #1365 it is also the blocking CI gate for `scripts/ui/*`, and check 8 is what earns that: a normal headless boot is asked, over its own console, which `scripts.ui.*` modules `package.loaded` actually holds, and the gate fails both when one of the 28 modules a non-preview boot loads is absent AND when the boot logged any Lua load/init failure — because `callModuleFunction` and `engine.loadScript` both log and DISCARD a Lua error, so READY plus a clean exit is no evidence at all that the widget kit loaded. Each half of that signal has its own negative regression against a really-broken boot on an alternate resource root: a `focus_indicator` that raises after it has already self-registered into `package.loaded` (a live partial table the presence half cannot see, caught and NAMED by the log half), and a covered module no longer required anywhere in the boot (absent with nothing logged at all, which only the presence half sees). |
| `disarm_probe.py` | #193 | arena | Disabled-hand auto-drop must re-fire. |
| `etymology_probe.py` | #1104, #1604, #1608 | worldgen (size 64, one offscreen boot; `--self-test` boots nothing) | Name etymology through the REAL in-game UI, windowless: a world named through the genuine `world.suggestName` -> `world.init` path (so its stored name really was rendered from the expression stored beside it), then all three entry points — the world's own name, a discovered location, and a river reached by selecting one of its visible segments through `world.getRiverAt`'s stable-identity resolution — opening ONE panel, retargeted rather than duplicated. Every control is located through the name plate's and the panel's own `dump()` oracles and clicked with `input.click` at its real interactive bounds, never a hardcoded coordinate. Asserts populated content (stored name, whole gloss, morpheme rows carrying concept/role/realized spelling/canonical free spelling/English lemma) and re-derives #1104 requirement 3 ITSELF — the reported surface tokens must concatenate back to the stored name — rather than trusting the engine's own claim that they do; that a bound form reports its free spelling as ONE morpheme; that a recurrence entry leaks nothing beyond an entity kind and an already-visible name; the honest unavailable state for a CUSTOM-named world (stored name still shown, reason `custom`, no invented morphemes); and that a resize keeps the panel valid and pointed at the same entity while close leaves no rows or stale viewport handle. Phases 3 and 4 each require the fixture to SUPPLY their entity: since #1604 a world that placed no location, or generated no named river with segments, is a `FIXTURE` failure naming the seed, world size and plate count and exits non-zero, rather than a `SKIP` that let a required phase vanish from an `all checks passed` run — as does phase 3's precondition (`main_world` active, and the chunk load around the camera drained to a zero remainder, which is the world-thread synchronization point that makes an empty location list an answer about the world rather than about timing). Phase 6's long-scroll case is a fixture requirement too, for a different reason (#1608): the phase MANUFACTURES its overflow by rebuilding the HUD at 800x600 under UI scale 4.0 — deliberately out of envelope — so nothing overflowing means that configuration stopped working and the six arrow and wheel routing checks never ran, reported as a `FIXTURE` failure naming the last panel dump's `rowCount`, `visibleRows` and `scrollbar` (each labelled even when the dump carried none of them) rather than a `SKIP`; the forced scale is restored on every exit from the phase, the failing one included. Only phase 5's bound-form/recurrence rows stay legitimately data-dependent and still skip. `--self-test` grades that classification with synthetic readings and no engine at all — phase 6's overflow readings included, since making a live run stop overflowing means breaking the very configuration it depends on — and it is the only coverage the missing-LOCATION branch can get — #997 places a guaranteed location on any world with land, so only a landless one reaches it; the missing-river branch also has a live fixture in `--size 8`, which generates no rivers at all. |
| `expedition_loop_probe.py` | #923, #917 | worldgen (two real engine boots, isolated resource root) | **The expedition arc's final integrated gate** — the whole first expedition as ONE session, from an empty world to a reloaded save: `prepare -> travel -> discover -> extract -> return -> invest`. Eight independently-reported stages (`setup`, `prepare`, `travel`, `extract`, `return`, `save`, `load`, `control`), so a failure names which part of the loop broke. A real `acolyte_portal` placed through `building.canPlaceAt` delivers its OWN six-unit roster (`scripts/building_spawn.lua`); one acolyte secures water by its own `getVisibleTiles` FOV scan, completing the shipped `first_session` tree to its exact expected latched set; a traveller is provisioned off the technomule through `unit.transferItemToUnit`. Two travellers then share ONE identical ~30-tile leg — mustered to a single staging tile and held there by the pause (`unit.setFrozen` is render-only and would report stale positions while the sim kept walking them), then the same verb (`commandMove`) to the same destination, issued in one paused window from the same seeded hunger deficit, both verified inside their carrying capacity — and are measured together once BOTH are at the ruin in **one coherent snapshot** (a single paired read, revalidated with the simulation stopped, with the control's metrics taken inside that same stopped window — two separate `unit.getInfo` round trips would let a pair that was never inside together satisfy the test), differing ONLY in the food they carried (the canteen is left FULL on both: a dry one puts `refill_canteen` at its 7.5 peak, above `follow_command`, so an unwatered traveller correctly abandons the leg and walks to the lake the scout radioed about — a behavioural difference rather than the supply being measured). Five things that would otherwise leak into the comparison are levelled deliberately: the origin (a shared destination is not a shared journey — hunger drains with time on the road, and an early run departed from 36.4 vs 31.5 tiles out — and a shared *distance* is not enough either, since a radial band is satisfied anywhere on a circle, so the departure check asserts how far apart the two stand as well as how far each must walk; each is pinned on arrival by the pause, which remains valid and is now belt-and-braces: since #1216 a completed PLAYER move order holds position, so an arrived traveller no longer drifts on its own — but the pause is still what makes the paired read a single coherent instant, and a survival interrupt can still carry a held unit off its anchor mid-measurement), the verb (`commandMove` walks at `ordered` = comfort x 1.15 while `pickup_ground` walks at `comfort`, so the prepared traveller's retrieval order is issued only AFTER the measurement), encumbrance (an over-encumbered acolyte crawls and its order stall-times-out — calibration observation E1), the observation point (simultaneous containment: the two travellers arrive at different times, and the first one's hold can still be interrupted by its own physiology before the second lands), and the control's own loot target (see below). The prepared one eats en route through the ordinary AI — watched live as a real `eat_from_inventory` action, so the delta is attributed to a mechanism rather than inferred from a number — and the **unprepared control is measurably worse off at the same observation point** (a predetermined adverse delta in stomach fraction — the metric `docs/expedition_survival_calibration.md` measured actually goes live on a trip this length; water is reported as evidence rather than gated, see the module docstring), so the gate proves preparation matters rather than proving a walk succeeds. The control carries no retrieval target of its own — handing it the ruin's second loot roll would put the loot TABLE inside the experiment, since a ruin can roll food and a control that eats what it finds destroys the measurement. Approach promotes the location instance to `discovered` exactly once with one player event, and per-unit knowledge only for the units that went (the never-went-there control is eligibility-based rather than held: a stay-at-home colonist counts only if it was never observed at the ruin during the leg and is away from it at check time, so one that genuinely wanders to the ruin is excluded rather than read as a leak); the carrier picks up the ruin's OWN seed-stable loot roll (nothing is staged), walks home, and banks it in colony storage from an adjacent tile. Since #917 it also recovers the ruin's GUARANTEED significant item on that same trip through the same player gesture — deliberately not the measured target, because `processing_unit` is a Materials def whose `store_materials` AI would bank it autonomously mid-return — and that pickup is what CLEARS the ruin: the selected ruin is zero-occupant, so its encounter half has been complete since placement and the item is the only outstanding condition, with exactly one clearance notice emitted. A FRESH process then reloads and re-checks every durable identity — the same `(page, instance id)` still `cleared` with `contents_spawned` and its significant obligation still taken under the same physical instance id, its one clearance notice recorded as spent and re-announcing nothing, the traveller's location knowledge, the exact completed objective-ID set, and both recovered items' instance id / definition / mutable properties / storage ownership — before a different colonist withdraws that exact instance, proving the recovered loot is ordinary colony stock. Never calls `world.setLocationLifecycle` (the expected end state is `cleared`, reached entirely through real gameplay since #917) and never stages an item. Prints a `FINGERPRINT` line (ruin instance, anchor, rolled loot, target, the guaranteed item's def and physical instance id, colony and water tiles, objective set, per-stage outcomes) so two consecutive fixed-seed runs can be diffed for identity AND result, not just compared on exit status; sampled measurements are printed separately and kept out of it. An unexpected operational failure (dead engine, socket timeout) is recorded against the stage it interrupted, so no run can traceback its way past a PASS summary. Since #2092 the scenario body lives in the `tools/expedition_loop/` package and this file is the facade: CLI parsing, the ordered two-engine lifecycle, stage dispatch, the `FINGERPRINT` line and the aggregate exit, and nothing else. The owners are `harness` (checks, isolation, bootstrap, the shared state record, the one fingerprint accumulator), `readers` (shared engine queries and geometry), `constants`, and one module per stage group — `setup`, `prepare`, `travel` (which also owns the `control` measurement scored from its own paired samples), `extract` (which also owns the `return` leg), `persistence` (`save` and `load`). Those are LIBRARIES, deliberately not named `*_probe.py`: the command, the single `expedition_loop` registration, the world, the experimental control, the save handoff and the fingerprint all stay singular, and adding a stage is one new owner module plus one entry in `STAGES` plus one call in the facade. |
| `expedition_retrieval_probe.py` | #920 | worldgen (two real engine boots, isolated resource root) | Player-driven remote retrieval and return, end to end against a `radio` the probe stages on the ground inside a real placed `ruin_small` (#921 made ruin contents weighted loot-table draws, so no ruin guarantees a specific item to target), using only the direct-RTS verbs a player already has (`commandPickup` / `commandMove` / adjacent `depositToCargo`) — no caravan or one-click retrieval interface. That final deposit is the LAX verb rather than a player gesture: #1249 retired the adjacent "Store in <cargo>" menu entry that made the identical call, so the Chebyshev-≤1 rule asserted around it here is this probe's own (the player's own way to bank it is a queued Store order). Capacity is legible BEFORE the trip (an over-capacity order is refused with a player-visible warning naming carrier and item, starts no journey, and the identical order is accepted once one ballast item is shed — so the gate is the live `getCarryingWeight` + ground-row `weight` sum, read since #1666 through `item.getGroundForUnit` on the carrier's own page); the carrier then travels tens of tiles over many ticks (no teleport), picks the item up through the real `pickup_ground` action, and the pickup lands a player-facing event naming the item and its carrier; a forced survival need (eating — hunger, not thirst: hydration feeds the consciousness model, so a thirst deep enough to outrank the order can knock the carrier out before it ever drinks) preempts the pending return order and the carrier keeps the item and resumes; the session is saved mid-inbound-leg, a FRESH process loads it with the same instance on the same carrier and the return intent still pending, finishes the walk, and deposits into colony storage; finally a different colonist withdraws that exact instance and drives an existing provenance-blind consumer with it (`notify_allies`' radio branch). Guards the two stall bugs it found: `pickup_timeout` and `maintainTask`'s `TASK_TIMEOUT_SEC` were total-trip budgets that abandoned still-progressing orders at ~21 and ~42 tiles. Both are STALL timers now, reset on a new closest approach and — since #1291 — charged only for time the unit was FREE to pursue the order, so the interruption above costs it nothing however long it lasts (`scripts/unit_ai_stall.lua`; hspec `--match "commanded order stall budget"`). |
| `farm_ai_probe.py` | #336 | worldgen | Farm AI capstone: till -> plant -> grow -> auto-harvest end to end through the real acolyte AI stack, plus `world.plantRowCropAt` and the `findHarvestableFlora` CropPlot scan. |
| `flora_growth_probe.py` | #332, #1711 | worldgen | Derived flora growth/age/phase under the advancing calendar; fruiting-window gating; every shipped `data/flora/*.yaml` still registers; the regrowth timer gates a repeat harvest and only a game-time tick reopens the tile; survives save/load. |
| `follow_command_priority_probe.py` | #306 | arena | Follow-command priority against other AI goals. |
| `foraging_probe.py` | #94 | worldgen | Foraging AI + harvestable-flora gating. |
| `infection_probe.py` | #593 | arena | Infection growth / antiseptic prevention / antibiotic cure / sepsis meter, end-to-end. Boots its own engine with `SYNARCHY_INFECTION_TEST_MODE=1` (test-tuned rate/grace, scoped to that one process) so growth is observable in seconds without touching production gameplay. |
| `injury_log_probe.py` | logging arc (general) | arena | Injury-log stream roundtrip: `injury.emit`/`drainEvents`, `unit.injure`, `emitEventForUnit` tagging. |
| `item_instance_probe.py` | #67 | worldgen | Per-instance item identity. |
| `item_temp_probe.py` | #344 | worldgen | Item temperature model. |
| `location_content_probe.py` | #90, #91, #915, #1101, #1230, #917 | worldgen + arena | Location content spawning + ruin probe; also the player-wide discovery layer (sight-based since #1230 — a location is revealed when a player-owned unit's night-aware visible tiles touch its stored bounds, so the negative cases are derived from the sight radius rather than a discovery halo, which no longer exists), the per-unit location-knowledge layer beside it, and (#1101) each placed location's name rendered in its world's own generated language — generated name + English gloss on a provenance-bearing world, the `ldLabel` fallback with no gloss on the same seed without one, both surviving save/load and reproduced by regenerating the same seed + language in a fresh process. Since #917 it also covers guaranteed SIGNIFICANT contents and the compound clearance predicate: every ruin owes exactly one `processing_unit` bound at spawn to its own physical `iiInstanceId` (distinct across obligations), no ruin is clearance-satisfied while that item is still on the floor — including a zero-nomad roll, whose encounter half is already complete — and a NON-PLAYER faction's pickup latches the same durable `taken` state without clearing a ruin whose roster is still alive. The file itself is the façade (CLI, artifact guard, the eight-process sequence, the helpers other probes import); the scenario owners — content/persistence, discovery/knowledge, content dispatch/rejection, naming — live under `tools/location_content/` beside the shared invocation infrastructure (#2095). |
| `location_overlay_probe.py` | #89 | worldgen + arena | World-gen location-overlay placement. |
| `location_stamp_idempotent_probe.py` | #424, #1575 | worldgen | Geometry-stamp idempotency survives clearing the anchor floor + save/restart/reload; a never-visited location still stamps on first load; and the 5x5 footprint under the tested room really materialized level, with no levelling edit the engine refused. |
| `lua_orphan_prune_probe.py` | #195, #1589 | worldgen | Lua per-id AI state is pruned (not inherited by id reuse) after a save load, and a stale reference from EVERY declared `unit_ai` family — planted before the save — is cleared by the automatic post-load reconcile. |
| `lua_strict_msg_probe.py` | #622 | none (no world/scripts needed) | A Haskell exception embedded, unevaluated, in a `LuaToEngineMsg`/`LuaMsg` field must not escape to the consuming thread and crash the whole engine — `engine.setText` with malformed UTF-8 must degrade to a caught Lua error instead. |
| `lunge_probe.py` | #1713 | arena | The combat lunge's leap → land → strike sequence, on a `red_squirrel` — attack range ~0.12 tiles, so `tryLunge` is the path it actually takes, and intelligence ~0.2, which puts it in `shouldLunge`'s instinct regime. Grades the **`combat.attack` boundary**: a Lua wrapper records every call's argument tuple and never delegates (resolving would kill the subject within seconds and would stamp `uiLastAttackerUid`, opening the retaliation branch that replaces the target under the very binding case B tests), and each case matches on the launch's own identity triple — launched target, stored reach, stored impact speed — snapshotted the moment phase `"air"` is observed. That is what separates the landing strike from the two other calls the same code makes: the `d < 0.5` in-place pounce (reach = the full `jr.height`) and an ordinary swing (no reach at all), neither of which a bare "an attack happened" count could tell apart — which is exactly the weakness of `mental_state_probe.py`'s lash-out coverage, whose acolytes never reach the lunge path in the first place. Four cases: a launched lunge lands and strikes **once** with its stored arguments; a target **replaced** mid-flight cancels rather than handing a substitute a free momentum strike; a lunge whose own clock is **aged** past `LUNGE_TIMEOUT_SEC` fires nothing; and a launch the engine **never lifts** (`unit.jump` returns true, `Unit.Thread.Command.Motion` rejects it) never sets `lungeSawAir` and so never strikes. Every case also asserts all **seven** lunge fields are cleared, read from the module's own `LUNGE_FIELDS` list so the probe cannot drift from the code — `lungeTarget` is a persisted typed reference, so a lunge that ends without clearing strands it where a save picks it up. A fixture failure reports unreached checks as `NOT RUN` (exit 2); `--self-test` drives that accounting and the identity match with no engine. |
| `machine_shop_probe.py` | #591 | arena | Electric furnace `smelt_steel_electric` recipe + the new `machine_shop` building's `machine_wiring`/`machine_electric_motor` recipes, real shipped content built on #590's power-draw mechanism. |
| `meal_waste_probe.py` | #1219 | arena | Stop-before-waste meal policy: a hungry acolyte withholds the ration its stomach can no longer hold most of (zero `unit.feed`/`mealSalt` side effects for the withheld item), a part-full quinoa sack still finishes that same meal, the first item of a meal stays exempt, a near-starving unit still eats, and the 10-feed bound plus the eat/forage entry gates are unchanged. |
| `medic_coord_probe.py` | squad-medic coordination (general) | arena | `bestMedicFor`/`medicAvailable` distance-discounted selection fix. |
| `mental_efficiency_probe.py` | #353 | arena | Combat/craft mental-effectiveness plumbing end to end: `unit.getMentalEffectiveness` reads the documented 0.75..1.10 values off a real `UnitInstance`; real `craft.addBillProgress` scales by it; a real `craft.executeAt` applies the #353 quality delta on top of #343's skill × knowledge base, clamped; and mean landed-hit damage energy (`combat.drainEvents`) isn't shifted by it (a sanity bound on top of the hspec suite's deterministic proof). |
| `mental_state_probe.py` | #352 | arena | Mental-state threshold ladder over `state_of_mind`: stable/stressed hysteresis, deterministic break episodes (wander/flee forced behaviours), cooldown. |
| `movement_probe.py` | movement arc (general, closed) | arena | Obstacle-course movement (pathing/climbs/falls/ramps) via `movement_arena.lua` courses; `--list` shows courses. |
| `multiworld_save_probe.py` | #214, #219 | worldgen + arena | Multi-world save → quit → restart → load; cross-page entity survival. |
| `offscreen_probe.py` | #650 | offscreen (needs a GPU) | `--offscreen` render mode end to end: windowless Vulkan boot + real UI flow, non-blank screenshot capture, F2 input injection driving the UI, parallel instances on separate ports, and (unless `--skip-worldgen`) a full click-to-generate-world path to the in-game HUD. |
| `persistence_contract_probe.py` | #767 | worldgen (size 8, four real engine boots, isolated resource root) | Compact fresh-process persistence contract smoke: three real fresh-process save → load → save cycles (four engine boots -- one to create the initial save, three more each loading the prior generation and saving the next), each save/load tied to its own `engine.getSaveStatus()`/`getLoadStatus()` request id reaching its terminal phase (not just the file appearing on disk), compared structurally (`SessionSnapshot` `Eq` + `lua.*` payload byte-equality, via `tools/persistence_snapshot.compare_session_files`) through the real production codec, plus reset-policy (seeded with a real unit/building/tile selection + non-default tool mode beforehand, each verified immediately after it's set), a pre-load-only throwaway page proving a load replaces rather than merges, and paused-stability-dwell checks. |
| `persistence_contract_sweep.py` | #767 | worldgen (size 64, four real engine boots, isolated resource root) | Broader persistence contract sweep: the SAME three-cycle/four-boot fresh-process structural comparison against a real generated-world representative scenario (a built craft station running a bill, an acolyte_portal roster + a real unit_ai attack for non-vacuous Lua state, a mine designation, a world identity, a SECOND real saved page with its own identity/visibility — round-6 review — and a pre-load-only throwaway page proving a load replaces rather than merges — round-5 review); actually RUNS (via `run_probes.py --only ... --exact --retries 1`) a DEFAULT set of all 12 cross-referenced probes except `craft_bill` (independently flaky per `ci_probes.py --status`, opt in explicitly) and propagates any failure. All 12 are isolated (round-6 review retrofitted `--resource-root` support into `chop`/`till`/`crop`/`plant`/`construction`/`power`/`transactional_load`/`save_barrier`, which previously wrote into this repo's real `saves/` directory), so `--cross-probe-keys` only ever accepts one of the 12 registered keys. |
| `persistence_integrity_probe.py` | #764, #1484 | worldgen (isolated resource root) | Shared save/load integrity graph: a unit's `attackTargetUid` pointing at a destroyed unit survives a real save → quit → restart → load round trip as a non-blocking diagnostic naming the component/kind/id, and a truncated save is rejected with `LoadFailed` while leaving the already-loaded live session's active page/unit state and paused status completely unchanged. Also the converse (#1484): an acolyte's cached `buildTarget` whose construction site was destroyed is cleared by the AI's own next ticks, so the same save carries no `lua.unit_ai` `dangling-reference` diagnostic for that unit/building pair. |
| `physiology_probe.py` | homeostasis (general) | arena | Thermoregulation/circulation sanity across controlled environments (temperate/arctic/humid-heat). |
| `plant_probe.py` | #335 | worldgen (isolated resource root) | Plant-designation layer: `world.getPlantSuitability` lists both shipped crops sorted best-first, designation refused on an untilled tile / for an unregistered crop name, succeeds on a tilled tile (row_crop and groundcover_crop names both accepted), replace-on-redesignate semantics, save/load. |
| `position_hold_probe.py` | #1216 | arena | **Position hold after a completed player move order (SURV-4).** Three acolytes share one flat arena with the real AI loop driving all of them, unpaused, every window spanning many thought cadences: a commanded acolyte anchors at the tile the order named and stays inside `TASK_ARRIVAL_TILES` (read from the shipped constant, not restated) with zero drift, while a never-commanded control in the same world visibly wanders and takes its own designation — without that control a dead AI tick would pass every containment check. Work suppression is a SAME-UNIT A/B, the only version that proves anything: one mine designation row spans both anchors, the held unit's only action for the whole window is `hold_position`, and the same unit takes the same row within seconds of `unitAi.releaseHold`. The survival interrupt is a dry canteen (`refill_canteen` peaks at 7.5 above the hold's 7.0 with no hydration deficit — `docs/engine_contracts.md` records why seeding thirst instead knocks the unit unconscious), traced as ONE excursion: ~5.5 tiles off the anchor to a real lake tile, a real refill, and back inside the radius holding again — an interrupt drunk in place would prove nothing about the return. Then the two boundaries: a new player `commandMove` clears the hold in the same breath and re-anchors on arrival, and an INTERNAL `commandMove(..., internal=true)` — `scripts/building_spawn.lua`'s portal walk-out — arrives and stays autonomous. Requirement 5's stall-timeout half is deliberately hspec's (`--match "position hold"`): this engine's units climb walls, wade oceans and outrun a zeroed `max_speed`, so every terrain fixture for an unreachable target is really a test of the pathfinder. The one staged fact is what the unit KNOWS (the lake is written into `knownWaterSources` rather than waiting on a facing-cone FOV scan); what it does with that is entirely the shipped AI's. |
| `power_probe.py` | #358 | arena (isolated resource root) | Build-tool-routed power-node placement: `buildTool.commitPlacement` consumes an item off the selected unit for `power.*`-placeable defs, role/parameter reporting, `building.destroy` retires the host's node live (#1206), save → quit → restart → load reconnects the surviving nodes and restores none of the retired one. |
| `power_workshop_probe.py` | #361, #590, #796 | arena | Job-dependent recipe `power_draw`: the synthetic workshop carries no `power_drain`, so demand exists only while a bill is claimed AND actively worked (`cbWorking`) — `drainW` 0 on claim alone, the recipe's draw once working, unchanged across a pause, 0 on release — and a fully generating station with no bill claimed still reports `drainW == 0`. Around that: unpowered `craft.executeAt` refusal, wired-but-uncharged still unpowered, noon flip powers it, a bare gate query for a second recipe summing its draw with the already-active bill's at the same station, a paused continuing bill whose completed cycle clears the claim and the demand with it (#796), `craft_job` AI stalls at 0 progress while browned out and resumes once powered, and battery `storedWh` rises/falls over a simulated day/night driven by that real active job's draw. `bdPowerDrain` / `power.isBuildingPowered` — #361's always-on building-consumer model — remain for a hypothetical non-crafting device and are trivially true throughout here, not what this probe exercises. |
| `preview_cli_probe.py` | #886, #887, #888, #1012, #1086, #1191 | none (pre-boot only, no window/engine thread) | `--preview` CLI contract: every explicitly unexposed category name (`equipment`/`hud`/`facemap`/`utility`/`vegetation`) is an ordinary unknown-category error listing exactly the canonical set; every grouped category (`units`/`flora`/`buildings`/`structures`) with no item prints the "select a specific ..." guidance and exits 0; a bare `--preview` errors without falling through to a real boot; a nonexistent/directory/path-escaping simple-category item all reject before ever creating a window; and (#887) an unknown unit, a `units/<name>` carrying path structure or `.`/`..`/absolute traversal, and a unit directory with no `animations/` subtree all reject the same pre-boot way. And (#888) the remaining grouped categories reject the identical pre-boot way: an unknown `flora`/`buildings`/`structures` item, a name carrying path structure or `.`/`..`/absolute traversal, a symlinked item directory, and a FILE where a browsable item directory was expected (`flora/unknown_flora.png`). And (#1012/CH-58) one case per row of `incompatibleFlagTable`: a flag given to a boot mode that does not honour it exits 1 naming both the flag and the selected mode. And (#1191) present-but-malformed VALUES in a mode that DOES honour the flag: every affected spelling (`--seed`/`--worldSize`/`--plates`/`--ages`/`--port`), a flag with no operand at all, empty and unknown `--dump=` layer selections plus empty segments, and malformed and non-positive `--size` each exit 1 pre-boot naming the flag and the offending token, with nothing on stdout — plus the two orderings the fix must preserve (validation runs ahead of mode-specific early exits and regardless of consumption; mode-compatibility rejection still outranks it) and the requirement that omitting a flag still keeps its default. And (#1086) boot-mode PRECEDENCE: argv naming two competing mode selectors resolves to the higher-precedence one — one case per boundary of `language-report > dump > preview > offscreen > headless`, each asserted through the rejection's exact stderr line and exit code, which is what the single `App.Cli.selectBootMode` encoding makes trustworthy as a statement about the mode that would actually have booted; plus a real `--dump=Elevation,ICE` run proving an explicit, mixed-case layer selection still emits exactly those layers' tile fields and no others. |
| `preview_probe.py` | #886, #887, #888, #1907 | hidden, non-activating real window (still needs GPU; no offscreen variant) | `--preview` real-boot browser (`SYNARCHY_PREVIEW_HIDDEN=1`, a hidden window per target, ~28 boots): simple-category list mode reports boot profile `"preview"` and the parsed target, its discovered entries (`require("scripts.preview_manager").dump()`) match an independently-computed filesystem expectation, the first entry auto-selects and resolves, clicking a different row (located from the dump's own row bounds) changes selection, wheel input changes the reported scroll offset, and a grow/shrink resize reflows without overflowing; focused item mode has no list while its texture resolves; every requested texture path stays under the browsed category's root (#886). Units viewer (#887): the animation list matches a filesystem-derived expectation exactly and in order, the default selection is `idle`/south, effective fps/loop match `data/units/<name>.yaml`, all eight direction cells appear in the game's order with the western three reporting their real mirror source, the frame index advances over wall time, a dump-located row click switches clips and a dump-located mirrored-cell click enlarges that direction, a resize preserves animation/direction/scroll, and a PROMOTED tree (`tiller`, declared under `asset_units:` by #1257 and under `units:` since #1261) reports its DECLARED fps=8/loop=true/flip=true with W/SW/NW mirrored from their eastern counterparts and is atlas-backed like any other unit. #1261 adds a roster phase: one boot per remaining declared unit (the list read from `data/units/`, never written in the probe), asserting the animation list still equals the filesystem expectation, every animation reports atlas storage naming its own compiled atlas from the index, and nothing outside that unit's own directory loaded. Buildings viewer (#888): the mixed animation-directory + loose-static entry list matches a filesystem+YAML expectation exactly and in order with each row's own static/animation identity, the default selection is the DIRECTORY holding `state_animations.built`'s declared frames (`idle`, not the YAML's `portal-idle` name), its fps/loop come from that entry, the frame index advances, a resize preserves selection/scroll, and a dump-located static-row click selects it and exposes no playback; a building with no `built` state falls back to its `sprite` and still recognizes a YAML-less `demolish/` folder by the numbered-frame convention at fps=8/loop=false; a building with no YAML at all (`dungeon_1`) surfaces its `damaged/` subtree as ordinary statics and defaults to its first entry; `flora/<name>` and `structures/wire` dispatch into the shared simple browser rooted at the item folder; and a final sweep proves every canonical category dispatches with no `placeholder` mode left anywhere. Centered bounded zoom (#1907): one boot per display kind (bare list, focused item, unit enlarged, building, flora item, structure item), driving the REAL wheel pipeline with `input.moveMouse` + `input.scroll` over the dump-reported zoom REGION rather than any hardcoded coordinate — a session starts at multiplier 1, delta MAGNITUDE is honoured rather than reduced to a sign, both limits clamp exactly at 1/8 and 1 with further input consumed and stable, the rendered dimensions are one eighth of the fitted ones at the floor and exactly the fitted ones back at the ceiling, the sprite stays centered and wholly inside its region throughout, plain and a really-held-Shift wheel behave identically, a wheel over a located list row moves the list and never the zoom while a wheel over the pane moves the zoom and never the list (including once saturated), the object-identity rule resets on a different BARE-category texture and preserves for a unit animation / building entry / flora piece, and a resize preserves the multiplier while recomputing the region. |
| `remote_warning_page_guard_probe.py` | #844 | arena (no worldgen) | Remote-settlement confirmation cross-page guard: `establishHere()` rejects a stale confirmation when the active world page changed while the modal was open (no spawn, `revalidationRejected` with reason `"active world changed"`), while the same-page happy path and `chooseAnotherSite()` cancel remain unaffected. |
| `repair_item_probe.py` | #300 | worldgen | `unit.repairItem` primitive. |
| `repair_probe.py` | #301 | arena | Repair policy layer (station-gated repair on top of #300). |
| `repair_ai_probe.py` | #302 | arena | `repair_job` AI end-to-end: claim, own/equipped/mule-held sourcing, station routing, dead-claimant release, `smith` role weighting. |
| `resource_root_probe.py` | #636, #1949 | worldgen (size 64, one dump) | Resource-root launch contract: the built binary run from a temp directory OUTSIDE the repo fails with an actionable error when no root is given, and works (`--dump` JSON via `--resource-root`, `--headless` READY/console/clean quit via `SYNARCHY_ROOT`) when pointed at the checkout. Also, run from the REPO so the cwd cannot answer for it, an explicitly empty `--resource-root` operand exits 1 naming the flag and the empty operand with no stdout, with and without a valid `SYNARCHY_ROOT` (#1949). |
| `retaliation_swap_probe.py` | #1483, #1578 | arena | Mid-fight retaliation target swap in `attack_target`'s shared execute. Every check is graded from ONE atomic console chunk that re-establishes the branch's preconditions and then drives a single `unitAi.update(dt)` under `pcall`, so nothing drifts between the precondition and the grade: `scripts.unit_ai_combat` exports one 3 s window the consumer module can see; a REAL hit from a third live unit makes it the recorded recent attacker; with that attacker live, non-collapsed, not a technomule and inside `unit.getAttackRange(subject) + 0.5`, the subject retargets onto it, its own tick reaches a post-execute completion point (`nextActionAt` advances), and a SENTINEL unit ordered after it — the order forced by wrapping `unit.getAllIds`, which the engine builds from `HashMap.keys` with no spawn-order contract — is reached by that same invocation, with no `Lua error in update()` logged across a window of natural ticks either; and a hit staged far outside the window swaps nothing while still completing its tick (asserting only the unchanged target would pass on the aborting code). `mental_state_probe.py`'s collapsed-attacker case cannot reach this comparison — `attPose ~= "collapsed"` short-circuits first. Each window owns its OWN quartet and stages its OWN real hit (#1578), so the stale case no longer inherits what the fresh case's combat cost a shared subject; the staging leg's damage is bounded on both axes — `neuter` writes `strength_base`, the one strength input neither the body recompute nor `starvation.refreshStrength` overwrites, and the staging wrap stops `combat.attack` at the first swing that lands — and each window then dresses the bleeding, watches its units back onto their feet, and captures the subject's POSE AND BLOOD FRACTION inside the same atomic pre-grade chunk, refusing to grade below the 50 % rise threshold. Every check is declared up front, so a fixture failure reports the ones it left unreached as `NOT RUN` (exit 2) instead of omitting them; `--self-test` drives that accounting with no engine. |
| `river_naming_probe.py` | #1102 | worldgen (size 16, three boots) | River identity + naming through the real Lua table: every river carries its `GeoFeatureId` as `id`, a second `world.getRivers()` returns the identical id→geometry association, a language-bearing world names every river with a non-empty `name`/`gloss` whose head morpheme recurs, the same seed with a CUSTOM name leaves both keys absent, and every id/name/gloss survives save → fresh process → load and is reproduced by regenerating the same seed + language. |
| `role_probe.py` | #265 | worldgen | Derived unit-role hysteresis/demotion/work-XP growth. |
| `save_compat_migration_probe.py` | #766, #1485 | none (a tracked fixture is placed directly on disk, isolated resource root) | Fresh-process save-compatibility migration, for EVERY `complete-session` fixture `docs/save_compat/manifest.json` declares (the pre-#760 B1 envelope through every later baseline's own session): each loads and publishes through the normal whole-session transaction, the migrated session begins paused and a dwell advances no gameplay date, re-saving under a new slot produces a genuine current-format re-encode (not a copy of the input bytes), and a FRESH engine process loads that re-saved file and reaches the same active page — proving the migration survives a real restart, not merely an in-memory decode. Each engine is provisioned with the SAME registry families (and order) `scripts/startup_loader.lua`'s `queueNormalProfile` loads, which a headless boot never runs (#1485); `--self-test` verifies that plan and its startup-loader parser with no engine at all. A fixture whose load is not accepted, or does not publish, stops there (#1486): that failure is the last check reported for it, the stages it made unreachable are listed as `[SKIP]` diagnostics (never a pass, never a failure), its cleanup and the rest of the sweep are unaffected, and `--self-test` drives both prerequisite branches through injected doubles. |
| `autosave_probe.py` | #913 | worldgen (size 32, isolated resource root with a COPIED `config/`) | Interval autosave end to end in one boot: the shipped default-off config produces neither a request nor a slot across a dwell longer than one configured interval; enabled, a REAL one-minute interval fires and hands an unpaused world back at its exact prior fast-forward time scale, while one that began paused stays paused and zero-scaled; a player pause/resume during the request window suppresses restoration even though the final pause boolean is unchanged (the time scale is the discriminator); an accepted autosave whose storage write fails stays paused and zero-scaled with `engine.getSaveStatus()` carrying the rendered `StoragePhase`; a deadline outside `uiManager.isGameplayView()` skips silently (no request, no failure event, cadence uninterrupted); a `save_load` category configured to pause wins over the restoration; a pre-existing MANUAL save on an `autosave-<n>` name — as a slot directory OR a pre-#762 legacy flat file a published directory would shadow — fails the attempt through `save_load` with nothing overwritten or partially rotated; rotation keeps `autosave-1` newest across generations that all stay classified autosave, a failed write against a FULL family discards and renumbers nothing (publish-then-rotate), a rotation that fails part-way leaves every generation on disk and retries cleanly (retire-by-rename, delete last), and one interrupted AFTER a partial shift resumes without ageing out a second generation; and reducing `rotation_depth` or disabling autosave retains every excess generation untouched. |
| `save_pause_probe.py` | #42 | worldgen | Save/load pause-semantics regression. |
| `scene_stats_probe.py` | #1921 | offscreen worldgen (size 64, needs a GPU) | **World.Render scene-assembly telemetry.** Builds a deliberate population — units, ground items and buildings, all on tiles at the camera's own pinned z-slice so a seed's relief cannot decide what is culled — and reads `debug.getSceneStats()` back: the complete ten-row shape in the contract's order and identifiers, an advancing sequence whose later snapshot is a whole replacement, per-category `scanned` counts moving by EXACTLY the population created (measured as a delta against a pre-spawn baseline, so nothing assumes an empty world), present non-negative `durationNs` on every row with no threshold asserted, and a Lua caller's mutation of the returned table leaving the engine's own snapshot untouched. Offscreen rather than headless for one reason: `Unit.Render`/`Building.Render` emit nothing without a live texture system, so the non-zero EMITTED counts are the half only a GPU run can prove. Publication, ordering, sequence and every scanned meaning are gated on CI by the pure hspec group `scene assembly telemetry`. |
| `startup_asset_logging_probe.py` | #1930 | offscreen boot, both profiles (needs a GPU) | **Startup YAML logging ownership.** Boots `--offscreen` normal and `--offscreen --arena` with `ENGINE_DEBUG=Asset`, waits on `startupLoader.isDone()` (the two profiles finish through different code — normal drains across frames under `loadingScreen.update`, arena runs `runAll()` synchronously inside `uiManager.checkReady`), and reads the real log: exactly the 12 / 11 expected aggregates located by their stable family identifier and in queue order, no flora aggregate or `loadFloraYaml` call in arena, no `load*Yaml: loaded` line left at Info for any of the twelve bindings, per-file Debug detail carrying each file's full `data/<family>/` path and the authoritative count that binding returned, and every aggregate equal to the sum — and the file count — of its own family's observed per-file lines. One check exists purely to prove the matcher discriminates: the unrelated startup lifecycle Info lines must be present and uncounted. Offscreen rather than headless because a `--headless` boot never reaches `scripts/loading_screen.lua` or `scripts/ui_manager_boot.lua` (which gates on `fontsReady`), so the startup asset queue never runs there and every check would pass vacuously. Both halves of the contract are gated on CI by the pure hspec group `Startup asset logging`. |
| `pause_speed_probe.py` | #1599 | worldgen | The chosen world speed survives every pause source: a `pause: true` notification, a whole manual `engine.saveWorld` driven to its terminal outcome, and a save taken from an already-paused session; load still resumes at the default speed. The hspec suite cannot drive `engine.saveWorld` (its barrier waits on owner threads the headless harness never starts), so the whole-verb proof lives here. |
| `save_barrier_probe.py` | #757 | worldgen (isolated resource root) | Coordinated save-owner acknowledgement and paused reload smoke test. |
| `save_storage_probe.py` | #762 | worldgen (size 64, isolated resource root) | Atomic save-storage transaction: a first save publishes with no previous generation, a second save to the same slot retains the first as the previous generation; restart-and-select across constructed on-disk states (missing/truncated/bad-framing/checksum-corrupt authoritative, a stray leftover temp file) always recovers the correct complete generation via the live camera position, never a hybrid; neither generation valid rejects the load outright; `engine.listSaves()` reports a recovered slot's machine-readable status; a real disk-level write failure (directory path pre-occupied) names its storage phase via `engine.getSaveStatus()` and the barrier recovers for a follow-up save. |
| `sleep_probe.py` | #612 | arena | The "go to sleep" AI goal + Sleeping pose end to end: multi-hop lie-down/wake pose chain, `go_to_sleep` goal selection, sleep-pressure regen while asleep, and all three wake conditions. |
| `state_of_mind_probe.py` | #350 | arena | Unified consciousness/mood model (`brain.lua`): fresh-unit baseline, pain-driven concentration/mood/emotional_pain drift, no-hunger-config species fallback, the locomotor-collapse regression guard, and the awareness/perception drift input. |
| `structure_rotation_probe.py` | #1712 | offscreen arena (needs a GPU) | **Structure wall rotation, visual.** Stamps ONE deliberate scene on the flat arena — a room whose four sides each carry a DIFFERENT authored wall slot with a corner post at every vertex (symmetric geometry would look right under a wrong rotation), a raised terrain rim on two sides for the #415/#417 per-strip interleave to resolve against, and flora billboards just outside all four walls for the #418 lift — then captures it at all four camera facings. What it asserts is the pipeline, not the pixels: four non-blank captures at the requested size, each DIFFERENT from the other three, every side's authored slot still readable back off the engine, the camera pin surviving all four rotations, and a clean shutdown on success and failure. Judging that the art READS correctly is the human's job — the four PNGs are the deliverable. The rotation arithmetic itself is proven exhaustively and deterministically by the pure hspec groups `World.Render.StructureRotation` and `World.Render.FrontWallLift`. |
| `text_encoding_probe.py` | #618, #665, #1961 | none (no world/scripts needed) | `TE.decodeUtf8Lenient` sweep across `Engine.Scripting.Lua`: `engine.setText` with a truncated multi-byte UTF-8 sequence (`"caf\195"`) no longer raises a Lua error (#618 Text API), plus the same malformed sequence through the representative non-Text-API `world.show` boundary (#665) proceeds to its normal semantic no-op instead of erroring, plus well-formed control cases and a liveness/responsiveness check. Both `setText` ids name no scene node — headless has no active scene — so since #1961 each is asserted to cache NOTHING for `engine.getText`, the decode itself being what the no-error checks measure; the cache-follows-node contract is `Test.Headless.Lua.SceneText`'s. |
| `thermo_altitude_probe.py` | #308 | worldgen (size 128) | Altitude-lapse thermal effect. |
| `thought_probe.py` | #351 | arena | Thought event stream (`thought.emit`/`drainEvents`), STATE/ENVIRONMENTAL thought triggers, state-of-mind-biased selection (mood-weighted valence), and the thought-log data path. |
| `till_probe.py` | #333 | worldgen (isolated resource root) | Till-designation layer + till AI end to end: designate/cancel, fluid-tile exclusion, save/load, autonomous tilling (`world.getVegAt` confirms the flip), idempotent re-sweep. |
| `tutorial_probe.py` | #922 | worldgen (four engine boots, two real save/loads) | First-session tutorial integration gate: the shipped `data/tutorials/first_session.yaml` branch driven end to end from real gameplay state — a fresh session revealing only the root row, a placed `acolyte_portal` completing the portal objective, one acolyte discovering generated water by FOV scan and radio-sharing it with a second acolyte held immobile with no water anywhere in its own `getVisibleTiles` field of view (so a received source cannot be a second independent discovery, and must be one of the finder's own), the live water/food subobjectives checking one at a time as supplies are restored stepwise, the composite latching when a single acolyte holds both, that latch surviving the supplies being stripped again, and a fresh-process save/load round trip preserving every completed objective while the HUD returns collapsed and the live subobjectives recompute from the loaded world. A separate boot covers the #996 branch that latches before it is ever revealed and, since #1941, what happens to it next: the real `scripts/hud.lua` is booted and shown, a visible HUD over a COLLAPSED panel still presents nothing, and opening the panel renders the whole branch. Since #2056 this GPU-less probe owns the NEGATIVE half of the presentation boundary rather than the presentation itself — an open panel on a visible HUD retires nothing while no frame is drawn, because `--headless` has no renderer to witness one — and then makes the transition explicitly through #958's own `acknowledgePresented`, after which the shipped session's checklist reaches the empty completed state it could never reach before and stripping the supplies brings the RETIRED branch back under the ordinary hide rule. The positive proof that the rows reach a rendered frame is `tutorial_hud_probe.py`'s, which runs `--offscreen`. A fourth boot then reloads that finished session in a fresh process and requires the checklist to stay empty across further evaluation ticks. Injects no tree and stubs no predicate. |
| `tutorial_hud_probe.py` | #960 | worldgen, `--offscreen` (needs-gpu, manual-only) | Tutorial checklist HUD rendered over a real world: collapsed at session entry, real toggle clicks opening/closing the list (located through the module's own `dump()`, never a hardcoded coordinate), a transparent overlay that leaves terrain visible inside the list's rect, a 41-row injected tree scrolling under a real wheel event, and a real click landing on a row still selecting the terrain tile beneath it. Since #1941 it also drives the presentation contract end to end on the GPU, and since #2056 it is the arc's only PIXEL proof of it: an already-latched branch renders on the open panel and the sticky composite's own ink is attributed in a real captured frame (its text element hidden and re-shown, the columns that change being its marker and label — the #1581 oracle's technique) before the update tick that reports it retires it, the still-open checklist reaches its empty completed state, and a real save/load of that finished session brings the panel back collapsed and does not repopulate it. The captures and #2056's boundary crossing run inside one Lua chunk, so no update tick can interleave and the measured frames provably precede the empty state; `tutorial_probe.py`, being GPU-less, owns only the negative half. Since #1419 it also measures the toggle caption's own RENDERED GLYPH COLUMNS — separately for the collapsed `> Objectives` and the open `v Objectives`, by hiding and re-showing the caption element and diffing the frames, so the 9-slice box behind it is never mistaken for text — and requires a non-empty glyph set contained half-open inside both the toggle box and the framebuffer. Since #1581 the same rendered-glyph measurement covers the OBJECTIVE ROWS over SHIPPED content: the live `engine.getTutorialTree()` registry tree is restored (never a copy of the labels in Python or Lua), so the strings measured are the authored `Place portal` / `Secure water source` / `Prepare an expedition` / `Prepare water` / `Prepare food` at all four authored depths. Since #1941 all five are reached in TWO stable captures rather than one: a suppression is now spent by the very act of showing it, so the old single capture — which latched the whole chain sticky at once — could not survive a measurement lasting several screenshots. Each stage instead picks a durable set under which its rows are not hideable at all (only the root latched, then the whole chain with no subobjective checked), and the depths actually rendered are collected as evidence that the two between them still cover every authored depth. Each row is hidden and re-shown individually — requiring a live text handle, a non-empty glyph set, a stable rebuild count and handle identity, and no drift between the two rendered frames — and its ink must fall half-open inside both the checklist panel and the framebuffer, with any violation naming the offending row's shipped id and authored label. That oracle proved every shipped row below the root overrunning the panel, so #1581 also gave the rows the width fit they never had (`scripts/ui/text_wrap.lua`'s shared #1157 truncation, against the budget each row's indent leaves) — the renderer still never clips a row, so that fit is the only thing the assertions have to hold onto. Containment is proved for the size actually run, not for every scale. |
| `transactional_load_probe.py` | #763 | worldgen (three real engine boots, isolated resource root) | Whole-session load transaction: several deliberately invalid loads (missing save, corrupt save, missing gameplay definition) each leave the current session unchanged and paused, reporting `LoadFailed` via `engine.getLoadStatus()`; mutual exclusion rejects a save mid-load (creating no save, and starting no save transaction), rejects a second concurrent load, keeps the original request authoritative and non-terminal across both, and makes `scripts.pause.set(false)` a complete no-op — all against an in-flight window ESTABLISHED and positively observed by request id via the test-only `debug.armLoadStageGate` staging gate (#1181), never raced for, and failing rather than skipping if that window cannot be established; a successful load REPLACES the complete session (a page live only pre-load, never part of the save, does not survive publication) rather than merging; Haskell and Lua state agree immediately post-publication; a paused dwell advances no gameplay state and unpausing lands on the default time scale; repeated loads accumulate no ghost pages. |
| `transfer_order_probe.py` | #1247, #1253 | arena (phases 1-8, 10, 11) + worldgen with two further engine boots (phase 9); manual-only, ~10 min | The transfer-order unit job (`scripts/unit_ai_transfer.lua`) driven by the real `unit_ai.update` tick: a queued order walks its carrier to a 3x1 hold and commits exactly once on arrival — approached from the FAR end, so arrival is measured against the footprint rather than the anchor — advancing `queued -> in_transit -> ready_to_commit -> completed` in the store, emitting exactly one attributed `unit_event`, never losing the carrier to the wander tick, and committing nothing further over ten more seconds of ticking. Then the refusals: the command-time capacity gate (nothing queued, a warning naming unit and item), a partial twelve-into-eight batch, the arrival gate re-checking capacity as `became_stale/receiver_full`, an instance that left the carrier's hands mid-walk as `became_stale/instance_missing` while its sibling still lands, a demolished destination retiring as `became_stale/receiver_missing`, a blocked approach (an ocean ring) stalling out to `out_of_range`, and a progressing ~90-tile haul completing despite outlasting the 60 s stall budget. #1253's terminal cleanup rides every one of those: each phase checks the outcome was surfaced EXACTLY once (summing the event log's own coalesced `count`, not rows) and that the store is left holding no terminal order, since an order that ends is pruned on the tick that ended it. Phase 10 is the player's own "Cancel transfer" mid-walk: nothing moves, the order goes, the carrier stops walking at the hold, and a second cancel is inert. Phase 11 KILLS a carrier mid-walk — the corpse stays a live instance, so every reference still resolves and nothing downstream would have flagged the abandoned order, but the AI short-circuits a `dead` pose and the order must be retired anyway. Phase 9 saves an order mid-walk, quits, and reloads it in a FRESH process on a real world page (never an arena, #365), where the carrier resumes and completes it — and carries a SECOND carrier destroyed mid-order, whose orphaned order must never reach the save, asserted by both boundaries logging no integrity diagnostic at all. |
| `transfer_context_menu_probe.py` | #1014, #1085 | worldgen, `--offscreen` (needs-gpu, manual-only) | The "Transfer" context-menu entry end to end: a real right-click on a real built storage building, a real technomule and (since #1085's faction-based widening) a second real player acolyte, with the "Transfer" row located through `ui.dumpWidgets()` by its visible label (never a hardcoded coordinate), the existing "Contents"/"Info" rows still present alongside it, `debug.drainActionOutcomes()` confirming the click routed through the context-menu handler and never fell through to a move order, a real click on the row producing a `scripts.transfer_session` session naming the exact NAMED source/destination endpoints (and no operation field), and the two exclusions that must survive the widening — self-transfer and a non-player-commandable wildlife target both offer no row. |
| `unified_transfer_probe.py` | #1255 | worldgen (size 64, fixed seed), `--offscreen` x2 (needs-gpu, manual-only), ~25 min | **The unified transfer arc's final integrated gate** (epic #1013, slice UIT-6): ONE fixed-seed session, eight independently-reported stages (`setup`, `knowledge`, `modeB`, `modeA`, `batch`, `widget`, `save`, `load`) so a failure names which part of the system broke. An exact item instance moves in BOTH directions between all three endpoint CLASSES - acolyte, technomule and built storage, two of which share the contract's `unit` endpoint KIND - through BOTH player modes: six directed legs as #1249 queued gestures that commit on arrival, and the same six as immediate commits inside three real #1250/#1251 escort sessions (one of which starts out of reach and has to walk). Every leg carries its OWN throwaway item def, so its row is single-membered and its gesture names an EXACT instance id that is checked against source ownership before the commit and destination ownership after it; each Mode B leg additionally asserts the endpoints were OUT of the contract's reach when the gesture fired, which is the whole promotion. D-1: twelve into a hold with room for exactly eight stores eight, reports the remainder once by the contract's own reason, and leaves every one of the twelve in exactly one endpoint. D-2: a never-inspected container reads unknown with its capacity still live, reveals nothing on being opened and interacted with, and is not revealed by eight seconds of a player unit standing beside it; a NON-player withdrawal then makes the record genuinely stale (every unit-driven reveal is player-gated), and a Mode A session OPENING is what refreshes it. Requirement 1d: every container view encountered - building endpoint, unit endpoint, unit-info inventory, never-inspected window, and both escort panes - is proved to be a live instance of the ONE item-list widget, from evidence collected as each view opened. D-3: a save taken with a Mode B order in flight AND a Mode A session open on a DIFFERENT pair is reloaded in a FRESH PROCESS, where the order returns with its exact id, endpoint identities and instance ids, still non-terminal, and completes exactly once, while the session is gone and both units it held take player orders again. Rendered UI is located through `ui.dumpWidgets()` and the widget's own dump; world entities through `building.hitTestAt` / `unit.hitTestInRect` + `unit.hitTestAt` after `probelib.focus_and_locate` pins the z-slice (#1286) - no coordinate is written down. Runs on an isolated resource root shared only by its two processes (`config/` COPIED, so no local settings leak between runs) and prints one deterministic `FINGERPRINT` line (fixture and order identities plus every stage outcome, no timings or sampled measurements) so two consecutive runs diff clean. An operational failure is recorded against the stage it interrupted, so no run can traceback past a PASS summary. |
| `wander_hazard_probe.py` | #1217 | arena | Ambient wander never routes over a damaging drop: a sustained window of REAL `unit_ai` wander on a 5x5 3-high ledge (zero falls, at least one leg observed carrying the protected policy), then — all on the SAME unit — `unit.setMoveSpeed` on an in-flight protected request leaving its policy intact, a protected `unit.moveTo(..., 'avoid_falls')` over the edge that never falls and TERMINATES rather than replanning forever, and an ordinary `unit.moveTo` over that same edge that still does fall (so the policy is neither stochastic nor sticky). The acolyte's `find_water` goal is retired: `search_for_water` is purposeful movement and keeps the fall-permitted default by design. |
| `wire_probe.py` | #359 | arena | Wire structure piece: connection-aware autotile shape derivation (adjacency → isolated/end/straight/corner/tee/cross) and the `construct_job` AI placing a real wire tile from a designation. |

Invocation is bare `python3 tools/<name>.py` for sane defaults; most accept
`--port`/`--seed`/`--size` overrides and a handful have scenario-specific
flags (`--course`, `--phase`, `--attacker`/`--target`, ...) — see the
script's header docstring for its exact flag set.

### `run_probes.py` — opt-in aggregate runner

Runs a selection of the probes above and prints a per-probe PASS/FAIL
summary, exiting non-zero if any failed. `python3 tools/run_probes.py
--list` is the authoritative listing of registered probes, and `python3
tools/ci_probes.py --status` prints the derived CI-eligible / manual-only
/ total counts. This doc states no total of its own: a hand-written one
drifted three times (#539, #721, #1584), so `tools/test_run_probes.py`
fails if this section states one again.

`run_probes.py` is the COMMAND and nothing else (#2074) — argument parsing,
invocation validation, `--list`, dependency construction, scheduler dispatch
and the process exit. The implementation sits in five importable owners
beside it: `probe_runner_registry.py` (the probe list, selection, port spans,
per-key timeout declarations), `probe_runner_diagnostics.py` (the durable
progress and failure record protocols), `probe_runner_resources.py` (the
reader/writer conflict model, the cross-process holds, the inherited ancestor
holds, the engine preflight and its `ENGINE_EXECUTABLE` cell),
`probe_runner_lifecycle.py` (launching one probe and reaping its whole process
group) and `probe_runner_scheduler.py` (sequential and `--jobs` orchestration,
retries, presentation, summary). Dependencies run one way — registry and
diagnostics are leaves, resources and lifecycle build on them, the scheduler
builds on all four. A facility has exactly one owner and `run_probes.py`
re-exports none of them, so a tool or test reaching for `PROBES`, `run_one` or
a record parser imports the owner and an assignment there reaches the
implementation. Every operator and CI invocation stays `python3
tools/run_probes.py`.

```bash
# Run everything, sequentially (slow — low tens of minutes)
python3 tools/run_probes.py

# Run up to 4 probes concurrently, each its own engine on its own reserved
# port span (#531, #1571)
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

# ... and with --jobs, base the per-probe spans there instead of at 9400
python3 tools/run_probes.py --jobs 4 --port 9500
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
  engine on its own reserved port span (#531, #1571), cutting wall-time to
  roughly `total / N`, bounded by the slowest single probe. Concurrency raises
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

**Progress records and timeout attribution (#1768).** A probe's stdout is a
pipe the runner drains only when the child exits, and the child is a plain
`python3` with no `-u` — so ordinary `print` output sits in the child's own
block buffer and is LOST when a hung probe is SIGKILLed at `--timeout`. A
long probe that wants a phase to survive that emits a *progress record*
instead: one flushed line in the single convention
`probe_runner_diagnostics` defines (`PROGRESS_MARKER`, `ProgressEmitter`,
`parse_progress`) and the runner's failure presentation reads back.

```
#probe-progress# 19:25:04 +0.0s   | phase | engine A                            | build the scenario, save 'gen1'
#probe-progress# 19:25:04 +0.1s   | begin | chop (chop_probe.py) attempt 1/2    | dispatched
#probe-progress# 19:29:11 +247.2s | end   | chop (chop_probe.py) attempt 1/2    | PASS (247.1s)
```

The fields are the stamped marker (wall clock plus an offset from the
emitting producer's own start), the KIND, the IDENTITY, and free text.
`begin` and `end` share an identity, so every `begin` with no matching
`end` is an attempt that never finished. Two producers use this today:
`persistence_contract_sweep.py` records each of its long phases, and the
runner itself records every attempt it dispatches in a `--jobs` batch or
retries solo — which is what makes the probes active inside a NESTED
runner nameable when the outer one times out.

Above the ordinary `--tail` context, a failing probe's report therefore
also prints a short attribution drawn from the COMPLETE capture — the
latest phase the child entered, and every attempt still in flight:

```
[1/1] persistence_contract_sweep.py ... [timeout 120s] TIMEOUT (120.0s)
    progress: latest phase entered at 19:25:04 +0.0s: engine A -- build the representative scenario, save 'gen1'
    ... the ordinary last-25 lines follow, unchanged ...
```

The complete capture is never dumped, and a probe emitting no progress
records has exactly the failure presentation it always had.

**Failure records and the retained failed check (#1982).** The record above
solves the TIMEOUT half of that loss; a probe that FINISHES and fails loses
something else. Its per-check verdicts go to a block-buffered stdout while its
terminal `FAIL:` summary goes to an unbuffered stderr the runner merges into
that same pipe (`stderr=subprocess.STDOUT`) — so the `FAIL:` lines overtake the
buffered output and land near the TOP of the merged capture, above whatever
`--tail 25` prints. A 279.5-second run reported "1 check(s) FAILED" and named
the check nowhere. Flushing alone would not fix it: with more failed checks
than `--tail` lines the tail truncates them again.

So a failed check is recorded the way a phase is — one flushed line in a
sibling convention (`FAILURE_MARKER`, `FailureEmitter`, `parse_failure`), read
back from the COMPLETE capture:

```
#probe-failure# 19:29:11 +279.4s | check   | location_embark_probe | the discovered icon never appeared at (12,7)
#probe-failure# 19:29:11 +279.4s | setup   | location_embark_probe | no conforming [flat] site in 6 seeds
#probe-failure# 19:29:11 +279.4s | context | engine log            | /tmp/loc_embark_x9/logs/engine.log
```

The kinds are three, not one. `check` and `setup` are the two vocabularies the
probes already print — "there is a bug" against "try another seed" — and
`context` carries the bounded invocation evidence beside them: the engine log
this run owned, a short tail of it, and what became of the artifact tree.
Keeping them apart is what lets an operator tell a product failure from a
fixture or infrastructure one without rerunning the probe. Emitting one is
never a substitute for CLEANUP: the excerpt is read while the tree still
exists, and a passing or failing run removes its own artifacts exactly as
before.

Both default failure presentations — sequential and `--jobs` — print this block
above the phase attribution and the ordinary tail, and withhold the raw records
from that tail, so every recorded failure appears exactly once:

```
[1/1] location_embark_probe.py ... [timeout 900s] FAIL (279.5s)
    failure: 2 recorded failure(s) from location_embark_probe:
        [19:29:11 +279.4s] FAIL: the discovered icon never appeared at (12,7)
        [19:29:11 +279.4s] FAIL: the map icon was the unknown bitmap after discovery
    failure: retained context:
        engine log: /tmp/loc_embark_x9/logs/engine.log
        engine log tail: vulkan: swapchain out of date
    ... the ordinary last-25 lines follow, unchanged ...
```

The six producers today are `location_embark_probe.py`,
`location_stamp_idempotent_probe.py`, `location_content_probe.py`,
`location_overlay_probe.py`, `portal_location_probe.py` and
`portal_ghost_probe.py`. As with progress records, the complete capture is
never dumped, and a probe emitting no failure records has exactly the failure
presentation it always had.

**Timeouts are per probe.** Most registered probes use the ordinary 900-second
default. A scenario whose complete expected workload structurally exceeds that
class declares a validated key-specific default in
`probe_runner_registry.PROBE_TIMEOUT_OVERRIDES`; the runner prints each
probe's effective
budget when it starts or completes. `save_compat_migration` uses 3600 seconds
because its manifest-wide two-process/two-real-codec-dump path has a measured
clean runtime above 2300 seconds. Passing `--timeout SECONDS` explicitly
overrides both the ordinary and key-specific defaults for every selected probe,
including solo retries. Keep short explicit values available for deliberate
timeout-path testing; do not raise the global default to accommodate one
exceptional scenario.

**Prebuilt execution: one Cabal contact per run (#1570).** Probes used to
launch their engine as `cabal run -v0 exe:synarchy --`, so a `--jobs N`
sweep put N concurrent Cabal processes on the one `dist-newstyle` and an
otherwise healthy probe died on the shared inplace package database
before its engine started (`package.conf.inplace already exists`,
`package.cache: removeDirectoryRecursive: does not exist`). Retries only
re-ran the loser.

The runner now resolves the executable ONCE. After selection is validated
and every port refusal has had its chance — so `--list`, an unknown
`--exact` key and a `--port` that reaches 8008 all stay build-free — and
before a single probe process exists, it runs one freshness `cabal build
exe:synarchy` plus one read-only `cabal list-bin exe:synarchy`, prints the
resolved path, and hands it to every probe it launches through
`SYNARCHY_PROBE_ENGINE_EXE`. A preflight that fails is the runner's own
exit 2 with Cabal's reason — never a retry, never a probe's assertion
failure. **The build itself runs inside an EXCLUSIVE `cabal-build` hold**:
a preflight is a Cabal writer like any other, so two aggregate runs cannot
build at once, and a build cannot land inside another runner's `cabal
repl` probe. The hold covers the build alone and is released before any
probe is dispatched, so a sweep never queues its own probes behind it. That applies to `--jobs 1` as much as to `--jobs N`, and it
reaches the initial attempt, every parallel worker, every solo retry and
a nested `run_probes.py` (which adopts the inherited path rather than
building again).

`tools/probe_engine.py` is the single funnel every launch goes through —
`probelib.boot` and the four probes with their own private launchers
(`debug_console_boot`, `preview_cli`, `resource_root`, `thermo_altitude`)
alike. Handed an executable it execs that absolute path; handed none it
reaches Cabal itself, with the engine's own arguments in the same order
either way. **That is why running a probe by hand still needs no prior
build step**: `python3 tools/chop_probe.py` from the repository root
behaves exactly as it always has. A supplied path that is relative,
missing or not executable is a refusal rather than a quiet fallback —
falling back would put a Cabal process straight back inside a parallel
sweep.

**Preparation, not readiness (#1913).** In direct invocation that Cabal
contact is a BUILD, and `probelib.boot` used to start its 180-second
READY deadline the instant the child existed — so a cold compile was
timed as though an engine were already starting. Two coordinated runs
expired that way, both reported as `engine never printed READY` against
an empty `-v0` log, one of them with the finished 179 MB executable
timestamped 25 seconds AFTER the recorded timeout. `boot` now calls
`probe_engine.prepare_executable` FIRST: one freshness `cabal build`
plus one `cabal list-bin`, inside an **exclusive `cabal-build` hold**
(the direct path's answer to the same race the preflight solves for a
sweep), and what it launches afterwards is the resolved absolute binary.
So `ready_timeout` measures engine startup and nothing else, and a probe
that passes its own keeps exactly the readiness allowance it asked for.

Preparation has its OWN finite allowance, `prepare_timeout`, defaulting
to 1800 s — the repository's established full-cold-build watchdog — and
covering lock acquisition, the build and the query together. It is not a
second readiness budget and enlarging one does not enlarge the other.
Every preparation failure names PREPARATION rather than readiness, and
carries the build output: it goes to a `<engine log>.prepare` file whose
path and tail appear in the diagnostic, so an operator never again reads
"engine never printed READY" pointing at a log the engine never reached.
The engine log itself is not even opened until preparation has
succeeded. Each preparation subprocess owns its process group and the
whole group is reaped on a nonzero exit, an overrun or an interrupt, so
a `cabal` that fails cannot leave `setup`/GHC descendants compiling
behind it. An engine that dies or hangs AFTER its executable exists is
still diagnosed as the boot failure it is.

`tools/deflake.py` prepares the same way but a step earlier: **before**
the measurement takes its resource hold, never inside it. A measurement
holds `cabal-build` (shared for an ordinary probe, exclusive for the
three that drive Cabal themselves) across all ten runs, and
`probe_runner_lifecycle.run_one` strips the inherited runner variables
on the way
down — so a child left to prepare its own executable would take an
exclusive interest underneath its own ancestor's hold and wait out its
whole allowance for a holder blocked on it. Preparing first removes that
by ordering, and the resolved path is installed as the runner's
executable, which is also what stops each of the ten runs rebuilding.

Three registered probes legitimately still drive Cabal themselves, and
they are not engine boots: `persistence_contract`,
`persistence_contract_sweep` and `save_compat_migration` each shell out to
`cabal repl test:synarchy-test-headless` (through
`persistence_snapshot.compare_session_files` /
`save_compat_audit.dump_canonical_summary`). A `cabal repl` recompiles
into the same package database, so each of them declares the
`cabal-build` resource EXCLUSIVELY in the reader/writer tables below,
while every other probe holds it SHARED. They therefore cannot overlap
each other, nor a probe reading the binary they may be relinking — the
same scheduling mechanism `repo-config` already used, with no new one
invented.

**Shared repository resources (#1322, #1444, #1570).**
`probe_runner_resources` declares two tables: `IMPLICIT_SHARED_RESOURCES`, which EVERY registered
probe holds in a shared interest (`repo-config`, the checkout's tracked
`config/` tree, and `cabal-build`, its `dist-newstyle`), and
`EXCLUSIVE_RESOURCES`, which names the few probes that need one to
themselves. Shared holders coexist; an exclusive holder runs alone.
`tools/probe_resource_lock.py` (#1436) enforces the same two tables
BETWEEN processes, so a second sweep or a `/deflake` measurement cannot
overlap what this one holds either. A runner tells each probe what it
holds exclusively on that probe's behalf (`SYNARCHY_PROBE_HELD_EXCLUSIVE`),
which is what lets `persistence_contract_sweep`'s own nested
`run_probes.py` run inside its ancestor's `cabal-build` hold instead of
deadlocking against it.

**Reserved port spans (#1571).** A probe is handed one `--port`, but two
registered probes derive a second, concurrently live listener from it:
`debug_console_boot_probe.py` boots its successful-bind and
widget-module checks on `--port + 1`, and `offscreen_probe.py` starts a
second offscreen engine on `--port + 1` while the first is still up. So a
probe's port count is DATA — `probe_runner_registry.PROBE_PORT_SPANS`
declares 2 for
each of those two, and every other probe reserves its base alone. A
declared count `N` reserves the contiguous span `base … base + N - 1`, and
`--jobs` lays the selected probes' spans end to end so no two concurrent
probes overlap. Before #1571 the allocator used stride 1, so selecting
`debug_console_boot` immediately before `transactional_load` under
`--jobs 2` put both on 9401 and the resulting `Address already in use`
read as a regression in two probes that each pass alone. Adding a future
multi-port probe is one row in that table: nothing in the allocator, the
GUI-port refusal, or `tools/probe_flake.py`'s lease scanner knows any
probe by name, and `tools/test_run_probes.py` validates every row against
the live registry.

`--port` is the allocation ORIGIN, not just a sequential override: with
`--jobs > 1` the spans are laid out from it instead of from the default
`9400`, so the flag is honoured in both modes. The whole plan — every port
every selected probe may bind, in the mode it is about to run in — is
computed and checked before the first subprocess exists, and a span that
covers the user's GUI port 8008 is refused there (exit 2), not discovered
by an engine booting against the running game. `tools/probe_flake.py`
leases the same declared span in full before it launches a probe, and
releases nothing until `run_one` has reaped the process group.

`--list` shows the full probe registry but not CI status. For that, see
`tools/ci_probes.py --status` below.

**The runner reaps each probe's process group on EVERY completion path**
(#1323) — success, ordinary nonzero exit, timeout, and an exception in the
runner itself — reusing one SIGTERM-then-SIGKILL escalation. Probes are
launched into their own session precisely so this is possible. It matters
because a probe that dies of an unexpected exception after booting its
engine never reaches its own teardown, and `communicate()` cannot notice:
`probelib.boot` redirects the engine's output to a log file rather than
the runner's inherited pipe, so the pipe reaches EOF the moment the probe
exits. The stranded engine keeps its port, and the next `--retries`
attempt (or a parallel solo retry, which reuses the allocation origin) then
fails its boot under #1190 — reporting a leak as an unrelated "exited
before READY". Reaping a group that already exited is a silent no-op and
never alters a probe's status, elapsed time, or output tail. Ctrl-C exits
130 after terminating every probe still running, its engine included, and
launches none of the probes still queued.

The reap returns only once nothing in the group is still RUNNING, not when
the signal is sent: SIGKILL delivery is asynchronous, and until the last
member exits it still owns the port the next retry is about to reuse. A
ZOMBIE deliberately does not count as running there — it has already
exited and released its port, and signals cannot tell it apart from a live
process (a zombie-only group answers EPERM on macOS and succeeds on
Linux), so the reap consults process state and would otherwise spend its
whole grace on an engine that was long gone.

`python3 tools/test_run_probes.py` is that behavior's gate: deterministic,
GPU-free, synthetic probes and synthetic engine descendants in a throwaway
tree, no registered probe ever run. It covers the executable preflight
(#1570) the same way — with the preflight's subprocess entry point
doubled, so one Cabal contact per run, its ordering ahead of every probe
spawn, its failure starting nothing, the resolved path reaching every
attempt including solo retries, and the build-free `--list`/refusal paths
are all the shipped `main`'s behaviour rather than a restatement of it.
It is a blocking CI step alongside `ci_probes.py --self-test`.

### `test_action_outcome_probe.py` — chop-fixture classification (#1398)

`action_outcome_probe.py`'s chop stage has two failures that mean
different things, and conflating them is what the old fixture did: "the
loaded region holds no wood-bearing flora" says the contract went
UNVERIFIED, while "the drained record is absent, the wrong kind, not
`partial` or miscounted" says the contract is BROKEN. Only the first
earns exit **2**, which takes precedence over any concurrent ordinary
failure; a missing till box or an unusable portal fixture stays an
ordinary exit **1**.

`python3 tools/test_action_outcome_probe.py` is that classification's
gate, and it is ENGINE-FREE by design — the probe itself boots a real
headless engine and generates a 64-world, roughly eight minutes, which is
exactly the cost this coverage exists to stop depending on. It imports the
real probe and swaps its `send`/`send_json` for a fake console, so the
shipped `find_chop_fixture` / `evaluate_chop_designation` /
`run_chop_stage` / `probe_exit_status` paths run rather than a copy. It
also pins the property that makes "found nothing" trustworthy: the query
origins' radius-64 Euclidean discs COVER the probe's loaded region, so
discovery cannot regress into a sparse sample grid that misses trees.
Blocking CI step alongside `test_run_probes.py`; `action_outcome` itself
stays `manual-only`.

### `test_probelib.py` — `send_json`'s result contract (#1160)

`probelib.send_json` is the shared JSON layer over the debug console, and
twenty-two probes used to define a private `jget` copy of it instead. The
copies differed from the shared helper in ways nothing documented, so how
a probe decoded a reply depended on which spelling its author reached for.
The one difference that was observable: an EMPTY result is `None` from
`send_json` and was `""` from `jget`, so an `is None` check meant
different things in different files. #1160 deleted every copy.

`python3 tools/test_probelib.py` keeps that consolidated, and is
ENGINE-FREE: it stands up a real socket that speaks the console's reply
protocol, so the transport (`send`) runs for real without a world, a GPU
or a subprocess. It also owns `tools/probe_engine.py`'s contract (#1570):
runner mode execs the resolved binary with no Cabal anywhere in the argv,
direct mode keeps the historical `cabal run` fallback with identical
argument ordering, an unusable supplied path raises instead of falling
back, `probelib.boot` really launches the supplied binary with its logging
unchanged, and `resolve_executable` builds before it locates. It pins the
three result cases — empty is `None`, valid
JSON is the decoded value, non-JSON is returned AS TEXT (which is why a
Lua `nil`, arriving as the JSON literal `null`, still reads as `None`) —
plus `idle` being reachable, the knob the copies hid.

Its source guard is structural, not a name check: a `tools/` function
qualifies as a copy when it `json.loads` a `send` result AND hands `send`
one of its own PARAMETERS as the Lua — argument 2 or keyword `lua`, and
only that position, so a helper merely forwarding its own `timeout` is
not mistaken for one. It is deliberately not keyed on
`return`, so a copy that guards the decode or buries it in a branch
counts the same. That parameter clause is the scope line: a helper
decoding ONE fixed query it builds itself (`snap`, `measure`, `msummary`,
`get_identity`) is a different, deliberately out-of-scope duplication —
see `docs/code_health_findings.md` CH-129. Both directions are
mutation-tested against synthetic trees, so the guard is proven to fire
on a reintroduced copy — under its old name, a new one, or a guarded
decode — and to stay quiet on a fixed-query helper, rather than merely
agreeing that today's tree is clean. ~5 s; blocking CI step alongside
`test_run_probes.py`.

### `test_probe_root_cleanup.py` — the isolated root's staging boundary (#1791)

`foraging_probe.py` (#1618), `flora_growth_probe.py` and `farm_ai_probe.py`
(#1616) and `item_temp_probe.py` (#1613) each give one invocation its own
throwaway resource root and promise to remove it on every exit path.
The promise had a hole: `tempfile.mkdtemp` AND `make_isolated_root(base)`
both ran before the `try` whose `finally` owns `remove_run_root(base)`, so
a failure while STAGING the tree — the root, three symlinks into the
checkout, a copied `config/`, `saves/`, created in that order — bypassed
cleanup entirely and left the invocation-owned directory on disk. Staging
now happens inside that guard, one statement after the base exists.

`python3 tools/test_probe_root_cleanup.py` pins the boundary for all four,
and it drives each probe's REAL `main()` rather than `make_isolated_root`
alone — calling the builder in isolation would pass while the defect
above it stood. Each case runs the probe in a subprocess against a
stand-in checkout and a private `TMPDIR`, with `boot`/`quit_engine`
observable and no engine anywhere, so the exit status and the
operator-visible cause are the process's own. Four scenarios per probe: a
`copytree` that fails once the root and its three symlinks exist (real
partial state to leak) must end non-zero with the cause visible, the base
gone, the temp directory empty, and neither `boot` nor `quit_engine`
reached — a staging failure precedes any engine, and an `engine.quit()`
sent anyway would be aimed at whoever else holds the port; a removal that
silently no-ops and one that raises must each still be non-zero and name
the residue, because cleanup cannot promise absence when the filesystem
refuses; and a `boot` abort must still announce the staged root and slot,
still leave nothing, and still send no quit. The stand-in checkout's
`scripts/`, `assets/` and `data/` sentinels sit behind the symlinks the
partial tree holds, and an unrelated outside directory beside it — both
are asserted byte-identical after every scenario, which pins the
"deletion stays inside the run" half without assuming anything about how
`shutil.rmtree` treats a symlink.

All four probes are manual-only, so without this companion the contract
is only ever observed by long engine runs. Engine-free, GPU-free,
network-free, under a second; blocking CI step alongside
`test_location_embark_probe.py`.

```bash
python3 tools/test_probe_root_cleanup.py
```

### `test_flora_growth_probe.py` — the flora probe's artifact ownership (#1682)

`flora_growth_probe.py` already owned an isolated resource root and
removed it on every exit path (#1616, #1791) — but only its SAVE slot had
moved there. Its two fixture YAMLs and its engine log stayed at the fixed,
process-global names `/tmp/probe_berry.yaml`, `/tmp/probe_clover.yaml` and
`/tmp/flora_growth_probe_engine.log`. Each was written with a truncating
`open(..., "w")` (`probelib.boot` opens the log the same way), none carried
a PID, port or any other invocation identity, and nothing removed any of
them. Two concurrent runs — a supported mode, between `run_probes.py
--jobs N` and `probe_flake.py`'s machine-wide port lease — collided on all
three, one overwriting a fixture between another's write and the
engine-side read of it while both interleaved into one truncated log; a
developer's own same-named file was truncated outright. All three now live
inside the directory the invocation already owned, so nothing the probe
writes can collide with another run or with a file it did not create.

`python3 tools/test_flora_growth_probe.py` drives the probe's REAL `main()`
with `run_probe` substituted, so the guard's own paths are exercised
without an engine: two invocations share no fixture, log or root path; no
artifact resolves to a legacy `/tmp` name; the tree is released after a
pass, an early return, an exception, a `probelib.boot` abort and a handled
Ctrl-C; `--keep-artifacts` is opt-in, keeps whatever result the run's own
checks produced, names where the artifacts are, and reports only what that
run ACTUALLY produced (a directory that was never created — a tree whose
staging failed part-way — says so, rather than being reported as empty); a
default failing run says its log went with the tree and points at the flag
rather than leaving the operator chasing a deleted path; a cleanup that
cannot finish makes an otherwise passing run non-zero; an engine the run
LAUNCHED is always disposed of, while only a boot that RETURNED may be
shut down through the PORT (a boot fails on a busy port exactly because
somebody else's instance holds it) — `probelib.boot` hands the handle
over the statement after its `Popen` through an appended optional
`on_launch`, because it then waits up to three minutes for READY and a
caller learning of the engine only from the return value owns nothing
for that whole span, and anything raised while that hand-off is in
progress kills the child inside `boot` rather than let it escape holding
the port, and the orderly shutdown runs inside a finally whose fallback
kills the engine outright, because `quit_engine` sends, waits and
hard-kills and an interrupt in any of those would otherwise unwind past
a live engine; a read-only
checkout still yields a REMOVABLE tree, with the source's own modes
untouched (`copytree` reproduces the source's mode bits, so a read-only
`config/` would otherwise give this run a private copy whose entries
cannot be unlinked — residue from a source it only read; the
`_make_owner_writable` treatment is `location_embark_probe.py`'s, #1569);
and an outside directory holding same-named decoys comes through
byte-identical. It also pins what the
probe still proves after the move: the registration ORDER placement hashes
are indexed by — the sorted shipped flora, then `probe_berry`, then
`probe_clover` — both fixture bodies by `sha256`, and `load_fixture_yaml`
still stopping the run at setup on a fixture that registers nothing
(#1342).

The probe is manual-only and worldgen-heavy, so without this companion the
contract is only ever observed by a run that generates a world.
Engine-free, GPU-free, network-free, about a second; blocking CI step
alongside `test_probe_root_cleanup.py`.

```bash
python3 tools/test_flora_growth_probe.py
```

### `test_location_content_probe.py` — the location-content probe's artifact ownership (#1884)

The same split, one probe over. `location_content_probe.py` already owned
an isolated resource root and removed it on every exit path (#1620) — but
again only its SAVE slots had moved there. Its five fixture YAMLs and its
engine log stayed at the fixed, process-global names
`/tmp/loc_content_probe_bogus.yaml`, `…_bogus_loot.yaml`, `…_quinoa.yaml`,
`…_quinoa_loot.yaml`, `…_dense.yaml` and
`/tmp/location_content_engine.log`, each written with a truncating
`open(..., "w")`, none carrying any invocation identity, and none removed
by anything. Two concurrent runs collided on all six. The log collision is
the sharp one here: this probe ASSERTS against that log twice — the
integrity diagnostic after phase 2's load, and phase 3's two
unknown-content warnings — so a foreign truncation could turn a passing
phase into a failure or a failure into a pass, not merely muddle a
post-mortem. All six now live inside the directory the invocation already
owned.

`python3 tools/test_location_content_probe.py` drives the probe's REAL
`main()` with `run` substituted, so the guard's own paths are exercised
without an engine: two invocations share no fixture, log or root path;
all five fixture paths are absolute (the engine is chdir'd into the
isolated root, so a relative one would resolve elsewhere) and inside the
run's own tree; no `/tmp` literal is left in the module at all, and a
real run leaves each of the six legacy paths exactly as it found it —
absent if absent, byte-identical if a developer has one; the tree is
released after a pass, a failure, an early return, an exception, a
`_PhaseAborted`, a `probelib.boot` abort and a handled Ctrl-C; an engine
the run merely LAUNCHED is killed BEFORE the tree it is writing into is
removed, and killed directly rather than sent an `engine.quit()` that
might reach somebody else's instance; `--keep-artifacts` is opt-in, keeps
whatever result the run's own checks produced, names the retained
directory, and reports only what that run ACTUALLY produced (a directory
that was never created says so rather than being called empty); a default
failing run says its log went with the tree and points at the flag; and a
cleanup that cannot finish makes an otherwise passing run non-zero,
through #1620's own `remove_isolated_root` reporting. It also pins what
the probe still proves after the move: all seven boot CALL SITES go
through the one funnel that hands each this invocation's log and
registers its process as it is launched; both log-reading ASSERTIONS
read that same log; the five fixture bodies are pinned by `sha256`;
their registration order and loaders are unchanged (placement and loot
draws are order- and content-sensitive); `load_fixture_yaml` still
guards every one of them (#1342); and `make_isolated_root`,
`remove_isolated_root` and `save_and_wait` are still the shapes
`portal_ghost_probe.py` imports.

Since #2095 every structural check scans the COMPLETE reorganized
surface — the façade plus every scenario owner under
`tools/location_content/` — and asserts its own non-vacuity first,
because an exclusion-style property ("no bare `boot`", "no raw fixture
`send`", "every log read is this invocation's") is True over an empty
node set and would otherwise report OK while inspecting nothing once the
assertion bodies left `run`. It also pins the scenario split itself:
only the façade boots; the regeneration call site is still a loop over
the same and reversed visit orders, so the run still LAUNCHES eight
processes from seven call sites; each fixture constant has exactly one
definition, resolved wherever its owner keeps it; the façade offers one
`run(args, art, token)`; every PASS diagnostic and recorded failure
belongs to an owner rather than the façade; and no owner keeps
cross-scenario state in a mutable module global.

The probe is manual-only. It boots from seven call sites and launches
eight engine processes across several generated worlds, so without this
companion the contract is only ever observed by a run nothing in CI can
make. Engine-free, GPU-free, network-free, about a second; blocking CI
step alongside `test_flora_growth_probe.py`.

```bash
python3 tools/test_location_content_probe.py
```

### `test_location_probe_config_isolation.py` — the location probes' private `config/` (#1729)

`location_content_probe.py`, `location_overlay_probe.py` and
`location_stamp_idempotent_probe.py` each build one invocation's throwaway
resource root, and `portal_ghost_probe.py` imports the first of those
builders and hands the same root to both its headless writer and its
offscreen reader. All four used to SYMLINK `config/` in beside `scripts`,
`assets` and `data`, calling all four "read-only content, safe to share".

`config/` is not read-only content. Engine initialization is itself a
writer: `Engine.Asset.YamlNotifications` materializes
`config/notifications.local.yaml` from registry defaults whenever that file
is absent, and `Engine.Core.Init.migrateLegacyConfig` copies a tracked
legacy file over an absent local one — or, when that legacy file is a
neutral placeholder (#1937), writes a `config/*.legacy-neutral.local.yaml`
record instead. Through the alias those writes landed
in the developer's own checkout, and teardown — which unlinks a symlink
rather than descending it — then left them there. A personal `*.local.yaml`
was visible to the run in the same breath, so a local override could decide
what the probe observed. Each builder now COPIES `config/` with
`shutil.ignore_patterns("*.local.yaml")` and makes the copy owner-writable,
the `location_embark_probe.py` pattern from #1569.

`python3 tools/test_location_probe_config_isolation.py` pins that for all
three distinct builders, and asserts rather than assumes that
`portal_ghost_probe.py` is still sharing the corrected one. Each builder is
driven against a synthetic checkout, a read-only synthetic checkout, and the
REAL checkout: the private `config/` is neither a symlink nor an
`os.path.samefile` alias, and neither is any file inside it; a seeded
`*.local.yaml` is absent at both the top level and nested; creating a new
`*.local.yaml` AND rewriting an already-copied file through the root leave
the source's entry names, entry types, file bytes and mode bits all
unchanged; a read-only source still yields a copy that is owner-writable
recursively and really removable, with the source's own modes untouched;
and teardown never follows the content symlinks. The checkout comparison is
a full manifest including ignored entries, because `.gitignore` hides the
exact `*.local.yaml` paths at issue — `git status --porcelain config/`
cannot see the failure this file exists to catch — and it restores anything
it finds changed before reporting, so a regression cannot leave the
developer's tree dirty on its way out.

Three of those probes are long and the fourth is manual-only `needs-gpu`, so
without this companion the contract is only ever observed by runs neither
gate can make. Engine-free, GPU-free, network-free, under a second; blocking
CI step alongside `test_location_embark_probe.py`.

```bash
python3 tools/test_location_probe_config_isolation.py
```

### `ci_probes.py` — CI probe selection + eligibility (#530, #540)

Computes which probes CI should run for a given set of changed files (see
`.github/workflows/ci.yml` and the CLAUDE.md "Testing Tiers" section for
the gate this feeds). It also owns `CI_ELIGIBLE` — the curated,
small smoke subset of the full registry that's actually allowed to run in
the blocking CI gate. Deterministic probes can still be manual-only when
they are too narrow or too expensive for every matching PR, and paths
covered only by manual-only probes select no behavior probe by default.

A manual-only probe records ONE OR MORE reasons (#1440), each a
`(category, explanation)` record naming one INDEPENDENT ground for
exclusion — `expedition_retrieval` needs a real generated world *and*
walks its legs in real time, either of which would keep it out on its
own. Categories are unique within a probe, so two facets of the same
category belong in one explanation; the declared order is the render
order. Adding a `flaky` reason is the de-flake workflow's job (#1426),
never an opportunistic edit from an unrelated branch.

```bash
# What would CI run for these changed files?
python3 tools/ci_probes.py --changed src/Power/Network.hs

# Validate the mapping (no engine) — also a blocking CI step
python3 tools/ci_probes.py --self-test

# Every registered probe's CI status: CI-eligible, or manual-only with
# EVERY reason excluding it, each tagged with its category (flaky /
# base-failing / slow/worldgen-heavy / scenario-heavy / targeted /
# needs-gpu / unclassified). A probe appears exactly once however many
# reasons it carries.
python3 tools/ci_probes.py --status
```

### `probe_flake.py` — repeat-run flakiness measurement (#1425)

Nine manual-only probes are classified `flaky` and several more have known
flakes recorded under other reasons, but none of that is measured. This runs
ONE registered probe N times in a row under a fixed `+RTS -N4 -RTS` setting
and reports a per-check `PASS`/`FAIL`/`MISSING` table plus an aggregate
failure rate (timeouts in the numerator, and separately visible).

```bash
python3 tools/probe_flake.py --probe role --runs 10
python3 tools/probe_flake.py --probe role --runs 10 --result /tmp/role.json
python3 tools/test_probe_flake.py     # the focused self-test (no engine)
```

Only probes that implement the shared `probe-result/v1` protocol
(`tools/probe_protocol.py`) can be measured; everything else is `legacy` and
is rejected BY NAME before execution, without running the probe at all —
heuristically parsing free-form stdout is the guesswork a reliability harness
must not do, and invoking a legacy probe to find out would boot a real engine.
`blood_decal`, `blood_impact`, `circadian`, `circadian_species`,
`collapse_crawl`, `concussion_revive`, `config_state`, `disarm`, `injury_log`,
`lua_orphan_prune`, `lua_strict_msg`, `machine_shop`, `meal_waste`,
`mental_efficiency`, `position_hold`, `remote_warning_page_guard`, `role`,
`state_of_mind`, `text_encoding`, `thermo_altitude`, `thought`, and `wire` are
the migrated probes today. Later changes normally migrate one at a time; this
ten-probe batch was an explicit operator request so the related mechanical
work could land in one pull request.

A migrated probe prints its ordered, stable check declaration with
`--describe` (no engine) and, when the harness supplies an event path, writes
one flushed JSON event per line instead of bracketed stdout markers. Run by
hand it is unchanged: `python3 tools/role_probe.py --port N` still prints its
`[PASS]`/`[FAIL]` lines and exits 0/1, and `run_probes.py --only role` behaves
exactly as before.

`probe_runner_lifecycle.run_one` still owns process launch, output capture,
elapsed
timing, deferred SIGINT, timeout escalation and process-group cleanup; the
harness reuses it through four keyword-only parameters (event path, artifact
dir, engine-log dir, RTS capabilities) that default to today's behavior.

Ports come from an atomic cross-process lease over 8009-8999 (8008 — the GUI
port — is always forbidden), held until `run_one` has reaped the probe's whole
process group, so concurrent harnesses never collide. The lease is an advisory
`flock`, not a file anyone deletes, so a harness that dies releases it with no
staleness heuristic — and the live-invocation registry behind the reported peak
concurrency is held the same way, so an abandoned entry can never look live
because the operating system recycled its pid; its namespace is `/tmp` ITSELF — not
`--artifact-root`, not `TMPDIR`, and with no uid in it, because a TCP port is
host-global and anything that split the namespace would let two harnesses lock
different files and land on one port. The lease files are flat in `/tmp` rather
than in a subdirectory: a directory's OWNER may unlink entries in it however
the sticky bit is set, so a harness-created subdirectory would let whoever made
it replace another user's held lease. `/tmp` is root-owned and sticky, which
the harness verifies rather than assumes, and every lease file is opened
`O_NOFOLLOW` and checked to be a plain unlinked-to file. Artifacts,
by contrast, DO follow the platform temp dir —
`<platform temp dir>/synarchy-probe-flake` — and never land inside a worktree;
successful runs are deleted and `FAIL`/`TIMEOUT`/harness-error runs keep their
stdout, protocol events and every engine log.

A valid measurement exits 0 whatever rate it observed. Nonzero is reserved for
pre-execution rejections (2), port exhaustion (3) and harness errors (4) — a
malformed, truncated, duplicate, out-of-order or unclassifiable protocol
event, which is never reported as a probe pass.

### `probe_census.py` — the probe census (#1425, #1428, #1430, #1492, #1434, #1441, #1439, #1438)

Builds, validates and updates `docs/probe_census.json`, now
`probe-census/v5`: every registered probe exactly once, with its script, its
CI-eligible/manual-only classification, its protocol status (`legacy` or
`probe-result/v1`) and one census record holding the acceptable-failure
policy, the estimated worst-case duration, the current commit cohort, the
archived cohorts, an append-only attempt log, an append-only log of claim
ACQUISITIONS keyed for idempotency by acquisition token (#1434), and an
append-only log of a de-flake attempt's non-repair OUTCOMES keyed for
idempotency by attempt identity — the three stable ones (#1439) and the
production defect a tracker issue was filed for (#1438). The record also has
a nullable `deferred` decision: when present, its non-blank `reason` and
`resume_when` condition keep the probe registered with all evidence intact
while excluding it from de-flake selection. All three logs are separate
collections on purpose: an attempt is a result ingestion and is deliberately
non-idempotent, while recording one acquisition or one attempt outcome twice
must stay one record — and an outcome carries evidence neither of the other
two has a field for. The census lives in the
worktree whose branch is `docs-wip`, resolved BY BRANCH the way
`tools/docs_land.sh` does — never a hard-coded path and never the primary
checkout.

```bash
python3 tools/probe_census.py --print          # what the live registry implies
python3 tools/probe_census.py --seed           # create/migrate in docs-wip
python3 tools/probe_census.py --validate       # check the inventory
python3 tools/probe_census.py --record R.json  # ingest one probe-flake-result/v1
python3 tools/probe_census.py --summary        # every probe's current statistic
python3 tools/probe_census.py --summary --probe KEY --json
python3 tools/probe_census.py --summary --as-of 2026-08-21T05:00:00Z \
    --stale-after-days 7
python3 tools/probe_census.py --promotion-candidates   # who could be promoted
python3 tools/probe_census.py --promotion-candidates --json \
    --as-of 2026-08-21T05:00:00Z --stale-after-days 7
python3 tools/probe_census.py --probe KEY --set-acceptable-failures 2 \
    --justification "two known engine-side races"
python3 tools/probe_census.py --probe KEY --set-acceptable-failures 7
python3 tools/probe_census.py --probe KEY --set-acceptable-failures 0 \
    --clear-justification
python3 tools/probe_census.py --probe KEY --set-estimate 480
python3 tools/probe_census.py --defer --probe KEY \
    --reason "the required content is not implemented" \
    --resume-when "the planned content assets merge"
python3 tools/probe_census.py --resume --probe KEY
```

An X update that omits `--justification` never clears the stored text —
`--clear-justification` is the only thing that does, and it may not be combined
with `--justification`. `--justification` stores its argument verbatim, so
every literal (`keep`, `none`, text with surrounding whitespace) is storable.

**The acceptable-failure policy (#1430).** X is how many failures a COMPLETE
ten-run measurement may show and still be acceptable, so the admissible values
are 0 through 9: an X of ten would declare a probe that never passes
acceptable. A result is acceptable at `failures <= X` and over tolerance at
`failures > X`, so X=1 accepts both 10/10 and 9/10 and rejects 8/10. That is a
statement about ONE ten-run result, not a reclassification of the probe —
`tools/ci_probes.py` remains the authority on which probes are flaky,
base-failing or scenario-heavy. `tools/probe_flake.py` accepts any positive
`--runs`, and a measurement with another run count stays valid data this
threshold reports as `not-comparable` rather than rescaling X into a rate.

**One measurement, never a cohort's pooled totals.** A cohort accumulates
same-commit runs, so its combined counts are the right basis for its `rate` and
the wrong one for a fixed-N threshold: two five-run measurements are not a
ten-run one, and two ten-run measurements are not a twenty-run one. The policy
is therefore evaluated against the authoritative cohort's LAST-APPENDED
complete ten-run sample — append order being what "newest" already means here,
since commit hashes do not compare — and reports `not-comparable` when the
cohort holds no such measurement.

**X=0 is the default and every probe has one.** #1428 staged a nullable X while
the policy was being chosen; there is no null X any more, and no
`--set-acceptable-failures none`. A fresh seed, a `probe-census/v1` migration
and a newly registered probe all arrive at 0, and `--seed` initializes an
existing record whose X is still null — that single null-to-zero transition is
the ONLY automatic policy repair there is, and the preservation gate permits
exactly it: every non-null X, justification, estimate, cohort, sample and
attempt comes through untouched, on a retired row as much as a live one. A
malformed stored X (a boolean, a string, a float, a negative, ten or more)
stays VISIBLE as a refusal rather than being silently corrected.

**An X above 0 must say why, and may not be CI-eligible.** It needs a
non-whitespace justification, already stored or supplied in the same command —
a tolerance without a stated reason is indistinguishable from a bug someone
gave up on. `--clear-justification` is therefore valid only while setting X
back to 0. And because CI stops on a single failure, tolerance is a manual-only
concept: a row with X>0 that `tools/ci_probes.py` now classifies CI-eligible is
refused, including during `--seed`'s promotion reconciliation, so a promotion
reports the conflict instead of quietly erasing a maintainer's decision. Reset
X to 0 first; the written reason survives.

These three rules are CODE, not schema, because each spans fields — and they
are applied ASYMMETRICALLY, unlike the declared schema and the cross-field
invariants: on every mutation's CANDIDATE and on `--validate`, never on the
stored side, since `--seed` has to be able to READ a null X in order to
initialize it. `--validate` reports every offending row in one pass alongside
inventory drift; a candidate refusal names the first and counts the rest, and
writes no bytes.

`--summary` (#1429) is the selection-facing view of what those measurements
MEAN over time. The newest cohort is the current statistic and DISPLACES the
previous one without deleting it; runs accumulate only within one commit hash,
and a cohort's rate is recomputed as `sum(failure_count) / sum(requested_runs)`
over its samples rather than averaged across batches of unequal size. Commit
hashes do not compare, so "newest" is append order: an A → B → A sequence ends
with a THIRD cohort keyed by A, never with the first reopened. Every displaced
cohort is retained for the lifetime view, and the authoritative cohort is
`current` when one exists and `history[-1]` otherwise — which is where a probe
promoted to CI eligibility keeps its newest measured statistic.

Staleness is purely AGE-based. A commit never invalidates a record and
repository HEAD moving is not a census event at all: only a measurement changes
census state. Age runs from the cohort's own freshness anchor — the latest
measurement timestamp contributing to it, never the commit's date, the file's
mtime, the moment of ingestion or live HEAD, so an out-of-order same-commit
result adds counts without dragging the anchor backwards — to an evaluation
time the CALLER supplies (`--as-of`, defaulting to now), against a horizon the
caller supplies (`--stale-after-days`, defaulting to 14). The boundary is
inclusive, and age is clamped at zero so a future-anchored cohort is fresh
rather than negatively old. Each row reports `measured`, the exact commit, the
latest measurement, the nonnegative age, the stale flag, and the combined
run/failure counts and rate; every measurement field of an UNMEASURED probe is
null, so a zero failure rate can only ever mean an observed zero. Each row also
carries its `acceptable_failures` and the `tolerance` of the authoritative
cohort's newest complete ten-run measurement — `acceptable`, `over-tolerance`,
or `not-comparable` when there is no such measurement to compare, an unmeasured
probe included. The table
prints the commit IN FULL, in its last column: a selection-facing row reports
the exact hash the statistic was measured on, and an abbreviation is not that
hash.

Semantic validation is narrow and fails closed at BOTH boundaries. Before a
`status: "ok"` sample changes a cohort, and again when an already-stored cohort
is read, the commit must be a real 40-character lowercase-hex identity — not
`probe_flake`'s `unknown` placeholder, which is a well-formed and schema-valid
result field but names no commit — the timestamp must parse as UTC, and the
aggregation counts must be usable nonnegative integers. The STORED current
cohort is held to the same standard on the INGESTION side too, because the
append-or-archive decision reads it: a legacy or hand-edited cohort that cannot
be summarized refuses the measurement outright rather than being extended or
archived and only failing on a later read. Each refusal is controlled and
writes nothing. A harness error is deliberately NOT gated: it reads no cohort
and contributes to none, and unmeasurable provenance is exactly what the
attempt log retains. The CROSS-FIELD invariants remain #1493's.

**CI-promotion candidates (#1441).** `--promotion-candidates` reports what a
person needs in front of them before editing `tools/ci_probes.py`, and it edits
nothing itself. Promotion has two halves and only one of them is measurable:
the census can say a probe is RELIABLE, and it cannot say whether the probe
covers enough to be worth a slot on every matching PR, whether its wall time
fits the gate's budget, or whether the CI runner can host it at all. Those stay
a human judgement, and so does the promotion.

A probe is RELIABILITY-QUALIFIED when every measurable precondition holds: it
is registered and LIVE-classified manual-only; its protocol is
`probe-result/v1` (a legacy probe emits no structured result, so there is
nothing to have measured); its stored X is integer ZERO — not "non-positive"
and not unset, because only a zero states that the probe is expected to pass
every run, and a null X states no expectation at all; it has a CURRENT cohort
rather than an archived one; that cohort is FRESH against the caller's horizon;
that cohort is COMPLETE; and it shows zero failures AND zero timeouts, which
`probe_flake` counts separately. A probe failing any of these is reported in
neither list — the report answers "what could a human promote?", and a row
with nothing measured is not an answer to it.

COMPLETE means three things, because a spotless-looking cohort can be missing
runs in ways its own counts cannot show: it reaches the policy's ten runs
pooled across every sample; every sample finished EXACTLY the runs it
scheduled; and no harness error is charged to it. That second one is checked
PER SAMPLE and in both directions, because pooled totals cannot answer it — a
9-of-10 beside an 11-of-10 pools to a flawless 20 of 20 while holding a
measurement that lost a run, and more completions than were requested is a
count nothing could have produced. That last is what a harness error looks
like in the record — an ATTEMPT and no sample, so ten scheduled runs of which
one never reported are indistinguishable from nine clean ones by counting
alone. Attribution FAILS CLOSED: an attempt is excluded only when provably
outside the cohort — a usable commit identity that is not the cohort's, or a
usable timestamp strictly before the cohort opened — so the `unknown`
provenance a harness error legitimately carries counts against it. "We cannot
tell which cohort lost a run" is not evidence that this one did not.

Statistics are POOLED over the whole cohort, never taken from the newest sample
and never averaged across samples of unequal size: summed requested and
completed runs, summed failures and timeouts, and a rate recomputed from the
combined numerator and denominator. Duration is reported as TWO fields, because
they answer two questions: `observed_worst_elapsed_seconds` is the MAXIMUM
`worst_elapsed_seconds` across the cohort, and `estimated_worst_case_seconds`
is the record's stored estimate. A missing estimate displays as `unset` and is
never filled in from the observation beside it.

Qualified probes are then split by their MANUAL-ONLY REASONS, every one of
which is reported (`ci_probes.MANUAL_ONLY_REASONS` records one entry per
independent ground since #1440, and several probes carry more than one). Only
`flaky` and `unclassified` are grounds a measurement can answer, so a probe
lands in the ready list only when EVERY declared category is one of those, and
a single `needs-gpu`, `slow/worldgen-heavy`, `scenario-heavy`, `targeted` or
`base-failing` puts it in the mechanically-blocked list however clean its runs
are — a clean GPU probe is not a disappointment, it is a probe whose obstacle
was never flakiness. That allowlist FAILS CLOSED by construction: a category
added later, or one this tool has never heard of, is absent from it and
therefore blocks, and a probe with no declared reason at all is blocked rather
than vacuously ready.

Every cardinality in the report is derived from the live registry. There is no
frozen probe total anywhere in it, so it stays correct as probes are registered
and promoted.

**A CI-eligible probe takes no measurement at all (#1431).** "A promoted probe
receives no further census samples" is a STORAGE invariant, not only a
reporting one: `--record` refuses a probe `tools/ci_probes.py` currently
classifies CI-eligible, before the census is locked and without writing a byte,
and refuses a harness error for one on the same ground. `probe_flake.py`
already refuses to RUN such a probe, but a result document outlives its run —
one measured before a promotion, or replayed from an artifact tree afterwards,
is still well-formed and schema-valid. Eligibility is read LIVE from the
registry, never from the stored row's classification, which a census not yet
reconciled by `--seed` still holds at its old value. The retained history stays
exactly as the promotion left it.

That is the whole of what a promotion does to the census: the probe keeps its
row in the global manifest with an updated classification, `--seed` ARCHIVES
its current cohort into `history` rather than deleting it, its attempt log is
untouched, it leaves `--promotion-candidates`' manual-only report, and it
accepts no further samples.

`--print` never touches the docs worktree. `--seed` is the ONLY operation that
migrates: it creates an absent census and migrates a `probe-census/v1`, `/v2`,
`/v3` or `/v4` one losslessly — the v2→v3 transition adds only the empty claim
log, v3→v4 only the empty outcome log, and v4→v5 only a null `deferred` field,
keeping every policy field, cohort, sample, attempt, claim and outcome — and
reconciles inventory drift — appending newly registered probes, refreshing
inventory metadata, retaining a row whose probe left the registry for a person
to dispose of, and archiving a `current` cohort when a probe becomes
CI-eligible. It never regenerates accumulated census data.
`--record`, `probe_census.record_claim`, `probe_census.record_outcome` and
the policy and deferral operations refuse, naming `--seed`, when the census is
absent or still on an older schema.

Every mutation is one locked read-modify-write: a cross-process `flock` keyed
by the resolved target, held from the read through serialization and the
preservation checks to a same-filesystem `os.replace`. Any failure before the
replacement leaves the old bytes exactly as they were. Only summarized
outcomes and external artifact references are stored — no stdout, no protocol
stream, no engine log. Exit codes: 2 for a missing or unusable docs worktree,
1 for inventory drift and every controlled refusal.

Shape validation is DECLARED, in `tools/probe_census_schema.json` — a JSON
Schema 2020-12 document, self-checked against that draft when it loads, that
describes the v1 seed, frozen v2/v3/v4 censuses, the current v5 census and the
incoming `probe-flake-result/v1` document alike. Each older root definition is
FROZEN at the record it really held: it now describes migration INPUT, so
widening it would make a stored older census invalid for lacking the field
migration exists to add. Every object in it is closed (`required` plus
`additionalProperties: false`, with a nullable field REQUIRED and
null-inclusive rather than optional), so a deleted field is a violation rather
than an absence. The stored census is checked before any operation transforms
it, an incoming result before one nested field of it is read, and the complete
candidate immediately before the atomic replacement; each refusal names the
offending JSON path and changes no bytes. `jsonschema` is therefore a required
dependency, pinned in `tools/requirements-assets.txt` and repeated in
`.github/ci/Dockerfile`: an absent one is a single loud error naming the
install command, never a skipped check. `--print` is the exception that proves
it validates nothing — it renders the live registry, reads and writes nothing,
and stays dependency-free so a fresh checkout can run it.

The CROSS-FIELD invariants are CODE (#1493), because rules that span fields
cannot be declared: accepted attempts reconcile against the samples retained
across `current` and `history`; `accepted` agrees with `status`; a harness
error never reports completing every requested run; `check_counts` is keyed by
exactly the descriptor's declared checks and each entry is the tally `runs`
shows; a PASS run carries no FAIL check; and a cohort holds one commit's
samples. Each rejects state no real run could have written, and each
runs on both sides of a mutation exactly as the schema does — so an
inconsistent census stops `--record`, the policy updates, `--seed` and
`--validate` alike instead of being rewritten and made durable.

Still deliberately absent: any requirement that the census AGREE with the live
registry — complete-inventory drift stays `--validate`'s report and `--seed`'s
repair, never a write precondition.

Nothing at runtime reads it: `probe_flake.py` takes protocol status from its
own in-repo `PROTOCOL_PROBES` and check identity from each probe's descriptor,
so a checkout with no docs worktree behaves identically.

`python3 tools/test_probe_census.py` is its deterministic, engine-free
self-test, and since #1429 it runs unconditionally in CI's probe-runner
self-test step and in `make ci`. It drives the real checked-in schema — a valid
census and result against every declared definition, each nullable field
deleted, non-object per-run `checks` including truthy values, unexpected
properties in every representative nested object, invalid enum/range/length
values, non-finite numbers, and an environment where `jsonschema` is genuinely
unimportable — asserting for each refusal that nothing was written. Every
cross-field rule gets three cases: a rejecting fixture that is still
SCHEMA-valid, so only that rule can reject it, driven through `--record`, a
policy update, `--seed` and `--validate`; the legitimate flows it must not
over-reject (an empty census, a TIMEOUT run carrying a FAIL check, a harness
error that completed no run at all, cohort rollover, promotion); and a
mutation check that lifts that one rule out of the production rule set and
requires its own fixture to be accepted again. Its cohort cases inject a fixed
evaluation time rather than reading a clock: unequal same-commit batches (which
an unweighted mean would report as 0.30 instead of 0.17), an A → B → A
sequence, HEAD moving with no measurement, the inclusive staleness boundary
from both sides, a future anchor, an unmeasured probe beside a real zero rate
and a cohort with no denominator, a promoted probe whose statistic lives in
`history[-1]`, the placeholder/malformed refusals at the incoming,
stored-cohort-on-ingest and stored-read boundaries alike, and the exact commit
in the rendered table.

### `probe_census_page.py` — the manual-only census page (#1431)

`docs/probe_census.json` stays the authoritative global manifest — every
registered probe, CI-eligible and manual-only alike, with its retained history.
This renders the READABLE half of it, `docs/probe_census.md`: one row per
MANUAL-ONLY probe, in stable key order, beside the manifest in the same
`docs-wip` worktree. It writes nothing else, migrates nothing, and neither
publishes nor lands the page.

```bash
python3 tools/probe_census_page.py --generate
python3 tools/probe_census_page.py --generate --as-of 2026-08-21T05:00:00Z
python3 tools/probe_census_page.py --audit
python3 tools/test_probe_census_page.py   # the synthetic self-test
```

**Manual-only, deliberately.** Neither command, and not the self-test, runs in
`make ci` or GitHub CI — nothing here is a gate on anything, and the page it
audits lives in a worktree a fresh clone does not have.

**It has two sources and needs both.** The manifest holds classification,
protocol status, the acceptable-failure policy, the duration estimate and the
measurements; #1440's reason records live in `tools/ci_probes.py`'s
`MANUAL_ONLY_REASONS`. Joining them is what lets one row answer both "why is
this probe not in the blocking gate?" and "how flaky is it, on which commit,
how long ago?". The reason categories render in each probe's DECLARED order,
which is the order the registry states the independent grounds in.

**Which probes appear is derived, never listed.** The row set is exactly
`ALL_KEYS - CI_ELIGIBLE`, computed from the live registries, so a newly
registered probe appears the day it registers and a promoted one leaves the
page the day it is promoted — while keeping its manifest row, its policy and
its whole archived history, and receiving no further samples (see the
`--record` refusal above).

**The source manifest is validated before either operation.** A missing,
duplicate, extra, stale-classification or stale-protocol manifest row would
otherwise yield a subset page that is perfectly consistent with itself and
quietly wrong about which probes exist, so `--generate` and `--audit` both run
`probe_census`'s shape check and inventory comparison first and stop there —
`--audit` before it so much as READS the page, since a stale manifest beside a
missing page must name the manifest rather than the missing page.
#1430's acceptable-failure policy is deliberately NOT applied: a null X is
state the page must be able to DISPLAY (`unset`), and a policy problem is
`probe_census --validate`'s report either way.

**Three explicit measurement states, and no value invented.** A row with a
current cohort is `measured`. A row with none is `not yet measurable` when its
protocol status is `legacy` — `probe_flake.py` refuses to run a legacy probe at
all — and `not yet measured` when it has been migrated and simply has no sample
yet. `unset` (no X) and `unknown` (no duration estimate) are their own cells,
so an absent value never renders as a zero, and an unmeasured row shows no
rate, no commit and no age. The ladder is cohort-FIRST: a legacy row that
nonetheless holds a cohort reports the cohort, because `probe_flake.py` cannot
produce one and hiding a stored measurement behind "not yet measurable" would
be a lie rather than a safeguard.

**Rate is pooled, age is relative to a DECLARED as-of.** A row's rate is the
current cohort's total `failure_count` over its total `requested_runs` —
`probe_census.cohort_statistic`'s own arithmetic — because averaging the
samples' stored rates would weight a three-run measurement like a ten-run one.
Age runs from that cohort's freshness anchor to the as-of time the page records
in its own header, clamped at zero; that is why the header carries it, and why
`--audit` recomputes every age from it rather than from a clock.

**The audit checks values, not membership.** Missing, duplicate and extra rows
are diagnosed independently, so one never masks another, and an extra row says
WHY it does not belong (CI-eligible, or not registered at all). Every surviving
row is then compared cell by cell against the row the same generator would
render, so a hand-edited protocol, reason set, state, rate, X, duration, commit
or age is a finding naming that column. A page whose table is not the generated
one is one structural finding rather than ninety derived ones, and a header
field declared TWICE is refused rather than read first-wins: a second,
contradicting `as-of` under ages computed from the first would otherwise audit
clean.

`python3 tools/test_probe_census_page.py` is the deterministic, engine-free,
offline self-test: synthetic censuses and a synthetic probe registry in a
throwaway tree, an injected as-of rather than a clock, and the real module
driven rather than a copy. It covers the whole-registry manifest filtered down
to the manual-only page, the three measurement states, several reason
categories in declared order, promotion (the row leaves while the manifest
entry, policy and archived history stay, and `--record` then refuses), missing
/ duplicate / extra / reordered rows, a tampered value in every audited column,
the source-manifest gate, byte-stable rendering, and the CLI's exit codes
against a real two-worktree scratch repository.

### `probe_external_evidence.py` — the Codex `$test` record, read-only (#1432)

The Codex `$test` skill independently records coordinated non-CI runs against
this same probe set, with exact commit provenance and interpreted
observations. This reports what it knows about ONE registered probe. The two
systems run side by side and must not interact; this is strictly a reader.

```bash
python3 tools/probe_external_evidence.py --probe role
python3 tools/probe_external_evidence.py --probe transfer_order --json
python3 tools/test_probe_external_evidence.py   # the synthetic self-test
```

State lives at `<git-common-dir>/codex-test`, resolved with `git rev-parse
--git-common-dir` — in a linked worktree `.git` is a pointer file, so a
literal `.git/codex-test` would be wrong. That tree is untracked and
machine-local: it is ABSENT on a fresh clone, on another machine, and wherever
Codex is not installed, and its absence is a normal "no external evidence"
result (exit 0, no diagnostic). A state root that EXISTS but is not a directory,
and an existing but unreadable or malformed registry or report, are different —
each a non-fatal diagnostic beside whatever could still be read. Absence and
damage are told apart with `lstat`/`stat` directly through the shared
`entry_state` helper, never with `Path.exists` / `.is_dir` / `.is_file`, which
swallow `OSError` and answer False: under those an unstattable state root reads
exactly like one that was never created, and a consumer failing closed on
unreadable active-run state would never learn the difference. A recorded report path that is simply not there is data (a run
has not written it yet); one that EXISTS but is not a regular file is damage,
and damage is diagnosed. So is a registry carrying JSON's non-standard
`NaN`/`Infinity` constants, which Python's `json` would otherwise read and
write straight back out, making `--json` invalid JSON. Every filesystem call
catches `ValueError` beside `OSError`, because a registry field is arbitrary
external text: a path string with an embedded NUL raises the former, and one
malformed record must cost its own run a diagnostic, never the whole read.

It never writes, never takes a `$test` lock, and never invokes the `$test`
coordinator at all. That last part is not squeamishness: every one of the
issue's four permitted read subcommands (`list`, `show`, `proposal-list`,
`value-status`) goes through the coordinator's `read_registry` ->
`locked_registry`, which takes the exclusive `registry.lock` flock, `mkdir`s
the state tree and rewrites `registry.json` with a fresh `updated_at`. Reading
the JSON directly is the only way to honour the permission boundary — and the
only way that works when the machine-local coordinator is not installed.

A `probe_runner_registry.PROBES` key maps to TWO `$test` run ids by
underscores-to-hyphens: `probe:<hyphenated-key>` for an ordinary execution and
`probe-flake:<hyphenated-key>` for a flakiness measurement, so `transfer_order`
maps to both `probe:transfer-order` and `probe-flake:transfer-order`. Both name
the same canonical probe and both are its evidence, while each matched run
reports its own `test_id` and `test_kind` so the two stay distinct in history.
Both are derived from the KEY and never from the script filename —
`persistence_contract_sweep.py` has no `_probe` suffix to strip. Matching is
EXACT against the generated ids (which is also how the reverse lookup works, so
`probe:transfer_order` never resolves onto the registered `transfer_order`), and
a key the registry does not carry is a controlled unknown-key rejection
(exit 2), not a "no external evidence" answer.

Per matching run it reports the run id and state, the tested commit, the
MECHANICAL execution outcome (from the registry's own `execution_status` /
`test_exit_code`, never inferred from the report's interpretation), the
recorded duration and the observation status. A value the record does not
carry reads as unavailable, never as a fabricated `false` or `0`, so active
and legacy runs are surfaced rather than dropped. The whole known history is
reported; there is no limit option and no default truncation. Report reads are
confined to resolved `*.test-result.md` files directly beneath the state
tree's own `reports/`, so a recorded path can never widen read scope. Both
fixed names are confined the same way first: `registry.json` and `reports/`
are each resolved and then required to still be an immediate child of the
RESOLVED state root, of the right kind. A symlink at either name would
otherwise relocate what gets read while the name itself still looked right —
and a regular file standing in for `reports/` would make every recorded report
read as a silent `absent`.

Every diagnostic is emitted twice: as free text in `diagnostics`, and in
`diagnostics_detail` tagged with the state it concerns (`registry`, `record` or
`report`). A consumer that must fail closed on unreadable ACTIVE-RUN state has
to tell an unparseable `registry.json` apart from one finished run's missing
report, and matching on diagnostic prose to do that would be a trap.

**The reader itself makes no scheduling decision.** A run appearing, passing,
failing or recording observations changes no census sample and no statistic —
one interpreted `$test` run is context, not a measurement in the lab's
statistics. What the read is FOR is wider than presentation: since #1433 its
evidence is also consumed by `probe_inflight.py` below, which owns the
active-run predicate and the eligibility verdict. The read-only boundary is
unchanged and unconditional — no coordinator invocation, no lock, no state
creation, no `registry.json` write, no report write.

### `probe_inflight.py` — is other work already in flight for this probe? (#1433)

Measuring a probe's flakiness costs ten or more runs and can take an hour. This
answers, for ONE registered probe key, whether that work should start at all:

```bash
python3 tools/probe_inflight.py --probe injury_log
python3 tools/probe_inflight.py --probe injury_log --json
python3 tools/test_probe_inflight.py   # the synthetic, offline self-test
```

Exactly one of three results comes back. `clear` — every required source was
read completely and nothing matched. `in-flight` — something matched, with
inspectable evidence for every match. `source-error` — a required source could
not be read or interpreted completely, with an actionable diagnostic. It is a
point-in-time ELIGIBILITY SNAPSHOT: its consumer takes it immediately before
selecting or claiming, and this component never polls or cancels a measurement
already under way. Candidate iteration and ranking belong to #1435; claim
acquisition and launch to #1434/#1436.

Four sources are consulted: active `$test` runs (through
`probe_external_evidence.py`, under both identities), every page of the target
repository's open issues, every page of its open pull requests, and the four
findings reports (`non_ci_test_audit_findings.md`, `ci_test_audit_findings.md`,
`python_testing_findings.md`, `code_health_findings.md`) in BOTH the checked-out
repository and a branch-resolved `docs-wip` worktree. Issues and PRs match on
TITLES only; reports match on the full finding heading, with the heading marker
authoritative for whether the finding is still open. Since the title is the
whole subject, a tracker record whose title, number or state cannot be read is a
`source-error`, not a non-match — "no tokens" and "no match" would otherwise be
the same answer.

It fails closed. A partial scan never returns `clear`, and the read-only
boundary is absolute: no coordinator invocation, no lock, no state directory, no
`registry.json` write, and not one byte written to any findings report.

### `probe_claim.py` — one probe, one claimant (#1434)

Many `/deflake` agents run at once, one probe each. Nothing stopped two of
them measuring the SAME probe: `run_probes.py` coordinates only the probes
inside ONE runner process, and `probe_flake.py`'s port leases coordinate
host-global ports, not probe identity. This is the claim that does, and the
one claim-aware orchestration boundary that holds it.

```bash
python3 tools/probe_claim.py --probe role --runs 10 --result /tmp/role.json
python3 tools/probe_claim.py --status            # every claim in this repository
python3 tools/probe_claim.py --status --json
```

The claim is a FILE, created `O_CREAT|O_EXCL` at
`<git-common-dir>/probe-claims/<probe>.json`, carrying its owner, its
acquisition token and its lease. **The file is the lock**, which is the
deliberate opposite of `probe_flake.PortLease`: an `flock` dies with its
holder, and a claim must NOT — an agent SIGKILLed mid-measurement leaves real
ambiguity behind, so its probe stays unavailable until the lease elapses
rather than becoming instantly reclaimable. A sidecar `flock` exists all the
same and is not the claim: it serializes the read-decide-write of the claim
file for microseconds per operation, which is what makes TAKEOVER
single-successor — reclaiming a lapsed claim is unavoidably a read-then-write,
and two unserialized reclaimers is the race where the second lands on top of
the first's fresh claim.

Claims are keyed by the canonical `probe_runner_registry.PROBES` key, so the
several
human spellings of one probe cannot claim it twice, and the namespace is the
REPOSITORY-common git directory: every linked worktree of one repository
resolves the same directory, and none of the three things that would split it
— the current worktree, `--artifact-root`, `TMPDIR` — moves it. It is
repository-scoped rather than host-global because a port is a host resource
and a probe key is a repository's.

The lease is RENEWED rather than sized. `probe_flake.measure` accepts any
positive `--runs` and each run inherits a 900-second timeout, so no constant
TTL can outlive every supported measurement; a background renewer refreshes
the lease while the probe runs, and the lease only has to exceed one run's
worst case. A long, supported measurement therefore never becomes
reclaimable, while a dead holder's claim lapses one lease after its last
renewal. Two things make that real rather than nominal: the orchestration
boundary **refuses** a `--lease-seconds` below twice one run's timeout,
because a lease that can elapse while a single run is still going hands the
probe to a second agent mid-measurement; and claim timestamps carry
microseconds, because whole-second stamps round a lease DOWN — a sub-second
lease would be born already expired. A lease is required to be finite as well
as positive, at the boundary and in `acquire` alike: `float` parses `nan`,
`inf` and `-inf`, NaN fails every ordering comparison and infinity passes
every lower bound, so both slip past a bare `<` and reach `timedelta`, which
raises where this module promises a controlled refusal. It is bounded ABOVE
too, at the census `claim` schema's own 1e9 cap: a lease can be finite and
positive and still overflow `timedelta`, and one the acquisition record could
not hold would only move the refusal to after the probe was claimed. Every acquisition mints a unique token, and release, renewal and
takeover are all checked against it: concurrent reclaimers of one lapsed claim
yield exactly one successor, and an expired owner that exits late finds a
token that is not its own and leaves the successor alone. EXPIRY IS ONE-WAY:
a holder that stalled past its own lease cannot renew back to life, because
that would deny a claimant entitled to reclaim the probe and make the lease
meaningless whenever the holder is merely slow rather than dead. Every
instant a decision is judged against is read INSIDE the sidecar lock, never
before it: waiting for that lock can outlast a lease, so a pre-sampled instant
would stamp a new claim already expired and would judge a denial against one
that had since lapsed. An empty,
truncated,
unparseable or INCOMPLETE claim is treated as OCCUPIED until its own
filesystem age reaches the lease — which covers a crash between the exclusive
create and the payload write without letting a competitor straight in.
Completeness is checked against every field a claim carries, not merely the
ones an ownership decision reads: a partial file holding a probe, a token and
a far-future expiry would otherwise read as live forever, never aged out, and
one stray write would wedge the probe. Well-typed fields can still contradict
each other, so consistency is checked too: a one-second `lease_seconds` beside
an expiry years away is the same wedge, and the lease has to be the distance
between the two timestamps that describe it, within exactly the rounding a
legacy second-precision file carries.

`run_claimed_measurement` is the orchestration boundary, in order: reject an
unmeasurable probe before claiming anything; acquire, where a DENIED claimant
stops having created no artifact directory, no result document and no census
entry, reporting the current owner and the claim's age; record the acquisition
in the census BEFORE the probe runs — inside one hold of the sidecar lock that
renews the lease first, because that write is a census mutation and can block
on another writer for as long as that writer takes — releasing the claim and
refusing outright if that write fails or no `docs-wip` census is reachable;
REASSERT ownership immediately before the probe starts and refuse to start it
if that is gone, since beginning a measurement this run no longer owns is the
duplicated work the claim exists to prevent; measure, renewing
throughout; RETAIN the completed measurement on disk — the `--result` path or
the run's own invocation directory — before anything that can fail touches it,
because the measurement is the expensive thing and the retained file is a
complete `probe-flake-result/v1` that `probe_census.py --record` ingests once
the cause is fixed; ingest the result inside ONE hold of the sidecar lock that first
re-reads the claim file, confirms the claim is still ours and still live, and
renews the lease so it cannot elapse mid-commit — checking and then writing
would leave a gap in which a slow commit outlives the lease and another agent
starts measuring, and under the hold no acquisition can interleave. If the
claim was already lost, ingest NOTHING: a probe two agents may have been
measuring at once has no attributable result, so the artifacts are kept and
the run reports the loss. Then release, token-checked. The lock order is
claim-then-census everywhere, so the two never wait on each other.

`probe_flake.py` is deliberately unchanged by all of this. Its contract is
that a checkout with no `docs-wip` worktree behaves identically, so the
census-backed claim lives in the orchestration path and the low-level
measurement API stays usable on its own.

Exit codes: 0 a measurement that ran and was ingested, whatever rate it
observed; 2 rejected before anything was claimed; 3 ALREADY CLAIMED; 4 a
harness error, whose non-accepted attempt is still ingested; 5 a claim audit
failure — before the probe ran, the acquisition was not recordable so nothing
ran; after it ran, the measurement happened and its retained file is named in
the diagnostic, and re-running the probe is never the recovery; 6 no leasable
port; 7 the claim was lost — before the probe started, so it was never run, or
while it ran, so nothing was ingested. An unusable `--result` is refused up
front, before anything is claimed — the target itself, not merely its
directory, so an existing directory, an unwritable file or a dangling symlink
fails in the first second rather than after an hour of engine time.

Gate: `python3 tools/test_probe_claim.py` (in CI and `make ci`) — engine-free
and GPU-free, but genuinely multi-process: its concurrency cases race real
interpreters against a shared barrier file and its crash case SIGKILLs one of
them, because a claim that must hold between OS processes cannot be proved by
threads.

That bare invocation is the whole gate and the only one CI or `make ci` runs.
Its 29 cases live with three independently changing contract owners, and
`--only` runs one owner's cases for iteration (#2100):

```bash
python3 tools/test_probe_claim.py --only claim          # 12 cases, ~7 s
python3 tools/test_probe_claim.py --only census         #  4 cases
python3 tools/test_probe_claim.py --only orchestration  # 13 cases
```

| Owner module | `--only` | Owns |
|---|---|---|
| `probe_claim_selftest_claim.py` | `claim` | the atomic claim and its lease: namespace and key validation, exclusive acquisition, cross-process contention, expiry, renewal, stale reclaim, owner-safe release, acquisition timing, malformed claims, managed exit, crash recovery, the renewer |
| `probe_claim_selftest_census.py` | `census` | acquisition recording, the claim log kept separate from the measurement log, lossless schema migration, and `probe_flake` staying usable with no `docs-wip` worktree |
| `probe_claim_selftest_orchestration.py` | `orchestration` | the claimed measurement end to end: denied and audit-failure paths, harness-error ingestion, pre-claim rejection, lease validation, lost claims, serialized audit and ingestion, the retained result and its `--result` destination, the CLI |

`probe_claim_selftest_support.py` is the single source of everything the three
share — the assertion helpers and the ONE failure accumulator behind them, the
synthetic registries, the scratch trees and scratch repository, the real
`probe_flake.Measurement` builder, and the subprocess programs the concurrency
cases race. None of the four is a gate of its own; `test_probe_claim.py` holds
no case body and is composition and selection only, spelling out the
interleaved run sequence the aggregate has always used and refusing to run at
all if it and the three inventories have drifted apart.

### `probe_select.py` — which probe does `/deflake` measure next? (#1435)

The census (#1428/#1429), the acceptable-failure policy (#1430), the in-flight
check (#1433) and the atomic claim (#1434) each answer one question about ONE
probe. This is the PURE DECISION that consumes all four and answers the one
that spans the roster: which single probe should the next measurement spend
its hour on?

```bash
python3 tools/test_probe_select.py   # the synthetic, in-memory self-test
```

There is no CLI, by design. Every input — the registry, the two
classifications, protocol status, the census document, the in-flight set, the
claim set, the evaluation time and the age horizon — is passed in, so nothing
here boots an engine, runs a probe, reads a wall clock, resolves a docs
worktree, or touches the census file or the claim lockfiles the prerequisites
own. Reading that state, claiming, launching, recording and releasing are all
#1436's.

Borrowed vocabulary is consumed as-is. What a valid run is, what a failure is,
which cohort is current and what makes a measurement stale are #1429's
definitions, reached through `probe_census.summarize_entry`; N is
`probe_census.POLICY_RUN_COUNT` and each probe's X is the validated
`acceptable_failures` #1430 stores in its census record. Manual-only
eligibility is KEY MEMBERSHIP of `CI_ELIGIBLE` versus `MANUAL_ONLY_REASONS`
and nothing else — #1440 made the latter's value a tuple of `Reason` records,
and no selection may depend on their count, category or shape. Protocol status
is likewise supplied, never inferred: a legacy probe is skipped with a
recorded `requires protocol migration` reason rather than treated as
unmeasured, because `probe_flake.resolve_probe` would refuse to run it.

Every eligible probe lands in exactly one rung, or in none. **1. Incomplete
measurement** — no cohort, or fewer than N valid runs; fewest valid runs
first, so an unmeasured probe (zero valid runs) precedes every partial cohort.
**2. Over tolerance** — at least N valid runs and a failure rate strictly
above X/N; worst rate first, then oldest. **3. Stale within tolerance** —
oldest first, then worst rate. A fresh probe within tolerance is in NO rung:
that is the successful terminal state, not a fourth one. Every remaining tie
breaks on the exact registered key, so registration order, script filenames
and census array order move nothing.

Tolerance is decided as `failures * N > X * valid_runs`, in integers, so a
cohort sitting exactly on X/N is within tolerance whatever its size — 3
failures in 30 valid runs against X=1 is at the threshold, not over it. A
cohort is only divided once it has N valid runs, so a zero-run cohort reaches
rung 1 with no rate arithmetic at all.

Three outcomes come back: a selected registered key, a valid census in which
nothing qualifies, or malformed census data. The reason channel never changes
WHICH one: `no-candidate` is returned both when every eligible probe is fresh
and when every probe was excluded, and `Selection.skipped` carries the
recorded reasons that tell "the roster is healthy" from "everything is
claimed" — the difference between stopping and waiting. A structurally broken
container always errors, and so does a record for a REGISTERED, MANUAL-ONLY
probe that is missing a field ranking needs, whatever that probe's in-flight,
claimed or legacy status — transient external state must not hide corrupt
persistent data. A record keyed outside that domain, unregistered or
CI-eligible, is ignored instead: reconciling the census against the live
registry is `probe_census.validate_manifest`'s job. An ABSENT record is not
malformed at all; it is rung 1's commonest case.

`test_probe_select.py` supplies its own registries, classifications and census
documents rather than asserting against the live ones, because CLAUDE.md's
CI-promotion procedure moves keys between the two classifications and a gate
pinned to live membership would redden on unrelated registry work. Purity is
proved mechanically: subprocesses, sockets, `open`, `os.replace`, `time.time`
and `datetime.datetime.now`/`utcnow` are tripwires for the whole run, the last
pair installed by swapping the `datetime` module inside `probe_select` and
`probe_census` for one that refuses a clock read while still delegating
`isinstance`. Like #1431's, #1432's and #1433's self-tests, it is not wired
into `make ci` or GitHub CI.

### `probe_resource_lock.py` — cross-process shared/exclusive resources (#1436)

`probe_runner_resources.ResourceLedger` is a reader/writer lock over the probes
inside ONE
runner process — its own docstring says so, and the scheduler owns it from a
single thread. So it coordinates nobody else: a `/deflake` measurement and a
`tools/run_probes.py` sweep are independent processes, both driving the same
checkout, and `config_state_probe.py` taking `repo-config` exclusively could
not stop a foreign engine booting into the tracked `config/` tree it asserts
against. This is the same model between processes.

```bash
python3 tools/test_probe_resource_lock.py   # the deterministic self-test
```

There is no CLI. `probe_runner_resources` and `tools/deflake.py` both import
it, and both read the interests from
`probe_runner_resources.shared_resources` / `exclusive_resources`, so there is
one conflict model rather than two.

The lock IS an `flock` on one file per (namespace, resource): `LOCK_SH` for a
shared interest, `LOCK_EX` for an exclusive one. Nothing counts holders or
decides staleness — the two things a hand-rolled protocol gets wrong — and a
holder that is SIGKILLed releases everything the moment it dies, which is what
makes waiting for a resource safe rather than a wedge. Lock files are never
unlinked, for the reason `probe_flake.PortLease` records at length.

The files are FLAT entries in `/tmp`, the arrangement
`probe_flake._machine_wide_scratch` establishes and for the same reason: a
dedicated subdirectory is owned by whichever account created it, and a
directory's owner may unlink entries in it however the sticky bit is set, so
that account could replace a held lock's pathname and hand one exclusive
resource to two processes. `_check_shared_dir` verifies sticky, root-or-us
ownership and writability rather than assuming them, and repairs nothing.

The namespace is the repository's COMMON git directory, hashed:
`probe_engine.REPO_ROOT` is derived from the tools file's own location, so every
linked worktree resolves a different value and two worktrees of one repository
would namespace separately while driving the same tracked tree. A checkout git
cannot answer for is a controlled refusal, never a path-derived guess.

`acquire` takes the whole interest set or none of it, in sorted name order and
always non-blocking, so nobody ever waits while holding and a deadlock cycle
cannot form; a conflict rolls back everything already taken and raises
`ResourceBusy` naming the resource, the interest and whatever live holders
could be identified. `wait_acquire` is the polling wrapper the sequential probe
runner uses. Release drops OUR file descriptors and nothing else, so a late
cleanup cannot take a successor's resource away — by construction, not by a
token check.

Holder notes are DIAGNOSTICS ONLY: mutual exclusion is the flock, and a
refused acquirer reads the notes so it can say who is in the way. A note is
live while its own flock is held, never by the pid it records.

### `deflake.py` — one bounded census measurement, end to end (#1436)

The orchestration that puts the five components in order: `probe_select`
(#1435) picks a probe, `probe_claim` (#1434) makes it this agent's,
`probe_resource_lock` keeps a foreign engine out of the tracked files it
touches, `probe_flake` (#1425) measures it, and `probe_census` (#1428/#1429)
records the result.

```bash
python3 tools/deflake.py            # one probe, one measurement
python3 tools/deflake.py --json     # the machine-readable outcome document
python3 tools/test_deflake.py       # the deterministic self-test (the gate)
python3 tools/test_deflake.py --only orchestration   # one owner's cases (#1436)
python3 tools/test_deflake.py --only handoff         # the handoff's (#1659)
python3 tools/test_deflake.py --only preparation     # engine preparation's (#1913)
```

No arguments select a probe, and there is deliberately no run-count or RTS
override: one invocation is one bounded measurement of whichever probe the
ladder chose, which keeps ownership, artifacts and census updates bounded and
makes every failure attributable to a single measurement. Repetition across
probes belongs to a surrounding workflow.

**The fixed measurement contract** is ten sequential runs at four RTS
capabilities. The two are INDEPENDENT dimensions and `-N4` is the second, not
the first: the engine's baked-in default is `-N -A128M`, so an unpinned run
takes every core and measures a different condition on every machine. Both
values are supplied to `probe_flake.measure` explicitly rather than left to
its defaults, because "the orchestrator declined to override a default" is not
a property a test can assert. The run count is
`probe_census.POLICY_RUN_COUNT`, the same N the ladder compares a cohort
against, so a private copy cannot drift into producing permanently incomplete
cohorts.

**Ownership, in order.** Select — a malformed census, or a `docs-wip` worktree
that cannot be resolved, fails before anything is claimed. Claim — losing the
selection-to-claim race is an ordinary no-work success that measures nothing,
writes nothing, releases nothing of the winner's and selects no second probe.
Audit — the acquisition is recorded before the probe runs, and a failure there
releases the claim and refuses to run. Resources — non-blocking, so a conflict
is `resource-busy` and the claim is given back rather than held hostage for
however long a foreign sweep takes. A **success-shaped outcome owns nothing**:
`resource-busy` and `claim-busy` are exit 0, which a surrounding workflow reads
as "nothing happened, move on", so if giving the claim back fails the retained
ownership becomes the result — the nonzero `managed-error` — rather than a
footnote on a status nobody investigates. Measure — resources held and the lease
renewed throughout. Record — the measurement is retained on disk first, then
ingested under one hold of the claim's sidecar lock. Release — only what this
invocation acquired.

**The commit cohort.** `probe_census.ingest_result` keys a cohort on the result
document's own `commit_sha`, and that stays the value ingested. `/deflake`
adds a REFERENCE: the commit is captured once, immediately after the claim,
and before recording all three of that reference, the document's own value and
a fresh `HEAD` read must agree. Ten engine-booting runs occupy many minutes to
over an hour and the PR drainer fast-forwards the primary checkout after every
merge, so a measurement really can straddle a HEAD change. A disagreement, or
a value that does not name a commit at all, is the nonzero `commit-changed`
outcome.

**Recorder outcomes are decided by a signal, never a message.**
`probe_census.update` guarantees unchanged bytes only for a failure BEFORE its
`os.replace`; the directory fsync after it used to reach a caller as an
indistinguishable bare `OSError`. #1436 added
`probe_census.CensusDurabilityUnconfirmed` for exactly this decision, so
`record-failed` (before) and `record-indeterminate` (after) are told apart
deterministically. The indeterminate case retries nothing, appends no
compensating record, and deliberately LEAVES the claim for token-aware
diagnostics and TTL recovery — census ingestion is append-only and
deliberately non-idempotent, so an automatic retry would duplicate the sample.
A census update that commits but whose claim release then fails is
`recorded-release-failed`: both facts reported, nothing rolled back or
repeated.

**The handoff (#1659).** On the exact `recorded` outcome — and on no other —
`/deflake` writes a `deflake-handoff/v1` document beside the retained result,
named after it, and reports the path in `handoff_document` (null on every other
outcome, so a consumer branches on the value rather than on whether the key is
there). It carries the probe, X, the target checks, the result document
EMBEDDED unchanged, the invocation, the `config/*.local.yaml` manifest and every
retained artifact.

It exists so the diagnosis step consumes a record the measuring process WROTE
rather than a hand-written account of a run nobody observed — a consumer given
one of those has to detect a forgery field by field, which has no natural
stopping point. So every value comes from what the process saw: argv and the
working directory captured at CLI entry, the configuration read under the
resource hold immediately before the first engine, the base port of each
completed run, the two settings passed to the measurement adapter, and the
acceptable-failure count read from the census row this invocation's own
recording transaction INSTALLED — not a reread, because the lock is gone by
then and another agent's policy edit would be attributed to this measurement.

Written LAST, after ingestion and a successful release, because
`commit-changed`, `record-failed`, `record-indeterminate`,
`recorded-release-failed` and `harness-error` can all happen after the
measurement and none of them is the complete attributable measurement a
diagnosis reads a handoff as. Capturing the configuration OPENS files, so it can fail
the way reading a file fails; that is a managed pre-measurement failure —
the claim goes back, no engine starts, and there is no handoff, because a
handoff describes a measurement. The write is STAGED and renamed, because only
`recorded` may leave a handoff and a failed write must leave none: writing in
place truncates first, so a disk filling mid-write would leave a partial
`*-handoff.json` beside the result while the command reported no handoff at
all. A `recorded` run with nothing retained to sit beside, or a handoff that
cannot be written, is the same existing nonzero `managed-error`: the committed census update is append-only and is neither
retried nor rolled back, both facts are reported, and the outcome vocabulary
does not grow.

**Outcomes.** Successful, exit 0: `recorded`, `no-qualifying-probe`,
`claim-busy`, `resource-busy`. Nonzero: `selector-error`,
`claim-audit-failed`, `commit-changed`, `harness-error`, `record-failed`,
`record-indeterminate`, `recorded-release-failed`, `managed-error`,
`interrupted`. Every no-work result says why no measurement began; every
nonzero one states which ownership, if any, remains.

#### Real runs are evidence, never a merge gate

A completed real ten-run `/deflake` invocation is useful manual pull-request
evidence, and it is best-effort:

- **real ten-run `/deflake` execution is manual, environment-sensitive
  evidence.** It boots roughly ten engines in sequence and can legitimately
  run for an hour;
- **it is NOT required by CI, branch protection, or merge.** The real command
  and any engine-booting ten-run measurement are deliberately absent from
  `make ci` and from GitHub CI;
- **no PASS/FAIL/TIMEOUT rate this lab measures is itself a merge verdict.**
  A rate is an observation about a probe, not a judgement about a change;
- **the repository's narrow automated CI and normal agent review remain the
  merge gates**; and
- **a maintainer may attach a completed real measurement to a pull request as
  supplemental evidence.**

Only a real `recorded` outcome proves the live end-to-end path. The other
legitimate outcomes — `no-qualifying-probe`, `claim-busy`, `resource-busy`, or
an environment error — remain valid smoke evidence but must not be described
as proving measurement, ingestion, artifact handling or release. There is
deliberately no forced-probe override to manufacture a completion with.

The gate is `tools/test_deflake.py`, which boots no engine: every collaborator
is an injected adapter, while `probe_census` and `probe_flake.Measurement`
themselves are driven for real against throwaway censuses, because the census
claims it checks are properties of the shipped recorder.

Since #2093 that file is composition and selection only. Its 50 cases live
with three independently delivered contract owners, each declaring its own
`CASES` inventory: `deflake_selftest_orchestration` (#1436's
select/claim/measure/record orchestration, 31 cases),
`deflake_selftest_handoff` (#1659's retained diagnosis handoff, 15) and
`deflake_selftest_preparation` (#1913's preparation-before-hold ordering, 4);
`deflake_selftest_support` is the single source of what they share — the
assertion helper and the ONE failure accumulator behind it, the temporary
census/claim/artifact tree, the real `Measurement` builder, the fake claim,
the recording, resource and engine-preparation adapters behind `run`, and
the one save/restore of the runner's executable seam.
The bare command runs every case once in the order it always has
(orchestration, handoff, preparation) and is what CI and `make ci` invoke;
`--only <owner>` runs one owner's cases in a fresh process for iteration.
Every invocation reports the number of cases it ran and asserts afterwards
that every module global, environment variable and patched function a case
reaches is back where it started; an owner whose inventory is absent or
empty fails the run rather than reporting a vacuous pass; and an
unrecognized argument or selector is a usage error rather than a
fall-through to the aggregate.

### `deflake_diagnosis.py` — is the probe wrong, and did the fix work? (#1437)

`deflake.py` answers "how often does this probe fail". A number is not a fix,
and this is the step after it: given one complete measurement handoff, decide
mechanically whether the evidence supports a probe-side repair and, when it
does not, which declared non-repair route the invocation takes instead.

```bash
python3 tools/deflake_diagnosis.py --handoff handoff.json      # the entry gate alone
python3 tools/deflake_diagnosis.py --diagnosis diagnosis.json  # the route its evidence supports
python3 tools/deflake_diagnosis.py --manifest .                # a checkout's config manifest
python3 tools/test_deflake_diagnosis.py                        # the deterministic self-test
```

Nothing here boots an engine, runs a probe, opens a port, edits a worktree or
talks to GitHub: it reads documents and answers questions about them. The
expensive half — twenty real runs across two worktrees — is `probe_flake.py`
performing ten twice, and its retained `probe-flake-result/v1` documents are
this tool's input.

**Why a tracked module rather than skill prose.** The `/deflake` workflow
surface is not tracked in this repository (`.gitignore` ignores `.claude/`), so
a repository test cannot cover prose. The MECHANICAL half lives here so
`tools/test_deflake_diagnosis.py` can hold it: the entry gate and one-probe
enforcement, the closed producer-provenance contract (#1661),
X-out-of-10 arithmetic, the configuration manifest including
confirmed absence, MISSING evaluation and stable check identity,
same-environment verification and the no-retry rule, the required diagnosis
evidence and preservation attestations, and the one-PR limit. Whether a
diagnosis is convincing, a repair minimal, or an expectation genuinely
obsolete stays the reviewer's judgement — the module refuses a route whose
machine-checkable evidence is missing and claims no more than that.

**The entry gate.** One invocation, one handoff, one probe. A
`deflake-handoff/v1` EMBEDS the original `probe-flake-result/v1` document
rather than paraphrasing it, because `probe_census.ingest_result`
deliberately drops the ports, per-run check maps, descriptor labels, artifact
root, invocation directory and exact command — so a handoff rebuilt from the
durable census row cannot identify the baseline invocation, and the gate
refuses it by name.

Every result document is validated by `probe_census.validate_result`, the
shipped validator for that schema, before a single field is read. Deriving a
subset of it locally is the failure this delegation exists to avoid:
`_rule_pass_run_has_no_failed_check` alone is what stops a document whose runs
all claim PASS while their check maps carry FAIL, which a run-outcome failure
count would otherwise read as a spotless batch. Its `timestamp_utc` is read by
`probe_census.parse_timestamp` too, so it names a real UTC instant rather than
merely having the right shape.

**A descriptor is compared whole.** Identifiers, their order, AND their labels:
a label is the check's stated meaning, so a batch that kept every identifier
while relabelling one to describe a different assertion has changed what it
measures and said so nowhere. A repair's `changed_paths` are normalised
repository-relative paths before the `tools/` scope check is applied, since
`tools/../src/Engine/Core/Init.hs` begins with `tools/` and changes production
code — and a repair may not touch the measurement APPARATUS at all, because
`measure`'s timeout and starting port are module constants and lengthening one
would buy a calmer verification while both command records still compared
equal. That apparatus is a CLOSED inventory (`HARNESS_MODULES`), pinned
exactly by the self-test rather than spot-checked, and stated there rather
than counted here — a hand-written total is the drift #1584 already cost
this file once: `probe_flake`, `probe_protocol`, `probe_census`,
`probe_claim`, `probe_resource_lock`, `probe_select`, `probe_engine`,
`probelib`, `run_probes` and its five `probe_runner_*` owners, `deflake`
and `deflake_diagnosis`, each of which owns probe selection, launch, port
or resource leasing, protocol reconciliation, measurement timing and
construction, result recording or census intake, or diagnosis semantics.

**The producer's spelling is the contract, down to the argv FORM.**
`deflake.build_handoff` writes `invocation.argv`, `cwd` and `timeout`, and a
bare `configuration` LIST; the controlled batches this module defines itself
use `command`, `directory`, `timeout_seconds` and a manifest object. And the
two argv forms genuinely differ: a controlled record is a COMMAND this
workflow ran, interpreter first, while the handoff's is `sys.argv` —
`deflake.main` passes `list(sys.argv)`, whose [0] is the SCRIPT, so a truthful
handoff carries no interpreter token at all. The entry gate reads what #1659
actually writes and adapts it at the boundary, so one vocabulary reaches
everything downstream while the producer stays the authority. Putting an
interpreter into a handoff argv is refused as the wrong form, which is a
stronger statement than refusing a particular interpreter. The self-test
builds its handoffs by CALLING `deflake.build_handoff` rather than assembling
an envelope by hand, and takes the argv form from an actual subprocess rather
than writing it out — a hand-written fixture agrees with whatever the
validator happens to require, which is exactly how a gate that could not
consume a single real handoff kept a green suite, twice.

**The envelope's redundant relationships are enforced.** `probe` equals
`result.probe`, `targets` equals the descriptor-ordered union of FAIL/MISSING
identifiers, `artifacts` equals `result.retained_artifacts`, and
`invocation.ports` equals the ordered run ports. Each is a value the producer
DERIVED from the embedded result, so a document where the two disagree was not
the one that measurement produced — and `probe_census.validate_result` checks
none of them.

**The targets are not a selection from the measurement — they ARE it.** Every
non-PASS identifier is a diagnosis input, so the handoff's target list must
equal the measurement's ordered non-PASS identifiers exactly. A subset would
let a repair be declared verified while another observed failure stayed
quietly out of scope. An EMPTY list is legitimate rather than malformed:
`/deflake` writes one for an all-PASS measurement, and it is the `no-target`
route to #1439 — nothing to diagnose, no batch run, no PR opened.

**Two independent qualifications for repair.** The controlled baseline exceeds
X, OR a target check is reproducibly MISSING. The second is not reachable
through the first: `probe_protocol.parse_event_stream` represents an unemitted
declared check as MISSING while `probe_flake.reconcile` classifies a zero-exit
run carrying no FAIL event as PASS, so a batch can sit at 0 failures with a
target that was never observed at all. Verification is NOT relaxed to match —
it must still come in at or below X and satisfy the MISSING rules.

**The handoff's manifest defines both batches' configuration**, not whatever
`config/` held when the diagnosis started: `Engine.Core.Init.migrateLegacyConfig`
can materialize an absent local file during a first boot, so those are
different questions. Bytes that cannot be recreated exactly are the
`cannot-reproduce` outcome for #1439 — a result to hand on with its evidence,
not a malformed input.

**Each batch holds the probe's declared cross-process interests**, for the
configuration install as well as the runs. `probe_flake`'s `peak_concurrency`
counts other flake-harness invocations only, so an independent
`tools/run_probes.py` sweep holding the same repository-relative resource
never appears in it; `probe_resource_lock` is what coordinates across
processes. A hold that was not obtained is a batch that did not run under
control, routed to #1439 rather than rejected.

**One common SHA.** The clean comparison worktree is created at the handoff's
baseline commit and the repair worktree from that same commit, so the baseline
result must name the handoff's SHA and the repair must record the `base_sha`
it was cut from. If the intended base moves, both states are recreated on one
new common SHA and the baseline is repeated.

**Descriptor equality, not identifier normalization.** `probe-result/v1`
already requires static identifiers and puts every runtime value in an event's
`detail`, so a per-run undeclared or changing identifier is a MALFORMED
protocol result — a rejected handoff — and never a diagnosis outcome to be
scored.

**The scoped MISSING rule.** Every TARGET identifier has ZERO MISSING across
all ten runs; every PASSING run emits everything; an accepted failing or
timed-out run may lose only a contiguous suffix of the declared order, which is
checked rather than assumed; and no identifier disappears from the batch as a
whole.

The suffix allowance is for the checks that are NOT targets: an accepted
failing run may abort, but not BEFORE a target, because a run that never
reached the target did not demonstrate it was fixed. Since the targets are
every non-PASS identifier and an aborting probe's form a suffix of the
descriptor, that means a verification's accepted failing runs must not abort
before the end — restrictive for an X above zero, satisfiable (a run that FAILs
its last check, or reports a failed check and keeps going, emits everything),
and exactly what the approved contract says.

**Same environment means the same measurement, not the same characters.** The
two batches necessarily differ in worktree and destination, and ports are
leased dynamically, so they are compared on behavior-affecting settings with
effective defaults filled in — including the TIMEOUT and STARTING PORT
`probe_flake.measure` applies from module constants that no command line
exposes. Each invocation records them, and each is PINNED to the harness's own
value — neither CLI can set them, so the default is the only value a real
measurement can have used, and altering all three records together would agree
perfectly and still be fiction. The comparison runs the
whole chain, handoff → baseline → verification: comparing only the last pair
would let both controlled batches agree on some arbitrary value while the
handoff sat at the defaults, and the first link crosses launchers, which is
where `/deflake`'s own constants and the harness's command line are made to
agree. A command is checked against the REAL
interface of the tool that ran it, and there are TWO tools because the three
batches do not come from one command: `/deflake` does not shell out — it calls
`probe_flake.measure` in process, and its CLI has no `--probe`, `--runs` or RTS
override — so the handoff's command is `python3 tools/deflake.py` with its
contract from `/deflake`'s own constants and its probe from the result document,
while the two controlled batches are the `probe_flake.py --probe … --runs 10`
the issue spells out. Requiring a harness argv everywhere would make a truthful
#1436 handoff impossible to submit while accepting an argv nobody ran.

Within a tool's surface: every option one it accepts, spelled the way its
argparse would read it (`--runs` and `--rts-caps` are `type=int`, so `--runs
10.0` is refused rather than compared as ten), `--result` required of
`probe_flake.py` and optional for `/deflake`, order-sensitive argv (interpreter,
then script, then options — Python rejects an unknown option before the script
runs), no duplicate option — `--runs 10 --runs 3` reads as three to
argparse and as ten to anyone scanning the record, so diagnosis evidence
gets one unambiguous spelling rather than a last-value-wins resolution — and
a PERMITTED PYTHON INTERPRETER TOKEN, which is `python3` or a version-qualified
`python3.<minor>[.<patch>]` at or above the 3.10 floor. Not bare `python`,
which is whichever of the two the machine means; nor `python2`, which cannot
parse these programs; nor one given by path, since a document cannot show which
binary sits at `/tmp/counterfeit/python3`; nor `python3.9`, which names a
version that could not have run the program whose document quotes it. A
version-qualified spelling names the SAME interpreter more precisely — a
machine with several Python 3 installations spells the one it means
`python3.12` — so it is admitted, while the floor is what keeps it honest: the
shipped tools annotate with `X | None`, and although `from __future__ import
annotations` defers the ones in signatures, nothing defers a type evaluated at
runtime, so 3.10 is where these sources stop being runnable rather than merely
parseable. The version is a dotted run of digits with no leading zero, so
`python3.010`, `python3.` and `python3.x` are refused as malformed. Also a
script RESOLVED FROM THE DIRECTORY THE COMMAND RAN IN (which is what Python
does with a relative script path) and required to be the tool the declared
checkout ships — `<worktree>/tools/probe_flake.py`
for a controlled batch, `<directory>/tools/deflake.py` for the handoff, since
matching only the file name would admit `/tmp/counterfeit/probe_flake.py` —
and the handoff's directory must be the PRIMARY checkout rather than any path
calling itself one. Each command is
then bound to its own result document, so two commands agreeing with each other
cannot stand in for the contract.

**The artifact layout is the one the harness creates.** Every path is absolute
and fully RESOLVED — `check_artifact_root` calls `Path.resolve` on its root
before a run begins, so no real path carries a `.`, a `..`, a doubled separator,
a trailing slash or an unresolved symlink. A lexical check alone would accept
`/tmp/evidence/forged/../artifacts/…`, and on a host where `/tmp` links to
`/private/tmp` it would call two different places the same one. `new_invocation_dir` puts the invocation directory
directly under that root and GENERATES its name,
`{probe}-{%Y%m%dT%H%M%SZ}-{pid}-{uuid8}`, whose stamp must be a real UTC instant
and whose pid must be a real process — the shape alone matches
`99999999T999999Z` and a pid of 0. That name is split from the RIGHT, so the
three generated fields come off the end and the probe segment is left whole
before being required to EQUAL the document's own probe; every registered probe
key is hyphen-free today, so a left-to-right split happens to agree, but it
would misattribute part of a hyphenated key the day one were registered.
And every run directory is `invocation_dir /
f"run-{index:03d}"` — three recorded values determine the whole layout, so a
failed run's directory cannot be swapped for an unrelated path, and the run
directories are unique by construction. The containment sweep over the paths a
result NAMES stays for the case `--artifact-root` is omitted, which is
legitimate: `default_artifact_root` supplies a temporary directory, and nothing
else then constrains the root the document reports.

Two labels are not two worktrees, either: the declared paths are canonicalised,
neither may contain the other, and each invocation must have run inside the
worktree its section names. Deliberately not "must be a registered `git
worktree`" — the workflow removes or hands off both comparison worktrees when
it finishes, so by evaluation time the paths it correctly names may be gone.
Both declarations are collected before the HANDOFF is admitted and before either
batch is validated, so every path — the handoff's own result tree and extra
retained artifacts included — is checked against both comparison states even
once neither is registered.

Both worktrees are attested source-clean at measurement time — the recorded SHA
cannot reveal an uncommitted change — and a configuration manifest's entries
must be members of the `config/*.local.yaml` family, since two manifests that
agree perfectly about `../outside.local.yaml` establish nothing about the state
the probes actually read.

The two controlled batches may SHARE an artifact root — `--artifact-root` is
optional and `default_artifact_root` supplies a temporary directory to both —
but never the invocation directory beneath it, which `new_invocation_dir`
creates fresh per invocation: a verification naming the baseline's is claiming
the baseline's artifacts as its own. Nor may either batch write INTO the
other's invocation directory — a `--result` pointing at the baseline's retained
`run-001/events.jsonl` is a distinct path that overwrites the very evidence the
comparison is made of — while a shared root stays legitimate, since a root
contains both invocation directories rather than sitting inside either.

Run indices are the sequence `measure` emits, `1..len(runs)`, with the
harness-error record next: ten records all numbered `1` is one run replayed ten
times, and every other rule reads a run's index.

Destinations must sit outside every worktree, registered or declared:
`check_artifact_root` guards the artifact root, but `--result` is written
wherever it is pointed — and it is opened RELATIVE TO THE PROCESS'S DIRECTORY,
so a destination is joined onto the recorded invocation directory and
normalised first. Where a batch SAYS it wrote is bound by the same rule, and
its declared `--artifact-root` must agree with the root its result document
reports.

**Artifacts are paired with outcomes, and handed on.** `probe_flake` deletes a
run's directory the moment it passes and retains every unsuccessful one, so
`artifact_dir` pairs with the outcome exactly and `retained_artifacts` is the
ORDERED list of the non-null ones — the completed runs in index order, then the
error run — compared as a list rather than as a set, since a shuffled list
names the same directories in an order no producer ever wrote. Checked in both
directions, because "no successful-run raw artifacts remain" is one of
verification's success conditions and a failing run whose logs are gone is a
failure nobody can diagnose; a present `error_run` is the one record whose logs
say why the stream broke, so its own directory is required rather than merely
permitted. An emitted outcome then names the retained artifacts of EVERY batch
the invocation ran, not just the handoff's: the batch that went wrong is
usually the verification.

**An invalid batch is a route, not a rejection.** "Verification remains above
X, contains any MISSING result, becomes invalid, or only partially improves the
rate" is one list in the issue and every entry goes to #1439 — so a harness
error, a short batch or a contended machine reaches `partial-improvement` in
the verification and `cannot-reproduce` in the baseline, with the evidence
retained. A gate rejection would describe an invocation that got nowhere and
lose the artifacts it did keep. What stays a rejection is a document that is
not a `probe-flake-result/v1` at all, and a descriptor whose identities
changed.

**Identities are the census's own grammars, not second copies.** A
`commit_sha` goes through `probe_census.require_commit_identity` and a
`timestamp_utc` through `probe_census.parse_timestamp`, so both are matched
full-string — a trailing newline or a suffix is refused — and the literal
`unknown` that `probe_flake` writes when `git rev-parse` could not be consulted
is refused BY NAME. A `CensusError` from either is caught and reported as this
module's own malformed-input rejection, never allowed to escape as a traceback.

**The repair is frozen before it is verified.** `probe_flake` records only
`git rev-parse HEAD` and cannot see uncommitted source, so a declared repair
needs a source-clean repair worktree and a verification whose `commit_sha` is
the repair commit being proposed.

**Routes, and the reason each was taken.** `repair-pr` is the only one that
opens a pull request, and the `Diagnosis` session enforces at most one per
invocation. `handoff-rejected` stops at the gate; `no-target`,
`cannot-reproduce`, `no-confident-fix` and `partial-improvement` hand off to
#1439; `production-defect` hands off to #1438 and does not touch the probe.

The route alone is ambiguous in both directions that matter, so the emitted
record also carries a machine-readable `reason` from a closed vocabulary
(`REASONS`, with `ROUTE_REASONS` saying which route can be reached for which).
`cannot-reproduce` is reached by a controlled batch that ran under the
handoff's own recorded condition and observed nothing, by one whose
configuration could not be recreated from the manifest, and by one that never
became a controlled measurement — three different findings, and only the first
says anything about the PROBE. `partial-improvement` is reached by four gate
failures, of which `verification-not-comparable` and
`verification-not-a-controlled-measurement` are facts about the INVOCATION
rather than about either result document, so a consumer handed only the
documents could never derive them.

Emitting a handoff means emitting `deflake-diagnosis-outcome/v1`, and #1437
owns that PRODUCER record: the route, its owning issue, the identity of the
`/deflake` invocation consumed (its command, directory, artifact root,
invocation directory and timestamp — none of which the census row retains),
the probe and targets, the baseline SHA and X, the configuration manifest,
references to the controlled results, the diagnosis evidence, the preservation
attestations, and the repair commit and verification evidence when the route
has them. A route with no batches states those halves as `null` rather than
dropping the keys, so a consumer reads one shape. `deflake_outcome.py` and
`deflake_issue.py` below are its two consumers, and they read the same
envelope through the same entry gate — `deflake_handoff.py`, which owns that
gate for both of them: `deflake_handoff.RouteOwnership` is the only part that
differs, so every rule the two share is checked once rather than forked.

The gate is `tools/test_deflake_diagnosis.py`, engine-free and document-only.
It is deliberately NOT wired into `make ci` or GitHub CI — #1437's approved
rereview amendment scopes this lab's own self-test to manual invocation — so
run it by hand when touching `tools/deflake_diagnosis.py`. It takes seconds and
boots nothing. (`tools/test_deflake.py`, the #1436 orchestrator's self-test,
IS in both; the two issues scoped their gates differently.)

Every provenance invariant above is asserted TWICE (#1661). A rejection test
supplies a fixture violating it and pins the message; a mutation case then
NEUTRALISES exactly that one rule in a private compiled copy of the module and
proves the same fixture is no longer refused for that reason. The second half
is what makes the first half evidence: a rejection test passes just as happily
when some other rule is what did the rejecting, and then the invariant it
claims to cover could be deleted without a single test turning red — which is
not hypothetical, since the retained-artifact ORDER rule is caught by its
mutation case alone. The bypass is a textual edit to the module source, never a
hook in the shipped module, and an anchor that no longer matches exactly once
is a loud failure rather than a quiet pass. A rule that cannot be isolated this
way is redundant or untestable and does not belong in the module. Where one
invariant genuinely nests inside a broader one — two batches sharing an
invocation directory are also, necessarily, each writing inside the other's —
the bypassed module refuses for the other reason and the case records that
outcome distinctly.

What stays out of everything, exactly as for `deflake.py`, is the REAL
engine-booting measurement: a diagnosis consumes twenty ten-run batches' worth
of wall clock and is supplemental manual pull-request evidence, never a merge
gate.

### `deflake_handoff.py` — the handoff contract both consumers read (#2097)

The `deflake-outcome-handoff/v1` envelope, and every rule the two consumers
below share. No CLI: it records nothing, publishes nothing, and imports
neither consumer, so the two workflows cannot come to disagree about what the
envelope means and neither is the other's prerequisite.

It owns the schema and the measurement-role vocabulary (`handoff`,
`baseline`, `verification`), `RouteOwnership` and what each of #1437's endings
IS, the `HandoffError` / `NonSuccess` classifications, the `Measurement` and
`Handoff` representations, every producer-record, identity, manifest,
invocation, path, worktree, descriptor, artifact and producer-binding rule,
`require_reproduced` — #1437's two-part reproduction qualification, which
every route past the `cannot-reproduce` fork rests on — and the `utc_now` /
`reuse_stored_timestamp` pair a durable record is stamped and replayed with.

`require_diagnosis_outcome` and `require_handoff` take `owned` EXPLICITLY: a
shared contract that defaulted to one consumer's routes would answer for that
consumer whenever a caller forgot to say which routes it owns. Each consumer
keeps its own `OWNED` instance and supplies it. `deflake_outcome.py` also
re-exports the contract's names — the same class objects, so an `except`
written against either module's `HandoffError` matches a refusal raised
through the other — for callers that have not moved to the contract yet.

Its gate is `python3 tools/test_deflake_diagnosis.py`, the same deterministic
run both consumers are covered by.

### `deflake_outcome.py` — the non-success outcomes of a de-flake attempt (#1439)

Consumes one `deflake-outcome-handoff/v1` for one exact registered probe and
one de-flake attempt, decides whether the evidence supports a STABLE
non-success outcome, and appends that outcome durably to the probe's census
row. It reuses `probe_census.update`'s locked read-modify-write and adds no
second state store, no second write path and no second lock. Reading the
envelope is `deflake_handoff.py`'s above; what is here is #1439's own — which
route becomes which stable outcome and on what evidence, the records and
recommendations they produce, the census append, the no-pull-request boundary,
and this CLI.

```bash
python3 tools/deflake_outcome.py --handoff <document.json>
python3 tools/deflake_outcome.py --handoff <document.json> --census <path> --json
```

The handoff embeds #1437's `deflake-diagnosis-outcome/v1` record, the attempt
identity, the diagnostic or attempted-fix summary, and one entry per
measurement — its ROLE (`handoff`, `baseline`, `verification`), its
`tools/probe_flake.py` EXIT CODE, and the `probe-flake-result/v1` document
that exit wrote, if it wrote one. Exit 0 and 3 exits are not interchangeable
and neither are their documents, which is why the exit travels with the
result rather than being inferred from it.

**The route is not the whole finding.** #1437's machine-readable `reason` is
carried, validated against its own route, and cross-checked against the
documents wherever it can be: a record naming a measurement-visible condition
its own evidence denies has contradicted itself and is not a stable outcome,
while one naming a condition the documents cannot see is taken on the
producer's word, which is where that finding was actually made.

**Three stable outcomes, and everything else.** `cannot-reproduce` — the
designated pre-fix measurement is complete, trustworthy and observed nothing
wrong at all; it appends the outcome plus an ADVISORY de-list recommendation.
The other two `cannot-reproduce` reasons record their evidence and NO
recommendation: a batch that passed under a configuration the invocation could
not recreate passed somewhere else, and de-listing a probe on that would
promote a measurement the producer itself says was not the condition.
`no-confident-fix` — the failure reproduced, and the evidence establishes no
one probe-side cause. "Reproduced" is #1437's own two-part qualification and is
asked of every route past the `cannot-reproduce` fork: over X or a lost target,
AND at least one TARGET actually non-PASS, since failures confined to unrelated
checks satisfy the aggregate while demonstrating nothing about the checks under
diagnosis. `partial-improvement` — a repair candidate measurably
improved the failure count and still failed #1437's acceptance gate. None of
the three opens a pull request: the publisher boundary is a table every route
is looked up in, so the gate can inject a spy and prove the silence rather
than resting on the call not having been written.

Everything else is an ACTIONABLE NON-SUCCESS that records nothing, publishes
nothing and exits non-zero — a production-code or shipped-script defect (which
names #1438, a sibling in epic #1426 that this workflow neither requires nor
stubs), every operational error, and a census write that refused. A malformed
handoff is a REJECTION (exit 2) instead: it never reached a classification, so
it is not one of the three and must not be recorded as one.

**Operational errors are not "cannot reproduce".** `probe_flake.py`'s exit
contract is preserved rather than paraphrased — exit 0 is a valid measurement
whatever rate it observed, 2 is a rejection before execution, 3 is port
exhaustion, and 4 is an untrustworthy harness result. Exits 2 and 3 write NO
document; exit 4 DOES write one, whose own `status` is `harness-error` with a
populated `error` and `error_run` and a null rate. So a document that merely
exists and parses establishes nothing, and every classification reads the
document's own `status`, `error_run`, `completed_runs` and per-check tallies.
Every declared measurement goes through #1437's CANONICAL result gate first,
not just the census validator it starts with:
`deflake_diagnosis.require_result` adds the run indices, the artifact topology
and the retention pairing on top of declared shape and the cross-field
invariants. `probe_flake.measure` deletes a run's directory the moment it
passes and keeps every unsuccessful one, so a non-PASS run with a null
`artifact_dir` is producer-impossible — and a `no-confident-fix` recorded from
one would be a failure nobody can diagnose stored as the evidence FOR a
diagnosis.

`error_run` gets its own clause because `probe_flake` keeps a harness-error
run OUT of `runs`, so an all-PASS run list is not on its own evidence of a
trustworthy batch.

A document that contradicts ITSELF is refused before any route is classified,
not only before `cannot-reproduce`. `probe_census.validate_result` binds
`check_counts` to `runs` and refuses a PASS run carrying a FAIL check, but
nothing binds `failure_count`, `timeout_count` or `failure_rate` to the run
list — so an all-PASS batch under a forged failure count is schema-valid and
would read as a REPRODUCED failure, which is what `no-confident-fix` and
`partial-improvement` rest on. The three totals are reconciled against the run
list using `probe_flake.Measurement`'s own arithmetic — and so is
`completed_runs`, which the producer writes as `len(runs)` and which makes the
rest mean anything, since a nine-run batch claiming ten completed satisfies
`completed_runs == requested_runs` and would be stored as ten of ten. The
remaining defect
predicate then reads the RUN LIST and the PER-CHECK TALLIES, which are
genuinely independent of each other (a run can time out after emitting every
check, and a check can go MISSING across an all-PASS batch) rather than reading
a total the reconciliation has already bound.

**The record has to agree with itself first.** #1437's record states the
input identity twice — its `handoff` section carries the probe, commit, X and
targets of the `/deflake` invocation consumed, and the top-level fields derive
from that same handoff, `baseline_sha` being `handoff.commit_sha`. Each
duplicated field is re-parsed with its twin's grammar and required to match, so
a record whose handoff identifies one commit while its top-level field, its
baseline reference and the supplied measurement all name another is refused
rather than stored as an attempt about a baseline the producer never diagnosed.

**One attempt reports against one declared contract.** A batch's descriptor is
what it reports against and the identity binding cannot see it — a result can
keep its probe, targets, commit, instant and every artifact path while swapping
or relabelling an unrelated declared check, which #1437 rejects outright. #1437's
record carries the whole ordered descriptor beside the identifier list, and
every supplied measurement is held to it through that module's own
`require_descriptor` — against the RECORD rather than a sibling measurement,
since the routes carrying one baseline have no sibling and are the ones that
record a de-list recommendation. The
targets are held to that same descriptor: one it never declared cannot be among
the measurement's own non-PASS identifiers.

**The measurement is the one that diagnosis judged.** Binding on the probe
alone would admit any well-formed batch of that probe — one taken at another
commit or another instant, supplied under a diagnosis that judged a different
one, leaving the census holding two conflicting accounts of a single attempt.
Each declared measurement is held to EVERY field of the producer record's
reference for its role that a result document also reports — the commit, the
instant, the artifact root, the invocation directory and the ordered retained
artifacts. The commit and the instant alone are not an identity: two batches of
one probe at one commit differ in where they wrote, which is exactly why
`probe_flake.new_invocation_dir` stamps a fresh directory per invocation, and a
substitute agreeing on the first two would still hand the census another
batch's artifacts as this attempt's evidence. The pre-fix roles are held again
to the `baseline_sha` the census row is about to record: an independent
statement, since a producer record whose reference and `baseline_sha` disagreed
would satisfy either alone. A role the producer ran no batch for carries a
`null` reference, and a measurement supplied for it describes work the
invocation did not do.

**The run count is the measurement's own.** Completeness is `completed_runs ==
requested_runs` and the ceiling is X out of that measurement's own requested
count. Ten is the standard configured N, and hard-coding it here would
misclassify a measurement taken at any other one.

**A lower failure rate is not success.** `partial-improvement` is numeric on
both halves: both batches complete and trustworthy, taken at the SAME run
count and the SAME RTS capability count, the verification's failure count
strictly lower, and the verification STILL failing #1437's acceptance gate for
the reason #1437 named. `verification-over-tolerance` is re-derived from the
failure count and `verification-missing-rule` by CALLING
`deflake_diagnosis.missing_problems`, whose scoped rule has four clauses of
which only one is about targets — a PASSING run omitting a NON-target check
fails it too, and a paraphrase of "no target went MISSING" would call such a
verification passing while `evaluate` had just routed it here. The other two
reasons are the producer's to make and are recorded on its word. A record whose
own evidence denies the measurement-visible condition it names is refused, and
`unmet_condition` is DERIVED from the reason rather than accepted as free text.

**The de-list recommendation is advisory, and only that.** Nothing here edits
`tools/ci_probes.py`, removes a manual-only reason, changes a classification
or promotes a probe. `MANUAL_ONLY_REASONS` models a probe's grounds as several
INDEPENDENT `Reason` records, so "it turned out not to be flaky" is one of
them and the rest still stand; acting on the recommendation is a person's
decision taken with all of them in view.

**Paths are bounded, not merely absolute.** Every artifact reference this module
would store is refused if it lies inside a worktree — the live registered ones
AND the comparison worktrees the producer record declares, because `/deflake`
removes those when it finishes and an artifact that sat inside one was still
inside a worktree when it was written. Each declared worktree is required and
held to #1437's canonical spelling: a boundary compared in one spelling while
the paths it bounds are written in another covers nothing, and a section that
declared none would contribute no boundary while looking like a record that
had one. Neither may contain the other, since two labels are not two states.

What that cannot establish is that a named directory ever WAS a worktree —
nothing else in the record identifies those directories, and both are usually
gone by the time an outcome is recorded. The declared worktrees are an
ADDITIONAL boundary the record supplies about itself: they can only add
refusals, never lift one, and the guarantee rests on the live registered
worktrees and the primary checkout, which no document can edit. Containment is compared over resolved
path forms, so a `..` segment or a symlinked spelling of the same place is the
same place. `probe_flake.check_artifact_root` enforces this at measurement
time; restating it here is what stops a self-consistent handoff putting a
worktree-resident tree into a durable record.

Absolute is also not the same as usable, so every recorded path goes through
`deflake_diagnosis.require_path` first — including the ones stored without ever
being resolved, a `/deflake` command token and a configuration manifest entry
among them, because those are evidence too. An embedded NUL makes
`Path.resolve()` raise `ValueError` from `lstat` rather than `OSError`, so such
a string passes an absoluteness test, names no location for the containment
check to find, and would be stored while the CLI printed a traceback.

**The artifact list is rebuilt, not believed.** #1437 produces its top-level
`retained_artifacts` by accumulating every batch it ran, in role order,
deduplicated, so the field is derived and the entry gate reconstructs it from
the same per-batch references. Checking each path individually only asks
whether each names a legal place; an unrelated directory appended to the list
alone would still be stored as this attempt's evidence.

**Durable, idempotent, and destroying nothing.** The stored record is a RESUME
point — attempt identity, probe, timestamp, baseline commit, X, targets, the
configuration manifest both batches read, the exact command and directory of
the `/deflake` invocation consumed (neither of which any census field has ever
held: `ingest_result` drops the command and the invocation directory, and
nothing stores the manifest), one summary per measurement (commit, timestamp, run counts, failure and timeout
counts, rate, RTS capabilities, per-run outcomes, per-check PASS/FAIL/MISSING
tallies), the retained artifact REFERENCES, the summary, and the
route-specific evidence. Recording is idempotent on the attempt identity: a
resume installs the identical bytes and appends nothing, while the same
identity carrying different evidence is refused rather than appended past.
Idempotency is the WHOLE record, and exactly one field is not derived from the
handoff — `timestamp_utc` comes from a clock, which reads differently on a
retry — so a retry reuses the instant the stored attempt was first stamped
with instead of restamping itself into a conflict. That lookup happens INSIDE
the census transaction, under the same lock the append is made under (through
`probe_census.record_outcome_installed`'s `reconcile` hook), so two concurrent
invocations of one new attempt serialize and the loser rebuilds against what
the winner actually committed; running the command twice over one handoff
succeeds twice and appends once, concurrently or not. A
write that refuses leaves the census byte-identical, records no outcome and
returns an actionable non-success without reaching a publisher. The one
failure that is NOT a refusal is `CensusDurabilityUnconfirmed`, raised after
the replacement: the record is already what a later reader parses, so it is
reported and never retried. Only path references are stored — raw stdout,
protocol streams and engine logs stay in the harness's artifact tree outside
every worktree.

The gate is `python3 tools/test_deflake_diagnosis.py`, the same engine-free,
document-only self-test #1437 owns, extended with this module's cases: the
diagnosis records they feed it are PRODUCED by `dd.evaluate` rather than
hand-assembled, and the census they append to is a real seeded one in a
temporary directory. Like #1437's, it is deliberately not wired into `make ci`
or GitHub CI.

### `deflake_issue.py` — file an issue when the bug is in the engine (#1438)

The second outcome of diagnosis, and the one that must not be skipped. When
#1437 routes an attempt to `production-defect` — the diagnosis is that
PRODUCTION code or SHIPPED scripts are wrong, a real race rather than a racy
test — this module files one review-ready tracker issue, records it in the
probe's census row, and stops.

```bash
python3 tools/deflake_issue.py --handoff <document.json> --origin claude
python3 tools/deflake_issue.py --handoff <document.json> --dry-run
python3 tools/deflake_issue.py --handoff <document.json> --origin codex \
    --census <path> --repo owner/name --json
```

An engine race that reaches a pull request as a probe adjustment is a bug
converted into a permanent green light, so this route is TERMINAL: the probe
is not touched, no production code is edited, and no pull request is opened.
Both boundaries are injected parameters consulted through `CHANGES_THE_PROBE`
and `OPENS_PULL_REQUEST`, so the silence is a branch a gate exercises rather
than a call nobody happened to write — flipping either entry makes the
injected spy fire.

"Engine" means production Haskell under `src/`/`app/` and shipped Lua under
`scripts/`; probe implementation under `tools/*_probe.py` is explicitly not
that, and is #1437's repair route. Nothing here inspects a diff to decide: the
CALLER's explicit diagnosis is the branch input, because the issue assigns
that judgement to the calling agent and a second heuristic classifier would be
a second opinion nobody asked for.

**Evidence, not pathnames.** `probe_census.summarize_sample` stores retained
artifacts as PATHS, and a path is machine-local — a reader of the filed issue
cannot open it. So this module READS the retained FAIL and TIMEOUT
directories (`events.jsonl`, `stdout.txt`, `engine/*`) and quotes bounded
excerpts into the body. The tree is walked component by component from the
declared artifact root with `O_NOFOLLOW` below it and the engine directory
listed by descriptor, because what is found there is published: a symlinked
`engine`, or a run directory substituted after #1437's canonical-path check
passed, would otherwise publish whatever lives elsewhere as this probe's
failure evidence. Files are opened `O_NONBLOCK` and required to be regular, so
a FIFO cannot block the workflow. The bounds: at most `MAX_EVIDENCE_RUNS` runs, `MAX_EVIDENCE_FILES_PER_RUN`
files each, and the trailing `MAX_EXCERPT_LINES`/`MAX_EXCERPT_CHARS` of each,
which is where an aborting probe's failure lands. An attempt whose artifacts
have all been pruned is REFUSED rather than filed on paths alone — but only an
attempt with no issue at ALL ever collects evidence. A recorded outcome and a
reconciled publication key are both checked first, so either recovery works
long after the artifact tree has been swept; earlier still is the route's own
evidence check, which needs no artifact, so an unsupported handoff is refused
without even a search.

**Quoted content cannot forge the routing marker.** An engine log is
arbitrary text, and `approve_issues.issue_origin` scans the whole raw body —
fenced blocks included — and RAISES on two markers naming different brands, so
a quoted log carrying one would stop the filed issue entering the review gate
at all. The assembled body therefore passes through one funnel that breaks
every HTML-comment opener (rendering `<!--` as `<! --`) before the two real
markers are appended, and `require_one_marker_each` then checks the finished
text — exactly one origin, one publication key, two comments — rather than
trusting that it did.

**Nothing required is silently cut.** Only the second and later runs' evidence
may be dropped to fit a tracker body; when even that is not enough the
publication is refused, because a defect report published with its
measurements or its log evidence truncated away is what this workflow exists
to prevent. #1437 bounds neither the diagnosis summary nor its evidence list,
so `require_defect_diagnosis` bounds both here — refused rather than trimmed,
since the summary is the issue's own claim. Beside them
the body carries what the amendment names: the failure numerator, denominator
and rate, the timeout count, every declared check's PASS/FAIL/MISSING tally,
the measured commit, the requested and completed run counts, the RTS
capability setting, the targets and X, the configuration manifest, and the
`/deflake` command and directory.

**It enters the review gate.** The body ends with the `issue-origin` marker
`approve_issues.py` reads to route a new issue to the opposite agent brand.
That brand is the INVOKING agent's, which no document can derive, so
`--origin` is a required input rather than a default; no label is applied,
because labelling is the review lane's.

**Publication is idempotent, including across a crash.** A recorded outcome is
the completion marker: a resume reuses the stored issue and does not reach the
tracker at all. The window that marker cannot close is the one where creation
TOOK EFFECT and its identity was never recorded — a timeout, a crash, or a
census write that refused in between. So every diagnosis carries a stable
`publication_key`, DERIVED from the attempt identity, probe, route and
baseline commit rather than supplied, and written into the body as a marker
line. It is reconciled against the tracker BEFORE anything is created, and the
marker is verified in the returned body rather than trusted from a search
index — a search matches text anywhere, and an issue that merely quotes a key
is not the one filed under it. Since a filed issue QUOTES engine logs, only a
standalone marker line outside every code fence counts. A reconciled issue
also supplies its own `issue-origin` brand, read off that same body: the issue
was filed by whoever filed it, so a Claude-origin creation resumed by a Codex
invocation still routes to Claude's opposite brand, and one carrying the key
with no readable origin marker is a publication failure rather than something
to record under the caller's guess. Two invocations of one brand-new attempt racing
at the same instant can still both miss the reconcile; the census refuses the
loser, so the durable history holds one outcome, and serializing the attempt
itself is `probe_claim.py`'s (#1434) per-probe claim. The census's `flock` is
deliberately NOT held across the remote call: one hung request would stall
every unrelated census writer.

**Failure leaves the attempt resumable.** A publication that fails, or a census
write that refuses, records nothing, changes no bytes, and never falls through
to the probe-adjustment or fix-PR path. The issue may exist remotely; the next
invocation reconciles the key, finds it, and records it once.

The census record is deliberately the same shape `deflake_outcome.py` writes
— that record is #1439's own, not the shared contract's — plus an `issue`
block (number, URL, publication key, origin) — one `outcomes`
collection holds every ending of a de-flake attempt, and the schema pairs the
two halves: `production-defect` REQUIRES `issue` and the three stable outcomes
forbid it.

The gate is `python3 tools/test_deflake_diagnosis.py` again — engine-free,
GPU-free and network-free, with the tracker faked at the publication boundary
so "exactly one issue" is a counted fact. Like #1437's and #1439's cases, it
is deliberately not wired into `make ci` or GitHub CI.

### `ci_expensive_gates.py` — CI worldgen/graphical/unit-assets/save-compat selection

Selects the four expensive gates that are conditional on pull requests:
quick worldgen-output regression checking, graphical test-suite compilation,
the unit-asset inventory pair (`test_pack_atlas.py` +
`pack_atlas.py --validate-only --strict`, #1257), and the save-compat
fixture-reproducibility test (`test_save_compat_audit.py
--only-reproducibility`, #1360). All four run unconditionally after a merge
to master. The mapping is intentionally explicit; add a relevant glob when
introducing a new worldgen output, graphics entry point, unit-asset input, or
save-format/fixture/save-tooling path.

Patterns are matched with `fnmatch`, where `*` crosses `/` and `**` carries no
special meaning — write `dir/*` for a whole subtree. `--gate` names are
cross-checked against the pattern table by `--self-test`, so an unknown gate
raises instead of silently inheriting another gate's globs.

```bash
python3 tools/ci_expensive_gates.py --changed src/World/Geology/Timeline.hs --gate worldgen
python3 tools/ci_expensive_gates.py --changed data/units/acolyte.yaml --gate unit-assets
python3 tools/ci_expensive_gates.py --changed src/World/Save/Envelope/Codec.hs --gate save-compat
python3 tools/ci_expensive_gates.py --self-test
```

**`save-compat` is the one gate `make ci` also selects with** (#1360), so it
is the one that needs a LOCAL notion of "what changed":

```bash
python3 tools/ci_expensive_gates.py --local-changed-paths \
  | python3 tools/ci_expensive_gates.py --stdin --gate save-compat
```

`--local-changed-paths` prints every TRACKED path differing from the merge
base with the checked-out default branch — commits on the branch, staged and
unstaged edits alike — which is the local counterpart of CI's
`git diff --name-only <pr-base> HEAD`. If no default branch or merge base
resolves, it prints a conservative sentinel that selects EVERY gate: a local
gate that cannot tell what changed runs the coverage rather than skipping it.

`tools/ci-local.sh` calls it at the **top** of the script, before writing its
own temporary `cabal.project.local`, and that order is load-bearing rather than
incidental: that file is not gitignored, so a change can track one and cabal
applies it in CI (hence `cabal.project*` in the gate's table) — resolving after
the write would report this gate's own scratch edit as the candidate's.
`ci_parity_audit.py` checks the ordering. The decision itself still goes
through the same
`--stdin --gate` command CI runs, so there is one matcher and one answer;
`tools/ci_parity_audit.py` fails if the two files stop agreeing about which
gate decides the reproducibility member or which command it guards, and it
proves the local half by EXECUTING `ci-local.sh`'s marked selection block
against a positive and a negative sample rather than reading it.

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
  `unit.transferItemToUnit`, then walks the pair 3 × 8 tiles out and
  straight back under the real
  player move order (`unitAi.commandMove`). It reports, at every waypoint:
  inventory, carrying weight vs capacity, hunger/calories/hydration/
  exhaustion/stamina, blood and bleed rate, pain, wounds and their dressing
  state, the AI's current action/role/treatment claim, and position —
  plus an observations list for anything that ended a trip early.

  Provisioning is **prospectively capacity-gated** (#1212), so no
  successful transfer can leave a traveller above its carrying capacity.
  The engine verb itself deliberately has no capacity check (its laxity is
  a contract the fetch/repair/medic AI callers depend on,
  `src/Unit/Transfer.hs`) and is unchanged; the runner does the projection
  instead. It picks the concrete source instance the verb would move —
  the first `defName` match in `unit.getInventory` — and requires
  `getCarryingWeight(receiver) + that instance's weight ≤
  getStat(receiver, 'carrying_capacity')` before passing that same
  `instanceId` to the verb, re-measuring after every accepted item. Those
  are the modifier-applied capacity and full recursive instance weight the
  strict `unit.checkTransfer`/`unit.commitTransfer` policy uses; the
  strict path itself is not usable here because it additionally requires
  Chebyshev reach ≤ 1, which the first-aid scenario's ridge-top scout does
  not have. An item that would not fit is **refused and reported** — for
  partial provisioning as well as for zero transfers, naming the item, the
  receiver's load, its capacity and the overshoot — and the report prints
  each traveller's post-provisioning load against its capacity. A receiver
  already over capacity refuses further items and says so; the runner
  never unloads inventory it did not put there. A capacity refusal is a
  gameplay observation and still exits 0, in both scenarios.
- **`first-aid`** (~4 min) builds a wide arena ridge, issues the mule's
  pre-stocked `first_aid_kit` to the selected expedition acolyte via that
  same capacity-gated transfer path — a kit refused for want of room is
  recorded as an observation and the run continues rather than aborting,
  with the pre-fall baseline dropping only its kit precondition (and
  saying so) and `unit.treatBleeding` falling back to the makeshift
  tourniquet it improvises when the kit owner has no supplies — walks
  that acolyte off the ridge for a real fall, and then **follows the real
  medic AI's treatment to a named terminal condition** (#1221). It
  reports the injury, that treatment trajectory, the kit's remaining
  contents and holder, and the final unit state.

  **The runner administers no treatment at all.** It used to fire
  `unit.treatBleeding` itself the moment the injury landed, which made
  the AI's own throughput unmeasurable — every dressing the report showed
  was the runner's. Post-#998 a 2-z fall leaves well over a minute before
  a naive exsanguination (`test-headless/Test/Headless/Unit/Fall.hs`), so
  the real `treat_ally` path — real claim, real kit fetch, real
  `unit.treatBleeding` calls, live blood tick and real clotting — is
  simply followed instead. Two scenario-local Lua pieces make that
  observable without touching any engine surface or AI script: a
  **transparent wrapper** around `unit.treatBleeding` that forwards each
  call with the caller's exact argument list, logs the arguments and the
  returned table, and returns the original results unchanged; and a
  one-round-trip sampler that reads the patient and drains that log.

  Every sampling interval (2 s, within a 120 s budget) prints a
  trajectory row: elapsed time, pose and knockdown state, current/max
  blood and aggregate bleed rate, total/dressed/undressed/external/
  still-bleeding wound counts, remaining bandages and where they are,
  who currently holds a treat claim, and each treatment result observed
  since the previous row (`ok`/`method`/`part`/`kind`/`bandagesUsed`/
  `attempts`/residual `seep`/`message`, with the treating unit's id).

  Which acolyte treats is **discovered, never staged**: every acolyte
  rolls its own `bleed_control` knowledge at spawn and `bestMedicFor`
  ranks the whole squad, so the acolyte placed beside the landing is only
  a bystander and the report names the units that actually claimed and
  treated. "Still bleeding" is `scripts/unit_ai_medic.lua`'s own
  `needsTreatment` policy rather than a cutoff invented by the runner —
  external kinds only (`concussion`/`fracture`/`internal` excluded), seep
  above `treat_min_seep` (read live off `unit_ai_tunables.lua`) and clot
  below `CLOT_ENOUGH` — so `controlled` means the medic itself would
  stop. The terminal condition is one of **controlled**, **supplies
  exhausted**, **collapsed**, **died**, **gone** or **timeout**, checked
  in that precedence with patient-state outcomes first; a `collapsed`
  terminal requires pose `collapsed` **with `knockedDown` false**, since
  every real fall lands in that same pose with the flag set and the
  ordinary knockdown is not an outcome. The terminal row is printed
  whatever happened, including when no treatment call was ever made, and
  the condition is reported as an OBSERVATION — it never affects the exit
  status.

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

The end-to-end run stays human-run, but the harness's own missed-click
handling (#2052) has an offline gate needing no engine, window or
network:

```bash
python3 tools/input_check.py --selftest   # or --self-test
```

A missed click leaves the fixture's nil `shiftAtClick` out of the
serialized state; the self-test replays exactly that state and asserts
the dependent checks report a diagnostic naming the absent field
instead of raising, so the primary click failure stays first, the later
sections and the summary still run, and the status stays non-zero.

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
before sampling.

From a `borderless` start it is the #1731 gate and from a `fullscreen`
start the #1882 one; they are the same gate, and each asserts one thing
beyond the round trip. `defaultWindowConfig` asks GLFW for borderless as
well as fullscreen, and either mode is applied to the decorated window
`createWindow` just made; applying it there consumes the first-switch
caching opportunity, so `createWindow` seeds the windowed cache from
that decorated window at the CONFIGURED size immediately before mutating
it. The `windowed` leg must therefore reach that saved resolution rather
than `defaultWindowState`'s 800x600 fallback — which is what
distinguishes a correct startup seed from an incorrect one, since
`getVideoConfig()` reports the QUEUED target mode independently of what
the render thread applied and so round-trips either way. Run both starts
from a non-default saved resolution.

That leg is asserted on SIZE only: configuration persists no position,
and the size comparison against the config is a proxy — a window manager
need not honour the requested size exactly. The authoritative seed
contract, position included, is pinned headlessly by
`Graphics.WindowMode`'s `bootPos`/`bootSize` fixture. A `fullscreen`
start additionally leaves its OUTER round trip reported but not
asserted, because its return leg re-enters fullscreen rather than
restoring a cached windowed window.

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
├── test_audit.py           (world_audit/world_check/world_baseline self-test — the façade)
├── test_audit_support.py           (its shared fixtures and assertion facility)
├── test_audit_categories.py        (its emitted-category inventory owner)
├── test_audit_world_audit.py       (its world_audit check-behavior owner)
├── test_audit_world_check.py       (its world_check summary/determinism owner)
├── test_audit_content_hash.py      (its baseline content-hash gate owner)
├── test_audit_strict_capture.py    (its strict baseline-capture owner)
├── test_audit_missing_baseline.py  (its missing-baseline exit-policy owner)
├── ci_expensive_gates.py   (path selector for the worldgen/graphical/unit-assets/save-compat gates)
├── lua_module_budget.py    (Lua module split line-budget guard)
├── action_outcome_coverage.py (F4 action-outcome verb instrumentation self-audit; --verify-tier1 is the CI gate)
├── language_report.py      (generated-language native-name report/check, #710/#1094/#1095/#1096)
├── run_probes.py           (opt-in aggregate behavior-probe runner — the command)
├── probe_runner_registry.py    (its probe registry, selection, port spans, per-key timeouts)
├── probe_runner_diagnostics.py (its durable progress/failure record protocols)
├── probe_runner_resources.py   (its resource conflict model, cross-process holds, engine preflight)
├── probe_runner_lifecycle.py   (its one-probe launch and process-group teardown)
├── probe_runner_scheduler.py   (its sequential/--jobs orchestration, retries, summary)
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
