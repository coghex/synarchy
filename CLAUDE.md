# CLAUDE.md

Guidance for Claude Code and other agents working in this repository.

This file carries only the rules that prevent damage, each with the
gate that proves it. Two layers sit below it:

- [`docs/engine_contracts.md`](docs/engine_contracts.md) — the as-built
  mechanics behind every contract named here, each enforced by the gate
  its entry names. Read the relevant section before changing code it
  covers.
- Nested `CLAUDE.md` files, loaded on demand when you work under that
  directory: `app/` (boot modes, CLI flags, resource root), `scripts/`
  (Lua and UI contracts), `src/World/` (worldgen, the seam frame,
  worldgen testing), `src/World/Save/` (save format, enum policy),
  `src/Unit/Atlas/` (unit animation art). If a change reaches into one
  of those areas from elsewhere, read its file first.

Every earlier version of this file is archived verbatim under
`docs/history/claude_md_<date>_pretrim.md`; consult those, git history,
or the referenced issues when you need the story behind a terse rule.

## Build, run, test

- **Build:** `cabal build all` (does NOT build test suites — use
  `cabal build synarchy-test-headless` explicitly)
- **Run:** never bare — see **Launch rules** below
- **Debug output:** `ENGINE_DEBUG=Vulkan,Graphics,...`
- **Do NOT use `-f dev` for routine work.** Full prod rebuild ~1.5 min
  (parallelized via `ghc-options: -j` in `cabal.project` — NOT cabal's
  `semaphore:` jobserver, which deadlocks under concurrent worktree
  builds, #471), and flag-profile switches force one. The `dev` flag
  (Vulkan validation layers, ASan on macOS, `ENGINE_DEBUG` plumbing) is
  only for actively chasing graphics/memory bugs — give it its own
  build dir so flipping back is free:
  `cabal build -f dev --builddir=dist-dev` (every run/test in that
  profile needs the same pair). Production builds use `-O2 -optc-O3`.
- The executable is built with `-rtsopts` (baked-in default
  `-N -A128M`) — append `+RTS -s` etc. at run time without a rebuild.
  Cost-centre profiling has two hard rules; see `src/World/CLAUDE.md`.
- **Pre-push gate:** `make ci` runs the same gate set as `ci.yml`'s
  `test-and-audits` worker — the warning-clean (`-Werror`) builds, the
  full-tier headless suite, and every `python3 tools/*.py` audit and
  self-test — and `tools/ci_parity_audit.py` (#1355) fails on any
  drift between the two in either direction. Enumeration and the
  CI-only exemptions: `docs/engine_contracts.md` §The `make ci` gate
  set. It uses the prod profile and your warm `dist-newstyle`. **It is
  NOT an iteration loop and must not be run automatically before
  opening a PR** — only on an explicit user request for full local CI
  validation.

### Testing tiers

Worldgen is the entire cost of the test stack (~10 s per w64
generation); every non-worldgen test is milliseconds. Pick the cheapest
tier that covers the change; don't run the gates as an iteration loop.

1. **Iteration (seconds–1 min).** Targeted hspec:
   `cabal test synarchy-test-headless --test-options='--match "<describe name>"'`.
   Worldgen-output sanity: `python3 tools/world_check.py --quick`
   (6 seeds, <1 min).
2. **Before reporting done — select only relevant checks.** A targeted
   `--match` describe exercising the changed behavior, plus the focused
   probe for the affected subsystem when one exists. Run each of these
   ONLY when its own inputs changed:

   | Gate | Run it when you changed |
   |---|---|
   | `world_check.py --quick` | worldgen output |
   | persistence inventory audit | its root owners/registry or inventory docs |
   | EngineEnv capability audit | `EngineEnv`'s field set or `docs/engineenv_capability_inventory.md` |
   | a module-budget guard | a capped module |
   | `test_audit.py` | `world_audit.py` / `world_check.py` |
   | `test_run_probes.py` (~15 s, GPU-free) | `run_probes.py` |
   | `test_persistence_contract_sweep.py` (pure, <1 s) | `persistence_contract_sweep.py`'s `SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` or `probe_runner_registry.PROBES` |
   | `test_save_compat_audit.py --only-reproducibility` (~26 s, spawns a `cabal repl`) | the save format, the tracked fixture corpus, `save_compat_audit.py`, or a Cabal path — `ci_expensive_gates.py --gate save-compat` is the authority; `--without-reproducibility` covers the rest of the module and is cheap |
   | `findings_report_audit.py` | a findings report |
   | unit-asset gate (`test_pack_atlas.py` + `pack_atlas.py --validate-only --strict`, ~2 s) | `assets/textures/units/`, `data/units/`, `tools/unit_texture_budget.json`, `src/Unit/Atlas/`, or the unit-YAML / preview / registration decoders |

   Do NOT run the whole headless suite, the 21-seed world check, or
   `make ci` by default — CI is the full-suite authority.

   **A headless fixture logs nothing (#1925).** Every `test-headless`
   engine boots through `Test.Headless.Harness.Log`, never
   `Engine.Core.Init.initializeEngineHeadless`; rerun with
   `SYNARCHY_TEST_LOG=stderr` to get a quiet fixture's output back
   (any value other than `stderr`/`stdout`/`quiet`/empty is a hard
   error). Contract: `docs/engine_contracts.md` §Headless fixture
   logging. Gate: hspec `--match "headless fixture logging"`.
3. **Worldgen-OUTPUT changes only (full tier).**
   `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless`, then
   re-capture baselines with `python3 tools/world_baseline.py` (~7 min),
   re-run world_check, and bump the save version. The variable is
   wholesale, not per-test, and an EMPTY value reads as ENABLED; CI
   sets it on every worldgen-output PR and every push to master.
   Procedure and the fast-suite conventions: `src/World/CLAUDE.md`;
   semantics: `docs/engine_contracts.md` §The full test tier.
4. **Behavior probes — opt-in, not a default gate.** About a hundred
   headless `tools/*_probe.py` scripts each boot a real engine and gate
   one system (`tools/README.md`). Run the ones relevant to what you
   touched: `python3 tools/run_probes.py --only <substrings> [--jobs N]`
   (bare run = full sweep, tens of minutes).
   `python3 tools/ci_probes.py --status` is the authoritative list of
   every probe's CI eligibility — never trust a prose list of probe
   names. The path→probe map for CI's
   blocking, path-selective PR probe gate lives in `tools/ci_probes.py`
   (a change there re-runs its `--self-test`); promoting a probe to the
   gate = move its key from `MANUAL_ONLY_REASONS` to `CI_ELIGIBLE` after
   proving it deterministic, broad, and cheap.

**Module-budget scope:** the 500-line Haskell/Lua limits
(`tools/haskell_module_budget.py`, `tools/lua_module_budget.py`, CI +
`make ci`) are per-split ratchets, enforced only for module families
explicitly listed in the budget tool — not a tree-wide size policy. For
a structural split with no explicit budget entry, extract the cohesive,
correctness-relevant boundary first even if the facade remains above
500 lines; record a later pass rather than forcing unrelated refactoring
just to hit 500.

## Launch rules

- **NEVER launch `cabal run synarchy` / `cabal run exe:synarchy` without
  `--dump`, `--headless`, or `--offscreen`** — otherwise it opens a
  graphical window that steals the user's focus. `--offscreen` uses the
  GPU but creates no window, so it is safe. **`--preview` is NOT in this
  safe list** — outside the sprite-signoff workflow in §AI asset
  generation, never launch it yourself even transiently; a valid target
  steals focus like the graphical path.
- **Prefer `--dump` for testing** — self-contained, no TCP, JSON to
  stdout, implies headless.
- With `--headless`/`--offscreen`, use `--port 9008` (or another
  non-8008 port) — 8008 may be the user's graphical instance. The
  console is required in those modes: a port that cannot bind aborts
  the boot with no `READY` marker (#1190), so a wait loop fails fast.
- **NEVER use `pkill -f synarchy`** — it kills the user's GUI. Shut down
  your own instance with `echo 'engine.quit()' | nc -w 2 localhost 9008`,
  or track your PID (`HPID=$!`) and `kill $HPID`. A stale instance on a
  port: `lsof -ti:9008 | xargs kill`.
- Boot commands, the wait loop, dump layers and per-tile fields, the
  console workflow, the query API, and console save/load:
  [`docs/headless_console.md`](docs/headless_console.md). Per-mode
  flags and their validation: `app/CLAUDE.md`.

## Working-tree discipline

**`~/work/synarchy` is the PRIMARY checkout and must be left CLEAN.**
The PR drainer fast-forwards it after every merge, autostashing
whatever uncommitted work it finds; a restore that CONFLICTS wedges
every later drainer pass until a human resolves it. So: **any file you
write into the repo but do not commit belongs in the docs worktree,
never the primary checkout** — report annotation, findings documents,
design-doc drafts, anything a workflow leaves sitting for review. The
same rule covers EDITS to tracked documents (`CLAUDE.md`, anything
under `docs/`): unless the work is a PR running in its own separate
worktree, make the edit in the docs worktree and land it with
`tools/docs_land.sh` — never write to a markdown doc in the primary
checkout. Resolve the worktree by BRANCH — never hard-code the path,
never assume the current directory is right:

```bash
DOCS_WT="$(git worktree list --porcelain \
  | awk '/^worktree /{p=substr($0,10)} /^branch refs\/heads\/docs-wip$/{print p; exit}')"
[ -n "$DOCS_WT" ] || { DOCS_WT=~/work/synarchy-docs
                       git worktree add "$DOCS_WT" -b docs-wip origin/master; }
```

**An agent never lands docs on its own.** Landing is the user's call:
either they ask for it explicitly, in which case use
`tools/docs_land.sh -m "Subject" docs/foo.md […]` (`-n` dry run, `-f`
to proceed past the risk warning), or the work accumulates uncommitted
in the `docs-wip` worktree until they batch it. Never push `docs-wip`,
never run `docs_land.sh` unasked, never hand-roll an equivalent.
`docs-wip` is not a feature branch — it tracks `origin/master` and
lands by direct push; the script's guarantees and the manual fallback:
`docs/engine_contracts.md` §Docs landing.

Exempt, because they either create their own worktree or must operate
on the primary checkout: `solve`, `pr-revise`, `repair`, the read-only
`pr-review` / `pr-rereview` / `issue-review` reviewers, `drain-prs`,
`janitor`, `finalize`.

## Findings-report field ownership

A findings report (`docs/code_health_findings.md` and its siblings) is
written by two independent lanes that own DIFFERENT FIELDS of the same
entry. **The report-processing lane (`/process-report`) exclusively
owns an entry's status fields:** the checklist checkbox, the trailing
checklist marker, and the heading marker (`[#N]`, `[#N, <note>]`,
`[no-issue]`, `[deferred]`, or none), changed together in one edit.
**An implementation PR may add to or update a finding's narrative body,
and nothing else.** Landing the fix does not disposition the entry;
leave the box and both markers exactly as you found them. Why a
master-side report edit costs an open PR its approval:
`docs/engine_contracts.md` §Findings-report lane split. Gate:
`tools/findings_report_audit.py` (agreement of the markers only).

## AI asset generation

**Art is tracked work, exactly like code.** A texture, icon, sprite, or
animation that does not exist yet is a first-class blocker: it gets its
own issue and its own PR, and the project owner signs off on every
texture before it lands. **Stopping is the default.** At an art
blocker, STOP and return to the owner with the exact list of missing
assets and what each is for. Unless the owner has ALREADY stated which
method they want for that specific asset, assume neither. Never ship a
`wtNoTexture` or reused-sprite placeholder as if the work were done,
and never quietly narrow a slice to avoid the art.

Textures can be generated via the PixelLab MCP server. **Read
[`docs/asset_generation.md`](docs/asset_generation.md) before
generating** — validated pipelines, the raw API parameters the MCP
tools hide, and the gotchas that waste hours. That document also owns
the separate job most sessions actually need: regenerating, validating
and reviewing compiled unit atlases from tracked source frames, which
invents nothing and needs no external service.

**Sprite signoff uses a real preview window.** When an active task
reaches owner approval of a sprite or sprite group, do not ask the
owner to judge a chat thumbnail. From the task's isolated worktree,
first run `cabal build all`; only after that succeeds, launch that same
worktree's executable with
`cabal run exe:synarchy -- --preview <category>/<item>` and leave it
open so it takes focus for the decision. This is the one exception to the no-focus-stealing rule.
Record the owner's verdict.

## Language & conventions

- **Haskell with GHC2024**, cabal 3.16
- **NoImplicitPrelude** globally — all modules import `UPrelude`
- **UnicodeSyntax** globally — `∷` for type signatures, `→` for arrows,
  `⇒` for constraints, `∀` for forall

| Operator | Meaning | Standard equivalent |
|----------|---------|-------------------|
| `⌃` | Bitwise AND | `.&.` |
| `⌄` | Bitwise OR | `.\|.` |
| `⊚` | fmap | `<$>` |
| `⌦` | bind | `>>=` |
| `⌫` | reverse bind | `=<<` |
| `⚟` / `⚞` | const replace | `<$` / `$>` |
| `⊘` | filepath join | `</>` |
| `⊙` | filepath extension | `<.>` |
| `≫=` / `=≪` | monadic bind (from Control.Monad.Unicode) | `>>=` / `=<<` |
| `≡` | equality (from Prelude.Unicode) | `==` |
| `≢` | inequality (from Prelude.Unicode) | `/=` |
| `∧` / `∨` | logical and/or (from Prelude.Unicode) | `&&` / `\|\|` |
| `≠` | inequality — **prose only**, never an operator | use `≢` |

Five are **enforced** by `tools/unicode_operator_audit.py`: `.&.`,
`.\|.`, `>>=`, `==`, and `/=` must not appear as Haskell operators in
`src/`/`app/` outside its short exemption list (`UPrelude`'s own
definitions, `ShaderCode.hs`'s GLSL, the `Eq`/`Monad` instance method
names). `<$>` and `⊚` are a deliberate exception — both are kept,
picked per call site. `≠` is not: it is allowed only inside comment
prose and never as an operator (#1494); the same audit enforces that by
its own single-code-point path.

## Architecture

**Base/Types split.** Modules split into `Base.hs` (no local
dependencies — external packages only) and `Types.hs` (imports other
project modules freely). This prevents circular imports.

**EngineM.** `Engine.Core.Monad` defines `EngineM σ α` — a
continuation-passing-style monad with a concrete `EngineEnv` Reader
environment, concrete `EngineState`, IO, error handling, and logging.
Neither the environment nor the state is a type parameter.

**EngineEnv is a closed record.** `Engine.Core.State`'s `EngineEnv` is
one shared record reachable from any thread; the capability-split epic
that narrowed it is complete.
[`docs/engineenv_capability_inventory.md`](docs/engineenv_capability_inventory.md)
is the authoritative ownership inventory for every field, and
`tools/engine_env_capability_audit.py` (CI + `make ci`) fails if a
classification drifts from the live record. **Before adding any state,
read that doc's §6.4** — most cases resolve to "not on `EngineEnv` at
all" (`WorldState`, a manager, `EngineState`, or a local). Before adding
a capability record, read §2.1's convention block: each capability is
its own `Engine.Core.Capability.<Name>` module exporting one
`<Name>Capability` record plus a total projection; `EngineM` stays hard-wired to `MonadReader EngineEnv`.
**Full access is a closed allowlist:** importing `Engine.Core.State`
with `EngineEnv(..)` or bare is production-only and allowed only for
§6.1's hard-coded permanent list, which the audit requires to equal the
checked-in constants. The temporary ceiling is EMPTY and shrink-only —
"add the field now, narrow it later" does not exist. §6.4(c)/(d)'s two
escape hatches need explicit maintainer approval and synchronized
doc + constant + self-test changes.

**Threads,** communicating via STM: main (Vulkan render loop,
`app/Main.hs` → `Engine.Loop`), input (`Engine.Input.Thread`, a thin
facade over `.Dispatch`/`.Keyboard`/`.Char`/`.Mouse`/`.Scroll`, each
capped at 500 lines), Lua scripting (`Engine.Scripting.Lua.Thread`),
world (`World.Thread`), unit (`Unit.Thread`).

**Graphics.** Vulkan + GLFW. Bindless textures in
`Engine.Graphics.Vulkan.Texture.*`; batch rendering in
`Engine.Scene.Batch.*`; scene graph in `Engine.Scene.Graph` /
`Engine.Scene.Manager`.

**World generation.** Chunk-based with zoom-level LOD. Namespaces,
[`docs/hydrology_pipeline.md`](docs/hydrology_pipeline.md) (read before
adding river, lake, ocean, ice, or water-table logic — the namespaces
do not divide the way their names suggest), and the worldgen testing
conventions: `src/World/CLAUDE.md`.

**Lua scripting.** `Engine.Scripting.Lua.*` exposes the engine to
`scripts/` (repo-root relative). **Nothing under `scripts/` may call
`math.randomseed`** — `math.random` is gameplay's stream; UI code uses
`scripts/ui/random.lua`. Module layout, the unit-AI split, persistence
modules, and every UI contract: `scripts/CLAUDE.md`.

**UI.** `UI.*` handles focus, text input, and rendering; layout and
behavior are driven from Lua. The six input-routing contracts, the
container window stack, and the responsive lifecycle:
`docs/engine_contracts.md` §UI input routing / §Container window stack
/ §Responsive UI lifecycle, summarized on sight in `scripts/CLAUDE.md`.

**Resource root.** Every runtime resource family is loaded by
cwd-relative paths; the executable resolves ONE root at startup
(`--resource-root` > `SYNARCHY_ROOT` > cwd) and chdirs into it, so
relative OUTPUT paths land there too. Details: `app/CLAUDE.md`.

## Project layout

- `src/` — library source
- `app/Main.hs` — executable entry point (draw loop)
- `test/` — the `synarchy-test-graphical` hspec suite: GLFW-window and
  Vulkan window-target specs. **Automated gates only COMPILE it** —
  `test/Spec.hs` calls `GLFW.init` before `hspec` runs, so with no
  display it yields no assertions. Every GPU-free spec belongs in
  `test-headless/` instead (#1153)
- `test-headless/` — the `synarchy-test-headless` hspec suite, the
  running gate on every CI run
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config: tracked `*_default.yaml` templates +
  gitignored `*.local.yaml` runtime state
- `data/` — game data YAML (materials, vegetation, flora, units)
- `assets/` — images and graphical resources
- `scripts/` — Lua game logic
- `docs/` — design documents and contracts; `tools/` — Python audits,
  probes, and the CI gate scripts (`tools/README.md`)

## Domain contracts

The rule to know on sight, the gate, and where the mechanism lives.
Read the named section before touching the area.

- **Unit/combat animations** — headless has no pixels, but
  `unit.getInfo(uid)` exposes `currentAnim`/`animStart`; poll over time.
  Gate: `combat_anim_probe.py`. §Unit and combat animations headless.
- **Movement** — arenas via the tile-edit API on `world.initArena`;
  `setSlope` is the ONLY way to make a step walkable; fall checks
  assert fall + landing z, never arrival. Gate: `movement_probe.py`.
  §Movement arenas.
- **Position hold (#1216)** — completing a player-issued move holds the
  destination and stops autonomous contribution until re-commanded, a
  deliberate trade-off. `hold_position` scores EXACTLY
  `FOLLOW_COMMAND_UTILITY` (no second constant); only the ARRIVAL of a
  PLAYER-intent move creates a hold, only an explicit command or
  `unitAi.releaseHold` clears it; `lua.unit_ai` v6, older versions
  decode as not-holding. Gates: hspec `--match "position hold"`,
  `position_hold_probe.py`. §Position hold.
- **Tile-coordinate seam frame (#1175/#1230)** — points use CANONICAL
  coords and accept any alias; RECTANGLES localize the second endpoint
  to the anchor's frame BEFORE any clamp; terrain lookups
  `wrapChunkCoordU` before `lookupChunk`. Gates: hspec
  `--match "World.Render.PickSeam"` / `"World.DesignationSeam"` /
  `"a seam-frame unit"`. `src/World/CLAUDE.md`; §Tile-coordinate seam
  frame.
- **Construction (#95/#96)** — claim → source → progress → place →
  stake; costs in `data/structure_packs/*.yaml`. Gate:
  `construction_probe.py` (stake runs LAST). §Construction.
- **Roles (#265)** — DERIVED labels, never assigned; they multiply
  work-action ENTRY utilities only, never in-progress locks, survival,
  combat, or orders. Gate: `role_probe.py`. §Roles.
- **Crafting (#325–#795)** — `craft.execute` is station-blind;
  `craft.executeAt` needs an adjacent Built station; bills have three
  modes and the until-stock one re-checks LIVE ground stock; quality
  assertions must pin neutral mental effectiveness (#878). Gates:
  `craft_probe.py`, `craft_bill_probe.py`. §Crafting and bills.
- **Player transfers + orders (#1000–#1255)** — ONE pure policy
  (`src/Unit/Transfer.hs`) decides player moves between any two
  endpoints; the lax AI verbs are a SEPARATE path the fetch/repair/medic
  ladders depend on — never route AI work through the strict one, never
  delete them — and nothing crosses a world page (#1673). Two player
  modes share one commit policy; the immediate paths retired with #1249
  must not come back. Container contents are REMEMBERED, never live:
  exactly four things reveal, every unit-driven reveal is gated on
  `isPlayerCommandable`, knowledge is player-global. Design authority
  [`docs/unified_item_transfers.md`](docs/unified_item_transfers.md);
  read §Player transfers before touching an executor, a gesture's
  eligibility, or a teardown path. Gates: hspec `--match "Unit transfer"`
  / `"Unit cargo"` / `"AI page pairing"` / `"Transfer context menu"` /
  `"durable transfer orders survive"` / `"Container knowledge"`;
  `transfer_order_probe.py`, `item_list_widget_probe.py`, and the
  integrated `unified_transfer_probe.py` (manual-only, `needs-gpu`).
- **Power (#358–#1206)** — networks are recomputed every tick, only
  battery `storedWh` persists; load lives on the RECIPE (`power_draw`);
  pass the bill's own id to `power.isStationPoweredForRecipe`; a node's
  lifetime is its building's, retirement is a delete never a compaction,
  and there is no public `power.removeNode`. Gates: `power_probe.py`,
  `power_workshop_probe.py`, `machine_shop_probe.py`, hspec
  `--match "power node demolition"`. §Power.
- **Farming (#331–#336)** — flora growth is DERIVED from the calendar,
  nothing per-instance in saves; tagged harvest skips the fruiting
  window; consumers use `world.isPlantable`, never raw veg id 77. Gates:
  `flora_growth_probe.py`, `till_probe.py`. §Farming.
- **Location instances (#911)** — ids allocated at PLACEMENT in
  `overlayToList` order, never at stamp time; consumers read STORED
  values; lifecycle is one-way; `hinted` is unreachable but never
  deleted. Gates: hspec `--match "Location instance identity"`,
  `location_content_probe.py`. §Location instances.
- **Location clearance (#917)** — a CONJUNCTION of every condition the
  location authors, and the empty conjunction is false;
  `resolveLocationClearance` is the single writer; `significant: true`
  is legal only on a fixed `kind: item` entry whose id resolves; `taken`
  latches at the first successful pickup and nothing clears it. Gates:
  hspec `--match "Location significant contents"` /
  `"compound clearance with significant contents"`,
  `location_content_probe.py`, `expedition_loop_probe.py`. §Guaranteed
  significant contents.
- **Naming and etymology (#1092–#1104, #1265)** — names are WRITE-ONCE,
  rendered in the page's own language by the single writer at creation
  (`newLocationInstance`; `buildRiverNames` at world init) and read
  thereafter; a page with
  no provenance falls back to `ldLabel` with `gloss` ABSENT, never an
  invented language; river identity is `(WorldPageId, GeoFeatureId)`,
  CHECKED before trusted; etymology re-renders from the persisted source
  and checks against the stored text, and `world.getEtymology`'s
  `pageId` names the TARGET only. Gates: hspec
  `--match "Location naming"` / `"River naming"` / `"River identity"` /
  `"Language etymology"` / `"Etymology panel"`;
  `river_naming_probe.py`, `etymology_probe.py`. §World identity,
  §Location and river naming, §Name etymology.
- **Location discovery, map icons, per-unit knowledge
  (#780/#781/#915)** — discovery is a sight-based one-way promotion
  that ticks for every loaded page regardless of pause; `unknown` and
  `hinted` draw the one shared unknown icon; per-unit knowledge is keyed
  by identity, never distance, and neither layer derives from the
  other. Gates: hspec `--match "Location discovery"` /
  `"Location map icons"` / `"unit location knowledge"` /
  `"Unit.LineOfSight"`; `location_content_probe.py`,
  `location_embark_probe.py`, `tools/location_map_icon_asset_check.py`.
  §Location discovery.
- **Expedition retrieval (#920/#1291)** — direct-RTS verbs only
  (`unitAi.commandPickup` → `commandMove` home → the adjacent LAX
  `unit.depositToCargo`), no caravan interface until direct retrieval
  proves inadequate;
  `commandPickup` gates capacity at COMMAND and again on ARRIVAL;
  `pickup_timeout`/`TASK_TIMEOUT_SEC` are STALL timers in eligible time,
  never trip budgets. Gates: `expedition_retrieval_probe.py`, hspec
  `--match "commanded order stall budget"`. §Commanded-order stall
  budget.
- **The expedition loop (#923)** — `expedition_loop_probe.py`
  (manual-only, ~15 min, two boots) pins a real portal roster, a
  `cleared` end state earned by recovering the significant item, every
  identity re-checked in a fresh process, and an unprepared control
  that must end worse off. Read §The expedition loop before editing it;
  design authority
  [`docs/expedition_gameplay_loop.md`](docs/expedition_gameplay_loop.md).
- **Blood decals (#603)** — transient BY DESIGN: never persisted, a
  loaded session starts clean; a save/load survival test is testing
  absent behavior. Gates: hspec `Blood.*` groups, `blood_*_probe.py`.
  [`docs/blood_decals.md`](docs/blood_decals.md); §Blood decals.
- **Logging streams** — event, combat and injury logs are DRAINED
  streams; don't drain manually while the panel script is loaded. Gate:
  `injury_log_probe.py`. §Logging streams.
- **Autosave (#913)** — OFF by default; a wall-clock interval that
  fires only in the gameplay view and never resets in menus; slot
  ownership is the `smAutosave` flag, NEVER the name; publish first,
  rotate second; a failed autosave stays paused. Gate:
  `autosave_probe.py`. §Autosave.
- **Config state (#638/#786/#1937)** — settings save to gitignored
  `config/*.local.yaml`; legacy tracked files are a one-time migration
  source and a neutral placeholder is NOT promoted. A headless spec that
  drives a path WRITING `config/` wraps `withIsolatedResourceRoot`
  AROUND `withHeadlessEngine` (#1357). Gates: hspec `--match "config"`,
  `config_state_probe.py`, `config_migration_probe.py`. §Config state,
  §Config-writing tests.
- **Preview mode** — a real window; pre-boot rejection is the
  load-bearing rule. `app/CLAUDE.md`; §Preview mode.
- **UI** — code-point text coordinates, the six input-routing rules,
  the container window stack, the responsive lifecycle:
  `scripts/CLAUDE.md`.

## Save / Load

[`docs/persistence_contract.md`](docs/persistence_contract.md) is the
authoritative contract for what a save represents;
[`docs/persistence_state_inventory.md`](docs/persistence_state_inventory.md)
is the field-by-field classification, enforced by
`tools/persistence_inventory_audit.py`. Read the contract before adding
state to `EngineEnv`, `WorldState`, `World.Save.Types`, the three
gameplay managers, or the Lua save-module registry.

- **Enums are append-only.** Any enum serialized via
  `Generic Serialize` is positional by constructor tag, and a constructor's
  fields are positional too; inserting or reordering silently corrupts
  saves. Anything beyond appending is a per-component migration, never a
  `currentSaveVersion` change. Gate: `tools/enum_append_only_audit.py`.
- **A load is a whole-session transaction** that REPLACES the session;
  save and load mutually exclude; a failed load leaves the old session
  unchanged and paused. Disk I/O goes ONLY through
  `World.Save.Storage.publishGeneration`. `engine.loadSave` only
  ACCEPTS synchronously — poll `engine.getLoadStatus()`.
- **Not preserved by design:** selection, build-tool placement mode,
  active toolbar tool, time scale, blood decals.
- Save version bumps for the in-memory bridge are free; component
  version bumps need a frozen DTO, a migration, and a fixture.

Architecture, the full enum procedure, and every gate:
`src/World/Save/CLAUDE.md`.

## Unit animation art

Source PNG frames are the editable artwork; unit YAML is the only
hand-edited semantic authority; everything under a unit's `atlas/` is
DERIVED and nobody hand-edits it. One command covers inventory,
freshness and both budgets:
`python3 tools/pack_atlas.py --validate-only --strict`. Every shipped unit loads through the
compiled atlas path and there is no other; a bad index refuses the
whole unit. Read frames through the storage accessors, never by
matching the constructor; display a live frame via
`unit.getFrameSample` + `UI.setSpriteFrame`, never `getFrameTexture`.
The 384 MiB resident-memory threshold is the owner's call. Deleting art
needs the owner's explicit confirmation. Full contract and gates:
`src/Unit/Atlas/CLAUDE.md`.

## Platform notes

- Tested primarily on macOS; works on Linux with minor adjustments
- macOS: GLFW produces unavoidable junk on stdout
- macOS builds get `-DDARWIN` cpp flag and address sanitizer in dev mode
