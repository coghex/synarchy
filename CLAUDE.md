# CLAUDE.md

Repository instructions for all agents; `AGENTS.md` links here.
Synarchy is a Haskell/Vulkan game with Lua gameplay and UI scripts.
Keep this file to global safeguards, everyday commands, and required-reading
links. Put subsystem mechanics and gate details in their existing owners.
When moving a rule, verify its destination contains the rule and its gate;
preserve incoming section references. Prior trims are in
`docs/history/claude_md_*_pretrim.md`: use them for rationale, checking current
contracts and code before restoring historical instructions.

## Required reading

Before working in an area, read its instructions below, including when a
change reaches into that area from another directory. These rules bind every
agent; do not depend on automatic loading.

| Area | Read |
|---|---|
| Boot, CLI, resource roots | [app/CLAUDE.md](app/CLAUDE.md) |
| Lua, UI, input routing, container windows | [scripts/CLAUDE.md](scripts/CLAUDE.md) |
| Worldgen, terrain, tile coordinates, worldgen profiling | [src/World/CLAUDE.md](src/World/CLAUDE.md) |
| Save/load, serialized types, state, persisted content identifiers | [src/World/Save/CLAUDE.md](src/World/Save/CLAUDE.md) |
| Unit source frames, YAML, atlases, animation consumers | [src/Unit/Atlas/CLAUDE.md](src/Unit/Atlas/CLAUDE.md) |

For gameplay and engine behavior, also read the relevant section of
[engine_contracts.md](docs/engine_contracts.md), which owns the invariants
and their validation gates. Use its contents to select sections; see
**Domain contracts** below for additional authorities.

## Working-tree discipline

- **Keep the primary checkout (`~/work/synarchy`) clean.** The PR drainer
  updates it after merges; conflicting uncommitted work can block the drainer.
- Implement PRs in isolated worktrees. Put uncommitted reports, drafts, and
  documentation edits (including this file) in the `docs-wip` worktree.
  Resolve it by branch, never by an assumed directory:

  ```bash
  DOCS_WT="$(git worktree list --porcelain \
    | awk '/^worktree /{p=substr($0,10)} /^branch refs\/heads\/docs-wip$/{print p; exit}')"
  [ -n "$DOCS_WT" ] || { DOCS_WT=~/work/synarchy-docs
                         git worktree add "$DOCS_WT" -b docs-wip origin/master; }
  ```

- **Land docs only when the user requests it**, through
  `tools/docs_land.sh -m "Subject" <paths>` (`-n` dry run, `-f` proceeds past
  the risk warning). Otherwise leave them uncommitted in `docs-wip`.
  Never push `docs-wip` or hand-roll a landing; the helper integrates onto
  `master` and pushes directly. Details: engine contracts §Docs landing.
- Workflow skills that create their own worktree or require the primary
  checkout may use it: `solve`, `pr-revise`, `repair`, read-only reviewers,
  `drain-prs`, `janitor`, `finalize`.
- **Findings status belongs to `process-report`:** checklist checkbox,
  trailing marker, and heading marker change together. Implementation PRs
  may edit narrative only; fixing a finding does not disposition it.
  Gate: `python3 tools/findings_report_audit.py`.

## Launch rules

- **Never launch the game without `--dump`, `--headless`, or `--offscreen`.**
  A normal launch or `--preview` opens a window and steals focus. The sole
  preview exception is sprite signoff below.
- Prefer `--dump` for testing: headless, self-contained JSON, no TCP.
  For `--headless` / `--offscreen`, use `--port 9008` or another non-8008
  port; 8008 may belong to the user's GUI. Failed console binding aborts
  boot without `READY`.
- **Never `pkill -f synarchy`.** Track and stop only your own process, or
  send `engine.quit()` to its console port.
- Read [headless_console.md](docs/headless_console.md) for boot/wait loops,
  dump queries, and console use. Resource-root selection also determines
  where relative output paths land; see `app/CLAUDE.md`.

## Build, run, test

Use the production profile for routine work:

```bash
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "<describe name>"'
```

`cabal build all` does not build test suites. Use `-f dev` only for active
GPU/memory debugging, with a separate `--builddir=dist-dev` on every build,
run, and test in that profile. Debug logging: `ENGINE_DEBUG=Vulkan,Graphics,...`.
Keep build parallelism in `ghc-options: -j`; Cabal's `semaphore:` jobserver
can deadlock concurrent worktree builds.

### Testing tiers

- **Choose validation by changed behavior.** Before reporting done, run the
  relevant targeted hspec group and applicable subsystem gates from the
  linked contracts. Run tool self-tests when their inputs change.
  Cross-file test dependencies worth keeping explicit:

  | Changed input | Self-test under `tools/` |
  |---|---|
  | `world_audit.py` / `world_check.py` | `test_audit.py` |
  | `run_probes.py` | `test_run_probes.py` |
  | `persistence_contract_sweep.py`'s `SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` / `probe_runner_registry.PROBES` | `test_persistence_contract_sweep.py` |

- **Do not run full suites by default:** no whole headless suite, 21-seed
  world check, or bare probe sweep. Focused probes use
  `python3 tools/run_probes.py --only <substrings> [--jobs N]`.
  Probe eligibility and path selection live in `tools/ci_probes.py`
  (`--status`; run `--self-test` when changing it). Tool documentation:
  [tools/README.md](tools/README.md).
- **Worldgen-output changes require the full tier**, rebaselining, world
  checks, and a save-version bump: follow `src/World/CLAUDE.md`. Iteration
  sanity check: `python3 tools/world_check.py --quick`.
- **Run `make ci` only on an explicit request for full local CI.** It matches
  the combined `test-and-audits` and `static-audits` CI gate set; parity is
  enforced by `tools/ci_parity_audit.py`. Details: engine contracts
  §The `make ci` gate set. It is not a prerequisite to opening a PR.
- Expensive gate selection is owned by `tools/ci_expensive_gates.py`.
  Save-compat reproducibility runs only when its `save-compat` inputs change;
  `test_save_compat_audit.py --without-reproducibility` covers the cheap part.
- GPU-free specs belong in `test-headless/`; automated gates only compile
  `test/`, whose GLFW initialization prevents assertions without a display.
  Boot fixtures through `Test.Headless.Harness.Log`; use
  `SYNARCHY_TEST_LOG=stderr` to inspect quiet fixture output.
- **Headless success does not prove visual correctness.** It has no rendered
  pixels, and full `ui_manager` boot needs GPU fonts. Use `--offscreen` for
  end-to-end UI evidence; use the preview workflow below for sprite signoff.
- Static inventory audits check declarations and coverage, not successful
  serialization or restoration. Persistence changes also need behavioral
  evidence from the relevant tests/probes.
- Tests that write `config/` must wrap `withIsolatedResourceRoot` around
  `withHeadlessEngine`. See engine contracts §Config-writing tests.
  Runtime settings go in gitignored `config/*.local.yaml`; tracked defaults
  and legacy migration sources are not runtime output.
- Haskell/Lua 500-line budgets apply only to families listed in the budget
  tools. Run the matching guard when changing a capped module. For uncapped
  splits, extract a cohesive boundary; do not force unrelated refactoring
  to reach 500 lines.

## Language & conventions

- Haskell, GHC2024, Cabal 3.16; `NoImplicitPrelude` and `UnicodeSyntax`.
  Import `UPrelude`. Use `∷`, `→`, `⇒`, `∀`.
- Enforced operators: `⌃` / `⌄` for bitwise AND/OR, `⌦` or `≫=` for bind,
  `≡` / `≢` for equality/inequality. `≠` is prose only. Both `<$>` and `⊚`
  are allowed. See `src/UPrelude.hs` for other aliases and
  `tools/unicode_operator_audit.py` for the audited scope and exceptions.

## Architecture

- `Base.hs` imports external packages only; `Types.hs` may import project
  modules. Preserve this dependency split.
- `EngineM σ α` has concrete `EngineEnv` and `EngineState`; neither is a
  type parameter. Threads communicate through STM.
- `World.Hydrology` / `World.Fluid` concern world generation; runtime fluid
  simulation lives under `Sim.Fluid`. Read the hydrology map below before
  choosing an owner from a module name.
- **Before adding state**, read
  [persistence_contract.md](docs/persistence_contract.md) and
  [engineenv_capability_inventory.md](docs/engineenv_capability_inventory.md) §6.4.
  Classify new state owned by the inventoried roots, including transient
  state, in
  [persistence_state_inventory.md](docs/persistence_state_inventory.md).
  `EngineEnv` full access is a closed allowlist; use capabilities. Read the
  capability inventory §2.1 before adding one; §6.4(c)/(d) exceptions need
  maintainer approval. Run the affected inventory/capability audits.
- **`Generic Serialize` constructors and their fields are positional.**
  Append enum constructors only; other schema changes need a component migration.
  A save-version bump does not repair incompatible serialization. Every
  retained legacy wire type must preserve its original nested enum shapes;
  freezing only the latest outgoing type is insufficient. Follow
  `src/World/Save/CLAUDE.md` and `tools/enum_append_only_audit.py`.
- **Content changes can break saves without changing a codec.** Before
  renaming/removing authored definition identifiers, check persisted references
  and the persistence contract's content-integrity rules. A human-readable
  `name` can be a stable key (flora is one example); do not assume it is cosmetic.
- **Load replaces the whole session; save and load mutually exclude.**
  `engine.loadSave` only accepts the request; poll `engine.getLoadStatus()`
  for completion. Pre-publication failure preserves the old session and
  leaves it paused; post-publication reconciliation failure is unsuccessful
  but cannot roll back to the old session. Save publication goes through
  `World.Save.Storage.publishGeneration`. See engine contracts §Save/load transaction.
- Save/load preserves classified session state, not deterministic replay.
  Selection, placement/tool mode, time scale, and blood decals intentionally
  reset or disappear; check the persistence contract before treating that as a bug.

## Domain contracts

Read the relevant sections of [engine_contracts.md](docs/engine_contracts.md)
before changing behavior: movement/combat, position hold, roles, construction,
crafting, power, farming, transfers, expeditions, locations/naming/discovery,
blood, logging, autosave, config, UI, and save/load. Each section identifies
its rules and gates. Keep these cross-cutting traps visible:

- **Tile coordinates:** points use canonical coordinates and accept aliases;
  rectangles localize the second endpoint to the anchor before clamping.
  Wrap chunk U before `lookupChunk`. See §Tile-coordinate seam frame.
- **Transfers:** player moves share `src/Unit/Transfer.hs`; keep the lax AI
  verbs separate and never route AI work through the strict player policy.
  Neither path permits cross-page transfers. Container contents are remembered
  observations, not live reads. See §Player transfers.
- **Player orders:** arrival of a player move creates a position hold until
  explicitly cleared; autonomous work yielding to that hold is intentional.
  Order timeouts measure eligible-time stalls, not total trip duration.
  See §Position hold and §Commanded-order stall budget.
- **Lua randomness:** never call `math.randomseed` under `scripts/`.
  `math.random` belongs to gameplay; UI uses `scripts/ui/random.lua`.
  See §Lua random streams.
- **Log queries consume data:** event, combat, and injury logs are drained
  streams; do not drain them manually while their panel script is loaded.
  See §Logging streams.
- **Blood decals (#603):** transient by design; see
  [blood_decals.md](docs/blood_decals.md) for architecture and probes,
  and engine contracts §Blood decals for the transience contract.

Section names above refer to engine contracts. Additional required reading:

| Work | Authority |
|---|---|
| Rivers, lakes, oceans, ice, water tables | [hydrology_pipeline.md](docs/hydrology_pipeline.md) |
| Player transfers, gesture eligibility, executor/teardown behavior | [unified_item_transfers.md](docs/unified_item_transfers.md) and engine contracts §Player transfers |
| Expedition loop or its integrated probe | [expedition_gameplay_loop.md](docs/expedition_gameplay_loop.md) and engine contracts §The expedition loop |

## AI asset generation

- **Missing art is a blocker.** List the missing assets and their purpose;
  stop unless the owner has already chosen how to supply/generate those
  specific assets. Each new asset needs its own issue and PR, and owner
  signoff before landing. Do not substitute placeholders or narrow scope
  to hide missing art. Deleting art also needs explicit owner confirmation.
- Before generating, read [asset_generation.md](docs/asset_generation.md).
  PixelLab is available.
- **Sprite signoff uses a real preview window.** In the task's isolated
  worktree, first complete `cabal build all`, then run
  `cabal run exe:synarchy -- --preview <category>/<item>` from that worktree.
  Leave it open for the owner's decision and record the verdict. This is
  the sole exception to the no-window rule.

## Unit animation art

- Recompiling atlases from tracked source frames requires no external
  generation: PNG frames are artwork, unit YAML is semantic authority,
  and `atlas/` is derived; never hand-edit it.
- Unit art, YAML, budget, atlas, and decoder changes require
  `python3 tools/test_pack_atlas.py` and
  `python3 tools/pack_atlas.py --validate-only --strict`.
  Full rules: [src/Unit/Atlas/CLAUDE.md](src/Unit/Atlas/CLAUDE.md).

## Platform notes

- Developed primarily on macOS; Linux works with minor adjustments.
  Windows is not supported.
- On macOS, GLFW can emit diagnostic noise on stdout.
- macOS builds define `DARWIN`; `-f dev` enables address sanitizer for
  the graphical test suite.
