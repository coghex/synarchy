# River runtime characterization evidence

Evidence for epic #2718. Every archive here is **characterization, not
behaviour approval**: it records what a solver did, so later slices can measure
against it. Archives are never edited after they land; a new run adds a new
archive beside the old ones.

## Archives

| Archive | Produced by | What it records |
|---|---|---|
| `baseline-solver.json`, `baseline-solver-manifest.json` | `tools/river_runtime/Characterize.hs` | Eight two-cell cases through the real `Sim.Fluid.Active.simulateActiveTick`, ten fixed ticks each: exact source, target and total units per tick. |
| `arena-v1/`, `arena-v2/` | `tools/river_runtime_arena.py` | A finite-charge arena laboratory driven over the debug console: recipe, wet/dry and whole-z ceiling observations after wall-clock intervals, the socket transcript, and a manifest. |
| `arena-v3/` | `tools/river_runtime_arena.py` at `3e9f86887` | #2719's bounded smoke run of the recovered tool: the same recipe as `arena-v2` (byte-identical `recipe.json`), with the same stated limitations. |
| `harness-v1/` | `exe:river-runtime-harness` at `3e9f86887` | The five authored fixtures through the legacy adapter at origin, ordinary and wrapped-seam placements, every check result, origin-versus-translation comparisons, and the eight baseline cases reproduced exactly. |

Both sources were recovered from #2533's worktree and committed byte for byte
in #2719 (commit `2c69fe475`), so each archive names a tracked blob:

- `Characterize.hs` has SHA-256 `daaa59be…`, the manifest's `source_sha256`.
- `river_runtime_arena.py` at that commit has SHA-256 `0aaa93c9…`,
  `arena-v2/manifest.json`'s `driver_sha256`. The `arena-v1` driver predates
  it and was not recovered.

## Reproducing them

The baseline reproduces byte for byte from tracked source:

```bash
cabal run -v0 exe:river-runtime-characterize \
  | cmp - docs/evidence/river-runtime/baseline-solver.json
```

The arena tool has changed since `arena-v2` (#2719 made its console port
session-owned and its teardown own-process-only, and added manifest fields),
but its recipe and observation procedure have not: a new run writes a
`recipe.json` identical to `arena-v2/recipe.json`. Its observations are taken
after wall-clock intervals and are whole-z ceilings, so a rerun reproduces the
procedure, never the historical samples or an exact tick count. New socket
archives keep that limitation in their manifests.

```bash
cabal build exe:synarchy
python3 tools/river_runtime_arena.py --engine "$(cabal list-bin exe:synarchy)" \
  --out /tmp/river-arena-run
```

It boots `--headless` on a free loopback port (never 8008), with a resource
root copied into the output directory, so config, saves and the engine log
stay there, and it stops only the engine it launched. It is a manual tool, not
a CI or probe-runner gate.

## The hydraulic harness (#2719, RVR-01)

Exact, fixed-step experiments use the controlled hydraulic harness under
`tools/river_runtime/src/RiverRuntime/Harness/`:

- **Fixtures** (`Fixture`, `Catalog`) declare per-cell terrain elevation and
  fluid quantity in eighth-level units over named chunks, each chunk
  resident-active or resident-inactive; barriers; and timed terrain edits,
  sources and sinks with explicit accounting. The catalog holds a multi-chunk
  channel and reservoir, a closable dam with an openable side diversion, a
  raised sill on a chunk seam, a lake at rest that reaches equilibrium
  deactivation before a gate opens, and water beside dry cells.
- **Placements** (`Placement`) translate a fixture by whole chunks, including
  across the u seam of a cylindrical page, and normalize every report back
  into the fixture's own frame.
- **Adapters** (`Adapter`) are the interface a solver implements. The legacy
  adapter (`Legacy`) runs the real `simulateActiveTick`, one production tick
  per 100 ms logical step, and reports exact quantities only: it has no face
  records, and the harness reports face-level checks as unavailable for it.
  Candidate kernels (RVR-02) must supply face records.
- **Runs and checks** (`Run`) advance an explicit step count with no clock,
  then check every step against the fixture alone: declared initial state,
  storage limits, terrain, edit and solver accounting, barrier occupancy and
  per-region conservation, and face records when present.
- **Comparisons** (`Compare`) report the Appendix B metrics between two runs:
  equivalent-surface error (nearest-rank p95 and maximum) over the union of
  wet extents, wet/dry disagreement, wet-boundary distance, and milestone
  timing. They set no thresholds. The definitions are pinned in the module
  header and by the `Sim.Fluid.Harness` group.

Regenerate a harness archive with:

```bash
cabal run -v0 exe:river-runtime-harness -- --out /tmp/river-harness-run
cabal run -v0 exe:river-runtime-harness -- --list   # fixture names
```

It runs every selected fixture (`--fixture <name>`, repeatable; all by
default) through the legacy adapter at three placements (origin, an ordinary
translation, and a translation across a wrapped seam), compares the origin run
with each translation, and reproduces the eight baseline cases above against
`baseline-solver.json`. It writes `results.json` and `manifest.json`: the
source commit, whether those sources were dirty, the SHA-256 of every harness
and solver source and of the executable, each fixture's content hash, step
count and interval, the adapters' identity and configuration, and the
classification. Neither file records a time or the output path, so a rerun on
identical inputs writes identical bytes. It exits 1 when any check recorded a
violation or a baseline case failed to reproduce.

The gate is `cabal test synarchy-test-headless --test-options='--match
"Sim.Fluid.Harness"'`.
