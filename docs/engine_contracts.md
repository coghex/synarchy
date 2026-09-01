# Engine contracts: as-built detail

`CLAUDE.md` is auto-loaded into every session, so it carries the rules
that prevent damage: what you must not undo, and which gate proves it.
This file carries the layer below that — the as-built mechanics behind
those rules, extracted from CLAUDE.md in two passes (2026-08-18 and
2026-08-20) to keep the always-loaded file navigable.

**This is not a design document.** The design docs
(`docs/texture_infrastructure.md`, `docs/unified_item_transfers.md`,
`docs/expedition_gameplay_loop.md`, `docs/persistence_contract.md`, …)
record what was *decided*; this records what was *built*, and it is the
only prose record of most of it. Read the section here before changing
code in the area it covers — CLAUDE.md points you at each one by name.

Every contract below is mechanically enforced by the gate its CLAUDE.md
entry names, so a breach fails loudly rather than silently. That is
exactly why the detail could move out of the always-loaded file.

---

## Contents

**Build & CI**

- [The `make ci` gate set](#the-make-ci-gate-set)

**Assets and rendering**

- [Unit animation art: inventory structural invariants](#unit-animation-art-inventory-structural-invariants)
- [Unit atlas compiler: output and index invariants (#1258)](#unit-atlas-compiler-output-and-index-invariants-1258)
- [Unit animation atlas runtime: index validation and digests](#unit-animation-atlas-runtime-index-validation-and-digests)
- [Preview mode: the two viewers and the dump contract](#preview-mode-the-two-viewers-and-the-dump-contract)

**UI**

- [UI input routing (#742-#749)](#ui-input-routing-742-749)
- [Container window stack: panes, widget naming, teardown reasons](#container-window-stack-panes-widget-naming-teardown-reasons)
- [Responsive UI lifecycle (#748/#750)](#responsive-ui-lifecycle-748750)

**Scripting**

- [Lua random streams (#1330)](#lua-random-streams-1330)

**World and naming**

- [World identity and language provenance (#707/#1092/#1101)](#world-identity-and-language-provenance-70710921101)
- [Location and river naming (#1101/#1102)](#location-and-river-naming-11011102)
- [Name etymology: internals (#1104)](#name-etymology-internals-1104)
- [Location instances (#911)](#location-instances-911)
- [Guaranteed significant contents and compound clearance (#917)](#guaranteed-significant-contents-and-compound-clearance-917)
- [Location discovery, map icons, and per-unit knowledge (#780/#781/#915)](#location-discovery-map-icons-and-per-unit-knowledge-780781915)

**Gameplay systems**

- [Tile-coordinate seam frame (#1175/#1230)](#tile-coordinate-seam-frame-11751230)
- [Position hold (#1216)](#position-hold-1216)
- [Player transfers: the three player-facing modes](#player-transfers-the-three-player-facing-modes)
- [Commanded-order stall budget (#920/#1291)](#commanded-order-stall-budget-9201291)
- [The expedition loop: the unprepared control](#the-expedition-loop-the-unprepared-control)

**Persistence**

- [Autosave: staging, rotation order, and the intent mutex](#autosave-staging-rotation-order-and-the-intent-mutex)
- [Save/load transaction: phases and failure semantics](#saveload-transaction-phases-and-failure-semantics)
- [Enum append-only audit: baseline and payload normalization](#enum-append-only-audit-baseline-and-payload-normalization)
- [Config-writing tests: the isolation fixture (#1357)](#config-writing-tests-the-isolation-fixture-1357)

**CLI and boot modes**

- [CLI value validation (#1191)](#cli-value-validation-1191)
- [Debug-console listener policy (#1190)](#debug-console-listener-policy-1190)

**Process gates**

- [Findings-report lane split: why it matters](#findings-report-lane-split-why-it-matters)
- [Docs landing: docs-wip, autostash, and the protected-ref warning](#docs-landing-docs-wip-autostash-and-the-protected-ref-warning)

---

## The `make ci` gate set

CLAUDE.md states the rule: `make ci` runs the same gate SET as `ci.yml`'s
`test-and-audits` worker, `tools/ci_parity_audit.py` keeps the two from drifting,
and the gate is never an iteration loop. This is the enumeration and the
exemptions.

The set: warning-clean (`-Werror`) build of library/exe + both test
suites, the headless hspec suite, `test_audit.py`,
`test_determinism.py`, the Lua/Haskell module-budget guards, the Lua
duplicate-function audit, the Unicode-operator audit, the Lua
strict-decoder audit
(`lua_strict_decode_audit.py --self-test` then the bare audit, #1605 —
no direct `Data.Text.Encoding.decodeUtf8` under
`src/Engine/Scripting/Lua/`), the persistence-inventory / EngineEnv-capability
/ save-compat / enum-append-only / cabal-library-module-inventory /
material-id / bare-name-icon / concept-id-inventory /
findings-report-status audits (each
with its own self-test), the F4 Tier 1 coverage-mapping gate
(`action_outcome_coverage.py --self-test` then `--verify-tier1`, #1704 —
the only half of that tool that reads the real tree, and the only half
that blocks: the plain report stays a visibility report and always exits
0), the unit-asset inventory gate (`test_pack_atlas.py` +
`pack_atlas.py --validate-only --strict`), `world_check.py --quick`, the
sixteen probe-runner self-tests (`ci_probes.py --self-test`,
`ci_expensive_gates.py --self-test`, `ci_docs_fast_path.py --self-test`,
`test_run_probes.py`, `test_persistence_contract_sweep.py`,
`test_action_outcome_probe.py`, `test_tillable_fluid_filter.py`,
`test_probelib.py`, `test_probe_flake.py`, `test_probe_census.py`,
`test_probe_claim.py`, `test_probe_resource_lock.py`, `test_deflake.py`,
`test_location_embark_probe.py`, `test_probe_root_cleanup.py`,
`test_movement_probe.py`), `ci_cache_report.py --self-test` (#1358 — the
cache-outcome report's own classification, plus the `ci.yml` wiring it
reads), the project-cache epoch and cleanup policy self-tests
(`ci_cache_epoch.py --self-test`, `ci_cache_cleanup.py --self-test`), and the
parity audit itself.

**The bare-name-icon check (#1740)** is the newest member.
`tools/bare_name_icon_asset_check.py` resolves every authoritative
bare-name icon reference — `scripts/injuries.lua`'s `KIND_ICON`,
`INJURY_ICON` and its four icon-carrying functions,
`scripts/unit_info_v2_stat_defs.lua` and `scripts/unit_info_v2_status.lua`'s
literal `icon =` fields, `scripts/knowledge.lua`'s registry and
`M.UNKNOWN_ICON`, `data/infections/*.yaml`'s `icon:` scalars, and the
engine's own publications of that Lua field (`Units/Combat.hs`'s immunity
literal and `Asset/YamlInfection.hs`'s decoder default, found by scanning
every `.hs` under `src/`/`app/` that names it, so a new site fails rather
than joining unchecked) — through the SHIPPED GLOBAL index `scripts/unit_info_v2_panel_engine.lua`'s
`buildIconIndex` builds over `ICON_SUBDIRS`, last-wins on a duplicate
basename exactly as the runtime resolves one. It never requires a
reference to live in the row's own family: intentional cross-family reuse
is instead PINNED (skill rows drawing stat `agility`/`strength`, the
Status panel's stat `weight`, injury rows drawing status `pain`, status
condition rows drawing injury `nerve_injury`/`festered_injury`/
`frostbite`), so a family-local reinterpretation fails rather than
silently changing meaning. Each pin binds to the exact reference SITE and
the exact ROWS of it that reuse the asset — never to "the basename appears
somewhere" — because `agility` and `strength` are each used by their own
physical-stat row AND by a skill row in one file, so a basename-only pin
would keep passing after the pinned reuse was deleted. It also pins the
two runtime family inventories (`ICON_SUBDIRS` and
`scripts/startup_loader.lua`'s preload list) to each other and to every
family's `<kind>_unknown.png`. Extraction refuses rather than narrows: an
unsupported table shape, a computed `icon` assignment outside the closed
reason-carrying forwarding allowlist, an `icon` assignment outside the
enumerated reference sites, an unterminated string, and any enumerated
source, table, anchor or allowlist entry yielding zero matches are each an
error naming `file:line`. Per-FAMILY fallback-asset presence stays
`tools/texture_subset_audit.py`'s job, and
`assets/textures/icons/location/` is outside `ICON_SUBDIRS` and owned by
`tools/location_map_icon_asset_check.py`.

**The world-determinism content-identity self-test (#1724)** was the
previous newest member. `tools/test_determinism.py` is the executable
specification of what `tools/world_determinism.py` means by
"content-identical" — a reversed tile array and a reordered-key tile
must hash EQUAL, while a changed field, a missing tile and an unstable
canonical form must not. Issue #23 / PR #34 chose content identity over
byte identity deliberately, and this is the only place that choice is
asserted; it defines the relation, it does not change it. `world_check
--quick` hashing six real seeds against their baselines (#1361) does
NOT cover it — the engine emits tiles in a stable order, so a regression
that made the checker order-SENSITIVE would still produce matching
hashes and pass every gate. Pure Python, no engine, no GPU, no network,
sub-second, and deliberately UNCONDITIONAL on both sides rather than
behind the worldgen selector that gates `world_check --quick`: the
contract lives in `tools/`, and a change that selector would not fire on
can break it.

**The concept-id-inventory audit (#1717, #1868).**
`tools/concept_id_inventory_audit.py` pins every concept id
`data/language/concepts.yaml` has shipped against
`data/language/concept_id_baseline.json`: a removal fails naming the id
and why it is immutable, a rename fails as BOTH a removal and an
addition, and a new id passes only through `--update-baseline`, a
MONOTONIC ratchet that refuses any run which would drop a recorded id.
It guards the id's presence and exact string — the whole compatibility
boundary, since `Language.Etymology` reports a missing id as
`EtyInvalidConcept` and `Language.Generated.Hash` seeds each concept's
native root from the id string — and deliberately does NOT freeze the
four authored English forms or the `domain`, which stay editable. That
scope means same-string REPURPOSING is review policy, not something
this gate can see. Contract comment: `src/Language/Semantic/Types.hs`.

Since #1868 the artifact records one more thing, and it is not
documentation: each id's append-only **ordinal**, which is the order
`Language.Generated.Root.assignRoots` places concepts in. That is why
the file lives under `data/` — `Language.Semantic.Catalogue` LOADS it at
run time through the resource root beside `concepts.yaml`, both files
are validated against each other, and a missing, malformed or
disagreeing artifact rejects the catalogue rather than falling back to
ascending-id, authored-YAML or caller order. `Catalogue` carries the
result as `catOrdinals`, so root assignment stays pure and cannot be
reached without it. The ordinal exists because a reroll mixes
`attempt + 1` into the concept seed, so a displaced concept gets a
completely different root, not a near variant: under the old
ascending-id placement a newly ADDED id sorting before an incumbent
could take that incumbent's root and silently cost every persisted
`EtymologySource` naming it its etymology (the name itself is
write-once, #1101, so nothing visible changed). The 151 seeded ordinals
are ascending-id RANK, so the change was byte-identical for every
existing language and needed no `currentGeneratorVersion` bump.

Two rules are worth knowing before touching either side. The audit
enforces the artifact's **shape** — ids unique, ordinals unique, and the
recorded ordinals exactly `0..n-1` — while the Haskell reader enforces
only what PLACEMENT needs (unique ids, unique ordinals, and id-set
agreement with the catalogue); that split is deliberate, so the two
enforcement points cannot drift into disagreeing about the same rule.
And addition-stability is scoped to the FREE root: from generator
version 4 on, bound-form selection ranks the complete current concept
set (`Language.Generated.Bound`), so an addition can still move a bound
form and the names that use one. Gates: `--match "concept roots"` (the
identity against ascending-id placement over every supported version,
the addition panel, its adversarial ascending-id twin, and a pinned
full root map for seed 1337) and `--match "concept placement order"`
(the artifact's own loading and every rejection).

**One member of the save-compat self-test is path-selective on BOTH
sides (#1360).** `tools/test_save_compat_audit.py` gained two flags that
partition it: `--without-reproducibility` runs every member except
`test_normalize_fixture_timestamp_makes_generation_reproducible`, and
`--only-reproducibility` runs exactly that one. The excluded member
spawns its own `cabal repl test:synarchy-test-headless` to build two
envelopes differing only in `smTimestamp` — ~26 s of a ~58 s module on a
warm tree — and it exercises fixture GENERATION, which only a
save-format, fixture, save-tooling or Cabal change can move. Local
`make ci` runs `--without-reproducibility` unconditionally; CI runs it
for every non-docs-only change and every save-compat input change. Both
sides reach `--only-reproducibility` through
`ci_expensive_gates.py`'s `save-compat` gate. A bare `python3
tools/test_save_compat_audit.py` still runs everything, which is what a
developer running the module by hand gets.

This is the ONE case where `make ci` is path-selective rather than
unconditional, so it needs its own local changed-path notion:
`ci_expensive_gates.py --local-changed-paths` prints every TRACKED path
differing from the merge base with the checked-out default branch
(committed, staged and unstaged alike), and `tools/ci-local.sh` pipes
that into the very same `--stdin --gate save-compat` command CI runs —
one matcher, one answer, no second table to drift. When no default
branch or merge base resolves, `--local-changed-paths` emits a
conservative sentinel that selects EVERY gate: a local gate that cannot
tell what changed runs the coverage rather than skipping it.

**`ci-local.sh` resolves that list BEFORE it writes its own temporary
`cabal.project.local`, and the order is load-bearing.** That file is not
gitignored, so a change can legitimately track one — and cabal would
apply it in CI, which is why `cabal.project*` (the `.local` member
included) is in the gate's pattern table. Resolving after the write
would report this gate's own scratch edit to a tracked file as if it
were the candidate's. `ci_parity_audit.py` checks that ordering, and the
marked block reads the already-resolved `$SAVE_COMPAT_PATHS` rather than
re-deriving it, which would put the resolution back after the write.

Outside CI's ordinary-docs fast path, the main save-compat step stays
deliberately blocking: `save_compat_audit.py` runs in full, as does every
other member selected by `--without-reproducibility`. Local `make ci`
always runs that step. It is not cabal-free:
`save_compat_audit.py`'s real-manifest run decodes the tracked fixtures'
envelope descriptors through a `cabal repl` of its own
(`verify_fixture_descriptors` → `dump_fixture_descriptors`), and
`test_real_manifest_passes_the_audit` runs that same audit. Therefore CI
skips the whole step only when `ci_docs_fast_path.py` has proved that the
change is ordinary documentation outside `docs/save_compat/`.

The conditions themselves are gated, not just the command set:
`ci_parity_audit.py` pins the main audit's docs-only exception and the
reproducibility member's `save-compat` guard to their exact canonical
text, checks that the latter reads the output the selector step writes,
and refuses a bare invocation on either side. The post-merge backstop is
now supplied by the selector input: an ordinary-docs push uses its real
changed range, while every other master push supplies `git ls-files`,
which necessarily selects `save-compat` through the workflow, Cabal and
Makefile entries in `SAVE_COMPAT_GLOBS`. Because a set comparison cannot
see a condition, the parity audit also EXTRACTS the marked selection
block from `ci-local.sh` and EXECUTES it against a positive and a
negative changed-path sample with `python3` shimmed, so a block that
stopped guarding the member fails there rather than after a push.

**Same gate SET, not the same conditional control flow:** CI
path-selects the graphical suite build, the unit-asset gate and
`world_check` on PRs, while `make ci` runs all three unconditionally.
Since #1490 CI also has a **docs-only fast path on pull requests and
master pushes**: when every path in the complete base/pushed range is
documentation — under `docs/`, a plain add or modify, and never under
`docs/save_compat/`, whose machine-readable contracts require the real
codec — dependency-plan resolution, both caches, the cabal build, both
test-suite builds, the headless hspec suite, `world_check`, and both
Cabal-backed save-compat steps are skipped. **Every engine-free Python
audit still runs, self-tests included.** That asymmetry is the point
rather than an oversight: #1490's cause was a docs-only push breaking
`test_findings_report_audit.py`, so a fast path that skipped all audits
would hide the very failure it was built for. The separate PR-only
behavior-probe job also selects no probes for documentation. `make ci`
has no event change range and so has no fast path; it always runs
everything.
The CI-only invocations split across the two worker jobs. In
`test-and-audits`, the path SELECTORS
(`ci_expensive_gates.py --stdin --gate worldgen|graphical|unit-assets`
and `ci_docs_fast_path.py --stdin --explain`) have nothing to select
locally. Its bare `ci_cache_report.py` (#1358) classifies what the two
`actions/cache` restore steps got from outputs only a runner publishes;
`make ci` restores no GitHub Actions cache, so it has no outcome to
classify, and the command reports rather than gates. Its `--self-test`
form is not exempt and runs on both sides.

The same CI-only exemption applies to `ci_cache_epoch.py --ref ...`: that
invocation derives the GitHub Actions key and writes runner outputs, while a
local gate has no GitHub cache to address. Its `--self-test` is not exempt and
runs on both sides. The epoch is counted in one `git log` pass from the tool's
checked-in anchor over first-parent master history, advancing on each eighth
build-relevant change. PR workers derive it from the PR base SHA and are
restore-only; only a successful master push saves `dist-v3`. A missing,
pre-anchor or rewritten base emits a warning and uses epoch 0 rather than
failing an otherwise valid older PR. Ordinary docs and runtime-resource changes
do not count.

Every v3 primary key and compatible restore prefix also carries the exact
immutable image reference selected by `resolve-image`, in addition to the OS,
GHC, Cabal and plan inputs. Therefore an image-only PR cannot exact-hit or
prefix-restore project objects created in another image. The pre-v3 bootstrap
prefix is enabled only when the resolved image equals the one known to have
created those legacy entries; later image identities get a disabled prefix.
That historical image constant must never be advanced with the image recipe.

Old project caches are never deleted by CI. The maintainer command
`python3 tools/ci_cache_cleanup.py` is a dry run unless `--delete` is present,
uses exact cache IDs, keeps three v3 snapshots per compatible image/toolchain by default,
and scopes itself to `refs/heads/master`. `--include-legacy` remains guarded:
it refuses to select v2 until the same ref contains a successfully seeded v3
project cache. Dependency caches and PR-ref caches are outside the default
selection.

The separate `behavior-probes` job owns `ci_probes.py --stdin` and the
engine-booting `run_probes.py` sweep; neither is part of the
`test-and-audits` / `make ci` parity contract because CLAUDE.md keeps
behavior probes opt-in locally. The stable `build-test` context depends
on both parallel workers and is the single CI verdict the admin-bypass
PR drainer consumes, so moving the sweep out of the heavy worker does
not weaken its blocking PR verdict. The parity audit pins that aggregate
wiring and the probe worker's selector and runner commands.

One invocation is LOCAL-only for the mirror-image reason:
`ci_expensive_gates.py --local-changed-paths`, because CI is handed a
pull-request base sha and `make ci` has to resolve its own. The
`--stdin --gate save-compat` decision both of them feed is NOT exempt —
it runs on both sides. Everything else must
run on both sides — `tools/ci_parity_audit.py` (#1355, CI + `make ci`,
with its own `--self-test`) compares the two files' `python3 tools/*.py`
invocations at command-and-arguments granularity in both directions and
fails on any difference outside that hard-coded, reason-carrying
exemption list, so this enumeration cannot go stale silently.
Environment preparation and cache actions are not part of the audited
gate set.

Mechanics: it uses the prod profile and your warm `dist-newstyle`.
`-Werror` is checked into `synarchy.cabal`'s warning policy (not
injected by this gate), so `tools/ci-local.sh` only scopes a temporary
`-fforce-recomp` via `cabal.project.local`, restored on exit.

---

## Unit animation art: inventory structural invariants

#1261 (TEX-6) promoted `tiller`, `unknown_unit` and `white_tailed_deer`
to real `units:` entries. `unknown_unit`'s hard-coded missing-texture
fallback (`unknownUnitTexture` in
`Engine.Scripting.Lua.API.Units.List`) is untouched by any of this.
Outside this inventory's scope:
`assets/textures/units/unknown_unit/rotations/*.png` and the per-unit
`portrait.png` files, referenced from hard-coded Haskell or non-animation
YAML fields. In preview mode a `flora/unknown_flora.png`-style FILE where
a directory was expected is a pre-boot rejection, not a fallback.

Enforced by `python3 tools/pack_atlas.py --validate-only --strict`; gate
for the checker itself is `tools/test_pack_atlas.py`. Each breach names
the real problem rather than failing generically.

- A unit identifier is one lowercase `[a-z0-9_]+` path component. An
  animation identifier is the same, plus ONE narrowly matched approved
  exception, `<lowercase>_RH_<lowercase>`, for the documented
  asymmetric-weapon animations — so `attack_heavy_RH_dagger` passes while
  `AnyThing`, `attack_heavy_RH_Dagger` and `attack_LH_dagger` do not.
- Frames are `frame_NNN.png` with exactly three digits, so `frame_1.png`
  and `frame_0002.png` are rejected rather than read as another spelling
  of an index.
- A declared path is relative, `..`-free, symlink-free, and resolves
  inside its EXACT `<unit>/animations/<animation>/<direction>/`
  directory, so cross-unit, cross-animation and cross-direction
  references are each named as such.
- `flip: true` declares exactly the canonical five authored directions;
  `flip: false` exactly all eight.
- Per direction, indices start at 0, ASCEND in the order they are
  declared, and have no gaps or duplicates. Ascending order matters
  because playback walks the declared list: a contiguous-but-shuffled
  list plays out of sequence while every set-based check still passes.
  Different directions of one animation may hold different counts.
- `fps` is a positive number that survives the engine's 32-bit `Float`,
  and `loop` a boolean — rejected rather than coerced when they are not.
  The `fps` guards stack because a positivity test alone is not enough:
  PyYAML resolves `.nan`/`.inf` to real floats (`nan <= 0` is False like
  every NaN comparison, and infinity really is greater); a Python int has
  unbounded precision, so a thousand-digit `fps:` is valid YAML that
  makes `math.isfinite` RAISE rather than answer; and `1.0e+100` /
  `1.0e-100` fit a 64-bit double but land in `UnitYamlAnim`'s
  single-precision field as infinity and zero.
- No symlink may appear anywhere in the walk — unit directory,
  `animations/` root, animation directory, direction directory, or frame.
  A symlinked entry is an ERROR, never a skipped one, or a linked tree
  would evade the inventory while its frames still ship.
- A `--unit` naming neither a declaration nor an asset tree exits
  non-zero rather than reporting a clean run of an empty inventory.

### Content validation: why three checks (#1311)

Every declared frame is opened and decoded, in three checks because each
covers ground the others cannot:

1. A full `decode_rgba8` covers the compressed pixel stream —
   truncation, corrupt deflate data, a non-image, and (via its own format
   check) a valid image of another format renamed `.png`.
2. Pillow's `verify()` then CRCs the chunks, which is the only thing that
   sees an intact payload under a WRONG checksum: the decoder reads and
   discards IDAT CRCs while streaming.
3. `locate_png_stream_end` covers the terminal **IEND** chunk, which
   `verify()` breaks ON without checksumming and the decoder never reads,
   plus anything appended after the image ends.

That last one walks chunk FRAMING only — length, type, payload, CRC —
decoding nothing, knowing no chunk type but IEND, and running only after
Pillow has CRC-validated that sequence, so it cannot disagree with the
real decoder about where a chunk lies. **Keep it that narrow**: a second
hand-rolled PNG parser is what sank the previous attempt at this issue.

Checking the FILE's last bytes is NOT equivalent, and was the round-2
review finding: appending a second canonical IEND leaves a perfect tail
while the real image ended 12 bytes earlier.

Do not "simplify" the three into one — `tools/test_pack_atlas.py`'s
`every content check earns its keep` case exists because each has a
fixture the other two accept.

Every frame of one animation must then decode to the same pixel size (the
atlas cell is that size and nothing resamples), while frame COUNTS may
still differ per direction. The rule is "decodes as a PNG", never "is
already RGBA8": paletted, greyscale, greyscale+alpha, 16-bit and
interlaced frames all pass.

Pillow is therefore load-bearing for validation, not just compilation —
an absent decoder is one loud error naming the install command, never a
silent skip that would print OK while checking nothing. It is still
imported lazily, which now only spares a run with no declared frames. The
content pass adds roughly half a second over the whole 4,620-frame corpus
(~1 s structural, ~1.5 s total), so it is unconditional rather than
hidden behind a flag.

---

## Unit atlas compiler: output and index invariants (#1258)

Enforced by `python3 tools/test_pack_atlas.py` (fixture-based, isolated
temp trees, never touching shipped assets) plus the strict
`pack_atlas.py --validate-only --strict` run. CLAUDE.md keeps the
one-atlas-per-animation shape and the index-aware validation rule; these
are the exact invariants.

- **Rows** are the AUTHORED directions in `ATLAS_DIRECTION_ORDER` — the
  engine's own `Unit.Direction` order `S, SW, W, NW, N, NE, E, SE` —
  five for `flip: true`, eight for `flip: false` (D-4), each row index
  recorded explicitly so nothing downstream re-derives the order.
- **Columns** are the max authored frame count. Unequal per-direction
  lengths are real (D-5): the index records each direction's TRUE count,
  shorter rows are padded with transparent RGBA8 zero SLOTS, and no
  padding slot is addressable — `frame_count` is the sole authority.
- **Cells are exact integers, at a PADDED stride** (#2076). Each cell
  occupies a physical SLOT of `(cell_width + 2*cell_padding)` x
  `(cell_height + 2*cell_padding)`, and frame `c` of row `r` has its
  LOGICAL cell at `(c*slot_width + cell_padding,
  r*slot_height + cell_padding)`. `cell_padding` is one texel per side
  and is the only value the runtime accepts; widening it is a schema
  change, not a constant edit. A size mismatch is a compile error, never
  an implicit rescale (D-6). Each cell is a byte-for-byte copy of its
  source frame's decoded RGBA8 SAMPLES, alpha included.
- **The gutter is that cell's own edge texels, extruded.** Sides copy the
  adjacent edge row or column; each corner square copies the single
  corner texel it touches. Nothing is blended or resampled — every gutter
  byte is a duplicate of a real frame texel. That is what isolates a cell
  under a LINEAR filter (epic #2072's TSR-3 precondition): a bilinear tap
  taken anywhere inside a logical cell reaches at most one texel past its
  edge, and so reads a copy of its own cell rather than the neighbouring
  frame. A rectangularization slot has no art, so its gutter is
  transparent too. NEAREST is unchanged by construction — the index
  addresses the inner cell, so no fragment centre moves.
- **The index** carries `schema_version` (the format the runtime parses)
  separately from `tool_version`, a documented `direction_order`, and
  per animation its storage format and path, atlas/cell dimensions,
  `cell_padding`, columns, rows, per-direction row and frame count,
  `flip`/`fps`/`loop` as the engine will hold them (`fps` narrowed to
  32-bit), and two `sha256` digests: a PER-ANIMATION `source_digest` over
  that animation's own declarations, cell geometry INCLUDING the gutter,
  and decoded pixels, and an `atlas_digest` over the atlas's decoded
  CONTENT rather than its file bytes. Per-animation is the point — one
  animation's edit must not invalidate an unrelated atlas (D-12).
  `source_digest`'s domain tag carries `v2` for the gutter, so no digest
  recorded before #2076 can collide with one taken over the same art at
  the padded stride.
- **Determinism and locality.** A clean rebuild under an unchanged
  toolchain is byte-identical; an incremental run writes only on a real
  content difference (an mtime-only touch changes nothing); obsolete
  atlases are removed from that unit's `atlas/` and nowhere else.
- **`--validate-only` is index-aware.** A unit with NO index is valid to
  THIS tool (an uncompiled tree is a legitimate working-copy state) but
  not to the ENGINE. Where an index exists it is REGENERATED from
  sources and compared, so a stale digest, a hand-edited index, a
  missing atlas and tampered pixels all report — and a tampered index
  cannot certify a tampered atlas. Compilation refuses outright on an
  invalid inventory.

Corpus numbers, kept out of CLAUDE.md because prose counts drift: 131
committed atlas PNGs + eight `index.json`, all tracked, well under
D-12's 2x on-disk ceiling (animation sources only), so the
choose-a-distribution-strategy clause is not reached. Resident cost is
the SEPARATE budget in `tools/unit_texture_budget.json`, re-measured at
121,620,320 bytes (115.99 MiB) after #2076's gutter — a 5.10% increase
over the edge-adjacent stride, leaving 152 MiB of headroom under the
unchanged 384 MiB threshold.

---

## Unit animation atlas runtime: index validation and digests

Gate entry points worth knowing by name: `registerUnitDefs` is what
`loadUnitYamlFn` delegates to, the on-disk fixture tree is driven through
`loadUnitAtlasIndexIn`, and the atlas slot's adoption of each global filter
value is checked through `planFilterRebind`.

Enforced by hspec `--match "Unit.Atlas"` and
`--match "the real unit registration boundary"`.

`Unit.Atlas.Load.loadUnitAtlasIndex` validates in three passes, cheapest
first, stopping at the first failure.

**(1) The index parses and is structurally sound.** Supported
`schema_version` and `digest_algorithm`; the unit's own identity;
duplicate animation names; containment of `atlas_path` inside that unit's
`atlas/` directory AND its equality with that animation's canonical
`<animation>.png`; positive geometry; a `cell_padding` equal to the one
supported layout; every reachable SLOT — the padded slot, not the bare
cell — lying inside the sheet; unique and in-range direction rows; real
frame counts bounded by row capacity; a positive finite `fps`.

`schema_version` is read off the RAW document and checked BEFORE the full
decode. A genuine v1 index legitimately lacks #2076's `cell_padding`, so
decoding first would blame that missing field and send a reader looking
for a corrupt index rather than an outdated one; the version has to be
the reported cause.

The canonical-path equality is what makes D-2's one-atlas-per-animation
hold *by construction*: no two animations can name one file, so the
upload path's otherwise-correct same-path aliasing can never collapse two
animations onto one image and one bindless slot.

**(2) It still describes what the unit YAML declares.** Animation set,
`fps`/`loop`/`flip`, direction set, per-direction frame counts, columns.
`Unit.Atlas.Index.planUnitAtlasStorage` owns this half, including the
reverse coverage: an animation the YAML DECLARES that the index does not
name rejects, because publishing the unit without it would silently drop
art the file asks for. Its result therefore covers exactly the YAML's
animation set, which is what lets the loader publish straight from
`atlasTextureRequests` — its own upload set, each request carrying the
animation's index record — with no second lookup that could miss.

**(3) Each atlas decodes to the image the index describes** (dimensions
plus `atlas_digest` over decoded RGBA8), AND every declared SOURCE frame
decodes to exactly the pixels its atlas cell holds AND its slot carries
the one-texel extrusion ring compiled from that same frame, corners
included (#2076). The gutter is generated, so it is verified rather than
assumed: an artifact whose ring does not reproduce is exactly as stale as
one whose cell does not, and a wrong ring is what would let a linear tap
read a neighbour.

### Both digests earn their keep

`atlas_digest` catches an artifact the index does not describe.
`source_digest` (`Unit.Atlas.Digest.sourceDigest`, recomputed from the
same inputs the compiler digests) catches a forged digest and a frame
whose PATH changed while its pixels did not — nothing else in the index
records paths. The per-frame pixel comparison still runs first, because
it localizes a stale artifact to one direction and one frame where the
digest can only say that something moved.

Reproducing `source_digest` means reproducing Python's `repr()` of the
narrowed fps (`pythonFloatRepr`), whose positional/scientific thresholds
Haskell's own `show` does not share. That is pinned against
CPython-generated reference values across the whole float32 range: a
formatting divergence must fail in the test, not by rejecting every atlas
of a unit whose fps lands in the disagreeing range.

Pass 3's source reading SURVIVED TEX-6, which #1259 expected to retire
it. Its cost was measured rather than assumed: decoding all 4,620 shipped
source frames across all seven units totals ~1.8 s of one-time unit-def
loading (`bear_brown`, the largest, 0.74 s), paid on the Lua thread while
YAMLs load and not on any frame. The source PNGs remain the tracked,
hand-edited artwork (D-1), so they remain something a developer can
repaint without recompiling, and CI's asset gate only runs on a push —
this stays the check that catches a stale artifact locally, in the same
run that would otherwise have drawn the stale art.

### The policy-aware upload cache

Every texture upload declares an `UploadSampler` policy
(`Engine.Graphics.Vulkan.Texture.Policy`), and the slot it creates is
registered with the sampler that policy names. `UploadPinnedNearest`
slots keep NEAREST for the session; `UploadGlobalSampler` slots are
repainted by a runtime `setTextureFilter` toggle.

Gameplay unit atlases use `UploadGlobalSampler` with one mip level (#2085),
so they follow the player's scene-art setting without introducing mipmap
sampling. Their one-texel extrusion rings (#2076) isolate every logical cell
under linear filtering. UI chrome and the icons the UI/HUD layers draw are
pinned by #2075, because the player's filter setting is a SCENE-art setting
and selecting linear used to blur the HUD. The world preview and zoom atlas
remain separately pinned to nearest and linear respectively.

**The policy is declared by the CALLER and never derived from a path**
(D-4). No directory rule survives the tree as it stands:
`assets/textures/icons/location/*` are drawn on the world's zoom map
while the rest of `icons/` is toolbar chrome;
`assets/textures/ui/hud/utility/{zoom,world}_*` and the six
`*_designate` markers are loaded in `hud.init` beside real chrome but
handed to `world.set*CursorTexture` / `<tool>.setDesignateTexture` and
drawn in the world; and `assets/textures/utility/white.png` is drawn by
both layers.

In Lua the declaration is `engine.loadTexture(path, "ui"|"scene")`.
OMITTING the argument selects `"scene"`, so every pre-#2075 call site is
unchanged. That is the ONLY shape that selects the default: an argument
that is PRESENT but names no policy — a typo, an explicit `nil`, or any
non-string value — is REFUSED with a warning and a `nil` handle, and
queues no load. An explicit `nil` is refused with the rest because it is
almost always a pass-through helper that lost its value, and accepting it
would file the texture as scene art on the strength of a bug
(`scripts/startup_loader.lua`'s preload helpers therefore *require*
their policy argument rather than defaulting it).

Haskell-side YAML art declares the same way: `loadAndRegisterWithPool`
takes an `UploadSampler` per call site. Most of it is world-drawn and
passes `UploadGlobalSampler`; the families whose only consumer is a UI
panel — a unit's authored `portrait:`, an equipment silhouette — pass
`UploadPinnedNearest`; and **genuinely dual-use art is loaded twice, once
per policy**, because one slot cannot carry two samplers:

| art | scene handle | UI handle |
|---|---|---|
| item `sprite:` | `idTexture` (ground-item quads) | `idIconTexture` (inventory / equipment / container rows) |
| building `sprite:` | `bdTexture` (`Building.Render`) | `bdIconTexture` (`building.listDefs`'s `iconTex`) |
| broken-equipment badge | texture name `broken_equipment` | texture name `broken_equipment_ui` |

**Known live-frame exception:** when a unit has no authored `portrait:`,
the unit-info panel mirrors its current animation through
`unit.getFrameSample`, reusing the atlas handle and cell sub-rect. That UI
fallback therefore follows the player-selected sampler and appears linearly
filtered when the scene setting is linear; it is not a pinned UI copy. This
preserves #1259/#2085's one image, handle, and slot per animation. Supplying
an authored `portrait:` selects the pinned UI path above.

Unit atlases keep their own `LuaLoadAtlasTextureRequest` to preserve the
one-image-per-animation boundary, but its handler selects the global policy.

**The path cache is keyed by `(path, policy)`,** not by path:
`apAssetPaths` is a `Map TextureCacheKey AssetId`. Each policy therefore
owns one reusable canonical slot per path, so a scene→UI→scene→UI
sequence for one file allocates exactly TWO slots and every later request
of either policy is a cache hit aliasing its own. That is the accepted
cost of D-4 for a genuinely dual-use texture. `cacheEntryReusable`
against `btsPinned` remains as the GPU-side consistency check that a
canonical really was registered the way its key claims — it is no longer
what separates the policies. `engine.getLoadedTexturePaths()` collapses
the policy back out and still reports one entry per distinct FILE, which
is what `tools/preview_probe.py` checks against its allowlist.

**One burst carries one policy.** `Engine.Scripting.Lua.Message` extends
a run of ordinary loads only while the declared policy stays the same,
because the batch registers every slot in it with one sampler and its
within-batch same-path dedup folds later requests into an earlier
request's slot. Adjacent runs of different policies are simply
consecutive batches. A PRELOAD must declare the same policy its eventual
consumer declares (`scripts/startup_loader.lua` splits `hudUiPaths` from
`hudScenePaths` for exactly this reason), or it uploads a slot nobody
samples and the consumer uploads the real one anyway.

Cell UVs sit on the LOGICAL cell's own exact edges — one texel inside its
padded slot (#2076) — with no half-texel inset. In nearest mode a fragment
centre lands on exactly the source texel the retired per-frame path drew;
an inset would shift those samples and break pixel identity. In linear mode
the surrounding footprint reaches only the cell's own extrusion gutter, so
isolation moves no logical texel at all — epic #2072's D-3, and the reason
TSR-3 depends on TSR-2 rather than on an inset.

---

## Preview mode: the two viewers and the dump contract

Enforced by `tools/preview_cli_probe.py` (CI-eligible, no boot) and
`tools/preview_probe.py` (manual-only, `needs-gpu`); pure logic by hspec
`--match "Preview.Discovery"` / `"Preview.UnitAnimation"` /
`"Preview.Building"`.

The real-boot probe preserves the windowed GLFW/Vulkan surface, swapchain,
input, and resize paths, but sets `SYNARCHY_PREVIEW_HIDDEN=1` for its engine
children. Presence of that probe-only variable makes preview creation hidden
and non-activating (`fullscreen=false`, `visible=false`, `focused=false`, and
`focus-on-show=false`); an ordinary `--preview` launch remains visible and
focused. This is deliberately not an offscreen conversion: the live-window
resize behavior below is part of the probe's contract.

### Simple-category browser behavior

- **Bare category** (`--preview icons`): a scrollable left-hand list of
  every texture found recursively under the category root, labeled by its
  category-relative path with `/` separators and the file extension
  INCLUDED (e.g. `skill/climbing.png`), sorted lexicographically. The
  first entry auto-selects; its texture renders in the main panel,
  nearest-neighbour scaled, fit to the panel with aspect ratio preserved.
  Click a row to select it; wheel-scroll the list. A resize (the preview
  window is resizable) reflows the panel/list bounds while preserving the
  current selection and scroll offset
  (`previewManager.onFramebufferResize`).
- A label displayed here is ALWAYS a valid item target for the focused
  form — discovery and item resolution apply the identical extension
  rule, so they can never disagree.
- **Focused item** (`--preview icons/skill/climbing.png`): shows only
  that one texture, no list.
- `previewManager.init` forces `engine.setTextureFilter("nearest")`
  live-session-only — never assumed from the default video config, which
  a user's persisted `config/video.local.yaml` can override to
  `"linear"`.

### Units viewer (#887/#1261)

- **Ordering + default selection:** animations sort case-sensitively by
  exact directory name (the same `Ord`-on-the-label rule
  `Engine.Preview.Discovery.sortEntries` uses); `idle` is selected when
  present, else the first entry in that order, direction south.
- **Directions:** the game's own `S, SW, W, NW, N, NE, E, SE` order. A
  directly authored direction ALWAYS wins; W/SW/NW mirror SE/E/NE only
  when flipping is permitted, which is the index's `flip` (proved equal
  to the YAML's before anything is published). A direction the animation
  does not author and may not mirror stays unavailable rather than being
  invented or filled from another unit's textures.
  #1261 retired the no-YAML-entry INFERENCE with the rest of the
  YAML-less path: since #1257 every shipped animation declares its own
  `flip`, and the three trees #1261 promoted declare `flip: true` over the
  canonical five, which is exactly what the inference used to produce.
- A mirrored cell renders genuinely mirrored via `UI.setSpriteFlipX`
  (#887's `ussFlipX`), applied to the CLIPPED UV slice — flipping before
  clipping would sample the wrong slice; #1259 generalized that
  reflection to the sprite's own source sub-rect.
- **Playback:** ONE clock per selected animation. Every direction
  computes its own index from the SAME elapsed value against its OWN
  frame count, so unequal per-direction frame counts (four checked-in
  acolyte animations have them) stay phase-aligned. Selecting a different
  ANIMATION resets the clock; enlarging a different DIRECTION does not.
  End-of-clip REPLAYS (#1833): frame `N-1` is followed, after its own
  normal duration, by frame `0` again, indefinitely — for EVERY clip,
  whatever its authored `loop` says, because the viewer exists to
  inspect an animation and a short `loop: false` clip otherwise looked
  static within half a second. The wrap is in the index computation
  (`Engine.Preview.Unit.frameIndexAt`, which takes the source `loop`
  and deliberately does not read it), never in the clock — nothing
  restarts `animStart` at a cycle boundary, which is what preserves the
  phase across a direction change and a resize. Gameplay is the
  separate `Unit.Render.pickFrame` path and still HOLDS the last frame
  of a non-looping clip. Past the first cycle, directions with unequal
  frame counts wrap at different times and so no longer show the same
  frame ordinal — the same modular behavior a `loop: true` clip already
  had. An effective fps of 0 stays on frame 0. The frame index comes
  from a wall clock, so the script tick rate only affects smoothness.
- **Reflow:** a resize preserves the selected animation, selected
  direction, list scroll offset, AND playback phase.
- **Pre-boot rejection:** an unknown unit, a name with path structure or
  `.`/`..`/absolute traversal, a symlinked unit directory OR symlinked
  `animations/` root, and a unit with no animations all exit 1 before a
  window exists. Both symlink levels matter — `doesDirectoryExist`
  follows links, so a real unit directory with a symlinked `animations/`
  would otherwise browse and load another tree's assets, breaking trimmed
  loading. Since #1261 a missing, animation-less, or uncompiled YAML IS a
  rejection: with no declaration there is nothing to browse
  (`UnitNoAnimations`), and a declaration whose compiled artifacts are
  missing or stale rejects as `UnitAtlasRejected` — the same refusal the
  game makes.

### Buildings viewer (#888)

- **The filesystem is authoritative**, the same split the units viewer
  uses. The building's own folder decides which entries exist and, in an
  animation directory, the numeric `frame_NNN.png` order;
  `data/buildings/<name>.yaml` only AUGMENTS a matched animation with
  `fps`/`loop` and supplies the default-selection hints. A missing,
  malformed, or unmatched YAML never rejects a valid asset folder
  (`dungeon_1` has no YAML at all; `cargo_hold_S`/`furnace` ship a
  `demolish/` folder no YAML mentions).
- **One list, both kinds.** A recognized animation directory is ONE entry
  labeled by its directory name; every other directory is descended into
  so its textures surface as ordinary item-relative statics
  (`dungeon_1/damaged/floor.png`) rather than being played as one clip or
  silently lost. Ordering is the single label-lexicographic rule the rest
  of the browser uses, across both kinds together.
- **A directory is an animation** iff a YAML animation's declared frame
  paths live in it, OR every `.png` in it follows the numbered-frame
  convention (`frame_000.png`, `frame_10.png`, `frame-3.png`).
- **YAML association is by CONTENT, never by equal names.**
  `acolyte_portal.yaml` names its animations
  `portal-appear`/`portal-idle` while the directories are
  `appear/`/`idle/`, so a directory is matched through the frame paths its
  animation declares.
- **Default selection ladder:** `state_animations.built`'s animation
  (resolved that same way — selected label `idle`, not `portal-idle`),
  else the def's own `sprite` when it names a discovered static, else
  `default.png`, else the first entry. `dungeon_1` (no YAML, no
  `default.png`) lands on the last rung.
- **Playback defaults are `fps=8`, `loop=false`** — `BuildingYamlAnim`'s
  own, NOT the units viewer's `loop=true`. One wall clock per selected
  animation, reset on a real selection change but preserved across a
  resize; end-of-clip REPLAYS (#1833) on the units viewer's identical
  terms — every animation entry repeats indefinitely regardless of its
  authored `loop`, the wrap coming from the index rather than a
  restarted `entryStart`. A STATIC selection has no playback at all,
  and forced replay does not change that: `buildingAssetView.update`
  still advances nothing outside `entry.animated`, so a static entry
  keeps exposing no `playback` in the dump.

### Centered bounded zoom (#1907)

Every main preview display — the bare simple-category list's panel,
focused-item mode, the units viewer's ENLARGED direction, the buildings
viewer's static entries and animations, and the flora/structures item
folders that reuse the shared browser — has ONE zoom multiplier per
session. `scripts/ui/preview_zoom.lua` owns the limits and the
arithmetic; every pane fits through its `fitRect`, so no two panes can
drift onto different math.

- **Limits.** `1` is the initial multiplier AND the maximum; `1/8` is
  the minimum. The rendered scale is `multiplier × fit`, where `fit` is
  the aspect-preserving fit-to-region scale. At `1` the complete texture
  fills as much of its region as its aspect ratio permits; at `1/8` both
  rendered dimensions are exactly one eighth of the fitted ones. There is
  no zooming IN past the fit, which is why the complete texture is
  inside its region at every multiplier by construction — never cropped,
  never overlapping the asset list, the direction strip, or the window
  margin.
- **Centered, with no pan state.** Zoom is centered on the region.
  There is no source point, no anchor and no translation: the cursor's
  position within the region cannot affect where the texture lands. This
  is deliberately NOT the gameplay camera's zoom, which is
  source-anchored.
- **The zoom REGION.** In unit mode it is `layout()`'s `enlarged`
  sub-rect (`scripts/ui/unit_animation_view.lua`), never `panelBounds`
  — the panel also holds the direction row. It is both the wheel's
  capture rect and the fit denominator. Every other mode uses
  `panelBounds`. The fit for an atlas-backed unit frame uses the
  compiled index's own CELL dimensions, never the sheet's
  (`frameSize` asks `engine.getTextureSize` only for residency).
- **Input ownership.** The preview region owns an invisible element with
  `UI.setScrollCapture(handle, true)` and nothing else — no
  `UI.setClickable`, no `UI.setPointerBlocking`, because #743 made those
  three policies independent and direction-cell/list-row clicks must
  keep working. The capture is load-bearing for plain/Shift parity:
  `Engine.Input.Thread.Scroll.dispatchScrollEvent` only reaches Lua as
  `onUIScroll` when `routeScroll` finds a capturing surface; with none, a
  plain wheel becomes `LuaScrollEvent` and a Shift wheel
  `LuaZSliceScroll` — two different broadcasts. `previewManager.onUIScroll`
  then dispatches on the ELEMENT HANDLE, so a list element scrolls the
  list and the surface zooms, neither reaching the other even at a
  limit. The `browserId` guard there is scoped to the list-forwarding
  branch only, because focused-item mode never builds a browser.
  The surface reuses a texture handle the session has ALREADY requested
  (at alpha 0), never a fresh load — focused-item mode allows no chrome
  at all (`tools/preview_probe.py`'s `allow_chrome=False`), so
  `list.getChromeTexture()` there would break trimmed loading. It is
  borrowed from the REQUEST, and the surface is installed as each mode's
  UI is built, NOT when the upload completes: an upload is asynchronous,
  so waiting for it would leave list and focused-item mode with no
  capturing surface for the whole load — a window in which a wheel over
  the pane never reaches `onUIScroll` and leaks to the gameplay/z-slice
  broadcasts. A zoom performed during that load is applied to the
  texture when it finally arrives. If the borrowed request then FAILS
  (#1690), only the handle is released — the element is left alone and
  re-pointed at a live handle, because deleting it would take wheel
  capture down with it and `"empty"` is terminal by design. That release
  runs BEFORE `onAssetFailed`'s three "is this failure ours?" tests, not
  after: a request that created the surface and was then ABANDONED (a
  new selection superseded it before it resolved) is none of pending,
  in-view or cached, so a check placed after them never runs and the
  surface stays bound to a dead texture for the rest of the session.
- **Wheel response.** `dy < 0` ENLARGES toward `1` and `dy > 0` SHRINKS
  toward `1/8` — the gameplay convention (`Engine.Loop.Camera`: `dy > 0`
  zooms out, `dy < 0` zooms in, `camZoom` being the viewport
  half-height), NOT the list-scroll convention. The response is
  multiplicative in the delta, so a fractional delta moves less than a
  whole one and an OS that splits one notch into several deltas totals
  the same as one clean delta of that sum — the same decision
  `zoomScrollScale` records for the camera. Both ends clamp EXACTLY, and
  further input at a limit is consumed without changing the scale.
- **Reset follows preview-OBJECT identity, not sprite selection.** A new
  session starts at `1`. In a BARE simple-category browser each texture
  is its own object, so selecting a different one resets to `1`. Within
  `units/<name>`, `buildings/<name>`, `flora/<name>` or
  `structures/<name>` the object is the unit/building/item, so another
  animation, direction, entry, stage or piece PRESERVES the multiplier;
  so do playback, frame changes, and a framebuffer resize (which
  recomputes the fitted size from the new region). Zoom is never
  persisted between sessions.
  The discriminator is `engine.getPreviewTarget()`, not the mode string:
  `mode == "list"` backs BOTH a bare category and a flora/structures
  item folder, and `item` is omitted only for a bare category. No new
  engine field was needed. Mechanically, only a genuine selection fires
  `onSelect` — a resize restores via `assetBrowser.selectEntrySilently`,
  which fires none — so resize preservation falls out of the existing
  restore contract rather than needing its own flag.
- **Not zoomed:** list thumbnails and unit direction-row cells keep
  their existing fixed sizing.
- **Degenerate geometry.** `previewZoom.fitRect` returns no rect at all
  for a missing/non-finite/non-positive box or source size, and callers
  then leave the previous geometry alone and retry — the same thing they
  already did for an unresolved texture size, so a heavily shrunk window
  can never write an inverted, negative or non-finite rect.

Gates: hspec `--match "Preview.Zoom"` (CPU-only; drives the REAL
`preview_zoom`/`preview_manager`/unit/building Lua in a stdlib-only
interpreter, and is the only BLOCKING automated gate this feature has,
since `preview_probe.py` is manual-only `needs-gpu`), plus
`tools/preview_probe.py`'s phase 11, which drives real
`input.moveMouse`/`input.scroll` over dump-reported bounds on all six
display kinds.

### The dump contract

`require("scripts.preview_manager").dump()` (self-registered into
`package.loaded` the same way `unit_ai.lua`/`debug.lua` are, despite
being `engine.loadScript`-loaded, not `require`d) reports `mode`
(`"list"`/`"item"`/`"unit"`/`"building"` — #632's `"placeholder"` is GONE
as of #888, every canonical category now dispatching to real behavior),
`state` (`"loading"`/`"ready"`/`"empty"`), the current `selected` entry,
and in list mode the FULL ordered `entries` list (not just its
`entryCount` — a probe needs the complete list to catch an
omission/substitution anywhere past the visible/selected rows),
`scrollOffset`, and per-visible-row interactive bounds/handles (`rows`,
`scripts/ui/list.lua`'s existing F3 dump contract) — enough to drive real
`input.click`/`input.scroll` against a located row without ever
hardcoding a screen coordinate.

**Unit mode** adds `unit`, the animation `entries` list (each with
`fps`/`loop`/`flip`/`thumb`/`directionCount`, plus #1260's `storage` and
`atlas` path — the WHOLE list, so a probe can prove every animation
selected the atlas, not just the one playing; since #1261 `storage` can
only read `"atlas"`, but it is still DERIVED Lua-side from the atlas path
the engine actually pushed rather than asserted, so a missing one reports
`"legacy"` and fails a probe instead of passing silently), `defaultAnim`,
and `playback` — current `animation`, `direction`, `mirrored`,
`sourceDirection`, `frameIndex`, effective `fps`/`loop`, the same
`storage`/`atlas` pair with the playing frame's `texturePath` and
index-derived `cell`, plus a per-direction `directions` array carrying
each cell's own mirrored flag, source, frame index, sampled
`texturePath`/`uv`, and interactive bounds/handle.

**Building mode** adds `building`, the ordered `entries` list (each with
`kind` `"animation"`/`"static"`, `animated`, `fps`, `loop`,
`frameCount`), `defaultEntry`, `selected`, `scrollOffset`,
per-visible-row `rows` bounds/handles, and — for an animation selection
ONLY — `playback` (`entry`, `frameIndex`, `frameCount`, effective
`fps`/`loop`, `ready`).

**Every mode** additionally carries `zoom` (#1907): `multiplier`, `min`,
`max`, the `region` the wheel is captured over (unit mode's is the
enlarged sub-rect, not `panelBounds`), the capturing `surface`'s element
handle, and the selected `sprite`'s ACTUAL rendered bounds. Those bounds
come from `UI.getElementInfo`, not the module's own arithmetic — the
same engine-is-the-authority rule the direction cells' bounds follow —
so a probe verifies containment and centering against what is really on
screen. The unit and building views report the same block from their own
`dump()`s too, which surfaces as `playback.zoom` wherever `playback`
itself is reported — so a STATIC building entry, which by design exposes
no `playback` at all, still reports its zoom at the top level like every
other mode.

### Trimmed loading

Preview mode loads only its font, the list widget's own chrome textures
(`assets/textures/ui/{highlight,scroll*}.png`, loaded once, list-mode
only), and textures within the requested category/item — never
`data/*.yaml` gameplay catalogs. There are exactly TWO exceptions, both a
single file for the requested item: the units viewer's
`data/units/<name>.yaml` and the buildings viewer's
`data/buildings/<name>.yaml`.

`tools/preview_probe.py` verifies this against
`engine.getLoadedTexturePaths()` — the distinct paths of `Engine.Asset`'s
`apAssetPaths`, populated by `engine.loadTexture`'s own Haskell handler
regardless of Lua caller, so it is the engine's own authoritative
loaded-texture record, not previewManager's self-reported bookkeeping.
The cache is keyed by `(path, policy)` since #2075; this API reports each
FILE once, so a dual-use texture holding two slots still appears once.

---

## UI input routing (#742-#749)

Enforced by hspec `Test.Headless.UI.*` (InputOwnership, Clipping,
PopupPlacement, InteractiveBounds). CLAUDE.md keeps the on-sight digest;
these are the six contracts in full.

**Layers + modal boundary (#742).** Pages live on six `UILayer`s,
painted bottom-to-top `LayerHUD < LayerOverlay < LayerMenu < LayerModal
< LayerTooltip < LayerDebug`; `uiLayerBand` is the single paint-order
source of truth shared by hit-testing and rendering. Whether a page
BLOCKS pointer input is the separate per-page `upInputExclusive` flag —
`LayerModal` defaults exclusive, everything else pass-through. The
topmost visible exclusive page owns the modal boundary: input that misses
every control on or above it is consumed (empty modal space blocks).
Stacking-only modal pages opt out via `UI.setPageInputExclusive(page,
false)`. `LayerDebug` is pass-through above any modal.
`UI.isInputBlocked()` reflects the boundary; `ui_manager.lua`'s
`isGameplayInputActive()` folds it in; Escape's dismiss cascade
(`init_keys.lua`) deliberately runs before that gate. Raw handlers that
iterate widget instances outside `routePointer` use
`UI.isPageInScope(pageHandle)`.

**Per-element input policies (#743).** Three independent policies —
fires a click callback, blocks pointer (`UI.setPointerBlocking`),
captures scroll (`UI.setScrollCapture`); query via
`UI.isPointerBlocking`/`isScrollCapturing`. A click callback still implies
pointer-blocking by default; a blocking element with no relevant callback
consumes the press (`RouteBlocked`) across all three buttons. Wheel
routing (`routeScroll`) picks the topmost in-scope scroll-capturing
surface via the same `topHitBy` paint-order walk — never the click
machinery.

**Scroll dispatch (#744).** Plain and Shift wheel go through the
IDENTICAL pipeline (`Engine.Input.Thread.Scroll`): a capturing element
wins first (`LuaUIScrollEvent`, carrying the Shift flag), else a visible
modal boundary consumes, and only past both does Shift select z-slice vs
camera zoom. Don't reintroduce `UI.isInputBlocked()` self-gates in the
Lua handlers — the engine decides once, upstream.

**Control activation + keyboard focus (#745).** A press on a discrete
control records `UI.ControlActivation.PendingActivation` (firing
`LuaUIPressBeginEvent`); the release re-runs `routePointer` and only
activates if it still resolves to the same element. Interruptions
reverted before release are caught by epochs: global `upmPageEpoch`
(bumped by `hidePage`/`showPage`, each only on a REAL visibility
transition, and — #1748 — by `setPageInputExclusive`, only when the
assignment really changes `upInputExclusive` on a page that is CURRENTLY
VISIBLE: a modal boundary inserted and removed during one press is
route-affecting at page scope via `inputBoundaryPage`/`pagesInScope`,
while exclusivity on a hidden page is invisible to routing, which is
what keeps `popup.init`'s genuine `true → false` opt-out from cancelling
an unrelated in-flight click) + per-element `ueRouteEpoch`
(bumped by `setVisible`/`setClickable` on THAT element, only on a real
value change; by every detach; and — #1694 — by an
`addToPage`/`addChild` that actually CHANGES that element's structural
owner, a fresh or same-owner attachment staying neutral);
`PendingActivation` snapshots the pressed element's and every
ancestor's epoch and cancels on mismatch. Unrelated
sibling/child churn (hover highlights, focus-ring attach) must never
cancel an activation — that constraint shaped this design; don't
"simplify" it back to a global counter. Sliders/scrollbar thumbs opt out
via `UI.setDragActivation`. Keyboard CONTROL focus (`upmControlFocus`,
`UI.FocusNavigation`) is independent of text focus: Tab/Shift+Tab
traverse in-scope focusables (a modal traps traversal like pointers;
`LayerDebug` stays reachable), Enter/Space fire the real
`LuaUIClickEvent`, arrows step `ueSteppable` controls (`LuaUIStepEvent`);
consumed keys are withheld from `inpKeyStates`. `UI.getElementInfo`'s
`focused` stays text-only; control focus reports as `controlFocused`.

**Clipping + popup placement (#747).** `UI.setClipChildren(el, true)`
clips DESCENDANTS to the container's live bounds (overflow:hidden; nested
clips intersect; recomputed fresh, nothing cached).
`UI.Clipping.effectiveClip` is the ONE helper both rendering (`clipQuadUV`
— partial quads, not all-or-nothing culling) and hit-testing
(`UI.Manager.Query.isPointInElement`) consult, so paint and hit-test
can't drift. Floating root-mounted content is unaffected — clipping walks
real ancestors only. `UI.placePopup(anchorX, anchorY, anchorW, anchorH,
contentW, contentH, direction)` (`"below"/"above"/"right"/"left"/
"anchored"`) is the one placement algorithm for floating content (pass
the FULL interactive size incl. scrollbar); `UI.fitVisibleRows` backs
oversized-list row reduction. Tooltips keep their own cursor-relative
clamp.

**Interactive bounds (#749).** Three rects per element — LOGICAL
(`uePosition`+`ueSize`), VISUAL (overflow-expanded render rect), and
INTERACTIVE (what all hit-testing uses,
`UI.InteractiveBounds.interactiveRect`). A box opts its visible border
into interaction via `UI.setInteractiveOverflow`; overflow alone never
enlarges a target. Overflow is clamped: non-finite → 0, astronomically
large → capped, inverting → zero-extent, non-hittable AND non-rendering.
`UI.getElementInfo` adds `interactiveOverflow` + `interactiveBounds`
(`x/y/width/height` stay content bounds).

---

## Container window stack: panes, widget naming, teardown reasons

A world-page panel is reopened after a resize through its own real entry
point — `reopenWithTab` / `reopenWithState` / `restoreStack` — and widgets
that hold raw text (textbox, randbox, dropdown filters) round-trip via
`snapshotPage`/`restoreAll`. Stacking-only modal pages opt out of the
boundary with `UI.setPageInputExclusive(page, false)` (e.g. `popup.lua`
cards); the F8 overlay hit-tests itself through a parallel
`tryClaimClick`.

Enforced by hspec `--match "container window stack"` /
`"Container knowledge"` / `"Nested item contents"` / `"Item list widget"`,
plus `tools/item_list_widget_probe.py` (manual-only, `needs-gpu`).

**The four level kinds.** `endpoint` (a storage building or a unit);
`unitItem` (LIVE, `unit.getItemContents`, which searches loose inventory,
equipment AND accessories — the three the unit-info list merges);
`buildingItem` (the player's REMEMBERED contents,
`building.getRememberedItemContents`, carrying the PARENT record's own
`revealedAt` — never a live storage read, never a knowledge write); and
`escort` (#1250's Mode A pair).

The two item kinds descend by EXACT INSTANCE IDENTITY along a path of
instance ids, and a path that stops resolving closes that level AND every
level below it rather than retargeting a same-def sibling. An
item-container level is RENDER-ONLY (D-5): no transfer endpoint, no
transfer operation — only inspection (scroll, close, open a child), so a
building row keeps its Retrieve gestures and merely GAINS "Contents".
(That sentence named a "Withdraw with <unit>" entry until #1249 retired
it; the row's transfer entries are Mode B's Retrieve 1 / Retrieve all.)

`scripts/item_contents_panel.lua` no longer owns a window lifecycle
(D-13): it supplies the two item-level kinds and nothing else — no page,
no panel, no singleton, no `setup()`, no `update()`.
`scripts/transfer_session_panels.lua` supplies the `escort` kind the same
way and owns no lifecycle either.

**Panes (#1250).** A level owns one or more PANES — a pane being one panel
box, its header and one item list, with its own tab and scroll — and for
every kind but `escort` the level table IS its own single pane
(`panes[1] == level`), so `level.listId`/`activeTab`/`scroll` still mean
exactly what they meant before. A level stays the unit of NESTING,
modality, teardown and restore, which is what makes two flanking panels
ONE level.

**Widget naming is load-bearing.** The stack is transient session UI:
`hud.createUI()` snapshots and restores the WHOLE thing across a resize
(path + per-PANE tab and scroll), and every pane names its widgets from
`paneWidgetName` — the single pane keeps the historic bare `cargo_inv`, a
further pane appends its key — because keyboard control focus is restored
BY NAME to the first visible match, so two panes sharing one name would
return focus to the wrong one. Also, `uiManager.onSaveLoaded` drops it.

**Teardown reasons.** A level teardown carries a REASON, and `"layout"` —
passed only by that resize snapshot/restore pass and by
`view_teardown`'s `resize` hook — is the one that does NOT fire a kind's
`onClose`; every other teardown does. That distinction is what lets an
escort session (and the unit it holds) survive a resize while a zoom-band
change, a HUD hide, Escape, or another container replacing it all end it.

---

## Responsive UI lifecycle (#748/#750)

Enforced by hspec `Test.Headless.UI.ResponsiveMenus` /
`ResponsiveGameplay`. CLAUDE.md keeps the registry split and the
one-line resize rules; these are the numbers and the full rules.

`scripts/ui/responsive.lua` owns the supported envelope — bands
(inclusive): framebuffer height 600-900 @ 0.5-1x UI scale, 901-1200 @
0.75-2x, 1201-1600 @ 1-3x, 1601-2160 @ 1.5-4x; formal minimum 800x600.
`responsive.classify` is introspection only — out-of-envelope
combinations degrade best-effort (never crash, never invalid geometry,
fixed actions stay reachable), typically via `math.max(20, ...)` floors
and `math.min(panelW, fbW)` caps. Menu screens register via
`responsive.register(name, mod)` + `responsive.notifyResize(w, h)`
(0x0-minimize-guarded; re-notify with the SAME size = scale-only change).
Gameplay surfaces stay OFF that registry: they're reached either through
`ui_manager_boot.lua`'s manual forward or the engine's automatic
`broadcastToModules` resize — registering a broadcast-reached module
DOUBLE-FIRES it. Scale-only changes reach gameplay via
`uiManager.notifyGameplayRescale`.

Rules that keep resizes correct — follow them for any new screen/panel:

- A geometry rebuild must preserve state a semantic re-entry may reset:
  pending settings edits, scroll offsets, in-progress text, selected
  tabs, open-panel targets. `hud.createUI()` snapshots each world-page
  panel's "open for" state before the `view_teardown.lua` `"resize"`
  sweep and reopens via each panel's real entry point; restores must not
  re-fire `onChange`/`onSelect` (use the widgets' `silent` params,
  `toggle.restoreSlotIdentity`, `list.setSelectedIndex` — never
  `selectItem`). A surface with NESTING restores the whole nesting path.
- Keyboard control focus survives rebuilds by NAME:
  `responsive.snapshotControlFocusName()`/`restoreControlFocusName()`
  around any destroy+recreate; restore only after pages are re-shown.
- Fixed-size widgets fit via a LOCAL effective uiscale
  (`responsive.fitScale` against the reserved column/row/panel width);
  row labels reserve a `LABEL_COLUMN_FRACTION` 0.35 column. Shrink a
  box's font together with its box, never separately.
- Panels sized as `BASE * uiscale` must cap width/height to the
  framebuffer, and their content must derive from the panel's REAL bounds
  (`panel.getContentBounds()`), never an independently recomputed value
  that can drift. `scripts/ui/reserved_regions.lua` (pure) keeps popups
  clear of toolbar clusters (`hud.getToolbarRects()`, `avoidReserved`,
  `maxAvailableWidth`, `maxRightAnchoredWidth`, `findEscapes`).
- zIndex ACCUMULATES through the parent chain (`elementPaintKey` sums up
  `ueParent`) — leave wrapper/viewport elements at zIndex 0.
- Resize ordering: hud rebuilds first; dependent surfaces (`popup`,
  `unit_info_v2`) expose a separate `reflow()` called after it so they
  never read stale hud geometry.

---

## Lua random streams (#1330)

Enforced by hspec `--match "random stream ownership"`, which pairs
behavioural isolation and per-instance-entropy cases with two source
guards. CLAUDE.md keeps the two rules (no `math.randomseed` under
`scripts/`; non-gameplay code keeps its own stream); this is the story
behind them.

A Lua state has exactly one `math.random` stream, and eleven gameplay
modules draw from it (AI cadence, thoughts, mental state, wildlife,
sleep, water scanning, location rolls). Its entropy is established once
per state by `Lua.openlibs` in
`Engine.Scripting.Lua.Thread.createLuaBackendState`, before
`scripts/init.lua` loads. Reseeding replaces per-state entropy (clock
AND state address) with the caller's choice, and two engines launched in
the same second then share one simulation. `scripts/ui/randbox.lua` did
exactly that, and also spent eight gameplay draws per suggested world
seed, so clicking randomize shifted every later simulation decision.
`scripts/ui/random.lua` (SplitMix64, seeded from the same time+address
recipe Lua's own auto-seed uses) is the UI widget kit's own stream.

---

## World identity and language provenance (#707/#1092/#1101)

`world.init(pageId, seed, worldSize, plateCount [, displayName[, gloss[,
languageSeed[, languageVersion]]]])`. The optional identity (#707) is
display text, immutable per page, persisted in saves, independent of
pageId and save-slot name; `world.getIdentity(pageId)` reads it;
`engine.listSaves()` exposes `worldName`/`worldGloss`.

A name supplied with no languageSeed is a CUSTOM name and has NO
language provenance (#1092) — `world.getLanguageProvenance(pageId)`
returns nil for it, and `{ seed = "<decimal string>", version = N }`
only for an identity built through the generated-name path (the seed is
a STRING: a Word64 has no lossless Lua number). `languageSeed` (#1101)
is that path: it states that displayName/gloss were RENDERED from that
language, and is what makes the page's placed locations named in the
same one. It is a decimal string; `languageVersion` defaults to the
current generator. Provenance is never inferred: with no displayName
there is no identity to attach it to, and a malformed seed or an
unconstructible version is refused with a warning, leaving an ordinary
custom name.

---

## Location and river naming (#1101/#1102)

Enforced by hspec `--match "Location naming"` / `"River naming"` /
`"River identity"`; `tools/river_naming_probe.py`,
`tools/location_content_probe.py`. CLAUDE.md keeps the write-once rule,
the no-invented-language rule and the checked-identity rule; this is
the rest.

A LOCATION's concept pools are DATA (`ldNaming`'s ordered, nonempty
`heads`/`modifiers`, validated against `data/language/concepts.yaml` at
load — an unknown id rejects the whole file rather than degrading to
`ldLabel`); the engine has no `ldType`→concept mapping. RIVERS have no
definition file, so their pools are in code (`riverHeadConcepts`:
`RIVER`, `FORD`, `CROSSING`, `BAY`, `VALE`, `HOLLOW` — a NARROW head
pool against a WIDE modifier pool of every catalogue concept with a
modifier form, which is what makes a head morpheme recur across a map
and in the world's own name). The expression is always `Modifier
modifier head`, chosen deterministically from the entity's own stable id
plus the language seed/version, never from hashmap order.

River identity is `(WorldPageId, GeoFeatureId)`, reusing the id the
timeline already allocated. `World.River.Identity` is the ONE place
events are paired with features, and the pairing is CHECKED against
source/mouth/flow before it is trusted — a violated invariant yields no
id rather than a wrong one. Names live in a per-page `wgpRiverNames`
keyed by `GeoFeatureId`, deliberately NOT on `PersistentFeature` (whose
`GeoTimeline` is positionally serialized worldgen OUTPUT).
`world.getRiverAt` is the minimal selected-segment→identity resolution.

UI: `scripts/etymology_panel.lua` is the ONE panel all three entry
points open, hosted by `scripts/name_plate.lua` on `hud.global_page`
(NOT `world_page` — a plate on a band-swapped page is unhittable in the
zoom map). `Language.Suggest` (#1106) is the one remaining copy of the
profile+roots+catalogue resolution — fold it in rather than adding a
fourth.

---

## Name etymology: internals (#1104)

The chosen expression is deterministic from the instance's own stable
`liId` (plus the language seed/version and the def id). Growing the
catalogue never re-renders a stored name even though `assignLanguageRoots`
re-resolves collisions over the whole concept set. River event/feature
pairing walks `gtFeatures` order.

Enforced by hspec `--match "Language etymology"` / `"Etymology panel"`
and `tools/etymology_probe.py` (manual-only, `needs-gpu`).

What makes decomposition possible is a small optional `EtymologySource`
(the originating `NameExpr` plus the `LanguageProvenance` that rendered
it) persisted beside the name on all three carriers: `wiEtymology`,
`liEtymology`, `rvnEtymology`. A precomputed morpheme list is
deliberately NOT stored — the presentation is reconstructed on query.

`Language.Generated.Render` produces an ordered token TRACE and
`renderNative` IS its concatenation, so "concatenating the trace
reproduces the stored name" holds by construction;
`Language.Generated.Boundary.joinMorphemesTrace` is the one
implementation both views of a boundary share.

`Language.Etymology` re-renders from the source and CHECKS the result
against the authoritative stored text before showing any of it — a
mismatch (a tampered name, a source from another language, a historical
version this build renders differently) reports unavailable rather than
explaining the wrong word.

Morpheme identity is `(LanguageProvenance, ConceptId)` — never spelling —
so #1096's bound form and its free root are ONE morpheme while two
languages' homographs, and the SAME seed under two generator versions,
are not. Capitalization is a surface-POSITION effect: the leading token
carries it, every canonical free spelling stays the unmarked lowercase
root.

A source is additionally required to belong to the PAGE's own recorded
language (`decomposeEntityName`): the surface check proves an expression
renders to the stored text under ITS OWN language, so a stale or foreign
source that happens to reproduce those letters would otherwise pass while
attributing every morpheme — and every recurrence link — to a language
the world does not have. A page with no provenance admits no source at
all.

`world.getEtymology(kind[, id][, pageId])` feeds world/location/river
adapters into that one path; an unavailable reply still carries the
stored name so the UI can keep showing it.

### Recurrence, and why self-exclusion is page-qualified

Recurrence is computed on demand from the ACTIVE page — current world +
`LifecycleDiscovered`-or-later locations + ONLY the river being inspected
(a world or location target admits no river at all), the inspected entity
excluded from its own links, entries exposing nothing but an entity kind
and an already-visible name. There is no session history.

The optional `pageId` names the TARGET only (#1265) and never widens that
set: omitted, target and recurrence are both `resolveActiveWorld`'s page;
a live INACTIVE page resolves the target there — its stored name, gloss,
source and page-language validation all that page's — while candidates
still come only from the active page, so no inactive name is ever a
recurrence entry; a page that does not exist is the unchanged
`available=false`/`no_entity`.

With no visible page, recurrence follows `resolveActiveWorld` exactly,
head-of-`wmWorlds` fallback included, and substitutes nothing when that
resolves to `Nothing` — a missing ingredient on the RECURRENCE page (no
active page, no gen params) leaves an explicitly selected target's result
intact with recurrence empty, never downgrading it.

That crossing is what makes self-exclusion PAGE-QUALIFIED: every page's
world entry is `("world", Nothing)` and location ids are page-local, so
comparing kind and id alone would silently drop the active page's own
world name, or an equal-numbered active location, from an inactive
target's links. A river target on another page admits no river at all —
the inspected river is not on the active page, and its `GeoFeatureId`
re-resolved there is a different river.

### The suggestion chain

The expression travels the whole Create World chain —
`world.suggestName`'s `expr` → `name_suggest` → `generation` →
`world_view` → `world_manager` → `world.init`'s 9th argument — and is
cleared with the gloss and provenance the moment the player edits the
name.

### Persistence

`world-pages` v9 (v8 frozen by #917 as
`PageCoreDTOv8`/`WorldGenParamsDTOv7`/`LocationInstancesDTOv5`/
`LocationInstanceDTOv5`/`LocationEncounterDTOv1`; v7 frozen by #916 as
`PageCoreDTOv7`/`WorldGenParamsDTOv6`/`LocationInstancesDTOv4`/
`LocationInstanceDTOv4`; v6 frozen by #1230 as
`PageCoreDTOv6`/`WorldGenParamsDTOv5`/`LocationInstancesDTOv3`), with
`PageCoreDTOv5`/`WorldGenParamsDTOv4`/`WorldIdentityDTOv2`/
`LocationInstanceDTOv2`/`RiverNameDTOv1` frozen — every historical shape
decodes with the source ABSENT, never inferred. #917 changed nothing
about etymology itself: v8 is a frozen migration boundary that carries
each stored source across untouched.

---

## Location instances (#911)

Enforced by hspec `--match "Location instance identity"` and
`tools/location_content_probe.py`. CLAUDE.md keeps the
placement-time-id, read-the-stored-values and one-way-lifecycle rules;
this is the rest.

An instance stores definition id, anchor, resolved absolute bounds,
display name + optional gloss, a one-time content-spawn flag, and
lifecycle `unknown → hinted → discovered → active → cleared → depleted`.
`wgpLocationStamped` stays chunk-keyed (#424). `hinted` is deliberately
unreachable but must NOT be deleted (the enum is positionally serialized
and append-only). #916's ruin encounters are the first runtime owner of
`active` and `cleared`: first autonomous aggression activates an encounter
(without revealing an unknown location), while first sight exposes an already
activated ruin as `active`. Since #917 `cleared` is no longer the
encounter's to grant on its own — see §Guaranteed significant contents
below: it is the conjunction of every condition the location authors,
and `leCleared` records ENCOUNTER completion alone.

A generated `ruin_small` also stores its one-time uniform 0–3 occupant
roll. Once content spawning completes, its exact nomad roster is durable:
each entry carries the unit id, distinct home tile, and guard-policy state. A zero
roll starts with its ENCOUNTER half complete, and (since #917) still
waits on the location's significant items before it can clear at all;
either way it remains undiscovered until sight. A positive roster
clears only when every originally assigned unit is exactly dead; collapsed,
crawling, absent, or disengaged occupants keep it uncleared. Missing ids
remain in the roster, while an occupant resolved on another page is a hard
load-integrity error. Hand-stamped locations without a placed instance do
not acquire an encounter.

Queries: `world.listPlacedLocations([pageId])` (extended, not
repurposed — `id` is still the DEFINITION id), `getLocationInstance`,
`setLocationLifecycle`, `markLocationContentsSpawnedById`
(`instance_id`/`lifecycle`/`name`/`contents_spawned` are instance
fields, and `encounter` exposes the roll, roster-complete/death-only/
cleared policy, activation/current-episode/feedback state, and per-occupant
state). Encounter spawning registers the exact roster through
`world.registerLocationEncounterOccupants`, one successful prefix at a time;
an interrupted retry preserves that prefix and allocates only its missing
slots. Guard AI updates persisted
engagement/return state through `world.setLocationEncounterOccupantState` and
the encounter-wide, once-per-episode notification state through
`world.setLocationEncounterEpisodeState`. The coordinate-addressed
`hasSpawnedLocationContents`/`markLocationContentsSpawned` remain
compatibility wrappers resolving to the chunk's first instance.

Persistence: `world-pages` v9, with v8's pre-significant-contents
location record frozen as `LocationInstanceDTOv5` (its encounter, still
carrying the clearance-notice flag, as `LocationEncounterDTOv1`) and
v7's pre-encounter one as `LocationInstanceDTOv4`. Each migration adds
NOTHING the payload did not carry — `migrateWorldPagesV8` gains no
significant obligations and `migrateWorldPagesV7` no encounter — rather
than letting current content reinterpret a materialized world; #917's
own §Guaranteed significant contents has the detail, including where
the notice moves to. The frozen v1 DTO's per-chunk flags still decode
PENDING and resolve against the registry at the load path's
content-validation stage (`resolveLegacyLocations`).

---

## Guaranteed significant contents and compound clearance (#917)

Enforced by hspec `--match "Location significant contents"` (pure) and
`--match "compound clearance with significant contents"` (the real
discovery tick and the real ground boundary), plus
`tools/location_content_probe.py` and `tools/expedition_loop_probe.py`.
CLAUDE.md keeps the headline rules; this is the mechanism.

**The predicate.** A location clears when EVERY condition it actually
authors is satisfied, and it authors at most two: an encounter (#916)
and a set of guaranteed significant items. `locationClearanceSatisfied`
is the conjunction over the conditions present —
`locationEncounterCondition` and `locationSignificantCondition` each
answer `Maybe Bool`, `Nothing` meaning "not authored". A location
authoring ONE clears on that one. A location authoring NEITHER never
clears: the empty conjunction is deliberately `False`, not the vacuous
`True`, which is what keeps every pre-#917 location — and every
historical save's — behaving exactly as it did.

**Where the two halves live, and why they are separate.**
`markLocationEncounterCleared` records encounter completion and nothing
else: no lifecycle move, no event. So `leCleared` may be true while the
location is uncleared, which is the whole point — a ruin with its nomads
down and its reward still on the floor is not finished with.
`resolveLocationClearance` is the SINGLE writer of the cleared
transition and of the one player-facing notice, and it is called from
both places a condition can land: the clearance pass in
`World.Thread.Discovery` (which polls, because the item latch is set on
the Lua thread and has no edge of its own) and the discovery edge (for a
location completed while it was still unknown). Whichever conjunct lands
last promotes exactly once.

**The notice is on the INSTANCE.** `liClearEventEmitted` generalizes
#916's per-encounter `leClearEventEmitted` so a location authoring
significant items and no encounter has one too. It starts SPENT exactly
when the instance is born already clearance-satisfied — a zero-roll
encounter owing no items, i.e. #916's own `rolled == 0` rule — because
nobody cleared such a place and discovering it must not say otherwise.
A hidden completion stays private: `resolveLocationClearance` requires
`isDiscoveredLifecycle`, so it defers until sight and then fires once.

**Authoring.** `significant: true` is legal ONLY on a fixed
`kind: item` content entry; `Engine.Asset.YamlLocations` rejects it on
any other kind, which is what keeps a `loot_table` draw out of the
predicate whatever it rolls. Its item id must also RESOLVE against the
live item registry, checked by
`Engine.Asset.YamlLocations.significantItemErrors` and enforced by the
API loader, which rejects the whole file — the same all-or-nothing
outcome a bad naming scheme earns. That is deliberately stricter than an
ordinary content id, which may warn and be skipped at spawn time (#90):
an incidental entry that spawns nothing costs the location some salvage,
while a significant one that spawns nothing costs it its clearance
forever, because the obligation is created at placement and
`item.spawnGround` then fails on every chunk load.

The LOAD path holds the same line from the other side
(`World.Save.Types.missingSignificantItemReferences`, folded into
`engine.loadSave`'s content-validation ladder): a save whose UNSPAWNED
obligation names an item definition this build no longer registers is
refused before anything publishes, because that obligation is exactly
what the next chunk load would try to spawn. A BOUND obligation is
exempt — nothing re-spawns a filled slot, so its def name is a
historical record and the item may legitimately have been consumed or
destroyed.

That makes an ORDERING requirement load-bearing: items must be
registered before `engine.loadLocationYaml` runs, or the shipped ruin is
rejected and no location registers at all.
`scripts/startup_loader.lua` already does this in both profiles and
`data/locations/*.yaml`'s own header states it; anything else that loads
location YAML directly — a probe, a fixture harness — owes the same
order. `data/locations/ruin_small.yaml` authors
one `processing_unit` — appended AFTER the existing contents, because
#948 keys each incidental draw on the entry's positional index, so
reordering those lines would silently change what every
already-generated ruin rolls. It is deliberately not `radio` (D-6).

**Cardinality is fixed at PLACEMENT.** `significantItemsFromDef` builds
the whole obligation list when the instance is created, one slot per
authored item per `count`, with no item bound yet. That is what stops an
empty collection reading as satisfied before `scripts/locations.lua` has
spawned anything, and what makes an unspawned or failed-to-spawn item
keep the condition incomplete rather than silently vanish.

**Provenance is the PHYSICAL item.** `lsiInstanceId` holds
`Item.Types.iiInstanceId`, never a page-local ground id: the physical id
survives pickup, transfer, storage and drop, while `spawnGroundItem`
hands out a NEW ground id every time an item is dropped or a failed
pickup is rolled back. `registerLocationSignificantSpawn` is WRITE-ONCE
per slot — a retried content spawn cannot repoint an obligation and
orphan the item it first named, and the refusal is exactly the edge a
resuming spawn uses to tell "still owed" from "already done". It also
refuses an item that is not the DEFINITION the slot names — an
obligation says what is owed, so binding a ration to a
`processing_unit` slot would otherwise let picking the ration up latch
the slot and clear the location with the guaranteed item still on the
floor — and an item ALREADY owed by any obligation on the page, because
`latchLocationSignificantTaken` latches every entry naming that id, so
one physical item bound twice would let a single pickup discharge two
required items. Both refusals live at the registration boundary rather
than only in the validators: the verb is public Lua, and the decode and
save rules reject the duplicate state only once it is already on disk. `significantProvenanceErrors` holds the same line at the save
boundary, for an untaken obligation whose item is on the right ground
but is the wrong thing.

Lua hands that verb the GROUND id `item.spawnGround` returned, and the
verb resolves it to the physical id AND commits the binding
SYNCHRONOUSLY, on the calling thread — unlike every sibling location
editor, which queues to the world thread. That is load-bearing rather
than a shortcut: every ground pickup runs on that same thread
(`pickupGroundOnPage` is reached only from `item.pickupGround`) and its
latch matches on the obligation's BOUND id, so a queued binding would
leave a window in which the item is already pickable with its slot
unbound. A pickup landing there latches nothing, the binding then names
an item already in an inventory, no second ground pickup can happen,
`contents_spawned` blocks a respawn, and the location is permanently
unclearable — with a save `significantProvenanceErrors` would then
refuse, an untaken obligation resolving inside an inventory. Committing
on the calling thread puts the spawn, the binding and any pickup in one
serial order, so the window does not exist.

`item.spawnGround` answers exactly ONE value and must keep doing so: the
debug console serializes every return value tab-separated, so a second
one would turn `return item.spawnGround(...)` — which several probes
parse as a bare number — into `"0\t14"`.

**A latch alone is not enough.** `significantRecovered` counts an
obligation as discharged only when it names a spawned item AND that item
was taken. No engine path can produce the other shape —
`latchLocationSignificantTaken` matches on a bound id — but it is
precisely the shape the session provenance rules below cannot see, since
there is no id for them to resolve, so a corrupt payload would otherwise
clear a location with nothing ever spawned.
`locationSignificantItemErrors` rejects it at decode as well.

**The latch.** `taken` is set by
`Engine.Scripting.Lua.API.Items.Ground.pickupGroundOnPage`, the
authoritative ground→inventory boundary, on the first SUCCESSFUL insert
— never on the rollback — by ANY unit of ANY faction. Nothing anywhere
writes it back to false: dropping, transferring, losing, consuming or
destroying the item afterwards changes nothing, because the location was
looted and that does not become untrue.

**Spawning.** `scripts/locations.lua`'s `spawnSignificantContent` fills
only the slots still empty, registering each item the instant it spawns,
and the ordinary content loop skips significant entries (they have their
own pass, exactly like the ranged roster). If ANY obligation cannot be
filled the whole spawn returns WITHOUT marking `contents_spawned`, so
the next chunk load retries — warning and skipping would burn the
location's exactly-once content lifecycle on a location that could then
never be cleared. A hand-stamped location has no `LocationInstanceId`,
so it owes nothing and its incidental contents are unaffected.

**Persistence.** `world-pages` v9. `migrateWorldPagesV8` preserves every
stored value, lifts the encounter's clearance-notice flag onto the
instance, and adds NO obligations — reading them off today's YAML would
owe a materialized world an item it never spawned, permanently blocking
a clearance the pre-#917 build had already granted. The v1
reconstruction discards both for the same reason.
`Location.Instance.locationSignificantItemErrors` rejects a slot below
1 (unbindable — the registration boundary refuses a non-positive slot,
so the content spawn would orphan an item on every load for ever), a
bound item id of 0 (the never-minted "no id given" sentinel; because
the provenance rules skip a TAKEN obligation by design, it is the one
value that would otherwise satisfy clearance with nothing ever spawned
— `significantRecovered` refuses to count it either way), a
duplicated slot, a CONTENTS-SPAWNED instance still owing an unbound slot
(unrecoverable — `spawnContents` returns at its one-time
`hasSpawnedLocationContents` gate and never fills it, and neither the
missing-definition check nor the provenance rules can see the shape), an
obligation marked taken that names no item, and same-page duplicate
ownership at component decode;
`World.Save.Integrity.significantProvenanceErrors` hard-fails an UNTAKEN
obligation whose item resolves on another page, in an inventory or
storage (it cannot be held without having been picked up), or only
NESTED inside a ground container; one whose ground item is the wrong
DEFINITION; and one physical id owed by two obligations. Meanwhile
`significantDanglingWarnings` reports an absent item and tolerates it,
leaving the obligation untaken. Once taken there is no rule at all.

The ground set it resolves against is each ground entry's OUTER item
only, never recursed through `iiContents`, and that is load-bearing:
`pickupGroundOnPage` removes a ground-map entry and latches the OUTER
item, so an id reachable only from inside a container is not pickable
as its own ground item and could never discharge its obligation —
accepting it would pass a save that is permanently unclearable. The
container ITSELF is a perfectly good obligation item; a top-level
ground entry is pickable whatever it holds. `peItems` still flattens,
because the question it answers — does this id exist anywhere on the
page — is a different one.

**Queries.** `world.listPlacedLocations` / `world.getLocationInstance`
expose `significant` (always an array; `{slot, item, taken}` plus
`item_instance_id` once bound — OMITTED before that, which is how
"not spawned yet" is expressed) beside `authors_clearance`,
`clearance_satisfied` and `clear_event_emitted`. The predicate is
REPORTED rather than left for callers to re-derive, because a second
implementation is what would drift.

## Location discovery, map icons, and per-unit knowledge (#780/#781/#915)

Enforced by `tools/location_content_probe.py`,
`tools/location_embark_probe.py`,
`tools/location_map_icon_asset_check.py`; hspec
`--match "Location discovery"` / `"Location map icons"` /
`"Unit.LineOfSight"` / `"unit location knowledge"`. Detail behind
CLAUDE.md's entries:

**Discovery (#780, sight-based since #1230).** Sight is
`Unit.LineOfSight.visibleTilesOnPage` — the SAME calculation
`unit.getVisibleTiles` runs (perception radius scaled by the page-local
`nightPerceptionFactor`, 120° facing cone, terrain-Z occlusion) minus
that query's `wmVisible` gate, which keeps reveal working on a
loaded-but-hidden page while `unitVisibleTiles` still reports `[]`
there. Terrain, clock and world size come from the RESOLVED page's own
refs, never `activeWorldSizeChunks`. The two known distance-sensitive
consumers that must pin the clock (a night-scaled radius is
intentionally shorter): `scripts/unit_ai_water.lua`'s `scanForWater` and
`tools/tutorial_probe.py`'s `sees_water`.

**Map icons (#781/#1230).** The shared unknown icon is registered once
under `locationUnknownIconTextureName`, independently of every
definition; `cleared`/`depleted` darken RGB via `clearedIconTint` with
the zoom-fade alpha preserved exactly in all six lifecycle cases. The
dark tint is an explicit, enumerated exception to the no-tinting rule
(`docs/expedition_gameplay_loop.md` D-16), confined to the icon quad's
own `Vec4`.

**Per-unit knowledge (#915).** Global lifecycle = "the player has
mapped it"; `aiState[uid].knownLocations` = "this acolyte knows where it
is". Both layers come from ONE containment enumeration in
`Location.Discovery` (`findDiscoveries`/`findAwareness`), so they cannot
drift; awareness additionally reports EVERY qualifying unit and ignores
lifecycle, so a unit arriving at an already-mapped ruin still learns it.
`world.getLocationAwareness()` walks every loaded page;
`scripts/unit_ai.lua` ingests it BEFORE its pause guard. A memory whose
`(page, id)` no longer resolves is a non-blocking diagnostic, scrubbed
at reconcile. Radio sharing/range deliberately deferred.

---

## Tile-coordinate seam frame (#1175/#1230)

Enforced by hspec `--match "World.Render.PickSeam"` /
`"World.DesignationSeam"` / `"a seam-frame unit"`. The contract is also
stated in full on `World.Render.HitTest`. CLAUDE.md keeps the
canonical-coords rule, the rectangle exception and the lookup-wrap rule;
this is the full enumeration.

Chunks are STORED u-wrapped, so one physical tile has two names near
the seam. Picking (`pickWorldTile` and every Lua caller it backs —
`world.pickTile`/`pickPos`/`getHoverTile`/`getHoverPos`), designation
maps, and every point read / mutation / cancellation — including the
verbs a worker FINISHES a job with (`world.getDigInfoAt`/`digTile`,
`harvestFlora`, `setVegAt`, `plantCropAt`/`plantRowCropAt`,
`structure.place`/`hasAt`/`floorZAt`/`clear`, and
`building.spawn`/`canPlaceAt`, whose footprint walk resolves each tile)
— use CANONICAL coords and accept any alias, so pre-#1175 saved job
coords need no migration.

RECTANGLES are the exception: canonical is a STORAGE frame, not a
geometry one, so a drag's second endpoint is re-expressed in the
anchor's local alias frame (`localizeTileToAnchor`, shared by
`World.Thread.Command.Cursor.Common.designateRect` and the `CursorQuads`
previews; Lua `world.localizeTile` for `build_tool.lua`'s wire snap /
occupancy scan) BEFORE any clamp/`min`/`max`, canonicalising per
enumerated tile at lookup/storage only. Job-SELECTION ranges need that
frame too — `construction.getPendingJobs` reports `lx`/`ly` beside
canonical `x`/`y`, and `unit_ai_construct.lua` measures with those.
Canonicalising one end alone MEASURED worse than seam-blind behaviour;
don't.

Terrain LOOKUPS take the same frame: `World.Tile.Types.lookupChunk`
wraps nothing, so any consumer must `wrapChunkCoordU` first —
`Unit.LineOfSight.tileTerrainZ` now does — a miss reads as "not loaded
→ assume flat", which for occlusion means "nothing blocks". The
chunk-init queue is wrapped at the drain. Where a tile is DRAWN is the
separate `bestWrapOffset` axis (#1176). Away from the seam, and in
arenas, every step is the identity. `world-activity` v1/v2 payloads are
re-keyed on load.

---

## Position hold (#1216)

Enforced by hspec `--match "position hold"` and
`tools/position_hold_probe.py` (manual-only). CLAUDE.md keeps the
trade-off statement, the one-constant rule and the create/clear summary;
this is the full mechanism.

`scripts/unit_ai_hold.lua`'s `hold_position` (anchored on
`s.holdAnchor`) scores EXACTLY `unit_ai_combat.lua`'s
`FOLLOW_COMMAND_UTILITY`, so the #306 ladder is reused rather than
restated — every interrupt that could preempt the order (dire self
survival, combat, treatment, a mental break) still preempts the hold and
the unit walks BACK to its anchor afterwards, and everything the order
outranked (wander, work entry and its in-progress locks, situational
goals) still loses. Don't add a second constant.

Only an ARRIVAL creates a hold — a `TASK_TIMEOUT_SEC` stall creates
none — and only a PLAYER-intent move does: `commandMove(uid, x, y,
speed, internal)`'s `internal` flag is what keeps
`scripts/building_spawn.lua`'s portal walk-out from pinning a fresh
acolyte. Only an ACCEPTED, EXPLICIT player command clears one
(`commandMove`, a COMMITTED `commandAttack`, an accepted
`commandPickup`/`commandTransferOrder`, a Mode A session, or
`unitAi.releaseHold`) — a refused pickup and the AI's own emergent
engage leave it standing. The walk home is charged against the same
eligible-time stall budget the order was (§Commanded-order stall
budget), so an unreachable anchor expires instead of re-pathing forever.
Persisted via `lua.unit_ai` v6; v1-v5 decode as not-holding, never
inferred.

---

## Player transfers: the three player-facing modes

Design authority for the *decisions* is
`docs/unified_item_transfers.md`; this is the as-built behavior. The pure
policy itself (`src/Unit/Transfer.hs`) and the lax-AI-verb rule stay in
CLAUDE.md, because routing AI work through the strict path is the mistake
that has to be prevented on sight.

Enforced by hspec `--match "Unit transfer"` / `"Transfer context menu"` /
`"durable transfer orders survive"`, plus
`tools/transfer_order_probe.py` and `tools/item_list_widget_probe.py`
(both manual-only; the latter owns the real-AI behavioural proof that a
MOVING target is preempted and then stays put for the whole approach,
which no fixture that ticks no simulation can state).

The durable ORDER store is #1246's per-page `wsTransferOrdersRef`.
Outcome vocabulary is deliberately small: a stall is `out_of_range`, an
arrival refusal is `became_stale` carrying the real cause, a worn item is
refused as `item_not_transferable`, and an escort source that never
registered the action is refused as `source_not_escortable`. Only
`ready_to_commit` entries are ever submitted. `unit.cancelTransferOrder`
takes pending entries only (via `cancelBatch`); escort/hold eligibility
stays `isPlayerCommandable` of the live faction, never a def allowlist.

- **Durable orders (#1246/#1247/#1253).** `createTransferOrder`
  validates with adjacency DEFERRED (`ReachPolicy`; same page still
  required); `checkTransfer`/`commitTransfer` still require it.
  `unit_ai_transfer.lua` walks the ACTING unit under a 7.5 lock, and
  ARRIVAL IS THE COMMIT (`unit.commitTransferOrder` re-validates
  atomically) — a refusal there is `became_stale` carrying
  the real cause, and a create-time refusal is never retried. The 60 s
  timer is a STALL timer over ELIGIBLE time, reset on every new closest
  approach — never a trip budget. Every way an order ENDS is one rule
  (`unit_ai_transfer_outcome.lua`): surface once via `unit_warning`,
  then PRUNE unconditionally, so nothing terminal rides a save and
  handling stays edge-triggered and idempotent. `cancelTransferOrder`
  (pending only) + `pruneTransferOrder` (terminal only,
  ownership-scoped, idempotent); the player's way in is **"Cancel
  transfer"** on the unit's context menu, omitted (never disabled) when
  it carries no live order. A CARRIER ceasing to act is the one exit
  the executor can't reach, so `retireTransferOrdersEverywhere` drops
  orders engine-side from BOTH destroy and kill — death is easier to
  miss than destruction, since the instance remains and every reference
  still resolves. Collapsed/crawling are excluded (merely suspended). A
  commit result reports EVERY requested item, so the arrival report
  excludes what the command-time gate already surfaced (`settledIds`).
- **Mode B — queued gestures (#1249, `transfer_gestures.lua`, ONE
  builder both hosts call).** **Store 1 / Store all** from a unit-info
  row into the open container window's ACTIVE level; **Retrieve 1 /
  Retrieve all** from a container row into the unit
  `transfer_session.resolveSource` picks. NEITHER requires adjacency —
  that is the whole promotion. Granularity is 1-and-all only, and "all"
  is every instance id the merged row stands for
  (`itemList.rowInstanceIds`, signed into the rebuild identity), never
  a count. A gesture is OMITTED, never disabled, whenever it could not
  run: no window, no eligible source, an equipped/accessory item, a
  self-transfer, or an ACTIVE level that is an item container
  (render-only — never fall back to a transfer-capable ancestor) or an
  escort pair.
- **Mode A — escort (#1250/#1251, `transfer_session.lua`).** Walk
  FIRST, then choose items. `unit_ai_escort.lua`'s `escort_transfer` (a
  7.5 lock, peer of the queued order) walks the source to the
  destination's FOOTPRINT and stops. An eligible SOURCE is one whose
  species actually registered that action (`unit_ai_actions.lua` records
  every species' action names); an EMPTY action inventory means no AI is
  loaded and answers yes to everything, never a refusal invented from
  absence. The two 440-wide panes are fitted as a PAIR
  (`responsive.fitScale`, a level kind's `paneScale`, against
  `reserved_regions.maxAvailableWidth`)
  then placed as ONE rect that is split — both halves matter at the
  800x600 minimum. The one-way transition to open/held fires EXACTLY
  ONCE and does everything else: `building.refreshContainerKnowledge`
  (its only caller in the game), opening the panes, and the camera snap
  — each reading LIVE endpoint positions, never the creation-time
  snapshot. Rows commit IMMEDIATELY through `checkTransfer` then
  `commitTransfer`, the COMMIT authoritative: drift out of reach is
  refused with the contract's own proximity reason and the session
  stays open. The hold is released BY the session ending, and that
  release STOPS the unit rather than merely letting go. A UNIT
  destination is held too (#1251) — unit-to-unit is the one pairing
  where BOTH ends can walk away. The session's `roleOf` is the one
  answer both actions consult: `"source"` walks then stands, `"target"`
  (`escort_hold`) stands from CREATION, both scoring 7.5 so neither end
  outscores the other. Being a source is a per-species capability;
  being a target is player-commandability and nothing else, so
  `escort_hold` is auto-prepended to EVERY species by
  `registerActions`. Every teardown path is the same coupled,
  idempotent one, extended to the pair; only a resize is exempt — the
  full trigger list is the next subsection.

### Mode A session failures (#1254, UIT-5B)

Every way a session can be interrupted ends it through that ONE coupled
teardown, and the module's job is to NOTICE each of them. The noticing
splits by phase: while the pair is open the container window's own
per-tick `stillThere` hook closes the level (and with it the session) on
an endpoint that vanished, but a session spends its whole APPROACH with
no window at all, so `transfer_session.update` — a real 0.2 s script
tick, the cadence the container window already runs at — is the
canonical liveness check and covers BOTH phases. Its rule is
`staleReason`: either endpoint gone, the contract's own `eligible` gone
(a demolished building, a unit that left the player's factions), or a
UNIT endpoint whose pose is `dead` or `collapsed`. That last one cannot
come from the contract — `Unit.Transfer.endpointEligible` is
`uevCommandable` alone, so a corpse is a perfectly eligible endpoint by
its lights — so it is tested here rather than widened there, and the
RECOVERABLE poses (crawling, sleeping) are deliberately excluded: a
session sits those out.

A **new player order to a held unit** ends the session and then
proceeds (signed off 2026-08-11 — player intent wins), through the one
shared `notePlayerOrder` boundary called from the player's own ingress
sites and NOWHERE else (`init_mouse_entity.lua`'s right-click move
order, `init_context_menu.lua`'s Attack / Pick up / Move here) — never
from inside `unitAi.commandMove`/`commandAttack`/`commandPickup`, which
`building_spawn.lua` and `unit_ai_combat.lua` also call for scripted and
autonomous behaviour, and never from the escort's own approach. It runs
BEFORE the command, since the teardown stops every unit it held.

A zoom-band change or a HUD hide reaches the session through
`scripts/ui/view_teardown.lua` (#156) rather than a one-off call —
which is what covers the approach, where the container window's own
entry has no window to close — while `"resize"` stays exempt. Exit to
Menu keeps calling `clear` BEFORE `world.destroyAll`, so the release
still reaches live entities — since #1610 through
`scripts/lib/session_teardown.lua`, the one declared boundary that path
runs, rather than a `pcall` hand-listed in `pauseMenu.onExitToMenu`; the
ordering is unchanged and is exactly why the boundary runs first. The SUCCESSFUL-load reset is the one path
that stops neither unit, because its recorded uids no longer name those
units — a durable Mode B order the unit is carrying survives every
release untouched, since stopping is all a release does to either end.
The teardown itself is step-isolated: the panels close first and each
held unit is released independently, so a missing FIRST endpoint costs
neither the other endpoint its release nor either panel its close.
#1254's requirement 7 is per-REQUEST atomicity and nothing wider: a
session owns no transaction, so ending one never rolls back a commit
that already succeeded and can only ever land between two whole
requests. Deliberately NOT added: a stall timer, and any handling of an
endpoint that is merely unreachable or drifted — both are live and
commandable, so neither is a session failure. Gate coverage: hspec
`--match "Transfer context menu"` includes the escort session with the
two-sided hold and every failure trigger above.

---

## Commanded-order stall budget (#920/#1291)

Enforced by hspec `--match "commanded order stall budget"` and
`tools/expedition_retrieval_probe.py` (manual-only). CLAUDE.md keeps
the stall-not-trip-budget rule; this is the accounting.

`pickup_timeout`/`TASK_TIMEOUT_SEC` reset on a new closest approach.
Don't restore the from-`issuedAt`/`startedAt` shape — it capped ordered
retrieval at ~21 tiles. Since #1291 they are spent in ELIGIBLE time only
(`unit_ai_stall.lua`, which owns the accounting and `maintainTask`): an
interval another action won (the #306 ladder's
eating/drinking/refill/combat/`treat_ally`, or a `forage` that walks the
unit AWAY), or one the AI never ticked through at all (collapse, an
engine animation, a mental break, a load boundary — seen as a gap longer
than `MAX_CHARGED_INTERVAL`), costs a pending order nothing, while the
budget still ACCUMULATES across interruptions so no order becomes
immortal.

That state (`stalledFor`/`stallSeenAt` on the order) arrived in
`lua.unit_ai` v5 (the component is at v6 since #1216, and every version
from v1 is still an accepted input); a v1–v4 order carries the old
absolute `progressAt` and is seeded from it on its first tick. The
position-hold walk home (#1216) is charged against the same budget, so
an unreachable anchor expires instead of re-pathing forever.

---

## The expedition loop: the unprepared control

Enforced by `tools/expedition_loop_probe.py` (manual-only, fixed-seed,
~15 min, two engine boots). `docs/expedition_gameplay_loop.md` is the
design authority for the arc; CLAUDE.md states that the control exists and
must end measurably worse off. This enumerates the six conditions that
keep the comparison honest — weakening any one turns the control into
theatre — and the traps found while building it.

1. **`find_water` retired and `forage_max_fraction` disabled** for the
   session. #94's emergency foraging ladder has its own gate,
   `foraging_probe.py`.
2. **BOTH travellers shed to inside carrying capacity first.** An
   over-encumbered acolyte crawls, its order stall-times-out, and it never
   arrives (`docs/expedition_survival_calibration.md` E1).
3. **The control gets NO retrieval target of its own** — a ruin can roll
   food, and a control that eats what it finds destroys the measurement.
4. **The travel VERB matches.** `commandMove` walks at
   `movement_speed.ordered` = comfort × 1.15, while `pickup_ground` walks
   at comfort, so the retrieval order is issued only after the
   measurement.
5. **The ORIGINS are equalised as a PLACE, not merely a distance.** Hunger
   drains with time on the road and route shape is time; a radial band is
   satisfied anywhere on a circle, so the check asserts separation as well
   as distance spread, verified with the simulation STOPPED.
6. **The observation point is both travellers at the ruin in ONE COHERENT
   SNAPSHOT** — a single paired read revalidated with the simulation
   stopped. Two separate `unit.getInfo` round trips let the sim run in
   between, and a pair that was never inside together can satisfy them.
   The two arrive at different times, and since #1216 the first one HOLDS
   its destination rather than wandering off — but a survival interrupt
   can still carry a held unit off its anchor while the second is walking,
   so the coherent snapshot is still what the check needs.

**Canteens stay full on both.** A dry one puts `refill_canteen` at its 7.5
peak, above `follow_command`, and the control then abandons the leg to
walk to the water the scout radioed about — a behavioural difference, not
the supply being measured. The gated metric is FOOD (stomach fraction),
matching what the calibration measured actually goes live on a trip this
length; water is reported as evidence, not gated. The eating itself is
watched live as a real `eat_from_inventory` action, so the delta is
attributed to a mechanism rather than inferred from a number two
differently-massed acolytes could reach by other routes.

**Don't "fix" that by seeding a thirst deficit.** `scripts/salts.lua`
derives blood salt concentration as saltFrac/hydrationFrac and
`scripts/brain.lua` folds it straight into consciousness, so a unit
dehydrated far enough to prefer drinking over its orders is knocked
unconscious by the electrolyte imbalance — and scaling the `salt` pool
down to compensate just moves the blackout to the first meal's salt bolus
(`salts.mealSalt` restores 0.30 of max_salt per feed). Both were observed
live while building the gate.

**Two instrument gotchas.** A completed PLAYER move order now holds
position (#1216, SURV-4) — E3's "it does not" is retired — so the pause
pinning here is belt-and-braces rather than the only thing keeping an
arrival in place. Do not lean on the hold alone: it yields to the same
survival ladder the move order did, so an interrupted traveller still
leaves its anchor mid-measurement. And **`unit.setFrozen` is not a hold
at all**: `uiFrozen` only makes
`publishToRender` skip the sim-derived update, so a "frozen" unit keeps
walking while `unit.getInfo` reports where it was when the flag went up.
Use `engine.setPaused` when you need a unit to actually stay put, and
re-read positions after pausing.

---

## Autosave: staging, rotation order, and the intent mutex

Enforced by `tools/autosave_probe.py` (manual-only). Slots are the
reserved `autosave-<n>` family, `autosave-1` newest; ownership is the
durable `smAutosave` metadata flag (`"metadata"` v2; v1 migrates to manual),
NEVER the name — a manual save squatting on one of those names fails
the attempt with nothing rotated. PUBLISH FIRST, ROTATE SECOND: every
autosave writes to the reserved `autosave-incoming` staging slot and
the family ages down only once that transaction succeeds; a staged
generation left by a crash is rotated in next cycle. The rotation is
ordered the same way — the oldest is RETIRED by rename and deleted only
once every other move succeeded — so an interrupted rotation leaves a
partially shifted family, never a shorter one, and the shift plan is
DERIVED from what's on disk. A SUCCESSFUL autosave restores the
pre-request pause + visible time scale only if `playerIntentGenRef`
still matches — an `MVar` doubling as the mutex, so the comparison and
the writes are one critical section: any `engine.setPaused` /
`world.setTimeScale` during the window means the player wins. A FAILED
one stays paused and zero-scaled. Gate: `autosave_probe.py`
(manual-only).

---

## Save/load transaction: phases and failure semantics

CLAUDE.md carries the four architectural bullets (the Lua save-module
registry, `publishGeneration`'s write-fsync-revalidate-rotate transaction,
the whole-session load transaction, and the typed-reference integrity
graph). This is the phase and failure detail it defers.

**`engine.getLoadStatus()` exposes a 12-phase lifecycle plus a 13th
terminal phase, `LoadReconciliationFailed` (#1204):** publication
SUCCEEDED but a Lua `onSaveLoaded` callback raised, so the live session is
incompletely reconciled.

It is a THIRD terminal disposition, not a flavour of either existing one.
Every poller must treat it as terminal (its outcome is non-nil, so
`loadInProgress` is already false) AND as UNSUCCESSFUL. It deliberately
leaves `failedAtPhase` unset, because that field's presence promises the
old session survived unchanged — which a post-publish failure cannot. The
outcome aggregates every failing module, and `reconciliationFailures`
carries the per-module `{module, error}` breakdown. Callback isolation is
unchanged: the broadcast still attempts every module.

**Storage failures name their `StoragePhase`** through
`engine.getSaveStatus()`. A corrupt authoritative file falls back to
`.prev` and says so loudly (`recovered` in `engine.listSaves()`); an
INCOMPATIBLE one reports directly with no fallback. Symlinked slot
dirs/files are refused.

---

## Enum append-only audit: baseline and payload normalization

Enforced by `tools/enum_append_only_audit.py` (CI + `make ci`, with its
own `--self-test`). CLAUDE.md states the rule and the two hard facts about
the baseline (it is GENERATED; a pure append ratchets it with
`--update-baseline`). This is the rest.

**Coverage.** Of the 43 guarded types, 38 are on the save wire and 28 are
named by a live component today; the rest are guarded pre-emptively, which
is the point of keying on the `Serialize`-via-`Generic` instance rather
than on save reachability. Don't hand-count these: the audit prints the
guarded total on every run, and `docs/save_compat/enum_baseline.json`'s
per-type `onSaveWire` / `components` fields are the other two.

**What the baseline records.** Module-qualified constructor lists, each
constructor recording its name and its ordered PAYLOAD signature, plus the
save-wire attribution captured alongside.

**How a payload slot is normalized.** A slot is the field's declared type
with strictness markers, `{-# UNPACK #-}`, layout, `::`/`∷` and the
parentheses a `!` forces all erased. Field order and type structure are
NOT erased. For a record alternative the selector is kept — which is what
makes swapping two same-typed record fields visible, and means a selector
rename reports too.

**Diagnostics.** An incompatible change's output names every component and
historical shape that carries the type, with the reachability path. That
holds even for a type that was renamed or deleted, read back from the
recorded attribution because there is nothing left in the tree to walk.

**Boundary against `tools/save_compat_audit.py`.** Since #1270 this audit
is the one exhaustive gate owning payload drift INSIDE a
multi-constructor sum. Single-constructor record field order stays the
frozen-DTO boundary's and `save_compat_audit.py`'s.

---

## Config-writing tests: the isolation fixture (#1357)

Enforced by hspec `--match "Settings Defaults keybind persistence"`
(the isolation boundary itself, plus the player-facing Defaults
write-through it must not weaken). CLAUDE.md keeps the rule — wrap
`Test.Headless.Harness.Isolation.withIsolatedResourceRoot` AROUND
`withHeadlessEngine`, outside never inside; this is why the fixture is
built the way it is.

It points the process cwd at a scratch root that symlinks every
top-level checkout entry but owns a real COPY of `config/` — the one
family production code writes into — so every cwd-relative write lands
in a temp dir. Outside, never inside: engine init is itself a writer
(`migrateLegacyConfig`, the notification-overrides materializer), so a
fixture that intervened after the engine came up would already be too
late. The checkout is only ever READ, so no crash can leave developer
state half-restored.

Two properties keep the fixture from deleting the wrong thing: the root
is created FRESH and EXCLUSIVELY per invocation under a random name via
`createDirectory` (a fixed path could already hold a symlink, and
`doesDirectoryExist` follows one, so teardown would enumerate and
recursively delete the TARGET's children), and "am I isolated?" is
`isInsideIsolatedResourceRoot` — fixture-owned state checked against
the real cwd, never a marker file, which any same-named file on disk
could forge into skipping isolation entirely.

The two suites that need it (`UI.ResponsiveMenus`,
`UI.ResponsiveGameplay`, both reaching the write-through
`settingsMenu.onDefaults()`) each carry a one-line in-suite guard
asserting they run under it, because every other assertion in them
passed while the developer's bindings were being replaced.

---

## CLI value validation (#1191)

Enforced by hspec `--match "App.Cli"` and `tools/preview_cli_probe.py`
(no boot). CLAUDE.md states the rule, the flags it covers, and
`--region`'s exclusion. This is the rest.

**Empty selections and empty segments** are errors too, not just unknown
layers: `--dump=` and `--dump=terrain,` each exit 1 naming the flag and
the offending token.

**Ordering.** Validation runs AFTER the mode-compatibility rejection,
which keeps its priority — a malformed `--seed` given to `--headless`
still reports as unsupported in headless mode, not as a bad number. It
runs BEFORE every mode-specific early exit, regardless of whether the
selected mode would ever consume the value.

**`--region`'s exclusion** is deliberate and tracked: its identical silent
default is `docs/code_health_findings.md` CH-67, sequenced after #1081.

---

## Debug-console listener policy (#1190)

Enforced by hspec `--match "debug-console listener policy"` and
`tools/debug_console_boot_probe.py` (CI-eligible). CLAUDE.md keeps the
rule — `--headless`/`--offscreen` ABORT when the listener can't start;
this is the detail.

Those two modes have no window, so the console is their only
interactive control surface. If the listener can't start — an occupied
or unbindable port, or `--port 0` (issue #46's "no TCP listener at all"
sentinel, which belongs to `--dump` alone) — the boot exits non-zero,
prints no `READY` marker, names the mode / effective port / cause on
stderr, and tears down what it had already built (the pre-thread Lua
state, plus offscreen's input worker), each cleanup step announcing
itself on stderr. `--dump`, `--graphical` and `--preview` keep their
existing tolerance unchanged, port-0 behavior included.

The per-mode decision is
`Engine.Scripting.Lua.DebugServer.debugConsolePolicy`, keyed on
`EngineConfig`'s `ecBootMode` — `ecHeadless` can't tell dump from
headless and is `False` for offscreen.

---

## Findings-report lane split: why it matters

Enforced by `tools/findings_report_audit.py` (CI + `make ci`). The
ownership rule — the processing lane owns all three status markers, an
implementation PR owns only the narrative body — stays in CLAUDE.md.

That split is not stylistic. The two lanes had already drifted an entry
in each direction, and each drift re-files merged work: the processor
selects a bare-headed finding as unprocessed, and the "headings win,
correct the checklist" tie-break then unchecks a finding an issue already
resolved. The cost lands on other people's PRs too —
`.github/workflows/review-gate.yml` strips `reviewed:approve` when a push
touches a file an open PR also owns, so every master-side report edit
costs an open PR its approval.

---

## Docs landing: docs-wip, autostash, and the protected-ref warning

The rule — the primary checkout stays CLEAN, uncommitted work lives in
the docs worktree, land with `tools/docs_land.sh` — stays in CLAUDE.md.

The manual fallback, if the script itself is ever unusable:

```bash
cd "$DOCS_WT" && git add -- <paths> && git commit -m "…" \
  && git fetch origin && git rebase --autostash origin/master \
  && git push origin docs-wip:master
```

Landing ONE document while others are still half-written is the normal
case, so the rebase must tolerate a dirty tree — a plain `git rebase`
aborts with "cannot rebase: You have unstaged changes" and strands the
landing. `--autostash` is required there, not decorative. Should ITS restore
conflict, the damage is confined to this worktree and surfaces
immediately in front of you — it cannot wedge the drainer, which is the
whole point of doing the work here.

**`docs-wip` is not a feature branch.** It tracks `origin/master` and
lands by direct push, so it is a second working copy of master rather
than something that accumulates and merges later. Uncommitted work can
sit in it indefinitely without the drainer ever seeing it; that is its
job. A bare `git push` from it fails safe (`push.default=simple` refuses
the differing name) — use the explicit refspec above. That push prints
`Cannot update this protected ref` and `N of N required status checks are
expected` and then **succeeds anyway** under admin bypass — judge it by
`git rev-list --left-right --count HEAD...origin/master`, not the warning.
