# Engine contracts: as-built detail

`CLAUDE.md` is auto-loaded into every session, so it carries the rules
that prevent damage: what you must not undo, and which gate proves it.
This file carries the layer below that — the as-built mechanics behind
those rules, extracted from CLAUDE.md in three passes (2026-08-18,
2026-08-20 and 2026-09-02 — the last also introduced the nested
per-directory `CLAUDE.md` files) to keep the always-loaded file
navigable.

**This is not a design document.** The design docs
(`docs/texture_infrastructure.md`, `docs/unified_item_transfers.md`,
`docs/expedition_gameplay_loop.md`, `docs/persistence_contract.md`, …)
record what was *decided*; this records what was *built*, and it is the
only prose record of most of it. Read the section here before changing
code in the area it covers — the root `CLAUDE.md` and the nested
per-directory `CLAUDE.md` files point you at each one by name.

Every contract below is mechanically enforced by the gate its
`CLAUDE.md` entry names, so a breach fails loudly rather than silently. That is
exactly why the detail could move out of the always-loaded file.

---

## Contents

**Build & CI**

- [The `make ci` gate set](#the-make-ci-gate-set)
- [Headless fixture logging (#1925)](#headless-fixture-logging-1925)
- [The full test tier: `SYNARCHY_FULL_TESTS` (#1364)](#the-full-test-tier-synarchy_full_tests-1364)

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
- [Startup readiness: the YAML fail-fast rule (#2203)](#startup-readiness-the-yaml-fail-fast-rule-2203)

**World and naming**

- [World identity and language provenance (#707/#1092/#1101)](#world-identity-and-language-provenance-70710921101)
- [Location and river naming (#1101/#1102)](#location-and-river-naming-11011102)
- [Name etymology: internals (#1104)](#name-etymology-internals-1104)
- [Location instances (#911)](#location-instances-911)
- [Guaranteed significant contents and compound clearance (#917)](#guaranteed-significant-contents-and-compound-clearance-917)
- [Location discovery, map icons, and per-unit knowledge (#780/#781/#915)](#location-discovery-map-icons-and-per-unit-knowledge-780781915)
- [Page incarnation: in-flight work across a reused page id (#2474)](#page-incarnation-in-flight-work-across-a-reused-page-id-2474)
- [Entity teardown on destroy and same-id re-init (#2476)](#entity-teardown-on-destroy-and-same-id-re-init-2476)

**Gameplay systems**

- [Tile-coordinate seam frame (#1175/#1230)](#tile-coordinate-seam-frame-11751230)
- [Position hold (#1216)](#position-hold-1216)
- [Player transfers: the three player-facing modes](#player-transfers-the-three-player-facing-modes)
- [Nested ownership moves (#2487)](#nested-ownership-moves-2487)
- [Portable container knowledge (#2512)](#portable-container-knowledge-2512)
- [Commanded-order stall budget (#920/#1291)](#commanded-order-stall-budget-9201291)
- [The expedition loop: the unprepared control](#the-expedition-loop-the-unprepared-control)
- [Unit and combat animations headless](#unit-and-combat-animations-headless)
- [Movement arenas](#movement-arenas)
- [Movement tick: residual time across waypoints (#2473)](#movement-tick-residual-time-across-waypoints-2473)
- [Construction (#95/#96)](#construction-9596)
- [Roles (#265)](#roles-265)
- [Crafting and bills (#325/#326/#329/#343/#795)](#crafting-and-bills-325326329343795)
- [Power (#358-#361, #590/#591, #1206)](#power-358-361-590591-1206)
- [Flora species identity: the authored name is the key (#2241)](#flora-species-identity-the-authored-name-is-the-key-2241)
- [Flora visual state and fallback (#2526)](#flora-visual-state-and-fallback-2526)
- [Loot profiles (#2499)](#loot-profiles-2499)
- [Farming (#331-#336)](#farming-331-336)
- [Fluid reaction: unlike-fluid contact and its stone (#2481, #2485)](#fluid-reaction-unlike-fluid-contact-and-its-stone-2481-2485)
- [Blood decals: transience (#603)](#blood-decals-transience-603)
- [Logging streams](#logging-streams)

**Persistence**

- [Autosave: staging, rotation order, and the intent mutex](#autosave-staging-rotation-order-and-the-intent-mutex)
- [Save/load transaction: phases and failure semantics](#saveload-transaction-phases-and-failure-semantics)
- [Enum append-only audit: baseline and payload normalization](#enum-append-only-audit-baseline-and-payload-normalization)
- [Local-config writes: one atomic-replace helper (#2202)](#local-config-writes-one-atomic-replace-helper-2202)
- [Config-writing tests: the isolation fixture (#1357)](#config-writing-tests-the-isolation-fixture-1357)
- [Config state and legacy migration (#638/#786/#1937)](#config-state-and-legacy-migration-6387861937)

**CLI and boot modes**

- [CLI value validation (#1191)](#cli-value-validation-1191)
- [Debug-console listener policy (#1190)](#debug-console-listener-policy-1190)

**Engine core**

- [Monotonic elapsed time (#2204)](#monotonic-elapsed-time-2204)

**Process gates**

- [Findings-report lane split: why it matters](#findings-report-lane-split-why-it-matters)
- [Docs landing: docs-wip, autostash, and the protected-ref warning](#docs-landing-docs-wip-autostash-and-the-protected-ref-warning)

---

## The `make ci` gate set

CLAUDE.md states the rule: `make ci` runs the same gate SET as `ci.yml`'s
two audited workers together — `test-and-audits` for everything needing a
Cabal build product, `static-audits` for every engine-free `python3
tools/*.py` gate (#2272) — `tools/ci_parity_audit.py` keeps the union and
`make ci` from drifting, and the gate is never an iteration loop. This is
the enumeration and the exemptions.

The two-job shape is a CI-side optimisation only: the engine-free half used
to queue behind ~11.6 minutes of Cabal work purely by sharing a job with it.
`make ci` is one script, so the UNION is what it mirrors — but the audit
collects each job's set separately and rejects any command run by both
before it takes that union, because the union alone cannot distinguish a
correctly split gate set from one CI pays for twice. Each audited job must
also yield at least one invocation, so an emptied job fails rather than
shrinking the comparison. `static-audits` carries no job-level condition:
that, and not the docs-only selector, is what now keeps the engine-free
audits running on a docs-only change.

The set: warning-clean (`-Werror`) build of library/exe + both test
suites, the headless hspec suite, `test_audit.py`,
`test_determinism.py`, the Lua/Haskell module-budget guards, the Lua
duplicate-function audit, the Unicode-operator audit, the haddock link
audit (`test_haddock_link_audit.py` then the bare audit, #2292 — no
qualified `'Module.function'` haddock link outside its generated
baseline), the Lua
strict-decoder audit
(`lua_strict_decode_audit.py --self-test` then the bare audit, #1605 —
no direct `Data.Text.Encoding.decodeUtf8` under
`src/Engine/Scripting/Lua/`), the config-write / persistence-inventory /
EngineEnv-capability
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

**The haddock link audit (#2292)** is the newest member, and is owned
by `ci.yml`'s `static-audits` worker — the engine-free half #2272 split
out, which is where every gate needing no Cabal build product, engine
boot, display or generated runtime data now lives. `make ci` is one
script and runs it unconditionally, as it does every other gate in the
union of the two workers.

`tools/haddock_link_audit.py` fails when a QUALIFIED haddock link
`'Module.function'` in a `src/` or `app/` comment names a function the
module does not export: such a link renders as plain text and sends a
reader to a module that hides the symbol. `synarchy.cabal` passes
`-haddock` to GHC, but that validates comment SYNTAX only — link
TARGETS are resolved by the `haddock` tool, which no gate runs, and
this audit does not run it either. Comment awareness is
`unicode_operator_audit.py`'s scanner rather than a second Haskell
lexer: `_scan_code` now reports COMMENT spans too, published as
`haskell_comment_spans`, so a link in a string literal or a character
literal is never a candidate. That report is separate from the code
spans on purpose — the complement of the code spans is not "comments",
because strings are non-code as well. Quasiquotes are masked by the
haddock audit itself, since the shared scanner has no quasiquote state
and the Unicode audit handles its one quasiquoting file with a
separate, file-scoped step.

**Its baseline is a temporary ratchet, not an exemption list.**
`tools/haddock_link_baseline.json` is generated by
`--update-baseline` and never hand-edited; a run fails on any dead link
absent from it AND on any entry no longer found, so the file can only
shrink, and a sweep must delete its entries in the same change that
fixes its links. Owner decision D-3 in
`docs/haddock_link_resolution_design.md` lands the guard first (HLR-1)
and drains the baseline across HLR-2 to HLR-4; **HLR-5 deletes the
baseline file and `--update-baseline` together**, after which zero dead
links is the permanent state and the tool cannot grow a new allowlist.
Contrast the standing exemption lists elsewhere in this section — the
Unicode-operator audit's `WHOLE_FILE_EXEMPT`, the parity audit's
`EXEMPT_COMMANDS` — which are permanent, reason-carrying and
hand-maintained.

**The config-write audit (#2202)** was the previous newest member.
`tools/config_write_audit.py` is structural, not a text filter: it
requires every module in its checked-in config-persistence set to
contain no raw `encodeFile`/`writeFile`/`copyFile`/`renameFile` and to
import `Engine.Core.ConfigWrite`, requires any OTHER file under
`src/`/`app/` that names a `config/` literal to contain no raw write
unless it carries an exemption reason, and requires the helper itself to
still CALL the durable primitives it is built from (import lines are
excluded from that check, so deleting a call cannot hide behind an
import). Its raw-write vocabulary includes `removeFile` and friends,
because a config family that publishes by DELETING owes the same
directory sync as one that publishes by renaming. The shape is
deliberate: the issue's own
`rg 'encodeFile|writeFile' src app | rg 'config/'` acceptance returned
no matches on the defective snapshot, because the raw write and the
`config/` literal sat on different lines and three of the six writers
never name a config path at all. Comment and string-literal awareness
comes from `unicode_operator_audit.py`'s lexer, so a haddock naming
`encodeFile` — which several of these modules now do, describing what
they replaced — is never a hit. See §Local-config writes.

**The bare-name-icon check (#1740)** held that spot before it.
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

**The world-determinism content-identity self-test (#1724)** is an
earlier member. `tools/test_determinism.py` is the executable
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
The CI-only invocations split across three worker jobs. In
`test-and-audits`, the path SELECTORS
(`ci_expensive_gates.py --stdin --gate worldgen|graphical` and
`ci_docs_fast_path.py --stdin --explain`) have nothing to select
locally, and its bare `ci_cache_report.py` (#1358) classifies what the two
`actions/cache` restore steps got from outputs only a runner publishes;
`make ci` restores no GitHub Actions cache, so it has no outcome to
classify, and the command reports rather than gates. Its `--self-test`
form is not exempt and runs on both sides — in `static-audits`, with the
cache-key and retention self-tests, since none of the three reads a cache.
`static-audits` also owns `ci_expensive_gates.py --stdin --gate
unit-assets`, which moved there with the unit-asset gate it decides
(#2272); it is exempt for the same reason its siblings are.

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
engine-booting `run_probes.py` sweep; neither is part of the CI /
`make ci` parity contract because CLAUDE.md keeps behavior probes opt-in
locally. The stable `build-test` context depends on all three parallel
workers and is the single CI verdict the admin-bypass PR drainer consumes,
so moving the sweep — and, since #2272, the engine-free audits — out of the
heavy worker does not weaken its blocking PR verdict. `static-audits` is
required with a plain `= success` on both events, because it carries no
condition and so is never legitimately skipped. The parity audit pins that
aggregate wiring, the static-audit worker's own topology, and the probe
worker's selector and runner commands.

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
`pack_atlas.py --validate-only --strict` run. `src/Unit/Atlas/CLAUDE.md`
keeps the one-atlas-per-animation shape and the index-aware validation
rule; these are the exact invariants.

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
`assets/textures/ui/hud/utility/{zoom,world}_*` and the four
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
| building `sprites:`/legacy `sprite:` | `bdTextures`' four camera views, selected per facing by `Building.Visual` (#2088) | `bdIconTexture` (`building.listDefs`'s `iconTex`) |
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

**Category contract and pre-boot rejection (moved from CLAUDE.md).**
Canonical category contract (`App.Cli.classifyPreviewCategory`) — the
unknown-category error lists exactly this set, no compatibility aliases:
**simple** (a flat, recursively-browsable asset folder): `icons`,
`items`, `ui`, `world`; **grouped** (one named entry per item — a bare
grouped category prints "select a specific ..." and exits without
booting): `units`, `flora`, `buildings`, `structures`. `equipment`,
`hud`, `facemap`, `utility`, `vegetation` are NOT exposed.

Pre-boot rejection is the load-bearing rule (`Engine.Preview.Discovery`
/ `.Unit` / `.Building`; `resolveItemDir` shared by all four grouped
categories): an unknown name, a name with path structure or `.`/`..`/
absolute traversal, a symlinked directory (BOTH levels for
`units/<name>` — `doesDirectoryExist` follows links), and a FILE where a
directory was expected all exit 1 **before a window exists**. Trimmed
loading: only its font, the list widget's own chrome textures, and
textures within the requested category/item — never `data/*.yaml`
gameplay catalogs, with exactly TWO single-file exceptions: the units
viewer's `data/units/<name>.yaml` and the buildings viewer's
`data/buildings/<name>.yaml`.

`flora/<name>` and `structures/<name>` reuse the shared browser
(`scripts/ui/asset_browser.lua` + `scripts/ui/list.lua`, #888) rooted at
the ITEM's folder — anything beyond routing the resolved folder into
`discoverEntries` means the routing is wrong, not the reuse. The units
viewer (#887/#1261) samples the compiled atlas through the SAME loader
(`Unit.Atlas.Yaml.resolveUnitAtlases`) and frozen cell arithmetic
(`atlasCellUV`) the game uses — a preview-only decoder would miss the
regressions the viewer exists to catch — and a rejected, missing,
animation-less or uncompiled index is a PRE-BOOT failure, never a quiet
fallback to source frames. The buildings viewer (#888) is the opposite
authority split: the filesystem is authoritative and
`data/buildings/<name>.yaml` only AUGMENTS a matched animation
(association by CONTENT, never by equal names; playback defaults
`fps=8`, `loop=false` — NOT the units viewer's `loop=true`). #2492
LAYERS a declared lifecycle/facing matrix, read only from that same
YAML, over that browser without replacing any of it — see §The declared
lifecycle/facing matrix.

Centered bounded zoom (#1907): every main preview display has ONE
per-session zoom multiplier, `1` (the initial value AND the maximum,
i.e. the aspect fit) down to `1/8`, centered on its region with NO
anchor, pan or crop — the complete texture is inside its region at every
level by construction. The region owns a scroll-CAPTURING invisible
element and nothing else (#743's three policies stay independent), which
is what makes plain and Shift wheel identical; `dy < 0` ENLARGES, the
gameplay camera's sign, not the list's. Reset follows preview-OBJECT
identity — a different BARE-category texture resets, while another
animation, direction, building entry, flora stage, structure piece,
playback frame or a resize preserves — discriminated by
`engine.getPreviewTarget()`'s `item`, never the mode string. Unit mode's
region is the enlarged sub-rect, never `panelBounds`.

Gates: `tools/preview_cli_probe.py` (CI-eligible, no boot at all — every
rejection above) and `tools/preview_probe.py` (manual-only, `needs-gpu` —
discovery/selection/scroll/resize via the dump, forced nearest
filtering, both viewers, trimmed loading verified against the engine's
own authoritative texture record, and zoom on all six display kinds via
real `input.moveMouse`/`input.scroll`). Pure logic: hspec
`--match "Preview.Discovery"` / `"Preview.UnitAnimation"` /
`"Preview.Building"` / `"Preview.BuildingMatrix"` / `"Preview.Zoom"` /
`"Preview.KeyboardNavigation"` — the last three are the only BLOCKING
automated gates zoom, the buildings matrix and keyboard routing have,
the probe being manual-only.


Enforced by `tools/preview_cli_probe.py` (CI-eligible, no boot) and
`tools/preview_probe.py` (manual-only, `needs-gpu`); pure logic by hspec
`--match "Preview.Discovery"` / `"Preview.UnitAnimation"` /
`"Preview.Building"` / `"Preview.BuildingMatrix"`.

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

- **The filesystem is authoritative** — the OPPOSITE of the units
  viewer's split, not the same one. #888 amended #887's filesystem-first
  units viewer, but #1261 then replaced that half: for units the
  `data/units/<name>.yaml` declaration and its compiled
  `atlas/index.json` decide which animations exist and how their frames
  are stored, so an animation folder on disk and absent from the YAML is
  EXCLUDED rather than browsed. The contrast is about DISCOVERY and
  frame storage only — a unit target must still contain a real,
  unsymlinked asset tree, and those containment checks remain pre-boot
  requirements. Here the building's own folder decides which entries
  exist and, in an animation directory, the numeric `frame_NNN.png`
  order; `data/buildings/<name>.yaml` only AUGMENTS a matched animation
  with `fps`/`loop` and supplies the default-selection hints. A missing,
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
  ROW, reset on a real selection change but preserved across a facing
  change and a resize; end-of-clip REPLAYS (#1833) on the units viewer's
  identical terms — every animated row repeats indefinitely regardless
  of its authored `loop`, the wrap coming from the index rather than a
  restarted `rowStart`. A STATIC selection has no playback at all,
  and forced replay does not change that: `buildingAssetView.update`
  still advances nothing outside an animated row, so a static entry
  keeps exposing no `playback` in the dump.

#### The declared lifecycle/facing matrix (#2492, BDA-4)

`data/buildings/<name>.yaml` gains a SECOND, separate job here. It still
only augments the filesystem browser above; it additionally supplies a
DECLARED inspection matrix, and the two authorities never mix — nothing
in the matrix discovers, filters, reorders or relabels a raw entry, and
nothing in the browser reads the matrix.

- **The declaration is decoded through the GAME's own field decoders**
  (`Engine.Asset.YamlBuildings.defSprites` / `defRoleAnims` /
  `defAnimations`, reached from `Engine.Preview.BuildingMatrix`), so the
  closed `BuildingRole` vocabulary, the legacy `appearing` resolution
  through `Building.Schema.legacyRoleFor` on `build_work` (default 0),
  the canonical-vs-legacy `sprites`/`sprite` and `frames` forms, and
  every rejection are literally the code the game applies.
- **Every rejection lands in ONE fallback**: no declared rows at all,
  and the complete raw browser with its existing default behavior. An
  unknown lifecycle key, a legacy `appearing` beside the canonical role
  it resolves to, both sprite forms, a negative or non-finite
  `build_work`, a malformed animation, a missing or unreadable file, and
  a file matching no definition all answer the same way. Never a
  precedence rule, never a partial matrix, never a pre-boot rejection.
- **Rows.** Declared lifecycle rows in the fixed order `construction`,
  `appearance`, `built`, `destruction` (an UNDECLARED role is absent,
  not reported missing), then the declared static-sprite row, then every
  raw filesystem row in its existing relative order, label, kind, frame
  order, fps, loop and playback. Identities are `lifecycle:<role>`,
  `sprite` and `filesystem:<label>`, so a lifecycle row and the raw row
  backing the same files cannot alias each other — duplicate-looking
  rows are expected, not a bug. A role whose animation reference does
  not resolve is RETAINED as a diagnostic row naming the role and the
  reference, never silently reclassified as undeclared.
- **Cells.** Four per declared row, in camera order south, west, north,
  east, each holding that facing's COMPLETE ordered path list. A
  canonical declaration keeps its four independently authored lists; a
  legacy `sprite` repeats one path and a legacy `frames.default` repeats
  its whole ordered list, and every such cell and its containing row are
  marked `legacy` both structurally (the dump) and visibly (the cell
  caption's `*`, plus a `legacy` flag on the enlarged view) so four
  repeated views can never read as four authored ones. Provenance is
  per ENTRY: one building may declare its sprite legacy and an animation
  canonically, and the dump's top-level `declaration` is the SELECTED
  declared entry's source, absent for a raw selection.
- **Missing cells.** A cell is diagnostic when any declared path is
  absent, a directory, a symlink (at the leaf OR any ancestor below the
  building's folder), a special file, carries an unsupported extension,
  or does not resolve under `assets/textures/buildings/<name>/` at all —
  the last with its own `outside_root` reason, because requesting it
  would break trimmed loading. Those verdicts come from ONE `lstat` walk
  down every component below the building's folder, never from an
  existence predicate first: `doesPathExist` FOLLOWS links, so a
  dangling symlink would answer "absent" and never reach the symlink
  rule. A missing cell never substitutes a path from another facing,
  role, raw row or the static sprite, never requests its invalid texture
  (its dump entry carries no `handle`), and draws a textureless marker.
  The selection then reports `state == "ready"` with its diagnostic
  flags — never `"loading"`, and never `"empty"`, which is #1690's
  terminal bindless-failure state.
- **Diagnostic state is reported per ROW, not only per cell.** Each
  declared row — in `lifecycle`, in `staticSprite` and in the combined
  `rows` alike, computed once so the three cannot disagree — carries
  `missing`, `missingReason` and `missingCells` beside `resolved`.
  `resolved` alone cannot say it: a row whose animation reference
  resolved but whose west cell is absent is a real authoring fault, and
  `rows` is the surface automated input uses to pick a row to click. A
  raw row reports `undeclared` there instead. The dump also names the
  marker ELEMENTS — per cell and for the enlarged region — so a gate can
  read their visibility and text back through `UI.getElementInfo` and
  prove the indicator was drawn, rather than trusting that a dump flag
  implies a pixel.
- **Compatibility.** `entries`, `defaultEntry` and `selected` keep their
  pre-#2492 meaning: `entries` is the raw list in its existing order and
  shape, `defaultEntry` is the unchanged raw ladder, and `selected` is
  the RAW projection of the active row — a declared static projects onto
  the raw static whose frame equals its SOUTH declared path, a lifecycle
  row onto the first raw animated entry in label order whose frames
  overlap ANY facing's declared paths, and it is absent when none does.
  The projection is FACING-INDEPENDENT: changing facing never moves it.
  The combined list is described by the separate `selection`,
  `defaultSelection`, `lifecycle`, `staticSprite`, `filesystemEntries`,
  `selectedLifecycle`, `selectedFacing`, `facingRow`, `rows` and
  `totals` fields.
- **Initial selection** is the declared `built` row, else the declared
  sprite row, else `filesystem:<defaultEntry>`, else nothing for an
  empty browser. A declared `built` row wins even when it is a pure
  diagnostic. The initial facing is south, and the enlarged facing then
  CARRIES across a row change so two roles can be compared from the same
  view — falling back to south for a row that lacks it. A raw row clears
  it (it has no facing model), so returning to a declared row through
  one starts from south again.
- **Input.** Up/Down move the combined list. Left/Right move the facing
  strip in displayed order with wraparound, and only while a DECLARED
  row is selected — a raw row has no facing model, so they stay
  unhandled there exactly as before #2492. Held-key repeat uses the same
  `previewManager` clock as unit directions; because facings wrap, a
  hold continues until key-up rather than terminating at a boundary.
  Clicking a facing cell (`onPreviewFacingClick`, a callback name
  distinct from the units viewer's) enlarges it. A facing change never
  resets the replay cycle, the zoom multiplier, the list selection or
  the scroll offset.
- **Zoom region.** A declared row reports `layout().enlarged` — the
  sub-rect ABOVE the facing strip, the units viewer's rule — and a raw
  row, having no strip, keeps the whole panel it always had. The
  multiplier still follows the preview OBJECT (the building), so a row
  change and a facing change both preserve it; only a new session
  resets it.
- **Resize** preserves the selected row IDENTITY, the selected facing,
  the list scroll offset, the cycle-local playback phase and the zoom
  multiplier, recomputing row, cell, enlarged-sprite and zoom bounds
  without reselecting or restarting playback.
- **Frame selection** matches gameplay WITHIN each forced-replay cycle:
  `construction` maps the cycle phase onto build progress and compares
  with `Building.Visual.pickBuildingFrame`; `appearance` and `built` use
  cycle-local elapsed time against the same function; `destruction` uses
  `Building.Destruction.destructionFrame`. Past that cycle the two
  deliberately diverge — gameplay clamps a non-looping clip and expires
  a destruction effect while the preview replays (#1833) — so equality
  at an unbounded preview clock time is NOT the contract.
  `pickBuildingFrame`'s last-frame pin fires only when no `built`
  animation resolves, and no `built` row exists in that case, so no
  declared row reaches it.

Gates: hspec `--match "Preview.BuildingMatrix"` — the pure matrix,
identity, projection, classification and gameplay-equality half, plus
the CPU-only Lua half that drives the REAL `preview_manager` /
`building_asset_view` / `preview_zoom` through the shared
`Test.Headless.Preview.LuaHarness`; `--match "Preview.Zoom"` and
`--match "Preview.KeyboardNavigation"` for the pane and routing
regressions. `tools/preview/buildings.py` (through the manual-only,
`needs-gpu` `tools/preview_probe.py --only buildings`) exercises the
same surface against a live engine, locating rows by identity rather
than by label. Its phase 8 generates a fixture building — gitignored,
written to the canonical `assets/textures/buildings/<name>/` and
`data/buildings/<name>.yaml` paths because that is where the viewer
resolves one, and removed in a `finally` — because no shipped definition
declares a canonical `sprites`/`frames` block, a `destruction` role, an
unresolved animation reference, or art that is not on disk, so the
missing, unresolved, legacy and provenance states have nowhere else to
be verified through real marshalling and rendering.

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
  capture rect and the fit denominator. Since #2492 the buildings viewer
  answers the same way, but per ROW CLASS: a DECLARED row draws a facing
  strip and reports its own `enlarged` sub-rect, while a RAW filesystem
  row has no strip and keeps the whole `panelBounds` it always had.
  Every other mode uses `panelBounds`. Whichever it reports is the
  region the sprite was actually fitted to, so a containment or
  centering assertion runs against the real denominator in both cases.
  The fit for an atlas-backed unit frame uses the compiled index's own
  CELL dimensions, never the sheet's (`frameSize` asks
  `engine.getTextureSize` only for residency).
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
  `list.getChromeTexture()` there would break trimmed loading. The
  buildings viewer adopts that chrome handle EXPLICITLY (#2492): its
  initial `built` row may legitimately be a pure diagnostic that
  requests no texture at all, so a surface borrowed only from the first
  frame the pane happened to load would never exist and the wheel would
  leak to the gameplay/z-slice broadcasts for the whole session. That
  mode always builds a list, so the handle is already in flight there. It is
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
  animation, direction, entry, facing, stage or piece PRESERVES the
  multiplier;
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
- **Not zoomed:** list thumbnails, unit direction-row cells and the
  buildings viewer's facing-strip cells keep their existing fixed
  sizing.
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

A DECLARED building path (#2492) that does not resolve under
`assets/textures/buildings/<name>/` — after the same containment and
symlink rules discovery applies — is a missing cell with its own
`outside_root` reason and is never requested, so the buildings viewer
keeps loading only the building's own textures plus list chrome however
a definition is authored.

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
PopupPlacement, InteractiveBounds). `scripts/CLAUDE.md` keeps the
on-sight digest; these are the six contracts in full.

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
`"Container knowledge"` / `"Nested item contents"` / `"Item list widget"` /
`"Transfer context menu"` / `"cargo_inventory_panel"` (the last reaching
`Test.Headless.UI.ResponsiveGameplay.Container`'s three describes — the
framebuffer cap, tab shrink-to-fit, resize tab preservation, #1234
endpoint agnosticism and the #1237 age indicator), plus
`tools/item_list_widget_probe.py` (manual-only, `needs-gpu`).

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

**Module ownership inside the manager (#2155).**
`scripts/cargo_inventory_panel.lua` stays the public module, the
`package.loaded["scripts.cargo_inventory_panel"]` singleton, the only
engine-loaded script of the three, and the SOLE stack-lifecycle owner:
the ordered `levels` array, base-versus-nested targeting, replacement and
deeper-level removal, modal page creation and deletion, Escape
dismissal, teardown reasons and `onClose` dispatch, resize
snapshot/restore, the per-tick liveness/staleness/close decisions, and
the public surface. Behind it:

- `scripts/cargo_inventory_endpoints.lua` owns everything that knows an
  endpoint is a BUILDING or a UNIT and everything that knows contents can
  be REMEMBERED: the `ENDPOINTS` table, unit-title precedence, the live
  unit read and the remembered building read, `knowledgeState` /
  `formatAge` / `ageText` / `weightText` / `emptyText`, endpoint tab
  policy and list params, exact-instance child identities, and the
  `endpoint` level-kind descriptor. It creates no UI element.
  Its presentation helpers branch on the view's own `knowledge`
  sub-table and NEVER on an endpoint or level kind, which is what lets
  `item_contents_panel.lua`'s `buildingItem` level get the "as of…" line
  by supplying the same sub-table. `building.refreshContainerKnowledge`
  is deliberately absent from all three modules: that absence is what
  makes "opening never reveals" true.
- `scripts/cargo_inventory_render.lua` owns the GENERIC pane: window and
  row layout constants, header baselines and the title/subtitle/age
  labels, item-list parameter completion, pane measurement and
  placement, pane and level element teardown, the row context menu with
  its appended "Contents" entry, and scroll capture. It is level-kind
  agnostic — it never resolves a kind, reads a building or a unit, or
  touches the stack, and reaches the façade only through a narrow
  controller table of callbacks.

Direction is one-way and acyclic: façade → {endpoints, render}, render →
endpoints (for the single-owned `ageText`, because the height the
renderer reserves for that line must key on the very string it draws),
and nothing back. Endpoint policy may NOT import the renderer, so the
shared tab spec and row-name colour — single-owned in the renderer — are
composed by the façade and injected as values through
`endpoints.setStyle`. The five `endpoint*` helpers and `formatAge`
remain callable on the façade with unchanged signatures, which is where
`transfer_session_panels.lua` and every probe still reach them.

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
`ResponsiveGameplay`. `scripts/CLAUDE.md` keeps the registry split and
the one-line resize rules; these are the numbers and the full rules.

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
guards. The root `CLAUDE.md` and `scripts/CLAUDE.md` keep the two rules
(no `math.randomseed` under `scripts/`; non-gameplay code keeps its own
stream); this is the story behind them.

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

## Startup readiness: the YAML fail-fast rule (#2203)

`scripts/startup_loader.lua` measures READINESS, not dispatch. A registry
family the active profile queued that **discovered no YAML files**, or
that had **any file fail to parse**, is a TERMINAL startup failure on
both the normal and the arena profile: `startupLoader.isDone()` stays
false forever, `isFailed()` becomes true, `getFailure()` retains the
payload, and exactly one error-level line names the family — plus the
failing FILE for a parse failure, or the DIRECTORY it looked in when
there was no file to name. A family whose files all parse and all
return zero is NOT a failure and boots as it always has.

**The family boundary is the fail-fast boundary.** Every discovered file
in the current family runs, all parse outcomes are retained, that
family's #1930 aggregate goes out exactly once — unchanged in spelling,
carrying the healthy counts and the original discovered-file count — and
only then does the queue stop, before any later family, the tutorial
tree, or a texture preload. A zero-file family emits its zero aggregate
the same way before failing. The failure latches: a further `tick`
advances no progress and re-logs nothing, `runAll` RETURNS rather than
spinning on `done` (the arena profile's only exit), and only `build` or
`reset` clears it.

**The bindings' outcome is opt-in.** `engine.load*Yaml(path)` still
answers exactly ONE number, zero included, for a parse failure and for a
successfully parsed empty file alike — `executeDebugLua` tab-joins every
returned value, so a second result appended unconditionally would
silently rewrite what a bare `return engine.loadRecipeYaml(p)` reads
back (`tools/craft_probe.py`). The loader is the one caller that passes
a truthy SECOND argument and gets `(count, parsed)`; `parsed` is about
the DECODE alone, so a file rejected afterwards by a family's own schema
validation reports `true` with whatever count that rejection left. A
queued binding that answers no outcome at all is treated as a failure,
not as success. The loot-table family bypasses
`Engine.Asset.YamlList.loadYamlList` entirely and follows the same rule
through `Engine.Asset.YamlLootTables`.

`scripts/loading_screen.lua` shows the retained message in place of
"Complete!", freezes the bar, and settles in phase `"failed"` — never
`"done"`, which is what `scripts/ui_manager_boot.lua` keys its
`finishStartupBoot` transition on, so the main menu is never shown.
Arena boot drains synchronously before anything is on screen, so
`loadingScreen.runArenaStartup()` owns that profile's visible-failure
path and returns false instead of running `finishArenaBoot`.

Gates: hspec `--match "Startup readiness"` and `--match "Startup asset
logging"`.

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

`world-pages` v11 (v10 frozen by #2471 as `PageCoreDTOv10`; v9 frozen by
#917 as
`PageCoreDTOv9`/`WorldGenParamsDTOv7`/`LocationInstancesDTOv5`/
`LocationInstanceDTOv5`/`LocationEncounterDTOv1`; v7 frozen by #916 as
`PageCoreDTOv7`/`WorldGenParamsDTOv6`/`LocationInstancesDTOv4`/
`LocationInstanceDTOv4`; v6 frozen by #1230 as
`PageCoreDTOv6`/`WorldGenParamsDTOv5`/`LocationInstancesDTOv3`), with
`PageCoreDTOv5`/`WorldGenParamsDTOv4`/`WorldIdentityDTOv2`/
`LocationInstanceDTOv2`/`RiverNameDTOv1` frozen — every historical shape
decodes with the source ABSENT, never inferred. #917 changed nothing
about etymology itself: v9 is a frozen migration boundary that carries
each stored source across untouched.

---

## Location instances (#911)

**Placement-time ids, one-way lifecycle (moved from CLAUDE.md).** A
placed location is a persisted per-page record (`Location.Instance`)
keyed by a stable `LocationInstanceId` (from 1), allocated at PLACEMENT
time in the deterministic overlay's `overlayToList` order — never at
stamp time, never from hashmap order — so ids survive save/load and
chunk eviction. Consumers read the STORED values, never re-derive from
the live registry. Lifecycle transitions are one-way (`promoteLifecycle`
refuses backward AND same-state — what makes discovery fire exactly one
event).


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

Persistence: `world-pages` v11, with v10's pre-sub-minute-remainder page
core frozen as `PageCoreDTOv10` and v9's pre-significant-contents
location record frozen as `LocationInstanceDTOv5` (its encounter, still
carrying the clearance-notice flag, as `LocationEncounterDTOv1`) and
v7's pre-encounter one as `LocationInstanceDTOv4`. Each migration adds
NOTHING the payload did not carry — `migrateWorldPagesV9` gains no
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
pickup is rolled back. `Location.Instance.registerLocationSignificantSpawn`
— the pure binding step behind the verb, not a verb of its own — is
WRITE-ONCE
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

There is deliberately NO public binding verb. `world.spawnLocationSignificantItem(instanceId, slot, x, y [, pageId])`
spawns the item AND binds it in one engine call, and it is the only way
an obligation is ever filled. A separate bind-this-ground-item API
would let a caller spawn or pick out an unrelated item of the right
definition, bind it, and take THAT: the location would never spawn its
own guaranteed item — a bound slot is skipped — and the unrelated
pickup would clear the ruin. Neither the definition nor the
duplicate-identity check can see that, because the substitute is
exactly the right kind of item. So Lua chooses only WHERE: the
definition comes from the obligation's own persisted
`lsiItemDefName`, the item is materialized engine-side through the same
`spawnSalvageOnPage` core `item.spawnGround` uses (so a guaranteed
reward is worn by #1421's rules like any other find), and the binding
names the instance that call just created — never one read back off the
ground map, which is the very window a substitution needs. A refused
call spawns nothing, and a binding that loses a race takes its item
back off the ground rather than leaving an unowned duplicate reward.

The binding commits SYNCHRONOUSLY, on the calling thread — unlike every
sibling location editor, which queues to the world thread. That is load-bearing rather
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

**Persistence.** `world-pages` v11. `migrateWorldPagesV9` preserves every
stored value, lifts the encounter's clearance-notice flag onto the
instance, and adds NO obligations — reading them off today's YAML would
owe a materialized world an item it never spawned, permanently blocking
a clearance the pre-#917 build had already granted. The v1
reconstruction discards both for the same reason.
`Location.Instance.significantEntryErrors` is the ONE per-entry rule
set, and both boundaries that can admit an obligation consult it —
component decode through `locationSignificantItemErrors`, and
`Location.Instance.registerLocationSignificantSpawn`, the pure binding
step, which refuses a binding whose
RESULTING entry would fail it. That sharing is deliberate: every rule
below was added because some path could reach a state the other checks
could not see, and two copies is how the next one gets added to a
validator and missed by the live API. The rules are: a slot below 1
(unbindable — the registration boundary refuses a non-positive slot, so
the content spawn would orphan an item on every load for ever); a bound
item id of 0 (the never-minted "no id given" sentinel; because the
provenance rules skip a TAKEN obligation by design, it is the one value
that would otherwise satisfy clearance with nothing ever spawned —
`significantRecovered` refuses to count it either way); a
CONTENTS-SPAWNED instance still owing an unbound slot (unrecoverable —
`spawnContents` returns at its one-time `hasSpawnedLocationContents`
gate and never fills it, and neither the missing-definition check nor
the provenance rules can see the shape); and an obligation marked taken
that names no item. Two SET-wide rules stay with the table walk, since
they are about the relationship between entries rather than any one of
them: a duplicated slot, and same-page duplicate ownership;
`World.Save.Integrity.significantProvenanceErrors` hard-fails an UNTAKEN
obligation whose item resolves on another page, in an inventory or
storage (it cannot be held without having been picked up), or only
NESTED inside a ground container; one whose ground item is the wrong
DEFINITION; and one physical id owed by two obligations. Meanwhile
`significantDanglingWarnings` reports an absent item and tolerates it,
leaving the obligation untaken. Once taken there is no rule at all —
with ONE exception.

That exception is the item-id CURSOR. A bound `lsiInstanceId` at or
above the session's `snapNextItemId` names an identity the monotonic
allocator could never have minted, and that is a hard error for a
TAKEN obligation as much as an untaken one. Every other rule can skip
a taken entry because a taken item is legitimately allowed to be
anywhere or gone — but "gone" is precisely what an unmintable id looks
like to a resolution check, so a forged `taken: true` paired with an
id past the cursor would resolve nowhere, draw at most a tolerated
dangling warning, and then satisfy `significantRecovered`, clearing a
location with no spawn and no pickup having ever happened.
`itemAllocatorErrors` already refuses a live `ItemInstance` above the
cursor, but an obligation is not an item: its id is a bare reference
nothing else in the session has to agree with. The lower bound (0, the
never-minted sentinel) is component decode's, in
`significantEntryErrors`; this upper one has to be session-wide,
because the cursor lives in `core-session`.

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

**The three rules on sight (moved from CLAUDE.md).**

- **Discovery (#780, sight-based since #1230)** is a one-way promotion
  to `discovered`, fired when a player-faction unit SEES the location:
  its visible-tile set intersects the instance's stored `liBounds`,
  seam-aware, one tile being enough (the `discovery_margin` halo is GONE
  from YAML, def, instance, Lua and wire). Sight is
  `Unit.LineOfSight.visibleTilesOnPage` — the SAME calculation
  `unit.getVisibleTiles` runs, minus its `wmVisible` gate, so reveal
  works on a loaded-but-hidden page. Ticks for EVERY loaded page,
  independent of pause; emits exactly one `location_discovery` event. A
  night-scaled radius is intentionally shorter — any distance-sensitive
  expectation over `unit.getVisibleTiles` must pin the clock. Gates:
  `location_content_probe.py`, `location_embark_probe.py`; hspec
  `--match "Location discovery"` / `"Location map icons"` /
  `"Unit.LineOfSight"`.
- **Map icons (#781/#1230)**: all six lifecycle constructors map
  explicitly (`World.Render.Zoom.Icons.locationIconAppearance`):
  `unknown`/`hinted` draw the ONE shared `location_unknown.png` so the
  zoom map never leaks WHAT is there before a unit has seen it;
  `discovered`/`active` draw the def's own `map_icon`;
  `cleared`/`depleted` draw that SAME bitmap darkened — an explicit,
  enumerated exception to the no-tinting rule, confined to the icon
  quad. A def with no `map_icon` places no annotation. Asset gate:
  `tools/location_map_icon_asset_check.py`.
- **Per-unit knowledge (#915)** is the EXPERIENTIAL layer beside that
  CARTOGRAPHIC one, and neither derives from the other: global
  lifecycle = "the player has mapped it", `aiState[uid].knownLocations`
  = "this acolyte knows where it is". Keyed by the durable `(page,
  instance id)` pair — dedup is by IDENTITY, never by distance (don't
  copy `knownWaterSources`' 6-tile rule across). Both layers come from
  ONE containment enumeration in `Location.Discovery`, so they cannot
  drift; awareness ignores lifecycle, so a unit arriving at an
  already-mapped ruin still learns it. Persisted via `lua.unit_ai` v4
  typed refs; v1-v3 decode with the field ABSENT, never inferred.
  Gates: hspec `--match "unit location knowledge"`,
  `location_content_probe.py`.


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

## Page incarnation: in-flight work across a reused page id (#2474)

A `WorldPageId` names a slot, not an object. `main_world` is re-initialised
on every Exit to Menu, an arena replaces a page wholesale, and a
transactional load republishes the whole session — and each of those builds
a **fresh** `WorldState` under the id the old one had. So work computed
against the page that used to be there can still be in flight when its
replacement is live, and a page-id comparison cannot tell the two apart.

**The epoch.** Every fresh `WorldState` mints one process-unique,
monotonic `ChunkGeneration` (`src/World/Chunk/Residency.hs`) into its
`wsChunkResidencyRef`, and that number IS the page's incarnation. There is
exactly one per page: `World.Chunk.Admit.pageIncarnation` is how anything
reads it, nothing advances it, and a new one exists only where a new
`WorldState` does. It is never persisted — it is meaningless across a save.

**Replacing a page id discards its simulation state first.** The epoch is
recorded per page, so a replacement's first seed would otherwise re-label
whatever the outgoing incarnation left under that key — and writebacks derived
from those retained chunks would then carry the *live* epoch and pass the fence
below. Every same-id replacement therefore enqueues `SimDropWorld` for the id
before registering the replacement and long before its first seed:
`WorldDestroy`, `WorldDestroyAll` and `World.Load.Publish` already did;
`WorldInit` and `WorldInitArena` do too (#2477), which is the one replacement
that reaches no teardown of its own. The sim queue is FIFO, so "drop, then seed
the same id" is correct in queue order whatever the overlap. The drop also
clears `swsActive`, which is right — the flag belonged to a page that no longer
exists — and every caller shows a page after initialising it.

### Entity teardown on destroy and same-id re-init (#2476)

**A page's units and buildings belong to the incarnation that admitted
them, and leave with it.**

A page's `WorldState` is replaced or removed by `WorldDestroy` and by
either init path. Its entities are not: they live in the PROCESS-global
unit and building managers, keyed only by a page name the replacement
reuses. Before #2476 neither path touched those managers at all, so the
replacement inherited the old incarnation's unit instances, unit
selection and sim states, building instances, building selection,
destruction effects and outstanding footprint reservations.

**One lifecycle boundary, shared by teardown and admission.** A page
lifecycle transition and an entity admission take the same
process-lifetime mutex, `EngineEnv.pageLifecycleLock`, projected as
`WorldSimCapability.wsPageLifecycleLock` and reached only through
`withPageLifecycle`:

- A **lifecycle transition** — a single-page `WorldDestroy`, and either
  `WorldInit` / `WorldInitArena` that REPLACES a registered page id —
  holds it while it reads `umNextId` and `bmNextId`, enqueues
  `UnitClearPage` / `BuildingClearPage` carrying those readings as
  EXCLUSIVE cutoffs, and removes or replaces the page. An init that
  registers an id no page held replaces no incarnation and enqueues no
  clear. Both init paths release the lock as soon as the replacement is
  registered — worldgen runs outside it.
- An **admission** — `unit.spawn`, `building.spawn` and
  `power.placeNode`, the only three sites that allocate a `UnitId` or a
  `BuildingId` — holds it from its final live-page and page-binding
  revalidation through the id allocation, the footprint reservation
  where it takes one, and the queue insertion. It is the OUTERMOST
  coordination boundary of an admission: no holder may acquire another
  page or entity lock underneath it.

**Old work is retired; replacement work survives.** Teardown stays
queue-ordered (#58): each clear runs behind the spawns already on its
queue. What the cutoff adds is the ability to tell those spawns apart
from the replacement's, which reuse the same page name. An admission
completed before a transition holds an id BELOW the cutoff that
transition captured, so it is either inserted and then cleared, or
dropped first by its handler's existing absent-page guard. An admission
begun after the transition sees the replacement (or the absence), takes
an id AT OR ABOVE the cutoff, and cannot be erased by the clear still
queued behind it. `UnitClearPage` retires only matching-page instances
below its cutoff, those ids from `umSelected`, and their
`utsSimStates`; `BuildingClearPage` retires only matching-page
instances, destruction effects and footprint reservations below its
cutoff, plus `bmSelected` when it names one of them. Neither rewinds an
allocator.

The reservations are why a bare page filter is not enough. A claim is
taken synchronously, ahead of the commit, and is keyed by page (#2326),
so a page-only clear would delete a REPLACEMENT's claim before its own
spawn could consume it and `commitFootprint` would then refuse the very
placement it was taken for.

**Page-bound placement is unchanged (#1602).** A bound commit stays on
the world thread. A binding captured against the replaced visible
incarnation is refused by the existing selection-generation check and
releases its reservation; a bound admission naming a page that is not
the visible head is refused for the same reason, because replacing a
hidden page bumps no generation. A valid binding admitted against the
replacement may commit ahead of the delayed building clear — its id is
at or above the cutoff, so both its claim and its committed instance
survive it.

**A previous incarnation's entities stop being addressable at once.**
The queued clears retire those rows eventually, and "eventually" is not
enough on its own: a page id is a reusable NAME, so until a clear drains
an old unit or building is still in the manager answering to a name that
now belongs to the replacement. Every production verb that resolves an
entity's page — an item drop, a transfer, a construction payment, a
container reveal, a power placement — would keep finding it, and could
spend it into durable state on the replacement that outlives the row the
teardown removes.

So the lifecycle transition applies the retirement DIRECTLY, in the same
locked step, through the same pure bodies the queued clears use
(`Unit.Types.Manager.retirePageUnits`,
`Building.Types.retirePageBuildings`). A page teardown then behaves
exactly as `UnitDestroy` / `BuildingDestroy` already do: the entity is
gone from the manager and every resolver simply fails. One body, two
callers, so the immediate removal and the queued mop-up cannot disagree
about what belonged to the departed incarnation.

This is not the direct clear #58 forbids. That one was unbounded, so a
spawn already queued re-inserted an orphan after it with nothing left to
remove them. These are bounded by the same exclusive cutoff, and the
queued clears still run behind every such spawn. `utsSimStates` is the
one exception: it belongs to the unit thread, which mutates it with a
read-modify-write across a tick, so its removal stays in the queued
handler — which runs ahead of that tick's movement, and
`publishToRender` maps over the instances and would never visit an
orphan anyway.

The retirement precedes the `wmWorlds` write, and on the init paths that
order is load-bearing: registering first would open an interval in which
the replacement is reachable while the departed incarnation's rows are
still in their managers, and every verb that resolves an entity's page
reads those two in exactly that order.

**A spawn already dequeued is refused at its own commit.** Neither half
of the teardown can catch a spawn command that had already left its
queue when the transition ran: the immediate retirement finds no
instance to remove, and the queued clear is enqueued behind nothing. So
`UnitSpawn`, `BuildingSpawn` and `WorldSpawnBoundBuilding` each carry
the page's `ChunkGeneration` incarnation epoch (#2474's existing
per-`WorldState` value, read by the admission from the page it resolved,
inside the lifecycle lock). `handleUnitSpawnCommand` and the shared
`applyBuildingSpawn` compare it against the page's current epoch and
drop a mismatch exactly as they drop an absent page — retiring the
footprint claim with it. "The page exists" and "the page this request
was validated against exists" are different questions, and only the
epoch answers the second.

The epoch is verified again at the commit itself, inside the lifecycle
lock, in the same critical section as the insertion. The first check
happens early in each handler and a great deal of work follows it — stat
rolls, a capacity shed, a footprint commit — so on its own it is a
time-of-check that a transition can outlive. Holding the lock across
revalidation and write is what makes it a fence.

**Entity-to-page resolution reads the page set first.** The transition
retires before it registers, so a resolver that reads the ENTITY first
and the page second can straddle it — old entity, new page — and hand
its caller a pair that lets a durable row (a ground item, a transfer
order, a container observation, a construction receipt) outlive the
entity naming it. `World.Page.Resolve` inverts the order and is the one
way to go from an entity to its live `WorldState`: the page snapshot
comes first, taken through `atomicModifyIORef'` so it is a real ordering
point rather than a plain load two others could be reordered around.
Either the snapshot holds the old page, and the pair is consistent
because the write lands in the state that is leaving, or it holds the
replacement, in which case the entity read happened after the publish
and therefore after the retirement, so a departed row is already gone.
`unitOwningWorldState`, `unitOrderStore`, `containerPage` and
`construction.payMaterials` all resolve this way.

Both fences and the resolution order are gated by built interleavings
rather than raced-for ones: each production body carries one seam (the
shape `Unit.Thread.UnitTickSeams` and `World.Thread.worldTickWith`
already use), and the examples land a transition through it at the one
instant the code cannot see coming. Reverting either revalidation, or
the resolver's read order, fails its own examples.

What remains open is only a caller that reads an entity, is descheduled
across the whole transition, and then performs a mutation it had already
resolved everything for. That is the ordinary read-then-write straddle
every entity verb already has against `UnitDestroy` /
`BuildingDestroy` — not something a page teardown introduces — and
closing it means holding the lifecycle lock across every verb's durable
mutation. That, and `construction.payMaterials`' pre-existing failure to
check its supplier against the page it was handed, are #2474's PIN-3.

**Not a session boundary.** Neither path joins #2291's
`wmTeardownsPending` fence or enqueues `UnitEndSession` /
`BuildingEndSession`: one page ending is not the session ending.
Destroy-all's four-message sequence, load publication, hide, show and
the world-thread placement path are untouched. Transfer orders, power
nodes and container knowledge remain `WorldState` rows and leave with
the replaced page; nothing can add more to the replacement because the
entity that would have named them is already gone from its manager.

Gate: `Page incarnation entity teardown` in
`test-headless/Test/Headless/World/PageIncarnation.hs`, which drives
every admission through its production Lua verb and every transition
through its production handler, and holds the mutex directly to show
each of the six sites blocking on it.

### Simulation writebacks (#2477)

**A fluid writeback batch is applied only to the incarnation it was
computed against.**

The simulation carries the epoch rather than deciding on it. Every sim
message that carries a page's seam topology carries its epoch too —
`SimActivateWorld`, `SimChunkLoaded`, `SimChunkEdited` — read from the
sending page's own `WorldState` by all four senders (show, chunk
admission, edit sync, load publication). The sim records it on the page's
`SimWorldState` (`swsIncarnation`) on every one of those, exactly as it
records the topology, and stamps it onto every `FluidWritebackBatch` it
emits: per tick, and under `SimFastSettleAll`, for stored worlds as well
as active ones. Activation alone would not be enough, because the fast
settle emits for a page nothing has activated.

The world thread — the sole writer of `wsTilesRef` — makes the decision, in
`applyFluidWritebacks` (`src/World/Thread/Command.hs`):

- a batch for a page absent from `wmWorlds` is dropped, as it always was;
- a batch whose epoch is not the live page's own, **or is absent**, is
  refused whole and logged at debug level naming the page and both epochs;
- only a batch stamped with exactly the live epoch reaches the per-chunk
  freshness fence (#1596).

The order matters and is the whole point: the per-chunk fence compares
`fwEditGen` against the page's own `wsChunkEditGenRef`, an absent entry
reading as generation zero on both sides. A replacement page has issued no
live-edit generations at all, so **every** chunk of it reads as zero —
exactly where a batch computed against the previous incarnation was
stamped. The per-chunk fence therefore reads such a batch as fresh, and
cannot be the thing that stops it.

**Refusal is not failure.** A refused batch acks exactly as a page-gone
drop does — `FluidAckApplied` — because it is another of the
nothing-to-do outcomes the handler has always completed normally. A raise
still acks `FluidAckFailed` and rethrows (#2334). This is what keeps
`--dump`'s fast settle bounded: a settle that refused a batch still
completes and exits rather than blocking on an acknowledgement that never
comes.

Gates: `fluid writeback incarnation fence (#2477)` and `fluid writeback
staleness (#1596)` in `test-headless/`, plus `dump fast-settle wait
(#2334)`. `--dump` output is unaffected — the refusal only ever suppresses
a write that would have been wrong — so `python3 tools/world_check.py
--quick` passes with no rebaseline.

## Tile-coordinate seam frame (#1175/#1230)

Enforced by hspec `--match "World.Render.PickSeam"` /
`"World.DesignationSeam"` / `"a seam-frame unit"`. The contract is also
stated in full on `World.Render.HitTest`. `src/World/CLAUDE.md` and the
root's domain list keep the canonical-coords rule, the rectangle
exception and the lookup-wrap rule; this is the full enumeration.

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

**The shared policy, the lax AI verbs, and the reveal rule (moved from
CLAUDE.md).** Design authority:
[`docs/unified_item_transfers.md`](unified_item_transfers.md). ONE pure
policy (`src/Unit/Transfer.hs`) decides whether exact item instances may
move between two endpoints (a unit inventory or a built building's loose
storage, on BOTH sides; direction DERIVED from the pair): Chebyshev ≤ 1
between occupied RECTANGLES, capacity weighs the actual instance,
batches are ordered and report per-item outcomes, and no item ever
half-moves. The lax AI verbs
(`transferItemToUnit`/`transferItemToBuilding`/`depositToCargo`/
`withdrawFromCargo`) are a SEPARATE path the fetch/repair/medic ladders
depend on — never route AI work through the strict one, and never
delete them. What is unchecked there is adjacency and receiver
eligibility (and unit-to-unit capacity), **never the world PAGE**
(#1673): all four refuse a cross-page endpoint pair, mutating nothing
and revealing nothing, which is the floor `Unit.Transfer.reachable`
holds even where it defers adjacency. The AI finders page-qualify every
candidate against the ACTING unit (`scripts/unit_ai_page.lua`) instead
of trusting the active page that `unit.getAllIds` /
`building.getActiveIds` / `craft.getBills` each snapshot separately, and
revalidate every PERSISTED building reference (`deliveryClaim.bid`,
`craftJob.bid`, `repairJob.bid`) before it can steer a walk or reach a
verb. Gates: hspec `--match "Unit cargo"` / `"AI page pairing"`.

**TWO player modes, ONE commit policy.** Mode B queues a durable order
and Mode A commits on the spot, but both build the IDENTICAL request
and both reach `checkTransfer`/`commitTransfer`. The player-facing
IMMEDIATE paths retired with #1249 must not come back — the Store /
Retrieve gestures replaced them and NEITHER requires adjacency; only
the PLAYER paths retired, the verbs stay registered for the AI (D-7).

**Contents are REMEMBERED, never live (D-2).** A container window
renders the player's last observation. Exactly four things reveal
(`Building.Knowledge.Live`): a completed transfer commit into or out of
the container, the lax AI cargo verbs, a Mode A session OPENING on it
(`building.refreshContainerKnowledge`'s only in-game caller), and the
first completion of a storage-capable building (seeds KNOWN-EMPTY
because the player watched it go up). Walking past, selecting,
right-clicking and opening the window reveal NOTHING; every unit-driven
reveal is gated on `isPlayerCommandable`; knowledge is player-global,
never per-unit.

Beyond the gates listed below, the reveal rule is pinned by hspec
`--match "Container knowledge"`, and the arc's INTEGRATED gate is
`tools/unified_transfer_probe.py` (#1255, manual-only `needs-gpu`): one
fixed-seed session proving an exact instance moves both ways between
all three endpoint classes through BOTH modes, plus the partial batch,
the reveal rule, one widget rendering every container view, and a Mode
B order surviving a fresh-process reload while a Mode A session does
not.


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

## Nested ownership moves (#2487)

**One pure boundary owns every nested insert and remove.** Design
authority: [`docs/portable_loot_containers.md`](portable_loot_containers.md)
PLC-4, D-1, D-4, D-5, D-16. `src/Item/Ownership.hs` is the only place a
production path may put an `ItemInstance` into, or take one out of,
another instance's `iiContents`. It is pure and EngineEnv-free, the same
shape `Unit.Transfer` has: the caller projects the live managers into an
`OwnershipScene` (the edited tree, everything else the owner carries, the
root's weight capacity, and `itemTotalWeight` partially applied to the
live `ItemManager`) and applies the list the policy returns. PLC-8 and
PLC-9 are its first production callers.

**Five other writers exist, in four modules, and none is a move.**
`Item.Materialize.materializeNode` MINTS a tree (#1418's one mint
boundary), `World.Save.Component.PageActivity.fromItemInstanceDTO`
REBUILDS one already materialized, `Item.Temperature.coolItem` RE-VALUES
temperatures in place, and the two medical draws
(`Engine.Scripting.Lua.API.Units.Medical.consumeBandages` /
`consumeKitFill`) DESTROY contents rather than re-owning them. That
allowlist is scoped per FUNCTION, not per module, so a later unrelated
writer in the same file is still a finding. It holds nine entries: those
five exceptions plus the four functions inside the boundary itself.

**What a move must satisfy.** Exact instance identity survives —
`iiInstanceId` and every descendant, in authored order. An insert needs
the destination's own `isWeightCapacity` (recursive weight of its direct
children) and `isBulkCapacity` (direct children's own external
`iiBulk`, D-5) to hold, both bounds inclusive. Every weight-bearing
ANCESTOR above the destination is revalidated for weight too — the
immediate parent fitting proves nothing — while bulk is charged at the
immediate parent ONLY, because a container's external bulk is fixed. The
root owner is revalidated last, in the `Unit.Transfer.fits` sense where
a limited capacity of 0 means no room rather than unlimited; the ground
carries `RootUnlimited` and passes no weight limit at all.

**Absence fails closed, and each absence is its own refusal.** A
destination whose `iiStorage` is `Nothing` accepts no insert, and
neither does anything below an ancestor that declares none — which today
is every shipped kit and toolbox, since `data/items` authors no
`storage:`. A candidate whose `iiBulk` is `Nothing` is never a
candidate, and a destination already holding such a child refuses
because its used bulk cannot be summed. Removal charges no capacity at
all, so a legacy tree can always be emptied; that asymmetry is D-30's.

**Cycles, duplicates, and the final arrangement.** A move into the
instance itself or into one of its own descendants is refused as a
CYCLE, and `moveInstance` decides that BEFORE its internal detach,
against the tree that still holds both ends of the move — after the
detach the destination has simply vanished from it. `insertInstance`
independently rejects a target inside the candidate VALUE, which needs
no tree and so also covers a candidate arriving from another owner;
neither check subsumes the other.
Duplicate detection compares every id in the moved SUBTREE, descendants
included, against the whole destination tree. Because `moveInstance`
removes before it inserts, requirements about the post-move arrangement
fall out: a shared ancestor is never charged for the subtree twice, a
relocation within one owner nets zero against the carrier, and the
instance never reads as a duplicate of itself.

**Refusal is atomic, and rollback is not an insert.** A refused move
returns the reason and the caller's tree exactly as it was, including
when the refusal lands after the internal detach — nothing is
duplicated, dropped, or reordered. `OwnershipRemoval` records the source
parent AND index, generalizing `Unit.Transfer`'s flat `tpIndex`, and
`restoreRemoval` splices the instance back there checking NO capacity:
a rollback must restore a bandage into a first-aid kit that would refuse
an ordinary insert, or a failed transaction becomes a lost item. That
guarantee is against the corresponding post-removal snapshot and
promises nothing about arbitrary intervening mutations. Ordinary
remove-then-reinsert goes through `insertInstance` instead, needs an
eligible parent, and appends.

Every refusal is enumerable (`allOwnershipRefusals`,
`ownershipRefusalId`) so PLC-9's endpoints can surface it verbatim.
Gate: hspec `--match "Item.Ownership"`, whose capacity cases are each
mutation-tested by loosening the fixture bound one unit past the guard
and asserting the verdict flips, and whose structural writer guard holds
the allowlist above.

## Portable container knowledge (#2512)

What the player REMEMBERS about a portable container — a crate, a
toolbox, a kit — as opposed to what it physically holds. Keyed by the
item's own `iiInstanceId`, so the record follows the crate across pages
and owners and is never copied on a move
(`docs/portable_loot_containers.md` D-7/D-13/D-24). The model is
`Item.Knowledge`; the live owner is the SESSION-scoped
`World.State.Types.wmPortableKnowledge`. `Building.Knowledge` is the
building-keyed, page-scoped sibling and is deliberately a separate
type.

**Four states, never conflated** (`portableKnowledgeStateId`):

| State | Id | Means |
|---|---|---|
| never-inspected | `unknown` | nothing is known — no record, or a record carrying neither observation |
| weight-only | `weight-only` | hefted, never opened |
| known-empty | `empty` | opened, and there was nothing in it |
| known-contents | `known` | opened, and this is what was inside |

`empty` and `unknown` are different facts and must never be rendered as
one another, exactly as in the building layer. The two ids the building
projection also has carry the SAME spellings, so one window renderer
consumes either.

**Two independently stamped observations.** A record holds an optional
weight observation (the recursive `itemTotalWeight` of the WHOLE crate —
its own mass, its fill and everything nested — plus the game-time it was
weighed) and an optional contents observation (full `ItemInstance`
COPIES of `iiContents` plus the game-time they were seen). The rules:

- Weighing records the weight and its stamp and NOTHING else; any
  existing contents observation survives untouched with its own older
  stamp.
- Opening records the contents, the weight, and BOTH stamps at the
  current time — you cannot see inside a crate without holding it — and
  REPLACES the whole record rather than merging.
- So the stamps diverge in one direction only: a fresh weight over
  older contents. `weighedAt < revealedAt` is not reachable.
- An observation never mutates the live item, and a container NESTED
  inside an observed crate gets no record of its own until it is itself
  observed.
- Capacity is never remembered: it is read live from the located
  instance's `iiStorage`, and is simply absent when the instance cannot
  be located or declares none — never a fabricated `0`.

**The locator.** `World.Item.Locate` answers "where is this instance,
and what does it look like now" across every page's ground items, unit
inventories/equipment/accessories and building materials/storage,
recursively through nesting, using the container set
`World.Save.Types.pageItemContainers` enumerates and the walk
`flattenItemInstances` performs — so a container added to a unit or a
building reaches it from the same one edit that makes the save system
see it. Hidden pages are walked like visible ones. A REMEMBERED-only id
never resolves: an observation holds copies, and a copy is not a live
entity.

**Persistence.** The optional session component `portable-knowledge`
(v1, `World.Save.Component.PortableKnowledge`) — the third optional
component in the static Haskell registry and the first SESSION-scoped
one. Absence restores the empty map (every crate never-inspected);
a present payload that is malformed or at an unsupported version fails
the load. `csValidate` refuses a remembered weight or either timestamp
that is not finite and non-negative, and deliberately does NOT re-derive
the weight from the remembered contents.

Remembered instance ids are HISTORICAL OBSERVATIONS: excluded from
`allItemInstanceIds`, the allocator bound, the duplicate-live-id check
and live `item_instance` resolution — they may legitimately overlap a
live id or sit above the allocator. Their def names remain ordinary
content references, validated by
`World.Save.Types.missingPortableItemDefReferences` BEFORE staging, so
the scrub below cannot quietly discard the evidence for a rejection.

**The scrub.** At load, `World.Load.Stage` drops any record whose
instance is absent from the REPLACEMENT session's own complete live item
enumeration, with one diagnostic naming the dropped ids — never a load
failure, mirroring #1087's demolished-container scrub. That enumeration
is read off each staged page's ground ref AFTER every reconciliation
pass, not off the decoded map written into it earlier: a self-cleared
construct designation refunds its paid materials onto exactly that ref
during staging, and a refunded item is as live in the replacement
session as any other.
`World.Load.Publish` then installs the scrubbed map as part of the
replacement `WorldManager`, so a load REPLACES the memory wholesale and
an absent payload CLEARS it. Exit to Menu empties it in the same atomic
update that clears the page set: an instance id is only unique within a
session.

**Lua surface** (`item.*`, `Engine.Scripting.Lua.API.Items.Knowledge`):
`getContainerKnowledge(instanceId)` →
`{state, items, storedWeight, weighedAt, revealedAt, capacity}` with
every field but `state` present ONLY when known — reading a missing
field as `0` is the conflation the four states exist to prevent, and an
OBSERVED-empty crate answers an EMPTY `items` table rather than none.
`observeContainerWeight` / `observeContainerContents` locate the
instance and record, answering false when it cannot be found;
`forgetContainerKnowledge` drops a record and does NOT require locating
anything, since a destroyed crate's memory is the one a caller most
needs to clear. Each verb refuses a non-number argument outright:
`Lua.tointeger` coerces, so the string `"47"` must not act on crate 47.

**Who writes the map.** The three mutating verbs MEASURE on the calling
thread and ENQUEUE a `WorldRecordPortableKnowledge` command; the world
thread merges it (`World.Thread.Command.Basic`). That split is the point,
not an implementation detail. Measuring must happen where the located
instance and the clock were read, or the record would describe the crate
as it is when the command runs rather than when the player looked.
Merging must happen on the world thread, which owns the session state
`WorldManager` carries: the two places that REPLACE that state wholesale
— a load publish and an Exit-to-Menu teardown — both run there, so a
caller-thread write could land a departed session's crate memory in the
session that replaced it, while a queued one is ordered against them by
FIFO. It is also why the command carries a `PortableObservation` rather
than a finished record: whether a weigh preserves an older contents
observation is a fact about the map at merge time, which the measuring
side does not have. The consequence for callers is that a verb's `true`
means accepted, and the record is readable after the world queue drains
— the same contract `world.markLocationContentsSpawned` has.

**FIFO is not enough on its own**, which is what `wmSessionEpoch` is
for. A turn that queues `WorldDestroyAll` and THEN observes locates the
outgoing crate perfectly well — the teardown has not run yet — so
ordering alone would clear the map and then insert the departed
session's memory straight back into it, where a reused instance id could
pick it up. So every mutating verb reads the epoch from the SAME
`WorldManager` it located in, the command carries it, and the handler
refuses a command whose epoch has moved.

The hazard that guard closes is the TEARDOWN one specifically:
`WorldDestroyAll` is an ordinary queued command, so a turn can queue it
and then observe, and nothing else would stop the insert landing after
the clear. A load publish is already covered by a different mechanism —
`World.Thread.processAuthorizedSave` flushes the world queue and
DISCARDS every non-authorized command when a `WorldLoadPublish` is in
it, so no observation queued against the outgoing session survives to
run against the restored one. The epoch is nonetheless bumped by the
publish too, in the same atomic step that installs the new page set, so
that it means "which session is this" for every reader rather than
"which teardown was this", and so the refusal does not depend on that
discard staying exactly as it is. A forget carries the epoch for the
mirror-image reason an observation does: it must not reach across a
boundary and delete a same-numbered record the next session
legitimately owns.

PLC-7 ships no gameplay caller — pickup and open are PLC-8's, the window
is PLC-9's.

Gate: hspec `--match "Portable container knowledge"`, plus
`python3 tools/persistence_inventory_audit.py` and
`python3 tools/save_compat_audit.py` for the component's inventory rows
and fixture.

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
   arrives (`docs/history/expedition_survival_calibration_2026-07.md`
   observation E1).
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
position (#1216, SURV-4), retiring the "it does not" of observation E3 in
`docs/history/expedition_survival_calibration_2026-07.md` — so the pause
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

**The rules on sight (moved from CLAUDE.md).** Autosave is OFF by
default (`config/save_default.yaml` + key-level `save.local.yaml`
overlay; Settings → General edits it). `scripts/autosave.lua` owns the
WALL-CLOCK interval and fires only when `uiManager.isGameplayView()` —
a deadline reached in a menu / with no world / mid save-or-load is
SKIPPED silently, and menus never suspend or reset the cadence.
Interval autosaves ride the SAME save transaction — they only add a
request-time `AutosaveRequest` (pre-request pause, visible time scale,
player-intent generation) plus the durable `smAutosave` classification
`engine.listSaves()` exposes. Slot ownership, rotation order and the
failure disposition follow.

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

`src/World/Save/CLAUDE.md` carries the architectural bullets (the Lua
save-module registry, `publishGeneration`'s write-fsync-revalidate-rotate
transaction, the whole-session load transaction, and the typed-reference
integrity graph). This is the phase and failure detail it defers.

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

**Every owner parks at its own final-pass acknowledgement (#2221).** The
barrier establishes a WAIT, not mutual exclusion: an acknowledgement
records only that the owner's current tick finished, and the boundary
still needs every OTHER owner's final acknowledgement plus the
initiator's `reachSnapshot`. So an owner's tick-boundary gate is
`Engine.Save.Barrier.ownerGated`, never the bare `captureLocked` — from
its own acknowledgement of the FINAL required quiescence pass, that
owner performs no gated (persistent-state-mutating) work until the
transaction reaches `SaveEncoding` or a terminal failure. Without it the
acked owner starts a fresh unlocked tick that can still be running when
the snapshot is captured (save) or when `publishStagedSession` swaps the
session refs (load).

The park is per-owner and final-pass-only. During the final pass an
owner that has acknowledged is parked while owners that have not are
still free to finish — otherwise the boundary would be unreachable —
and earlier passes park nobody, so the multi-pass causal drain is
unchanged. Acknowledgement stays a state update, never a block: parking
gates WORK only, which is what lets `SaveUnit` and `SaveBuilding` share
one loop and still both acknowledge. Two standing exceptions run inside
the window because they ARE the transaction: the world owner keeps
consuming its authorized `WorldSave`/`WorldLoadPublish` commands, and
the Lua transaction driver (`saveWorldFn`'s component collection after
`reachSnapshot`; `handleLoadStaged`'s Lua apply and `WorldLoadPublish`
queueing) runs inline on a thread already blocked in the driver call.
Ordinary Lua ticks are gated like any other owner's.

`reachSnapshot` refuses to declare the boundary until the final pass is
actually complete (`ssAcknowledged ≡ ssOwners`), and refuses to re-close
a window `releaseCaptureLock` already opened. Release and failure clear
the park immediately: #758's early release resumes every owner once the
snapshot is captured, validated and encoded but before disk I/O
finishes, and a failed or aborted transaction unparks everyone with no
phase in between where gameplay briefly runs.

What a parked owner leaves queued is disposed of by transaction kind: a
save DEFERS it (the live session is the same session before and after,
so it simply runs after release, and is therefore absent from the
snapshot that transaction captured), while a load publish DISCARDS it
(`World.Thread.partitionAuthorized` for the world queue,
`World.Load.Publish.discardStaleQueues` for the rest) because it was
queued against the session being replaced.

**Parking is not a licence to DISCARD.** Parking an owner destroys
nothing — whatever stays queued is still there however the transaction
ends — so it is safe from that owner's own final acknowledgement. A
discard is irreversible, and there is NO moment on an owner's own
timeline that is a correct one for it:

- Before the publication commits, a load can still fail —
  `applyLuaLoad` runs AFTER `reachSnapshot` and can raise — and that
  failure leaves the OLD session live and, by
  `docs/persistence_contract.md`, unchanged, with its queued work still
  owed a run. Neither the park nor the capture boundary is late enough.
- After it commits, the REPLACEMENT session is already producing:
  `publishStagedSession` queues `LuaSaveLoaded`, whose `onSaveLoaded`
  handlers legitimately enqueue new-session work. A flush keyed off any
  load state would destroy that along with the backlog, because the
  world thread publishes and releases on its own schedule and an owner
  is not guaranteed a tick in between.

So the Lua-to-engine discard is a CUTOVER, taken exactly once at the
one instant where "everything queued" and "the replaced session's work"
are the same set: inside
`Engine.Scripting.Lua.Thread.Dispatch.commitLoadPublish`, which cuts
over (`Engine.Scripting.Lua.Message.discardStaleLuaToEngineWork`),
announces `LoadWaitingPublish`, and queues `WorldLoadPublish` as one
action. It runs on the PRODUCER thread — `luaToEngineQueue` is written
by the Lua API, on the Lua thread executing this call — against a
consumer that is provably parked, since the render owner is a
registered owner of this transaction and the boundary was reached only
after its final acknowledgement had already landed. The render owner
therefore never discards at all: it stops consuming while parked and
resumes afterwards. The world owner's `processAuthorizedSave` discard
needs no cutover of its own: it only ever triggers on a batch that
actually contains the `WorldLoadPublish`, and `discardStaleQueues`
likewise runs inside the publish itself.

Gates: hspec `--match "save snapshot barrier"` (the bare-barrier park
protocol in `Test.Headless.Save.Barrier`, and the owner-loop
consequences driven through the real tick entry points in
`Test.Headless.Save.OwnerPark`); `tools/save_barrier_probe.py`,
`tools/transactional_load_probe.py`.

---

## Enum append-only audit: baseline and payload normalization

Enforced by `tools/enum_append_only_audit.py` (CI + `make ci`, with its
own `--self-test`). `src/World/Save/CLAUDE.md` states the rule and the
two hard facts about the baseline (it is GENERATED; a pure append
ratchets it with `--update-baseline`). This is the rest.

**Coverage.** Most guarded types are on the save wire, and fewer again
are named by a live component; the rest are guarded pre-emptively, which
is the point of keying on the `Serialize`-via-`Generic` instance rather
than on save reachability. How many of each is deliberately not written
down here. Adding a guarded type moves one, two or all three of them,
depending on what reaches it — nothing for an off-wire type but the
guarded total, no component for one carried only by a bare wire root —
so a transcribed set goes stale unpredictably, which is what this
paragraph itself did while telling its reader not to hand-count
(#2299). The audit's success line reports all three on every run,
derived from the guarded set it discovered and from
`docs/save_compat/enum_baseline.json`'s per-type `onSaveWire` /
`components` fields.

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

**Retaining a version means still DECODING it (moved from CLAUDE.md).**
Anything beyond appending is a per-component migration, never a
`currentSaveVersion` change — that marker does not gate on-disk
compatibility. Find EVERY component storing the enum — `Direction` is
stored by both `units` (`UnitInstanceDTO.uidFacing`) and `unit-sim`
(`UnitSimStateDTO.simFacing`), while `Pose` and `UnitActivity` are
`unit-sim`'s alone — and for each: raise its `csVersion`, freeze the
outgoing DTO, and register that frozen type in `csOlderVersions` via
`atVersion` with an explicit migration. `componentCodec` derives
`ccInputVers` from those declarations, so the reader gains the new
version while retaining every version it already accepted.

Freezing the OUTGOING DTO is only half the job: **every** version left
in `csOlderVersions` needs a wire type that reaches a frozen COPY of the
constructor order that version was written with — transitively, the
`Pose` nested in `UnitActivity` included. Today's frozen DTOs do not
satisfy that. `UnitSimStateDTOv1` (which `unit-sim` v1 AND v2 both
decode through) still names the live `Pose`/`UnitActivity`/`Direction`,
and `UnitInstanceDTOv1.uid1Facing` still names the live `Direction`, so
a reorder that froze only the current shape would decode every retained
legacy payload against the new order anyway. `unitSimCodec`'s v1/v2
entries are the exemplar for version dispatch and explicit migration
only — no codec has needed a frozen enum yet, so they do not
demonstrate that half.

---

## Local-config writes: one atomic-replace helper (#2202)

Enforced by hspec `--match "Core.ConfigWrite"` and
`tools/config_write_audit.py` (CI + `make ci`).

**Every write under `config/` goes through
`Engine.Core.ConfigWrite`.** It writes a fresh, uniquely named
temporary in the TARGET'S OWN directory, `fsync`s it, `rename(2)`s it
onto the target, and then `fsync`s the target's DIRECTORY — a file's
own `fsync` says nothing about the directory entry naming it. The
primitives are `World.Save.Storage.Durable`'s, reused rather than
reimplemented; the save transaction itself
(`World.Save.Storage.publishGeneration`) is not reused, because it is
bound to the save-slot envelope and its `.prev` rotation. The
durability stance is the one `World.Save.Storage` already documented —
plain POSIX `fsync`, never macOS's `F_FULLFSYNC` — and is not reopened
here.

Six writers route through it: video (`Engine.Graphics.Config`),
keybinds (`Engine.Input.Bindings`), notification overrides and their
boot-time materializer (`Engine.Asset.YamlNotifications`), autosave
(`Engine.Save.Config`), and both of `Engine.Core.Init`'s legacy paths —
the migration copy and the #1937 neutrality record. The migration copy
matters most: migration is gated on the local file's mere EXISTENCE, so
one interrupted partial copy used to suppress every later migration
attempt permanently.

**Deleting is a publication too.** The autosave family's "no overrides
left" state is the ABSENCE of `config/save.local.yaml`, not an empty
document, so `removeConfigFile` unlinks and then `fsync`s the parent
directory before reporting success — an unlink is a directory-entry
change exactly like the publish rename. Without that sync a crash after
the reported success could leave the old file on disk and restore
autosave settings the player had just reset. It distinguishes "removed"
from "nothing was there" (nothing changed, so nothing is synced),
reports a failed unlink without claiming the file is gone, and reports
an unconfirmed post-unlink sync as `Left` while the unlink itself
stands.

**Failure is stated by phase.** Every pre-rename failure leaves the
previous target byte-identical. A directory-sync failure happens AFTER
the rename, so it returns `Left` — durability is unconfirmed — while
the visible target is the COMPLETE new file, never a partial one.
Synchronous filesystem failures become a descriptive `Left` naming the
path and the cause; ASYNCHRONOUS exceptions clean up the temporary and
are rethrown, because
`Engine.Scripting.Lua.API.Internal.registerLuaFunction` re-throws them
on purpose so shutdown's `killThread` still reaches the Lua thread.

**Cleanup ownership spans every pre-rename phase, and a cleanup failure
is never swallowed.** The temporary is owned from the moment its name is
claimed until the rename consumes it, under an `onException` that covers
every escaping exception whatever its source — a rethrown asynchronous
one included, which is exactly the path a per-branch discard misses. If
the removal itself fails, its warning is appended to the `Left` already
being returned: "every returned outcome leaves no temporary" is either
true or said out loud, never quietly false.

Ownership starts one step earlier than that, inside
`World.Save.Storage.Durable.claimUniquePath` itself: it opens a real
file and only then removes it, so the caller cannot own the placeholder
before the claim returns its name. The claim therefore runs under
`mask_` with an `onException` covering `hClose`'s one interruptible
point, and the exception always propagates — this closes a leak, never
a shutdown path. Every caller of the primitive gains that, the save
transaction and the generated-world library included.

**Outcome vocabulary.** Every Haskell writer returns `Either Text ()`.
`engine.saveVideoConfig`, `engine.saveKeybinds`,
`engine.setNotificationOverrides` and `engine.setSaveConfig` each
return `true` on success and `false` on failure, log the path and the
cause at warning level, and NEVER raise a Lua error for a filesystem
failure — a raised one used to abort `data.save()` before autosave
settings were persisted. Higher-level boot workflows (`loadOverrides`,
`migrateLegacyConfig`, `recordNeutralLegacy`) keep their own return
types but consume the outcome explicitly, and never log a success line
after a `Left`.

**A failed write must not move a baseline either.** `data.save()`
refreshes Settings Back's persisted video baseline
(`data.captureSavedVideo`) only when `engine.saveVideoConfig()` returned
true. Adopting values that reached the live ref but never reached disk
would leave Back with no way back to the configuration that is
genuinely saved — the same class of loss the durable write exists to
prevent, one layer up.

**Live state on a failed write is unchanged, per family, by design.**
Video and keybinds keep the already-applied live ref; notifications
keep the live merge (the YAML is the next-session record, the in-memory
config routes the next emit); autosave keeps its existing semantics —
it has no live ref, and the Lua scheduler was already notified
independently. Rolling any of them back would take an applied setting
away from the player in order to report a disk failure, which is
strictly worse than losing it at the next boot with a warning.

The audit is STRUCTURAL rather than a text filter for a reason: the
issue's own `rg 'encodeFile|writeFile' src app | rg 'config/'`
acceptance returned NO MATCHES on the defective snapshot, because the
raw write and the `config/` literal sat on different lines and three of
the writers never name a config path at all. It reasons about modules
instead — the config-persistence set must contain no raw
write/copy/rename and must import the helper; any other file naming a
`config/` literal must contain no raw write; and the helper must still
call the durable primitives it is built from. `removeFile`,
`removePathForcibly` and `removeDirectoryRecursive` count as raw writes
inside the config-persistence set, so the deletion side cannot regress
past the durability contract either.

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
(no boot). `app/CLAUDE.md` states the rule, the flags it covers, and
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
`tools/debug_console_boot_probe.py` (CI-eligible). The root `CLAUDE.md`
§Launch rules and `app/CLAUDE.md` keep the rule —
`--headless`/`--offscreen` ABORT when the listener can't start; this is
the detail.

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

## Monotonic elapsed time (#2204)

Four interval consumers turn "seconds since my last sample" into
simulation or pacing: the render loop's frame timing
(`Engine.Loop.Timing`), the world tick (`World.Thread`), the unit tick
(`Unit.Thread`), and the Lua scheduler
(`Engine.Scripting.Lua.Util.nowSeconds`, whose six call sites are all
scheduling). All four read `GHC.Clock.getMonotonicTime` through
`Engine.Core.Clock`. The deliberate WALL-clock consumers —
`engine.realTime()`, log timestamps, save-metadata timestamps, and the
seed mixed into `World.Page.GeneratedId` — are not elapsed-time
consumers and keep their own sources.

`Engine.Core.Clock` is the ONE boundary. `maxElapsedStep` is exactly
`0.25` s; `sanitiseElapsed` maps a negative, `NaN`, or infinite
difference to `0`, passes `[0, 0.25]` unchanged, and caps anything above
at exactly `0.25`. Every raw difference the four consumers use passes
through it — the render loop's pre-sleep `frameDt` and post-sleep
`actualDt` SEPARATELY (pacing uses the first; the stored `deltaTime`
and FPS window use the second), the world tick's `dt`, and the unit
tick's simulation `dt` and its execution `elapsed`. A consumer that
retains a raw clock sample replaces it with the CURRENT raw sample after
every measurement (`sampleElapsed`), invalid, negative, and over-cap
ones included, so excess above the cap is DROPPED, never carried into
the next tick as debt. Catching up a host sleep is deliberately out of
scope.

The render loop takes its initial sample in
`Engine.Loop.Mode.runStartupHandshake` (`primeFrameTiming`), before any
frame is measured, so the first stored `deltaTime` is a real bounded
frame, never a difference from the `0.0` in `defaultEngineState`. An
unpaused world tick advances the calendar by exactly
`sanitised seconds × effectiveTimeScale` game-minutes (an over-cap
sample by `0.25 × effectiveTimeScale`) and can never hand
`advanceWorldClock` a negative value. Lua's deadline rule after a due
callback (`TickPolicy.advanceTick`) is stated in interval multiples and
is independent of the cap: lateness below one complete interval keeps
#1695's cadence (`oldDeadline + interval`); lateness of one complete
interval or more drops the missed executions (`now + interval`), so a
script whose clock jumps across several intervals runs once, leaves a
deadline strictly later than the jumped clock, and is not due again
when the pass repeats at that same `now`.

**The sub-minute part of that world-tick advance is RETAINED, not floored
away (#2471).** The stored clock (`WorldTime`) holds whole hours and
minutes, and every tick used to floor the advance straight back into it —
so at the shipped default scale, one game-minute per real second against
the 0.25 s cap, no admitted tick ever contributed a whole minute and the
calendar never moved at all; at higher scales the same elapsed time
advanced it by different amounts depending on how the worker happened to
partition it. Each page's `wsTimeRef` now holds a `PreciseWorldTime` — the whole
minutes plus the leftover fraction, always in `[0, 1)` game-minutes — and
`advanceWorldClock` threads it through, so equal admitted elapsed time
advances the calendar by the same duration however it is partitioned.

Both halves live in ONE ref deliberately. The world thread writes the
clock while other threads read it (`Unit.LineOfSight` on the unit thread,
`Engine.Scripting.Lua.API.Power` on the Lua thread), so with two refs a
reader landing between the writes of a minute carry would pair the new
minute with the previous remainder — a clock no tick ever produced, which
would run the sun angle backwards. One ref makes every observable state a
whole one, structurally: there is no setter for half a clock.

The rules that go with it:

- **Ownership.** Per page, written only by the world thread — the tick,
  the queued `WorldSetTime`, and load staging — exactly like `wsDateRef`
  beside it, and always as one whole-clock write. Only `wmVisible` pages
  are ticked, unchanged by #2471: a hidden page keeps the remainder it
  was last left with and never catches up.
- **Boundaries.** `WorldSetTime` names a whole minute and therefore
  CLEARS the remainder; `WorldSetDate` leaves it alone (it changes no
  time of day). A fresh page starts at zero, a paused tick rewrites the
  same value it read, and a time-scale change keeps the accumulated
  remainder and applies the new scale to later elapsed time only.
- **Presentation.** Every whole-minute consumer sees the floor, and a
  rollover across minute, midnight, month or year carries the remainder
  rather than dropping it. `preciseSunAngle` is what a live page's solar
  consumers (rendering, line of sight, power) read, and it takes the
  WHOLE clock rather than a minute and a remainder separately:
  nondecreasing within a day, equal to `worldTimeToSunAngle` at a zero
  remainder, and still wrapping at midnight.
- **Numerics.** The per-tick product `scale × dt` is EXACT in `Double`
  (two `Float` significands are 48 bits against 53 available). Its
  WHOLE-minute part is then split off and carried in exact `Int`
  arithmetic, and only the leftover fraction — below one minute — is ever
  added to the retained remainder, itself below one minute. So the single
  rounding a tick performs is on a sum in `[0, 2)` **whatever the scale**,
  bounded by `World.Time.Scale.clockTickErrorBound` = half an ulp below
  2 = 2⁻⁵³ game-minutes. Reaching a whole minute of drift would take over
  9×10¹⁵ ticks.

  That split is a correctness requirement, not an optimisation.
  Recombining the stored minutes with the remainder before flooring would
  round `1439 + nextDownDouble 1` to 1440, so a page at 23:59 holding
  `maxClockRemainder` would cross midnight and roll its date on a PAUSED
  tick that advanced it by nothing.

  That split is exact at every representable input, and needs no cutoff
  of its own to be: `added` is the exact product of two `Float`s, so it
  carries at most 48 significant bits, and a 48-bit value at or above 2⁵³
  is necessarily an integer already — the floor is the value itself and
  the fraction is exactly zero. So `worstCaseMinuteTotal` guards `Int`
  representability and nothing more, and `maxTimeScale` is the largest
  scale that survives it. `worstCaseDayCount` is that minute total
  divided by `clockMinutesPerDayInt`, and is a BOUND rather than a
  prediction: it includes the minute a remainder can carry, which a given
  start may not.

  The STORED clock is checked too (`clockStartMinutes`). Nothing
  range-checks `wpsTimeHour`/`wpsTimeMinute` — the component validator
  deliberately does not judge them and staging stores them as they came —
  so a corrupt save can present `hour = maxBound`, and a bare `hour * 60`
  would wrap to a small negative and "advance" a clock the contract
  promises to leave alone.
- **Totality is unchanged.** A refused scale, a refused elapsed value, a
  whole-minute count this tick cannot represent, a STORED clock whose own
  minute total will not fit an `Int`, a minute total that will not fit,
  or an overflowing calendar carry all return the exact input time,
  remainder and date with zero rolled days.
- **Persistence.** `world-pages` v11 carries it (`pcTimeRemainder`,
  `wpsTimeRemainder`); `migrateWorldPagesV10` loads every earlier payload
  with none, which is the value those saves actually recorded. The
  component validator deliberately does not judge it: an out-of-domain
  stored value is repaired to zero by `World.Load.Stage`, with a warning
  naming the page, rather than costing the player the rest of the save.

Gate: hspec `--match "monotonic elapsed-time contract"`, which drives
the real `updateFrameTimingWith`, `worldTickWith`, `unitTickWith`, and
`runDueScripts` with an injected clock; production callers pass
`monotonicSeconds`. The retained-remainder rule has its own gate
beside it — hspec `--match "Calendar retains sub-minute progress"`, which
drives that same real `worldTickWith` across long irregular schedules
against an independent exact-arithmetic oracle, samples the published
clock from a concurrent reader across 600 minute carries, and takes the
persistence half through the real component codec, `validatePages` and
`World.Load.Stage`. Live save evidence is
`tools/persistence_contract_probe.py`, whose three fresh-process
save→load→save cycles are compared through the real codec while paused,
so a remainder dropped anywhere on the capture/encode/decode/stage path
breaks it.

---

## Movement tick: residual time across waypoints (#2473)

The per-unit mover (`Unit.Thread.Movement.PathAdvance`) spends one tick's
elapsed budget along the route, not on one segment of it. Reaching a
waypoint COSTS the time that waypoint's own distance requires at the
segment's effective speed, and whatever is left continues along the next
segment inside the same tick.

Before this, a tick that reached a waypoint snapped there and returned,
discarding the unspent remainder — so a unit crossing waypoints lost
motion in proportion to tick size. The retained reproduction
(`docs/audit_evidence/2026-09-05/movement_timing.ghci`) drives one second
of game time over a flat route from x = 0.4: four 0.25 s ticks ended at
1.25, ten 0.10 s ticks at 1.40, and twenty 0.05 s ticks at 1.45. All
three now end at 1.40.

**The guarantee.** Where the effective terrain speed stays constant,
equal admitted elapsed time yields equal travelled distance however the
elapsed time is partitioned, to within `arrivalTolerance` (1e-4 tiles)
per waypoint crossed.

An arrival is billed at the segment's own effective SPEED — the
unclamped `rawStepLength` — never at the protected-clamped step.
`maxProtectedStep` is a cumulative distance ceiling, not a speed limit:
billing a cap-active arrival for its clamped budget would overcharge it
(0.44 s instead of 0.067 s for a 0.4-tile waypoint at 6 tiles/s) and
starve the continued segment even where the ceiling never binds again.
The clamped step still decides reachability and still bounds distance.

**What it excludes.** Three paths deliberately DROP their unused
elapsed time and are outside the guarantee:

- a tick that exhausts `maxProtectedStep`;
- a tick that exhausts the continuation bound;
- a tick that stops for a transition (climb or fall) or a replan.

It also excludes any tick whose effective speed itself changes.
Effective speed is the commanded speed scaled by the slope grade and
divided by the surface material factor under the unit's feet
(`rawStepLength`), and an ORDINARY non-arriving step samples both ONCE,
at the tick's start tile. So a step that crosses a terrain boundary
without reaching a waypoint is still partition-dependent, and its
resulting `UnitSimState` is byte-identical to the pre-#2473 mover's.
Boundary-based time integration for those steps is a separate,
unmade change; do not add it here.

**The arrival tolerance.** `arrivalTolerance` is 1e-4 tiles. A step that
REACHES its sub-goal is charged the sub-goal's exact distance, so nothing
a step buys is unpaid for. The tolerance itself is a bounded exception to
that, and the only one: a sub-goal ALREADY within it is reconciled at
zero elapsed cost, which is what lets a legitimately zero effective step
finish an arrival instead of stranding the unit (see the invalid-budget
rule below). It is capped at 1e-4 tiles per waypoint precisely so that
exception stays inside the floating-point residue of the snap arithmetic
rather than becoming free travel.

It replaced a 0.1-tile `arrivalEpsilon` sized to prevent overshoot — a
job the charge-what-you-reach rule now does at any tick size — under
which a 0.05-tile step snapped a full 0.1 tiles, and a last waypoint a
tenth of a tile short of the target cleared that target through a
separate per-axis check. Both are gone. The final-target test is now the
SAME radial test against the SAME tolerance the sub-goal arrival uses:
per-axis comparisons admitted a target 1.27 times the tolerance away when
it was offset diagonally, and left that last leg neither travelled nor
charged.

**Per-segment behavior.** Each continuation is a fresh movement segment
from the waypoint's exact position: material and slope are RESAMPLED
there, and the same cost, snap-validation, replan, cliff and fall logic a
fresh tick would apply is applied again. The existing arrival
distinction is unchanged — a protected arrival snap stays subject to
`snapBlocked`, while a fall-permitted arrival snap still bypasses the
cost and fall checks. The one per-tick gate that does NOT re-run is
`tickUnit`'s protected-terrain check (`FallProhibited` against a
`MoveWorld` that is not the mover's own page): the `MoveWorld` is fixed
for the tick and cannot change under a continuation.

**The protected ceiling is cumulative.** A `FallProhibited` request's
total PATH LENGTH in one tick — summed across every continued segment
and every turn — never exceeds `maxProtectedStep` (0.9 tiles). Do not
reset it at a waypoint, measure only endpoint displacement, remove it,
raise it, or bypass it; that bound is what makes the single
`stepCostUnder` check a complete check (#1217).

**Invalid budgets and the continuation bound.** A NaN or infinite
effective step refuses movement outright, BEFORE any arrival decision:
no snap, no waypoint pop, no cleared target. A finite ZERO effective
step refuses all movement too, with one carve-out — when the remaining
distance to the current sub-goal is already within `arrivalTolerance`,
that arrival completes at zero time cost (still subject to
`snapBlocked`) and nothing continues past it. The carve-out exists
because zero speed is an in-domain command and #2204's `sanitiseElapsed`
can hand a tick `dt = 0`; without it a unit standing on its own target
would never clear it. At most `maxWaypointContinuations` (64) sub-goals
are crossed after a tick's first segment; on exhaustion the unconsumed
route stays in `usLocalPath`, the unused elapsed time is dropped, and NO
elapsed-time debt carries into the next tick — nothing stores one.

**`usMoveGrade`** names the grade of the last segment that consumed
movement time. A continued segment that consumes none (a blocked-snap
replan, a zero-length waypoint) does not overwrite it, and the value is
zero only when no segment in the tick consumed movement time. A
single-segment tick keeps its pre-#2473 behavior exactly, including
stamping the grade before a replan or a transition that moved nothing.

**Gates.** The Hspec describe `Movement carries residual time across
waypoints` (in `test-headless/Test/Headless/Unit/Pathing/Hazard.hs`)
drives the real `tickUnit` over the three reproduction schedules, the
waypoint resampling, the cumulative ceiling across a turn, the
continuation bound, the invalid-step refusals, and the identical-state
boundary case. The neighbouring describes `the movement tick` and
`stepCostUnder at the configured fall boundary` hold the hazard policy
this repair must not disturb. Live evidence is `tools/movement_probe.py`
(default, `--course cliff`, `--course ramp`) and
`tools/wander_hazard_probe.py`; run engine probes ONE AT A TIME, and
A/B `wander_hazard_probe.py` stage D against an unmodified checkout
before attributing a failure to a change.

No new state field, worker, RNG draw, or save change is involved.

---

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

---

## Headless fixture logging (#1925)

Every `test-headless` engine boots through `Test.Headless.Harness.Log`,
never `Engine.Core.Init.initializeEngineHeadless` — a preference-free
fixture takes `initializeEngineHeadlessQuiet` (a discarding callback,
chosen BEFORE initialization, which is the only point the initializer's
own entries can still be steered), and a spec that wants the entries
takes `initializeEngineHeadlessLogging` with `newLogCapture`'s atomic
backend. To get a quiet fixture's output back with no source edit, rerun
with `SYNARCHY_TEST_LOG=stderr` (`stdout` restores the pre-#1925 stream;
unset, empty and `quiet` are quiet; anything else is a hard error, not a
silent quiet run). The variable steers only the quiet default, so it
never overrules a spec that named its own backend. Production is
unchanged: `initializeEngineHeadless` still logs to stdout for
`App.Headless`, and `App.Dump` still picks stderr. Gate: hspec
`--match "headless fixture logging"`.

---

## The full test tier: `SYNARCHY_FULL_TESTS` (#1364)

`SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless` costs +~11 s
on a warm macOS/aarch64 tree and +~64 s of hspec wall on CI's Linux
runner — measure each platform, don't port one number. Since #1364 this
tier is no longer local-only: CI's `Headless test suite` step sets the
variable whenever the SAME worldgen selector that gates `world_check
--quick` fires — so every worldgen-output PR and every push to master
runs it and a failure blocks — and `tools/ci-local.sh` (`make ci`) sets
it unconditionally. Running it by hand is still the fast way to see a
failure before pushing; it is no longer the only thing standing between
a full-tier regression and master.

**The variable is wholesale, not per-test.** It has exactly one consumer
today (`Test.Headless.WorldGen.Exposure`'s w128 seed-42 volcano case),
and any new example added behind it automatically joins BOTH of those
gates. Add one only after deliberately accepting that recurring CI
cost. The guard matches any present value: `SYNARCHY_FULL_TESTS=`
(empty) reads as ENABLED, so anything turning it off must leave it
unset.

---

## Unit and combat animations headless

No pixels headless, but `unit.getInfo(uid)` returns
`currentAnim`/`animStart` (the unit thread runs headless); poll over
time to verify timelines. Gate: `combat_anim_probe.py`. Drive by hand:
load `scripts/unit_stats.lua` + `unit_resources` + `unit_ai`, then
`require('scripts.unit_ai').commandAttack(atk,tgt)`.

---

## Movement arenas

`scripts/movement_arena.lua` builds obstacle courses on a flat
`world.initArena` world via the tile-edit API
(`world.addTile`/`deleteTile`/`setFluidTile`/`setSlope` — `setSlope` is
the ONLY way to make a step walkable). `startFall` clears the move
target on landing, so fall checks assert the fall + landing z, not
arrival. Gate: `movement_probe.py` (neutralises the unit_ai wander tick
so `moveTo` is the only steering).

---

## Construction (#95/#96)

`construction.*` designations + construct_job AI (claim → source
materials → progress → place → stake); build costs in
`data/structure_packs/*.yaml` `build:` blocks. Gate:
`construction_probe.py` (stake phase runs LAST).

### Structure construction presentation (#2488)

A structure designation has three presentations and they are disjoint.
UNPAID draws #1846's designation ghost at D-19's 60 %. PAID draws the
authored CONSTRUCTION FRAME its own `cdProgress` selects, at full
opacity, through the same `structurePieceQuadsResolved` body the placed
piece uses. Anything else draws nothing.

A pack declares an ordered frame list per authored APPEARANCE — one per
`pieces.<kind>`, per `walls.<edge>` (all four caps share the sprite), per
`variants.<name>` override and per Wire `connections.<name>`. Keyed to
exactly that appearance: a variant never inherits the default's, and an
appearance with no declaration resolves NONE, which is every shipped
appearance today and draws nothing (BDA-15/BDA-16 author the art,
BDA-13 enforces coverage). The gap is reported once per (pack,
appearance) at registration, never per frame or per candidate.

Five rules the schema doc spells out and the gates hold:

* the frame index is `floor (progress * n)` clamped — buildings'
  convention (`Building.Visual.pickBuildingFrame`);
* a wall draws the sequence of the edge whose art is really DRAWN —
  `drawnWallEdge` asks `Structure.WallCatalog.rotatedWallArt` and takes
  the screen edge only when it resolves, so the frame and the cap mask
  can never name two appearances — and a family's declared directions
  must run to equal lengths or registration refuses the pack;
* a declared path that escapes the resource root is neither queued for
  load nor opened: the loaders ask `structure.isSafeArtPath` (the
  catalogue's own predicate) and send no handle, and registration
  preflights it before measuring;
* the last frame must occupy the static sprite's exact canvas, measured
  from the FILES at registration (`rvTextureSizeRef` is upload-filled and
  empty headless);
* the site keeps drawing until its piece is COMMITTED to the rendered
  overlay — `World.Construct.Art.structureCommittedAt` ignores the
  staging cache, because a staged-but-uncommitted piece is on screen
  nowhere;
* `renderFlagLifecycleAlpha` (bit 1) makes the frame's own alpha
  authoritative so the reused facemap's silhouette cannot clip it; the
  RGB/top-light path is untouched and an unflagged quad is unchanged.

Saved designations carry no new field. Full schema, refusal table and
ownership: [structure_pack_schema.md](structure_pack_schema.md). Gates:
hspec `--match "structure construction frames"`, `--match "structure
ghost"`, `--match "Structure.ArtCatalog"`; probes
`construction_probe.py`, `wire_probe.py`, `structure_rotation_probe.py`,
and the offscreen pixel gate `structure_construction_probe.py`.

---

## Roles (#265)

DERIVED labels, never assigned: highest work skill ≥ 30 (+5 switch
hysteresis). Roles multiply work-action ENTRY utilities only (on-role
×1.4, off-role ×0.7) — never the 6.0 in-progress locks, never
survival/combat/orders. `unitAi.getRole`. Gate: `role_probe.py`.

---

## Crafting and bills (#325/#326/#329/#343/#795)

Recipes in `data/recipes/*.yaml` (station tag, inputs, optional
fuel/knowledge/skill, work, outputs, optional `power_draw`).
`craft.execute(uid, recipeId)` is station-blind (tests/console);
`craft.executeAt(uid, recipeId, bid[, billId])` needs a Built station
offering the operation with the unit adjacent (Chebyshev ≤ 1). Bills
(`Craft.Bills`, per-page, engine-side atomic claims, persisted) have
three modes: fixed count, repeat-forever, until-stock — the last
re-checks LIVE ground stock via `unit_ai_fetch.untilStockSatisfied`,
the same formula the crafting panel uses. Skill-tagged recipes derive
output quality from the crafter, then shift by live mental
effectiveness (±10), so quality assertions must pin the
neutral-effectiveness precondition (#878). Gates: `craft_probe.py`,
`craft_bill_probe.py`.

**Bill selection order (#2523).** A station's queue is ordered by
`cbSeq`, not by `cbId`: `billsForStation` sorts on it and `reorderBill`
swaps only that field, so the panel's displayed order is the authority
on what a worker takes next. `scripts/unit_ai_craft.lua`'s
`findCraftBill` therefore ranks candidates by DISTANCE first, and only
then by queue order — and only between bills at the SAME station, which
is the only scope `seq` is comparable in. Three consequences:

- A fresh selection takes the earliest bill in that station's displayed
  queue among the ones the worker can currently claim and perform.
  Moving an unclaimed bill up or down changes subsequent fresh
  selections. Every existing eligibility filter is unchanged and still
  applies first, so an earlier bill that is paused, freshly claimed by
  someone else, unaffordable, off-page or knowledge-gated is skipped
  rather than blocking a later one.
- Proximity still wins between stations: a queue position at a distant
  station never becomes a global priority. An exact equal-distance tie
  between DIFFERENT stations goes to the lower station id, purely so the
  outcome cannot depend on the order `craft.getBills()` returns rows in
  (the AI's no-argument listing is `sortOn cbId`; nothing may rely on
  that).
- FRESH selections only. Reordering never revokes a claim or preempts an
  in-progress job: a worker holding a bill keeps `craft_lock_utility`
  and its `craftJob`, and `completeBillCycle` still chains an unpaused
  repeating bill into its next cycle without re-entering `findCraftBill`.
  So a repeating bill that keeps its claim across cycles continuing to
  run is not a violation of this rule.

Gate: hspec `--match "craft bill queue priority"`
(`Test.Headless.Lua.CraftBillQueuePriority`), which drives the real
`craftUtility` over a stubbed engine API and re-runs every case with the
listing reversed. `seq` is already a persisted `CraftBill` field, so
nothing here is a serialization change.

---

## Power (#358-#361, #590/#591, #1206)

Solar/battery nodes are item-consuming placements (`power.placeNode`
via `buildTool.commitPlacement`); networks (wire 4-adjacency +
nodes/consumers) are recomputed fresh every tick — only battery
`storedWh` persists. Solar follows the sun angle and
`world.setTimeScale`. Electrical load lives on the RECIPE
(`power_draw`), not the building: a bill draws only while claimed AND
`cbWorking`; `power.isStationPoweredForRecipe(bid, recipeId[, billId])`
is the gating query — pass the bill's own id so its already-registered
draw isn't double-counted. A node's LIFETIME is its building's:
`BuildingDestroy` retires it in the same live transaction that removes
the instance (`Power.Live.retirePowerNodeEverywhere`), so a demolition
never reaches the save — but that is NOT load-time pruning: a save
already carrying a dangling node still restores it verbatim.
Retirement is a delete, never a compaction (`pnsNextId` keeps
advancing; a retired id is never reissued), and there is deliberately
no public `power.removeNode`. Gates: `power_probe.py`,
`power_workshop_probe.py`, `machine_shop_probe.py`, hspec
`--match "power node demolition"`; pure algorithm in
`Test.Headless.Power.Network`.

---

## Flora species identity: the authored name is the key (#2241)

A flora species' authored YAML `name` is its stable key. The numeric
`FloraId` is a SESSION-LOCAL registration ordinal and nothing durable
may be derived from it.

**Three consequences, each with its own gate.**

1. **Placement never depends on catalog position.**
   `worldGenSpecies` returns species in canonical authored-name order
   (`floraWorldGenKey`, tie-broken by `FloraId` so the order stays
   total when a `fcWorldGen` entry has no `fcSpecies` record — such an
   entry keys off a synthetic `\SOH`-prefixed spelling of its id, which
   no authored name can collide with). The per-tile placement ROLL and
   each instance's own offset/variant/age draw are salted from that
   same key (`floraPlacementSalt` / `floraInstanceSalt`,
   `World.Flora.Identity`), never from an index into the list. So
   discovery order, registration order and `HashMap` traversal order
   cannot change generated flora.

   This is ORDER-independence, not final-layout invariance. Flora share
   one occupancy map and `markOccupied` lets an earlier placement
   suppress a later candidate, so adding or removing a species that
   ACTUALLY PLACES may still move another's plants. That competition is
   deliberate. A species that never occupies a tile changes nothing at
   all, however it reorders the catalog.

2. **`data/flora` loads in canonical byte order.**
   `queueNormalProfile` uses `addYamlDirCanonical`, a flat directory
   sorted through `startupLoader.canonicalFileOrder`. It is the one flat
   family that sorts, because its sequential ids are what a save's
   numeric flora references name; every other flat family keeps
   `engine.listFiles`'s raw enumeration, and `engine.listFiles` itself
   does NOT sort. All three `addYaml...` verbs stay at exactly three
   arguments — `tools/save_compat_migration_probe.py` parses those call
   shapes verbatim.

3. **A duplicate authored name is refused, whole-file and atomically.**
   `engine.loadFloraYaml` preflights a file against the live catalog AND
   against itself before allocating an id, registering a texture or
   queueing a load, so a refusal leaves no partial registration from the
   definitions ahead of the collision. It answers `(0, true, <name>)` —
   the file DECODED, so `pushYamlResult`'s decode-only second value is
   unchanged for the other twelve families; the third value exists only
   on a refusal, so a healthy call's arity is still one bare and two
   when asked. `scripts/startup_loader.lua` turns that third value into
   a TERMINAL startup failure naming the file and the name. The runtime
   verb `flora.register` is nonfatal by contrast: a collision returns
   `nil`, warns, and mutates nothing.

4. **A save names its species; only a live session numbers them
   (#2243).** The three durable references — a planted-flora edit
   (`WePlaceFloraRefD`), a crop plot's `cpiSpecies`, a plant
   designation's `ptiCrop` — carry a
   `World.Flora.Reference.FloraRef`, and no code writes a `FloraId` to
   a save. The catalog is the only thing that relates a name to a
   handle, and it is read at exactly TWO boundaries:
   `World.Thread.Command.Save.WriteWorld` names every live handle at
   capture (`nameFloraReferences`), and `World.Load.Stage.stageSession`
   resolves every name back (`resolveFloraReferences`) for the whole
   session before any page's live refs are written. Between them the
   value is a name — in the `SessionSnapshot`, in the `SaveData` bridge,
   and on disk. Each boundary refuses rather than guesses: a live handle
   this build cannot name fails the SAVE before publication, and
   `missingFloraReferences` fails the LOAD, naming the species, the
   site, the page and the tile — over all three sites, plant
   designations included, which the pre-#2243 check never walked at all.
   Component migrations stay pure (`atVersion` sees only the decoded
   payload), which is exactly why the resolution lives at those
   boundaries and not in the migration.

**Legacy numeric references are reinterpreted once, on purpose.**
Canonical registration renumbers nearly the whole shipped catalog, so a
`FloraId` persisted in a `WorldEditDTO`, `CropPlotDTO` or
`PlantDesignationDTO` before #2241 generally names a different species
afterwards. Accepted, not mitigated: `currentSaveVersion` is a worldgen
bookkeeping marker with no on-disk compatibility role, so its bump
neither migrates nor rejects anything.

#2243 applies that reinterpretation one last time and then closes the
hole. A `world-edits` v1/v2 or `world-activity` v1-v5 payload — and the
B1 `session` v90 tree, which reshapes its numeric flora fields through
the same migrations — carries its ordinals forward as
`FloraByLegacyId`, and the load boundary resolves them by EXISTENCE
against the catalog of the build that loads them: unresolvable is a
refusal naming the NUMBER, resolvable is accepted as whatever species
that catalog happens to number there, **which may not be the species
that was planted**. That is the documented limitation (D-2), not a
guard — the catalog that minted the number was never saved, and
refusing every pre-name save would strand all of them. The very next
save of a loaded session writes names, so each save crosses this
boundary at most once.

**Chop reconciliation is bounded by ownership.** `admitChunkFlora` drops
every durable chop designation whose canonical tile the admitted chunk
owns but whose `FloraInstanceId` that chunk does not hold, with a
diagnostic per removal. It never inspects a designation another chunk
owns, so an entry whose chunk is simply not resident survives.

Design record:
[`docs/flora_species_identity_design.md`](flora_species_identity_design.md).
Gates: hspec `--match "World.FloraOrder"` (two opposing registration
orders, the impossible-fit lexically-earlier species, the checked-in
seed-42/world-size-64 golden, and the pre-change numeric-reference
fixture — both fixtures live under `test-headless/data/flora-order/` and
are re-capturable through the env vars the module header names, and
neither is registered in `docs/save_compat/manifest.json`, so neither
owes the save-compat gate); `--match "Startup"` (the two-order loader
proof in `Startup asset logging`, the shipped-duplicate readiness
failure in `Startup readiness`); `--match "Asset.FloraContent"`
(whole-file refusal atomicity, `flora.register`'s nonfatal refusal);
`--match "Chop authority"` (the three-designation reconciliation case);
and for the persistence rule, `--match "flora species references"` (each
site refused by name, a legacy ordinal refused by number, the
catalog-reorder regression, and the save-side refusal), `--match
"persistence contract"` (all three sites read back by name after a real
round trip) and `--match "flora species names across baselines"` (the
tracked `x1-flora-species-names` fixture really carries all three, and a
pre-#2243 baseline still decodes as ordinals).
Flora stays outside `tools/world_check.py`'s baselines, so no terrain
recapture is owed.

---

## Flora visual state and fallback (#2526)

A flora occurrence's SEMANTIC appearance is resolved from five axes,
and from nothing else: context (`wild`/`cultivated`), life phase, annual
stage, condition (`alive`/`dead`) and cause of death. One documented
state sits OUTSIDE that resolver — harvest depletion, an outer override
for living occurrences, below — and it is the only one. The complete
vocabulary, the `textureVariants` and `corpsePolicy` schemas, the
worked matching examples and the ten-step fallback ladder live in
[`docs/flora_visual_state_contract.md`](flora_visual_state_contract.md).
The invariants are here.

**Declared, never discovered.** Every optional variant is an explicit
YAML selector with a texture path; the filename convention is readable
and carries no runtime meaning. Resolution never touches the
filesystem, so art added to a directory and left out of YAML changes
nothing. This is live today, not hypothetical:
`assets/textures/flora/wheat/{wild,cultivated}/` ship twelve files
(PR #2136) while `data/flora/crops.yaml` still points wheat's `texDir`
at `white_clover`, and none of them render. Duplicate selectors and
unknown vocabulary are whole-file refusals, like the closed
vocabularies `Asset.FloraVocabularySchema` already gates, and so is an
UNREACHABLE declaration — one naming a phase or stage the species never
declares, or naming `phase: dead` at all — following the
`requireDeclared` rule `cycleOverrides` has enforced since #2315.

**`dead` is a legacy phase token, never a phase at death.** Every
mortal shipped species authors `phases: [{tag: dead}]` and
`World.Flora.Growth` pins the age to it, so the token stays — but it
normalizes to `condition: dead` with every other axis wildcard, and its
`phase: dead` cycle overrides normalize to stage-specific generic-dead
declarations. Everything else legacy normalizes to `wild` + `alive`. A
natural-lifespan death requests `condition: dead`, `cause: natural` and
the frozen LAST LIVING phase and stage, resolving to the same
`dead.png` shown today. `PhaseDead` therefore survives in
`LifePhaseTag` for decoding only, and natural and hazard deaths share
one fallback path.

**The fallback priority is death, then phase, then cause, then annual
stage.** An exact selector wins; a cultivated request tries the same
WILD semantic state before discarding any other axis; a dead request
exhausts every dead candidate before showing living art; and
phase-appropriate generic-dead art beats an adult-shaped cause asset
that would misrepresent a juvenile.

**The decided ten-step ladder is the TRACE of a total order, not an
enumeration.** Candidates are ranked by descending lexicographic order
of which semantic axes a matching declaration NAMES, in that same
death/phase/cause/stage priority, with context explicitness as the
final tiebreak. Descending order over those keys reproduces the ten
steps exactly, and it also ranks every legal mask the trace does not
name — `{stage, condition: dead, cause}` sorts between steps 5 and 6
rather than falling off the ladder. Two candidates can only tie by
naming the same axes with the same values, which the duplicate rule
already rejects, so the winner is a function of the selector and the
declared set alone — never of file, alphabetical or `HashMap` order.
Living requests carry no cause and run their own eight keys; they do
not enter the dead trace at all.

**Context is an ATTEMPT loop, not a matched axis.** A declaration
matches on the four semantic axes; context eligibility is separate. A
wild request makes one attempt (`wild`); a cultivated request makes two,
`cultivated` then `wild`, BOTH at each key before the key weakens — which
is invariant 3, and is what makes an explicit `context: wild` asset
reachable from a cultivated request (step 2). A context-less
declaration is eligible in every attempt and is the shared default; an
explicit context beats it within one attempt. A `cultivated`
declaration is never reachable from a wild request, which is what makes
cultivated art an override rather than a parallel lifecycle.

**Legacy LIVING entries keep their own precedence and are NOT ranked by
that key.** `phases`, `annualCycle` and `cycleOverrides` resolve as one
fixed step after every declared variant, exactly as
`resolveSpeciesTexture` does today — annual stage ABOVE life phase,
which is the opposite of the variant key's order. Ranking them by the
key would silently change what every shipped species draws. The legacy
`dead` phase and its `phase: dead` overrides are the exception: they DO
enter the dead order, at the generic-dead and stage-specific-generic-dead
keys, which is where today's rendering already puts them.

**Harvest depletion is the one state outside the five axes, and death
supersedes it.** `World.Render.FloraDraws` draws `fhHarvestedTexture`
for any instance in the harvest map and never calls
`resolveFloraTexture` at all. That stays true while the occurrence is
ALIVE. Once it is dead, condition wins and the dead order runs — a
deliberate, narrow change from today, where a plant inside its regrowth
window at its lifespan keeps drawing harvested stubble. Invariant 4
requires it, and EFM-7 owns and gates it.

**Two behaviour changes, both deliberate.** The depletion change above
is one. The other is cultivated expiry: `floraGrowth` derives a
generation arithmetically today, with no occurrence state and no notion
of context, so EVERY occurrence wraps to a fresh sprout. D-13 stops
cultivated occurrences doing that — they become empty and await
replanting — under an explicit policy AND under the omitted legacy one,
since the two are the same code path. An omitted `corpsePolicy`
therefore supplies today's 60-day window and today's WILD successor,
not an exemption from D-13. EFM-10 owes the gate on both sides.

**Two fallbacks, not one.** The final SEMANTIC fallback is the
species' own base texture — the first `phases` entry's texture, or
`matured.png` when a species declares none, exactly as
`registerFloraSpecies` resolves it today.
`assets/textures/flora/unknown_flora.png` is only the ERROR fallback
for a base that is itself missing or invalid, and a semantic miss never
reaches it.

**Context is chosen, never inferred.** Natural generation is `wild`;
planted row flora and groundcover `CropPlot`s are `cultivated`. Age,
health, density, placement category and texture path decide nothing. A
species declaring no cultivated variants renders identically in both
contexts, so the axis changes no existing visual.

**Retention is authored, and cause does not move it.** `corpsePolicy`
declares `visibility` (`transient`/`persistent`) with `durationDays`
and `successor` required for transient and refused for persistent;
optional `phase`/`cause` overrides each declare a COMPLETE outcome and
inherit nothing. `successor` (`reseed`/`absent`) names the WILD outcome
only: a cultivated occurrence always becomes empty and awaits
replanting by rule, so `await_replanting` is a documented cultivated
outcome and a REJECTED species token. A corpse freezes its phase and
annual stage at death and snapshots its retention outcome, so a later
content edit cannot reinterpret an existing save. An omitted policy
keeps today's behaviour: transient, 60 days, wild successor `reseed`,
matching `World.Flora.Growth.deadWindowDays`.

**Persisted state is semantic.** Condition records carry tags —
context, condition, cause, frozen phase and stage, retention outcome
and expiry — and never a texture handle or a resolved path, so art can
be added or renamed without migrating a save.

Design record:
[`docs/environmental_flora_mortality_design.md`](environmental_flora_mortality_design.md).
Gates: TODAY, `tools/texture_subset_audit.py` (declared flora texture
paths resolve to real files — note it enumerates `phases`,
`annualCycle`, `cycleOverrides` and `harvested_texture`, and does NOT
yet inspect `textureVariants`), plus hspec `--match
"Asset.FloraVocabularySchema"` (closed phase/stage/lifecycle
vocabularies refused at the authoring boundary), `--match
"Asset.FloraContent"` (whole-file refusal atomicity) and `--match
"World.FloraGrowth"` (the lifespan and dead-window behaviour the legacy
default preserves). This section is documentation and adds no gate of
its own. OWED by the epic's children, each with its own issue: EFM-2
owes loader and audit gates for `textureVariants` and `corpsePolicy`
declarations, including the duplicate-selector, unknown-vocabulary and
legacy-collision refusals and the extension of the texture-subset audit
to declared variants; EFM-3 owes table-driven resolver tests covering
all ten ladder steps and both fallbacks; EFM-4 through EFM-6 owe
occurrence-identity, render-context and persistence gates; EFM-7 owes
the depletion-versus-death gate above (a depleted harvestable species,
killed, renders its dead candidate; alive and depleted, it still
renders `harvested_texture`); EFM-10 owes retention and successor
behaviour, including the wild-reseeds/cultivated-empties pair above; and EFM-9 owes the pilot's
end-to-end headless and preview evidence.

---

## Loot profiles (#2499)

A loot PROFILE is not a loot table, and the two registries are separate
on purpose. A loot table (`data/loot_tables/`) is ONE weighted draw:
exactly one entry wins and `weight` is relative to its siblings. A
profile (`data/loot_profiles/`) rolls every entry INDEPENDENTLY against
its own absolute `chance`, and each entry that appears contributes a lot
sized by the profile's `quantity_multiplier` and the entry's own
`quantity_factor` (design D-2/D-6 in
[`docs/portable_loot_containers.md`](portable_loot_containers.md)).
Nothing of the entry shape or the roll carries over.

**The schema.** One YAML document per file — the file IS the profile,
with no wrapping list, exactly like a loot table and unlike
locations/items/units:

```yaml
id: ruin_industrial_salvage
quantity_multiplier: { min: 1, max: 4 }
entries:
  - { item: steel_bar, chance: 0.30, quantity_factor: 5 }
```

`entries` keeps its AUTHORED order. That order is not cosmetic: PLC-13's
admission pass shuffles a seeded copy of it, so it is the stable input
that shuffle is a function of, and reordering a file is a content
change.

**Every rule rejects the WHOLE file.** A partially admitted profile is a
distribution nobody authored, and whole-file rejection is what makes the
registry's insert/replace policy safe — a rejected replacement leaves
the previously registered profile of that id exactly as it was.
`Engine.Asset.YamlLootProfiles` refuses:

- a missing, empty, or non-string `id`;
- a REPEATED key at any depth. libyaml resolves a duplicated mapping key
  by keeping the LAST binding, so a document saying `id` twice would
  otherwise decode cleanly as whichever id came second. This is the one
  family decoded through `Yaml.decodeFileWithWarnings` rather than
  `decodeFileEither`, because that warning list is the only place the
  collision is visible;
- an absent, null, non-list or EMPTY `entries` — unlike an empty loot
  table, which is a defined outcome (its single draw answers `Nothing`);
- a `chance` outside the INCLUSIVE `[0, 1]`, or non-finite. Both checks
  run after narrowing to the stored 32-bit `Float`, so an ordinary
  `1.0e+100` is refused as the `Infinity` the runtime would actually
  compare against;
- a `quantity_factor` that is not a positive whole number. Zero is not a
  disable toggle (#1721's settled rule for the sibling multiplicities):
  an entry that should not appear is deleted;
- a `quantity_multiplier` that is absent, null, not a `{min, max}`
  block, whose bounds are not whole numbers, or whose `min < 1` or
  `max < min`;
- after a successful decode, an `item` that is not in the live item
  registry (D-20). Items load before profiles, so the registry is
  complete — the same ordering #917 relies on for a location's
  guaranteed significant contents.

Diagnostics name the FILE always, the profile once it is known, and a
1-based entry index only for an ENTRY-level rule. A missing `id` and a
bad `quantity_multiplier` have no entry to name and do not invent one.

The duplicate-key rejection follows the same discipline, over the
coordinates a duplicated key leaves trustworthy. A repeated key inside
an entry is named by profile, 1-based entry index and item, exactly as
a bad `chance` in that entry would be. Each coordinate is dropped only
when the duplicate is what made it untrustworthy, and dropping one
never costs the others:

- a repeated top-level `id` leaves no name to print — either one would
  be the value the rule exists to distrust — so that case, and only
  that case, falls all the way back to the raw 0-based YAML path;
- a repeated top-level `entries` makes the INDEX untrustworthy, because
  libyaml reports an inner duplicate against whichever list it walked
  while only the last list decoded. The profile is still named;
- an entry whose own `item` repeated has a decoded item name that is
  likewise just the last binding, so every duplicate in that entry is
  named by profile and 1-based index without it.

A duplicate nested deeper inside an entry — in a sub-block of its own —
keeps that entry's coordinate and names the sub-block after it, since it
is still that entry's duplicate. The index is 0-based only in the
raw-path fallback, where it is a YAML path rather than an entry
coordinate: renumbering a path is not something an author could then
find.

**Duplicates are settled before the typed parse runs.** The file is
decoded to a plain `Value` first, which cannot fail on schema, so the
warning list is always in hand. Decoding straight to the definition type
loses it on exactly the documents that need it most: a validation
failure answers with the warnings discarded, so a file that repeated
`id` *and* authored a bad `quantity_multiplier` would be rejected by a
message quoting the last-wins id — the value the duplicate rule exists
to distrust — with the duplicate never mentioned.

**The loader's outcome contract is the #2203 one, unchanged.**
`engine.loadLootProfileYaml(path)` answers ONE number; a truthy second
argument opts in to `(count, parsed)`, where `parsed` is about the
DECODE alone. A decode failure answers `(0, false)`; a successful
registration `(1, true)`; and an unknown item id `(0, true)` while
registering nothing — that file parsed, so reporting it as a parse
failure would make an ordinary content mistake indistinguishable from a
corrupt `data/` tree. It warns once per unresolved entry rather than
answering a silent zero. There is no third value: `pushYamlRefusal`
exists for #2241's duplicate-NAME collision inside a list-shaped family,
and nothing here is that. Registration is insert/replace by profile id,
and a replacement says so in a warning naming the file and the profile.

**Capabilities.** `lootProfileRegistryRef` is a `content-registries`
field, written only by `Engine.Scripting.Lua.API.LootProfiles` through
`ContentRegistriesCapability`. That module is the one in the group
holding BOTH records: it reads `itemManagerRef` for the load-time item
check through `ContentRegistriesViewCapability`, as a `ReadOnlyRef`, so
it never gains write authority over items.

**Reads are read-only and copy-free of the registry.**
`loot.profile(id)` answers
`{id, quantity_multiplier = {min, max}, entries = {{item, chance,
quantity_factor}, …}}` with dense 1-based `entries` in authored order,
or nil; `loot.listProfiles()` answers the sorted, repeat-free id list.
Both build a fresh table per call, so a script that edits what it got
back has edited its own copy. They live under the existing `loot`
namespace by D-20; the namespace list stays closed.

**No consumer rolls a profile yet.** PLC-13 owns realization, PLC-14 the
container content entries, and PLC-10 both the wooden crate and any
retuning of the shipped `ruin_industrial_salvage` calibration. The
shipped file exists because #2203 makes a queued registry family that
discovers no YAML a terminal boot failure, so registering the family
required shipping its first file (D-20).

Gates: hspec `--match "Loot profiles"` (the shipped file's pinned
entries, every rejection rule at the tightest value its guard admits and
refuses, the loader's three outcomes, insert/replace, and the two
queries), `tools/content_registry_probe.py` (the family end to end
against the real item tree), `tools/startup_asset_logging_probe.py` and
hspec `--match "Startup"` (the thirteenth normal family and twelfth
arena one).

---

## Farming (#331-#336)

Flora growth is DERIVED state from the advancing calendar (nothing
per-instance in saves; `world.getDate`/`setDate`,
`world.getFloraGrowthAt`). Fruiting windows gate bare food-harvest
calls; whether a TAGGED call skips the window is AUTHORED per
`harvestable:` block (#2212, below), and chop-claim keys on
`regrowthRemaining`+`tags`, not `harvestable`.
Tilling: `till.*` mirrors `chop.*`; completion writes `world.setVegAt`
(edit-log — survives eviction/saves); consumers must use
`world.isPlantable`, never compare `getVegAt` to raw id 77. Gates:
`flora_growth_probe.py` (registers a max-tolerance `probe_berry`
species), `till_probe.py`.

### Authored harvest-tag policy (#2212)

Before #2212 a tagged `world.harvestFlora` skipped `harvestOpen`
unconditionally — a wood-REMOVAL policy written as a property of
"being tagged", which any future `fruit`/`grain` tag would have
inherited — and yield had no phase input, so a day-zero sprout dropped
a mature tree's logs, hid for its regrowth and could be felled again.
Two additive `harvestable:` keys now author both halves:

- **`ungated_tags:`** — the subset of `tags:` whose harvest may take
  the plant OUTSIDE the growth window. **Absent means growth-gated**,
  so a tagged call against a non-declaring block is refused in exactly
  the states a bare one is. Every entry must appear in `tags:`
  (rejected at the authoring boundary, like a `cycleOverrides`
  selector).
- **`phase_yield:`** — a life phase → that phase's own yield list. An
  ABSENT phase inherits `yield:`; a phase mapped to `[]` yields
  nothing. Absent and explicitly-empty are deliberately
  distinguishable; keys are checked against the `LifePhaseTag`
  vocabulary AND against the species' own `phases:`.

`white_oak`, `paper_birch` and `sugar_maple` author `ungated_tags:
[wood]` plus an empty `sprout` yield, so every tree the chop drag-box
selects stays designatable and fellable as a sprout, matured or
standing dead, and a felled sprout pays nothing.

`World.Flora.Growth.floraHarvestAdmits` is the SINGLE predicate the
screen-space hit test (`World.Flora.HitTest`), the world thread's
designation commit (`World.Thread.Command.Cursor.Chop`), both tagged
harvest verbs and the tagged finder consult, evaluated from the same
tag and derived growth state; the per-instance regrowth timer, the
exact-identity filter and a bare finder's edible-yield filter stay
caller-side. The growth clock is `World.Flora.Clock.growthClock`, so
the world thread and the Lua verbs read one value.

An ACCEPTED fell whose authored yield is empty returns a non-nil EMPTY
Lua table, starts the regrowth timer and permits designation
cancellation; `nil` stays reserved for a refusal or a raced-away
target. `scripts/unit_ai_chop.lua` grants woodcutting XP only when the
result carries spawned yield, so sprouts and races earn none.

Gates: hspec `--match "harvestOpen"` (the pure predicate and the phase
yields), `--match "Asset.FloraHarvestPolicySchema"` (the authoring
boundary and the shipped corpus), `--match "Chop tag policy"` (all
four surfaces agreeing against a real engine), `--match "chop fell
XP"` (the Lua grant). `tools/chop_probe.py` stays ADVISORY until #2058
gives it a fixture.

---

## Fluid reaction: unlike-fluid contact and its stone (#2481, #2485)

Design record: [`docs/fluid_reaction_design.md`](fluid_reaction_design.md)
(decisions D-1, D-2, D-3, D-5 and D-7). FR-1 of epic #2480 makes the
contact react inside the sim; FR-2 (#2485) is the world-side consumer
that turns the events into durable stone. The two halves are separated
by a thread boundary that is the whole reason the rules below read the
way they do: the sim is the only thing that knows a contact happened,
and the WORLD thread is the only writer of the tiles, the only owner of
the durable edit log, and the only minter of live-edit generations.

**Occupied-contact identity invariant.** No occupied contact changes
either cell's fluid type. Unlike contact is `Lava` versus any of `Ocean`,
`Lake` or `River`; the three water types are ONE compatible class among
themselves. Every live cell keeps its type until its volume reaches zero,
at which point it becomes empty. Before #2481 every occupied-destination
write kept the DESTINATION's type and added the incoming volume, so lava
arriving in water silently became water and vice versa.

**Live contact annihilation.** A contact is resolved from both cells'
CURRENT LIVE type and volume, before either side is debited. The consumed
amount is the smaller live volume; it is subtracted from BOTH sides and
none of the requested transfer moves. A cell reaching zero becomes empty
(`Nothing`). Exactly one side of an unlike contact is lava, so a contact
yields at most one solidification event — emitted only when the LAVA side
reached zero. A water cell reaching zero is not a solidification.

**The five protected branches.** There are four transfer mechanisms but
FIVE occupied-destination write branches in `src/Sim/Fluid/Active.hs`:
seam exchange (`transferCell` via `reconcileSeams`), `phaseGravity`,
`phaseLateral` with a snapshot-occupied destination, `phaseLateral` with a
snapshot-empty destination that an earlier source filled live in the same
phase, and `phaseWaterfall`. All five route through ONE applier,
`Sim.Fluid.Reaction.applyTransfer` — do not re-derive the rule at a call
site. A wrapped cylindrical-seam event names the exhausted lava cell's own
CANONICAL stored chunk key (#2044), whichever side of the seam it is on,
and names the contacting water cell's canonical key the same way.

**Snapshot plans are paid from live cells.** The three in-chunk phases
plan requests from a frozen snapshot and mutate a live grid, and a
reaction can consume MORE than the planned transfer. Every request is
therefore resolved against the live source and destination when it is
applied; a later request cannot overdraw, recreate, or keep spending an
exhausted source.

**Bounded compatible transfers.** An ordinary transfer into an empty or
compatible destination is bounded by the requested amount, the live source
volume, AND the destination's remaining `maxBound :: Word16` capacity.
Undelivered units stay at the source; no subtraction or addition wraps.
An empty destination takes the source's type, a compatible occupied one
keeps its own.

**Refill policy.** A cell emptied by annihilation is an ordinary empty
destination for later phases and requests in the same tick, and may be
refilled with any fluid. Refill neither cancels nor duplicates the event
that coordinate already emitted. At most one event is emitted per
canonical coordinate PER TICK — keeping the FIRST contact's product and
water type — and a later tick may emit another there once new lava has
arrived and been exhausted again.

What the dedupe does NOT drop is the later contacts' participating
chunks. A coordinate exhausted against an in-chunk neighbour, refilled
with lava by a later phase and exhausted again across the seam has taken
fluid from two chunks, so `sevWaterChunks` carries the UNION in contact
order. Both of those chunks' consumed-fluid writebacks ride the same
delivery as the one stone, and keeping only the first would leave the
second outside the result's own admission — an intervening edit there
could then stale its writeback while the stone committed anyway (#2485).

**Event accumulation.** Events land in `SimWorldState`'s `swsSolidEvents`
(`src/Sim/State/Types.hs`), in emission order. The collection is transient
simulation OUTPUT: `emptySimWorldState` starts it empty, and a
nonreacting, inactive or deactivating tick carries it forward unchanged —
a deactivating tick bakes its grid to passive fluid but keeps the events
it already produced. **Every delivery DRAINS it whole** (#2485): an
emitting tick takes the entire collection, groups it, and leaves the
world holding none of it, which is what makes "committed exactly once" a
property of the drain rather than of the world thread's bookkeeping. An
INACTIVE world emits nothing and therefore drains nothing — its history
is output it has not delivered yet. It is otherwise cleared only when the
page itself is dropped (`SimDropWorld`; `SimDeactivateWorld` keeps the
world entry, so events survive hide/show like the chunks do). Never
serialized: see
[`docs/persistence_state_inventory.md`](persistence_state_inventory.md)
§6.

### The product is durable terrain, not a writeback (#2485)

**The stone is an EDIT.** Each accepted event appends one
`World.Edit.Types.WeAddTile` for its column and product material to
`wsEditsRef` and applies it through the same `World.Edit.Apply.applyEdit`,
`replaceChunkForgettingFlora`, plant/construct revalidation and
`UnitReGround` a player's own add-tile uses
(`World.Thread.Command.Reaction`). A fluid writeback could not do this:
`applyOneWriteback` replaces a chunk's sim-owned fields in memory and
appends nothing, so terrain written that way would vanish on eviction and
never reach a save. Replay over regenerated terrain and a fresh-process
load both reproduce the raised column and its top material. The live
add-tile handler's out-of-column-range pre-check applies unchanged, and a
skipped event is logged rather than silently dropped.

**The product material resolves through the registry.** `basalt` and
`obsidian` (`data/materials/igneous_extrusive.yaml`) are looked up by
NAME through `World.Reaction.Stone.stoneMaterialFor`, never a literal id.
A name the registry does not know is reported and the event commits
NOTHING — omitting the tile would leave the consumed lava with no product
at all. Which of the two is chosen was decided at the reaction by
`solidProductFor` above, from the contact's own reading, so queue timing
cannot change it and both contact orderings agree.

**A result is admitted whole or rejected whole, BEFORE anything is
applied.** A delivered `Sim.Fluid.Reaction.ReactionResult` names every
participating chunk — the lava chunk and the water chunk, including
across the cylindrical seam — and the live-edit generation each half was
computed from. `World.Thread.Command.Reaction.admitReaction` decides it
against the tiles as they stood before the delivery, and it decides four
things, not one:

1. every participant is still at the generation its half was computed
   from — the same equality `writebackIsFresh` applies per chunk, over
   all of them at once, with an absent entry reading as generation 0;
2. every participant is still LOADED. Eviction retires a chunk's
   generation entry, so a result computed at generation 0 and delivered
   after one participant was evicted passes (1) on the number alone, and
   would then have its events, its writeback and its sync entry all
   skipped for that chunk — a partial commit by omission;
3. every event's product material resolves through the registry; and
4. every event's edit actually applies, rehearsed in order against a
   private overlay, so a sibling that only becomes applicable after an
   earlier one has grown its column is judged on what it will really
   meet.

The page-incarnation fence (#2477) comes first, exactly as it does for
writebacks. Two events share a result exactly when they share a
participating chunk, transitively, so genuinely disjoint contacts in one
delivery stay independently eligible.

Deciding all four up front is what makes the delivery all-or-nothing.
The writebacks are applied on the strength of that decision, so a commit
that could still drop an event — for a material it could not name, or a
column it could not raise — would leave the annihilation recorded with
no stone. The commit is therefore total by construction, and an
impossible state raises rather than skipping.

**Generations advance once, after every admitted event has landed.** The
stone edits themselves bump the generations admission compares against,
so an event judged after its sibling landed would read as stale purely
because of it. Every admitted event is applied first; then each EDITED
chunk's generation is bumped once, and one
`Sim.Command.Types.SimReactionCommitted` carries the post-commit terrain
and generation for every participant. A participant that received no
stone keeps the generation it had.

**A rejected result takes its own fluid with it.** The writebacks for a
stale result's participating chunks are quarantined — they are the other
half of the same reaction, and landing the annihilation without the stone
would destroy volume with no product and then be the state the
convergence re-seed reads back as authoritative. Every participating
chunk is then re-seeded from the authoritative tiles through
`syncEditToSim`, which is what restores the consumed lava and fences any
sim output still in flight from the refused state. A rejection is a
completed stale-result decision, not a partial commit: the delivery still
acknowledges `FluidAckApplied`.

**Exact active volumes survive the handoff.** `SimChunkEdited` rebuilds a
chunk's active grid from the passive `FluidMap` through
`fluidCellToActive`, whose `depth * volumePerLevel` rounding turns the 1
unit a reaction left in the contacting water cell into 7.
`SimReactionCommitted` therefore does NOT re-seed an active chunk: it
adopts the post-edit terrain and generation and KEEPS the live grid
(`Sim.Chunk.applyReactionCommit`). An inactive or absent chunk has no
exact volumes to keep and re-seeds from the passive map as before.

An ACTIVE chunk's solidified cell is DISPLACED, not emptied. One z of
terrain arrived under it, so exactly one level's worth of volume no
longer fits and whatever stood above that still does — the same rule
`World.Edit.Apply` applies to the passive cell, in volume terms. Clearing
it outright would contradict the refill policy above: a cell emptied by
annihilation is an ordinary empty destination for the rest of that tick,
so the cell an event names may be holding water again by the time the
commit lands, and deleting it would then be carried into the tiles by the
next generation-correct writeback.

The INACTIVE branch is not displaced at all. It rebuilds from the
post-edit passive map, which `World.Edit.Apply` has already taken that
level out of, so displacing again would charge a deep cell twice — and
that branch is reached in ordinary play, because a synchronous fast
settle drains reaction results only after settling its chunks inactive. This applies while the chunk is active; it
changes neither the serialized nor the passive representation.

**The acknowledgement still means applied.** Reaction commits run inside
`handleApplyFluidsCommandWith`'s `try`, so a delivery with an ack reports
success only after every accepted edit and its fluid state have landed,
and a raise acknowledges `FluidAckFailed` before the exception leaves the
handler (#2334).

**Both live presentations refresh, with no page reload.** The detailed
tile render rebuilds its quads from the chunk the edit replaced, so
dropping the quad caches is all it needs. The zoom map is NOT: its
renderer samples a precomputed atlas
(`World.Render.Zoom.Quads.renderFromBaked` reads `wsZoomCacheRef` and
`wsZoomAtlasRef`, and `ensureBakedAtlas` only re-derives QUADS), whose
terrain pixels are produced once at page initialization. Clearing
`wsZoomAtlasRef` to force per-material baking is not a repair either —
that path colours a whole chunk by its majority material, in which one
new stone tile cannot appear. An accepted commit therefore refreshes
BOTH of the zoom map's own inputs:

* the per-chunk SUMMARY entry in `wsZoomCacheRef` — which the baked
  quads carry — recomputed from the live chunk and threaded through ONE
  vector, so a delivery touching two chunks does not have the second
  write restore the first chunk's original entry; and
* the atlas BLOCK, regenerated from the live post-edit chunk and patched
  into the image the page retains (`wsZoomLiveRef`), then republished
  through the same `zoomAtlasDataRef` handoff a fresh init and a load
  publish use, targeted at the exact `WorldState` that accepted the edit
  (#763/#1670).

The regeneration's override set comes from the CHUNK'S OWN EDIT LOG, not
from the delivery: the block is rebuilt from generation-time data, so
overriding only the cells one commit touched would repaint every earlier
edit in that chunk back to its generated appearance. Diffing live against
generated would carry them too but is not the same thing — a loaded chunk
and `generateZoomTerrain` disagree on far more tiles than any edit
touched, and following that would repaint the whole block.

**A page with a zoom map has an atlas — that is an invariant, not a
hope.** `World.Load.Stage` assembles and retains one for EVERY staged
page, not only the session's atlas owner, and a page whose atlas the
device refuses drops its zoom CACHE with it (`World.Thread.Command.Init`
does the same on a fresh world). So the only pages without an atlas are
the ones with no zoom map at all — arenas, and refused pages. This
matters because the alternative presentation cannot be refreshed per
tile: `World.Render.Zoom.Bake.bakeEntries` colours a whole chunk by its
majority material, so a map rendered that way would silently stop
tracking the world the first time anything was edited. Only the owner's
image is handed to the GPU at load; a non-owner's first refresh publishes
its own. #1670 is unchanged — a page still only ever renders through an
atlas its OWN cache produced; what it governs is who receives an upload,
not who may have one.

**The handoff is a queue, and GPU ownership is per page.**
`zoomAtlasDataRef` holds one pending image per page: two pages can commit
between render frames, and a single slot would drop one while its page
kept retained pixels its displayed texture no longer matched. A second
refresh of one page replaces that page's own pending entry, so a busy
page cannot queue without bound; a world init or a load publish
supersedes only the payloads for the page (or session) it rebuilds.
Supersession is keyed by the PAGE ID
(`Engine.Core.State.queueZoomAtlasUpload`) and never by the target
`WorldState`s: a same-id reinitialization builds fresh refs, so keying on
those would leave the previous incarnation's image queued for a page that
no longer exists. `GraphicsState`'s `zoomAtlasTextures` is keyed per page
too (`replaceZoomAtlasTextures`), because with one slot an upload for
page B disposed the texture page A's `wsZoomAtlasRef` still named and
left A sampling a dead handle — and entries whose page is gone are
retired every frame (`retireZoomAtlasTextures`), since nothing uploads
for a destroyed, reinitialized or replaced page and its GPU image, view,
sampler and bindless slot would otherwise live until shutdown. The new
texture handle is what makes `ensureBakedAtlas` drop the entries baked
against the old one, and the commit drops them directly too, so a
refreshed summary shows on the very next bake rather than waiting for the
upload.

Gates: hspec `--match "unlike-fluid reaction"`
(`test-headless/Test/Headless/Sim/Reaction.hs`) — one fixture per branch
per ordering, plus the live-source, capacity-edge, refill and
event-accumulation cases. hspec `--match "solidification"`
(`test-headless/Test/Headless/World/Solidification.hs`) — the product
predicate clause by clause in both contact orderings, the grouping and
admission rules, the exact-volume handoff, the atlas patch, and
world-thread integration against the real
`World.Thread.Command.applyFluidWritebacks` for the durable commit, a
delivery carrying the reaction's own writeback beside its stone,
sibling events at one generation, accepted and rejected cross-chunk
results, an evicted participant, convergence, a refused pre-commit
writeback, acknowledgement ordering, a missing product material, a real
eviction and regeneration, the cumulative same-chunk zoom refresh, a
page with no atlas to patch, and the per-page publication queue.
`tools/fluid_reaction_probe.py` is the fresh-process durability case,
and also the alias check for the `world.getMaterialAt` query both probes
read the product through; `tools/fluid_reaction_visual_probe.py`
(offscreen, needs a GPU) is the two-presentation evidence — it locates
the zoom change's own region and asserts it reads as the product the
reaction chose, rather than accepting a whole-frame delta. The neighbouring groups `Sim.Fluid.Seam`,
`Sim.Fluid.Conservation` and `fluid writeback staleness` must stay green
unchanged; `Sim.Fluid.Conservation`'s randomized sweep is Lake-only, so
the reaction never fires in it and a change there is a regression in
compatible transfers, not a fixture that needs relaxing.

---

## Blood decals: transience (#603)

Architecture record: [`docs/blood_decals.md`](blood_decals.md). Six
`--match`-able hspec groups under `test-headless/Test/Headless/Blood/`:
`Blood.Types`, `Blood.Texture`, `Blood.Impact`, `Blood.Trail` (includes
`Blood.Pool`), `Blood.Teardown`, and `Blood.LuaApi` (the registered-Lua
`blood.gpuHandles` query, #1585, on its own isolated engine). Probes:
`blood_decal_probe.py`, `blood_impact_probe.py`,
`bleeding_trail_probe.py`, and the needs-GPU
`blood_gpu_lifecycle_probe.py` (manual-only). **Transience contract**:
blood is transient BY DESIGN — `wsBloodStoreRef` and every unit's
`TrailState` are deliberately never persisted, and a loaded session
always starts with no decals and no accumulators. A test asserting a
mark survives a save/load round trip is testing for behavior this
engine deliberately does not have (closed issue #884 is the spec for
reversing it).

---

## Logging streams

Event log: `engine.getEventLog()`, emit via `engine.emitEvent(cat,text)`
/ `emitEventAt` / `emitEventForUnit(cat,text,uid[,gx,gy])`; a category
lands only if its notifications YAML has `log: true`. Combat:
`combat.drainEvents()`. Injury (NON-combat only — falls, hazards, wound
deaths): `injury.drainEvents()`. These are DRAINED streams — don't
drain manually in a test while the panel script is loaded, or you'll
race it. Gate: `injury_log_probe.py`.

---

## Config state and legacy migration (#638/#786/#1937)

Settings save to gitignored `config/*.local.yaml`; boot falls back to
tracked `*_default.yaml` (notifications self-materializes from
`data/notification_categories.yaml`; `save` resolves as an explicit
KEY-LEVEL overlay instead, so a sparse local file keeps every tracked
default it doesn't mention). The tracked legacy
`video.yaml`/`keybinds.yaml`/`notifications.yaml` exist ONLY as a
one-time migration source: `Engine.Core.Init.migrateLegacyConfig`
copies a legacy file to the local path iff the local file is absent AND
the legacy file decodes against the real target schema; failures fall
back to defaults and never touch a valid local file.

**A neutral placeholder is NOT promoted (#1937).** Those tracked legacy
files hold the versioned default's own content, and copying that was
never a no-op — it froze the then-current defaults as durable local
state that outranks the template for ever after, so a revised shipped
value never reached anyone who booted once and never saved. Video and
keybindings therefore pass a `LegacyNeutralityCheck`: a legacy file
whose DECODED value (not its bytes) equals the tracked
`_default.yaml`'s is recognized, not copied — the local file stays
absent and the log line is deliberately not the migration line. The
determination is recorded in a gitignored
`config/*.legacy-neutral.local.yaml` so a LATER revision of that
template cannot make the untouched placeholder look like player state;
a legacy file the player really edited still migrates, with the
unchanged `Migrated legacy config <legacy> -> <local>` message.
Notifications get no check (`Nothing`) and keep the unconditional copy:
they have no tracked template to be neutral against, and an absent
overrides file already defers to `data/notification_categories.yaml`.
Gates: hspec `--match "config"`, `tools/config_migration_probe.py`,
`tools/config_state_probe.py`.
