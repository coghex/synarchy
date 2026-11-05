# Pre-release save policy design

Before public release, Synarchy needs reliable saves of the current game, not
an obligation to migrate disposable development saves. This arc removes the
mandatory historical compatibility work while preserving versioned refusal,
current-session persistence, and the save/load transaction.

Design state: `exploring`

Owner: `coghex/synarchy`; publication branch: `master`.
Request: `user-syn-save-policy-20261105`, owner via the Synarchy manager,
2026-11-05. Task brief read first:
`~/.local/state/project-manager/synarchy/tasks/save-policy-design.md`.

The policy decisions below are settled. The implementation proposals and
slice boundaries still need readiness signoff; Q-1 and Q-2 are open. This
document is published for review and resumption, not authorization to file
issues or change the runtime. No engine, probe, build, or test was run while
writing it.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Retire mandatory pre-release legacy-save compatibility
- [ ] PSP-1. Prove safe unsupported-version refusal at the load boundary
- [ ] PSP-2. Enforce current-schema version changes without requiring migrations
- [ ] PSP-3. Separate current-save auditing from historical compatibility auditing
- [ ] PSP-4. Separate historical test obligations from current persistence coverage
- [ ] PSP-5. Activate the pre-release gate set and publish the operating policy

## Epic contract

- **Goal:** A pre-release save-format change can advance its actual on-disk
  version and safely refuse older saves without creating a migration, frozen
  outgoing DTO, or permanent historical fixture.
- **Done when:** All five slices are accepted; normal CI and local gates
  require current-save correctness and the agreed version-change enforcement,
  but not historical migrations, corpus preservation, or historical fixture
  reproducibility. At least one real-engine current-version save, process
  exit, fresh-process load, and resave remains selected for persistence changes.
  The authoritative rules agree and a public-release restoration checklist is
  recorded. The separate migration-probe opt-in work must also be confirmed
  delivered before declaring the whole policy operational.
- **Users and operators:** The owner loading development saves; contributors
  evolving persisted gameplay; reviewers and CI maintainers.
- **Arc label:** Existing `save-load`; no new label proposed.
- **Release boundary:** Reinstating the full regime is a release prerequisite,
  not a reason to leave this pre-release epic open until release.

## Verified current state and evidence

Evidence was read from `master` at
`ad35e6f7cdf82dbc389c6f3ced91cb8a7277261f` on 2026-11-05. Paths below
are repository-relative. Recheck them when processing each slice.

### Versions and refusal

- `src/World/Save/Component/Types.hs`, `componentCodec`, constructs both
  `ccInputVers` and the decode dispatch from `csVersion` and
  `csOlderVersions`. An unknown version, including an older version with no
  registered decoder, returns `DecodePhase` with "unsupported schema
  version (reader supports ...)" before invoking a DTO decoder. A registered
  older version still decodes through its `atVersion` DTO and conversion.
  `olderVersionTableError` rejects duplicate/current/future older-version entries.
- `atVersion` calls `Data.Serialize.decode` on the selected DTO. There is no
  payload shape identity beyond the component version. Changing a shape at
  the same version invokes the changed decoder: truncation or validation may
  fail, but equal-width field permutations or constructor-tag reinterpretation
  can silently produce a different valid value. Checksums authenticate the
  bytes' integrity, not the meaning assigned to their positions. This is a
  conclusion from the dispatch and Generic encoding, not a runtime experiment.
- `src/World/Save/Types.hs` explicitly describes `currentSaveVersion` as an
  exported, consumer-free bookkeeping marker for the transitional in-memory
  `SaveData`/`WorldPageSave` bridge. Bumping it does **not** gate component
  decoding. The real boundaries are component `csVersion`, the separate
  metadata version, Lua module versions, and `currentEnvelopeVersion` for
  framing. `Envelope/Codec.hs:decodeEnvelope` checks the framing version
  before decoding the manifest.
- `src/World/Save/Envelope.hs:decodeMetadataComponent` dispatches exact
  accepted metadata versions. `scripts/lib/save_modules.lua:prepareLoad`
  checks `inputVersions` before data decoding and calls module decode and
  validation before application. Both have readable version errors, although
  the exact wording differs from the task's proposed "unsupported save version".
- `Envelope.hs:decodeSessionEnvelopeClassified` classifies component errors
  as `GenerationIncompatible`; its B1 and B2 fallback recognizers accept only
  their particular legacy shapes. `Storage.hs:selectLoadGenerationUnsafe`
  returns incompatible-authoritative failures directly, without loading
  `.prev`. Corrupt generations retain their separate recovery behavior.
  The transaction contract preserves the old session on pre-publication
  failure and leaves it paused. Post-publication reconciliation failures are
  different and cannot promise rollback.
- Keeping an old declaration is not proof of a frozen wire shape.
  `Component/EntitySimulation.hs:UnitSimStateDTOv1` still embeds live
  `Pose`, `UnitActivity`, and `Direction`; historical entity DTOs also share
  live enum leaves. `docs/engine_contracts.md` §Enum append-only audit already
  identifies this caveat. Changing one shared type can require refusing
  several historical component versions and a B1/B2 route, not just bumping
  the newest component.

### Mandatory compatibility machinery

| Surface | Current obligation and consequence for this arc |
|---|---|
| Root `CLAUDE.md` positional-serialization bullet; `src/World/Save/CLAUDE.md`; `docs/persistence_contract.md` §5; `docs/engine_contracts.md` §Enum append-only audit | Require frozen outgoing shapes and explicit migrations beyond append-only evolution; §5 promises compatibility from B1 onward. Replace the pre-release obligation, keeping positional safety and transaction semantics. |
| `tools/save_compat_audit.py` and `save_compat_audit_{components,manifest,fingerprint,codec}.py` | Discover real Haskell/Lua registrations; check manifest declarations, fixture existence/hash, accepted versions, required/current/oldest coverage, B1 helper coverage, framing and frozen-DTO fingerprints, real descriptors, and orphan files. A valid bump currently creates corpus work. |
| `docs/save_compat/manifest.json` | Has 51 active baselines and 57 fixture entries at the inspected commit, rather than the task brief's earlier 55-fixture count. Entries include complete-session and component-focused fixtures. Retirement history already has a separate `retiredBaselines` field. |
| `test-headless/data/save-compat/` | Contains 110 files including binaries and summaries. This is not 110 independent fixtures. It mixes historical contracts with useful current behavior checks; indiscriminate directory deletion would lose the latter. |
| `save_compat_audit_fingerprint.py:frozen_dto_fingerprint` | Fingerprints B1 `SessionV90` declarations and their transitive component DTO leaves. This is a historical anchor, not a complete per-current-component schema/version guard. Framing fingerprinting separately covers manifest declarations, header bindings, and effective Cabal extensions. |
| `tools/enum_append_only_audit.py` and its owner modules; `docs/save_compat/enum_baseline.json` | Guard all multi-constructor Generic/Serialize declarations under `src/` and `app/`, even off-wire ones. Constructor order and payload signatures are checked. `--update-baseline` refuses non-appends regardless of version bumps. Single-constructor records, changes hidden behind instances/synonyms, and swaps of indistinguishable positional fields are outside its complete detection guarantee. |
| `tools/test_save_compat_audit_coverage.py` | Pins oldest/current/required coverage, B1 helper obligations, descriptor-backed claims, real-manifest validity, and orphan detection. Removing only production audit requirements leaves these ratchets behind. |
| `tools/test_save_compat_audit_codec.py` | Checks actual corpus decoding, generated summaries, normalized bytes, helper errors, and `_EXPECTED_DESCRIPTORS` for discovered fixtures. Every added fixture presently needs its independent oracle row. |
| `tools/test_save_compat_audit_reproducibility.py` | Its live `TESTS` list contains both timestamp normalization and seeded-tag reproducibility, including historical fixture inputs. Comments still saying "one member" are stale. `--only-reproducibility` follows the list. Codec parity/re-encoding checks also exist outside this partition. |

The memory note `gotcha_component_version_bump_needs_fixture.md` describes
three coupled ratchets, all verified in the current tree: baseline plus real
fixture, current-version test pins, and the descriptor oracle. Lua pins are
now v11 in `Lua/UnitAiHold.hs`, `UnitAiLocations.hs`,
`UnitAiYieldProximity.hs`, `UnitAiHarvest.hs`, and
`Lua/SaveModules/Components.hs`; some also demand every historical input
version. Current-version assertions still have value; requiring the entire
old version range is a separate promise that must become optional.

### Tests, CI, and the coverage that stays

- `test-headless/Spec.hs` registers **save migrations** and **save components**.
  There is no literal **save compatibility** Hspec group in the inspected
  source; older prose uses that name. Processing must name actual nonempty
  groups, not accept an empty `--match` run. `save components` includes
  `Components/Compatibility.hs` as well as registry, assembly, integrity, and
  current-payload tests. `save migrations` includes the manifest/B1/B2
  families, but also unknown-optional-data and overwrite protections,
  `Compat/TagTargets` staging, and `SimExactFluid.saveSpec`. Those present-day
  protections must survive any historical-suite separation.
- `tools/ci-local.sh` runs the main audit and
  `test_save_compat_audit.py --without-reproducibility`; its `save-compat`
  selection block runs `--only-reproducibility`. `Makefile` simply invokes
  that script. `.github/workflows/ci.yml` has the corresponding commands,
  docs-only conditions, and `tools/ci_expensive_gates.py` selection.
  `tools/ci_parity_audit.py` and `ci_parity_save_compat.py` enforce command
  arguments and the executable selection behavior, not just names.
  Changing CI alone or making the selector always false is insufficient.
- `tools/persistence_inventory_audit.py` and its Haskell/Lua/policy owners
  check root-state classifications, registered component ownership, typed
  references, and the coverage map. They do not import the compatibility
  audit, decode historical fixtures, inspect `csOlderVersions`, or require
  migrations. Their references to migration decisions in documentation do
  not impose a historical decoding gate. Keep these audits and the capability
  audits unchanged; keep inventory coverage links valid when tests move.
- `tools/persistence_contract_probe.py` creates saves with the running build
  and performs three fresh-process load/resave cycles, plus focused current
  behavior cases. It is CI-eligible in `tools/ci_probes.py` and registered in
  `tools/probe_runner_registry.py`. It uses `tools/persistence_snapshot.py`
  and the compiled `exe:synarchy-save-codec` through the compatibility tool's
  shared bridge. Preserve that bridge and comparison protocol even if
  historical checking becomes optional. The generic round trip does not
  replace feature-specific runtime coverage such as the camera-ring
  admission failure identified in #2557/PR #2911.
- The separate opt-in slice owns `save_compat_migration_probe.py` selection,
  including the `persistence_contract_sweep.py` default cross-probe path and
  any runner/lab selection changes. This design does not redraft it.
  Registry edits there require the existing runner/sweep and Quruntul gates.

Tracker check: no open pre-release-save-policy epic was found. CI epic
#2742 overlaps gate wiring, not this policy decision; coordinate changes to
its gate-selection surfaces. Open #2564 concerns legacy faction-enum
retirement; it is not blanket authorization to delete save migration code.
Recheck these overlaps and obtain the manager's opt-in issue number at
readiness. No tracker artifact was created or modified for this design.

## Decisions

### D-1. Development saves may become unsupported before public release

Owner decision in the task brief: no important saves need migration. A format
change advances the relevant version; older saves may be refused. New
migrations, frozen historical DTOs, and historical fixtures are not required.
This does not authorize same-version reinterpretation or weakening the
current save/load contract.

### D-2. Restore the full compatibility regime at public release

Owner decision in the task brief. Release must restore supported-baseline
tracking, migration coverage, and the full verification obligations described
below. Which baseline begins that promise is deliberately left to Q-2.

### D-3. Migration-probe opt-in is a separate existing work item

Owner decision in the task brief; the manager is filing that small issue.
The probe must run only when explicitly wanted before release. Do not create
a duplicate child or use that change as proof that the rest of the regime is
retired.

### D-4. Preserve working code, safe refusal, and current round trips

The task explicitly directs adoption of the manager's recommendations unless
the code supplies a contrary reason. Keep existing migrations and DTOs;
stop requiring new ones. Retire or regenerate historical fixtures when they
break, retaining only the current fixtures tests need. Keep a real-engine
current-version round trip. No contrary evidence requires reopening these
recommendations. Disabling an unsafe decoder registration after its shared
wire types change is required for safe refusal; it is not a request to delete
the migration implementation.

## Desired behavior and scope

For a persisted-layout change, a contributor advances every affected on-disk
component version, updates its current tests, and exercises current save/load.
There is no automatic requirement to preserve the outgoing layout or append
another historical baseline. The contributor may leave existing, genuinely
safe migrations registered. Unsupported saves fail before publication with
a clear **unsupported save version** diagnostic naming the component or
envelope, the found version, and supported versions; the prior session remains
intact and paused. The file is not rewritten and an incompatible authoritative
generation does not silently fall back to `.prev`.

In scope: the rule texts, schema/version enforcement, historical audit and
test selection, fixture retirement bookkeeping, CI/local parity, and release
restoration instructions. Out of scope: implementing gameplay, redesigning
the envelope, deleting working migration code, migrating the owner's saves,
restoring deterministic replay, relaxing content/reference integrity, changing
optional-component default semantics, and changing unrelated worldgen gates.
No new runtime state or serialized policy flag is needed for this design.

## Design proposals

### P-1. Keep exact version dispatch; make refusal the normal escape hatch

Use the existing dispatch architecture. On a wire-shape change, advance every
carrier's actual component version, including metadata or Lua where relevant.
A framing-layout change advances the framing version. Keep the bridge marker's
existing bookkeeping rule, but never present its bump as on-disk protection.
Require a version advance for every shape change, including enum appends; a
pure implementation refactor with unchanged encoding is not a shape change.

Old registrations may remain only when their complete transitive decoder shape
is still valid. If a shared record or enum changes, remove affected versions
from accepted dispatch and disable affected exact-shape legacy fallback routes
before they can decode changed layouts. Preserve their source implementations
where practical. Do not silently repair old payloads through a current DTO,
catch a decode failure and retry it as the newest version, or equate optional
absence with an optional component carrying an unsupported payload.

Tests must use deliberately distinguishable values: an equal-width two-field
swap can decode successfully while changing meaning. A codec round trip
written and read by the same changed code cannot detect that error. Check the
old version against the new accepted-version policy before payload decoding,
as well as normal malformed-byte and current-round-trip cases.

### P-2. Replace historical immutability with current-schema/version enforcement

**Conditional on Q-1.** Recommended enforcement is a small source-based guard
recording the current wire shape and version of each component and framing
boundary. Reuse the existing discovery, enum-carrier, and fingerprint helpers
where sound; do not repurpose the B1 fingerprint as if it already covered all
current wire types. Include single-constructor records, nested enums and DTOs,
newtypes/type aliases, and explicitly identified custom serialization owners.
Missing or ambiguous coverage fails rather than silently omitting a component.

Compare the candidate with the base revision's schema/version record; a
changed digest at an unchanged version must fail even if the candidate
regenerates its baseline. Each affected carrier must advance, and any retained
historical dispatch depending on the changed shape must be demonstrably safe
or refused. The guard's baseline is a small schema record, not a binary save
corpus or a growing history of releases. Pure comment/layout edits should not
demand a bump. The implementation must state its limits: text inspection cannot
prove arbitrary serializer semantics or see swapping two indistinguishable
positional values. Such encoding/meaning changes still require a bump, explicit
review evidence, and a targeted distinguishing test.

Under this proposal, the enum audit gains a pre-release, version-aware update
path instead of a blanket non-append veto. Its historical append-only mode
remains available. A changed off-wire enum does not create a fictional save
component version, but newly reachable types cannot escape coverage. Keep
existing strict checks in force until the replacement has negative tests and
is wired into the mandatory gate set. Do not remove the old checks first.

If the owner chooses review enforcement instead, replace this proposal and
PSP-2's scope with an explicit review contract and its residual detection
limits before readiness. Do not quietly substitute review for an approved
automated guard.

### P-3. Split current correctness from optional historical evidence

Provide explicit current and historical audit/test selections. The current
selection retains discovery correctness, honest required/optional registration,
malformed/unsupported refusal, current codec behavior and bridge errors, and
the agreed schema guard. Historical selection owns the B1 frozen fingerprint,
oldest-version obligations, historical baselines, migration expectations, and
historical corpus reproduction. Synthetic tests of those retained tools can
remain cheap default tests if they do not impose the real historical corpus
as a prerequisite.

Remove the requirement to add a new permanent baseline on every component
bump. Maintain a small replaceable current fixture set only for tests that
need real bytes. Update its summaries, checksums, and independent descriptor
expectations together, exercising nonempty changed data; do not weaken a
descriptor oracle into comparing the implementation to itself. Current-version
pins remain conscious assertions, but no longer demand an uninterrupted range
of historical versions.

Retire a broken historical fixture with its manifest entry, summary, oracle
row, and named consumers in the same change. Preserve the retirement reason
and original provenance in the existing retirement record; never relabel old
bytes as current or claim they remain supported. Before removing any test,
retain its current behavioral assertion using current fixtures or synthetic
current values. Optional historical runs must report absent/retired coverage
honestly; a reduced corpus is not proof of release compatibility. Bulk fixture
or DTO deletion is not a prerequisite to completing this arc.

### P-4. Activate one consistent pre-release policy across all entry points

CI, `make ci`, direct audit commands, Hspec registration, and documented review
gates must agree. Path selection alone must not re-enable historical obligations
on `master` pushes, on Cabal changes, or through the default self-test façade.
An explicit historical invocation remains available. Keep the current
fresh-process probe selected under its existing persistence-change policy;
there is no need to reduce its three existing cycles to one.

The current schema guard and current codec checks remain mandatory. Retain
framing safety checks; only historical immutability/reproduction obligations
become optional. `ci_parity_save_compat.py` must test the new actual policy,
including absence of implicit historical work, rather than deleting its parity
assertions. The standalone migration-probe opt-in issue owns probe defaults;
verify that dependency without expanding this design into its issue body.

## Open questions

### Q-1. Automated current-schema guard or review enforcement?

P-2 recommends the automated guard; review-only enforcement is smaller but
does not mechanically detect arbitrary same-version record drift. This is a
material engineering tradeoff, not a settled owner choice. It blocks PSP-2
and the activation of relaxed historical rules in PSP-5. PSP-3/PSP-4 may be
specified as preparatory partitions, but must not disable their replacement
safety obligations before this choice is settled.

### Q-2. Which saves enter the release compatibility promise?

Recommendation: establish the first public release as the supported baseline
and preserve all subsequently supported release formats; do not resurrect
retired pre-release saves. The other reading of "full regime comes back" is
restoring the B1-and-later pre-release corpus too, which entails recovering
retired fixtures and repairing their decoders before release. The owner must
choose. This affects the release checklist and PSP-5's policy text, not D-1's
permission to refuse development saves now.

Both questions were posted together to `#syn-save-policy` as
`syn-designer-1`, verified message `3sx3nmsq853m5tupnjda739vzi`, under this
request. No answer or design-readiness signoff has been recorded. On receipt,
record the verified answer and resolve each Q-N to a new D-N; then present
the resulting design/slice boundaries for readiness. Keep this document
`exploring` until then.

## Verification strategy

Implementation gates below are future requirements, not claims of execution
during this read-only design task. Build the production profile and headless
suite when needed. Use targeted, nonempty Hspec groups; do not run the whole
suite or `make ci` merely to prepare an issue. Full local CI requires the
owner's explicit request.

The end-state evidence must demonstrate: current nonempty data survives real
codec and fresh-process round trips; unsupported older and future versions
fail before publication; a known old version uses only its correct decoder;
optional-present errors cannot default away; same-version shape changes fail
the agreed enforcement; historical fixture failure cannot accidentally block
the default pre-release path; and CI/local selection remains equal. Inventory,
reference integrity, storage recovery/protection, pause/reset semantics, and
feature-specific persistence tests retain their present obligations.

Runtime evidence must use isolated resources and owned processes, wait for
terminal save/load statuses, and preserve the non-8008 port/launch rules. The
existing current probe supplies process-restart coverage; feature changes
still need their own loaded-world/chunk-admission evidence. Headless codec
tests alone did not expose the #2557 camera-ring admission problem.

## Delivery plan

These are proposed one-PR slices, listed in valid dependency order. No issue
numbers are reserved. All slices require design readiness before tracker
processing; a Q-N stop applies in addition to that global readiness gate.

### PSP-1. Prove safe unsupported-version refusal at the load boundary

- **Outcome:** Existing refusal paths have explicit tests and consistent
  user-facing unsupported-save-version diagnostics, without dropping working
  migrations or bumping the format just for this policy change.
- **Scope / touched paths:** `src/World/Save/Component/Types.hs`,
  `Envelope.hs`, `Envelope/Types.hs` as diagnostic rendering requires;
  `scripts/lib/save_modules.lua`; relevant `World/Save/Components/Registry.hs`,
  `World/Save/Storage.hs`, `World/Save/Envelope.hs`, Lua save-module and
  transactional-load tests. Extend the existing transaction probe only if
  its unsupported-version preservation case is missing.
- **Phase / ordering:** 1, can land first. **Depends on:** none.
- **Relevant decisions:** D-1, D-4. **Open questions:** None for refusal;
  no choice of the Q-1 enforcement mechanism in this slice.
- **Acceptance signals:** Required and optional-present unsupported component,
  metadata, Lua and framing versions fail clearly before application; malformed
  bytes remain distinct. A registered old decoder works unchanged. Incompatible
  authoritative save does not fall back to `.prev`; prior session and queued
  work survive, paused. Current round trip remains valid.
- **Gates:** `save components`, `save envelope`, `atomic save storage`,
  `Lua persistence components`, `persistence contract`; targeted transaction
  tests and `tools/transactional_load_probe.py` when its path changes.
  Run the existing `persistence_contract` probe for the runtime boundary.
- **Out of scope:** Changing accepted version sets globally; fixture retirement;
  new schema guard; broad transaction refactoring.

### PSP-2. Enforce current-schema version changes without requiring migrations

- **Outcome:** Under Q-1's recommended option, mechanical shape changes cannot
  pass by refreshing a golden file at the same version; compatible refusal
  replaces the demand to author migrations.
- **Scope / touched paths:** `tools/enum_append_only_audit*.py`, its generated
  baseline and self-tests; reusable `save_compat_audit_{components,fingerprint}.py`
  discovery; a focused new current-schema guard and small schema baseline if
  required. Owner-visible declarations and custom-codec coverage are explicit.
  Policy comments in `Component/Types.hs` describe retained-decoder safety.
- **Phase / ordering:** 2, critical path. **Depends on:** PSP-1.
- **Relevant decisions:** D-1, D-4. **Open questions:** Q-1; stop before
  drafting a final issue until the enforcement approach is selected.
- **Acceptance signals:** Negative tests cover unchanged-version record field
  add/remove/reorder/retype, enum change, nested shared shape, partial carrier
  bump, baseline-only rewrite, unknown coverage, and a retained unsafe older
  decoder. Correct version advance plus safe refusal passes without a new
  migration/legacy fixture; comment-only edits do not cause false bumps.
  Custom serializer/semantic limitations and required review evidence are stated.
- **Gates:** Guard self-tests including synthetic base/candidate trees;
  `enum_append_only_audit.py --self-test`; discovery/fingerprint self-tests;
  `save components` for dispatch invariants. Existing mandatory gates remain
  active until PSP-5 switches policy.
- **Out of scope:** Historical corpus expansion, whole-serializer formal proof,
  deleting migrations, or disabling legacy checks before activation.

### PSP-3. Separate current-save auditing from historical compatibility auditing

- **Outcome:** Audit and Python self-test entry points can select current
  correctness independently of the historical corpus and its reproducibility.
- **Scope / touched paths:** `tools/save_compat_audit.py`, its common,
  components, manifest, fingerprint, registration and generation owners;
  `tools/test_save_compat_audit*.py`, particularly coverage, codec, comparison,
  and reproducibility selection; `docs/save_compat/manifest.json` if explicit
  selection metadata is needed. Preserve `save_compat_audit_codec.py`'s
  production-codec API for `persistence_snapshot.py` and the current probe.
- **Phase / ordering:** 2, independent preparation after PSP-1.
  **Depends on:** PSP-1.
- **Relevant decisions:** D-1, D-3, D-4. **Open questions:** No new policy
  choice beyond Q-1's separately owned replacement enforcement.
- **Acceptance signals:** Current mode does not demand oldest-version coverage,
  B1 helper completeness/fingerprint stability, or a permanent baseline per
  bump. Explicit historical mode still diagnoses those obligations honestly.
  Current descriptors, checksums and helper failure behavior are still checked
  for the current fixtures tests actually use. Synthetic negative tests prove
  classification and neither mode silently runs the other.
- **Gates:** The façade's targeted current/historical self-test selections;
  codec and comparison self-tests through the compiled helper; current audit;
  one explicit historical audit against the still-intact corpus to validate
  partitioning. No migration engine probe is required.
- **Out of scope:** Changing CI defaults, Hspec registration, retiring all
  fixtures, or altering production serialization.

### PSP-4. Separate historical test obligations from current persistence coverage

- **Outcome:** Hspec and fixture consumers distinguish optional legacy promises
  from mandatory current behavior, and fixture retirement is an atomic operation.
- **Scope / touched paths:** `test-headless/Spec.hs`,
  `World/Save/Compat*.hs` and its families, `World/Save/Components/Compatibility.hs`,
  the Lua current-version pins listed above, `test-headless/data/save-compat/`,
  `docs/save_compat/manifest.json`, descriptor/summary consumers in the Python
  codec and reproducibility tests, and Cabal registration if needed. Inspect
  the extra `save migrations` registrations for tag staging and exact fluid
  state; retain their current semantics in mandatory groups. Update inventory
  coverage links only if renamed tests require it; no audit relaxation.
- **Phase / ordering:** 3, critical path. **Depends on:** PSP-3.
- **Relevant decisions:** D-1, D-4. **Open questions:** None about preserving
  current coverage; activation still waits for Q-1.
- **Acceptance signals:** A nonempty mandatory current suite covers registry,
  assembly, integrity, storage/unknown-optional protection and current payloads.
  Historical migration cases have an explicit opt-in selector with a visible
  count. Removing one obsolete fixture updates every consumer together; a
  needed behavioral assertion is replaced with current data. No empty-match
  success or blanket deletion of `Compatibility`/`Compat` modules counts.
  Current-version tests no longer imply acceptance of every earlier version.
- **Gates:** Actual current save groups, `persistence contract`,
  `persistence reference integrity`, `Lua persistence components`, moved
  feature-specific groups; PSP-3's current self-tests; persistence inventory
  audit and its self-tests. Preserve and verify an explicit historical selector
  while leaving existing default coverage active until PSP-5.
- **Out of scope:** Proactive bulk fixture removal, removal of useful migration
  implementations, reducing feature-specific current round trips.

### PSP-5. Activate the pre-release gate set and publish the operating policy

- **Outcome:** Default tools, CI, Hspec and local gates apply the same
  pre-release contract; contributors can make an intentional format break
  without being forced back into historical migration work.
- **Scope / touched paths:** `.github/workflows/ci.yml`, `tools/ci-local.sh`,
  `tools/ci_expensive_gates.py`, `tools/ci_parity_{audit,config,save_compat}.py`
  and affected parity tests; the audit/test defaults prepared in PSP-2–4;
  root `CLAUDE.md`, `src/World/Save/CLAUDE.md`, `docs/persistence_contract.md`
  §5 and test disposition, `docs/engine_contracts.md` §The `make ci` gate set,
  §Enum append-only audit and save/load wording, `tools/README.md` and relevant
  tool headers. `Makefile` remains a wrapper unless an explicit manual target
  is useful. Check `.quruntul/adapter.py` registration impact without changing
  the lab's authority or blindly editing measured corpus records.
- **Phase / ordering:** 4, critical path. **Depends on:** PSP-1, PSP-2,
  PSP-3, PSP-4; confirm the manager's separate probe opt-in item has landed.
- **Relevant decisions:** D-1–D-4. **Open questions:** Q-1 and Q-2; stop
  before activation until both and design readiness have explicit signoff.
- **Acceptance signals:** Default invocations require the current guard/tests
  and never historical corpus success; explicit historical invocation works.
  Save/Cabal/fixture changes and master pushes cannot implicitly restore old
  obligations. CI/local parity tests prove both positive current selection and
  negative historical selection. `persistence_contract` remains CI-eligible;
  inventory/capability gates are unchanged. Rules no longer require freezing
  an outgoing pre-release DTO, a legacy baseline/oracle row per bump, or the
  migration probe. They name the actual version boundary and release checklist.
- **Gates:** `python3 tools/ci_expensive_gates.py --self-test`,
  `python3 tools/ci_parity_audit.py --self-test` and bare audit; agreed guard,
  current audit/self-tests and current Hspec groups; inventory audits; a real
  `persistence_contract` probe. If registry/adapter inputs change, also run
  `tools/test_run_probes.py`, `tools/ci_probes.py --self-test`,
  `tools/test_persistence_contract_sweep.py`, and `python3 .quruntul/checks.py`.
  Do not request a full migration probe as acceptance for this policy.
- **Out of scope:** Full local CI without an explicit request, the separate
  opt-in issue's implementation, or reinstating release compatibility now.

Documentation in these implementation slices lands with `docs-push` before
final review and is linked from the PR as `Docs: <path> @ <commit>`. Files
that tools/tests execute or consume, including machine-readable baselines,
remain with code under the repository's docs-push exclusions. Do not publish
rule text claiming activation before the implementing gates are ready.

## Public-release restoration checklist

The release owner must complete this checklist before public release; it is
not a new child issue to file during this design task.

1. Resolve Q-2 and declare the supported baseline and retention promise.
   Freeze the release's full transitive DTO/enum shapes, component versions,
   metadata, Lua schemas and framing; capture real, nonempty release fixtures
   with provenance, checksums, canonical summaries and descriptor oracles.
2. Restore the requirement that evolution of supported formats preserves their
   exact historical decoders and supplies explicit migrations. Re-enable
   historical fingerprint and append-only protections, with explicit migration
   exceptions only through safe frozen representations. Keep current version
   enforcement and transaction/refusal tests as well.
3. Restore supported-baseline manifest coverage: required/current/oldest versions,
   declared-vs-real descriptor agreement, fixture presence/integrity, historical
   canonical-result checks and reproducibility. If Q-2 includes pre-release
   saves, recover their retired records/fixtures and repair every promised
   decoding route before claiming support; git history alone is not coverage.
4. Restore mandatory historical Hspec selection, the migration probe's real
   load/publish/resave/restart/reload coverage, sweep/lab selection, CI and
   `make ci` parity, and the relevant expensive-gate selection. Run the actual
   release validation; turning a flag back on is not evidence of passing it.
5. Publish the restored contracts and contributor version-bump procedure.
   Retain current-version fresh-process tests and all inventory, reference,
   storage, paused-failure and content-integrity guarantees throughout.

## Handoff

This design adds four settled decision records, two open questions and five
proposed delivery slices. There is no divergence from the manager's requested
retention, refusal, fixture or fresh-process recommendations. Q-1 selects how
to enforce the version-bump obligation; Q-2 resolves the scope of the future
promise. The task's authorized endpoint is publication of this document and
reporting its commit in `#syn-save-policy`, not issue creation.

After answers and explicit readiness signoff, `process-design-doc` processes
EPIC first and then exactly one dependency-ready child per invocation, with
separate owner approval for every tracker artifact. Recheck the tracker and
the separately managed opt-in issue then; do not infer readiness from this
document's successful publication.
