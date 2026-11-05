# Pre-release save policy design

Before public release, Synarchy needs reliable saves of the current game, not
an obligation to migrate disposable development saves. This arc removes the
mandatory historical compatibility work while preserving versioned refusal,
current-session persistence, and the save/load transaction.

Design state: `ready for issue processing`

Owner: `coghex/synarchy`; publication branch: `master`.
Request: `user-syn-save-policy-20261105`, owner via the Synarchy manager,
2026-11-05. Task brief read first:
`~/.local/state/project-manager/synarchy/tasks/save-policy-design.md`.

The policy decisions below are settled, including the owner's answers to
Q-1 and Q-2. The owner approved the whole design and its five delivery slices
at revision `faa7f3803d` with "Ready, process it" on 2026-11-05. Verified
`syn-manager` message `f9dh8cp6n3ub4yqain6cx2uzhw` in `#syn-save-policy`
(15:09:22 UTC), replying to `e85gc2qkbhkyzidqtk6iq9cjv6`, relays that
readiness signoff and directs publication of this state change. The manager
launches `process-design-doc` separately; this task creates no issues or
runtime changes. No engine, probe, build, or test was run while writing it.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Retire mandatory pre-release legacy-save compatibility — [#2914]
- [x] PSP-1. Prove safe unsupported-version refusal at the load boundary — [#2915]
- [x] PSP-2. Enforce current-schema version changes without requiring migrations — [#2916]
- [x] PSP-3. Separate current-save auditing from historical compatibility auditing — [#2917]
- [x] PSP-4. Separate historical test obligations from current persistence coverage — [#2918]
- [x] PSP-5. Activate the pre-release gate set and publish the operating policy — [#2919]

## Epic contract

- **Goal:** A pre-release save-format change can advance its actual on-disk
  version and safely refuse older saves without creating a migration, frozen
  outgoing DTO, or permanent historical fixture.
- **Done when:** All five slices are accepted; normal CI and local gates
  require current-save correctness and automated version-change enforcement,
  but not historical migrations, corpus preservation, or historical fixture
  reproducibility. At least one real-engine current-version save, process
  exit, fresh-process load, and resave remains selected for persistence changes.
  The authoritative rules agree and a public-release restoration checklist is
  recorded. Historical tooling, tests and corpus are removed at PSP-5 activation;
  reusable current helpers remain. No historical mode is maintained. The separate
  migration-probe opt-in work must also be confirmed
  delivered before declaring the whole policy operational. D-8 then retires
  that historical probe at activation, superseding its on-demand availability.
- **Users and operators:** The owner loading development saves; contributors
  evolving persisted gameplay; reviewers and CI maintainers.
- **Arc label:** Existing `save-load`; no new label proposed.
- **Release boundary:** Reinstating the full regime is a release prerequisite,
  not a reason to leave this pre-release epic open until release. Build its
  baseline and verification afresh under D-7; do not revive pre-release saves.

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
old version range is a separate promise that D-7 retires.

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
  historical checking is retired. The generic round trip does not
  replace feature-specific runtime coverage such as the camera-ring
  admission failure identified in #2557/PR #2911.
- The separate opt-in slice owns `save_compat_migration_probe.py` selection,
  including the `persistence_contract_sweep.py` default cross-probe path and
  any runner/lab selection changes. This design does not redraft it.
  Registry edits there require the existing runner/sweep and Quruntul gates.

Tracker rechecked at readiness on 2026-11-05: no open
pre-release-save-policy epic was found. The separately managed opt-in issue
is #2912, "[tools] Make save_compat_migration an opt-in probe that runs only
on request"; do not duplicate it. CI epic
#2742 overlaps gate wiring, not this policy decision; coordinate changes to
its gate-selection surfaces. Open #2564 concerns legacy faction-enum
retirement; it is not blanket authorization to delete save migration code.
Recheck these overlaps during issue processing. No tracker artifact was
created or modified for this design.

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
below. D-6 settles the baseline as the first public-release save format.

### D-3. Migration-probe opt-in is a separate existing work item

Owner decision in the task brief; the manager has filed that small issue as #2912.
The probe must run only when explicitly wanted before release. Do not create
a duplicate child or use that change as proof that the rest of the regime is
retired. D-8 later supersedes continued on-demand availability: PSP-5 removes
the probe at activation. #2912 remains a completed intermediate step.

### D-4. Preserve working code, safe refusal, and current round trips

Keep existing migration/DTO source where practical, stop requiring new ones,
and retain current fresh-process round trips. D-7 supersedes the earlier
option to regenerate broken historical fixtures: retire the historical corpus
at activation, preserving its provenance in git history. The owner's later D-5
simplification supersedes the earlier permission to keep safe older decoders
registered on a changed carrier: after a shape change that carrier accepts
only its new current version. Remove its older accepted dispatches and affected
exact-shape legacy routes; deleting their source implementations is unnecessary.

### D-5. Enforce schema/version changes with an automated guard

Owner answer to Q-1: "Automated guard." On 2026-11-05 the owner refined
that decision with "Simplify first": a persisted shape change advances every
affected component, metadata, Lua or framing version, and each affected
carrier then accepts only its current version. Drop its older accepted decoders
and affected exact-shape legacy routes so PSP-1 refuses older saves. Do not
analyze whether a retained historical decoder could still be compatible.

Keep the small generated current-schema record, base-ref comparison that
candidate regeneration cannot bypass, coverage/ambiguity failures, documented
semantic limits and synthetic self-tests. D-9 supersedes source-text discovery
with compiled shape evidence, live registries and behavioral checks. No new
migration or historical fixture is required. PSP-2 supplies the guard; PSP-5 activates it.

This amendment is authorized by verified syn-manager reply
`k8nvcbvvekhi9gqddjdbn86vcs` to PSP-2 question
`3v3vgxseunbsnh4pe9skwxqu6n` in #syn-save-policy, 2026-11-05 16:32:14 UTC.
The reply requests a simplified redraft, explicitly forbids filing it yet,
and directs this design update. It does not approve the revised issue body.

### D-6. Begin the compatibility promise with the first public-release save

Owner answer to Q-2 on 2026-11-05: "First release save." The full
compatibility promise starts at the first public-release save format.
Pre-release saves remain outside that promise and are not resurrected at
release. Existing working migrations may stay under D-4, but do not establish
a support obligation for development saves. Restoring the B1-and-later
pre-release corpus as a release requirement was not selected.

**Decision provenance for D-5/D-6:** verified `syn-manager` message
`uw5x59zk2qw882wu427vaf6csn` in `#syn-save-policy`, 2026-11-05 14:59:08 UTC,
replies directly to question `3sx3nmsq853m5tupnjda739vzi` and relays both
owner answers under `user-syn-save-policy-20261105`. The owner also directly
instructed this session to read that reply and act on it. This authorizes
recording and applying both choices to this design. The later whole-design
readiness signoff is recorded at the top of this document; issue creation
belongs to the manager's separate processing workflow.

### D-7. Retire pre-release historical tooling at activation

Owner answer on PSP-3: "Current mode only". Build only current-save audit and
self-test selection, live registrations, current codec/helpers and a small
current fixture set with independent expectations. No historical mode is built
or maintained. PSP-3 prepares those replacements; PSP-4 preserves current Hspec
and consumer assertions; PSP-5 removes historical machinery in the same change
that switches gates, so strict defaults never break before activation.

Remove the historical audit, 57-fixture corpus and obsolete records, B1
fingerprint/helper checks, historical reproducibility and historical-only tests
from the tree. Retire the strict append-only audit replaced by PSP-2, retaining
reusable current helpers. Git history preserves the retired material. Keep the
APIs/helpers used by persistence_snapshot.py and the persistence_contract probe.
Release compatibility starts afresh at the first public-release baseline; it
does not revive a dormant historical mode or the pre-release corpus.

Authority: verified syn-manager reply `5qm4ui6r7vjk4yfumtujxpxs3s` to
PSP-3 question `p9ne2z4dqarw4v29qbc3rgtpu2`, 2026-11-05 16:44:49 UTC,
relaying the owner and directing the P-3/PSP-3/PSP-4 design changes. It requires
a redraft for separate signoff and explicitly forbids filing that draft yet.

### D-8. Retire the historical migration probe with its corpus

Owner answer: "Retire it in PSP-5". At the same gate switch, remove
`tools/save_compat_migration_probe.py` together with its dedicated self-test,
registry, timeout, sweep, runner, census and lab references. Keep any helper
used by current probes. This supersedes #2912's runnable-on-demand promise;
that completed opt-in issue remains the intermediate step, not ongoing support.
Historical probe source, fixtures and retired measurements survive in git
history. Release verification is built afresh from its first supported baseline.

Authority: verified syn-manager reply `34bhbcdfuuzkcwuqbz3bbq7yi2` to question
`ere9za9kecyg5ht4xk6q37qv9i`, 2026-11-05 17:07:32 UTC, relaying the owner
and directing a design decision record followed by the PSP-5 draft for signoff.
This policy answer does not itself approve the PSP-5 tracker artifact.

### D-9. Read shapes from compiled code and versions from live registries

Owner ruling on 2026-11-05: "Rewrite on compiled code". Close the source-parser
attempt #2922, retain its branch for reference, amend #2916, and obtain fresh
issue approval before a new implementation/review cycle. This supersedes the
intermediate "Strict canonical forms" direction; do not keep expanding a
source parser or require a special source spelling for this guard.

The compiled helper walks actual persisted Haskell DTO Generic/Typeable
representations transitively, including constructor/field order, instantiated
field types, record selectors and newtypes; GHC resolves aliases. Bind the
roots to the actual codecs before type erasure. Current/accepted versions
come from the compiled component registry and metadata/framing values, plus
the evaluated Lua save-module registry after loading the real modules in the
existing headless Lua harness. Loading or coverage failures fail closed.
Verify the real decoder entry points agree with reported acceptance, including
full-session and metadata-only framing and exact-shape legacy paths.

Generic is not proof of a Serialize implementation. Explicitly cover custom,
newtype-derived and library encoding boundaries; fail when unlisted. Lua's
dynamic payloads and hand-written framing require explicit owned contracts
and distinguishing behavioral tests, not an invented GHC representation of
Lua tables. State those limits and semantic changes with identical shape
honestly. Undetected encoding/meaning changes still require version advancement,
current-only acceptance and reviewed distinguishing evidence.

Retain D-5's per-affected-carrier comparison rule and current-only acceptance.
The owner further approved "Approve, committed base record": after the one-time
bootstrap, base evidence is the validated current-schema record committed at the
base ref. Each merged revision passes freshness against its own compiled code.
Routine CI compiles/extracts only the candidate and requires its committed record
to match that evidence exactly; it never builds both trees. Only first
introduction extracts the actual pre-guard base without a record/helper; any
inspection adapter preserves base wire declarations, versions and decode
behavior. Missing, malformed or unvalidated base records fail clearly; never
substitute the candidate record or an empty base, or silently bootstrap again. Keep the record/comparison/test ideas
from #2922 where sound and independent of parsing. PSP-5 owns activation;
strict gates remain until then. This changes no slice boundaries.

Authority: task brief `save-policy-psp2-rewrite.md`, read first for this
amendment, and verified syn-manager message `524565svgg4h9fvs3xfamdgnua`
in #syn-save-policy at 2026-11-05 19:35:19 UTC, relaying the owner ruling.
The brief authorizes this design publication now and requires the complete
#2916 amendment and exact #2919/#2914 consistency edits to be presented for
separate owner signoff via the manager. Verified syn-manager reply
`qhziga3rpj59z96ugaqfmfgs6i` to question `upfj42rmd57qfraqirpb7wias6` at
2026-11-05 19:43:57 UTC relays "Approve, committed base record" for the
complete #2916 body, six #2919 edits and two #2914 edits, with the base-record
change above. The owner directly instructed this session to read and act on
that reply. The approved payload below incorporates the change; no further
signoff is required before applying it.

## Desired behavior and scope

For a persisted-layout change, a contributor advances every affected on-disk
component version, updates its current tests, and exercises current save/load.
There is no automatic requirement to preserve the outgoing layout or append
another historical baseline. Each affected carrier accepts only its new
current version; remove its older accepted dispatches and affected exact-shape
legacy routes. Unchanged carriers need no blanket purge. Unsupported saves fail before publication with
a clear **unsupported save version** diagnostic naming the component or
envelope, the found version, and supported versions; the prior session remains
intact and paused. The file is not rewritten and an incompatible authoritative
generation does not silently fall back to `.prev`.

In scope: the rule texts, schema/version enforcement, historical audit and
test retirement, replacement current coverage, CI/local parity, and release
restoration instructions. Out of scope: implementing gameplay, redesigning
the envelope, deleting working migration code, migrating the owner's saves,
restoring deterministic replay, relaxing content/reference integrity, changing
optional-component default semantics, and changing unrelated worldgen gates.
No new runtime state or serialized policy flag is needed for this design.

## Design

### P-1. Keep exact version dispatch; make refusal the normal escape hatch

Use the existing dispatch architecture. On a wire-shape change, advance every
carrier's actual component version, including metadata or Lua where relevant.
A framing-layout change advances the framing version. Keep the bridge marker's
existing bookkeeping rule, but never present its bump as on-disk protection.
Require a version advance for every shape change, including enum appends; a
pure implementation refactor with unchanged encoding is not a shape change.

After a shape change, each affected carrier accepts only its new current
version, regardless of whether an older decoder might still work. Remove
older accepted dispatches and disable affected exact-shape legacy routes.
Keep their source implementations where practical. Do not reinterpret old
payloads through the current DTO, retry a failed old decode as the newest
version, or equate optional absence with an unsupported optional payload.

Tests must use deliberately distinguishable values: an equal-width two-field
swap can decode successfully while changing meaning. A codec round trip
written and read by the same changed code cannot detect that error. Check the
old version against the new accepted-version policy before payload decoding,
as well as normal malformed-byte and current-round-trip cases.

### P-2. Replace historical immutability with current-schema/version enforcement

**Selected by D-5 and D-9.** Emit a small canonical current-schema record
from a compiled helper in the headless Hspec target or an existing Cabal
executable. Walk each actual Haskell wire root's Generic/Typeable representation
transitively, preserving ordered constructors/fields, record selectors,
instantiated types and newtypes; aliases resolve through GHC. Capture roots
before the component registry erases the DTO type. Cover metadata and framing,
shared-type ownership and every live registration; unknown, duplicate, newly
reachable or ambiguous coverage fails with a named carrier/reachability path.
No source parser or source-text fingerprint is authoritative for this guard.

Obtain Haskell current/accepted versions from live registries and compiled
metadata/framing values. Load the real Lua registering modules in the existing
headless Lua harness and inspect the evaluated registry, including expressions
and aliases. Service stubs must not substitute fake save modules. Check actual
public decoder behavior against the reported policy, including framing and
legacy fallbacks on full-session and metadata-only paths. Explicit legacy-route
ownership includes B2's Haskell components; unknown ownership fails closed.

Compare the validated current-schema record committed at the explicit base
revision against independently compiled candidate evidence. Routine checks
compile only the candidate and require its committed record to match exactly;
every merged revision passes that freshness check against its own compiled code. A persisted shape change, including an enum append, requires every
affected real on-disk version to increase. A changed shape or version increase
requires that carrier to accept exactly its current version and disables its
affected exact-shape legacy routes. Do not analyze historical decoder shapes.
Unchanged carriers need no blanket purge; retiring one old registration alone
does not force retirement of other unchanged versions. Reject version decreases
and unaccounted coverage loss; new carriers enter explicit coverage.

Record regeneration, editing or deletion cannot bless same-version drift.
Only first introduction extracts the actual pre-guard base without a record or
helper, retaining its composed decoders and behavior; an inspection adapter
cannot copy candidate types/records or replace base values. After introduction,
a missing, malformed or unvalidated base record fails and never silently
re-enables bootstrap. Missing refs, failed candidate builds/extraction,
incomplete evidence and candidate-to-itself checks also fail. Record commits,
base record identity/validation provenance, tracked candidate working-edit
identity and candidate toolchain/extractor identity; stale candidate binaries
cannot pass. No routine two-tree build is required.

Classify custom serialization explicitly. Generic does not prove that a
Serialize instance uses generic encoding; hand-written, newtype-derived and
library codecs need explicit owned wire contracts and distinguishing tests
where generic shapes do not describe their encoding. Lua dynamic payloads and
hand-written framing are such boundaries. Unlisted boundaries fail closed.
Compiled framing constants and custom contracts belong in the current record.
This is a small replaceable record, not release history. Comments/layout and
off-wire changes do not cause bumps. Semantic changes at identical shape and
arbitrary custom behavior remain review/test obligations: a refreshed descriptor
or an oracle generated by the changed serializer is not independent evidence.

**Implementation placement approved with PSP-2 revision 2:** enum changes
use the same new guard; do not add a separate enum-audit pre-release mode or
updater. Keep the existing strict enum command/default gates unchanged during
preparation. PSP-5 switches mandatory selection to the new guard and retires
the old strict append-only audit with the historical machinery under D-7.
Off-wire enum changes need no
fictional component bump, while newly persisted types must enter coverage.
This replaces the previous draft's separate enum mode. Verified syn-manager
reply `f3jc64fgxzn9q58ky9bjn3m5s2` to question
`dc4x2bdhwdpx5dizc95psm85g6` relayed the owner: "Approve as drafted".
The revised issue is #2916. Its requirement 4 was reconciled by the manager
after D-7 (verified reply `2j49dp577uskvcfc24t956ayen`, 2026-11-05);
GitHub readback confirms the historical-retention drift is resolved.

### P-3. Prepare current correctness and retire historical obligations

Build only current audit/self-test selection. Retain live discovery, honest
required/optional registration, malformed/unsupported refusal, current codec
behavior and bridge errors. Combine this with PSP-2's schema guard at activation.
No historical mode is built or maintained.

Use a small replaceable current fixture set, with nonempty data, provenance,
checksums, summaries and independent descriptors. Synthetic current values may
supplement it. Do not compare the implementation with its own generated expected
output, relabel old bytes, or infer currentness from migrationTarget=current.
Current selection must work without opening the historical corpus.

PSP-3 prepares current audit/self-tests; PSP-4 preserves current Hspec and fixture
consumer behavior. PSP-5 switches defaults and removes the historical audit,
corpus, obsolete manifest/oracle records, B1 fingerprint/helper obligations,
historical reproducibility and historical-only tests together. Preserve current
assertions and reusable production codec/comparison helpers before deletion.
Historical provenance remains in git history, not a maintained retirement corpus.
D-8 also removes the historical migration probe and all its active references
at PSP-5 activation, preserving helpers that current probes use.
Existing strict defaults stay intact through preparation; no new historical
selection or promise is introduced. Runtime migration/DTO source may remain.

### P-4. Activate one consistent pre-release policy across all entry points

CI, `make ci`, direct audit commands, Hspec registration, and documented review
gates must agree. Path selection alone must not re-enable historical obligations
on `master` pushes, on Cabal changes, or through the default self-test façade.
PSP-5 removes the historical machinery when those defaults switch. Keep the
current
fresh-process probe selected under its existing persistence-change policy;
there is no need to reduce its three existing cycles to one.

The compiled current schema guard, its nonempty extraction/behavior tests,
live Lua registry inspection and current codec checks remain mandatory. The
Python entry point may orchestrate builds and compare records, but may not
parse source for schema/version/dispatch facts. PSP-5 wires validated committed
base-record evidence and fresh candidate evidence for PRs, master push spans
and tracked local edits, enforcing freshness on every merged code revision.
Only the one-time introduction extracts the actual pre-guard base; normal CI
compiles only the candidate and fails on missing/malformed/unvalidated base records.
Retain framing safety checks; historical immutability/reproduction obligations
are retired, including their obsolete implementations and records.
`ci_parity_save_compat.py` must test the new actual policy,
including absence of implicit historical work, rather than deleting its parity
assertions. #2912 delivered the intermediate migration-probe opt-in; under
D-8, PSP-5 removes that probe and its registry/sweep/runner/census/lab references
at the same gate switch. Do not leave a broken advertised on-demand command.

## Open questions

No unresolved policy questions remain. Q-3 was resolved during processing by
D-8. The resolved IDs are retained below
so the original question, accepted answers, and affected decisions remain
traceable. Whole-design readiness signoff is recorded above.

### Q-1. Automated current-schema guard or review enforcement?

Resolved by D-5: automated current-schema/version guard. PSP-2 and PSP-5 no
longer await this decision. PSP-3/PSP-4 prepare current coverage until
PSP-5 activates the replacement mandatory safety gates.

### Q-2. Which saves enter the release compatibility promise?

Resolved by D-6: the first public-release save format starts the promise.
Pre-release saves are not resurrected. PSP-5's policy text and the release
checklist use this boundary.

Both questions were posted together to `#syn-save-policy` as
`syn-designer-1`, verified message `3sx3nmsq853m5tupnjda739vzi`, under this
request. Both owner answers are recorded in D-5/D-6 with the verified reply's
provenance. The five slice boundaries remain unchanged and have whole-design
readiness signoff in verified message `f9dh8cp6n3ub4yqain6cx2uzhw`. Neither
settled answer needs reopening.

### Q-3. Keep the optional migration probe after deleting its inputs?

Resolved by D-8: retire it in PSP-5. The current probe unconditionally reads
`docs/save_compat/manifest.json`, so keeping its command after corpus removal
would leave it broken. The owner explicitly superseded #2912's continued
on-demand availability; no historical input API or maintained mode is added.

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
the automated enforcement; historical fixture failure cannot accidentally block
the default pre-release path; and CI/local selection remains equal. Inventory,
reference integrity, storage recovery/protection, pause/reset semantics, and
feature-specific persistence tests retain their present obligations.

Runtime evidence must use isolated resources and owned processes, wait for
terminal save/load statuses, and preserve the non-8008 port/launch rules. The
existing current probe supplies process-restart coverage; feature changes
still need their own loaded-world/chunk-admission evidence. Headless codec
tests alone did not expose the #2557 camera-ring admission problem.

## Delivery plan

These are the five approved one-PR slice boundaries, listed in valid
dependency order. No child issue numbers are reserved. Design readiness is
signed off; each tracker artifact still receives its own approval during
processing. Q-1/Q-2 are resolved; they impose no further decision stop.

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
- **Relevant decisions:** D-1, D-4. **Open questions:** None; PSP-2 owns
  the automated enforcement selected by D-5.
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

- **Outcome:** Mechanical shape changes cannot pass by refreshing a golden
  file at the same version. Every affected carrier advances and accepts only
  its current version, with older dispatch and affected legacy routes disabled.
- **Scope / touched paths:** Compiled Generic/Typeable inspector, live Haskell
  and Lua registry inspection, thin orchestration/comparison driver, small
  generated record, necessary type/inspection hooks and focused tests. Explicit
  ownership/contracts for custom serializers, Lua payloads and framing.
  No authoritative source parsing or canonical-source restrictions.
- **Phase / ordering:** 2, critical path. **Depends on:** PSP-1 (#2915), delivered.
- **Relevant decisions:** D-1, D-4/D-5 and D-9. **Open questions:** None.
  #2916 remains the linked slice. Its source-parser implementation #2922 is
  closed; the compiled replacement amendment below is owner-approved with
  committed base-record evidence after bootstrap, then requires fresh issue
  review and implementation. No ledger advancement is needed.
- **Acceptance signals:** Compiled fixtures and comparator cases cover field
  add/remove/reorder/retype, same-typed named-field swaps, enum reorder/append,
  GHC-resolved aliases/newtypes, shared-type partial bumps, record bypasses,
  unknown/custom coverage and actual retained older acceptance/legacy routes.
  All three #2922 reviews map to regressions over evaluated values and real
  public decoder paths, including syntax variations, dead-code wrappers and
  direct legacy calls. Legal syntax preserving behavior passes. Custom cases
  have owned behavioral tests and honest limits, not generic-proof claims.
  Complete bumps plus current-only dispatch pass without historical fixtures.
  Only bootstrap extracts the actual pre-guard base. Routine checks use its
  validated committed record and compile only the candidate, checking exact
  record freshness; missing/malformed/unvalidated base records fail. Record
  evidence identity and fail on stale extraction. Layout/off-wire cases pass.
- **Gates:** Proposed nonempty `current save schema` Hspec group; thin driver's
  self-tests, base-ref and record-update interfaces; existing enum checks;
  `save components`, `save envelope`, `Lua persistence components`; applicable
  reused-helper regressions. Strict defaults remain until PSP-5 activation.
- **Out of scope:** Arbitrary serializer proof, historical decoder compatibility
  analysis, a separate enum mode, corpus expansion, gameplay/wire redesign,
  deleting migration source or disabling legacy gates before activation.

### PSP-3. Separate current-save auditing from historical compatibility auditing

- **Outcome:** Current audit and Python self-tests run independently of historical
  files; no historical mode is built or maintained.
- **Scope / touched paths:** `tools/save_compat_audit.py`, necessary discovery,
  manifest, codec and helper owners; `tools/test_save_compat_audit*.py`; a small
  separate current fixture set and its independent expected descriptors. Preserve
  production codec/comparison APIs used by `persistence_snapshot.py` and the probe.
- **Phase / ordering:** 2, independent preparation alongside PSP-2.
  **Depends on:** PSP-1 (#2915).
- **Relevant decisions:** D-1, D-3, D-4, D-5, D-7. **Open questions:** None.
- **Acceptance signals:** Live registration, current framing/codec/helper behavior
  and fixture integrity remain checked. Current selection never opens historical
  files or executes historical reproducibility. Nonempty selections and negative
  cases prove corrupt current inputs fail and absent historical inputs do not
  matter. Current expectations are independent of the implementation under test.
- **Gates:** `cabal build all`; `python3 tools/save_compat_audit.py --mode current`;
  `python3 tools/test_save_compat_audit.py --mode current`, including compiled
  codec and comparison checks. Verify old default selection stays unchanged.
- **Retirement boundary:** PSP-5 removes historical tooling/corpus/tests at the
  gate switch after PSP-4 prepares current consumers. No historical-mode work.
- **Out of scope:** Default/CI activation, historical deletion, Hspec registration,
  schema guard implementation, or production serialization changes.

### PSP-4. Separate historical test obligations from current persistence coverage

- **Outcome:** Current Hspec and fixture consumer assertions survive retirement
  of historical obligations. Do not build a maintained historical selector.
- **Scope / touched paths:** `test-headless/Spec.hs`, `World/Save/Compat*.hs`,
  `World/Save/Components/Compatibility.hs`, Lua current-version pins, current
  fixtures and their consumers, and Cabal registration as needed. Extract current
  tag-staging and exact-fluid assertions from the mixed `save migrations` groups.
  Keep persistence inventory coverage links valid without relaxing its audit.
- **Phase / ordering:** 3, critical path. **Depends on:** PSP-3.
- **Relevant decisions:** D-1, D-4, D-5, D-7. **Open questions:** None.
- **Acceptance signals:** Nonempty mandatory current groups cover registry,
  assembly, integrity, storage/unknown-optional protections and current payloads.
  Replace historical fixture dependencies with current/synthetic inputs and retain
  meaningful independent expectations. Current-version tests need not accept every
  earlier version. No empty-match success or blanket Compat deletion counts.
- **Gates:** Actual current save groups, `persistence contract`, `persistence
  reference integrity`, `Lua persistence components`, moved feature groups,
  PSP-3 current self-tests, and persistence inventory audit/self-tests.
- **Retirement boundary:** Prepare retained current coverage here, leave strict
  default coverage intact until PSP-5, and identify historical-only registrations,
  consumers and records for removal together at that activation. Do not maintain
  an optional historical suite. PSP-5 owns deletion and the default switch.
- **Out of scope:** Removing useful runtime migrations, reducing current round
  trips, historical-selector construction, or premature gate/corpus retirement.

### PSP-5. Activate the pre-release gate set and publish the operating policy

- **Outcome:** Default tools, CI, Hspec and local gates apply the same
  pre-release contract; contributors can make an intentional format break
  without being forced back into historical migration work.
- **Scope / touched paths:** `.github/workflows/ci.yml`, `tools/ci-local.sh`,
  `tools/ci_expensive_gates.py`, `tools/ci_parity_{audit,config,save_compat}.py`
  and affected parity tests; the audit/test defaults prepared in PSP-2–4;
  removal of historical audit/strict enum audit, B1 checks, reproducibility, the
  57-fixture historical corpus and obsolete manifest/oracle records, historical-only
  Hspec/Python tests, registrations and consumers; the migration probe and its
  self-test, registry/timeout, sweep, runner, census and lab references under D-8.
  Preserve reusable current
  helpers and current fixtures/assertions; retired provenance remains in git history;
  root `CLAUDE.md`, `src/World/Save/CLAUDE.md`, `docs/persistence_contract.md`
  §5 and test disposition, `docs/engine_contracts.md` §The `make ci` gate set,
  §Enum append-only audit and save/load wording, `tools/README.md` and relevant
  tool headers. `Makefile` remains a wrapper unless an explicit manual target
  is useful. Check `.quruntul/adapter.py` registration impact without changing
  the lab's authority or blindly editing measured corpus records. Retire only
  the named census row through a scoped locked write, preserving every other
  probe's data; ordinary seed/reconcile must not become automatic deletion.
- **Phase / ordering:** 4, critical path. **Depends on:** PSP-1, PSP-2,
  PSP-3, PSP-4; confirm the manager's separate probe opt-in issue #2912 has landed.
- **Relevant decisions:** D-1–D-8. **Open questions:** None. Implementation
  depends on PSP-1–PSP-4 and #2912 as stated above.
- **Acceptance signals:** Default invocations require the current guard/tests
  and never historical corpus success. Historical machinery is removed in the
  same change as gate activation; no historical mode remains. Current commands
  and production snapshot/probe helpers work without the retired files. The
  historical probe is absent from runner/sweep/lab/census selection; explicit
  use of its old key fails without launching an engine or reporting zero-work
  success. #2912's runnable-on-demand promise is superseded, not left broken.
  Save/Cabal/fixture changes and master pushes cannot implicitly restore old
  obligations. CI/local parity tests prove both positive current selection and
  absence of implicit historical selection and stale references. `persistence_contract` remains CI-eligible;
  inventory/capability gates are unchanged. Rules no longer require freezing
  an outgoing pre-release DTO, a legacy baseline/oracle row per bump, or the
  migration probe. They name the actual version boundary and release checklist.
- **Gates:** `python3 tools/ci_expensive_gates.py --self-test`,
  `python3 tools/ci_parity_audit.py --self-test` and bare audit; automated guard,
  current audit/self-tests and current Hspec groups; inventory audits; a real
  `persistence_contract` probe. Registry/adapter retirement also requires
  `tools/test_run_probes.py`, `tools/ci_probes.py --self-test`,
  `tools/test_persistence_contract_sweep.py`, `tools/test_readme_registry_count.py`,
  `tools/test_probe_census.py`, census validation, and `python3 .quruntul/checks.py`.
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
not a new child issue to file during this design task. Historical pre-release
tooling, fixtures and the migration probe will have been retired under D-7/D-8
when this pre-release arc completes. Git history is reference
material, not a dormant supported mode to re-enable.

1. Declare the first public-release save format as the supported baseline,
   following D-6, and record the retention promise from that baseline forward.
   Freeze the release's full transitive DTO/enum shapes, component versions,
   metadata, Lua schemas and framing; capture real, nonempty release fixtures
   with provenance, checksums, canonical summaries and descriptor oracles.
2. Restore the requirement that evolution of supported formats preserves their
   exact historical decoders and supplies explicit migrations. Build release-era
   fingerprint and append-only protections afresh, with explicit migration
   exceptions only through safe frozen representations. Keep current version
   enforcement and transaction/refusal tests as well.
3. Build fresh supported-baseline manifest coverage: required/current/oldest versions,
   declared-vs-real descriptor agreement, fixture presence/integrity, historical
   canonical-result checks and reproducibility for supported public-release
   formats. Do not resurrect retired pre-release saves or repair their corpus
   as a release requirement; they remain outside the compatibility promise.
4. Establish release-baseline historical Hspec coverage and fresh migration
   verification of real
   load/publish/resave/restart/reload coverage, sweep/lab selection, CI and
   `make ci` parity, and the relevant expensive-gate selection. Run the actual
   release validation; turning a flag back on is not evidence of passing it.
5. Publish the restored contracts and contributor version-bump procedure.
   Retain current-version fresh-process tests and all inventory, reference,
   storage, paused-failure and content-integrity guarantees throughout.

## Handoff

This design has eight settled decision records and five delivery slices. The
owner amended D-5 during PSP-2 processing to require current-only acceptance
after a shape change; the revised PSP-2 artifact is approved and filed as #2916.
D-7 subsequently requires current-only auditing and retires historical machinery
at PSP-5 activation. PSP-3 and PSP-4 prepare current coverage and are filed
as #2917/#2918. D-8 extends activation retirement to the historical migration
probe and its references. PSP-5 is approved and filed as #2919. All tracker
artifacts are now processed; implementation and review remain separate work.
D-5 selects automated enforcement; D-6 starts the future
promise at the first public-release save format. The task's authorized
endpoint is publication of this document and
reporting its commit in `#syn-save-policy`, not issue creation.

Readiness is explicitly signed off at `faa7f3803d` and recorded in this
revision. The designer's task ends after publishing this revision and reporting
its commit. The manager launches `process-design-doc` separately: EPIC first,
then exactly one dependency-ready child per invocation, with separate owner
approval for every tracker artifact. Recheck the tracker and #2912 during
processing; this publication does not itself create or approve issue bodies.

## EPIC processing record

D-8 supersedes older on-demand-probe wording in the original approval records
below. The four exact #2914 corrections were approved and applied with PSP-5
creation; the original approval bodies remain as historical evidence.


D-7 supersedes historical-mode wording in the original approved bodies below.
The manager reconciled #2916 requirement 4 (verified message
`2j49dp577uskvcfc24t956ayen`); readback confirmed it. The original bodies remain
here as approval evidence. The three #2914 amendments were approved and applied with PSP-3 revision 2.

The body below records the original approved issue. Amended D-5 supersedes
its retained-decoder wording; the approved PSP-2 revision 2 included exact
epic wording edits, now applied to GitHub alongside its #2916 checklist link.

Created [#2914](https://github.com/coghex/synarchy/issues/2914) on 2026-11-05.
Verified syn-manager reply `ge89pkamx45zn8svwv3dxxk4g6` to question
`hftund5mig95kjc4rc4u548knn` relayed the owner: "Approve, create it".
The exact approved draft below was filed with existing labels; only the EPIC
ledger entry is processed. No child is approved by this record.

Title: [epic] Retire mandatory pre-release legacy-save compatibility

Labels: `epic`, `save-load`.

```markdown
## Goal

Before public release, permit save-format changes to advance the real on-disk versions and safely refuse older development saves, without requiring new migrations, frozen outgoing DTOs, or permanent historical fixtures. Keep current-save correctness mandatory.

## Background

Verified at master ad35e6f7c (2026-11-05): exact component-version dispatch exists, but currentSaveVersion is only a bridge marker. Contracts and CI/local gates still require historical compatibility. Headless groups mix historical and current protections; the current probe supplies three fresh-process load/resave cycles.

The owner approved an automated current-schema/version guard and a future compatibility promise beginning with the first public-release save format. Retired pre-release saves will not be resurrected at release.

## Children

Phase 1:
- [ ] PSP-1. Prove safe unsupported-version refusal at the load boundary

Phase 2:
- [ ] PSP-2. Enforce current-schema version changes without requiring migrations
- [ ] PSP-3. Separate current-save auditing from historical compatibility auditing

Phase 3:
- [ ] PSP-4. Separate historical test obligations from current persistence coverage

Phase 4:
- [ ] PSP-5. Activate the pre-release gate set and publish the operating policy

## Dependency structure

PSP-1 can land first. PSP-2 depends on PSP-1 (critical path). PSP-3 depends on PSP-1 (independent preparation alongside PSP-2). PSP-4 depends on PSP-3. PSP-5 depends on PSP-1 through PSP-4 and is blocked by #2912 until it lands. Keep strict gates until tested replacement safety checks activate in PSP-5.

## Done when

- All five children and #2912 are delivered. CI, local gates, direct audit defaults and headless selection enforce current correctness and automated schema/version changes without requiring historical migrations, corpus preservation or historical reproducibility. Explicit historical runs remain available and report retired coverage honestly.
- Every changed wire shape advances each affected component, metadata, Lua or framing version. Unsupported versions fail before decoding/publication with a clear diagnostic identifying the boundary, found version and supported versions. Unsafe older registrations and legacy fallback routes are refused; incompatible authoritative files never fall back to .prev. The old session remains intact and paused, and the file is not rewritten.
- Current codec, inventory, reference/content integrity, storage recovery and feature-specific persistence coverage remain. The existing three fresh-process current-version load/resave cycles remain selected for persistence changes. Current assertions survive historical test/fixture retirement, with provenance retained.
- Working migrations and DTO implementations remain; retaining a decoder requires its transitive wire shape to remain safe. The automated guard rejects same-version shape drift even after baseline regeneration and documents its semantic limits.
- Authoritative rules agree and the public-release restoration checklist is published: establish the first release baseline, freeze its shapes, capture real fixtures, and restore migration, supported-baseline and historical verification obligations before release. This epic can close before release once that checklist is recorded.

## Out of scope

Gameplay, envelope redesign, bulk migration deletion, restoring development saves, deterministic replay, unrelated worldgen gates; probe opt-in belongs to #2912.

## Related

- #2912: separate probe opt-in dependency.
- #2742: coordinate shared CI/gate surfaces.
- #2564: separate faction retirement scope.
- Supporting design: docs/designs/prerelease_save_policy_design.md.

<!-- issue-origin:codex -->
```

Evidence and dedup: rechecked all 66 open issue titles and three open/closed
searches (pre-release save, compatibility save, historical save). No duplicate
policy epic found. Read #2912, #2742 and #2564: respectively separate probe
opt-in, CI optimization, and faction retirement. Current dispatch, existing
CI/local compatibility commands, actual headless group registrations,
persistence policy and current fresh-process probe match the epic premise
at `ad35e6f7c`. Six unique ledger entries match the five delivery headings;
dependencies exist and are acyclic. No child body was drafted or evaluated.
No builds, tests, engines or probes were run for this tracker-only step.

## PSP-1 processing record

Created [#2915](https://github.com/coghex/synarchy/issues/2915) on 2026-11-05
and linked it in epic #2914 with its implementation checkbox still unchecked.
Verified syn-manager reply `sbadzax42c9hcdh92f8ayv87c6` to question
`vt73fjgxbw8ejnx6s6357mg6xs` relayed the owner: "Approve as drafted".
The exact approved draft and linking operation below are retained as evidence.

Title: [save-policy PSP-1] Prove safe unsupported-version refusal at the load boundary

Labels: `enhancement`, `save-load` (both existing).

````markdown
## Background

PSP-1 of #2914, Phase 1, can land first; no prerequisite child. Before release, format changes may refuse older saves, but refusal must be explicit and leave the current session intact. This slice establishes that boundary before later policy/gate changes.

Verified against master code ad35e6f7c on 2026-11-05:
- `src/World/Save/Component/Types.hs:componentCodec` dispatches exact versions before decoding. `Components/Registry.hs` already tests older/future rejection, malformed payloads and genuine old DTO dispatch; reuse this coverage.
- Metadata refusal in `Envelope.hs:decodeMetadataComponent` says "Save format incompatible"; component/Lua refusal says "unsupported schema version"; `renderEnvelopeError = tshow` exposes the framing constructor. These do not yet give the agreed consistent user-facing diagnostic.
- `atomic save storage` covers incompatible framing with a valid `.prev` and unsupported-component progress separately. `tools/transactional_load_probe.py` checks missing/corrupt save preservation, but has no unsupported-version load case. Its existing queued-command checks concern successful publication.

## Requirements

1. Unsupported Haskell component, metadata, Lua module and envelope framing versions report `unsupported save version`, identify the component/module or framing boundary, and show the found version and supported versions. Preserve structured failure classification, progress, error aggregation and ordering; do not derive phases from diagnostic text.
2. Add or strengthen focused tests for unsupported older and future versions at each boundary, including required and optional-present Haskell/Lua components. Version refusal precedes that payload's decoding and all application; deliberately malformed bytes under an unsupported version must still report version refusal. At an accepted version malformed bytes remain a distinct payload error. Optional absence keeps its existing default semantics; optional-present errors cannot default away.
3. Preserve exact registered old-decoder dispatch and current nonempty round trips. Use distinguishing values where a wrong decoder could accept the same-width bytes with changed meaning; a current-code self-round-trip alone is insufficient evidence. Keep working migrations, accepted version sets, wire bytes and version numbers unchanged: no format bump merely for this policy slice.
4. Prove that an incompatible authoritative generation with a verified usable `.prev` refuses without fallback or rewriting either file. Cover unsupported component/metadata/framing paths in storage tests and Lua preparation refusal at its real boundary; preserve separate corrupt-generation recovery.
5. Extend the existing transactional-load probe with an unsupported-version case built from a current save in isolated resources. Verify a terminal pre-publication failure with useful diagnostics, unchanged prior session and queued work, paused state, no successful publication and unchanged files. Cover queue preservation with a deterministic targeted transaction test if that exact queue boundary cannot be observed reliably through TCP; do not substitute successful-load discard coverage. Preserve post-publication reconciliation-failure semantics and the existing current-version fresh-process probe.

## Acceptance

Run the production builds and these existing, nonempty targeted groups:

```bash
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "save components"'
cabal test synarchy-test-headless --test-options='--match "save envelope"'
cabal test synarchy-test-headless --test-options='--match "atomic save storage"'
cabal test synarchy-test-headless --test-options='--match "Lua persistence components"'
cabal test synarchy-test-headless --test-options='--match "persistence contract"'
cabal test synarchy-test-headless --test-options='--match "Load.Status"'
cabal test synarchy-test-headless --test-options='--match "Load.Terminalize"'
python3 tools/run_probes.py --only transactional_load --exact
python3 tools/run_probes.py --only persistence_contract --exact
```

Expected: every selected group executes assertions and passes; the transaction probe proves unsupported-version preservation, and the persistence probe retains its three real fresh-process load/resave cycles with nonempty current state. Record the new cases and outcomes in the PR. Run probes serially with isolated resources and owned processes, using the documented headless/non-8008 rules and terminal status waits. Run applicable changed-path audits; no full suite or `make ci` is required by this issue.

Scope: component/metadata/framing diagnostic owners, `scripts/lib/save_modules.lua`, their existing headless tests, targeted load tests, and `tools/transactional_load_probe.py`. Any accompanying contract/evidence documentation lands with `docs-push` and is linked in the PR body.

## Out of scope

Changing accepted-version sets globally; deleting migrations/DTOs or retiring fixtures; the automated schema guard (PSP-2); audit/test partitions (PSP-3/4); gate activation and operating policy (PSP-5); broad transaction refactoring; new persistent state.

## Related

- #2914: umbrella epic; PSP-2 and PSP-3 depend on this slice.
- #2912: separate migration-probe opt-in dependency of policy activation; it does not block PSP-1 and is not duplicated here.
- #1093, #1275, #1919: delivered codec/dispatch/structured-progress foundations; retain their behavior except the diagnostic wording explicitly changed above.
- Supporting design: `docs/designs/prerelease_save_policy_design.md`, PSP-1, D-1 and D-4.

<!-- issue-origin:codex -->
````

Approval also authorizes this exact replacement in epic #2914, with `N`
bound to the newly created issue number, and the matching PSP-1 ledger link:

```markdown
- [ ] PSP-1. Prove safe unsupported-version refusal at the load boundary
```

becomes:

```markdown
- [ ] PSP-1. #N — Prove safe unsupported-version refusal at the load boundary
```

The epic checkbox remains unchecked until implementation completes.
No other epic line, label, decision or child is changed by this proposal.

Evidence/dedup: all 67 open issue titles and three open/closed searches
(unsupported version save, incompatible load, refusal save) rechecked.
#2914 has this planned, unfiled child and matches the ledger. Read the
closed #1919 and #1275; their delivered structured-progress and version-table
work does not cover the new diagnostic/preservation matrix. No duplicate
open issue found. Checked actual refusal/decoder tests, metadata and framing
rendering, Lua prepareLoad ordering, storage progress/no-fallback tests and
the transaction probe; the latter has no unsupported-version case.
No later slice was evaluated. No engine, test or probe was run while drafting.

## PSP-2 processing record (revision 2)

Created [#2916](https://github.com/coghex/synarchy/issues/2916), linked it in
epic #2914 and applied the three exact approved epic replacements below.
Verified syn-manager reply `f3jc64fgxzn9q58ky9bjn3m5s2` to question
`dc4x2bdhwdpx5dizc95psm85g6` relayed the owner: "Approve as drafted".
The prior proposal at `564e65e228` was superseded by the owner's
"Simplify first" reply `k8nvcbvvekhi9gqddjdbn86vcs` and was never filed.
The complete approved revision 2 and its side effects are retained below.

Title: [save-policy PSP-2] Enforce current-schema version changes without requiring migrations

Labels: `enhancement`, `save-load`, `tooling` (existing).

````markdown
## Background

PSP-2 of #2914, Phase 2, critical path, depends on #2915 (PSP-1). Implement after that refusal boundary lands.

Owner decision, 2026-11-05: before release, a persisted shape change advances every affected component, metadata, Lua or framing version, and each affected carrier then accepts only its current version. Drop its older accepted decoders and affected exact-shape legacy routes. Do not analyze whether an old decoder could still be safe.

The current enum audit excludes single-constructor records and custom serializers, and its baseline updater rejects non-appends. The B1 frozen-DTO fingerprint is not a current-schema guard. Existing version dispatch already provides the refusal mechanism; #2915 supplies its diagnostic and regression coverage.

## Requirements

1. Add a small generated current-schema record and source-based guard covering all current Haskell components, metadata, registered Lua modules and framing. Include transitive records, enums, newtypes/aliases and explicit custom-serializer ownership. Cross-check real registrations; missing, newly reachable or ambiguous coverage fails with a named boundary/path. Reuse existing discovery/fingerprint helpers where sound.
2. Compare candidate source/schema against an explicit base revision. Every detected shape change, including an enum append, requires a strictly increased real on-disk version for every affected carrier. Regenerating, editing or deleting the candidate record cannot bless same-version drift. Missing comparison evidence fails; initial record introduction must have a non-vacuous bootstrap check. The bridge-only currentSaveVersion is not protection.
3. For every affected carrier, require its accepted-version set to be exactly its new current version and require affected exact-shape legacy fallback routes to be disabled. Fail if any older version remains accepted, even if its decoder appears compatible. Detect affected routes through explicit/discovered ownership, failing on unknown coverage; do not compare historical decoder shapes. Unchanged carriers need no blanket purge. Removing old registrations/routes does not require deleting their source implementations or creating migrations/fixtures.
4. Keep enum shape/version enforcement in this guard; no separate enum-audit pre-release mode or updater is needed. Leave the existing strict enum audit/default gates unchanged here. PSP-5 switches mandatory selection to the new guard and retains the old audit for explicit historical use. Off-wire enum changes require no fictional component bump; newly persisted types must enter coverage.
5. Ignore comment/layout-only edits. State the limits of source inspection, especially arbitrary custom serializers and indistinguishable positional-value swaps. Encoding/meaning changes outside mechanical detection still require a version bump, current-only acceptance, explicit review evidence and a distinguishing test.

## Acceptance

Expose and document the guard's base-ref check, generated-record update and self-test interfaces. Proposed public entry point:

```bash
python3 tools/current_save_schema_audit.py --self-test
python3 tools/current_save_schema_audit.py --base-ref origin/master
python3 tools/enum_append_only_audit.py --self-test
python3 tools/enum_append_only_audit.py
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "save components"'
python3 tools/test_save_compat_audit.py --without-reproducibility
```

Use synthetic base/candidate trees to prove failures for field add/remove/reorder/retype, enum changes, shared shapes with partial bumps, record refresh/deletion bypass attempts, unknown/ambiguous coverage, and an affected carrier retaining any older version or legacy route. Include aliases/newtypes, custom-owner coverage and an enum append. Correct bumps plus current-only dispatch pass without migration/fixture inputs; unchanged, comment/layout-only and off-wire cases pass. Cover initial bootstrap and subsequent checks. Record the resolved comparison-base and candidate SHAs; no candidate-to-itself proof.

The real unchanged schemas and listed existing checks pass. Preserve discovery/fingerprint regressions when reusing helpers. No runtime probe, full headless suite or `make ci` is required for this tooling slice.

## Out of scope

Historical decoder compatibility analysis; a separate enum pre-release mode; migration/fixture creation or bulk source deletion; gameplay or schema changes just to exercise the guard; historical audit/test partitioning (PSP-3/4); default/CI activation (PSP-5).

Scope is the guard/current record, necessary reusable helpers/tests and brief ownership/retained-source comments. Tool-consumed records stay in the code PR. Human-facing documentation lands with docs-push and is linked from the PR.

## Related

- #2914: epic. Depends on #2915; PSP-5 depends on this guard.
- #2912: separate probe opt-in dependency of policy activation, now delivered; not duplicated here.
- Design: `docs/designs/prerelease_save_policy_design.md`, P-2/PSP-2 and amended D-5. Owner simplification relayed by syn-manager in reply k8nvcbvvekhi9gqddjdbn86vcs.

<!-- issue-origin:codex -->
````

Approval also authorizes these exact replacements in epic #2914, where
N is the created issue number, and the matching PSP-2 ledger link.
The implementation checkbox stays unchecked; no other tracker edits.

1. Replace:

```text
- [ ] PSP-2. Enforce current-schema version changes without requiring migrations
```

with:

```text
- [ ] PSP-2. #N — Enforce current-schema version changes without requiring migrations
```

2. Replace:

```text
Unsafe older registrations and legacy fallback routes are refused; incompatible authoritative files never fall back to .prev.
```

with:

```text
Each carrier affected by a shape change accepts only its new current version: older accepted decoders and affected exact-shape legacy routes are disabled. Incompatible authoritative files never fall back to .prev.
```

3. Replace:

```text
Working migrations and DTO implementations remain; retaining a decoder requires its transitive wire shape to remain safe.
```

with:

```text
Existing migration/DTO source may remain, but carriers affected by a shape change accept only their new current versions; no historical-decoder compatibility analysis is required.
```

Evidence/dedup refreshed on 2026-11-05: all 67 open titles plus three
open/closed searches (save-policy, schema guard, enum) checked. No PSP-2
issue exists. #2915 remains the prerequisite; #2912 is closed after PR #2913
merged as 32e1af213. Current component, Lua and framing dispatch confirms that
accepted-version checks can enforce current-only acceptance without proving
historical decoder shape compatibility. The existing enum command has only
strict audit, baseline update and self-test modes; the new guard can own
pre-release enum enforcement while PSP-5 owns selection. No later slice was
evaluated. No builds, tests, engines or probes were run for this redraft.

## PSP-3 processing record

Revision 2, current mode only. Supersedes the draft published at `3f1283bdac`
and question `p9ne2z4dqarw4v29qbc3rgtpu2` (continuation
`ubqxpa2w2fzrq4xdxfewkcc3qw`). Owner revision instruction and provenance are
recorded in D-7. Created [#2917](https://github.com/coghex/synarchy/issues/2917)
from the exact revision 2 payload. Verified syn-manager reply
`xz3vwn2fudv6gyj9fkm392qzae` to question `5e6yv366iegec62duuxiw37z7e`
(continuation `w4wgam5ukctrr4hw3f9sxuhuga`) relayed the owner: "Approve as drafted".
All three exact #2914 amendments below were applied and verified by readback.
No later child is approved by this record.

Title: [save-policy PSP-3] Separate current-save auditing from historical compatibility auditing

Labels: `enhancement`, `save-load`, `tooling`.

````markdown
## Background

PSP-3 of #2914, Phase 2, depends on #2915; independent preparation alongside #2916. The owner chose "Current mode only": build current-save auditing and self-tests, with no maintained historical mode. PSP-5 switches defaults and removes historical machinery after PSP-3/4 preserve current coverage.

`save_compat_audit_manifest.py:audit` mixes live registrations and fixture integrity with B1 fingerprint/helper checks, historical coverage and oldest-version obligations. The self-test facade only partitions reproducibility; ordinary tests still demand historical fixtures. Codec/comparison helpers also serve `persistence_snapshot.py` and the current `persistence_contract` probe and must survive.

## Requirements

1. Add `--mode current` to `tools/save_compat_audit.py` and `tools/test_save_compat_audit.py`. Build only that new selection; no historical mode. Leave existing no-mode commands and strict default coverage unchanged until PSP-5. Preserve the existing facade/owner boundaries where useful.
2. Current auditing checks live Haskell/metadata/Lua registrations, required/optional declarations, current framing/codec behavior and helper failures. It requires no B1 fingerprint/helper completeness, oldest-version coverage, historical migration/reproducibility or permanent baseline per bump. #2916 owns schema/version enforcement.
3. Use a small replaceable set of current fixtures, with nonempty data, provenance, checksums, summaries and independent expected descriptors checked by the real compiled codec. Synthetic current values may supplement them. Do not derive expected results from the implementation under test, relabel historical bytes as current, or infer currentness from `migrationTarget: current`. Current selection must not read or decode the historical corpus.
4. Preserve current codec/helper, malformed/unsupported refusal and comparison regressions using current inputs. Keep APIs/re-exports used by `persistence_snapshot.py` and the current probe, including compiled-helper resolution and distinct equal/mismatch/decode/tool-error outcomes. Preserve current registration/generation rollback behavior where retained. Mixed tests must retain their current assertions before historical dependencies are removed.
5. Self-tests prove a nonempty current selection; duplicate or missing selected test registrations and empty selections fail. Missing/corrupt historical-only files do not affect current mode. Corrupt current bytes/checksums and dishonest descriptors fail. Prove current mode never executes historical reproducibility; reject incompatible legacy reproducibility flags clearly.

## Retirement boundary

This slice prepares current audit/self-tests and current fixtures. PSP-4 prepares current Hspec coverage and fixture consumers. PSP-5, in the same change that switches the gates, removes the historical audit, the 57-fixture historical corpus and its obsolete manifest/oracle records, B1 fingerprint/helper checks, historical reproducibility and historical-only tests. It also retires the strict append-only audit replaced by #2916, preserving reusable current helpers. Historical material survives in git history; no historical mode is built or maintained. Existing strict defaults remain intact until that activation. The public-release checklist starts fresh from the first release baseline.

## Acceptance

```bash
cabal build all
python3 tools/save_compat_audit.py --mode current
python3 tools/test_save_compat_audit.py --mode current
```

Both current selections pass; self-tests execute nonempty compiled-codec/comparison assertions and the negative cases above. Record selected test/fixture identities and counts. A synthetic tree without historical files still passes current selection. Verify unchanged no-mode selection during preparation without duplicating expensive runs just to prove selection. No new historical mode, migration engine probe, whole headless suite or `make ci` is required.

## Out of scope

Default/CI activation and historical deletion (PSP-5), Hspec preparation (PSP-4), schema guard implementation (#2916), production serialization changes, and probe opt-in work. Scope is audit/self-test facades, necessary owner helpers and current fixture records. Tool-consumed records belong in the code PR; human documentation lands with docs-push and is linked from it.

## Related

- #2914: epic; #2915: prerequisite; #2916: independent current-schema guard.
- #2912: separate delivered probe opt-in dependency of activation.
- #2049/#2073: prior production/test owner splits; preserve useful boundaries.
- Design: `docs/designs/prerelease_save_policy_design.md`, P-3/PSP-3, D-1/D-3/D-4/D-5/D-7.

<!-- issue-origin:codex -->
````

### Approved and applied tracker side effects

The three replacements below were applied to #2914 with N bound to 2917.
Its implementation checkbox remains unchecked; the design ledger links #2917.
No sibling issue was edited by this creation step.

1. Replace:

```text
- [ ] PSP-3. Separate current-save auditing from historical compatibility auditing
```

with:

```text
- [ ] PSP-3. #N — Separate current-save auditing from historical compatibility auditing
```

2. Replace:

```text
Explicit historical runs remain available and report retired coverage honestly.
```

with:

```text
PSP-3/4 prepare current coverage; PSP-5 switches gates and removes historical auditing, the 57-fixture corpus and obsolete records, B1 checks, historical reproducibility/tests and the strict append-only audit replaced by the new guard. Preserve reusable current helpers. Retired material survives in git history; no historical mode is maintained.
```

3. Replace:

```text
establish the first release baseline, freeze its shapes, capture real fixtures, and restore migration, supported-baseline and historical verification obligations before release.
```

with:

```text
establish the first release baseline, freeze its shapes, capture real fixtures, and build migration, supported-baseline and historical verification afresh before release. Do not resurrect the pre-release corpus or a dormant historical mode.
```

### Verification and deduplication

Rechecked master `32e1af213a27b881cd5c587061e108e35f85f69b` and all 68 open
issue titles plus three all-state title searches (save audit, compatibility,
current save). #2049/#2073 are earlier owner splits, not this policy change.
#2915/#2916 match the linked ledger; #2912 is delivered. The historical
manifest still contains 51 active baselines/57 fixtures. Current selection
cannot be inferred from migrationTarget=current. The production codec facade
serves persistence_snapshot.py and the current persistence_contract probe.
No duplicate current-only audit issue found. No code, build, test, engine,
probe or review was run for this authoring step. Two children remain unchecked.

## PSP-4 processing record

Drafted under verified syn-manager continuation `xz3vwn2fudv6gyj9fkm392qzae`
after PSP-3 revision 2 approval. Its prerequisite is linked as #2917;
implementation waits for that preparatory slice. Created
[#2918](https://github.com/coghex/synarchy/issues/2918) from the exact approved
payload. Verified syn-manager reply `3j2yt52xt2vhtqvsw5d84fijjw` to question
`837x3ytidzugxs33pc7wv2s4cw` (continuation `2y6kc7da26jx95tkhp9trdre2e`)
relayed the owner: "Approve as drafted". The approved epic link below was
applied with N = 2918 and verified by readback. No later child is approved. This draft does not evaluate PSP-5 beyond
the already-approved activation/retirement boundary.

Title: [save-policy PSP-4] Separate historical test obligations from current persistence coverage

Labels: `enhancement`, `save-load`, `tooling`.

````markdown
## Background

PSP-4 of #2914, Phase 3, depends on #2917. Prepare current Hspec and fixture-consumer coverage so PSP-5 can retire historical tests and files at gate activation. Owner policy: "Current mode only"; do not build or maintain a historical selector.

`test-headless/Spec.hs` registers `save migrations`, not a group named `save compatibility`. Its aggregate and siblings include current protections as well as historical decoding: tag-target admission and `Sim.ExactFluid.saveSpec` are registered there. `Components/Compatibility.hs` also contains current recursive-item, simulation, crafting, power, climate and diagnostic assertions. Lua tests mix current behavior with literal versions and acceptance of every previous version. Deleting those families wholesale would lose current coverage.

## Requirements

1. Preserve current assertions in mandatory, nonempty Hspec groups using current fixtures or synthetic current values. Inspect `World/Save/Compat` families, `Components/Compatibility.hs`, `Sim/ExactFluid.hs` and Lua save/AI tests; record each moved current assertion's old and new test path. Reuse #2917's small current fixture set where suitable, retaining independent expected values and nonempty payloads. Current tests must not read the historical corpus or decode old DTOs as their setup.
2. Preserve current registry/assembly/integrity, optional-absence versus optional-present-error behavior, unknown-data overwrite protection, diagnostic/reference rendering and current payload round trips. Keep recursive item physical values, non-default simulation/crafting/power/climate data, container observations, river names/etymology, movement hazard policy and authored flora references where the mixed families currently exercise them. Retire historical migration expectations later; do not replace these current assertions with mere successful decoding.
3. Keep meaningful tag-admission coverage with current inputs: nonempty cell/occurrence assignments survive real staging, while invalid cell targets, absent occurrences and out-of-bounds arena plantings are detected. Preserve exact fluid remainders and identity-specific flora removal through the current world-edits codec. Split assertions that combine old-version migration with current behavior so removing the former cannot remove the latter.
4. Separate Lua current-version/schema and transient-reset assertions from historical accepted-range/migration assertions in `Lua/SaveModules/Components.hs`, `UnitAiHold.hs`, `UnitAiLocations.hs`, `UnitAiYieldProximity.hs` and `UnitAiHarvest.hs`. Current tests require the declared current version to work and validate meaningful payload/reset behavior; they must not require an uninterrupted historical version range. Keep intentional schema assertions; do not turn every expected value into a readback from the implementation. #2916 owns current-only acceptance after future shape changes; this test preparation does not change runtime registrations.
5. Keep existing strict default coverage intact during preparation. Move or replace current assertions now and identify historical-only tests, registrations, fixtures and consumers for PSP-5's coordinated removal. Do not add a historical mode, optional historical suite or permanent retirement manifest. Update Cabal imports/registrations and persistence-inventory coverage links when tests move. Keep current snapshot/probe helpers and feature-specific runtime coverage intact.

## Acceptance

```bash
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "save components"'
cabal test synarchy-test-headless --test-options='--match "persistence contract"'
cabal test synarchy-test-headless --test-options='--match "persistence reference integrity"'
cabal test synarchy-test-headless --test-options='--match "Lua persistence components"'
python3 tools/test_save_compat_audit.py --mode current
python3 tools/persistence_inventory_audit.py
python3 tools/test_persistence_inventory_audit.py
```

Also run the exact nonempty new/current groups containing moved tag-admission, exact-fluid and affected Lua AI assertions; record their paths, commands and example counts. Existing AI owners are `position hold after a completed move order`, `unit location knowledge`, `harvest collection proximity` and `skill-scaled auto-harvest`; target their changed persistence subgroups. Demonstrate retained current groups use no historical fixture files and preserve distinguishing expected values. Record the before/after assertion mapping and the bounded removal list for PSP-5. Existing default historical coverage is still registered until activation. No whole headless suite, historical probe or `make ci` is required.

## Out of scope

Historical corpus/test deletion and gate/default activation (PSP-5), the current Python audit implementation (#2917), schema guard (#2916), production codec/accepted-version changes, runtime migration removal, gameplay changes and new historical support. Do not weaken inventory/reference/content integrity or substitute headless codec evidence for the existing fresh-process probe.

Code/test inputs land in the implementation PR. Human-facing assertion mapping and coverage references land with docs-push and are linked from it; do not publish a claim that retirement is already active.

## Related

- #2914: epic; depends on #2917. PSP-5 consumes the prepared current coverage and removes the retired machinery at the gate switch.
- #2915/#2916: refusal and schema guard prerequisites of activation.
- #2912: separate delivered migration-probe opt-in work; not duplicated here.
- #2894: tag-admission regression must survive; #2564 is separate faction retirement scope.
- Design: `docs/designs/prerelease_save_policy_design.md`, PSP-4/P-3, D-1/D-4/D-5/D-7.

<!-- issue-origin:codex -->
````

### Approved and applied tracker side effects

The approved replacement in #2914 was:

```text
- [ ] PSP-4. Separate historical test obligations from current persistence coverage
```

with:

```text
- [ ] PSP-4. #N — Separate historical test obligations from current persistence coverage
```

N is the created issue number; keep the epic implementation checkbox unchecked.
Link the same number in the design PSP-4 ledger. No other tracker edits.

### Verification and deduplication

Read all 69 open issue titles and three all-state searches (save historical,
compatibility test, save fixture). #2917 owns Python current-mode preparation;
#2073 is the completed Python owner split; #2804 is a blood GPU probe setup
problem, not Hspec policy; #2894 fixed fixture tag targets and its current
admission assertions must survive. No duplicate PSP-4 issue found.
Epic #2914 links #2915/#2916/#2917 consistently with this ledger.

At source master `32e1af213a27b881cd5c587061e108e35f85f69b`, inspected actual
Spec.hs and Cabal registration, mixed Compat/Components families, the tag
admission harness and seeded failures, exact-fluid saveSpec, Lua current-version
and historical-range pins, and inventory audit/coverage references. Relevant
current assertions are named in the body rather than inferred from filenames.
No implementation, test, build, engine, probe or issue review was run.
One child remains unchecked: PSP-5.

## PSP-5 processing record

The final slice was requested by verified syn-manager reply
`3j2yt52xt2vhtqvsw5d84fijjw` after PSP-4 approval. Its probe boundary is now
resolved by D-8. Created [#2919](https://github.com/coghex/synarchy/issues/2919)
from the exact approved payload. Verified syn-manager reply
`rjmdzdsqeuqd5y6i5vvkz5epde` to question `mg9mu6stwct6c96jii9nc83m7w`
(continuations `iktc7bxk77c8mwymgit8rxf3tw`, `yxbermvkkyh2wa27acfnj6mpms`)
relayed the owner: "Approve as drafted". All four approved #2914 edits below
were applied with N = 2919 and verified by readback. Implementation waits for
#2915/#2916/#2917/#2918 to land; #2912 is already delivered.

Title: [save-policy PSP-5] Activate the pre-release gate set and publish the operating policy

Labels: `enhancement`, `save-load`, `tooling`.

````markdown
## Background

PSP-5 of #2914 is the final activation slice. Implement after #2915, #2916, #2917 and #2918 land; #2912's intermediate probe opt-in is already delivered. Owner decisions: "Current mode only" and "Retire it in PSP-5". At one gate switch, activate current-save safety and remove the historical machinery, including the migration probe. The latter supersedes #2912's promise to keep that probe runnable on demand.

Today CI/local commands still require the combined compatibility audit, historical reproducibility and strict enum append-only audit. `ci_parity_save_compat.py` checks their executable selection, not just command presence. The migration probe unconditionally reads the historical manifest; leaving its entry point after corpus removal would break it. Current snapshot/probe helpers share audit modules and must survive.

## Requirements

1. Activate #2916's current-schema guard, #2917's current audit/self-tests and #2918's current Hspec coverage together across direct defaults, `.github/workflows/ci.yml`, `tools/ci-local.sh` and documented gates. Bare audit/self-test commands select current behavior. Remove obsolete historical flags/commands with clear argument failures, never a silent successful no-op. Keep unrelated gates and the current headless lane guarantees intact.
2. Wire a real comparison base into the schema guard: PR base, the full before/after span on master pushes, and the local merge base including tracked working edits. Record resolved base/candidate identities. Missing comparison evidence fails clearly; do not compare the candidate to itself, skip the guard, or bless same-version drift through record regeneration. Preserve #2916's non-vacuous bootstrap behavior where the base predates its record. Use the existing docs-only policy consistently; source, current record/fixtures, tooling and Cabal edits must not bypass current checks.
3. In that same activation change, delete historical auditing, B1 fingerprint/helper obligations, historical reproducibility, the 57-fixture historical corpus and obsolete manifest/oracle records, historical-only Hspec/Python tests and registrations, and the strict append-only audit replaced by the current guard. Follow #2918's assertion mapping/removal list. Preserve the small current fixture set, independent expectations, current framing/refusal/integrity/storage tests, reusable schema helpers, and compiled codec/comparison APIs used by `persistence_snapshot.py` and `persistence_contract`. No maintained historical mode, retirement corpus or mandatory outgoing DTO/migration/fixture per bump remains. Retired material survives in git history; useful runtime migration/DTO source need not be deleted.
4. Remove `tools/save_compat_migration_probe.py`, its dedicated self-tests and all active registry, timeout, sweep-selection, runner, census and lab references. Keep any helpers used by current probes. Reconcile `.quruntul/adapter.py`/checks, `probe_runner_registry.py`, `ci_probes.py`, sweep tests, README counts and affected census consumers together. The old probe key must fail as unknown/retired without launching an engine, not succeed with zero work. Remove only its census row through a scoped locked retirement path, preserving every other probe's measurements/attempts/policy; ordinary census seeding must not become an automatic pruning operation. Git history preserves the retired evidence.
5. Replace historical gate-selection/parity assertions with positive tests of current safety and negative tests of historical reachability. Cover PR and multi-commit master changes, local tracked edits, Cabal/fixture/tool paths, docs-only paths and unavailable bases. Current checks still execute and fail on bad inputs after historical files are absent. Keep `persistence_contract` CI-eligible and selected for persistence changes, with its existing three fresh-process cycles; retain inventory/capability gates and feature-specific persistence requirements.
6. Publish consistent operating rules in root `CLAUDE.md`, `src/World/Save/CLAUDE.md`, persistence/engine contracts, `tools/README.md` and relevant tool headers. A shape change advances every affected component/metadata/Lua/framing version and makes each affected carrier current-only, disabling its older accepted dispatches and affected exact-shape legacy routes. Unsupported saves fail before publication, preserve the old paused session and are not rewritten; incompatible authoritative generations do not fall back to `.prev`. The bridge-only `currentSaveVersion` is not on-disk protection. Explain the guard's semantic limits and current-test procedure.

## Release checklist

Publish this checklist as a release prerequisite, without implementing it now: begin the support promise with the first public-release format; freeze its transitive wire shapes and capture real nonempty release fixtures with independent expectations; build supported-baseline tracking, migrations, historical verification and real restart coverage afresh; wire and actually validate release gates. Do not resurrect pre-release saves, their corpus or a dormant historical mode. Keep current correctness and transaction/refusal guarantees throughout. This epic may close before release once its activation and checklist are delivered.

## Acceptance

```bash
cabal build all
cabal build synarchy-test-headless
python3 tools/current_save_schema_audit.py --self-test
python3 tools/ci_expensive_gates.py --self-test
python3 tools/ci_parity_audit.py --self-test
python3 tools/ci_parity_audit.py
python3 tools/save_compat_audit.py
python3 tools/test_save_compat_audit.py
python3 tools/persistence_inventory_audit.py
python3 tools/test_persistence_inventory_audit.py
python3 tools/engine_env_capability_audit.py
python3 tools/test_engine_env_capability_audit.py
python3 tools/test_run_probes.py
python3 tools/ci_probes.py --self-test
python3 tools/test_persistence_contract_sweep.py
python3 tools/test_readme_registry_count.py
python3 tools/test_probe_census.py
python3 tools/probe_census.py --validate
python3 .quruntul/checks.py
```

Run the guard's real base-ref check with recorded SHAs and the nonempty current Hspec groups retained by #2918, including `save components`, `save envelope`, `atomic save storage`, `persistence contract`, `persistence reference integrity`, `Lua persistence components` and moved feature assertions. Record selection and example counts. Prove the old probe is absent from runner/sweep/lab selection and census, and that unrelated census records are preserved.

Run one owned, isolated current probe via `python3 tools/run_probes.py --only persistence_contract --exact --jobs 1 --port 9324` (or another available non-8008 port), using the manager's live-probe slot protocol and retaining its evidence. Verify terminal save/load success and all three fresh-process comparisons. No migration probe, whole local headless suite or `make ci` is required. Verify active references/imports after removal, rather than deleting shared files by name.

## Scope and delivery

Gate/default wiring, historical-tool/test/corpus/probe retirement and the operating rules only. No gameplay/serialization redesign, new migration support, unrelated CI optimization or bulk runtime DTO deletion. Coordinate shared CI surfaces with #2742. #2912 is complete and is not reopened or reimplemented.

Code, executable Markdown and tool-consumed records follow the code PR. Human-facing contracts/evidence use docs-push and are linked in the PR before final review; describe staged activation accurately until the code lands. Preserve concurrent census work through its owning lock/write path.

## Related

- #2914: epic; implementation depends on #2915, #2916, #2917, #2918.
- #2912: delivered intermediate opt-in; owner superseded continued probe availability in D-8.
- #2742: overlapping CI surfaces; #2564: separate faction retirement.
- Design: `docs/designs/prerelease_save_policy_design.md`, PSP-5/P-4, D-1–D-8.

<!-- issue-origin:codex -->
````

### Approved and applied tracker side effects

These four replacements were applied to #2914 with N = 2919. The epic
implementation checkbox remains unchecked; the design ledger links #2919.
No other tracker edits were made.

1. Replace:

```text
- [ ] PSP-5. Activate the pre-release gate set and publish the operating policy
```

with:

```text
- [ ] PSP-5. #N — Activate the pre-release gate set and publish the operating policy
```

2. Replace:

```text
historical reproducibility/tests and the strict append-only audit replaced by the new guard.
```

with:

```text
historical reproducibility/tests, the strict append-only audit replaced by the new guard, and the historical migration probe with its self-test, registry, sweep, runner, census and lab references.
```

3. Replace:

```text
unrelated worldgen gates; probe opt-in belongs to #2912.
```

with:

```text
unrelated worldgen gates. #2912 delivered intermediate probe opt-in; PSP-5 now owns retirement of that probe under the later owner decision.
```

4. Replace:

```text
- #2912: separate probe opt-in dependency.
```

with:

```text
- #2912: delivered intermediate opt-in dependency; PSP-5 supersedes its runnable-on-demand promise by retiring the historical probe.
```

### Verification and deduplication

Rechecked all 70 open titles and three all-state searches (save policy,
compatibility CI, historical gate); no duplicate activation issue found.
#2742 owns CI runtime optimization, not this owner-approved policy retirement.
Epic child links #2915–#2918 reconcile with the design. #2912 is closed;
its #2913 implementation landed at master `32e1af213a27b881cd5c587061e108e35f85f69b`.

Inspected actual CI/local audit commands, reproducibility selection and parity
execution, source/fixture/Cabal patterns, push/PR comparison-base handling,
probe imports and manifest-only inputs, runner/timeout/CI classification,
sweep opt-in tests, lab OPT_IN_ONLY/checks and census reconciliation. The
census intentionally keeps removed registry rows, so scoped retirement must
preserve other measured records instead of making ordinary seeding prune them.
Current snapshot/probe helpers and current fixture/assertion coverage remain.
No implementation, test, build, engine, probe or issue review was run.
No ledger entries remain unchecked.

## Tracker processing complete

All six artifacts were individually approved and filed: epic #2914 and children
#2915, #2916, #2917, #2918, #2919. The processing ledger has zero unchecked
entries. The epic implementation checkboxes remain unchecked; creating issues
is not implementation completion. The manager owns readiness reviews and solver
dispatch. No code, tests, engine runs, issue reviews or implementation PRs were
performed by this design-processing task.

This document is the arc record and keeps its processing history. Do not archive
it while epic #2914 remains open or open issues cite this path. Once the epic is
closed and no open issue cites it, follow the repository's archival rule with
`git mv` and `docs-push`; that future disposition is not performed here.

## PSP-2 compiled-code amendment — approved, awaiting application

D-9 records the owner-approved compiled approach and subsequent committed-base
record amendment. Verified reply `qhziga3rpj59z96ugaqfmfgs6i` approves the full
six-part question beginning `upfj42rmd57qfraqirpb7wias6`, subject to that one
change. The following is the exact approved tracker payload with the change
applied throughout. It is being published before tracker application, as the
manager instructed. The earlier PSP-5 payload remains historical filed text;
the exact replacements below update it. Titles, labels, dependencies and child
checkboxes stay unchanged. The manager re-runs readiness after application;
this task does not add approval labels or start implementation.

### Approved #2916 body (complete)

Title unchanged: [save-policy PSP-2] Enforce current-schema version changes without requiring migrations

Existing classification labels unchanged: `enhancement`, `save-load`, `tooling`.
The current readiness approval is not reused for the amended specification.

````markdown
## Background

PSP-2 of #2914, Phase 2, critical path, depends on #2915 (PSP-1), now delivered. The owner ruled "Rewrite on compiled code" on 2026-11-05 after three CHANGES_REQUESTED rounds on the now-closed PR #2922. This replaces the source-parsing approach and its canonical-syntax restrictions; the retained branch is reference material only. A fresh implementation and review cycle is required.

Before release, a persisted shape change advances every affected component, metadata, Lua or framing version, and each affected carrier then accepts only its current version. Disable its older accepted decoders and affected exact-shape legacy routes. No historical-decoder compatibility analysis, new migration or historical fixture is required.

The live Haskell registry exposes rcVersion/rcInputVers, but erases the wire DTO type; componentCodec still knows that type through ComponentSpec d a. Metadata/framing have separate compiled constants and dispatch. The real Lua save-module registry contains evaluated version/inputVersions values and is exercised by the existing headless Lua harness. These are the evidence sources, not parsed source spellings. Generic describes representation, not arbitrary Serialize behavior.

## Requirements

1. Produce a small canonical current-schema record with a compiled helper in the headless Hspec target or an existing Cabal executable. Walk actual persisted Haskell wire roots using GHC Generic/Typeable representations, transitively including instantiated field types, ordered constructors/fields, record selectors, newtypes and GHC-resolved aliases. Preserve shared-type ownership and report every affected carrier with a reachability path. Bind roots to actual codec DTO types before erasure; do not maintain an unchecked parallel list of names. Cover metadata and framing as well as the live component registry. Missing, newly reachable, duplicate or ambiguous coverage fails closed. No source parser or source-text fingerprint is authoritative for shapes, versions, registrations or dispatch.
2. Explicitly classify reachable serialization boundaries. Generic/Typeable alone cannot tell whether a Serialize instance uses generic encoding: hand-written, newtype-derived and library codecs need explicit treatment, owned wire descriptors and distinguishing behavioral tests where their encoding is not represented by the generic walk. Unlisted/unsupported boundaries fail closed; a Generic instance must not silently bless a custom encoder. Lua's dynamic payload schemas and the hand-written framing bytes belong to this explicit treatment, not a fictional GHC representation of Lua tables. Record compiled framing constants and the custom owners in the canonical record. State honestly which custom contracts are declared/tested rather than mechanically derived. Encoding or meaning changes outside detection still require every affected version to advance, current-only acceptance, review evidence and a distinguishing test; a refreshed descriptor or self-generated expected output is not that evidence.
3. Read current and accepted versions from the live compiled Haskell registry and metadata/framing values, and from the evaluated Lua registry after loading the real registering modules in the existing headless Lua harness. Load production registration paths with only necessary engine-service stubs; do not substitute fake save modules or discover registrations with source patterns. Cross-check complete carrier coverage and fail on loading/coverage errors. Aliases, expressions, concatenations and registration wrappers must expose their actual runtime values. Prove advertised acceptance agrees with the real decoder entry points, including full-session and metadata-only framing/legacy paths; constants or a manually declared enabled flag alone are insufficient. Explicit legacy-route ownership must cover all affected carriers, including B2's Haskell components. Unknown ownership fails; do not compare historical decoder shapes.
4. Compare the validated record committed at an explicit base revision against independently compiled candidate evidence, with the one-time bootstrap exception in requirement 5. A changed recorded shape, including an enum append, requires a strictly increased real on-disk version for every affected carrier. A changed shape or version increase requires that carrier's accepted set to be exactly its current version and its affected exact-shape legacy routes to be disabled. Unchanged carriers need no blanket purge; removing one old registration alone must not demand retirement of other unchanged versions. Reject version decreases and unaccounted coverage loss. New carriers enter explicit coverage. Regenerating, editing or deleting the candidate record cannot bless same-version drift. The bridge-only currentSaveVersion is not protection.
5. Use the current-schema record committed at the explicit base ref as base-side evidence after the one-time bootstrap. Trust only a valid base record validated against its own compiled code at that revision; every merged commit must have passed that freshness check. Normal CI compiles/extracts only the candidate and requires its committed current-schema record to match that compiled evidence exactly; never build both revision trees for routine comparison. Only first introduction extracts the actual pre-guard base as well as the candidate, including a base without a record/helper and with the existing composed older decoders. Any bootstrap inspection adapter must preserve base wire declarations, version values and decoder behavior and record its provenance. A missing, malformed or unvalidated base record fails clearly; absence never silently enables bootstrap after introduction. Never substitute candidate types/records, hand-entered expected shapes or an empty base. Reject missing refs, extraction/build failures, incomplete evidence, stale candidate binaries and candidate-to-itself comparisons. Record resolved commits, the base record identity/validation provenance, tracked candidate working-edit identity and candidate toolchain/extractor identity. PSP-5 owns PR/master/local selection wiring and enforcement of the freshness check on every merged code revision.
6. Keep enum enforcement in this guard; no separate enum pre-release mode/updater. Off-wire changes and comment/layout-only edits do not change the record. Reuse #2922's record/comparison/self-test ideas only where independent of source parsing. Keep existing strict enum/default gates unchanged until PSP-5 activates the replacement and retires historical machinery. No production wire/acceptance changes merely to satisfy the tooling; minimal inspection hooks, type evidence and behavior-preserving boundary factoring are in scope.

## Acceptance

Keep the Python entry point only as orchestration/comparison over compiled/runtime evidence. Provide --self-test, --base-ref and --update-record; update-record does not replace the independent base check. Add a nonempty compiled Hspec group named "current save schema" for extraction, coverage and behavioral regressions. Proposed commands after implementation:

```bash
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "current save schema"'
python3 tools/current_save_schema_audit.py --self-test
python3 tools/current_save_schema_audit.py --base-ref origin/master
python3 tools/enum_append_only_audit.py --self-test
python3 tools/enum_append_only_audit.py
cabal test synarchy-test-headless --test-options='--match "save components"'
cabal test synarchy-test-headless --test-options='--match "save envelope"'
cabal test synarchy-test-headless --test-options='--match "Lua persistence components"'
python3 tools/test_save_compat_audit.py --without-reproducibility
```

Record actual nonzero example counts, resolved base/candidate identities, base record identity/validation provenance and candidate extraction identity. Verify normal comparison reads the validated record committed at the distinct base ref, compiles only the candidate, and checks its committed record for exact freshness. Only first introduction extracts the actual pre-guard base as well as the candidate. Missing, malformed and unvalidated base records must fail; test that later missing records cannot trigger bootstrap or an empty/self comparison. Demonstrate that a same-version compiled shape mutation fails both before and after record regeneration/deletion, and that a correct bump with current-only dispatch passes without migration/fixture inputs.

Use compiled shape fixtures plus comparator/base-record tests for field add/remove/reorder/retype, enum reorder/append, alias parameter permutations, nested/newtype/container types, shared types with partial bumps, missing custom ownership, new or omitted live registrations, and retained older versions/legacy routes. Include an equal-typed named-field swap. Positive cases cover unchanged shapes, whitespace/comments, off-wire edits and removing just one older registration. A model-only synthetic dictionary test does not establish extractor correctness.

Map all three #2922 reviews to regression evidence: evaluated multiline/prefix version expressions; Lua registry aliases, quote/comment/spacing variants and whole-table expressions; appended/wrapped Haskell registries; older decoders behind composition/aliases; metadata list concatenation; framing calls through aliases or similarly prefixed identifiers; a dead-code rejection wrapper; direct legacy entry-point bypasses; and custom-encoder string-literal whitespace. Legal syntax with unchanged behavior passes; unsafe actual versions, acceptance or routes fail. Custom/semantic cases must have an explicit owned behavioral test and stated limit, never a claimed generic guarantee. Exercise public decode paths, not only the inspection report.

No game boot, runtime probe, full headless suite or make ci is required. Follow the shared build-lock protocol for compiled validation. Preserve relevant helper regressions if reused and existing gate behavior throughout preparation.

## Out of scope

Historical-decoder compatibility analysis; a Haskell/Lua source parser or canonical-source language; automatic proof of arbitrary serializers; a separate enum mode; new migrations/historical fixtures or bulk runtime source deletion; gameplay/save-format changes; historical audit/test partitioning (PSP-3/4); mandatory gate activation (PSP-5).

Scope is the compiled inspector, thin driver/comparator, current record, necessary live inspection hooks and focused tests. Tool-consumed records and executable Markdown follow the code PR. Human-facing documentation uses docs-push and is linked from the PR before final review.

## Related

- #2914: epic. Depends on #2915; #2919 activates this guard after all prerequisites land.
- #2922: closed source-parser attempt; read its three review comments as the regression catalogue, not implementation instructions.
- #2912: delivered intermediate probe opt-in; PSP-5 retires that probe.
- Design: docs/designs/prerelease_save_policy_design.md, P-2/PSP-2, D-5/D-9. Owner ruling relayed by verified syn-manager message 524565svgg4h9fvs3xfamdgnua in #syn-save-policy on 2026-11-05.

<!-- issue-origin:codex -->
````

### Exact related tracker edits — approved with committed base record

Titles, labels, child checkboxes and dependencies remain unchanged. The Python command remains a thin driver, so it is not removed. No source-parsing command is left authoritative.

#### #2919

##### Replacement 1

Replace exactly:

```text
Activate #2916's current-schema guard,
```

With:

```text
Activate #2916's compiled current-schema guard (including its nonempty compiled extraction/behavior tests and live Lua registry inspection),
```

##### Replacement 2

Replace exactly:

```text
Record resolved base/candidate identities. Missing comparison evidence fails clearly;
```

With:

```text
Use #2916's validated current-schema record committed at the base ref as base-side evidence after its one-time bootstrap. Compile/extract only the candidate and require its committed record to match that evidence exactly; routine CI never builds two trees. Enforce the freshness check for every merged code revision. Only first introduction extracts the actual pre-guard base. Missing, malformed or unvalidated base records fail clearly; never substitute the candidate record or an empty base, or silently bootstrap again. The Python command is orchestration/comparison only, never source parsing. Include the compiled helper and its tests in gate selection. Record resolved base/candidate commits, base record identity/validation provenance, tracked candidate working-edit identity and candidate toolchain/extractor identity; never reuse a stale candidate binary. Missing comparison evidence fails clearly;
```

##### Replacement 3

Replace exactly:

```text
Explain the guard's semantic limits and current-test procedure.
```

With:

```text
Explain the compiled guard's semantic and custom-serializer limits, explicit custom ownership (including dynamic Lua payloads and hand-written framing), and current-test procedure. Runtime acceptance and legacy-route behavior must agree with the inspected policy; do not restore canonical-source restrictions.
```

##### Replacement 4

Replace exactly:

```text
cabal build synarchy-test-headless
python3 tools/current_save_schema_audit.py --self-test
```

With:

```text
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "current save schema"'
python3 tools/current_save_schema_audit.py --self-test
```

##### Replacement 5

Replace exactly:

```text
Run the guard's real base-ref check with recorded SHAs
```

With:

```text
Run the guard's real base-ref check using the validated record committed at the base ref and fresh compiled candidate evidence; separately verify the one-time extraction of the actual pre-guard bootstrap base, with recorded SHAs
```

##### Replacement 6

Replace exactly:

```text
PSP-5/P-4, D-1–D-8.
```

With:

```text
PSP-5/P-4, D-1–D-9.
```

#### #2914

##### Replacement 1

Replace exactly:

```text
The owner approved an automated current-schema/version guard and a future compatibility promise beginning with the first public-release save format.
```

With:

```text
The owner approved an automated current-schema/version guard and a future compatibility promise beginning with the first public-release save format. The later owner ruling "Rewrite on compiled code" replaces #2922's closed source-parser attempt: #2916 reads persisted Haskell shapes through compiled Generic/Typeable evidence and versions through live Haskell/Lua registries, with explicit treatment of custom serializers and behavioral checks of actual acceptance/legacy paths.
```

##### Replacement 2

Replace exactly:

```text
The automated guard rejects same-version shape drift even after baseline regeneration and documents its semantic limits.
```

With:

```text
The compiled guard compares the validated current-schema record committed at the base ref with fresh compiled candidate evidence; only its non-vacuous first bootstrap extracts the actual pre-guard base. Every merged revision passes record freshness against its own compiled code; routine CI never builds two trees. The guard rejects same-version shape drift even after record regeneration. Missing, malformed or unvalidated base records and missing coverage/evidence fail closed. It documents semantic/custom-serializer limits, including dynamic Lua payloads; source-pattern matching and canonical-source restrictions are not enforcement.
```

### Amendment evidence and scope

Read all three #2922 reviews (comments 6666285557, 6666512335, 6667325506)
and the live #2916/#2919/#2914 bodies. Verified current code at master
`a431c215752f25ec4e9142fb1768c8f333437544`: ComponentSpec retains the actual
DTO type before componentCodec/RegisteredComponent erasure; rcVersion and
rcInputVers are runtime projections of the dispatch table. Metadata and framing
are separate compiled boundaries. The Lua registry exposes evaluated specs;
SaveModules.Support/Components exercise real modules without engine boot.
Generic does not describe arbitrary Serialize code or dynamic Lua payloads,
so D-9's explicit custom treatment is essential. No divergence from the owner's
direction was identified; no new policy question is opened.

This amends an existing child and its two named consistency surfaces, creates
no issue and processes no later slice. The ledger remains six linked entries,
zero unchecked; that records filing, not implementation. No implementation,
branches, builds, tests, engines or review runs were performed. The separate
PSP-4 work and unrelated dirty probe-census files were left untouched.
