# Quruntul onboarding design

## Scope change (2026-10-08, owner)

Owner decision **laz `z3hd4feg`**, request
`user-reference-generic-harness-scope-20261008`: Synarchy is a **reference
prototype for what will be written into Hetoimasia**, no longer a shipping
product. Quruntul must remain generic and independently adoptable. The
owner's words: "quruntul shouldn’t reference synarchy, synarchy needs to use
quruntul as its harness, i want the harness generic enough to be used on
hetoimasia". Consumer selections, budgets, trial planning and onboarding
belong here and in Synarchy's adapter/policy, not in Quruntul.

**Administratively DEFERRED, not passed:** further campaign-wide shakedowns;
the post-measurement trial-count tuning or reduction issue; the campaign
adapter PR and validating batch; full flake-ledger seeding and onboarding
assessments. Existing defects and completed evidence remain explicit. Finite
correctness checks and their applicable existing gates remain mandatory;
long actual simulations and campaigns are optional, local only, never CI,
regardless of Hspec/Python wrappers. The detailed classification below is a
source review, with a report-only current enforcement gap.

This owner decision governs all older onboarding text and review amendments.
Historical plans below preserve rationale and do not authorize resuming
unfinished campaigns. The original issue bodies are retained in the edited
issues' historical sections and exact local before/after snapshots. #2777, #2785,
#2786, #2787 and #2788 remain **OPEN**, with previous approvals removed by
their body-edit commands; closure belongs to the owner. #2789 remains an
open correctness defect and is unedited. No B1–B9 decision, Group 7 work,
retained evidence, lab claim or other issue is dispositioned here.

Quruntul's coordinator has already narrowed and closed
[coghex/quruntul#5](https://github.com/coghex/quruntul/issues/5) for its
**delivered generic scope**, explicitly not claiming the removed consumer
onboarding complete. Its design/vision and historical record at
`aea8e068bc83049cd65ccbffea33f5788deb1a6a` state independent adoptability.
Quruntul neither tracks nor depends on this consumer's progress. Generic
engine mechanics stay in Quruntul; any future engine request must itself be
generic and separately authorized. No Quruntul file or issue is edited here.

## Archival ownership and QS-6 amendment

Under owner/manager instruction **laz `gdvt2c4i`**, the **whole** historical
Quruntul-side design is now preserved verbatim in
[the Synarchy-owned archive](../history/quruntul_shakedown_synarchy_design.md).
This is the authority for the historical Quruntul-side Synarchy plan,
including **D-10, Q-6 and QS-3 to QS-6**, suite selections, budgets, slice sizes
and trial counts. Source: `coghex/quruntul`,
`docs/designs/shakedown_synarchy_design.md`, last-change commit
`aea8e068bc83049cd65ccbffea33f5788deb1a6a`, Git blob
`ce8bccc4df7f9a6a14ec9e15bed1ca856144040b`. Source-byte SHA-256:
`a14613977eb688d15fe2f17af36ddf68e54c87533a736abcb82552face60b490`.
The provenance header is the only addition; removing it yields that same
Git blob. Quruntul's coordinator owns removing its original copy separately;
this task changes no Quruntul file. Active consumer decisions are stated in
this onboarding document, not prescribed by the archive.

Under **laz `6bed6ihh`**, QS-6's legacy-history hook and import are optional
consumer onboarding, **not prerequisites for the generic harness or
Hetoimasia adoption**. The unfinished hook, import, drain and idempotence
operations are administratively **DEFERRED**, not complete or passed.
The existing generic import mechanism and Synarchy adapter are delivered;
that does not establish a delivered Synarchy hook or import receipt.
The historical 2026-10-04 registry snapshot and unverified drain/accepted
proposal consistency concerns remain in #2785's original contract; no private
registry is inspected, drained or migrated here.

Finite adapter/hook fixtures become **MANDATORY only when a future authorized
change implements or touches the optional hook**: `python3 .quruntul/checks.py`
and applicable Testing-tiers self-tests, with synthetic-tree terminal/status,
source-approval, proposal/design mapping, evidence-manifest, stable identity,
refusal and successful/refused source-immutability coverage. Those fixtures
must not read the real private registry. Existing finite checks for unrelated
changed behavior keep their own applicability. No import or fixture is run
by this issue/docs-only amendment.

Design state: `historical consumer plan — remaining campaigns administratively deferred`

## Processing and execution status

The old `[x]` processing entries meant **filed**, not implemented, passed or
closed. Stable QS identifiers are retained; this ledger now states execution
status separately.

| Slice | Consumer issue | Completed evidence | Unfinished stage |
|---|---|---|---|
| EPIC | [#2777](https://github.com/coghex/synarchy/issues/2777) | Four children filed; retained operations linked | Remaining campaign-wide onboarding acceptance **DEFERRED**; OPEN |
| QS-6 | [#2785](https://github.com/coghex/synarchy/issues/2785) | Generic import engine (#8/#9) and existing consumer adapter (#2760) delivered; historical source snapshot retained | Consumer hook, import, drain and idempotence **OPTIONAL onboarding, administratively DEFERRED**, not passed; not an adoption prerequisite; OPEN |
| QS-3 | [#2786](https://github.com/coghex/synarchy/issues/2786) | Pre/post-repair shakedown evidence; verified repairs; once-only Tier A evidence | Further campaign shakedowns and onboarding assessments/completion **DEFERRED**; failures retained; OPEN |
| QS-4 | [#2787](https://github.com/coghex/synarchy/issues/2787) | One measured trial and projection, run a99deb93 | Trial choice/reduction issue, campaign adapter PR, validating batch **DEFERRED**; neither choice made; OPEN |
| QS-5 | [#2788](https://github.com/coghex/synarchy/issues/2788) | Historical census/ledger snapshots retained | Full seeding, coverage-completion and onboarding assessments **DEFERRED**, not passed; OPEN |

## Completed evidence retained (2026-10-08)

- [Tier A, 2026-10-08](https://github.com/coghex/synarchy/issues/2786#issuecomment-6066585081), Synarchy `856c199ab`: A1 hit the 60-minute sweep cap (exit 124, 3600.164 s). Four generations compared structurally IDENTICAL and ten cross-probes passed; `save_compat_migration` was still active, so neither the migration nor the sweep completed. That child has an existing allowance of up to **3600 s**; an outer 3600-second sweep also includes earlier work. No replacement budget is chosen. The old structural mismatch remains a historical unresolved observation; this pass did not reproduce it or demonstrate a mismatch's semantic diff.
- Tier A A2: **control-icon pixel mismatch** after reload; the explicit target/control discovery-state checks passed. Baseline/reload PNGs and source-boot logs are retained. This is an open visual/oracle finding, not demonstrated discovery-state loss.
- Tier A A3: **return intent not pending after load**, with a separate **pre-save inbound-leg miss** (25.5 → 25.4 tiles). The intermediate per-carrier load-stage task state was not exposed, so the loss boundary is unproved. Arrival, adjacent deposit and both radio-consumer checks also failed; exact item identity/properties survived. All six failure labels and engine A/B logs are retained; neither movement nor radio causality is guessed.
- Tier A A4/A5/A6 passed once: item-list sessions, unified transfers (337 checks), and context-menu site diagnostics. These passes do not erase the earlier failures or establish stability. A6 reported seed 2882023494, five eligible sites among 33 candidates at (0,0).
- [QS-4 measurement](https://github.com/coghex/synarchy/issues/2787#issuecomment-6068722762): run **`a99deb93-c23a-4690-b20d-abc8f92e909b`**, trial **595.9 s** (595.947089), build 297.760554 s and enumeration 6.180877 s separately. 10,772 eligible identities: 10,771 passed, 0 failed, 1 pending. Two duplicate paths/four occurrences remained excluded from identity measurement: [#2789](https://github.com/coghex/synarchy/issues/2789); Quruntul's detection works but does not fix the names.
- Measurement projection at consumer settings 400 / 3600 s / 43200 s / 10 trials: **27 slices**, 270 trial processes, **23.305–600.424 s** per full slice and **1.744–45.032 h** of trial loops (illustrative preparation-inclusive range 1.873–47.311 h). This is a conditional scenario from one whole-suite trial, not linear per-example pricing, a calibrated upper bound or validating-batch acceptance. Neither trial count nor slice size was chosen.
- [Full retained-evidence dispositions](https://github.com/coghex/synarchy/issues/2786#issuecomment-5998763581) preserve every older failed or unclassified finding and its individual follow-up. New evidence supplements them; administrative deferral is not a clean-suite or completion claim.

## Other retained findings (not waived by deferral)

The [22-row disposition packet](https://github.com/coghex/synarchy/issues/2786#issuecomment-5998763581) and its linked updates remain authoritative for the individual observations, limits and follow-ups:

| Finding | Retained disposition / link |
|---|---|
| Duplicate StructureRotation names | [#2789](https://github.com/coghex/synarchy/issues/2789): two paths each count 2; four twins unmeasured. |
| Blood GPU lifecycle fixture admission | [#2804](https://github.com/coghex/synarchy/issues/2804); a later clean trial does not erase the old missing fixture. |
| Combat-animation non-integer handle response | [#2803](https://github.com/coghex/synarchy/issues/2803); earlier oversized request repaired by #2792, strict integer rejection retained. |
| Blueprint whole-frame visual oracle | [#2654](https://github.com/coghex/synarchy/issues/2654); no footprint geometry defect established. |
| Craft-bill no-candidate scan | [#2523](https://github.com/coghex/synarchy/issues/2523); rejected eligibility input absent, no stale-candidate acceptance claimed. |
| Etymology custom-page selection race | [#2771](https://github.com/coghex/synarchy/issues/2771). |
| Retrieval radio and return/restart signatures | [#920](https://github.com/coghex/synarchy/issues/920), [#1769](https://github.com/coghex/synarchy/issues/1769), and Tier A A3 above; separate pre-save miss and post-load intent failure. |
| Follow-command combat/treatment priority | [#724](https://github.com/coghex/synarchy/issues/724); old cause unproved despite later clean trial. |
| Item-list lost failure identities | [#2802](https://github.com/coghex/synarchy/issues/2802); A4 passed without reproducing old four failed checks. |
| Embark control-icon mismatch | Tier A A2 above; fixed caller #2791 is separate; no persisted flag loss established. |
| Overlay load fixture registration omission | [#2799](https://github.com/coghex/synarchy/issues/2799); correct integrity refusal, separate from verified #2794 log retention. |
| Mental-state disturbed setup | [#2773](https://github.com/coghex/synarchy/issues/2773); policy not graded by failed fixture integrity. |
| Offscreen wrong-page initialization wait | [#2800](https://github.com/coghex/synarchy/issues/2800). |
| Persistence cap and old comparison mismatch | Tier A A1 above; incomplete migration and old mismatch remain distinct. |
| Physiology threshold signatures | [#724](https://github.com/coghex/synarchy/issues/724), [#2761](https://github.com/coghex/synarchy/issues/2761) adjacent context only; input/cause unproved. |
| Power-workshop arrival window | [#1758](https://github.com/coghex/synarchy/issues/1758), [#1577](https://github.com/coghex/synarchy/issues/1577); no accounting cause established. |
| Repair-AI ownership/arrival | [#724](https://github.com/coghex/synarchy/issues/724); precise transitions absent. |
| River wrong-page fixture / unreported checks | [#2653](https://github.com/coghex/synarchy/issues/2653); wrapper pass does not pass failed or missing checks. |
| Context-menu terrain-search precondition | [#1255](https://github.com/coghex/synarchy/issues/1255) gate context; A6 passed on its reported seed, old failure remains retained. |
| Tutorial glyph/frame oracle | [#2801](https://github.com/coghex/synarchy/issues/2801); no renderer cause inferred. |
| Unified-transfer modeA label/session loss | [#2802](https://github.com/coghex/synarchy/issues/2802), [#1255](https://github.com/coghex/synarchy/issues/1255); A5 pass does not recover old labels or clear reason. |

Completed repairs remain explicit: #2790 preview desktop classification verified under a claim; #2793 expedition-loop budget passed its later 1131-second run; #2791/#2792 crossed their old failure points; #2794 retained the actual rejecting boot log. See [terminal reconciliation](https://github.com/coghex/synarchy/issues/2786#issuecomment-5998429811). The owner's [acceptance of the historical preview evidence](https://github.com/coghex/synarchy/issues/2786#issuecomment-5990091289) preserves the fact that its original launch was outside the approval then in force.

## Consumer settings and evidence limits

The measured consumer adapter at `856c199ab` declares headless
`batch_tests = HSPEC_SLICE = 400`, `trial_seconds = 3600`,
`batch_seconds = 43200`, and global `flake_trials = 10`; the same 400 slice
constant currently feeds the graphical suite. These are Synarchy settings,
not harness defaults required by Quruntul. They were not changed. The
measurement's identity hash and exact phase provenance remain in its linked
comment/durable record; counts alone cannot price a new selection. A whole
trial is not a linear per-example cost: process startup and world-generation
fixtures repeat per trial and slice. Its 27-slice projection is conditional,
not validated or chosen. The fixed-process overhead proxies were about
1.22–5.70 seconds; shared fixture cost was unmeasured.

The Tier A pass preserved all six logs/artifacts under
`~/.local/state/synarchy-tier-a-20261008/`; its worktree remains detached at
`856c199ab`. The measurement's logs and selection identity list remain under
`~/.local/state/synarchy-2787-trial-20261008/`. Neither directory nor a lab
ledger/run directory is copied into this repository. Administrative deferral
does not erase the old mismatch, pre-save movement miss, control pixel
mismatch, missing historical labels or duplicate names. No old finding is
silently classified stable, fixed or assessed.

## Check classification by actual work

Owner authority: laz `z3hd4feg`, 2026-10-08. **MANDATORY:** finite correctness checks and the relevant existing gates for the behavior changed. **OPTIONAL, LOCAL ONLY, NEVER CI:** long actual game simulations/campaign work, even inside Hspec or Python. Neither a Python filename, an Hspec assertion at the end, determinism, nor a CI-eligible declaration settles the classification. A finite check may need a compiled codec, a bounded engine API fixture or a one-shot generated data input; engine startup alone does not make it a campaign. Conversely, a time cap does not make a real gameplay evolution experiment a finite tool check.

The classifications below describe source behavior at Synarchy `856c199ab`; they are reasoned source classifications, not new timing measurements. Short direct numerical API checks remain finite; autonomous AI scenarios or live fluid/infection evolution are conservatively classified as simulation. Existing gate declarations are reported, not changed. No command in this section was run for this scope task.

### Mandatory Hspec correctness and inventory commands

From CLAUDE.md §Build, run, test / §Testing tiers, choose the relevant finite group:

```sh
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "<describe name>"'
```

On this Mac the production Cabal is `cabal-3.16.1.0`; the source spellings above/CI below use `cabal`. No `-f dev`. Builds are prerequisites where needed, not permission to execute simulation groups. Whole suites and `make ci` are not default local validation; CLAUDE.md requires an explicit request for `make ci`.

#2789 preserves its exact focused finite acceptance (not executed here):

```sh
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "World.Render.StructureRotation"'
cabal test synarchy-test-headless --test-show-details=direct --test-options='--ignore-dot-hspec --no-color --unicode --format=checks --dry-run'
```

The last command enumerates; it runs no examples. Source: [#2789](https://github.com/coghex/synarchy/issues/2789), `test-headless/Test/Headless/World/Render/StructureRotation.hs`.

Existing CI headless invocations (matrix lane `world` or `rest`, `.github/workflows/ci.yml:830–837`) are exactly:

```sh
cabal test synarchy-test-headless -v0 --test-show-details=direct --test-options='--lane ${{ matrix.lane }} --print-slow-items=20 --format=failed-examples'
SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless -v0 --test-show-details=direct --test-options='--lane ${{ matrix.lane }} --print-slow-items=20 --format=failed-examples'
```

The worldgen selector chooses the second branch. The local mirror, `tools/ci-local.sh:188`, currently uses:

```sh
SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless -v0 --test-show-details=direct --test-options='--print-slow-items=20 --format=failed-examples'
```

These are existing mixed-inventory commands, **not** a declaration that every selected example is mandatory. Finite correctness groups keep their applicable gates; the long simulation examples identified below fall under the new optional/local-only rule despite sharing this executable. No lane selection or implementation is changed by this document.

`python3 tools/headless_lanes.py` is a finite coverage gate: it uses dry-run inventories (with multiplicity) and synthetic lane selection checks, not suite execution (`tools/headless_lanes.py:1–53`). Graphical CI only builds `synarchy-test-graphical` when its selector fires; no graphical assertions are executed by that workflow. Finite GPU-free specs belong in `test-headless/` (CLAUDE.md Testing tiers).

### Mandatory finite adapter/tool self-tests when their inputs change

Exact Testing-tiers commands from CLAUDE.md:

| Changed input | Mandatory applicable check |
|---|---|
| `world_audit.py` / `world_check.py` | `python3 tools/test_audit.py` |
| `run_probes.py` | `python3 tools/test_run_probes.py` |
| Sweep `SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` / registry `PROBES` | `python3 tools/test_persistence_contract_sweep.py` |
| `PROBES`, `CI_ELIGIBLE`, `PROTOCOL_PROBES`, or `.quruntul/` | `python3 .quruntul/checks.py` |

`python3 tools/ci_probes.py --self-test` validates selection data without an engine; `python3 tools/ci_parity_audit.py --self-test` and `python3 tools/ci_parity_audit.py` validate CI/local command parity. Engine-free companion tests such as `python3 tools/test_location_overlay_probe.py`, `python3 tools/test_item_list_widget_probe.py`, `python3 tools/test_transfer_context_menu_probe.py` and `python3 tools/test_expedition_loop_day_budget.py` remain finite mandatory checks of the changed contracts even though the corresponding real game probes are optional. The existing CI comments at `.github/workflows/ci.yml:1320–1620` explicitly document the fake consoles, synthetic process descendants, temporary trees and injected collaborators used by these companions.

### Exact finite gate inventory from existing CI

The following source inventory records the literal Python tool-check commands in CI's `static-audits` and `test-and-audits` jobs (plus source-manifest checks); no tool module was imported or executed to collect it. Conditions, where present, continue to apply. Codec/native/compiler-backed assertions are still finite correctness work. World-check's quick mode is a finite deterministic generation/baseline comparison, not an advancing game campaign; its existing worldgen selection applies. The distinct engine-free `test_determinism.py` proves the content-identity checker itself.

| Exact command | Existing CI definition |
|---|---|
| `python3 tools/sdist_manifest_audit.py --self-test` | [.github/workflows/ci.yml:285](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L285) |
| `python3 tools/sdist_manifest_audit.py` | [.github/workflows/ci.yml:286](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L286) |
| `python3 tools/test_audio_native.py --sanitize` | [.github/workflows/ci.yml:540](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L540) |
| `python3 tools/test_audio_build_dependencies.py` | [.github/workflows/ci.yml:541](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L541) |
| `python3 tools/headless_init_import_audit.py --record --builddir dist-newstyle -- build synarchy-test-headless -v0` | [.github/workflows/ci.yml:571](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L571) |
| `python3 tools/headless_init_import_audit.py --builddir dist-newstyle` | [.github/workflows/ci.yml:572](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L572) |
| `python3 tools/headless_init_import_audit.py --cabal-regression` | [.github/workflows/ci.yml:573](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L573) |
| `python3 tools/headless_lanes.py` | [.github/workflows/ci.yml:586](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L586) |
| `python3 tools/test_save_compat_audit.py --without-reproducibility` | [.github/workflows/ci.yml:618](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L618) |
| `python3 tools/save_compat_audit.py` | [.github/workflows/ci.yml:619](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L619) |
| `python3 tools/test_save_compat_audit.py --only-reproducibility` | [.github/workflows/ci.yml:644](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L644) |
| `python3 tools/world_check.py --quick` | [.github/workflows/ci.yml:653](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L653) |
| `python3 tools/ci_cache_epoch.py --self-test` | [.github/workflows/ci.yml:938](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L938) |
| `python3 tools/ci_cache_cleanup.py --self-test` | [.github/workflows/ci.yml:939](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L939) |
| `python3 tools/ci_cache_report.py --self-test` | [.github/workflows/ci.yml:940](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L940) |
| `python3 tools/test_audit.py` | [.github/workflows/ci.yml:943](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L943) |
| `python3 tools/test_determinism.py` | [.github/workflows/ci.yml:960](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L960) |
| `python3 tools/lua_module_budget.py` | [.github/workflows/ci.yml:967](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L967) |
| `python3 tools/test_lua_duplicate_function_audit.py` | [.github/workflows/ci.yml:980](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L980) |
| `python3 tools/lua_duplicate_function_audit.py` | [.github/workflows/ci.yml:981](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L981) |
| `python3 tools/test_lua_registration_audit.py` | [.github/workflows/ci.yml:998](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L998) |
| `python3 tools/lua_registration_audit.py` | [.github/workflows/ci.yml:999](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L999) |
| `python3 tools/test_haskell_module_budget.py` | [.github/workflows/ci.yml:1006](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1006) |
| `python3 tools/haskell_module_budget.py` | [.github/workflows/ci.yml:1007](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1007) |
| `python3 tools/test_unicode_operator_audit.py` | [.github/workflows/ci.yml:1019](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1019) |
| `python3 tools/unicode_operator_audit.py` | [.github/workflows/ci.yml:1020](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1020) |
| `python3 tools/tshow_spelling_audit.py --self-test` | [.github/workflows/ci.yml:1036](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1036) |
| `python3 tools/tshow_spelling_audit.py` | [.github/workflows/ci.yml:1037](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1037) |
| `python3 tools/test_haddock_link_audit.py` | [.github/workflows/ci.yml:1059](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1059) |
| `python3 tools/haddock_link_audit.py` | [.github/workflows/ci.yml:1060](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1060) |
| `python3 tools/lua_strict_decode_audit.py --self-test` | [.github/workflows/ci.yml:1080](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1080) |
| `python3 tools/lua_strict_decode_audit.py` | [.github/workflows/ci.yml:1081](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1081) |
| `python3 tools/headless_init_import_audit.py --self-test` | [.github/workflows/ci.yml:1097](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1097) |
| `python3 tools/config_write_audit.py --self-test` | [.github/workflows/ci.yml:1114](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1114) |
| `python3 tools/config_write_audit.py` | [.github/workflows/ci.yml:1115](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1115) |
| `python3 tools/test_persistence_inventory_audit.py` | [.github/workflows/ci.yml:1127](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1127) |
| `python3 tools/persistence_inventory_audit.py` | [.github/workflows/ci.yml:1128](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1128) |
| `python3 tools/test_engine_env_capability_audit.py` | [.github/workflows/ci.yml:1140](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1140) |
| `python3 tools/engine_env_capability_audit.py` | [.github/workflows/ci.yml:1141](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1141) |
| `python3 tools/enum_append_only_audit.py --self-test` | [.github/workflows/ci.yml:1157](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1157) |
| `python3 tools/enum_append_only_audit.py` | [.github/workflows/ci.yml:1158](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1158) |
| `python3 tools/test_cabal_module_audit.py` | [.github/workflows/ci.yml:1169](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1169) |
| `python3 tools/cabal_module_audit.py` | [.github/workflows/ci.yml:1170](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1170) |
| `python3 tools/material_id_audit.py --self-test` | [.github/workflows/ci.yml:1184](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1184) |
| `python3 tools/material_id_audit.py` | [.github/workflows/ci.yml:1185](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1185) |
| `python3 tools/bare_name_icon_asset_check.py --self-test` | [.github/workflows/ci.yml:1203](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1203) |
| `python3 tools/bare_name_icon_asset_check.py` | [.github/workflows/ci.yml:1204](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1204) |
| `python3 tools/concept_id_inventory_audit.py --self-test` | [.github/workflows/ci.yml:1228](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1228) |
| `python3 tools/concept_id_inventory_audit.py` | [.github/workflows/ci.yml:1229](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1229) |
| `python3 tools/action_outcome_coverage.py --self-test` | [.github/workflows/ci.yml:1248](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1248) |
| `python3 tools/action_outcome_coverage.py --verify-tier1` | [.github/workflows/ci.yml:1249](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1249) |
| `python3 tools/test_findings_report_audit.py` | [.github/workflows/ci.yml:1262](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1262) |
| `python3 tools/findings_report_audit.py` | [.github/workflows/ci.yml:1263](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1263) |
| `python3 tools/test_pack_atlas.py` | [.github/workflows/ci.yml:1289](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1289) |
| `python3 tools/pack_atlas.py --validate-only --strict` | [.github/workflows/ci.yml:1290](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1290) |
| `python3 tools/test_check_texture_paths.py` | [.github/workflows/ci.yml:1310](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1310) |
| `python3 tools/check_texture_paths.py` | [.github/workflows/ci.yml:1311](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1311) |
| `python3 tools/test_map_page_codec_measure.py` | [.github/workflows/ci.yml:1316](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1316) |
| `python3 tools/ci_probes.py --self-test` | [.github/workflows/ci.yml:1622](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1622) |
| `python3 tools/ci_expensive_gates.py --self-test` | [.github/workflows/ci.yml:1623](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1623) |
| `python3 tools/ci_docs_fast_path.py --self-test` | [.github/workflows/ci.yml:1624](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1624) |
| `python3 tools/test_run_probes.py` | [.github/workflows/ci.yml:1625](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1625) |
| `python3 tools/test_persistence_contract_sweep.py` | [.github/workflows/ci.yml:1626](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1626) |
| `python3 tools/test_action_outcome_probe.py` | [.github/workflows/ci.yml:1627](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1627) |
| `python3 tools/test_tillable_fluid_filter.py` | [.github/workflows/ci.yml:1628](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1628) |
| `python3 tools/test_probelib.py` | [.github/workflows/ci.yml:1629](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1629) |
| `python3 tools/test_probe_flake.py` | [.github/workflows/ci.yml:1630](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1630) |
| `python3 tools/test_probe_census.py` | [.github/workflows/ci.yml:1631](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1631) |
| `python3 tools/test_probe_claim.py` | [.github/workflows/ci.yml:1632](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1632) |
| `python3 tools/test_probe_resource_lock.py` | [.github/workflows/ci.yml:1633](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1633) |
| `python3 tools/test_deflake.py` | [.github/workflows/ci.yml:1634](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1634) |
| `python3 tools/test_location_embark_probe.py` | [.github/workflows/ci.yml:1635](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1635) |
| `python3 tools/test_location_probe_config_isolation.py` | [.github/workflows/ci.yml:1636](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1636) |
| `python3 tools/test_location_overlay_probe.py` | [.github/workflows/ci.yml:1637](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1637) |
| `python3 tools/test_probe_root_cleanup.py` | [.github/workflows/ci.yml:1638](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1638) |
| `python3 tools/test_flora_growth_probe.py` | [.github/workflows/ci.yml:1639](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1639) |
| `python3 tools/test_location_content_probe.py` | [.github/workflows/ci.yml:1640](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1640) |
| `python3 tools/test_movement_probe.py` | [.github/workflows/ci.yml:1641](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1641) |
| `python3 tools/test_farm_ai_probe.py` | [.github/workflows/ci.yml:1642](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1642) |
| `python3 tools/test_probe_boot_logs.py` | [.github/workflows/ci.yml:1643](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1643) |
| `python3 tools/test_item_list_widget_probe.py` | [.github/workflows/ci.yml:1644](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1644) |
| `python3 tools/test_construction_probe.py` | [.github/workflows/ci.yml:1645](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1645) |
| `python3 tools/test_expedition_loop_day_budget.py` | [.github/workflows/ci.yml:1646](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1646) |
| `python3 tools/test_mental_efficiency_probe.py` | [.github/workflows/ci.yml:1647](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1647) |
| `python3 tools/test_transfer_context_menu_probe.py` | [.github/workflows/ci.yml:1648](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1648) |
| `python3 tools/review_gate_decision.py --self-test` | [.github/workflows/ci.yml:1661](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1661) |
| `python3 tools/review_gate_label_policy.py --self-test` | [.github/workflows/ci.yml:1673](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1673) |
| `python3 tools/ci_parity_audit.py --self-test` | [.github/workflows/ci.yml:1688](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1688) |
| `python3 tools/ci_parity_audit.py` | [.github/workflows/ci.yml:1689](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/.github/workflows/ci.yml#L1689) |

The mirror is `tools/ci-local.sh` and `Makefile` (`make ci` delegates to it); parity ownership is `tools/ci_parity_audit.py`. CI cache/selection/report commands taking live GitHub/cache inputs are workflow operations rather than correctness tests; only their `--self-test` forms above are classified as finite assertions. Required subsystem guards retain their existing change-selection rules: unit-art inputs require both `python3 tools/test_pack_atlas.py` and `python3 tools/pack_atlas.py --validate-only --strict`; capped modules use their matching budget guards; persistence/capability inputs keep their audits. Nothing here authorizes running the entire gate list for an unrelated docs edit.

### Optional local-only simulation/campaign work

- Game-running scenario probes selected by `python3 tools/run_probes.py --only <substrings> [--jobs N]`: selection/registration is `tools/probe_runner_registry.py`; execution is the selected source, not the runner's filename. Examples: `expedition_loop` is a whole expedition with AI survival, real combat, return and reload (`probe_runner_registry.py:118`); `expedition_retrieval` walks a remote retrieval journey (`:129`); `construction`, `farm_ai`, `combat_anim`, `physiology`, `position_hold` and similar timed AI scenarios remain optional local evidence. Finite direct-API probes are distinguished in the 15-row table below; do not classify the entire registry uniformly.
- `python3 tools/persistence_contract_sweep.py`: representative real-world scenario and default eleven cross-probes via `run_probes.py --exact --jobs 2`; `tools/persistence_contract_sweep.py` defines its phase and cross-probe key selection, with default internal retries. This broad composite simulation/campaign work is optional, local only, never CI. Its engine-free `test_persistence_contract_sweep.py` stays mandatory for applicable input changes.
- `quruntul shakedown [--target SUITE]`, `quruntul flake` and full seeding/validation batches are optional local campaigns, never required gates. Synarchy's `.quruntul/adapter.py` owns its suites, budgets, slice size and global trial count. Generic mechanics belong to Quruntul; this consumer campaign is currently administratively DEFERRED. No adapter operation, trial override or seeding occurs here.
- **Hspec long actual simulation:** the two outer `ruin occupant survival (#2754)` examples in `test-headless/Test/Headless/Unit/RuinOccupantSurvival.hs:433–439` generate the seed-14 size-64 world and advance real unit movement, physiology and AI at 0.1-second steps for **10,800 engine seconds per occupant**, with 1,200-second preroll in the late case (`:19–48`, `:88–100`, `:320–348`). Scripted acceleration and an Hspec wrapper do not change the actual work: these two scenarios are optional, local only, never CI. The nested **`survival exemption`** finite unit assertions (`Unit/SurvivalExemption.hs:1–15,204`) stay mandatory; the whole parent group must not be indiscriminately exempted. Existing registration is `test-headless/Spec.hs:1027`.
- **Do not misclassify finite stepping assertions:** `Sim.Fluid.Conservation` uses one production tick on bounded states; `Sim.Fluid.Harness` checks authored fixtures/step accounting/refusals against bounded trajectories, invalid adapters and hand-computable metrics (`Sim/Conservation.hs:147–199`, `Sim/Harness.hs:1–39,141–157`). `source drinking pose lifecycle` controls a tiny scene and directly pumps deterministic transitions with no sleep/poll (`Unit/SourceDrinkPose.hs:18–40`). A module name containing Sim or a numeric logical duration alone does not make these long game campaigns. Their correctness assertions remain mandatory. This is a bounded inspection for the requested examples, not an exhaustive redesign of all Hspec groups.

## Minimal CI enforcement gap: the 15 behavior probes

Source: `tools/ci_probes.py:55–146` `CI_ELIGIBLE` (the same set used by `--status`, statically read without running that command). Exactly 15 entries are classified below. CI does **not** run all 15 for every PR: `.github/workflows/ci.yml:1697,1725–1737` path-selects a subset, while core/unclassified changes select the whole eligible set. The job builds the engine/codec, then runs exactly:

```sh
python3 tools/run_probes.py --only "${{ steps.probe-selection.outputs.only }}" --exact --retries 1 --jobs 2
```

That existing retry/parallel behavior is described, not authorized or changed here (`ci.yml:1808–1823`).

| CI-eligible key | Actual-work class | One-line source evidence |
|---|---|---|
| `audio_null` | finite check | Empty arena, generated sample/synth fixtures; bounded callback/admission/volume/reset/shutdown assertions, no gameplay scenario. [tools/audio_null_probe.py:102](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/audio_null_probe.py#L102) |
| `canteen_instance` | finite check | AI update disabled and simulation paused; direct production drink/refill execute calls assert the exact selected instance fill. [tools/canteen_instance_probe.py:115](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/canteen_instance_probe.py#L115) |
| `cargo_capacity` | finite check | AI disabled; at most 200 direct depositToCargo calls compare final instance-derived weight with capacity. [tools/cargo_capacity_probe.py:94](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/cargo_capacity_probe.py#L94) |
| `consumable_effects` | finite check | AI disabled; direct brew/drink calls assert numeric quality/temperature/fill effects and short 0.3/2-second numeric decay/regeneration comparisons, no autonomous journey. [tools/consumable_effects_probe.py:264](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/consumable_effects_probe.py#L264) |
| `content_registry` | finite check | Eight public load/query registry pairs plus reload/loot joins; no simulated campaign or AI arbitration. [tools/content_registry_probe.py:18](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/content_registry_probe.py#L18) |
| `cooking` | finite check | AI disabled; direct executeAt/transfer/build API assertions check kitchen content, consumption, quality and output temperature. [tools/cooking_probe.py:63](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/cooking_probe.py#L63) |
| `craft` | finite check | AI, brain and mental ticks neutralized; direct catalogue/refusal/execute/executeAt assertions over controlled inputs. [tools/craft_probe.py:117](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/craft_probe.py#L117) |
| `debug_console_boot` | finite check | Port/CLI rejection, successful binding, socket protocol and widget-load assertions; no generated gameplay world. [tools/debug_console_boot_probe.py:275](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/debug_console_boot_probe.py#L275) |
| `fluid_exact_restart` | game simulation | Generated world; actual fluid flow must turn a full cell partial (FLOW_TIMEOUT=60 s), then settle, save and fresh-process reload. Paused comparison does not remove the simulation needed to create the fixture. [tools/fluid_exact_restart_probe.py:254](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/fluid_exact_restart_probe.py#L254) |
| `infection` | game simulation | Actual engine wound/resource evolution: waits 8+6+5+8 seconds for growth/sepsis/prevention; accelerated test rate and disabled AI do not make those real-time dynamics a stubbed tool check. [tools/infection_probe.py:107](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/infection_probe.py#L107) |
| `medic_coord` | game simulation | Active unit_ai moves autonomous medics and treats patients; repeatedly unpauses/polls for up to 40 seconds to infer which medic intervened. [tools/medic_coord_probe.py:191](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/medic_coord_probe.py#L191) |
| `persistence_contract` | finite check | Four small-world fresh-process boots assert save/load reset policy and complete production-codec structural equality; post-load checks stay paused. Live attack/roster references make the serialization fixture non-vacuous; no fight/campaign outcome is graded. [tools/persistence_contract_probe.py:16](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/persistence_contract_probe.py#L16) |
| `preview_cli` | finite check | Mostly pre-boot CLI rejection; check_dump_layer_selection actually performs a bounded one-region --dump and asserts selected JSON fields. It is not wholly no-boot despite its header; no live game simulation/window. [tools/preview_cli_probe.py:625](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/preview_cli_probe.py#L625) |
| `repair` | finite check | AI disabled; direct repairAt/craft refusal checks assert cost, range, station, wear axes and identity, without simulated combat wear. [tools/repair_probe.py:2](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/repair_probe.py#L2) |
| `repair_item` | finite check | Generated terrain is fixture preparation; direct repairItem deltas, bounds, identity and non-finite-input refusals, no autonomous repair gameplay loop. [tools/repair_item_probe.py:11](https://github.com/coghex/synarchy/blob/856c199ab85c9fadbfc597982f1fa92b01146912/tools/repair_item_probe.py#L11) |

**Conflict with “simulation is never CI”: `fluid_exact_restart`, `infection`, `medic_coord`.** They remain in `CI_ELIGIBLE` and can be selected into the blocking PR behavior-probes job. Required minimal enforcement would be to keep those real simulation invocations out of CI while retaining finite correctness/companion coverage; that is report-only here. No mapping, registry, workflow, mirror, adapter, test or code change is made, and no broad redesign is proposed. Their determinism, shortened timing or paused comparison stage does not erase the actual live simulation.

The long two Hspec occupant scenarios above also show why the existing all-lane/full-suite command cannot be blanket-labelled finite correctness. This is a classification observation, not a second CI audit; the requested enforcement census is restricted to the 15 behavior probes.

### Separate generic-harness observations (report only)

**(a) Actual CI simulation leakage.** The three behavior-probe invocations
identified above advance real game state in CI. Finite direct API/codec
assertions in the other twelve entries are distinct, even when their fixture
starts an engine. These are source classifications, not fresh timing results.
The simulation exclusion is intended policy; current CI eligibility has not
been changed by this documentation task.

**(b) Active consumer-specific skill/routing references.** Quruntul's installed
Codex skills are symlinks to its tracked `skills/` directories. At source
revision `aea8e068bc83049cd65ccbffea33f5788deb1a6a`:
`skills/test/SKILL.md:18–29`, `skills/flake/SKILL.md:30–38` and
`skills/deflake/SKILL.md:17–27` retain Synarchy-specific legacy fallbacks
behind their adapter-first route; those fallbacks do not run for Synarchy's
existing adapter. `skills/assess-tests/SKILL.md:19–26` actively names a
Synarchy legacy-assessment side route even when an adapter exists;
`skills/playtest/SKILL.md:19–25` selects Synarchy's own harness before its
adapter capability route. The legacy coordinator also retains
`require_synarchy_probe_runner` and `require_synarchy_probe_timeout`
(`skills/test/scripts/test_coordinator.py:248–304`), which test for
`synarchy.cabal` and encode consumer runner/timeout rules. These are concrete
remaining consumer couplings in skill routing/legacy tooling, distinct from
(a). Quruntul's [design §Legacy repositories](https://github.com/coghex/quruntul/blob/aea8e068bc83049cd65ccbffea33f5788deb1a6a/docs/design.md#legacy-repositories)
acknowledges this gap and calls for a separately requested code change.
No skill, coordinator or engine change is made here, and this report does not
inspect the private registry or authorize its drain/import.

**(c) Questions existing metadata already answers.** `Suite.kind` already
validates `ci` versus `probe` (`quruntul/adapter.py:25–50`); the latter means
local only, never CI. `$test` automatic selection already excludes CI suites
(`quruntul/select.py:57–65`), while `$flake` can measure both kinds locally;
kind is not a universal simulation detector. Synarchy owns the mapping from
`CI_ELIGIBLE` to kind, census omission/deferral and its per-suite budgets in
`.quruntul/adapter.py:116–164`. Generic engine selection honors consumer
ledger deferrals (`quruntul/select.py:10–13`); an explicit shakedown target
is checked against deferrals and otherwise restricts the suite list
(`quruntul/lab.py:599–630`). Explicit `--target` and consumer-owned deferrals
already bound the available operations; no new engine field is proposed.
This task does not alter metadata or issue operational deferrals.

## Historical consumer contract and rationale

The sections below preserve the 2026-09-30 to 2026-10-02 consumer design,
including the original QS-3 to QS-6 specifics. Dated counts are historical;
2026-10-08 evidence and the owner scope above take precedence. References to
Quruntul D-10/Q-6 are historical consumer choices, not current harness
requirements. Any operational imperative below for an unfinished campaign
is administratively DEFERRED. Targeted finite correctness acceptance remains
mandatory for a separately authorized change touching that contract.

## Identifiers and authorities

- **Slice IDs** `QS-3` to `QS-6` are the stable IDs from the quruntul design.
  They are not renumbered here, and QS-1 and QS-2 are quruntul slices that do
  not appear in this ledger.
- **Decisions and questions** of this document are `D-N` and `Q-N`. The
  quruntul design's are always written qualified, as "quruntul D-N", meaning
  the [archived `docs/designs/shakedown_synarchy_design.md`](../history/quruntul_shakedown_synarchy_design.md) from coghex/quruntul.
- **External prerequisites** are named by canonical identity: an issue or
  pull request as `coghex/<repo>#N`, a commit by full repository and SHA. A
  bare `#N` in this document means a coghex/synarchy issue. The local
  tracking epic is #2777; the filed children are #2785, #2786, #2787 and #2788.
- **Authority for the engine contracts.** This document summarises the
  quruntul contracts only for orientation. When they disagree, the
  authorities win, and a solver re-reads them, never this summary:
  - For the legacy-history import: the body of
    [coghex/quruntul#8](https://github.com/coghex/quruntul/issues/8), its
    canonical review amendments (the `issue-review:v2` comment of
    2026-09-30, verdict APPROVE), and `docs/design.md` in coghex/quruntul at
    `0269675` (the commit that merged coghex/quruntul#8 through
    coghex/quruntul#9) or later.
  - For the shakedown lane: coghex/quruntul#6 as merged by
    coghex/quruntul#7 at `b2e2a52a35b7de8a591b718d720c4fe941b1c7f6`, and
    quruntul's `docs/design.md` at that commit or later.
- **Evidence figures** below are dated snapshots, supporting context only.
  They are not acceptance truth: a solver re-reads the current registry,
  ledger, adapter and upstream revision before relying on any count. The
  `docs-wip` worktree is not upstream: operational evidence pins the real
  upstream revision it ran at, and the census provenance it used.

## Historical epic contract (unfinished campaign acceptance DEFERRED)

- **Goal:** Synarchy's side of coghex/quruntul#5 is delivered:
  - its adapter launches every applicable suite cleanly under a shakedown;
  - its quruntul ledger holds the legacy `$test` history's closed records;
  - it has a first flake measurement whose coverage is reconciled against
    the suites the adapter declares, with every observation the onboarding
    raised assessed.
- **Done when:**
  - QS-3, QS-4, QS-5 and QS-6 each reach their completion signal under the
    delivery lifecycle below. Each is recorded on its own coghex/synarchy
    issue, owned by the local tracking epic (D-2), and closed by the owner
    once verified (D-3);
  - coghex/quruntul#5 references those issues through the handoff sequence
    in Q-4.
- **Users and operators:** the owner, and the Codex and Claude agents that
  run `$flake`, `$test`, `$deflake` and `$assess-tests` in Synarchy.
- **Arc label:** None proposed.

## Current state and evidence

Snapshots taken on 2026-09-30 (the ledger at 14:52Z). Re-read everything
before relying on it.

- **Upstream.** `origin/master` was at `9a274311de75`, and this document's
  `docs-wip` worktree at `313657b59b9d`.
- **Adapter.** `.quruntul/adapter.py` (coghex/synarchy#2760, merged) declares
  102 suites:
  - `synarchy-test-headless`: CI, Hspec, `batch_tests = 400` (`HSPEC_SLICE`);
  - `synarchy-test-graphical`: a probe and a desktop suite; CI only compiles
    it, and it needs a display;
  - every registered probe the census does not defer, as a `command` or
    `exit` suite. `audio_manual` is left out (`DIRECT_ONLY`), because it needs
    a mode flag on a direct invocation.

  It has `flake_trials = 10`, `refresh_days = 7` and a `seed` hook fed by
  `docs/probe_census.json`. Its checks are `python3 .quruntul/checks.py`,
  which cover suites, results and seeding, not an import hook, which does not
  exist yet.
- **Ledger** (`<git-common-dir>/quruntul/ledger.sqlite3`):
  - 102 suite rows, of which 4 had tests enumerated;
  - 10,538 tests: 10,528 `new` and 10 `stable`, the latter seeded from the
    census;
  - 9 flake runs (8 complete, 1 blocked), and 1 open observation;
  - no `$test`-lane runs.
- **Legacy registry** (`<git-common-dir>/codex-test/`, schema
  `codex-test-coordinator/v1`, `updated_at` 2026-09-30T09:06:29Z):
  - 359 completed coordinator runs over 127 targets. Each has a report, a
    log and a completion time; execution status is `passed`, `failed` or
    `cancelled`.
  - 309 runs (85 targets) name a current suite. 42 targets name none.
  - Its assessment registry lists 15 assessments, all `completed`, with
    source-approval fields.
  - 4 proposals: 1 `rejected`, and 3 `accepted`. The proposal-design registry
    has a `completed` design naming each accepted proposal's id.
  - 633 artifact files (145 MB).
  - The number of observations not yet assessed was not computed.

  A recent `updated_at` shows neither current use nor that the store is
  drained.
- **Engine.**
  - The shakedown lane exists: coghex/quruntul#6, merged by
    coghex/quruntul#7.
  - The legacy-history import is merged (coghex/quruntul#8, merged by
    coghex/quruntul#9 on 2026-10-01 at `0269675`). Its merged hook shape and
    import-report schema are facts for QS-6 to read from the merged contract.
  - quruntul is at engine version 0.4.0 (re-read 2026-10-02).
- **Related Synarchy work.** coghex/synarchy#2743 measures real Linux CI
  runs to partition CI lanes. It is not QS-4, which sizes local quruntul
  batches; its figures come from another platform and runner.
- **Umbrella overlap check** (2026-09-30, read-only, coghex/synarchy, run at
  readiness). This was not an exhaustive search.
  - **Scope:** open issues labelled `epic`, of which 13 were listed, plus
    searches across all states for "quruntul", "shakedown", "flake lab
    onboarding", "flake ledger", "codex-test registry" and "legacy test
    history", each capped at 6 results.
  - **No results:** "quruntul", "shakedown" and "flake lab onboarding".
  - **Unrelated matches:** "flake ledger" matched closed probe-runner and
    atlas work (#1571, #1436, #1570, #2168, #2130, #1256). The other two
    searches matched open scenario, save, fluid, structure and flora work
    (#2700, #2649, #2548, #2717, #2721, #2735, #2722, #2519, #2707, #2547,
    #2557).
  - **Closest open epic:** #2742, which brings pull-request CI under twenty
    minutes. #2743 above belongs to that CI lane-partition effort, as the
    first independent review of this document found.

  None tracks quruntul onboarding, so the local tracking epic (D-2) duplicates
  no Synarchy epic. Its umbrella is the explicitly linked coghex/quruntul#5.
  Final per-child deduplication remains each processing run's job.

## Historical desired experience (campaign stages DEFERRED)

The owner asks for each slice in turn:
- QS-6 brings the legacy `$test` history into the ledger once, after the old
  registry is drained;
- QS-3 shows every suite launching cleanly, with each failure explained and
  owned;
- QS-4 presents the measured cost of seeding the headless suite and waits for
  the owner's choice;
- QS-5 seeds the ledger, reconciles what was and wasn't measured, and ends
  with every onboarding observation assessed.

## Historical scope (superseded by the owner scope above)

### In scope

- Synarchy's adapter changes under `.quruntul/` needed for these slices, with
  their checks.
- Running quruntul's shakedown, import and flake lanes against Synarchy, when
  the owner asks.
- Assessing the observations those runs raise.

### Out of scope

- Any generic engine change: the shakedown lane, the import, a per-suite
  trial count, or anything else in `quruntul/`. Engine defects found here are
  reported to coghex/quruntul.
- Candidate-revision shakedowns or imports: both run at the upstream head
  only (quruntul D-13; coghex/quruntul#8 review).
- Product fixes and flaky-test fixes. They go through `$assess-tests` issues
  and `$deflake`, each separately.
- New Synarchy features, the `codex-profile` lab, and `$playtest`.
- Editing quruntul's skills. If the legacy `$assess-tests` route needs a
  follow-up once the import is complete (see QS-6), that is coghex/quruntul
  work.

## Design

This is orientation only; the authorities above govern.

### Delivery lifecycle

QS-6 changes code, as does QS-3 when it finds adapter defects to repair.
Both then need a real operation that can only run once that code is
upstream. The shakedown runs only at the upstream head
(quruntul D-13), and the import resolves adapter declarations from one pinned
upstream revision (coghex/quruntul#8 review). QS-4 and QS-5 may change no
code at all. Each slice therefore has two stages:

- **Pre-merge**, in the slice's pull request:
  - the adapter change;
  - `python3 .quruntul/checks.py`, plus the adapter's own fixtures, such as
    temporary-history fixtures for QS-6;
  - focused evidence where the lanes support it;
  - code-like, test-read or agent-executed documentation only.

  Ordinary documentation, including measurement notes, mappings, verdicts and
  owner choices, lands with `docs-push` and is linked from any code PR. This
  current repository rule supersedes the older documentation-in-PR wording.
- **Upstream-only**, after the merge: the real operation at the upstream head
  (the shakedown, the import, or the flake seed), with its evidence recorded
  on the slice's issue.

Each issue stays open until its upstream-only evidence is verified; its pull
request references it without closing it (D-3). No slice's pull request
claims an operation it cannot yet have run.

### Durable evidence

A slice's durable record is a short summary: the upstream revision, the
platform, the commands, the quruntul run and observation ids, and the
outcome. It cites the local lab evidence, and never commits the SQLite
ledger or copies a run directory into the repository.

### QS-6: importing the legacy registry

- The adapter implements coghex/quruntul#8's hook to read the `codex-test`
  registry. It supplies records with stable, namespaced identities: source
  store, record kind and record id, independent of machine paths.
- **Evidence.**
  - Every run's report, and every run log and assessment document that
    exists, is declared `copy`.
  - A log or document that never existed is declared absent explicitly. An
    existing file is never declared absent.
  - A promised file that is missing refuses the whole import.
  - Artifacts are declared `copy` or `reference` file by file.
  - Unmatched targets are archived, with no alias remapping and no
    freshness.
- **Adapter-facing consequences of coghex/quruntul#8.** These are already in
  its approved contract; they are listed so the adapter's fixtures cover
  them, not as new engine work.
  - A `completed` legacy assessment is imported as approved only with actual
    source approval. An assessment or proposal Markdown file is not, by
    itself, an approved assessment.
  - A legacy `accepted` proposal is normalized to a decided status only when
    a `completed` design names its proposal id. The raw proposal still reads
    `accepted`.
  - The adapter documents how every legacy status maps to terminal and
    approved.
  - Identity and content are stable across repeated calls and machine paths.
    Engine-generated timestamps, local destinations and later suite matching
    are not source-content changes, and identical records keep their
    original attachment and identity snapshot.
  - Identical records already imported, proposals included, are no-ops on a
    repeated import. A proposal-target conflict arises only between distinct
    proposal identities sharing a target: a genuinely new proposal against
    the ledger, or two within the supplied batch. It refuses the import, as
    do missing mandatory evidence declarations and changed source content or
    manifests under an imported identity. None of these is ever skipped.
  - The import prepares, enumerates, seeds and executes nothing, and
    recovers no unrelated native run. The durable ledger commit is its
    success boundary: leftovers from before the commit stay invisible to
    readers and export. A refused or pre-commit interrupted import leaves no
    durable schema migration behind. An interruption after the durable
    commit preserves the complete import and its committed migration.
- **Portability.** A run with a `reference` is not self-contained. Copied
  assessment Markdown may still link to absolute external paths, and copying
  a file doesn't copy what it links to. The import discloses those external
  dependencies; nothing copies them recursively.
- **When it runs.** The import runs only through quruntul's command, only
  after coghex/quruntul#8 has merged and the hook is upstream, and only once
  the registry meets the drain bar (quruntul D-8). Just before importing, the
  solver checks for source changes and active coordinator claims. The
  registry is never modified.
- **After it completes.** quruntul D-5 says `$assess-tests` keeps its legacy
  Synarchy route until the import is done. QS-6 records that the import is
  complete, and then determines whether the skill's routing needs any
  change. Its legacy path already depends on there being unassessed legacy
  observations. Any change needed goes to coghex/quruntul, not this pull
  request.

### QS-3: shaking down every suite

A shakedown of every applicable Synarchy suite at the upstream head. It is
advisory, and it leaves test and suite state unchanged while recording its
own run, report and observations (quruntul D-3, D-12, D-13). It includes
desktop suites under the carried standing lab approval, one window-opening
suite at a time (quruntul D-7; see Q-2).

- **Clean** means the suite built, every listed test reported, and none
  failed (quruntul D-7). Hspec `pending` and command `unproven` results in a
  clean suite are recorded as reported, not as passed.
- **Reconciliation.** Every suite the adapter declares at the pinned revision
  is reconciled against the report, keeping every problem the engine
  records:
  - build failed;
  - enumeration failed;
  - failed tests;
  - unreported results;
  - incomplete execution;
  - never run: `busy` or `not-run`.

  A `busy` or `not-run` suite is incomplete work, not coverage. Declared
  exclusions stay disclosed exclusions: the engine's `skipped` results, with
  their reasons (platform, or a ledger deferral made with `quruntul defer`),
  a census deferral, or `DIRECT_ONLY`. A later attempt may measure unreached suites, but it
  never rewrites finished evidence or replays a retained trial.
- **Dispositions** for each non-clean suite:
  - an adapter defect, repaired in the QS-3 pull request with evidence;
  - an engine defect, reported to coghex/quruntul;
  - a product failure, assessed and filed separately;
  - a verified environment blocker.

  An engine or environment defect that prevents trustworthy measurement
  carries a checkable follow-up or resume condition. Filing it doesn't show
  that the suite launched correctly.

### QS-4: sizing the headless suite

- **Order.** QS-4 keeps the approved order: first one measured headless
  trial, then the projection and the owner's choice, and only then a
  complete validating batch at the chosen count. It is not an open-ended
  profiling campaign.
- **Measure, don't extrapolate.** Per-process start-up and world-generation
  fixtures can dominate a small selection, so one whole-suite trial is not a
  linear per-example cost.
- **What it records:**
  - the upstream revision, toolchain and platform;
  - the commands and quruntul run ids;
  - build and enumeration time, kept apart from trial time;
  - selected example counts;
  - the slice estimate and its uncertainty.
- **The batch model.** `batch_seconds` bounds the trial loop, which starts
  another trial only when a full `trial_seconds` still fits; preparation and
  enumeration come earlier.
- **The projection** of seeding at 10 trials includes the slice count and the
  start-up cost every trial repeats.
- **Stop point.** QS-4 then stops for the owner's choice between 10 trials and
  a per-suite reduction (quruntul D-10). It never lowers the global
  `flake_trials`, and never substitutes the existing per-command trial
  override for that decision.
- **Validation** uses a complete batch at the chosen count, with every
  planned trial and selected result accounted for. A short elapsed time from
  a batch that stopped early doesn't show the batch fits.
- **Existing figures,** such as coghex/synarchy#2743's CI measurements, are
  cited with their platform and runner differences, never treated as local
  measurements. Its lane-partition work is not repeated.

### QS-5: seeding the ledger

QS-5 runs `$flake` to `no-candidate` and `$assess-tests` on what it raised,
only after QS-3, QS-4 and the owner's choice. No fixes are bundled.

`no-candidate` alone doesn't certify completion. Flake selection skips
deferred, platform-inapplicable, suite-claimed and desktop-claimed suites
before considering them, and still reports `no-candidate`. So QS-5 adds a
coverage reconciliation:
- the pinned revision, adapter identity and platform;
- the declared suites and their applicable tests;
- measurements, or census evidence inherited through the `seed` hook;
- intentional exclusions, with their reasons and resume conditions;
- any work remaining `new`.

A transient `busy` or claim is never completion. `pending`, `failing` and
`flaky` stay distinct from `stable`. An approved assessment doesn't mean a
product fix landed.

## Decisions

### D-1. Synarchy's quruntul slices are processed from this document

The owner chose this on 2026-09-30, while processing quruntul QS-6: the
recommended option, "track the Synarchy slices in Synarchy's own design". It
implements quruntul D-6 and neither reverses nor extends it: QS-3 to QS-6
are coghex/synarchy issues, and coghex/quruntul#5 references them. Rejected:
- filing them in coghex/quruntul, which reverses D-6;
- hand-filing them in Synarchy outside the transaction helpers, which gives
  up recording that survives a crash.

On 2026-10-02 the owner had the four slices removed from the quruntul
design's ledger and delivery plan, leaving a pointer to this document, rather
than recording them there as `[no-issue]` (Q-4).

### D-2. A small Synarchy tracking epic owns the four children

The owner chose this on 2026-09-30 ("a tracking epic is fine"; resolves Q-1).
This document's EPIC entry becomes a small coghex/synarchy tracking epic,
filed through the unchanged `/process-design-doc` workflow. It owns exactly
the four local children, QS-6, QS-3, QS-4 and QS-5, and links
coghex/quruntul#5 as the umbrella for the whole arc. It claims no second
product arc and adds no scope; its checklist lists only the four children.
Rejected: marking EPIC `[no-issue]`, which would need a bounded exception to
the installed processing workflow.

### D-3. Issues stay open until their post-merge evidence is verified

The owner chose this on 2026-09-30 ("issues can stay open until post merge is
verified"; resolves Q-3). This is the bounded post-merge completion gate.
- **The pull request** carries everything required before merge: the code,
  the adapter checks and fixtures, and code-like documentation only.
  Ordinary documentation and evidence notes land with `docs-push` and are
  linked from the PR under current repository instructions. It references its issue without a closing keyword, so merging
  doesn't close the issue.
- **After the merge,** the solver runs the slice's upstream-only operation at
  the upstream head and records its durable summary on the issue, naming
  the evidence.
- **The owner** closes the issue once that evidence is verified.
- **A slice that changes no code,** such as QS-5, or QS-3 or QS-4 when no
  adapter change is needed, uses the same gate with no pull request: its
  evidence is recorded on the issue, and the owner closes it.

Rejected:
- splitting each post-merge operation into its own operational issue;
- landing the evidence as a standalone docs pull request, which would be an
  exception to Synarchy's docs landing lane.

Execution note: a workflow that adds a closing reference by default must use
a non-closing reference for these issues instead.

### Carried from the quruntul design

These bind the slices here as decided there; the full text is in that
document:

| Decision | What it settles |
|---|---|
| D-2 | The legacy data to port is the `codex-test` registry. |
| D-3 | A shakedown is advisory and never gates `$flake`. |
| D-4 | Imported runs count toward `$test` freshness, at the suite's current identity. |
| D-5 | Drain the legacy open items first, then import only closed history; `$assess-tests` keeps its legacy route until the import is done. |
| D-6 | Synarchy's slices are tracked in coghex/synarchy. |
| D-7 | A shakedown runs every applicable suite, desktop suites included under the standing lab approval, one window-opening suite at a time; clean means built, every listed test reported, none failed. |
| D-8 | "Drained" means every observation assessed and every proposal rejected, designed or implemented. |
| D-9 | Every unmatched registry target is archived. |
| D-10 | The headless suite's trial count is chosen by the owner after QS-4 measures. |
| D-12 | A shakedown leaves test and suite state unchanged, and records only its own run, report and observations. |
| D-13 | A shakedown runs only at the upstream head; adapter fixes are confirmed by a shakedown after they merge. |
| D-14 to D-18 | The import's explicit command, named copy-or-reference evidence, all-or-nothing idempotence and conflicts, equal-time conflict, and disclosure of references. |

For the import, D-14 to D-18 are superseded wherever coghex/quruntul#8's body
or its review amendments say more.

## Open questions

### Q-1. Which tracking mechanism does this document use?

Resolved by D-2: a small Synarchy tracking epic that owns the four children
and links coghex/quruntul#5. The installed `/process-design-doc` workflow
selects children only after a local `[#N]` EPIC exists, and its transaction
helper cannot adopt coghex/quruntul#5.

### Q-2. Which approval covers desktop suites?

Resolved as carried: quruntul D-7's standing lab desktop approval covers
Synarchy's applicable desktop suites, `synarchy-test-graphical` among them,
in shakedowns and flake batches. Its scope:
- the consent the adapter supplies;
- the lab's `desktop` claim;
- advance notice that windows will appear;
- one window-opening suite at a time.

It covers the adapter's direct launch of the Hspec executable. It is not
general permission for an ordinary game launch, which opens a window:
Synarchy's launch rules forbid that apart from sprite preview. The game's
`--dump`, `--headless` and `--offscreen` modes stay permitted under those
same rules, and the probes these slices run use them. Running any of it
still needs the owner's request for that slice.

If a suite turns out to launch something outside this scope, the slice stops
and asks about that specific command.

### Q-3. How does a slice complete when its outcome is operational?

Resolved by D-3: each issue stays open until its upstream-only evidence is
verified. Its pull request references the issue without closing it and
carries all pre-merge evidence, and the owner closes the issue.

### Q-4. What is the cross-repository handoff sequence?

Each repository's transactions are local: creating an issue in Synarchy
doesn't update coghex/quruntul#5 or the quruntul design. The sequence, as it
stands on 2026-10-02:
1. The owner approved this document as ready.
2. The local tracking epic (D-2), coghex/synarchy#2777, is filed and confirmed.
   Each Synarchy child is filed and confirmed here, one approved artifact at a
   time.
3. The quruntul design's four ledger lines for these slices were deleted by the
   owner's choice on 2026-10-02, with a pointer to this document. That replaces
   the earlier proposal to mark them `[no-issue]: tracked in
   coghex/synarchy#N`. The quruntul design's own record of the move is its
   pointer note.
4. As a separate approved step in coghex/quruntul, coghex/quruntul#5's
   checklist names each Synarchy child's full URL once it exists.

A partially completed cross-reference is reconciled before either cursor
advances. Neither tracker's labels nor either durable cursor is ever
hand-edited as a shortcut. Filing here never changes coghex/quruntul#5 by
itself.

## Verification strategy

- **Adapter changes:** `python3 .quruntul/checks.py`, the adapter's own
  fixtures, and the tool self-tests in the Testing tiers table of `AGENTS.md`
  that the change touches.
- **Lab runs:** each slice's durable summary, citing quruntul run and
  observation ids at a pinned upstream revision:
  - QS-3: the shakedown reconciliation;
  - QS-4: the measurement and projection;
  - QS-5: the coverage reconciliation and approved assessments.
- **The import:** its receipt, reconciled by record kind against the eligible
  source history at import time rather than against the snapshot above.

## Historical delivery plan (unfinished campaign stages DEFERRED)

### QS-6. Read Synarchy's `codex-test` registry through the adapter's legacy-history hook

> Tracked by #2785. Under laz `6bed6ihh`, the hook, import, drain and
> idempotence stages below are optional consumer onboarding, administratively
> DEFERRED, not an adoption prerequisite or completed acceptance. Finite
> adapter/hook fixtures become mandatory only for a future authorized change
> that implements or touches the hook.

- **Outcome:** Synarchy's ledger holds the legacy registry's closed history,
  imported once through quruntul's command with the adapter's hook.
- **Scope:**
  - the adapter's hook and its fixtures and checks;
  - documenting the legacy-status mapping;
  - the engine-version requirement;
  - after the merge, verifying the drain bar, running the import, and
    recording its durable summary.
- **Phase:** 2 (after quruntul's engine work)
- **Depends on:**
  - coghex/quruntul#8 merged; its merged contract is the authority;
  - for the upstream-only stage, this slice's hook upstream, and the legacy
    registry drained to quruntul D-8's bar, re-verified against the legacy
    coordinators just before importing. That means every legacy observation
    assessed through the legacy `$assess-tests` path, and every proposal
    rejected, designed or implemented.

  No local slice.
- **Ordering:** not on the critical path; independent of QS-3, QS-4 and QS-5
  (flake selection reads no `$test` history).
- **Relevant decisions:** D-1, D-3; quruntul D-2, D-4, D-5, D-8, D-9, D-14 to D-18,
  as superseded by coghex/quruntul#8.
- **Acceptance signals:**
  - **Pre-merge:**
    - `python3 .quruntul/checks.py` passes;
    - hook fixtures cover: normalized terminal statuses; source approval;
      the proposal-to-design join; stable identities; manifest completeness,
      including explicit absence; unmatched targets archived; and refusal of
      open or inconsistent records.
  - **Upstream-only**, the completion signal:
    - a successful import whose imported and archived records reconcile, kind
      by kind, with the eligible source history;
    - a second import that changes nothing;
    - freshness for matched suites as quruntul D-4 and D-17 specify;
    - the drain verification recorded;
    - portability limits disclosed: references, and external links in copied
      files;
    - imported history outside every open queue.
  - A refused import is a blocker with its reason, not a partial success.
- **Out of scope:** draining the registry, which precedes the import; any
  engine change; editing quruntul's skills; `codex-profile`.
- **Open questions:** None. Stop and report if coghex/quruntul#8's merged
  contract cannot represent something the registry holds.

### QS-3. Shake down every Synarchy suite and repair its adapter

> Tracked by #2786. Further full shakedowns and onboarding assessment/completion
> are administratively **DEFERRED**; completed run/repair evidence is retained above.

- **Outcome:** a shakedown of every applicable Synarchy suite at the upstream
  head is clean, or each non-clean suite has a recorded disposition, with
  every declared suite reconciled.
- **Scope:**
  - running quruntul's shakedown at the owner's request. It is a full-suite
    run, which Synarchy's "no full suites by default" rule otherwise
    forbids.
  - evidence-backed adapter repairs;
  - dispositions as in the design above;
  - confirming repairs by a shakedown after they merge (quruntul D-13).
- **Phase:** 2
- **Depends on:** coghex/quruntul#6, merged by coghex/quruntul#7 at
  `b2e2a52a35b7de8a591b718d720c4fe941b1c7f6`, which is already satisfied. No
  local slice.
- **Ordering:** critical path; can land first (it needs nothing from QS-6).
- **Relevant decisions:** D-1, D-3; quruntul D-3, D-6, D-7, D-12, D-13.
- **Acceptance signals:**
  - **Pre-merge:** for any repair, `python3 .quruntul/checks.py` and focused
    evidence.
  - **Upstream-only:** a shakedown reconciliation of every declared suite,
    either clean or dispositioned, that keeps each problem kind separate and
    lists `busy` and `not-run` suites as incomplete. `pending` and `unproven`
    are recorded as not passed, and exclusions are disclosed.
- **Out of scope:** product and flaky-test fixes; engine defects, which are
  reported to coghex/quruntul; gating `$flake`; candidate-revision
  shakedowns.
- **Open questions:** None.

### QS-4. Size `synarchy-test-headless`'s flake slices from a measured trial

> Tracked by #2787. One measurement and projection completed; subsequent owner
> tuning/reduction choice, campaign adapter PR and validating batch **DEFERRED**.
> Neither trial count was selected. The historical Stop point is deferred.

- **Outcome:**
  - `batch_tests` for `synarchy-test-headless` is set from one measured
    trial;
  - the projected seeding time at 10 trials is reported;
  - the owner's trial-count choice is recorded;
  - a complete batch at the chosen count validates the setting.
- **Scope:** the measurement as the design above describes, the adapter's
  slice size, the recorded choice, and the validating batch.
- **Phase:** 2
- **Depends on:** QS-3
- **Ordering:** critical path
- **Relevant decisions:** D-3; quruntul D-10
- **Acceptance signals:**
  - the recorded measurement and projection, separating build and
    enumeration time from trial time and stating the slice estimate with its
    uncertainty;
  - the owner's choice, recorded;
  - a complete batch at that count, with every planned trial and selected
    result accounted for, finishing within `batch_seconds`.
- **Stop point:** after the projection, stop and ask the owner to choose
  between 10 trials and a per-suite reduction. Choose neither. A reduction is
  a separate coghex/quruntul engine issue, and QS-5 waits until it is
  identified, merged, and usable by the Synarchy adapter.
- **Out of scope:**
  - splitting the Hspec executable;
  - any engine change;
  - lowering the global `flake_trials`;
  - repeating coghex/synarchy#2743's CI lane-partition work.
- **Open questions:** quruntul Q-6 (the trial count), deliberately open until
  the stop point above.

### QS-5. Seed Synarchy's flake ledger

> Tracked by #2788. Full flake seeding, coverage-completion and onboarding
> assessments administratively **DEFERRED**, not passed.

- **Outcome:** `$flake` reports `no-candidate` for Synarchy on this platform.
  The coverage reconciliation shows every declared suite either measured,
  seeded from the census, or intentionally excluded with a reason and resume
  condition. Every observation from the onboarding runs has an approved
  assessment.
- **Scope:** running `$flake` to completion at the owner's request, the
  coverage reconciliation, and `$assess-tests` on the observations of the
  onboarding runs.
- **Phase:** 3
- **Depends on:** QS-3, QS-4, and QS-4's recorded owner choice (with any
  engine issue it requires merged in coghex/quruntul).
- **Ordering:** critical path; last
- **Relevant decisions:** D-3; quruntul D-3, D-7, D-10
- **Acceptance signals:**
  - `no-candidate` together with the coverage reconciliation. A `busy` or
    claimed suite never counts as complete.
  - `pending`, `failing` and `flaky` reported apart from `stable`.
  - Approved assessments for every observation raised by the onboarding
    runs. That means the QS-3 shakedown runs, any QS-4 measurement or
    validation runs that emitted observations, and the QS-5 flake runs,
    listed by run id. It is a finite set, not a growing global queue, and no
    product or flaky-test fix is required before an assessment closes.
- **Out of scope:** product fixes, and flaky-test fixes (`$deflake`).
- **Open questions:** None.

## Delivery constraints

These come from Synarchy's `AGENTS.md`; re-read it before working.

- **Worktrees and docs.**
  - Implement in an isolated worktree, with one issue per pull request.
  - Keep the primary checkout clean.
  - Ordinary documentation and evidence summaries land with `docs-push`,
    including those accompanying code; link their path and commit from the PR.
    Markdown read by tests or executed by agents stays with code. A docs-only
    push is explicitly owner-authorized for this scope change; dry run first.
- **Build lock.** Wait for the `cabal-build` lock in a foreground 60-second
  wake loop for up to 30 minutes. Never bypass the lock or stop another
  owner's process.
- **Launch rules.**
  - Never launch the game normally, meaning an ordinary windowed launch.
    Sprite preview is the only exception; `--dump`, `--headless` and
    `--offscreen` remain allowed.
  - Desktop suites run only under the carried lab approval (Q-2).
  - Use non-8008 ports, and never `pkill -f synarchy`.
- **Full suites.** A shakedown or a flake seed of Synarchy is a full-suite
  run: only on the owner's explicit request for that slice.
- **Concurrency.** Respect quruntul's claims, resource holds, desktop
  consent, the legacy coordinators' claims, and other agents' active work.
  Stop only processes you started.
- **Evidence.**
  - Record a short durable summary citing quruntul run and observation ids
    at a pinned upstream revision.
  - Never commit the SQLite ledger or copy its run directories into the
    repository.
  - Never modify the legacy registry.
