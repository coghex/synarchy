# Project Review Findings: PRs #2543–#2119

Completed the paused 17th batch in `coghex/synarchy`: PRs #2543, #2143,
#2139, #2137, #2136, #2134, #2127, #2132, #2123, #2122, #2120, and #2119.
These are the exact twelve selected before the pause, not every PR in the
numeric interval. Reviewed their linked requirements where present,
individual commit messages, landed first-parent patches, and current
implementations and consumers. The landing intervals contained no additional
direct commits requiring review. The previously excluded #2377 concern was
not revisited. This batch ends here; the older stop remains PR #1423.

Final source verification used `e004672ae4986f77cad61e92c744bfa1545e411b`.
Earlier focused headless evidence below was obtained before the pause, at
the then-built revision; it is not presented as a fresh build of this HEAD.
No implementation, artwork, tracker issue, or PR was changed.

## Review coverage and validation

| PR | Reviewed boundary and result |
| --- | --- |
| #2543 / #2308 | Save-queue ownership comments checked against the two thread consumers and barrier lifetime; no new finding. |
| #2143 / #1925 | Quiet fixture initialization and migrated call sites; the later Chop fixture bypasses the contract, captured below. |
| #2139 / #2095 | Eight-process location-content orchestration, extracted scenario owners, results, configuration isolation, and cleanup; no new finding. |
| #2137 / #2080 | Four-facing schema, independent legacy/path and lifecycle axes, loader handles, runtime lifecycle, Lua consumers, preview decoding, eight shipped declarations, and acceptance fixtures; no new finding. Later numeric validation work addressed the historical animation-rate gap. |
| #2136 / #1787 | All 13 wheat PNGs inspected at native resolution, with dimensions, alpha, baseline, and cultivated footprint checks against the approved asset scope; no new finding. This PR does not claim gameplay integration. |
| #2134 / #2049 | Save-compatibility owner extraction, CLI defaults, codec ownership, registration and generation rollback; exception-path finding below. |
| #2127 / #2057 | Enum-audit scanner/policy/command extraction, stable facade, relocation attribution, and mutation coverage; no new finding. |
| #2132, standalone | Meal-waste probe protocol and its six behavioral checks; no new finding. |
| #2123 / #2021 | Opaque allocation, page ownership, save metadata, versioned codecs, legacy staging, deterministic fixture IDs, and real-engine identity tests; test filesystem finding below. |
| #2122 / #2097 | De-flake outcome/issue owners, facade defaults and exception identity, dispatch, retained self-tests, and documentation; no new finding. |
| #2120 / #1922 | Shared self-test reporting and converted callers; the already-reported literal `parse_verbose()` guard rejects the valid `parse_verbose(argv)` call and is not duplicated here. |
| #2119 / #2098 | Worldgen type-owner extraction, export compatibility, enum relocation, save audit discovery, and retained positional wire shapes; no new finding. |

Fresh Python checks passed: location-content 185 assertions; location config
isolation 86 assertions; de-flake outcome 36 tests / 674 assertions and issue
34 tests / 532 assertions; save-compatibility 96 selected self-tests / 362
assertions (`--without-reproducibility`); enum-audit self-tests and live audit;
save-compatibility audit (29 baselines / 35 fixtures); expensive-gate selector
self-tests; and CI parity audit. Mechanical comparisons checked the extracted
definitions and defaults: 56 enum-tool definitions, 40 de-flake definitions,
and 135 normalized worldgen declaration/conversion blocks, alongside source
review of their ownership changes. Location failure-record arguments were
also compared; that comparison alone is not proof of branch equivalence.

Before the pause, focused fixture-logging and persistence checks passed,
as did the meal-waste protocol checks. The quiet-logging counterexample
below was reproduced then. On resumption, `cabal build
synarchy-test-headless` failed while renaming `Input.o.tmp` to `Input.o`
because the temporary object was absent. Consequently no newly built
headless executable, fresh real-codec Hspec run, full eight-process location
probe, or GPU scenario is claimed for the final HEAD. No full CI was run.
The save-registration timeout counterexample was rerun successfully at the
final HEAD in a temporary directory.

The prior self-test guard finding remains in
`docs/project_review_2494-2313.md`. Later building-rate validation (#2347)
and frozen historical page/worldgen wire types (#2125 and subsequent
versioned fixtures) are accounted for, not recaptured as current defects.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] PRR-1. Route Chop authority initialization through the quiet fixture boundary — [#2648]
- [x] PRR-2. Restore save-fixture registration when codec validation raises — [#2649]
- [x] PRR-3. Isolate generated-world identity tests from caller-owned saves — [#2650]

## 1. Headless fixture logging

### [#2648] PRR-1. Route Chop authority initialization through the quiet fixture boundary

> **Captured note:** The quiet fixture contract established by PR #2143 is
> bypassed by the Chop authority setup added later in #2121. Its production
> initializer ignores the test logging selection and writes to stdout.

**Verification:** Before the pause, ran the built headless executable with
`ENGINE_DEBUG=event SYNARCHY_TEST_LOG=quiet`, matching
`Chop authority/designates exactly the named plants` and using
`--format=failed-examples`. The example passed, but stdout contained the
timestamped `DEBUG Event YamlNotifications` message
`Notification registry loaded: 12 categories`. Source reinspection at the
final HEAD confirms the same direct call and stdout backend remain.
The notification message is now Debug rather than Info, so this does not
claim that every ordinary run is noisy without an enabled category.

**Evidence:**

- `test-headless/Test/Headless/World/Chop/Authority.hs:30` — imports the production `initializeEngineHeadless` directly.
- `test-headless/Test/Headless/World/Chop/Authority.hs:444` — setup calls that initializer before the example runs.
- `src/Engine/Core/Init.hs:608` — the production initializer chooses `LogToHandle stdout`.
- `test-headless/Test/Headless/Harness/Log.hs:81` — names `SYNARCHY_TEST_LOG`; its resolver selects the fixture backend.
- `test-headless/Test/Headless/Harness/Log.hs:113` — the canonical quiet initializer supplies that backend before initialization can log.

**Handoff context:**

- **Current behavior:** This fixture bypasses explicit quiet selection, and also bypasses the diagnostic stderr override. Initialization diagnostics can contaminate stdout despite the suite's logging contract.
- **Expected behavior:** Ordinary Chop fixture setup obeys the same pre-initialization backend selection as the other headless fixtures, including explicit quiet and diagnostic stderr modes.
- **Scope and constraints:** A current integration regression against #1925/#2143, not a defect introduced by #2143's helper. Preserve production logging and deliberate logger-testing fixtures.
- **Verification target:** Run a narrowly matched Chop authority example with the Event debug category enabled under quiet and stderr modes; assert stdout suppression and the selected diagnostic destination. Retain the dedicated fixture-logging tests.
- **Deduplication:** Open/closed Chop/quiet/headless-logging searches returned the originating logging work (#1925/#1928 and related issues), not an owner for this later bypass. Existing local findings do not capture it.
- **Remaining uncertainty:** The executable reproduction predates the pause; final-HEAD confirmation is source-based because the fresh Haskell build failed. No unrelated initialization warning was induced.

## 2. Save compatibility tooling transactions

### [#2649] PRR-2. Restore save-fixture registration when codec validation raises

> **Captured note:** PR #2134 preserves rollback for a returned validation
> failure, but a real validation timeout raises after the new manifest is
> written and bypasses both manifest rollback and generation's outer cleanup.

**Verification:** At the final HEAD, used the production registration module
with a `TemporaryDirectory`-owned manifest. Wrote `{"old": true}`, then
patched only `register.subprocess.run` to raise
`subprocess.TimeoutExpired(['cabal', 'test'], 1800)` while calling
`_finalize_manifest_write({'new': True}, path, 'complete-session', False,
'fixture')`. The observed result was:

```text
TimeoutExpired escaped; prior manifest restored: False
retained manifest: {'new': True}
```

No tracked manifest or fixture was modified. This directly exercises the
production writer and cleanup branch; it does not wait thirty minutes or
claim a naturally occurring Cabal timeout. The outer fixture/summary impact
is established by the caller's control flow below.

**Evidence:**

- `tools/save_compat_audit_register.py:96` — real-codec validation uses a subprocess with `timeout=1800` and catches only `FileNotFoundError`.
- `tools/save_compat_audit_register.py:124` — finalization records prior contents and writes the replacement before calling validation.
- `tools/save_compat_audit_register.py:155` — an exception from validation exits before the returned-false rollback branch.
- `tools/save_compat_audit_generate.py:357` — its `GenerationError` handler covers the earlier generation call, not registration.
- `tools/save_compat_audit_generate.py:404` — registration is called directly; fixture/summary restoration runs only when it returns a nonzero code.

**Handoff context:**

- **Current behavior:** A codec-validation timeout leaves the replacement manifest registered despite unsuccessful validation. In generation mode, the exception also bypasses restoration of fixture and summary files already written during that operation.
- **Expected behavior:** Failed validation must not leave the operation's new registration published, and generation must restore its prior fixture/summary state when registration cannot complete. Report the actual failure; cancellation must not become success.
- **Scope and constraints:** This defect predates the extraction but remains in its current registration/generation boundary. It contradicts #2049's retained rollback requirements. Preserve the distinct codec invocations, normal nonzero-result diagnostics, and explicit `--skip-validation` behavior.
- **Verification target:** Inject validation timeout and other synchronous launch failures through the real registration path, for both existing and absent manifests. Cover generation with preexisting and newly created fixture/summary paths; assert exact restoration or removal as appropriate, and an unsuccessful outcome. Keep ordinary false-result rollback tests.
- **Deduplication:** Open/closed save-compatibility validation/rollback/timeout searches found antecedent #2049 and related work, but no specific owner for raised validation failures. Local project-review reports do not capture this transaction gap.
- **Remaining uncertainty:** Manifest non-restoration is directly reproduced; generation's additional file impact is source-verified rather than reproduced through a real engine. No production fixture corruption is asserted.

## 3. Generated-world identity test filesystem ownership

### [#2650] PRR-3. Isolate generated-world identity tests from caller-owned saves

> **Captured note:** PR #2123's engine-backed identity tests delete fixed
> `saves/` paths without entering an isolated resource root. A developer's
> existing slot at either test name is deleted before the test writes its
> own fixture, and a fixed legacy file is overwritten and removed.

**Verification:** Traced the final-HEAD executable entry point, Hspec wiring,
engine fixture, and all three save-writing paths. `main = hspec` supplies
no outer filesystem isolation; the identity spec is wrapped only in
`withHeadlessEngine`, whose setup initializes the engine and world worker
without changing the working directory. The tests then unconditionally call
`removePathForcibly` on `saves/gwid_spec_roundtrip` and
`saves/gwid_legacy_capture`. Both strings are permitted ordinary save names.
The legacy case writes `saves/gwid_legacy_v8_source.bin` with `BS.writeFile`
and removes that fixed path in its finalizer. There is no ownership check or
backup on these paths. No caller-owned save was inspected, created, or
deleted to demonstrate this directly evident control flow.

**Evidence:**

- `test-headless/Spec.hs:413` — plain Hspec entry point, without an enclosing isolated resource root.
- `test-headless/Spec.hs:538` — wires `GeneratedIdentity.spec` with only `aroundAll withHeadlessEngine`.
- `test-headless/Test/Headless/Harness.hs:226` — engine bracket setup does not isolate relative filesystem writes.
- `test-headless/Test/Headless/World/GeneratedIdentity.hs:435` — round-trip cleanup deletes the fixed slot; line 438 also deletes it before use, with its name defined at line 619.
- `test-headless/Test/Headless/World/GeneratedIdentity.hs:495` — legacy example finalizer removes the fixed file; line 509 overwrites it, and line 543 defines its caller-relative path.
- `test-headless/Test/Headless/World/GeneratedIdentity.hs:565` — snapshot capture selects a second fixed slot and deletes it before and after saving.
- `src/World/Save/Serialize.hs:77` — production saves use the same relative `saves` directory.
- `test-headless/Test/Headless/Harness/Isolation.hs:150` — the existing scratch-resource-root fixture changes directory before the wrapped action and owns separate config and saves directories.

**Handoff context:**

- **Current behavior:** Running these tests in a normal resource checkout can destroy preexisting data under the predictable test names. Independent test processes using the same resource root can also interfere with those fixtures.
- **Expected behavior:** Every identity-test save write and cleanup is confined to exclusively test-owned storage, with isolation established before engine initialization. Existing caller saves and configuration remain untouched on success and failure.
- **Scope and constraints:** Keep the real save/load-staging identity assertions and production save behavior. Address this spec's ownership boundary, not a broad test-suite rewrite. Do not demonstrate the defect against actual user saves.
- **Verification target:** In an outer, test-owned resource fixture, plant sentinel slot directories and the fixed legacy filename, invoke the identity spec through its real wrapper, and assert the sentinels remain byte-identical. Verify inner scratch cleanup on both normal completion and an injected failure, plus the existing identity assertions.
- **Deduplication:** Open/closed searches for GeneratedIdentity, generated-world identity isolation, headless save isolation, predictable save paths, and save-slot tests found the originating #2021 and unrelated probe isolation work. The earlier #2162 scratch-saves improvement is available but this spec never enters that fixture. No existing local findings report owns this specific gap.
- **Remaining uncertainty:** This is a source-proven destructive path conditional on a preexisting name collision, not a claim that a user's save has already been lost. Fresh engine execution was not attempted after the build failure.
