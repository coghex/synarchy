# CI test audit findings

This audit reviews the tests currently run by GitHub CI for correctness,
usefulness, coverage, and disproportionate cost. It focuses on the blocking
headless suite, Python audit checks, worldgen gate, and path-selected behavior
probes.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

Inspected `.github/workflows/ci.yml`, the CI selectors, the headless Hspec
harness/specs, and audit-tool self-tests. Reviewed recent completed Actions
runs:

- Headless Hspec: about 4m20–4m33.
- Selected behavior probes: about 6–7m.
- Save compatibility audit: about 2m08–2m23.
- Quick world check: about 1m36 on a master push.
- Other audit self-tests were seconds or less.

The headless suite was retained as deliberately useful: it uses one engine and
shares the canonical generated world across many readers rather than repeatedly
generating it.

## Status

- [x] CIT-1. Test-only PRs unnecessarily run the entire behavior-probe smoke bundle — [#1359]
- [x] CIT-2. Save-compatibility’s one GHCi self-test runs on every PR — [#1360]
- [x] CIT-5. Per-example headless-engine fixtures add substantial suite overhead — [#1363]
- [x] CIT-7. UI-script changes run an unrelated full behavior-probe bundle — [#1365]
- [x] CIT-3. CI does not directly detect a current worldgen determinism regression — [#1361]
- [x] CIT-6. CI skips the known w128 volcano exposure regression — [#1364]
- [x] CIT-4. A passing page-scope test leaves its world thread crashed — [#1362]
- [x] CIT-8. UI fixtures flood passing CI output with missing-world warnings — [#1366]
- [x] CIT-9. Pure same-input comparisons do not test language-generation determinism — [#1367]
- [x] CIT-10. Language suggestion and semantic tests use tautological determinism assertions — [#1368]
- [x] CIT-11. Unit and blood helper tests use tautological determinism assertions — [#1369]
- [x] CIT-12. Visual/layout helper tests use tautological determinism assertions — [#1370]
- [x] CIT-13. Autosave race test does not force the competing restore to reach the lock boundary — [#1372]
- [x] CIT-14. Headless harness lets worker crashes pass as successful examples — [#1388]
- [x] CIT-15. Responsive-menu UI tests restart the engine for every example — [no-issue]
- [x] CIT-21. Settings scale-change fan-out coverage omits Save and tolerates duplicate shell rebuilds — [#2027]
- [x] CIT-22. World-audit self-test compares identical pure calls — [#1380]
- [x] CIT-23. Building spawn/preview agreement test compares the same helper to itself — [#1381]
- [x] CIT-16. Location-overlay wiring test has a tautological assertion — [#1375]
- [x] CIT-17. Location-loot suite contains two false-green assertions — [#1376]
- [x] CIT-18. Blood texture and pool-placement determinism tests compare identical calls — [#1377]
- [x] CIT-19. Flora-lifespan determinism test only checks a pure call against itself — [#1378]
- [x] CIT-20. Final-climate determinism test has no independent expected result — [#1379]
- [x] CIT-24. Location-placement determinism test only compares pure same-input calls — [#1382]
- [x] CIT-25. Location-name determinism test compares the same pure construction twice — [#1383]
- [x] CIT-26. Location-instance mapping test derives both sides from the same construction — [#1384]
- [x] CIT-27. River-name determinism test compares the same pure construction twice — [#1385]
- [x] CIT-29. River-ID stability assertion repeats the same pure timeline query — [#1386]
- [x] CIT-30. Focus-navigation integration tests restart the engine for nearly every example — [no-issue]
- [x] CIT-31. Control-activation integration tests restart the engine for nearly every example — [no-issue]
- [x] CIT-32. Lua text-contract tests boot a full engine for each Lua assertion — [no-issue]

---

## CI selection cost

### [#1359] CIT-1. Test-only PRs unnecessarily run the entire behavior-probe smoke bundle

A change confined to `test-headless/` is classified as an unknown production
change, so it starts all eleven CI-eligible probes. Those probes exercise the
already-built executable, not the changed test source, and took roughly 6–7
minutes in representative PR runs. The full headless suite still runs and
compiles the changed test code, so this broad probe run adds little behavioral
evidence for a test-only change.

**Evidence:**

- `.github/workflows/ci.yml:484` — every PR executes the selector and then runs the selected probes.
- `tools/ci_probes.py:321` — `SKIP_GLOBS` excludes docs/assets but not `test/` or `test-headless/`.
- `tools/ci_probes.py:380` — unmatched paths return `ALL`.
- `tools/ci_probes.py:399` — any `ALL` contribution selects the complete `CI_ELIGIBLE` set.
- A selector invocation for `test-headless/Test/Headless/UI/ItemList.hs` selected all eleven probes.
- Completed Actions runs 32029132459 and 32003791798 spent about 7m08 and 6m25 respectively in the behavior-probe step.

**Handoff context:**

- **Current behavior:** Test-only PRs run unrelated production smoke probes.
- **Expected direction:** Treat ordinary test-only paths as probe-neutral, while retaining selector self-tests and explicit exceptions for tests that validate CI wiring or runtime assets.
- **Scope and constraints:** Preserve fail-safe `ALL` behavior for unknown production paths; do not suppress probes when a PR also changes runtime code.
- **Remaining uncertainty:** `test-headless/Spec.hs` is intentionally relevant to the unit-asset gate and needs an explicit exception if generic test-path skipping is introduced.

---

### [#1360] CIT-2. Save-compatibility’s one GHCi self-test runs on every PR

The “Save compatibility audit” step always runs its whole self-test module.
One test launches `cabal repl test:synarchy-test-headless` to manufacture two
real save fixtures before testing timestamp normalization. This is useful
integration coverage for fixture generation, but it is disproportionate as an
unconditional gate: the combined step consumes about 2m08–2m23 on every PR,
including unrelated UI and test changes.

**Evidence:**

- `.github/workflows/ci.yml:365` — both the save-audit self-test and static audit run unconditionally.
- `tools/test_save_compat_audit.py:891` — the reproducibility test is one member of the self-test module.
- `tools/test_save_compat_audit.py:907` — that test launches `cabal repl test:synarchy-test-headless`.
- `tools/test_save_compat_audit.py:921` — it then invokes the fixture normalizer on real encoded save data.
- `tools/test_save_compat_audit.py:1668` — the test is always included in the module’s default runner.
- Recent CI runs show the combined step consistently taking more than two minutes, while the other static audit steps take seconds.

**Handoff context:**

- **Current behavior:** Every PR pays for a full GHCi session to test a rarely changed fixture-generation workflow.
- **Expected direction:** Keep the real-codec test, but separate it from the fast static audit and select it for save-format, fixture, tool, or Cabal changes; retain a post-merge backstop.
- **Scope and constraints:** The ordinary static audit must remain blocking for every relevant manifest/source change. The integration test must remain available and must not be silently deleted.
- **Remaining uncertainty:** A focused timing run of the two portions separately would quantify the exact saving, though the CI-step history and the sole real GHCi subprocess already make the dominant cost clear.

---

### [#1363] CIT-5. Per-example headless-engine fixtures add substantial suite overhead

The headless suite starts a real world thread 267 times in a successful CI
run. `withHeadlessEngine` always sleeps 100 ms after each thread shutdown, so
the mandatory serial teardown delay alone accounts for at least 26.7 seconds,
before engine initialization and the test action itself. Many of those starts
are Hspec `around withHeadlessEngine` wrappers, which construct a fresh engine
for each example.

The isolation is legitimate for mutation-heavy scenarios, but it is not the
only established pattern. Several UI suites already use one `aroundAll`
engine/Lua fixture and reset their owned state between examples. Applying that
pattern selectively to independent UI/input integration groups would preserve
their assertions while removing repeated lifecycle work.

**Evidence:**

- `test-headless/Test/Headless/Harness.hs:30-43` starts the world thread and unconditionally waits 100 ms during teardown.
- Passing Actions run 32029132459 logged 267 `Starting world thread` lines in its `Headless test suite` step.
- `test-headless/Test/Headless/UI/FocusNavigation.hs` uses five per-example `around withHeadlessEngine` blocks.
- A direct focused run of `UI.FocusNavigation` executed 50 examples with 36 engine starts in 4.30 seconds; the fixed teardown wait alone accounts for 3.6 seconds.
- `UI.ResponsiveGameplay`, `UI.TransferSession`, and `UI.TransferGestures` show the existing shared-fixture pattern: `aroundAll` plus explicit per-case reset.

**Handoff context:**

- **Current behavior:** Small integration examples repeatedly pay for full engine/thread lifecycle.
- **Expected direction:** Consolidate only demonstrably independent groups behind a shared fixture with explicit reset helpers.
- **Scope and constraints:** Do not merge worldgen readers with mutation-heavy tests, or weaken state isolation merely to reduce timing. Keep per-example fixtures where reset cannot make the contract reliable.
- **Remaining uncertainty:** The exact saving depends on which groups can safely share state; the 26.7-second figure is a conservative lower bound from the fixed teardown sleep alone.

---

### [#1365] CIT-7. UI-script changes run an unrelated full behavior-probe bundle

`tools/ci_probes.py` has no feature rule for `scripts/ui/*`. Such files fall
through to the fail-safe `ALL` selector and launch every CI-eligible behavior
probe: item instances, crafting, persistence, repair, content registry, and
CLI boot. Those probes boot headless engines; they do not exercise the real
GPU/UI flow, whose offscreen probes are deliberately manual-only. The complete
headless UI suite already runs on every pull request, so the additional 6–7
minute smoke bundle supplies little direct evidence for an ordinary UI-script
change.

This is an instance of an oversized selector category, not a reason to weaken
coverage generally. UI is a good first candidate for smaller, subsystem-aware
probe selections; the same audit should then examine other broad categories
that currently map changes to unrelated smoke bundles.

**Evidence:**

- `tools/ci_probes.py:321-329` does not exclude `scripts/ui/*`, and its feature rules contain no UI-script mapping.
- `tools/ci_probes.py:380` maps unmatched paths to `ALL`; `:399-401` expands `ALL` to every CI-eligible probe.
- The selector for both `scripts/ui/slider.lua` and `scripts/ui/ui_manager.lua` returns all eleven CI-eligible probes.
- `tools/ci_probes.py:48-80` defines the eligible set as headless item/crafting/persistence/content/CLI coverage, not a UI-flow category.
- `tools/item_list_widget_probe.py:1-14` and `tools/transfer_context_menu_probe.py:1-12` document that the real HUD/UI path needs `--offscreen` and is manual-only because headless does not boot it.
- Representative full behavior-probe CI steps took approximately 6–7 minutes (CIT-1 evidence).

**Handoff context:**

- **Current behavior:** UI-script changes pay for unrelated headless behavior smokes while no selected probe directly covers their rendered flow.
- **Expected direction:** Split broad selector categories into smaller subsystem-relevant selections. Start with an explicit UI rule, selecting no unrelated behavior probes unless a deterministic, appropriately scoped UI smoke is promoted.
- **Scope and constraints:** Keep the full Hspec suite blocking and preserve `ALL` for genuinely unknown production paths. GPU/offscreen UI probes must not be promoted casually; their determinism and runner support need separate evidence.
- **Remaining uncertainty:** Some UI-adjacent scripts may legitimately affect console boot or shared runtime wiring, so any UI glob must be narrower than a blanket `scripts/*` exclusion and may need explicit exceptions.

---

## Worldgen coverage

### [#1361] CIT-3. CI does not directly detect a current worldgen determinism regression

The worldgen CI gate runs each quick seed only once. Its own code documents that
a single run cannot observe a determinism regression. The Hspec substitute
initializes one additional seed twice, but compares only chunk surface maps,
not the full dumped world contract. Consequently a nondeterministic change in
other generated fields, or one occurring on the CI quick-seed set but not Hspec
seed 123, can pass.

A related pure content-identity self-test is already recorded as un-gated in
`docs/project_review_71-33.md`; this finding is specifically about the
runtime, current-output determinism check.

**Evidence:**

- `.github/workflows/ci.yml:452` — CI invokes `world_check.py --quick`.
- `tools/world_check.py:337` — `--runs` defaults to one.
- `tools/world_check.py:119` — the checker states that with one run a determinism regression cannot be observed.
- `tools/world_check.py:230` — only the requested number of dumps is generated.
- `test-headless/Test/Headless/WorldGen.hs:126` — Hspec generates only seed 123 twice.
- `test-headless/Test/Headless/WorldGen.hs:135` — that Hspec test compares only `lcSurfaceMap`.
- `docs/project_review_71-33.md:41` — the pure canonical-dump determinism contract is separately not in a maintained gate.

**Handoff context:**

- **Current behavior:** CI catches output drift against a baseline but does not directly test repeated current output for the quick worldgen corpus.
- **Expected direction:** Add a bounded repeated-run determinism check for worldgen-relevant changes, covering the full dump identity contract without doubling every unrelated CI run.
- **Scope and constraints:** Preserve the current one-run baseline check and keep any added coverage path-selective. Do not re-baseline to hide nondeterminism.
- **Remaining uncertainty:** The appropriate sample size is a policy choice: one representative quick seed repeated is much cheaper; repeating all six quick seeds gives broader confidence at roughly another world-check interval.

---

### [#1364] CIT-6. CI skips the known w128 volcano exposure regression

`WorldGen.Exposure` contains a targeted regression for the known seed-42,
world-size-128 volcano case, including its lava and basalt-cap columns. It is
an Hspec example in the normal test tree, but it calls `pendingWith` unless
`SYNARCHY_FULL_TESTS` is set. Neither GitHub CI nor the local `make ci` gate
sets that variable. Their path-selected worldgen coverage is instead
`world_check.py --quick`, whose six baseline seeds include one w64 case but no
w128 case.

The test is valuable rather than redundant: its comment identifies the
specific known repro and its loaded 5x5 chunk ring, while the quick baseline
corpus has a different purpose and geometry. The full tier remains appropriate
for broad manual validation, but this single focused case is a bounded
candidate for the CI worldgen path.

**Evidence:**

- `test-headless/Test/Headless/WorldGen/Exposure.hs:161-172` marks the w128 seed-42 volcano example full-tier-only and converts the ordinary CI path into `pendingWith`.
- The same test’s body at `:174-184` loads the known volcano chunk and its surrounding ring before checking the exposure invariant.
- `.github/workflows/ci.yml:459-460` runs only `world_check.py --quick` when the worldgen selector fires, and does not set `SYNARCHY_FULL_TESTS`.
- `tools/ci-local.sh:122` likewise runs only the quick world check.
- `tools/baselines/_seeds.json:249` documents that the quick corpus has six seeds and only one w64 scale case.

**Handoff context:**

- **Current behavior:** The targeted known-volcano regression is manual full-tier coverage and passes CI as pending.
- **Expected direction:** On worldgen-selected CI runs, execute this one focused example with `SYNARCHY_FULL_TESTS=1` rather than enabling every full-tier test.
- **Scope and constraints:** Retain the current quick baseline gate and the full manual tier. Do not make unrelated PRs pay the extra generation cost.
- **Remaining uncertainty:** The source estimates this case at roughly 25 seconds; CI timing on the Linux runner should be measured before finalizing the selector budget.

---

## Test correctness

### [#1362] CIT-4. A passing page-scope test leaves its world thread crashed

`Language etymology (page scope)` is wrapped in `withHeadlessEngine`, which
starts a real world thread. Its fixture then replaces the live world manager
with two `emptyWorldState` pages containing hand-built identity and naming data
but no tectonic plates. The still-running world thread touches the visible
fixture page and terminates with `twoNearestPlates: no plates`; the Hspec
examples continue to pass because they call the Lua query directly and assert
neither the background thread's health nor its exception.

This is a false-green test run and makes its reported integration boundary
weaker than intended: a later assertion could accidentally depend on the
thread, yet the test infrastructure has already failed.

**Evidence:**

- `test-headless/Spec.hs:311` wraps this exact suite in `aroundAll withHeadlessEngine`.
- `test-headless/Test/Headless/Harness.hs:38` starts the world thread for every such wrapper.
- `test-headless/Test/Headless/Language/EtymologyPageScope.hs:167` installs two `emptyWorldState` fixtures into `worldManagerRef`.
- `test-headless/Test/Headless/Language/EtymologyPageScope.hs:174` gives the active fixture ordinary world-generation parameters, and `src/World/Generate/Types.hs:247` leaves that default's plate list empty.
- `src/World/Plate/Query.hs:78` throws when elevation logic receives no plates.
- A focused `cabal test synarchy-test-headless --test-options='--match "Language etymology"'` run logged `World thread crashed: twoNearestPlates: no plates (seed=42 worldSize=128 tile=(-14,-14))`, then exited successfully with 70 examples and 0 failures.
- The same crash appears in the passing GitHub Actions run 32029132459, immediately before the page-scope examples.

**Handoff context:**

- **Current behavior:** The query assertions pass after their harness's world thread has crashed.
- **Expected direction:** Run this direct-Lua query fixture without a world thread, or construct a thread-safe fixture and explicitly fail if its required worker exits.
- **Scope and constraints:** Preserve the real registered Lua API and the two-page, no-worldgen setup; the repair should not turn this lightweight query regression into an expensive generated-world test.
- **Remaining uncertainty:** The crash's first scheduling point can vary, but its cause is deterministic: the fixture installs an empty plate list while the worker remains active.

---

### [#1366] CIT-8. UI fixtures flood passing CI output with missing-world warnings

The passing headless suite in Actions run 32029132459 emits 942 warnings that
`main_world` is absent during cursor-texture updates. `hud.lua` defaults to
that page identifier and sends texture-update commands from `hud.createUI()`.
The `ResponsiveGameplay` and `TutorialHud` fixtures boot the HUD to test layout
and widget behavior without supplying that world, so each otherwise-successful
boot creates several irrelevant warnings.

These warnings are not harmless test output: the same passing run contained the
world-thread crash recorded in CIT-4. A large, expected warning flood makes
unexpected worker failures and other diagnostics materially easier to overlook.

**Evidence:**

- Passing Actions run 32029132459 contains 942 `World not found for ... cursor ... texture update: main_world` warnings.
- `scripts/hud.lua:25` defaults `hud.worldId` to `main_world`.
- `scripts/hud.lua:389-407` submits the zoom/world cursor and designation texture updates during `hud.createUI()` whenever its synthetic test handles are present.
- `test-headless/Test/Headless/UI/ResponsiveGameplay.hs:85-99` shares a headless engine and resets UI/Lua state between examples, but does not install `main_world` before the many HUD boots.
- `test-headless/Test/Headless/UI/TutorialHud.hs:102-116` likewise boots `hud.createUI()` against synthetic assets and no main world.
- `src/World/Thread/Command/Cursor/Select.hs:80-173` correctly logs a warning for each missing-world cursor update.

**Handoff context:**

- **Current behavior:** Layout-oriented HUD tests produce hundreds of production missing-world warnings while still passing.
- **Expected direction:** Give affected fixtures a minimal valid `main_world`, or isolate/stub only the cursor-texture dispatch that is outside each test's contract. Centralize real lifecycle binding in a HUD helper such as `bindWorld`/`applyWorldTextures`, invoked when a world is created or load publication makes it active.
- **Scope and constraints:** Do not suppress the production missing-world warnings globally, and retain a focused test for the warning/command behavior itself. Lua modules that are valid before a world exists must remain loadable, so gate their world-dependent operations rather than `require` itself. `world.getActiveWorldId()` is the existing engine-backed active-world guard; Lua's remembered `worldManager.currentWorld` is not proof that the world thread has created the page.
- **Remaining uncertainty:** `world.getActiveWorldId()` is suitable only when the HUD targets the active page. If genuine multi-world UI needs to bind a non-active page, add an engine-backed `world.exists(worldId)` query instead. Cases that verify world-specific dispatch should keep a real minimal world and assert the target, rather than inheriting a broad no-world suppression.

---

### [#1367] CIT-9. Pure same-input comparisons do not test language-generation determinism

Several examples described as determinism checks evaluate the same pure Haskell
expression twice and compare the two results. In ordinary pure code, both
evaluations necessarily agree: a change that consistently alters profiles,
roots, bound-form selection, or rendered words passes unchanged. The assertions
therefore only establish that the computation did not throw, not the advertised
stable-output contract.

Nearby language tests do exercise useful independent properties, including
catalogue-order invariance and generated-form validity. This finding is limited
to the tautological same-input comparisons, not the language-generation suite as
a whole.

**Evidence:**

- `test-headless/Test/Headless/Language/Generated.hs:1167-1169` compares `buildProfileV3 seed` and `nativeRenderingsV3 seed` to identical re-evaluations.
- `test-headless/Test/Headless/Language/Generated.hs:1171-1174` does the same for `joinMorphemes` and `joinSyllables` with unchanged arguments.
- `test-headless/Test/Headless/Language/Generated.hs:1178-1181` compares `lrBound (rootsFor p)` to itself for every fixed seed.
- The same module repeats the pattern for version-1 profiles (`:453-455`) and version-5 profiles (`:1677-1679`).
- The meaningful neighbouring test at `:1183-1196` compares outputs from differently ordered catalogue inputs, demonstrating the kind of independently varying input the same-input cases lack.

**Handoff context:**

- **Current behavior:** The CI suite reports determinism coverage while these individual assertions pass after any deterministic output change.
- **Expected direction:** Replace them with compact fixed-seed golden fingerprints or selected expected profile/rendering/bound-form outputs; retain only tests whose two sides differ in a meaningful input or independent implementation path.
- **Scope and constraints:** Do not freeze the entire generated-language corpus gratuitously, or mistake changed product policy for a regression. Keep the existing catalogue-order and structural-invariant tests.
- **Remaining uncertainty:** A focused expected-output sample should cover profile selection, one repaired join, and at least one bound form. The exact samples should be chosen for stability and diagnostic clarity rather than maximum volume.

---

### [#1368] CIT-10. Language suggestion and semantic tests use tautological determinism assertions

The language-consumer suites repeat pure calls with identical arguments and
label equality as determinism. The suggestion test rebuilds the same catalogue,
seed, and ordinal sequence on both sides; the semantic test renders the same
expression on both sides. Neither detects a consistently changed suggestion
sequence or English rendering.

**Evidence:**

- `test-headless/Test/Headless/Language/Suggest.hs:100-102` compares `suggestionsFor prodCat s 12` to the identical call for each sample seed.
- `test-headless/Test/Headless/Language/Semantic.hs:138-141` compares `gloss e` to itself.
- Both modules already contain non-tautological neighboring assertions: `Language.Suggest` re-renders stored expressions and checks reroll distinctions (`:104-137`), while `Language.Semantic` pins concrete English results and independently reparses the catalogue (`:108-152`).

**Handoff context:**

- **Current behavior:** The CI suite labels these two pure self-equalities as determinism coverage, although coherent output changes leave them green.
- **Expected direction:** Replace each with a small fixed-input expectation meaningful to its consumer contract, or remove it if the adjacent independent assertions already make it redundant.
- **Scope and constraints:** Keep catalogue parse/reparse coverage, expression-to-render agreement, and reroll diversity. Do not turn ordinary content evolution into a large brittle golden corpus.
- **Remaining uncertainty:** For suggestions, choose a compact tuple of expression/name/gloss/provenance fields; for semantic rendering, choose the existing representative grammar forms rather than only one modifier expression.

---

### [#1369] CIT-11. Unit and blood helper tests use tautological determinism assertions

Several pure gameplay-helper tests compare a seeded or classified result to an
identical evaluation. They do not test the stated repeatability contract and
would pass if a formula or mapping changed consistently.

**Evidence:**

- `test-headless/Test/Headless/Blood/Impact.hs:142-144` compares `impactFallbackAngle 12345` to itself.
- `test-headless/Test/Headless/Unit/Stats.hs:44-46` compares a fixed-seed `rollOne` call to itself; `:230-232` does the same for `pickName`.
- `test-headless/Test/Headless/Unit/Injury.hs:36-38` compares the same `tissueInjuryKind "bone" "blunt"` call to itself.
- The surrounding tests supply useful range, threshold, and classification coverage, but none makes these same-input equalities non-vacuous.

**Handoff context:**

- **Current behavior:** These examples add a passing test count but no regression signal for deterministic helper output.
- **Expected direction:** Replace them with selected seeded expected values where the exact result is contractual (angle/roll/name), and a concrete expected injury classification for the blunt-bone case.
- **Scope and constraints:** Preserve range/distribution tests and avoid freezing pseudo-random sequences beyond a small diagnostic sample. Do not conflate deterministic reproducibility with a particular balancing policy unless the test names that policy.
- **Remaining uncertainty:** The desired golden values should be reviewed alongside any intentional random-generator or balance revision; the classification case is the least policy-sensitive replacement.

---

### [#1370] CIT-12. Visual/layout helper tests use tautological determinism assertions

Two pure visual/layout tests claim repeated unchanged calls prove stable
identity/order, but each side recomputes exactly the same value from exactly
the same inputs. They cannot detect a consistent change in font-key policy or
location-icon quad ordering.

**Evidence:**

- `test-headless/Test/Headless/Graphics/FontRepertoire.hs:581-583` compares `sdfFontKey arcadePath` to itself.
- `test-headless/Test/Headless/Location/MapIcons.hs:557-560` compares the same `run ()` icon-quad projection to itself.
- Useful neighboring checks already distinguish fonts/repertoires (`FontRepertoire.hs:570-593`) and pin the expected icon texture order (`MapIcons.hs:548-557`), so these self-comparisons can be replaced by more discriminating sort-key/position expectations or removed.

**Handoff context:**

- **Current behavior:** The examples are green for any pure implementation that returns a value, including one with a consistently wrong key or order.
- **Expected direction:** Retain explicit cross-input distinction and expected-order assertions; remove the repeated-call equality or replace it with a targeted expected sort/geometry value not already covered.
- **Scope and constraints:** Do not require GPU rendering for these pure contracts, and do not duplicate the adjacent assertions verbatim merely to preserve test count.
- **Remaining uncertainty:** The map-icon expected texture order may already be sufficient; the reviewer should decide whether a separate position/sort-key assertion covers a distinct contract before adding another fixed expected vector.

---

### [#1372] CIT-13. Autosave race test does not force the competing restore to reach the lock boundary

The player-intent lock test holds a transition, forks a competing
`restoreIfPlayerIdle`, sleeps for 50 ms, then releases the transition. It never
observes that the restore has reached the competing lock/check boundary before
release. Under an unfavorable schedule, the restore can start only after the
transition has incremented the generation; it then correctly returns `False`,
so every current assertion passes even if the intended mutual exclusion had been
removed from the race path.

This is a test-coverage weakness, not evidence that the current MVar-based
implementation is wrong. The implementation's critical section is present;
the test does not deterministically exercise it.

**Evidence:**

- `test-headless/Test/Headless/Save/AutosaveGuards.hs:134-143` starts the holder and competing restore on separate threads, but provides no acknowledgement from inside `restoreIfPlayerIdle`.
- `:144-146` uses `threadDelay 50000` as the only opportunity for the competing operation to run before release.
- `:148-152` releases the holder and accepts the same `False`/`"player"` outcome that occurs when a delayed restore first observes the already-bumped generation.
- `src/Engine/Core/Capability/WorldSim.hs:166-188` contains the critical `modifyMVar` operations whose relative ordering the test claims to verify.

**Handoff context:**

- **Current behavior:** Scheduling can skip the intended overlap while the test reports that the lock race passed.
- **Expected direction:** Make the interleaving deterministic with a narrowly scoped test hook at the restore pre-check/lock boundary, or extract a testable synchronization primitive that can be held and observed exactly there.
- **Scope and constraints:** Preserve the production MVar contract and the real generation-change assertion. Do not merely increase the sleep; elapsed time cannot prove a competing thread reached a particular program point.
- **Remaining uncertainty:** The smallest clean test seam may be a callback only in test builds, or an internal helper with explicit before-check/after-acquire hooks; choose one that does not leak test policy into the public capability API.

---

### [#1388] CIT-14. Headless harness lets worker crashes pass as successful examples

`withHeadlessEngine` starts a real world worker, runs an Hspec action, and then
unconditionally shuts the worker down. It does not assert that the worker or
engine remained healthy while the action ran. A fatal worker exception changes
the lifecycle to `CleaningUp` and is logged, but direct assertions can continue
and pass. CIT-4 is a concrete successful-CI example of that failure mode.

This is distinct from the invalid fixture in CIT-4: repairing that fixture
removes one trigger, while the harness blind spot remains available to every
future engine-backed test.

**Evidence:**

- `test-headless/Test/Headless/Harness.hs:27-43` starts the world thread, invokes the action, and only performs ordinary shutdown; it has no post-action worker/lifecycle health assertion.
- `src/World/Thread.hs:120-123` logs a caught worker exception and writes the engine lifecycle to `CleaningUp` rather than throwing into the Hspec example.
- CIT-4's focused test and passing Actions run 32029132459 demonstrate the result: `World thread crashed` was logged while Hspec reported zero failures.

**Handoff context:**

- **Current behavior:** A worker may fail-stop in the background while the example that booted it remains green.
- **Expected direction:** Have the harness verify worker/engine health after each wrapped action and report a failing expectation before its normal, exception-safe teardown.
- **Scope and constraints:** Preserve teardown after assertion failures. Tests deliberately exercising engine shutdown need a narrow, explicit opt-out rather than making every worker failure acceptable.
- **Remaining uncertainty:** Checking the lifecycle covers the current world-worker failure path. If worker state does not retain failure provenance, a small test-only crash/result channel may give better diagnostics and distinguish an intentional stop from an exception.

---

### [no-issue] CIT-15. Responsive-menu UI tests restart the engine for every example

> **Disposition:** No issue — #1363 removed the fixed teardown delay that
> supplied 77% of the original cost. A fresh focused run on 2026-08-31 completed
> 110 examples in 3.8085 seconds with zero failures. Saving part of that residual
> does not justify sharing and resetting menu, Lua, filesystem, and scale state;
> revisit only if future profiling identifies this suite as a material blocker.

The responsive-menu suite has 93 examples, all wrapped with Hspec's `around`
and `withMenusEngine`. Each example therefore creates a fresh headless engine
and world thread and pays the harness's unconditional 100 ms teardown delay,
even though the tests interact with a bare Lua backend and most need only a
known UI/Lua state.

This is a deliberately separate UI-region finding from CIT-5. The nearby
responsive-gameplay suite has already established the appropriate alternative:
one engine and Lua backend for its 109 examples, with an explicit reset at the
start of every case.

**Evidence:**

- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:94-99` defines
  `withMenusEngine` and applies it with `around`, which Hspec invokes once per
  example.
- The focused group ran 93 passing examples in 11.839 seconds; its 93 harness
  teardowns alone impose at least 9.3 seconds (`Harness.hs:40-43`). The run
  also logged a fresh `Starting world thread` for each example.
- `test-headless/Test/Headless/UI/ResponsiveGameplay.hs:57-99` instead shares
  one engine and Lua backend via `aroundAll`, calling `resetFixture` at the
  start of each example to restore its page manager, loaded modules, and
  baseline UI scale.
- `ResponsiveMenus.hs:89-96` already identifies the only universal setup it
  needs: restoring the in-memory UI scale before modules initialize. Its cases
  then create bare Lua backends and drive screen modules directly.

**Handoff context:**

- **Current behavior:** A focused suite spends most of its test time in
  repeat engine startup/shutdown rather than testing responsive layout and
  lifecycle behavior.
- **Expected direction:** Split the responsive-menu region into resettable
  groups and run each under a shared engine/Lua fixture. Reset the UI manager,
  Lua package/module state, callbacks, and baseline UI scale before every
  example; reserve a private engine only for a demonstrably non-resettable
  subgroup.
- **Scope and constraints:** Preserve per-example behavioral isolation and
  the canonical 1x scale guarantee, including examples that explicitly set a
  different scale. Do not weaken menu lifecycle, resize, focus, or save-browser
  assertions merely to combine setup.
- **Remaining uncertainty:** The smallest safe grouping may be per menu
  family rather than all 93 examples in one fixture. Establish which globals
  and package-loaded modules require reset, then add an order-independence
  regression before removing private engines.

---

### [#2027] CIT-21. Settings scale-change fan-out coverage omits Save and tolerates duplicate shell rebuilds

The Settings menu exposes four scale-changing paths: Apply, Save, Defaults,
and Back. Each is supposed to notify registered menus, the live shell, and
gameplay surfaces exactly once when the UI scale actually changes. CI proves
the shell's exact-once behavior only for Apply. Defaults and Back accept any
positive call count, so a duplicate shell rebuild passes, while Save is not
invoked by either responsive UI suite at all.

This is a focused UI coverage gap, not a separate report category: the
headless CI suite is the current automated coverage for these blocking
settings paths. A duplicate route is materially observable because a shell
resize rebuilds its live UI; absence of the Save path is an equally direct
false green if its duplicated fan-out later drifts.

**Evidence:**

- `scripts/settings_menu.lua:155-181` implements Defaults' scale-change
  branch, and `:963-1011` implements separate Apply, Save, and Back branches;
  all four call `responsive.notifyResize`, `shell.onFramebufferResize`, and
  `uiManager.notifyGameplayRescale` on a real scale change.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:1410-1434` spies on
  Apply's shell handler and correctly requires `calls == 1`.
- `ResponsiveMenus.hs:1443-1473` drives Defaults and Back, but asserts only
  `calls > 0`; two or more shell rebuilds therefore pass.
- Repository-wide test searches find no invocation of
  `settingsMenu.onSave`; the nearby comment at `ResponsiveMenus.hs:1390-1392`
  says Apply and Save take the same direct-shell path, but the test executes
  Apply only.
- `scripts/shell.lua:1071+` rebuilds visible console geometry on a resize,
  making accidental duplicate delivery a real lifecycle/performance defect,
  not merely an implementation detail.

**Handoff context:**

- **Current behavior:** A removed Save fan-out, or a duplicate Defaults/Back
  shell dispatch, can leave every CI test green. Apply's exact-once test
  protects only its own separately implemented handler.
- **Expected direction:** Add a parameterized/table-driven test over Apply,
  Save, Defaults, and Back. For each action, force an actual scale change and
  assert exactly one shell dispatch and the expected menu/gameplay fan-outs.
  Keep the existing unchanged-scale case to assert zero dispatch.
- **Scope and constraints:** Keep the actions independently exercised unless
  production first extracts their common fan-out into one helper; in that
  case, test the helper's exact-once behavior plus a thin reachability test
  for every public action. Do not weaken `== 1` to a nonzero assertion merely
  because multiple code paths happen to be tolerated.
- **Remaining uncertainty:** The gameplay fan-out has many conditional
  recipients; the focused test need only spy on one stable representative per
  route, provided the separate `notifyGameplayRescale` suite continues to
  cover its complete recipient set.

---

### [#1380] CIT-22. World-audit self-test compares identical pure calls

The Python CI self-test named `test_determinism_of_audit` runs `audit_dump` on
the same synthetic grid three times, canonicalizes each result with
`json.dumps(..., sort_keys=True)`, and asserts equality. `audit_dump` builds a
fresh result from its explicit tile input and has no time, randomness, or
mutable cross-call state. The equality is therefore reflexive for every
non-throwing deterministic change, including one that changes or removes an
audit rule's detection behavior.

This is a separate Python-audit region from the Haskell/Lua determinism
assertions above. The surrounding synthetic tests remain valuable: they pin
each issue category's positive and negative examples and world-check's summary
rules. This one test supplies no independent audit contract.

**Evidence:**

- `tools/test_audit.py:557-564` builds one 10x10 grid, invokes
  `audit_dump(tiles)` three times with identical arguments, and compares only
  the three canonicalized outputs.
- `tools/world_audit.py:1112-1129` constructs a local coordinate grid and a
  new `AuditResult` from the supplied data, then applies `ALL_CHECKS`; it has
  no random, clock, or retained state dependency to make repeat invocation a
  meaningful oracle.
- `test_audit.py:80-553` already exercises the real contracts through
  synthetic issue shapes, while `:573-680` separately pins `world_check`
  classification and determinism-status decisions.
- `tools/test_audit.py:690` includes this redundant test in CI's default
  runner, invoked by `.github/workflows/ci.yml:290-291`.

**Handoff context:**

- **Current behavior:** The named determinism test stays green after any
  deterministic semantic drift in `audit_dump`; it detects only exceptions or
  an unlikely hidden state/randomness regression.
- **Expected direction:** Delete the duplicate-call assertion, or replace it
  with an order-invariance test: feed a deliberately nontrivial tile set in
  two distinct input orders and require identical serialized audit results.
  The latter pins a real boundary because `audit_dump` derives a coordinate
  grid from an input list while aggregate statistics and issue reporting must
  remain independent of that list's order.
- **Scope and constraints:** Preserve every existing per-category positive
  and negative fixture. Do not substitute a new same-input comparison for the
  removed one.
- **Remaining uncertainty:** If input order is deliberately meaningful for a
  future audit diagnostic, document and test that ordering explicitly instead;
  the current implementation's coordinate-keyed checks and aggregate counts
  indicate order independence is the intended behavior.

---

### [#1381] CIT-23. Building spawn/preview agreement test compares the same helper to itself

The test titled "direct spawn and preview validation agree" invokes the pure
`canPlaceAt` helper twice with identical arguments and compares the results.
It never exercises either public caller named by the test—Lua
`building.spawn` or `building.canPlaceAt`—so it cannot detect a wiring or
argument-scoping divergence between direct placement and ghost preview. As a
same-input comparison of a deterministic helper, it also remains green for
every non-throwing semantic change to placement validation.

This is a separate building-placement region from the UI and Python-audit
findings. The nearby fixture tests already pin meaningful placement behavior:
location-bound rejection, boundary/adjacency cases, distinct rejection
reasons, and seam handling.

**Evidence:**

- `test-headless/Test/Headless/Building/Placement.hs:270-287` names the two
  public callers in a comment, but the example at `:276-286` calls only
  `canPlaceAt` on both sides with the same building manager, world tile data,
  location instances, world size, definition, and coordinates.
- `src/Engine/Scripting/Lua/API/Buildings/Spawn.hs` exposes the direct spawn
  path, while the preview API exposes `building.canPlaceAt`; neither boundary
  is invoked by this Hspec example.
- `Placement.hs:202-270` already provides exact expected results for the
  helper's actual location-overlap and ordinary rejection contracts, so
  removing the duplicate call does not reduce that useful unit coverage.

**Handoff context:**

- **Current behavior:** The named agreement example passes whenever one
  deterministic helper call returns normally; a future caller can bypass,
  mis-scope, or transform validation without this test noticing.
- **Expected direction:** Delete the duplicate comparison, or replace it with
  a narrow integration fixture that drives both public Lua entry points against
  the same page-scoped location overlay and asserts equivalent externally
  visible success/rejection results.
- **Scope and constraints:** Keep the existing pure `canPlaceAt` edge and
  seam matrix. A public-path test must use a real page-scoped fixture so an
  absent page or empty overlay cannot make both paths agree vacuously.
- **Remaining uncertainty:** If production intentionally centralizes both
  callers permanently through this helper, the correct disposition may be
  deletion rather than integration coverage; the current test still does not
  prove that structural fact.

---

### [#1375] CIT-16. Location-overlay wiring test has a tautological assertion

The test titled "world init wires a serializable overlay field" accepts every
possible overlay value: it asserts only that a hash map's size is at least
zero. In its actual headless fixture, the location registry is empty, so world
initialization is supposed to retain the default empty overlay. The test thus
does not prove either placement-to-params wiring or persistence of an
initialized overlay; the preceding test already proves that generation params
exist.

The two claimed behaviors are currently tested only in isolation: placement is
exercised through pure synthetic-definition tests, and serialization is
exercised with a hand-built nonempty overlay. No test drives initialization
with registered location content and observes the resulting populated overlay
through the save representation.

**Evidence:**

- `test-headless/Test/Headless/WorldGen.hs:212-217` calls `sharedWorld` and
  checks `HM.size (wgpLocationOverlay p) >= 0`, a mathematical invariant of
  `HashMap`; `:69-77` already checks that the same initialized world has
  generation params.
- `src/Engine/Core/Init.hs:249-254` initializes the headless location registry
  as empty. `src/World/Thread/Command/Init.hs:218-227` therefore computes an
  empty placement and stores it in `wgpLocationOverlay` for this fixture.
- `src/World/Thread/Command/Init.hs:223-243` is the production wiring point:
  it derives the overlay from the loaded registry and writes the forced result
  into `WorldGenParams`.
- `WorldGen.hs:278-283` proves the manual `Serialize` instance preserves a
  synthetic map; `test-headless/Test/Headless/World/Save/Components.hs:1528-1543`
  round-trips a rich worldgen fixture whose location overlay remains the
  default empty value.

**Handoff context:**

- **Current behavior:** The named integration assertion remains green if
  initialization drops the generated overlay, leaves it empty despite loaded
  definitions, or changes its contents.
- **Expected direction:** Replace it with a narrow integration fixture that
  registers at least one location definition before world initialization, then
  verifies that the initialized params contain the placement result and that
  the nonempty result survives the relevant worldgen DTO/save round trip.
- **Scope and constraints:** Keep the existing pure placement matrix and
  synthetic serialization test; they cover different boundaries. If creating
  a populated init fixture is disproportionate, delete the tautological test
  rather than preserving its misleading behavioral claim.
- **Remaining uncertainty:** The cheapest fixture may directly populate the
  headless content-registry capability before a private `WorldInit`, avoiding
  YAML loading and a second broad engine boot.

---

### [#1376] CIT-17. Location-loot suite contains two false-green assertions

Two assertions in the otherwise useful location-loot contract suite cannot
fail for the behavior their titles claim to test. The "at most one item
definition per roll" predicate maps both `Nothing` and every `Just` result to
`True`; more fundamentally, `rollLootTableFor` returns `Maybe Text`, so its
type already constrains a call to zero or one item id. The negative-instance-id
"determinism" assertion compares an expression to the identical expression,
so it can only expose an exception rather than a changed deterministic result.

The surrounding suite retains substantial coverage: pinned vectors over the
shipped loot table, context-component separation, order independence, weighted
selection, and empty/single-entry tables. This finding is limited to these two
assertions, not a claim that the seeded loot mapping lacks CI coverage.

**Evidence:**

- `test-headless/Test/Headless/Location/LootDeterminism.hs:87-89` defines the
  sequence as `[Maybe Text]`; `src/LootTable/Roll.hs:108-114` gives
  `rollLootTableFor` the same `Maybe Text` result type.
- `LootDeterminism.hs:222-223` checks
  `all (maybe True (const True)) ... == True`, which is true for every
  possible list of `Maybe Text` values.
- `LootDeterminism.hs:187-192` compares the negative fallback context's roll
  to the exact same call, while separately checking only that the result is
  present and the unit draw lies in range.
- `src/LootTable/Roll.hs:86-106` documents and implements the distinctive
  negative-component conversion into the stable `Word64` mixing path; a fixed
  expected result is needed to pin that conversion.

**Handoff context:**

- **Current behavior:** Both examples pass when loot selection's value changes
  or the negative fallback mapping drifts, provided it neither throws nor
  returns `Nothing` for the shipped positive-weight table.
- **Expected direction:** Delete the return-type tautology. Replace the
  negative-context self-comparison with a fixed expected unit draw and/or
  selected item id, chosen from the established context, so the signed-to-
  `Word64` conversion is observable.
- **Scope and constraints:** Preserve the existing shipped-data vectors and
  distribution check; those are meaningful contract coverage. Do not replace
  an exact negative fallback pin with another same-input equality.
- **Remaining uncertainty:** If the expected value is intentionally not a
  public compatibility commitment, assert a documented algebraic relation to
  the canonical context mixer instead, but it must compare distinct states or
  a stable external oracle.

---

### [#1377] CIT-18. Blood texture and pool-placement determinism tests compare identical calls

The procedural blood texture and stationary-pool placement suites both claim
to test reproducibility, but compare each pure function's output with an
identical invocation. These assertions necessarily pass for any non-throwing
value, including a changed texture, a changed deterministic seed mix, or an
accidentally constant output. That leaves the actual byte-stability contract
for reconstructible decal textures and seed-stable pool placement unpinned.

This is a separate blood-rendering region from CIT-11's impact helper. The
surrounding tests remain useful: they check texture size, transparency,
descriptor variation, pool radius, non-overlap, and seed differentiation.
None supplies a stable known-output oracle for the identical-input contract.

**Evidence:**

- `test-headless/Test/Headless/Blood/Texture.hs:35-42` compares generated
  bytes and their hash from the same `baseDescriptor` expression on both sides.
- `src/Blood/Texture.hs:229-264` documents `generateBloodTexture` as fully
  deterministic from its descriptor; `docs/blood_decals.md:380-385` relies on
  that byte-identical regeneration property for a saved descriptor.
- `test-headless/Test/Headless/Blood/Trail.hs:835-855` checks pool radius,
  uniqueness, and variation across seeds, but `:842-844` tests the same
  `(seed, index)` with an identical self-comparison.
- `src/Blood/Pool.hs:258-272` defines the deterministic golden-angle spiral
  whose exact coordinate is the claimed contract.

**Handoff context:**

- **Current behavior:** The named reproducibility examples pass after any
  non-exceptional output change; the remaining property tests do not establish
  that a particular descriptor or pool seed recreates the same prior result.
- **Expected direction:** Replace the texture self-comparisons with a compact
  golden hash and, if useful, a small fixed pixel sample for `baseDescriptor`.
  Replace the pool self-comparison with an expected coordinate (with an
  explicit floating-point tolerance) for one fixed `(seed, index)`.
- **Scope and constraints:** Retain the existing descriptor-distinctness,
  transparency, radius, uniqueness, and cross-seed assertions. Goldens should
  change only with a deliberate visual/placement contract decision, not merely
  be regenerated to make a changed implementation pass.
- **Remaining uncertainty:** A full raw-pixel golden would be unnecessarily
  brittle and bulky; a stable image hash plus a few semantic pixel assertions
  gives a concise, reviewable oracle.

---

### [#1378] CIT-19. Flora-lifespan determinism test only checks a pure call against itself

The test titled "lifespans are deterministic and within the species range"
evaluates `instanceLifespan` twice for the same species and instance, then
compares the two results. Since the function is pure, that equality is
reflexive. Its remaining range checks prove only that the current result lies
between the perennial species' configured limits.

This misses the important stated contract: a perennial lifetime must derive
from stable placement fields so the same instance survives chunk regeneration,
while distinct instances do not all collapse to one lifetime. A changed or
constant hash/mixing function that remained in the allowed range would keep
this test and the nearby lifecycle tests green, because those lifecycle tests
derive their own timing from the same unchecked value.

**Evidence:**

- `test-headless/Test/Headless/World/FloraGrowth.hs:185-195` binds two
  identical `instanceLifespan berry seedling` calls, compares them, and checks
  only the `[1080, 3600]` range.
- `src/World/Flora/Growth.hs:83-94` specifies that perennial lifespans come
  from a hash of an instance's placement fields, specifically to remain stable
  across chunk regeneration.
- `FloraGrowth.hs:170-184` and `:214-218` obtain their dead/rebirth boundary
  from that same `instanceLifespan` result, so they validate later lifecycle
  transitions but cannot independently detect a changed lifetime mapping.

**Handoff context:**

- **Current behavior:** A constant or incorrectly mixed perennial lifetime
  remains green as long as it stays in the species range and leaves the
  derived dead/reseed arithmetic internally consistent.
- **Expected direction:** Retain the range assertion, then add a small stable
  oracle: recreate one fixture exactly and require its known lifespan, and
  show that one or more placement-field changes produce a distinct expected
  lifetime. Use fixtures chosen to avoid accidental hash collisions.
- **Scope and constraints:** Do not hard-code a value merely to mirror an
  implementation detail without documenting it as a persistence/replay
  contract. The test must continue to cover the existing perennial lifecycle,
  evergreen, annual, and biennial distinctions.
- **Remaining uncertainty:** If exact floating-point goldens are judged too
  brittle, compare a first construction against a separately persisted/rebuilt
  instance and assert an exact expected ordering/difference across named
  fixtures; it still must not derive both sides from the same unverified call.

---

### [#1379] CIT-20. Final-climate determinism test has no independent expected result

The final-climate suite builds the same `ClimateState` twice from identical
pure inputs and compares the two values. That verifies no semantic climate
contract: any changed deterministic algorithm, including a constant or
incorrect regional computation, produces equal values on both sides. The
surrounding forcing-sensitivity, mean-consistency, and initialized-world
wiring examples remain meaningful and should be kept.

**Evidence:**

- `test-headless/Test/Headless/World/Climate.hs:75-80` defines `a` and `b`
  with identical `buildClimateFromOceanSet` applications and asserts `a == b`.
- `src/World/Weather/Generate/ClimateBuilder.hs:20-27` gives
  `buildClimateFromOceanSet` a pure result type and constructs the result from
  its explicit world/ocean/freshwater/forcing inputs.
- `Climate.hs:54-73` independently tests a physically meaningful CO2 response
  and agreement between the summary temperature and its regional grid; `:82+`
  tests the integration path. Those assertions establish real contracts but
  do not make the duplicate-call equality meaningful.

**Handoff context:**

- **Current behavior:** The determinism example remains green for every
  non-throwing deterministic change to climate construction.
- **Expected direction:** Remove the redundant assertion, or replace it with
  a small documented synthetic climate golden (selected regional seasonal
  values plus the summary fields) for the fixture already used by this suite.
- **Scope and constraints:** Retain the forcing, regional-mean, and
  completed-world wiring tests. Do not use a re-invocation of the same builder
  as the expected value for a golden; the oracle must be explicit and reviewed.
- **Remaining uncertainty:** Exact float values may need a stated tolerance;
  a few representative regions and summary values are preferable to freezing
  the whole grid byte-for-byte.

---

### [#1382] CIT-24. Location-placement determinism test only compares pure same-input calls

The guaranteed-placement fixture's test titled "is deterministic" evaluates
the same pure location-placement tuple again and compares the results. Its
additional comparison regenerates plates from the same fixed seed, size, and
count that produced the fixture's original plates. Both routes are pure, so
every non-throwing deterministic change to placement, fallback selection, or
plate generation changes both sides together and remains green.

The surrounding examples already establish the useful #997 contract: the
strict pass is exhausted, fallback is selected, one overlay entry is produced,
the coordinate is canonical land, and definition ordering is respected. This
example therefore adds no independent determinism or placement oracle.

**Evidence:**

- `test-headless/Test/Headless/WorldGen.hs:344-381` defines `gplates` as
  `generatePlates gseed gws 3` and `wetPlacement` as one fixed
  `computeLocationPlacement` invocation through `placeWith`.
- `WorldGen.hs:397-406` invokes that same `placeWith HS.empty allWet [flatDef]`
  again, then regenerates the same `gplates` value with identical arguments
  before comparing only the derived overlays/outcomes.
- `src/Location/Overlay.hs:155-170` defines `computeLocationPlacement` as a
  pure function of its explicit generation and content inputs; `src/World/Plate/Generation.hs:30-32`
  defines `generatePlates` as a pure map over deterministic plate generation.
- `WorldGen.hs:383-395` and `:408-425` contain the fixture's independent,
  behavior-bearing assertions for the fallback outcome, overlay cardinality,
  canonical land, and definition ordering.

**Handoff context:**

- **Current behavior:** The named determinism example detects an exception or
  unexpected hidden effect, but cannot detect deterministic semantic drift.
- **Expected direction:** Delete the duplicate-call comparison, or replace it
  with one documented expected coordinate and outcome for the synthetic tuple
  if that exact seeded fallback selection is a compatibility contract.
- **Scope and constraints:** Retain the strict-exhaustion, one-placement,
  land/canonical-coordinate, dry-preference, and definition-order tests. Do
  not turn an implementation-derived second call into the expected oracle.
- **Remaining uncertainty:** An exact coordinate golden should be used only
  if future worldgen changes are intended to preserve this small synthetic
  tuple's choice; otherwise the surrounding semantic properties are the more
  durable contract and the duplicate example should simply be removed.

---

### [#1383] CIT-25. Location-name determinism test compares the same pure construction twice

The location-naming test titled "the same seed and world produce identical
names every time" invokes `buildLocationInstances` with the exact same
immutable namer, registry, and overlay used to define `builtA`, then compares
their names. `buildLocationInstances` is a pure constructor, so deterministic
changes to instance construction, location-name selection, ordering, or
language rendering alter both sides and leave the example green.

This is a separate location-naming region from the language-generator and
location-placement findings. The suite retains useful checks for native names,
English glosses, per-world language differences, authored naming validation,
and the independent relationship between the placement constructor and
`nameLocationInstance`.

**Evidence:**

- `test-headless/Test/Headless/Location/Naming.hs:82-92` defines `builtA` as
  `buildLocationInstances (Just namerA) registry overlay` from fixed fixture
  data.
- `Location/Naming.hs:127-130` repeats that exact application via
  `namerOf provA` and compares only `namesOf` with `builtA`.
- `src/Location/Instance.hs:342-358` defines `buildLocationInstances` as a
  pure construction from its three explicit inputs.
- `Location/Naming.hs:178-183` already verifies that names allocated by the
  constructor agree with independently invoked `nameLocationInstance` calls
  for the three expected IDs, while `:97-125` and `:132-225` cover the
  remaining stated naming behavior.

**Handoff context:**

- **Current behavior:** The named determinism example can expose an exception,
  but has no independent oracle for any non-throwing deterministic regression.
- **Expected direction:** Remove the duplicate comparison, or pin a concise,
  reviewed name-and-gloss vector for one fixed provenance and overlay if exact
  replay output is intentionally stable across releases.
- **Scope and constraints:** Keep the constructor-to-namer relationship test
  and the existing language, gloss, uniqueness, and authored-scheme coverage.
  Do not derive the expected names by calling the same unverified constructor.
- **Remaining uncertainty:** A golden couples location naming to the current
  language-generator compatibility policy; if names are allowed to evolve,
  deletion is preferable to a brittle self-comparison.

---

### [#1384] CIT-26. Location-instance mapping test derives both sides from the same construction

The location-instance identity test claims that a recomputed placement keeps
the same ID → definition → anchor mapping, but compares a pure
`buildLocationInstances Nothing registry overlay3` call with `instances3`,
which is defined by the exact same call. It therefore does not test the
mapping it names: a deterministic regression assigning the wrong definition
to an otherwise correctly ordered anchor changes both sides together.

This is a separate location-instance allocation/identity region from
location-name rendering and overlay placement. The preceding examples prove
the allocator starts at one and that chunk coordinates are canonicalized into
the expected order, but neither independently checks the definition paired
with each allocated ID.

**Evidence:**

- `test-headless/Test/Headless/Location/Instance.hs:70-84` deliberately
  creates an unordered overlay with two `ruin` entries and one `camp` entry,
  then defines `instances3` from `buildLocationInstances Nothing registry
  overlay3`.
- `Location/Instance.hs:120-138` defines `identityOf` as the ID, definition,
  and chunk triple, but compares a second identical constructor invocation to
  `instances3` rather than to an explicit expected triple list.
- `src/Location/Instance.hs:342-358` shows the constructor is pure and sorts
  overlay entries before assigning IDs.
- `Location/Instance.hs:123-132` independently asserts only `[1,2,3]` and
  the expected chunk order; `:145-151` checks a different partial-registry
  case but does not pin the full fixture's mapping.

**Handoff context:**

- **Current behavior:** The named recomputation test detects an exception but
  has no independent oracle for definition-to-ID association.
- **Expected direction:** Replace it with the explicit fixture mapping:
  `(1, "camp", ChunkCoord (-3) 4)`, `(2, "ruin", ChunkCoord 0 0)`, and
  `(3, "ruin", ChunkCoord 2 (-1))`.
- **Scope and constraints:** Preserve the unordered-overlay fixture, ID-floor
  assertion, coordinate-order assertion, missing-definition reservation, and
  migration coverage. The expected mapping must remain hand-stated, not be
  generated by a second call to the constructor under test.
- **Remaining uncertainty:** None material; the fixture's comments already
  specify this exact canonical mapping as its regression contract.

---

### [#1385] CIT-27. River-name determinism test compares the same pure construction twice

The river-naming example titled "produces the same names every time" repeats
`buildRiverNames` with the same immutable namer and feature-ID list that
define `builtA`, then compares their display names. Because the builder is
pure, deterministic changes to river-name selection, ordering, or rendering
change both values and the test remains green.

This is a separate river-naming region from location naming and generic
language generation. The surrounding suite retains meaningful contracts for
nonempty and distinct names, English glosses, language-root assignment, head
recurrence, feature-ID keying under reordered input, no-language behavior,
persistence, and catalogue drift.

**Evidence:**

- `test-headless/Test/Headless/River/Naming.hs:111-123` defines `builtA` as
  `buildRiverNames (Just namerA) riverIds` from fixed fixture inputs.
- `River/Naming.hs:196-199` invokes the same builder with `namerOf provA` and
  the same `riverIds`, comparing only its names with `builtA`.
- `src/World/River/Naming.hs:195-214` defines `nameRiver` and
  `buildRiverNames` as pure functions of their explicit namer and feature IDs.
- `River/Naming.hs:144-194` and `:201-239` cover the independent behavioral
  contracts that should remain after removing or replacing this assertion.

**Handoff context:**

- **Current behavior:** The example can expose an exception but has no
  independent oracle for deterministic semantic drift.
- **Expected direction:** Delete the duplicate comparison, or pin a compact,
  reviewed name-and-gloss vector for selected fixed feature IDs if exact river
  names are a compatibility contract.
- **Scope and constraints:** Retain the key-by-feature-ID/reordered-input
  test, which is a real order-independence boundary, along with the existing
  language, glossary, persistence, and absent-language coverage. Do not
  produce expected names through another call to the builder under test.
- **Remaining uncertainty:** A golden is appropriate only if river name text
  is intended to remain stable across language-generator changes; otherwise
  deletion is less brittle and the surrounding semantic assertions suffice.

---

### [#1386] CIT-29. River-ID stability assertion repeats the same pure timeline query

The real-worldgen river-identity test makes several meaningful assertions:
the canonical generated world has rivers, every river has a feature ID, and
the IDs are unique. Its final assertion, however, labels a second
`timelineRivers` invocation as stable and compares it with `paired`, which was
defined by the exact same pure call over the same immutable timeline. It adds
no independent ordering or identity contract.

This is a narrow generated-world river-identity cleanup, distinct from the
synthetic river-naming/identity suite. Removing the duplicate preserves the
valuable integration signal that compaction produced identifiable, non-aliased
rivers on actual worldgen output.

**Evidence:**

- `test-headless/Test/Headless/WorldGen.hs:181-204` binds `paired` from
  `timelineRivers (wgpGeoTimeline p)`, checks nonempty/identified/unique
  output, then invokes the same expression again and compares only IDs.
- `src/World/River/Identity.hs:48-75` documents and implements
  `timelineRivers` as a pure query of one `GeoTimeline`.
- `test-headless/Test/Headless/River/Naming.hs:205-245` separately pins the
  identity algorithm against synthetic expected feature IDs, ordering, inactive
  features, and mismatch behavior; this finding does not weaken that coverage.

**Handoff context:**

- **Current behavior:** The final equality only detects an exception or hidden
  impurity; deterministic semantic drift in river pairing remains green.
- **Expected direction:** Delete the duplicate final assertion while retaining
  the generated-world nonempty, all-identified, and uniqueness assertions.
- **Scope and constraints:** Do not remove the test's actual integration
  fixture; it is the CI coverage proving that worldgen compaction produces
  rivers for which the defensive no-ID fallback is not taken.
- **Remaining uncertainty:** None material. A different-input order test
  would not fit this real timeline fixture because period ordering is part of
  the river-stream contract and is correctly owned by the synthetic identity
  suite.

---

### [no-issue] CIT-30. Focus-navigation integration tests restart the engine for nearly every example

> **Disposition:** No issue — #1363 removed the 100 ms teardown delay that made these 36 engine boots costly; the current 50-example suite completes in 0.6955 seconds, so shared-fixture consolidation is not justified by present measurements.

The UI focus-navigation suite has 50 examples and applies
`around withHeadlessEngine` to each of its six integration describes. Every
contained example therefore boots and tears down an engine/world thread,
despite beginning with `resetAll` that already resets the input, UI, focus,
key-binding, action-outcome, and Lua-message state relevant to the fixture.
A focused run logged roughly 36 world-thread starts and took 4.35 seconds;
the harness's mandatory 100 ms teardown delay alone accounts for about 3.6
seconds of that wall-clock cost.

This is the UI keyboard-control-focus/input-routing fixture region, separate
from CIT-15's responsive-menu suite and CIT-5's cross-suite harness finding.
The pure traversal cases correctly avoid engine setup; the integration cases
still need a real engine/Lua boundary but do not inherently need a new one per
example.

**Evidence:**

- `test-headless/Test/Headless/UI/FocusNavigation.hs:186,491,528,709,727,791`
  wrap six integration groups in `around withHeadlessEngine`.
- `FocusNavigation.hs:856-865` defines `resetAll`, called before every
  integration example, which restores the fixture's input/UI/focus/keybinding
  state and drains queued Lua messages.
- `FocusNavigation.hs:932-976` creates fresh bare or fixture Lua backends per
  example, so sharing the engine need not share Lua module state.
- Focused CI-equivalent command
  `cabal test synarchy-test-headless --test-options='--match "UI.FocusNavigation"'`
  completed 50 examples in 4.3482 s and emitted about 36 `Starting world
  thread` lines on 2026-08-18.
- `test-headless/Test/Headless/Harness.hs:39-42` adds the fixed 100 ms worker
  stop grace to every fixture teardown.

**Handoff context:**

- **Current behavior:** Most wall-clock time in this focused suite is repeated
  fixture lifecycle rather than focus-navigation behavior.
- **Expected direction:** Add a shared `aroundAll` engine fixture for the
  integration blocks, retain `resetAll` before each example, and keep a fresh
  Lua backend per case where the test loads or mutates Lua state.
- **Scope and constraints:** Preserve the existing real input-routing,
  engine-to-Lua event, widget-family, detach/delete, and focus-indicator
  coverage. Do not share mutable Lua states across examples merely to remove
  engine boots.
- **Remaining uncertainty:** Before collapsing all six blocks into one shared
  fixture, verify/reset any future test-owned `EngineEnv` fields not covered
  by `resetAll`; current examples use the reset helper consistently and create
  their own Lua backend state.

---

### [no-issue] CIT-31. Control-activation integration tests restart the engine for nearly every example

> **Disposition:** No issue — #1363 removed the 100 ms teardown delay that made these 19 engine boots costly; the current 33-example suite completes in 0.3615 seconds, so shared-fixture consolidation is not justified by present measurements.

The UI control-activation suite applies `around withHeadlessEngine` to its
integration blocks, restarting an engine/world thread for each contained
example. A focused run of 30 examples logged 19 world-thread starts and took
2.29 seconds. The harness's 100 ms teardown grace accounts for about 1.9
seconds before the press/release behavior itself runs.

This is the UI primary-press/release, cancellation, and action-outcome region,
separate from CIT-30's keyboard-control-focus coverage. Every integration
example calls a local `resetAll`, and Lua-facing cases create a fresh backend,
so their required case isolation need not require an engine lifecycle per
example.

**Evidence:**

- `test-headless/Test/Headless/UI/ControlActivation.hs:144,536` wrap the
  engine-input and Lua-facing integration groups in `around
  withHeadlessEngine`.
- `ControlActivation.hs:553-562` defines `resetAll`, called before the
  integration examples, resetting input/UI/focus/keybindings/action outcomes
  and draining Lua messages.
- `ControlActivation.hs:599-606` creates a fresh Lua backend for the
  Lua-facing example.
- Focused CI-equivalent command
  `cabal test synarchy-test-headless --test-options='--match "UI.ControlActivation"'`
  completed 30 examples in 2.2873 s and emitted 19 `Starting world thread`
  lines on 2026-08-18.
- `test-headless/Test/Headless/Harness.hs:39-42` gives every such teardown a
  fixed 100 ms worker-stop grace.

**Handoff context:**

- **Current behavior:** Fixture lifecycle dominates this focused suite's
  runtime, while useful activation behavior receives relatively little of its
  wall-clock budget.
- **Expected direction:** Introduce a shared `aroundAll` engine fixture for
  the integration blocks; retain `resetAll` before each case and fresh Lua
  state where a test uses the Lua API.
- **Scope and constraints:** Retain the existing ordinary click,
  drag-activation, multi-button, focus-loss, restored-before-release,
  sibling-churn, no-op epoch, and action-outcome coverage. Do not share a Lua
  state across cases merely to reduce engine boots.
- **Remaining uncertainty:** As with CIT-30, audit any newly added
  `EngineEnv` state against `resetAll` before extending a shared fixture; the
  current integration examples consistently reset the state they exercise.

---

### [no-issue] CIT-32. Lua text-contract tests boot a full engine for each Lua assertion

> **Disposition:** No issue — #1363 removed the 100 ms teardown delay that made these 41 engine boots costly; the three current text-focused runs total 47 examples and 0.8197 seconds of Hspec time, so shared-engine refactoring is not justified by present measurements.

The CI text/Unicode Lua suites use `around withHeadlessEngine` per example
while creating a fresh Lua backend and deterministic resource/measurement
stubs inside each test. The wrapping and character-truncation suites ran 16
examples with 14 world-thread starts in 1.69 seconds; width truncation ran 15
examples with 15 starts in 1.44 seconds. Shell input has a further 11
engine-wrapped editing/completion examples, each with its own Lua backend and
explicit UI/focus reset. About forty worker lifecycles are spent on
deterministic Lua text behavior, with the harness's fixed teardown grace alone
adding roughly four seconds.

This is the UI text/Unicode behavior region, distinct from keyboard focus
(CIT-30) and press/release activation (CIT-31). The tests need the Lua API and
fresh Lua state, but their stated contracts do not require a fresh world worker
for every assertion.

**Evidence:**

- `test-headless/Test/Headless/Lua/TextWrapping.hs:28-29`,
  `TextTruncation.hs:34`, `WidthTruncation.hs:37-38`, and
  `ShellInput.hs:38-39` apply `around withHeadlessEngine` to their behavioral
  examples.
- Their helpers create fresh Lua states per case: `TextWrapping.hs:117-133`,
  `TextTruncation.hs:159-181`, `WidthTruncation.hs:243-269`, and
  `ShellInput.hs:222-260`; each supplies deterministic width/resource stubs.
- `ShellInput.hs:223-225` additionally resets the UI and focus managers before
  creating its backend.
- Focused CI-equivalent runs on 2026-08-18:
  `--match "Lua.Text"` completed 16 examples in 1.6854 s with 14 starts;
  `--match "Lua.WidthTruncation"` completed 15 examples in 1.4441 s with 15
  starts.
- `test-headless/Test/Headless/Harness.hs:39-42` applies a fixed 100 ms grace
  at every engine fixture teardown.

**Handoff context:**

- **Current behavior:** Fixture lifecycle, rather than the text behavior under
  test, dominates several compact deterministic Lua suites.
- **Expected direction:** Share one engine per text suite with `aroundAll`,
  retain a fresh Lua backend per example, and reset the small EngineEnv surface
  each suite exercises before every case.
- **Scope and constraints:** Preserve real module loading, UTF-8-validating
  nonzero text-width stubs, byte-fragment rejection, width-call accounting,
  and source-delegation audits. Do not share a Lua state or weaken the stubs to
  eliminate setup.
- **Remaining uncertainty:** TextWrapping/TextTruncation/WidthTruncation do
  not presently centralize their minimal environment reset as ShellInput does;
  introduce that reset first and verify it owns all mutable fields touched by
  their Lua API calls before using a shared engine.
