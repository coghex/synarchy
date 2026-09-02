# CI stdout and logging hygiene findings

This report inventories successful-path stdout that currently makes CI logs harder to scan, and separates runner presentation noise from runtime messages that should be silent or available only through an explicit diagnostic category. It preserves warnings, errors, protocol output, and concise gate summaries as useful signal.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

## Methodology

The audit followed CI commands from `.github/workflows/ci.yml` and `tools/ci-local.sh` into the Hspec runner, Python audit self-tests, module-budget tools, the headless engine fixture, the Haskell logger, and the Lua logging API. It searched the complete tracked Haskell, Lua, Python, workflow, and recent measurement-artifact surfaces for unconditional successful-path output and Info-level diagnostics.

Two proportionate observations were used instead of a full CI run: Hspec's dry-run enumerated 4,523 examples, and the focused nine-example `Input.Followup` group demonstrated that a normal fixture allocation also prints three engine Info lines. Static call-site counts provide scale for the Python self-tests and module-budget tools. A recent tracked concurrency artifact provides an independent real-run classification of runtime boot noise. No full headless suite, full probe sweep, or `make ci` was run, and no GitHub duplicate search was performed; later `process-report` runs own tracker deduplication and disposition.

## Status

- [x] STDOUT-1. Headless CI prints every passing Hspec example — [#1916]
- [x] STDOUT-2. Cabal build progress grows linearly with the project — [#1920]
- [x] STDOUT-3. Audit companion tests print every successful assertion — [#1922]
- [x] STDOUT-4. Probe-orchestration self-tests print every successful assertion — [#1922]
- [x] STDOUT-5. Module-budget guards print every passing file — [#1924]
- [x] STDOUT-6. Headless fixtures inherit the production stdout logger — [#1925]
- [x] STDOUT-7. The parity spec prints its passing diagnostic unconditionally — [#1926]
- [x] STDOUT-8. Notification-registry success is logged on every engine allocation — [#1928]
- [x] STDOUT-9. Content loading reports the same success at multiple layers — [#1930]
- [x] STDOUT-10. Lua module lifecycle boilerplate is logged at Info — [#2174]
- [x] STDOUT-11. Worldgen tectonic and climate banners are ordinary Info logs — [#1933]
- [x] STDOUT-12. Worker and engine startup emit paired lifecycle lines — [#1934]
- [x] STDOUT-13. Internal Lua state and action telemetry bypasses Debug — [#1935]

---

## CI runner and gate presentation

### [#1916] STDOUT-1. Headless CI prints every passing Hspec example

The headless suite uses Hspec's default formatter while both CI entry points request direct test output. That combination prints the description of every successful example, so the log's routine success volume grows with a suite that already contains thousands of examples and visually separates failures from their surrounding context.

**Evidence:**

- `test-headless/Spec.hs:225` — the suite ends with plain `hspec specs`, with no CI-specific formatter selection.
- `.github/workflows/ci.yml:480-483` — CI invokes the headless suite with `--test-show-details=direct`, forwarding the formatter output into the job log.
- `tools/ci-local.sh:98` — the local parity gate uses the same direct-output setting.
- A read-only Hspec dry-run with the current test binary enumerated 4,523 examples; that is the approximate successful-example line surface before runtime logs and Cabal framing are counted.

**Handoff context:**

- **Current behavior:** A successful full suite emits one human-readable description for essentially every example.
- **Expected direction:** Routine CI should use a compact success presentation while retaining complete failure names, diagnostics, and a final example/failure summary; detailed per-example output should remain opt-in.
- **Scope and constraints:** Keep CI and `make ci` presentation aligned. This finding concerns Hspec formatting, not suppressing application logs captured during a failed example.
- **Remaining uncertainty:** The preferred compact Hspec formatter and whether CI should attach a full verbose transcript need product/maintainer choice.

### [#1920] STDOUT-2. Cabal build progress grows linearly with the project

CI's warning-clean builds and test invocation use Cabal's normal verbosity. On a cold or invalidated build, Cabal prints the build plan and a numbered compile line for every module; the amount of successful-path output therefore scales with a library that already has hundreds of modules.

**Evidence:**

- `.github/workflows/ci.yml:444-453` — the warning-clean library, executable, graphical-test, and headless-test builds are invoked without a quieter Cabal verbosity setting.
- `.github/workflows/ci.yml:480-483` — the headless `cabal test` invocation also uses Cabal's normal command presentation around direct test output.
- `tools/ci-local.sh:79-98` — the local gate mirrors those ordinary-verbosity build and test commands.
- During the focused headless observation, invalidated build state produced 219 numbered headless-module compile lines before the nine selected examples ran.

**Handoff context:**

- **Current behavior:** Recompilation floods the job log with successful module progress before the tests and audits that engineers usually need to inspect.
- **Expected direction:** CI should retain compiler diagnostics and useful command summaries while collapsing routine build-plan and per-module success progress; a verbose reproduction path should remain available.
- **Scope and constraints:** Warning visibility is load-bearing because the project deliberately builds with `-Werror`. CI/local command parity and the usefulness of cold-build diagnostics must be preserved.
- **Remaining uncertainty:** Cabal verbosity should be validated on both success and a representative compile failure before standardizing the exact flag.

### [#1922] STDOUT-3. Audit companion tests print every successful assertion

The Python audit companion tests use `expect` helpers that print an `OK:` record for every passing case. The main audit self-test runs dozens of groups, and the wider companion corpus contains hundreds of direct success-printing assertions, turning a passing invariant check into a long assertion transcript rather than a concise summary.

**Evidence:**

- `tools/test_audit.py:389-395` — the shared `expect` helper prints `OK: <message>` whenever a condition passes.
- `tools/test_audit.py:1755-1820` — `main` invokes 51 named test groups and then prints a summary, so the per-assertion lines are additional successful-path noise rather than the only proof of completion.
- A static search of the audit companion scripts found roughly 673 direct `expect(...)` call sites after helper definitions were excluded, establishing that the pattern is systemic rather than isolated to one test.
- `tools/test_pack_atlas.py:2454-2508` — a neighboring large Python self-test already demonstrates the desired shape: quiet successful assertions by default, failure details, an optional verbose mode, and one final summary.

**Handoff context:**

- **Current behavior:** Passing audit self-tests print each assertion and then print an overall success summary.
- **Expected direction:** Default successful runs should report one concise per-tool summary, with assertion-by-assertion success output available behind a verbose/debug option and failure diagnostics always visible.
- **Scope and constraints:** Apply a consistent contract across audit companion tests without weakening assertion coverage, exit codes, or failure messages. Avoid a mechanical rewrite that makes failures harder to localize.
- **Remaining uncertainty:** The exact script inventory and whether a shared helper can provide the policy should be re-established when this finding is processed.

### [#1922] STDOUT-4. Probe-orchestration self-tests print every successful assertion

The probe runner, census, claim, and deflake self-tests repeat the same success-printing `expect` pattern at still larger scale. CI runs these tools as gate self-tests, so their internal assertion narration competes with the actual failure and selection information that the probe infrastructure is meant to expose.

**Evidence:**

- `.github/workflows/ci.yml:750-763` — CI runs the probe orchestration, persistence sweep, claim, census, and deflake self-tests as a group.
- `tools/ci-local.sh:227-239` — the local CI parity script runs the corresponding self-tests.
- `tools/test_run_probes.py:70`, `tools/test_probe_census.py:58`, `tools/test_probe_claim.py:62`, and `tools/test_deflake.py:80` — representative helpers print `OK:` for every successful expectation.
- A static search across the probe-orchestration companion tests found roughly 1,070 direct `expect(...)` call sites, before any subprocess output they intentionally exercise is included.

**Handoff context:**

- **Current behavior:** A healthy orchestration gate emits hundreds of assertion-level success messages across several Python programs.
- **Expected direction:** Each self-test should be concise by default, retain full failure context, and expose detailed success narration only through a deliberate verbose/debug mode.
- **Scope and constraints:** Do not suppress probe subprocess output that a test is explicitly validating, and preserve the distinction between self-test presentation and real probe execution logs.
- **Remaining uncertainty:** Some self-tests may rely on captured stdout as part of their contract; processing should classify those cases before centralizing the helper behavior.

### [#1924] STDOUT-5. Module-budget guards print every passing file

Both module-budget gates print one success line for every governed module and then print a summary. These are ratchet checks whose useful successful result is the aggregate pass; enumerating every file makes routine output grow whenever another module family is brought under a budget.

**Evidence:**

- `tools/lua_module_budget.py:50-69` — the Lua guard prints an `OK:` line for every checked module before its final summary.
- `tools/haskell_module_budget.py:50-69` — the Haskell guard has the same per-module success presentation.
- The current inventories produce 71 Lua and 17 Haskell passing-file lines, for 88 lines before their two summaries.

**Handoff context:**

- **Current behavior:** Passing CI repeats the path, actual line count, and budget for every governed module.
- **Expected direction:** Default output should summarize the number of checked modules and print individual entries only for violations or explicit verbose/debug requests.
- **Scope and constraints:** Preserve exact over-budget diagnostics and the guards' exit behavior. A concise mode should not change which module families are governed or how counts are computed.
- **Remaining uncertainty:** None at draft time.

---

## Headless test-owned output

### [#1925] STDOUT-6. Headless fixtures inherit the production stdout logger

The ordinary headless test harness initializes each engine with the production logger, whose default backend writes Info and above to stdout. Tests that do not care about logs therefore inherit runtime boot chatter automatically, and suites with many independent engine allocations multiply it.

**Evidence:**

- `src/Engine/Core/Log/Types.hs:140-149` — `defaultLoggerConfig` enables logging to stdout at `LevelInfo`.
- `src/Engine/Core/Init.hs:360-368` — `initializeEngineHeadless` delegates to the default logger configuration, while the adjacent backend-aware initializer already provides an injection boundary.
- `test-headless/Test/Headless/Harness.hs:225` — the shared harness calls `initializeEngineHeadless`, so most fixtures do not explicitly choose their output policy.
- `test-headless/Test/Headless/Harness.hs:240` — the harness documents approximately 270 engine boots across the suite, making even a few unconditional boot lines material.
- A focused nine-example `Input.Followup` run printed `Notification registry loaded`, `Starting world thread`, and `World thread started` even though the selected assertions did not concern logging.

**Handoff context:**

- **Current behavior:** Every normal headless engine allocation may write unrelated production Info messages into the test runner's stdout.
- **Expected direction:** Test fixtures should be quiet by default and allow an individual test or diagnostic run to opt into a capturing or stdout backend at a chosen level/category.
- **Scope and constraints:** Tests that assert log behavior need an explicit capture path. Failure-time diagnostics should remain recoverable, and production initialization semantics must not be changed accidentally.
- **Remaining uncertainty:** Whether quiet fixtures should discard logs, buffer them for failure reporting, or route them through Hspec is a design choice for processing.

### [#1926] STDOUT-7. The parity spec prints its passing diagnostic unconditionally

One world-generation parity test writes its calculated comparison directly to stdout before making its assertions. The values are valuable when the assertion fails, but on success the line is an implementation diagnostic embedded in the middle of the test formatter's output.

**Evidence:**

- `test-headless/Test/Headless/WorldGen/Parity.hs:88-95` — the example calls `putStrLn` with the fast/reference water totals and relative difference unconditionally.
- `test-headless/Test/Headless/WorldGen/Parity.hs:97-101` — the actual expectations follow the print, so Hspec can already identify a failure and can carry the values in assertion context instead of requiring successful-path stdout.

**Handoff context:**

- **Current behavior:** Every execution of the parity example prints numeric diagnostics even when both expectations pass.
- **Expected direction:** Attach the calculated values to failure context or place their successful display behind an explicit test-verbosity/debug switch.
- **Scope and constraints:** Preserve the numbers needed to diagnose tolerance regressions and do not make the expensive world-generation case harder to reproduce.
- **Remaining uncertainty:** None at draft time.

---

## Runtime boot and load logging

### [#1928] STDOUT-8. Notification-registry success is logged on every engine allocation

Notification registry construction emits an Info message after normal successful loading. Because the registry is part of engine initialization, the same low-information confirmation appears in headless tests, probes, and application boots even though callers already learn about failure through the error path.

**Evidence:**

- `src/Engine/Asset/YamlNotifications.hs:102-121` — registry loading performs validation and then logs `Notification registry loaded` at Info on success.
- The focused headless observation printed this message once for the fixture allocation before any selected test output related to notifications.
- `docs/measurements/probe_concurrency_1427/artifacts/role-20260822T041947Z-8708-a9bd1213__run-001.txt:9-15` — the recorded probe artifact explicitly classifies deterministic boot noise as something elided from the useful transcript.

**Handoff context:**

- **Current behavior:** A fixed success sentence is emitted for every registry load, regardless of whether registry detail is relevant to the run.
- **Expected direction:** Keep validation failures prominent, but make ordinary registry success silent or available only through an asset/initialization debug category.
- **Scope and constraints:** This should not hide malformed YAML, duplicate registration, missing-resource, or other actionable failures.
- **Remaining uncertainty:** A useful structured success metric, such as the loaded entry count, may justify debug-level retention but was not assessed here.

### [#1930] STDOUT-9. Content loading reports the same success at multiple layers

Several asset families log successful loading both in the Haskell/YAML owner and in the Lua loader that consumes or registers the same content. A normal boot can therefore report one logical operation twice, often once as a count and again as a lifecycle sentence, without adding actionable information at Info level.

**Evidence:**

- `src/Engine/Asset/YamlTextures.hs:130` — the Haskell texture loader emits an Info-level success count.
- `scripts/material_loader.lua:13-24` — the Lua material loader emits start and completion Info messages around its content registration.
- `src/Item/Defs.hs:101` — item-definition loading emits its own Info-level success report.
- `scripts/building_loader.lua:13-25` — the building-side Lua loader also emits start and completion Info messages for routine successful loading.
- The recent concurrency artifact groups 98 asset/script lines into the deterministic boot noise it elides, confirming the aggregate cost rather than only isolated call sites.

**Handoff context:**

- **Current behavior:** Successful content initialization is narrated at multiple abstraction layers, producing repeated start/count/complete messages on every boot.
- **Expected direction:** Give each logical load one owner for concise Info-level reporting, if any, and move layer-internal details to appropriate asset or Lua debug categories.
- **Scope and constraints:** Preserve errors, validation failures, missing references, and counts that are genuinely needed as a user-facing boot health signal. The finding is about duplicate success narration, not removing observability.
- **Remaining uncertainty:** Each asset family's intended ownership boundary should be mapped during processing; the examples establish a category, not an exhaustive loader list.

### [#2174] STDOUT-10. Lua module lifecycle boilerplate is logged at Info

The main Lua initializer loads a large module graph whose components frequently announce routine `load`, `initialize`, and `ready` transitions with `engine.logInfo`. The Lua logging API maps all of those calls to the single `CatLua` Info stream even though it already exposes a debug-level operation, so normal startup is dominated by deterministic lifecycle boilerplate.

**Evidence:**

- `scripts/init_loader.lua:73-279` — the initializer requires and starts dozens of modules during every normal Lua boot.
- A corpus search found at least 50 obvious lifecycle-shaped `engine.logInfo` calls in Lua modules, including loaded/initialized/ready/start/complete messages.
- `src/Engine/Scripting/Lua/API/Log.hs:27-42` — Lua's `logInfo` binding emits at Info under `CatLua`, without a more specific subsystem category.
- `src/Engine/Scripting/Lua/API/Log.hs:84-101` — Lua already has a separate debug binding, so routine lifecycle details have an existing lower-severity destination.
- `docs/measurements/probe_concurrency_1427/artifacts/role-20260822T041947Z-8708-a9bd1213__run-001.txt:50` — 130 of 147 captured lines were elided as deterministic boot noise, including 98 asset/script lines.

**Handoff context:**

- **Current behavior:** Normal Info logs narrate much of the deterministic Lua module graph on every engine boot.
- **Expected direction:** Retain a small, intentional set of high-level readiness milestones at Info and move module-internal lifecycle details behind Debug and, where useful, narrower categories.
- **Scope and constraints:** Warnings, errors, and messages that mark externally observable server/application readiness are not candidates for blanket demotion. `ENGINE_DEBUG` category reachability must be considered before depending on a new or existing category.
- **Remaining uncertainty:** The exact allowlist of meaningful Info milestones needs per-call-site review; this report deliberately identifies the category rather than pre-dispositioning every Lua log call.

### [#1933] STDOUT-11. Worldgen tectonic and climate banners are ordinary Info logs

World generation emits multi-line tectonic and weather summaries through the normal Info logger as well as through the generation-log channel. These deterministic banners account for a substantial block of every relevant probe or headless run even when the caller did not request world-generation diagnostics.

**Evidence:**

- `src/World/Thread/Command/Init.hs:156-186` — the world initializer formats plate and weather summary lines, logs each with `logInfo`, and also forwards each through `sendGenLog`.
- `docs/measurements/probe_concurrency_1427/artifacts/role-20260822T041947Z-8708-a9bd1213__run-001.txt:50` — the artifact classifies 32 worldgen lines as deterministic boot noise within the 130 elided lines.

**Handoff context:**

- **Current behavior:** Every world initialization publishes verbose geological/climate summaries to ordinary stdout and to the specialized generation-log path.
- **Expected direction:** Keep the detailed summaries in the explicit generation/debug channel and make normal Info output limited to a concise milestone, if one is operationally useful.
- **Scope and constraints:** Preserve generation progress consumers and any protocol/UI surface that relies on `sendGenLog`. Only the duplicate ordinary Info emission is clearly in scope here.
- **Remaining uncertainty:** Whether any external automation parses the human-readable Info banner should be checked before changing it.

### [#1934] STDOUT-12. Worker and engine startup emit paired lifecycle lines

The generic worker primitive emits both `Starting …` and `… started` at Info, while top-level headless lifecycle code adds its own engine start/shutdown messages. These pairs are useful during a startup race investigation but are repetitive in healthy runs, especially when several workers and many short-lived fixture engines are involved.

**Evidence:**

- `src/Engine/Core/Thread.hs:143-150` — the shared worker startup path logs both a pre-start and post-start Info message for every worker.
- `app/App/Headless.hs:57-64` — headless application orchestration adds engine-start and shutdown Info messages around the worker lifecycle.
- `docs/measurements/probe_concurrency_1427/artifacts/role-20260822T041947Z-8708-a9bd1213__run-001.txt:35-43` — the captured run shows repeated worker `Starting`/`started` pairs alongside the engine start message.
- The focused headless fixture observation printed both `Starting world thread` and `World thread started` before the selected assertions.

**Handoff context:**

- **Current behavior:** Healthy worker initialization spends two Info lines per thread, plus higher-level engine lifecycle lines.
- **Expected direction:** Normal output should expose only lifecycle events with operational value; detailed transition pairs should be behind a thread/lifecycle debug category or collapsed into one concise milestone.
- **Scope and constraints:** Startup failures, timeouts, abnormal termination, and user-facing readiness signals must remain prominent. Thread-race diagnostics need an easy opt-in path.
- **Remaining uncertainty:** Which single event best represents readiness differs between worker creation and application/server readiness and should be decided at their respective ownership layers.

---

## Runtime diagnostic telemetry

### [#1935] STDOUT-13. Internal Lua state and action telemetry bypasses Debug

Several Lua UI and gameplay modules log mutable state snapshots and frequent user actions with `engine.logInfo`. These messages are diagnostics rather than application milestones, and some can occur once per edit or placement, so an ordinary interactive or automated run accumulates data that belongs behind explicit debug selection.

**Evidence:**

- `scripts/create_world_menu.lua:1095-1102` — world-creation setup emits five Info lines describing internal state in one diagnostic block.
- `scripts/settings/graphics_tab.lua:297-350` and `scripts/settings/graphics_tab.lua:812-814` — pending graphics changes and textbox values are written at Info while the user edits settings.
- `scripts/build_tool.lua:920-925` and `scripts/build_tool.lua:992` — build placement/action details are logged at Info on interactive paths.
- `scripts/pause.lua:105` — pause state is reported with an Info log for a routine input action.
- `src/Engine/Scripting/Lua/API/Log.hs:27-42` and `src/Engine/Scripting/Lua/API/Log.hs:84-101` — these sites use the broad Lua Info stream even though a Debug operation exists.

**Handoff context:**

- **Current behavior:** Internal UI state, field values, and frequent actions appear in normal stdout without the user enabling diagnostics.
- **Expected direction:** Treat state snapshots and per-action telemetry as Debug, ideally under categories that can be enabled for the affected subsystem, while reserving Info for durable application milestones.
- **Scope and constraints:** Review values for sensitivity before retaining them even at Debug. Error reporting and explicit user-visible command results are outside this demotion category. An existing project-review finding already notes that `ENGINE_DEBUG` cannot currently enable every declared category, so category reachability may be a dependency rather than something to duplicate here.
- **Remaining uncertainty:** Some individual lines may have been serving as temporary probes and can be removed outright; others may be worth retaining behind Debug. Processing should decide per cluster.
