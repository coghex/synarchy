# Python testing infrastructure findings

This report records correctness and operational risks in the Python tooling
that selects, runs, and validates Synarchy's tests and behavior probes. It is
an evidence handoff for later one-at-a-time processing, not an issue backlog or
implementation plan.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The audit inventoried all 152 Python files under `tools/`, including the 84
entries registered with the aggregate probe runner. It concentrated on
false-green paths in CI selectors and audit scripts, self-test completeness,
probe selection and teardown, parallel-run isolation, and representative probes
that mutate repository-relative state.

The owning Haskell paths were inspected when necessary to verify what a Python
selector or probe actually covers. In particular, the dump pipeline was traced
through synchronous simulation settling and world-thread writeback rather than
assuming that `src/World/Generate*` contains every input to baseline output.

Focused non-engine verification included:

- strict Python syntax compilation of `tools/` and `debug-console.py`;
- invoking `--help` on all 84 registered probes and checking their `--port`
  interface;
- `tools/ci_probes.py --self-test`;
- `tools/ci_expensive_gates.py --self-test`;
- all 35 groups in `tools/test_audit.py`;
- the current `tools/lua_duplicate_function_audit.py`;
- isolated selector, missing-baseline, classification, and command-line
  reproductions using temporary files or imported functions.

Existing reports were checked for overlap. The known `probelib` duplication is
already CH-129 / issue #1160; tooling size and directory-layout concerns were
disposed as no-issue in CH-130 through CH-132; and BUG-5 / issue #1192 already
covered the earlier strict Python warning failure. Those concerns are not
repeated here. No GitHub duplicate search was performed; that belongs to
`process-report`.

No engine, graphical process, world dump, real parallel config-probe run, full
Haskell test suite, probe sweep, baseline capture, or `make ci` was run. The
parallel config collision was not exercised because its subjects include
tracked configuration files and an intentional race could damage the primary
checkout.

## Status

- [x] PYT-1. The PR worldgen selector skips the dump's simulation and writeback stages — [#1318]
- [x] PYT-2. Missing world baselines are reported as skips but return success — [#1319]
- [x] PYT-3. New world-audit categories can bypass the intended classification gate — [#1320]
- [x] PYT-4. Exact probe selection silently drops unknown requested keys — [#1321]
- [x] PYT-5. Parallel config probes race on the same tracked files — [#1322]
- [x] PYT-6. Unexpected probe failures can leave engine descendants running — [#1323]
- [x] PYT-7. The Lua duplicate-function guard can pass after losing its scan scope — [#1324]

---

## World-regression gates

### [#1318] PYT-1. The PR worldgen selector skips the dump's simulation and writeback stages

The path selector for the quick worldgen regression gate assumes that simulation
and world-thread plumbing cannot change bare dump output. The dump path actually
runs the simulation's fast-settle operation, waits for its writebacks to be
applied by the world thread, and only then reads the tiles used for the
baseline.

A pull request that changes this simulation or writeback code can therefore
alter baseline-observed terrain, fluid, rendered surface, or decorations while
the PR's `world_check --quick` gate is skipped. Pushes to master run the gate as
a post-merge backstop, but that detects the regression only after the change has
already merged.

**Evidence:**

- `tools/ci_expensive_gates.py:16` — the worldgen patterns include generation
  families but no `src/Sim/*` paths.
- `tools/ci_expensive_gates.py:21` — the selector explicitly excludes
  `src/World/Thread/*` on the premise that it cannot shift dump output.
- `.github/workflows/ci.yml:219` — pull requests derive expensive-gate
  eligibility solely from this path selector.
- `.github/workflows/ci.yml:421` — `world_check --quick` runs only when the
  selector returns true.
- `app/App/Dump.hs:146` — dump mode synchronously requests
  `SimFastSettleAll` before reading tile data.
- `src/Sim/Thread.hs:283` — fast settling simulates every loaded world, marks
  every chunk dirty, emits writebacks, and waits for the world thread to apply
  them.
- `src/Sim/Thread.hs:363` — those writebacks contain fluid, terrain, rendered
  surface, and side-decoration state.
- `src/World/Thread/Command.hs:242` — the world thread overwrites the dump's
  live chunk fields before acknowledging completion.
- `tools/ci_expensive_gates.py:52` — the selector self-test has no positive
  cases for these post-generation dump dependencies.

Focused selector calls returned `false` for `src/Sim/Thread.hs`,
`src/Sim/Fluid/Active.hs`, `src/World/Thread/ChunkLoading.hs`, and
`src/World/Thread/Command.hs`.

**Handoff context:**

- **Current behavior:** PRs touching code that participates directly in the
  final dump state can skip the PR-time worldgen regression gate.
- **Expected direction:** PR gate eligibility should include every production
  stage that can affect the data observed by `App.Dump`.
- **Scope and constraints:** Preserve deliberate negative cases for unrelated
  save, designation, and gameplay code rather than broadening the selector to
  all of `src/World/`. Add positive self-test cases for the simulation settle
  and world-thread writeback boundaries.
- **Remaining uncertainty:** No deliberate output mutation was made to prove a
  resulting baseline delta; that would require an expensive worldgen-output
  change. The selector/dataflow mismatch itself is directly verified.

### [#1319] PYT-2. Missing world baselines are reported as skips but return success

`world_check` classifies a selected seed with no baseline as `SKIP`, never runs
its dump, and then exits successfully as long as no result has status `FAIL`.
This makes a deleted baseline—or a seed added to the quick set without its
baseline—a green regression check covering fewer worlds than requested.

Because changes under `tools/baselines/` select the PR gate, deleting a baseline
can trigger `world_check --quick`, have that seed skipped, and still satisfy the
blocking check.

**Evidence:**

- `tools/world_check.py:220` — a missing baseline changes the seed result to
  `SKIP` and returns before executing the dump.
- `tools/world_check.py:390` — the summary counts skipped results but does not
  convert them into failures.
- `tools/world_check.py:404` — process success depends only on the number of
  `FAIL` results.
- `tools/ci_expensive_gates.py:34` — baseline-file changes select the worldgen
  gate.
- `.github/workflows/ci.yml:421` — CI treats the resulting command as a
  blocking gate.
- `tools/test_audit.py:687` — the current world-check logic tests cover summary
  comparison and determinism but not missing-baseline status or `main()`'s exit
  code.

A focused run with a temporary one-seed quick configuration and no matching
baseline printed `Summary: SKIP=1 (total: 1)` and exited with status 0 without
starting an engine.

**Handoff context:**

- **Current behavior:** A requested seed can receive no regression coverage
  while the world-check command reports success.
- **Expected direction:** A selected seed without its required tracked baseline
  should make the regression gate fail clearly.
- **Scope and constraints:** Cover both quick and full seed selections. If a
  local exploratory skip remains useful, it should require an explicit mode
  that CI does not use.
- **Remaining uncertainty:** None at draft time; the zero exit status was
  reproduced through the real CLI.

**Resolved in #1319.** The evidence above describes the behavior as it stood
when this finding was drafted, and the line numbers are those of that revision.
`world_check.py` now records the baseline path each skipped seed expected and
exits 1 whenever any selected seed carries one, naming every such seed and its
expected file rather than stopping at the first. The externally visible `SKIP`
disposition and the summary line are unchanged. `--allow-missing-baselines`
restores the tolerant exit for local exploratory runs and narrows nothing else,
so an ordinary `FAIL` still exits 1 under it; neither
`.github/workflows/ci.yml` nor `tools/ci-local.sh` passes it. Engine-free cases
in `tools/test_audit.py` drive the real `main()` across the unfiltered,
`--quick` and `--seed N` selections and pin both bad-selection paths at their
existing exit 2.

### [#1320] PYT-3. New world-audit categories can bypass the intended classification gate

The audit's test claims that every category produced by `ALL_CHECKS` must be
classified as either a bug or a quality metric, but its category inventory is a
second manually maintained set. Adding or misspelling a production category
does not make that test discover the new value.

Production then defaults any unknown category to quality severity with an
implicit threshold of 1000. A new category present in a freshly captured
baseline can therefore pass deterministic comparison without ever being added
to either classification set or receiving an intentional threshold. The same
unknown category also passes directly on the non-strict path when its count is
below 1000.

**Evidence:**

- `tools/world_audit.py:1079` — `severity_of` returns `QUALITY` for every
  category not found in `BUG_CATEGORIES`.
- `tools/world_check.py:181` — an unknown quality category receives the
  fallback threshold 1000.
- `tools/test_audit.py:415` — the purported classification-coverage test uses
  a hardcoded `every_cat` set rather than deriving categories from
  `ALL_CHECKS` or observed issue emissions.
- `tools/test_audit.py:433` — only entries present in that manually maintained
  set are checked against the classification sets.
- `tools/test_audit.py:443` — threshold completeness likewise begins from
  `QUALITY_CATEGORIES`, so it cannot detect a category missing from that set.
- `tools/world_audit.py:916` — `ALL_CHECKS` is the actual production check
  registry, but the classification test does not inspect it.

A focused call using synthetic category `NEW_CORRUPTION` with count 1 returned
`PASS` with no failures or notes on the non-strict path. It also returned
`PASS` in strict mode when the freshly captured baseline contained the same
count.

**Handoff context:**

- **Current behavior:** A new or mistyped category can receive implicit
  non-failing semantics while the test named for classification coverage still
  passes.
- **Expected direction:** An audit category should remain invalid until its bug
  or quality classification—and, for quality metrics, its threshold—is
  declared explicitly.
- **Scope and constraints:** Keep deterministic baseline comparison and the
  deliberate distinction between bugs and quality metrics. Add a test that
  injects an unknown category and derives or otherwise validates the complete
  production category inventory without another silent manual fallback.
- **Remaining uncertainty:** All categories emitted by the current checked-in
  audit appear classified. This is a verified fail-open extension path, not an
  already-unclassified live category.

---

## Probe orchestration and isolation

### [#1321] PYT-4. Exact probe selection silently drops unknown requested keys

The aggregate runner's `--exact` mode converts requested keys to a set and
returns the intersection with its registry. It does not report unmatched
tokens when at least one valid token remains. The command then runs or lists
that subset and exits successfully.

This is especially misleading in exact mode, whose callers are expressing a
specific set of probes rather than performing an exploratory substring search.
A typo can make an expected probe disappear while logs and exit status imply
that the request succeeded.

**Evidence:**

- `tools/run_probes.py:295` — `select` parses every comma-separated requested
  token.
- `tools/run_probes.py:299` — exact mode retains registered matches without
  calculating unmatched keys.
- `tools/run_probes.py:363` — the CLI describes `--only` as a comma-separated
  requested selection.
- `tools/run_probes.py:365` — `--exact` promises exact probe-key semantics.
- `tools/run_probes.py:392` — the command rejects the selection only when no
  probe matched.
- `tools/run_probes.py:397` — listing a partial match returns success.

`python3 tools/run_probes.py --only craft,not_a_probe --exact --list` listed
only `craft_probe.py` and exited with status 0.

**Handoff context:**

- **Current behavior:** A mixed valid/invalid exact request silently omits the
  invalid entries and reports success for the remainder.
- **Expected direction:** Exact mode should reject every unknown requested key
  and identify it before running any probe.
- **Scope and constraints:** Preserve the deliberately permissive substring
  behavior of non-exact selection. Keep the existing empty-selection error and
  the current `ci_probes.py` registry validation.
- **Remaining uncertainty:** The checked-in CI mapping currently validates its
  keys separately, so the clearest present exposure is manual use or other
  automation calling `run_probes.py` directly.

### [#1322] PYT-5. Parallel config probes race on the same tracked files

The runner treats distinct processes, ports, and save names as sufficient
parallel isolation. Two registered config probes instead manipulate the same
repository-relative configuration paths. Both move, delete, recreate, and
restore the three tracked legacy files, but use separate backup directories and
have no shared lock.

Selecting `config` with multiple jobs can schedule these probes together. One
probe can remove files while the other is checking or booting against them, and
their independent cleanup blocks can delete or restore state belonging to the
other run. At minimum this produces timing-dependent failures; adverse
interleavings can leave the primary checkout's tracked configuration disturbed.

**Evidence:**

- `tools/run_probes.py:84` — `config_migration` is registered independently.
- `tools/run_probes.py:87` — `config_state` is also registered and matches the
  same `config` substring.
- `tools/run_probes.py:423` — parallel mode asserts that the probes have no
  isolation issue and submits every selected entry concurrently.
- `tools/config_state_probe.py:54` — the state probe owns local config paths
  and the tracked `video.yaml`, `keybinds.yaml`, and `notifications.yaml`
  paths.
- `tools/config_state_probe.py:84` — it uses its own fixed temporary backup
  directory.
- `tools/config_state_probe.py:87` — it moves all existing local and legacy
  files out of the repository before boot.
- `tools/config_state_probe.py:105` — its cleanup removes current files before
  restoring its private backups.
- `tools/config_migration_probe.py:53` — the migration probe declares the same
  local and legacy path sets.
- `tools/config_migration_probe.py:64` — it uses a different fixed backup
  directory.
- `tools/config_migration_probe.py:117` — its fixtures delete all shared paths.
- `tools/config_migration_probe.py:145` — later phases move all shared paths
  into that probe's independent backup.
- `tools/config_migration_probe.py:156` — its restoration begins by deleting
  whatever currently occupies the paths.
- `config/video.yaml`, `config/keybinds.yaml`, and
  `config/notifications.yaml` are all tracked by git.

**Handoff context:**

- **Current behavior:** The supported parallel runner can overlap two probes
  that mutate and restore the same tracked files without coordination.
- **Expected direction:** Probes sharing repository-relative mutable resources
  should either run exclusively or receive isolated resource roots.
- **Scope and constraints:** Preserve concurrency for independent engine
  probes. Account for both tracked legacy files and gitignored local files;
  unique ports and save names do not isolate these resources.
- **Remaining uncertainty:** The destructive race was deliberately not run.
  The conflicting path sets and unsynchronised mutation boundaries are
  verified statically; which failure or dirty-tree outcome occurs depends on
  timing.

### [#1323] PYT-6. Unexpected probe failures can leave engine descendants running

The aggregate runner creates a process group for each Python probe and kills
that group only on a wall-clock timeout. If the probe exits promptly with an
ordinary nonzero result, the runner records the failure without checking for or
terminating descendants.

Most probes protect their engine with `try`/`finally`, but four registered
offscreen probes boot one or more engines without any `finally` block. An
unexpected socket, parsing, image, or assertion exception after boot bypasses
their normal `quit_engine` call. The probe process exits, while its `cabal run`
child inherits the same process group and can remain alive. A retry can then
fail against the occupied port or interact with stale engine state.

**Evidence:**

- `tools/run_probes.py:308` — each probe is started in a new session so its
  descendants can be managed as a process group.
- `tools/run_probes.py:319` — the process group is terminated only inside the
  timeout exception.
- `tools/run_probes.py:337` — an ordinary nonzero exit is returned directly
  without descendant cleanup.
- `tools/probelib.py:141` — `boot` starts `cabal run` as a child without
  creating a separate session, so it inherits the probe's process group.
- `tools/probelib.py:157` — `quit_engine` is explicitly safe for use in a
  `finally` and escalates to a hard kill when graceful shutdown fails.
- `tools/construction_blueprint_footprint_probe.py:271` — the probe boots an
  offscreen engine, but its only final normal cleanup is at line 394.
- `tools/item_list_widget_probe.py:821` — the probe boots an engine, but its
  final normal cleanup is at line 924.
- `tools/offscreen_probe.py:691` — the probe boots multiple offscreen engines,
  with normal-path shutdowns at lines 735, 781, and 791 but no enclosing
  `finally`.
- `tools/transfer_context_menu_probe.py:462` — the probe boots an engine, but
  its final normal cleanup is at line 704.

An AST inventory of every registered probe found these four booting probes with
no non-empty `finally` block.

**Handoff context:**

- **Current behavior:** An unexpected exception in any of the four probes can
  finish the Python subprocess while leaving its engine descendant and port
  alive.
- **Expected direction:** Probe completion should guarantee descendant teardown
  on success, ordinary failure, exception, timeout, and interruption.
- **Scope and constraints:** Retain graceful debug-console shutdown where
  possible and the runner's timeout escalation. Cover multi-engine probes and
  retries, not only the common one-engine case.
- **Remaining uncertainty:** No real offscreen process was intentionally
  crashed during this audit. The missing cleanup paths and runner behavior are
  verified from their control flow.

---

## Static guard reliability

### [#1324] PYT-7. The Lua duplicate-function guard can pass after losing its scan scope

The duplicate-function audit recognizes a widget module only when it contains
an exact `local <name> = {}` declaration. If a module table is initialized with
fields or its declaration otherwise leaves that narrow grammar, the entire file
is silently ignored. The command also returns success when its glob matches no
files.

These are false-green modes for a guard whose purpose is to protect all
`scripts/ui/*.lua` widget modules. A harmless-looking module-table
initialization change or a broken scope path can remove coverage without
failing CI or announcing that the guard's premise no longer holds.

**Evidence:**

- `tools/lua_duplicate_function_audit.py:35` — the complete scan scope is one
  hardcoded glob.
- `tools/lua_duplicate_function_audit.py:37` — module recognition accepts only
  an exact empty table literal.
- `tools/lua_duplicate_function_audit.py:49` — `check_file` silently returns no
  failures when that declaration is absent.
- `tools/lua_duplicate_function_audit.py:73` — `main` does not require the glob
  to match any files.
- `tools/lua_duplicate_function_audit.py:84` — zero matched files produce the
  same successful message and exit status as a clean populated scan.
- `.github/workflows/ci.yml:298` — CI describes the audit as guarding every
  `scripts/ui/*.lua` widget module.
- `.github/workflows/ci.yml:303` — CI invokes the production audit directly,
  with no separate self-test or fixture test.

A temporary file containing `local widget = { value = 1 }` followed by two
identical top-level `function widget.f()` definitions produced no failure. With
the audit pointed at an empty temporary tree, it printed that zero files were
checked and returned status 0. The current repository scan still passes across
31 matched files.

**Handoff context:**

- **Current behavior:** Losing the expected module declaration or the entire
  file scope disables coverage while the guard remains green.
- **Expected direction:** The audit should fail when its expected corpus is
  empty or when a scoped widget module cannot be analyzed under the declared
  grammar.
- **Scope and constraints:** Preserve the deliberate focus on column-zero dot
  definitions; local helpers, colon methods, and nested functions need not be
  added to scope. Add isolated fixture tests for duplicate detection,
  unsupported module declarations, and an empty corpus.
- **Remaining uncertainty:** Every currently matched module still uses the
  expected empty-table form, and no live duplicate was found. The concern is
  loss of the regression guard rather than a current Lua overwrite.
