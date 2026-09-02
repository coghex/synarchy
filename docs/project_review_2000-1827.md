# Project Review Findings: PRs #2000–#1827

This report records the senior review of the next twelve uncovered merged pull requests in merge order — #2000, #1999, #1998, #1993, #1992, #1991, #1832, #1831, #1830, #1828, #1829, and #1827 — plus direct first-parent commits `4960d4d9`, `0dd0cdc8`, `99d73d07`, `83fddc35`, `91444631`, `19af28ea`, `1f591b9d`, and `dc470999` in the same landing interval. The review read each pull request, its linked specification, merged diff and commits, then traced the surviving behavior at current HEAD. PR #1993 produced the one current concern below. The other eleven selected pull requests and all eight direct documentation commits produced no separate current concern, and no concern was explicitly excluded from this batch.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] PRR-1. Loot-weight Hspec fixtures collide across concurrent suite processes — [#2163]

## 1. Loot-table test isolation

### [#2163] PRR-1. Loot-weight Hspec fixtures collide across concurrent suite processes

> **Captured note:** Give every loot-weight Hspec fixture an invocation-owned temporary path and cleanup boundary. PR #1993's fixed directory and filename let independent test processes overwrite one another's YAML and recursively delete one another's fixtures.

**Verification:** Verified against PR #1993 / issue #1946, the current fixture helper, one isolated focused run, and concurrent invocations of the already-built headless test executable. The isolated `Location loot determinism` group passed all 38 examples. Eight simultaneous processes restricted to `weight domain (#1946)` then all exited 1. A twelve-process diagnostic run reproduced cross-contaminated weights and warning messages, malformed or missing YAML, and `removeDirectoryRecursive` failures after another process had deleted the shared directory. This is fixture interference rather than a Cabal build race: the reproduction resolved one built executable with `cabal list-bin` and launched that binary directly.

**Evidence:**

- `test-headless/Test/Headless/Location/LootDeterminism.hs:278-359` — thirteen weight-domain examples exercise the real YAML loader, and twelve of them create authored YAML through the same helper rather than using independent files.
- `test-headless/Test/Headless/Location/LootDeterminism.hs:396-430` — both rejection and acceptance assertions read the helper's file after writing case-specific contents, so another process can replace the expected weight or diagnostic between those operations.
- `test-headless/Test/Headless/Location/LootDeterminism.hs:443-450` — `withTempLootYaml` always chooses `<system-temp>/synarchy-loot-weight-spec/probe_loot_table.yaml`, writes it with truncation, and recursively removes the shared directory in `finally`; neither directory nor file carries invocation identity.
- `test-headless/Test/Headless/Location/LootDeterminism.hs:467-486` — the live load-and-register examples reuse the same helper, so the collision reaches both the pure loader assertions and the engine-backed registry boundary.
- Review reproduction: `cabal test synarchy-test-headless --test-options='--match "Location loot determinism"'` passed 38/38 alone. Eight concurrent direct-binary runs of `--match 'weight domain (#1946)'` returned exit codes `1 1 1 1 1 1 1 1`; the expanded run observed values from sibling cases, `Yaml file not found`, malformed input, and competing `removeDirectoryRecursive` errors.

**Handoff context:**

- **Current behavior:** The focused test is reliable in one process, but two or more independent suite processes share one mutable YAML file and one recursively deleted directory. A case can therefore assert against another case's contents or fail in setup/cleanup, making parallel local, worktree, or agent test runs nondeterministic.
- **Expected behavior:** Every suite process and fixture use a path owned by that invocation, and cleanup remove only artifacts that invocation created on every success and failure path. Concurrent runs of the focused group must produce the same result as isolated runs.
- **Scope and constraints:** Preserve PR #1993's real-file YAML boundary, exact authored spellings such as `.nan`, `.inf`, and narrowing overflow/underflow, callback-log assertions, whole-document rejection, the engine-backed load-and-register checks, and cleanup on exceptions. This finding does not change loot-weight validation or production loot behavior.
- **Verification target:** Run the isolated `Location loot determinism` group, then launch several instances of the already-built headless executable concurrently against `weight domain (#1946)` and the load-and-register group. Every process must pass, no process may observe a sibling's authored weight or warning, and no invocation may remove another's directory.
- **Deduplication:** All-state tracker searches for the exact directory, filename, loot temporary paths, and loot-test isolation found no owner. Closed #1884 fixes the same failure class only in `tools/location_content_probe.py`; its requirements and explicit scope do not include this Hspec module. The findings-report corpus contains loot-weight validation and other fixed-temp probe concerns, but no entry for `withTempLootYaml` or `synarchy-loot-weight-spec`.
- **Remaining uncertainty:** None about the defect or failure path. The eventual repair may use an invocation-owned directory around the group or a unique file per helper call, provided ownership and cleanup remain unambiguous under concurrent processes.
