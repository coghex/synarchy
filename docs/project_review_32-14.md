# Project Review Findings: PRs #32–#14

These entries record focused evidence from the senior review of the repository's
terminal merged-PR window in first-parent order: #32, #17, #16, and #14. There
are no other merged PRs before #33. Because early development was mostly landed
as bare commits, the exact interval contains 625 first-parent commits: 23
PR-owned commits (PR #14 alone was rebased as 18 commits) plus 602 direct
commits between PR #33's parent and PR #14's base. The direct commits were
inventoried with the PRs rather than silently omitted.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an
issue · `[deferred]` blocked on a concrete precondition

PR #17's 86-file Unicode conversion is mechanically equivalent after
normalizing the intended token and pragma substitutions; the comparison found
no residual semantic mismatch. PR #16's geology split preserved the live
timeline entry point while removing the documented dead volcano path, and its
current descendants do not expose a missed split contract. A current-survivor
scan attributed 78,599 lines across 1,385 behavior-bearing files to this large
early interval and then traced higher-risk tooling, lifecycle, randomness,
threading, and world-generation remnants. The RandBox/global-RNG coupling from
direct commit `b274780` is already captured with a stronger fixed-seed
reproduction as LUA-4 in `docs/lua_script_findings.md`; the repeated worker
thread lifecycle pattern is already owned by open issue #1147. Neither is
duplicated here. `python3 tools/test_audit.py` passed all 35 groups and
`python3 tools/world_check.py --quick` passed all six configured cases. No
graphical/offscreen session, full headless suite, full probe sweep, or `make ci`
was run. Two non-duplicate concerns remain.

## Status

- [x] PRR-1. The coastal-parallel threshold counts violating components instead of limiting run length — [#1952]
- [x] PRR-2. Swapchain fallbacks remain below the warning level required by PR #14 — [#1954]

## 1. River regression-tool threshold semantics

### [#1952] PRR-1. The coastal-parallel threshold counts violating components instead of limiting run length

> **Captured note:** PR #32 made `MAX_COASTAL_PARALLEL = 5` part of the shared
> river-tool threshold contract, but the single-seed checker hard-codes five as
> the minimum component size and then applies the configurable maximum to the
> number of already-violating components. One arbitrarily long coastal run can
> therefore pass a threshold described as the maximum consecutive run length.

**Verification:** Verified with a synthetic stdin fixture containing one
connected run of ten high-elevation river tiles, each adjacent to ocean. With
the checked-in defaults, `tools/test_river_pour.py` exited 0 and printed
`PASS  Coastal parallels: 1 (max 5)` followed by `RESULT: PASS`. The
multi-seed stress tool does not calculate, aggregate, accept a flag for, or
gate this metric at all.

**Evidence:**

- `tools/river_thresholds.py:1-11,26-27` says the single- and multi-seed tools
  share pass/fail thresholds, that both expose corresponding `--max-*` flags,
  and defines `MAX_COASTAL_PARALLEL = 5` as consecutive river tiles alongside
  the ocean.
- `tools/test_river_pour.py:199-241` independently hard-codes `> 5` when
  deciding whether a connected coastal component is returned. The configured
  threshold is not passed into this calculation, and the returned record's
  `size` is not consulted by the pass/fail decision.
- `tools/test_river_pour.py:243-268` computes the coastal result as
  `len(results["coastal"]) <= args.max_coastal_parallel`. This compares the
  number of components longer than five with the supposed maximum run length.
- `tools/test_river_stress.py:20-32,56-106,109-122` shares and reports only
  visible-drop, dry-gap, and mask-dry thresholds. It has no coastal calculation
  or `--max-coastal-parallel` flag, contrary to the shared module's contract.
- Direct commit `22bf74d6` introduced both river checkers. PR #32 / issue #24
  later introduced the shared threshold module and explicitly claimed that the
  two tools use the same thresholds and expose the same overrides. Searches of
  the open tracker, all issue titles/bodies, and pending findings reports found
  no owner for the coastal mismatch.

**Handoff context:**

- **Current behavior:** The default checker permits up to five separate
  coastal components whose lengths are each greater than five, including one
  component of unbounded length. The stress runner never checks the condition.
- **Expected behavior:** The configured coastal threshold governs the length
  it names, and every tool advertised as sharing that pass/fail contract
  evaluates and reports the same metric. If the intended policy is instead a
  count of offending components, the minimum offending length and component
  count need distinct names and thresholds.
- **Scope and constraints:** This is a regression-tool defect surfaced through
  PR #32, not evidence that the current hydrology implementation produces a bad
  coastal river. Preserve the cheap JSON-only single-seed analysis and avoid
  adding extra world generations merely to test the checker itself. A small
  synthetic self-test can pin both threshold meanings and stress parity.
- **Remaining uncertainty:** The repository does not state whether connected
  component size is the best geometric definition of a river running parallel
  to a coast. The present implementation and shared constant nevertheless
  disagree under either reasonable reading, and the false pass is reproduced.

## 2. Swapchain fallback observability

### [#1954] PRR-2. Swapchain fallbacks remain below the warning level required by PR #14

> **Captured note:** PR #14's explicit swapchain contract required warnings
> when the preferred surface format or present mode was unavailable. The PR
> added broad swapchain logging and the selection fallbacks, but no fallback
> warning. The current code still silently chooses an alternate format and
> reports a FIFO present-mode fallback only as an ordinary selection at Info.

**Verification:** Verified by tracing the PR contract, its patch, and the
current selection path. `chooseSwapSurfaceFormat` is pure and returns the first
available format (or the preferred value for an anomalous empty vector) with no
logging branch. With VSync disabled, `chooseSwapPresentMode` falls through from
MAILBOX to IMMEDIATE to FIFO, then emits the same Info record regardless of
whether the choice was preferred or a compatibility fallback. No warning is
emitted at the caller. A Vulkan session with restricted format/mode support was
not available, so verification is static rather than driver-backed.

**Evidence:**

- PR #14's original requirement 2 says to add **Warn** logs when falling back
  to the default swapchain format or present mode. The PR has no linked tracker
  issue, so that prompt is its durable delivery contract.
- `src/Engine/Graphics/Vulkan/Swapchain.hs:170-176` selects the preferred
  surface format when present, otherwise the first advertised format; an empty
  vector returns the preferred value. None of the three branches can report
  which path was taken.
- `src/Engine/Graphics/Vulkan/Swapchain.hs:178-197` selects MAILBOX, then
  IMMEDIATE, then FIFO for disabled VSync and logs only
  `VSync disabled: using present mode` at Info with the final value. The log
  does not classify FIFO as a fallback when neither low-latency mode exists.
- The PR #14 patch added debug inventory and swapchain-created records but did
  not add a warning around either selector. Later changes converted present
  mode selection into `EngineM` and added its Info record without closing the
  warning-level contract. Searches of the tracker and pending findings reports
  for swapchain fallback, surface-format warning, and present-mode warning
  found no owner.

**Handoff context:**

- **Current behavior:** Logs show the eventual format in the swapchain-created
  record and the eventual disabled-VSync mode at Info, but do not distinguish a
  normal preferred selection from degraded compatibility behavior. The empty
  format-vector safety fallback is also silent.
- **Expected behavior:** When preferred swapchain capabilities are unavailable,
  the selection path emits a warning that names the unavailable preference and
  chosen fallback. Normal preferred selections retain their existing lower
  severity.
- **Scope and constraints:** Surfaced from PR #14's explicit observability
  contract. Keep format selection deterministic and preserve Vulkan's
  guaranteed FIFO behavior. Logging can live around the pure format selector
  rather than forcing an otherwise unnecessary effectful API. Avoid warning
  for VSync-enabled FIFO, where FIFO is the requested and guaranteed mode, not
  a fallback.
- **Remaining uncertainty:** The user-visible severity is low because the
  selected values are partially reconstructible from existing Info records,
  and common drivers may never exercise the format fallback. The missing
  distinction still defeats the warning contract specifically intended to make
  unusual driver capability paths obvious.
