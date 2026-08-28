# Project Review Findings: PRs #1684–#1656

These entries record focused evidence from the senior review of the twelve most recently merged PRs in merge-time order — #1684, #1683, #1678, #1677, #1665, #1664, #1663, #1652, #1662, #1658, #1657, and #1656 — plus the 23 direct first-parent commits in the same landing interval, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

PRs #1684, #1683, #1677, #1663, #1652, #1662, and #1657 retain their intended contracts in the current tree, and their focused self-tests/audits passed during this review. PR #1656's structural-only Lua validation and PR #1658's narrow tutorial-label truncation were explicit project-owner decisions in their review histories, so they are not recast here as defects. Of the interval's direct commits, one updated the de-flake census and its validated inventory, while the other 22 published findings-report dispositions; none introduced a separate executable defect. The closed issues behind this batch and pending project-review reports contain no owner for the three concerns below.

## Status

- [x] PRR-1. Foraging probe setup failure can leak its isolated resource root — [#1791]
- [x] PRR-2. The till probe can select a fluid tile as its dry control — [#1793]
- [x] PRR-3. Authored location bounds can overflow into inverted live geometry — [#1796]

## 1. Foraging probe setup cleanup

### [#1791] PRR-1. Foraging probe setup failure can leak its isolated resource root

> **Captured note:** Extend PR #1665's failure-path cleanup around creation of the isolated resource root itself. Its `finally` covers engine boot and every later phase, but a staging error after `mkdtemp` and before entry to that `try` leaves the invocation-owned tree behind.

**Verification:** Verified structurally and by isolated fault injection. Replacing `shutil.copytree` with a synthetic `OSError` made `make_isolated_root` fail after creating `<base>/root`; the invocation base and that child both remained until the review harness removed them externally.

**Evidence:**

- `tools/foraging_probe.py:183-202` — `tempfile.mkdtemp` and `make_isolated_root(base)` both execute before the cleanup-owning `try` begins. The comment correctly explains why `boot` belongs inside the guard, but the same reasoning is not applied to root staging.
- `tools/foraging_probe.py:56-66` — staging mutates the new tree incrementally: it creates `root`, adds three symlinks, copies `config`, and creates `saves`. A permission, source, disk, or interruption failure can therefore occur after cleanup-worthy state exists.
- `tools/foraging_probe.py:321-337` — `remove_run_root(base)` is unconditional only after control reaches this `finally`; setup exceptions at line 187 bypass it entirely.
- Issue #1618 requirement 6 requires that no run-created save artifact remain after either a passing or failing run, and requirement 7 constrains deletion to invocation-owned state. PR #1665 intentionally strengthened boot-failure cleanup but left this earlier failure boundary outside its guard.
- Tracker searches across open and closed issues for the foraging probe's setup/resource-root cleanup found only closed #1618, whose implementation this concern follows, and no separate owner. Findings-report searches likewise found no duplicate.

**Handoff context:**

- **Current behavior:** A root-staging exception terminates the probe non-zero but leaves its uniquely created temporary directory and any symlinks, copied configuration, or partial save tree already placed there.
- **Expected behavior:** Once `mkdtemp` succeeds, every subsequent exit path attempts deletion of exactly that invocation-owned base; orderly engine shutdown remains conditional on a process having actually launched.
- **Scope and constraints:** Surfaced in PR #1665 / issue #1618. Preserve the unique save slot, copied-without-local-overrides configuration, symlink-safe deletion, no deletion of pre-existing paths, and the rule that a busy port never receives `engine.quit()` from this probe.
- **Verification target:** Add a setup-failure self-test or fault-injection case that raises after root creation and asserts the invocation base no longer exists; retain the existing boot-failure, passing-run, and cleanup-failure behavior.
- **Deduplication:** Open/closed tracker and project/findings-report searches found no issue or pending finding beyond closed #1618.
- **Remaining uncertainty:** The review injected the exception directly into `copytree`; it did not force a real filesystem exhaustion or permission failure. The bypass and surviving directory are independent of which staging operation raises.

## 2. Till-probe dry-tile selection

### [#1793] PRR-2. The till probe can select a fluid tile as its dry control

> **Captured note:** Apply PR #1664's corrected `world.getFluidAt` multi-return decoding to `find_tillable` as well as `find_fluid_tile`. The former still tests for a JSON object the API never returns, so its supposedly dry control can be chosen from under water.

**Verification:** Verified from the API boundary and with a focused stub. Feeding `find_tillable` a flat tile whose fluid reply is the production-shaped string `"river"` and whose flora reply is absent returned that tile as `(0, 0)` instead of rejecting it.

**Evidence:**

- `tools/till_probe.py:67-81` — `find_tillable` calls `return world.getFluidAt(...)` and skips only when the decoded result is a dictionary with a `type` key. Every nonempty string therefore falls through as dry.
- `src/Engine/Scripting/Lua/API/WorldQuery/Fluid.hs:20-55` — the supported API returns two Lua values for fluid (`type`, `surface`) and one `nil` for dry/unloaded tiles; it never returns a table/object.
- `tools/probelib.py:104-121` — `send_json` returns non-JSON response text unchanged. With the unbound multi-return expression used by `find_tillable`, a fluid reply is text rather than the dictionary that its predicate expects.
- `tools/till_probe.py:93-109` — PR #1664 corrected the adjacent exclusion-fixture scan by binding the first return and requiring a nonempty string. The two helpers now encode contradictory meanings for the same API in the same file.
- `tools/till_probe.py:143-166` — the selected coordinate drives the probe's designation control and its later save/AI phases. If the first flat, flora-free lattice point is wet, the helper can create a false setup/behavior failure before a valid dry point is considered.
- Issue #1609 made a missing fluid fixture fail loudly but put the tillable-tile filter out of scope. Tracker searches across open and closed issues found no owner for this distinct dry-control decoder mismatch; findings-report searches found only the already-dispositioned missing-fixture concern.

**Handoff context:**

- **Current behavior:** The dry-tile search recognizes no actual fluid response. A wet, flat, flora-free point can be announced as tillable and used as the positive control, where the real tilling predicate may reject it and make an otherwise healthy run fail misleadingly.
- **Expected behavior:** Both scans interpret `world.getFluidAt` through the documented first-return contract: a nonempty type string means wet and must be excluded from `find_tillable`; `nil` means the point may continue through the remaining filters.
- **Scope and constraints:** Surfaced while reviewing PR #1664 / issue #1609. Preserve the default world fixture, sampling lattice, real supported API, hard failure when no wet exclusion fixture exists, and independence of all six declared phases.
- **Verification target:** Add pure/self-test coverage that presents flat wet and flat dry candidates and proves only the dry one is returned, then run the till probe on its default fixture and confirm both the positive designation and fluid-exclusion phases pass.
- **Deduplication:** Open/closed tracker and project/findings-report searches found no current owner; closed #1609 covers only skipping the exclusion phase when its wet prerequisite is absent.
- **Remaining uncertainty:** The exact first sampled wet coordinate depends on the generated fixture, so the default run may currently encounter a dry candidate first. The helper's classification error is deterministic whenever a wet candidate precedes one.

## 3. Authored location-bound arithmetic

### [#1796] PRR-3. Authored location bounds can overflow into inverted live geometry

> **Captured note:** Close the overflow edge documented during PR #1678: the YAML loader validates only relative-bound ordering, then `translateBounds` performs unchecked `Int` addition. An ordered extreme authored box at a nonzero anchor can wrap one endpoint and become the same unusable inverted geometry that save validation now rejects.

**Verification:** Verified structurally and with the platform's `Int` arithmetic. For anchor `1`, relative endpoints `maxBound - 1` and `maxBound` are ordered, but translation produces `maxBound` and `minBound`; the translated minimum is therefore greater than its maximum.

**Evidence:**

- `src/Engine/Asset/YamlLocations.hs:57-69` — all four authored coordinates decode directly as unrestricted `Int`s.
- `src/Engine/Asset/YamlLocations.hs:170-198` — the definition parser enforces minimum-versus-maximum ordering but establishes no representable translation range.
- `src/Location/Bounds.hs:64-70` — the current contract explicitly records that unchecked translation can wrap and invert an extreme authored box, and calls the edge deliberately unaddressed after #1668.
- `src/Location/Bounds.hs:87-90` — `translateBounds` adds the anchor independently to each endpoint using ordinary `Int` addition, with no checked conversion or overflow result.
- `src/Location/Instance.hs:284-300` — every newly placed location immediately stores that translated result as its authoritative `liBounds`; there is no runtime validation between construction and use.
- `src/Location/Instance.hs:488-509` — current commentary documents the consequences: discovery contains no point while placement intersection can still reject unrelated ground. The new `locationInstanceBoundsErrors` check runs at the save component boundary, so it prevents persistence but does not prevent the invalid instance from becoming live before a save is attempted.
- `test-headless/Test/Headless/Location/Bounds.hs:203-206` covers only a small non-wrapping translation. The stored-bounds regressions validate manually constructed absolute boxes, not an engine placement produced by overflowing authored relative coordinates.
- Tracker searches across open and closed issues found only closed #1668, which fixed decoded stored geometry and explicitly left authored-coordinate range policy outside that change. No open issue or pending report owns the runtime construction edge.

**Handoff context:**

- **Current behavior:** A syntactically valid location definition can produce an inverted authoritative footprint when placed at a nonzero anchor. The location becomes undiscoverable, can distort placement exclusion, and is rejected only if/when the world later crosses the save-validation boundary.
- **Expected behavior:** Authored bounds and their legal anchors have a checked contract that makes every accepted translation representable and ordered, or placement fails explicitly before publishing the instance; valid ordinary and seam-straddling footprints remain unchanged.
- **Scope and constraints:** Surfaced in PR #1678 / issue #1668 but is not a decoded-save defect. Preserve inclusive/degenerate boxes, durable stored geometry, cross-chunk and wrapped-world consumers, and the rule that valid stored footprints are never rederived from a later definition edit. The solution needs an explicit coordinate-range decision rather than silently clamping.
- **Verification target:** Add boundary tests around `minBound`/`maxBound` for the chosen validation site, including an ordered box that would currently wrap at a nonzero anchor, and retain focused location-bounds, instance-identity, discovery, and save-component coverage.
- **Deduplication:** Open/closed tracker and project/findings-report searches found no owner beyond closed #1668, whose implementation documents this as a separate unaddressed decision.
- **Remaining uncertainty:** Shipped location YAML uses small offsets, so no current asset triggers the edge. Future authored/modded data and any API path accepting the same definition shape remain exposed until the accepted coordinate domain is explicit.
