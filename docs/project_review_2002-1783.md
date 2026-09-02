# Project Review Findings: PRs #2002–#1783

This report records the senior review of the next twelve uncovered merged pull
requests in merge-date order — #2002, #1802, #1801, #1800, #1799, #1798,
#1797, #1795, #1794, #1792, #1784, and #1783 — plus direct first-parent
documentation commits `dc470999`, `1f591b9d`, `19af28ea`, `91444631`,
`83fddc35`, `99d73d07`, `0dd0cdc8`, `4960d4d9`, and `87ae3951` in the same
landing interval. The review read each pull request, its linked specification
where one existed, merged diff and commits, then traced the surviving behavior
at current HEAD. The direct documentation commits retain their intended design,
findings-report, probe-census, and project-review cursor roles; the known
`text_encoding` census drift remains captured in
`docs/project_review_1987-1893.md` and is not duplicated here. The other eleven
selected pull requests produced no separate current concern, and no concern was
explicitly excluded from this batch.

Focused checks passed for swapchain selection (15/15), the player coffee-drink
gesture (18/18), Lua tick-interval policy (10/10), loaded-atlas shutdown release
(9/9), click-correlation widget routing (11/11), arena base seeding (6/6),
material `move_cost` validation (19/19), craft output identity (5/5), existing
structure rotation (46/46), portal spawn page binding (10/10), and preview
window policy (3/3). `tools/test_run_probes.py`, the findings-report audit, and
the CI parity audit also passed. No full headless suite, graphical session,
worldgen tier, world check, behavior probe, baseline capture, or `make ci` was
run.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] PRR-1. One ambiguous wall-art path can still select a family through the companion path — [#2160]

## 1. Wall-family ambiguity containment

### [#2160] PRR-1. One ambiguous wall-art path can still select a family through the companion path

> **Captured note:** Make `rotatedWallArt` leave a wall exactly as placed when
> either member of its placed texture/facemap pair has contradictory ownership.
> The catalogue records each ambiguous path correctly, but the resolver drops
> that marker and can still select a family through the pair's unambiguous
> companion path.

**Verification:** Confirmed in the current pure catalogue implementation. Two
complete families were registered with the same owned `WallNE` sprite path but
different, uniquely owned cap-facemap paths. The reverse texture index correctly
reported `Just Nothing` for the shared path. Resolving family A's placed pair at
`FaceWest` nevertheless returned `Just (TextureHandle 11, TextureHandle 104)`
instead of `Nothing`, because family A's unambiguous facemap ownership remained
in the flattened `owners` list. The symmetric case — an ambiguous facemap beside
a uniquely owned texture — follows the same control flow. The shipped structure
rotation group passes 46/46 because its ambiguity fixture makes both halves
ambiguous; it does not cover either one-half-ambiguous combination.

**Evidence:**

- PR #1794 / issue #1712 introduced `Structure.WallCatalog` and the
  data-driven directional wall-art resolver. The module contract says two
  families owning one path is contradictory pack data and that the ambiguous
  path stops rotation rather than allowing registration order to choose
  (`src/Structure/WallCatalog.hs:41-45`).
- `src/Structure/WallCatalog.hs:97-103` stores an owner as `Maybe Int`, with an
  inner `Nothing` representing ambiguous ownership; `:172-177` installs that
  marker whenever a different family claims the same path.
- `src/Structure/WallCatalog.hs:190-193` explicitly promises that ambiguity in
  either path returns `Nothing`. The implementation at `:224-230` instead runs
  `catMaybes` over two `HashMap Text (Maybe Int)` lookups. That removes an
  absent outer lookup but retains an ambiguous owner as an ordinary `Nothing`
  element, after which `matches` can still accept the `Just familyId` supplied
  by the companion path.
- `test-headless/Test/Headless/World/Render/StructureRotation.hs:627-657`
  verifies the narrower case where a second family owns both the sprite and all
  of the placed edge's masks. Both owner entries are ambiguous there, so the
  test cannot expose selection through one still-unambiguous half.
- All-state tracker searches for wall-catalog ambiguity, facemap ambiguity,
  `rotatedWallArt`, structure-wall ownership, and contradictory pack paths
  found no issue owner. Searches across the primary and docs-worktree findings
  corpus found no pending report entry for this behavior.

**Handoff context:**

- **Current behavior:** Contradictory ownership of one placed asset does not
  necessarily stop rotation. If the exact placed pair is carried by one family
  and its other asset still uniquely names that family, the resolver returns
  that family's rotated handles despite the explicit ambiguity marker. Invalid
  or future pack/variant data can therefore render directional art instead of
  taking the catalogue's documented safe fallback.
- **Expected behavior:** A present ambiguous owner entry for either placed path
  short-circuits the resolution to `Nothing`. An absent owner remains distinct
  from an ambiguous owner so inherited-path matching continues to work, and a
  pair with no ambiguity still selects exactly one carrier as today.
- **Scope and constraints:** Keep the fix inside the pure wall-catalogue
  ownership reduction and its focused tests. Preserve all-or-nothing family
  registration, subset inheritance, first-handle retention, exact-pair carrier
  matching, `FaceSouth` identity, and the rule that both rotated assets come
  from one family. No renderer, YAML schema, asset, or palette redesign is
  required.
- **Verification target:** Add registration-order-independent cases for
  ambiguous texture + unique facemap and unique texture + ambiguous facemap;
  both return `Nothing`. Existing default-family, partial-variant, inherited
  handle, both-halves-ambiguous, and four-facing rotation examples remain
  unchanged and pass.
- **Deduplication:** No matching tracker issue or findings-report entry was
  found. Issue #1712 and PR #1794 are the closed specification and
  implementation history, not an owner for this surviving edge.
- **Remaining uncertainty:** No shipped structure pack currently appears to
  declare this contradictory one-half collision, so the defect is presently a
  containment failure at the data-driven authoring boundary rather than a
  reproduced visible artifact in the bundled `dungeon_1` pack.
