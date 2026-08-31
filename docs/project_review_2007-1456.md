# Project Review Findings: PRs #2007–#1456

This report records the senior review of the 85 merged pull requests above
#1455 that had no durable prior-review evidence: #2007, #1606, #1601, #1574,
#1568, #1567, #1566, #1565, #1564, #1563, #1562, #1561, #1560, #1559,
#1558, #1550, #1557, #1556, #1555, #1554, #1553, #1552, #1551, #1549,
#1548, #1534, #1532, #1533, #1531, #1530, #1529, #1528, #1527, #1526,
#1523, #1521, #1522, #1520, #1519, #1518, #1517, #1516, #1515, #1514,
#1513, #1511, #1512, #1509, #1510, #1508, #1507, #1506, #1505, #1504,
#1503, #1502, #1501, #1500, #1499, #1498, #1497, #1496, #1495, #1480,
#1478, #1477, #1491, #1473, #1489, #1472, #1470, #1469, #1468, #1467,
#1466, #1465, #1464, #1463, #1462, #1461, #1460, #1459, #1458, #1457,
and #1456, in merge-time order. Thirty other selector hits in the same numeric
range were excluded because `docs/project_review_1642-1631.md`,
`docs/project_review_1630-1614.md`, and `docs/project_review_1547-1535.md`
explicitly record their prior review. The review read each selected pull
request, its linked specification when present, its commits and merged diff,
then traced the surviving behavior at current HEAD. It also classified all 162
direct first-parent commits in the landing interval
`570607805a^..9f947f6870`; every one changes only `CLAUDE.md` or `docs/`, and
none produced a separate current concern. PR #1568's consolidation invariant
has since been violated by PR #1984, producing the one current concern below.
The other 84 selected pull requests produced no separate current concern, and
no concern was explicitly excluded from this batch.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. A later recipe-loader change bypassed the shared `tshow` conversion

## 1. Shared show-to-Text conversion

### PRR-1. A later recipe-loader change bypassed the shared `tshow` conversion

> **Captured note:** Replace the recipe loader's new hand-written
> `T.pack (show ...)` wrapper with the shared `UPrelude.tshow`, and decide
> whether the repository-wide spelling needs a lightweight regression guard.

**Verification:** Verified against PR #1568 / issue #1099, current production
source, and the later introducing commit. PR #1568 defined
`tshow ∷ Show α ⇒ α → Text` once in `UPrelude`, replaced all 709 direct
`T.pack (show …)` / `T.pack $ show …` production wrappers it found, and removed
the local copies. Current HEAD again contains one direct Text wrapper:
`checkCount` in `Engine.Asset.YamlRecipes` formats a rejected non-positive
ingredient count with `T.pack (show (ryiCount ing))`. `git blame` attributes
that line to `11a02b350e`, the implementation commit merged through PR #1984
after #1568. The recipe validation itself is correct and its focused coverage
passes; the defect is that the shared-vocabulary invariant has already
regressed at the first later call site that needed the same conversion.

**Evidence:**

- `src/Engine/Asset/YamlRecipes.hs:67-73` — the non-positive-count diagnostic
  ends in `T.pack (show (ryiCount ing))`, the exact hand-written production
  form PR #1568 removed.
- `src/UPrelude.hs:68-72` — `tshow` is the shared project spelling, with its
  contract explicitly stating that local copies are not to reappear.
- `git blame -L 68,73 -- src/Engine/Asset/YamlRecipes.hs` — the complete
  `checkCount` branch, including the wrapper, was introduced by commit
  `11a02b350e` on 2026-08-30 through PR #1984.
- `rg -n --glob '*.hs' 'T\.pack\s*\(\s*show\b|T\.pack\s*\$\s*show\b' src app`
  — the recipe-loader line is the only current direct Text occurrence in
  production. The `BC.pack (show …)` sites under `Unit.Atlas.Digest` produce
  bytestring digest material and are a different conversion contract.
- `docs/code_health_findings.md:1790-1815` — CH-75 records the original
  duplication and resolves it through issue #1099; its note explains why
  duplicate local definitions fail to compile, but there is no equivalent
  mechanism preventing a new direct wrapper.

**Handoff context:**

- **Current behavior:** The diagnostic text emitted for an invalid recipe is
  correct, but production now has two spellings for the same show-to-Text
  operation. The shared helper no longer describes the complete production
  convention that PR #1568 established.
- **Expected behavior:** Production call sites that render a `Show` value to
  strict `Text` use `UPrelude.tshow`; byte-string conversions and formatting
  contracts that are not a direct show-to-Text wrapper remain untouched.
- **Scope and constraints:** Surfaced while reviewing PR #1568 against current
  HEAD; introduced later by PR #1984. Preserve #1984's rejection of
  non-positive recipe counts, its exact diagnostic content, and all loader
  behavior. Do not mechanically rewrite `Data.ByteString.Char8.pack (show …)`
  digest fields. A guard, if chosen, must be syntax-aware enough not to flag
  comments, strings, or non-Text packers.
- **Verification target:** Replace the current recipe-loader wrapper with
  `tshow`, run the recipe-YAML count-validation examples, and repeat the
  production-source search. If a guard is added, pin both the forbidden Text
  forms and permitted byte-string conversions in its self-test.
- **Deduplication:** All-state tracker searches for the exact wrapper,
  `YamlRecipes`, show-to-Text, and `tshow` found only closed issue #1099, which
  owns the original consolidation rather than this later regression. The
  findings-report corpus contains CH-75 and unrelated historical examples, but
  no pending owner for the current `YamlRecipes` occurrence.
- **Remaining uncertainty:** None about the current violation or its introducing
  commit. Whether one later recurrence justifies a permanent audit is a
  maintainability decision for one-at-a-time disposition; the direct code fix
  does not depend on that decision.
