# Project Review Findings: PRs #1455–#1424

This entry records the senior review of the final 13 merged PRs after the
previous sweep: #1455, #1453, #1454, #1452, #1451, #1450, #1449, #1448,
#1446, #1445, #1443, #1442, and #1424, merged on 2026-08-19 through
2026-08-20. The first-parent interval also contains direct commit
`16f04f07`, which only records the existing project-review checkpoint and
cleared review. PR #1411 is deliberately excluded because the earlier sweep
already reviewed that late-merged PR. This batch stops at the exclusive
checkpoint before PR #1423; no older history was reopened.

The sweep produced one confirmed current finding from PR #1449. The other
twelve PRs and the direct checkpoint commit cleared their linked contracts,
current-code inspection, descendant-history review, and focused verification.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Ground-salvage condition accepts an out-of-range base that guarantees pristine loot

## 1. Ground-item condition

### PRR-1. Ground-salvage condition accepts an out-of-range base that guarantees pristine loot

> **Captured note:** PR #1449 (`16c0c870`, issue #1421) correctly retained
> the wear-penalty draw when callers provide an explicit condition base, but
> the public Lua entry point accepts bases outside condition's 0–100 domain.
> A caller can pass `condition=120`; every allowed 0–20 penalty then clamps
> back to 100, guaranteeing the pristine result that #1421 explicitly says
> the ground-spawn API cannot request or guarantee.

**Verification:** Current production behavior was reproduced directly with
`salvageCondition 120` and penalty values 0, 7, and 20; all three results were
`100.0`. The focused `Item.Condition` suite passes because its explicit-base
coverage uses only 7 and 80, while its real-verb "pristine is unreachable"
case omits the explicit condition property.

**Evidence:**

- `src/Item/Types.hs:324` — `iiCondition` declares the value domain as
  0–100.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:91-103` — the shared numeric
  property reader converts any Lua number to `Float` without validating the
  condition domain.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:120-128` — that unchecked
  value becomes the explicit base passed through the salvage roll and then
  into the root item's materialization override.
- `src/Item/Roll.hs:112-124` — the explicit base is used unchanged before the
  0–20 penalty, after which `salvageCondition` clamps the result to 0–100.
- `src/Item/Materialize.hs:156-160` — materialization stores the override as
  the instance condition without another validity check.
- `test-headless/Test/Headless/Item/Condition.hs:407-418` — explicit-base
  tests cover only in-domain values 7 and 80.
- `test-headless/Test/Headless/Item/Condition.hs:457-470` — the real Lua verb
  proves pristine is unreachable only when no explicit condition is supplied.

**Handoff context:**

- **Current behavior:** `item.spawnGround(name, x, y, {condition=120})`
  accepts the out-of-domain base, consumes the required penalty draw, and
  nevertheless returns condition 100 for every possible penalty.
- **Expected behavior:** A public ground spawn must not accept an explicit
  condition in a way that escapes the 0–100 condition domain or guarantees a
  pristine salvage item by neutralizing the mandatory wear penalty.
- **Scope and constraints:** Preserve #1421's explicit-value-as-base
  semantics, the mandatory one-penalty-draw behavior and RNG ordering, and
  the rule that only the root ground item receives salvage condition;
  definition-spawned contents remain pristine. Quality and ordinary
  materialization behavior are out of scope.
- **Verification target:** Exercise the real Lua verb with an out-of-range
  explicit condition and prove it is either refused or normalized before the
  penalty so that the request cannot guarantee 100; retain the valid-base-80
  range of 60–80 and verify the established draw count/order.
- **Deduplication:** Searches across open and closed tracker issues found the
  originating #1421 and unrelated condition/item/probe work, but no issue for
  out-of-range explicit ground-condition bases or this pristine-guarantee
  bypass. Existing findings reports contain no matching concern.
- **Remaining uncertainty:** The public invalid-input policy is not yet
  specified: rejecting the spawn and constraining the base before applying
  wear can both satisfy the observable contract. That choice belongs in the
  later issue-disposition pass.
