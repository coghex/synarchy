# Epic Review Findings: Epic #342 — Cooking / consumables (on the unified recipe system; coffee first)

This report records the current-HEAD review of epic #342 at
`43da378c1033b651ab25d2aed773d5c897ff354d`. The epic has no native GitHub
sub-issues; its body declares five children, reconciled as #343 through #347.
Every child is closed as completed and its implementation PR is merged. The
children compose into the promised arc: the unified recipe path derives output
quality from skill and knowledge, tracks and cools item temperature, exposes
quality tiers, provides a kitchen and coffee recipe, and applies
quality/temperature-scaled hydration, caffeine, and mood through an
exact-instance player gesture. The focused current tests and the `craft`,
`cooking`, `consumable_effects`, and `item_temp` probes all pass. One new,
deduplicated tracker-steering mistake survives: the epic and final child still
describe that completed payoff as unchecked, remaining, or blocked.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Epic #342 and child #347 still present the completed payoff as unfinished and blocked — [#342]

## 1. Completed-arc tracker steering

### [#342] ER-1. Epic #342 and child #347 still present the completed payoff as unfinished and blocked

> **Captured note:** Epic #342 leaves all five completed children unchecked,
> calls #347 its “last remaining piece,” and leaves the caffeine design choice
> open; #347 still says it is blocked on an unfiled future Mood epic even though
> #350 and #347 both shipped.

**Verification:** The live tracker closes #343 through #347 as completed, and
the epic's closure comment explicitly reconciles all five as shipped and says
that #347 settled the caffeine/stimulation choice. PR #528 merged the final
effects slice after its exact-instance correction. The current implementation
still contains that complete path: `brew_coffee` produces hot coffee through
the unified recipe system; `consumable.drinkInstance` drains the selected
instance before applying hydration, caffeine, mood, and temperature-scaled
effects; and the player coffee gesture exercises the exact-instance route.
Nevertheless, the epic body retains five unchecked boxes and remaining-work
language, while #347's body retains its pre-#350 blocker and “Mood epic not yet
filed” dependency. A reader using those bodies as the durable roadmap receives
the opposite status from the live children, merged code, and closure comment.

**Evidence:**

- [Epic #342](https://github.com/coghex/synarchy/issues/342) — all five
  build-order entries remain unchecked; #347 is still called the “last
  remaining piece,” and its already-settled caffeine choice remains open.
- [Epic closure reconciliation](https://github.com/coghex/synarchy/issues/342#issuecomment-4922863583)
  — the closing comment states that all five children shipped and that #347
  settled caffeine/stimulation.
- [Child #347](https://github.com/coghex/synarchy/issues/347) — the opening,
  rationale, dependency, and done sections still say Mood is an unfiled future
  blocker, despite #350 and #347 being closed.
- [PR #528](https://github.com/coghex/synarchy/pull/528) — the final consumable
  effects child merged after correcting the instance-selection race found in
  review.
- `data/recipes/basic_food.yaml:7` and `scripts/consumable.lua:234` — the
  current recipe and exact-instance consumption entry point remain present.
- Focused current checks: `Craft.Execute` (44 examples), `Item.Temperature`
  (13), `Item.QualityTier` (44), and `Player coffee drink gesture` (18) pass;
  the `craft`, `cooking`, `consumable_effects`, and `item_temp` probes pass 4/4.

**Handoff context:**

- **Current behavior:** The closed epic and its final closed child steer readers
  as if the implementation is still pending and blocked on a prerequisite that
  shipped before either issue closed.
- **Expected behavior:** Reconcile both issue bodies with their live completed
  state: check all five epic children, remove remaining/unblocked language,
  record the caffeine/stimulation decision as delivered, and replace #347's
  future-Mood blocker with the completed #350 dependency and merged outcome.
- **Scope and constraints:** Tracker-body correction only for #342 and #347.
  Preserve the deliberate deferrals for brewing/chemistry, multi-step cuisine,
  spoilage, nutrition rebalancing, automated AI consumption, and renewable
  water/coffee-ground production. Do not change the working implementation.
- **Verification target:** The two live issue bodies agree with their CLOSED
  state, the five live child states, #350, PR #528, and the epic's closure
  reconciliation; no wording still calls #347 remaining, actionable, or blocked.
- **Deduplication:** Exact all-state tracker searches for `342 unchecked`,
  `BLOCKED on a Mood system`, and `last remaining piece` found only the source
  epic/child. A 3,000-item all-state issue inventory found no corrective owner,
  and the docs-worktree report corpus contains no pending tracker-steering
  concern for #342 or #347.
- **Remaining uncertainty:** None.
