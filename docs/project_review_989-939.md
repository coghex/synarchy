# Project Review Findings: PRs #989–#939

These entries record focused evidence from the senior review of the next twelve merged PRs, #989 through #939 in merge order, plus the direct first-parent commits `3eed2906` (portable-loot design) and `64306746` (code-health findings) in the same window, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #988's clipped tutorial-toggle caption is already PRR-4 in `docs/project_review_1018-991.md`, and PR #955's expedition-probe instability overlaps the existing #1212–#1221 expedition findings/issues, so neither is duplicated here.

## Status

- [x] PRR-1. Already-latched tutorial branches never retire from the active checklist — [#1941]
- [x] PRR-2. Nearest known-location lookup ignores cylindrical seam aliases — [#1944]
- [x] PRR-3. Loot-table weights are registered without semantic validation — [#1946]

## 1. Tutorial active-history lifecycle

### [#1941] PRR-1. Already-latched tutorial branches never retire from the active checklist

> **Captured note:** Give an already-latched tutorial branch a way to retire after it has actually been made observable. The current `stickyActive` repair suppresses the normal hide rule forever, and reconstructing it after a load makes every completed objective in the chain active again.

**Verification:** Verified — the current implementation explicitly applies a permanent hide override, the focused tests assert that a load revives the whole completed chain, and a deterministic Lua reproduction leaves the completed prepare branch active on every successive view-model read.

**Evidence:**

- Issue #958 / PR #962's original tree-state contract says a parent remains in the default active view only until it and all relevant children/subobjectives are complete, after which it hides while remaining available as completed history.
- Issue #996 required already-completed branches to become observable, the shipped checklist to remain non-empty, and all five rows to be seen “at least once.” It did not replace completed history with a permanently active view.
- `scripts/tutorial_progress.lua:122-129` — `stickyActive` is decided at first structural reveal and documented to override hiding “FOREVER” in order to achieve “seen at least once”; no later state records that the branch has now been exposed.
- `scripts/tutorial_progress.lua:350-380` — the module still states #958's normal hide-to-history rule, then says the #996 override suppresses it permanently for an already-latched node.
- `scripts/tutorial_progress.lua:415-418` — the actual hide decision is `rawHidden and not stickyActive`, so a sticky node has no transition that can ever move it into history while its live subobjectives remain checked.
- `test-headless/Test/Headless/Lua/TutorialProgress.hs:319-339` — an ordinarily revealed completed composite is required to hide once both live checks are true and to return only when a check reverses. `:394-430` proves the pre-latched variant remains active but never asserts an eventual retirement.
- `test-headless/Test/Headless/Lua/TutorialProgress.hs:647-665` — load reconstruction deliberately marks the whole completed chain sticky and asserts `place_portal`, `secure_water`, `prepare_expedition`, `prepare_water`, and `prepare_food` are all active again. Thus a save/load can repopulate the default checklist with objectives that had already been completed and left behind.
- A review-time bare-Lua reproduction pre-latched a composite and both checks, revealed it by completing its ancestors, then called `getViewModel()` three times. Every pass returned the same active rows: `prep,water,food`; there is no observation/acknowledgement transition between them.
- The focused `--match "Tutorial progress"` group passed with 31 examples during this review. That confirms the permanent behavior is encoded in the current gate rather than refuting it. Tracker searches for permanent `stickyActive`, an already-latched branch that never retires, and post-load completed-history revival found no owner beyond closed #996.

**Handoff context:**

- **Current behavior:** The shipped pre-provisioned `prepare_expedition` branch becomes visible as completed when water is secured, then remains in the active HUD forever while its water/food checks stay true. Loading a completed tutorial reconstructs every completed ancestor as sticky too, so the whole chain returns to the active checklist.
- **Expected behavior:** An already-latched branch is guaranteed to be observable with its completed markers, then can retire through the same completed-history boundary as an ordinarily completed branch; reversing a live subobjective still brings the composite back without clearing its durable latch.
- **Scope and constraints:** Surfaced by comparing PR #962 / issue #958's durable-progress and hide-to-history contract with current HEAD. The permanent override itself landed later in PR #1020 / issue #996. Preserve monotonic full completion, reversible and non-persisted subobjective checks, evaluation independent of reveal state, deterministic load initialization, and the guarantee that the pre-latched prepare branch does not vanish in the instant it is first revealed.
- **Remaining uncertainty:** The missing contract is what counts as “shown”: construction of one view model, one HUD render/update, a minimum presentation interval, or an explicit transient acknowledgement. The processor should settle that lifecycle before choosing whether the state belongs in `tutorial_progress`, the HUD, or a one-tick/one-render handshake; it should also decide what a load may legitimately reactivate without persisting UI history.

## 2. Experiential location distance

### [#1944] PRR-2. Nearest known-location lookup ignores cylindrical seam aliases

> **Captured note:** Make `nearestKnownLocation` compare anchors in the page's cylindrical coordinate space. PR #954 correctly made acquisition containment seam-aware, but its nearest-memory helper uses raw Euclidean `(x,y)` deltas and can prefer a physically farther ruin over one directly across the U seam.

**Verification:** Verified with the repository's own seam fixture and a deterministic Lua reproduction — from `(6,70)`, a remembered anchor at `(70,6)` is the same physical point through the world-size-8 alias `(-64,+64)`, but the current helper selects a raw-distance-4 anchor instead.

**Evidence:**

- Issue #915 / PR #954 requires a per-unit nearest-location query analogous to water knowledge, while the PR specifically says location acquisition shares discovery's seam-aware geometry.
- `scripts/unit_ai_locations.lua:61-65` — the only distance function is raw `sqrt((ax-bx)^2 + (ay-by)^2)` with no world size or alias localization.
- `scripts/unit_ai_locations.lua:79-93` — `nearestKnownLocation` is page-scoped but compares every same-page anchor with that raw distance; neither its state-table primitive nor its uid wrapper obtains the page's wrap size.
- `test-headless/Test/Headless/Location/Bounds.hs:197-214` — the canonical location fixture proves that in a world of 8 chunks a point around `(6,70)` is adjacent to a location around `(70,6)` after the `(-64,+64)` alias, while raw coordinates put them 62 or more tiles apart.
- `src/World/Generate/Coordinates.hs:108-135` — `localizeTileToAnchor` and `seamTileDist2` are the existing authoritative point-distance machinery: they minimize over the same U aliases and reduce to ordinary Euclidean distance away from the seam or in a non-wrapping world.
- `test-headless/Test/Headless/Lua/UnitAiLocations.hs:121-141` — nearest coverage uses only collinear raw coordinates and page filtering. The suite covers seam-aware *acquisition* elsewhere, but has no seam case for nearest selection.
- In a review-time bare-Lua call, memories `(id=1,x=70,y=6)` and `(id=2,x=10,y=70)` were queried from `(6,70)`. The helper returned id 2 because its raw distances were about 90.51 and 4; applying the world-size-8 alias to id 1 produced distance 0.
- The focused `--match "unit location knowledge"` group passed with 35 examples during this review, demonstrating that the current tests accept the raw nearest result. Tracker and report searches found seam work for discovery, placement, rendering, and name-plate containment, but no owner for experiential known-location ranking.

**Handoff context:**

- **Current behavior:** A unit near the cylindrical boundary can ask for its nearest known ruin and receive a numerically close raw-coordinate ruin even when another remembered ruin occupies the same or an adjacent physical tile across the seam.
- **Expected behavior:** “Nearest on this page” uses the page's physical topology, with raw Euclidean behavior preserved away from the seam and for non-wrapping pages; a focused fixture distinguishes a raw-near candidate from a seam-near candidate.
- **Scope and constraints:** Surfaced in PR #954 / issue #915. Preserve `(page, instance id)` identity dedup, page filtering, pure memory lookup where practical, acquisition's shared discovery predicate, and the persisted anchor. The primitive currently has no world-size input, so the boundary may need an explicit size, a page-aware engine wrapper, or a canonical shared Lua query rather than duplicating the U-wrap formula.
- **Remaining uncertainty:** Unit movement/pathing still uses an unwrapped frame and may walk the long way around the seam, a limitation documented in `scripts/unit_ai.lua`. The processor should decide whether this API promises physical nearest or current-pathing-cost nearest; its present name and the rest of the location topology support the former, but fixing ranking alone will not make pathfinding cross the seam.

## 3. Loot-table authoring validation

### [#1946] PRR-3. Loot-table weights are registered without semantic validation

> **Captured note:** Reject or explicitly define non-positive and non-finite loot weights before registration. The YAML loader accepts any `Float`, and #953's shared weighted walk assumes a meaningful positive total; zero-weight entries can be selected and all-zero/NaN tables degenerate into arbitrary first/last-entry results.

**Verification:** Verified through the current parser/registration path and direct evaluation of `pickByWeight` — an all-zero two-entry table selected its first entry, a zero-weight first entry was selected at draw 0 ahead of a positive entry, and a table with a NaN final weight selected that final entry.

**Evidence:**

- `src/Engine/Asset/YamlLootTables.hs:15-24` — an entry's weight decodes directly as `Float`; there is no smart constructor or parser guard for positivity or finiteness.
- `src/Engine/Asset/YamlLootTables.hs:42-53` — every syntactically decodable document returns `Just def`, with no table-id, entry-id, empty-list, duplicate-id, or weight validation stage.
- `src/Engine/Scripting/Lua/API/LootTables.hs:52-65` — the Lua loader converts and registers every decoded entry unchanged, then reports success. Invalid numeric semantics therefore become live registry data rather than a load diagnostic.
- `src/LootTable/Roll.hs:22-32` — `pickByWeight` sums weights, multiplies by the draw, and accepts the first cumulative weight `>= target`, with the final entry as an unconditional fallback. With total 0, target is 0 and a leading zero-weight entry wins; NaN makes comparisons false until the fallback.
- A review-time GHCi evaluation on the current library returned `Just "zero-a"` for weights `[0,0]` at draw `0.75`, `Just "zero"` for weights `[0,1]` at draw `0`, and `Just "nan-last"` for weights `[1,NaN]` at draw `0.5`.
- `test-headless/Test/Headless/Location/LootDeterminism.hs:225-240` — the focused contract covers the shipped positive distribution, an empty table, and a positive single-entry table. It never supplies a zero, negative, infinite, or NaN weight through either the pure walk or YAML loader.
- The focused `--match "Location loot determinism"` group passed with 21 examples during this review; the shipped `ruin_common` data is valid, so this is an authoring/extension failure boundary rather than a current ruin-balance change. Tracker and findings-report searches found no issue owning loot-weight validation.

**Handoff context:**

- **Current behavior:** A malformed or experimental loot-table file loads successfully but can grant an item explicitly assigned zero weight, collapse every roll onto the first or last entry, or otherwise produce a deterministic but meaningless reward mapping. The seed-stability vectors then pin behavior for valid shipped data without protecting the registry boundary.
- **Expected behavior:** Loaded tables have an explicit numeric contract. A straightforward policy is finite, strictly positive weights; alternatively zero may mean a disabled entry, but then it must be excluded from selection and an all-zero table needs a defined warning/skip result. Invalid documents should fail atomically before replacing a previously valid registry entry.
- **Scope and constraints:** Surfaced in PR #953 / issue #948 because both contextual and uncontextual rolls now share `pickByWeight`; the permissive YAML loader predates that PR. Preserve relative weighting, unknown/empty-table non-crashing behavior, the seed-stable mapping for valid existing data, and the one-argument entropy-backed compatibility surface.
- **Remaining uncertainty:** The repository never states whether weight 0 is an author-supported way to disable an entry or simply invalid. The processor should settle that policy, plus empty tables and duplicate item ids, before choosing validation at YAML decode, registry construction, or both; the confirmed defect is that the current path silently assigns unintended selection semantics.
