# Project Review Findings: PRs #474–#461

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #474, #473, #472, #470, #469, #468, #467, #465, #464, #463, #462, and #461 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The current derived-role selection, river/graben bed-depth work, smelting content, item/accessory modifier axis, shared view teardown, craft-quality calculation, workstation operations, item-temperature evolution, and basic tree-designation flow retain their intended core behavior in source and focused coverage. Later work repaired #469's accessory-removal edge through #1209, and the missing temperature presentation from #464 is already tracked by open #1268; neither is duplicated here. The focused `World.FloraGrowth` suite passed 25/25, `Craft.Execute` passed 28/28, `WorldGen.CoastBreach` passed 4/4, and `WorldGen.BedDepth` passed 11/11. No graphical session, chop/craft behavior probe, generated-world coastline survey, full suite, world check, or `make ci` was run. Three non-duplicate concerns remain; all preserve uncertainty for the processor to settle before drafting an issue.

## Status

- [x] PRR-1. Recipe counts accept zero and negative values with success semantics — [#1940]
- [ ] PRR-2. Tagged harvests turn lifecycle-ineligible trees into full-yield trees — [deferred]: #1854 exact-instance harvesting
- [x] PRR-3. The coastline-variety issue closed after PR 1 without its fjord slice — [#1947]

## 1. Recipe count validity

### [#1940] PRR-1. Recipe counts accept zero and negative values with success semantics

> **Captured note:** Reject non-positive counts on recipe input, fuel, and output lines before a recipe enters the catalogue. The current YAML parser accepts them; consumption treats a non-positive demand as already satisfied, while output construction clamps a non-positive count to zero. A typo such as `count: 0` on an input can therefore turn an otherwise ordinary recipe into a successful free-output craft.

**Verification:** Partially verified. The parser and execution path prove the behavior without a live engine: a non-positive input or fuel count consumes nothing and succeeds, a positive output is still appended, and a non-positive output silently produces no instances. No shipped recipe currently uses a non-positive count, so this is a latent authoring-boundary defect rather than a demonstrated player exploit. It remains possible that a zero-count line is intentionally accepted as optional data, although neither the schema documentation nor the linked issue records such a meaning.

**Evidence:**

- Issue #325 required count-bearing input, optional fuel, and output entries, with all-or-nothing consumption and production. PR #462 states that count defaults to one, but neither artifact defines zero or negative counts as valid semantics.
- `src/Engine/Asset/YamlRecipes.hs:20-30` decodes every ingredient/fuel/output count directly as `Int` and applies only the default of one. `:49-74` validates `repair_axis` but performs no count-domain validation before the definition is accepted.
- `src/Engine/Scripting/Lua/API/Craft/Recipe.hs:47-70` converts the decoded values unchanged and inserts each recipe into the live catalogue. Runtime `engine.loadRecipeYaml` therefore exposes the same path as startup loading.
- `src/Craft/Execute.hs:20-34` deliberately returns the inventory unchanged for `n <= 0`. `consumeIngredients` at `:60-71` applies that helper to recipe inputs and fuel, so a zero or negative demand is reported as satisfied rather than malformed or short.
- `src/Engine/Scripting/Lua/API/Craft/Execute.hs:174-229` rolls outputs, consumes the accepted demands, and appends the outputs in one successful craft. Output construction at `:241-260` uses `replicateM (max 0 count)`, silently turning a zero or negative output count into an empty output line.
- The focused suite explicitly pins `takeItemsByName`'s generic `n <= 0` no-op behavior and otherwise tests valid positive counts. Its YAML fixtures at `test-headless/Test/Headless/Craft/Execute.hs:102-109,170-177,201-208` also show that empty input/output lists are accepted, but no test distinguishes an intentionally empty list from an invalid non-positive line.
- All shipped `data/recipes/*.yaml` count values are positive. All-state tracker and pending findings-report searches for non-positive recipe counts, free crafts, or recipe numeric validation found the closed foundation issue but no follow-up for this boundary.

**Handoff context:**

- **Current behavior:** A recipe line with `count: 0` or a negative count loads successfully. On an input or fuel it imposes no demand; on an output it produces nothing. A recipe with a mistyped non-positive input and a normal output can be executed repeatedly without that input.
- **Expected behavior:** Every present input, fuel, and output line has a strictly positive count, enforced at the YAML/catalogue boundary with a useful load error. Execution should not silently reinterpret malformed authored counts as successful no-ops.
- **Scope and constraints:** Surfaced from PR #462 / issue #325. Preserve count's default of one, all-or-nothing inventory mutation, duplicate input-plus-fuel summing, and the generic helper's no-op contract if other callers need it. Do not conflate this with empty-list policy: repair recipes legitimately have no outputs, and existing parser tests intentionally construct empty lists. `work: 0` is also an established instantaneous/default recipe contract and is outside this finding.
- **Remaining uncertainty:** There is no bad shipped datum today, and an author might use a zero-count line as a temporary content toggle. The processor should confirm whether that convenience is intentional; if it is, the public schema needs to say so and execution should make the result conspicuous rather than silently accepting negative values too.

## 2. Tagged flora lifecycle semantics

### [deferred] PRR-2. Tagged harvests turn lifecycle-ineligible trees into full-yield trees

> **Deferred:** The yield half cannot be specified — no one has defined what a sprout or standing-dead tree should drop — and tagged harvest still selects the first matching instance on a tile (`Forage/Harvest.hs:127-131`) rather than the designated one, so a growth-aware yield would be attributed to the wrong plant. Clears when approved open #1854 lands exact per-instance harvesting on a stable `FloraInstanceId`, after which tag-scoped eligibility and growth-aware yield specify together against an exact instance.

> **Captured note:** Separate "this designated plant may be removed" from "this growth state earns the species' normal harvest yield." Sprout and standing-dead trees are intentionally allowed to remain choppable, but the generic tagged-harvest bypass also gives either state the same static 2–6-log roll as a mature tree and would bypass season/age gates for any future harvest tag.

**Verification:** Partially verified. Current source unconditionally skips `harvestOpen` whenever any tag argument is present, and the only shipped tagged caller passes `"wood"`; the static yield table proves that age does not affect the resulting log count. The focused lifecycle tests passed and prove that the underlying growth predicate rejects sprouts and dead plants, but they exercise only the bare-harvest contract. No live probe forced the same tree through sprout, mature, and dead states, and the right deadwood/juvenile yield policy is a product decision.

**Evidence:**

- Issue #332 required one lifecycle to drive harvestability, and PR #463's merged description says dead plants and juveniles never yield. The same merge nevertheless integrated #461's deliberate requirement that a designated sprout or standing-dead tree remain choppable, creating two distinct concepts that share one harvest verb.
- `src/World/Flora/Growth.hs:170-192` implements the promised predicate: `harvestOpen` rejects dead, sprout, seedling, withering, and dead-phase instances before applying any seasonal fruiting window.
- `src/World/Thread/Command/Cursor/Chop.hs:55-103` accepts a designation solely from a matching harvest tag and an expired regrowth timer. It never consults the instance's derived growth state; this is consistent with the current explicit ability to remove an unwanted juvenile or dead tree.
- `scripts/unit_ai_chop.lua:143-151` likewise documents that `harvestable` is the bare-forage signal and intentionally keeps sprouts/dead trees choppable. At completion, `:221-229` calls `world.harvestFlora(x, y, "wood")`, grants woodcutting XP, and clears the designation.
- `src/Engine/Scripting/Lua/API/Forage/Harvest.hs:114-139` implements tagged execution with `Just _ -> True`, bypassing `harvestOpen` for every tag rather than applying a wood-specific removal policy. `:154-195` rolls the species' static yield with no phase, age, health, or dead-state input.
- `data/flora/temperate_deciduous.yaml:2-18,54-66` gives white oak distinct sprout/mature/dead phases but one 3–6-log wood yield; paper birch repeats the shape at `:85-101,137-144` with 2–4 logs. A day-zero sprout and a mature specimen therefore use the same species-level roll.
- `test-headless/Test/Headless/World/FloraGrowth.hs:195-218` passes explicit assertions that sprouts and dead plants do not satisfy `harvestOpen`. `tools/chop_probe.py:7-23,132-154` deliberately ignores that flag for wood and verifies only tag presence/regrowth state; it never asserts the chosen tree's phase or phase-sensitive yield.
- Current Lua callers use tags only for wood; all food/farm harvests are bare. All-state tracker and pending findings-report searches found #332/#97 and the documented current exception, but no issue separating removal eligibility from lifecycle-sensitive yield.

**Handoff context:**

- **Current behavior:** A player can designate and fell a sprout or standing-dead deciduous tree, which is intentional. Completion treats it as a normal tagged harvest, starts the full stump-regrowth timer, awards normal XP, and rolls the same species-configured logs as a mature tree. Any future caller passing `"fruit"`, `"grain"`, or `"leaves"` would also disable the lifecycle and seasonal gates.
- **Expected behavior:** Removal eligibility and material yield are explicit, separately tested policies. If sprouts and dead trees must remain removable, their output is deliberately defined rather than inheriting the mature static roll by accident; non-wood tags do not automatically become universal lifecycle bypasses.
- **Scope and constraints:** Surfaced from PRs #463/#461 and issues #332/#97. Preserve designation stability, tagged disambiguation on mixed-flora tiles, the bare-forage query/action contract, regrowth protection, saveable designations, and the intentional ability to clear juveniles/deadwood. Prefer a tag-specific policy or a distinct removal/felling operation over weakening `harvestOpen` globally. Add direct tests for tagged sprout, mature, and dead execution, including returned yields and regrowth/XP consequences.
- **Remaining uncertainty:** A standing-dead tree may reasonably yield a mature quantity of usable wood, while a sprout almost certainly should not; the correct split is not specified. It is also possible the game intentionally abstracts every placed tree instance as harvest-sized despite its sprout art. The processor should settle those semantics before prescribing yield scaling or refusal.

## 3. Coastline delivery completeness

### [#1947] PRR-3. The coastline-variety issue closed after PR 1 without its fjord slice

> **Captured note:** Reconcile issue #220's closed state with the second coastline PR that #472 explicitly deferred. The merged first PR added coast profiles, tectonic steepness, polar steepness bias, and generic sealed-basin breaches, but no later change delivered or measured recognisable tectonic/glacial fjord inlets.

**Verification:** Partially verified. The historical tracker state is exact: PR #472 calls itself part 1 of 2, says it does not close #220, and names fjord inlets as PR 2; issue #220 closed at the same second that PR merged, and repository/PR history contains no subsequent #220 fjord implementation. Current source still exposes only the first-PR mechanisms and its measurement categories. What remains unverified is the visual premise: generic glacier carving and sealed-basin breaches may already produce enough fjord-like terrain that a maintainer intentionally accepted the result without recording the rescope.

**Evidence:**

- Issue #220's acceptance requires four readily discoverable, geographically appropriate coast forms. Two requirements specifically call for deep, steep-walled fjords associated with rising tectonic boundaries and a distinct high-latitude glacial coast with more fjords/ice-shaped shorelines.
- PR #472 says it is "Part 1 of 2," explicitly says "Does not close #220," and assigns carved fjord inlets to PR 2. Its result table measures cliffs, steep shores, ramps, beach plains, and marshes; it has no fjord or glacial-coast result.
- The issue timeline records #220 closing at `2026-07-03T03:21:46Z`, one second after PR #472's merge timestamp. Searches of all merged PRs referencing #220 find only #472 plus later structural/cross-reference work, not the promised second implementation.
- `src/World/Geology/Coastal.hs:132-149` combines tectonic steepness with a polar bias so polar shores read rocky/steep. The shaping logic at `:190-252` selects marsh/delta or a gentle-to-cliff profile and then invokes generic basin breach; it does not carve inlet morphology from latitude, glacier paths, or convergent-margin identity.
- `src/World/Geology/Coastal/Breach.hs:22-62` qualifies already-existing sealed sub-sea basins by size/depth and connects them by a short channel. The pass is independent of latitude and tectonic boundary type, excludes glacier material from carving, and only says a long channel may *read* as a fjord inlet.
- `tools/coast_report.py:1-16,31-40,137-170` remains the #220 measurement tool but classifies only `cliff`, `steep`, `ramp`, `beach_plain`, and `marsh`. It cannot establish the issue's fjord/glacial-coast acceptance criteria.
- `test-headless/Test/Headless/WorldGen/CoastBreach.hs:1-102` passed all four synthetic contracts for basin connection, lower-only shaping, glacier exclusion, and seam projection. None associates an inlet with convergent or polar geography or asserts a recognisable fjord form.
- Repository-wide source/history and pending findings-report searches found pre-existing generic glacier-carving references and the closed #220 work, but no later coast-specific fjord slice or duplicate finding.

**Handoff context:**

- **Current behavior:** Coast generation distinguishes plains, marsh/deltas, ramps, and steep/cliff shores. Polar latitude biases a shore toward steepness, and a qualifying sealed depression may receive a narrow outlet. There is no tracked or measured guarantee that uplift margins or glaciated polar coasts form the deep inlet shapes #220 accepted.
- **Expected behavior:** Either complete and measure the deferred fjord/glacial-coast slice, or explicitly rescope #220 and document why the existing cliff/breach/glacier mechanisms satisfy the visual requirement. The tracker should not silently present a deliberately incomplete two-PR plan as complete.
- **Scope and constraints:** Surfaced from PR #472 / issue #220. Read `docs/hydrology_pipeline.md` before locating any new water/carving stage. Preserve wrap-seam consistency, lower-only coast shaping where that remains the chosen invariant, glacier-boundary protection, final-river ordering, and the already improved beach/marsh/cliff mix. Any worldgen-output change requires the repository's full output tier, regenerated tracked baselines, world check, and a save-version bump.
- **Remaining uncertainty:** No fresh graphical survey or multi-seed coast report was run. Existing glacier paths predate #472 and generic breached basins can look fjord-like, so the missing artifact may be tracker/design closure rather than necessarily missing terrain code. The processor should begin with visual, geography-correlated evidence instead of assuming a second carver is required.
