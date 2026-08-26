# Project Review Findings: PRs #432–#412

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #432, #442, #444, #431, #430, #425, #420, #419, #417, #416, #413, and #412 — plus direct first-parent documentation commit `07015dbb` (#429), for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The settings-log gating, production warning policy, algorithm-comment numbering, scripts-directory guidance, and #429 documentation corrections remain accurate in the current tree. The semaphore jobserver added by #444 later caused the tracked multi-worktree deadlock #471 and was replaced with safe per-package `ghc-options: -j`; that repaired historical failure is not duplicated here. Location overlay determinism, supported anchor validation, current five-by-five ruin bounds, and the basic interior front-wall lift retain focused coverage. `Location overlay` passed 32/32, `World.Render` passed 90/90, and `YAML bounds parsing` passed 13/13. The standalone texture-path checker failed on one path that exists only in a Haddock counterexample, which is itself one finding below. No graphical rotation/seam session, location behavior probe, malformed-content boot, full suite, world check, or `make ci` was run. Seven non-duplicate concerns remain; the latent and visual ones preserve uncertainty for the processor to settle before drafting an issue.

## Status

- [x] PRR-1. The texture-path guard is absent from CI and currently rejects prose — [#1705]
- [x] PRR-2. Structure quads do not follow the cylindrical render alias — [#1706]
- [x] PRR-3. Front-wall handling is fixed to the FaceSouth edge pair — [#1712]
- [x] PRR-4. Hidden walls can still lift visible flora and vegetation — [#1715]
- [ ] PRR-5. Guaranteed placement can bypass the location water-safety rule — [deferred]: no measured wet-footprint case
- [ ] PRR-6. A partial location stamp is permanently recorded as complete
- [ ] PRR-7. Non-positive location-content counts become successful empty spawns

## 1. Texture-reference enforcement

### [#1705] PRR-1. The texture-path guard is absent from CI and currently rejects prose

> **Captured note:** Make the all-reference texture-path check an enforced, syntax-aware contract. PR #430 introduced it as the mandatory guard against silent magenta assets, but neither CI nor `make ci` runs it, and its raw line regex currently treats a Haddock counterexample as a real path and fails on `assets/textures/iconsEvil/x.png`.

**Verification:** Verified. Running `python3 tools/check_texture_paths.py` at current HEAD exits nonzero after scanning 4,530 references and reports exactly one missing path from a documentation comment. Repository-wide wiring searches find no invocation from CI, `tools/ci-local.sh`, the Makefile, or Cabal, so the failure has not blocked the change that introduced the false positive.

**Evidence:**

- Issue #428 requires a path-existence check that scans every `assets/textures/...` reference and fails build/CI when one is missing. PR #430 calls `tools/check_texture_paths.py` the guard for the otherwise silent magenta-texture failure mode, but its verification ran the script manually and skipped the requested GUI smoke test.
- `tools/check_texture_paths.py:18-28` defines a broad regex over source text and treats every extension-ending match as a file path. `:30-49` walks raw lines without distinguishing comments, string literals, or generated examples.
- `src/Engine/Preview/Discovery.hs:305-308` contains the deliberate Haddock counterexample `assets/textures/iconsEvil/x.png` while explaining path-boundary checking. The checker reports it as missing even though no runtime loader can consume that prose.
- `.github/workflows/ci.yml:216-232` selects only the established worldgen and graphical expensive gates, while `tools/ci-local.sh:57-113` enumerates all 16 `make ci` checks. Neither invokes the texture-path checker. A complete repository wiring search finds the script mentioned only by prose and the narrower texture-subset audit.
- `tools/texture_subset_audit.py:27-35` explicitly leaves bare-name icon resolution outside its scope and describes itself in relation to `check_texture_paths.py`; it is not an equivalent all-literal backstop.
- `python3 tools/check_texture_paths.py` failed during this review with `MISSING (1)` and attributed the path to `src/Engine/Preview/Discovery.hs:308`. All-state tracker searches found the unit-animation-specific open #1257, but no issue owning this cross-family guard or its false-positive behavior.

**Handoff context:**

- **Current behavior:** The check promised by #428 is advisory and presently red for non-runtime prose. A real moved/missing texture can merge without this tool running, while simply adding it to CI today would make every build fail on a valid comment.
- **Expected behavior:** An enforced gate covers runtime texture references and directory bases without interpreting comments or counterexamples as live paths. It has fixture/self-test coverage for file literals, directory bases, concatenation prefixes, malformed/missing references, and ignored prose, and local CI mirrors hosted CI.
- **Scope and constraints:** Surfaced from PR #430 / issue #428. Preserve bare-name icon resolution, directory-base coverage added in the PR's review fix, generated/runtime fallback policy, and the narrower authoritative inventories owned by later asset issues such as #1257. Do not turn this into a full unused-asset inventory.
- **Remaining uncertainty:** A language-aware parser across YAML, Lua, and Haskell may be disproportionate; a carefully tested lexical policy could be sufficient. The processor should settle the supported reference shapes before prescribing how comments and concatenations are recognized.

## 2. Structure rendering at the cylindrical seam

### [#1706] PRR-2. Structure quads do not follow the cylindrical render alias

> **Captured note:** Route structure pieces through the same facing-aware nearest-wrap placement used by terrain, flora, ground items, blood, spoil, cursors, and their hit tests. `Structure.Render` enumerates stored canonical coordinates and projects them directly, so a room beside a camera parked on the opposite seam alias can render a whole world away or disappear while the terrain beneath it is correctly wrapped into view.

**Verification:** Partially verified. The render paths establish the coordinate-frame mismatch, and the later #1176 implementation explicitly records structure rendering as independently unfixed. The focused `World.Render` suite passed 90/90 because its seam cases cover the six offset consumers and the front-wall lookup helper, not emitted structure geometry. No graphical or offscreen scene placed a room at the seam during this review.

**Evidence:**

- PRs #417 and #419 changed structure and adjacent-sprite sort behavior but retained a separate `renderStructureQuads` path. Their visual verification was not performed before merge; both relied on numeric models and the location-overlay probe, which does not inspect pixels.
- `src/World/Render.hs:170-183` calls `renderStructureQuads` with facing, z slice, depth, and alpha only. It does not pass the camera position, framebuffer/view bounds, world size, or a wrap offset.
- `src/Structure/Render.hs:62-80` reads every loaded chunk's structure map and flattens the stored `(gx,gy)` keys into one list. `:95-105` emits each piece directly, without `isChunkVisibleWrapped` or another nearest-alias decision.
- Each structure producer projects the raw stored coordinate through `gridToScreen`: ordinary pieces at `src/Structure/Render.hs:136-156`, wall strips at `:238-243`, and posts at `:331-348`. None adds a screen-space wrap displacement.
- By contrast, `src/World/Render/ChunkCulling.hs:18-87` defines the facing-aware two-dimensional alias offset and promises that visibility and placement use the same pair. The main terrain pass receives and applies it through `QuadContext` at `src/World/Render/Quads.hs:129-154`.
- `src/World/Render/Quads.hs:463-468` explicitly notes after re-examination under #1176 that `Structure.Render` applies no wrap offset and that a wall's cross-seam screen position remains unfixed. Issue #1176 scoped structures out, and all-state tracker and pending-report searches found no follow-up owner.

**Handoff context:**

- **Current behavior:** Loaded terrain can be selected and drawn through its nearest cylindrical alias at every camera facing, while structure overlays on those same canonical tiles retain the far canonical projection. The room and its ground can therefore separate at the seam.
- **Expected behavior:** Structure visibility, projection, painter depth, and any structure interaction use one coherent nearest alias at all four facings. Interior/non-seam output remains unchanged, and the same physical structure is not emitted twice.
- **Scope and constraints:** Surfaced while rechecking PRs #417/#419 and later confirmed by #1176's explicit deferral. Preserve page isolation, palette-handle resolution, per-strip wall sorting, z-slice filtering, cylindrical U wrapping with bounded V, and the established two-dimensional facing-aware period. Add emitted-quad coverage analogous to `GroundItemSeam`, including FaceWest/FaceEast and a visual/offscreen room check.
- **Remaining uncertainty:** Structure APIs currently have no pointer hit-test path analogous to ground items, so the immediate effect is visual rather than click parity. The processor should verify the smallest camera/seam fixture before assigning severity.

## 3. Rotated front-wall policy

### [#1712] PRR-3. Front-wall handling is fixed to the FaceSouth edge pair

> **Captured note:** Rotate structure edge identity together with the camera before deciding which walls are front-facing, which art they use, which walls receive depth strips, and which walls lift overlapping billboards. The current code rotates sort coordinates but always treats authored `wall_se`/`wall_sw` as the front pair, so the #415/#418 protections do not follow the visible front of a room after rotation.

**Verification:** Partially verified from the implementation. The fixed slot predicates and unremapped-art comment are direct, and no focused structure-render test iterates all facings. A graphical room rotation was not captured, so the exact visible artifact at each orientation remains to be confirmed against the authored sprites.

**Evidence:**

- PR #417 says its sort anchors are rotation-aware and asks for final visual confirmation from multiple rotations; that visual step was not run. PR #419 similarly calls its wall-overlap detection rotation-correct because it uses `applyFacing`, while noting the wall sprite facing as a separate limitation.
- `src/Structure/Render.hs:19-22` states that sort anchors rotate but the per-direction sprite is not remapped: a wall keeps its authored face after camera rotation.
- `src/Structure/Render.hs:88-101` defines `isFrontWall` as exactly `SWallSE ∨ SWallSW` for every `CameraFacing`. Only those two slots receive the 16-strip path introduced to solve #415; NE/NW remain single quads even when rotation brings their edges to the visible front.
- `src/World/Render/Quads.hs:429-481` likewise searches only `SWallSE` and `SWallSW` when deciding whether flora/vegetation needs the #418 lift. `applyFacing` changes the numeric depth comparison but never changes which physical edge tags are candidates.
- `src/Structure/Render.hs:436-470` defines back/front anchors and tie-break groups permanently by slot, while only the selected coordinates pass through `applyFacingF`. At FaceNorth the screen-depth orientation reverses, but the fixed policy groups do not.
- `test-headless/Test/Headless/World/Render/FrontWallLift.hs:61-106` covers FaceSouth and two FaceEast helper cases, but every fixture still contains only an SE wall. It does not assert that the candidate slot set rotates or inspect structure wall geometry/art.
- All-state tracker and pending-report searches found no issue for structure-wall rotation, fixed front-slot classification, or directional sprite remapping.

**Handoff context:**

- **Current behavior:** Rotating the camera changes the projection and depth values while leaving wall texture direction and the special front-wall membership fixed. A different physical pair becomes screen-front, but it does not inherit the strip/lift policy that fixed the original FaceSouth view.
- **Expected behavior:** At every facing, physical wall edges map consistently to screen-facing art and to front/back occlusion policy. The walls currently visible at the front receive the same terrain and billboard protections as the SE/SW pair at FaceSouth.
- **Scope and constraints:** Surfaced from PRs #417/#419 and issues #415/#418. Preserve cap variants, post/wall joints, damaged variants, per-strip UV continuity, z-aware terrain interleaving, and stable tie-breaking. Test all four facings with an asymmetric room/art fixture so a mistaken 90°/180° mapping cannot pass through symmetric geometry.
- **Remaining uncertainty:** The correct policy might rotate slots before rendering or select alternate textures while retaining world-edge identity; the current structure-pack contract does not state that mapping. Visual verification is required before fixing both art and sort behavior as one issue.

## 4. Z-slice-aware billboard lifting

### [#1715] PRR-4. Hidden walls can still lift visible flora and vegetation

> **Captured note:** Restrict the #418 billboard lift to wall pieces that participate in the active structure render slice. `structureFrontWallClear` reads a wall's z only to manufacture a key; it does not reject a wall above `zSlice` or below `zSlice - effectiveDepth`, so an invisible wall can still reorder visible flora/vegetation.

**Verification:** Verified structurally; player-visible severity is unmeasured. The structure renderer and the lift helper apply different eligibility predicates to the same piece. Existing pure tests use only a wall exactly at `zSlice`, so the 90/90 green `World.Render` result does not constrain the mismatched cases.

**Evidence:**

- PR #419 intends to lift a billboard just above the highest front-wall strip it actually overlaps. The helper signature receives `zSlice` but not `effectiveDepth`, even though the wall renderer needs both to decide whether the piece exists in the frame.
- `src/Structure/Render.hs:223-228` returns no strips when `gridZ > zSlice` or `gridZ < zSlice - effDepth`. Ordinary structure quads and posts apply the same active-slice boundary in their own producers.
- `src/World/Render/Quads.hs:426-478` accepts any matching front-wall record. It adds `(spdGridZ - zSlice) * 0.001` to the returned key but never rejects a wall outside the visible slice.
- The caller at `src/World/Render/Quads.hs:172-180` applies that key to a visible flora/vegetation quad through `max`, independently of whether `Structure.Render` emitted the wall.
- `test-headless/Test/Headless/World/Render/FrontWallLift.hs:35-44,61-98` fixes both wall z and camera slice to 5. It has no above-slice or below-depth case, and the helper cannot express the latter because it lacks effective depth.
- Tracker and pending-report searches found no owner for z-slice eligibility in `structureFrontWallClear`.

**Handoff context:**

- **Current behavior:** Lowering the camera slice can hide an upper wall while retaining its positive z-derived clearance key. A lower visible billboard near that tile can still be promoted in painter order as though the wall remained present.
- **Expected behavior:** The helper considers exactly the wall population the structure renderer can emit for the same frame. Removing a wall from the slice also removes its influence on adjacent billboard sort keys.
- **Scope and constraints:** Surfaced from PR #419 / issue #418. Preserve in-slice #418 behavior, wall-strip/tie-break agreement, seam canonicalization, rotation work's eventual slot mapping, and the cheap chunk-near-structures gate. A pure test should cover above-slice, just-inside lower bound, and just-below-depth walls.
- **Remaining uncertainty:** Painter keys are depth-dominated, so the unintended promotion may be visually subtle or masked by other quads in common terrain. The eligibility mismatch is direct; an offscreen image should establish severity before issue drafting.

## 5. Water-safe guaranteed locations

### [deferred] PRR-5. Guaranteed placement can bypass the location water-safety rule

> **Deferred:** Severity is unmeasured and the fix is an undecided three-way choice — a safer builder, tile-level footprint validation, or an explicit no-valid-location outcome. `src/Location/Overlay.hs:230` falls back to the wet pool only when NO land chunk in the world is `dryEnough`, and `dryEnough` is chunk-coarse, so even then water need not reach the 5x5 footprint; the only fixture that reaches this path (`test-headless/Test/Headless/WorldGen.hs:417-460`) contains no actual lakes, rivers, or ocean. Clears when a seed sweep over real generations reports whether any tuple reaches an empty dry pool and, for one that does, whether the guaranteed footprint is adjacent to a real lake, river, or ocean tile.

> **Captured note:** Reconcile the “at least one location” guarantee with the earlier no-infinity-pool contract instead of satisfying one by knowingly violating the other. When strict placement finds no dry candidate, the current fallback chooses wet land and stamps the same lowest-ground room whose proximity to water motivated #416.

**Verification:** Partially verified. The pure placement fixture proves that an all-wet land world receives a guaranteed location, and the source explicitly permits bypassing `dryEnough`. The original failure mechanism—flattening a footprint below adjacent fluid—is unchanged. No generated-world/offscreen fixture showed the guaranteed placement actually undercutting a particular lake, river, or ocean tile, because the chunk-level filter is deliberately conservative and a wet chunk does not prove water touches the five-by-five footprint.

**Evidence:**

- Issue #414 / PR #416 requires no ordinary location stamp where the footprint plus margin is near lake, river, or ocean, because `flattenFootprint` can carve below the adjacent fluid and leave an overhanging “infinity pool.” The implementation's `dryEnough` rule remains at `src/Location/Overlay.hs:261-289`.
- Strict placement enforces `wantsWater ∨ dryEnough` at `src/Location/Overlay.hs:293-326` for definitions such as the shipped flat ruin.
- The later guaranteed path at `src/Location/Overlay.hs:191-218` says it may violate both anchor tags and #414's water-proximity filter. It prefers dry land but falls back to all land when the dry list is empty.
- `test-headless/Test/Headless/WorldGen.hs:349-395` constructs a land world whose every chunk has ocean distance one, confirms the strict path is empty specifically because of #414, and then requires `PlacedGuaranteed` with one location. The passing 32-case `Location overlay` suite therefore pins the safety bypass rather than detecting it as a conflict.
- `scripts/locations.lua:143-173` retains the lowest-surface flattening that triggered #414. The room builders call it before laying structure pieces at `:198-229` and `:263-274`.
- Issue #997's original requirements say the guarantee must construct a valid-looking location at any requested coordinate and must avoid relaxing the strict water-proximity filter. Its merged implementation instead documents the contradiction as an allowed violation; tracker and pending-report searches found no follow-up reconciling the two accepted contracts.

**Handoff context:**

- **Current behavior:** Normal worlds with a strict candidate retain #414 water clearance. A rare world whose strict pass finds none is deliberately given one location on wet land, restoring the pre-#416 risk precisely in the fallback world most likely to be water-constrained.
- **Expected behavior:** A land world still receives a usable location without reintroducing unsupported fluid overhangs. The fallback builder/placement result has an explicit visual and terrain-safety contract even when no ordinary dry candidate exists; genuinely impossible worlds surface an actionable regeneration result.
- **Scope and constraints:** Surfaced by rechecking PR #416 / issue #414 against the later #997 guarantee. Preserve deterministic selection, strict placements byte-for-byte, no-def and `max_count: 0` behavior, canonical coordinates, saveable ordinary location instances, and the expedition requirement for at least one usable location. Any worldgen-output change follows the repository's full output tier and save-version policy.
- **Remaining uncertainty:** The chunk-level `dryEnough` predicate intentionally over-approximates danger, so some fallback placements it rejects may already be visually safe. The processor should build a tile-level wet fallback fixture and decide whether the solution is a safer builder, finer validation, or an explicit no-valid-location outcome rather than assuming every `PlacedGuaranteed` world is broken.

## 6. Atomic location materialization

### PRR-6. A partial location stamp is permanently recorded as complete

> **Captured note:** Mark generated-location geometry complete only after the entire declared footprint has materialized successfully, or make failed pieces retryable. The Lua builder ignores individual terrain/structure placement failures and reports success once it has called the builder; the stamper ignores even that result and persists the completion marker unconditionally.

**Verification:** Partially verified. The failure and marker paths are direct, and `structure.place` explicitly calls an unloaded cross-chunk room target reachable. The shipped five-by-five worldgen ruin is centered far enough inside its host chunk to avoid the case, so no current generated ruin was reproduced partially stamped. The accepted schema has no maximum bounds size, making this a latent extension failure and a possible direct-stamping boundary case.

**Evidence:**

- PR #412 / issue #89 establishes lazy materialization from a durable overlay; PR #413 adds many asynchronous terrain and structure writes. The lifecycle contract requires an unstamped location to materialize later and an already completed one not to clobber player edits.
- `src/World/Thread/ChunkLoading.hs:165-197` dispatches a stamp because the anchor's host chunk loaded. It does not require every chunk intersecting the location's declared bounds to be loaded.
- `src/Engine/Asset/YamlLocations.hs:171-203` validates bounds ordering and fixed point containment but places no maximum footprint or single-loaded-region restriction on a definition.
- `src/Engine/Scripting/Lua/API/Structure.hs:125-153` checks the target chunk before staging/queuing a piece and returns `False` when it is unloaded. Its comment explicitly calls room stamping across a chunk boundary a reachable failure; otherwise staging would create phantom geometry.
- `scripts/locations.lua:198-229,263-310` issues every floor/post/wall call but ignores their Boolean results. `buildAt` at `:317-332` then returns `true` merely because a definition and builder existed, regardless of partial writes.
- `scripts/location_stamper.lua:43-54` ignores `locations.stamp`'s result, calls `world.markLocationStamped` unconditionally, and immediately proceeds to content spawning. A future chunk reload sees the persisted marker and will not retry the missing geometry.
- `YAML bounds parsing` passed 13/13, but those cases stop at parser geometry and the shipped five-by-five fixture. Tracker and pending-report searches found #424's repaired anchor-floor idempotency bug but no owner for failed/partial first materialization.

**Handoff context:**

- **Current behavior:** If any footprint target is unavailable or another builder operation fails, the location can contain only a prefix/subset of its intended terrain and pieces while its durable state says geometry is complete. Contents may still spawn into that incomplete space, and later loads will not repair it.
- **Expected behavior:** The durable marker represents a completed materialization. A failed or partial attempt either rolls back, waits for/requires all footprint chunks, or retains enough pending state to retry safely without duplicating successful work or overwriting player edits.
- **Scope and constraints:** Surfaced from PRs #412/#413 and issues #89/#424. Preserve asynchronous world-thread ownership, page targeting, edit-log persistence, chunk eviction, save-before-first-stamp recovery, read-your-writes staging, content's independent exactly-once marker, and bounds that may legitimately span chunks for downstream spatial consumers. Test a declared footprint crossing a loaded/unloaded boundary and a later retry after the missing chunk loads.
- **Remaining uncertainty:** The supported production placement policy may intend to keep every builder footprint within its anchor chunk forever, despite the schema and spatial contracts not saying so. If that is the decision, load-time validation should enforce it explicitly; otherwise materialization needs a multi-chunk completion protocol.

## 7. Location-content numeric validity

### PRR-7. Non-positive location-content counts become successful empty spawns

> **Captured note:** Validate `count` and `rolls` according to the content kind before a location definition registers. Zero or negative unit/item/building counts and loot-table roll counts currently execute zero loop iterations, emit no warning, and are followed by the same permanent contents-spawned marker as a real successful spawn.

**Verification:** Partially verified. Lua numeric-for semantics and the current dispatch prove the empty execution and permanent marker. All shipped ruin content uses `rolls: 2`, so this is a latent authoring-boundary defect. A zero count might be intended as a temporary content toggle, but neither issue #90 nor the schema comments define that convention, and negative values have no plausible distinct meaning.

**Evidence:**

- Issue #90 / PR #431 defines `count` as how many unit/item/building instances to place and `rolls` as how many loot selections to perform, both defaulting to one. Unknown kinds/ids have explicit warning-and-skip semantics; non-positive quantities are not assigned a meaning.
- `src/Engine/Asset/YamlLocations.hs:32-49` decodes both values directly as `Int` with default one. The definition validation at `:171-218` checks spatial and anchor/naming invariants but never checks content kind or numeric domains.
- `src/Engine/Scripting/Lua/API/Locations.hs:140-147` copies `lycCount` and `lycRolls` unchanged into the runtime content definition and `:212-238` exposes them unchanged to Lua.
- `scripts/locations.lua:388-450` uses `for _ = 1, entry.count` for unit/item/building content and `for roll = 1, entry.rolls` for loot tables. Lua performs zero iterations when the upper bound is zero or negative, so no spawn call and no warning occurs.
- `scripts/locations.lua:512-535` marks the location's contents spawned after dispatching all entries regardless of whether an entry executed any iteration. The malformed quantity is therefore converted into a permanent successful empty result, not a retryable or visible data error.
- `data/locations/ruin_small.yaml:65-81` currently contains only one valid `loot_table` entry with `rolls: 2`. Existing location-content probes exercise positive counts/rolls and unknown ids/kinds, not a malformed quantity. Tracker and pending-report searches found no location-content numeric validation issue.

**Handoff context:**

- **Current behavior:** A typo such as `rolls: 0` loads cleanly, produces no loot and no warning, and permanently records the ruin's content lifecycle as completed. A negative value behaves the same way.
- **Expected behavior:** Every count-bearing entry has a documented, kind-specific numeric domain enforced before registration. Invalid data names the location, entry, field, and value; an explicit disabled-entry feature, if desired, is distinct from accidental non-positive quantities.
- **Scope and constraints:** Surfaced from PR #431 / issue #90. Preserve default one, positive multi-spawn behavior, unknown-id/kind non-crashing policy, deterministic per-instance loot selection, exactly-once lifecycle, fixed/random positioning, and current shipped data. Avoid conflating this with `max_count: 0`, which later #997 explicitly defines as “do not place this location type.”
- **Remaining uncertainty:** Content authors may value zero as a temporary toggle. The processor should decide that policy before drafting; if zero remains legal, reject negatives and make zero's disabled/no-warning/no-spawn semantics explicit and tested rather than accidental loop behavior.
