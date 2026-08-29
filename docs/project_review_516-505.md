# Project Review Findings: PRs #516–#505

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #516, #515, #514, #513, #512, #511, #510, #509, #508, #507, #506, and #505 — for later one-at-a-time disposition. The first-parent window contains no direct commits between those merges.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The current tilling path and its later dedicated art, wire builder, page-owned power network, cooking content, crafting-bill semantics, power-node placement, and unified build tool retain their intended behavior in source and focused regression coverage. Later work repaired several original limitations: #590 replaced #515's always-on workshop load with active recipe-level demand, #795 made `UntilStock` a live persisted bill mode, and #1205/#1207 made power placement and wire topology page-owned. #511's surviving load-boundary concern is already captured more broadly as `LUA-3` in `docs/lua_script_findings.md`, so it is not duplicated here. #509's animation-inventory enforcement gap is already covered by open #1257. `python3 tools/texture_subset_audit.py` passed all 13 subsets, and the focused `World.TimeLocal` suite passed all 9 examples. No graphical session, 512/1024 world generation, full probe, full suite, world check, or `make ci` was run. Three non-duplicate current concerns remain.

## Status

- [x] PRR-1. Secondary visible worlds inherit the active page's solar uniforms — [#1869]
- [ ] PRR-2. Create World advertises sizes with unsafe whole-world memory bounds — [deferred]: no isolated w512/w1024 residency measurement
- [x] PRR-3. Powered-workshop probe descriptions document the superseded consumer model — [#1871]

## 1. Multi-world rendering

### [#1869] PRR-1. Secondary visible worlds inherit the active page's solar uniforms

> **Captured note:** Preserve page identity through world rendering far enough to apply each visible page's own clock and circumference. The renderer batches every page in `wmVisible` together, but the one frame UBO carries only the head page's `sunAngle` and world size, so secondary pages with different clocks or sizes are lit as if they belonged to the active page.

**Verification:** Verified structurally against the current multi-world, clock, render, and shader paths. Visible pages advance independent clocks and can have independent generation parameters, but only the first visible page publishes the shared render-facing sun angle. The renderer then merges every visible page's quads before one uniform upload, and the vertex shader applies that one angle and circumference to every world vertex. A secondary page therefore cannot receive its own solar inputs when the pages disagree. No graphical reproduction was run.

**Evidence:**

- Issue #483 / PR #512 introduced longitude-local rendering around one global `sunAngle` plus one `worldCircumferenceTiles`; the implementation predates the later complete multi-page session contract.
- `src/World/Thread/Time.hs:33-38` advances every entry in `wmVisible` using that page's own `wsTimeScaleRef`, so visible page clocks can legitimately diverge.
- `src/World/Thread/Time.hs:105-110` then derives `wsSunAngleRef` from only the head of `wmVisible`.
- `src/World/Render.hs:69-96` builds terrain quads for every visible page and merges them into one layer map, discarding page identity at that boundary.
- `src/Engine/Loop/Frame.hs:283-290` explicitly supports multiple visible pages in the shared batch stream and performs one uniform update after the pages have been merged.
- `src/Engine/Loop/Frame.hs:354-382` uploads the shared `sunAngleRef` and `activeWorldCircumferenceTiles` once for the frame; `activeWorldCircumferenceTiles` itself resolves only `activeWorldState` at `:66-73`.
- `src/Engine/Graphics/Vulkan/ShaderCode.hs:116-135` computes every vertex's local phase from those two UBO values and its packed longitude. There is no page-specific input left in the vertex or batch.
- `src/World/Load/Publish.hs:164-178` restores a save's complete visible-page set with the active page at the head, making this a supported persisted state rather than a synthetic manager shape.
- Closed #797 fixed the analogous unit-awareness bug by reading the defender's page-local clock and size, and explicitly declared redesigning the global render UBO/sun-angle path out of scope. Targeted all-state tracker and findings-report searches found no separate rendering follow-up.
- `test-headless/Test/Headless/World/TimeLocal.hs:1-61` covers the pure longitude formula for one w64 circumference. The focused suite passed 9/9 during this review, but it cannot exercise page attribution or the one-UBO render boundary. #797's two-page tests cover gameplay awareness, not rendered lighting.

**Handoff context:**

- **Current behavior:** When two visible pages have different clocks, every secondary page renders with the active page's time of day. When their world sizes differ, it also divides longitude by the active page's circumference. Reordering `wmVisible` changes which page's solar state is imposed on all of them.
- **Expected behavior:** Each rendered page derives longitude-local light from its own `wsTimeRef` and `wsGenParamsRef`, independent of visible-list order. A multi-page save restores the same per-page lighting relationship it had before save.
- **Scope and constraints:** Surfaced from PR #512 / issue #483 after the multi-world architecture made simultaneous pages real. Preserve the unwrapped seam interpolation, packed world coordinates, face-map lighting, stable texture handles, page batching correctness, and single-page output. Because page identity is currently merged away before uniform upload, the repair may require page-scoped draw batches, another per-vertex/per-instance input, or another explicit render contract rather than merely changing which page fills the existing UBO.
- **Remaining uncertainty:** The bad input attribution is unavoidable in the current data flow, but its visual severity depends on how much simultaneously visible pages overlap on screen and how far their clocks/sizes differ. No GPU/offscreen screenshot comparison was made.

## 2. World-generation resource bounds

### [deferred] PRR-2. Create World advertises sizes with unsafe whole-world memory bounds

> **Deferred:** No isolated peak-residency measurement exists above w256, so the issue cannot state which advertised sizes it gates or what threshold it gates on — the only w512 datapoint (PR #508) ran two generations concurrently and was killed at 63/64 GB. Clears when one isolated generation each at `--worldSize 512` and `--worldSize 1024` has been run on the reference machine with `+RTS -s` and its maximum residency recorded, which is the owner's scheduling call on a shared box.

> **Captured note:** Give world size an explicit supported resource boundary. The normal Create World picker advertises 512 and 1024 chunks, while generation materializes quadratic whole-world bordered caches whose raw vector payload alone reaches several to tens of GiB. Reject or clearly preflight sizes beyond a measured budget, or redesign the pipeline so an ordinary menu choice cannot exhaust the host.

**Verification:** Partially verified from exact current allocation shapes and the original PR's recorded w512 attempt. The source establishes large lower bounds before HashMap/list/vector-object overhead, simultaneous intermediate retention, terrain/timeline state, or the rest of the engine. Actual peak residency remains hardware-, evaluation-, and GC-dependent, and this review deliberately did not launch a 512/1024 generation that could pressure the shared workstation.

**Evidence:**

- `scripts/create_world/settings_tab.lua:37-43` presents `Huge (512)` and `Massive (1024)` as ordinary selectable sizes, alongside 32–256, with no resource qualification.
- `src/World/Generate/Config/Normalize.hs:25-31` enforces only a minimum and region multiple. There is no upper bound; `world.init` forwards the normalized result at `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:143-162`, so scripting can request values beyond even the menu's 1024.
- `src/World/Generate/InitTerrain.hs:53-62` defines each `BorderedTerrainCache` entry as two unboxed vectors of `(chunkSize + 2 * chunkBorder)^2 = 44^2 = 1,936` cells. `src/World/Geology/Timeline/Stitch.hs:49-63` constructs one entry for every coordinate in the `worldSize × worldSize` chunk grid before collecting them into a `HashMap`.
- On the supported 64-bit targets, the raw payload is at least one 8-byte `Int` plus one 1-byte `MaterialId` per cell (`src/World/Material/Id.hs:26-33`). That is about 4.25 GiB at w512 and 17.02 GiB at w1024 for this cache's component vectors alone, before container and allocation overhead.
- `src/World/Generate/InitTerrain.hs:64-84` adds #508's `PlateBaseCache`; its own comment records about 134 million entries at the UI's w1024 maximum. The component-vector payload for those `(Int, MaterialId)` entries is another approximately 1.13 GiB while live.
- `src/World/Geology/Timeline.hs:172-199` also stitches whole-world elevation/material grids and transforms the timeline cache into a finished bordered cache. A single w1024 stitched elevation/material pair has about 2.25 GiB of raw component payload; evaluation order determines how much overlaps, so this figure is not added to the preceding values as a claimed exact peak.
- PR #508's review measured w256 at roughly 3.04 GB maximum residency. Its follow-up w512 experiment ran two generations concurrently, drove the 64 GB machine to 63/64 GB used, and was stopped to avoid system-wide degradation; no w512 completion figure or w1024 evidence was produced before merge.
- Targeted all-state tracker and findings-report searches for w512/w1024 generation memory, `PlateBaseCache` memory, and the advertised Massive option found no existing issue or finding.

**Handoff context:**

- **Current behavior:** A normal player can select a size whose generation retains at least a 17 GiB raw bordered-cache payload, and a script can request still larger quadratic allocations. The only UI warning says larger worlds take longer; there is no memory estimate, preflight, supported maximum, or prompt before generation begins.
- **Expected behavior:** Every accepted menu/API size has a documented and tested resource contract. Inputs beyond it fail promptly with a clear explanation or require an explicit informed override; alternatively the generation pipeline streams/tiles its intermediate state so the advertised choices fit the intended hardware envelope.
- **Scope and constraints:** Surfaced while reviewing PR #508 / issue #500's performance cache. Preserve deterministic worldgen output and the current small-world performance win. If an algorithmic change affects worldgen output, it pays the repository's tier-3 baseline/save-version cost; a pure admission-control or preflight change should not. Existing oversized saves, if any, need a deliberate load policy rather than an accidental allocation attempt.
- **Remaining uncertainty:** No universal safe maximum can be inferred from source alone, and the exact live peak for one isolated w512/w1024 run was not measured here. The finding records the missing resource contract and the demonstrably large lower bounds, not a proposed hard-coded maximum.

## 3. Probe documentation

### [#1871] PRR-3. Powered-workshop probe descriptions document the superseded consumer model

> **Captured note:** Update the probe registry and README to describe the behavior `power_workshop_probe.py` now exercises: recipe-level `power_draw` that becomes demand only for an actively worked bill. The current summaries still call it a `requires_power` workshop consumer, a field/model that #590 deliberately superseded for crafting.

**Verification:** Verified by comparing the current executable probe to its two user-facing registry descriptions. The probe creates a station with no building-level drain and asserts that idle/claimed-only bills draw zero, while an actively worked power-drawing recipe registers demand. Both summaries instead describe the old #361 always-on building-consumer contract.

**Evidence:**

- Issue #361 / PR #515 originally specified a workshop-level `requires_power` plus drain model. The merged implementation reduced that to one `power_drain` field, and later #590 replaced it for crafting with job-dependent recipe demand.
- `tools/power_workshop_probe.py:2-18` explicitly says #590 supersedes the original #361 crafting model: recipes carry `power_draw`, idle stations and merely claimed bills draw nothing, and demand begins only while a bill is actively worked.
- `tools/power_workshop_probe.py:32-45` states that no shipped craft station sets `power_drain`, `power.isBuildingPowered` is therefore trivial for them, and the probe's synthetic station deliberately has no building drain.
- `tools/run_probes.py:196-197` still labels the probe a “requires_power workshop consumer” and omits active bill state or recipe `power_draw`.
- `tools/README.md:341` repeats the same obsolete description, attributing unpowered refusal and day/night drain to a `requires_power` workshop consumer.
- Current recipe fixtures in `data/recipes/smelting.yaml` and `data/recipes/machining.yaml` use `power_draw`; no shipped `data/buildings/*.yaml` sets `power_drain`.
- Targeted all-state tracker and findings-report searches for the stale `requires_power` probe description found no existing issue or finding.

**Handoff context:**

- **Current behavior:** `run_probes.py --list` and the probe catalogue tell maintainers that the registered scenario gates an always-on building consumer. Someone selecting coverage from those surfaces can reasonably believe it tests `requires_power`/`power_drain`, even though its key assertions are active recipe demand, self-exclusion, pause/release transitions, and bill stall/resume.
- **Expected behavior:** Registry, catalogue, probe header, and current data model agree that this is the #590 job-dependent recipe-power scenario, while retaining #361 in its provenance where useful.
- **Scope and constraints:** Surfaced from PR #515 / issue #361 after #590 superseded its crafting model. This is documentation-only; preserve the existing probe behavior and keep the hypothetical generic `bdPowerDrain` path distinct from the shipped recipe-driven crafting contract.
- **Remaining uncertainty:** None about the mismatch. The processor may reasonably combine the two one-line description repairs with adjacent probe-documentation maintenance instead of filing a standalone implementation issue.
