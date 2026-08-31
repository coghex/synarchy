# Rendered ground-item lifecycle and hit-test agreement test design

Test design state: `ready for implementation`

Source proposal: `20260831T184754Z-proposal-probe-ground-item-render-c5724c`

Source test ID: `probe:ground-item-render`

Source ref and commit: `origin/master` at `740bbf5edd29a9e0bdfe38479f3c04c63425fc37`

Designed against: `origin/master` at `b94fdb65466078656762b05274fa7ae375af6efd`

Accepted at: `2026-08-31T18:54:58Z`

Implementation authorization: `not granted`

Recommended tier: `manual-only`

## Purpose and coverage gap

Prove that one known ground item completes the real GPU-backed lifecycle: its
shipped texture reaches the Vulkan framebuffer, its painted location agrees
with the production hit-test and mouse-routing path, and removing it clears
both paint and hit state. This guards the integration boundary among item
definitions, texture handles, `World.Render.GroundItemQuads`, the world render
pass, and real gameplay input.

The headless `GroundItemSeam` examples cover shared geometry, culling, and
hit-test arithmetic, and `item.debugQuads()` can execute the render-quad
builder without touching a GPU. Existing item probes cover state and item
lifecycle APIs. None of those checks shows that a resolved ground-item texture
is sampled into a real frame, that a player click at the painted item selects
the same ground-item id, or that a removed item disappears from both systems.

## Binding decisions

- ok this is fine, but its not a hard gated ci test, its a low priority optional test. either make it as a design doc or as an issue if its small enough
- Preserve this as a test design rather than a tracker issue because the
  fixture, GPU pixel oracle, real input route, lifecycle cleanup, runner
  registration, and reliability controls require coordinated choices. This
  decision does not authorize implementation or issue creation.
- The result is an optional, low-priority, manual-only probe. It must not enter
  `CI_ELIGIBLE`, a path-selective CI mapping, `make ci`, a pre-push check, or
  any other required gate.
- Keep the graded scope to one shipped item in one static scene: appearance,
  input agreement, selection identity, and removal. Inventory presentation,
  pickup into a unit, AI fetching, save/load, stacks of overlapping items,
  broken-item overlays, underwater tint, seams, and aesthetic approval remain
  excluded.

## Current repository evidence

At the design commit, `World.Render.GroundItemQuads` derives rendering and
`hitTestGroundItemAt` from the same `itemGeometry` function. The render path
resolves the item definition through `ContentRegistriesViewCapability`, reads
the stable texture handle, emits a world-layer quad, and adds a selected
outline when the cursor names that ground-item id. This is the integration
changed by `57589627`; capability and structural audits cannot demonstrate its
framebuffer result.

`item.debugQuads()` calls the actual quad builder and reports total ground
items, emitted quads, camera state, and a bounded sample containing quad
rectangles, texture handles, face-map handles, and sort keys. Its sample does
not contain ground-item ids. The probe therefore must establish a singleton
fixture and correlate the returned spawned id with `item.listGround()`,
`item.hitTestAt()`, and `item.getSelected()` rather than pretending the debug
sample directly identifies a gid. Extending production instrumentation is a
fallback only if that singleton correlation proves insufficient during
implementation.

The production left-click route in `scripts/init_mouse_entity.lua` is active
only in the HUD's `zoomed_in` band. It checks units, then ground items, then
buildings; an item hit calls `item.select(gid)`, clears the other selections,
and records `item_select`. A raw arena boot does not by itself prove that this
HUD and routing lifecycle is established. The existing offscreen and
construction-blueprint probes already demonstrate the real main-menu to
generated-world/HUD flow, pinned generation, `ui.dumpWidgets()`,
`input.click()`, screenshots, and action-outcome draining. This design uses
that established route instead of mutating `hud.currentView` directly.

The shipped `tomato` definition names
`assets/textures/items/supply/tomato.png`, a small red sprite present at the
design commit. It is a suitable distinctive fixture, but the test remains
about the binding and lifecycle rather than the tomato's artistic appearance.
`tools/probelib.py` supplies owned engine boot and teardown, window/framebuffer
size queries, coordinate conversion, camera pinning, and polling. The probe
registry and its complete CI/manual-only classification live in
`tools/run_probes.py` and `tools/ci_probes.py` respectively.

## Scenario and boundaries

Boot one invocation-owned `--offscreen` Vulkan engine at a fixed extent using
a leased port, private resource root, private artifact directory, and private
log. Follow the real rendered main-menu and create-world controls into a pinned
generated world, reach the gameplay HUD, dismiss any notification cards, enter
the `zoomed_in` view, pause simulation, and pin the camera. Use authoritative
world queries to choose a loaded, visible, dry tile near screen centre with no
unit or building overlapping the intended item rectangle. Treat inability to
establish this scene as setup failure.

Require the active page to contain no ground items, then capture two stable
no-item frames. Spawn exactly one `tomato` near the chosen tile centre with a
non-broken condition and retain the returned gid. Poll until
`item.listGround()` contains exactly that gid and `item.debugQuads()` reports
one ground item and one emitted quad with finite geometry, texture, face-map,
and sort-key fields. Capture two stable item frames.

Locate the item's production window-space hit region with a bounded scan of
`item.hitTestAt(px, py)` around the pinned tile. Refine its edges sufficiently
to obtain a reliable bounding rectangle and at least one interior point whose
framebuffer pixel changed between the stable baseline and item frames. Require
the changed pixels attributable to the item to lie inside that hit rectangle,
allowing only a documented small edge tolerance for rasterization. Convert the
chosen window point through the live window-to-framebuffer transform before
injecting input.

Clear existing selections and action outcomes, then send one real
`input.click()` at that framebuffer point. Require the route to select the
spawned gid, clear unit/building selection, and record one `item_select`
outcome without a unit/building selection, move order, or terrain-deselect
fallback. Capture the selected frame as diagnostic evidence; the outline is
useful corroboration but is not a golden-image requirement.

Call `item.deselect()` to remove the selection outline from the cleanup
comparison, then remove exactly the spawned gid with `item.removeGround()`.
Poll until the ground list is empty, `item.debugQuads()` reports zero ground
items and zero quads, and every previously positive sampled hit point returns
nil. Capture two stable post-removal frames and shut down through the shared
teardown path on success, failure, setup failure, or exception.

## Oracle

Engine startup, command success, screenshot decodability, and clean shutdown
are apparatus checks; they are not evidence that the rendering behavior
passed. Setup is inconclusive unless the test establishes the real zoomed-in
HUD, a static unobscured scene, matching live window/framebuffer dimensions,
loaded item definitions and textures, an initially empty ground-item set, the
exact singleton gid, stable frames, and a non-empty production hit region.

The behavior passes only when all of the following agree:

- State: the spawn produces one `tomato` row with the returned gid, and removal
  transitions that exact singleton back to an empty ground-item set.
- Quad construction: `item.debugQuads()` transitions from zero quads to one
  well-formed quad and back to zero, without an unknown definition, culled
  fixture, or broken-overlay second quad.
- Paint: the spawned frames contain a bounded, repeatable pixel delta above
  the scene's measured repeat-capture noise inside the production hit region;
  a state-present but paint-absent item fails.
- Input: one real injected click at a changed interior point yields
  `item.getSelected() == gid` and the `item_select` route, with no higher- or
  lower-priority entity branch claiming the gesture; a painted but
  unclickable item or selection of another id fails.
- Cleanup: after deselection and removal, no old positive hit sample returns a
  gid, no quad remains, and the scoped pixels return to the baseline within
  the documented tolerance derived from stable-frame noise; stale paint or
  stale hit state fails.

Measure noise from repeated captures at each lifecycle phase. Set an explicit
per-channel threshold and minimum changed-pixel count in the implementation,
and retain both the raw measurements and the chosen margin. Do not compare a
whole-frame hash, require bit-identical GPU output, or let unrelated animated
HUD pixels satisfy the paint oracle. An ambiguous coordinate transform,
unstable scene, missing texture readiness signal, or comparison whose noise
overlaps its item delta is inconclusive rather than a false pass or product
failure.

Retain the engine log; baseline, item, selected, and post-removal PNGs; their
scoped crops and difference masks; viewport/camera snapshots; ground-item
rows; debug-quad dumps; sampled hit rectangle and point; action outcomes; and
numeric noise/delta summaries.

## Apparatus and integration

Implement `tools/ground_item_render_probe.py`. Reuse `probelib` for lifecycle,
polling, camera and viewport handling; reuse the existing offscreen probe's
screenshot contract and bounded Pillow comparisons rather than introducing a
second capture mechanism. Borrow the established real generated-world/HUD
entry flow and isolated resource-root handling from the offscreen construction
probes. Keep new code local to fixture selection, bounded item hit-region
sampling, scoped image analysis, evidence retention, and setup-versus-behavior
failure classification.

Register the exact key `ground_item_render` and script name
`ground_item_render_probe.py` in `tools/run_probes.py`, with a description that
names GPU paint, real click selection, and removal cleanup. Add independent
`targeted`, `needs-gpu`, and `slow/worldgen-heavy` reasons to
`MANUAL_ONLY_REASONS` in `tools/ci_probes.py`: it is a narrow optional
ground-item regression, requires a Vulkan device unavailable on the CI runner,
and deliberately uses the production generated-world/HUD route. Do not add it
to `CI_ELIGIBLE` or any changed-path selection table. Document the manual probe
and its retained evidence in `tools/README.md`.

Add production instrumentation only if the singleton item list, quad counts,
hit-test identity, and selected gid cannot establish trustworthy correlation.
Any such instrumentation must remain diagnostic and must not replace the real
framebuffer or input assertions.

## Reliability and cost

Expected warm runtime is roughly 1–3 minutes: one offscreen Vulkan boot, one
pinned world generation, and short stable-frame polling. It requires a local
GPU and is intentionally low-priority and optional. Its GPU dependency,
worldgen cost, and narrow focus each independently keep it out of every hard
gate.

Pin generation inputs, camera, zoom band, pause state, item coordinates, and
viewport size. Poll semantic readiness rather than sleeping for correctness.
Measure local frame noise before grading deltas, confine the oracle to the
hit-correlated crop, and use the shipped high-contrast tomato rather than a
platform-specific golden image. Detect and reject notification/UI overlap,
unit/building overlap, missing chunks, off-band input, unresolved texture
state, and background instability as setup failures. Always reap the engine
and preserve enough evidence to distinguish a renderer regression from an
unestablished fixture.

## Implementation plan

1. Add the offscreen probe with owned resources, the pinned real-HUD fixture,
   singleton tomato lifecycle, hit-region discovery, real input routing,
   scoped noise-aware image comparisons, retained evidence, and cleanup on
   every exit path.
2. Add engine-free tests for coordinate conversion, hit-region refinement,
   crop clipping, image-noise/delta classification, and setup/behavior exit
   mapping. Use synthetic images with transparent and edge pixels so the
   pixel oracle cannot pass on a whole-frame change.
3. Register `ground_item_render`, add all three manual-only reasons, and update
   the probe documentation without adding a CI selector or required-gate
   invocation.
4. Run the focused self-tests and registry audit, then execute the exact probe
   once on a GPU-capable host and visually inspect its retained crops and masks
   against the state-backed verdict before considering implementation done.

## Validation and handoff

Focused validation is the probe's engine-free self-test, its new helper unit
tests, `python3 tools/ci_probes.py --self-test`, and one explicit GPU run:

`python3 tools/run_probes.py --only ground_item_render --exact --jobs 1`

The coordinated `$test` workflow should discover it through the normal
`run_probes.py --list` inventory and `ci_probes.py --status`, and select it only
occasionally when ground-item definition/texture registration, ground-item
quad construction, world rendering, or the corresponding entity hit-test/input
route changes. Its optional status is part of the design: it stays absent from
hard-gated CI, default suites, `make ci`, pre-push validation, and all other
mandatory paths even after implementation.

## Open questions

None.
