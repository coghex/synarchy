# Debug Grab acceptance (#2489)

Captured 2026-09-24 in the implementation worktree based on
`d2a782088b4477bef2ba6ec8c965c2dc11bced25`.
[Source fingerprints](source-sha256.json) identify the uncommitted implementation
that produced these captures. All files here belong with the code PR.

## Owner decision

**Accepted by the owner on 2026-09-24:** “Accept and proceed to PR review.”
The decision covered the toggle placement and before/held-uphill/released-downhill
images, with the offscreen 1× / controller-test 2× coverage distinction disclosed.
Canonical opposite-agent PR review remains the next delivery gate.

## Rendered interaction

The capture opens F8, locates `grab_button` through `ui.dumpWidgets`, and clicks
its actual label bounds. Every recorded move uses `input.mouseDown`,
`input.moveMouse`, and `input.mouseUp`; no relocation API is called by the
capture script. Setup alone uses console commands to create worlds, place two
tracked items, set the camera and change live temperatures.

| Scene/item | Before | Held | Released |
|---|---|---|---|
| Arena lantern | [before](arena-lantern-before.png) | [mid-drag](arena-lantern-mid.png) | [after](arena-lantern-after.png) |
| Arena steel axe | [before](arena-axe-before.png) | [mid-drag](arena-axe-mid.png) | [after](arena-axe-after.png) |
| Generated-world lantern | [height 1](world-lantern-before.png) | [height 2](world-lantern-mid-higher.png) | [height 1](world-lantern-after-lower.png) |
| Generated-world steel axe | [before](world-axe-before.png) | [mid-drag](world-axe-mid.png) | [after](world-axe-after.png) |

The generated page uses seed 1, size 8, three plates. The chosen columns have
no ice cover or fluid; the visible pale surface is snow vegetation. The lantern
moves from terrain height 1 to 2 and back to 1. Its stored coordinates are
fractional throughout. These heights are the second (`terrainSurfaceZ`) result
of `world.getTerrainAt`, not the rendered/ice surface.

The arena lantern retains exactly its last accepted row at the
[invalid off-window/off-world pointer](arena-lantern-invalid.png), whose
`world.pickPos` is nil, then resumes on release back over valid ground.
Headless coverage additionally drives an in-viewport nil pick and a refused
destination followed by a valid sample, distinguishing both from identity loss.

The offscreen framebuffer and window are both 1280×720. Captures use camera
zooms 0.30/0.40 in the arena and 0.35/0.40 in the generated world, with UI scales
1.0 and 0.75. The real controller tests use a 1280×720 window with a 2560×1440
framebuffer, asserting the 2× conversion at the pointer-blocking query. Offscreen
boot fixes window and framebuffer to equal dimensions; these images do not
claim a rendered Retina-window check.

## Identity and persistence

[evidence.json](evidence.json) retains each screenshot's complete `item.listGround`
row, effective temperature, terrain height, input position, camera/quads, page,
armed state and dimensions, plus the actual Grab widget and save/load statuses.

| Item | Ground ID | Instance ID | Live temperature before drag and after load |
|---|---:|---:|---:|
| Arena lantern | 0 | 1 | 0°C (no save in this scene) |
| Arena steel axe | 1 | 2 | 0°C (no save in this scene) |
| Generated-world lantern | 3 | 6 | 123°C |
| Generated-world steel axe | 4 | 7 | 87°C |

In the generated world, `item.setGroundTemp` changes each live instance after
spawning. Simulation is paused to prevent cooling during the comparison. The
capture asserts equality of every exposed instance field before and after each
move. It saves the relocated rows, starts another real held Grab, loads that
save using its own request ID, waits for successful publication, then injects
further motion and release. Both complete rows and temperatures still match
the saved state; Grab is disarmed. See [after load](world-after-load.png).

## Reproduction and checks

```sh
cabal build all synarchy-test-headless
python3 tools/debug_grab_capture.py --port 9551 --output /tmp/grab-new-capture
cabal test synarchy-test-headless --test-options='--match "Debug Grab gesture"'
```

The capture output path must not exist. The helper boots its own offscreen
engine with a private resource root and config copy, retains the engine log and
save under `run-root`, and shuts down only its own engine. The successful raw
run for this archive is `/tmp/grab-acceptance-8`; no private config or resource
symlinks are included in this PR archive.

Completed checks (logs retained beside this document):

- Production `cabal build all`: passed, no compiler warnings.
- `Debug Grab gesture`: 32 examples, 0 failures. Real overlay, both subscriber
  orders, deferred click accounting, all cancellation hooks, modal/pointer UI,
  page/generation and instance changes, fractional seams, and real Lua load apply.
- `drag-select deferred capture`, `UI descriptor`, `session teardown`,
  `Ground item move`, `ground item page ownership`, `non-finite spawn geometry`,
  and `input claiming`: passed.
- Lua module budget, Lua registration, persistence inventory and Unicode audits:
  passed. EngineEnv capability audit also passed, retaining its existing 377
  non-blocking pass-on residue reports. No `EngineEnv` state was added.
- Real-input capture and generated-world save/load: passed.

- `persistence_contract_probe.py --port 9553`: passed; all four generations
  structurally identical through three fresh-process save/load/save cycles.

- `cabal run exe:synarchy -- --dump` with the private resource root: passed;
  20,125,735 bytes of valid JSON. See [dump summary](dump-summary.json).

Canonical opposite-agent PR review remains required before delivery is complete.
