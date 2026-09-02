# Headless mode and the debug console

Reference for driving the engine without a window: boot modes, the
dump layers, the TCP console, and the world-generation query API. The
rules that keep this safe for agents (never open a window, never
`pkill`, which ports to use) are in `CLAUDE.md` §Headless mode; this
file is the lookup table behind them.

## Boot modes

`app/Main.hs` selects exactly one of six boot modes from argv, in this
precedence when more than one selector is present:
`--language-report` > `--dump` > `--preview` > `--offscreen` >
`--headless` > graphical (the default). Per-mode flags and their
validation rules: `app/CLAUDE.md`.

## Starting headless

```bash
cabal run exe:synarchy -- --headless --port 9008 > /tmp/engine.log 2>&1 &
# Wait for the debug server (prints "READY port=NNNN" to stdout)
until grep -q "READY" /tmp/engine.log 2>/dev/null; do sleep 0.2; done
```

The console is required in `--headless`/`--offscreen` (#1190). They
have no window, so if the listener can't start — an occupied or
unbindable port, or `--port 0` (a `--dump`-only sentinel) — the boot
ABORTS: non-zero exit, no `READY` marker, cause on stderr, partial boot
torn down. So the wait loop above fails fast instead of hanging forever
on a live process with no reachable `engine.quit()`. `--dump`, graphical
and `--preview` keep their existing tolerance. Details:
`docs/engine_contracts.md` §Debug-console listener policy. Gates: hspec
`--match "debug-console listener policy"`,
`tools/debug_console_boot_probe.py` (CI-eligible).

Launching the built binary from outside the repo needs a resource root
(`App.ResourceRoot`, #636): `--resource-root <path>` flag >
`SYNARCHY_ROOT` env var > cwd.

```bash
$(cabal list-bin exe:synarchy) --headless --port 9008 --resource-root ~/work/synarchy
SYNARCHY_ROOT=~/work/synarchy $(cabal list-bin exe:synarchy) --dump
```

## Offscreen render mode (#650)

`--offscreen` is GPU on, window off — the full Vulkan pipeline into
offscreen images, no GLFW window/swapchain. Unlike `--headless`, the
REAL UI flow runs (loading screen → menus → HUD),
`debug.captureScreenshot` works, and `input.*` injection (#644) drives
the UI; multiple instances run concurrently on distinct ports.

```bash
cabal run exe:synarchy -- --offscreen --port 9018 --size 1280x720 > /tmp/off.log 2>&1 &
until grep -q "READY" /tmp/off.log 2>/dev/null; do sleep 0.2; done
echo "return debug.captureScreenshot('/tmp/shot.png')" | nc -w 10 localhost 9018
echo "return input.click(640, 260)" | nc -w 5 localhost 9018
echo 'engine.quit()' | nc -w 2 localhost 9018
```

Frames pace on a fixed ~60 fps sleep; window-requiring video settings
no-op with a warning. Gate: `tools/offscreen_probe.py` (manual-only,
`needs-gpu`) — locate click targets via the `ui.dumpWidgets` oracle,
never hardcoded coordinates.

## Dump mode (no TCP, JSON to stdout)

```bash
cabal run exe:synarchy -- --dump > world.json 2>/dev/null
cabal run exe:synarchy -- --dump=terrain,ice --seed 42 --worldSize 32 --region -2,-2,2,2 > ice.json 2>/dev/null
# --plates is the canonical tectonic-plate-count flag (--ages is a legacy alias)
cabal run exe:synarchy -- --dump --seed 1337 --worldSize 256 --plates 5 --region -5,-5,5,5 > world.json 2>gen.log
```

**Layers:** `terrain` (or `elevation`), `material`, `fluid`, `ice`,
`ore` (the default five). `slope` is **opt-in only** so a bare `--dump`
stays byte-identical to historical output (baselines/audits drive it).
Region coordinates are **chunk coords**. Per-tile fields:

| Field | Layer | Description |
|-------|-------|-------------|
| `x`, `y`, `v` | always | Global tile coords and v-axis (gx+gy) |
| `terrainZ`, `surfaceZ` | terrain | Raw terrain and max(terrain, fluid) |
| `waterTableZ` | terrain | Finalized per-tile water-table z from the chunk's own map (climate baseline, fluid/shoreline-adjusted) |
| `waterTableSummer`, `waterTableWinter` | terrain | Seasonal water-table z-levels for the tile, bilinearly interpolated from the climate model |
| `matId` | material | Top surface material ID |
| `fluidType`, `fluidSurf` | fluid | "ocean"/"lake"/"river"/"lava" or null |
| `iceSurf`, `iceMode` | ice | Ice surface Z and "basin"/"drape" or null |
| `oreId`, `oreTopZ`, `oreCount` | ore | Topmost ore band in the column (null/0 if none) |
| `slope`, `hardness` | slope | Slope bitmask (bit0=N,1=E,2=S,3=W; 0=flat) + surface hardness |
| `glacierZone`, `beyondGlacier` | always | World boundary flags |

`python3 tools/ore_report.py` for cross-seed ore statistics.

## Debug console (TCP)

Single-line Lua via netcat; return values auto-serialize (tables →
JSON). The console is single-line only — use semicolons:
`local r=world.getRivers(); return #r`.

```bash
echo 'return world.getInitProgress()' | nc -w 2 localhost 9008
```

## World generation workflow

```bash
# world.init(pageId, seed, worldSize, plateCount
#           [, displayName[, gloss[, languageSeed[, languageVersion]]]])
echo 'world.init("test", 42, 256, 5)' | nc -w 2 localhost 9008
# Block until done (preferred; timeout in seconds)…
echo 'return world.waitForInit(300)' | nc -w 300 localhost 9008
# …or poll: phase 0=idle,1=setup,2=chunks,3=done
echo 'return world.getInitProgress()' | nc -w 2 localhost 9008
# Activate for queries (required before chunk/tile queries)
echo 'world.show("test")' | nc -w 2 localhost 9008
```

The optional identity (#707) is immutable display text, persisted,
independent of pageId and slot name. A name with no languageSeed is a
CUSTOM name with NO language provenance (#1092); languageSeed (#1101, a
decimal STRING — a Word64 has no lossless Lua number) states the name
was RENDERED from that language and is what names the page's placed
locations in the same one. Provenance is never inferred; a malformed
seed is refused with a warning, leaving a custom name. Full contract:
`docs/engine_contracts.md` §World identity and language provenance.

worldSize 256 generates in about two minutes; 512 takes much longer.
Prefer `loadChunksInRegion` + `waitForChunks` over camera movement for
bulk tile loading.

## Query API (returns JSON)

```bash
echo 'return world.getRivers()' | nc -w 5 localhost 9008          # rivers with segments
echo 'return world.getChunkInfo(cx, cy)' | nc -w 2 localhost 9008
echo 'return world.getTerrainAt(gx, gy)' | nc -w 2 localhost 9008 # surfaceZ, terrainSurfaceZ
echo 'return world.getSlopeAt(gx, gy)' | nc -w 2 localhost 9008   # slope bitmask
echo 'return world.getVegAt(gx, gy)' | nc -w 2 localhost 9008     # vegetation id
echo 'return world.isPlantable(gx, gy)' | nc -w 2 localhost 9008  # tilled-soil contract (#333)
echo 'return world.getFluidAt(gx, gy)' | nc -w 2 localhost 9008
echo 'return world.getSurfaceAt(gx, gy)' | nc -w 2 localhost 9008
echo 'return world.getAreaFluid(gx, gy, radius)' | nc -w 5 localhost 9008  # max radius 64
echo 'return world.loadChunksInRegion(cx1, cy1, cx2, cy2)' | nc -w 5 localhost 9008
echo 'return world.waitForChunks(120)' | nc -w 120 localhost 9008
echo 'return camera.getPosition()' | nc -w 2 localhost 9008
echo 'camera.goToTile(gx, gy)' | nc -w 2 localhost 9008
echo 'engine.quit()' | nc -w 2 localhost 9008                     # shutdown
```

## Save and load from the console

```bash
echo 'engine.saveWorld("test", "my_save"); return "saved"' | nc -w 2 localhost 9008
echo 'engine.loadSave("my_save"); return "queued"' | nc -w 2 localhost 9008
# loadSave only ACCEPTS synchronously (#763) — poll engine.getLoadStatus()
# for phase == "LoadPublished" (or "LoadFailed", or "LoadReconciliationFailed")
# before touching anything. Loaded pages keep their saved ids (no
# main_world remap) — world.getActiveWorldId() finds the active one.
echo 'return engine.getLoadStatus()' | nc -w 2 localhost 9008
# Loads come up paused: engine.setPaused(false) (in-game: scripts.pause,
# which also restores the time scale).
```

Budget ~15 s after a 128-world load before querying tiles — chunks
queue progressively after `LoadPublished`. NB #365: a save containing
an arena page hangs the world thread on load — never use arenas as a
save-test page.

## Subsystem probes

Each subsystem has a turnkey `tools/*_probe.py` gate that boots a real
headless engine. `tools/README.md` lists them all;
`python3 tools/ci_probes.py --status` is the authoritative list of every
probe's CI eligibility — never trust a prose list of probe names.
`python3 tools/run_probes.py --only <substrings> [--jobs N]` runs a
subset (bare run = full sweep, tens of minutes).
