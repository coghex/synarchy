# Dig / Mining Issues Review

Found 6 concrete issues.

## 1. High — spoil from different materials can compact into the wrong terrain type — FIXED 2026-06-15

**Files:** `src/World/Spoil/Types.hs:123-134`, `src/World/Thread/Command/Edit.hs:385-407`

`slotUsable` only forbids material mixing **within one vertex pile**. But spoil promotion assumes all four corners of a tile share one material and picks the first corner's pile material for `WeAddTile`. If granite spoil and loam spoil fill different corners of the same tile, one material is silently converted into the other.

**Fix:** `slotUsable` now also enforces the no-mixing rule at the TILE level via the new `tileMatConflict` helper — a slot is refused if any other corner of its tile (across the four vertices) already holds a different spoil material. Routes through `spoilCapacity`/the dig-refusal gate, so no destination → dig refused, material never vanishes (design intent). Promotion's single-material assumption is now an enforced invariant. Regression test: `test-headless/Test/Headless/World/Spoil.hs` (`World.Spoil`, 6 examples).

## 2. High — re-designating a partially dug tile wipes its real dig state

**Files:** `src/World/Thread/Command/Cursor.hs:245-255`, `src/World/Mine/Types.hs:61-81,140-161`

The designation path always overwrites existing entries with a fresh `designationFromSlope`, and that function can only reconstruct a coarse 0/1 corner shape from the slope mask. That loses exact `mdCorners` progress and also drops `mdChunkProgress`, so dragging the mine tool over an in-progress tile can reset or distort mining progress and yield state.

## 3. High — tiles with exposed water become effectively unmineable on the next designation

**Files:** `src/World/Thread/Command/Cursor.hs:223-226`, `src/Engine/Scripting/Lua/API/World.hs:823-831`, `src/World/Edit/Apply.hs:41-45`

Designation captures `lcSurfaceMap`, but digging reads material at `mdZ` from `ctMats`. Once groundwater is exposed, surface z can sit above terrain z, so the designation points at water height, `getDigInfoAt` returns `nil`, and miners refuse the job. That conflicts with the intended “successive digs through revealed water continue to advance terrain” behavior.

## 4. Medium — miners only evaluate one designation, so one bad nearest tile can block all other jobs

**Files:** `scripts/unit_ai.lua:2366-2385`

The AI asks `world.nearestMineDesignation` for exactly one tile, then gives up if that tile is claimed, unloaded, spoil-blocked, or uses a tool the unit lacks. There is no fallback search, so a unit can ignore other valid designations that are only slightly farther away.

## 5. Medium — the AI’s `spoilBlocked` check is not using the same position as the real dig refusal

**Files:** `src/Engine/Scripting/Lua/API/World.hs:837-852`, `src/World/Thread/Command/Edit.hs:211-223`, `scripts/unit_ai.lua:2552-2559`

The Lua-facing `spoilBlocked` flag is computed from the tile center, but the world-thread refusal uses `spoilStartVertex (ux, uy)`. In asymmetric terrain, the center can look clear while the miner's actual working side is blocked, so the AI keeps calling `world.digTile` and the world thread keeps refusing it.

## 6. Medium — dig claims are not restored coherently across save/load

**Files:** `scripts/unit_ai.lua:2293-2304`, `scripts/unit_ai.lua:3131-3139`, `src/Engine/Scripting/Lua/API/Save.hs:138-143`

Active reservations live in the module-local `digClaims` table, but the save hook only serializes `aiState`. `engine.loadSave` restores Lua blobs in place before the engine-side world restore, so old `digClaims` can survive a load even though per-unit dig jobs were replaced from save data.
