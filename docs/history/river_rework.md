# River Rework

This document describes a new river runtime model for Synarchy.

It is **not** a roadmap or implementation schedule. It is a design brief for a new system: what it owns, how it relates to world generation, how it interacts with the simulator, and how it should behave at chunk boundaries.

## Core idea

The current system mixes three different concerns:

1. **World generation** determines where rivers should exist.
2. **Chunk generation** places river fluid into per-chunk `FluidMap`s.
3. **The simulator** mutates those same chunk-local river fluids afterward.

That is workable for static river placement, but it is not a great fit for a streamed world where the camera can move upriver and the active source of water should move with the loaded region.

The new model should split those concerns cleanly:

1. **Worldgen owns river geometry and topology.**
2. **The simulator owns live river water.**
3. **Loaded chunks do not own authoritative river fluid state.**

The important shift is that the simulator should not think in terms of "each loaded chunk has its own independent river water map." It should think in terms of **a river graph with runtime state**, and loaded chunks should only receive a derived view of that state for rendering, queries, and interaction.

## High-level model

### Worldgen responsibilities

World generation should continue to produce the static, deterministic parts of rivers:

- carved riverbeds
- river path tracing
- per-river identity
- segment ordering
- source and mouth locations
- confluences
- nominal discharge / flow strength
- downstream direction
- connections to lakes and oceans

Worldgen should define **what the river network is**.

It should not be the long-term owner of **live river water occupancy** in loaded chunks.

### Simulator responsibilities

The simulator should own:

- where river water currently exists
- how much water is currently moving through a reach
- which loaded frontier acts as the current source
- which ocean / lake / unloaded continuation acts as the current drain
- local runtime adjustments caused by streaming, player edits, or later dynamic systems

The simulator should define **how water is currently moving through the already-generated river network**.

## The river graph

The new system should treat each river as part of an immutable world-level graph.

### Graph contents

A river graph should contain:

- **Nodes**
  - headwaters
  - confluences
  - lake inlets / outlets
  - river mouths
  - chunk-boundary crossing points if useful as explicit graph anchors

- **Edges / reaches**
  - ordered river segments between nodes
  - per-reach downstream direction
  - nominal flow / discharge
  - geometric path information
  - bed elevation profile
  - width / valley width

- **Relationships**
  - upstream reaches
  - downstream reach
  - parent river / tributary identity
  - terminal sink type

This graph should be generated once from worldgen and remain immutable.

### Why use a graph

A graph matches what rivers actually are in gameplay terms:

- a headwater feeds a trunk
- tributaries merge into larger rivers
- a mouth drains into a sink
- the river exists continuously even when only part of it is loaded

That is much closer to the intended runtime behavior than "there is water painted into each chunk."

## Runtime river simulation object

The live water state for rivers should live in a separate runtime object, not in `LoadedChunk.lcFluidMap`.

Example conceptual object:

- `RiverSimWorld`
  - immutable river graph reference
  - loaded chunk set
  - active river reach states
  - source/drain state for each connected loaded subgraph
  - cached chunk projections for render/query

Per-river or per-reach runtime state might include:

- current discharge
- current surface profile
- current filled extent
- whether the reach is active in the loaded window
- whether its upstream side is fed by a true headwater, a loaded upstream reach, or a frontier source
- whether its downstream side drains to a true sink, a loaded downstream reach, or a frontier outflow

The key property is:

> **River water is authoritative in the river sim object, not in chunk-local fluid maps.**

## Chunk ownership

### What chunks should still own

Chunks should still own static terrain and static fluids:

- terrain surface
- carved riverbed terrain
- ocean fluid
- static / reservoir lake fluid
- lava
- ice

Chunks may also own precomputed river metadata for convenience:

- which river reaches intersect this chunk
- which tiles belong to each reach's channel mask
- where the river crosses chunk edges
- local tile masks for rendering

### What chunks should not own

Chunks should not own the authoritative live state of river water:

- not the current river source
- not the current flowing extent
- not the current runtime-filled river channel
- not the current cross-chunk continuity state

Those belong to the river sim object.

### Why this helps

This avoids the worst boundary problem in the current model:

- chunk A thinks it has a source at its edge
- chunk B thinks it has a different source at the opposite edge
- both sides mutate their own local `FluidMap`
- consistency has to be repaired after the fact

With a separate river sim object, the boundary is no longer a fluid ownership boundary. It is only a **projection boundary**.

## Sources and drains

The new system needs explicit source and drain semantics.

### Source types

#### 1. True headwater source

This is the real upstream origin of the river.

When the actual headwater area is loaded, the simulator should use that as the source of inflow.

There should be exactly one true source for a simple river, or multiple true sources for a braided / multi-headwater system if you ever support that.

#### 2. Frontier source

This is a streaming artifact used when the real upstream continuation is not loaded.

If the loaded region cuts through a river and the upstream continuation is offscreen, the simulator should synthesize an inflow at the loaded upstream frontier.

This is the mechanism that makes the source "move upstream" as the camera pans.

Behavior:

- load a mid-river chunk: its upstream edge crossing becomes a frontier source
- load the next upstream chunk: the old frontier source disappears
- the next farther-upstream crossing becomes the new frontier source
- keep doing that until the actual headwater loads
- once the real headwater is loaded, frontier sources are no longer needed for that branch

#### 3. Confluence inflow

When two loaded upstream reaches feed a loaded downstream reach, that is not a synthetic source. It is a normal graph merge.

The downstream reach should receive the sum of upstream inflows.

### Drain types

#### 1. Ocean drain

The ocean is an effectively infinite sink.

For river simulation purposes, an ocean mouth should behave like a fixed drain:

- water can always leave
- it should not back-pressure the river unless you explicitly add tides or flooding later

#### 2. Large lake / reservoir drain

A sufficiently large lake can act as a drain or reservoir.

This should be a worldgen classification, not an ad hoc runtime guess.

Examples:

- caldera lake large enough to be terminal
- tectonic basin lake with stable outlet
- major inland sea

These can be treated as:

- fixed sink
- large storage reservoir
- sink with optional controlled outflow

The exact reservoir model can stay simple at first.

#### 3. Frontier outflow

If a loaded river continues downstream into an unloaded chunk and has not yet reached a true sink, the simulator should allow water to leave through that frontier.

This prevents water from unrealistically pooling at the edge of the loaded window.

Like frontier sources, frontier outflow is a streaming boundary condition, not a world feature.

## Chunk boundaries

Chunk boundaries should stop being hydrological ownership boundaries.

They should only matter for:

- loading/unloading chunk terrain and masks
- projecting river sim state to visible/renderable chunk data
- discovering which graph reaches are currently inside the loaded window

### Boundary crossings

Each chunk should know which river reaches cross its borders:

- north edge crossings
- south edge crossings
- east edge crossings
- west edge crossings

That lets the runtime detect:

- this reach enters from an unloaded upstream chunk
- this reach exits to an unloaded downstream chunk
- this crossing is now internal because the neighbor chunk loaded

### Loaded-window behavior

For each connected loaded subgraph:

1. inspect its upstream ends
2. inspect its downstream ends
3. classify each end as:
   - true source / true sink
   - loaded continuation
   - frontier source / frontier outflow

This gives the simulator enough information to run the river continuously across the loaded window without requiring every chunk to carry its own local version of the river.

## How simulation should work

The simplest useful model is:

1. worldgen creates the immutable river graph
2. chunk load registers intersecting reaches with `RiverSimWorld`
3. `RiverSimWorld` computes active boundary conditions for the currently loaded window
4. the simulator advances water along graph reaches
5. the result is projected back into loaded chunks as a render/query overlay

The important detail is that the simulator is advancing water **on the graph**, not solving river continuity separately inside each chunk.

### Suggested runtime representation

The exact representation can vary, but conceptually each active reach should have:

- inflow
- outflow
- current stored water
- target surface profile along the reach
- a mapping from reach distance to chunk-local tiles

That makes it possible to:

- keep continuity across chunks automatically
- move the active source upstream as new chunks load
- avoid re-solving boundary seams between chunk-local fluid maps

### Projection back into chunks

Loaded chunks still need to render rivers and answer tile queries.

That should happen via a derived overlay:

- `renderFluidAtTile = staticFluid(chunk) merged with riverOverlay(chunk)`

Where:

- `staticFluid(chunk)` is the chunk-owned ocean/lake/lava/etc.
- `riverOverlay(chunk)` is derived from the separate river sim object

This means:

- chunks can stay lightweight
- runtime river state is centralized
- render/debug/query code can still ask for a merged fluid view when needed

## Relationship to the existing sim

Today the sim receives:

- chunk terrain
- chunk fluid map

and then mutates per-chunk fluid directly.

That is not enough for the target behavior because it lacks:

- graph topology
- source/drain classification
- frontier awareness
- authoritative cross-chunk river ownership

The new system should add a dedicated river runtime layer instead of trying to stretch the current passive chunk fluid settle logic into a streamed river-network simulator.

## Relationship to worldgen

Worldgen should still do the expensive deterministic work:

- river tracing
- valley carving
- source/mouth determination
- confluence construction
- discharge estimation
- lake/ocean attachment

That is exactly the information the simulator needs.

The new division is:

- **worldgen computes the river network**
- **sim computes the current water moving through the loaded part of that network**

This is a much cleaner contract than:

- worldgen paints river water into chunks
- sim later edits those same chunk-local river cells

## Static fluids vs river fluids

This design works best if runtime fluids are split into two categories.

### Static or quasi-static fluids

These can remain chunk-owned:

- ocean
- large reservoir lakes
- lava, if it stays worldgen-driven
- ice overlays, if they remain mostly climate/worldgen-driven

### Dynamic river fluid

This should become sim-owned:

- channel occupancy
- moving surface level
- streaming source/drain behavior
- cross-chunk continuity

Smaller dynamic lakes can be added later if needed, but they do not need to block the river rework.

## Recommended invariants

The new system should aim to preserve these invariants:

1. **There is exactly one authoritative owner of live river water.**
   - That owner is the river sim object, not loaded chunks.

2. **Chunk boundaries do not break river continuity.**
   - They only break projection and loading.

3. **Every active loaded river reach has a valid upstream condition.**
   - true source, loaded inflow, or frontier source

4. **Every active loaded river reach has a valid downstream condition.**
   - true sink, loaded outflow, or frontier outflow

5. **Loading farther upstream moves the synthetic source upstream.**
   - old frontier source disappears
   - new farther-upstream frontier source appears
   - true headwater replaces frontier source when loaded

6. **Oceans and designated large lakes are terminal drains.**

7. **Rendering and queries use a merged fluid view, but storage remains split.**

## What another agent should take away

The design target is not "make chunk-local river fluid smarter."

The design target is:

> **Represent rivers as a worldgen-produced immutable graph, and simulate live river water in a separate runtime river object that spans the currently loaded region.**

From that, the rest follows:

- chunk-owned river fluid stops being authoritative
- source/drain logic becomes explicit
- frontier sources and frontier outflows become normal streaming behavior
- cross-chunk seams stop being the center of the design
- loaded chunks become consumers of river sim state instead of owners of it

## Summary

The proposed rework keeps the best part of the current system:

- worldgen already knows where rivers are, how they connect, and how they carved the world

and replaces the weakest part:

- per-chunk ownership of live river water

with a model that better matches the streamed world:

- immutable river graph from worldgen
- separate runtime river simulation object
- explicit source/drain/frontier semantics
- chunk-local projection only for rendering and queries

That should support the desired behavior where river water appears to originate from the currently loaded upstream frontier, moves downstream through the loaded region, and hands off correctly as the camera pans.
