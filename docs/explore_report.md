# Code exploration findings: comments that do not match their code

This report records places where a comment, haddock, or inline note makes a
claim the surrounding code does not support. It is produced by a code-first
exploration: a function is read and understood from its implementation alone,
and only then is the prose beside it checked against that understanding. It is
being drafted for later one-at-a-time processing rather than as an issue backlog
or implementation plan.

Findings are keyed `EXPL-N`. (`EXP-N` was not available — `docs/expedition_gameplay_loop.md`
already owns that key space.)

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The walk starts at `app/Main.hs` and descends through whatever that function
actually calls, one call at a time. For each function the implementation is read
first and its behaviour derived from the code — argument handling, ordering,
short-circuiting, edge cases — before any comment on it is read. The comment is
then treated as a claim to be verified, not as documentation to be trusted.
Cross-file claims ("this is the only caller", "the dump walks it twice", "derived
rather than restated") are checked by grepping for the other side rather than
assumed.

A finding is recorded only when the code and the prose genuinely disagree, or
when a comment states an invariant the code does not enforce. Stylistic
disagreements, stale issue references, and wording preferences are not findings.

No build, test suite, probe, or engine boot has been run for this report; every
finding so far is derived by reading source and is verifiable by reading the
cited lines.

## Status

- [ ] EXPL-1. `climateRegionSize`'s comment calls the climate grid coarser than the geological grid, when it is finer

---

## Worldgen configuration and region grids

### EXPL-1. `climateRegionSize`'s comment calls the climate grid coarser than the geological grid, when it is finer

`src/World/Weather/Types.hs:46-50` declares the climate region grid:

```haskell
-- | Climate regions are 4×4 chunks = 64×64 tiles.
--   This is coarser than your geological RegionCoord (8×8 chunks)
--   but you could unify them if you prefer.
climateRegionSize ∷ Int
climateRegionSize = 4  -- in chunks (so 64 tiles per side)
```

The comparison is inverted. With `chunkSize = 16` (`src/World/Chunk/Types.hs:95`)
and `regionSize = 8` chunks per geological region side
(`src/World/Region/Types.hs:29`):

| Grid | Chunks per side | Tiles per side | Area |
|---|---|---|---|
| Climate region (`climateRegionSize`) | 4 | 64 | 4,096 tiles |
| Geological region (`regionSize`) | 8 | 128 | 16,384 tiles |

A climate region covers one quarter of the area of a geological region, so the
climate grid is the **finer** of the two, not the coarser one. The arithmetic in
the first line of the comment is correct (`4 × 16 = 64`); only the comparison in
the second line is wrong.

This is more than a wording slip because a second module derives a
world-generation invariant from exactly this relationship.
`src/World/Generate/Config/Normalize.hs:15-18` reads:

```haskell
-- | Smallest supported world side, in chunks. A world must contain at
--   least one complete region for every region grid used by generation.
minimumWorldSize ∷ Int
minimumWorldSize = max regionSize climateRegionSize
```

`max` here selects the grid with the **larger** cell, which is the geological
one — the correct choice, since a world large enough for one geological region
is automatically large enough for one climate region. A reader who trusts the
`climateRegionSize` comment would expect that `max` to be selecting the climate
grid, and would mis-derive why the minimum world side is 8 rather than 4. The
value is right; the stated reason for it becomes unfollowable.

Two secondary observations in the same area, neither yet incorrect:

- `normalizeWorldSize` (`Normalize.hs:25-31`) rounds a world side up to the next
  multiple of `minimumWorldSize` and its comment claims this makes "every region
  grid tile the world exactly." That holds today only because 8 happens to be a
  multiple of 4. The code composes the two grid sizes with `max`, not `lcm`, so
  if the two sizes ever became mutually indivisible (say 8 and 12) the stated
  invariant would silently stop holding while the code kept compiling and
  generating worlds. The comment states a guarantee the expression does not
  provide.
- The phrasing "your geological RegionCoord … but you could unify them if you
  prefer" addresses the reader rather than describing the code, and reads as an
  unedited assistant-authored comment left in place.

Verification: read `src/World/Weather/Types.hs:44-50`,
`src/World/Region/Types.hs:19-29`, `src/World/Chunk/Types.hs:94-95`, and
`src/World/Generate/Config/Normalize.hs:15-31`. No build or test run is needed —
all four values are literals.
