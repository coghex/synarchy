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
- [ ] EXPL-2. `preRenderWorkers` justifies the sim thread's teardown position with a dataflow the sim thread does not have
- [ ] EXPL-3. `Engine.Core.Workers`'s module haddock says "the two windowless modes" when there are three
- [ ] EXPL-4. `shutdownEngineWorkers`'s stated reason for not announcing is false for both of its normal-path callers
- [ ] EXPL-5. `stopWorkers` claims to be the only `WorkerSlot` traversal in the tree; `App.Boot` has another
- [ ] EXPL-6. `App.Boot` haddock links to `Engine.Core.Workers.allWorkers`, which is not exported
- [ ] EXPL-7. `sortFrameFiles` claims to be shared by the units and buildings viewers; the units viewer no longer calls it

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

---

## Engine worker teardown

`src/Engine/Core/Workers.hs` is, by its own opening line, "the one
definition of the order [worker threads] stop in." Its haddock does not just
state that order — it explains why the order is correct and which callers
consume it. Those explanations are what a future reader would reason from when
adding a worker or reordering the list, and four of them do not survive being
checked against the code. They are recorded separately because each is wrong
about a different thing and each would be fixed by a different edit.

### EXPL-2. `preRenderWorkers` justifies the sim thread's teardown position with a dataflow the sim thread does not have

`src/Engine/Core/Workers.hs:50-59`:

```haskell
-- | The workers that stop /before/ Vulkan and GLFW teardown, in order.
--
--   Combat and sim lead because they are producers for the unit thread:
--   wound ticks enqueue UnitKill\/UnitCollapse onto the unit queue, so
--   they have to stop before the consumer does. They also stop ahead of
--   the render teardown, which is where the windowed modes have always
--   stopped them.
preRenderWorkers ∷ EngineWorkers → [WorkerSlot]
preRenderWorkers w = [ ("combat", ewCombat w)
                     , ("sim",    ewSim w) ]
```

The claim is true of combat and false of sim.

Combat really is a producer for the unit thread. `src/Combat/Wounds/Tick.hs:127`
and `:132` write `UnitCollapse uid` and `UnitKill uid` onto
`ucUnitQueue (toUnitCombatCapability env)`, and
`src/Combat/Resolution/Events.hs:87` writes another `UnitKill`. Combat must
therefore stop before the unit thread drains that queue, exactly as the comment
says.

The sim thread has no such relationship. `src/Sim/` contains exactly one queue
write — `src/Sim/Thread.hs:371`, onto `wsWorldQueue` — and the entire directory
contains **zero** occurrences of the identifier `Unit`. It drains `simQueue`
and pushes fluid writebacks to the world thread
(`src/Engine/Core/Capability/WorldSim.hs:80-83` lists `SimThread` as a producer
for `wsWorldQueue` and names no unit queue at all).

The teardown order itself is still correct, which is why this has not caused a
bug: sim's real constraint is producer-for-**world**, and it is satisfied
incidentally, because sim is in `preRenderWorkers` while world is in
`postRenderWorkers`. But the comment attributes sim's position to a queue it
never writes and a consumer it never feeds. Anyone reordering this list — or
deciding whether a newly added worker belongs before or after the render
teardown — would be reasoning from a dependency that does not exist, and would
have no way to discover sim's actual one from this file.

Verification: `grep -rn "UnitKill\|UnitCollapse" src/Combat/ src/Sim/` and
`grep -rn "Unit" src/Sim/` (the latter returns nothing).

### EXPL-3. `Engine.Core.Workers`'s module haddock says "the two windowless modes" when there are three

`src/Engine/Core/Workers.hs:4-12`:

```haskell
--   Both teardown paths go through this module. The clean exit reaches
--   it via 'Engine.Loop.Shutdown.shutdownEngine', which stops
--   'preRenderWorkers' before its Vulkan\/GLFW teardown and
--   'postRenderWorkers' after; the fatal-error tail
--   (@App.Boot.handleBootResult@, #1021) and the two windowless modes
--   reach it via 'shutdownEngineWorkers', which stops every worker in
--   one pass.
```

`shutdownEngineWorkers` has exactly three callers: `App.Boot.handleBootResult`
(`app/App/Boot.hs:101`), `App.Headless` (`app/App/Headless.hs:60`), and
`App.Dump` (`app/App/Dump.hs:244`). So the non-fatal pair is headless and dump.

But headless and dump are not "the two windowless modes" — there are three.
`app/App/Offscreen.hs:1-2` opens: "Offscreen boot path (#650): full Vulkan
render with no window — no GLFW at all." Offscreen is windowless and does
**not** reach this module via `shutdownEngineWorkers`; it goes through
`shutdownEngine` (`app/App/Offscreen.hs:88-89`, passing
`stWindow = Nothing`), which correctly splits the two phases around its
`initializeVulkanOffscreen` teardown.

The property that actually separates headless and dump from the other three is
that they run **no render teardown at all**, so there is nothing to split
around and one pass is correct. Naming the set by "windowless" both
mis-identifies which modes are meant and implies, wrongly, that offscreen
collapses its teardown into a single pass.

### EXPL-4. `shutdownEngineWorkers`'s stated reason for not announcing is false for both of its normal-path callers

`src/Engine/Core/Workers.hs:86-90`:

```haskell
-- | Stop every worker the mode started, in @allWorkers@ order, without
--   announcing them — the paths that use this either have no engine log
--   left to write to or are already reporting a fatal error.
shutdownEngineWorkers ∷ EngineWorkers → IO ()
shutdownEngineWorkers = stopWorkers (\_ → pure ()) ∘ allWorkers
```

Of the three callers, only `App.Boot.handleBootResult` matches the stated
disjunction. The other two are on the ordinary, successful shutdown path and
have a fully live logger:

- `app/App/Headless.hs:57-64` runs inside `engineAction`. It logs
  `"Headless engine shutting down..."` at `:59`, calls
  `shutdownEngineWorkers workers` at `:60`, and only then reads `loggerRef` and
  calls `shutdownLogger` at `:61-62`.
- `app/App/Dump.hs:243-246` has the same shape: `shutdownEngineWorkers workers`
  at `:244`, then `readIORef loggerRef` and `shutdownLogger` at `:245-246`.

In both, the logger is alive across the call and neither is reporting a fatal
error, so neither limb of "no engine log left to write to or already reporting
a fatal error" applies.

The real constraint is a type, not a lifecycle: `shutdownEngineWorkers` is
`EngineWorkers → IO ()`, while the announcing path
(`Engine.Loop.Shutdown`, `src/Engine/Loop/Shutdown.hs:53` and `:118`) runs in
`EngineM'` and can therefore reach the engine's logging combinators. Stating a
lifecycle reason instead of the real one invites a future maintainer to "fix"
the missing announcements by threading the logger in, and to be surprised that
the two non-fatal callers already had one.

### EXPL-5. `stopWorkers` claims to be the only `WorkerSlot` traversal in the tree; `App.Boot` has another

`src/Engine/Core/Workers.hs:74-80`:

```haskell
-- | Stop one phase's workers in list order, announcing each by name
--   first. The only traversal of a 'WorkerSlot' list in the tree —
--   'shutdownEngine' splits its two phases through it, and
--   'shutdownEngineWorkers' runs the whole list through it.
```

`app/App/Boot.hs:139-140` traverses one:

```haskell
luaThreadOrAbort env started (Left _) = do
    let live = [ slot | slot@(_, Just _) ← started ]
```

That list comprehension walks a `[WorkerSlot]` and filters it before handing
the result to `stopWorkers`. The claim as written is therefore false.

The claim the comment is reaching for — that `stopWorkers` is the only place a
worker is actually *stopped* — is true, and is the invariant worth stating,
since it is what keeps `shutdownThread`'s idempotence and the announce
behaviour from being duplicated. As written it is an exhaustiveness claim about
traversal, which a reader can falsify with one grep and will then distrust the
rest of the block.

### EXPL-6. `App.Boot` haddock links to `Engine.Core.Workers.allWorkers`, which is not exported

`app/App/Boot.hs:90` and `app/App/Boot.hs:122` both refer to
`@Engine.Core.Workers.allWorkers@`:

```haskell
--   on the mode's stream, tear down every worker it started (in
--   @Engine.Core.Workers.allWorkers@ order), flush the logger, and exit
```

```haskell
--   @Engine.Core.Workers.allWorkers@ teardown order. Passing the real
```

`allWorkers` is defined at `src/Engine/Core/Workers.hs:71` but is absent from
that module's export list (`src/Engine/Core/Workers.hs:13-20`, which exports
`EngineWorkers(..)`, `WorkerSlot`, `preRenderWorkers`, `postRenderWorkers`,
`stopWorkers`, `shutdownEngineWorkers`). Both references therefore name a
symbol no reader can navigate to from the outside, and neither resolves as a
haddock link.

The order they mean is documented and reachable — `allWorkers` is
`preRenderWorkers ⧺ postRenderWorkers`, and the module's own comment at
`:68-70` spells it out as "combat → sim → unit → world → input → Lua". This is
the smallest of the four and is recorded only so a sweep of the module's
documentation is complete.

---

## Preview asset discovery

### EXPL-7. `sortFrameFiles` claims to be shared by the units and buildings viewers; the units viewer no longer calls it

`src/Engine/Preview/Discovery.hs:131-140`:

```haskell
-- | Order a directory's @frame_NNN.png@ files NUMERICALLY, not
--   lexicographically: the shipped names are zero-padded so the two
--   agree today, but an unpadded @frame_10.png@ must not sort before
--   @frame_2.png@. Files whose stem carries no trailing digits sort
--   after the numbered ones, by name, so nothing is silently dropped.
--   Shared by the units viewer ('Engine.Preview.Unit', which re-exports
--   it) and the buildings viewer ('Engine.Preview.Building') so the two
--   can never disagree about frame order.
sortFrameFiles ∷ [FilePath] → [FilePath]
```

The ordering logic is correct and the first half of the comment is accurate.
The sharing claim in the last sentence is not.

`sortFrameFiles` occurs in `src/Engine/Preview/Unit.hs` exactly twice — line 49
(the module's export list) and line 74 (its import list). It is **never called
in that module's body**. The only caller in the tree is
`src/Engine/Preview/Building.hs:288`.

The cause is #1261. The units viewer no longer enumerates source frames at all:
it resolves the unit's compiled atlas through the production loader
(`resolveUnitAtlasesIn`, `src/Engine/Preview/Unit.hs:422`) and derives every
frame from the generated index via `atlasFrames` and the frozen cell arithmetic
`atlasCellUV` (`src/Engine/Preview/Unit.hs:212-220`). Frame order there comes
from the index's recorded per-direction row and column layout, which
`tools/pack_atlas.py` wrote. There is no directory listing left for
`sortFrameFiles` to order.

The stated invariant is therefore vacuous. There is only one consumer, so
"the two can never disagree about frame order" guarantees nothing — and someone
changing frame ordering in the buildings viewer would believe, on the strength
of this comment, that they were also constraining the units viewer. They would
not be, and the units viewer's real ordering authority (the compiled index)
is not mentioned here at all.

Two supporting details, both consequences of the same drift rather than
independent problems:

- The re-export at `src/Engine/Preview/Unit.hs:49` is what keeps the import at
  `:74` from tripping `-Wunused-imports` under CI's `-Werror`. Nothing
  mechanical flags the dead relationship, which is why it survived the #1261
  migration.
- `test-headless/Test/Headless/Preview/UnitAnimation.hs:280-289` covers
  `sortFrameFiles`, importing it from `Engine.Preview.Unit`
  (`:34`). The coverage is real and worth keeping; it now lives in the suite
  for the one viewer that does not use the function, which is a second place a
  reader would infer a relationship that no longer exists.

Verification: `grep -rn "sortFrameFiles" src/ app/ test-headless/` returns the
definition, the `Building.hs` call, the `Unit.hs` export/import pair, and the
test — no call site in `Unit.hs`.
