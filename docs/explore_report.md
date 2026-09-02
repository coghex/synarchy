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

A finding is recorded when the code and the prose genuinely disagree, when a
comment states an invariant the code does not enforce, or when a claim is
imprecise in a way that would mislead a reader who takes it literally. Stylistic
disagreements and wording preferences are not findings.

The prose in question is usually a comment or haddock, but the same standard
applies to repository documentation that describes the code — `CLAUDE.md`, an
audit tool's docstring, a contract document — and to the code itself where a
walk turns up a stated convention the tree does not follow (EXPL-21).

**Calibration (owner, 2026-08-20): small findings are the point.** A comment
that is correct in spirit, or correct on a quick read, but imprecise in its
literal claim still counts — the standard for this report is precise and correct
language, not defensible-on-charitable-reading language. Findings are therefore
recorded regardless of how consequential they are, and each one states its own
severity honestly rather than being inflated or suppressed to fit a bar.

No build, test suite, probe, or engine boot has been run for this report; every
finding so far is derived by reading source and is verifiable by reading the
cited lines.

## Status

- [x] EXPL-1. `climateRegionSize`'s comment calls the climate grid coarser than the geological grid, when it is finer — [#2176]
- [x] EXPL-2. `preRenderWorkers` justifies the sim thread's teardown position with a dataflow the sim thread does not have — [#2182]
- [x] EXPL-3. `Engine.Core.Workers`'s module haddock says "the two windowless modes" when there are three — [#2186]
- [x] EXPL-4. `shutdownEngineWorkers`'s stated reason for not announcing is false for both of its normal-path callers — [#2188]
- [x] EXPL-5. `stopWorkers` claims to be the only `WorkerSlot` traversal in the tree; `App.Boot` has another — [#2193]
- [x] EXPL-6. `App.Boot` haddock links to `Engine.Core.Workers.allWorkers`, which is not exported — [no-issue]
- [x] EXPL-7. `sortFrameFiles` claims to be shared by the units and buildings viewers; the units viewer no longer calls it — [#2195]
- [x] EXPL-8. `discoverBuildingEntries` claims every frame is lstat-proved regular; its static-entry branch tests only the name — [#2199]
- [x] EXPL-9. `buildPreviewUnit`'s pipeline summary still describes the filesystem-first flow #1261 replaced — [#2201]
- [x] EXPL-10. `App.LanguageReport` claims constant runtime "regardless of how many seeds are requested"; the work is linear — [#2206]
- [x] EXPL-11. `runPreview`'s `mBrowse` haddock says a `Nothing` is "the degenerate no-target case"; that case never reaches it — [#2208]
- [x] EXPL-12. `anySegmentIsSymlink` says it checks "any prefix" and "every ancestor"; the root itself is never lstat'd — [#2209]
- [x] EXPL-13. `shutdownEngine`'s inline safety argument says Vulkan teardown precedes "the worker threads" stopping; two are already stopped — [no-issue]
- [x] EXPL-14. `migrateLegacyConfig` reports a destination write failure as a malformed legacy file, in a user-facing warning — [#2210]
- [x] EXPL-15. `Unit.Atlas.Digest`'s description of the digest stream omits the tag's length prefix and the label field entirely — [#2211]
- [x] EXPL-16. `pickFrame`'s "used by the render path and the hit-tester" omits both Lua API consumers — [#2213]
- [x] EXPL-17. `unitToQuad`'s climb-occlusion branch is justified by a `spriteRowSpan` push the same function says it does not apply — [#2214]
- [x] EXPL-18. `Building.Render`'s sort comment says units add a `spriteRowSpan` term "as units do"; they no longer do — [#2215]
- [x] EXPL-19. `Unit.HitTest` claims to mirror a tile hit-test that no longer holds the math, and to use the "same math" the engine documents as different — [#2216]
- [x] EXPL-20. `resolveTexture`'s haddock credits itself with animation mirroring; animations never reach it and it cannot honour `flip: false` — [#2218]
- [x] EXPL-21. 98 production call sites spell inequality `≠` instead of `≢`; neither the operator audit nor CLAUDE.md's table can see it — [#1494]
- [x] EXPL-22. The capability inventory says four of the five capability splits are §3.1 thread-privacy splits; two are, and the doc contradicts itself — [#2219]
- [x] EXPL-23. `inputBoundaryPage` explains the modal-boundary tie-break by `PageHandle` and show-recency; the sort key is `(upLayer, upZIndex)` — [#2223]
- [ ] EXPL-24. `hitsAtPointBy`'s haddock cites `UI.InputOwnership.pagesInScope`, which is not exported — [deferred]: EXPL-27 dispositioned
- [x] EXPL-25. `isPointerSurfaceBlocked` names `Engine.Input.Thread` as its caller; since #787 that is `Engine.Input.Thread.Mouse` — [#2225]
- [x] EXPL-26. `World.Save.Storage`'s header says it "receives only" four of six parameters, and its numbered transaction omits the requirement-9 refusal — [#2226]
- [ ] EXPL-27. 59 cross-module haddock links point at functions their named module does not export, concentrated on module-split seams
- [ ] EXPL-28. `Engine.Asset.YamlVegetation`'s summary names a `data/vegetation.yaml` that does not exist; the data is a directory of five files
- [ ] EXPL-29. `engine_contracts.md`'s enum-audit coverage counts are each off by one, in the paragraph that says not to hand-count them
- [ ] EXPL-30. CLAUDE.md says `tools/README.md` "lists all ~85" probes; there are 89 and README names 83
- [ ] EXPL-31. `Unit.Transfer`'s header calls its serializable set "the six types" and then names seven
- [ ] EXPL-32. `runGatedByCaptureLock` cites "the same shape every other owner uses" and names Combat, which acks only while paused
- [ ] EXPL-33. CLAUDE.md's `text_wrap.lua` summary names two functions and four consumers; there are three functions and ten
- [ ] EXPL-34. `flattenItemInstances` says "all three now go through this one definition" and enumerates three; there are four
- [ ] EXPL-35. `loadVegetationYamlFn`'s body says it parses "the single vegetation YAML file"; both Lua callers loop over five
- [ ] EXPL-36. `computeAmbientLight`'s inline labels name noon and midnight; its input convention puts those values at dawn and dusk
- [ ] EXPL-37. The Tier 3 damage model's derivation block is stale in two places: a `modeCoupling` constant that does not exist, and a `delivered` formula missing two factors
- [ ] EXPL-38. Three references outlived the deleted `World.Fluids` facade, one of them a CI path-selector self-test case
- [ ] EXPL-39. `World.Geology.Ore` says its caller is `World.Geology.Timeline.buildAge`; that module exports only `buildTimeline`
- [ ] EXPL-40. `World.Save.Component.Types` says concrete components live in three modules; five define them, and the two omitted are the optional pair
- [ ] EXPL-41. `World.Save.Component.Entities`' header omits the `core-session` dependency from two of its five components
- [ ] EXPL-42. A haddock names `Building.Knowledge.SeedAtSpawn`; the constructor is `SeedWhenBuilt`, and "at spawn" is what the design rejects
- [ ] EXPL-43. `circadian.lua` documents its function as `unit.getCircadianUrge`; no such engine verb is registered

---

## Worldgen configuration and region grids

### [#2176] EXPL-1. `climateRegionSize`'s comment calls the climate grid coarser than the geological grid, when it is finer

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

### [#2182] EXPL-2. `preRenderWorkers` justifies the sim thread's teardown position with a dataflow the sim thread does not have

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

### [#2186] EXPL-3. `Engine.Core.Workers`'s module haddock says "the two windowless modes" when there are three

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

### [#2188] EXPL-4. `shutdownEngineWorkers`'s stated reason for not announcing is false for both of its normal-path callers

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

### [#2193] EXPL-5. `stopWorkers` claims to be the only `WorkerSlot` traversal in the tree; `App.Boot` has another

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

### [no-issue] EXPL-6. `App.Boot` haddock links to `Engine.Core.Workers.allWorkers`, which is not exported

> **Disposition:** No issue — the two `@Engine.Core.Workers.allWorkers@` mentions are code spans, not links: PR #1407 (#1083, requirement 4) deliberately rewrote them from `'…'` when it un-exported `allWorkers`, and the qualified name still denotes the real definition at `Workers.hs:71`.

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

### [#2195] EXPL-7. `sortFrameFiles` claims to be shared by the units and buildings viewers; the units viewer no longer calls it

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

### [#2199] EXPL-8. `discoverBuildingEntries` claims every frame is lstat-proved regular; its static-entry branch tests only the name

`src/Engine/Preview/Building.hs:249-255` states an unqualified invariant about
every frame the buildings viewer produces:

```haskell
--   A frame is always a REGULAR FILE, established by @lstat@ rather
--   than by either existence predicate. A supported extension is a
--   NAME test, so a directory called @frame_001.png@ — or a FIFO, or
--   any other special file — is never a frame: a directory is only
--   ever a container, descended into like any other when its only
--   @.png@ children are themselves directories, and whatever lies
--   beneath is classified by these same rules.
```

`discoverBuildingEntries` has TWO branches that produce a frame, and the claim
holds for only one of them.

The animation branch does exactly what is claimed. `classifyDir`
(`:283-284`) filters candidate names through
`filterM (isRegularFileChild dir)`, and `isRegularFileChild` (`:317-323`) is a
genuine `lstat` — `pathIsSymbolicLink` first, then
`isRegularFile ⊚ getSymbolicLinkStatus`.

The static branch does not. `walk`'s non-directory case (`:272-278`) is:

```haskell
                    isDir ← doesDirectoryExist full
                    if isDir
                        then classifyDir segs' full ⌦ \case
                            Just entry → pure [entry]
                            Nothing    → walk segs'
                        else pure [ staticEntry segs' full
                                  | isSupportedTextureFile name ]
```

The only guard on that path is `isSupportedTextureFile name` — the pure name
test the comment explicitly disclaims — and it is reached for anything
`doesDirectoryExist` reports as not-a-directory. `staticEntry` (`:324-330`)
then writes that path directly into `pbeFrames = [T.pack full]`, so the result
is a frame by this module's own record shape; `PreviewBuildingEntry` carries
`pbeFrames` for animated and static entries alike.

The gap is one the file documents against itself. The inline comment on
`isRegularFileChild` (`:310-316`) explains why an existence predicate is not
enough:

> `'doesDirectoryExist'` misses a FIFO, socket or device node, and
> `'doesFileExist'` means "exists and is NOT a directory", so it ACCEPTS every
> one of them. The type therefore comes from a real `@lstat@`.

`walk` decides the static case on `doesDirectoryExist` alone. So a FIFO, socket,
or device node named `x.png` sitting directly in a building's asset folder
becomes a static `PreviewBuildingEntry` whose one frame path cannot be loaded —
exactly the outcome the module says the `lstat` exists to prevent, and exactly
the class of file the comment names.

A *directory* named `x.png` at that same level is handled correctly: it routes
to `classifyDir`, finds no regular-file PNGs, yields `Nothing`, and is descended
into. So the half of the claim about directories is true and the half spelled
out as "or a FIFO, or any other special file" is not.

Scope note: `Engine.Preview.Discovery.walkFiles`
(`src/Engine/Preview/Discovery.hs:182-196`) has the same structure and the same
absent check, but it makes no regular-file claim anywhere, so it is not a
finding. The defect recorded here is that `Engine.Preview.Building` asserts an
invariant that one of its two frame producers does not enforce.

### [#2201] EXPL-9. `buildPreviewUnit`'s pipeline summary still describes the filesystem-first flow #1261 replaced

`src/Engine/Preview/Unit.hs:392-395`:

```haskell
-- | The whole pre-boot pipeline for @--preview units/\<name\>@:
--   validate the target, discover its animations, resolve its compiled
--   atlases through the PRODUCTION loader, augment the rest from the
--   unit's own YAML, and pick the default selection.
```

That is #887's pipeline. The code beneath it is #1261's, and the same file
states the difference twice.

The body (`src/Engine/Preview/Unit.hs:417-439`) runs four steps:

1. `resolveUnitDir root name` — validate the target;
2. `loadUnitAnimMetaIn resourceRoot unitName` — read `data/units/<name>.yaml`;
3. `resolveUnitAtlasesIn resourceRoot unitName meta` — resolve the compiled
   atlases, **taking that YAML as an argument**;
4. `buildPreviewAnims` + `defaultAnimationName` — assemble and pick the
   default.

Two of the summary's five steps correspond to nothing in that.

**"discover its animations"** — there is no discovery step. The animation set is
the key set of the resolved atlas map, i.e. whatever the compiled index covers.
The module header at `:23-32` says so directly: "That REPLACES #887's
filesystem-first discovery," and adds that an animation folder present on disk
and absent from the YAML "is therefore EXCLUDED from the browse list rather
than rendered from its frames."

**"augment the rest from the unit's own YAML"** — nothing is augmented from the
YAML, and the stated ordering is reversed. The YAML is an INPUT to atlas
resolution, not a pass after it:

```haskell
resolveUnitAtlasesIn root name yamlAnims =
    loadUnitAtlasIndexIn root name (unitAnimFacts yamlAnims)
```

(`src/Unit/Atlas/Yaml.hs:87-88`), where `unitAnimFacts` reduces the declarations
to `YamlAnimFacts` purely so the index can be checked for staleness and reverse
coverage.

Every playback field the viewer displays comes from the index instead.
`buildPreviewAnims` (`src/Engine/Preview/Unit.hs:375-390`) reads `aaFps`,
`aaLoop`, `aaFlip`, `aaPath`, `aaCellWidth`/`aaCellHeight` and `aaDirections`
off `AtlasAnimation`, and those three playback fields are populated at
`src/Unit/Atlas/Index.hs:308-310` from `rawFlip`/`rawFps`/`rawLoop` — the
parsed `index.json` record, not the unit YAML.

The contradiction is internal and thirty lines apart. `buildPreviewAnims`'s own
haddock at `src/Engine/Preview/Unit.hs:359-364` says:

> Every animation takes EVERYTHING from its index record — directions, real
> per-direction frame counts (never the padded column count),
> cell geometry, `@fps@`/`@loop@`/`@flip@` — because that is what the game does
> with it. There is no other branch: since #1261 there is no representation an
> animation could be in that this does not describe.

So a reader who starts at the public entry point is told the viewer discovers
animations from the asset tree and augments them from YAML; a reader who starts
one function earlier is told neither happens. The first is the one a caller
reads.

Contrast rather than a second defect: the sibling `buildPreviewBuilding`
(`src/Engine/Preview/Building.hs:335-338`) summarises its pipeline as "validate
the target (the shared grouped-item containment rule), read the building's own
YAML, discover its entries, and pick the default selection" — which matches its
code exactly, ordering included. Buildings genuinely ARE filesystem-first with
YAML augmentation (see this module's own authority-split header). Units stopped
being that at #1261, and only this summary sentence was left behind, which is
also why the two files now read as if they share a design they no longer share.

---

## Report and dump entry points

### [#2206] EXPL-10. `App.LanguageReport` claims constant runtime "regardless of how many seeds are requested"; the work is linear

`app/App/LanguageReport.hs:1-7`:

```haskell
-- | @--language-report@ boot path (#710): dump every requested seed's
--   generated-language profile, signature, and canonical-expression
--   native/English renderings as JSON to stdout, then exit. Reads the
--   production concept catalogue from disk and does pure computation
--   only — no engine init, no world thread, no Lua, no GPU
--   (requirement 17), so it starts and finishes in a fraction of a
--   second regardless of how many seeds are requested.
```

The premise is true and the conclusion does not follow from it. Skipping engine
init, the world thread, Lua and the GPU bounds the FIXED startup cost; it says
nothing about the per-seed cost, and the per-seed work is plainly linear:

```haskell
      case mapM (buildSeedReport cat currentGeneratorVersion)
                [loSeed .. hiSeed] of
```

(`app/App/LanguageReport.hs:42-43`.) Runtime is O(`hiSeed` − `loSeed`), and
because `mapM` in the `Either` monad forces the entire result list before
`encode` is ever reached (`:47-61`), so is memory — nothing about this path
streams.

The flag's documented domain is what makes that reachable rather than
theoretical. `parseSeeds` (`app/App/Cli.hs:362-375`) accepts any inclusive range
inside the full `Word64` space, and `app/Main.hs:114-115` advertises exactly
that to the user:

```haskell
          hPutStrLn stderr $ "--language-report requires --seeds LO:HI "
              ⧺ "(an inclusive range within 0.." ⧺ show (maxBound ∷ Word64) ⧺ ")"
```

So `--seeds 0:1000000` is a legal and plausible thing to type when hunting for a
rare language, and it will neither start-and-finish in a fraction of a second
nor produce any output until it has built a million reports in memory.

**Severity is lower than the findings above, deliberately recorded as such.**
The canonical in-tree sample is `--seeds 0:255` (`tools/README.md:294`), at
which the claim is true, and nothing shipped drives a large range. This is a
false performance invariant on a flag with a 2^64 domain — the same shape as
EXPL-4 (the stated reason is not the operative one) rather than EXPL-9 (a
pipeline description that no longer matches its code). It is recorded because
the sentence is the only guidance a caller has about what a wide range costs,
and it actively misleads about both time and memory.

Verified truthful in the same file, and noted here so a fix does not disturb it:
the header's `outputInventory` cross-language claim
(`app/App/LanguageReport.hs:52-57`) holds. `tools/language_report.py:149` does
carry its own literal `OUTPUT_INVENTORY`, and `:1098-1108` does `fail()` on any
divergence from the emitted value, naming the generator-only and checker-only
characters in both directions.

### [#2208] EXPL-11. `runPreview`'s `mBrowse` haddock says a `Nothing` is "the degenerate no-target case"; that case never reaches it

`app/App/Preview.hs:34-42`:

```haskell
-- | Run the engine in preview mode: GLFW window + Vulkan, but no world,
--   unit, sim, or combat thread. The input thread is kept so the OS
--   window-close button and the debug console (started inside the Lua
--   thread, same as headless) both work normally. 'mBrowse' is the
--   browsing state @app/Main.hs@ already resolved (discovery,
--   containment, and default selection all done pre-boot — #886/#887/
--   #888); as of #888 every canonical target supplies one, so a
--   'Nothing' here is only the degenerate no-target case.
runPreview ∷ (Text, Maybe Text) → Maybe PreviewBrowse → Maybe Int → IO ()
```

Everything up to the last clause is accurate. The last clause names a case that
cannot occur.

`runPreview` has exactly five call sites, all in `app/Main.hs`, and every one
passes `Just`:

| Site | Argument |
|---|---|
| `Main.hs:173` | `Just (PreviewList entries)` — bare simple category |
| `Main.hs:184` | `Just (PreviewItem entry)` — focused simple item |
| `Main.hs:206` | `Just (PreviewUnitAnims unit)` — `units/<name>` |
| `Main.hs:211` | `Just (PreviewBuildingAssets building)` — `buildings/<name>` |
| `Main.hs:217` | `Just (PreviewList entries)` — `flora`/`structures` item |

The "degenerate no-target case" does not reach here either. It is intercepted
upstream at `app/Main.hs:133-139`, which prints `--preview requires a target`
and exits 1 without ever calling `runPreview`. Main documents that at
`:133-135`, in terms that make the exhaustiveness explicit:

```haskell
        -- @--preview@ with no target at all. Plain 'Nothing' cannot
        -- occur here — 'parsePreview' answering 'Just' is what selected
        -- this mode — and would be the same user error if it did.
```

So `Nothing` corresponds to no reachable case at all, and the
`Maybe PreviewBrowse` parameter is vestigial rather than covering a residual
possibility. A reader trying to work out what a `Nothing` would mean is pointed
at a case the CLI already refuses, and would reasonably go looking for a
call site that produces one.

**Severity: low**, comparable to EXPL-6 and well below EXPL-2, EXPL-8 and
EXPL-9. It is the same shape as EXPL-6 — a pointer to something that is not
there: a symbol that is not exported in that case, a case that cannot happen in
this one.

Verified nominal in the same file, so a fix does not disturb them: the trimmed
topology really is trimmed (`ewCombat`/`ewSim`/`ewUnit`/`ewWorld` all `Nothing`
at `:58-61`), and the input thread really is kept and really is passed as the
already-started worker to `luaThreadOrAbort` at `:53`.

### [#2209] EXPL-12. `anySegmentIsSymlink` says it checks "any prefix" and "every ancestor"; the root itself is never lstat'd

`src/Engine/Preview/Discovery.hs:289-303`:

```haskell
-- | True if 'root' followed by any prefix of 'segs' (checked
--   incrementally, root-outward) is itself a symlink — every ancestor
--   directory as well as the final leaf, so a symlinked directory
--   further up the chain can't smuggle a file discovery would never
--   have reached (walkFiles skips a symlinked directory the moment it's
--   encountered, at whatever depth; this mirrors that one level at a
--   time instead of jumping straight to the final candidate path).
anySegmentIsSymlink ∷ FilePath → [String] → IO Bool
anySegmentIsSymlink root = go root
  where
    go _ [] = pure False
    go acc (s:rest) = do
        let acc' = acc </> s
        isLink ← pathIsSymbolicLink acc'
        if isLink then pure True else go acc' rest
```

Two statements of the same off-by-one, both in the first sentence:

- **"root followed by any prefix of `segs`"** — the prefixes of a list include
  the EMPTY one, and `root` followed by the empty prefix is `root` itself.
  `go _ [] = pure False` returns before calling `pathIsSymbolicLink` at all, so
  the empty prefix is never tested. What the code checks is every NON-EMPTY
  prefix.
- **"every ancestor directory as well as the final leaf"** — `root` is an
  ancestor directory of the item, and it is not among the ones checked.

The BEHAVIOUR is deliberate and correct, which is why this is a wording defect
rather than a security gap. `walkFiles` does not test its own root either: the
only thing `discoverEntries` does to `root` is `doesDirectoryExist`
(`src/Engine/Preview/Discovery.hs:155`), which follows symlinks. So a symlinked
category root is accepted identically by discovery and by focused-item
resolution — precisely the agreement `FocusSymlink`'s haddock at `:44-49` says
the unconditional symlink rule exists to guarantee:

> rejected unconditionally (not just an escaping one) so a bare category listing
> ('discoverEntries', which skips every symlink the identical way) and a
> typed-out item target ('resolveFocusedEntry') can never disagree about the
> same path.

The defect is that the quantifiers are absolute — "any prefix", "every ancestor"
— where the code means "every level strictly below `root`". A reader auditing
containment would conclude the root is covered, and would have to read `go`'s
base case to discover it is not.

**Severity: low.** No behaviour is wrong and no attack is enabled beyond what
`discoverEntries` already permits identically. It is recorded because this
haddock is the containment argument for a pre-boot security check, and an
absolute quantifier in that position should be literally true.

### [no-issue] EXPL-13. `shutdownEngine`'s inline safety argument says Vulkan teardown precedes "the worker threads" stopping; two are already stopped

> **Disposition:** No issue — already fixed by #1408 (PR #1555, `e51b4e96d`, merged 2026-08-22, two days after this report was opened): `src/Engine/Loop/Shutdown.hs:78-83` now says the pre-render phase has already stopped and the Vulkan teardown precedes only the post-render phase, which is the precise statement this finding asked for.

`src/Engine/Loop/Shutdown.hs:50-61`:

```haskell
shutdownEngine targets = do
    logInfoM CatSystem "Starting engine shutdown..."

    stopWorkers announceStop (preRenderWorkers (stWorkers targets))

    state ← gets graphicsState
    let device = vulkanDevice state

    -- Vulkan teardown below runs BEFORE the worker threads stop. That
    -- is safe only while Vulkan objects are touched exclusively by
    -- this (main) thread — workers hand pixel data over via
    -- IORefs/queues and must never call into Vulkan.
```

By the time that comment is reached, the pre-render workers have already
stopped — six lines above it, at `:52`. `preRenderWorkers`
(`src/Engine/Core/Workers.hs:57-59`) is combat and sim, so the Vulkan teardown
runs before FOUR of the six workers stop (unit, world, input, Lua), not before
"the worker threads" stop.

This is more than word choice because the comment is the stated SAFETY ARGUMENT
for the phase split — the reason it is acceptable to destroy Vulkan objects
while workers are still live. A reader checking whether the split is sound is
told the "must never call into Vulkan" constraint has to hold for all six
workers across the teardown, when in fact two are already gone and the
constraint only needs to hold for the four in `postRenderWorkers`. That
overstates what the rest of the engine must guarantee, and understates how much
the ordering above it is doing.

The function's own haddock, eight lines earlier at `:41-48`, states the same
thing precisely:

> combat and sim stop ahead of the render teardown, unit, world, input and Lua
> after it.

So the file already contains the correct sentence; the inline comment is an
imprecise restatement of it sitting at the point a reader is most likely to
stop and check the invariant.

**Severity: low**, and no behaviour is at issue — the ordering is correct and
matches the precise haddock. Recorded because the two sentences describe the
same boundary and only one of them is right.

---

## Engine initialization

### [#2210] EXPL-14. `migrateLegacyConfig` reports a destination write failure as a malformed legacy file, in a user-facing warning

`src/Engine/Core/Init.hs:112-130`:

```haskell
migrateLegacyConfig ∷ ∀ a. FromJSON a ⇒ Proxy a → LoggerState → FilePath → FilePath → IO ()
migrateLegacyConfig _ logger legacyPath localPath = do
  hasLocal ← doesFileExist localPath
  unless hasLocal $ do
    hasLegacy ← doesFileExist legacyPath
    when hasLegacy $ do
      outcome ← try $ do
        eVal ← Yaml.decodeFileEither legacyPath
        case (eVal ∷ Either Yaml.ParseException a) of
          Left err → ioError $ userError $ show err
          Right _  → copyFile legacyPath localPath
      case (outcome ∷ Either SomeException ()) of
        Right () → logInfo logger CatInit $ ...
        Left e → logWarn logger CatInit $
          "Legacy config " <> T.pack legacyPath
            <> " could not be migrated (malformed, partial, "
            <> "schema-incomplete, or unreadable); falling back to "
            <> "the versioned default: " <> T.pack (displayException e)
```

The `try` spans the `copyFile` as well as the decode. A DESTINATION write
failure — a read-only `config/`, a full disk, wrong permissions — therefore
takes the same `Left` branch as a bad legacy file, and is reported to the user
as:

> Legacy config config/video.yaml could not be migrated (malformed, partial,
> schema-incomplete, or unreadable); falling back to the versioned default: ...

All four named causes are properties of the SOURCE file's content. In this case
the source decoded cleanly against the very type the real loader expects; it was
the write that failed. The `displayException e` tail does carry the true cause,
but the parenthetical is stated as the diagnosis, and it accuses a file that is
fine.

The haddock at `src/Engine/Core/Init.hs:107-110` presents the same taxonomy as
exhaustive:

```haskell
--   look at legacy again). A legacy file that fails this check
--   (malformed, partial, schema-incomplete, or unreadable) is left
--   untouched and logged rather than copied, so it falls back to the
--   versioned default/registry exactly like a missing legacy file.
```

A copy failure is not "a legacy file that fails this check" — the check passed
and the copy was then attempted.

Two nearby statements ARE correct and a fix should preserve them:

- **"left untouched"** holds in every case. `copyFile` never writes the source,
  so the legacy file survives a decode failure and a copy failure alike.
- **"falls back to the versioned default/registry exactly like a missing legacy
  file"** holds on a copy failure too: no local file results, so
  `resolveConfigPath` (`:77-80`) returns the `_default.yaml` template exactly as
  it would with no legacy file at all. The migration also stays retryable,
  because the existence gate at `:114` still sees no local file next boot —
  which is the property the surrounding haddock spends a paragraph protecting.

**Severity: low-medium**, above the pure-comment findings in this report. The
misattribution reaches a warning a player or an agent actually reads, and would
send them to inspect valid YAML when the real problem is a non-writable
`config/` directory. No behaviour is wrong: the fallback is correct either way,
and nothing is corrupted (`System.Directory.copyFile` replaces the destination
atomically, so a failed copy leaves no partial local file to poison the gate).

Verified truthful in the same function, and noted so a fix does not disturb it:
the Proxy-based validation claim holds for all three call sites. `Proxy
KeyBindingConfig` (`:172`) matches `loadKeyBindings`'s decode
(`src/Engine/Input/Bindings.hs:110-111`); `Proxy VideoConfigFile` (`:179`)
matches `loadVideoConfig`'s (`src/Engine/Graphics/Config.hs:240-246`); `Proxy
OverridesFile` (`:262`) matches `loadOverrides`'s
(`src/Engine/Asset/YamlNotifications.hs:188`). The cited example is right too —
`resolution` really is the one required field among the video file's optionals
(`src/Engine/Graphics/Config.hs:192`).

---

## Unit animation atlases

### [#2211] EXPL-15. `Unit.Atlas.Digest`'s description of the digest stream omits the tag's length prefix and the label field entirely

`src/Unit/Atlas/Digest.hs:2-10`:

```haskell
-- | The two digests @tools\/pack_atlas.py@ records in a compiled unit
--   index (#1259, TEX-3), reproduced exactly so the runtime can verify
--   them.
--
--   Both are @sha256@ over a canonical, length-prefixed stream of
--   labelled fields (the compiler's @digest_stream@): the domain tag,
--   then each field as @\<u64 LE length\>\<bytes\>@. The length prefixes
--   are what make the stream injective — a bare concatenation would let
--   a character move across a field boundary without changing the hash.
```

The stream the code actually hashes (`src/Unit/Atlas/Digest.hs:120-131`) is:

```haskell
digestStream tag fields =
    hex ∘ SHA256.finalize ∘ SHA256.updates SHA256.init $
        [lengthPrefix (BS.length tag), tag] <> concatMap field fields
  where
    field (label, value) =
        [ lengthPrefix (BS.length label), label
        , lengthPrefix (BS.length value), value ]
```

That is:

```
<u64 LE len(tag)> tag
  then, per field:  <u64 LE len(label)> label  <u64 LE len(value)> value
```

The description departs from it in two places:

- **The domain tag is itself length-prefixed.** The sentence names it bare and
  ahead of the "then each field as `<u64 LE length><bytes>`" clause, so the
  prefixing reads as starting at the fields.
- **A field contributes TWO length-prefixed units, not one.** The label is
  emitted, length-prefixed, immediately before its value. That is what makes
  them "labelled fields" in the first place, and it is what the very next
  sentence's injectivity argument rests on — yet the label appears nowhere in
  the stated wire format.

This is worth more than a usual wording nit because reproduction is the
module's stated job. Line 3 says the digests are "reproduced exactly so the
runtime can verify them", and a mismatch is not a soft failure: a wrong digest
REJECTS a unit's whole definition at load
(`Unit.Atlas.Load.loadUnitAtlasIndex`). Someone reconstructing the format from
this paragraph — porting the check to another tool, or re-implementing the
compiler side — would emit an unprefixed tag and drop the labels, and get a
different SHA-256 for every input, with no hint from the prose that they had
missed two of the four components.

The Python side states it correctly, which is where the drift is visible.
`tools/pack_atlas.py:1584-1589`:

> A canonical, length-prefixed digest over labelled fields.
>
> Every field carries **its label** and an explicit byte length, so no
> concatenation of two different field sequences can produce the same stream

**Severity: low** in the sense that no behaviour is wrong — the two
implementations agree, which I verified field by field (see below). It is
recorded because this paragraph is the only specification of a cross-language
wire format, and it is not usable as one.

Verified truthful in the same module, so a fix does not disturb any of it:

- `sourceDigest` (`:93-116`) matches `pack_atlas.py`'s `source_digest`
  (`:1613-1633`) label for label and in order: the eight header fields
  (`unit`, `animation`, `flip`, `loop`, `fps`, `cell`, `columns`,
  `direction_count`), then per direction `direction`/`row`/`frame_count`, then
  per frame `frame_path`/`frame_size`/`frame_pixels`.
- `atlasContentDigest` (`:53-58`) matches `content_digest`
  (`tools/pack_atlas.py:1647-1651`): `width`, `height`, `pixels`.
- `lengthPrefix` (`:129-131`) really is u64 little-endian, matching Python's
  `struct.pack("<Q", ...)`.
- `pythonFloatRepr`'s thresholds are right. CPython's `format_float_short` in
  mode `'r'` switches to exponential on exactly `decpt <= -4 || decpt > 16`,
  and Haskell's own `show` does NOT share that pair (it switches at `0.1` and
  `10^7`), which is the stated reason the function exists. The trailing `.0`
  and the signed, two-digit-minimum exponent are both correct.

One narrow divergence in `pythonFloatRepr` was found and deliberately NOT filed
as a finding, recorded here so it is not rediscovered: `render`'s
`d ≡ 0 = "0.0"` guard fires for negative zero (IEEE `-0.0 == 0.0`) while the
`x < 0` test above it is False for `-0.0`, so the function yields `"0.0"` where
CPython's `repr(-0.0)` is `'-0.0'`. It is not a defect in practice on two
counts: the haddock scopes itself in the same sentence ("for the narrowed value
the compiler records as `fps`"), and `-0.0` can never BE an fps —
`fits_runtime_float` (`tools/pack_atlas.py:689-703`) requires
`narrowed != 0.0`, which is False for `-0.0`, so such a declaration is rejected
before compilation. Negative non-zero rates are accepted there and are
formatted correctly.

### [#2213] EXPL-16. `pickFrame`'s "used by the render path and the hit-tester" omits both Lua API consumers

`src/Unit/Render.hs:46-64`:

```haskell
-- | Choose a frame for a unit. If the unit has an active animation and
--   the requested frames exist, pick by elapsed time; otherwise fall back
--   to the T-pose. Used by the render path and the hit-tester.
--
--   [...]
--
--   THE FRAME-INDEX ARITHMETIC BELOW IS FROZEN (#1259, D-3). [...]
```

`pickFrame` has FOUR call sites in `src/`, not two:

| Site | Consumer |
|---|---|
| `src/Unit/Render.hs:210` | `renderUnitQuads` — the render path |
| `src/Unit/HitTest.hs:248` | `unitHitRect` — the hit-tester |
| `src/Engine/Scripting/Lua/API/Units/List.hs:333` | `unit.getFrameTexture` |
| `src/Engine/Scripting/Lua/API/Units/List.hs:375` | `unit.getFrameSample` |

(The `pickFrame` in `src/UI/Tooltip/Render.hs:273` is an unrelated function of
type `Float → TooltipSprite → TextureHandle`, not a consumer of this one.)

The two-item list reads as complete and omits an entire consumer CLASS — the
Lua query API, where this function's output crosses out of Haskell into script
code.

That omission is the opposite of incidental for this particular function. Two
paragraphs below the same haddock declares:

> THE FRAME-INDEX ARITHMETIC BELOW IS FROZEN (#1259, D-3).

so the caller list is exactly what a reader consults to find everything a
change to that arithmetic would reach. A frozen-arithmetic notice and a partial
caller list are a bad pairing: the notice tells you the blast radius matters and
the list understates it by half.

The omitted pair is also where the atlas migration bit hardest, which is why
they are worth naming rather than glossing. `List.hs:327-332` carries its own
warning beside the first of them:

```haskell
                            -- Handle only. Enough for a WHOLE-IMAGE
                            -- sample (a T-pose's direct sprite), and
                            -- never enough for an animation frame,
                            -- which since #1261 is always an atlas cell
                            -- and needs the sub-rect too — such a
                            -- caller must use `unit.getFrameSample`.
```

Both consumers read `pickFrame`'s `FrameSample`, and neither is discoverable
from `pickFrame` itself.

**Severity: low.** No behaviour is wrong, and each omitted consumer is
correctly documented on its own side. Recorded because it is a completeness
claim about the callers of a function whose arithmetic is explicitly declared
frozen — precisely the case where a caller list needs to be exhaustive.

Verified truthful in the same function, so a fix does not disturb any of it:
`fsFlipX` really is `True` only on the `mirrorDir` branch (`:129-135`); the
present-but-empty direction really does fall to the T-pose via the `n ≤ 0`
guard while an ABSENT one falls to its mirror, exactly as
`Unit.Atlas.Types.storageFrameCount`'s haddock claims; the stride comment is
right (`strided = raw * stride` shows frames 0, 2, 4, … at stride 2, halving a
9-frame transition); and the reverse path really does hold frame 0 when a
non-looping clip runs out, since `fwdIdx` clamps to `n - 1` and
`idx = (n - 1) - fwdIdx`.

---

## Quad sorting: a removed mechanism still cited as live

`spriteRowSpan` does not exist anywhere in this tree. A grep across `src`,
`app`, `scripts`, `test-headless`, `cbits`, `tools` and `shaders` returns
exactly three hits, all inside comments and none in code:

```
src/Unit/Render.hs:273:            -- spriteRowSpan forward-push would otherwise draw the climber
src/Building/Render.hs:205:            -- top. Adding spriteRowSpan (the sprite's vertical extent)
src/Building/Render.hs:207:            -- a 96×96 cargo hold has spriteRowSpan ≈ 2.0 — outrank
```

`src/Unit/Render.hs:254-262` records why it went, and is the one CURRENT
statement of the rule:

```haskell
            -- It deliberately does NOT add a "sprite row span" forward
            -- push. A tall sprite spans more than one screen row, so a
            -- push sized to its height (~1.33 rows for a 1:1 sprite, more
            -- for taller units) exceeded a full row and let an elevated /
            -- climbing unit out-sort — and draw OVER — a cliff a full row
            -- in FRONT of it. The screen row (faF+fbF) already orders the
            -- unit correctly against tiles ahead of and behind it; [...]
```

The two findings below are the comments left behind, recorded separately
because they are in different files, make different claims, and would be fixed
by different edits.

### [#2214] EXPL-17. `unitToQuad`'s climb-occlusion branch is justified by a `spriteRowSpan` push the same function says it does not apply

`src/Unit/Render.hs:269-278`, nineteen lines below the passage above:

```haskell
            -- Far-side climb occlusion: while climbing onto a cliff
            -- column whose face is between the unit and the camera (its
            -- screen-row is in FRONT of the unit's frozen base), sort the
            -- unit just BEHIND that column so the cliff hides it. The
            -- spriteRowSpan forward-push would otherwise draw the climber
            -- OVER the column it's climbing. [...]
```

That sentence is the stated REASON the branch exists, and it names a push the
same function has already said it deliberately does not apply. The two comments
describe mutually exclusive versions of `normalSort`; only one of them can
describe the code as it stands, and `:254` is the one that does.

**This finding does NOT claim the branch is dead**, and a fix must not assume
so. The branch still changes the outcome: `sortKey = destRow - 0.5` moves the
climber ahead of every intermediate screen row while keeping it behind the
destination column, which `normalSort = baseRow + 0.0006` would not do. The
defect is that the justification given cannot be the operative one, so a reader
deciding whether the branch is still needed — the exact question this comment
exists to answer — reasons from a mechanism that was removed.

The rest of the branch's comment is accurate: "Only applies while the unit is
still on the base side (its tile ≠ the dest column)" matches the
`baseTile ≢ dest` guard, and the fallback to `normalSort` once the pullup
carries the unit onto the top tile is real.

### [#2215] EXPL-18. `Building.Render`'s sort comment says units add a `spriteRowSpan` term "as units do"; they no longer do

`src/Building/Render.hs:203-212`:

```haskell
            -- Sort by the iso depth of the GROUND TILE, not the sprite
            -- top. Adding spriteRowSpan (the sprite's vertical extent)
            -- to the sort key as units do made tall buildings — e.g.
            -- a 96×96 cargo hold has spriteRowSpan ≈ 2.0 — outrank
            -- units at the same tile, drawing the building on top of
            -- a unit standing in front of it. Keeping just the iso
            -- bottom plus the +0.0005 tiebreaker means a unit at the
            -- same row sorts in front (their key has +0.0006), and
            -- units north of the building still get obscured because
            -- their key is lower (north = smaller faF + fbF).
```

**"as units do" is false.** Units did once; `src/Unit/Render.hs:254` states
they deliberately no longer do. This is the more misleading of the pair,
because it presents a deliberate DIFFERENCE between building and unit sorting
on the one point where the two now agree: both key off the iso bottom of the
ground tile with no sprite-height term, differing only in their constant
tiebreaker.

The remaining arithmetic in the same block is correct, and I checked it:
`unitSortNudge = 0.0003` (`src/Unit/Render.hs:39`) and `normalSort` adds
`2 * unitSortNudge` (`:264-267`), so a unit's key really does carry `+0.0006`
against the building's `+0.0005`, and a unit at the same row really does sort
in front.

**Severity for both: low** — no behaviour is wrong, and the sort keys are
correct as written. Recorded because these are load-bearing rendering-order
comments, both are the first thing a reader consults before touching a sort
key, and both describe a mechanism that no longer exists.

---

## Unit hit testing

### [#2216] EXPL-19. `Unit.HitTest` claims to mirror a tile hit-test that no longer holds the math, and to use the "same math" the engine documents as different

Two claims, one in the module haddock and one inside `hitTestUnitAt`.

`src/Unit/HitTest.hs:4-7`:

```haskell
-- Given mouse coordinates in framebuffer pixels, find which (if any)
-- spawned unit is under the cursor. Mirrors the screen→world projection
-- in `World/Render/CursorQuads.hs::renderWorldCursorQuads::hitTest` and
-- the per-unit sprite math in `Unit/Render.hs::unitToQuad`.
```

`src/Unit/HitTest.hs:81-83`:

```haskell
                -- Screen pixel → world coord. Same math as the tile
                -- hit-test in `renderWorldCursorQuads::hitTest`:
                --   normX/Y in [0..1]
```

**(a) The named target no longer contains the cited math.**
`renderWorldCursorQuads::hitTest` is now a delegation
(`src/World/Render/CursorQuads.hs:74-76`):

```haskell
    let hitTest pixX pixY =
            pickWorldTile facing zoom zSlice camX camY fbW fbH winW winH
                          worldSize effectiveDepth vb tileData pixX pixY
```

The projection moved into `World.Render.HitTest.pickWorldTile`. A reader
following either pointer lands on a call site, not on a projection to compare
against.

**(b) "Same math" is false, and the engine's own documentation classifies it as
false.** The two derive the aspect ratio from DIFFERENT sources:

| | aspect | pixel→norm | degeneracy guard |
|---|---|---|---|
| `pickWorldTile` (`src/World/Render/HitTest.hs:101-104`) | `fbW / fbH` — **framebuffer** | `winW` / `winH` | `viewportDegenerate winW winH fbW fbH` |
| `hitTestUnitAt` (`src/Unit/HitTest.hs:87-91`) | `winW / winH` — **window** | `winW` / `winH` | `windowDegenerate winW winH` |

`src/Engine/Graphics/Viewport.hs:26-43` names these as two families and states
the split outright — `windowDegenerate` is

> Used by the hit-test paths, which derive their aspect ratio from the window
> size.

while `viewportDegenerate` is for

> the paths that normalize by the window size AND derive their aspect ratio
> from the framebuffer (the world tile pick and the zoom-map chunk pick).

So the difference is intentional, documented, and carries a distinct guard on
each side — which is exactly what makes the flat "Same math as the tile
hit-test" claim wrong by the codebase's own taxonomy.

**No behaviour is wrong today.** Under uniform DPI scaling
`fbW / fbH == winW / winH`, so both projections produce identical world
coordinates; that is presumably why the divergence has been tolerable. This
finding is about the claim, not about the projection.

**The "cannot drift" guarantee does not extend here**, which is the part most
likely to mislead. `src/World/Render/CursorQuads.hs:65-67` says of
`pickWorldTile`:

> Shared with the synchronous Lua pick (@world.pickTile@) so the two can't
> drift — see 'World.Render.HitTest'.

Unit hit-testing is a THIRD consumer of the same projection that keeps its own
copy rather than calling the shared function, so it sits outside that
guarantee — the opposite of what "Mirrors the screen→world projection in …"
leads a reader to assume. A maintainer changing `pickWorldTile` would read
`Unit.HitTest` as a mirror of it and reasonably conclude nothing needed
updating.

`hitTestUnitsInRect` carries the same copy at `src/Unit/HitTest.hs:156`; its own
comment there ("Mirrors the math in hitTestUnitAt", `:159-160`) is accurate and
should be left alone.

**Severity: low-medium** — above the pure-nit tier because the misdirection is
about which code two implementations must be kept in step with, and no shipping
configuration currently exposes it.

Verified truthful in the same module, so a fix does not disturb any of it:

- `unitHitRect`'s claim that "the ONE deliberate difference from the renderer is
  the height offset, which uses the INTEGER `uiGridZ` here against the
  renderer's continuous `uiRealZ`" survives a line-by-line diff against
  `unitToQuad`: every other term — `applyFacingF`, `rawX`, `rawY`,
  `baseRadius`, `frameDimensions`, both scales, both quad dimensions, `drawX`
  and `drawY` — is identical, including the correct ABSENCE of a
  `tileHalfDiamondHeight` term.
- `frameSampleOf` really does delegate to `Unit.Render.pickFrame` and really
  does keep the directional T-pose fallback for a unit whose def is missing,
  matching `unitToQuad`.
- The `effDepth` expression matches the renderer's. (It is the same expression
  in ten places across nine files — duplication, but with no drift, and not a
  comment defect.)

### [#2218] EXPL-20. `resolveTexture`'s haddock credits itself with animation mirroring; animations never reach it and it cannot honour `flip: false`

`src/Unit/Sprite.hs:49-56`:

```haskell
-- | Pick the correct directional sprite for a unit given its world-space
--   facing and the current camera rotation.
--
--   Lookup order: requested screen direction → its `mirrorDir` (returned
--   with `flipX = True` so the renderer flips UVs) → fallback default
--   (no flip). The mirror step lets animations ship 5 directional
--   sprites (S/SE/E/NE/N) instead of 8 — SW/W/NW are produced by
--   horizontal mirror at draw time.
```

The lookup order is exactly right. The last sentence is wrong about this
function on two counts, and the module's OWN header contradicts it twelve lines
earlier (`src/Unit/Sprite.hs:12-16`):

```haskell
--   Since #1259 this is reached only when there is no animation frame
--   to show: an ANIMATED unit is both drawn and hit-tested from
--   'Unit.Render.pickFrame''s sample, which is the same shared-resolution
--   principle applied one level up (a frame's size is its atlas CELL,
--   which no texture-handle lookup can report).
```

**(1) Animations never reach `resolveTexture`.** Its
`Map.Map Direction TextureHandle` argument is `uiDirSprites`, which traces back
through `udDirSprites` (`src/Unit/Thread/Command/Spawn.hs:165`,
`src/World/Save/Types.hs:691`) to the unit YAML's `directional_sprites` key
(`src/Engine/Asset/YamlUnits.hs:417`) — the T-POSE sprite set, which
`docs/engine_contracts.md` and the asset inventory both classify as a
non-animation unit texture. All three call sites
(`src/Unit/Render.hs:119`, `src/Unit/Render.hs:213`,
`src/Unit/HitTest.hs:251`) are the T-pose fallback. Since #1261 an ANIMATION's
five-versus-eight economy is implemented in `Unit.Render.pickFrame`'s
`lookupFlip`, against the authored rows the atlas index records.

**(2) The two mirroring paths differ in a way the sentence conceals.**
`resolveTexture` mirrors UNCONDITIONALLY: when the direct direction is absent it
always tries `mirrorDir`, and there is no opt-out anywhere in the function. The
animation path gates on the per-animation `aFlip` flag, and
`src/Unit/Render.hs:117-122` states why the gate is load-bearing:

> `flipOK` from the animation's `aFlip` flag gates the mirror fallback. When
> False we deliberately do NOT mirror — an anim with an asymmetric held prop
> (weapon in right hand) would otherwise have the prop visually swap sides on
> western directions. Author sets `flip: false` (or omits) to opt out.

So a reader taking `resolveTexture`'s haddock at face value concludes that this
is the mechanism behind animation mirroring, and therefore that it honours
`flip: false`. Neither holds. The asymmetric-weapon case is one the asset
pipeline explicitly supports — the approved `<lowercase>_RH_<lowercase>`
animation-identifier form exists for exactly it — so the distinction is not
hypothetical.

Worth distinguishing, so a fix does not over-correct: `src/Unit/Direction.hs:40-45`
uses the same "lets bilaterally-symmetric animations ship 5 directional sprites
instead of 8" framing for `mirrorDir` itself, and there it is CORRECT —
`mirrorDir` genuinely serves both paths, `pickFrame`'s and `resolveTexture`'s.
The defect is confined to `resolveTexture`, whose domain is the T-pose only.

**Severity: low.** No behaviour is wrong. Recorded because it is a same-file
contradiction in which the function-level comment survived the correction its
own module header applies, and because it hides the
unconditional-versus-`aFlip` difference between the two mirroring paths.

Verified truthful in the same module, so a fix does not disturb it:
`cameraRotSteps`' "each 90 deg CW rotation = 2 steps" is right (S 180° → W 270°
→ N 0° → E 90° is clockwise, and eight directions over four quarter-turns is
two steps each); `screenDirOf` subtracts the camera's steps in the correct
sense and its `mod` is non-negative; and `resolveTexture`'s stated lookup order
matches the code exactly, `flipX` included.

---

## Unicode operator convention

### [#1494] EXPL-21. 98 production call sites spell inequality `≠` instead of `≢`; neither the operator audit nor CLAUDE.md's table can see it

**The rule, from the project owner (2026-08-20):** in actual Haskell code
inequality is always `≢`. `≠` is acceptable only in PROSE — a comment writing
pseudocode or a maths formula. It must not appear as an operator.

The tree does not currently obey that rule, and nothing in the gate set can
tell.

**Measurement.** Counting occurrences of `≠` in `.hs` files, separating code
positions from comment positions:

| Scope | Occurrences | Files |
|---|---|---|
| `src/` + `app/` — code | **98** | 43 |
| `test-headless/` — code | 12 | 8 |
| anywhere — inside comments | 8 | 8 |

For comparison, the correct spelling `≢` appears 175 times in `src/` + `app/`.
**Ten files use both spellings**, including `src/Unit/Render.hs`,
`src/Engine/Core/State.hs`, `src/World/Render/Quads.hs` and
`src/Engine/Input/Thread/Keyboard.hs` — so the divergence is not a
module-by-module habit that could be explained as one author's style; it is
mixed within single files.

Representative sites (`src/World/Mine/Types.hs:151,159-160`, a file with no
`≢` at all):

```haskell
    in if mask ≠ 15
...
        in maskOf (dugNW ∧ imax ≠ 0) (dugNE ∧ imax ≠ 1)
                  (dugSE ∧ imax ≠ 2) (dugSW ∧ imax ≠ 3)
```

and `src/World/Render/HitTest.hs:146`, where this was first noticed:

```haskell
                   else if ctMats col VU.! i ≠ 0
```

**Both spellings are `/=`, and neither is locally defined.** Nothing in `src/`
or `app/` defines `(≠)`. It arrives through `import Prelude.Unicode`
(`src/UPrelude.hs:27`, re-exported at `:8`) from the `base-unicode-symbols`
dependency, whose `Data.Eq.Unicode` exports `≡`, `≢` AND `≠`, the latter two
both being `/=`. So the 98 sites are correct code with the wrong spelling — this
is a convention violation, not a bug, and every fix is a pure textual
substitution with identical fixity.

**Why the enforcement gate cannot catch it.**
`tools/unicode_operator_audit.py` is built to find ASCII operators that should
have been converted. Its lexer is:

```python
_SYMBOL_RUN = re.compile(r"[!#$%&*+./<=>?@\\^|~:-]+")
```

an ASCII-only character class, and its forbidden set is
`TOKEN_REPLACEMENTS = {".&.", ".|.", ">>=", "==", "/="}` (`:32-40`). `≠` is
already non-ASCII, so it is never even lexed as a candidate. The audit is
structurally incapable of seeing it: it hunts the un-converted ASCII spelling,
and this is a converted-but-wrong Unicode one. A file could be 100% `≠` and the
audit would report a clean tree.

Its own docstring maps the intended conversion one way only (`:12`):

```
  /=  -> ≢   inequality,   infix 4
```

and cites `base-unicode-symbols Data/Eq/Unicode.hs` at `:5` — the very module
that also exports `≠`.

**Why the documentation does not say so either.** CLAUDE.md's "Unicode
operators defined in UPrelude" table lists one inequality:

| `≢` | inequality (from Prelude.Unicode) | `/=` |

with no row for `≠` and no statement that `≠` is disallowed. That silence is
notable because the same file explicitly handles the one OTHER two-spelling
situation it has: "`fmap`'s two spellings, `<$>` and `⊚`, are a deliberate
exception: **both are kept**, picked per call site by readability, not enforced
either way." A reader who has internalised that sentence and then meets `≠` in
98 places has every reason to infer a second such exception, because nothing
tells them otherwise.

**The eight prose occurrences are correct and must be left alone.** They are
exactly the exemption the owner's rule allows — pseudocode and maths inside
comments:

```
src/Unit/Render.hs:275          -- is still on the base side (its tile ≠ the dest column); once
src/Combat/Resolution.hs:142    -- "uiPose ≠ dead" lets stale CombatAttacks land on
src/Language/Suggest.hs:210     --   the previous one — so @headIndexAt base n k ≠ headIndexAt base n
src/Engine/Core/State.hs:516    --   per-image semaphore (image count ≠ frames in flight).
src/World/Edit/Apply.hs:292     --   (≠ 0) cell, or -1 if the column is entirely air. Top-level (not a
src/World/Generate/Timeline.hs:244  -- (so some tile's @ru0@ ≠ the chunk-centre's), the per-
src/Combat/Wounds/Tick.hs:427   -- UnconsciousNow gating, which already checks uiPose ≠
test-headless/Test/Headless/WorldGen/Exposure.hs:17  --   > ctMats c ! (z - ctStartZ) ≠ matAir    -- ∀ z ∈ [min ns .. tz]
```

Any mechanical fix must therefore be comment-aware, which the existing audit's
`_strip_haskell_comments` machinery already is — it was written for precisely
this hazard, and its docstring says so: "extended with string-literal and GLSL
quasiquote awareness since a false hit here would rewrite content this guard
must never touch."

**Severity: the highest in this report so far, and of a different kind.** Every
other EXPL finding is prose that misdescribes correct code; this is code that
violates a stated convention, at 98 sites, with a guard that reports clean and
a documentation table that implies the situation is fine. It is also the most
tractable: the substitution is mechanical, the exemption set is eight known
lines, and the audit already has the comment-aware scanner needed to enforce it
afterwards — extending `TOKEN_REPLACEMENTS`-style checking to a Unicode
forbidden token, plus a CLAUDE.md row stating the rule, would close it
permanently.

Scope note: the 12 `test-headless/` occurrences are outside
`unicode_operator_audit.py`'s current `src/` + `app/` scope. Whether they are in
scope for the convention is the owner's call; they are recorded here so the
count is complete either way.

---

## EngineEnv capability split

### [#2219] EXPL-22. The capability inventory says four of the five capability splits are §3.1 thread-privacy splits; two are, and the doc contradicts itself

`docs/engineenv_capability_inventory.md:109-113`:

```markdown
**Eight identifiers, thirteen record/view types.** The record set is
finer-grained than the identifier set, because five capabilities are
deliberately split — four of them by §3.1's pointer-record visibility
rule (a thread-private field forces a strictly narrower worker-safe
view, never a documented restriction on a wider record), one by
consumer coupling:
```

Two of the three counts are right. "Eight identifiers, thirteen
record/view types" checks out — `src/Engine/Core/Capability/` holds thirteen
modules, each exporting exactly one `<Name>Capability` record and one total
`to<Name>Capability` projection, and §2.1's own identifier table sums to
thirteen. "Five capabilities are deliberately split" is right too.

**"Four of them by §3.1's pointer-record visibility rule" is wrong. It is
two.**

§3.1's rule produces a specific shape: one main-only/owner-only record PLUS a
strictly narrower worker-safe view that omits the thread-private field. Exactly
two capabilities have it:

| Identifier | Record / view pair | Thread-private field | Enforced at |
|---|---|---|---|
| `render-gpu-asset` | `RenderCapability` / `RenderViewCapability` | `engineStateRef` | `tools/engine_env_capability_audit.py:693-706` (#891) |
| `input-lua-transport` | `InputCapability` / `InputViewCapability` | barrier-token allocator, current-key handoff | `tools/engine_env_capability_audit.py:839-853` (#892) |

The remaining three two-record identifiers are PEER-DOMAIN splits, and each one
says so in its own module header:

- **`ui-hud-events`** — both halves carry the literal heading
  `== No thread-private field, so no split record`
  (`src/Engine/Core/Capability/Ui.hs:20`,
  `src/Engine/Core/Capability/Events.hs:19`) and state:

  > Unlike `@render-gpu-asset@` (§3.1) and `@input-lua-transport@` (§7.3),
  > this capability owns nothing one thread privately owns […] So there is
  > **one record here, not a main-only/worker-safe pair**, and
  > `@tools/engine_env_capability_audit.py@` needs no import boundary for it
  > beyond the §6 ratchet.

- **`units-buildings-combat`** — split on domain, not thread
  (`src/Engine/Core/Capability/Building.hs:9-19`):

  > __"Building" is a domain, not a thread__ (§2.2 […]). There is no building
  > thread: the command queue below is drained on `@UnitThread@` […]. That is
  > precisely why the record is separate from
  > `UnitCombatCapability` rather than folded into it […]

- **`world-sim-render-handoff`** — the document itself excludes this one at
  `:127`: "The `world-sim-render-handoff` split is the one that is not a §3.1
  thread-privacy split".

**The document contradicts itself sixty-three lines later.** Its own canonical
convention block, at `:170-174`, lists `ui-hud-events` among the capabilities
that do NOT meet §3.1's trigger:

> A field being read or written by only one thread today is not by itself the
> trigger — `save-load-coordination`'s `slLastSaveTimeRef` and two
> `ui-hud-events` fields are single-role with no privileged pointer behind
> them, so documenting the restriction on the field alone is sufficient there.

So `ui-hud-events` is counted inside the "four" at `:111` and excluded from the
rule at `:172`.

**The arithmetic only closes on the error.** As written, 4 (§3.1) + 1 (consumer
coupling) = 5. Correctly attributed it is 2 (§3.1) + 1 (consumer coupling) + 2
(domain separation) = 5 — and the sentence offers no category for those last
two, so `units-buildings-combat` and `ui-hud-events` are not merely unexplained,
they are actively miscounted into a rule that excludes them.

**Nothing catches it.** `tools/engine_env_capability_audit.py` enforces the
eight `CAPABILITIES` identifiers (`:99-103`), the §3 main-render ownership
boundary, and the §7.3 `LuaThread` ownership boundary. It does not parse §2.1's
split taxonomy, so this count is unguarded in both directions. The audit's own
comments corroborate the correct number: it describes exactly two capabilities
being split for thread privacy — "#891 therefore splits `render-gpu-asset` into
two interfaces" (`:702`) and "#892 therefore splits the capability into two
interfaces" (`:850`) — and names no third or fourth.

**Severity: low-medium.** No code is wrong and no boundary is unenforced. Above
the nit tier because `CLAUDE.md` still directs every agent to this exact block —
"Before adding a capability record, read §2.1's canonical convention block
rather than inferring the shape from an existing one" — so it is the first thing
read before adding a ninth capability. From this sentence a reader takes that
thread-privacy is the dominant reason to split (four of five, when it is two of
five) and that `ui-hud-events` is a worked example of the rule, when both of its
modules explicitly disclaim it.

Scope note: an earlier revision of `CLAUDE.md` restated this count directly
("five capabilities are split, four by the thread-private rule (§3.1)"). The
CLAUDE.md trim landed on master as `78ca07ec` / `b84c2dd7` removed that
sentence, so no `CLAUDE.md` edit is required — the defect now lives only in the
authoritative document.

---

## UI input ownership

### [#2223] EXPL-23. `inputBoundaryPage` explains the modal-boundary tie-break by `PageHandle` and show-recency; the sort key is `(upLayer, upZIndex)`

`src/UI/InputOwnership.hs:125-134`:

```haskell
-- | The topmost visible input-exclusive page — the modal boundary
--   pointer input cannot cross. 'getVisiblePages' paints bottom to
--   top, so the boundary is the LAST exclusive page in that order
--   (when two modals are visible, the more recently shown one — the
--   higher 'PageHandle' — paints on top and owns the boundary).
inputBoundaryPage ∷ UIPageManager → Maybe UIPage
inputBoundaryPage mgr = case filter upInputExclusive (getVisiblePages mgr) of
    [] → Nothing
    xs → Just (last xs)
```

The first sentence is correct: `getVisiblePages` does return bottom-to-top, and
taking `last` of the exclusive pages does select the topmost one. The
parenthetical that explains WHICH page that is, when two are visible, is wrong
in three separate ways.

**(1) The sort key contains no `PageHandle`.**
`src/UI/Manager/Page.hs:128-132`:

```haskell
getVisiblePages ∷ UIPageManager → [UIPage]
getVisiblePages mgr =
    let visibleList = mapMaybe (`Map.lookup` upmPages mgr)
                              (Set.toList $ upmVisiblePages mgr)
    in sortOn (\p → (upLayer p, upZIndex p)) visibleList
```

**(2) `upZIndex` — the actual within-layer tie-break — is never mentioned.**
Two exclusive pages on the same layer are ordered by their zIndex, and the
comment offers a handle rule in its place.

**(3) "The more recently shown one" is not "the higher `PageHandle`".** A
handle is allocated at CREATION (`src/UI/Manager/Page.hs:22-43`):

```haskell
createPage name layer mgr =
    let handle = PageHandle (upmNextPageId mgr)
```

with `upmNextPageId` incremented on the same construction. `showPage`
(`:67-75`) sets `upVisible` and inserts into `upmVisiblePages`; it records
nothing about ordering, and touches neither the handle nor `upZIndex`. So show
order is not stored anywhere in the manager.

A concrete falsification, on the ordinary lifecycle for modal pages — created
once when the HUD is built, then shown and hidden repeatedly:

> Page A is created first (handle 3), page B later (handle 7). Both are
> `LayerModal`, so both default `upInputExclusive = True`. Show B, then show A.
> Both are visible. `Set.toList` yields `[3, 7]`; the sort is stable on
> `(LayerModal, 0)`; `last` selects **B** — created later, shown EARLIER. The
> comment predicts A.

**Why the stated conclusion nevertheless holds today**, which is the part most
worth recording, because it is what makes this invisible: `upZIndex` has NO
SETTER anywhere in the tree. A grep finds the initializer
(`src/UI/Manager/Page.hs:29`, `upZIndex = 0`) and four readers
(`src/UI/Render.hs:135`, `src/UI/Manager/Query.hs:193` and `:321`,
`src/UI/Manager/Page.hs:132`) — nothing writes it. Every page therefore carries
zIndex 0, `sortOn` is stable, and its input is already in ascending-handle order
because `upmVisiblePages` is a `Set PageHandle`. So ties really do fall to the
higher handle — by sort stability over a set's ordering, a property the comment
does not state, resting on a field it never names being permanently zero.

**Severity: low-medium.** No behaviour is wrong today. Above the nit tier
because this comment exists to explain WHICH page owns the modal boundary — a
routing decision that determines whether a click reaches the world — and it
supplies two rules that are both false (handle ordering, show recency) while
omitting the one the code uses. Should `upZIndex` ever gain a setter, the
comment becomes wrong in outcome as well as in reasoning, and nothing in the
gate set would catch it.

Verified truthful in the same module, so a fix does not disturb it:
`pagesInScope`'s `dropWhile` really does keep the boundary page itself in scope
along with everything above it; `isGameplayBlocked` really is
`isJust ∘ inputBoundaryPage`; and `createPage`'s "a modal-layer page is a real
input boundary by default; every other layer defaults pass-through"
(`src/UI/Manager/Page.hs:34-38`) matches `upInputExclusive = layer ≡ LayerModal`
exactly.

### [deferred] EXPL-24. `hitsAtPointBy`'s haddock cites `UI.InputOwnership.pagesInScope`, which is not exported

> **Deferred:** EXPL-27's tree-wide dead-link sweep already lists both of this finding's sites (`UI/Manager/Query.hs`, `UI/FocusNavigation.hs`), so a standalone fix would collide with whatever that finding files — cleared once EXPL-27 carries a `[#N]`, `[no-issue]` or `[deferred]` marker, at which point this links to its issue (adding the `scopedPageOk` wording point and the third site, `UI/Types.hs:481`) or files alone if excluded there.

`src/UI/Manager/Query.hs:182-186`:

```haskell
--   @pageOk@ is a plain filter here, not a modal-boundary decision —
--   callers that need the #742 modal-input-exclusive boundary (a miss
--   on the boundary page must not fall through to a lower one) go
--   through 'UI.InputOwnership.routePointer', which computes a scoped
--   @pageOk@ from 'UI.InputOwnership.pagesInScope' and passes it in
--   here/'topHitBy' unchanged.
```

`pagesInScope` is defined at `src/UI/InputOwnership.hs:139` but does not appear
in that module's export list (`src/UI/InputOwnership.hs:65-73`), which exports
only `PointerKind(..)`, `InputRoute(..)`, `isPageInScope`, `isGameplayBlocked`,
`routePointer`, `routeScroll` and `isPointerSurfaceBlocked`. The haddock link
therefore does not resolve and a reader cannot navigate to the named function.

The sentence is also slightly wrong about the mechanism it describes.
`routePointer` does not compute its `pageOk` from `pagesInScope` directly — it
passes `scopedPageOk mgr` (`src/UI/InputOwnership.hs:170-173`), a second
unexported helper that builds a membership `Set` once and closes over it, and
which calls `pagesInScope` internally. Naming the inner function skips the one
that actually exists at the call site.

`src/UI/FocusNavigation.hs:14` carries the identical dangling reference.

**Severity: low**, the same shape as EXPL-6 (`Engine.Core.Workers.allWorkers`),
and recorded separately for the same reason: it is a distinct file naming a
distinct symbol, and would be fixed by a distinct edit — either by exporting
`pagesInScope` or by pointing at something a reader can reach.

### [#2225] EXPL-25. `isPointerSurfaceBlocked` names `Engine.Input.Thread` as its caller; since #787 that is `Engine.Input.Thread.Mouse`

Two places in `src/UI/InputOwnership.hs` attribute the middle-click check to
the wrong module.

The module header, `:38-40`:

```haskell
--   'isPointerSurfaceBlocked' extends the boundary to middle-click
--   (camera drag), which has no owned handler and no page concept of
--   its own in 'Engine.Input.Thread' [...]
```

and the function's own haddock, `:239-241`:

```haskell
-- | #742: the middle-click "UI surface blocks" check
--   ('Engine.Input.Thread' — middle-click has no owned handler of its
--   own and exists purely to pan the camera). [...]
```

Since #787, `Engine.Input.Thread` is a thin lifecycle facade exporting only
`startInputThread`, `processInputs` and `processInput`
(`src/Engine/Input/Thread.hs:18-22`), and it does not import
`UI.InputOwnership` at all. The sole production caller of
`isPointerSurfaceBlocked` is `src/Engine/Input/Thread/Mouse.hs:172`, importing
it at `:32`.

This is weaker than EXPL-19's stale pointer: the named module IS the facade for
the subsystem that performs the check, and the work does happen on the input
thread, so the sentence reads as nearly right. That is precisely the category
worth recording under this report's calibration — a pointer that survives a
split because the parent name still means something.

The rest of the tree already uses the precise name where it matters
(`src/UI/ControlActivation.hs:11`, `src/UI/FocusNavigation.hs:122`,
`src/UI/Types.hs:171` all say `Engine.Input.Thread.Mouse` or
`.Keyboard`), so this is an outlier rather than a convention.

**Severity: low.** No behaviour is involved.

Verified truthful in the same module, so a fix does not disturb any of it:
`routePointer`'s documented ordering matches its code exactly, including the
`ueClickable` gate applied to BOTH callback fields via `activeCallback` and the
right-click fallback to a left-clickable control; `RouteConsumedNoHandler`'s
"consumed (focus clears)" is honoured by its caller
(`src/Engine/Input/Thread/Mouse.hs:339-345`); `routeScroll` really does apply
the same `scopedPageOk` restriction as `routePointer`; and
`isPointerSurfaceBlocked`'s deliberately UNSCOPED `topHitBy (const True)` with
the boundary folded in through `isGameplayBlocked` is exactly what its haddock
describes.

---

## Save publication

### [#2226] EXPL-26. `World.Save.Storage`'s header says it "receives only" four of six parameters, and its numbered transaction omits the requirement-9 refusal

`src/World/Save/Storage.hs:11-16`:

```haskell
--   This module never reads live gameplay state and never participates in
--   snapshot capture (issue #758's barrier already released by the time
--   'publishGeneration' runs — see "World.Thread.Command.Save.WriteWorld").
--   It receives only already-encoded bytes, the metadata that encode
--   produced, a slot directory, and a slot name for diagnostics, and
--   performs a classic write-validate-publish-rotate transaction:
```

followed by a numbered 1-7 list of the transaction's steps. Two linked
exhaustiveness claims, both incomplete in the same place.

**"Receives only" names four of six parameters.** The real signature
(`src/World/Save/Storage.hs:349-357`):

```haskell
publishGeneration
    ∷ FilePath        -- ^ slot directory
    → Text             -- ^ slot name (diagnostics only)
    → SaveMetadata      -- ^ metadata this candidate must decode back to
    → BS.ByteString      -- ^ complete, already-encoded envelope bytes
    → HS.HashSet Text    -- ^ every Lua component NAME this encode included
    → HS.HashSet Text    -- ^ the subset of those marked required
    → IO (Either PublishFailure [Text])
```

The two Lua registry sets appear nowhere in the prose, and the word "only"
makes the list an exhaustive claim rather than a summary.

**The numbered transaction omits two phases the code runs**, both of which this
module's own `StoragePhase` type enumerates — and whose haddock states it lists
them "in the order a real publication reaches them"
(`src/World/Save/Storage.hs:296-317`):

```haskell
data StoragePhase
    = PhaseUnsafePath
    | PhaseForeignOptionalData
    | PhaseDirectoryCreate
    | PhaseCandidateCreate
    ...
```

`publishGenerationWithCandidateCreator` (`:373-397`) runs, in order:
`rejectSymlinkedSlotDir` → **`foreignOptionalDataCheck`** (`:379-383`) →
**`createDirectoryIfMissing`** (`:387-390`) → `createCandidate` →
`writeValidateAndPublish`. The header's step 1 is the symlink check and its step
2 is "Write the candidate to a UNIQUELY named temporary file", so both middle
phases vanish.

**The two omissions are one omission.** `foreignOptionalDataCheck dir
luaKnownNames` is precisely what the missing fifth parameter feeds. Drop the
step from the list and the parameter has no reason to appear in the input
sentence either.

**Why this is more than a missing bullet.** `PhaseForeignOptionalData` is a
REFUSAL, not a stage of the write. Requirement 9 (#766) aborts the entire
publish before any candidate exists, specifically so a slot whose existing
generation carries an optional component this build does not recognise is never
overwritten — its constructor haddock (`:301-306`) and a thirty-line rationale
at `:411-440` (including why it must check `world.synworld.prev` as well as the
authoritative file) both spell this out. A reader consulting the numbered list
to answer "what can stop a save before anything is written?" is told: the
symlink check, and nothing else. Answering that question is what a numbered
transaction list is for.

**Severity: low-medium.** No behaviour is wrong, and the omitted step is
documented thoroughly at its own definition and on its `StoragePhase`
constructor — it is only the module-level specification that skips it. Above
the nit tier because this header is the specification for a 932-line
transactional module, it makes two explicit exhaustiveness claims, and both drop
the same first-class failure phase.

Verified truthful in the same header, so a fix does not disturb any of it:

- `rejectSymlinkedSlotDir` (`:267-272`) really does check the slot directory AND
  `takeDirectory dir`, and really is built from the single `rejectSymlinkedPath`
  primitive the generation-file decoder shares.
- The macOS durability trade-off is real: the module uses
  `System.Posix.Unistd.fileSynchronise` (`:169`), which is plain POSIX `fsync`,
  not `F_FULLFSYNC`.
- The crash-window section's per-topology guarantee matches
  `AuthoritativeTopology`'s three constructors (`:646`), including the
  staging/rotation skip in the two topologies where the previous generation is
  the live load source.

---

## Haddock references that cannot resolve

### EXPL-27. 59 cross-module haddock links point at functions their named module does not export, concentrated on module-split seams

Unlike every other finding in this report, this one is systematic rather than
situational: it was found by sweeping the whole tree rather than by reading one
function.

**Method.** Every haddock reference of the form `'Module.function'` in a comment
under `src/` or `app/` was resolved against the named module's export list.
A reference counts as dead only when all of the following hold, which keeps the
obvious false positives out:

* the named module exists in this tree;
* it HAS an explicit export list (a module without one exports everything);
* the symbol is a top-level function there (it has a `name ∷` signature at
  column 0), not a record field — a field reached through `Type(..)` IS
  exported and is excluded;
* the symbol does not appear in the export list;
* the export list contains no `module X` re-export that could smuggle it out.
  ZERO of the targets below have one, so that path is not a source of error
  here.

**Result: 59 cross-module dead links**, plus one same-module self-reference
(`src/World/Save/Storage.hs:905` → `publishValidated`, which at least sits in
the same file a reader already has open).

**Spot-checked by hand**, the three most-referenced targets:

| Target | Refs | Verified |
|---|---|---|
| `Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged` | 8 | defined at `:384`; module exports only `processLuaMsg`, `processLuaMsgs` |
| `Engine.Input.Thread.Dispatch.dispatchInput` | 4 | defined at `:96`; module exports only `processInputs`, `processInput` |
| `World.Fluid.River.Identify.traceRivers` | 4 | defined at `:157`; export list is `identifyWorldRivers`, `riverThreshold`, `labelRiverComponents`, `computeBedDepth`, `depthFromRadius`, `maxBedDepth` |

Two more were confirmed against export lists read in full earlier in this
exploration: `App.Boot.patchBootConfig` (referenced from `app/Main.hs:142` and
`src/Engine/Core/Defaults.hs:19`) and `Engine.Preview.Discovery.walkFiles`
(referenced from `src/Engine/Preview/Unit.hs:94` and
`src/Engine/Preview/Building.hs:247`).

**Why these cluster the way they do.** The pattern concentrates on
MODULE-SPLITTING SEAMS. All four of `Engine.Input.Thread.Keyboard`, `.Char`,
`.Mouse` and `.Scroll` open by saying they are "reached only through
`'Engine.Input.Thread.Dispatch.dispatchInput'`" — #787 moved the router into a
module that exports only its two public entry points, so the function each
sibling names to explain how it is reached is the one thing a reader cannot
reach. The same shape produced the eight `handleLoadStaged` references across
the save/load path and the four `traceRivers` references across the
`World.Fluid.River.Identify.*` split. These are not typos; they are the residue
of extracting a module and then documenting the extraction from the outside.

The reader cost is uniform: the comment names the function that actually does
the thing, and that is precisely the name that will not resolve as a link, will
not import, and cannot be jumped to. Nothing in the gate set inspects haddock
link targets, so the count only grows as more modules are split.

**Severity: low individually, moderate in aggregate.** No behaviour is involved
anywhere. It is recorded as one finding because 59 instances across 46 files is
a property of the tree rather than a defect in any one file, and because the fix
is a policy choice — export the named function, point at the exported entry
point instead, or drop the module qualifier — that wants deciding once.

**The count of 59 was a FLOOR** (established by EXPL-39, then closed by the
re-run below). The original detector required the referenced symbol to have a
top-level `name ∷` signature IN THE NAMED MODULE before judging it unexported —
so it silently skipped every reference where the named module exists but the
symbol is defined somewhere else entirely. `'World.Geology.Timeline.buildAge'`
is exactly that case and was missed.

**Corrected re-run.** Under the rule "the symbol is not exported by the named
module AND is a real top-level function defined elsewhere in the tree", eight
further sites appear — seven of them new here, plus EXPL-39's, which is filed
separately for the context around it:

| Reference site | Dead target | Actually defined in |
|---|---|---|
| `src/Engine/PlayerEvent.hs:14` | `'Engine.PlayerEvent.emitEvent'` | `Engine.PlayerEvent.Emit` |
| `src/Engine/Core/Log/Types.hs:59` | `'Engine.PlayerEvent.emitEvent'` | `Engine.PlayerEvent.Emit` |
| `src/Engine/Core/State.hs:418` | `'Engine.PlayerEvent.emitEvent'` | `Engine.PlayerEvent.Emit` |
| `src/Engine/Graphics/Vulkan/Texture/Limits.hs:35` | `'Engine.Graphics.Vulkan.Texture.Handle.generateTextureHandle'` | `Engine.Asset.Manager` |
| `src/World/Plate/Coast.hs:105` | `'World.Plate.Elevation.continentalShelf'` | `World.Plate.Profiles` |
| `src/World/Save/Types.hs:1243` | `'Engine.Scripting.Lua.API.Units.Combat.lookupInfection'` | `Infection.Types` |
| `src/World/Thread/Command/Save.hs:51` | `'World.Thread.handleWorldCommand'` | `World.Thread.Command` |
| `src/World/Geology/Ore.hs:4` | `'World.Geology.Timeline.buildAge'` | `World.Geology.Timeline.Loop` (EXPL-39) |

Verified by reading the named modules' export lists: `Engine.PlayerEvent`
exports only `PlayerEvent(..)`, `CategoryCfg(..)`, `NotificationCfg` and
`eventStoreCap`; `Engine.Graphics.Vulkan.Texture.Handle` exports only
`BindlessTextureHandle(..)` and `toBindlessHandle`. Neither re-exports the
named function.

Two of these are worth singling out. **`src/Engine/PlayerEvent.hs:14` is
SELF-referential** — the facade's own haddock points at a function its own
module does not export. And **`src/World/Plate/Coast.hs:105`** names
`World.Plate.Elevation` where `World.Plate.Profiles` owns the function, while
`World.Plate`'s own facade header gets it right ("`World.Plate.Profiles` —
boundary elevation profiles + continental shelf"), so the tree contains both
the correct and the incorrect attribution.

**Corrected total: 67 cross-module dead links.** The full list below is the
original 59; the eight above are additional.

**Excluded as false positives**, so a re-run does not resurface them: haddock
references of the form `'UI.setVisible'`, `'UI.setClickable'`,
`'UI.removeElement'`, `'UI.setControlFocus'`, `'UI.clearControlFocus'` name LUA
API bindings, not Haskell functions. They resolve as module references only
because `src/UI.hs` happens to exist.

**Mechanically detectable.** The sweep above is about twenty-five lines of
Python with no engine dependency, in the same shape as
`tools/unicode_operator_audit.py`. If the policy is "a haddock link must
resolve", that guard is cheap to add and would hold the line afterwards.

Already filed separately, and cross-referenced rather than removed, because each
was found in context with detail this entry does not carry: **EXPL-6**
(`Engine.Core.Workers.allWorkers`, referenced twice from `App.Boot`) and
**EXPL-24** (`UI.InputOwnership.pagesInScope`, referenced from
`UI.Manager.Query` and `UI.FocusNavigation`).

#### The full list

| Reference site | Dead target |
|---|---|
| `src/Building/HitTest.hs:6` | `'Building.Render.buildingToQuad'` |
| `src/Engine/Asset/YamlLocations.hs:90` | `'Location.Bounds.rawContainsPoint'` |
| `src/Engine/Asset/YamlLocations.hs:101` | `'Location.Overlay.anchorOk'` |
| `src/Engine/Asset/YamlLocations.hs:104` | `'Location.Overlay.anchorOk'` |
| `src/Engine/Core/Defaults.hs:19` | `'App.Boot.patchBootConfig'` |
| `src/Engine/Core/State.hs:349` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/Engine/Input/Thread/Char.hs:8` | `'Engine.Input.Thread.Dispatch.dispatchInput'` |
| `src/Engine/Input/Thread/Keyboard.hs:7` | `'Engine.Input.Thread.Dispatch.dispatchInput'` |
| `src/Engine/Input/Thread/Mouse.hs:8` | `'Engine.Input.Thread.Dispatch.dispatchInput'` |
| `src/Engine/Input/Thread/Scroll.hs:10` | `'Engine.Input.Thread.Dispatch.dispatchInput'` |
| `src/Engine/Input/Thread.hs:83` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/Engine/Loop/Mode.hs:220` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/Engine/Loop/Mode.hs:225` | `'Engine.Scripting.Lua.API.Save.saveOwnerSet'` |
| `src/Engine/Preview/Building.hs:247` | `'Engine.Preview.Discovery.walkFiles'` |
| `src/Engine/Preview/Unit.hs:94` | `'Engine.Preview.Discovery.walkFiles'` |
| `src/Engine/Scripting/Lua/API/InputInject.hs:19` | `'Engine.Input.Inject.deferModUps'` |
| `src/Engine/Scripting/Lua/API/Items/Ground.hs:48` | `'Engine.Scripting.Lua.API.Structure.resolveStructurePage'` |
| `src/Engine/Scripting/Lua/API/Power.hs:144` | `'Unit.Selection.onActivePage'` |
| `src/Engine/Scripting/Lua/API/Save/Bridge.hs:318` | `'World.Save.Integrity.luaEdgeResolves'` |
| `src/Engine/Scripting/Lua/API/Save/Bridge.hs:362` | `'World.Save.Integrity.luaEdgeResolves'` |
| `src/Engine/Scripting/Lua/API/Save/Bridge.hs:422` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/Engine/Scripting/Lua/API/Save/Bridge.hs:475` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/Engine/Scripting/Lua/API/Save/Bridge.hs:503` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/Engine/Scripting/Lua/API/Save.hs:243` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/Engine/Scripting/Lua/API/Units/Page.hs:22` | `'Engine.Scripting.Lua.API.Units.Inventory.unitAmbientTemp'` |
| `src/Engine/Scripting/Lua/API/Units/Transfer.hs:765` | `'Unit.Transfer.withinReach'` |
| `src/Engine/Scripting/Lua/API/WorldQuery/Pick.hs:251` | `'Location.Bounds.seamAliases'` |
| `src/Engine/Scripting/Lua/Thread/Dispatch.hs:412` | `'Engine.Scripting.Lua.API.Save.saveOwnerSet'` |
| `src/Language/Generated/Boundary.hs:50` | `'Language.Generated.Render.capitalizeWord'` |
| `src/Language/Generated/Hash.hs:5` | `'World.Gem.mix64'` |
| `src/Language/Generated/Hash.hs:6` | `'Location.Overlay.idSalt'` |
| `src/Language/Generated/Hash.hs:41` | `'Location.Overlay.idSalt'` |
| `src/Language/Generated/Orthography.hs:23` | `'Language.Generated.Render.capitalizeWord'` |
| `src/Location/Overlay.hs:368` | `'Engine.Asset.YamlLocations.validAnchorTags'` |
| `src/UI/FocusNavigation.hs:14` | `'UI.InputOwnership.pagesInScope'` |
| `src/UI/InteractiveBounds.hs:145` | `'UI.Clipping.absolutePosition'` |
| `src/UI/Manager/Query.hs:185` | `'UI.InputOwnership.pagesInScope'` |
| `src/UI/Manager/Query.hs:312` | `'Engine.Scripting.Lua.API.UI.Property.pushElementInfoTable'` |
| `src/UI/Types.hs:66` | `'UI.Render.uiLayerToLayerId'` |
| `src/UI/Types.hs:206` | `'UI.Manager.Query.hitsAtPointBy'` |
| `src/World/Command/Types.hs:265` | `'Engine.Scripting.Lua.API.Save.continueLoad'` |
| `src/World/Fluid/River/Identify/BedDepth.hs:8` | `'World.Fluid.River.Identify.traceRivers'` |
| `src/World/Fluid/River/Identify/Breakthrough.hs:6` | `'World.Fluid.River.Identify.traceRivers'` |
| `src/World/Fluid/River/Identify/ChunkIndex.hs:9` | `'World.Fluid.River.Identify.traceRivers'` |
| `src/World/Fluid/River/Identify/Components.hs:9` | `'World.Fluid.River.Identify.traceRivers'` |
| `src/World/Fluid/River/Identify.hs:92` | `'World.Fluid.River.Identify.Components.targetRiverCount'` |
| `src/World/Fluid/Types.hs:51` | `'Sim.Thread.emitWorldDirtyFluids'` |
| `src/World/Generate/Chunk/Fluid.hs:549` | `'Sim.Thread.emitWorldDirtyFluids'` |
| `src/World/Load/Publish.hs:57` | `'World.Thread.processAuthorizedSave'` |
| `src/World/Load/Publish.hs:74` | `'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'` |
| `src/World/Load/Stage.hs:99` | `'Engine.Scripting.Lua.API.Save.continueLoad'` |
| `src/World/Load/Stage.hs:156` | `'World.Save.Snapshot.Adapter.pageToWorldPageSave'` |
| `src/World/Render/Zoom/Icons.hs:231` | `'World.Render.Zoom.Cursor.emitCursorQuad'` |
| `src/World/Save/Component/Page.hs:1175` | `'World.Save.Component.WorldGen.fromLocationInstanceDTOv1'` |
| `src/World/Save/Serialize.hs:176` | `'World.Save.Storage.selectLoadGenerationUnsafe'` |
| `src/World/Save/Storage.hs:905` | `'World.Save.Storage.publishValidated'` (same module) |
| `src/World/Save/Types.hs:807` | `'World.Save.Snapshot.itemAllocatorErrors'` |
| `src/World/Slope/Recompute.hs:18` | `'World.Slope.Compute.computeTileSlope'` |
| `src/World/Slope/Roughness.hs:60` | `'World.Slope.Compute.computeTileSlope'` |
| `app/Main.hs:142` | `'App.Boot.patchBootConfig'` |

---

## Asset-loader path references

### EXPL-28. `Engine.Asset.YamlVegetation`'s summary names a `data/vegetation.yaml` that does not exist; the data is a directory of five files

`src/Engine/Asset/YamlVegetation.hs:2`:

```haskell
-- | Vegetation definitions loaded from @data/vegetation.yaml@.
```

That one line is wrong twice.

**No such file exists.** `data/vegetation` is a DIRECTORY, holding five files:
`farmland.yaml`, `grasses.yaml`, `ground_cover.yaml`, `mosses.yaml`,
`snow.yaml`.

**And the module does not load from a fixed path at all.** Its loader is
path-parameterised (`src/Engine/Asset/YamlVegetation.hs:40-42`):

```haskell
loadVegetationYaml ∷ LoggerState → FilePath → IO [VegetationDef]
loadVegetationYaml logger =
    loadYamlList logger "vegetation" "vegetation types" vfVegetation
```

Production reaches it through `buildColorPalette`
(`src/World/ZoomMap/ColorPalette.hs:92-101`), which enumerates the directory
and folds every file's definitions together:

```haskell
    -- Load all vegetation YAMLs
    vegFiles ← listVegetationYamls vegDir
    vegDefs ← concat ⊚ mapM (loadVegetationYaml logger) vegFiles
```

with `vegDir` supplied as `"data/vegetation"` by both callers
(`src/World/Load/Stage.hs:114`, `src/World/Thread/Command/Init.hs:271`). The
third consumer, `loadVegetationYamlFn`
(`src/Engine/Scripting/Lua/API/YamlTextures.hs:140-153`), likewise takes its
path from Lua.

**A sibling module documents the identical situation correctly**, which is what
marks this as isolated staleness rather than a house convention.
`src/Engine/Asset/YamlMaterials.hs:2`:

```haskell
-- | Material definitions loaded from @data/materials/*.yaml@, and the
--   fold of those definitions into a 'World.Material.MaterialRegistry'.
```

— the directory-glob form, and `buildColorPalette` treats the two symmetrically
(`loadMaterialDirectory logger matDir` beside `listVegetationYamls vegDir`).

**Severity: low.** No behaviour is involved; this is the module's one-line
summary. Recorded because it sends a reader looking for a single data file that
does not exist, does not tell them the five real ones are enumerated by the
caller, and is contradicted by the sibling loader one directory over.

#### Method note: two sweeps that did not pay off

This finding is the residue of two tree-wide sweeps run after EXPL-27, both
recorded here so the technique's limits are on file and the negative results are
not rediscovered.

**References to nonexistent project modules** — 195 raw hits, ALL false
positives. The matcher treated `'Engine.Core.State.EngineEnv'`-style TYPE links
as module names. A correct version would have to split the trailing
capitalised component and check it against the target's exported types and
constructors; EXPL-27's function-level technique does not transfer as cheaply.

**Comment references to nonexistent file paths** — 749 raw hits across `src/`,
`app/`, `tools/`, `docs/` and `test-headless/`, and nearly all legitimate:
self-test fixtures (`scripts/fake.lua` ×144, `src/Fake/Init.hs` ×26,
`docs/a.md` ×25, `tools/foo.py` ×6), gitignored-by-design runtime files
(`config/video.local.yaml` ×35, `config/notifications.local.yaml` ×34,
`config/keybinds.local.yaml` ×28, `config/save.local.yaml` ×20), and
deliberate counterexamples (`assets/textures/iconsEvil/x.png`, the
containment case in `Engine.Preview.Discovery`). Restricting to comments in
`src/`+`app/` naming a non-gitignored path left SIX mentions across four
distinct paths — two of them false positives from a regex splitting
`--preview items/tools/hammer.png` at a slash, and one correct as written:
`src/World/Save/Compat/SessionV90.hs:62` and `:300` cite
`scripts/lib/serialize.lua` while explicitly describing it as "long-removed"
and "removed by #761", which is the point of those comments.

---

## Numeric claims in repository documentation

A targeted sweep of the countable assertions in `CLAUDE.md` and
`docs/engine_contracts.md`. Most held exactly, which is worth recording
alongside the two that did not: `src/` really is 731 Haskell modules against a
stated "~730"; the compiled-atlas corpus really is 7 unit trees, 7
`data/units/*.yaml` files, 116 compiled atlas PNGs and 4,620 source animation
frames, matching every statement of those figures in CLAUDE.md and in
`Unit.Atlas.Load`'s "ALL SEVEN shipped units" header.

### EXPL-29. `engine_contracts.md`'s enum-audit coverage counts are each off by one, in the paragraph that says not to hand-count them

`docs/engine_contracts.md:1337-1342`:

```markdown
**Coverage.** Of the 43 guarded types, 38 are on the save wire and 28 are
named by a live component today; the rest are guarded pre-emptively, which
is the point of keying on the `Serialize`-via-`Generic` instance rather
than on save reachability. Don't hand-count these: the audit prints the
guarded total on every run, and `docs/save_compat/enum_baseline.json`'s
per-type `onSaveWire` / `components` fields are the other two.
```

Running the audit this paragraph names:

```
$ python3 tools/enum_append_only_audit.py
enum_append_only_audit.py: 44 guarded sum type(s) match docs/save_compat/enum_baseline.json
```

and counting the two `docs/save_compat/enum_baseline.json` fields it names:

| Claim | Documented | Actual |
|---|---|---|
| guarded types | 43 | **44** |
| entries with `onSaveWire` | 38 | **39** |
| entries with a non-empty `components` | 28 | **29** |

All three are off by exactly one, consistent with a single type having been
appended without the prose being revisited — which is precisely what the
audit's `--update-baseline` ratchet is designed to make routine.

What lifts this above arithmetic drift is that this is the paragraph that says
**"Don't hand-count these: the audit prints the guarded total on every run"**.
It is itself the stale hand-count it warns against, and it cites the two
baseline fields that disprove its own other two figures. A reader who follows
its advice gets the right answer and finds the sentence that gave the advice
contradicting it.

**Severity: low.** Nothing is enforced by these numbers and the audit itself is
green — `44 guarded sum type(s) match` — so no gate is weakened. Recorded
because the three figures are trivially re-derivable from artifacts the same
sentence points at, and because a documentation paragraph whose stated purpose
is to stop people hand-counting should not be a hand-count.

### EXPL-30. CLAUDE.md says `tools/README.md` "lists all ~85" probes; there are 89 and README names 83

`CLAUDE.md:73-74`:

```markdown
4. **Behavior probes — opt-in, not a default gate.** ~85 headless
   `tools/*_probe.py` scripts each boot a real engine and gate one system
```

and `CLAUDE.md:720`:

```markdown
engine, pass/fail checks). `tools/README.md` lists all ~85;
```

There are **89** probe files in `tools/`, and `python3 tools/ci_probes.py
--status` — which CLAUDE.md itself designates "the authoritative list of every
probe's CI eligibility — never trust a prose list of probe names" — enumerates
89.

The tilde absorbs 85 against 89. **"lists all ~85" does not**, and that is the
actual defect: it is a COMPLETENESS claim about another file.
`tools/README.md` names **83** distinct probes, so **six probes are absent from
the file CLAUDE.md says lists them all**:

- `blood_gpu_lifecycle_probe.py`
- `construction_blueprint_footprint_probe.py`
- `item_list_widget_probe.py`
- `location_embark_probe.py`
- `portal_ghost_probe.py`
- `portal_location_probe.py`

The discrepancy runs one way only — README names no probe that does not exist
on disk, so nothing there is stale in the other direction.

Four of the six are probes CLAUDE.md discusses BY NAME elsewhere in its own
subsystem-contract section — `item_list_widget_probe` (the container window
stack and the moving-target preemption proof), `location_embark_probe`
(location discovery), and `blood_gpu_lifecycle_probe` (the needs-GPU blood
gate). So the reader is told these exist, told README lists all of them, and
will not find them there.

**Severity: low.** Nothing is enforced by the count, and `ci_probes.py
--status` remains authoritative and correct — CLAUDE.md's own instruction to
prefer it over any prose list is exactly the right one and is what makes this
harmless in practice. Recorded because "lists all" is checkable, is currently
false by six, and points at a file a reader is being sent to as complete.

---

## Player transfers

### EXPL-31. `Unit.Transfer`'s header calls its serializable set "the six types" and then names seven

`src/Unit/Transfer.hs:29-38`:

```haskell
--   __On the 'Data.Serialize.Serialize' instances below__ (#1246): the
--   six types a durable transfer ORDER carries — 'TransferEndpoint',
--   'TransferItemRef', 'TransferReason', 'TransferFailure',
--   'TransferState' and the 'QueuedTransfer'/'TransferBatch' pair — are
--   serializable ONLY so 'Unit.Transfer.Orders.TransferOrders' can ride
--   'World.Save.Types.WorldPageSave', the transitional IN-MEMORY load
--   bridge [...]
```

Counting the list as written: `TransferEndpoint`, `TransferItemRef`,
`TransferReason`, `TransferFailure`, `TransferState`, `QueuedTransfer`,
`TransferBatch` — **seven**, in the sentence that calls them six. Naming the
last two as "the `QueuedTransfer`/`TransferBatch` pair" makes them read as one
list ITEM, but they are two types, and the noun the number governs is "types".

The enumeration is otherwise exactly right, which is worth stating so a fix does
not go looking for a missing entry: extracting every `data` declaration paired
with a `deriving (… Serialize …)` in this module yields those seven and nothing
else —

| Type | Declared |
|---|---|
| `TransferEndpoint` | `:138`, deriving at `:147` |
| `TransferItemRef` | `:174`, deriving at `:182` |
| `TransferReason` | `:196`, deriving at `:207` |
| `TransferFailure` | `:285`, deriving at `:288` |
| `TransferState` | `:303`, deriving at `:310` |
| `QueuedTransfer` | `:339`, deriving at `:342` |
| `TransferBatch` | `:346`, deriving at `:350` |

Nothing is missing from the list and nothing extra is claimed; only the count is
wrong.

**Where the "six" plausibly came from**, which is the reason this is worth
correcting rather than shrugging at: six is the RIGHT number twenty lines
earlier, for a different thing. `src/Unit/Transfer.hs:22-24` says "every item
keeps its own six-state lifecycle entry", and `TransferState` really does have
exactly six constructors (`:303-310`): `TransferQueued`, `TransferInTransit`,
`TransferReadyToCommit`, `TransferCompleted`, `TransferCancelled`,
`TransferFailed`. A reader who has just absorbed "six-state lifecycle" and then
meets "the six types" now has two sixes in play in one header, one correct and
one not, describing different sets.

**Severity: low.** No behaviour is involved and the list is accurate, so a
reader who counts rather than trusts gets the truth. Recorded because it is a
stated count immediately followed by the items it counts — the easiest kind of
claim to check and the least excusable kind to get wrong — and because the
adjacent correct "six" invites the two to be conflated.

Verified truthful in the same header, so a fix does not disturb it: the claim
that `@tools/enum_append_only_audit.py@ guards the constructor ORDER of both
halves` holds. `docs/save_compat/enum_baseline.json` carries
`Unit.Transfer.TransferEndpoint`, `Unit.Transfer.TransferReason` and
`Unit.Transfer.TransferState` alongside their
`World.Save.Component.Transfer.TransferEndpointDTO` / `TransferReasonDTO` /
`TransferStateDTO` mirrors — both halves, as claimed. The remaining four are
records rather than sum types and so have no constructor order to guard, which
is why they are correctly absent.

#### Also verified exact in this pass, and needing no change

Two dense contracts checked alongside this one, recorded so the negative
results are on file:

- **The #1330 `math.random` contract** (`CLAUDE.md:271-278`). "Eleven gameplay
  modules draw from it" is EXACT: stripping comments and counting real
  `math.random(` call sites gives precisely eleven Lua files —
  `bear_ai.lua`, `locations.lua`, `mental_state.lua`, `red_squirrel_ai.lua`,
  `thoughts.lua`, `unit_ai_combat_attack.lua`, `unit_ai_core.lua`,
  `unit_ai_mental.lua`, `unit_ai_needs.lua`, `unit_ai_sleep.lua`,
  `unit_ai_water.lua` — every one of them gameplay, none under `scripts/ui/`.
  "Nothing under `scripts/` may call `math.randomseed`" is upheld: zero call
  sites, and the only two occurrences of the identifier are comments in
  `scripts/ui/random.lua:9` and `scripts/ui/randbox.lua:159` describing the
  rule and the removed call respectively.
- **The 500-line module budgets.** Both tools really do cover the families
  CLAUDE.md names — `tools/haskell_module_budget.py:32` globs
  `src/Engine/Input/Thread.hs` plus `src/Engine/Input/Thread/**/*.hs`, and
  `tools/lua_module_budget.py:31` globs `scripts/unit_ai.lua` plus
  `scripts/unit_ai_*.lua` — and both report every budgeted module within its
  limit.

---

## Main loop and the save barrier

### EXPL-32. `runGatedByCaptureLock` cites "the same shape every other owner uses" and names Combat, which acks only while paused

`src/Engine/Loop/Mode.hs:205-213`:

```haskell
--   the transaction moves on to the snapshot boundary and the publish
--   only after it returns. That is why the
--   'acknowledgeCurrent' below is UNCONDITIONAL — it is this thread's
--   half of that handshake — and why the function has the same "check
--   locked, do unlocked work if not locked, always ack" shape every
--   other owner uses (Unit/Building/Combat/Simulation, see e.g.
--   'Unit.Thread').
```

Checking each owner the sentence names against its real tick:

| Owner | Actual shape | Matches the quoted shape? |
|---|---|---|
| `SaveUnit` / `SaveBuilding` (`src/Unit/Thread.hs:88-105`) | `unless locked $ …` for every side-effecting step, then TWO unconditional acks | yes, exactly |
| `SaveSimulation` (`src/Sim/Thread.hs:119-136`) | branches on `locked ∨ ssPaused ∨ enginePaused ∨ not (anyLiveWorld ss)`; BOTH branches ack | yes, effectively |
| `SaveCombat` (`src/Combat/Thread.hs:88-105`) | branches on **`paused`**; acks in the paused branch ONLY | **no** |

Combat's tick:

```haskell
    paused ← readIORef (wsEnginePausedRef (toWorldSimCapability env))
    next ← if paused
        then do
            -- A save boundary drains accepted combat commands
            -- before acknowledging; ordinary pause retains the
            -- historical no-work behaviour.
            locked ← captureLocked (saveBarrierRef env)
            saving ← saveInProgress (saveBarrierRef env)
            when (saving ∧ not locked) $ processAllCommands env
            acknowledgeCurrent (saveBarrierRef env) SaveCombat
            pure tick
        else do
            processAllCommands env
            ...                       -- no acknowledgeCurrent on this path
```

`acknowledgeCurrent` appears exactly once in that file and the not-paused
branch never reaches it. So combat is not "always ack", and its outer
discriminator is the PAUSE FLAG rather than the capture lock — a different
shape on both axes of the quoted description.

**"Every other owner" is a wider claim than the parenthetical, and it fails
twice more.** Of the seven `SaveOwner` constructors, the per-tick ackers divide
like this:

- `World/Thread.hs:93` acks inside a branch its own neighbouring comment
  describes as "this whole branch only runs when locked is False" — world
  acknowledges only on an UNLOCKED tick, the opposite of unconditional.
- `SaveLua` has no per-tick acknowledgement at all, deliberately.
  `src/Engine/Scripting/Lua/Thread.hs:278-285` says so outright: "SaveLua's own
  self-ack (in saveWorldFn/handleLoadStaged) persists across every later
  quiescence pass by design […] so this loop never needed a per-tick
  `acknowledgeCurrent` the way Unit/Combat/Simulation/Input do".
- `SaveInput` (`src/Engine/Input/Thread.hs:90-97`) DOES match — `unless locked`
  around the work, then an unconditional ack.

So the shape is genuinely shared by Unit, Building, Simulation and Input, and
not by Combat, World or Lua. The sentence names Combat among the four it holds
for.

**No bug.** A save transaction pauses before it waits on owners, so combat is
already in its paused branch by the time the barrier needs its acknowledgement;
the handshake completes and the protocol works as designed.

**Severity: low-medium**, above the nit tier because of what this module exists
to be. Its own header (`src/Engine/Loop/Mode.hs:1-12`) says the three loops were
unified precisely because "the save-barrier handshake below were duplicated
between them, so a change to the barrier protocol had to find and correctly
update two copies of the same code and only one copy of the reasoning behind
it." This sentence IS that single copy of the reasoning. It tells a maintainer
auditing the barrier that four named owners all acknowledge unconditionally,
when one of them acknowledges only while paused — so a generalisation drawn
from it (for instance, that combat is safe to gate on the capture lock alone)
would be wrong.

Worth noting for whoever fixes it: `src/Engine/Scripting/Lua/Thread.hs:284`
names a DIFFERENT four — "Unit/Combat/Simulation/Input" — and also includes
Combat, so the same misattribution exists in two places and both deserve the
same correction.

Verified truthful in the same module, so a fix does not disturb any of it:

- The three-mode difference table (`:39-46`) matches all three `LoopMode`
  values field for field — `lmPollEvents`, `lmCameraUpdates`, `lmExitRequested`
  and `lmEndOfTick` differ exactly as tabulated.
- `frameBudgetMicros = 16666` really is ~60 fps, and really is slept by exactly
  the two non-windowed modes (`Engine/Loop.hs:65`, `Engine/Loop/Headless.hs:37`)
  and by neither windowed one.
- `promoteToRunning`'s documented four-row transition table matches its
  `atomicModifyIORef'` exactly, and the stated requirement that the read and
  the write be ONE atomic step is honoured.

---

## Lua text display

### EXPL-33. CLAUDE.md's `text_wrap.lua` summary names two functions and four consumers; there are three functions and ten

`CLAUDE.md`'s #1159 text-display contract:

```markdown
Pixel-width wrapping goes through `scripts/ui/text_wrap.lua` —
`byCharacter` (the debug console) and `byWord` (all three log panels) —
rather than a fourth private copy.
```

The rule this sentence exists to state — one shared code-point-aware
implementation, no private copies — HOLDS, and nothing in `scripts/` implements
a rival walk. The parenthetical attributions do not.

**Measured across `scripts/`:**

| Function | Defined | Consumers |
|---|---|---|
| `textWrap.byCharacter` | `scripts/ui/text_wrap.lua:48` | `shell.lua` — **1**, exactly the debug console |
| `textWrap.byWord` | `scripts/ui/text_wrap.lua:73` | `combat_log.lua`, `unit_log.lua`, `injury_log_panel.lua`, **`etymology_panel.lua`** — **4** |
| `textWrap.truncateToWidth` | `scripts/ui/text_wrap.lua:148` | `event_log.lua`, `popup.lua`, `loading_screen.lua`, `ui/list.lua`, `ui/item_list.lua` — **5** |

Three departures:

1. **`byWord` has four consumers, and one is not a log panel.**
   `scripts/etymology_panel.lua` is the name-etymology popup (#1104's single
   panel for all three entry points), not a log.
2. **`event_log.lua` never calls `byWord`.** The archetypal log panel requires
   the module at `:34` and uses `truncateToWidth` at `:534`, `:538` and `:549`.
   So whichever three panels "all three log panels" is counting, the primary
   event log is not among the word-wrapping ones.
3. **The module exports a THIRD function the sentence has no room for.**
   `truncateToWidth` has more consumers than either wrapping function, and the
   framing "pixel-width wrapping goes through … `byCharacter` and `byWord`"
   describes the module as doing one job when it does two.
   `scripts/ui/item_list.lua:372-376` documents its own thin re-export of it as
   the shared implementation, so the third function is load-bearing rather than
   incidental.

**The module's own header is accurate**, which is what makes this a CLAUDE.md
defect rather than a code one. `scripts/ui/text_wrap.lua:1-12`:

```lua
-- Shared pixel-width FITTING for text DISPLAY surfaces (#1159, #1107):
-- wrapping text that may run onto more lines, and truncating text that
-- must stay on one.
--
-- [...] this module is the one implementation
-- that does, shared by the debug console (character wrap) and the log
-- panels (word wrap with a character hard-break).
```

It names BOTH jobs — wrapping and truncating — and says "the log panels"
without asserting a count. The summary in CLAUDE.md narrowed both.

**Severity: low-medium.** No behaviour is involved and the no-private-copies
rule is intact. Above the nit tier because CLAUDE.md is the always-loaded file
and this is the entry a contributor reads before adding a display surface: told
the module offers two WRAPPING functions with a fixed four-consumer roster,
someone who needs single-line truncation has been given no reason to believe
`text_wrap.lua` covers their case — and would write exactly the private copy
the sentence's last clause forbids.

#### Also verified exact in this pass

Recorded so the negative results are on file and are not re-derived:

- **`scripts/unit_ai.lua`'s #538 split.** Its only `utility =` / `execute =`
  occurrences are action-registry wiring pointing at submodule functions
  (`combat.retreatUtility`, `needs.idleUtility`, …), never an inline body — so
  "entry/orchestration module only" holds. Every submodule its header names
  exists, `unit_ai_locations.lua` included, and the five designation-job fields
  land in exactly the four files claimed, with `s.tillJob` and `s.plantJob`
  sharing `scripts/unit_ai_farm.lua`.
- **`World.Generate.Chunk.generateLoadedChunk`.** "Chunk loading and the
  zoom-map ore survey both go through here" is exactly its consumer set:
  `World/Thread/ChunkLoading.hs:97` and `:275`, and `World/Thread/Cursor.hs:188`
  (the Resources-tab ore survey, which `World/Thread/Helpers.hs:68` calls the
  "zoom-chunk ore survey").
- **`Engine.Loop.Frame.computeAmbientLight`.** Both inline value comments are
  arithmetically exact — day `0.5 + 0.2·sin` gives 0.5 at the horizon and 0.7
  at noon, night `0.15 + 0.35·(1+sin)` gives 0.15 at midnight and 0.5 at the
  horizon. This also confirms `Engine/Core/Init.hs`'s
  `sunAngleRef ← newIORef 0.25 -- start at noon`, since 0.25 · 2π is π/2.
- **The offscreen render target.** "One plain color image per frame in flight"
  and "the image index IS the frame-in-flight index" both hold:
  `Engine/Graphics/Vulkan/Init.hs:138-140` passes `gcMaxFrames
  defaultGraphicsConfig` (2) as the image count, and `drawFrameOffscreen`
  passes `frameIdx` for both arguments of `renderSceneFrame`.
- **`World/Render/Quads.hs:75`** really does use the 128-chunk fallback that
  `Engine.Loop.Frame.activeWorldCircumferenceTiles` cites as "the same default".

---

## Save-system item walk

### EXPL-34. `flattenItemInstances` says "all three now go through this one definition" and enumerates three; there are four

`src/World/Save/Types.hs:793-799`:

```haskell
--   THE recursive item walk of the save system (#1090). It used to be
--   written out three times, once per consumer; all three now go
--   through this one definition, together with 'pageItemContainers'
--   below: 'World.Save.Snapshot.allItemInstanceIds' (the id-allocator
--   and duplicate-id checks),
--   'Engine.Scripting.Lua.API.Save.Integrity.knownEntitiesFromSaveData'
--   (the load-time known-entity set), and 'missingItemDefReferences'.
```

`flattenItemInstances` has FOUR consumers:

| Consumer | Call site | Named in the comment? |
|---|---|---|
| `World.Save.Snapshot.allItemInstanceIds` (defined `:336`) | `src/World/Save/Snapshot.hs:343` | yes |
| `Engine.Scripting.Lua.API.Save.Integrity.knownEntitiesFromSaveData` (defined `:38`) | `src/Engine/Scripting/Lua/API/Save/Integrity.hs:81` | yes |
| `missingItemDefReferences` (defined `:913`, same module) | `src/World/Save/Types.hs:937` | yes |
| **`World.Save.Integrity.pageEntitiesFrom`** (defined `:373`) | `src/World/Save/Integrity.hs:381` | **no** |

**The unnamed consumer uses exactly the idiom the comment describes.**
`pageEntitiesFrom` calls `pageItemContainers ItemsGroundFirst` at
`src/World/Save/Integrity.hs:378` and `flattenItemInstances` at `:381` — the
same paired walk, in the same shape, as all three named consumers. The
companion function tracks identically: `pageItemContainers` has three external
call sites (`Snapshot.hs:340`, `Integrity.hs:378`,
`Lua/API/Save/Integrity.hs:78`) plus one internal (`Types.hs:921`) — the same
four consumers, so the pair really is consumed as a pair everywhere.

**It is not an obscure caller.** `transferOrderRefs`' own haddock in that same
file (`src/World/Save/Integrity.hs:318-322`) treats `pageEntitiesFrom` as a
first-class stage of the integrity graph:

> 'orderRefErrors' (wrong-page, fatal) and 'danglingOrderRefErrors' (absent,
> tolerated) both consume it, and the load boundary consumes it a third time
> through 'pageEntitiesFrom', so a reference kind added to an order is checked
> everywhere from one edit.

**What is true and what is not.** "It used to be written out three times, once
per consumer" is a historical statement about the pre-#1090 tree and is not in
question. The falsified part is "**all three now** go through this one
definition", followed by an enumeration that stops at three. `pageEntitiesFrom`
belongs to the later transfer-order integrity work (#1246), so this is the
familiar shape seen in EXPL-29, EXPL-30 and EXPL-31: a consumer added after a
consolidation, with the consolidation's own count left behind.

**Severity: low.** No behaviour is involved, and the consolidation itself is
intact — the fourth consumer DOES route through the shared walk, which is the
invariant that matters. Recorded because the sentence opens "THE recursive item
walk of the save system" and then enumerates its consumers, which is an
exhaustiveness claim, in a module whose entire subject is that nothing walks
items privately any more.

#### Also verified exact in this pass

- **`wsSpoilRef` "has exactly two writers"**
  (`src/World/Render/SpoilQuads.hs:124-132`) holds as written: the only writes
  are `src/World/Thread/Command/Edit/Dig.hs:158` and `:317` and
  `src/World/Load/Stage.hs:240`, i.e. the two modules named. Its further claim
  that "the scripting API only READS this state" also holds —
  `src/Engine/Scripting/Lua/API/World/Query.hs:66` and `:232` are both
  `readIORef` — so the deliberately-raw (unwrapped) chunk lookup it justifies
  is sound.
- **`WorldGenParamsDTO`'s "all five are decode-only"**
  (`src/World/Save/Component/WorldGen.hs:1095-1103`) is exact: five frozen DTOs
  are named and five are declared — `WorldGenParamsDTOv5` (`:1200`), `v4`
  (`:1302`), `v3` (`:1403`), `v2` (`:1503`), `v1` (`:1599`).
- **`Unit.Thread.Movement.Climb`'s spelled-out slip-chance formula**
  (`:44-55`) matches `slipChancePerZ` term for term — base 0.05, squared skill
  modifier, the `1.5 / (dex × str)` control modifier clamped to [0.1, 5], the
  `max(1, mass/70)` weight modifier, and the final [0.001, 0.5] clamp. The
  code's extra `max 0.05` floor on `dex × str` is a divide-by-zero guard the
  outer clamp already subsumes, so the documented formula is equivalent for
  every reachable input. `heightPerClimbZ = baselineUnitHeight = 1.8`
  (`Movement/Types.hs:41`) really does give a baseline acolyte a climb reach of
  exactly 1 z, and `data/units/acolyte.yaml:40` really does declare a height
  mean of 1.8.

### EXPL-35. `loadVegetationYamlFn`'s body says it parses "the single vegetation YAML file"; both Lua callers loop over five

`src/Engine/Scripting/Lua/API/YamlTextures.hs:148-153`:

```haskell
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                -- Parse the single vegetation YAML file
                defs ← loadVegetationYaml logger filePath
```

There is no single vegetation YAML file. `data/vegetation/` holds five —
`farmland.yaml`, `grasses.yaml`, `ground_cover.yaml`, `mosses.yaml`,
`snow.yaml` — and `loadVegetationYaml` takes its path as a parameter,
decoded from the Lua argument on the line above.

**Both Lua callers enumerate the directory**, which is what makes this a
cardinality claim rather than a wording preference.
`scripts/vegetation_loader.lua` describes itself in its first line as
"enumerates data/vegetation/*.yaml and loads each one", and does exactly that:

```lua
    local files = engine.listFiles(folder, ".yaml")
    ...
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadVegetationYaml(fullPath)
```

and `scripts/startup_loader.lua:190` and `:235` both route it through
`addYamlDir`, whose whole body (`:23-30`) is a `engine.listFiles` call and a
`for` loop queueing one `loaderFn(path)` per file. So this binding is invoked
FIVE times per boot, once per file, and the comment at the point of the call
says it parses the one file that exists.

**Distinct from EXPL-28, and recorded separately.** EXPL-28 is
`src/Engine/Asset/YamlVegetation.hs:2` naming a `data/vegetation.yaml` that
does not exist — a wrong PATH, in a module summary. This is a different file, a
different sentence, and a wrong CARDINALITY, asserted in a function body at the
exact seam where the caller loops. Two separate edits, and together they show
the "vegetation is one file" belief outliving the split in more than one place.

**A near-miss worth naming so a fix does not over-reach.** The same function's
sibling in that file, `src/Engine/Scripting/Lua/API/YamlTextures.hs:72`, reads
`-- Parse the single YAML file` for materials — and `data/materials` is a
directory too, loaded through the same `addYamlDir` at
`scripts/startup_loader.lua:189` and `:231`. That one is NOT counted as a
defect here: it does not name a family, and immediately follows the decoding of
one `filePath` argument, so "the single YAML file" reads naturally as "the one
file passed in". It is the qualifier "vegetation" at `:152` that turns the same
phrase into a statement about how many vegetation files there are.

**Severity: low.** No behaviour is involved — the function correctly parses
whatever path it is handed. Recorded because it is an in-body comment at the
call site, contradicted by both of its callers in the adjacent language, and
because it corroborates EXPL-28 rather than duplicating it.

#### Also verified in this pass

- `src/Sim/State/Types.hs:39`'s "the world thread, the sole writer of
  `wsTilesRef`" is consistent with the tree: every mutation site is under
  `src/World/Thread/` (Command, ChunkLoading, and the Edit/Cursor modules) plus
  `src/World/Load/Stage.hs:326` and `:398`, which write freshly-staged
  replacement state rather than the live session's.
- `src/Building/Thread/Command.hs:7`'s "its only caller — `Unit.Thread`" holds:
  `processAllBuildingCommands` is invoked once, from
  `src/Unit/Thread.hs:99`, inside that thread's `unless locked` block.

---

## Sun angle: three curves, one input

### EXPL-36. `computeAmbientLight`'s inline labels name noon and midnight; its input convention puts those values at dawn and dusk

**This is a comment defect, not a behavioural one.** That conclusion is load
bearing and was reached only after the phase difference below turned out to be
documented elsewhere as deliberate — see "Why this is not a bug".

`src/Engine/Loop/Frame.hs:50-56`:

```haskell
computeAmbientLight ∷ Float → Float
computeAmbientLight sunAngle =
    let angle = sunAngle * 2.0 * π
        sunHeight = sin angle
    in if sunHeight ≥ 0
       then 0.5 + 0.2 * sunHeight   -- day: 0.5 at horizon, 0.7 at noon
       else 0.15 + 0.35 * (1.0 + sunHeight)  -- night: 0.15 at midnight, 0.5 at horizon
```

**The input convention is fixed, documented, and not what those labels assume.**
`src/World/Time/Types.hs:42-47`:

```haskell
-- | Convert world time to sun angle (0.0 .. 1.0)
--   Mapping: midnight (0:00) = 0.0, 6am = 0.25, noon = 0.5, 6pm = 0.75
worldTimeToSunAngle (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60.0 + fromIntegral m ∷ Float
    in totalMinutes / 1440.0   -- 1440 = 24 * 60
```

and that is exactly the value reaching the function:
`src/World/Thread/Time.hs:113-115` writes `worldTimeToSunAngle wt` into
`wsSunAngleRef` each tick, `src/Engine/Loop/Frame.hs:352` reads `sunAngleRef`,
and `:356` passes it straight in.

`sin(2π·a)` peaks at `a = 0.25` and troughs at `a = 0.75`, so the labels land a
quarter-day away from the times they name:

| Clock | `sunAngle` | `computeAmbientLight` | The label at that value |
|---|---|---|---|
| midnight | 0.00 | 0.50 | comment calls 0.50 "horizon" |
| 6am | 0.25 | **0.70** | comment calls 0.70 "noon" |
| noon | 0.50 | 0.50 | comment calls 0.70 "noon" |
| 6pm | 0.75 | **0.15** | comment calls 0.15 "midnight" |

So "0.7 at noon" is really 0.7 at dawn, and "0.15 at midnight" is really 0.15 at
dusk. Both extreme labels name the wrong time of day.

**Why this is not a bug.** Two sibling curves read the SAME `sunAngle` and are
clock-correct, each saying so explicitly:

- `Power.Network.solarIntensity` (`src/Power/Network.hs:112-113`) is
  `max 0 (negate (cos (2 * pi * sunAngle)))` — `+1` at `a = 0.5`, i.e. noon,
  matching its haddock's "0 = midnight, 0.25 = dawn, 0.5 = noon, 0.75 = dusk …
  1 at noon, 0 at dawn/dusk".
- `Unit.LineOfSight.nightPerceptionFactor` (`src/Unit/LineOfSight.hs:227-231`)
  is keyed on `cos ((sunAngle - 0.5) * 2 * π)` — peak at noon.

and that second one names the discrepancy outright
(`src/Unit/LineOfSight.hs:221-225`):

> A cosine keyed to `'sunAngle'` peaking at 0.5 (noon) and troughing at 0.0/1.0
> (midnight) — the mapping `'WorldTime'` documents (`@World.Time.Types@`) —
> rather than reusing `'computeAmbientLight'` (`@Engine.Loop.Frame@`), **whose
> own phase is tuned for the lighting shader, not gameplay, and shouldn't be
> coupled to this.**

So the tree already knows the ambient curve's phase differs, treats it as an
intentional artistic choice for lighting, and deliberately declines to share it.
The curve should be left alone; only its two labels are wrong.

**A second site encodes the same mislabelling.** `src/Engine/Core/Init.hs:206`:

```haskell
  sunAngleRef ← newIORef 0.25       -- start at noon
```

By `worldTimeToSunAngle`'s mapping, 0.25 is 6am, not noon. The seed is
overwritten by the world thread on its first tick — and `defaultWorldTime` is
10:00, i.e. 0.4167 — so nothing observable follows from it, but the comment
carries the same quarter-day error and would be fixed by the same pass.

**The GLSL twin is a faithful port and needs no change.**
`src/Engine/Graphics/Vulkan/ShaderCode.hs:174-181` reproduces the Haskell curve
line for line (`sin(sunAngle * 6.28318530718)`, same two branches), correctly
described at `:168-173` as a "GLSL port of Engine.Loop.Frame's
computeAmbientLight". It carries no clock labels, so it states nothing false.

**Severity: low-medium**, and higher than a typical label slip for two reasons.
First, three functions in this tree consume one `sunAngle` value with two
different phases, which is exactly the situation where a reader needs the
comments to be precise — and the one that is imprecise is the odd one out.
Second, a maintainer has ALREADY had to work this out and write it down in a
third module; had `computeAmbientLight`'s own labels been right (or had they
said "phase deliberately offset for lighting"), that paragraph in
`Unit.LineOfSight` would not have needed to exist to warn the next person off.

Recorded scope note: this entry claims no behavioural defect and proposes no
change to any curve. The suggested repair is to relabel the two inline comments
in terms the input convention supports, note the deliberate offset that
`Unit.LineOfSight` already documents, and correct `Init.hs:206`'s "start at
noon".

---

## Combat damage model

### EXPL-37. The Tier 3 damage model's derivation block is stale in two places: a `modeCoupling` constant that does not exist, and a `delivered` formula missing two factors

`src/Combat/Resolution/Constants.hs:43-53` is the derivation block for the whole
Tier 3 damage model — the thing `Combat.Resolution`'s own haddock sends readers
here for ("see that module's haddock for the model derivation these tunables
feed"):

```
-- Tier 3 physical damage model (real-units kinematics). The wielder
-- does muscular WORK on the swing; that work becomes kinetic energy of
-- an effective striking mass at an impact velocity (capped by how fast
-- the limb can move). From the swing we read off ENERGY (what shears /
-- penetrates tissue) and MOMENTUM (what crushes):
--
--   work  = eHuman · strength · modeWork · skillEff · stamina · (1−pain)   [J]
--   m_eff = weaponMass + modeCoupling · bodyMass                          [kg]
--   v_max = vHuman · modeSpeed · (0.6 + 0.4·dexterity)                    [m/s]
--   v     = min(v_max, sqrt(2·work / m_eff))     -- work-limited OR capped
--   E     = ½·m_eff·v²        p = m_eff·v
```

Three of those five lines match the implementation. Two describe a model the
code does not implement.

**`modeCoupling` does not exist.** A grep across `src/`, `docs/` and
`test-headless/` returns exactly one occurrence: line 51 of this comment. There
is no such constant anywhere in the tree, and it is absent from
`Constants.hs`'s own export list.

**The implemented effective mass is ROTATIONAL, not a linear coupling.**
`swingKinematics` (`src/Combat/Resolution/Damage.hs:203-217`) builds a moment of
inertia about the shoulder and reduces it to an effective mass at the tip:

```haskell
swingKinematics work wMass wLen wCoM armLen armMass dexterity mode =
    let lw   = wLen   / 100.0                       -- m
        la   = armLen / 100.0                       -- m
        rCoM = la + clamp 0.0 1.0 wCoM * lw          -- implement CoM radius
        bigR = max 0.05 (la + lw)                    -- contact (tip) radius
        iArm = (1.0 / 3.0) * armMass * la * la        -- rod about one end
        iWep = wMass * rCoM * rCoM                    -- point mass at CoM
        inertia = max 1.0e-4 (iArm + iWep)
        vMax = vHuman * modeSpeed mode * (0.6 + 0.4 * dexterity)
        omegaMax = vMax / bigR
        omega = min omegaMax (sqrt (2.0 * work / inertia))
        vTip  = omega * bigR
        mEff  = inertia / (bigR * bigR)
    in (0.5 * inertia * omega * omega, mEff * vTip)
```

So `m_eff` is `inertia / R²`, and BODY MASS enters only through
`armMass = armMassFrac * bodyMassA` (`Damage.hs:265`) as part of the limb's own
inertia — five percent of it — never as a `modeCoupling · bodyMass` term added
to the weapon's mass. The documented velocity line is likewise a reduction of
the real one: the code solves for ANGULAR speed against `inertia`
(`omega = min omegaMax (sqrt (2·work / inertia))`) and only then converts,
`vTip = omega * bigR`.

**Two lines DO survive, which is what makes the block read as current.**
`½·inertia·ω²` expands to `½·(m_eff·R²)·(v/R)² = ½·m_eff·v²`, and the returned
momentum is `mEff * vTip` outright — so the final line is algebraically correct
as written. It is only the DEFINITION of `m_eff` feeding it that is wrong, and
nothing in the block hints that the quantity is derived from a lever rather than
stated directly.

**The rotational model is the design, not a drift.** Two exported constants
exist solely to build `iArm`, and their own haddock describes the lever:

```haskell
armMassFrac ∷ Float
armMassFrac = 0.05

-- | Inferred swinging-limb length as a fraction of height (single-arm
--   reach ≈ 0.4·height). Sets the lever arm length.
armLengthFrac ∷ Float
armLengthFrac = 0.40
```

Neither has any role in the documented linear model. The formula block predates
the rotational form and was not revisited when it landed.

**Severity: medium — the highest non-behavioural rating in this report.** No
behaviour is wrong and every number the model produces is correct. It ranks
above the other documentation findings because of where it sits and who reads
it: this is the derivation for the entire Tier 3 damage model, at the head of
the module whose sole purpose is to hold its tunables, and it is the block a
maintainer consults before touching `armMassFrac`, `armLengthFrac`, `vHuman`,
`eHuman`, or a weapon's mass / length / centre-of-mass. Two of its five lines
describe a mass model built on a constant that does not exist, and omit the
lever-arm geometry that actually determines the answer — so reasoning from it
about how weapon length or reach affects a strike gives the wrong result.

#### Second defect in the same block: the `delivered` formula omits two factors

Ten lines below the kinematics, the same block states the delivery and severity
chain (`src/Combat/Resolution/Constants.hs:61-63`):

```
--   delivered = driver · η_kind · (1 − natRes[kind]) · (1 − toughCut)
--   severity  = delivered · kindSeverityFactor[kind] / (partMaxHp · perHp)
--   perHp = energyPerHp (stab/slash) | momentumPerHp (blunt)
```

The implementation (`src/Combat/Resolution/Damage.hs:308-309`) carries SIX
multiplicative factors where the comment lists four:

```haskell
        budget = driver * rsEff strike * qualityF
                        * (1.0 - natRes) * (1.0 - toughCut) * kindWeight
```

- **`qualityF` is a real, missing tuning term.** It is
  `0.6 + 0.4 * rsQuality strike` (`Damage.hs:300`), a 0.6–1.0 multiplier, and
  `rsQuality` is a SEPARATE field from `rsEff`, so it is not folded into
  η_kind — `ResolvedStrike` declares them distinctly (`Damage.hs:53-54`):
  `rsEff` is "0..1 weapon suitability for kind" (η_kind), `rsQuality` is
  "0..1 build quality". The line immediately above it records that the term is
  test-pinned: "weaponPenetration's quality term is pinned by a unit test
  ('a better-made weapon penetrates more')". A weapon's build quality can cut
  delivered damage by forty percent and the documented formula has no term for
  it.
- **`kindWeight`** is 1.0 for a single-kind attack and splits the swing across
  components for a combo attack (`computeSeverity`'s haddock,
  `Damage.hs:232-236`) — a decomposition factor rather than a tuning one, so a
  weaker omission, but still absent.

A third, softer gap in the same two lines: the chain runs `delivered → severity`
directly, while the code computes `sev` from **`sevDriver`**
(`Damage.hs:394`), not from `budget`. The function's own haddock
(`Damage.hs:220-224`) draws that distinction explicitly — "`driver` is the
swing's raw energy …; `sevDriver` the tissue-weighted damage that survived the
layer stack" — so the entire layered-penetration stage sits between the block's
two lines. This is recorded as a simplification the surrounding prose partly
qualifies ("layered-penetration target model"), not as a flat error.

Correct as written in those same lines, and needing no change:
`severity = … · kindSeverityFactor[kind] / (partMaxHp · perHp)` matches
`Damage.hs:394` in shape exactly, and
`perHp = energyPerHp (stab/slash) | momentumPerHp (blunt)` matches
`Damage.hs:280-282` exactly.

Both defects live in the one comment block and one editing pass fixes them.
They are recorded together for that reason, while being distinct failures: the
kinematics half names a constant that does not exist and swaps a rotational
model for a linear one, whereas this half keeps the right shape and drops two
factors from it.

Verified accurate in the same block, so a fix need not touch them:

- `work = eHuman · strength · modeWork · skillEff · stamina · (1−pain)` matches
  `src/Combat/Resolution/Damage.hs:252-253` exactly, including the
  `skillEff = 0.6 + 0.4 · clamp 0 1 (skill/100)` shaping at `:245` and the
  stamina floor of 0.3 at `:250`.
- `v_max = vHuman · modeSpeed · (0.6 + 0.4·dexterity)` matches
  `src/Combat/Resolution/Damage.hs:211` exactly.
- `E = ½·m_eff·v²` and `p = m_eff·v` are the correct readings of the returned
  pair, as shown above.

---

## Hydrology namespace map

### EXPL-38. Three references outlived the deleted `World.Fluids` facade, one of them a CI path-selector self-test case

`World.Fluids` no longer exists. Commit `88d8c96f` — "Delete World.Fluids
facade, import World.Fluid.Ocean directly" — removed `src/World/Fluids.hs`,
resolving `docs/code_health_findings.md` CH-81 (filed as `[#1110]`). Three
references survived it.

**1. `tools/ci_expensive_gates.py:22-24` — the comment's example does not
exist.**

```python
    # Generation-family subtrees use a `Name*` prefix (not `Name/*`) so each
    # family's facade module (e.g. src/World/Generate.hs, src/World/Fluids.hs)
    # matches alongside its directory. Deliberately NOT src/World/* wholesale:
```

`src/World/Generate.hs` exists. `src/World/Fluids.hs` does not. This comment is
the RATIONALE for why the globs at `:28-32` are written `Name*` rather than
`Name/*`, and half of the evidence it offers is a deleted module.

The fluid family makes that worse rather than incidental: there is now no fluid
facade of any kind. CH-81's own verification note records that
`src/World/Fluid.hs` never existed either
(`docs/code_health_findings.md:1986-1988`: "`src/World/Fluid.hs` does not
exist, so the confusable pair is `World.Fluids` vs `World.Fluid.<Sub>`"). So
fluid is precisely the family for which the `Name*` form buys nothing — and it
is the example chosen to justify the `Name*` form.

**2. `tools/ci_expensive_gates.py:135` — a self-test case for a path that can
never change.**

```python
        ("worldgen", ["src/World/Fluids.hs"], True),
```

The case asserts that this path selects the worldgen gate, and it still PASSES:
selection is a pure `fnmatch` of the diff paths against `"src/World/Fluid*"`
(`:29`), with no filesystem check anywhere. But it is now vacuous — no pull
request can touch `src/World/Fluids.hs`, so this case can never guard a real
diff, and the gate's genuine coverage of the fluid family rests entirely on its
other cases. A self-test that passes while testing an unreachable input reports
coverage it does not have.

**3. `docs/hydrology_pipeline.md:284-285` — cites a resolved collision as
current.**

```markdown
- `docs/code_health_findings.md` CH-80 (this document's origin), CH-81 (the
  `World.Fluids` / `World.Fluid.*` naming collision).
```

CH-81 is checked off and filed, and the collision was resolved by deleting one
of its two halves. This is the mildest of the three — a pointer into a findings
document, which is historical by nature — but a reader following it to
understand the current naming goes looking for a collision that no longer has
two sides.

**Everything else in that index is exact**, which is worth recording because
the index is large and was checked in full. All seventeen modules named in
`docs/hydrology_pipeline.md` §12 ("Where does X live?") and §13 ("Outside the
pipeline") were resolved against the tree, and sixteen exist exactly as
written:

`World.Geology.Timeline.River`, `World.Geology.Timeline.RiverTrace`,
`World.Hydrology.Event`, `World.Fluid.Ocean`, `World.Fluid.OceanMask`,
`World.Fluid.Lake.Identify.Ocean`, `World.Hydrology.WaterTable`,
`Sim.Fluid.Active`, `World.Hydrology.Simulation.Flow`, `World.Fluid.IceLevel`,
`World.Fluid.Ice`, `World.Fluid.Lava`, `World.River.Identity`,
`World.River.Naming`, `World.Fluid.Internal`, `World.Fluid.Types`.

`World.Fluids` is the only miss, and it is the one that was deleted.

**Severity: low-medium.** No behaviour is affected and no gate is currently
weakened — the `src/World/Fluid*` glob still matches every real fluid module,
so a genuine fluid change still triggers the worldgen gate. It sits above the
nit tier because one of the three sites is a CI path-selection SELF-TEST whose
passing status now proves nothing about the selector, and because the comment
it sits beneath is a rationale for the glob style that its own example no
longer supports.

---

## Geological ore deposition

### EXPL-39. `World.Geology.Ore` says its caller is `World.Geology.Timeline.buildAge`; that module exports only `buildTimeline`

`src/World/Geology/Ore.hs:1-6`:

```haskell
-- | Flow-routed sedimentary ore deposition.
--
--   Runs once per geological Age (from 'World.Geology.Timeline.buildAge'),
--   after that age's hydrology simulation. Every volcanic feature
--   matching a 'DepositSpec' sheds sediment flux proportional to its
--   size, activity and the age's duration; [...]
```

`buildAge` is not in `World.Geology.Timeline`. That module's entire export list
is one function:

```haskell
module World.Geology.Timeline
    ( buildTimeline
    ) where
```

`buildAge` is defined at `src/World/Geology/Timeline/Loop.hs:191`, and it is
that module — not `Timeline` — which calls `buildOreSheets`
(`src/World/Geology/Timeline/Loop.hs:308`, reached through `buildAge` at
`:194`). So the reference names the wrong module for a function that does exist
one level down, in the sibling a split moved it into. The same shape as
EXPL-25, where `Engine.Input.Thread` was named and `Engine.Input.Thread.Mouse`
was meant.

The rest of the sentence is correct: ore deposition really does run once per
Age, and really does run after that age's hydrology — `buildOreSheets` is
called inside `buildAge` with the age's own `FlowResult` in hand.

**Severity: low.** No behaviour is involved. Recorded because it is the first
line of the module's haddock, it is the pointer a reader follows to find where
ore deposition is sequenced within an Age, and it leads to a module that does
not contain the named function.

**This finding revises EXPL-27**, which is why it is worth more than its own
severity suggests. EXPL-27's sweep reported 59 cross-module dead haddock links;
its detector required the referenced symbol to have a top-level signature IN THE
NAMED MODULE before judging it unexported, so it skipped every reference whose
symbol lives in a different module altogether — precisely this case. The 59 is a
floor. A note to that effect has been added to EXPL-27.

Verified exact in the same module, so a fix does not disturb it: the structural
claim "adding a resource means adding a row here (plus its lever in
`OreLevers`)" holds — `depositSpecs = [ironSpec, copperSpec]`
(`src/World/Geology/Ore.hs:119-120`) corresponds one-to-one with `OreLevers`'
two material multipliers `olIron` and `olCopper` beside the global `olGlobal`
(`src/World/Geology/Ore/Types.hs:102-106`).

#### Also verified exact in this pass: `World.Geology.Erosion.Math`

Recorded because the module is unusually formula-dense and every claim in it
survived checking, arithmetic included:

- **Hydraulic** — documented "scales with elevation difference"; implemented
  `hydraulicSlopeBoost = 0.4 + 0.6 * slopeNorm` (`:140`). Its own note that
  "steep terrain erodes 2.5× faster" is exactly `1.0 / 0.4`.
- **Wind** — documented "less sensitive to slope"; implemented
  `windSlopeBoost = 0.8 + 0.2 * slopeNorm` (`:154`).
- **Thermal** — documented "scales with slope squared"; implemented
  `thermalSlopeBoost = slopeNorm * slopeNorm` (`:167`), literally squared.
- **Chemical** — documented "slow, uniform … increases effective erodability of
  softer rocks"; implemented
  `chemicalErodability = min 1.0 (erodability + chemical * 0.3)` with the rate
  multiplied by `0.5` (`:183-188`). Its worked example computes: limestone at
  hardness 0.4 (erodability 0.6) with `epChemical` 0.5 gives
  `0.6 + 0.15 = 0.75`, exactly as the comment states.
- **`applyErosionLerp4`** — documented as lerping "just the 5 hot Floats" and
  deferring "the 4 sediment-only Floats (temperature, precipitation, humidity,
  snow)". `ErosionParams`
  (`src/World/Geology/Timeline/Feature.hs:207-219`) has exactly that split:
  `epIntensity`, `epHydraulic`, `epThermal`, `epWind`, `epChemical` are the
  five `lerpHot` is applied to, and `epTemperature`, `epPrecipitation`,
  `epHumidity`, `epSnowFraction` are the four deferred into the sediment
  closure.

---

## Save component machinery

### EXPL-40. `World.Save.Component.Types` says concrete components live in three modules; five define them, and the two omitted are the optional pair

`src/World/Save/Component/Types.hs:7-12`:

```haskell
--   This module is deliberately CONTENT-FREE — it knows the SHAPE of a
--   component (id, version, required/optional, dependencies, an
--   encoder, a version-dispatched decoder, a validator) but nothing
--   about any specific gameplay slice. The concrete components live in
--   "World.Save.Component.Session"/".Page"/".Entities"; the authoritative
--   registry + cross-component assembly is "World.Save.Component". Both
--   import THIS module, so this one must not import them (no cycle).
```

**Five modules under `src/World/Save/Component/` define concrete components,
not three.** Counting `ComponentCodec` declarations:

| Module | Codecs it declares | Named in the header? |
|---|---|---|
| `World.Save.Component.Session` | `CoreSessionDTO`, `TexPaletteDTO`, … | yes |
| `World.Save.Component.Page` | `WorldPages`, `WorldEditsDTO`, … | yes |
| `World.Save.Component.Entities` | `BuildingsDTO`, `UnitsDTO`, … | yes |
| **`World.Save.Component.Knowledge`** | `ContainerKnowledgeDTO` | **no** |
| **`World.Save.Component.Transfer`** | `TransferOrdersDTO` | **no** |

`World.Save.Component.WorldGen` declares none — it holds the frozen DTOs that
`Page` consumes — so its absence from the list is correct and a fix should not
add it.

**The two omitted modules are precisely the pair the persistence contract
singles out.** `container-knowledge` and `transfer-orders` are the ONLY two
optional components in the envelope. CLAUDE.md lists the registry as
"…, `metadata`, the two OPTIONAL `container-knowledge` and `transfer-orders`,
plus dynamic `lua.<module>` components", and states that
`docs/persistence_contract.md` §5 must be read "before declaring a third". So a
reader consulting this header to find where concrete components live is pointed
at the three required-component modules and misses both optional ones — the two
whose contract is most delicate, since each has to supply an honest default for
its own absence.

**The sentence reads as closed rather than illustrative**, which is what makes
it misleading rather than merely brief. It is the statement that establishes
this module's place in the layering: content-free machinery here, concrete
components there, registry over there — and "**Both** import THIS module, so
this one must not import them (no cycle)" turns the preceding list into a claim
about the whole module graph.

**Severity: low-medium.** No behaviour is involved, and the registry itself
(`World.Save.Component.saveComponentRegistry`) remains the authority and lists
every component. Above the nit tier because this is the layering statement for
the persistence machinery — the thing a contributor reads before adding a
component — and the two entries it drops are exactly the two whose optionality
is a documented, contract-governed exception rather than the norm.

Verified exact in the same header, so a fix does not disturb it — including its
hardest and longest-reaching part:

- The **frozen-DTO boundary rule** (`:24-56`) is stated coherently and
  transitively ("recurse the decision into a frozen type's own fields, so the
  boundary is transitive"), with both FREEZE criteria, both LEAF criteria, and
  worked examples on each side (`MaterialId`, `ChunkCoord`, `Pose`,
  `Direction` as leaves; `GeoTimeline` as the leaf-by-a-different-mechanism
  case).
- Its explicit carve-out that `World.Save.Types`' positional entity snapshots
  (`BuildingInstanceSnapshot` / `UnitInstanceSnapshot`) do NOT qualify as
  leaves, because they directly carry mutable `ItemInstance` values and live
  `StatModifier` / `Wound` / `Scar` data.
- The frozen-wire-contract rule (`:14-22`): a shipped DTO is never edited in
  place, the old type is frozen and moved to `csOlderVersions` via `atVersion`,
  and the new one becomes `csVersion` / `csEncode` / `csDecode`.

### EXPL-41. `World.Save.Component.Entities`' header omits the `core-session` dependency from two of its five components

`src/World/Save/Component/Entities.hs:6-11` lists the module's five components
with their dependencies. Three are exact; two are incomplete:

```haskell
--   - @"buildings"@ (required) — per page: the building instances +
--     their delivered materials / storage / build progress. Owner:
--     'Building.Types.BuildingManager'. Depends on @"world-pages"@.
--   - @"units"@ (required) — per page: the unit instances (stats, skills,
--     modifiers, equipment, inventory, wounds, scars, immunity, blood).
--     Owner: 'Unit.Types.UnitManager'. Depends on @"world-pages"@.
```

Both declare TWO dependencies:

| Component | Header says | `csDeps` declares |
|---|---|---|
| `"buildings"` (`:355`) | `world-pages` | `[worldPagesComponentId, coreSessionComponentId]` |
| `"units"` (`:715`) | `world-pages` | `[worldPagesComponentId, coreSessionComponentId]` |
| `"unit-sim"` (`:1078`) | `world-pages` AND `units` | `[worldPagesComponentId, unitsComponentId]` |
| `"craft-bills"` (`:1328`) | `world-pages` + `buildings` | `[worldPagesComponentId, buildingsComponentId]` |
| `"power-nodes"` (`:1506`) | `world-pages` + `buildings` | `[worldPagesComponentId, buildingsComponentId]` |

The last three match exactly, and all five really are `csRequired = True` as the
header claims.

**The declaration sites document the missing dependency, in a wording that
gives the header away.** `src/World/Save/Component/Entities.hs:343-345`:

```haskell
-- Depends on @"core-session"@ too: assembly refills each page's
-- @bsnNextId@ from the GLOBAL building-id allocator that @"core-session"@
-- installs, so it must fold first (requirement 9).
```

and `:704-705`:

```haskell
-- Depends on @"core-session"@ too, for the global unit-id allocator
-- (@usnNextId@), same reasoning as @"buildings"@ above.
```

The word "**too**" at both sites reads as an addendum to a list stated
elsewhere — the dependency was supplemented locally rather than corrected in the
header, and the header's own bullets still read as complete.

**Why this is more than a missing word.** `csDeps` is load-bearing:
`registryStaticErrors` (`World.Save.Component`) checks that every declared
dependency names a registered component and that the graph is acyclic, and the
graph is what orders the fold. The reason given at the declaration site is a
real ordering constraint — `core-session` installs the global building- and
unit-id allocators and "must fold first (requirement 9)". A contributor adding a
sixth component, or reasoning about why a component folds when it does, reads
the header's per-component list as the dependency map, and for two of five
entries it is missing the dependency that pins their position.

**Severity: low-medium** — the same tier as EXPL-40, in the same subsystem and
plausibly the same fixing pass. No behaviour is affected: `csDeps` is correct in
code, so the fold order and the registry's static checks are right.

Verified exact in the same header, so a fix does not disturb it: the
requirement-4 freeze narrative (`:27-56`) correctly states that the
`"buildings"`/`"units"` components carry `BuildingInstanceDTO` /
`UnitInstanceDTO` rather than `World.Save.Types`' positional
`BuildingInstanceSnapshot` / `UnitInstanceSnapshot`, and correctly explains why
those snapshots cannot be reused — they directly carry mutable `ItemInstance`
values and, on units, live `StatModifier` / `Wound` / `Scar` records. The
runtime-state mirror list (`UnitSimStateDTO`, `CraftBillDTO`/`BillQueueDTO`,
`PowerNodeDTO`/`NodeRegistryDTO`) is accurate, as is the note that a demolished
station's lingering craft bill is tolerated so its dependency is for ordering
rather than a hard orphan reject.

---

## Container-knowledge seeding

### EXPL-42. A haddock names `Building.Knowledge.SeedAtSpawn`; the constructor is `SeedWhenBuilt`, and "at spawn" is what the design rejects

`src/Engine/Scripting/Lua/API/Buildings/Progress.hs:158-167`:

```haskell
--   wrong. The trigger is the FIRST crossing of the completion
--   threshold ('Building.Types.currentActivity''s worker-driven arm:
--   'biBuildProgress' reaching 'bdBuildWork'), deliberately NOT
--   @BuildingSpawn@, which creates a worker-built building at zero
--   progress — and deliberately not anything a LOAD can re-trigger, so
--   restoring an already-built container never masquerades as a new
--   construction event. This arm covers exactly
--   'Building.Knowledge.SeedAtBuildCompletion' defs; the INSTANT-BUILT
--   class ('Building.Knowledge.SeedAtSpawn', which never calls this
--   verb at all) is seeded by "Building.Thread.Command" at placement.
```

**`SeedAtSpawn` does not exist anywhere in the tree.** The type has exactly two
constructors (`src/Building/Knowledge.hs:148-155`):

```haskell
    = SeedAtBuildCompletion
      -- ^ WORKER-BUILT (@bdBuildWork > 0@): the building is created at
      --   zero progress and only becomes Built when 'biBuildProgress'
      --   reaches 'bdBuildWork'. Seeded by
      --   @building.addBuildProgress@'s crossing of that threshold —
      --   deliberately NOT at spawn, which would fire while the thing
      --   is still a construction site.
    | SeedWhenBuilt
      -- ^ INSTANT-BUILT (@bdBuildWork == 0@, the portal/solar-panel
      --   shape): there is no construction work at all, so
      --   'Building.Types.currentActivity' carries it to Built on the
      --   TIME-BASED arm [...]
```

The instant-built constructor is **`SeedWhenBuilt`**.

**This is not a harmless misspelling, because "at spawn" is precisely what the
surrounding paragraph exists to rule out.** Four lines above the bad reference,
the same comment says the trigger is "deliberately NOT `@BuildingSpawn@`, which
creates a worker-built building at zero progress", and `SeedAtBuildCompletion`'s
own haddock repeats it: "deliberately NOT at spawn, which would fire while the
thing is still a construction site." A reader who takes `SeedAtSpawn` at face
value concludes that the instant-built class seeds on the SPAWN event — exactly
the misreading the neighbouring sentences are written to prevent. The real
constructor carries the name `SeedWhenBuilt` because its transition is observed
on `currentActivity`'s TIME-BASED arm (once the appearing animation elapses),
not at placement-as-spawn.

The other half of the same sentence is correct and should be left alone:
`SeedAtBuildCompletion` exists and is spelled right, and "which never calls this
verb at all" matches `SeedWhenBuilt`'s own documentation ("Nothing calls the
progress verb for this class").

**Severity: low-medium.** No behaviour is involved. Above the nit tier because
the invented name asserts a seeding trigger the design explicitly rejects, and
it does so in the one comment whose job is to say which of the two classes this
verb handles.

#### Method note: the type-reference sweep, and its noise

This finding came from a new sweep — the uppercase analogue of EXPL-27, looking
for haddock references to a TYPE or CONSTRUCTOR in a module that does not define
it. Recorded because the technique is much noisier than the function version and
should not be re-run naively:

- **168 raw hits.** Nearly all are plain MODULE references
  (`'Engine.Input.Thread.Dispatch'`) that the pattern splits into a module plus
  a capitalised tail, or facade re-exports (`'Unit.Types.UnitInstance'`, really
  defined in `Unit.Types.Instance` and re-exported by the `Unit.Types` facade).
- **26 candidates** survive excluding references whose full dotted name is
  itself a module.
- Hand-checking those 26 showed all but one really do exist — the detector's
  constructor extraction misses `| Ctor` lines with interleaved haddock, so
  `WorldLoadPublish`, `LuaUIClickEvent`, `FactionDebug`, `SimChunkLoaded`,
  `BuildingDestroy` and `SeedWhenBuilt` were all false negatives.

`SeedAtSpawn` is the single genuine dead type reference the sweep found.

Also verified exact in this pass, and recorded so they are not re-checked:

- `Engine.Graphics.Vulkan.Device`'s claim that "`scoreDevice` ranks every
  bindless-capable candidate above every incapable one" is provably true:
  `bindlessCapableBonus = 10000` against a maximum device-type base score of
  1000 for a discrete GPU, so any capable device scores at least 10010 and any
  incapable one at most 1000. The claim it justifies — that a usable best which
  still fails the capability check means no usable device exists — therefore
  holds. `findQueueFamilies`' offscreen note is also right: with no surface the
  present family aliases the graphics family.
- `World.Fluid.Lake.Types.packBitmask`'s documented layout — "bit @i@ (LSB = 0)
  of byte @b@ encodes tile @b * 8 + i@", 32 `Word8`s for 256 elements, unused
  trailing bits zero — is exactly what its `foldr` produces.

---

## Lua scripts

### EXPL-43. `circadian.lua` documents its function as `unit.getCircadianUrge`; no such engine verb is registered

`scripts/circadian.lua:63-66`:

```lua
-- unit.getCircadianUrge(uid) — 0..1, or nil if the unit doesn't exist or
-- has no resolvable position (issue #611 requirement: skip gracefully,
-- never error). Named to match the issue's own example call shape.
function M.getCircadianUrge(uid)
```

**`unit.getCircadianUrge` does not exist.** `unit` is the ENGINE's Lua table,
populated by `registerLuaFunction` calls in
`src/Engine/Scripting/Lua/API/Register/Unit.hs`, and no verb of that name is
registered there or anywhere else. A repo-wide search finds the identifier in
exactly two places: this definition, and its single real caller —
`scripts/unit_ai_sleep.lua:138`:

```lua
    local urge = circadian.getCircadianUrge(uid) or 0
```

which reaches it as `circadian.getCircadianUrge`, through the module table, as a
Lua-side helper.

**The comment records how the wrong prefix got there**: "Named to match the
issue's own example call shape." Issue #611's text used a `unit.`-prefixed
example, and the doc line copied that shape rather than the shape the function
actually has. The `unit.` prefix is not a harmless stylistic difference — it
names a DIFFERENT namespace, the one the debug console exposes, so the obvious
thing to do with a doc line in this form (paste `unit.getCircadianUrge(uid)`
into the console) fails with a nil-index error rather than returning the
documented 0..1.

Everything else on the line is accurate: the function does return 0..1 or `nil`,
and it does skip gracefully rather than erroring when the unit is missing or has
no resolvable position (`unit.getInfo` nil-guard, then `world.getSunAngleAt`
nil-guard).

**Severity: low.** No behaviour is involved and the one real caller uses the
correct name. Recorded because it is the doc line directly above the definition,
it states a call shape that cannot work, and the wrong namespace is the one a
reader is most likely to try by hand.

#### Method note: what the `scripts/` sweep found, and did not

The Lua tree was swept with the same techniques used on the Haskell side.
Recorded so the negative results are on file:

- **References to a `scripts/*.lua` file that does not exist** — 2 hits, both
  CORRECT as written, describing history in the past tense:
  `scripts/build_tool.lua:4` ("#403 — absorbs the former
  scripts/construct_tool.lua") and `scripts/lib/data_codec.lua:2` ("Replaces
  scripts/lib/serialize.lua's load()-based Lua-expression codec").
- **References to a `scripts.*` module path that does not resolve** — ZERO.
- **Comment references to an engine verb that is not registered** — 2 hits. One
  is the finding above. The other, `debug.hide()` at
  `scripts/ui/view_teardown.lua:236`, is not a defect: it is shorthand for
  `require("scripts.debug").hide()`, which the same file spells out in full at
  `:245`, `:248` and `:249`.
- **Numeric-word claims** — every one checked was exact:
  `scripts/lib/data_codec.lua:241`'s "the four limits above" is exactly four
  (`MAX_DEPTH`, `MAX_TABLE_ENTRIES`, `MAX_STRING_BYTES`, `MAX_TOTAL_BYTES` at
  `:63-66`, with the per-call override resolving exactly those four at
  `:257-260`); `scripts/ui/randbox.lua:610`'s "the three handlers that change
  text in response to a keystroke" is exactly three `notifyUserEdit` call sites
  (`:644`, `:656`, `:668`); and `scripts/unit_ai_deliver.lua:28`'s "Bound like
  the three above" matches the three `fetch.*` bindings immediately above it,
  with its claim that `unit_ai_construct.lua` and `unit_ai_repair.lua` "do the
  same" holding at `unit_ai_construct.lua:36-37` and `unit_ai_repair.lua:31`.
- **Exhaustiveness claims** — `scripts/tutorial_progress.lua:525`'s "v1 is the
  only schema" matches its single `version = 1` literal at `:498`; and
  `scripts/starvation.lua:11`'s "This is the one place the two fraction
  thresholds + both multipliers live" holds, including its cross-language
  claim — `tools/physiology_probe.py` really does read through the module
  (`require('scripts.starvation').speedMultiplier`) rather than hardcoding the
  values, and its own test points sit inside the bands rather than restating
  their boundaries.
