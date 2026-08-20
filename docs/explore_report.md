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

- [ ] EXPL-1. `climateRegionSize`'s comment calls the climate grid coarser than the geological grid, when it is finer
- [ ] EXPL-2. `preRenderWorkers` justifies the sim thread's teardown position with a dataflow the sim thread does not have
- [ ] EXPL-3. `Engine.Core.Workers`'s module haddock says "the two windowless modes" when there are three
- [ ] EXPL-4. `shutdownEngineWorkers`'s stated reason for not announcing is false for both of its normal-path callers
- [ ] EXPL-5. `stopWorkers` claims to be the only `WorkerSlot` traversal in the tree; `App.Boot` has another
- [ ] EXPL-6. `App.Boot` haddock links to `Engine.Core.Workers.allWorkers`, which is not exported
- [ ] EXPL-7. `sortFrameFiles` claims to be shared by the units and buildings viewers; the units viewer no longer calls it
- [ ] EXPL-8. `discoverBuildingEntries` claims every frame is lstat-proved regular; its static-entry branch tests only the name
- [ ] EXPL-9. `buildPreviewUnit`'s pipeline summary still describes the filesystem-first flow #1261 replaced
- [ ] EXPL-10. `App.LanguageReport` claims constant runtime "regardless of how many seeds are requested"; the work is linear
- [ ] EXPL-11. `runPreview`'s `mBrowse` haddock says a `Nothing` is "the degenerate no-target case"; that case never reaches it
- [ ] EXPL-12. `anySegmentIsSymlink` says it checks "any prefix" and "every ancestor"; the root itself is never lstat'd
- [ ] EXPL-13. `shutdownEngine`'s inline safety argument says Vulkan teardown precedes "the worker threads" stopping; two are already stopped
- [ ] EXPL-14. `migrateLegacyConfig` reports a destination write failure as a malformed legacy file, in a user-facing warning
- [ ] EXPL-15. `Unit.Atlas.Digest`'s description of the digest stream omits the tag's length prefix and the label field entirely
- [ ] EXPL-16. `pickFrame`'s "used by the render path and the hit-tester" omits both Lua API consumers
- [ ] EXPL-17. `unitToQuad`'s climb-occlusion branch is justified by a `spriteRowSpan` push the same function says it does not apply
- [ ] EXPL-18. `Building.Render`'s sort comment says units add a `spriteRowSpan` term "as units do"; they no longer do
- [ ] EXPL-19. `Unit.HitTest` claims to mirror a tile hit-test that no longer holds the math, and to use the "same math" the engine documents as different
- [ ] EXPL-20. `resolveTexture`'s haddock credits itself with animation mirroring; animations never reach it and it cannot honour `flip: false`
- [ ] EXPL-21. 98 production call sites spell inequality `≠` instead of `≢`; neither the operator audit nor CLAUDE.md's table can see it
- [ ] EXPL-22. The capability inventory says four of the five capability splits are §3.1 thread-privacy splits; two are, and the doc contradicts itself
- [ ] EXPL-23. `inputBoundaryPage` explains the modal-boundary tie-break by `PageHandle` and show-recency; the sort key is `(upLayer, upZIndex)`
- [ ] EXPL-24. `hitsAtPointBy`'s haddock cites `UI.InputOwnership.pagesInScope`, which is not exported
- [ ] EXPL-25. `isPointerSurfaceBlocked` names `Engine.Input.Thread` as its caller; since #787 that is `Engine.Input.Thread.Mouse`
- [ ] EXPL-26. `World.Save.Storage`'s header says it "receives only" four of six parameters, and its numbered transaction omits the requirement-9 refusal
- [ ] EXPL-27. 59 cross-module haddock links point at functions their named module does not export, concentrated on module-split seams

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

### EXPL-8. `discoverBuildingEntries` claims every frame is lstat-proved regular; its static-entry branch tests only the name

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

### EXPL-9. `buildPreviewUnit`'s pipeline summary still describes the filesystem-first flow #1261 replaced

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

### EXPL-10. `App.LanguageReport` claims constant runtime "regardless of how many seeds are requested"; the work is linear

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

### EXPL-11. `runPreview`'s `mBrowse` haddock says a `Nothing` is "the degenerate no-target case"; that case never reaches it

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

### EXPL-12. `anySegmentIsSymlink` says it checks "any prefix" and "every ancestor"; the root itself is never lstat'd

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

### EXPL-13. `shutdownEngine`'s inline safety argument says Vulkan teardown precedes "the worker threads" stopping; two are already stopped

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

### EXPL-14. `migrateLegacyConfig` reports a destination write failure as a malformed legacy file, in a user-facing warning

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

### EXPL-15. `Unit.Atlas.Digest`'s description of the digest stream omits the tag's length prefix and the label field entirely

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

### EXPL-16. `pickFrame`'s "used by the render path and the hit-tester" omits both Lua API consumers

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

### EXPL-17. `unitToQuad`'s climb-occlusion branch is justified by a `spriteRowSpan` push the same function says it does not apply

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

### EXPL-18. `Building.Render`'s sort comment says units add a `spriteRowSpan` term "as units do"; they no longer do

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

### EXPL-19. `Unit.HitTest` claims to mirror a tile hit-test that no longer holds the math, and to use the "same math" the engine documents as different

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

### EXPL-20. `resolveTexture`'s haddock credits itself with animation mirroring; animations never reach it and it cannot honour `flip: false`

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

### EXPL-21. 98 production call sites spell inequality `≠` instead of `≢`; neither the operator audit nor CLAUDE.md's table can see it

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

### EXPL-22. The capability inventory says four of the five capability splits are §3.1 thread-privacy splits; two are, and the doc contradicts itself

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

### EXPL-23. `inputBoundaryPage` explains the modal-boundary tie-break by `PageHandle` and show-recency; the sort key is `(upLayer, upZIndex)`

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

### EXPL-24. `hitsAtPointBy`'s haddock cites `UI.InputOwnership.pagesInScope`, which is not exported

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

### EXPL-25. `isPointerSurfaceBlocked` names `Engine.Input.Thread` as its caller; since #787 that is `Engine.Input.Thread.Mouse`

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

### EXPL-26. `World.Save.Storage`'s header says it "receives only" four of six parameters, and its numbered transaction omits the requirement-9 refusal

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
