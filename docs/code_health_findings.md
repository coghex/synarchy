# Code health findings

A running audit of the source tree for stale/incorrect comments, dead code,
oversized modules, misplaced functions, and poor names. Each entry is scoped to
be filed as its own issue. Working order: engine core → engine subsystems →
world → gameplay → Lua → tools → docs.

Status legend: `[ ]` not filed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a stated precondition

A finding only PARTLY covered by a merged issue carries **no heading marker** and a
`> **Partial:**` note naming what landed and what remains. Do not put `[#N]` on its
heading: processing skips `[#N]` findings outright, so a partial-coverage marker there
strands the uncovered remainder permanently.

That rule governs a finding whose partial coverage is still *open*. A `>
**Partial:**` note that is followed by a later terminal disposition — the case
in CH-43 and CH-54 — is historical: it records what the entry looked like
before the remainder was dispositioned, and the heading marker beside it states
that later disposition, not the partial one. Read the marker, not the note, for
an entry's current status.

> **Methodology note (corrected 2026-07-25).** The "unreferenced export" scans
> in batches 2-11 originally searched `src/`, `app/`, and `test/` only, and
> **omitted `test-headless/` — 124 files, the project's main test suite**. All
> counts below have been corrected, and every individual dead-code claim was
> re-verified against the full corpus. Three findings were wrong and have been
> fixed in place (CH-79, CH-109, and the `ghostTint` case dropped from
> batch 12). Corrected totals:
>
> | Area | First reported | Actual |
> |---|---:|---:|
> | `src/Engine` | 115 | **97** |
> | `src/World` | 230 | **194** |
> | `src/Unit` + `src/Combat` | 25 | **17** |
> | `World/Thread` + `Render` + `ZoomMap` | 14 | **11** |
> | `Sim`/`Power`/`Infection`/`Craft` | 6 | **0** |
> | `Building`/`Structure`/`Location`/`LootTable` | 7 | **3** |
>
> Anything claiming a function is unused has been checked against
> `src/ app/ test/ test-headless/` together.

## Status

- [x] CH-1. `EngineM`'s `ε` type parameter is dead weight on ~295 signatures — [#931]
- [x] CH-2. `EngineConfig` carries four fields that nothing reads — [#932]
- [x] CH-3. Vulkan reports the application name as "Vulkan Device Test" — [#933]
- [x] CH-4. `EngineEnv.inputThreadActiveRef` carries `gameTimeRef`'s haddock — [#934]
- [x] CH-5. Two record fields share one source line in `GraphicsState` — [#936, closed obsolete]
- [x] CH-6. Two of four `LogBackend` constructors are never constructed — [#942]
- [x] CH-7. Large dead surface in `Engine.Core.Log` / `Engine.Core.Log.Monad` — [#943]
- [x] CH-8. `logMessage` and `logThreadMessage` are duplicated verbatim — [#944]
- [x] CH-9. `extractCallSite`'s skip-list is an untested, order-sensitive trap — [#945]
- [x] CH-10. Three whole error domains are never constructed — [#946]
- [x] CH-11. `ErrorContext` is exported by field but not by name — [#1077]
- [x] CH-12. `Engine.Core.Var` is a production module used only by tests — [#947]
- [x] CH-13. `luaQueue` is misnamed relative to its sibling — [no-issue]
- [x] CH-14. Capability-record conventions are documented in three places — [#1031]
- [x] CH-15. Cross-cutting: 136 comments cite PR review rounds — [#949]
- [x] CH-16. Cross-cutting: 555 files repeat a global `LANGUAGE` pragma — [#950]
- [x] CH-17. `Show Font` drops its closing brace when a cleanup action is present — [#951]
- [x] CH-18. `AssetConfig` advertises three features that do not exist — [#952]
- [x] CH-19. `TimingState` is five-sixths write-only, and `targetFPS` is a lie — [#964]
- [x] CH-20. `Engine.Input.Thread`'s module haddock describes an API it doesn't expose — [#965]
- [x] CH-21. The module-budget guard has a subdirectory hole, and code already sits in it — [#967]
- [x] CH-22. The 500-line norm guards 6 Lua files while 30 exceed it — [no-issue]
- [x] CH-23. Oversized Haskell modules are now concentrated in `World/Save/` — [no-issue]
- [x] CH-24. `runGatedByCaptureLock` documents a bug that no longer exists — [#1078]
- [x] CH-25. `tools/` is 122 flat Python files — [no-issue]
- [x] CH-26. `CHANGELOG.md` has not been touched in 18 months — [no-issue]
- [x] CH-27. Minor defects worth folding into one cleanup issue — [no-issue]
- [x] CH-28. Four modules are not in `synarchy.cabal` — never compiled, never linted — [#972]
- [x] CH-29. Dead types kept alive by other dead types — [#973]
- [x] CH-30. The demo quad vertex buffer is uploaded to the GPU every boot and never drawn — [#974]
- [x] CH-31. The bindless texture limit is duplicated in five places with no check — [#975]
- [x] CH-32. `Bindless.hs`'s header claims 64× the real texture limit — [#976]
- [x] CH-33. `Texture.System`'s "legacy path" is a throw — [#977]
- [x] CH-34. `destroyBindlessTextureSystem` is exported, never called, and incomplete — [#978]
- [x] CH-35. The uniform buffer layout is hand-maintained across five declarations — [#1072]
- [x] CH-36. `fontFragmentShaderCode` is dead, and says so — [#980]
- [x] CH-37. `graphicsState` nested-record-update boilerplate, 50× — [#981]
- [x] CH-38. Naming inconsistencies in the graphics records — [#982]
- [x] CH-39. Minor graphics defects for one cleanup issue — [#983]
- [x] CH-40. `currentSaveVersion` carries a 296-line changelog for a superseded scheme — [#984]
- [x] CH-41. The 500-line module budget doesn't constrain function size — [no-issue]
- [x] CH-42. Three different facade idioms across nine sibling API domains — [no-issue]
- [x] CH-43. Five Lua API modules are 400-520 lines with no split, while `Save.hs` is 1090 — [no-issue]: #985 split `Save.hs`, remainder closed
- [x] CH-44. Two `Focus` modules, neither of which says which focus it means — [no-issue]
- [x] CH-45. `ScriptFunction` is a dead constructor with a silent-failure handler — [#992]
- [x] CH-46. The Lua API tree holds 57% of the engine's unrestricted-`EngineEnv` surface — [no-issue]
- [x] CH-47. `Engine.Core.Log`'s callsite skip-list has a matching hazard here — [no-issue]
- [x] CH-48. Minor Lua-tree defects for one cleanup issue — [#1059]
- [x] CH-49. Cross-cutting: normalise the enforced Unicode operators (owner decision recorded) — [#1005]
- [x] CH-50. `Engine.Graphics.Transform` is a fully dead module — [#1006]
- [x] CH-51. `Engine.Asset.Manager` is a 470-line abstraction used as an ID generator — [#1007]
- [x] CH-52. 14 verbatim copies of the same YAML loader — [#1008]
- [x] CH-53. `Engine.Asset.YamlTextures` loads no textures and holds three unrelated things — [#1009]
- [x] CH-54. 97 exported names in `src/Engine/` have no consumer outside their module — [#1083]
- [x] CH-55. `Engine.Core.Init`'s three exports have no callers — [no-issue]
- [x] CH-56. `Engine/Scene` has the `X.hs` + `X/` + `Types/X.hs` triple layout — [no-issue]
- [x] CH-57. Minor remaining-Engine defects for one cleanup issue — [#1011]
- [x] CH-58. `--seed`, `--worldSize`, and `--plates` are silently ignored outside `--dump` — [#1012]
- [x] CH-59. `allLayers` is not all layers — [#1016]
- [x] CH-60. The preview category list is duplicated as an error-message string — [#1019]
- [x] CH-61. Five boot modes hand-copy the same error-recovery block — [#1021]
- [x] CH-62. `shutdownEngine`'s five positional parameters are mutually swappable — [#1036]
- [x] CH-63. Three separate main loops — [#1022]
- [x] CH-64. `--dump` emits three fields that no documentation mentions — [#1040]
- [x] CH-65. `App/Dump.hs` hand-concatenates JSON — [#1058]
- [x] CH-66. Primitive-obsession in the dump signatures — [#1081]
- [x] CH-67. `parseRegion` silently substitutes a default for malformed input — [#1481]
- [x] CH-68. Two module haddocks enumerate the boot modes and both are stale — [#1084]
- [x] CH-69. Minor `app/` defects for one cleanup issue — [#1086]
- [x] CH-70. The save system's item enumeration is implemented three times — [#1090]
- [x] CH-71. `WorldPageId` has no accessor, so ten sites hand-write one — [#1091]
- [x] CH-72. Nine near-identical `Missing*Ref` types, misplaced in `Types.hs` — [no-issue]
- [x] CH-73. `serializeCodec` cannot express the migration the component system exists for — [#1093]
- [x] CH-74. `Component/Entities.hs` is five components in one 1139-line module — [no-issue]
- [x] CH-75. `tshow` is invented four times, while 570 sites don't use it — [#1099]
- [x] CH-76. Envelope compat is named after the epic's internal phase letters — [no-issue]
- [x] CH-77. `LuaComponentSpec` is a bare 4-tuple — [#1103]
- [x] CH-78. `Envelope.hs` is 860 lines beside an `Envelope/` directory — [no-issue]
- [x] CH-79. An abandoned river redesign is still compiled, plus a design doc that reads as current — [#1108]
- [x] CH-80. "River" logic lives in four unrelated namespaces — [#1109]
- [x] CH-81. `World.Fluids` and `World.Fluid.*` differ by one letter — [#1110]
- [x] CH-82. The per-tile fluid-surface fold is written five times in one file — [#1111]
- [x] CH-83. The river-flat surface rule is written four times, and its comment overstates its own coverage — [#1112]
- [x] CH-84. `floorDivCS` is hand-rolled five times, with an unreachable branch, next to a correct helper — [#1113]
- [x] CH-85. `moSurface` is always empty, its lookup can never succeed, and two comments say it drives lava placement — [#1114]
- [x] CH-86. `composeFluidMap`'s haddock documents a parameter it does not have — [#1115]
- [x] CH-87. 43 modules carry `-fprof-auto`, defeating the cabal's `-fprof-late` profiling strategy — [#1116]
- [x] CH-88. Four dead bindings that `Strict` actually evaluates — [#1117]
- [x] CH-89. Material IDs are a hardcoded Haskell table mirroring `data/materials/*.yaml` — [#1118]
- [x] CH-90. 194 unreferenced exports in `src/World/` — [#1119]
- [x] CH-91. Minor worldgen defects for one cleanup issue — [#1131]
- [x] CH-92. `baseTileW` / `baseTileH` are defined identically in eight modules — [#1132]
- [x] CH-93. `World.ZoomMap` is a facade that inverts its own dependency direction — [#1133]
- [x] CH-94. Cross-chunk render lookups don't wrap at the world seam, but the chunk map is keyed wrapped — [#1135]
- [x] CH-95. Two zoom namespaces with a real but unstated split — [#1222]
- [x] CH-96. `docs/history/README.md` justifies an archive with a false claim — [#1136]
- [x] CH-97. Duplicate module basenames across the render stack — [no-issue]
- [x] CH-98. A fifth dead binding in `BuildPixels.hs` (extends CH-88) — [#1137]
- [x] CH-99. Minor Thread/Render/ZoomMap defects for one cleanup issue — [#1138]
- [x] CH-100. The save-critical enums tell you to bump the wrong version, and CLAUDE.md agrees with them — [#1139]
- [x] CH-101. Two components store the same enum two different ways; only one is order-safe — [no-issue]
- [x] CH-102. The codebase's only `TODO` is a comment claiming TODOs exist — [#1144]
- [x] CH-103. `Unit.Types.Combat` holds anatomy, not combat — [no-issue]
- [x] CH-104. The append-only enum policy is unenforced, in a codebase full of enforcement — [#1145]
- [x] CH-105. Minor Unit/Combat defects for one cleanup issue — [#1146]
- [x] CH-106. Six worker threads hand-implement one identical lifecycle — [#1147]
- [x] CH-107. 22 directories exist solely to hold a single `Types.hs` — [no-issue]
- [x] CH-108. Power hardware is hardcoded in Haskell while 16 other content categories are YAML — [#1148]
- [x] CH-109. Nineteen lines of reasoning prove two functions are dead, and they are still there — [#1149]
- [x] CH-110. Minor Sim/Power/Infection/Craft defects for one cleanup issue — [no-issue]
- [x] CH-111. `applyFacingF` — the camera rotation — is defined three times, identically — [#1150]
- [x] CH-112. `validRelBounds` documents a validation it doesn't perform — [#1151]
- [x] CH-113. Quad vertex construction is written out longhand in eight places — [#1152]
- [x] CH-114. Minor Building/Structure/Location defects for one cleanup issue — [no-issue]
- [x] CH-115. The `synarchy-test-graphical` suite is built by CI but never run — [#1153]
- [x] CH-116. The four largest files in the project are test modules — [no-issue]
- [x] CH-117. Seven test modules bypass the shared engine harness — [no-issue]
- [x] CH-118. `test/` and `test-headless/` were absent from this audit's own tooling — [no-issue]
- [x] CH-119. Minor remaining-Haskell defects for one cleanup issue — [#1154]
- [x] CH-120. Five focus modules, and three have no module haddock at all — [#1155]
- [x] CH-121. `src/UI` is the densest concentration of review-round archaeology — [#949]
- [x] CH-122. Verified: the UI tree's "single source of truth" claims are true — [no-issue]
- [x] CH-123. Minor UI defects for one cleanup issue — [#1156]
- [x] CH-124. `truncateToWidth` has five divergent implementations, and users can see the difference — [#1157]
- [x] CH-125. `clamp` is defined 11 times; `formatGameTimeHMS` 4 times, identically — [#1158]
- [x] CH-126. `shell.wrapText` says "by character" and iterates by byte — [#1159]
- [x] CH-127. Four features are split across both a flat file and a same-named directory — [no-issue]
- [x] CH-128. Five Lua modules sit at exactly the 500-line cap — [no-issue]
- [x] CH-129. `probelib` is imported by 71 of 72 probes and then reimplemented — [#1160]
- [x] CH-130. The seven largest files in the project are all tests and tooling — [no-issue]
- [x] CH-131. `tools/` is 122 flat files that divide cleanly by role — [no-issue]
- [x] CH-132. Minor `tools/` defects for one cleanup issue — [no-issue]
- [x] CH-133. `player_events.md` (786 lines) is marked "ready to implement" for a system that shipped — [#1161]
- [x] CH-134. `blood_decals.md` (445 lines) is marked "design draft" for a shipped subsystem — [no-issue]
- [x] CH-135. Status markers are inconsistent, and two of the six that exist are wrong — [no-issue]
- [x] CH-136. Minor doc defects for one cleanup issue — [no-issue]
- [x] CH-137. Verified: four docs are accurate and worth using as the pattern — [no-issue]
- [x] CH-138. Every GitHub Actions dependency is pinned by mutable tag, not by SHA — [#1482]

---

## Batch 1 — `src/Engine/Core/` (swept 2026-07-25)

### [#931] CH-1. `EngineM`'s `ε` type parameter is dead weight on ~295 signatures
`Engine.Core.Monad` declares `newtype EngineM ε σ α` but the body hard-wires
`EngineEnv`:

```haskell
newtype EngineM ε σ α = EngineM
  { unEngineM ∷ EngineEnv → (Either EngineException α → IO σ) → IO σ }
```

`ε` appears in no field, no instance head that constrains it, and no call site
ever instantiates it to anything but a bound variable (the only concrete
mention anywhere is `EngineM' EngineEnv`, 5 places). CLAUDE.md already states
the design decision — "`EngineM` stays hard-wired to `MonadReader EngineEnv`
(no capability typeclass layer)" — so the parameter is not reserved for a
planned feature either.

Cost: every one of ~295 `EngineM ε σ α` / `EngineM' ε α` signatures in the
codebase carries a meaningless type variable, and the module's own haddock
documents it as "ε = environment tag", which is not true.

Fix: drop `ε`, or (if it must stay) document it as vestigial. Mechanical but
wide; good candidate for a single sweeping PR.

### [#932] CH-2. `EngineConfig` carries four fields that nothing reads
`windowWidth`, `windowHeight`, `enableVSync`, `enableDebug` have **zero** read
sites in `src/`, `app/`, or `test/`. They are set once in
`Engine.Core.Defaults.defaultEngineConfig` (800 / 600 / True / CPP-gated) and
never overwritten — `initializeEngineWith` assigns `engineConfig =
defaultEngineConfig` verbatim while the *real* window size comes from
`VideoConfig`/`windowSizeRef`.

This is an active trap: `windowWidth (engineConfig env)` reads as the window
width and always returns 800. `enableDebug` additionally reads as a live dev
toggle when it is a compile-time constant.

Fix: delete all four (and the `#ifdef DEVELOPMENT` block that feeds
`enableDebug`).

### [#933] CH-3. Vulkan reports the application name as "Vulkan Device Test"
`Engine.Core.Defaults.defaultGraphicsConfig` sets
`gcAppName = "Vulkan Device Test"`, and `Engine.Graphics.Vulkan.Instance`
passes it straight to `VkApplicationInfo.applicationName`. The shipped game
identifies itself to the Vulkan driver (and to any driver-side profile,
capture tool, or bug report) as a test scaffold. `defaultWindowConfig` in the
same file correctly uses `"Synarchy"`.

Also here: `gcWidth`/`gcHeight` are a second hardcoded 800x600 that nothing
reconciles with `EngineConfig`'s (see CH-2) or `VideoConfig`'s.

### [#934] CH-4. `EngineEnv.inputThreadActiveRef` carries `gameTimeRef`'s haddock
`Engine/Core/State.hs:308-322`. Two `-- ^` blocks are stacked on
`inputThreadActiveRef`; haddock concatenates them, so the rendered docs for
"has the input thread started" end with:

> Monotonic game-clock in seconds. Advances by real-tick dt only when
> `enginePausedRef` is False. […] Updated by Unit.Thread.unitLoop once per tick.

That paragraph belongs to `gameTimeRef` (declared four lines earlier at
`:304`), which is left completely undocumented. A pure doc-motion fix.

### [#936, closed obsolete] CH-5. Two record fields share one source line in `GraphicsState`
> **Closed as obsolete (2026-08-02):** #974 (CH-30) deleted the never-read
> `vertexBuffer` field outright, so the two-fields-on-one-line defect this
> finding described no longer exists — `msaaColorImage` is alone on its own
> line at `Engine/Core/State.hs:514`. #974 landed after this issue was filed
> and reviewed, so the fix arrived as a side effect of unrelated cleanup
> rather than of #936 itself.

`Engine/Core/State.hs:456`:

```haskell
  , msaaColorImage     ∷ Maybe (Vk.Image, Vk.DeviceMemory, Vk.ImageView)  , vertexBuffer       ∷ Maybe (Vk.Buffer, Vk.DeviceMemory)
```

`vertexBuffer` is invisible when skimming the record and unreachable by a
line-oriented grep for its declaration.

### [#942] CH-6. Two of four `LogBackend` constructors are never constructed
`LogToFile` and `LogMulti` had no construction site anywhere in `src/`,
`app/`, `test/`, or `test-headless/`. They carried live handling code in
`writeLogEntry`, `writeThreadLogEntry`, and `shutdownLogger`.

The originally reported count of three was wrong: `LogToCallback` **is**
constructed, by the headless suite, to capture structured `LogEntry` values
(`test-headless/Test/Headless/Core/LogMonad.hs`, and likewise in
`LoopStartup.hs`, `LogParity.hs`, and `Asset/YamlList.hs`). Only `LogToHandle`
and `LogToCallback` are live.

That dead code was also **wrong**: `writeThreadLogEntry`'s `LogMulti` branch
recursed into `writeLogEntry`, so a thread-log entry fanned out to multiple
backends would have been formatted with the non-thread formatter. Unreachable,
but the kind of defect that ships the moment someone adopts `LogMulti`.

Resolved in #942 by deleting `LogToFile` and `LogMulti` and keeping the two
constructors that have consumers.

### [#943] CH-7. Large dead surface in `Engine.Core.Log` / `Engine.Core.Log.Monad`
Exported, documented, zero call sites: `traceLog`, `logException`,
`getEnabledCategories`, `setCategoryLevel`, plus two closed wrapper chains —
`withTiming`→`withTimingFor`, and `logWarnSM`→`logWarnSFor`→`logWarnS`. Each
chain is internally connected but externally unreachable, so it had to be
removed whole rather than one name at a time.

Two names in the original list were wrong and must NOT be removed:
`logDebugS` is live through `logDebugSFor`/`logDebugSM`, and `logAndThrowFor`
is live through `logAndThrowM` — both have production consumers across
rendering, asset, window, and loop error paths. `withTiming`/`withTimingFor`
are genuinely dead #889 capability-migration primitives that no consumer was
ever narrowed onto.

Resolved in #943 by removing the four standalone helpers and the two dead
chains, keeping the live structured-debug and exception paths intact.

### [#944] CH-8. `logMessage` and `logThreadMessage` are duplicated verbatim
`Engine/Core/Log.hs:169-223`. The two functions are identical across 27 lines
except for the final `writeLogEntry` vs `writeThreadLogEntry` call. Same
duplication repeats one layer down in `Log/Format.hs` (`formatLogEntry` /
`formatThreadLogEntry`) and again across the eight
`logDebug`/`logThreadDebug`/… wrappers.

Fix: parameterise on the writer.

### [#945] CH-9. `extractCallSite`'s skip-list is an untested, order-sensitive trap
`Engine/Core/Log.hs:134-167`. Source-location attribution depends on a
hand-maintained list of *function name strings*; a rename or a new wrapper
silently misattributes every log line, and nothing tests it.

Worse, the list is internally inconsistent in a way that makes the obvious
"fix" a regression: `logMessage` is listed but `logThreadMessage` is not, and
`logThreadDebug`/`Info`/`Warn`/`Error` are absent. They happen to work today
only because they are the outermost frame (`dropWhile` stops immediately and
returns the same `SrcLoc` as the fallback). Adding `logThreadInfo` to the list
"for symmetry" would drop that frame, land on the unlisted `logThreadMessage`,
and start reporting `Log.hs` as the call site for every threaded log line.

Also: the `-- All internal: use most recent frame` comment on the fallback
branch is wrong. `fallback` is the head of the *reversed* list — the oldest /
outermost frame, which is precisely why it is the correct answer.

### [#946] CH-10. Three whole error domains are never constructed
`Engine.Core.Error.Exception` defines 49 constructors across 7 domains. 31 are
never constructed outside their own declaration, including three complete
domains:

| Domain | Tag uses | Dead constructors |
|---|---|---|
| `ResourceError` / `ExResource` | 0 | 6 of 6 |
| `StateError` / `ExState` | 0 | 4 of 4 |
| `LuaError` / `ExLua` | 0 | 8 of 8 |

`LuaError` is the striking one: the Lua subsystem is one of the largest in the
tree and models its errors some other way entirely, leaving an eight-case
taxonomy that reads as the canonical Lua error vocabulary and is inert.

Also dead here: `tryEngine` (0 uses), `TestError` (a test-only constructor in
a production error domain).

### [#1077] CH-11. `ErrorContext` is exported by field but not by name
`Engine.Core.Error.Exception`'s export list omitted the `ErrorContext` type
while exporting its accessor `contextCallStack`, and `EngineException(..)`
exposed `errorContext ∷ ErrorContext`. Downstream code could read the field
but not name its type in a signature.

Resolved in #1077 by sealing `ErrorContext`: the type, `contextCallStack`,
and the `errorContext` field selector are no longer exported —
`EngineException`'s export narrowed from the `(..)` wildcard to just its
constructor and the `errorType`/`errorMsg` selectors callers actually use.
The minor siblings are also resolved: `throwEngineException` and
`catchEngine`, pointless aliases for `throwError`/`catchError`, are gone (2
and 1 call sites now use the underlying `MonadError` methods directly), and
the `ExceptionType` constructor comments are aligned on one column.
`AssetError`'s haddock gap was already closed by #946, which removed the
undocumented `AssetFailedCleanup` constructor.

### [#947] CH-12. `Engine.Core.Var` is a production module used only by tests
`src/Engine/Core/Var.hs` exports a thin renaming of `Control.Concurrent.STM`
(`Var = TVar`, `newVar = newTVar`, …). Its only importers are
`test/Test/Engine/Core/Var.hs` (which tests it) and three Vulkan test modules.
No `src/` or `app/` module uses it. `dupVar` has exactly one use — the test
that exercises `dupVar`.

Fix: delete, or move under `test/`. Note the tests are testing STM itself.

### [no-issue] CH-13. `luaQueue` is misnamed relative to its sibling
> **Disposition:** No issue — `luaQueue` is intentionally a destination-named, multi-producer Lua inbox: input, world, main-render, unit/combat, and Lua threads enqueue `LuaMsg`, while only the opposite queue has a single Lua origin; renaming it `engineToLuaQueue` would misstate its source.

`EngineEnv` has `luaToEngineQueue` (Lua → engine) and `luaQueue` (engine →
Lua). The direction-neutral name for the directional queue makes call sites
ambiguous. `Engine.Core.Init` already knows the right name — it binds the
local as `engineToLuaQueue` and then assigns `luaQueue = engineToLuaQueue`.

Fix: rename the field to `engineToLuaQueue`.

### [#1031] CH-14. Capability-record conventions are documented in three places
`Engine/Core/Capability/Core.hs` opens with a 63-line module haddock stating
the conventions **every** capability record must follow (naming, one-way
projection, shared containers, no back-imports, no records ahead of need,
thread-private splits). The same rules are stated in CLAUDE.md's "Capability
records (#889)" paragraph and in `docs/engineenv_capability_inventory.md`,
which the haddock itself names as the authority.

Three copies drift. The rules belong in the inventory doc; `Core.hs` should
document `CoreCapability` and link out.

### [#949] CH-15. Cross-cutting: 136 comments cite PR review rounds
`grep -rniE "round [0-9]+ (review|of review)|review round [0-9]+" src app`
returns 136 hits, e.g. `-- ^ #745 review round 12: bumped ONLY by a
route-affecting…`, `-- (round 9 review, issue #763)`. `UI/` is the densest
(`UI/Manager/*`, `UI/Types.hs`, `UI/ControlActivation.hs`).

A review round is not a fact about the code — it is a fact about how the code
came to be, and it is unresolvable without pulling the PR. CLAUDE.md already
made this call for itself ("Deep per-issue history … was trimmed from this
file"); the same principle applies in source. The *invariant* should stay, the
*provenance* should go (an issue number alone is fine).

### [#950] CH-16. Cross-cutting: 555 files repeat a global `LANGUAGE` pragma
> **Note:** #950 itself is closed **not planned**, but the finding was not
> dropped — a single-PR cleanup touched ~695 files and GitHub's pull-request
> diff API refuses more than 300, so no reviewer could ever see it. #950 was
> closed in favour of three disjoint scoped issues on directory boundaries —
> #969 (`src/World/`), #970 (`src/Engine/`), #971 (everything else) — all three
> of which are merged. The marker stays `[#950]` because that is where this
> finding was filed; follow it to those three for the outcome.

`UnicodeSyntax` is in `common lang`'s `default-extensions`, imported by all
four cabal components — yet 555 modules re-declare it in a `{-# LANGUAGE #-}`
pragma. `OverloadedStrings` (also global) is re-declared in 66. Pure noise on
the first line of nearly every file; also actively misleading, since it
implies the extension is *not* on elsewhere.

---

## Batch 2 — `Engine/Loop`, `Engine/Input`, `Engine/Asset`, structure (swept 2026-07-25)

### [#951] CH-17. `Show Font` drops its closing brace when a cleanup action is present
`Engine/Asset/Types.hs:114`:

```haskell
<> ", fCleanup = " <> if isJust (fCleanup f) then "<present>" else "<absent> }"
```

The `" }"` is inside the *else* string, so any `Font` that has a cleanup
action — i.e. every loaded font — renders as `Font { … fCleanup = <present>`
with no terminating brace. A real (cosmetic) bug in a hand-written `Show`.

### [#952] CH-18. `AssetConfig` advertises three features that do not exist
`Engine.Asset.Types.AssetConfig` declares `acMaxTextureAtlases`,
`acPreloadAssets`, `acEnableHotReload` (plus, until #1007 removed it alongside
the shader asset model, `acMaxShaderPrograms`). It is constructed exactly once
— positionally, as `AssetConfig 100 True True` in `Engine.Core.Defaults` —
stored in `EngineState.assetConfig`, and **never read anywhere**. So the engine
appears to support asset preloading and hot reload and to enforce an atlas cap;
it does none of those.

Worse than plain dead code: it is a false capability advertisement, written
positionally so the three bare literals cannot even be matched to their meaning
without opening a second file.

Fix: delete `AssetConfig` and `EngineState.assetConfig`, or implement it.

### [#964] CH-19. `TimingState` is five-sixths write-only, and `targetFPS` is a lie
Only `deltaTime` is read outside `Engine/Loop/Timing.hs`. The other five
fields have zero *external* readers.

**Correction (2026-08-02):** an earlier version of this entry treated "no
external reader" as "dead" and proposed making all five fields local. That was
wrong — `frameCount`, `frameTimeAccum`, and `lastFrameTime` are read back on
the *next* call to `updateFrameTiming`, so they are cross-frame state that a
local binding cannot hold. Only two of the five were genuinely unused.

The finding stands in its narrower form. `targetFPS = 60.0` (set in
`Engine.Core.Defaults`) was read by **nothing** — the real frame cap comes from
`VideoConfig`'s `vcVSync`/`vcFrameLimit`. A field named `targetFPS` sitting in
the engine's timing state is the first place anyone will go to change the frame
rate, and changing it did nothing. `currentTime` was written every frame and
never read, always holding the same value as `lastFrameTime`.

`frameCount` was also misnamed: it resets to 0 every second, so it is "frames
since the last FPS sample", not a frame count — and it is typed `Word64` for a
value that never exceeds a few hundred. `frameTimeAccum` is that same window's
elapsed duration, not accumulated engine time.

Fixed by #964: `currentTime` and `targetFPS` deleted; `frameCount` and
`frameTimeAccum` renamed to `fpsWindowFrames`/`fpsWindowElapsed`; a haddock on
`TimingState` now records that the window fields reset every second and that
pacing belongs to `VideoConfig`. The oversized `Word64` was left alone
deliberately — retyping was outside that issue's scope.

### [#965] CH-20. `Engine.Input.Thread`'s module haddock describes an API it doesn't expose
The header says the #787 split moved logic into `Dispatch` plus the four
per-domain modules, and concludes: *"Both are re-exported here so the public
API is unchanged."*

The export list is `startInputThread, runInputLoop, processInputs,
processInput`. Only `Dispatch`'s two functions are re-exported;
`Engine.Input.Thread.Keyboard`/`.Char`/`.Mouse`/`.Scroll` are neither imported
nor re-exported. Anyone trusting the header will look for the per-domain entry
points here and not find them.

### [#967] CH-21. The module-budget guard has a subdirectory hole, and code already sits in it
`tools/haskell_module_budget.py` guards the #787 input split with the pattern
`src/Engine/Input/Thread/*.hs` at 500 lines. `Path.glob` does not cross
directory separators, so `src/Engine/Input/Thread/Mouse/Activation.hs` is
**not** checked.

That is not hypothetical: `Mouse.hs` is at **exactly 500 lines** — the cap —
and 57 further lines live in the unguarded `Mouse/` subdirectory. The next
overflow has an obvious, silent escape hatch.

Fix: use `**/*.hs` in `BUDGETS`, and add a self-test asserting the pattern
matches nested files.

### [no-issue] CH-22. The 500-line norm guards 6 Lua files while 30 exceed it
> **Disposition:** No issue — the 500-line Lua guard is an intentional per-split ratchet, documented as applying only to module families with an explicit split agreement; 33 unrelated scripts exceeding 500 lines do not violate that contract.

`tools/lua_module_budget.py` enforces 500 lines on six historical splits
(`debug`, `unit_resources`, `unit_ai`, `unit_info_v2`, `init`, `ui_manager`).
Meanwhile **30** Lua files exceed 500 lines and are unguarded, including the
largest scripts in the project:

| Lines | File |
|---:|---|
| 1399 | `scripts/ui/dropdown.lua` |
| 1321 | `scripts/hud.lua` |
| 1171 | `scripts/create_world_menu.lua` |
| 1167 | `scripts/build_tool.lua` |
| 1118 | `scripts/shell.lua` |
| 1063 | `scripts/settings_menu.lua` |
| 1058 | `scripts/combat_log.lua` |

The budget currently reads as "these six splits must not regress" rather than
"Lua modules stay under 500 lines", which is how CLAUDE.md presents it. Either
generalise the rule or state plainly that it is a per-split ratchet.

(`scripts/ui/dropdown.lua` at 1399 lines for one widget is a split candidate
in its own right.)

### [no-issue] CH-23. Oversized Haskell modules are now concentrated in `World/Save/`
> **Disposition:** No issue — this is a size count, not a defect. The 500-line limit is an explicit per-split ratchet (`tools/haskell_module_budget.py` guards three named splits) and no persistence module carries a split agreement, the same policy that closed CH-22 and CH-41. Its actionable content is owned elsewhere, not discarded: `Component/Entities.hs` by CH-74 ("the largest single win available against CH-23"), `Types.hs` by CH-72, `Envelope.hs` by CH-78, and `Lua/API/Save.hs` by #985. The residue (`Storage.hs` 819, `Component/WorldGen.hs` 830, `Component/Page.hs` 792, `Integrity.hs` 531) is size-only with no identified boundary defect. The table is also stale: 19 modules now exceed 500, not 17; `Types.hs` is 1102 after #984 archived its changelog, so it is neither the largest module in the tree (`Entities.hs` 1139, `Lua/API/Save.hs` 1126) nor growing; `WorldGen.hs`/`Page.hs` were already 830/792 at the sweep commit.
> **Correction (2026-08-03):** two of the numbers above have since moved and one pointer
> is dead. `Lua/API/Save.hs` is now **858** (not 1126) — #985's split landed — and
> `Types.hs` grew to **1146**, so it IS now the largest module in the tree, narrowly over
> `Entities.hs` (1139); `Storage.hs` is 821, not 819. The "19 modules exceed 500" count
> still holds. Separately, CH-72 closed `[no-issue]`, so `Types.hs`'s size is no longer
> owned by any finding. Neither change affects this disposition: size alone remains a
> count rather than a defect under the same per-split-ratchet policy, and `Types.hs`'s
> bulk is the nine-validator family #764 deliberately declined to rewrite.

The 2026-07-07 triage (`docs/history/haskell_large_file_submodule_triage_2026-07.md`)
found 43 modules over 500 lines. That is down to **17** — good progress — but
the remainder is no longer spread evenly:

| Lines | File |
|---:|---|
| 1316 | `src/World/Save/Types.hs` |
| 1139 | `src/World/Save/Component/Entities.hs` |
| 1090 | `src/Engine/Scripting/Lua/API/Save.hs` |
| 860 | `src/World/Save/Envelope.hs` |
| 819 | `src/World/Save/Storage.hs` |
| 660 | `src/World/Save/Component/Page.hs` |
| 616 | `src/World/Save/Component/WorldGen.hs` |

Eight of the seventeen are the persistence subsystem, which was rebuilt by the
#756-#768 overhaul *after* the triage and never split. Note `World/Save/Types.hs`
was on the original list at 726 lines, rated "Low-Med — serialization-sensitive,
split only with care"; it has since grown to 1316. It is now the largest module
in the tree and is growing under an explicit "be careful here" flag, which is
the condition under which files quietly become unmaintainable.

The other five are `Engine/Scripting/Lua/API/*` (`Units/Inventory` 719,
`UI/Property` 645, `Units/Stats` 641, `Blood` 519, `Power` 516) — the same
category the triage rated "High feasibility".

### [#1078] CH-24. `runGatedByCaptureLock` documents a bug that no longer exists
> **Note:** Re-verified before the fix and relocated — the haddock was
> `Engine/Loop/Mode.hs:140-182` by then. Two count corrections: it was 43 lines (not 37)
> and the superseded-attempt narration was 16 lines (not ~25); #1022 added an accurate
> headless paragraph, and its move commit — not #949 — is what dropped the provenance
> phrase. #1078 scoped this as a REFRAMING, not a deletion: the "a point-in-time
> `captureLocked` pre-check is insufficient" constraint is load-bearing and had to survive
> in present tense, or the race is easy to reintroduce.

`Engine/Loop.hs:69-105` — a 37-line haddock in which ~25 lines narrated a
*previous failed attempt*: "The first attempt at this fix only READ
`captureLocked` as a point-in-time pre-check … but this thread was not a real
`SaveOwner` at all, so nothing ever waited for it …".

The durable content was about four lines (this thread is a real `SaveRender`
owner; it acknowledges unconditionally; `acknowledgeCurrent` no-ops when
`SaveRender` isn't in the owner set). The rest described code that does not
exist, which every future reader had to read and then discard. Worst instance of
CH-15.

Resolved in #1078 by restating the narration as a present-tense invariant: the
gate participates as a real `SaveRender` owner because a thread that only reads
`captureLocked` is not a `SaveOwner`, so `waitForOwners` has nothing to wait for
on its behalf. The rewrite states only what the protocol establishes — the
barrier waits for this thread's required quiescence acknowledgments before
proceeding — and explicitly declines the removed text's stronger claim that
owner participation excludes every interleaving before `reachSnapshot`. The
`Unit.Thread`, `waitForOwners`, and `publishStagedSession` cross-references and
the unconditional-`acknowledgeCurrent` rationale are retained; the other three
paragraphs of the haddock are unchanged.

### [no-issue] CH-25. `tools/` is 122 flat Python files
> **Disposition:** No issue — the role partition already exists in the filenames (`*_probe.py`/`*_audit.py`/`*_check.py`/`test_*.py`), which is why the finding could partition all 127 files "without ambiguity" from names alone; `ls tools/*_probe.py` gives the same grouping `tools/probes/` would, and directories would make the suffixes redundant. The two navigation surfaces the finding cites as evidence are the deliberate answer: `tools/README.md` carries a curated per-probe table (Probe/Gates/Boot/Purpose) and CLAUDE.md declares `tools/ci_probes.py --status` authoritative over any prose list. The "one atomic sweep" is also not achievable: 124 distinct `tools/*.py` paths are cited at 407 sites outside `tools/` (persistence_state_inventory 137, persistence_contract 36, CLAUDE.md 23, CI workflow, Makefile/ci-local.sh) and 385 inside it, and part of the invocation surface lives outside the repository (`~/.codex/rules/default.rules`, agent memory) where a PR cannot update it — a stale path fails as `No such file` exactly when an agent runs a gate. No defect behind the layout; same discretionary category as CH-22/CH-41/CH-42. Counts are also stale: 127 flat `.py` files and 76 probes today, not 122/74. CH-131 restates this finding and needs the matching disposition.

No subdirectories except `playtest/` and `baselines/`. The 122 files divide
cleanly by role — 74 `*_probe.py`, 10 `*_audit.py`, 6 `*_check.py`, 14
`test_*.py`, 22 reports/utilities — and `tools/README.md` plus
`tools/ci_probes.py` already exist to navigate what a directory layout would
make self-evident.

Proposed: `tools/probes/`, `tools/audits/`, `tools/reports/`, `tools/tests/`,
with the handful of shared helpers (`probelib.py`, `persistence_snapshot.py`)
staying at the top. Note this touches every `python3 tools/x_probe.py`
invocation in CLAUDE.md, CI, and the skills, so it needs to be done in one
sweep.

### [no-issue] CH-26. `CHANGELOG.md` has not been touched in 18 months
> **Disposition:** No issue — `CHANGELOG.md` records the sole `0.1.0.0` package version; the repository has no subsequent version, tag, GitHub release, or release process that would require a new entry.

Its entire content:

```
## 0.1.0.0 -- 1-22-2025
- First version. Released on an unsuspecting world.
- rendering of multiple textured sprites
- input handling
```

Since then: worldgen, hydrology, geology, combat, crafting, power, farming,
persistence overhaul, UI system, ~900 merged PRs. The file now
*misinforms* — it is the one place a newcomer looks for "what does this do"
and it says "sprites and input".

Fix: delete it, or regenerate from merged PRs and keep it current.

### [no-issue] CH-27. Minor defects worth folding into one cleanup issue
> **Disposition:** No issue — this is not one reviewable cleanup: 25 (not 29) Engine modules lack explicit export lists, the VSync-off compensation has no measured fault, and the remaining changes are harmless local cleanups.

- `Engine/Loop.hs:30` — `_state ← gets graphicsState` in `mainLoop`: an unused
  binding that reads engine state on every tick and discards it.
- `Engine/Loop.hs:127` — `GLFWError "handleEngineRunning: "` — the error
  payload is a bare function-name prefix with a trailing `": "` and no
  message; the real message is passed as a separate argument.
- `Engine/Loop/Timing.hs:24` — `-- Get video config (cache the read)` above a
  plain `readIORef` that caches nothing.
- `Engine/Loop/Timing.hs:55` — `compensatedTarget = targetFrameTime - 0.0012`,
  an unexplained 1.2 ms magic constant in the frame limiter.
- `Engine/Input/Types.hs:18` — `inpPendingUIClick ∷ Map GLFW.MouseButton
  (Text, Text, Double, Double)`; the four components' meanings exist only in
  prose. Should be a named record.
- `Engine/Asset/Types.hs:1-3` — `TypeApplications`, `AllowAmbiguousTypes`, and
  `ScopedTypeVariables` pragmas on a module with no type applications, no
  `forall`, and no ambiguous types (`ScopedTypeVariables` is also implied by
  GHC2024). `AllowAmbiguousTypes` in particular suppresses a useful error for
  no reason.
- 29 modules have no export list, including the four largest engine modules
  (`Engine/Core/State.hs`, `Engine/Input/Types.hs`,
  `Engine/Scripting/Lua/Types.hs`, `Engine/Scene/Render.hs`). Everything is
  public, so nothing can be refactored without a whole-tree grep.

---

## Batch 3 — `src/Engine/Graphics/` (swept 2026-07-25)

No module here exceeds 500 lines; the problems are dead modules, duplicated
constants, and comments that contradict the code beside them.

### [#972] CH-28. Four modules are not in `synarchy.cabal` — never compiled, never linted
`synarchy.cabal` uses an explicit module list. These `src/` modules are absent
from it, so GHC never compiles them, `-Wall -Werror` never sees them, and
`cabal sdist` would ship a broken tarball:

| Module | Lines | Status |
|---|---:|---|
| `Engine.Graphics.Vulkan.Types.Core` | 17 | unreferenced |
| `Engine.Graphics.Vulkan.Types.Font` | 16 | unreferenced |
| `World.Log` | 145 | unreferenced |
| `World.Hydrology.Log` | 174 | imported only by `World.Log` |

(`UPrelude` is listed; it was a false positive of the naive check.)

Proof they are not compiled: `Types/Core.hs` imports
`Engine.Graphics.Vulkan.Capability (TextureSystemCapability)` and never uses
it — an unused import that `-Wall -Werror` would reject on sight. Last touched
2026-01-30.

`World.Log` is the substantial one: a 145-line "unified world logger" facade
(`WorldLogger`, `WorldLogDest`, `WorldVerbosity`, `logWorldGen`,
`logGeoTimeline`, `logHydrology`, `logWeather`) that nothing imports, which in
turn is the only importer of the 174-line `World.Hydrology.Log`. 319 lines of
a logging subsystem that has never been part of the build.

Fix: delete all four. Then add a CI check that every `src/**/*.hs` appears in
the cabal module list — this class of rot is invisible otherwise.

### [#973] CH-29. Dead types kept alive by other dead types
`Engine.Graphics.Vulkan.Types.SyncObjects` carries this haddock:

> LEGACY: unused by the render loop. […] Type kept only because
> `Engine.Graphics.Types.vsSyncObjects` still references it.

`vsSyncObjects` is a field of `VulkanState` — and `VulkanState` has **zero**
uses anywhere outside its own declaration. So a type documented as retained
for one consumer is retained for a consumer that is itself dead. Deleting
`VulkanState` deletes the only reason `SyncObjects` exists.

Also zero uses in the same two files: `VulkanExtensions`, `VulkanLayers`,
`VulkanDescriptorInfo`. And `Engine.Graphics.Vulkan.Types.Font.FontState` /
`Types.Core.VulkanCore` (CH-28) duplicate field sets that live inline in
`GraphicsState` — they were the abstraction that never landed.

### [#974] CH-30. The demo quad vertex buffer is uploaded to the GPU every boot and never drawn
`Engine.Graphics.Vulkan.Vertex.quadVertices` is 12 hardcoded vertices —
"Two side-by-side quads with different atlas IDs … (unused demo geometry)",
per its own comment. `Engine/Graphics/Vulkan/Init.hs:202` calls
`createVertexBuffer` on it at every boot: allocates a staging buffer, maps it,
pokes the 12 vertices, copies to a device-local buffer, and stores the result
in `GraphicsState.vertexBuffer`.

**Nothing ever reads `vertexBuffer`.** The only other references are the field
declaration and `= Nothing` in `Defaults`. So every session carries a
permanent GPU allocation of tutorial geometry that is never bound and never
drawn.

Compounding it: the `vertexBuffer` field is the one crammed onto
`msaaColorImage`'s line (CH-5), so a grep for its declaration doesn't find it.

Fix: delete `quadVertices`, `createVertexBuffer`, the `Init.hs` call, and the
`GraphicsState.vertexBuffer` field.

### [#975] CH-31. The bindless texture limit is duplicated in five places with no check
`16384` must agree across:

1. `Bindless.hs:47` — `bcMaxTextures = 16384`
2. `Texture/System.hs:36` — `min 16384 (min maxSlots …)` (bare literal, no comment)
3. `Init.hs:208` — `tscMaxTextures = 16384`
4. `ShaderCode.hs:201` — `uniform sampler2D textures[16384]`
5. `ShaderCode.hs:370` — same, in the UI fragment shader

`65536` (`handleSlotTableSize`) is duplicated across three: `Bindless.hs:68`
and `HANDLE_TABLE_SIZE` in both fragment shaders (`ShaderCode.hs:223, 374`).

The comments say "MUST match shader" / "MUST match `HANDLE_TABLE_SIZE`" — and
nothing enforces it. A mismatch is not a build error; it is out-of-bounds
descriptor indexing at runtime. They agree today.

Fix: one Haskell constant, interpolated into the GLSL (the shaders are
`QuasiQuotes` strings already), or a test asserting the shader source contains
the Haskell values.

### [#976] CH-32. `Bindless.hs`'s header claims 64× the real texture limit
Line 1-2:

```haskell
-- | Bindless texture system using UPDATE_AFTER_BIND descriptors
-- This enables up to 1 million texture slots on MoltenVK/Metal
```

45 lines later, in the same file: `bcMaxTextures = 16384 -- Must match shader`.
The header describes the technique's theoretical ceiling as if it were the
system's capacity. Anyone sizing an asset budget off the module header is off
by 64×.

### [#977] CH-33. `Texture.System`'s "legacy path" is a throw
Module header: *"Unified texture system that handles both bindless and legacy
paths."* The legacy branch in full:

```haskell
    _ → do
      logInfoM CatTexture "BINDLESS TEXTURES NOT SUPPORTED - LEGACY SYSTEM BROKEN!!!"
      logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
        "Legacy texture system is not implemented."
```

There is no legacy path. Consequences:

- The header is wrong — the module handles exactly one path.
- `TextureSystemConfig.tscForceLegacy` ("Force legacy path (for testing)") is a
  config knob whose only effect is to force a throw. Set `False` at its one
  call site.
- The shouty `!!!` message is logged at *info* and immediately followed by a
  throw carrying a better message — two log lines for one condition.
- `loadTexture` takes a `_filterMode` parameter it ignores (correctly — atlases
  share the global sampler now), still present in the signature.

### [#978] CH-34. `destroyBindlessTextureSystem` is exported, never called, and incomplete
It releases the shared sampler and destroys the descriptor pool and layout. It
does **not** touch `btsUndefinedTexture` (image + view + memory),
`btsHandleSlotBuffer`/`btsHandleSlotMemory` (a persistently-mapped 256 KB
storage buffer), or any registered image view.

That is survivable only because those are `allocResource`-managed and freed at
process exit — i.e. the function's name promises a teardown that the codebase
actually performs somewhere else entirely. It has no call sites, so today it is
dead; the day someone wires up device-loss recovery it is a leak.

Fix: delete it, or complete it and document that it must run before the
`allocResource` scope unwinds.

### [#1072] CH-35. The uniform buffer layout is hand-maintained across five declarations
`UniformBufferObject` is declared in:

- `Vulkan/Types.hs` — the record (14 fields)
- `Vulkan/Types.hs` — a hand-written `Storable` instance with literal byte
  offsets (`+ 4`, `+ 8`, … `+ 32`) and `sizeOf _ = 5 * sizeOf (M44 Float) + 36`
- `Vulkan/Types.hs` — a 20-line comment block listing every field's offset
- `ShaderCode.hs` ×4 — inline GLSL `uniform UniformBufferObject { … }` blocks,
  which are **not identical**: `bindlessVertexShaderCode` declares 14 members,
  the other three declare 12 (they stop before `defaultFaceMapSlot`).

Adding one float means eight coordinated edits — record, `sizeOf`'s magic `36`,
a `peek` line, a `poke` line plus its 14-argument pattern, the offset comment,
and whichever GLSL blocks need it. Inserting a field *in the middle* silently
corrupts every shader that declares the shorter prefix, with no build error.

(Also: `alignment _ = 16` while `sizeOf` is 356, which is not a multiple of 16.
Harmless for a single instance, wrong for an array.)

### [#980] CH-36. `fontFragmentShaderCode` is dead, and says so
`-- | Legacy font fragment shader (non-SDF, kept for compatibility)` — zero
call sites. Compatibility with nothing. The SDF shader is the only one wired
up.

### [#981] CH-37. `graphicsState` nested-record-update boilerplate, 50×
```haskell
modify $ \s → s { graphicsState = (graphicsState s) { vulkanRenderPass = Just renderPass } }
```
appears **50 times** across 16 modules (18× in `Vulkan/Init.hs` alone, 12× in
`Recreate.hs`). No `modifyGraphicsState ∷ (GraphicsState → GraphicsState) →
EngineM ε σ ()` helper exists. Each site would collapse to one line.

### [#982] CH-38. Naming inconsistencies in the graphics records
- **`vc` prefix collision.** `VulkanCore`'s fields are `vcInstance`,
  `vcDevice`, … while `VideoConfig`'s are `vcWidth`, `vcVSync`,
  `vcWindowMode`, … Two records in the same subsystem sharing a prefix, with
  `DuplicateRecordFields` on globally.
- **`FontState.pendingInstanceBuffers`** is the one field in its record with
  no `fs` prefix — and it collides with `GraphicsState.pendingInstanceBuffers`.
- **`SwapchainSupportDetails`** uses bare `capabilities`, `formats`,
  `presentModes` while every other record in `Engine/Graphics/Types.hs` uses a
  prefix. `DevQueues` likewise (`graphicsQueue`, `presentQueue`, …). These are
  maximally generic names in a globally-shared field namespace.
- **`BufferUtils`** — a `*Utils` module name that says nothing about content;
  it sits beside `Buffer.hs`, which does something different.

### [#983] CH-39. Minor graphics defects for one cleanup issue
- `Bindless.hs:229` — `allocateBindlessDescriptorSet` takes `_config` and
  ignores it; every caller threads a `config` through for nothing.
- `Bindless.hs:421` — `Nothing → pure ()  -- shouldn't happen` silently keeps
  the wrong sampler if `btsHandleMap` and `btsImageViews` ever desync. Should
  log.
- `Bindless.hs:431` / `System.hs:71` — both return slot `0` to mean "lookup
  failed", which is also the real slot of the undefined texture. Documented in
  neither signature.
- `Vertex.hs:94-131` — six attribute-description comments are written `-- ^ TexCoord`
  inside a list expression. `-- ^` is haddock's "documents the *preceding*
  item"; there is no haddock inside expressions, and the first entry
  (`-- Position`) correctly omits it. Six stray carets.
- `Vertex.hs:81` — `stride = 48` plus offsets `0/8/16/32/36/40/44` must match
  `Vertex`'s `Storable` instance in `Types/Vertex.hs` by hand.
- `Init.hs:234` — `bindlessTexLayout = btsDescriptorLayout texSystem` reads
  from the pre-`createDefaultFaceMap` copy while `texSystemWithFaceMap` is the
  live one. Same value today because the layout is immutable; silently wrong
  the day it isn't.
- 399 lines in `src/` + `app/` have trailing whitespace (369 of them under
  `src/Engine/`).

---

## Batch 4 — `src/Engine/Scripting/` (swept 2026-07-25)

128 modules, 27k lines — the largest subsystem. Its problems are structural
inconsistency rather than dead code: three facade idioms, uncapped function
size, and one comment block that is 22% of the tree's largest file.

### [#984] CH-40. `currentSaveVersion` carries a 296-line changelog for a superseded scheme
`src/World/Save/Types.hs` — one `Int` constant with **296 lines of comment**
attached (70 lines of leading haddock + 226 lines of trailing right-hand-side
comment), documenting ~65 historical save versions back to v2. That is **22% of
the 1317-line file**, and the single reason `World/Save/Types.hs` is the
largest module in the tree (CH-23).

```haskell
currentSaveVersion ∷ Int
currentSaveVersion = 91  -- v91 (#761, save-overhaul B3): 'sdLuaModules'
                         -- removed — Lua-owned state no longer rides
                         -- through 'SaveData' at all; …
                         -- v90 (#759, save-overhaul B1): …
                         -- v89: WorldGenParams gains trailing …
                         [… 220 more lines …]
```

The changelog also documents a scheme that no longer governs compatibility.
CLAUDE.md states plainly:

> `currentSaveVersion` now versions only the transitional in-memory load
> bridge (`SaveData`) and is bumped freely — don't trust any number written in
> docs. Component evolution = per-component schema version bumps …

So the file's largest artefact is a growing changelog for a global version
number that was replaced by per-component versioning. `docs/save_compat/` and
`tools/save_compat_audit.py` already exist as the real home.

Fix: move the history to `docs/save_compat/`, leave two lines at the constant.

### [no-issue] CH-41. The 500-line module budget doesn't constrain function size
> **Disposition:** No issue — `tools/haskell_module_budget.py` is a per-split guard (three named splits held to their agreed shape), never a general size policy, so it was never meant to constrain function size; a tree-wide function-length gate would fail on ~96 definitions at once with no defect behind any of them, which the expedition-arc scope rule excludes as discretionary, and the repo already ran the module-granularity version of this program (#550-#588, closed). The counts also overstate: ~96/29/10 definitions exceed 100/200/300 lines (not 124/30/16), and three named offenders are misattributed — `World/Magma/Pool.hs`'s `rimJitter` is a two-line constant, `knownEntitiesFromSaveData` is 46 lines (not 265), `callSaveModules0` is 30 (not 194).

124 top-level definitions in `src/` exceed 100 lines; 30 exceed 200; 16 exceed
300. Worst offenders:

| Lines | Definition |
|---:|---|
| 450 | `Engine/Input/Thread/Mouse.hs:52` `dispatchMouseEvent` |
| 419 | `World/Generate/Chunk.hs:84` `generateChunk` |
| 402 | `World/Render/CursorQuads.hs:42` `renderWorldCursorQuads` |
| 389 | `World/Render/Quads.hs:41` `renderWorldQuads` |
| 371 | `Combat/Wounds/Tick.hs:73` `tickAllWounds` |
| 369 | `World/Weather/Generate/ClimateBuilder.hs:20` `buildClimateFromOceanSet` |
| 360 | `World/Generate/Timeline.hs:32` `applyTimelineChunk` |
| 327 | `Combat/Resolution.hs:118` `resolveAttack` |
| 318 | `Engine/Scripting/Lua/Thread/Dispatch.hs:49` `processLuaMsg` |
| 311 | `World/Magma/Pool.hs:75` `rimJitter` |

`dispatchMouseEvent` is the sharpest illustration: it lives in a file that sits
at *exactly* the 500-line budget (CH-21), and 450 of those lines are one
function. The module budget is satisfied while the actual reviewability problem
is untouched.

In the Lua tree specifically, `API/Save.hs` is 1090 lines across just 13
top-level definitions — `knownEntitiesFromSaveData` alone is 265 lines and
`callSaveModules0` is 194.

Fix: add a function-length guard alongside the module guard, or file the top
~16 as individual decomposition issues.

### [no-issue] CH-42. Three different facade idioms across nine sibling API domains
> **Disposition:** No issue — the two-idiom split is principled and documented rather than drift: the four hand-listed facades preserve their pre-split export lists verbatim per the closed split program's stability requirement (#565 and siblings), and each hand-listing narrows a surface a blanket `module` re-export would widen. Zero staleness exists today — all 151 hand-listed names (Equipment 10, Items 14, WorldQuery 24 — not 22 — Units 103) match their submodules' Lua-facing `*Fn` exports exactly. The silent hazard doesn't exist either: every `Register/*.hs` consumer imports its facade unqualified with no import list, so an unlisted function is a `Variable not in scope` build error at the registration site. `Register/` is not a third idiom but a non-facade — `Engine.Scripting.Lua.API` sequences its 13 `registerXAPI` entry points and has no cross-domain surface to re-export. Only Items (no internals) and WorldQuery (internals already isolated in `.Lookup`) are convertible as prescribed; Units and Equipment would need 7 shared helpers moved into new internal modules — churn inside the subsystem the live #537/#889-#899 capability epic is rewriting, with no observable change, which the expedition-arc scope rule excludes as discretionary.

Every `API/<Domain>.hs` facade does the same job — re-export its submodules —
in one of three ways:

| Idiom | Domains |
|---|---|
| `module X` re-exports | Buildings, Craft, Forage, UI, World |
| Hand-listed export names | Equipment (10), Items (14), WorldQuery (22), **Units (103)** |
| No facade at all | `Register/` |

The hand-listed variant is a live maintenance hazard: add a function to
`API/Units/Combat.hs` and it is silently absent from the `Units` facade until
someone edits the 103-name list. 149 export names are maintained by hand this
way.

`WorldQuery.hs`'s haddock even documents why it diverged (to keep
`WorldQuery.Lookup` internal) — which is achievable with `module` re-exports
by simply not re-exporting that one.

Fix: pick one idiom (module re-exports, with internal submodules omitted) and
apply it to all nine.

### [no-issue] CH-43. Five Lua API modules are 400-520 lines with no split, while `Save.hs` is 1090
> **Disposition:** No issue — `Save.hs` was split by #985 (merged, 1090 → 858, four `API/Save/*` submodules); the remainder is a size complaint with no policy behind it. `tools/haskell_module_budget.py`'s `BUDGETS` lists three module families and none of these five; CLAUDE.md states the 500-line limits "are per-split ratchets… not a tree-wide size policy." Re-measured 2026-08-03: three of the five are already under 500 (YamlTextures 435, Construct 407, InputInject 402) and only Blood 538 and Power 521 exceed it. #985's actual driver does not transfer either — it reduced code under the save path's PERMANENT full-access exception, and none of these five holds unrestricted access (Blood/Power/YamlTextures/InputInject import `Engine.Core.State` narrowly; Construct doesn't import `EngineEnv` at all). The testability argument is unproven: no test imports #985's own `API/Save/{Bridge,Config,Integrity,Page}.hs`. Both Blood (a contiguous `EngineEnv`-free tail at `:396-538`) and Power (~6 scattered free helpers incl. a generic `insertAt`) remain extractable if a tree-wide size policy is ever adopted — that is a separate, larger decision, not this finding.

> **Partial:** #985 (merged) extracted only `API/Save.hs`'s `EngineEnv`-free
> definitions into `API/Save/{Bridge,Config,Integrity,Page}.hs`, leaving the
> facade at 859 lines; its Out of scope defers `API/Blood.hs`, `API/Power.hs`,
> `API/YamlTextures.hs`, `API/Construct.hs`, and `API/InputInject.hs`, and its
> approved issue review requires this entry stay unchecked until those are
> dispositioned. **Historical (superseded 2026-08-06):** that condition has
> since been met — the remainder was dispositioned `[no-issue]` below, which is
> why the heading now carries a terminal marker and the entry is checked. Kept
> for the #985 scope facts it records, not as an active partial finding.

Nine domains were split into facade + subdirectory. These were not:

| Lines | Module |
|---:|---|
| 1090 | `API/Save.hs` |
| 519 | `API/Blood.hs` |
| 516 | `API/Power.hs` |
| 433 | `API/YamlTextures.hs` |
| 407 | `API/Construct.hs` |
| 402 | `API/InputInject.hs` |

`Save.hs` is more than twice the next largest module in the tree and the
obvious next split (it already partitions cleanly: save listing/status, the
save path, the load path, integrity/`KnownEntities`, and the Lua save-module
bridge).

### [no-issue] CH-44. Two `Focus` modules, neither of which says which focus it means
> **Disposition:** No issue — CH-120 states it "supersedes and widens CH-44", and its fix subsumes this one verbatim (the same `Engine/Scripting/Lua/API/ShellFocus.hs` rename, plus `UI/ShellFocus.hs` and a system-naming haddock on all five focus modules); a separate issue would fragment one rename across two PRs over the same files. Re-verified today: `UI/Focus.hs`, `UI/Manager/Focus.hs`, and `API/Focus.hs` still open straight into `module … where`, and `API/UI/Focus.hs`'s header is still the ambiguous "Lua bindings for keyboard/input focus management".

`Engine.Scripting.Lua.API.Focus` and `Engine.Scripting.Lua.API.UI.Focus` are
indistinguishable by name and bind **two genuinely different focus systems**:

- `API/Focus.hs` → `UI.Focus.FocusManager` via `focusManagerRef`, registered
  onto the Lua `engine` table (`engine.registerFocusable`, `requestFocus`,
  `releaseFocus`, `getFocusId`). Its only consumer is `scripts/shell.lua` —
  this is *debug-console/shell text focus*.
- `API/UI/Focus.hs` → `UI.Manager`'s `upmTextFocus`/`upmControlFocus`,
  registered onto the `UI` table. This is the *game UI* focus system CLAUDE.md
  documents at length (#745).

`Engine/Input/Thread/Keyboard.hs:97` already documents the priority order
("1. FocusManager (focusManagerRef) — shell/console text input"), so the
distinction is known — it just isn't reflected in the module names or their
haddocks. `API/Focus.hs` has **no module haddock at all**; `API/UI/Focus.hs`
says "Lua bindings for keyboard/input focus management", which describes both
equally well.

Fix: rename to `API/ShellFocus.hs` (or `API/Focus/Shell.hs`) and document the
two-system split once, in both.

### [#992] CH-45. `ScriptFunction` is a dead constructor with a silent-failure handler
`Engine.Scripting.Types.ScriptValue` has six constructors. `ScriptFunction
Dynamic` is **never constructed** anywhere, exists only to force a
`Data.Dynamic` dependency, and its handler silently drops the value:

```haskell
    ScriptFunction _ → Lua.pushnil
```

The doc says "'ScriptFunction' is still nil since 'Dynamic' has no Lua
representation" — "still" implying a deferred fix that has no consumer waiting
for it. If it were ever constructed, the argument would vanish silently rather
than error.

`ScriptBool` (1 use) and `ScriptNil`/`ScriptTable` (2 each) are thin but live.

### [no-issue] CH-46. The Lua API tree holds 57% of the engine's unrestricted-`EngineEnv` surface
> **Disposition:** No issue — the premise is false by the repository's own authoritative measure. `tools/engine_env_capability_audit.py`'s live scan (the CI-enforced definition: `EngineEnv(..)` or a bare `Engine.Core.State` import under `src/`+`app/`) finds **30** unrestricted importers, **7** under `Engine/Scripting` — 23%, not 57% of 49. Five of the seven (`Lua.Thread`, `.Thread.Dispatch`, `.Thread.Console`, `.Message`, `.API.Save`) are §6.1 permanent allowlist entries, which #899 records as orchestration boundaries that are "not migration targets". The real Lua-tree residue is two modules, and the migration issues already point at both: #894 (E5b) names `Engine.Scripting.Lua.API.Structure` as one of exactly four §6.2 remainder modules it migrates, and #899 (E8) empties the temporary ceiling entirely, covering `Engine.Scripting.Lua.API.Log`. The whole remaining §6.2 ceiling tree-wide is 6 modules. The "93 modules import `Engine.Core.State`" figure is accurate but is the wrong denominator — importing that module for the `EngineEnv` type alone is the narrowed shape the epic migrates *to*. This is not staleness: the sweep-date commit and HEAD have identical counts (202/93/18), so 49/28 was a measurement error when written.

The #889-#899 capability epic ratchets modules importing
`Engine.Core.State (EngineEnv(..))`. Of the 49 such modules in all of `src/` +
`app/`, **28 are under `Engine/Scripting/`** — 28 of the tree's 93 modules that
import `Engine.Core.State` at all.

Not a defect on its own, but it locates the epic's remaining work: the Lua API
binding layer is where narrowing pays off most, and it is not currently where
the migration issues point.

### [no-issue] CH-47. `Engine.Core.Log`'s callsite skip-list has a matching hazard here
> **Disposition:** No issue — the check CH-47 asks for was run and came back clean. `Engine/Scripting/Lua/API/Internal.hs` is 42 lines and performs no source-location reporting at all: it imports no logging module, carries no `HasCallStack`, and its `Catch.catch` handler pushes a Lua string and calls `Lua.error`, identifying the function by the `name` ByteString passed explicitly at registration — the opposite of CH-9's implicit dependence on wrapper names appearing as call-stack frames. The one place this path is logged is downstream at `Engine/Scripting/Lua/Script.hs:77` (`logWarn logger CatLua $ "Lua error in " <> funcName <> "(): " <> msg`), plain direct logging that #945 (CH-9's issue) must preserve under requirement 1 and whose skip-list mechanism its requirements 2-3 remove outright. No separate work exists at this choke point.

(Cross-reference to CH-9.) `Engine.Scripting.Lua.API.Internal.registerLuaFunction`
is the single choke point through which every Lua-facing Haskell function is
registered, and it wraps each in a `Catch.catch` handler. Any source-location
reporting through this path inherits CH-9's fragility. Worth checking together.

### [#1059] CH-48. Minor Lua-tree defects for one cleanup issue
> **Note:** Two sub-items below are verified FALSE and are excluded from #1059. `flattenItemInstanceIds'` DOES have an unprimed sibling — `World/Save/Snapshot.hs:299`, character-identical — which is the duplication CH-70 owns; renaming it away would erase CH-70's signal. `callSaveModules0` DOES have a sibling, `callSaveModules1`, whose haddock documents the 0/1 arity pair. #1059 covers the three real items: the duplicate `-- |` opener at `Script.hs:46-47`; `registerEngineAPI`'s explicit `Lua.State` (the only one of THIRTEEN registrars, not twelve); and folding `Engine.Scripting.Types` into `Engine.Scripting.Lua.Types`.

- `Engine/Scripting/Lua/Script.hs:46-47` — two consecutive `-- |` haddock
  openers on `callModuleFunction`; the first (`-- | Call a function on a module
  table`) is a leftover stub that haddock will fold into the real one. Same
  defect class as CH-4.
- `API/Save.hs:417` — `flattenItemInstanceIds'` has a prime suffix with no
  unprimed sibling anywhere; the prime conventionally marks a variant of an
  existing function.
- `API/Save.hs:745` — `callSaveModules0`: a `0` suffix with no
  `callSaveModules`.
- `API.hs` — `registerEngineAPI lst env backendState` is the only one of twelve
  registrars taking the `Lua.State` explicitly, even though all twelve already
  run inside `Lua.runWith lst`.
- `Engine.Scripting.Types` is a 14-line top-level namespace holding one type,
  sitting beside the 272-line `Engine.Scripting.Lua.Types`. The split buys
  nothing — `ScriptValue` is Lua-specific in practice.

### [#1005] CH-49. Cross-cutting: normalise the enforced Unicode operators (owner decision recorded)
CLAUDE.md publishes an operator table as the codebase convention, but adoption
varies from 99% to 10%. Measured across `src/` + `app/`:

| Operator | ASCII | Unicode | Adoption | Decision |
|---|---:|---:|---:|---|
| **bitwise AND** | **38 `.&.`** | **4 `⌃`** | **10%** | **ENFORCE `⌃`** |
| **bitwise OR** | **55 `.\|.`** | **17 `⌄`** | **24%** | **ENFORCE `⌄`** |
| **bind** | **28 `>>=`** | **34 `⌦`** | **55%** | **ENFORCE `⌦`** |
| **equality** | **42 `==`** | **547 `≡`** | **93%** | **ENFORCE `≡`** |
| inequality | 16 `/=` | 90 `≢` | 85% | (follows equality) |
| logical and | 3 `&&` | 558 `∧` | 99% | already converged |
| logical or | 4 `\|\|` | 315 `∨` | 99% | already converged |
| fmap | 108 `<$>` | 134 `⊚` | 55% | **LEAVE BOTH — deliberate** |

**Owner decision (2026-07-25):** `<$>` and `⊚` are both kept — they read
better in different circumstances, and this is intentional, not drift. Remove
the implication that `⊚` is mandatory from CLAUDE.md's table. Everything else
in bold gets normalised to the Unicode form: `>>=` → `⌦`, `.&.` → `⌃`,
`.|.` → `⌄`, `==` → `≡`.

Planned fix: a Python script that rewrites all `.hs` files in one sweep,
followed by a `make ci` grep guard so the four enforced operators cannot
regress. ~163 sites total.

Where the ASCII forms concentrate:

- **Bitwise** (93 sites) is overwhelmingly Vulkan flag composition —
  `src/Engine/Graphics/Vulkan` (8 files) and `Vulkan/Texture` (3) lead, with a
  long tail through `World/` (Slope, Geology, Fluid, Flora, ZoomMap, SideFace,
  Plate, Render) and `Sim/Fluid`. This is the worst-adopted operator pair in
  the codebase and the main prize.
- **`==`** (42 sites) — notably 8 of them are in `src/Engine/Core/Capability`,
  the *newest* code in the tree (the #889 capability epic), which suggests the
  convention isn't reaching new work.

Care needed in the sweep: `.&.`/`.|.` must not be rewritten inside string
literals, comments, or the embedded GLSL in
`Engine/Graphics/Vulkan/ShaderCode.hs` (GLSL uses `&`/`|`, not these forms, but
the quasiquoted blocks should be excluded anyway), and `==` must not be
rewritten inside Lua/GLSL string payloads or Python-facing text.

---

## Batch 5 — `Engine/Asset`, `Engine/Scene`, `Engine/Loop`, remaining Engine (swept 2026-07-25)

This closes `src/Engine/` coverage.

### [#1006] CH-50. `Engine.Graphics.Transform` is a fully dead module
67 lines, three exported functions (`createModelMatrix`, `applyTransform`,
`combineTransforms`), listed in `synarchy.cabal` so it compiles on every build,
and **no module imports it**. It operates on `Transform2D`, which lives in
`Engine.Scene.Base` and is used elsewhere — so the type is live and only this
module's operations on it are dead.

Distinct from CH-28: those modules were invisible to the build; this one is
compiled, warning-checked, and still useless.

### [#1007] CH-51. `Engine.Asset.Manager` is a 470-line abstraction used as an ID generator
23 of its 27 exports have **no external consumer**:

> `cleanupAssetManager`, `loadTextureAtlas`, `loadTextureAtlasWithHandle`,
> `unloadAsset`, `getTextureAtlas`, `getShaderProgram`, `lookupTextureAsset`,
> `lookupFontAsset`, `lookupShaderAsset`, `getAllTextureHandles`,
> `getAllFontHandles`, `getAllShaderHandles`, `getTextureHandleState`,
> `getFontHandleState`, `getShaderHandleState`, `getTextureStateMap`,
> `getFontStateMap`, `getShaderStateMap`, `deleteTextureState`,
> `deleteFontState`, `deleteShaderState`, `updateShaderState`,
> `generateShaderHandle`

Seven modules import it; six import only `generateTextureHandle`,
`generateFontHandle`, `updateTextureState`, or `updateFontState` — the handle
allocators and state setters. The one bare import
(`Lua/Message/Texture.hs`) then calls
`Engine.Graphics.Vulkan.Texture.Bindless.registerTexture` *directly* at line
233, bypassing `Manager.loadTextureAtlas` entirely.

So the real texture path is Lua message → bindless system, and `Manager` keeps
a complete parallel load/lookup/unload API — including its own
`registerTexture` call at line 252 — that nothing invokes. The whole shader
half (`ShaderProgram`, `generateShaderHandle`, `updateShaderState`,
`getShaderStateMap`, …) has no consumer at all.

`cleanupAssetManager` being dead also means the asset-cleanup path never runs.

Fix: reduce to the handle allocators + state setters actually used, and delete
the shader-asset half (or say why it is kept).

### [#1008] CH-52. 14 verbatim copies of the same YAML loader
Thirteen `Engine/Asset/Yaml*.hs` modules each define a `load<Thing>Yaml` that
is character-identical except for three strings and one field accessor:

```haskell
load<Thing>Yaml ∷ LoggerState → FilePath → IO [<Thing>Def]
load<Thing>Yaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse <noun> YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right f → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (<accessor> f)))
                <> " <plural> from " <> T.pack path
            return (<accessor> f)
```

Each is also preceded by an identical `newtype <Thing>YamlFile` +
`FromJSON` instance wrapping a single list field. 14 copies of the block in
total (Buildings, Equipment, Flora, Infection, Items, Locations, LootTables,
Names, Notifications, Recipes, Substance, Textures ×2, Units).

The whole family collapses to one helper:

```haskell
loadYamlList ∷ FromJSON f ⇒ LoggerState → Text → (f → [a]) → FilePath → IO [a]
```

Signatures also drift where nothing forced them to: `loadLootTableYaml`
returns `IO (Maybe LootTableYamlDef)` and `loadNamePool` returns `IO NamePool`
while every sibling returns `IO [x]` — three different failure conventions for
the same operation.

### [#1009] CH-53. `Engine.Asset.YamlTextures` loads no textures and holds three unrelated things
Its contents:

1. **Material YAML** — `MaterialDef`, `MaterialFile`, `loadMaterialYaml`,
   `loadMaterialDirectory`, `loadPopulatedMaterialRegistry`
2. **Vegetation YAML** — `VegetationDef`, `VegetationFile`, `loadVegetationYaml`
3. **A runtime texture-name registry** — `TextureNameRegistry`,
   `emptyTextureNameRegistry`, `lookupTextureName`, `registerTextureName`,
   `registryToList`, `registryToWorldCommands` — not YAML at all

Nothing in it loads a texture. The word "Textures" in the name refers only to
item 3, which is a live name→handle map, not a loader. Consequence:
`Engine.Core.State` — the engine's central record — imports a module called
`YamlTextures` to get a runtime registry type.

Compare the sibling naming: `YamlItems` loads items, `YamlFlora` loads flora,
`YamlRecipes` loads recipes. This one loads materials and vegetation.

Fix: split into `YamlMaterials.hs`, `YamlVegetation.hs`, and
`Engine/Asset/TextureNameRegistry.hs`.

### [#1083] CH-54. 97 exported names in `src/Engine/` have no consumer outside their module
> **Partial:** #1010's Out of scope excludes "any other module in CH-54's
> inventory", so it settles only the `Window/GLFW.hs` wrappers. Its body says the
> rest of kind (a) is tracked by #1007, #1006, #943, #946, #972, #947, #1009 and
> CH-55, and that kind (b) is excluded because the `Loop/Camera.hs` exemplar is
> wrong (four of its eight names are used by `test-headless/Test/Headless/Camera/`).
> Re-process to confirm that set closes the finding before checking it off.
> **Historical (superseded 2026-08-03):** that re-processing happened and is
> recorded below — the twelve tracking issues did NOT close the finding, and the
> surviving remainder is now filed as #1083. Kept for the #1010 scope facts it
> records, not as an active partial finding.

> **Note:** All twelve tracking issues (#943, #946, #947, #972, #978, #980, #1006,
> #1007, #1009, #1010, #1011, #1077) are now closed, and they did NOT close this
> finding. Scan re-run 2026-08-03: **64 names across 35 modules** survive, and
> #1083 covers that remainder. Confirmed landed: `Graphics/Transform.hs` gone,
> `fontFragmentShaderCode` gone, `Window/GLFW.hs`'s eleven gone, `Asset/Manager.hs`
> 23 → 2, `Core/Log.hs` 5 → 1, `Core/Error/Exception.hs`'s two gone. The body's
> `Loop/Camera.hs` exemplar was indeed wrong — 4 of its 8, not 8, are unreferenced.
> One survivor is more than an export-list defect: `API/Log.hs`'s `registerLogAPI`
> is a registrar nothing calls, leaving `engine.setDebugCategory` /
> `getDebugCategories` unreachable from Lua; #1083 requires that be decided, not swept.

Full inventory captured by scan (qualified uses counted, so no
`import qualified … as GLFW` false positives). It splits into two kinds:

**(a) Genuinely dead API** — nothing uses them anywhere, including inside the
module. Biggest cluster is CH-51's `Asset/Manager.hs` (23). Others:
`Engine/Core/Log.hs` (5, per CH-7), `Core/Log/Monad.hs` (3),
`Graphics/Transform.hs` (3, per CH-50), `Vulkan/Texture/Bindless.hs` (3, per
CH-34), `Core/Error/Exception.hs` (2, per CH-11),
`Asset/YamlUnits.hs` (`defaultUnitYamlBody`,
`defaultUnitYamlNaturalResistance`), `Vulkan/ShaderCode.hs`
(`fontFragmentShaderCode`, per CH-36).

**(b) Over-exported internals** — used only within their own module, so the
export list is simply too wide. Exemplar: `Engine/Loop/Camera.hs` exports 8
names (`zoomMin`, `zoomMax`, `cameraYLimit`, `cameraYLimitChunks`,
`applyLimits`, `applyLimitsChunks`, `stepCameraZoom`,
`cameraGotoBufferChunks`) that only it uses — `Engine.Loop` imports exactly
three functions from it. `Window/GLFW.hs` (11), `Scene/Batch/*` (5),
`Vulkan/Descriptor.hs`, `Vulkan/Device.hs`, `Vulkan/Swapchain.hs`,
`Loop/Frame.hs`, `Loop/Resource.hs` follow the same pattern.

Kind (b) matters because it is what makes the 29 export-list-less modules
(CH-27) hard to fix: with everything public by default and export lists that
don't narrow, no refactor can be reasoned about locally.

Fix: two issues — delete kind (a); shrink the export lists for kind (b).

### [no-issue] CH-55. `Engine.Core.Init`'s three exports have no callers
> **Disposition:** No issue — the defect is real but is a single export-list line, and CH-57 is the bundle for exactly this class. Verified: the module exports **seven** names (not three), and only `initializeEngineWith` is unreferenced — the string appears nowhere outside its own module, where `initializeEngine` (`Init.hs:135`) and `initializeEngineHeadlessWith` (`:364`) call it. The body's 2026-07-25 correction holds: `resolveConfigPath` and `migrateLegacyConfig` are each used by `test-headless/Test/Headless/Core/ConfigState.hs`, `initializeEngineHeadless` by `app/App/Headless.hs` plus five test modules, and `initializeEngineHeadlessWith` by `app/App/Dump.hs`. CH-57 already lists five bullets of the identical shape (`Loop/Resource.hs`'s `safeVector*`, `Font/Util.hs`'s `calculateTextWidth`, `Scene/Graph.hs`'s `withSceneGraph*`, `Input/Thread/Mouse.hs`'s `uiDragThresholdPx`, `Preview/Discovery.hs`'s `isSupportedTextureFile`/`sortEntries`); **un-exporting `initializeEngineWith` is a sixth bullet and must be folded into CH-57 when CH-57 is processed**. A standalone PR for one export-list line would fragment that bundle — the same reasoning that closed CH-44 in favour of CH-120. It cannot fold into #1010, whose Out of scope excludes every other module in CH-54's inventory.

**Corrected 2026-07-25:** only `initializeEngineWith` is genuinely
unreferenced — `resolveConfigPath` and `migrateLegacyConfig` each have one
consumer in `test-headless/`, which the original scan missed.

`initializeEngineWith` is exported and called by nothing; `initializeEngine`
and `initializeEngineHeadlessWith` reach it internally. A one-line
un-export.

### [no-issue] CH-56. `Engine/Scene` has the `X.hs` + `X/` + `Types/X.hs` triple layout
> **Disposition:** No issue — the exemplar is wrong and the proposed rule contradicts a documented, CI-enforced shape. Measured across `src/`: **69** `X.hs`-beside-`X/` pairs — 30 pure re-export facades, 8 facade-plus-content, 31 content-only siblings. The Scene exemplar does not hold: `Engine/Scene/Types.hs` (16 lines) and `Engine/Scene/Batch.hs` (19 lines) contain nothing but `module Engine.Scene.Types.Node`-style re-exports and zero definitions, so a scene batch type *cannot* live in `Scene/Types.hs` — `Engine/Scene/` already follows the rule the finding asks for. The Vulkan half is accurate (`Vulkan/Types.hs` 11 definitions, `Command.hs` 4, `Pipeline.hs` 6, none re-exporting). But "`X.hs` beside `X/` means facade, full stop" would restructure 39 of the 69 and would forbid a shape the repository documents and enforces: CLAUDE.md:172 calls `Engine.Input.Thread` (#787) "a thin lifecycle facade" although it keeps four definitions and re-exports nothing, `tools/haskell_module_budget.py` caps that parent together with its children, and CLAUDE.md:42 explicitly anticipates "the facade remains above 500 lines". The closed #543-#588 split program deliberately produced both shapes — pure shells (`World/Plate.hs`, `UI/Manager.hs`, `World/Slope.hs`, `Combat/Wounds.hs`, `Lua/API/Units.hs`) and orchestrating parents (`Combat/Resolution.hs`, `World/Generate/Chunk.hs`, `World/Geology/Coastal.hs`, `Engine/Input/Thread.hs`). Converging them is discretionary churn across 39 modules with no defect behind it — the category that closed CH-22, CH-41, and CH-42. Where the layout genuinely costs something it has its own finding: CH-78 (`World/Save/Envelope.hs`, 860 lines and 74 definitions beside `Envelope/`), CH-97, and CH-127.

`Engine/Scene/` contains `Base.hs`, `Graph.hs`, `Render.hs`, `Types.hs`, plus
both `Batch/` and `Types/` subdirectories — so a scene batch type can live in
`Scene/Types.hs`, `Scene/Types/Batch.hs`, or `Scene/Batch/Update.hs`, and the
naming gives no rule for which. Same shape as `Engine/Graphics/Vulkan/`
(`Types.hs` beside `Types/`, `Texture.hs` beside `Texture/`, `Command.hs`
beside `Command/`, `Pipeline.hs` beside `Pipeline/`).

This is a tree-wide convention gap: sometimes `X.hs` is a facade for `X/`
(`Engine/Scripting/Lua/API/Units.hs`), sometimes it is a sibling holding
different content (`Engine/Graphics/Vulkan/Types.hs` vs `Types/`). A reader
cannot tell which without opening the file.

Fix: state the rule — `X.hs` beside `X/` means facade, full stop — and move
non-facade content out.

### [#1011] CH-57. Minor remaining-Engine defects for one cleanup issue
- `Engine/Loop/Resource.hs` exports `safeVectorHead` and `safeVectorIndex`,
  both unused externally — generic container helpers sitting in a module named
  for the render loop's resources.
- `Engine/Graphics/Font/Util.hs` (24 lines) exists to hold one unused function
  (`calculateTextWidth`).
- `Engine/Scene/Graph.hs` exports `withSceneGraph`/`withSceneGraphM`, both
  unreferenced — a bracket abstraction nothing brackets with.
- `Engine/Input/Thread/Mouse.hs` exports `uiDragThresholdPx` (a tuning
  constant) that only it reads.
- `Engine/Preview/Discovery.hs` exports `isSupportedTextureFile` and
  `sortEntries` with no consumer — notable because CLAUDE.md advertises hspec
  coverage for "the pure discovery/labeling/ordering/containment logic", and
  the ordering helper is not what the tests reach for.

---

## Batch 6 — `app/` (swept 2026-07-25)

1275 lines across 10 modules; all are in the cabal `other-modules` list. The
problems are boot-mode duplication and CLI flags that silently do nothing.

### [#1012] CH-58. `--seed`, `--worldSize`, and `--plates` are silently ignored outside `--dump`
`Main.hs` parses all three, normalises them (`normalizeWorldSize`,
`normalizePlateCount`, `defaultPlatesFor`), and then hands them to exactly one
dispatch target:

```haskell
runDump      ∷ DumpLayers → Int → Int → Int → (Int,Int,Int,Int) → IO ()
runHeadless  ∷ BootProfile → Maybe Int → IO ()
runGraphical ∷ BootProfile → Maybe Int → IO ()
runOffscreen ∷ BootProfile → Maybe Int → Maybe (Int,Int) → IO ()
runPreview   ∷ (Text, Maybe Text) → Maybe PreviewBrowse → Maybe Int → IO ()
```

So `cabal run synarchy -- --headless --seed 42 --worldSize 256 --plates 5`
accepts every flag, computes with them, and discards all three without a word.
(Headless world generation goes through Lua's `world.init` instead.)

Compounding it, the normalisation warnings are themselves gated on dump mode:

```haskell
when (isJust mDump ∧ worldSize /= rawWorldSize) $ hPutStrLn stderr …
```

— so even in the one mode where a flag *is* honoured the feedback is
conditional, and in the five where it isn't there is no diagnostic at all.

Fix: reject unsupported flags per boot mode with a clear error, or thread them
through. Silently accepting a flag that does nothing is the worst of the three
options.

### [#1016] CH-59. `allLayers` is not all layers
`App/Cli.hs:38`:

```haskell
allLayers ∷ DumpLayers
allLayers = DumpLayers True True True True True False
```

The `False` is `dlSlope`. Its own haddock opens "**Default** layers (when --dump
has no =value)" and then explains slope is deliberately excluded — so the doc
and the name disagree in the first two words. Rename to `defaultLayers`.

Also: six bare positional booleans, unreadable without counting fields against
the record declaration (the same anti-pattern as `AssetConfig 100 True True`,
CH-18). Use field syntax.

### [#1019] CH-60. The preview category list is duplicated as an error-message string
`App/Cli.classifyPreviewCategory` holds the authoritative lists:

```haskell
    simpleCategories  = ["icons", "items", "ui", "world"]
    groupedCategories = ["units", "flora", "buildings", "structures"]
```

Neither is exported. `Main.hs:92` re-states them as prose:

```haskell
    ⧺ " (expected one of: icons, items, ui, world, units, "
    ⧺ "flora, buildings, structures)"
```

CLAUDE.md pins this as a contract — "the unknown-category error message lists
exactly this set, no compatibility aliases" — and `tools/preview_cli_probe.py`
is CI-eligible, so a drift here fails CI *late* rather than not compiling.
Export the lists and build the message from them.

### [#1021] CH-61. Five boot modes hand-copy the same error-recovery block
`Graphical`, `Headless`, `Offscreen`, `Dump`, and `Preview` each end with the
identical shape — including this comment reproduced **verbatim in all five**:

```haskell
        -- Flush buffered log lines — the error context is exactly
        -- what we must not lose — then exit with a failure code.
```

Each copy re-lists every worker thread to shut down, in an order that is an
undocumented invariant in some copies and documented in others. `Offscreen`'s
happy path explains it:

> Combat first: wound ticks enqueue UnitKill/UnitCollapse onto the unit queue,
> so the producer has to stop before the consumer …

— but its own error path re-implements the sequence separately, and the other
four copies carry no such note. A missed thread or a wrong order in one of five
hand-maintained copies is invisible.

The `env'` patch is duplicated too — this ten-line block appears in
`Graphical`, `Headless`, and `Offscreen`:

```haskell
  let env' = case mPort of
        Just p  → env { engineConfig = (engineConfig env)
                          { ecDebugPort = p, ecBootProfile = bootProfile } }
        Nothing → env { engineConfig = (engineConfig env)
                          { ecBootProfile = bootProfile } }
```

Both branches differ only in whether one field is set — `maybe id` collapses
it to one line.

Fix: `App.Boot` with `withBootConfig` and `shutdownAllWorkers`, used by all
five.

### [#1036] CH-62. `shutdownEngine`'s five positional parameters are mutually swappable
```haskell
shutdownEngine ∷ Maybe Window → Maybe ThreadState → Maybe ThreadState
               → ThreadState → ThreadState → EngineM ε σ ()
```

Two adjacent `Maybe ThreadState` (unit, world) and two adjacent `ThreadState`
(input, lua). Swapping either pair compiles silently and tears threads down in
the wrong order — precisely the invariant CH-61 shows the codebase already
knows is load-bearing. The call site reads:

```haskell
shutdownEngine Nothing (Just unitThreadState)
               (Just worldThreadState) inputThreadState luaThreadState
```

`Nothing` for what, at a glance? Fix: a record.

### [#1022] CH-63. Three separate main loops
`Engine.Loop.mainLoop`, `Engine.Loop.mainLoopOffscreen` (both in `Loop.hs`),
and `Engine.Loop.Headless.headlessLoop`. The first two are already
near-duplicates sharing `runGatedByCaptureLock` (CH-24); the third lives in a
different module. Combined with CH-61's five boot paths, the "start engine,
tick, shut down" story is told six times.

Worth one design issue: one loop parameterised by mode (poll events? present?
pace frames?), or an explicit statement of why three are irreducible.

### [#1040] CH-64. `--dump` emits three fields that no documentation mentions
Actual output includes `waterTableZ`, `waterTableSummer`, and
`waterTableWinter` under the `terrain` layer. CLAUDE.md's dump field table —
presented as the contract, and what the audit tooling is written against —
lists only:

> \| `terrainZ`, `surfaceZ` \| terrain \| Raw terrain and max(terrain, fluid) \|

No `tools/*.py` reads the three water-table fields. They ride in every default
`--dump` unread and undocumented.

This also puts pressure on `App/Cli.hs`'s claim that a bare `--dump` "stays
byte-identical to historical output" — true for `slope` being opt-in, but the
terrain layer grew three fields since.

Fix: document them in CLAUDE.md's table, or drop them behind an opt-in layer
like `slope`.

### [#1058] CH-65. `App/Dump.hs` hand-concatenates JSON
`dumpTilesJSON` builds output with ~100 lines of string concatenation —
`",\"terrainZ\":" ⧺ show terrainZ` — plus its own `boolStr`, `fluidTypeStr`,
and `iceModeStr` encoders. The package already depends on `aeson` (every
`Engine/Asset/Yaml*` module uses it).

Nothing escapes strings, which is safe *today* only because every emitted value
is numeric or a closed enum. The first string-valued field (a material name, a
location id) introduces an injection bug in a format the baselines and audit
tools parse.

Also here: `waitForInit` and `waitForChunks` (lines 234-275) are the same
function twice — identical `go n` recursion, identical timeout arithmetic, and
the second's comment literally defers to the first ("see 'waitForInit'"). Only
the readiness predicate and two log strings differ.

### [#1081] CH-66. Primitive-obsession in the dump signatures
> **Note:** Two corrections. `dumpTilesJSON` takes FIVE bare `Int`s, not four —
> `worldSize` sits immediately before the four region bounds. And the region tuple is
> not destructured "purely to be re-spread": `runDump` also uses the components at
> `Dump.hs:59-61` and `:125`. #1081 excludes `parseRegion`'s malformed-input defaulting,
> which is CH-67 below.
```haskell
runDump      ∷ DumpLayers → Int → Int → Int → (Int,Int,Int,Int) → IO ()
dumpTilesJSON ∷ DumpLayers → MaterialRegistry → Int → ClimateState
              → WorldTileData → Int → Int → Int → Int → BS.ByteString
```

`runDump`'s three bare `Int`s are seed, worldSize, plateCount — swap any two
and it compiles and generates a different world. `dumpTilesJSON` takes the
region as four loose `Int`s even though `parseRegion` already produced it as a
tuple, so the tuple is destructured purely to be re-spread positionally.

### [#1481] CH-67. `parseRegion` silently substitutes a default for malformed input
> **Note (2026-08-03, since superseded):** the 2026-08-03 verification widened this
> entry to every value-carrying flag — `--seed`/`--worldSize`/`--plates`/`--ages`/
> `--port` were observed defaulting just as silently via `fromMaybe` at their
> `Main.hs` call sites. That widening is **obsolete**: #1191 (`2046a8c3`, merged
> 2026-08-09) made all of them reject a present-but-malformed value, and
> deliberately left `--region` for this finding. A bare `--region` with no value
> still defaults silently, and `app/App/Cli.hs:195` still returns the bare tuple,
> so the finding itself — and its #1081 deferral — stand unchanged in the narrower
> form the body below now states.
```haskell
parseRegion ("--region":s:_) =
    case map reads (splitOn ',' s) of
        [[(cx1,"")],[(cy1,"")],[(cx2,"")],[(cy2,"")]] → (cx1, cy1, cx2, cy2)
        _ → (-8, -8, 8, 8)
```

`--region garbage` dumps the default 16×16 chunk region and reports nothing.
Because the return type is a bare tuple rather than `Maybe`, the caller cannot
distinguish "user asked for the default" from "user's input was rejected" — so
a typo in a long-running dump silently produces the wrong data.

Contrast `App.ResourceRoot`/`parsePreview`, which explicitly treat a bare flag
as an error rather than an absence, and — since #1191 — `parseArg`,
`parseDump` and `parseSize`, which all return `Either CliError (Maybe a)` so
absence and malformed presence are different answers. `parseRegion` is now the
only silent-default parser left in the file; #1191 deliberately left it for
this finding, sequenced after #1081's named-region type.

### [#1084] CH-68. Two module haddocks enumerate the boot modes and both are stale
> **Note:** Verified 2026-08-03 — both quoted haddocks are stale verbatim, and #1084
> folds in two refinements. `App/Exception.hs`'s *claim* is accurate (all five
> `runEngineM` sites are wrapped; language-report never boots an engine), so only its
> parenthetical is wrong. `App/Cli.hs` is stale a second way: its content summary
> predates `parseSize`/`parsePreview`/`classifyPreviewCategory`/`parseLanguageReport`/
> `parseSeeds`. A THIRD instance outside `app/` is included —
> `Lua/Message.hs:94-99`'s `whenGraphical` comment is wrong, not merely outdated:
> it gates on `ecHeadless`, so it also runs in offscreen and preview.
- `App/Cli.hs:1` — "shared by every boot mode (graphical, headless, dump)"
- `App/Exception.hs:2` — "shared by every `runEngineM` call site (graphical,
  headless, dump)"

There are six: graphical, headless, offscreen, dump, preview, language-report.
`guardNativeExceptions` is in fact used by five of them. Enumerating a list
that grows is the failure mode; say "every boot mode" and stop.

### [#1086] CH-69. Minor `app/` defects for one cleanup issue
> **Note:** Verified 2026-08-03. #1086 covers four of the six bullets. Two need no
> work: the ASCII `/=` was already normalised by #1005, and the hand-rolled `splitOn`
> is a non-defect by this finding's own wording ("fine as the only copy"). Line drift:
> `drop 7` is at `Cli.hs:57` (not `:48`), `splitOn` at `:191` (not `:175`). The
> `main`-length bullet is reframed — at 128 lines (not 110) it is unremarkable against
> CH-41's no-issue disposition on function length, so #1086 targets the REAL defect
> #1012 introduced: the six-way precedence is encoded twice, in the dispatch and again
> in `selectedBootModeName`, held in sync only by a comment.
- `Main.hs` — `fromMaybe 8008 port` appears **six times**. Given CLAUDE.md
  warns agents at length that 8008 collides with the user's GUI instance, the
  default port deserves one named constant.
- `Main.hs` — `main` is a single 110-line function: env setup, parsing,
  normalisation, two conditional warnings, and a five-level nested
  `if`/`case`/`case`/`case`/`case` dispatch. A `BootMode` ADT resolved once,
  then dispatched once, would make the precedence rules (`--language-report`
  beats `--dump` beats `--preview` beats `--offscreen` beats `--headless`)
  legible instead of inferred from nesting depth.
- `Main.hs:63,67` — ASCII `/=`, covered by the CH-49 sweep.
- `App/Cli.hs:48` — `drop 7 a` to strip `"--dump="`; a magic length.
- `App/Headless.hs` — the function haddock repeats the module haddock and is
  stale where the module one is right: it says "Starts Lua, world, and unit
  threads" while the code starts five (Lua, world, unit, sim, combat). Delete
  the duplicate.
- `App/Cli.hs:175` — a hand-rolled `splitOn`; fine as the only copy, but it is
  the third string-splitting idiom in the file alongside `break (≡ '/')` and
  `isPrefixOf`+`drop`.

---

## Batch 7 — `src/World/Save`, `src/World/Load`, `src/Engine/Save` (swept 2026-07-25)

9848 lines across 22 modules; all correctly listed in the cabal file. This is
the youngest large subsystem in the tree (the #756-#768 overhaul) and it shows:
the abstractions are sound but under-shared, so the same logic is written
several times.

See also **CH-40** (`currentSaveVersion`'s 296-line changelog) and **CH-23**
(eight of the tree's seventeen oversized modules live here).

### [#1090] CH-70. The save system's item enumeration is implemented three times
> **Note:** Verified 2026-08-03 — confirmed in full, including the consequence: the
> record and DTO codec fail loudly under `-Werror`, but all three enumerations are
> `concatMap` chains where an omission is silent, and nothing catches it (no test
> asserts the three agree; `tools/persistence_inventory_audit.py` has no field-level
> coverage). Two location corrections: the primed copy moved to
> `API/Save/Integrity.hs:35` (#985's split, not `API/Save.hs:417`), and
> `flattenItemInstances` is at `Types.hs:773` (not `:987`). The third entry is
> UNDERSOLD here — `missingItemDefReferences` (`Types.hs:783-806`) is a full third
> enumeration of the same six containers, not merely a different return type. The
> suggested fix is sound and needs no shape adaptation: the three item-bearing fields
> have identical types on both page records (`GroundItems`/`BuildingSnapshot`/
> `UnitSnapshot`).
The recursive walk over every item instance in a session — the basis of both
the id-allocator check and load-time integrity validation — exists in three
places:

| Module | Function | Returns |
|---|---|---|
| `World/Save/Snapshot.hs:299` | `flattenItemInstanceIds` | `[Word64]` |
| `Engine/Scripting/Lua/API/Save.hs:417` | `flattenItemInstanceIds'` | `[Word64]` |
| `World/Save/Types.hs:987` | `flattenItemInstances` | `[ItemInstance]` |

The first two are **character-identical apart from the prime**, and so is the
cascade built on them. `Snapshot.hs:308-323` and `API/Save.hs:455-468` are the
same function twice — same `pageItemIds` / `unitItemIds` / `buildingItemIds`
structure, same traversal of ground items, unit
`uisInventory`/`uisEquipped`/`uisAccessories`, and building
`bisMaterialsDelivered`/`bisStorage` — differing only in whether the page
accessors are `pgs*` (`SessionSnapshot`) or `wps*` (`SaveData`).

The third admits the duplication in its own comment: "mirrors
`World.Save.Snapshot.flattenItemInstanceIds`".

This is the highest-consequence duplication found so far. Adding a new item
container to `UnitInstanceSnapshot` requires editing all three; miss one and
integrity validation silently stops seeing those items — no error, no test
failure, corrupt saves accepted.

Fix: one traversal, parameterised over the page accessor (or run after the
`SaveData` to `SessionSnapshot` adaptation so only one shape exists).

### [#1091] CH-71. `WorldPageId` has no accessor, so ten sites hand-write one
> **Note:** Verified 2026-08-03 — ten sites confirmed, but they are not ten copies of
> one line. NINE are that `where` clause in `World/Save/Types.hs` (`:723, 769, 822,
> 855, 889, 931, 990, 1043, 1127`, one per `renderMissing*Ref`); the TENTH
> (`World/Thread/Helpers.hs:74-75`) is an exported top-level accessor that 17 modules
> import (95 uses across 19 files), so "costs nothing" understates it — though all 17
> already import `World.Types`/`World.Page.Types`, so no module gains a dependency.
> The finding misses the root cause: `Save/Types.hs` CANNOT reuse the canonical
> accessor, because `Thread/Helpers.hs` carries `EngineEnv` and Lua types; the newtype
> lives in `World/Page/Types.hs:17`, which has no local imports at all. The nine
> copies are a layering workaround, which is the real argument for the fix. The nine
> call sites are also CH-72's territory — the two are independent, and whichever lands
> second has less to delete.
```haskell
newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)
    deriving newtype (Hashable, Serialize)
```

No field label. Consequence — this exact line appears **ten times** across
`World/Save/Types.hs` and `World/Thread/Helpers.hs`:

```haskell
  where unWorldPageId (WorldPageId t) = t
```

Adding `{ unWorldPageId :: Text }` to the newtype deletes all ten and costs
nothing (the `deriving newtype` clauses are unaffected).

### [no-issue] CH-72. Nine near-identical `Missing*Ref` types, misplaced in `Types.hs`
> **Disposition:** No issue — both stated problems fail. Claim 1's premise is false:
> `World/Save/Integrity.hs:36-47`'s haddock states the placement rule and rejects this
> exact fix — the `missingDefReferences` family is "deliberately NOT folded into this
> module's Haskell types … 9 already-working, already-tested validators against 9
> different IO-loaded registries, rewritten onto one generic traversal, would be a large
> rewrite of working code for a vocabulary-only gain", and both halves already report
> through `continueLoad`'s single rejection gate (#764). That heterogeneity is real: the
> nine scans take `HS.HashSet Text` ×5, `MaterialRegistry`, `FloraCatalog`,
> `InfectionManager`, and `missingDefReferences` takes TWO registries over a different
> page tuple — so "one renderer" understates the scans, which are most of the bulk.
> Claim 2 is stale: #984 archived the 296-line changelog, and CH-23 already dispositioned
> the size dimension no-issue (no persistence module carries a split agreement). Counts
> corrected: the family spans `:711-1146` (~436 lines, 38%), not `:925-1316`/390/30%;
> `Integrity.hs` is 531 lines, not 477; `API/Save.hs` imports nine SCANS plus nine
> renderers, not nine types.
`World/Save/Types.hs:925-1316` — roughly 390 lines, 30% of the file — is nine
repetitions of one shape:

`MissingDefRef`, `MissingItemDefRef`, `MissingRecipeRef`,
`MissingBillOutputItemRef`, `MissingConstructDefRef`, `MissingMaterialRef`,
`MissingFloraRef`, `MissingLocationRef`, `MissingInfectionRef`

Each contributes a `data Missing<X>Ref` (a kind/source `Text`, a
`WorldPageId`, an entity id, an unresolved name), a `renderMissing<X>Ref`
producing the same "&lt;thing&gt; #N on page 'P' references unknown &lt;kind&gt;
'&lt;name&gt;'" sentence, a `missing<X>References` scan, and its own copy of
CH-71's `unWorldPageId`.

Two problems beyond the repetition:

1. **It is in the wrong module.** `World/Save/Integrity.hs` (477 lines) is the
   dedicated integrity module. Reference validation is split across two files
   with no stated rule for which half goes where — and
   `Engine.Scripting.Lua.API.Save` has to import 18 names from `Types.hs`
   (nine types plus nine renderers) to do one job.
2. **It is why `Types.hs` is the largest module in the tree.** 296 lines of
   `currentSaveVersion` comment (CH-40) plus 390 lines of this is over half
   the file.

Fix: one `MissingRef { mrKind, mrSource, mrPage, mrEntity, mrName }` with one
renderer, moved to `Integrity.hs`.

### [#1093] CH-73. `serializeCodec` cannot express the migration the component system exists for
> **Note (pre-resolution verification, 2026-08-03):** confirmed, and stronger than stated. The helper
> ALREADY takes a `migrate ∷ Word32 → d → …` seam, and all six call sites pass
> `(\_ d → Right d)`: it is vestigial, which the finding misses. The proposed fix is
> also insufficient alone — real multi-version decode reads a DIFFERENT frozen DTO type
> per version (`CraftBillsDTOv1` vs `CraftBillsDTO`), which one `S.Serialize d`
> constraint cannot express, so #1093 requires a per-version decoder table rather than
> just widening `ccInputVers`. Counts corrected: **6** use the helper and **4**
> hand-roll (ten codecs total), not 7/5; the workaround is documented FOUR times
> (`Page.hs:540-543`, `Entities.hs:658-661`, `:912-920`, `:1106-1109`), not twice; and
> all six call sites are opaque, not just `buildingsCodec`. No prior decision protects
> this — unlike CH-72, #766 never mentions the helper and `Entities.hs:914-918` calls
> it a gap ("no real multi-version dispatch wired up despite the seam being documented").
```haskell
serializeCodec :: S.Serialize d => ComponentId -> Word32 -> Bool -> [ComponentId] -> ...
serializeCodec cid ver req deps toDTO migrate validate = ComponentCodec
    { ccId        = cid
    , ccVersion   = ver
    , ccInputVers = [ver]        -- accepts only its own version
    ...
```

`ccInputVers` is hardcoded to `[ver]`, so a component built with the helper can
never decode an older payload. Per-component schema evolution is the stated
reason the envelope format exists (CLAUDE.md: "Component evolution =
per-component schema version bumps + explicit migrations from frozen vN DTOs")
— so the convenience constructor fails at precisely the case it was built for.

Result: two divergent idioms, split by whether a component has ever needed to
evolve. **7 codecs use `serializeCodec`; 5 hand-roll the full record.** The
workaround is documented twice, in two separate comments:

> `unitSimCodec` … hand-rolled `ComponentCodec` (mirrors
> `craftBillsCodec`/`powerNodesCodec` — `serializeCodec` has no real
> multi-version dispatch) now that this component needs v1 to v2 migration too.

Every component that has actually evolved abandoned the helper; the ones still
using it are one schema change away from doing the same.

The call site is also opaque:

```haskell
buildingsCodec = serializeCodec
    buildingsComponentId 1 True [worldPagesComponentId, coreSessionComponentId]
```

`1` is the version and `True` is "required"; neither is legible without opening
the definition.

Fix: give `serializeCodec` an input-version list (or a `migrate` variant), and
name the version/required arguments.

**Resolved (#1093).** `serializeCodec` is replaced by `componentCodec` taking a
named-field `ComponentSpec`, plus `atVersion`, which declares one accepted
version and closes over the frozen DTO type that version's bytes decode
through — so a single codec decodes a different DTO per version, which a
widened `ccInputVers` under one `Serialize d` constraint never could.
`ccInputVers`, the decode dispatch, and the unsupported-version message are all
derived from those same declarations. All eleven registered gameplay codecs
(including the four that hand-rolled multi-version decode) now go through it,
and no codec hand-writes either universal decode error.

### [no-issue] CH-74. `Component/Entities.hs` is five components in one 1139-line module
> **Disposition:** No issue — the counts are right (1139 lines, 69 top-level declarations,
> five codecs at :243/:476/:662/:921/:1110) but both structural claims fail. The module
> is not misnamed for its contents: its haddock opens "Entity + entity-ADJACENT page-scoped
> components", documents all five with owners and dependencies, and states the organizing
> principle they all satisfy — "all page-scoped, all validated against the `world-pages`
> authority (requirement 8)". And the split is not purely mechanical: `orderedPages`
> (`:161`) and `tshow` (`:164`) are file-local helpers used by all five and would need
> relocating (`applyPageSlices` is already shared from `Component/Types.hs:296`). What
> remains is size, which this repo has declined as a policy four times in this report
> (CH-22, CH-23, CH-41, CH-43) and in CLAUDE.md — the 500-line limits are per-split
> ratchets for named families, and this module has no budget entry. The one comparable
> split, #985 on `API/Save.hs`, was driven by the save path's permanent full-access
> exception; this module is pure DTO/codec with no `EngineEnv` access, so that driver
> does not transfer. CH-23 forwarded Entities.hs here; this is its answer.
It defines five independent codecs:

| Line | Codec | What it is |
|---:|---|---|
| 243 | `buildingsCodec` | buildings |
| 476 | `unitsCodec` | units |
| 662 | `unitSimCodec` | per-unit sim state |
| 921 | `craftBillsCodec` | craft bills |
| 1110 | `powerNodesCodec` | power nodes |

69 top-level declarations. Craft bills and power nodes are not entities, so the
module name describes three of its five contents. Each codec is a
self-contained group of `XDTO` + `toXDTO` + `fromXDTO` (plus `XDTOv1` +
`migrateXDTOv1` where versioned), so the five-way split is mechanical and
already drawn.

This is the largest single win available against CH-23.

### [#1099] CH-75. `tshow` is invented four times, while 570 sites don't use it
> **Note:** Verified 2026-08-03 — undercounted on both numbers. **Seven** definitions,
> not four: the four in `src/` (`Page.hs` is at `:161`, not `:148`) plus three the
> finding missed in `test-headless/` — `UI/ResponsiveMenus.hs:1460`,
> `UI/TutorialHud.hs:123` (monomorphic `Int → Text`), and
> `World/Save/Integrity.hs:628` (`where`-bound, so it shadows silently instead of
> conflicting — the one that needs deleting by hand). The hand-written form is at
> **618** sites across 123 files, not 570. Also not in the finding, and what makes the
> sweep safe: all seven defining modules use a bare `import UPrelude`, so exporting
> `tshow` there turns each top-level copy into an ambiguous occurrence — GHC forces the
> deletions and blocks re-divergence, so no CI guard is needed. The "same pass as
> CH-49" suggestion is moot: #1005 landed (84 files, 156/156), and is the precedent
> that makes a 123-file mechanical sweep normal practice here.
```haskell
tshow :: Show a => a -> Text
```
is defined independently in `World/Save/Component/Entities.hs:164`,
`World/Save/Component/Page.hs:148`, `World/Save/Component/Session.hs:52`, and
`Sim/Thread.hs:313` — three of them siblings in the same directory.

Meanwhile the un-abstracted `T.pack (show x)` / `T.pack $ show x` appears
**570 times** across `src/` + `app/`.

Fix: one `tshow` in `UPrelude` (which exists to carry exactly this kind of
shared vocabulary), delete the four local copies, and optionally sweep the 570
call sites in the same pass as the CH-49 operator normalisation.

### [no-issue] CH-76. Envelope compat is named after the epic's internal phase letters
> **Disposition:** No issue — the escalation that carries this finding is false and its
> second claim is already fixed. NONE of the four names is exported: `Envelope.hs`'s
> export list (`:59-75`) holds 16 names and includes none of them, and none is
> referenced outside the module — so this is not "promoted into the API surface", and
> "over-exports per CH-54" is wrong in the opposite direction. The finding is
> self-contradictory on this point, calling them API surface with call sites and then
> stating none is referenced outside `Envelope.hs`. They are also not unresolvable: each
> haddock defines the shorthand in place — `b1LegacyIds` (`:392`) "the exact frozen B1
> component-id set: precisely {metadata, session}", `b2Ids` (`:442`) "the exact #760-era
> ("B2") component-id set", `decodeB2SessionMetadata` (`:511`) "…for the B1 case: a
> #760-era save" — tying B2 to its issue number and enumerating the contents. The
> 25-review-round-comments claim was resolved by #949: re-measured 2026-08-03,
> `src/World/Save/`, `src/World/Load/`, and `src/Engine/Save/` each have ZERO, including
> all three files named as densest. What remains is a naming preference for four private
> helpers that document themselves; if CH-78's `Envelope.hs` split is ever filed, a
> rename rides along there for free.
`World/Save/Envelope.hs` defines `b1LegacyIds`, `b2Ids`,
`decodeB2SessionMetadata`, and `decodeLegacySessionMetadata`. "B1" and "B2" are
the save-overhaul epic's internal milestone labels (#759 = B1, #761 = B3) —
meaningless to a reader and unresolvable without pulling the epic.

This is CH-15's review-round archaeology promoted into the **API surface**,
which is worse: a comment can be deleted, an exported name has call sites.
None of the four is referenced outside `Envelope.hs`, so renaming is free
(they are also over-exports per CH-54).

Rename to what the formats are — e.g. `preLuaComponentIds`,
`flatSessionComponentIds` — or to the save versions they correspond to.

25 review-round comments also remain in this subsystem, densest in
`Component/Entities.hs` (6), `Save/Types.hs` (4), and `Load/Publish.hs` (4).

### [#1103] CH-77. `LuaComponentSpec` is a bare 4-tuple
> **Note:** Verified 2026-08-03 — right defect class, WRONG TUPLE. Every specific claim
> about `LuaComponentSpec` is false: its haddock (`Envelope.hs:139-144`) does name all
> four fields in order, and its slots are `Text`/`Word32`/`Bool`/`ByteString` — mutually
> DISTINCT types, so every permutation is already a compile error. It is therefore not
> the same defect class as `inpPendingUIClick` (CH-27, two `Text` + two `Double`) or
> `runDump`'s three bare `Int`s (CH-66), which are genuinely swappable. The real hazard
> is one line below it in `WorldSave`: the reference-edge payload
> `[(Text, Text, Int, Maybe Int, Text, Maybe Text)]`, whose slots 1/2/5 are all `Text`,
> which has NO alias at all and is spelled verbatim five times (`Bridge.hs:220`, `:344`,
> `:409`, `WriteWorld.hs:58`, `Command/Types.hs:229`), and which
> `Bridge.hs:344-371` assembles POSITIONALLY from named Lua fields with the three
> decoded `Text`s adjacent — a swap there mis-keys `luaReferenceErrors` and can
> reclassify a hard wrong-page error as a tolerated dangling diagnostic. #1103 covers
> both, and preserves `Command/Types.hs:235-236`'s documented reason for spelling the
> shape inline (keeping the Save/Envelope module graph out of `World.Command.Types`).
```haskell
type LuaComponentSpec = (Text, Word32, Bool, BS.ByteString)
```

A type alias over an anonymous tuple gives no safety and no documentation: the
components are (presumably) id, version, required, payload, but nothing says
so, and `Word32`/`Bool` are positionally interchangeable with nothing to catch
a swap. It crosses the Haskell/Lua persistence boundary — the place where a
silent field transposition is hardest to detect.

Same defect class as `inpPendingUIClick` (CH-27) and `runDump`'s three bare
`Int`s (CH-66). Should be a record.

### [no-issue] CH-78. `Envelope.hs` is 860 lines beside an `Envelope/` directory
> **Disposition:** No issue — the layout is documented and deliberate, and two of the
> numbers are wrong. `Envelope.hs:2-3` states the relationship outright: it "ties the
> generic tagged-envelope codec (`World.Save.Envelope.Codec`) to this codebase's
> concrete save components" — `Codec.hs` is the GENERIC codec, the parent is the
> CONCRETE binding, so it never purported to be a facade and its size follows from that
> role. The one plausible cohesion violation was checked and does not exist: legacy
> handling is split exactly as the haddock documents, with `Envelope.hs` owning the
> fallback DISPATCH ("every decode entry point below falls back to
> `decodeLegacySessionEnvelope`") and `World/Save/Compat/` owning payload MIGRATION
> (`SessionV90`, `MetadataV1`). Re-measured 2026-08-03: `Envelope.hs` is **905** beside
> **396** — 2.3×, not "four times"; and `Component.hs` (347) beside `Component/`
> (**3322**) is the INVERSE shape, a small parent with large children, i.e. the very
> facade layout this finding says is absent — not "same". The remaining convention
> complaint is answered by CH-56, which measured 69 such pairs and found both shapes
> deliberate, documented (CLAUDE.md:172), and CI-enforced; its forward of CH-78 as
> "where the layout genuinely costs something" does not survive inspection. NB: CH-76's
> note offers a B1/B2 rename "if CH-78's split is ever filed" — that ride-along will
> not occur.
`World/Save/Envelope.hs` (860) sits next to `Envelope/Types.hs` (173) and
`Envelope/Codec.hs` (223) — so the parent module is four times the size of the
subdirectory it appears to head, and is not a facade for it.

Same for `Component.hs` (347) beside `Component/` (2976 lines) and
`Snapshot.hs` (368) beside `Snapshot/Adapter.hs` (163). This is CH-56's
unstated convention gap, and this subsystem is where it bites hardest: given a
save concern, there is no rule saying whether it lives in `X.hs`,
`X/Types.hs`, or `X/Something.hs`.

---

## Batch 8 — the worldgen pipeline (swept 2026-07-25)

`World/Generate` (3926), `World/Geology` (8317), `World/Hydrology` (3276),
`World/Fluid` (4493), `World/Plate` (969), `World/Magma` (1395),
`World/Weather` (1332), `World/ZoomMap` (1018) plus the top-level `World/*.hs`.
The fluid surface is the densest and, as suspected, holds the most gaps.

### [#1108] CH-79. An abandoned river redesign is still compiled, plus a design doc that reads as current
> **Note:** Verified 2026-08-03. **Owner decision: ARCHIVE, not adopt** — #1108 removes
> the module and archives the doc. The 2026-07-25 correction below is itself HALF WRONG:
> tests do NOT exercise `RiverGraph`. The only external `RiverGraph` token is
> `Spec.hs:322`'s `RiverGraph.spec`, the test module's own qualified alias — a grep false
> positive. Measured per export: **7 of 9 have zero external references** (`RiverGraph`,
> `RiverRoute`, `RiverNodeId`, `RiverNode`, `NodeKind`, `buildRiverGraph`,
> `emptyRiverGraph`); only `classifyMouth` and the `SinkType` it returns are referenced,
> by one test. Dormancy dated: module created 2026-04-18, last substantive change
> 2026-06-26, every touch since mechanical (#435 warnings sweep, #950 pragmas);
> `docs/river_rework.md` added 2026-06-25 in one commit and never edited.
`src/World/River/Graph.hs` (257 lines) — `RiverGraph`, `RiverRoute`,
`RiverNode`, `buildRiverGraph`, `classifyMouth` — was listed in
`synarchy.cabal` and **imported by no production module**.

**Correction (2026-08-05):** the 2026-07-25 correction to this entry claimed
`test-headless/` exercised both `classifyMouth` and `RiverGraph`. Only the
first was true. `Spec.hs`'s `RiverGraph` is the test module's own qualified
import alias, not the type; `test-headless/Test/Headless/River/Graph.hs`
imported exactly `SinkType(..)` and `classifyMouth`. Seven of the nine exports
— `RiverGraph`, `RiverRoute`, `RiverNodeId`, `RiverNode`, `NodeKind`,
`buildRiverGraph`, `emptyRiverGraph` — had no reference outside the module
itself.

The finding stood in its narrower form: a fully-built model that no production
code path used, one classification function of it tested, kept compiling
indefinitely.

Its design brief, then at `docs/river_rework.md` (450 lines), opened "This
document describes a new river runtime model for Synarchy" and described the
current system as the thing to be replaced — with no status marker saying the
work stopped. `docs/history/README.md` explicitly moved it *up* out of the history
folder as "design reference, not a superseded audit", so a reader was told it
was live.

Resolved in #1108 by archiving rather than adopting: the brief moved to
`docs/history/river_rework.md` under a status marker stating it was not
adopted, `docs/history/README.md` now says the same, and the module, its test
module, their `synarchy.cabal` entries, the `Spec.hs` registration, and the
`src/World/River*` mapping in `tools/ci_expensive_gates.py` are all gone.
Nothing imported the module, so worldgen output is unchanged by construction.
Adopting the redesign instead would have been a worldgen rework needing its own
epic. CH-80 and CH-90 were out of scope there and still name the module as it
stood at their sweep.

### [#1109] CH-80. "River" logic lives in four unrelated namespaces
> **Note:** Verified 2026-08-03 — scatter confirmed and LARGER than the table says:
> `World/Fluid/River/` is 8 modules/1921 lines (not 7/1245), and
> `World/Hydrology/River/` is a 4-module directory/420 lines (not one `Carving.hs`/285);
> Timeline River+RiverTrace 9 modules/1125 ✓. The finding does not reach the concrete
> defect underneath: `src/World/Hydrology/DESIGN.md` (327 lines) — the ONLY design doc
> for this pipeline — is gitignored (`.gitignore:30`) and untracked, yet three tracked
> production modules cite it four times (`Chunk/Types.hs:122`, `Hydrology/Types.hs:64`,
> `Fluid/River.hs:7` and `:36`, the last naming "§5.4"). In a fresh clone all four
> dangle, and it is the only `.md` cited from `src/` at all. The local copy also
> self-declares "⚠️ STALE … Do NOT treat as the current state of the system" and
> redirects to agent memory notes that are not in the repo either. #1109 covers the
> tracked stage-boundary doc plus those four references; directory renaming is
> explicitly out of scope, as the finding itself allows.
| Path | Role |
|---|---|
| `World/Fluid/River/` (7 modules, 1245 lines) | identify rivers from terrain |
| `World/Hydrology/River/Carving.hs` (285) | carve river beds |
| `World/Geology/Timeline/River/` + `RiverTrace/` (9 modules, ~1200) | timeline-era river evolution |
| `World/River/Graph.hs` (257) | the abandoned model (CH-79) |

Plus `World/Hydrology/Simulation/Flow.hs` (396) doing flow accumulation. Nothing
names the split, so "where does river X live?" has no answer short of grepping
five trees. Ocean is similarly scattered across `World/Fluid/Ocean.hs`,
`World/Fluid/OceanMask.hs`, `World/Fluid/Lake/Identify/Ocean.hs`, and
`World/Ocean/Types.hs`.

Not necessarily a refactor — but the pipeline's stage boundaries (timeline
evolution → identification → carving → per-chunk composition → sim) should be
stated once, in a doc or module headers, and the directories named for them.

### [#1110] CH-81. `World.Fluids` and `World.Fluid.*` differ by one letter
> **Note:** Verified 2026-08-03. Severity OVERSTATED: `src/World/Fluid.hs` does not
> exist, so the confusable pair is `World.Fluids` vs `World.Fluid.<Sub>` and a typo
> either way is a module-not-found COMPILE ERROR, never a silent mis-resolution — it is
> a legibility problem, not a trap. Direct `World.Fluid.*` imports are **115 across 65
> files**, not 97. Two stronger arguments are missing from the finding: the shape is
> UNIQUE in the tree (zero singular/plural sibling `.hs` pairs anywhere under `src/`;
> `Fluids.hs` beside `Fluid/` is the only plural-facade-beside-singular-directory, which
> is why CH-56's facade disposition does not cover it), and two of the three importers
> (`BuildPixels.hs`, `Build.hs`) already import `World.Fluid.*` directly as well, so
> they reach the same subsystem two ways in one file. Only 2 of the facade's 11
> re-exported names are ever used through it. #1110 deletes rather than promotes —
> promoting would mean rewriting all 115 imports through an 11-name facade.
`src/World/Fluids.hs` is a 20-line facade re-exporting from `World.Fluid.*`.
`import World.Fluids` and `import World.Fluid.Types` sit one keystroke apart and
resolve to different modules.

The facade is also effectively bypassed: **3 modules import `World.Fluids`** (all
for the same two ocean predicates) against **97 imports of `World.Fluid.*`
directly**. Either promote it to the real entry point or delete it; as a
one-letter-different module used by 3% of consumers it is a trap with no payoff.

### [#1111] CH-82. The per-tile fluid-surface fold is written five times in one file
> **Note:** Verified 2026-08-03 — all five sites confirmed at the cited lines, and the
> load-bearing claim is confirmed by the code's OWN haddock (`:178-184`): the merge
> "mirrors `composeFluidMap`" and exists to stop "emitting lava into the water column".
> Two additions. The five are TWO shapes, not one: lake-keyed (`lakeSurfMap`,
> `lavaSurfMap`, `chunkWaterSurfMap`'s lake block — a single `lkSurface` per lake) and
> river-keyed (`riverSurfMap`, `chunkWaterSurfMap`'s river block — per-tile
> `rcePerTileSurfZ`), so `chunkWaterSurfMap` duplicates BOTH. And `chunkWaterSurfMap`
> has two production callers (`Chunk.hs:308`, `Chunk/Zoom.hs:193`), so a drift hits the
> main chunk path and the zoom path alike. The proposed fix carries a TRAP recorded in
> #1111: `chunkWaterSurfMap` folds lakes and rivers into ONE vector, and merging two
> separately-built vectors with `VU.zipWith min` would blank every tile — `minBound` is
> the absent sentinel AND the smallest `Int`; the current code is safe only because each
> write is bitmask-gated.
`World/Generate/Chunk/Fluid.hs` contains the same "fold this chunk's entries
into a per-tile lowest-wins surface vector" loop five times:

| Lines | Binding | Source table |
|---|---|---|
| 82-98 | `lakeSurfMap` | `gtWorldLakes` |
| 104-116 | `riverSurfMap` | `gtWorldRivers` |
| 123-136 | `lavaSurfMap` | `gtWorldLavaPools` |
| 191-199 | (in `chunkWaterSurfMap`) | `gtWorldLakes` again |
| 200-208 | (in `chunkWaterSurfMap`) | `gtWorldRivers` again |

Each is `VU.create` → `VUM.replicate chunkArea minBound` → `V.forM_
(lakesInChunk …)` → `forM_ [0 .. chunkArea-1]` → read/compare/write, with the
same defensive lowest-wins comment restated three times.

The duplication is load-bearing: `chunkWaterSurfMap` feeds the basalt-cap
decision and its own haddock says the merge "mirrors `composeFluidMap`". If the
two drift, a magma chamber's cap decision disagrees with where water actually
is — lava emitted into a lake.

One `perTileLowestSurface ∷ WorldLakes → ChunkCoord → VU.Vector Int` (plus a
river variant) replaces all five.

### [#1112] CH-83. The river-flat surface rule is written four times, and its comment overstates its own coverage
> **Note:** Verified 2026-08-03 — all four rule sites confirmed, and the `WeAddTile`
> safety argument holds exactly as argued (the `:132` guard makes `max` collapse to
> `fcSurface`). Three additions. There is a THIRD non-applying path the finding misses:
> `recomputeColumnSurface` (`Apply.hs:308-322`), reached via `WeSetCell`, same shape and
> no guard. Reachability is better than "narrow": `WeDeleteTile` is the MINING path
> (`Mine/Types.hs:10`, `Terrain.hs:48`) and `WeSetCell` the location-carving primitive
> (`Terrain.hs:199,216`) — what is narrow is the terrain precondition, not the edit. And
> the divergence PERSISTS: `mkSurfaceMap`'s comment ends "ChunkLoading uses
> mkSurfaceMap's output directly — no re-derivation", and a load regenerates chunks then
> replays edits, so a replayed `WeDeleteTile` re-applies the wrong value after every
> load. #1112 routes all seven sites through one shared rule.
`mkSurfaceMap`'s comment names the other copies:

> Same rule lives in Sim/Thread.hs::writeDirtyFluids (sim writeback) and
> World/Edit/Apply.hs::applyEdit (player edits).

Actual sites: `Generate/Chunk/Fluid.hs:520`, `Sim/Thread.hs:388`,
`Edit/Apply.hs:156`, `Edit/Apply.hs:169` — **four**, with `Edit/Apply` holding
two.

More importantly, `applyEdit` applies the rule in only 2 of its 4
fluid-touching branches:

- `WeSetFluidTile` (156) — applies it
- `WeSetFluidSnapshot` (169) — applies it
- `WeAddTile` (134-135) — uses `max newTopZ (fcSurface fc)`; **safe by
  construction**, because the guard above it (`newTopZ ≥ fcSurface fc → Nothing`)
  means the fluid only survives when `newTopZ < fcSurface`, so `max` equals the
  fluid surface anyway
- `WeDeleteTile` (77-78) — uses `max newTopZ (fcSurface fc)` with **no such
  guard**

The dig path is the gap. Digging preserves whatever fluid was there
(`Just _ → curFluid`), so on a River tile whose terrain protrudes above
`fcSurface` — which is precisely the case `mkSurfaceMap` exists to hide —
`newTopZ = oldTopZ - 1` can still exceed the river surface and `max` renders the
protrusion. Generation, sim writeback, and both `WeSetFluid*` paths would render
it flat.

Reachability is narrow (needs a protruding river tile that gets dug), so this is
an inconsistency to confirm-and-fix rather than a known visible bug. The durable
fix is one shared `renderedSurface ∷ Int → Maybe FluidCell → Int` used by all
four.

### [#1113] CH-84. `floorDivCS` is hand-rolled five times, with an unreachable branch, next to a correct helper
> **Note:** Verified 2026-08-03 — both substantive claims hold. The dead guard is
> confirmed EMPIRICALLY (`divMod (-33) 16 = (-3,15)`, `divMod (-1) 16 = (-1,15)`) with
> `chunkSize = 16` at `Chunk/Types.hs:95`, and `globalToChunk`
> (`Generate/Coordinates.hs:16-22`) uses plain `div`/`floorMod` with no guard, which
> independently corroborates it. The NAMING here is wrong in a way that matters: the five
> copies carry THREE names — `floorDivCS` (`Fluid.hs:288`), `fd` (`Pool.hs:170`), and
> `floorDiv` (`Field.hs:64`, `Init.hs:271` — not `:272` — and `Lookup.hs:31`) — so
> grepping `floorDivCS` finds one site and reads as stale. One nuance added: the three
> generic `floorDiv a b` copies are not dead by TYPE (a negative divisor would fire the
> guard), only because every call site passes `chunkSize`.
```haskell
floorDivCS a = let (q, r) = a `divMod` chunkSize
               in if r < 0 then q - 1 else q
```

appears verbatim in `World/Generate/Chunk/Fluid.hs:290`, `World/Magma/Pool.hs:171`,
`World/Magma/Field.hs:65`, `World/Magma/Init.hs:272`, and
`World/Magma/Lookup.hs:32`.

Two problems:

1. **The guard can never fire.** Haskell's `divMod` already floors — `div`
   rounds toward negative infinity and `mod` takes the sign of the divisor, so
   with `chunkSize > 0` the remainder is never negative. `divMod (-1) 16 =
   (-1, 15)`. The `r < 0` correction is dead in all five copies; it is the guard
   you would need with `quotRem`, pasted onto `divMod`.
2. **`World.Generate.Coordinates.globalToChunk` already exists** and does this
   correctly (`div` + an explicit `floorMod`). Five modules reimplemented it.

### [#1114] CH-85. `moSurface` is always empty, its lookup can never succeed, and two comments say it drives lava placement
> **Note:** Verified 2026-08-03 — confirmed, plus three additions. There is a THIRD
> construction site the field-name grep cannot see: `emptyMagmaOverlay = MagmaOverlay
> HM.empty HM.empty HM.empty` (`Overlay.hs:41`, positional). There is also a THIRD stale
> doc, and it is the worst one — the field's OWN haddock (`Overlay.hs:22-25`) still
> describes it as a live mechanism. And two constraints the finding omits, both of which
> shape the fix: `MagmaOverlay` is NOT serialized (`Show, Eq, Generic, NFData` only, and
> it lives on `LoadedChunk`, which regenerates on load), so removal is save-safe and
> needs no version bump; and the sibling field `moRevealed` is ALSO always empty but
> DELIBERATELY so ("Reserved for future dig integration. ALWAYS EMPTY in phase 1 + 2"),
> so it must survive a fix that removes `moSurface`. #384 touched this area and noted the
> overlay is "caps-only now" but never ruled on the field.
`MagmaOverlay.moSurface ∷ HM.HashMap (Int, Int) FluidCell` is written at exactly
two sites, both `HM.empty` (`Generate/Chunk/Fluid.hs:327`, `Magma/Init.hs:362`),
and read at one: `World/Magma/Lookup.hs:64`

```haskell
    case HM.lookup (gx, gy) (moSurface overlay) of
```

which therefore always misses. The field's own module acknowledges it —
`Generate/Chunk/Fluid.hs:174`: "the magma overlay's `moSurface` is no longer
populated (caps only)" — while two other comments still describe it as live:

- `World/Chunk/Types.hs:126` — "overlay's `moSurface` map drives lava placement in …"
- `World/Generate/Chunk.hs:301` — "decision: above water → lava cell in `@moSurface@`; sub-sea …"

So the codebase simultaneously documents this field as dead and as the mechanism
for surface lava. Surface lava actually comes from `gtWorldLavaPools`. Delete the
field, its lookup branch, and fix the two comments.

### [#1115] CH-86. `composeFluidMap`'s haddock documents a parameter it does not have
> **Note:** Verified 2026-08-03 — confirmed (signature at `:60-63` binds `params coord
> terrainMap`, no water-table arg). One correction that changes the fix: the paragraph's
> SUBSTANCE is still entirely true, only its subject is wrong. `lcWaterTableMap` really is
> still computed and stored on `LoadedChunk` (`Chunk/Types.hs:117`, populated at
> `Generate/Chunk.hs:69`, `Init.hs:308`, `Load/Stage.hs:339`, `Arena.hs:40,107`) and
> really is still read by the subsurface saturation query (`Hydrology/WaterTable.hs:93`
> `waterTableAtTile`). So #1115 re-anchors the explanation rather than deleting it —
> deleting three lines would discard a correct account of why the field still exists.
> Provenance: #384 dropped `_channelMask`/`_mMagma`/`oceanDist` from this same function;
> the paragraph outlived that cleanup.
```haskell
-- The 'waterTableMap' arg is no longer used for surface placement;
-- it stays computed and stored on 'LoadedChunk' so that the
-- subsurface dig path can still ask "is this buried tile saturated?"
composeFluidMap ∷ WorldGenParams → ChunkCoord → VU.Vector Int
                → V.Vector (Maybe FluidCell)
```

Three arguments, none of them a water-table map. The paragraph survived the
signature change that removed it.

### [#1116] CH-87. 43 modules carry `-fprof-auto`, defeating the cabal's `-fprof-late` profiling strategy
`synarchy.cabal` defines the profiling contract in one place:

```
flag profile
    description: Cost-centre profiling (-fprof-late on top of the prod -O2);
…
    if flag(profile)
      ghc-options: -fprof-late
```

and states the policy explicitly (#635): *"any change to profile options belongs
HERE, not in a component stanza."*

Yet **43 production modules** open with `{-# OPTIONS_GHC -fprof-auto #-}` —
essentially all of `World/Plate/`, `World/Geology/Timeline/`, `World/ZoomMap/`,
`World/Thread/`, plus `Sim/Thread.hs`, `World/Generate/Timeline*`,
`World/Hydrology/River/Carving.hs`, and `World/Fluid/Lake/Graben.hs`.

`-fprof-auto` and `-fprof-late` are not the same tool: `-fprof-auto` inserts
cost centres **before** optimisation (inhibiting it and changing the generated
code), which is exactly what `-fprof-late` exists to avoid. So the 43 modules
the profiling recipe targets — the worldgen hot path — are the only ones that
*don't* get the intended post-optimisation measurement.

They are inert in a non-profiling build (GHC ignores `-fprof-auto` without
`-prof`), so this costs nothing in production; it corrupts the profile, which
`docs/history/worldgen_timeline_profile_2026-07.md` is built on.

Fix: delete all 43 pragmas and let the flag do its job, or document why these
files need pre-optimisation instrumentation.

### [#1117] CH-88. Four dead bindings that `Strict` actually evaluates
> **Note:** Verified 2026-08-04. The load-bearing `Strict` claim was checked
> EXPERIMENTALLY, not assumed: a minimal `{-# LANGUAGE Strict #-}` program with one unused
> `let` binding calling `trace`, compiled at `-O2`, printed its trace — so the forcing
> survives optimisation. Cost confirmed (`isOceanChunk` = one `HS.member`,
> `Ocean.hs:157`; `hasAnyOceanFluid` = 25 lookups over the 5×5 Chebyshev-2 grid,
> `:160-169`), with one honest qualification: ≤104 per chunk is an UPPER BOUND — the `∨`
> short-circuits to 4 when the chunk is itself ocean. #1117 also covers CH-98's fifth
> binding, since `_wrapC` sits in the same `let` block (`:72`) and splitting nine lines
> across two PRs makes no sense — but `_wrapC` is a partial application of a 2-arity
> function, so it costs a PAP, NOT lookups; it is a legibility defect. CH-98 keeps its own
> disposition for its tree-wide sweep suggestion: `src/` holds 16 underscore-silenced
> local bindings, of which these five are one file's worth.
`World/ZoomMap/Cache/BuildPixels.hs:73-80` (a `{-# LANGUAGE Strict #-}` module):

```haskell
_chunkOceanN = isOceanChunk oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
            ∨ hasAnyOceanFluid worldSize oceanMap (wrapC (ChunkCoord ccx (ccy - 1)))
_chunkOceanS = …   _chunkOceanE = …   _chunkOceanW = …
```

All four are unused — the underscore prefix suppresses `-Wunused-binds` rather
than the code being removed. Under `Strict`, let-bindings are forced, so these
run for **every chunk** of every zoom-cache build.

They are not cheap: `hasAnyOceanFluid` scans a 5×5 chunk neighbourhood (25
wrapped `HashSet` lookups), so the four bindings cost up to ~104 lookups per
chunk that are discarded. The zoom cache is rebuilt per world init and load.

### [#1118] CH-89. Material IDs are a hardcoded Haskell table mirroring `data/materials/*.yaml`
> **Note:** Verified 2026-08-04 — every claim holds, including the exact count. Compared
> all 73 YAML entries against the Haskell table by BOTH id and name: zero mismatches, zero
> orphans either way, sole Haskell-only entry `matAir = MaterialId 0`. So it is a
> structural hazard, not a live defect. The unreferenced figure is **exactly 51** of 74
> (no consumer outside `Material.hs`). No gate exists: 14 `*_audit.py` tools ship, none
> for materials, and no hspec test asserts the mapping. One addition — the mapping is
> written in a THIRD place: the test suites hardcode bare ids (`WrapSeam.hs:110,125`,
> `CoastBreach.hs:42,83`, and `Spoil.hs:22`'s `granite = MaterialId 1`), which a YAML
> renumber would leave wrong while those tests still pass. #1118 takes the audit route
> rather than codegen: the repo has an established `*_audit.py` + `test_*_audit.py`
> pattern and no codegen step, and codegen would have to special-case `matAir` anyway.
`src/World/Material.hs` hardcodes 74 numeric ids:

```haskell
matGranite = MaterialId 1
matDiorite = MaterialId 2
matGabbro  = MaterialId 3
```

`data/materials/*.yaml` independently declares the same 73 materials with
explicit ids (`- id: 1 / name: granite`). Both tables run to id 255. Nothing
verifies they agree — no test, no audit tool.

They **are** in sync today (the only difference is `matAir = MaterialId 0`,
correctly absent from the content catalogue). The hazard is structural: a
designer renumbering or inserting a YAML material silently repoints every
Haskell constant above it, and **51 of the 74 constants have no Haskell
reference at all**, so a drift in those is invisible to every code path as well.

Same class as CH-31 (the five-way `16384`) but with content authors in the
loop. Fix: generate the constants from the YAML, or add a startup/CI check that
every `mat*` constant matches its YAML `id`/`name` pair.

### [#1119] CH-90. 194 unreferenced exports in `src/World/`
> **Note:** Verified 2026-08-04 — re-scanned with #1083's stated convention (value-level
> exports only; types/constructors and export-list-less modules excluded): **186 names
> across 58 modules**, and the concentrations match this table almost exactly (Material 51
> ✓, Vegetation 18 ✓, Volcano 7 ✓, Fluid/Internal 6 ✓, Strata 6 ✓; Magma/Init 7→6,
> Entities.hs **17→5**). Counting types and constructors as well gives 252, so 194 sits
> between the two conventions. BOTH "genuinely dead" cross-references are now stale:
> `World/Log.hs` no longer exists (#972 deleted it from disk and cabal) and
> `World/River/Graph.hs` is removed by #1108. One interaction worth recording:
> `Material.hs`'s 51 are a KEEP-WITH-REASON block, not an un-export block — #1118's audit
> pins them to `data/materials/*.yaml`, so #1119 retains them with that reason and the
> real edit surface is ~135 names across 57 modules.
Same scan as CH-54. Largest concentrations, and what they mean:

| Count | Module | Kind |
|---:|---|---|
| 51 | `World/Material.hs` | unreferenced id constants (see CH-89) |
| 18 | `World/Vegetation.hs` | veg id constants + `selectVegetation` (used internally) |
| 17 | `World/Save/Component/Entities.hs` | DTO `to*`/`from*`/`migrate*` reached only via their codecs |
| 7 | `World/Geology/Volcano.hs` | `applyCaldera`/`applyFissure`/… — dispatched internally by `applyVolcanicFeature`; only it and `perturbDist` need exporting |
| 7 | `World/Magma/Init.hs` | internal geometry helpers (`padBox`, `unionBoxes`, `squareAt`, …) |
| 6 | `World/Fluid/Internal.hs` | 6 of a 98-line module |
| 6 | `World/Generate/Strata.hs` | `applyDelta`, `applyEventDelta`, … |

Most are over-exports (kind (b) in CH-54) rather than dead code — verified for
the volcano and vegetation clusters, which are live internally. The effect is
the same: no module in the pipeline has a meaningful public surface, so nothing
can be refactored without a whole-tree grep.

Genuinely dead here: `World/Log.hs`'s five exports (CH-28 — the module is not
in the cabal at all) and `World/River/Graph.hs`'s three (CH-79).

### [#1131] CH-91. Minor worldgen defects for one cleanup issue
> **Note:** Verified 2026-08-05 — THREE of five bullets survive and are covered by #1131:
> the crossed index-builder prefixes (confirmed; 12 external refs, so contained), and both
> `smoothIslandColumns` items. Two are closed by precedent and excluded: the
> `X.hs`-beside-`X/` bullet (Material/Slope/Plate/Seabed) and `World/Ocean/Types.hs`'s
> 23-line namespace are the size/layout category CH-56 and CH-78 both dispositioned
> `no-issue` as discretionary churn, with the ocean half's actionable content owned by
> #1109. Two qualifications on the survivors: the "O(n²) dedup" is over a **≤4-element**
> list (≤16 comparisons) so it is legibility, NOT performance; and the aliasing fix is
> provably behaviour-preserving — `mTerr[idx]` is written only alongside
> `mFluid[idx] = Just …`, and the loop skips any tile whose fluid is already `Just _`, so
> a written tile is never re-read.
- **Parallel modules, swapped prefixes.** `Lake/Identify/ChunkIndex.hs` exports
  `buildChunkIndex` + `buildLakeCarveIndex`; the parallel
  `River/Identify/ChunkIndex.hs` exports `buildRiverChunkIndex` +
  `buildCarveDeltaIndex`. Each qualifies the opposite one of the pair.
- `World/Material.hs` (291 lines) sits beside `World/Material/` (one 35-line
  module) — CH-56's facade ambiguity again, as do `World/Slope.hs` (47) beside
  `World/Slope/` (807), `World/Plate.hs` (64) beside `World/Plate/` (969), and
  `World/Fluid/Seabed.hs` (423) beside `World/Fluid/Seabed/Types.hs`.
- `smoothIslandColumns` (`Chunk/Fluid.hs:447`) reads terrain from the immutable
  `terr` while writing to the mutable `mTerr` in the same loop. Safe today
  (smoothed tiles gain fluid and are skipped on later passes), but the mixed
  aliasing is a trap for the next edit.
- `smoothIslandColumns` hand-rolls a frequency count (`countBy`/`uniques`/`hits`)
  over a 4-element list with an O(n²) `foldr` dedup.
- `World/Ocean/Types.hs` (23 lines) is a whole namespace for `OceanMap`,
  `OceanDistMap`, and one accessor, while ocean *logic* lives in three other
  places (CH-80).

---

## Batch 9 — `World/Thread`, `World/Render`, `World/ZoomMap` (swept 2026-07-25)

5226 + 4453 + 1018 lines. **These three trees are the cleanest large area swept
so far**: only 11 unreferenced exports between them (against 194 in the rest of
`World/` and 97 in `Engine/`), `World/Thread.hs` and `World/Thread/Command.hs`
are a tidy loop + flat dispatch, and `World/Render.hs` is a proper two-export
facade. The findings below are correspondingly narrower.

### [#1132] CH-92. `baseTileW` / `baseTileH` are defined identically in eight modules
> **Note:** Verified 2026-08-05 — all sixteen definitions confirmed at the eight listed
> modules, every one `∷ Float` and textually identical. One OVERSTATEMENT corrected: "eight
> places to check when anything about tile geometry changes" is not right — all eight
> DERIVE from `defaultGridConfig`, so a config change propagates correctly and there is no
> latent correctness hazard. The real argument is stronger and comes from `World/Grid.hs`'s
> own header ("Changing the sprite size or proportions only requires editing
> `defaultGridConfig` — everything else is derived"): Grid already exports that derived
> family (`tileWidth`, `tileHeight`, `tileSideHeight`, `tileDiamondHeight`,
> `tileHalfWidth`, `chunkWorldWidth`) and ALL EIGHT modules already import from it, so
> these two are the only members living privately. They are NOT redundant with
> `tileWidth`/`tileHeight`, which are world-space (0.15) against these pixels (96/64) —
> the pair is used as a ratio at `FloraQuads:51-55`. The live defect is FloraQuads'
> `-- 96`/`-- 64` annotations, accurate today and silently stale on any config change.
```haskell
baseTileW = fromIntegral (gcTilePixelWidth  defaultGridConfig)
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)
```

Verbatim in: `Unit/HitTest.hs`, `Unit/Render.hs`, `Structure/Render.hs`,
`Building/Render.hs`, `Building/HitTest.hs`,
`World/Render/GroundItemQuads.hs`, `World/Render/BloodQuads.hs`,
`World/Render/FloraQuads.hs`.

These are *the* two constants of an isometric renderer — every quad, hit test,
and sprite placement derives from them. Eight private copies means eight places
to check when anything about tile geometry changes, and they are split across
four subsystems that would not obviously be searched together.

`World/Render/FloraQuads.hs:20` additionally annotates the derived values:

```haskell
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)   -- 96
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)  -- 64
```

A hardcoded comment of a config-derived number, in one copy of eight — it goes
stale the moment `defaultGridConfig` changes, and nothing will flag it.

Fix: export them once (`World.Grid` already owns `defaultGridConfig`).

### [#1133] CH-93. `World.ZoomMap` is a facade that inverts its own dependency direction
> **Note:** Verified 2026-08-05 — the inversion is real, and the facade is doing something
> WORSE than the finding describes. It has only three importers, each taking one name
> (`Render.hs:22` the inverted one; `Load/Stage.hs:51` and `Thread/Command/Init.hs:48` the
> cache one), and **two of its four re-exports have no consumer at all**. One of those,
> `generateBackgroundQuads`, means `World/Render/Zoom/Background.hs` — the whole
> background layer — is DEAD: the facade is its only importer anywhere, and
> `World/Render.hs` never mentions background. Git history shows a regression, not an
> abandoned experiment: #127 fixed a divide-by-zero in it while it still ran, the call was
> dropped during earlier visual/perf work, and `c89b0fca` (#435's `-Wall` sweep) then
> removed the dangling import from `Render.hs`. Because the facade still imports it, no
> unused-import warning has fired since — the shim is precisely why nobody noticed, and it
> also masks the module from #1119's scan. #1133 requires that be resolved as one decision
> (delete or re-wire), not swept.
```haskell
-- | Thin facade – re-exports the public entry points so that
--   existing call sites ('World.Render') need no import changes.
module World.ZoomMap
    ( generateZoomMapQuads, generateBackgroundQuads
    , buildZoomCache, buildZoomCacheWithPixels ) where

import World.ZoomMap.Cache          (buildZoomCache, buildZoomCacheWithPixels)
import World.Render.Zoom.Quads      (generateZoomMapQuads)
import World.Render.Zoom.Background (generateBackgroundQuads)
```

So `World.Render` imports `World.ZoomMap`, which imports `World.Render.Zoom.*`.
Half of what the `ZoomMap` facade exports is *rendering* code that lives under
`Render`. Its own haddock states the reason plainly: it exists "so that existing
call sites need no import changes" — an import-churn shim, kept permanently,
that now makes the module graph read backwards.

Fix: have `World.Render` import `World.Render.Zoom.*` directly and let
`World.ZoomMap` cover only the cache.

### [#1135] CH-94. Cross-chunk render lookups don't wrap at the world seam, but the chunk map is keyed wrapped
> **Note:** Verified 2026-08-06 — every code claim confirmed, and the seam confirmation
> this entry asked for is now SUPPLIED, arithmetically rather than visually: replicating
> `wrapChunkCoordU` (`Chunk/Types.hs:45-56`) over a 64-chunk world (canonical u range
> `[-32,32)`), at u=31 the stored chunk `(16,-15)` has its +u neighbour built as
> `(17,-15)` while that chunk is stored as `(-15,17)` — the `HM.lookup` cannot match. No
> GPU needed. `ChunkLoading.hs:78-84` states the violated contract outright ("so
> insert-time and lookup-time wrapping can't diverge"). The `SideDecoQuads.hs:83-87`
> comment is demonstrably false: it says Nothing happens "only when that neighbor chunk
> isn't loaded", but at the seam the neighbour IS loaded. Provenance: #26 added this
> cross-chunk lookup and **#423 fixed this exact bug class in this exact file** —
> prescribing `wrapChunkCoordU` canonicalisation and producing the `Quads.hs:394` wrap —
> but missed these two siblings. #1135 also requires `GroundItemQuads.hs:102` and
> `CursorQuads.hs:312` (same raw-lookup shape, input canonicality unestablished) be
> examined rather than assumed safe.
`World/Render/Quads.hs:82-85`:

```haskell
fluidMapLookup cc = case HM.lookup cc (wtdChunks tileData) of …
terrMapLookup  cc = case HM.lookup cc (wtdChunks tileData) of …
```

Raw `HM.lookup` on whatever coordinate the caller computed. Both callers step
one chunk outward without canonicalising:

- `World/Render/SideDecoQuads.hs:94-101` (`neighborCell`) — `ChunkCoord (cx±1) cy`
- `World/Render/WaterSlope.hs:38-45` (`waterSlopeAt`) — the same construction

But `wtdChunks` is keyed by `lcCoord`, and chunks are stored under the
canonical wrapped coordinate (`World/Thread/ChunkLoading.hs:78-82` applies
`wrapChunkCoordU`). And `World/Render/Quads.hs:394` — in the *same file* —
does wrap before looking up:

```haskell
cc@(ChunkCoord ccx ccy) = wrapChunkCoordU worldSize ccRaw
```

So at the u-axis wrap seam the neighbour lookup misses, both call sites take
their documented "neighbour chunk isn't loaded" conservative branch, and water
side faces and water slope tiles are silently not drawn along the seam. Both
comments describe that branch as covering an *unloaded* neighbour, which is not
the case here.

Needs confirmation at the seam before fixing (the memory note on wrap-seam
topology flags this exact hazard class), but the inconsistency inside one file
is plain either way.

### [#1222] CH-95. Two zoom namespaces with a real but unstated split
> **Deferral discharged (2026-08-07) — reopened for processing.** This entry was
> `[deferred]` on "**#1133 merges**, then state the boundary against the settled
> shape". #1133 has since merged (`34b9354f`, PR #1164) and its implementation
> commit `75f9224a` removed `src/World/ZoomMap.hs` and `Render/Zoom/Background.hs`,
> so the precondition is met and the module counts below are stale: `Render/Zoom/`
> is now 8 modules and `World/ZoomMap/` no longer has a facade. The finding
> therefore returns to unprocessed `[ ]` rather than staying deferred; it is not
> dispositioned here.

> **Note:** Verified 2026-08-06 — the split is real, but two of this entry's premises need
> correcting. "Nothing states it" is partly FALSE: `World/ZoomMap/Cache.hs:13-14` says
> "Pure world-generation logic – no rendering imports." And the proposed fix is
> inapplicable as written — `src/World/Render/Zoom.hs` DOES NOT EXIST, so the render tree
> has no top module to put a sentence in. Counts: `Render/Zoom/` is **9** modules, not 10.
> The `Bake.hs` example is weak — its haddock already reads "Bake `ZoomChunkEntry` vectors
> into render-ready `BakedZoomEntry` vectors." The substantive finding is one this entry
> misses: `ZoomChunkEntry`, the CACHE's output type built at `ZoomMap/Cache/Build.hs:109`,
> is defined in `World/Render/Zoom/Types.hs:36` and reaches the cache via
> `World/Types.hs:22`'s `module World.Render.Zoom.Types` re-export — so "no rendering
> imports" holds only because the dependency is laundered through `World.Types`. Whether
> that type moves is the real question, and it should be scoped once #1133 settles the
> tree's shape.
`World/ZoomMap/` (8 modules) builds the zoom **cache**: entries, per-chunk
pixels, the texture atlas, the colour palette, classification, ice noise.
`World/Render/Zoom/` (10 modules) **renders** from it: background, bake, quads,
cursor, icons, climate colours, view bounds.

That data-vs-render split is principled — but nothing states it, and the names
actively work against it: `ZoomMap/Cache/Pixels.hs` generates pixels while
`Render/Zoom/Bake.hs` bakes entries; both read as preparation. Combined with
CH-93's inverted facade, "where does zoom concern X live?" has no answerable
rule.

One sentence in each tree's top module would fix it.

### [#1136] CH-96. `docs/history/README.md` justifies an archive with a false claim
> **Note:** Verified 2026-08-06 — confirmed. `waterSideFaceQuads` is live
> (`SideDecoQuads.hs:3,30,45`, imported `Quads.hs:32`, called `:234`), while
> `drainOceanLakes` really is gone and the bullet's OTHER claim — "the side-face subsystem
> now lives in `src/World/SideFace/`" — is also TRUE (`Base.hs`/`Compute.hs`/`Types.hs`).
> So the bullet is two-thirds right. Likely cause, which makes the correction more useful:
> the side-face COMPUTE moved to `World/SideFace/` while the RENDER entry point stayed
> under `World/Render/`, so the function looked like it had moved with the subsystem. This
> entry's reasoning holds — the audit's complaint was fixed by `neighborCell`'s cross-chunk
> resolution (`:83-101`) — but the corrected note must not overcorrect: that resolution
> still fails at the U seam (#1135). The error is durable: `b4004a12` (an explicit
> "archive-accuracy" review pass) and `29748160` (#1108, merged 2026-08-05) each edited
> this file and left it standing.
It marks the 2026-04 fluid audit superseded because it

> References functions that no longer exist (`drainOceanLakes`,
> `waterSideFaceQuads`).

`waterSideFaceQuads` **does** exist and is live —
`World/Render/SideDecoQuads.hs:30`, imported and called from
`World/Render/Quads.hs:234`.

What actually happened is better news: the audit's specific complaint
("`waterSideFaceQuads` only checks in-chunk neighbors… water cliffs at chunk
boundaries don't get side faces rendered") was **fixed** — `neighborCell` now
resolves across chunk boundaries and says so in a comment. So the conclusion
(superseded) is right and the stated reason is wrong.

This matters because the README is the document telling future readers whether
an archived audit's findings are still live. A wrong reason here is how a
still-open finding gets dismissed; note that this same function retains the
separate seam defect in CH-94.

### [no-issue] CH-97. Duplicate module basenames across the render stack
> **Disposition:** No issue — duplicate basenames are this codebase's dominant convention,
> not a render-stack anomaly. Measured across `src/`: **119 of 412 distinct basenames are
> duplicated, spanning 408 modules** — roughly half the tree. `Types` alone appears **79**
> times, `Render` 13, `Thread` 7, `Query`/`Config`/`Base` 6 each,
> `Manager`/`Constants`/`Common`/`Camera`/`Texture` 5 each. The four clusters listed here
> are ordinary instances of hierarchical namespacing with short leaf names, which is
> standard Haskell practice and what this tree does everywhere. The proposed remedy
> ("prefix by role: `ZoomViewBounds`, `RenderCamera`") applied consistently would rename a
> large share of those 408 modules and contradict the convention — nobody would rename
> `Unit/Types.hs` to `UnitTypes.hs`. The counts are also wrong: there are **5** `Camera.hs`
> files, not 4 (it misses `Lua/API/Camera.hs` and `Lua/API/Register/Camera.hs`), so its
> headline cluster is neither complete nor the notable one. And the stated harm is weak —
> GHC diagnostics and stack traces identify modules by full module name
> (`World.Render.Camera`), not by file basename. The entry itself concedes "Not all are
> wrong." Same category as CH-22, CH-23, CH-41, CH-42, CH-43, CH-56, CH-74, and CH-78.
| Basename | Modules |
|---|---|
| `Camera` | `Engine/Graphics/Camera.hs`, `Engine/Loop/Camera.hs`, `World/Render/Camera.hs`, `World/Render/Camera/Types.hs` |
| `Textures` | `World/Render/Textures.hs`, `World/Render/Textures/Types.hs`, `World/Render/Zoom/Textures.hs` |
| `ViewBounds` | `World/Render/ViewBounds.hs`, `World/Render/Zoom/ViewBounds.hs` |
| `Quads` | `World/Render/Quads.hs`, `World/Render/Zoom/Quads.hs` |

Four distinct `Camera` modules across two subsystems. In a codebase where most
imports are unqualified, a stack trace, a grep hit, or an editor tab labelled
`Camera.hs` is ambiguous four ways.

Not all are wrong — `Zoom/ViewBounds` genuinely differs from `ViewBounds` — but
the pattern deserves one pass with a naming rule (prefix by role:
`ZoomViewBounds`, `RenderCamera`, …).

### [#1137] CH-98. A fifth dead binding in `BuildPixels.hs` (extends CH-88)
> **Note:** Verified 2026-08-06 — this entry has two halves and both are now covered. The
> `_wrapC` binding itself rides in **#1117**, folded there when CH-88 was processed because
> splitting nine adjacent lines across two PRs made no sense. The "broader sweep" half is
> **#1137**, and I ran the sweep: **16** underscore-silenced local bindings in `src/` — 5 in
> BuildPixels (→ #1117) and 11 elsewhere, of which **9 are genuinely dead** and **2 are
> NOT**. `_debugLandCount` and `_debugGridW` (`Timeline/Loop.hs:337-338`) are USED three
> lines below at `:341-343`, formatted into the geo-period label — so this entry's proposed
> grep produces false positives, and the underscore prefix is also a legitimate naming
> convention for debug values. Unlike CH-88's, the nine dead ones are all CHEAP (field
> reads, arithmetic, an O(1) length, an index, one hash), so the case is code health — dead
> code hidden from a `-Werror` build — not performance.
`World/ZoomMap/Cache/BuildPixels.hs:72`:

```haskell
_wrapC = wrapChunkCoordU worldSize
```

Unused — the `_chunkOcean*` lines beneath it call `wrapC`, a *different*
binding defined at line 235. So the module carries five underscore-silenced
dead bindings (`_wrapC` plus the four `_chunkOcean*` of CH-88), all forced
under `{-# LANGUAGE Strict #-}`.

The underscore prefix is doing real damage here: it converts "GHC will tell you
this is dead" into "this is deliberate", and five accumulated in one file.

Worth a broader sweep: `grep -rn "^\s*_[a-z]" src --include='*.hs'` for other
underscore-silenced bindings that are dead rather than intentionally ignored.

### [#1138] CH-99. Minor Thread/Render/ZoomMap defects for one cleanup issue
> **Note:** Verified 2026-08-06 — bullets 1 and 2 are ALREADY COVERED and are not new work:
> all twelve named exports are confirmed unreferenced outside their own module, sit in
> explicit export lists, and live under `src/World/`, which is exactly #1119's inventory
> (`emitQuadBg` additionally rides on #1133's decision about `Zoom/Background.hs`).
> Bullet 3 is real but reframed by #1138. "Nine producers share no common entry shape"
> overstates — seven top-level producers already take 1-2 arguments. The defect is TWO
> 14-parameter producers with same-typed positional runs disambiguated only by comment:
> `TileQuads.tileToQuad` (`Int → Int → Int` worldX/worldY/worldZ, plus `Int → Int` and
> `Float → Float`) and `SideDecoQuads.waterSideFaceQuads` (`Int → Int`, `Float → Float`).
> Swapping `worldX`/`worldY` compiles and silently misplaces every tile — the CH-66/CH-77
> class already filed as #1081 and #1103.
- Only 11 unreferenced exports across all three trees, but they cluster in the
  zoom render path: `Zoom/Bake.hs` (`bakeEntries`, `zoomQuadWorldUVs`),
  `Zoom/Cursor.hs` (`makeHoverQuad`, `makeSelectQuad`), `Zoom/Quads.hs`
  (`emitQuad`, `makeMapQuads`), `Zoom/Background.hs` (`emitQuadBg`),
  `ZoomMap/ColorPalette.hs` (`emptyColorPalette`, `lookupVegColor`),
  `ZoomMap/ChunkTexture.hs` (`chunkAtlasUVs`). All over-exports (kind (b) in
  CH-54) — the zoom facade already narrows the real surface to four names.
- `World/Thread/ChunkLoading.hs` exports `maxChunksPerTick` (a tuning constant)
  that only it reads; `World/Render/Camera.hs` exports `quadCacheMarginFrac`
  the same way. Both belong in a config or stay private.
- Nine `*Quads.hs` producers (`TileQuads`, `CursorQuads`, `Quads`,
  `BloodQuads`, `GroundItemQuads`, `SideDecoQuads`, `SpoilQuads`,
  `FloraQuads`, `Zoom/Quads`) share no common entry shape — some take
  `EngineEnv → WorldState → Float → IO (V.Vector SortableQuad)`, others take
  eight-plus positional arguments including two lookup functions, a
  `CameraFacing`, `zSlice`/`effectiveDepth`, `tileAlpha`/`xOffset`, and a
  `ViewBounds`. `waterSideFaceQuads` alone takes 14 parameters. A shared
  `QuadContext` record would collapse the repeated tail across all nine.

---

## Batch 10 — `src/Unit`, `src/Combat` (swept 2026-07-25)

6688 + 3069 lines. **The best-documented area swept so far.** Only 17
unreferenced exports and 2 review-round comments between them; the
`Unit.Injury` shared-physics boundary is explicit and correct ("the single
place both damage systems funnel through … used by BOTH `Combat.Resolution`
and `Unit.Fall`"); fall physics constants are defined once and derived, not
copied; and the save-critical enums carry accurate append-only warnings at
every declaration site.

The findings are therefore few but sharp — the first is the most consequential
single defect in the audit so far.

### [#1139] CH-100. The save-critical enums tell you to bump the wrong version, and CLAUDE.md agrees with them
> **Note:** Verified 2026-08-06 — every element confirmed, and the DECISIVE evidence is one
> this entry does not cite: `currentSaveVersion`'s OWN haddock
> (`World/Save/Types.hs:101-107`) already says it is "a developer-maintained bookkeeping
> marker … **It does not govern on-disk save compatibility**". So the constant the three
> comments send you to bump documents at its definition that bumping it achieves nothing
> here. Confirmed: the three comments verbatim; all three enums inside `UnitSimStateDTO`
> (`Entities.hs:516-525`); `unitSimCodec` at `ccVersion = 2` / `ccInputVers = [1,2]`;
> the failure path (bump 92→93, v2 payloads still accepted, tags remapped silently).
> CLAUDE.md's self-contradiction is at **`:1113`** vs **`:1135`** — **22** lines apart, not
> 738/755/17; the second is blunter than quoted ("is bumped freely — don't trust any number
> written in docs"). And the correct mitigation has a working exemplar in the SAME codec:
> `unitSimCodec`'s v1→v2 path decodes the frozen `UnitSimDTOv1` (`:640`) via
> `migrateUnitSimDTOv1` (`:655`). #1139 also requires the append-only warnings themselves
> be PRESERVED — they are correct, and only the mitigation clause is wrong.
`Direction`, `Pose`, and `UnitActivity` are positional-by-constructor-tag under
`Generic Serialize`. All three carry a clear, correct append-only warning — and
all three end with the same mitigation instruction:

> `Unit/Direction.hs:22` — "If the geometry ever needs different cardinality
> (16-way etc.), bump `currentSaveVersion` in `World.Save.Types`."
>
> `Unit/Sim/Types.hs:150` — "If the pose set legitimately needs to change,
> bump `currentSaveVersion` in `World.Save.Types`."
>
> `Unit/Sim/Types.hs:181` — "New activities go at the end; replacements bump
> `currentSaveVersion`."

**That instruction no longer does anything.** All three enums are serialized
inside `UnitSimStateDTO` (`simPose`, `simState`, `simFacing`,
`simPostTransition` — `World/Save/Component/Entities.hs:517-526`), which is the
**`unit-sim` component**, versioned by its own `ccVersion = 2` /
`ccInputVers = [1, 2]`. Since the #756-#768 overhaul, compatibility is gated
per component; `currentSaveVersion` versions only the transitional in-memory
`SaveData` bridge and, per CLAUDE.md, "is bumped freely".

So a developer who follows the instruction *correctly* — reorders a `Pose`
constructor and dutifully bumps `currentSaveVersion` — ships a change that
silently remaps every saved unit's pose, because `unitSimCodec` still
advertises version 2 and still accepts version 2 payloads. Nothing rejects the
old data; it is decoded against the new tag order.

**CLAUDE.md contradicts itself on this, 17 lines apart:**

- line 738 (Enum schema policy): "anything beyond appending requires a
  `currentSaveVersion` bump"
- line 755 (Architecture): "Component evolution = per-component schema version
  bumps + explicit migrations from frozen vN DTOs — **NOT a global save-version
  bump**"

Four stale instructions (three source comments plus CLAUDE.md's enum-policy
line), all pointing at a lever that is no longer connected.

Fix: correct all four to name the owning component's `ccVersion`/`ccInputVers`
plus a migration from a frozen DTO. See also CH-101.

### [no-issue] CH-101. Two components store the same enum two different ways; only one is order-safe
> **Disposition:** No issue — the premise is a misreading. The two DTO fields are not
> competing designs for one value; each faithfully mirrors a DIFFERENT source record, which
> is exactly what `Entities.hs`'s own haddock requires ("EVERY evolving live gameplay record
> … is mirrored by a component-owned DTO with an explicit, reviewable field-by-field
> conversion"). `UnitSimState.usPose ∷ Pose` (`Unit/Sim/Types.hs:31`) is authoritative;
> `UnitInstance.uiPose ∷ Text` (`Unit/Types/Instance.hs:58`) is its published tag —
> `Unit/Thread.hs:228` sets `uiPose = poseTag (usPose ss)` — consumed as a string by render,
> Lua (`parsePose`), and healing (`uiPose == "sleeping"`). Three specific errors: the
> proposed fix would make the `unit-sim` DTO stop mirroring its record AND add a fallible
> `Text → Pose` parse; "one of the two already solved the problem" is false because
> `Direction` is stored as an ENUM in both components (`uidFacing` `:377`, `simFacing`
> `:519`), so no name-based alternative exists for facing at all; and the Text side is not
> safer — `uiPose ∷ Text` accepts any string with no compile-time check, trading a
> reordering hazard for a spelling one. The genuine positional hazard is CH-100's, filed as
> #1139.
In the same file, `World/Save/Component/Entities.hs`:

```haskell
-- units component
    , uidPose           ∷ !Text        -- line 382
-- unit-sim component
    , simPose             ∷ !Pose      -- line 517
```

The `units` component stores pose **by name** — order-independent, immune to
constructor reordering, self-describing in a hex dump. The `unit-sim` component
stores the **enum**, inheriting the full positional hazard of CH-100.

One of the two already solved the problem. Nothing records that the divergence
is deliberate, and a reader comparing the two DTOs would reasonably conclude
either style is fine.

Fix: make `unit-sim` store names too (with a v2→v3 migration), or document why
the sim path must stay positional.

### [#1144] CH-102. The codebase's only `TODO` is a comment claiming TODOs exist
> **Note:** Verified 2026-08-06 — both stated claims confirmed (exactly **1** `TODO` in
> `src/`+`app/`, and it is this sentence; `stepCost` takes five args at `:77` with no
> modifier, and `AStar.hs:112` / `PathAdvance.hs:209` each pass exactly those five). A
> THIRD inaccuracy is the one that would actually mislead an implementer: "These can be
> added by widening the function signature" is contradicted by the extension that already
> shipped — material scaling needed NO signature change, via `materialFactor`
> (`Cost.hs:249`) derived from `MaterialRegistry`/`WorldTileData` (already parameters) plus
> the `pcMaterialReplanMargin` config scalar. `Config.hs:12-15` documents that mechanism
> and says the opposite of this comment ("appended as new fields … WITHOUT reshaping the
> call sites"). #1144 also requires the useful half — weather and per-unit as intended
> extension points — be preserved. NB: `materialFactor` is a FUNCTION, not a
> `PathingConfig` field; I assumed otherwise and corrected it on checking.
`src/Unit/Pathing/Cost.hs:30` is the sole occurrence of the string `TODO` in
all of `src/` and `app/` — a genuinely clean discipline. It reads:

```haskell
-- Future extension points (left as TODOs in the code, NOT plumbed yet):
--   * Weather modifier (snow/rain slowing units)
--   * Per-unit modifier (heavy armor slower, light units faster)
--
-- These can be added by widening the function signature; call sites
-- pass placeholder modifiers of 1.0 today.
```

Both factual claims are false:

1. **There are no TODOs in the code.** This comment is the only occurrence.
2. **No call site passes a placeholder modifier.** `stepCost` is
   `PathingConfig → MaterialRegistry → WorldTileData → (Int,Int) → (Int,Int) →
   Maybe Float` — it takes no modifier argument at all, and neither
   `AStar.hs:112` nor `PathAdvance.hs:209` passes one.

A reader looking for the placeholder to fill in will not find it.

### [no-issue] CH-103. `Unit.Types.Combat` holds anatomy, not combat
> **Disposition:** No issue — the premise is inaccurate and the proposed rename would be
> worse. All four exported types are combat-scoped, including the one that sounds
> anatomical: `BodyPart`'s haddock defines it as "a TARGETABLE body part" whose "numeric
> fields feed the RESOLUTION formulas" — `area_weight` is hit probability,
> `tactical_value` is picker bias, `bleed_factor` is the per-wound bleed rate,
> `height_low/high` drives the reach-band filter, `vital` is instant death; `bpName` is the
> "display name for the combat log"; `bpLayers` is what "a strike penetrates"; and its
> fields cite `Combat.Wounds.propagateSevering`, `Combat.Resolution`, and
> `Unit.Fall.fallInjuries`. `NaturalWeapon`/`StrikeProfile`/`NaturalResistance` are
> plainly combat data. So the module holds COMBAT body/weapon records, which is exactly
> what its own haddock says and what `Unit/Types.hs:7` documents; renaming it to
> `Unit.Types.Body`/`.Anatomy` would misdescribe it, since no general anatomy model exists.
> The `Unit/Fall.hs` vs `Unit/Thread/Movement/Fall.hs` paragraph is self-identified as
> CH-97's class, which closed `no-issue`: `Fall` appears twice and `Combat` three times,
> unremarkable against the 119 duplicated basenames across 408 modules measured there.
It defines `BodyPart`, `NaturalWeapon`, `StrikeProfile`, `NaturalResistance` —
the unit's body composition and innate attack data. `Combat.Types` (a separate
tree) defines `AttackMode`, `CombatCommand`, `CombatEvent` — the combat
system's commands and events.

Both are reasonable modules; the name `Unit.Types.Combat` for "unit anatomy"
sends you to the wrong one. `Unit.Types.Body` (or `.Anatomy`) says what it
holds and stops colliding with the `Combat` tree.

Related basename collision, same class as CH-97: `Unit/Fall.hs` (fall *injury*
physics — energy, fractures, concussion) versus
`Unit/Thread/Movement/Fall.hs` (fall *motion* — z-interpolation, duration,
initiation). Genuinely different concerns, identical basename.

### [#1145] CH-104. The append-only enum policy is unenforced, in a codebase full of enforcement
> **Note:** Verified 2026-08-06 — the concern is right but "unenforced" is OVERSTATED, and
> the scope is wider. It is PARTIALLY enforced: the manifest-driven compat gate
> (`Test.Headless.World.Save.Compat`, blocking) records enum values BY NAME in its
> canonical summaries (`unitSimStates[0].pose = 'Standing'`, `.state = 'Idle'`,
> `.facing = 'DirS'`), so a reorder disturbing a fixture-carried constructor fails today.
> Coverage measured across every `*.expected.json` is thin: **`Pose` 1 of 8** (`Standing`),
> **`UnitActivity` 2 of 7** (`Idle`, `Walking`), **`Direction` 1 of 8** (`DirS`) — so a
> reorder confined to e.g. `Climbing`/`Falling` or `Drinking`/`Eating` passes every gate.
> Scope: at least TWO more persisted positional enums exist and carry NO append-only
> warning at all — `BillMode` (`Craft/Bills.hs:64`, in `CraftBillDTO.bilMode`) and
> `PowerRole` (`Power/Types.hs:46`, a power-nodes leaf). `save_compat_audit.py`'s frozen-DTO
> fingerprint does not reach them (it walks `Compat/SessionV90.hs` and the
> `World.Save.Component.*` leaf DTOs, not `src/Unit/`). #1145 adds the golden-list audit and
> records the existing partial coverage so it is not duplicated.
No tool, test, or CI step checks that `Direction`/`Pose`/`UnitActivity` (or any
other `Generic Serialize` enum) stays append-only. Compare what *is* automated:
the persistence-inventory audit, EngineEnv-capability audit, save-compat audit,
Haskell and Lua module budgets, probe CI-eligibility, and action-outcome
coverage — each with its own self-test.

This is the highest-consequence silent-corruption rule the project has, it is
guarded only by prose, and CH-100 shows the prose has already drifted.

A guard is cheap: parse the named enums' constructor lists, compare against a
checked-in golden file, and fail on any change that is not a pure append —
exactly the shape `tools/engine_env_capability_audit.py` already uses.

### [#1146] CH-105. Minor Unit/Combat defects for one cleanup issue
> **Note:** Verified 2026-08-06 — TWO of five bullets survive into #1146 (the
> `gravity`/`metresPerZ` home, confirmed at `Unit/Fall.hs:62,65` with importers
> `Movement/Types.hs:18` and `Movement/Leap.hs:18`; and the unreferenced exports, measured
> at **19** not 17). Three are excluded. Bullet 2 is self-declared "deliberate and
> documented (#593)". Bullet 3 is the naming/layout category closed ten times here
> (CH-22/23/41/42/43/56/74/78/97/103), and its counts are off — `Resolution/Constants.hs`
> has **27** exports, not 24. Bullet 5 is **REFUTED**: `synarchy.cabal:85,100` enable
> `-Wincomplete-patterns` with `-Werror`, so `poseDepth` being a total function means a
> missing clause for a new `Pose` is a BUILD FAILURE — the compiler already enforces the
> coupling the bullet calls unenforced, the opposite of the tag-order hazard (#1145).
> Bullet 4's distribution has also moved: `Unit/Transfer.hs` (**5**, from the transfer arc)
> is now the largest cluster and is unmentioned; `Unit/Injury.hs` dropped 8→**3**; and
> `Combat/Thread.hs`'s `combatTickRate` has gained a consumer.
- **Shared physics constants live in the injury module.** `gravity` and
  `metresPerZ` are defined in `Unit/Fall.hs` (the *injury* model) and imported
  by `Unit/Thread/Movement/Types.hs` and `Unit/Thread/Movement/Leap.hs` (the
  *motion* system), so every motion module depends on the injury model for
  gravity. The sharing itself is correct and good; the constants belong in a
  neutral module.
- **Test-mode rates threaded through the production tick.**
  `testInfectionBaseRate` / `testInfectionGraceSec`
  (`Combat/Wounds/Infection.hs`) are selected by an env-var check inside
  `tickAllWounds` (`Combat/Wounds/Tick.hs:80`) and threaded as an extra
  positional `Bool` through `tickOneUnit`. Deliberate and documented (#593),
  but the same pattern as `SystemError`'s `TestError` (CH-11): test scaffolding
  on a production hot path.
- **Two `Constants` modules that aren't the only home for constants.**
  `Combat/Resolution/Constants.hs` (24 exports) and
  `Combat/Wounds/Constants.hs` (7) exist, yet `Wounds/Infection.hs` defines 9
  more tuning values inline and `Wounds/Bleed.hs` 10. All are named and
  well-commented — the issue is only that "where is the bleed/infection
  tuning?" has two plausible answers.
- **17 unreferenced exports**, almost all over-exported tuning constants and
  internal helpers: `Unit/Injury.hs` (8 — `bluntAbsorbScale`, `bruiseCap`,
  `cutAbsorbScale`, `layerAbsorb`, `maxInjurySeverity`, `weightedPick`, …),
  `Combat/Resolution/Damage.hs` (3), `Unit/Pathing/Cost.hs` (3 lookup
  helpers), `Combat/Thread.hs` (`combatTickRate`), `Unit/Stats.hs`
  (`boxMuller`).
- `poseDepth` (`Unit/Sim/Types.hs:159`) is a second hand-maintained ordering of
  `Pose`'s constructors, independent of the serialization tag order. Both must
  be updated when a pose is added, for different reasons, and only one of them
  corrupts saves if you get it wrong.

---

## Batch 11 — `src/Sim`, `src/Power`, `src/Infection`, `src/Craft` (swept 2026-07-25)

1050 + 586 + 60 + 597 lines. Small, tidy trees — **zero** unreferenced exports
between them (every export has a production or test consumer), `Power/Network.hs` is thoroughly documented, and the
sim-vs-worldgen fluid boundary is clean (shared `FluidType`/`FluidCell` with
explicit `fluidCellToActive`/`activeToFluidCell` conversions rather than a
parallel type). Three findings reach beyond these trees.

### [#1147] CH-106. Six worker threads hand-implement one identical lifecycle
> **Note:** Verified 2026-08-06 — confirmed: six `start*Thread ∷ EngineEnv → IO
> ThreadState`, the stack-growth comment present in exactly those six files, and
> `Engine/Core/Thread.hs` is 36 lines holding only `ThreadState`/`ThreadControl`/
> `shutdownThread`, so the shutdown-without-startup asymmetry is exactly as described.
> Three corrections. The bare `error` IS in all six, but Lua's reads `"Lua thread failed to
> start."` (`:138`), so a grep on the finding's string finds only five. "All built the same
> way" oversimplifies: the threads share a SHAPE, not text — each does its own setup inside
> the `catch`, and the **Lua thread is a genuine outlier** whose startup creates the backend
> state, registers the Lua API, sets up the shell sandbox, and loads `scripts/init.lua`, so
> a shared helper must take a per-thread startup ACTION, not just a per-tick body. The
> template snippet is also not literal (it shows `ccLoggerRef (toCoreCapability env)`;
> `Unit/Thread.hs:39` uses `readIORef (loggerRef env)`).
`startUnitThread`, `startCombatThread`, `startWorldThread`, `startSimThread`,
`startLuaThread`, `startInputThread` — all `EngineEnv → IO ThreadState`, all
built the same way:

```haskell
startXThread env = do
    logger ← readIORef (ccLoggerRef (toCoreCapability env))
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch
        (do logInfo logger CatX "Starting X thread..."
            tid ← forkIO $ xLoop env stateRef `finally` putMVar doneVar ()
            return tid)
        (\(e ∷ SomeException) → do
            logError logger CatX $ "Failed starting X thread: " <> T.pack (show e)
            error "X thread start failure.")
    return $ ThreadState stateRef threadId doneVar
```

and each `xLoop` repeats the same `ThreadStopped` / `ThreadPaused`
(`threadDelay 100000`) / `ThreadRunning` case, the same `captureLocked` save-
barrier gate, the same `acknowledgeCurrent`, and — verbatim in all six — this
comment:

> One guarded tick per iteration; the recursive call lives OUTSIDE the catch —
> inside it, each tick pushes a catch frame that never pops (unbounded stack
> growth).

That comment records a real bug that was fixed once and must now stay fixed in
six places independently. `Engine.Core.Thread` already exists and owns
`ThreadState`, `ThreadControl`, and `shutdownThread` — it owns *shutdown* but
not *startup*, and that asymmetry is the whole finding. A
`runWorkerThread ∷ WorkerSpec → EngineEnv → IO ThreadState` would leave each
thread with only its per-tick body.

Sub-point worth its own line: all six fail with `error "X thread start
failure."` — a bare `error` call in the engine's startup path, six times, in a
codebase that has an `EngineException` hierarchy (CH-10) and a
`guardNativeExceptions` boot wrapper (CH-61).

### [no-issue] CH-107. 22 directories exist solely to hold a single `Types.hs`
> **Disposition:** No issue — the proposed flatten would make naming worse, not better, and the finding's own exemplar is gone. The count holds (27 dirs whose only `.hs` is `Types.hs`, 5 with a sibling, **22** without) but the membership drifted: `Engine/Scripting/Types.hs` was deleted by `822e29b4` (2026-08-02, item 3 of closed #1059) and `src/Tutorial/Types.hs` (145 lines) — the second-largest — is missing from the table, making the single-file top-level namespaces four, not three. That one fold also disproves "every one could be `X.hs`": it was a content merge, and its commit had to declare the fields explicitly lazy because the destination enables `StrictData` while the former home did not, so even the 14-line case was not a free move. `.Types` is a documented tree-wide convention rather than a directory artefact — 82 `.Types` modules in `synarchy.cabal`, and 52 of the 79 `Types.hs` files sit beside other `.hs` files (CLAUDE.md:137-138, the Base/Types split). Flattening deletes the only disambiguator: `Infection.Types` → `Infection` collides in meaning with `Combat.Wounds.Infection`, and likewise `World.Ocean.Types` vs `World.Fluid.Ocean`, `World.{Chop,Till,Plant}.Types` vs `World.Thread.Command.Cursor.*` and `Engine.Scripting.Lua.API.*`, `World.Chunk.Types` vs `World.Generate.Chunk`, `World.Command.Types` vs `World.Thread.Command`, `World.Cursor.Types` vs `World.Thread.Cursor`, `World.Page.Types` vs `World.Save.Component.Page`. Cost is 448 references across 248 files plus 22 cabal entries — one review round short of the >300-file cap that makes a PR unreviewable — with no defect behind it, and none of the 22 is dead (the three least-referenced, `World.Texture.Types`/`Sim.State.Types`/`World.Region.Types`, all have live importers). CH-56 is the mirror image, closed for the same reason.

A directory `X/` containing nothing but `Types.hs`, with **no sibling `X.hs`**:

| Lines | Path |
|---:|---|
| 330 | `World/Command/Types.hs` |
| 299 | `World/State/Types.hs` |
| 193 | `Unit/Sim/Types.hs` |
| 144 | `World/Chunk/Types.hs` |
| 118 | `World/Cursor/Types.hs` |
| 99 | `World/Tile/Types.hs` |
| 88 | `Unit/Command/Types.hs` |
| 64 | `Sim/State/Types.hs`, `Sim/Command/Types.hs` |
| 60 | `Infection/Types.hs` |
| 57 | `World/Texture/Types.hs` |
| 52 | `World/Region/Types.hs` |
| 51 | `Substance/Types.hs`, `Equipment/Types.hs` |
| 48 | `World/Page/Types.hs` |
| 44 | `World/Plant/Types.hs` |
| 43 | `World/Till/Types.hs`, `World/Chop/Types.hs` |
| 24 | `World/Tool/Types.hs` |
| 23 | `World/Ocean/Types.hs`, `Building/Command/Types.hs` |
| 14 | `Engine/Scripting/Types.hs` |

Every one could be `X.hs` — `World.Tool.Types` (24 lines) and
`Engine.Scripting.Types` (14 lines) each get a whole namespace level for one
small module. `Substance`, `Infection`, and `Equipment` are top-level `src/`
namespaces containing exactly one file.

Contrast the **five legitimate** cases, where `X/Types.hs` sits beside a real
`X.hs`: `Location/Overlay`, `World/Render/Textures`, `World/Render/Camera`,
`World/Fluid/Seabed`, `World/Geology/Ore`.

Because both shapes exist, `X/Types.hs` carries no information — you cannot
tell from the path whether `X.hs` exists. Flattening the 22 makes the
remaining five meaningful (CH-56's rule, applied).

### [#1148] CH-108. Power hardware is hardcoded in Haskell while 16 other content categories are YAML
> **Note:** Verified. Two corrections: neither YAML carries a **build cost** — both are instant-placed with `build_work` at its 0 default and no `materials` block, as each file's own comment states — and the category count is conservative (`data/` holds 16 content directories plus two loose YAMLs, `buildings` among them). Two constraints the fix must respect, both recorded in #1148: `powerNodeSpecFor` is *also* the placeability registry behind `power.isPlaceable`, which `scripts/build_tool.lua:854,964` uses to route a placement through the item-consuming `power.placeNode` rather than the free `building.spawn` path (Lua holds no second list); and the refusal currently happens in `powerPlaceNodeFn` (`API/Power.hs:99-101`) **before** `placeNodeOn` pops the item, so moving the check inside `placeNodeOn` — which pops first and looks the building def up second — would pop-then-roll-back on every rejection. The consumer half is already YAML-driven (`power_drain`, `YamlBuildings.hs:84`), and `placeNodeOn` already looks the building def up on the same `defName` key it pops the item by, so the value is reachable where the code already goes.

`src/Power/Types.hs:88`:

```haskell
powerNodeSpecFor ∷ Text → Maybe (PowerRole, Float)
powerNodeSpecFor "solar_panel"          = Just (PowerSource,  400)
powerNodeSpecFor "high_voltage_battery" = Just (PowerStorage, 5000)
powerNodeSpecFor _                      = Nothing
```

Both buildings already have full YAML definitions —
`data/buildings/solar_panel.yaml` and
`data/buildings/high_voltage_battery.yaml` carry their name, sprite,
description, and build cost. Only their **power role and wattage** live in
Haskell.

So one device's definition is split across two files in two languages, and
adding a wind turbine or a fuel generator requires a source edit and a
recompile — in a project where `data/` holds buildings, items, recipes,
materials, flora, vegetation, units, equipment, substances, infections,
locations, loot tables, structure packs, names, language, and thoughts, all
loaded through the `Engine.Asset.Yaml*` family.

Note the recipe side already got this right: `power_draw` is a **recipe** field
in YAML (#590), and `machine_shop.yaml` comments explain the design. The node
side is the outlier.

Fix: add `power_role` / `power_capacity` fields to the building schema and
delete `powerNodeSpecFor`.

### [#1149] CH-109. Nineteen lines of reasoning prove two functions are dead, and they are still there
> **Note:** The open question — is the scenario reachable? — is answered NO, on two independent grounds, so the deletion branch is the right one and #1149 files it. (1) `missingDefReferences` (`World/Save/Types.hs:747-761`) rejects the whole load via `Engine/Scripting/Lua/API/Save.hs:722` if any saved building names an unregistered def, and its own haddock (`:720-726`) already states the conclusion independently: "the load boundary rejects a save carrying any such reference before publishing any live state, so that pruning path is unreachable in normal play." (2) **Neither function implements the scenario its doc names.** Both filter on a `BuildingId` *instance-id* set (`Craft/Bills.hs:396-399`, `Power/Types.hs:136-139`); deregistering a *definition* removes no instance ids, so the only set a caller could build is "ids that survived" — exactly the tolerated demolished-before-save case #763 forbids pruning. They cannot distinguish the two cases at all. Orphans are documented tolerated state for nodes as well as bills (`docs/persistence_state_inventory.md:301`, "`pnBuilding` absent from the whole session is tolerated (#758)"); `BuildingDestroy` (`Building/Thread/Command.hs:115-126`) deliberately leaves them, and `Power.Network.positionsOf` already skips nodes whose building doesn't resolve. The contract is pinned by `Test/Headless/World/Identity.hs:350` ("dangling craft bills / power nodes survive staging"), which stays. Line reference correction: the comment is at `:243-261`, not `:225-243`.

`World/Load/Stage.hs:225-243` is a carefully argued comment concluding that
`Craft.Bills.pruneToStations` and `Power.Types.pruneToBuildings` must **not** be
called during load, because:

> …that scenario is already unreachable by the time staging runs at all: this
> module's caller […] rejects the WHOLE load outright via
> `missingDefReferences` before staging ever starts […] Applying the prune here
> only ever catches the FIRST (tolerated) case, silently discarding bills/nodes
> #763 requires to be restored.

**Correction (2026-07-25):** an earlier version of this entry claimed both
functions were dead. That was wrong — `test-headless/` exercises
`pruneToStations`, `pruneToBuildings`, and `removePowerNode`. They are tested
behaviour, not dead code.

What remains is narrower but still real: **neither function has a production
call site.** Their only production mention is this comment explaining why they
must not be called, and the scenario they were written for ("a station's
building DEFINITION deregistered between sessions") is — by the comment's own
argument — unreachable, because `missingDefReferences` rejects such a load
before staging runs.

So the tree carries two functions plus their tests for a scenario the load path
provably cannot reach, and 19 lines of reasoning that must be re-read by anyone
who wonders why. Either the scenario is reachable by some path the comment
didn't consider (in which case the prune belongs somewhere and the comment is
incomplete), or it is not (in which case functions, tests, and comment can all
go, replaced by one line on `wpsCraftBills`/`wpsPowerNodes` saying
restore-verbatim is deliberate).

That question is the issue worth filing — not the deletion itself.

### [no-issue] CH-110. Minor Sim/Power/Infection/Craft defects for one cleanup issue
> **Disposition:** No issue — one bullet is already filed, one is false, one rests on a large overcount, and one is self-declared not a defect. **Bullet 1** is real (43 files confirmed carry `-fprof-auto`, `Sim/Thread.hs` among them) but is #1116, filed from CH-87. **Bullet 2 is wrong on all three names**: `solarIntensity` and `wireComponents` each own a `describe` in `Test/Headless/Power/Network.hs` (`:55-64`, `:66-79`, plus the adjacency case at `:112`), and `takeItemsByName` is imported by name at `Test/Headless/Craft/Execute.hs:14` with a `describe` at `:249` — all three are test seams, the category #1083/#1119/#1146 preserve deliberately. (Four names spot-checked from #1146's inventory appear in neither test tree, so those scans were test-aware; this is a CH-110-specific error.) **Bullet 3's "~20 modules" is 6.** Tree-wide, 14 files match any traversal token, two of them prose only; genuine connected-component labelling lives in `Power/Network.hs`, `World/Fluid/Ocean.hs`, `World/Fluid/Lake/Identify/{Ocean,Components}.hs`, `World/Fluid/River/Identify/Components.hs`, and `World/Geology/Coastal/Breach.hs`. They are not unifiable as posed: `Ocean.hs:87` keys its visited set on wrap-seam-canonicalised global coords, `Breach.hs:181-183` uses a generation-stamped mutable `VUM` reused across basins, the Lake/River labellers run over chunk-local vectors fused with domain aggregation, and `PriorityFlood.hs` is a different algorithm. The union-find is confirmed the tree's only one — a single instance is not duplication — and "a survey to see whether a helper is warranted" has no observable outcome to gate on. **Bullet 4** states outright it is a positive reference point, not a defect.

- `Sim/Thread.hs` and `World/ZoomMap/*`, `World/Plate/*`, etc. carry
  `{-# OPTIONS_GHC -fprof-auto #-}` — `Sim/Thread.hs` is one of the 43 files in
  CH-87.
- Over-exports (used only internally): `Power/Network.hs`'s `wireComponents`
  and `solarIntensity`, `Craft/Execute.hs`'s `takeItemsByName`.
- `Power/Network.hs` hand-rolls a union-find (`UnionFind`, `ufNew`, `ufFind`,
  `ufUnion`) and a flood-fill (`wireComponents`). Both are correct and
  well-commented, and the union-find is the only one in the tree — but
  connected-component search now exists in ~20 modules across `World/Fluid`,
  `World/Hydrology`, and here, each over a different data structure. Worth one
  survey to see whether a shared `World.Graph` helper is warranted, before a
  21st appears.
- `Craft/Bills.hs` is 399 lines of pure, well-factored bill state with 19
  top-level functions — a good model for what the rest of the tree could look
  like. Noted as a positive reference point, not a defect.

---

## Batch 12 — `src/Building`, `src/Structure`, `src/Location`, `src/LootTable` (swept 2026-07-25)

983 + 626 + 708 + 74 lines. Small and in good shape: **3 unreferenced exports**
across all four trees, and the module boundaries are deliberate
(`Location.Placement`'s haddock explains it was factored out specifically so
`Building.Placement.canPlaceAt` could reuse it purely). Two findings reach
beyond these trees.

### [#1150] CH-111. `applyFacingF` — the camera rotation — is defined three times, identically
> **Note:** Verified 2026-08-06 — all three definitions compared line-by-line and confirmed character-identical (`World/Grid.hs:177-181`, `Unit/Render.hs:291-295`, `Building/Render.hs:330-334`). The strongest evidence is not in this entry: **three modules already import `applyFacingF` from `World.Grid`** — `Unit/HitTest.hs:31`, `Building/HitTest.hs:28`, `Structure/Render.hs:45` — so each private copy sits beside a hit-test sibling that imports the canonical one, and the pair must agree numerically or a click resolves to a different tile than the renderer drew. `Unit/Render.hs:26-28` already imports `applyFacing` (the `Int` variant) from `World.Grid` on the same line, then defines the `Float` variant privately 260 lines later. Two corrections: the transform is NOT "copy-pasted across `Unit`, `Building`, `Structure`, and `World/Render`" — `Structure/Render.hs` imports it, and the private copies are exactly two; and `unapplyFacingF` is not unreferenced — `Grid.hs:197` uses it inside `worldToGridF`, so it is an over-export with no consumer outside its module, the category #1119 tracks. Filed separately from #1132 rather than combined: #1132 already carries `reviewed:approve`, and the two overlap only in two import lines.

```haskell
applyFacingF ∷ CameraFacing → Float → Float → (Float, Float)
applyFacingF FaceSouth gx gy = ( gx,  gy)
applyFacingF FaceWest  gx gy = ( gy, -gx)
applyFacingF FaceNorth gx gy = (-gx, -gy)
applyFacingF FaceEast  gx gy = (-gy,  gx)
```

Character-for-character in **`World/Grid.hs:177`**, **`Unit/Render.hs:291`**,
and **`Building/Render.hs:330`**.

`World.Grid` is the canonical home — it owns `defaultGridConfig`,
`worldToGrid`, and the matching inverse `unapplyFacingF`. The two renderers
reimplement the forward transform privately rather than importing it. And
`World/Grid.hs`'s `unapplyFacingF` is itself unreferenced (CH-90), so the
canonical module's inverse is unused while its forward transform is
triplicated.

Together with **CH-92** (`baseTileW`/`baseTileH` in eight modules), this is the
pattern: the isometric renderer's core geometry primitives — tile dimensions
and camera rotation — are copy-pasted across `Unit`, `Building`, `Structure`,
and `World/Render` instead of imported from `World.Grid`. Any change to the
projection has to be found in eleven places across four subsystems.

Fix these two together in one pass: export from `World.Grid`, delete the copies.

### [#1151] CH-112. `validRelBounds` documents a validation it doesn't perform
> **Note:** Core claim verified 2026-08-06 — `validRelBounds` has zero references beyond its own export (`Location/Bounds.hs:21`) and definition, and the haddock's enforcement claim is false. One structural fact this entry misses changes what the fix looks like: **the loader physically cannot call it.** `validRelBounds ∷ RelBounds → Bool`, while `YamlLocations.hs:162-169` validates `LocationYamlBounds` (`:56-`, fields `lybMinX`/`lybMaxX`/…) — a different type with no conversion at the validation point, so "call the canonical one" is a design choice, not a substitution. Also: the *working* check is the tested one (`Test/Headless/Location/Bounds.hs:63-73` covers both axes), while the documented canonical predicate is neither called nor tested; the two agree today (`>` is exactly `not (≤)`), so this is a canonicality defect, not a bug. **Two of the four sibling "dead exports" are wrong:** `distanceBoundsToBounds` is LIVE — `Bounds.hs:143` (`nearestBoundsDistance`) → `Location/Placement.hs:30`, the #779 chain — and `distancePointToBounds` is a test seam (`Test/Headless/Location/Bounds.hs:145-154`, `:183-184`, incl. a wrap-seam case). `Structure/Palette.hs:68` `lookupId` and `Location/Overlay/Types.hs:30` `overlayLookup` are confirmed unused and ride along in #1151.

```haskell
-- | True iff min ≤ max on both axes — the shape every location's
--   authored bounds must satisfy. 'Engine.Asset.YamlLocations' rejects
--   any definition whose bounds fail this at YAML load time.
validRelBounds ∷ RelBounds → Bool
validRelBounds b = rbMinX b ≤ rbMaxX b ∧ rbMinY b ≤ rbMaxY b
```

`Engine.Asset.YamlLocations` **does** reject inverted bounds — but by
hand-inlining the comparison rather than calling this function
(`YamlLocations.hs:162-168`):

```haskell
        when (lybMinX bounds > lybMaxX bounds) $ fail …
        when (lybMinY bounds > lybMaxY bounds) $ fail …
```

`validRelBounds` has **zero references** in `src/`, `app/`, `test/`, or
`test-headless/`. So the canonical predicate is dead, the loader carries its
own copy, and the predicate's haddock asserts a relationship that does not
exist in the code.

The consequence is the usual one: a rule change (adding a Z axis, allowing
degenerate boxes) would be made in the documented "canonical" function and have
no effect, because the only enforcement point reimplements it.

Sibling dead exports in the same tree: `Location/Bounds.hs`'s
`distancePointToBounds` and `distanceBoundsToBounds` (seam-aware Chebyshev
helpers with no callers — discovery uses the `expandBounds` +
`boundsContainsPoint` route instead, which *is* seam-aware, so this is dead
code rather than a seam bug), `Structure/Palette.hs`'s `lookupId`, and
`Location/Overlay/Types.hs`'s `overlayLookup`.

### [#1152] CH-113. Quad vertex construction is written out longhand in eight places
> **Note:** Verified 2026-08-06 — exactly eight sites (`Building/Render.hs:222,312`, `Structure/Render.hs:174,274,365`, `Unit/Render.hs:272`, `GroundItemQuads.hs:247`, `BloodQuads.hs:318`), each repeating the same five-value tail across four vertices. **Correction: they are NOT "identical UV corners, differing only in the position arithmetic", and the proposed `makeQuad origin size` signature covers only 5 of 8.** Three parameterise more, each load-bearing: `Unit/Render.hs:268-270` varies U by `flipX` (`(1,0,0,1)` vs `(0,1,1,0)`) to draw SW/W/NW from the SE/E/NE sprites; `Structure/Render.hs:264-280`'s `strip i` slices a front wall into `k` depth-sorted strips (#415) so `xa`/`xb` and `ua`/`ub` move together; and `BloodQuads.hs:300-321` builds four independently rotated corners via `rotateAround cx cy rot`, so there is no origin+size to pass. A helper taking four positions plus a UV rect covers all eight. Also relevant: a ninth builder already exists — `UI/Render.hs:389-394` `makeQuadVertices` takes exactly a UV rect — but it emits a 6-vertex triangle list, hardcodes `faceMapId = 0`, and takes no render flags or world UV, so it is evidence for the shape rather than a drop-in.

Every sprite renderer builds its four vertices by hand:

```haskell
v0 = Vertex (Vec2 drawX drawY)                 (Vec2 0 0) tint (fromIntegral slot) fmSlot 0 wuv
v1 = Vertex (Vec2 (drawX + quadW) drawY)       (Vec2 1 0) tint (fromIntegral slot) fmSlot 0 wuv
v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH)) (Vec2 1 1) tint (fromIntegral slot) fmSlot 0 wuv
v3 = Vertex (Vec2 drawX (drawY + quadH))       (Vec2 0 1) tint (fromIntegral slot) fmSlot 0 wuv
```

Eight occurrences across `Unit/Render.hs`, `Structure/Render.hs` (×3),
`Building/Render.hs` (×2), `World/Render/GroundItemQuads.hs`, and
`World/Render/BloodQuads.hs` — identical UV corners, identical
tint/slot/facemap/flags/worldUV tail, differing only in the position
arithmetic.

A `makeQuad ∷ (Float,Float) → (Float,Float) → Vec4 → Int → Float → Word32 →
Word32 → (Vertex,Vertex,Vertex,Vertex)` would reduce each site to one line and
make the UV winding a single fact. Pairs with CH-99's observation that the nine
`*Quads` producers share no common entry shape.

### [no-issue] CH-114. Minor Building/Structure/Location defects for one cleanup issue
> **Disposition:** No issue — one bullet is contradicted by the report's own batch header, one defers to a finding already closed, one is already filed twice over, and one is explicitly a positive example. **Bullet 1**: `Location/Placement.hs` is 32 lines whose haddock states it was "Factored out here so `Building.Placement.canPlaceAt` can reuse it purely", and its sole consumer is exactly that module — `Building/Placement.hs:21` importing both names, `:87` for the #778 portal-exclusion intersection and `:111` for the #779 remote-start distance. Batch 12's own header (`:3006-3010`) already records this: "the module boundaries are deliberate (`Location.Placement`'s haddock explains it was factored out specifically so `Building.Placement.canPlaceAt` could reuse it purely)." **Bullet 2** says outright that both directories are instances of CH-107, which closed `[no-issue]` — and `Building.Command.Types` → `Building.Command` colliding in meaning with `Building.Thread.Command` was one of the named reasons it closed. **Bullet 3** is filed twice: #1132 lists `Building/Render.hs:34,37` among its eight `baseTileW`/`baseTileH` modules, and #1150 names `Building/Render.hs:330-334` as one of its two private `applyFacingF` copies. **Bullet 4** describes `ghostTint` as the right pattern, "worth preserving as-is".

- **`Location.Placement` doesn't place anything.** It derives the absolute
  bounds of already-placed locations (`placedLocationBounds`,
  `nearestLocationDistance`) — a *query*, sitting beside `Location.Bounds`
  which holds the bounds *math*. `Building.Placement` (validation for a
  proposed placement) is the honest use of the name; these two modules named
  `Placement` do unrelated things.
- **`Building` splits one concern across two single-file directories** —
  `Building/Thread/Command.hs` (the processor) and
  `Building/Command/Types.hs` (the type). Both are instances of CH-107.
- `Building/Render.hs` and `Structure/Render.hs` both re-derive
  `baseTileW`/`baseTileH` (CH-92) and `Building/Render.hs` also re-derives
  `applyFacingF` (CH-111) — this pair of modules is the densest concentration
  of the copied-geometry problem.
- `Building/Render.hs`'s `ghostTint` is a good example of the *right* pattern
  and worth preserving as-is: a pure function split out of the renderer
  specifically so the decision is testable without a GPU, correctly documented
  as "the one place RGB tinting is allowed by design (see the no-tinting
  rule)", **and actually exercised by `test-headless/`.**

---

## Batch 13 — remaining Haskell: `Item`, `Language`, `Blood`, and the test suites (swept 2026-07-25)

`Item` (555), `Language` (1091), `Blood` (1413), `Equipment`/`Substance` (102)
are healthy — 15 unreferenced exports between them, all over-exported internal
helpers or documented named constants (`Blood/Trail.hs`'s severity ladder,
`Language/Generated/Root.hs`'s `generateRoot`, both used within their own
modules).

The real gap is the test tree: **134 files, 34,293 lines — never audited, and
larger than any production subsystem except `World/`.**

### [#1153] CH-115. The `synarchy-test-graphical` suite is built by CI but never run
> **Note:** Verified 2026-08-06. The core claim holds, but the MECHANISM is different and stronger than stated: the blocker is inside the suite, not in CI. `test/Spec.hs` calls `GLFW.init` and `error`s on failure (`:31-33`), then creates a real window and `error`s on failure (`:43-50`), both **before `hspec` runs** — so `cabal test synarchy-test-graphical` on a headless box yields no assertions at all, and moving CI would not be enough. Four corrections. (1) **`Test.Engine.Core.Var` no longer exists** — CH-12 was filed as #947, which is CLOSED and deleted `src/Engine/Core/Var.hs`, so consequence 2 is already resolved, not live work. (2) The GPU-free set is three but a DIFFERENT three: `Test.UPrelude` (44), `Test.Engine.Core.Queue` (69), and **`Test.Engine.Input.State` (95)** — 208 of 609 lines. The "zero GLFW/Vulkan imports" criterion wrongly excludes `Input.State`, which imports `Graphics.UI.GLFW` only for enum constructors and tests pure functions. (3) **`test/Spec.hs:53-57` already groups exactly those three** under `describe "Core Tests"` with the comment "(no graphics dependencies)" — the suite itself names the movable set. (4) `test/` is 609 lines across 9 files including `Spec.hs`, not 682; and GitHub CI's graphical *build* is path-conditional (`ci.yml:279-283`), not the unconditional pair shown here, so on a non-graphics PR the suite is not even compiled.

`.github/workflows/ci.yml` and `tools/ci-local.sh` (the `make ci` gate) both do:

```
cabal build synarchy-test-headless
cabal build synarchy-test-graphical     # built…
cabal test  synarchy-test-headless      # …but only headless is RUN
```

So `test/` — 682 lines, 9 modules covering `UPrelude`, `Engine.Core.Queue`,
`Engine.Core.Var`, `Engine.Input.State`, GLFW window creation, and Vulkan
instance/surface/device creation — is a **compile check only**. Its assertions
have never executed in CI or in `make ci`.

That is probably deliberate (the Vulkan/GLFW cases need a display), but nothing
records it: CLAUDE.md describes `test/` as "hspec unit tests (engine core and
Vulkan primitives)" with no hint that they don't run, and the cabal stanza
carries no comment.

Two consequences worth separating:

1. **Three of the nine modules need no GPU at all** — `Test.UPrelude`,
   `Test.Engine.Core.Queue`, and `Test.Engine.Core.Var` contain zero
   GLFW/Vulkan imports. Moved to `test-headless`, they would actually run.
2. **This closes the loop on CH-12.** `Engine.Core.Var` is a production module
   whose only consumer is a test — in the suite that never executes. So the
   module exists to be tested by assertions that never run.

Fix: run the graphical suite where a display exists, or state plainly in
CLAUDE.md and the cabal stanza that it is a build-only target, and move the
GPU-free specs to the suite that runs.

### [no-issue] CH-116. The four largest files in the project are test modules
> **Disposition:** No issue — the premise is denied by CLAUDE.md, the fix would cost engine boots, and every number has drifted. **CLAUDE.md:38-43 states outright** that "the 500-line Haskell/Lua limits are per-split ratchets, enforced only for module families explicitly listed in the relevant budget tool. **They are not a tree-wide size policy.**" So there is no convention that "applies most weakly where the largest files actually are" — the three `BUDGETS` entries (#787, #588, #575, all production, no test code — that part of the finding is correct) are ratchets on specific reviewability splits, never a tree-wide rule. **A split would multiply engine boots**: `ResponsiveGameplay.hs:99` is `spec = aroundAll withSharedFixture`, `withSharedFixture` (`:85-86`) calls `withHeadlessEngine`, and that is a `bracket` around `initializeEngineHeadless` (`Harness.hs:30-37`) — a fresh boot per call. One module = one boot for 16 describes; a per-surface split into N modules = N boots, against the "one engine, booted in `Spec.hs`" convention. The proposed per-screen axis also doesn't match the file: the 16 describes are keyed by Lua surface AND review round (round-1/2/4/6/7/15), with `popup.lua` and `build_tool` each spanning two. Counts measured 2026-08-06: ResponsiveGameplay **2805**, SaveModules **2021** (not 1728), ResponsiveMenus **1673**, Save/Components **1533** (not 1448); `World/Save/Types.hs` is **1172**, not 1316 (it shrank, and is still the largest production module, so "more than double" now holds at 2.39×); a fifth test module, `World/Save/Compat.hs` (1291), also exceeds it; and **28** test-headless files exceed 500 lines, not 15 — against 21 in `src`/`app`.

| Lines | File |
|---:|---|
| 2806 | `test-headless/Test/Headless/UI/ResponsiveGameplay.hs` |
| 1728 | `test-headless/Test/Headless/Lua/SaveModules.hs` |
| 1674 | `test-headless/Test/Headless/UI/ResponsiveMenus.hs` |
| 1448 | `test-headless/Test/Headless/World/Save/Components.hs` |

`ResponsiveGameplay.hs` alone is **more than double** the largest production
module (`World/Save/Types.hs`, 1316 — and 296 lines of *that* is one comment,
CH-40). **15 test-headless files exceed 500 lines.**

The 500-line norm is stated in CLAUDE.md and enforced by
`tools/haskell_module_budget.py` — which covers exactly three production splits
and no test code at all (CH-21). So the convention applies most weakly where
the largest files actually are.

Test code earns some latitude — a spec is a list of cases, not branching logic.
But a 2806-line spec is past the point where a reader can find the case they
broke, and `ResponsiveMenus`/`ResponsiveGameplay` (4480 lines combined) cover
one epic that already has a natural per-screen split.

### [no-issue] CH-117. Seven test modules bypass the shared engine harness
> **Disposition:** No issue — the private boots are a documented isolation pattern, and the convention cited is scoped to worldgen. `withHeadlessEngine` (`Harness.hs:30-43`) is not a thin wrapper: beyond `initializeEngineHeadless` it sets `camZoom`/`lifecycleRef`, **spawns a world thread** (`startWorldThread`), and tears down with a 100 ms `threadDelay`. The private-boot modules avoid exactly that, and name it as an established pattern — `Asset/TextureFallback.hs:125-137`: "A private, lightweight `initializeEngineHeadless` env (the same primitive `Test.Headless.World.LocationDiscovery`/`CursorInfo`/`Unit.LineOfSight` already use for exactly this 'needs its own throwaway engine state' case, no world/unit thread spawned)… the aroundAll-shared `_sharedEnv` above is **deliberately unused**", adopted after a round-7 review found reverting shared-env mutations (asset pool, texture-name registry, queue) error-prone; `World/LocationDiscovery.hs:9-13` records the same rationale. The "one engine, booted in `Spec.hs`" line is scoped: `sharedWorld`'s own haddock (`Harness.hs:52-54`) reads "Works because Spec.hs boots ONE engine **for all worldgen specs** (a single top-level `aroundAll withHeadlessEngine`)", and CLAUDE.md:73-77 is likewise about `sharedWorld` and the ~10 s `WorldInit` — not a rule that every spec route through the harness. `Spec.hs` itself calls `aroundAll withHeadlessEngine` **five times** (`:169, 220, 226, 232, 237`), so a general single-entry-point rule would already be broken four times over. Two corrections: it is **eight** modules, not seven (`Core/LoopStartup.hs:34` is absent from the list), and **`World/SelectChunk.hs` does not bypass the harness** — `Spec.hs:237` runs `aroundAll withHeadlessEngine SelectChunk.sharedSpec`, so it uses both. Converting the eight would spawn eight unwanted world threads, add ~800 ms of teardown delay, and reintroduce the contamination the pattern prevents.

`Test.Headless.Harness` exports `withHeadlessEngine ∷ (EngineEnv → IO α) → IO α`,
and CLAUDE.md states the convention: "one engine, booted in `Spec.hs`".

These seven call `initializeEngineHeadless` directly instead:

`Unit/LineOfSight.hs`, `Core/LogMonad.hs`, `Asset/TextureFallback.hs`,
`World/SelectChunk.hs`, `World/LocationDiscovery.hs`, `World/CursorInfo.hs`,
`Blood/Trail.hs`

Six of the seven do no worldgen, so the cost is small (`initializeEngineHeadless`
allocates IORefs; it is `WorldInit` that costs ~10 s). This is a consistency
problem, not a performance one — but it means the harness is not the single
entry point it is documented to be, and a future change to engine setup has
eight places to reach.

`World/SelectChunk.hs` is the one to check first: it does reference worldgen
(3 sites) while booting privately.

### [no-issue] CH-118. `test/` and `test-headless/` were absent from this audit's own tooling
> **Disposition:** No issue — the audit half is already discharged and the actionable tail checks out, with all three named candidates being the wrong candidates. The methodology note at `:15-30` records the correction, the corrected-totals table, and the three findings fixed in place, so the first paragraph is a record rather than outstanding work. The tail — "worth checking `engine_env_capability_audit.py`, `persistence_inventory_audit.py`, `action_outcome_coverage.py`" — was verified 2026-08-06: **none of the three reasons about "is this used?"**. `engine_env_capability_audit.py`'s `src/`+`app/` scope is a documented CONTRACT, not an omission — its header (`:54-55`) states "`test/` sources remain outside this ratchet entirely (§6.3's test-only exception)", matching CLAUDE.md's "production-only (`src/`+`app/`, `test/` exempt) full-access boundary"; widening it would break the contract. `persistence_inventory_audit.py` takes specific named files matched by `^data X = X` regexes and asks whether every declared state owner has a classification row. `action_outcome_coverage.py` reads named `src/Engine/Input/*.hs` files. Beyond those three, **no tool in `tools/` performs a cross-root usage scan at all** — no script globs `**/*.hs`/`rglob('*.hs')` — and the four mentioning unreferenced/unused/dead each declare a deliberate narrower scope: `cabal_module_audit.py` is `SOURCE_ROOT = REPO_ROOT / "src"` because it audits the *library* stanza (a test-suite module there would be a category error), `material_id_audit.py` lists "the bare `MaterialId n` literals in the test suites" under Out of scope, and `test_save_compat_audit.py`/`pack_atlas.py` are unrelated.

Recorded here as a finding against the audit, not the code: every
"unreferenced export" scan in batches 2-11 searched `src/ app/ test/` and
omitted `test-headless/` — the suite holding 124 of the project's 134 test
files. Corrected counts and the three wrong findings are listed in the
methodology note at the top of this document.

The general lesson applies to the codebase's own tooling too: any script that
reasons about "is this used?" must enumerate all four source roots. Worth
checking `tools/engine_env_capability_audit.py`,
`tools/persistence_inventory_audit.py`, and `tools/action_outcome_coverage.py`
for the same omission before trusting their coverage claims.

### [#1154] CH-119. Minor remaining-Haskell defects for one cleanup issue
> **Note:** Bullets 2 and 3 are **already #1119** — `Geology`, `Magma`, `Plate`, `Weather`, `Flora`, and `Slope` all live under `src/World/`, and #1119's own table names the same two clusters verbatim (`World/Geology/Volcano.hs`'s 7 `apply*` dispatched by `applyVolcanicFeature`; `World/Magma/Init.hs`'s geometry helpers), with `Geology/Generate.hs` in the same tree. Bullet 1 is filed as #1154 after re-measuring all four source roots with line comments stripped: **16** in-module-only exports across `src/Blood` (12), `src/Language` (3), `src/Item` (1); `src/Equipment`/`src/Substance` have none. **Three of bullet 1's eleven names are wrong** and must keep their exports — `trailModerateVolume` is imported and used by `Blood/Pool.hs:60,115`, and `generateRoot`/`minNativeWordLength` are used by `test-headless/Test/Headless/Language/Generated.hs:512,533,761,777,804` (batch 13's header repeats this error, calling the severity ladder and `generateRoot` "both used within their own modules"). It also omits eight: `catastrophicBluntThreshold`, `poolStyleVolume`, `poolFootprintFor`, `decalTint`, `bloodRenderRecord`, `shapeLength`, `formKindText`, `domainText`. Two names on the list are referenced only from OTHER modules' haddock — `trailBloodForVolume` (`Combat/Wounds/Bleed.hs:101`) and `removeDecalsForTexture` (`Blood/Render.hs:65`, `Lua/API/Blood.hs:303`) — so they belong on it, but un-exporting them leaves dangling documentation links.

- `Item`/`Language`/`Blood` over-exports (used only in-module):
  `Blood/Trail.hs`'s `trailModerateVolume`/`trailSevereVolume`/
  `trailCatastrophicVolume`/`trailBloodForVolume`,
  `Language/Generated/Root.hs`'s `generateRoot`/`minNativeWordLength`,
  `Blood/Impact.hs`'s `impactFootprint`/`impactOpacity`,
  `Blood/Types.hs`'s `matchThreshold`/`removeDecalsForTexture`,
  `Item/Types.hs`'s `defaultQualityTiers`.
- `World/Geology` + `Magma` + `Plate` + `Weather` + `Flora` + `Slope` carry 40
  unreferenced exports, dominated by two clusters already noted in CH-90:
  `Geology/Volcano.hs` (7 `apply*` feature builders dispatched internally by
  `applyVolcanicFeature`) and `Magma/Init.hs` (7 internal geometry helpers —
  `padBox`, `unionBoxes`, `squareAt`, `msBBoxFromShapes`, …). Both are
  export-list narrowing, not deletion.
- `Geology/Generate.hs`'s `generateCaldera`/`generateLavaDome`/
  `generateLavaTube`/`generateAndRegister` pair with the `Volcano.hs` cluster —
  worth checking together whether the whole feature-generation surface should
  be one narrowed module.

---

## Batch 14 — `src/UI` (Haskell only; the Lua UI is a later batch) (swept 2026-07-25)

4174 lines, 25 modules, **none over 500**. `UI/Manager.hs` (103) and
`UI/Tooltip.hs` (47) are proper narrow facades over their subdirectories — the
shape CH-56 asks for elsewhere. **11 unreferenced exports, all over-exports;
no dead code.**

Most of this batch is verification. The #742-#750 UI epic's contracts are the
most heavily documented in the codebase, and — unlike the comparable claims in
worldgen (CH-83) and locations (CH-112) — **they hold.**

### [#1155] CH-120. Five focus modules, and three have no module haddock at all
> **Note:** Verified 2026-08-06 — the header table is exactly right (three open straight into `module … (`, `Lua/API/UI/Focus.hs`'s "Lua bindings for keyboard/input focus management" names no system, `UI/FocusNavigation.hs`'s is the model), and the vocabulary collision is real (`UI/Focus.hs:13-15` vs `UI/Manager/Focus.hs:2-13`). Two corrections that change the fix. (1) **There are THREE systems, not two, and two modules span two each**: `UI/Manager/Focus.hs` holds element TEXT focus (`setElementFocus`/`getPageFocus`/`validateFocus`) AND keyboard CONTROL focus (`setControlFocus`… under its own `-- * Control focus (#745)` section), and `Lua/API/UI/Focus.hs` splits the same way — so "one shared sentence per module naming its system" is wrong for those two. (2) **The paragraph named as "the missing header" is incomplete**: `Engine/Input/Thread/Keyboard.hs:99-101` (not `:97`) reads "Two independent focus systems checked here: 1. FocusManager — shell/console text input; 2. UIPageManager — UI widget text input" — correct where it sits, since control focus is not text routing, but copying it into all five would mis-describe `FocusNavigation` and both control-focus halves. Rename scope measured: `UI.Focus` has 18 importers (23 references); `Engine.Scripting.Lua.API.Focus` has 3 sites. NB CH-44 closed `[no-issue]` *because* this finding carries its rename, so #1155 is what discharges it.

| Module | Governs | Header? |
|---|---|---|
| `UI/Focus.hs` | `FocusManager` — shell/console text focus | **none** |
| `UI/Manager/Focus.hs` | element/page text focus + control focus | **none** |
| `UI/FocusNavigation.hs` | keyboard control focus, Tab traversal (#745) | yes |
| `Engine/Scripting/Lua/API/Focus.hs` | Lua binding for `FocusManager` | **none** |
| `Engine/Scripting/Lua/API/UI/Focus.hs` | Lua binding for UI focus | vague |

This supersedes and widens **CH-44** (which covered only the two Lua modules).
Three of the five open straight into `module …  where` with no statement of
which focus system they belong to, and their vocabularies overlap directly:

- `UI.Focus` exports `setFocus`, `clearFocus`, `registerFocusTarget`
- `UI.Manager.Focus` exports `setElementFocus`, `clearElementFocus`,
  `setControlFocus`, `clearControlFocus`

A reader who greps `setFocus` lands in the shell-console system while looking
at game UI code, or vice versa. `Engine/Input/Thread/Keyboard.hs:97` already
documents the resolution order ("1. FocusManager (focusManagerRef) —
shell/console text input") — that paragraph is the missing header, and it
belongs at the top of all five.

Fix: one shared sentence per module naming its system, and rename the two
`Focus.hs` files to say which they are (`UI/ShellFocus.hs`,
`Engine/Scripting/Lua/API/ShellFocus.hs`).

### [#949] CH-121. `src/UI` is the densest concentration of review-round archaeology
> **Note:** Already done. CH-15 was filed as #949 ("Remove review-round provenance from production comments"), **closed 2026-08-03**, nine days after this report was written. CH-15's own regex — `rg -ni 'round [0-9]+ (review|of review)|review round [0-9]+' src app --glob '*.hs'` — now returns **0** across `src/` and `app/`, and `src/UI` has none under any `round <n>` variant, so this finding's 31 is now zero. (The five surviving `round [0-9]` matches tree-wide are all `around` in ordinary prose: "around 18 s", "around 30–50 J", "around 0", "around 1440 minutes", "around 0°C".) The sweep did exactly what this entry asked — kept the invariant, dropped the round number, retained the issue number: `UI/Types.hs:146` now reads `-- ^ #745: bumped ONLY by a route-affecting` (was `#745 review round 12: …`) and `UI/Manager/Property.hs:50` reads `--   #745: only bumps when 'visible' actually differs`.

31 comments citing PR review rounds — the highest of any tree, confirming
CH-15's identification:

| Count | Module |
|---:|---|
| 6 | `UI/Manager/Hierarchy.hs` |
| 5 | `UI/Manager/Page.hs` |
| 4 | `UI/Manager/Property.hs` |
| 3 | `UI/Render.hs`, `UI/Manager/Core.hs`, `UI/InputOwnership.hs`, `UI/ControlActivation.hs` |
| 2 | `UI/Types.hs` |
| 1 | `UI/Manager/Query.hs`, `UI/FocusNavigation.hs` |

They cluster on the activation-epoch machinery (`#745 review round 12` /
`round 13` appear repeatedly), where the *invariant* is genuinely subtle and
worth documenting — "bumped ONLY by a route-affecting change", "only bumps when
`visible` actually differs" — but the round number adds nothing a reader can
act on.

This is the best tree to do the CH-15 sweep in first: the invariants are
already well written, so the edit is purely deleting `review round N` and
keeping the sentence.

### [no-issue] CH-122. Verified: the UI tree's "single source of truth" claims are true
> **Disposition:** No issue — a positive-result record, re-verified 2026-08-06 rather than taken on the entry's word, and all five claims still hold. `uiLayerBand` has one definition (`UI/Types.hs:75`) reached by rendering (`Render.hs:58`) and hit-testing (`Query.hs:194`, `:322`); `effectiveClip` one (`Clipping.hs:86`) with all four consumers present (`Render.hs:163`, `Query.hs:161`, `InteractiveBounds.hs:182`, `API/UI/Property.hs:283`); `interactiveRect` one (`InteractiveBounds.hs:139`) with hit-testing through it (`Query.hs:159`). Paint-order parity confirmed at the current lines: `Render.hs:165` computes `elemLayerId = baseLayerId + ueZIndex elem` and `:172` recurses passing it as the child's base, so the render path accumulates through the ancestor chain exactly as `Query.hs:322`'s `elementPaintKey`. `bumpPageEpoch` still has exactly two call sites, `Page.hs:72` in `showPage` (`if upVisible page then id else bumpPageEpoch`) and `:87` in `hidePage` (the mirror) — each bumping only on a real value change; its definition has since moved to `UI/Manager/Core.hs:80-81`. Line drift since the sweep, for anyone navigating by number: the `uiLayerBand` render consumer is `:58` not `:56`, `effectiveClip`'s render/Lua consumers are `:163`/`:283` not `:161`/`:281`, and the paint-order pair is `:165`/`:172` not `:163`/`:170`.

Recorded as a positive result, because three comparable claims elsewhere in the
audit turned out to be false (CH-83 `mkSurfaceMap`, CH-112 `validRelBounds`,
CH-85 `moSurface`). Each of these was checked against the live code:

- **`uiLayerBand`** — one definition (`UI/Types.hs:75`), consulted by rendering
  (`UI/Render.hs:56`) and hit-testing (`UI/Manager/Query.hs:194, 322`). ✓
- **`effectiveClip`** — one definition (`UI/Clipping.hs:86`), consulted by
  rendering (`Render.hs:161`), hit-testing (`Query.hs:161`), interactive bounds
  (`InteractiveBounds.hs:182`), and the Lua introspection path
  (`API/UI/Property.hs:281`). ✓
- **`interactiveRect`** — one definition (`InteractiveBounds.hs:139`), and
  hit-testing goes through it (`Query.hs:159`). ✓
- **Paint-order parity** — I initially read `Render.hs:163`
  (`baseLayerId + ueZIndex elem`) as adding only the element's own zIndex while
  `Query.hs:322` accumulates ancestors. That was wrong: `renderElement`
  recurses passing `elemLayerId` as the child's base (`Render.hs:170`), so the
  render path accumulates through the chain exactly as `elementPaintKey` does.
  The two agree. ✓
- **`upmPageEpoch`** — CLAUDE.md says "bumped ONLY by `hidePage`/`showPage`…
  only on a real value change". `bumpPageEpoch` has exactly two call sites,
  both in `UI/Manager/Page.hs` (72, 87), each guarded by an actual-change
  check. ✓

No action needed. Worth keeping in the document so a future reader knows these
were checked rather than assumed.

### [#1156] CH-123. Minor UI defects for one cleanup issue
> **Note:** Verified 2026-08-06 — and this is the one export bundle in the report whose list measures **exactly** right: an independent scan of `src/UI` across all four roots with line comments stripped reproduces the same 11 names in the same 8 modules, so the batch's tokenizer caveat and re-check did their job. **One correction, to the methodology note itself**: "all 11 are genuine over-exports (used within their own module), none are dead" holds for ten, not eleven. **`submitBuffer` is dead** — checked separately for in-module usage, it has zero references beyond its export line (`:13`), signature (`:64`), and equation (`:65`), and across `src/`, `app/`, `test/`, `test-headless/`, and `scripts/` the only file mentioning it is `src/UI/TextBuffer.hs`. The bullet above the note says as much ("a text-submission entry point … that nothing calls"), so the two contradict each other. Consequence for the fix: `submitBuffer` is `clearBuffer`'s only caller, so deleting it makes `clearBuffer` unreferenced in-module too and the pair must be decided together. The other ten each have a real in-module call site (spot-verified `showTooltip` at `Tooltip/State.hs:116`, `hitsAtPointBy` at `Manager/Query.hs:218`). ~~Bullet 2 confirmed: `UI/Focus.hs:20` imports and `:4`/`:9` re-export `TextBuffer(..)`/`emptyBuffer`, defined at `UI/Types.hs:225,231`.~~ **Corrected 2026-08-21 (#1155): that confirmation was wrong.** Those lines are `, FocusTarget(..)` (`:4`), the `-- * focus operation` section comment (`:9`) and the `FocusId` newtype's own body (`:20`); the module imports only `UPrelude` and `Data.Map.Strict`, and its explicit export list carries no `UI.Types` name at all. Bullet 2 is withdrawn below. Bullet 3's count has drifted — `UI/Types.hs` is now **496** lines, not 488. NB #1155 renamed `UI/Focus.hs` to `UI/ShellFocus.hs`; with bullet 2 withdrawn, #1156 has no remaining requirement on that file.

- **11 over-exported internals** (no consumer outside their own module):
  `UI/InputOwnership.hs` (`inputBoundaryPage`, `pagesInScope`),
  `UI/TextBuffer.hs` (`clearBuffer`, `submitBuffer`),
  `UI/Tooltip/Layout.hs` (`hintLeadingPx`, `hintPixelWidth`),
  `UI/InteractiveBounds.hs` (`elementRawOverflow`),
  `UI/Manager/Query.hs` (`hitsAtPointBy`), `UI/Render.hs`
  (`uiLayerToLayerId`), `UI/Tooltip/State.hs` (`showTooltip`), `UI/Types.hs`
  (`emptyTooltipState`).
  `submitBuffer` is the one worth a look — a text-submission entry point on a
  documented contract (`UI.TextBuffer`) that nothing calls.
- ~~**`UI.Focus` re-exports `TextBuffer` and `emptyBuffer` from
  `UI.Types`**~~ — **withdrawn 2026-08-21 (#1155): this never happened.**
  The module (now `UI.ShellFocus`) has an explicit export list carrying
  only `FocusId`/`FocusTarget`/`FocusManager`/`InputMode` and its five
  focus functions, and it does not import `UI.Types` at all. `UI.Types`
  is where both names are defined and the only module that exports them
  in its own right; `UI/TextBuffer.hs:15` imports `TextBuffer(..)` from
  it without re-exporting it. There IS a second import path, but it is
  the umbrella aggregate rather than a focus module: `src/UI.hs:11,14`
  re-exports `module UI.Types` wholesale, exactly as it re-exports every
  other `UI.*` module it bundles. Nothing to fix here.
- `UI/Types.hs` (488 lines) is the tree's largest module and holds
  `UIPageManager`, `UIPage`, `UIElement`, `UILayer`, `uiLayerBand`,
  `TextBuffer`, and tooltip state. It is well under budget, but it is the one
  module in this tree with no export list narrowing — worth splitting only if
  it grows.

**Methodology note for this batch:** the tokenizer used for the dead-export
scans treats haddock `'quoted'` names as ending in an apostrophe, so a symbol
mentioned only inside haddock quotes elsewhere can read as unreferenced. Every
name above was re-checked with a direct call-site grep; all 11 are genuine
over-exports (used within their own module), none are dead.

---

## Batch 15 — `scripts/` (Lua) (swept 2026-07-25)

178 files, **59,531 lines** — the largest single-language surface in the
project. The problems here are duplication and layout, not dead code.

### [#1157] CH-124. `truncateToWidth` has five divergent implementations, and users can see the difference
> **Note:** The conclusion holds — five copies, visibly different output — but the evidence table is wrong in three of four columns and the sharpest claim is stale. **All five are BINARY SEARCH**, not "linear" (event_log, whose own comment says "Binary search the cut length"; unit_info) or "drop one char at a time" (cargo_inventory_panel — whose *comment* says that above a binary search, which is its own defect and almost certainly what misled this table). **Two ellipsis cells are wrong**: `popup.lua` appends `"..."` and `item_contents_panel.lua` appends `".."`, both listed as "—". **The UTF-8 claim is FALSE**: all five `require("scripts.ui.utf8_safe")` and call `utf8Safe.snapToCharBoundary(text, mid)` before slicing and again on the final cut — `#text` is only the binary search's upper bound, so nothing can cut mid-codepoint. Closed #618 fixed that class of bug. What genuinely diverges: the ellipsis (`...` in popup/event_log vs `..` in the three panels — the real user-visible defect), three different loop formulations, two different width measurements (four measure `sub .. ellipsis`; `unit_info` measures `sub` and adds a precomputed `ellW`, not equivalent under kerning), and three guard sets. **Dedup:** #1088 (open, C0 of epic #1013) extracts `scripts/ui/item_list.lua` from three of the five hosts and owns the row structure where truncation happens — but cannot fix the inconsistency, which is between those three and `popup`/`event_log`.

Five modules each define a private `truncateToWidth`, and unlike the other
duplicates in this batch **they have drifted**:

| Module | Algorithm | Ellipsis | nil/empty guard |
|---|---|---|---|
| `popup.lua:185` | binary search | — | no |
| `event_log.lua:400` | linear | `"..."` | no |
| `unit_info_v2_inventory.lua:42` | linear | `".."` | yes |
| `item_contents_panel.lua:117` | binary search | — | yes |
| `cargo_inventory_panel.lua:397` | drop one char at a time | `".."` | yes |

So a truncated item name ends in `..` in the inventory panel and `...` in the
event log — a visible inconsistency in the same UI, from the same operation.
Two of the five crash on `nil` where three return early.

All five also bound their search with `#text` (bytes) while each file imports
`utf8_safe` for other purposes, so a truncation can cut mid-codepoint —
precisely the class of bug `scripts/ui/utf8_safe.lua` exists to prevent, and
which CLAUDE.md's text contract calls out.

This is the sharpest illustration in the audit of why duplication matters: the
identical copies (below) are merely wasteful; this one already produces
different behaviour in shipped UI.

### [#1158] CH-125. `clamp` is defined 11 times; `formatGameTimeHMS` 4 times, identically
> **Note:** Both headline claims verified exactly 2026-08-06 — `clamp` in precisely the 11 listed modules (ten one-liners plus `movement_speed.lua:21-23`'s three-line form), and all four `formatGameTimeHMS` bodies compared byte-for-byte and found identical. `scripts/lib/` confirmed to hold only `save_modules.lua`/`data_codec.lua`, with `require("scripts.lib.…")` already the established form. **Correction: the "also duplicated 3+ times" list counts NAMES, not duplicate code**, and splits three ways. Only `clamp` and `formatGameTimeHMS` are true copy-paste. `worldId` (4: the mine/chop/till/plant tools) and `destroyChrome` (4: the three log panels + `crafting_panel`) share a shape but each closes over its OWN module table (`mineTool.hud` vs `chopTool.hud`; `combatLog.chromeLabels` vs `injuryLog.chromeLabels`), so no shared function absorbs them unchanged. And `destroyAll`, `destroyOwned`, `active`, `displayName` are same-name-different-function: `destroyAll` alone appears in **17** files as unrelated per-module teardowns (`toggle.destroyAll`, `shell.destroyAllElements`, `bottomButtons.destroyAll(menu)`, …). #1158 is scoped to the two verified cases and excludes the rest explicitly.

```lua
local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end
```

Character-for-character in **eleven** modules — `circulation`, `cardio`,
`brain`, `thermo`, `movement_speed`, `salts`, `consumable`, `starvation`,
`thoughts`, `mental_state`, `exhaustion` (one spread over three lines in
`movement_speed`). That is the entire physiology/mental-state family, which has
no shared utility module of its own.

```lua
local function formatGameTimeHMS(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    local ss = secs % 60
    return string.format("%02d:%02d:%02d", hh, mm, ss)
end
```

Seven identical lines in **four** modules — `combat_log`, `injury_log_panel`,
`unit_log`, `thought_log`. These are exactly the four panels that show the
player timestamped events; if one ever drifts, the same event displays a
different time depending on which panel you open.

Also duplicated 3+ times: `wrapText` (3), `worldId` (4), `destroyChrome` (4),
`destroyTransient`/`destroyOwned`/`destroyAll` (3 each), `spawnTab` (3),
`tabPixelWidth` (3), `displayName` (3), `processEvent` (3), `active` (4).

`scripts/lib/` already exists as the shared-library location — it just holds
only persistence code (`save_modules.lua`, `data_codec.lua`). A
`scripts/lib/util.lua` and `scripts/lib/format.lua` would absorb most of the
above.

### [#1159] CH-126. `shell.wrapText` says "by character" and iterates by byte
> **Note (pre-resolution verification, 2026-08-06):** all three defects verified, and — unlike CH-124's UTF-8 claim, which #618 had already fixed — this one is **live and current**. `shell.wrapText` has four call sites, all in `shell.lua` (`:419` wrapping `entry.result`, `:438` the typed command, `:745`/`:748` line counts), so the byte iteration runs on every console line. `shell.lua` has **no** `utf8_safe` import at all against 25 `string.sub`/`:sub(` sites; `textbox.lua` is confirmed as the model (`utf8Safe.prefix`/`suffix`/`codepointLength`, its one `string.sub` mention being the comment at `:278`). **Two additions this entry misses.** (1) The same mid-codepoint split exists in **three more modules**: the log panels' private `wrapText` is word-based and safe normally, but its hard-break fallback for an over-wide word uses `word:gmatch(".")` — Lua patterns are byte-oriented, so `.` matches one byte — at `combat_log.lua:706`, `injury_log_panel.lua:573`, `unit_log.lua:244`. (2) **`shell.wrapTextByWord` (`:712`) is dead** — zero callers — and it is the word-based variant that would be safe on the normal path, sitting unused beside the broken one in use. Trivia: the comment is `:682`, the function `:683`.

`scripts/shell.lua:682`:

```lua
-- Wrap text into multiple lines that fit within maxWidth (by character)
function shell.wrapText(text, maxWidth, font)
    ...
    for i = 1, #text do
        local char = text:sub(i, i)
        local testLine = currentLine .. char
        local width = engine.getTextWidth(font, testLine, fontSize)
```

Three defects in five lines:

1. **The comment is wrong** — `#text` and `text:sub(i, i)` are byte
   operations, not character ones.
2. **Multi-byte UTF-8 splits mid-sequence**, so any non-ASCII in shell output
   wraps into mojibake. The debug console is the most likely place in the game
   to display arbitrary text (Lua return values, engine log lines, error
   messages).
3. **`engine.getTextWidth` is called once per byte** — an O(n) text measurement
   per output line, on the panel that renders streamed log output.

Note `scripts/ui/textbox.lua` gets this right (it uses `utf8Safe.suffix`, and
its only `string.sub` mention is a comment explaining why not to use it) —
`shell.lua` uses the older `FocusManager` path (CH-120) and never adopted
`utf8_safe`. CLAUDE.md's UTF-8 rule is scoped to `UI.TextBuffer` widgets, so
the shell is outside its letter; it should not be.

The same byte loop appeared three more times: the combat, injury, and per-unit
log panels each carried a byte-identical private `wrapText` whose hard-break
fallback for an over-wide word iterated `word:gmatch(".")` — a byte pattern.

Resolved in #1159 by extracting one shared `scripts/ui/text_wrap.lua`
(`byCharacter` for the console, `byWord` for the three panels), whose walk
advances one code point at a time and is total on malformed input rather than
asserting like `utf8_safe`. `shell.wrapTextByWord`, the never-called
word-wrapping twin, is gone. CLAUDE.md's text contract now covers display as
well as editable widgets.

### [no-issue] CH-127. Four features are split across both a flat file and a same-named directory
> **Disposition:** No issue — the four pairs are one coherent pattern, not two competing conventions, and the finding's own precedent closed for this reason. Reading what each flat file does: `settings_menu.lua:16-20` requires `scripts.settings.{data,general_tab,graphics_tab,notifications_tab,input_tab}`, `create_world_menu.lua:16-20` requires `scripts.create_world.{settings_tab,advanced_tab,general_tab,timeline_tab,log_panel}`, `hud.lua:7` requires `scripts.hud.info_panel`, and `debug.lua:31-33` requires `scripts.debug.{mode,layout,modes}` — every one an entry point requiring its own parts. **"Same-named" is wrong for half the table**: `settings_menu.lua` ≠ `settings/` and `create_world_menu.lua` ≠ `create_world/`, so only `hud` and `debug` qualify. **The flat underscore families are deliberate and CI-enforced**: `tools/lua_module_budget.py` guards six of them by their flat names (#538 unit-AI, #541 unit-resource, #542 unit-info-v2, #543 init.lua router, #544 ui_manager, #545 debug overlay), and CLAUDE.md:228-234 documents the `unit_ai.lua` + `unit_ai_*.lua` shape explicitly — converting them would fight a checked-in guard. **CH-25, cited here as the same finding, closed `[no-issue]`** on the reasoning that the filename prefix already gives the grouping a directory would; `ls scripts/unit_ai_*.lua` is exactly `scripts/unit_ai/`. Counts measured 2026-08-06, all drifted upward: **189** files not 178, **142** flat not 134, **47,327** lines not 43,695, `unit_ai*` **25** not 23, `unit_*` **16,718** lines across 53 files not 16,077, `scripts/ui/` **29** not 27, `scripts/settings/` **5** not 4. Churn if reorganised: **464 of 794** `require("scripts.…")` sites name a flat module.

`scripts/` has 178 files, **134 of them flat in the root** (43,695 lines), and
uses two different namespacing conventions simultaneously — sometimes for the
same feature:

| Feature | Flat file | Directory |
|---|---|---|
| settings | `settings_menu.lua` (1063) | `scripts/settings/` (4 files) |
| create world | `create_world_menu.lua` (1171) | `scripts/create_world/` (7 files) |
| hud | `hud.lua` (1321) | `scripts/hud/` (1 file) |
| debug | `debug.lua` | `scripts/debug/` (3 files) |

Meanwhile the three *largest* families have no directory at all and are
namespaced by underscore: `unit_ai*` (23 files), `unit_info_v2*` (14),
`unit_resource*` (7) — 16,077 lines of `unit_*` in the flat root. And `ui` is a
directory (27 files) while `ui_manager*` (9 files) is flat beside it.

So the answer to "where does script X live?" depends on which convention its
epic happened to use. Same finding as CH-25 (`tools/` is 122 flat Python
files), one level larger.

### [no-issue] CH-128. Five Lua modules sit at exactly the 500-line cap
> **Disposition:** No issue — the second half is a finding already closed, and the new half describes the ratchet working. "Extends CH-22" restates CH-22, which is `[no-issue]`: "the 500-line Lua guard is an intentional per-split ratchet, documented as applying only to module families with an explicit split agreement; 33 unrelated scripts exceeding 500 lines do not violate that contract." CLAUDE.md:38-43 says the same and adds these are "not a tree-wide size policy." A ratchet sitting at its limit is the mechanism functioning — the finding concedes "the guard is working" and then treats that as the symptom; forcing the next split when the next line arrives is the whole point. The two unguarded near-misses argue against the criticism rather than for it: `loading_screen.lua` and `item_contents_panel.lua` are in no budgeted family and both reached 499 anyway, so the norm is being self-applied, not imposed. There is also no single fix — splitting six modules at cohesive boundaries is six independent design judgments, and CLAUDE.md's guidance for this case is to extract the cohesive boundary rather than force a fit. Counts measured 2026-08-06: **six** files sit at exactly 500, not five — `unit_ai_core.lua` is missing from the list. The 494-499 band is four files but not the same four: `unit_ai_save_refs.lua` (499) is missed and **`settings/data.lua` is now 677**, well outside it. **35** files exceed 500, not 30; the four largest are exact. All six at 500 are in guarded families (#538 ×3, #542 ×2, #543 ×1), as are two of the four near-misses (#538, #544), so the claim that only `loading_screen` and `item_contents_panel` are unguarded holds.

`unit_ai_construct.lua`, `unit_info_v2_status.lua`, `init_mouse.lua`,
`unit_info_v2.lua`, `unit_ai_craft.lua` — all **exactly 500**. Four more are
within six lines: `ui_manager_boot.lua` (499), `loading_screen.lua` (499),
`item_contents_panel.lua` (499), `settings/data.lua` (494).

The five at exactly 500 are all inside budget-guarded families, so the guard is
working — but content pinned precisely at a ceiling is being shaped by the
limit rather than by cohesion, the same signal as `Mouse.hs` at exactly 500
(CH-21). Two of the near-misses (`loading_screen.lua`,
`item_contents_panel.lua`) are in no guarded family at all and reached 499
independently.

Extends CH-22: the budget guards 6 named families while 30 files exceed 500,
including the four largest (`ui/dropdown.lua` 1399, `hud.lua` 1321,
`create_world_menu.lua` 1171, `build_tool.lua` 1167).

---

## Batch 16 — `tools/` (Python) (swept 2026-07-25)

130 files, **54,521 lines**. Better shared-infrastructure adoption than
`scripts/` — `probelib` exists and 71 of 72 probes import it — but the library
is routinely imported and then bypassed.

### [#1160] CH-129. `probelib` is imported by 71 of 72 probes and then reimplemented
> **Note:** The `jget` half is exact and is filed as #1160. Verified 2026-08-06: **77 of 78** probes import `probelib` (drift from 71/72, same ratio); exactly **20** define a local `jget`; all 20 import `probelib` and **none** references `send_json` — checked file by file. The three behavioural differences all hold, and there is a **fourth**: `send_json` catches `(ValueError, TypeError)` while `jget` catches only `json.JSONDecodeError`, a strict subclass of `ValueError`, so a `TypeError` out of `json.loads` kills the probe instead of returning the raw value. On difference 3, precisely: `send`'s own `DEFAULT_IDLE` still applies, so behaviour is correct today; what is lost is the ability to tune it. **Correction to the secondary list: those helpers have already DIVERGED, so they are a reconciliation, not a move.** `make_isolated_root` — called "the notable one" here — is **16 definitions in 12 distinct bodies**, not 13 copies; `as_int` is 9 in 5; `ai_off` 4 in 3. Other counts drifted up too (`expect` 5→10, `boot_probe` 5→7, `num` 4→5). That strengthens the underlying argument while invalidating "move the 13-plus common helpers into `probelib`" as the method, so #1160 scopes to `jget` and records the measured divergence for whoever scopes the rest.

`tools/probelib.py` is a real shared library with a documented purpose,
including the debug-console idle-read gotcha its module header explains at
length ("The console keeps the TCP connection open … idle gap and returns the
last non-empty `"> "` result line", `DEFAULT_IDLE = 0.3`).

Probes import it — and then define their own copies of what it provides:

| Helper | probelib has it | Local copies |
|---|---|---:|
| `jget` / `send_json` | `send_json` | **20** |
| `spawn_acolyte` | yes | **6** |
| `poll_until` | yes | **4** |

All 20 `jget` files import `probelib`; **none** references `send_json`. And the
local copy is not equivalent:

```python
# local jget (20 files)                    # probelib.send_json
def jget(port, lua, timeout=10.0):         def send_json(port, lua, timeout=10.0,
    raw = send(port, lua, timeout)                       idle=DEFAULT_IDLE):
    try:                                       raw = send(port, lua, timeout=timeout, idle=idle)
        return json.loads(raw)                 if not raw: return None
    except json.JSONDecodeError:               try: return json.loads(raw)
        return raw.strip('"')                  except (ValueError, TypeError): return raw
```

Three behavioural differences, none documented anywhere:

1. **Empty result** — `send_json` returns `None`; `jget` feeds `""` to
   `json.loads`, catches the error, and returns `""`. A probe checking
   `if result is None` behaves differently depending on which it called.
2. **Parse failure** — `send_json` returns the raw string; `jget` returns
   `raw.strip('"')`.
3. **`idle` is unreachable** — `jget` calls `send` positionally and cannot pass
   `idle`, so the one knob probelib exists to expose for the documented
   console-read gotcha is hardcoded away in 20 probes.

Also duplicated 4+ times across `tools/` without a probelib equivalent:
`bootstrap_defs` (18), `make_isolated_root` (13), `spawn_station` (9),
`as_int` (9), `run_dump` (7), `boot_probe` (5), `count_item` (5), `expect` (5),
`wid` (4), `wait_active` (4), `num` (4), `find_flat_strip` (4), `ai_off` (4).

`make_isolated_root` (13 copies) is the notable one — isolated resource roots
are how `persistence_contract_sweep.py` keeps probes from colliding, and the
setup is written thirteen times.

Fix: move the 13-plus common helpers into `probelib`, then replace the local
`jget` definitions with `send_json` — the semantic differences above have to be
reconciled deliberately, not by blind substitution.

### [no-issue] CH-130. The seven largest files in the project are all tests and tooling
> **Disposition:** No issue — the size family is settled policy and the one distinctive claim has a good explanation. CLAUDE.md:38-43 states the 500-line limits are per-split ratchets and "not a tree-wide size policy"; CH-22, CH-116, and CH-128 all closed on that, and "nothing bounds file size outside the six Lua and three Haskell families" is a restatement of it. The distinctive part — `test_persistence_inventory_audit.py` at **2.03×** its subject while siblings sit at **0.97×** and **0.63×** — is real but proportionate: the file is **164 test functions** over ~840 lines of fixtures, and its names are mutation cases against a regex parser for Haskell record declarations (`stray_brace_in_comment_is_harmless`, `unbalanced_brace_in_comment_does_not_truncate`, `nested_block_comment_does_not_truncate`, `survives_brace_in_string_literal_type`). A parser whose silent failure is truncating a record and under-reporting fields earns enumerated cases; the two siblings are smaller because they parse less. Counts measured 2026-08-06 and the table is now wrong in four rows: `save_compat_audit.py` **2138** (was 1951), `SaveModules.hs` **2021** (was 1728), `persistence_inventory_audit.py` **1707** (was 1689), and a file it does not list — `tools/expedition_loop_probe.py` at **2093** — now ranks 4th, making it eight files, not seven. "No production source until rank 8" is now **rank 11**, and the file there is `scripts/ui/dropdown.lua` (1399); `World/Save/Types.hs` has shrunk to **1172** and left the top thirteen.

| Lines | File |
|---:|---|
| 3430 | `tools/test_persistence_inventory_audit.py` |
| 2806 | `test-headless/Test/Headless/UI/ResponsiveGameplay.hs` |
| 1951 | `tools/save_compat_audit.py` |
| 1867 | `tools/playtest/critic.py` |
| 1728 | `test-headless/Test/Headless/Lua/SaveModules.hs` |
| 1689 | `tools/persistence_inventory_audit.py` |
| 1674 | `test-headless/Test/Headless/UI/ResponsiveMenus.hs` |

**No production source file appears until rank 8** (`World/Save/Types.hs`,
1316 — of which 296 lines is one comment, CH-40).

`test_persistence_inventory_audit.py` at 3430 lines is the largest file in the
repository, and it is a test for a 1689-line tool — **twice the size of its
subject**. Its two siblings are proportionate (`test_engine_env_capability_audit.py`
1066 vs 1126; `test_save_compat_audit.py` 1308 vs 1951), so this one is an
outlier rather than a house style.

Nothing bounds file size outside the six Lua and three Haskell families the
budget scripts name (CH-21, CH-22, CH-116, CH-128). The three guards cover
neither `tools/` nor any test tree — i.e. none of the seven files above.

### [no-issue] CH-131. `tools/` is 122 flat files that divide cleanly by role
> **Disposition:** No issue — a restatement of CH-25, whose `[no-issue]` disposition names this finding directly ("CH-131 restates this finding and needs the matching disposition"). The premise is unchanged, only the counts. Its decisive argument re-verified independently 2026-08-06 and now larger: **126** distinct `tools/*.py` paths are cited at **449** sites outside `tools/` across 75 files, plus **393** inside (CH-25 measured 124/407/385). The "one atomic sweep" this entry says the move needs is not achievable, because part of the invocation surface is outside the repository where a PR cannot reach it — `~/.codex/rules/default.rules` cites `tools/*.py` paths, as do at least five files in this project's agent memory; a stale path there fails as `No such file` exactly when an agent runs a gate. The partition argument stands too: the role split already exists in the filenames, which is why this entry can classify every file "without ambiguity" from names alone — `ls tools/*_probe.py` is `tools/probes/`, and directories would make the suffixes redundant — while the two navigation surfaces cited as evidence of a gap are the deliberate answer (`tools/README.md`'s curated table; `ci_probes.py --status`, which CLAUDE.md declares authoritative over any prose list). Counts drifted again: **144** Python files (**136** flat), **78** probes, **15** audits, 6 checks, **17** `test_*` — against this entry's 130/72/10/6/14 and CH-25's own 127/76.

Confirmed and quantified from CH-25: 130 Python files, only `playtest/` and
`baselines/` as subdirectories. The flat 122 partition without ambiguity:

| Count | Kind |
|---:|---|
| 72 | `*_probe.py` |
| 10 | `*_audit.py` |
| 6 | `*_check.py` |
| 14 | `test_*.py` |
| ~20 | reports, generators, shared helpers |

`tools/README.md` and `tools/ci_probes.py --status` exist to navigate what
`tools/probes/`, `tools/audits/`, `tools/reports/`, `tools/tests/` would make
self-evident. The move touches every `python3 tools/x_probe.py` invocation in
CLAUDE.md, CI, and the skills, so it needs one atomic sweep — but the
categories are already unambiguous, which is the hard part.

### [no-issue] CH-132. Minor `tools/` defects for one cleanup issue
> **Disposition:** No issue — all three bullets resolve, and the one actionable request is already satisfied. **Bullet 1** is confirmed (of **78** probes, exactly one omits `probelib`) but the comment it asks for already exists: `preview_cli_probe.py`'s module docstring opens "Every check here is a PRE-BOOT rejection or exit — no GPU, no window, no engine thread ever starts … This is what makes the probe CI-eligible", which is precisely why a boot-and-console library is irrelevant to it. Two corrections: the file is **329** lines, not 158, and it is 1 of 78 probes, not 72. **Bullet 2** declares itself no action and verifies: `tools/.gitignore` carries `__pycache__/` and `*.pyc`, and `git check-ignore -v tools/__pycache__` resolves to that rule. **Bullet 3** is answered, and needed one check CH-118 did not cover — that entry's tail named `action_outcome_coverage` as its third tool while this one names **`save_compat_audit`**. Checked fresh: `save_compat_audit` does no "is this used?" scanning; it is fixture-driven, and `test-headless/` is not omitted from it but is its entire data source (`FIXTURE_DATA_DIR = REPO_ROOT / "test-headless" / "data" / "save-compat"`). CH-118 already verified the other two. CH-104's recommended fourth audit is filed as #1145.

- `tools/preview_cli_probe.py` (158 lines) is the only probe of 72 that does
  not import `probelib` — legitimate (it is the CI-eligible no-boot probe and
  never opens a console), but worth a one-line comment saying so, since "every
  probe imports probelib" is otherwise an invariant a reader would assume.
- `tools/__pycache__/` is correctly ignored via `tools/.gitignore` — verified,
  no action.
- The three audit tools with self-tests (`persistence_inventory_audit`,
  `engine_env_capability_audit`, `save_compat_audit`) are the project's model
  for enforcement, and CH-104 recommends a fourth in the same shape for the
  append-only enum rule. Their own "is this used?" logic should be checked for
  the `test-headless/` omission described in CH-118 before their coverage
  claims are relied on.

---

## Sweep complete

Every source tree in the repository has now been audited:

| Batch | Area | Findings |
|---|---|---|
| 1 | `Engine/Core` | CH-1 … CH-16 |
| 2 | `Engine/Loop`, `Input`, `Asset`, structure | CH-17 … CH-27 |
| 3 | `Engine/Graphics` | CH-28 … CH-39 |
| 4 | `Engine/Scripting` | CH-40 … CH-49 |
| 5 | remaining `Engine` | CH-50 … CH-57 |
| 6 | `app/` | CH-58 … CH-69 |
| 7 | `World/Save`, `World/Load`, `Engine/Save` | CH-70 … CH-78 |
| 8 | worldgen pipeline | CH-79 … CH-91 |
| 9 | `World/Thread`, `Render`, `ZoomMap` | CH-92 … CH-99 |
| 10 | `Unit`, `Combat` | CH-100 … CH-105 |
| 11 | `Sim`, `Power`, `Infection`, `Craft` | CH-106 … CH-110 |
| 12 | `Building`, `Structure`, `Location`, `LootTable` | CH-111 … CH-114 |
| 13 | `Item`, `Language`, `Blood`, test suites | CH-115 … CH-119 |
| 14 | `UI` (Haskell) | CH-120 … CH-123 |
| 15 | `scripts/` (Lua) | CH-124 … CH-128 |
| 16 | `tools/` (Python) | CH-129 … CH-132 |

**132 findings.** Recurring themes worth filing as cross-cutting issues rather
than per-site fixes:

- **Copied primitives** — `baseTileW`/`baseTileH` ×8 (CH-92), `applyFacingF` ×3
  (CH-111), `clamp` ×11 and `formatGameTimeHMS` ×4 (CH-125), `tshow` ×4
  (CH-75), `floorDivCS` ×5 (CH-84), `jget` ×20 (CH-129), the fluid surface fold
  ×5 (CH-82), the item walk ×3 (CH-70).
- **Comments that contradict their code** — CH-4, CH-9, CH-32, CH-33, CH-64,
  CH-85, CH-86, CH-96, CH-100, CH-102, CH-112, CH-126.
- **Constants that must agree with nothing enforcing it** — CH-31 (16384 ×5),
  CH-35 (the UBO ×5), CH-89 (material ids), CH-104 (append-only enums).
- **Structure without a stated rule** — CH-56/CH-78/CH-107 (`X.hs` beside
  `X/`), CH-25/CH-127/CH-131 (flat directories), CH-97/CH-103 (duplicate
  basenames).
- **Guards that don't cover what they claim** — CH-21 (glob hole), CH-22/CH-116
  /CH-128 (budget scope), CH-115 (a suite that never runs), CH-118 (this
  audit's own tooling).

---

## Batch 17 — `docs/` markdown (swept 2026-07-25; CLAUDE.md excluded at owner's request)

9 live docs, 4,381 lines (excluding `docs/history/`, which is explicitly
labelled "superseded — context only", and this report).

Every code reference in every live doc was checked against the tree. **The
result is better than expected**: the two authoritative persistence docs
correctly mark deleted modules as deleted, `expedition_gameplay_loop.md`'s
cross-references resolve exactly, and `README.md` is accurate. The problem is
narrower and specific — **two large design docs still describe shipped systems
as unbuilt.**

### [#1161] CH-133. `player_events.md` (786 lines) is marked "ready to implement" for a system that shipped
> **Note:** Verified 2026-08-06 — status line, shipped modules (`PlayerEvent.hs` 105, `Emit.hs` 173, `Lua/API/PlayerEvent.hs` **293** not 290), and the phantom manifest entries all confirmed: `git log --all` returns nothing for `src/Engine/Event.hs` or `src/Engine/Scripting/Lua/API/Event.hs`, so they never existed under any name. **But this entry misses that the doc already declares itself history**: `:9-19` carries a dated note — "**Note (2026-06, issue #37):** this is the original design record… **treat this doc as design history, not a current API reference**" — four lines below the stale Status line. So "a reader planning work against it would rebuild something that exists" overstates it; what is actually wrong is narrower and sharper, and #1161 scopes to it. (1) `:7` **contradicts** `:9`, two dated statements four lines apart disagreeing about whether the system is built. (2) The note corrects the DESIGN (retired `PopupButton`/`PopupAction`, the removed `buttons:` key) but says nothing about MODULE NAMES, so a reader who has accepted it still hunts `Engine/Event.hs`. Also: three of the manifest's six entries are correct (`YamlNotifications.hs`, `popup.lua`, `notification_categories.yaml` all exist). The fix follows an established precedent rather than inventing one — `docs/history/` exists with a curated README ("Do not treat them as the current state of the system"), and closed **#1108** moved `river_rework.md` there for exactly this situation.

> **Disposition:** #1161 archives the document as
> `docs/history/player_events.md`, marks it as an implemented historical design
> record, and corrects its phantom module names while preserving the useful
> divergence note.

Before archival, line 7 read:
`Status: design accepted 2026-05-18. Phase 1 ready to implement.`

The player-event system is fully built: `src/Engine/PlayerEvent.hs`,
`src/Engine/PlayerEvent/Emit.hs`,
`src/Engine/Scripting/Lua/API/PlayerEvent.hs` (290 lines),
`data/notification_categories.yaml`, and `engine.getEventLog()` /
`engine.emitEvent` documented in CLAUDE.md as working APIs.

Worse, the doc's own file manifest (lines 667-670) names modules that do not
exist and never did:

```
- `src/Engine/Event.hs` — `Event`/`PopupButton`/`PopupAction` types,
- `src/Engine/Scripting/Lua/API/Event.hs` — Lua API binding
```

Both shipped as `PlayerEvent`, not `Event`. Sections 3-4 are still written in
future tense (`-- src/Engine/Event.hs (new module)`, `(new)`), and the design
table repeatedly qualifies decisions with "in Phase 1" as though Phase 1 were
pending.

Before archival, the largest unenforced live doc was out of sync three ways at
once: status, module names, and tense. A reader planning work against it would
have rebuilt something that exists.

### [no-issue] CH-134. `blood_decals.md` (445 lines) is marked "design draft" for a shipped subsystem
> **Disposition:** No issue — already fixed. `docs/blood_decals.md:3-6` now reads "**Status: implemented** — this is the final documentation/verification gate for epic #603 … **This is the as-built record**", not "design draft, written 2026-07-07". `git log -S'Status: design draft'` shows that string entered at `b09c1518` (2026-07-07) and was replaced by `c8884cb4` (2026-08-01, "Finalize blood decal documentation and gate the bleeding arc for epic closure"), with two further review-fix commits to the same file that day. The parenthetical resolves too: `Blood.Types.BloodStore` exists — `src/Blood/Types.hs:52` exports `BloodStore(..)`, `:53` `emptyBloodStore`, and the module haddock at `:9` describes it. Everything measured has also grown: the doc is **461** lines not 445, `src/Blood/` is **6** modules and **1787** lines not 5/1413, `Lua/API/Blood.hs` is **538** not 519, and there are **four** blood probes not three — `bleeding_trail_probe.py` (the #882/#883 gate) is missing from the list. CLAUDE.md:987 cites the doc as the "full architecture record", now consistent with its own status line. **NB CH-135's table row for `blood_decals.md` ("design draft" ✗) is stale for the same reason.**

Line 3: `Status: design draft, written 2026-07-07.`

Shipped: `src/Blood/` (5 modules, 1413 lines), `Engine/Scripting/Lua/API/Blood.hs`
(519 lines), and **three** probes (`blood_decal_probe.py`,
`blood_impact_probe.py`, `blood_gpu_lifecycle_probe.py`).

Unlike CH-133 this one is a one-line fix: the doc's module names
(`Blood.Impact`, `Blood.Render`, `Blood.Texture`, `Blood.Types`) match what
shipped, so the design was implemented as designed. Only the status line lies.

(One reference to check while editing: `Blood.Types.BloodStore` does not
resolve against the current `Blood/Types.hs`.)

### [no-issue] CH-135. Status markers are inconsistent, and two of the six that exist are wrong
> **Disposition:** No issue — both wrong markers are fixed and the proposal's
> premise no longer describes `docs/`. `blood_decals.md:3` has read `Status:
> implemented` since `c8884cb4` (2026-08-01, CH-134's evidence) and
> `player_events.md:7` was archived with a `**Status:** Historical design
> record` marker by #1161 (2026-08-09), so of the six markers this entry
> counted, **zero are now wrong**: the four surviving document-level ones
> (`persistence_contract.md`, `persistence_state_inventory.md`,
> `engineenv_capability_inventory.md`, `blood_decals.md`) each verify against
> the tree. Three further table rows are stale, including the model the
> proposal copies: `texture_infrastructure.md`'s `**Status:**
> Pre-implementation, written 2026-05-24` was demoted to `**Legacy status:**`
> at `:840` when the doc was rewritten as a design-epic document, and it and
> `expedition_gameplay_loop.md` now both carry `Design state:` at the top.
> The population changed 9× too — 9 live docs / 4,381 lines at sweep time,
> **82 live docs / 30,408 lines** now — and partitions into three families
> that each already run a maintained convention: **9** design docs
> (`Design state:` + the per-slice ledger `/process-design-doc` advances),
> **62** findings reports and project reviews (`Status legend:` + a per-entry
> checklist, agreement-audited in CI by `tools/findings_report_audit.py`,
> #1196), and **11** others of which 4 carry an accurate `**Status:**` line.
> So "one status line on every `docs/*.md`, checked in CI" would today either
> bind 62 generated files whose status is per-entry rather than per-document,
> or ship the exemption list that is this same inconsistency rewritten in
> Python. The residue — 7 unmarked docs — is not discarded: `asset_generation.md`
> and `player_manual.md` are the only two that existed at sweep time and
> **CH-136 itemizes both by name**; `hydrology_pipeline.md` (2026-08-09) and
> `engine_contracts.md` (2026-08-18) postdate this entry, and the remaining
> three are transient working notes.

| Doc | Marker |
|---|---|
| `persistence_contract.md` | **Status:** Authoritative ✓ |
| `persistence_state_inventory.md` | **Status:** Authoritative ✓ |
| `engineenv_capability_inventory.md` | **Status:** Authoritative ✓ |
| `texture_infrastructure.md` | **Status:** Pre-implementation, 2026-05-24 ✓ |
| `history/player_events.md` | **Status:** Historical design record — implemented with divergence; archived 2026-08-09 ✓ (CH-133 / #1161) |
| `blood_decals.md` | "design draft" ✗ (CH-134) |
| `expedition_gameplay_loop.md` | — none — (has its own status *section*) |
| `asset_generation.md` | — none — |
| `player_manual.md` | — none — |
| `history/river_rework.md` | **Status:** Not adopted, archived 2026-08-05 ✓ (CH-79 / #1108) |

The project already has the right convention —
`texture_infrastructure.md`'s `**Status:** Pre-implementation, written
2026-05-24` is the model `history/river_rework.md` was given in #1108 to stop
it reading as a live plan (CH-79). It just isn't applied uniformly, and where
it is applied it isn't maintained.

Proposal: require one status line on every `docs/*.md` — one of
*Authoritative* / *Pre-implementation* / *Implemented (see §X)* / *Superseded* —
with a date, and check it in the same CI step that already validates the
persistence and capability inventories.

### [no-issue] CH-136. Minor doc defects for one cleanup issue
> **Disposition:** No issue — three of the five bullets are fixed and the two
> survivors have shrunk below the bar this report files at. (1) The
> `test/Test/Headless/Harness.hs` citation is gone:
> `engineenv_capability_inventory.md:957` reads
> `test-headless/Test/Headless/Harness.hs`, and no bare `test/` citation
> remains in that document. (2) `texture_infrastructure.md`'s "still the plan,
> or move to `docs/history/`?" is answered — neither: it became the design
> authority for epic #1256, with TEX-1/2/3/4/6/7 shipped (#1257–#1262) and
> TEX-5 deliberately deferred, and `animations.yaml` is recorded at `:169` and
> `:389` as a **rejected** alternative rather than a pending plan (the
> "pre-implementation" label this bullet cites survives only as
> `**Legacy status:**` at `:840`). (5) The blood subsystem now has full
> CLAUDE.md coverage at `:1202-1215` — architecture-record link, five hspec
> groups, four probes, the transience contract. The two survivors both
> weakened on inspection: `asset_generation.md`'s account state has not gone
> stale and is not silent — checked live, `Tier 2: Pixel Artisan` /
> `generations_total: 5000` matches the doc, whose same sentence already says
> "Check with `get_balance`", and whose header self-dates its recipes to
> 2026-06; and `player_manual.md` is demonstrably maintained rather than
> drifting (`6d337004` #782, `4e8d2d08` #923, `8e17960d` #913 in six weeks,
> plus #776's correctness fix), so a "which build" marker on a document that
> feature PRs already update is itself a thing that goes stale. What is left is
> one optional date stamp, below the bar of the filed siblings (#1011, #1086,
> #1154, #1156) and in line with the closed ones (CH-27, CH-132). This is also
> the answer to the residue CH-135 routed here.

- **`engineenv_capability_inventory.md:709`** cites
  `test/Test/Headless/Harness.hs`; the file is at
  **`test-headless/`**`/Test/Headless/Harness.hs`. This is the same `test/` vs
  `test-headless/` confusion that produced CH-118 in this audit — in the
  document the capability audit treats as authoritative. (The audit script
  itself keys on module names, not this path, so nothing is broken; the doc is
  simply wrong.)
- **`texture_infrastructure.md`** is honestly labelled pre-implementation, but
  it was written 2026-05-24 and `animations.yaml` still does not exist. Worth
  an explicit decision — still the plan, or move to `docs/history/`?
- **`asset_generation.md`** has no status line and states "Tier 2 subscription
  (~5000 generations/cycle)" as current account state — a fact that goes stale
  silently and is worth dating.
- **`player_manual.md`** has no status line. Its content spot-checks as
  accurate against shipped features, but it is the one player-facing document
  and has no marker saying which build it describes.
- **The blood subsystem has no CLAUDE.md coverage** — 1413 Haskell lines, a
  519-line Lua API, and three probes, with `blood` appearing zero times in
  CLAUDE.md's subsystem contracts. Noted for completeness only; CLAUDE.md was
  excluded from this pass at the owner's request.

### [no-issue] CH-137. Verified: four docs are accurate and worth using as the pattern
> **Disposition:** No issue — this entry records a verification rather than a
> defect, and re-verification confirms all four still hold.
> `expedition_gameplay_loop.md`'s `### 9. Gate the full slice` still resolves
> (moved `:327` → `:1308`) and `## Implementation status` is still at `:390`;
> CLAUDE.md `:1185`'s claim that step 9's combat encounter and progression
> reward are deferred to #916/#917 matches the tracker (both **OPEN**) and the
> doc's own EXP-5 precondition. `persistence_contract.md` (`:87`, `:739`,
> `:743`) and `persistence_state_inventory.md` (`:152`, `:727`) still cite
> `LoadWorld.hs` and still mark it deleted, and the file is genuinely gone —
> `src/World/Thread/Command/Save/` holds only `WriteWorld.hs`. `README.md`
> spot-checks accurate on prerequisites, build commands, headless mode, the
> resource-root precedence, and testing. Only the framing moved: CLAUDE.md's
> 2026-08-18 trim dropped the "all discretionary work gates on step 9"
> sentence this entry quoted. **The closing lesson has since been
> implemented** — design docs now carry the lifecycle step it said they
> lacked, a `Design state:` line plus a per-slice ledger advanced by
> `/design-epic` → `/process-design-doc`, which `expedition_gameplay_loop.md`
> itself gained at `:8` and `:37` (EPIC #1229, EXP-1…EXP-5). That is the same
> convention CH-135's disposition rests on.

Recorded because the reference sweep was expected to find widespread rot and
did not:

- **`expedition_gameplay_loop.md`** — the model. CLAUDE.md gates all current
  discretionary work on "step 9, *Gate the full slice*, of the loop doc"; that
  reference resolves exactly (`### 9. Gate the full slice`, line 327). It
  carries its own `## Implementation status` section naming the shipped issues
  (#777-#782), explicitly reconciles terminology with what shipped ("the
  terminology above matches what shipped: 'remote-start threshold' is the
  `building.remoteCheck` distance gate…"), and states plainly what remains.
- **`persistence_contract.md`** and **`persistence_state_inventory.md`** — both
  name `src/World/Thread/Command/Save/LoadWorld.hs`, which no longer exists,
  and both mark it "(deleted)" / "deleted by" in the same sentence. Correct
  historical referencing, not rot.
- **`README.md`** — accurate on prerequisites, build commands, headless mode,
  the resource-root contract, and testing.

The lesson for CH-133/CH-134: the failure is not that docs drift from code —
these four don't — it is that *design* docs have no lifecycle step that marks
them implemented.

### [#1482] CH-138. Every GitHub Actions dependency is pinned by mutable tag, not by SHA
> **Note:** Verified 2026-08-18, with three corrections. (1)
> **`secrets.NTFY_URL` is NOT exposed** — `ntfy-notify.yml` has zero `uses:`
> entries (pure `run:` + `curl`) and secrets do not cross workflows, so no
> third-party action here can reach it; drop that consequence. The real
> exposure stands on its own:
> `packages: write` (`ci-image.yml:36-38`, `:64-66`; `ci.yml:153-155`),
> `secrets.GITHUB_TOKEN` handed to `docker/login-action` at `ci-image.yml:79`,
> and `pull-requests: write` (`review-gate.yml:52-54`) in the job running
> `actions/checkout@v4` at `:57`. (2) It is seven DISTINCT actions across **ten**
> call sites, not seven references. (3) **All seven are a major behind, not
> four** — the `docker/*` entries escape the Node 20 warning but not currency:
> `setup-buildx-action` v3 → **v4.2.0**, `login-action` v3 → **v4.6.0**,
> `build-push-action` v6 → **v7.3.0**, beside `checkout` v4 → **v7.0.1** and
> `cache` v4 → **v6.1.0**. A complete pinning pass therefore touches all ten
> sites, which reinforces the bump-first ordering rather than changing it.

All seven third-party `uses:` references across the four workflows are pinned to
a floating major tag rather than a commit SHA:

```
4  uses: actions/checkout@v4
1  uses: actions/cache@v4
1  uses: actions/cache/restore@v4
1  uses: actions/cache/save@v4
1  uses: docker/setup-buildx-action@v3
1  uses: docker/login-action@v3
1  uses: docker/build-push-action@v6
```

A tag is mutable: the owning repository can repoint `v4` at any commit at any
time, and a compromised or simply retagged action then executes inside
workflows holding `packages: write` (`ci.yml`'s `resolve-image`,
`ci-image.yml`) and `pull-requests: write` (`review-gate.yml`), with access to
`secrets.NTFY_URL` and the registry credentials. SHA pinning is the standard
mitigation and is what `actions/checkout`'s own hardening guidance recommends
for anything beyond a hobby repository.

Nothing here is known to be compromised; this is unexercised risk, not an
incident.

Two things make it a real rather than theoretical concern for THIS repository:
the merge pipeline is substantially automated (a drainer merges approved PRs
under admin bypass), so a malicious action would run against a branch nobody is
watching interactively; and the CI image is published to a registry the
workflow can write to, so a compromise is persistent rather than per-run.

Sequencing matters if this is taken up. The same four `actions/*` entries are
also on a **deprecated major** — they target Node 20, GitHub currently
force-runs them on Node 24 and warns in every build log, and the current majors
are `checkout` v7 and `cache` v6. Pin AFTER bumping, or the SHAs land on the
version that is about to stop working.

**Owner note (2026-08-18):** raised during a CI review and deliberately not
filed yet — the owner wants to explore SHA pinning before committing to it.
Recorded here so the option is not lost.
