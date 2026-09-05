# Unit animation atlas infrastructure design

This design consolidates the unit animation corpus into validated runtime
atlases while preserving the current authoring, animation, and preview
experience.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Compile unit animations into validated runtime atlases — [#1256]
- [x] TEX-1. Make the unit asset inventory authoritative and enforceable — [#1257]
- [x] TEX-2. Build the deterministic per-animation atlas compiler and index — [#1258]
- [x] TEX-3. Add atlas-frame storage and sampling to the unit runtime — [#1259]
- [x] TEX-4. Migrate acolyte as the end-to-end atlas pilot — [#1260]
- [ ] TEX-5. Add portable KTX2 atlas loading when the memory budget requires it — [deferred]: resume when the recorded unit-texture memory budget is exceeded — `tools/pack_atlas.py --validate-only --strict` checks it against `tools/unit_texture_budget.json` on every run and warns by name when it fires (measured 101.60 MiB, projected 203.19 MiB, threshold 384 MiB — see *Measured results*)
- [x] TEX-6. Migrate the remaining units and retire per-frame runtime loading — [#1261]
- [x] TEX-7. Close the pipeline with documentation and focused regression gates — [#1262]

## Epic contract

- **Goal:** Compile unit animation sources into deterministic per-animation
  atlases that materially reduce runtime images and registrations while
  preserving every existing animation and preview behavior.
- **Done when:** Every tracked unit uses the lossless PNG atlas format; gameplay and
  preview preserve timing, direction, looping, mirroring, dimensions, and frame
  choice; strict validation and freshness checks pass from a fresh checkout; the
  per-frame unit-animation runtime path is retired; and before/after resource
  measurements are recorded. The explicitly deferred KTX2 slice does not block
  this epic's PNG-atlas completion condition.
- **Users and operators:** Unit-art authors, gameplay and rendering maintainers,
  contributors running previews or validation, CI, and players loading units.
- **Arc label:** None proposed

## Problem

Unit animation content has outgrown the original single-unit loading model. The
repository now contains four unit definitions and 3,925 distinct animation frame
PNGs referenced by `data/units/*.yaml`. At runtime, every referenced path is
decoded through JuicyPixels, uploaded as its own Vulkan image, and assigned its
own bindless texture slot. The current assets consume about 70.8 MiB as decoded
RGBA pixels before image-allocation overhead, despite occupying only about 7.3
MiB as PNG files on disk.

The repository already has several pieces of the intended system: per-animation
frame rate, looping and horizontal mirroring are data-driven; directions can be
mirrored at render time; the preview browser understands unit animations; and
`tools/pack_atlas.py` validates the unit asset inventory. It does not, however,
pack atlases or produce a runtime index.

As of TEX-1 (#1257) its strict mode IS an enforceable gate: discovery is
filesystem-first rather than YAML-first, so it covers the complete corpus of 7
unit trees, 116 animations and 4,620 frames, and it exits 0 with zero warnings.
Every committed animation PNG is owned by exactly one animation-frame
declaration, with no directory- or glob-level exemption mechanism. See
**Unit animation art** in `CLAUDE.md` (and `docs/engine_contracts.md`
§Unit animation art) for the enforced contract.

The legacy plan also predates the current asset contract. Unit animation metadata
now lives in `data/units/*.yaml`, not a separate `animations.yaml`, and some
animations legitimately have different frame counts by direction. A viable
atlas pipeline must consolidate runtime images without weakening that existing
authoring model, breaking asymmetric animations, or introducing generated files
whose freshness cannot be verified.

## Scope

### In scope

- Compile the existing per-frame unit artwork into deterministic per-animation
  atlases with an explicit machine-readable index.
- Reduce runtime image, allocation, descriptor, and file-load counts while
  preserving the visible timing, dimensions, direction, looping, reversal,
  stride, force-loop, and mirroring behavior of every animation.
- Establish one authoritative animation manifest and make every compiled
  artifact mechanically traceable to it.
- Turn unit-asset validation into a clean, actionable gate that rejects missing,
  malformed, inconsistent, or stale assets before runtime.
- Preserve the current unit preview workflow and make it exercise the same atlas
  metadata and sampling path as gameplay.
- Make atlas metadata and runtime selection format-neutral, and preserve a
  durable deferred slice for eventual GPU-compressed KTX2 artifacts without
  assuming that every target device exposes BC7.
- Keep the migration incremental: one unit can prove the complete path before
  legacy per-frame loading is retired.

### Out of scope

- Changing animation names, state-selection semantics, frame timing, or combat
  animation-class ownership.
- Per-frame anchors, equipment overlays, palette swaps, shader-driven color
  replacement, or paper-doll composition.
- Texture streaming, asynchronous loading, world LOD, or mipmapped unit sprites.
- Replacing the current bindless handle-to-slot indirection or the existing
  deduplication and batched-upload infrastructure.
- General-purpose atlas packing for UI, terrain, flora, buildings, or arbitrary
  texture families.
- Live repacking or hot reload while the engine is running; restarting the
  relevant preview or game session is acceptable.
- Deleting source-frame PNGs after compilation.

## Desired experience

### Content author

1. Generate or edit individual PNG frames in the established
   `assets/textures/units/<unit>/animations/...` hierarchy.
2. Update the unit's authoritative animation declarations when frames,
   directions, timing, looping, or mirroring change.
3. Run one compiler command that validates the source inventory and regenerates
   only affected atlas artifacts and indices.
4. Use `--preview units/<unit>` to inspect animation names, directions, playback,
   mirroring, dimensions, and atlas sampling before committing the change.
5. Receive a specific build or CI error when a source, declaration, index, or
   compiled artifact is missing, orphaned, malformed, or stale.

### Runtime

1. Load one compiled index for the selected unit definition.
2. Load one image per animation instead of one image per frame.
3. Select a logical direction and frame using the existing animation state and
   timing rules.
4. Resolve that frame to an atlas cell and optional horizontal mirror.
5. Draw and hit-test using the cell's dimensions and UV rectangle, never the
   dimensions of the full atlas.

### Fresh checkout and CI

1. A fresh checkout has everything necessary to run the game and previews; it
   does not silently depend on an unrecorded local asset-generation step.
2. CI validates manifest/source consistency and proves that checked-in compiled
   artifacts match their recorded source digest.
3. The validation path is quick enough to run selectively when unit definitions,
   unit animation sources, the compiler, or compiled artifacts change.

## Current state and repository evidence

- `data/units/acolyte.yaml`, `bear_brown.yaml`, `red_squirrel.yaml`, and
  `technomule.yaml` embed the current animation declarations. Each declaration
  carries `fps`, `loop`, `flip`, and explicit per-direction frame lists; richer
  state-to-animation selection also lives in these unit definitions.
- The four definitions reference 3,925 unique frame PNGs. Their decoded RGBA
  footprint is approximately 70.8 MiB: 20.3 MiB for acolyte, 44.9 MiB for brown
  bear, 1.4 MiB for red squirrel, and 4.2 MiB for technomule.
- `Engine.Scripting.Lua.API.Units.Yaml` currently calls `loadAndRegister` for
  each referenced frame. `Engine.Scripting.Lua.Message.Texture` decodes each
  path with JuicyPixels and creates a distinct Vulkan image for every new path.
- `Unit.Direction.mirrorDir` and `Unit.Render.pickFrame` already implement the
  five-authored-direction mirroring convention and per-animation `flip` choice.
  Asymmetric animations may author all eight directions.
- Four existing acolyte animations have different frame counts across their
  authored directions. An atlas format therefore cannot assume one common
  frame count for every row; it must record each direction's real count and pad
  unused cells deterministically.
- `tools/pack_atlas.py` is currently a validator despite its name. As of TEX-1
  (#1257) it is filesystem-first and enforceable: `python3 tools/pack_atlas.py
  --validate-only --strict` covers the complete corpus (7 unit trees, 116
  animations, 4,620 frames) and exits 0 with zero warnings, and it runs
  unconditionally in `make ci` and post-merge master CI, path-selectively on
  PRs via `tools/ci_expensive_gates.py --gate unit-assets`. Since #1311 it
  also decodes every declared frame and enforces one pixel size per animation,
  so an unreadable or wrongly-sized frame fails the gate rather than the
  upload path.
- `docs/asset_generation.md` documents the live source layout and mirroring
  convention. The legacy proposal for a separate
  `assets/units/<unit>/animations.yaml` was never adopted.
- Unit preview issue #887 is closed, and `tools/preview_probe.py` already checks
  unit animation enumeration, timing, loop behavior, mirrored directions, and
  playback. Atlas migration must preserve that coverage.
- No KTX2 decoder or upload path exists in the repository. KTX2 Basis Universal
  payloads require a runtime transcode target selected from device-supported
  formats; the official
  [KTX library](https://github.khronos.org/KTX-Software/libktx/index.html)
  supplies KTX loading, transcoding, and Vulkan integration. The Vulkan sample
  likewise selects ASTC, ETC2, or BC according to device features rather than
  assuming a single universal GPU format.
- The current bindless shaders declare arrays of `sampler2D`, and the bindless
  descriptor set stores combined image samplers. Vulkan permits independently
  created sampled-image views in those descriptor entries when each selected
  format supports sampled-image use and matches the shader's floating-point
  sampled type. Therefore decoded RGBA PNG atlases and supported compressed
  KTX2-transcode targets can coexist across different bindless slots in one
  session; they do not need separate render pipelines.
- A broad tracker search found no dedicated open or closed atlas/KTX texture
  infrastructure issue. Closed epic #427 and child #887 cover asset preview,
  not this runtime consolidation.

## Design

### Constraints and invariants

- Source frame PNGs remain editable, tracked inputs even after the runtime stops
  loading them individually.
- An animation is the compilation boundary. Its atlas contains only that
  animation's frames, so changing one animation need not rebuild an entire unit
  or unrelated asset family.
- The index records each authored direction explicitly, including its row,
  actual frame count, cell dimensions, and whether runtime mirroring is allowed.
- Rows use a deterministic direction order; shorter directions are padded with
  transparent cells to the animation's maximum authored frame count.
- Every frame in one animation must have the same dimensions and pixel format.
  A mismatch is a validation error, not an implicit rescale or crop.
- Existing animation selection and `pickFrame` time arithmetic remain the source
  of truth. Atlas adoption changes frame storage and UV resolution, not which
  logical frame should play.
- Runtime horizontal mirroring remains an animation-level declaration. The
  compiler must not silently infer mirroring by deleting asymmetric source rows.
- Atlas UV selection must compose with render clipping, hit testing, texture
  handle indirection, nearest filtering, and isolated linear filtering.
- Unit sprites remain non-mipmapped unless a later design establishes an
  art-safe sampling policy.
- A missing, stale, unsupported, or corrupt compiled artifact fails with the unit
  and animation named. There is no silent substitution of an unrelated texture.
- The migration may temporarily support both legacy frame lists and compiled
  atlases, but each animation resolves through exactly one storage mode.
- Legacy loading is removed only after every tracked unit and preview path has
  migrated and parity checks pass.
- Any KTX2 path must negotiate a device-supported transcode target and retain an
  intentional uncompressed fallback for unsupported hardware or development
  configurations. BC7 support is never assumed merely because Vulkan is present.

### Dual-format runtime contract

- The generated index names all available encodings for an atlas and their
  shared logical identity, dimensions, rows, and frame counts. Animation
  metadata and renderer frame selection do not depend on the encoded image type.
- The loader chooses exactly one resident representation for each atlas. Once
  KTX2 support exists, it prefers KTX2 only when the artifact is present, the
  payload can be transcoded, and the resulting `VkFormat` supports sampled-image
  use on the active physical device; otherwise it loads the PNG atlas.
- Different animations may resolve to different representations in the same
  session and occupy ordinary slots in the existing bindless array. This permits
  incremental KTX2 adoption and an uncompressed compatibility path.
- Loading both representations of the same logical atlas simultaneously is not
  normal gameplay behavior because it duplicates image memory and a descriptor
  slot. An explicit diagnostic comparison mode may do so, but the production
  selection contract exposes only one handle for that logical atlas.

## Decisions

### D-1. Preserve individual PNGs as source artwork

Individual lossless PNG frames remain the editing inputs and are not replaced by
hand-edited atlas sheets. Compiled atlases are derived runtime artifacts, so art
can still be generated, reviewed, and corrected frame by frame.

### D-2. Use one atlas per animation

Atlas granularity is one image per animation, not one atlas per unit and not one
image per frame. This contains rebuilds and dimensions to one animation while
removing the dominant per-frame image/descriptor overhead.

### D-3. Preserve playback semantics

Per-animation `fps`, `loop`, and `flip` semantics remain exact. The existing
frame-choice arithmetic, including reversal, stride, and force-loop behavior,
continues to decide the logical frame; atlas work only changes its storage and
UV resolution.

### D-4. Preserve both direction-authoring modes

The established five-direction convention remains valid, with runtime mirroring
for west-facing views only when the animation opts in. Artwork that is not
mirror-safe may continue to author all eight directions.

### D-5. Record unequal direction lengths

Atlas rows may have different logical frame counts. The compiled index records
the real count per direction, and transparent row padding is never exposed as an
animation frame.

### D-6. Honor the player-selected sampler without mipmaps

Gameplay unit atlases follow the player-selected global nearest/linear sampler
and use no mipmaps. The one-texel extrusion ring around each logical cell keeps
linear footprints inside that cell; nearest output remains pixel-identical to
the original per-frame presentation. The preview browser continues to force the
same global sampler to nearest for its session.

### D-7. Require restart-to-reload

Runtime hot reload is out of scope. Source or compiled-asset changes take effect
after restarting the preview or game session, avoiding a second asset-lifecycle
problem inside the atlas migration.

### D-8. Keep composition and streaming outside the arc

Per-frame anchors, equipment overlays, palette swaps, texture streaming, and
general-purpose non-unit atlas support remain outside this arc.

### D-9. Keep preview on the production asset path

The current unit preview remains a first-class acceptance surface and must use
the production atlas/index path rather than a preview-only decoder. It therefore
detects the same malformed metadata and sampling regressions gameplay would see.

### D-10. Defer implementation but require eventual KTX2 support

The immediate critical path ships deterministic PNG atlases and measures their
real resource cost. KTX2 remains an explicit required future capability rather
than an abandoned experiment, but TEX-5 is deferred until the acolyte pilot has
produced a trustworthy PNG-atlas baseline and the projected unit-content plan
would exceed the unit-texture memory budget established from that baseline.

When resumed, KTX2 support is additive: the loader accepts both PNG and KTX2
atlas artifacts, chooses exactly one representation per logical atlas, and may
use different representations for different animations in the same Vulkan
session. KTX2 is preferred when it can be transcoded to a sampled format the
device supports; PNG is the compatibility fallback. Loading both copies of the
same atlas is reserved for an explicit diagnostic comparison because doing so
would duplicate resident image memory and a bindless slot.

### D-11. Keep unit YAML authoritative and generate runtime metadata

Each `data/units/*.yaml` remains the only human-edited semantic declaration for
animation names, source frames, direction ownership, timing, looping, mirroring,
and state selection. The compiler transforms that authority into a versioned
runtime atlas index. Atlas geometry, storage choices, freshness digests, and
other mechanically derivable metadata are generated, then parsed into memory;
contributors do not maintain a second `animations.yaml` or hand-edit the index.

### D-12. Track generated artifacts behind size and churn guardrails

Generated PNG atlases and indices are tracked with their source frames so a
fresh checkout can run without an unrecorded asset-generation prerequisite.
Per-animation atlas granularity also bounds ordinary churn: editing one
animation may change that animation's atlas and its unit index, but must not
rewrite unrelated atlases.

The acolyte pilot is the tracking gate. Before the remaining units migrate, it
must report derived bytes, source bytes, and changed-file locality. The initial
repository budget is that tracked derived PNG artifacts remain no more than two
times the corresponding source-frame PNG footprint. If the pilot exceeds that
ratio or rewrites unrelated artifacts, TEX-4 stops before mass migration and the
team chooses a distribution strategy such as Git LFS, release artifacts, or a
reproducible build cache. A later KTX2 artifact set is measured separately and
must not silently expand this budget.

## Proposals

### P-1. Keep unit YAML as the semantic authority

Accepted by D-11.

Keep each existing `data/units/*.yaml` file as the sole human-edited semantic
manifest. The compiler would read its explicit frame lists and emit a generated
per-unit atlas index containing UV cells, direction counts, dimensions, storage
format, and a digest of all relevant source declarations and pixels. The legacy
plan's second hand-edited `animations.yaml` would not be introduced.

### P-2. Make PNG atlases required and KTX2 evidence-gated

Accepted and strengthened by D-10: KTX2 is eventually required, but its
implementation is deferred behind an explicit resource-budget trigger.

Make deterministic PNG atlases the required consolidation outcome. Treat KTX2
as a measured extension: first land a small portable runtime spike using device
feature negotiation, then adopt compressed shipping artifacts only if measured
memory/load improvements justify the native dependency and cross-platform
packaging cost.

### P-3. Track reproducible runtime artifacts

Accepted with the size and churn guardrails in D-12.

Track generated runtime atlases and indices in Git while retaining all source
PNGs. Pin compiler/tool versions and make CI compare recorded source digests or
deterministically regenerated output. A fresh checkout would then run without a
locally installed packer or prior `toktx` invocation.

## Open questions

### Q-1. Is KTX2 compression required to complete this epic?

Resolved by D-10. KTX2 is an eventual required capability but is deliberately
deferred from the PNG-atlas completion path. TEX-5 owns its later implementation
after the pilot and memory-budget trigger.

This question distinguished immediate epic completion from long-term scale. The
chosen answer makes PNG atlas consolidation independently useful now while
retaining an explicit KTX2 obligation once measured memory pressure justifies
the native loader and packaging work.

### Q-2. Which file is the human-edited animation authority?

Resolved by D-11. Unit YAML remains authoritative and the runtime atlas index is
generated from it.

The rejected alternative was a second editable `animations.yaml`; it would
duplicate names, paths, timing, looping, and mirroring and create an avoidable
synchronization problem.

### Q-3. Where do compiled atlases and indices live?

Resolved by D-12. Generated artifacts are tracked while they remain within the
pilot's explicit size and locality guardrails.

Local/CI-only build outputs were rejected for the initial pipeline because
runtime resources are cwd-relative and expected to work from a fresh checkout.
The size gate preserves a route to external artifact storage if repository cost
eventually outweighs that convenience.

## Delivery plan

### TEX-1. Make the unit asset inventory authoritative and enforceable

> **2026-08-11 — filed as [#1257].** Amended at processing time with user
> signoff: genuinely-obsolete orphans are proposed in the PR and deleted only
> after the owner's explicit confirmation.

- **Outcome:** Strict unit-animation validation is clean, self-tested, and
  selectively enforced in CI.
- **Scope:** Extend `tools/pack_atlas.py`; classify every current orphan warning
  as referenced art, non-runtime source material, or genuinely obsolete content;
  validate names, containment, duplicate paths, direction sets, asymmetric
  exceptions, numbering, and exact orphan ownership; add self-tests
  and path-selective CI wiring. Image-content validation — decodability and
  per-animation pixel-size consistency — was descoped by owner decision on
  2026-08-14, tracked as #1311, and has since landed there.
- **Phase:** 1 — establish trustworthy inputs
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-1, D-4, D-5
- **Acceptance signals:** Strict validation succeeds with zero unexplained
  warnings, and each guarded invariant has a negative self-test that fails for
  the intended reason.
- **Out of scope:** Atlas generation and runtime loading.
- **Open questions:** None

### TEX-2. Build the deterministic per-animation atlas compiler and index

- **Outcome:** One command produces reproducible lossless atlases and versioned
  runtime indices from the authoritative source declarations.
- **Scope:** Pack directions in fixed order; preserve each real direction count;
  pad only unused row cells; record rows, counts, cell dimensions, mirroring,
  storage paths, source digest, and tool version; rebuild changed animations;
  reject stale output and unsafe paths.
- **Phase:** 2 — compile assets
- **Depends on:** TEX-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-4, D-5, D-6, D-11, D-12
- **Acceptance signals:** Compiler self-tests cover unequal lengths, five-way
  mirroring, eight-way artwork, determinism across clean and incremental runs,
  stale output, dimensions, and containment.
- **Out of scope:** Runtime atlas sampling and KTX2 encoding.
- **Open questions:** None

### TEX-3. Add atlas-frame storage and sampling to the unit runtime

- **Outcome:** The engine can load one image per animation and render any logical
  frame from indexed atlas metadata while legacy animations remain temporarily
  compatible.
- **Scope:** Add an explicit format-neutral atlas storage mode; retain current
  frame-time arithmetic; return cell UVs and horizontal flip; use cell
  dimensions for rendering, clipping, hit testing, and debug data; reject
  malformed indices; choose one resident representation per logical atlas; keep
  a narrow whole-animation legacy mode for migration.
- **Phase:** 3 — runtime capability
- **Depends on:** TEX-2
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-3, D-4, D-5, D-6, D-10, D-11
- **Acceptance signals:** Focused headless tests cover loop/reverse/stride,
  mirroring, asymmetric directions, unequal counts, UV bounds, dimensions,
  storage-mode separation, and corrupt-index errors.
- **Out of scope:** Migrating a production unit or removing legacy loading.
- **Open questions:** None

### TEX-4. Migrate acolyte as the end-to-end atlas pilot

- **Outcome:** Every acolyte animation uses the production atlas path in gameplay
  and preview with recorded behavior and resource parity evidence.
- **Scope:** Compile and select acolyte atlas metadata; preserve state animation
  selection and all-direction weapon artwork; make unit preview use the same
  metadata; record image count, slot count, uploaded bytes, load time, and
  representative screenshot or frame-hash comparisons; record source and
  derived artifact bytes and prove an animation edit does not rewrite unrelated
  atlas artifacts.
- **Phase:** 4 — production pilot
- **Depends on:** TEX-3
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-9, D-11, D-12
- **Acceptance signals:** Gameplay and preview parity checks pass; every acolyte
  animation is discoverable; measurements show the expected reduction in images
  and registrations; tracked derived PNG artifacts are no more than twice their
  source-frame PNG footprint; and an isolated animation edit changes only its
  atlas plus the generated unit index. Exceeding either artifact guardrail stops
  mass migration for an explicit storage-strategy decision.
- **Out of scope:** Other units, legacy-path removal, and final compression choice.
- **Open questions:** None

### TEX-5. Add portable KTX2 atlas loading when the memory budget requires it

- **Outcome:** The deferred capability is resumed when its trigger is met, after
  which the engine can choose KTX2 or PNG independently per atlas and different
  encodings can coexist safely in one bindless session.
- **Scope:** Integrate a KTX2 loader/transcoder; negotiate ASTC, ETC2, BC, or an
  uncompressed target from actual device features; prefer supported KTX2 while
  falling back to PNG; preserve one logical atlas handle and one resident image;
  provide an explicit diagnostic-only dual-copy comparison; compare package
  size, uploaded memory, load time, visual fidelity, native dependency, and
  platform packaging costs; measure the additional tracked KTX2 artifacts
  separately against the repository budget.
- **Phase:** Deferred compression capability
- **Depends on:** TEX-4
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-6, D-10, D-11, D-12
- **Acceptance signals:** Selected macOS and Linux configurations have recorded
  results; format selection is derived from device support; unsupported or
  failed KTX2 transcodes fall back to PNG; different atlas records can
  simultaneously occupy PNG-backed and compressed bindless slots; ordinary
  loading never retains duplicate representations of one atlas; and artifact
  size/churn evidence is recorded.
- **Out of scope:** Replacing the format-neutral animation/index model or
  requiring every atlas to use the same encoding.
- **Open questions:** None. Deferral precondition, MECHANICALLY CHECKED since
  TEX-7: the baseline is established (*Measured results*) and the budget that
  activates this slice is recorded in `tools/unit_texture_budget.json` —
  384 MiB of decoded RGBA8 unit-animation atlas projected from the compiled
  indices, compared as `index-projected total x 2.0 > threshold`.
  `pack_atlas.py --validate-only --strict`
  evaluates it on every run and fails the strict gate by name when it fires,
  so this entry stops being `[deferred]` because a check said so, not because
  someone remembered to look.

### TEX-6. Migrate the remaining units and retire per-frame runtime loading

> **2026-08-11 — filed as [#1261].** Amended at processing time with user
> signoff: animation folders TEX-1 confirms as kept are promoted to real
> unit-YAML entries (decoder defaults), so every browsable animation is
> atlased and the per-frame retirement is clean; the preview's YAML-less
> browsing convention ends with this slice.
>
> **2026-08-13 — TEX-1 (#1257) performed the declaration half.** Every
> shipped animation folder now carries a declaration, so no committed
> asset relies on YAML-less browsing any more; the preview retains that
> fallback only for uncommitted local content. TEX-1 deliberately stopped
> at the phase boundary: `tiller`, `unknown_unit` and `white_tailed_deer`
> are declared under the asset-only `asset_units:` key, which is read by
> the validator and the preview but never returned by
> `Engine.Asset.YamlUnits.loadUnitYaml`, so they load no gameplay
> texture and are neither listable nor spawnable. PROMOTING any of them
> to a runtime `units:` definition remains this slice's decision, and
> #1261's specification should be refreshed to say so explicitly before
> it is solved.

- **Outcome:** Brown bear, red squirrel, and technomule use the approved atlas
  representation, and no unit animation loads one image per frame.
- **Scope:** Compile and migrate every remaining state-selected and asymmetric
  animation using tracked PNG atlases; preserve the format-neutral index and
  loader boundary needed by deferred TEX-5; remove unit-animation legacy loading
  and compatibility metadata; leave unrelated direct single-texture loading
  unchanged.
- **Phase:** 6 — complete migration
- **Depends on:** TEX-4
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-7, D-8, D-9,
  D-10, D-11, D-12
- **Acceptance signals:** All unit previews and focused gameplay tests pass; no
  unit animation registers per-frame images; and no referenced or compiled unit
  artifact is stale or orphaned.
- **Out of scope:** Atlasing any non-unit texture family.
- **Open questions:** None. D-10 fixes PNG as the current representation and
  preserves a format-neutral boundary for deferred TEX-5.

### TEX-7. Close the pipeline with documentation and focused regression gates

> **2026-08-11 — filed as [#1262].** Amended at processing time with user
> signoff: the TEX-5-activating unit-texture memory budget is proposed by the
> solver from measured baselines and confirmed by the owner before being
> recorded.

- **Outcome:** Contributors can regenerate, validate, inspect, and review unit
  atlases from a fresh checkout, with regressions caught by focused automation.
- **Scope:** Update asset-generation and recovery documentation; extend preview
  and runtime probes for indices, mirroring, unequal counts, and format fallback;
  add a cheap per-unit image/slot-count budget; record pilot and full-migration
  measurements; record the unit-texture memory budget that activates TEX-5;
  report tracked derived bytes and changed-artifact locality; complete
  path-selective CI coverage.
- **Phase:** 7 — operational closure
- **Depends on:** TEX-6
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-7, D-8, D-9,
  D-10, D-11, D-12
- **Acceptance signals:** The documented fresh-checkout workflow validates and
  previews every unit, relevant changes select the expected checks, and stale or
  behaviorally invalid atlas changes fail automatically.
- **Out of scope:** Broad full-probe or full-CI expansion beyond path-relevant
  gates.
- **Open questions:** None

## Verification strategy

- Every unit animation is loaded from one image per animation rather than one
  image per logical frame.
- The current 3,925-frame corpus renders with the same animation choices, timing,
  dimensions, direction, looping, and horizontal mirroring as before migration.
- Animations with unequal per-direction counts never expose padding as frames and
  never index outside their atlas.
- Five-direction mirrorable animations and all-eight-direction asymmetric
  animations are both covered by automated tests and the production preview.
- Strict validation has zero unexplained warnings and is enforced selectively in
  CI with its own self-tests.
- The compiler detects stale artifacts and produces deterministic results under
  the recorded toolchain.
- A fresh checkout can run the game and unit preview without an undocumented
  prior generation step.
- The acolyte pilot reports source and derived bytes; tracked PNG atlases remain
  within D-12's two-times-source budget; and changing one animation does not
  rewrite unrelated atlas artifacts.
- Runtime image count, bindless registrations, uploaded pixel bytes, and unit-load
  time are measured before and after the acolyte pilot and full migration.
- PNG atlases remain the complete supported path while TEX-5 is deferred. When
  TEX-5 is resumed, runtime transcode format is selected from actual device
  support, PNG fallback is tested, mixed PNG/KTX2 atlas residency works in one
  session, and one logical atlas does not retain duplicate production images.
- The legacy per-frame unit-animation runtime path is removed only after every
  unit and preview path has migrated successfully.

## Measured results

Recorded by TEX-7 (#1262) so the arc's evidence outlives the pull requests
that produced it. Everything below is reproducible from the commands given.

### Provenance

- **Roster:** the 7 tracked unit trees, 116 animations, 4,620 source animation
  frames — the corpus `python3 tools/pack_atlas.py --validate-only --strict`
  reports, which is the authority. Non-animation textures (portraits, direct
  `sprite`s, `directional_sprites`, `unknown_unit/rotations/`) are outside
  this inventory and outside every number here.
- **Revisions:** *after* = this arc's completed state (`aabf94ef`, #1261, plus
  TEX-7's own non-asset changes); *before* = `b2bc401d`, the commit
  immediately preceding #1260's first atlas commit, where every unit animation
  still loaded one image per frame.
- **Platform (load time only):** macOS 25.6.0 on aarch64, GHC 9.12.2, cabal
  production profile (`-O2 -optc-O3`; *not* `-f dev`), warm `dist-newstyle`,
  warm page cache.
- **Source evidence:** #1260 (`0758edf8`, `9d8e91bc`, `af3ad5e9`) for the
  acolyte pilot; #1261 (`aabf94ef`) for the full migration.

### Images, bindless registrations, and uploaded bytes

One image per animation means one texture handle and one bindless
registration per animation, so the first two columns are the same number
measured two ways. Uploaded bytes are decoded RGBA8.

| Unit | Images before | Images after | Uploaded before | Uploaded after |
|---|---:|---:|---:|---:|
| acolyte | 2,250 | 54 | 19.78 MiB | 20.20 MiB |
| bear_brown | 1,380 | 32 | 44.56 MiB | 44.56 MiB |
| red_squirrel | 270 | 9 | 1.33 MiB | 1.33 MiB |
| technomule | 145 | 4 | 4.68 MiB | 4.68 MiB |
| tiller | 30 | 2 | 0.97 MiB | 0.97 MiB |
| unknown_unit | 80 | 2 | 2.58 MiB | 2.58 MiB |
| white_tailed_deer | 465 | 13 | 27.27 MiB | 27.27 MiB |
| **total** | **4,620** | **116** | **101.17 MiB** | **101.60 MiB** |

**Images and bindless registrations fall 97.5%** (4,620 → 116). **Uploaded
bytes are essentially unchanged** — up 0.42%, entirely D-5's transparent row
padding, and confined to `acolyte`, the one unit with unequal per-direction
counts. That is the expected result and worth stating plainly: atlasing moves
the same pixels into far fewer images. It removes per-image allocation,
descriptor and file-load overhead; it does **not** compress. Reducing resident
*bytes* is deferred TEX-5's job, which is why the budget that activates it is
about resident memory rather than image count.

Runtime confirmation, from a real engine rather than from the artifacts:
`python3 tools/combat_anim_probe.py --roster-only` resolves every animation's
atlas texture name and asserts every per-frame name is *unregistered* — 7
units, 116 animations, 4,620 frames, zero frames registered individually.

### Load time

Metric: wall clock from process start until `engine.getTextureHandle` resolves
**every** animation texture of the four units both revisions share
(`acolyte`, `bear_brown`, `red_squirrel`, `technomule` — 99 atlases after,
4,045 per-frame images before). Same art, same declarations, two storage
representations. Measured under `--offscreen --size 640x480` so a GPU is
present and both paths genuinely decode and upload; polled at 50 ms, each poll
verifying the complete name set.

| | before (per-frame) | after (atlas) |
|---|---:|---:|
| min of 4 runs | 6.56 s | 4.72 s |
| median | 6.60 s | 4.74 s |

**1.86 s faster (28%)**, with run-to-run spread under 50 ms on both sides.
The figure covers whole-process startup — Vulkan init and the entire startup
loader, not units alone — so the unit-specific improvement is larger than 28%
of the work it replaced.

Decomposition, and the cost the atlas path *adds*: timing
`engine.loadUnitYaml` alone under `--headless` (no device) gives 1.076 s after
versus 0.037 s before for the same four units. That is not a regression being
hidden — the two numbers measure different work. Headless, the per-frame path
only *enqueued* 4,045 upload messages and its decoding never happened, while
the atlas path performs #1259's pass-3 verification up front: it decodes every
source frame and compares it against its atlas cell. That verification is
one-time, at definition load, on the Lua thread, and off the frame path; the
offscreen numbers above are what the two paths actually cost end to end.

### Tracked artifacts and D-12's guardrail

| | source frames | derived atlases | ratio | indices |
|---|---|---|---|---|
| acolyte (TEX-4) | 1.58 MiB (2,250) | 0.93 MiB (54) | **0.59x** | 59.4 KiB |
| the six TEX-6 trees | 5.35 MiB (2,370) | 3.94 MiB (62) | **0.74x** | 67.0 KiB |
| all seven | 6.93 MiB (4,620) | 4.88 MiB (116) | **0.70x** | 126.5 KiB |

Derived artifacts are *smaller* than their sources at every scope, well inside
D-12's 2x ceiling, so its stop-and-choose-a-distribution-strategy clause (Git
LFS, release artifacts, a build cache) is not reached. Indices are reported
separately and are not in the ratio.

### Changed-artifact locality

D-12 also requires that editing one animation not rewrite unrelated artifacts.
Measured by zeroing every tracked artifact's mtime, flipping **one texel of
one frame** of `acolyte/idle/south`, and running `--compile`:

- 123 tracked artifacts across all seven units;
- **2 rewritten** — `acolyte/atlas/idle.png` and `acolyte/atlas/index.json`;
- expected change set: that animation's atlas plus its own unit's generated
  index. Observed set is exactly that, and nothing else was opened for writing.

`tools/test_pack_atlas.py`'s `an isolated edit rewrites only that atlas and
its unit index` scenario keeps this property under test on fixtures.

### The unit-texture memory budget (TEX-5's trigger)

Recorded in `tools/unit_texture_budget.json`, which is the single
machine-readable source `pack_atlas.py --validate-only --strict` reads.

**Two quantities, and neither is a measurement.** The *index-projected total*
is the sum of `atlas_width x atlas_height x 4` over every animation the stored
indices declare; the *roster-projected total* is that figure times the recorded
growth factor, and it is the only one the threshold compares. Both are computed
from the compiled artifacts — `pack_atlas.py` reads no GPU and no running
engine — so the vocabulary below stays consistent throughout.

- **Index-projected total:** 106,531,968 bytes (101.60 MiB) of decoded RGBA8
  unit animation atlas, from the compiled indices' declared dimensions,
  aggregated over the whole tracked roster.
  `scripts/startup_loader.lua` feeds every `data/units/*.yaml` to the loader at
  boot, so that whole total is resident in every session regardless of what
  spawns — the aggregation scope is the roster, not a spawned subset.
- **Projection input:** a 2.0x roster-growth factor, applied to the
  index-projected total rather than to a per-unit estimate, because per-unit
  cost varies by an order of magnitude with canvas size (48x48 `acolyte` at
  0.37 MiB/animation versus 92x92 `bear_brown` at 1.39 MiB/animation) and it is
  the mix that scales.
- **Threshold:** 402,653,184 bytes (384 MiB). The roster-projected 203.19 MiB
  is 53% of it, leaving ~181 MiB of headroom — roughly four more
  `bear_brown`-scale trees. Chosen so the trigger fires on a real content-plan
  expansion rather than on the next single large quadruped.
- **Comparison rule:** `index-projected total x roster_growth_factor >
  threshold` activates TEX-5. Strict `>`, so sitting exactly at the threshold
  does not fire. The breach is a WARNING, which makes a plain `--validate-only`
  report it and `--strict` — what CI and `make ci` run — fail on it, so the
  resume-or-raise decision cannot pass unnoticed.
- **Confirmation:** proposed by the solver from the projections above and
  confirmed by the project owner on 2026-08-16, per this slice's 2026-08-11
  sign-off. Raising it is the owner's call, not a maintenance edit.

This is a *resident memory* budget and is deliberately separate from D-12's
tracked-artifact-bytes guardrail above; a future KTX2 artifact set is measured
against D-12 separately and must not silently expand either one.

The generated-atlas-entry half of the same document is a hard error rather than
a warning: exactly one entry besides `index.json` in a unit's `atlas/`
directory per compiled animation, the bound derived from each unit's own index
so it keeps holding as the roster grows. A change that put one generated file
per *frame* back where one per animation belongs fails immediately, naming the
unit, the expected and actual counts, and the offending entries.

**Neither half observes the runtime.** `pack_atlas.py` reads the compiled tree
— the stored indices and the entries beside them — and has no input from the
loader, the texture request queue, the bindless table or a running engine. Both
figures above are projections computed from index metadata, not residency
measured on a GPU. A green `--validate-only --strict` run therefore says the
generated artifacts are well-formed and their projected decoded size is under
threshold, and says nothing about how many uploads, handles or bindless slots
the runtime actually takes.

That bound is owned by the Hspec group `Unit.Atlas.Load — the real unit
registration boundary` (`test-headless/Test/Headless/Unit/Atlas/Loader.hs`),
which runs in the always-blocking headless suite:

```bash
cabal test synarchy-test-headless \
  --test-options='--match "the real unit registration boundary"'
```

It drives the production registration path and inspects the `LuaToEngineMsg`
values it queues, asserting one `LuaLoadAtlasTextureRequest` and one distinct
logical texture handle per animation, each published into the definition's
animation storage, no per-frame ordinary requests, and — for every shipped
unit through the production resolver — as many atlas requests as animations.
Its authority is that request-and-handle boundary; the completed Vulkan upload
and the bindless-table publication happen downstream of the queue it reads
(`src/Engine/Scripting/Lua/Message.hs`). A loader regression that queued two
requests per animation while leaving `atlas/` untouched fails there, and the
Python budget reports nothing (#2217).

## Risks and mitigations

- Risk: duplicated human-edited manifests drift.
  - Mitigation: D-11 keeps unit YAML authoritative and generates the runtime
    index with a source digest.
- Risk: an atlas row assumes equal frame counts and plays transparent padding.
  - Mitigation: index real counts per direction and cover the four current
    unequal-count animations in compiler and runtime tests.
- Risk: atlas dimensions accidentally become sprite dimensions or hit bounds.
  - Mitigation: make cell size explicit in the runtime frame result and test
    render/hit-test geometry separately from image extent.
- Risk: mass migration hides behavioral regressions or exhausts review scope.
  - Mitigation: keep a temporary explicit legacy storage mode and prove acolyte
    end to end before migrating the remaining units.
- Risk: generated files are stale or depend on a contributor's local tool version.
  - Mitigation: pin tools, record digests/tool versions, and validate deterministic
    output or freshness in path-selective CI. D-12 additionally caps initial
    tracked artifact size and unrelated-file churn.
- Risk: KTX2 integration hard-codes a compression format unavailable on a target
  GPU or adds disproportionate native packaging complexity.
  - Mitigation: D-10 defers the work until the projected-memory budget
    activates it, selects transcode targets from device features, retains PNG
    fallback, and permits mixed encodings without retaining duplicate copies
    of one atlas.
- Risk: unowned asset files are silenced without understanding whether they are
  unused, unreferenced future art, or missing declarations.
  - Mitigation: TEX-1 (#1257) required an explicit path-level classification and
    owner confirmation before any deletion. All 695 then-unowned paths were
    RETAINED and declared, nothing was deleted, and the validator now has no
    exemption mechanism at all — so a future unowned file fails the gate rather
    than accumulating as a warning.

## Rollout and rollback

- Land validation first so later artifact and manifest mistakes fail early.
- Add compiler outputs without changing runtime behavior.
- Add the atlas runtime behind an explicit per-animation storage mode.
- Migrate acolyte and collect parity, performance, artifact-size, and churn
  evidence before expanding migration.
- Migrate the remaining units after the PNG pilot passes; do not wait for the
  deferred KTX2 capability.
- Retire legacy per-frame loading last.
- Resume TEX-5 after the pilot baseline exists and projected unit content would
  exceed the documented unit-texture memory budget. Its format-neutral loader
  work must preserve existing PNG assets as the fallback and rollback path.
- Before retirement, rollback is per animation or per unit: select its legacy
  frame-list storage while preserving generated artifacts for diagnosis.
- After retirement, rollback is a normal code/data revert restoring both the
  compatible loader and manifest form; compiled indices are versioned so an
  incompatible runtime rejects them rather than misrendering silently.

## Issue-processing handoff

- Proposed epic title: Compile unit animations into validated runtime atlases
- Proposed epic labels: none
- Process the epic first, then one dependency-ready slice per invocation.
- TEX-1 is the first dependency-ready child.
- TEX-2 has no remaining design-question gate.
- TEX-5 is deliberately deferred under D-10 and does not block TEX-6 or epic
  completion; resume it only when its recorded pilot-and-memory-budget
  precondition is met.
- The design is ready for one-at-a-time processing under the ledger above.

## Source notes

The remainder of this document preserves the 2026-05-24 plan as source material.
It is not the current design contract where it conflicts with the decisions,
proposals, open questions, or delivery slices above.

---

# Legacy texture infrastructure plan

**Legacy status:** Pre-implementation, written 2026-05-24.

## Goals

Lay foundations for a unit-animation pipeline that scales to:
- Multiple factions × multiple unit types × multiple weapon classes
- 8 directions × 4–16 frames per animation
- AI-generated frame sequences as the authoring input
- Editing-friendly PNG sources alongside compressed shipping atlases
- Clear errors on missing/extra assets

Set the pattern **before the second unit lands** so the first unit migration validates the design and every subsequent unit drops into a stable groove.

## Non-goals

- Per-frame anchor points / compositional equipment rendering (deferred indefinitely).
- Streaming / async asset loading (not needed at current scope).
- Mip-mapping (pixel art uses NEAREST sampling; mips would soften edges).
- LOD systems (worry about it when the world is bigger).

## Background: how GPU texture compression works

A confusion to clear up first: GPU-compressed formats (BC7, BC1, ASTC) **stay compressed in VRAM**. The GPU's texture sampler hardware decompresses on-the-fly during shader sampling. Compression saves VRAM, disk, AND load time, with no shader code changes.

This is fundamentally different from PNG/JPEG, which must be fully decoded to raw RGBA on the CPU before upload to VRAM.

**Format choice for this project:** **BC7** in a **KTX2** container.

- BC7 is high-quality, handles sharp pixel-art edges well, supports alpha. ~4× compression vs raw RGBA.
- KTX2 is the modern Khronos container — Vulkan-native, well-tooled, supports BasisU transcoding if multi-platform becomes a concern later.
- macOS/MoltenVK supports BC7 through Metal translation.

Tooling: `toktx` from [Khronos KTX-Software](https://github.com/KhronosGroup/KTX-Software). Install via `brew install ktx`. Command pattern:

```bash
toktx --t2 --encode uastc --uastc_quality 4 \
      --uastc_rdo_l 1.0 --zcmp 18 \
      atlas_out.ktx2 atlas_in.png
```

(UASTC + KTX2 zlib supercompression — universal, transcodes to BC7 at load. Easier than committing to BC7 directly and dealing with platform variance.)

## Directory layout

```
assets/
  units/
    acolyte/
      animations.yaml        # manifest — single source of truth
      sources/               # editable PNG frames (gitignored .ktx2 alongside is fine)
        idle/
          n_0.png n_1.png n_2.png n_3.png
          ne_0.png ne_1.png ...
          ...
        walk/
          ...
        combat/
          unarmed/
            attack/
              ...
            hit_react/
              ...
          1h_melee/
            attack/
              ...
      atlases/               # build output (gitignored — generated)
        idle.png
        idle.ktx2
        walk.png
        walk.ktx2
        combat_unarmed_attack.png
        combat_unarmed_attack.ktx2
        combat_unarmed_hit_react.ktx2
        ...
    brown_bear/
      animations.yaml
      sources/
        ...
      atlases/
        ...
```

**Rationale:**

- `sources/` holds individual frame PNGs — what the artist (or AI tool) produces. Editable, hot-reloadable during dev.
- `atlases/` holds the packed-and-compressed output — what the engine actually loads in release. Both PNG (debug) and KTX2 (ship) atlases live here.
- One subdir per animation, one folder deep for combat / class. Easy to navigate, easy to script.
- `animations.yaml` at the unit root is the manifest — the engine reads this; never does filesystem discovery in production. Discovery is for the validation tool only.

**File naming:** `<dir>_<frame>.png` where `<dir>` is one of `n`, `ne`, `e`, `se`, `s`, `sw`, `w`, `nw` and `<frame>` is 0-indexed. If `flip` is enabled in the manifest, only `n`, `ne`, `e`, `se`, `s` need to exist; `nw`/`w`/`sw` are mirrored at runtime.

## YAML manifest schema

```yaml
# assets/units/acolyte/animations.yaml
unit: acolyte

# Defaults applied to every animation unless overridden.
defaults:
  directions: 8        # 8 | 5 (with flip)
  fps: 12              # 12 fps = ~83ms per frame
  loop: true

animations:
  idle:
    frames: 4
    fps: 6             # slower than default
    flip: true         # 5 source dirs, 3 mirrored
  walk:
    frames: 8
    flip: true
  run:
    frames: 8
    flip: true
  pickup:
    frames: 4
    loop: false
  idle_fight:
    frames: 8
    flip: true
  hit_react:
    frames: 4
    flip: true
    loop: false
  death:
    frames: 8
    flip: true
    loop: false        # holds last frame

# Combat animations are grouped by weapon class. Each class adds the same
# set of attack-side animations on top of the base set above.
combat:
  unarmed:
    attack:
      frames: 6
      flip: false      # holds visible "right hand" punching — don't mirror
      loop: false
    # Future: attack_heavy, etc.
  1h_melee:
    attack:
      frames: 6
      flip: false      # weapon stays in right hand
      loop: false
```

**Key decisions baked into the schema:**

1. **`flip: true|false` per animation.** This is the user's concern — a character holding a weapon in their right hand cannot be naively mirrored. The manifest declares which animations are bilaterally symmetric (idle, walk, hit_react, death — usually most non-combat) vs. asymmetric (anims that involve a held prop). The atlas-packer respects this: `flip: true` packs 5 directions; `flip: false` packs 8.

2. **`loop: true|false`** controls whether the animation cycles or holds its last frame. Critical for `death`, `pickup`, `attack` (one-shot) vs. `idle`, `walk` (continuous).

3. **`fps` per animation.** Overridable so `idle` can breathe slowly while `attack` snaps.

4. **`combat:` groups class-specific anims** without mixing them into the base set. Adding a new weapon class = new top-level key under `combat:`.

## Atlas-packer tool

A Python script at `tools/pack_atlas.py` that takes a unit directory and produces all atlases listed in `animations.yaml`.

**Behaviour:**

1. Reads `animations.yaml`.
2. For each animation, looks at `sources/<animation_path>/`.
3. Validates that exactly the expected frame files exist (5 dirs × N frames if `flip: true`, 8 dirs × N frames otherwise). Loud error if mismatch.
4. Packs into a single atlas image: rows = directions, columns = frames. Atlas is `(48 × frames) × (48 × dirs)` pixels (e.g., 384×240 for 8 frames × 5 dirs).
5. Writes `atlases/<animation_name>.png` (debug) and `atlases/<animation_name>.ktx2` (ship) — both, always. KTX2 via `toktx` shellout.
6. Writes a `.json` sidecar next to each atlas with: `{frames, dirs, fps, loop, flip}` — so the runtime doesn't need to re-parse YAML to know packing geometry. (Optional refinement: bake this into KTX2 metadata via `--genmipmap` flags if `toktx` supports custom KV pairs.)

**Invocation:**

```bash
# Pack one unit
python3 tools/pack_atlas.py assets/units/acolyte

# Pack everything
python3 tools/pack_atlas.py --all

# Validate only (no output) — for CI
python3 tools/pack_atlas.py --validate-only --all
```

**Why Python:** ergonomic image manipulation via PIL, simple shell-out to `toktx`, no engine recompile needed when iterating on the tool. Existing project tooling at `tools/` follows this pattern.

## Validation tool

Same script with `--validate-only`. Checks:

1. Every `animations.yaml` declared animation has the matching source files (exact count, correct names).
2. No orphan source files in `sources/` that aren't declared in the YAML.
3. All frames within one animation are the same dimensions (catches AI-tool size drift).
4. Atlas KTX2 files are present and newer than their source frames (skip-rebuild logic).

Add to CI as a separate step from `cabal build`. Fast (filesystem-only, no image decode required for the count check).

## Runtime loader integration

**Phase 1 — minimum changes to existing engine.**

1. New module `Engine.Graphics.AnimationManifest` that reads `animations.yaml` and produces an in-memory map: `(UnitType, AnimationName, WeaponClass?) → AnimationMeta`.
2. `AnimationMeta` carries `{ atlasHandle, frames, dirs, fps, loop, flip }`.
3. Existing texture-loading path (presumably already loads PNGs into bindless slots) gains a code path for KTX2:
   - Prefer `.ktx2` if present.
   - Fall back to `.png` if `.ktx2` is missing (dev convenience).
4. Existing sprite-draw path picks the row in the atlas by direction: for `flip: true` animations, if the requested direction is one of `nw`/`w`/`sw`, look up the mirrored direction and flip UVs at draw time.

**UV flip implementation:** add a `flipX :: Bool` parameter (or a sign bit on the existing tex-coord) to the per-quad data. Single shader change — flip U coordinate in the vertex shader if the flag is set. Free at runtime.

## Migration order

This is the work plan. Each step is independently shippable; don't bundle.

1. **Add the `flipX` UV flip path to the sprite renderer.** No assets touched yet — just the engine capability. Verify with a manual flip on an existing sprite. (~30 min if the renderer is straightforward.)

2. **Pick KTX2 format + install `toktx`.** Document the install in the project README. Pin the tool version.

3. **Write `tools/pack_atlas.py` (validate-only mode first).** Walks the directory tree, reports what would be packed. Run against an existing unit's source frames as a dry-run; iterate on the script until it reports cleanly.

4. **Extend `pack_atlas.py` with actual packing (PNG output only, no KTX2 yet).** Manually inspect the packed atlas PNGs to verify dir/frame layout.

5. **Add KTX2 output to `pack_atlas.py`.** Shell out to `toktx`. Verify a KTX2 atlas loads in the engine.

6. **Migrate the acolyte's existing animations to the new layout.** Rename source files into `assets/units/acolyte/sources/<anim>/<dir>_<frame>.png`. Author `animations.yaml`. Run packer. Wire the engine's animation lookup to read from the manifest.

7. **Decide PNG-vs-KTX2 dev workflow.** Recommended: KTX2 is the default load path; PNG atlas falls back if KTX2 missing. Devs can `rm assets/units/.../atlases/*.ktx2` to force PNG hot-reload during iteration.

8. **Add validation to CI.** A `cabal test` extension or a separate `make validate-assets` target. Block PRs that miss assets.

9. **Author the brown bear's `animations.yaml` + sources.** First test of the system with a NEW unit. Should be drop-in if steps 1-7 went well.

10. **Author the unarmed combat animations for both units.** This is where the combat work formally begins; the infrastructure is now ready to receive it.

## What this plan does NOT cover

- Combat sim itself (separate plan).
- Particle/effect overlays for weapon-class differentiation (separate work; comes after class anims exist).
- Equipment-as-overlay rendering (deferred indefinitely per discussion).
- Per-frame anchor points (deferred indefinitely).
- Material-set / faction-palette skinning (deferred; current plan uses baked colors per faction unit).

## Open questions

- **Hot reload:** is texture hot-reload during dev a requirement? If yes, the PNG-fallback path needs filesystem-watching; if no, restart-to-reload is fine. Default to "no" for simplicity unless it slows iteration meaningfully.
- **CI duration:** `toktx` on a single 384×384 atlas is fast (<1s). On 100+ atlases it's still seconds, not minutes. But validate-only mode is essentially free and should run on every PR.
- **Per-faction palette swaps:** if the same acolyte sprite needs to render in different faction colors, that's a shader uniform per draw rather than a per-faction asset duplication. Worth deciding before authoring the second faction.
