# Project Review Findings: PRs #2542–#2144

Reviewed `coghex/synarchy` PRs #2542, #2540, #2191, #2153, #2178,
#2158, #2154, #2125, #2121, #2152, #2146, and #2144 against their
specifications, commit messages, landed patches, and current consumers.
Checked the first-parent landing intervals; there were no additional direct
commits to review. Large truncated GitHub patch output was recovered from
the first-parent landing diff. Verification used the package rebuilt at
`92948204f`; the finding paths are unchanged at `12d747399`.
The previously excluded #2377 concern remains excluded. No implementation
or tracker was changed.

Focused headless checks passed: 4 world-size normalization, 7 Lua wrapping,
15 width-truncation, 4 arena, 4 zoom-artifact pure and 1 full-world pixel
comparison, 51 save migrations, 34 building-facing, 104 Chop, 39 significant
contents, 12 compound-clearance, and 52 integrity-graph examples.
Python checks passed: atlas 133 tests; capability reader 237 groups / 538
assertions and writer 61 groups / 236 assertions; world audit 65 groups /
301 assertions; escort 93 assertions; location-content 185 assertions;
action-outcome and CI-selector self-tests. AST/body comparisons checked the
large tool extractions. The strict atlas validation still stops on the
pre-existing acolyte `.DS_Store`, which was preserved. The already-reported
promotion `parse_verbose` call mismatch is not a new finding. Later Chop
probe fixture/formatting repairs and shared eligibility corrections are
also not new entries. No full CI or fresh GPU scenario was run.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Fingerprint every accepted zoom-palette texture dependency
- [ ] PRR-2. Narrow the scene comparator's render-equivalence guarantee

## 1. Zoom artifact invalidation

### PRR-1. Fingerprint every accepted zoom-palette texture dependency

> **Captured note:** PR #2191 fingerprints four directory trees, but the
> palette loader accepts texture references outside them. Changing such a
> texture can change the palette without invalidating the cached pixels.

**Verification:** Ran the packaged Haskell APIs in an automatically cleaned
`withSystemTempDirectory` fixture. Created the four fingerprint roots and
`custom/`, with `data/materials/custom.yaml` defining material id 20 and
`tile`, `zoom`, and `bg` all referencing `custom/palette.png`. Copied the
tracked sandstone zoom PNG to that path, loaded the registry, and called
`buildZoomArtifactKey` and `buildColorPalette`. Replaced only the fixture
PNG by copying the tracked shale PNG and repeated both calls with the same
parameters and registry. The result, expressed as `(keySucceeded,
keysEqual, firstColor, secondColor)`, was:

```text
(True,True,(171,119,60,171),(129,114,104,171))
```

This proves an accepted resource dependency changes the sampled color
without changing the key. The current load-hit branch below establishes
that previously generated pixels are then reused. It was not a rendered
stale-map or full save/load reproduction.

**Evidence:**

- `src/World/ZoomMap/Artifact.hs:119` — the resource roots are fixed to material YAML, vegetation YAML, world zoom textures, and vegetation textures.
- `src/World/ZoomMap/Artifact.hs:137` — hashes files discovered only under those roots; the registry digest does not add sampled image bytes.
- `src/Engine/Asset/YamlMaterials.hs:80` — the material parser accepts authored texture path strings without imposing those directory roots.
- `src/World/ZoomMap/ColorPalette.hs:113` — samples the actual `mdZoom` path; line 128 similarly samples each authored vegetation variant path.
- `src/World/Load/Stage.hs:791` — builds the artifact key; lines 799–807 return cached entries and pixels on a hit, while the miss branch at line 813 uses the freshly built palette.

**Handoff context:**

- **Current behavior:** A valid authored texture outside the four directory trees can change while the artifact key remains identical, allowing a stale hit.
- **Expected behavior:** Every accepted input that can change generated zoom pixels must participate in invalidation, or an explicit, enforced resource-path contract must reject unsupported inputs before reuse.
- **Scope and constraints:** This is #2191's cache dependency boundary, not a request to expand save compatibility, change world generation, or assume arbitrary file paths are forbidden. Do not change tracked artwork to test it; use isolated fixtures and existing PNGs.
- **Verification target:** Exercise a real accepted out-of-root material texture, and the corresponding vegetation case, changing only the referenced bytes. Prove correct invalidation or explicit rejection and retain ordinary unchanged-input cache hits.
- **Deduplication:** Open/closed searches for zoom/cache/texture, zoom cache palette, and artifact fingerprint found antecedent/performance and other palette issues, but no owner for this missing accepted dependency. Existing project-review reports do not capture it.
- **Remaining uncertainty:** No shipped texture was found using an out-of-root path. The defect is in the accepted resource configuration space; its occurrence in stock content is not claimed. Fresh rendering and a complete load cycle were not executed.

## 2. Scene ordering contract

### PRR-2. Narrow the scene comparator's render-equivalence guarantee

> **Captured note:** PR #2121 documents `quadPainterOrder` as total on
> everything that can affect rendering, claiming residual ties render
> identically. That guarantee is false for the shared scene API: atlas UVs
> and other vertex payload can differ while its comparison key is equal.

**Verification:** Constructed two real `SortableQuad` values through the
packaged Haskell types with identical depth, rectangle, texture handle and
layer, but UV spans 0–0.25 and 0.5–0.75. `quadPainterOrder` compared equal;
reading the actual vertices' `tex` fields confirmed different UV inputs.
The current unit renderer below produces atlas-subrectangle UVs rather
than the comment's supposedly universal unit square. This proves a false
documentation guarantee, not an observed visual ordering failure.

**Evidence:**

- `src/Engine/Scene/Types/Batch.hs:141` — claims adding rectangle and texture makes the order total on all frame-affecting data.
- `src/Engine/Scene/Types/Batch.hs:145` — claims remaining fields follow from the key and UVs are fixed unit squares.
- `src/Engine/Scene/Types/Batch.hs:159` — the key actually contains only depth, two rectangle corners, and texture handle.
- `src/Unit/Render.hs:338` — selects an animation frame's UV subrectangle; lines 345–362 apply facing-dependent horizontal flip and pass the resulting UVs into the quad builder. The selected render flag at line 334 is another independent payload.
- `test-headless/Test/Headless/World/Chop/Selection.hs:706` — the render-equivalence assertion uses the constrained flora factory, not every scene-quad consumer.

**Handoff context:**

- **Current behavior:** Shared API documentation promotes a constrained flora assumption into a universal render-equivalence guarantee despite independently varying vertex payloads.
- **Expected behavior:** State the comparator's actual key and residual-tie limits; scope any render-equivalence guarantee to the factory and conditions that establish it.
- **Scope and constraints:** A documentation-contract correction associated with #1856/#2121. Do not infer that the scene sorting algorithm needs redesign, or that Chop's constrained flora tie handling is wrong, from this counterexample alone.
- **Verification target:** Check the corrected guarantee against current flora and unit quad factories, explicitly including equal-key/different-UV values. Keep the existing flora selection and marker ordering tests.
- **Deduplication:** Open/closed painter-order searches found the originating work and related historical depth issues, but no owner for this false general guarantee; local project-review reports do not capture it.
- **Remaining uncertainty:** No GPU flicker or incorrectly selected flora was reproduced. Only the documented global guarantee is established to be wrong.
