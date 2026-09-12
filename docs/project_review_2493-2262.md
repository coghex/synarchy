# Project Review Findings: PRs #2493–#2262

Reviewed `coghex/synarchy` PRs #2493, #2296, #2295, #2289, #2287,
#2281, #2279, #2254, #2271, #2270, #2268, and #2262 against their linked
issues, commit messages, landed first-parent patches, and current descendants.
Review began at `cfd30002dde1901f91ad7db251a07f1e01889330`; the subsequent
#2532 landing is reserved for the next batch. The older landing interval
contains PR merges only. Direct publication `53e448ac9` was also reviewed
(build-wait policy, existing audit documentation, and previously captured
findings/cursor publication). The excluded #2377 concern remains excluded.
No implementation, tracker, or publication changes were made by this review.

Focused config tests passed (28 helper, 5 Lua-verb, 8 writer examples), as did
the config-write audit and its self-test. Building ghost (30), footprint (7),
scene telemetry (40), camera-facing visuals (34), reconciliation (15), and
Lua persistence components (101) passed, along with the footprint probe's
offline self-test. Startup coverage passed in the preceding batch against
the same surviving implementation. Fluid admission passed 12 examples;
save components/migrations passed 176/51. Split Python audit families and
their composition guards were checked with focused tests and mechanical
source comparisons. The compatibility gate's extra ordinary Cabal startups
and overstated docstrings are already covered by #2273, not a new finding.
No full CI, new worldgen baseline, or fresh GPU/owner-signoff session was run.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. Escape rebased screenshot destinations as Markdown links
- [ ] PRR-2. Preserve the wrapped building position through the staking handoff

## 1. Playtest evidence links

### PRR-1. Escape rebased screenshot destinations as Markdown links

> **Captured note:** PR #2289 rebases screenshot paths for reports written
> outside their trace directory, but emits raw filesystem paths as bare
> Markdown destinations. A valid directory name containing spaces breaks
> the report's evidence images.

**Verification:** Ran the real canned trace and fake-critic pipeline with a
trace directory named `trace with spaces` and a separate output directory.
All 12 emitted targets existed on disk, but the report contained links such
as `![turn screenshot](../trace with spaces/frames/turn_0001.png)`. A normal
space-free trace provided the control. Bare destinations cannot contain
spaces under the [CommonMark link-destination rules](https://spec.commonmark.org/0.31.2/#link-destination).
Both existing playtest self-tests pass despite the defect. This is actual
generated output checked against the syntax contract, not a claimed visual
rendering run.

**Evidence:**

- `tools/playtest/critic_evidence.py:351` — `screenshot_target` defines the rebase boundary.
- `tools/playtest/critic_evidence.py:368` — returns a raw `os.path.relpath` result.
- `tools/playtest/critic_evidence.py:392` — interpolates that result into a bare image destination.
- `tools/playtest/critic_selftest.py:704` — its image-reference regex accepts spaces without checking Markdown syntax; filesystem existence alone cannot prove an image link works.

**Handoff context:**

- **Current behavior:** `--out` can produce existing filesystem targets that are not valid Markdown image destinations, defeating the evidence-link correction for ordinary directory names.
- **Expected behavior:** The report must contain valid image links whose decoded targets resolve to the original trace-owned screenshots, including when the rebase contains spaces or Markdown-significant punctuation.
- **Scope and constraints:** Keep screenshots owned by the trace and JSON references trace-relative. Preserve existing simple-path/default-location output where valid; do not copy frames or rewrite trace metadata as a workaround.
- **Verification target:** Generate reports from space-containing and punctuation-containing trace/output directories, parse their Markdown, and verify the decoded image destinations exist. Retain normal-path and unchanged-trace controls.
- **Deduplication:** Open/closed searches for critic screenshot spaces and Markdown links found the original #2220 work, not this remaining failure. No equivalent local finding was found.
- **Remaining uncertainty:** No Markdown renderer was installed for a pixel-level check; the generated invalid syntax and complete emission trace are established.

## 2. Building ghost continuity at the world seam

### PRR-2. Preserve the wrapped building position through the staking handoff

> **Captured note:** PR #2254 promises identical designation and staked
> building geometry, but only the designation applies the nearest world-wrap
> offset. Staking suppresses the correctly translated designation and leaves
> the instance drawn at its distant canonical position.

**Verification:** Traced the current cursor, placed-building, and shader
paths. The designation builds the same base rectangle as the instance, then
translates it by a possibly nonzero `isChunkVisibleWrapped` result. The
placed pass emits its base rectangle without that translation; the vertex
shader applies only the common model/view/projection and pixel snapping,
not a building-specific wrap correction. Once the matching instance exists,
the cursor pass suppresses the translated ghost. Thus a nonidentity alias
necessarily changes the displayed position at handoff, and can move the
building entirely out of view. This is a complete static trace, not a new
GPU reproduction. The 30-example ghost suite passes: its seam example stops
before staking, while its handoff comparisons use the unwrapped fixture.

**Evidence:**

- `src/World/Render/CursorQuads.hs:430` — matching page/definition/anchor instance suppresses the designation.
- `src/World/Render/CursorQuads.hs:433` — resolves the designation's visible alias.
- `src/World/Render/CursorQuads.hs:451` — applies `translateQuad wrapOff` to the emitted designation.
- `src/Building/Render.hs:123` — calls `buildingToQuad` and stamps its solar page without applying a world-wrap displacement.
- `src/Building/Render.hs:171` — obtains the placed rectangle from `placedBuildingQuad`.
- `src/Building/Visual.hs:234` — derives that rectangle from stored anchor coordinates; no camera-position/world-size input can select an alias here.
- `src/Engine/Graphics/Vulkan/ShaderCode.hs:102` — transforms the supplied vertex positions without an entity wrap step.
- `test-headless/Test/Headless/Building/Ghost.hs:582` — proves nonidentity translation for the designation alone, not for the ensuing instance.
- `src/Building/HitTest.hs:98` — hit testing consumes the same unwrapped placed rectangle and must remain aligned with any corrected rendering.

**Handoff context:**

- **Current behavior:** The plan appears over wrapped terrain, then yields to an instance emitted at a different screen-space alias. The instance renderer's omission predates this PR, but violates this PR's explicit no-jump contract.
- **Expected behavior:** Preview, committed designation, and staked/placed building must refer to the same visible physical site through the terrain's authoritative nearest-alias decision, without duplicate quads or a handoff jump.
- **Scope and constraints:** Preserve canonical stored coordinates, page qualification, frame-wide building-manager snapshot, lifecycle opacity, authored dimensions/anchor, z-band, and grid-derived sort/lighting data. Rendering and building hit testing must agree; do not simply remove the designation's correct wrap translation. Assess destruction geometry if a shared placed-presentation boundary changes.
- **Verification target:** Extend the existing nonidentity-seam fixture through staking and compare the final emitted designation/instance positions at all four facings, with an away-from-seam control and corresponding hit-test checks. Capture a gameplay-scale handoff after the focused geometry regression passes.
- **Deduplication:** Open/closed searches for building seam/wrap issues and local reports found no equivalent correction. #1706 fixes structure wrapping and explicitly excludes building rendering; #1845 is the originating no-jump contract, not a separate fix for this survivor.
- **Remaining uncertainty:** Fresh rendered evidence and the complete effect on preview/destruction presentation were not measured. The designation-to-instance positional mismatch itself is established by the current production paths.
