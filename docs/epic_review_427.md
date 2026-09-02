# Epic Review Findings: Epic #427 — `--preview <category>/<item>` — minimalist in-engine texture & animation viewer

This report records the completed-arc review of epic #427 at
`master@fef7d0ddb00c`, against its reconciled five-child scope. The epic has no
native GitHub sub-issues; its body declares children #428, #632, #886, #887,
and #888. The current preview runtime composes coherently across those slices
and its focused gates pass; one new steering-document mistake remains.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. The preview contract gives buildings and units the same authority split — [#2244]

## 1. Preview authority documentation

### [#2244] ER-1. The preview contract gives buildings and units the same authority split

> **Captured note:** Correct the buildings-viewer section of
> `docs/engine_contracts.md` to say that buildings and units now use opposite
> authority splits: buildings remain filesystem-authoritative under #888,
> while #1261 made unit YAML and its compiled atlas index authoritative.

**Verification:** Verified. The buildings-viewer contract accurately says a
building folder determines which entries exist and its YAML only augments
matched animations, but introduces that behavior as “the same split the units
viewer uses.” That comparison is no longer true. The adjacent units contract,
the current unit preview module, and `CLAUDE.md` all record #1261's replacement
of #887's filesystem-first discovery: unit YAML plus the compiled index decide
which animations exist, and missing or rejected compiled artifacts fail before
boot. The building implementation still follows the filesystem-first #888
contract. The runtime is consistent; the durable contract's cross-reference
is not.

**Evidence:**

- `docs/engine_contracts.md:692` — the buildings section calls its
  filesystem-authoritative behavior “the same split the units viewer uses.”
- `docs/engine_contracts.md:684` — the adjacent units section instead says a
  missing, animation-less, or uncompiled YAML/index is a pre-boot rejection.
- `src/Engine/Preview/Unit.hs:10` — the live unit module names unit YAML and
  the compiled index as its authority; line 23 explicitly says that this
  replaced #887's filesystem-first discovery.
- `src/Engine/Preview/Building.hs:11` — the live building module retains
  #888's filesystem-authoritative, YAML-augmenting split.
- `CLAUDE.md:683` — the current project handoff describes the compiled unit
  path, then line 688 calls buildings “the opposite authority split.”

**Handoff context:**

- **Current behavior:** Unit previews enumerate and sample the production
  YAML/index/atlas representation; building previews discover entries and
  frame order from the filesystem and use YAML only for matching metadata and
  default selection. Both behaviors are covered and passing.
- **Expected behavior:** Describe these as opposite authority splits in
  `docs/engine_contracts.md`, agreeing with its units section, `CLAUDE.md`, and
  the two implementations.
- **Scope and constraints:** Documentation-only correction. Do not change unit
  or building discovery, atlas loading, animation playback, pre-boot rejection,
  or default selection. Preserve #887/#888's historical provenance and do not
  edit archived `docs/history/` snapshots.
- **Verification target:** The buildings-viewer contract no longer claims the
  unit viewer is filesystem-authoritative, and the live contract consistently
  distinguishes #1261's unit YAML/index authority from #888's building
  filesystem authority.
- **Deduplication:** Exact and concept searches across all GitHub issue states
  found no issue for this contradictory authority statement. The findings
  corpus contains EXPL-9 for a different stale `buildPreviewUnit` pipeline
  summary, EXPL-8 for building special-file proof, and the now-fixed #1417
  building animation-directory defect; none owns this documentation mismatch.
- **Remaining uncertainty:** None.
