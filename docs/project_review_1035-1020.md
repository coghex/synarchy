# Project Review Findings: PRs #1035–#1020

These entries record focused evidence from the senior review of the next twelve merged PRs, #1035 through #1020 in merge order, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The startup-handshake race visible in PR #1026's shared loop remains real, but it is already PRR-1 in `docs/project_review_1210-1183.md` and open issue #1263, so this report does not duplicate it.

## Status

- [x] PRR-1. Autosave staging slots leak into the public save list — [#1413]
- [x] PRR-2. The reusable transfer-session entry point trusts an already-validated source — [#1415]
- [ ] PRR-3. Building preview accepts directories as animation frame files

## 1. Autosave presentation

### [#1413] PRR-1. Autosave staging slots leak into the public save list

> **Captured note:** Keep the autosave transaction's `autosave-incoming` and `autosave-retired` staging namespaces out of Continue and the save browser. PR #1029 deliberately made them internal rotation machinery, but the public listing path enumerates every valid slot directory without filtering them.

**Verification:** Verified statically — a successfully published incoming generation is a valid save directory, `engine.listSaves()` forwards it unchanged, and both public consumers use that list verbatim. A failed or interrupted rotation can leave the internal row visible indefinitely.

**Evidence:**

- `scripts/autosave.lua:176` — the scheduler names `autosave-incoming` as its private staging slot; `:192` publishes a complete autosave into it before rotation.
- `scripts/autosave.lua:216` — rotation does not begin until a later scheduler tick observes the save transaction's terminal success; `:234-237` deliberately leaves the staging generation in place when rotation fails.
- `src/World/Save/Serialize.hs:308` — `listSaves` enumerates every entry under `saves/`; `:316-325` attempts to list every slot directory without excluding the autosave staging names.
- `src/Engine/Scripting/Lua/API/Save.hs:114` — `engine.listSaves()` exposes the complete `listSaves` result and publishes each slot's directory name directly at `:117-121`.
- `scripts/main_menu.lua:61` — the main menu consumes that unfiltered list, sorts it newest-first, and assigns the first row to Continue at `:63-67`.
- `scripts/save_browser.lua:164` — the save browser likewise falls back directly to `engine.listSaves()` with no private-slot filter.
- `tools/autosave_probe.py:140` — the probe explicitly excludes the incoming staging slot from its numbered-family helper; `:636-642` obtains the public listing while a forced rotation failure has left that slot in place, but only compares numbered rows and disk presence. It never asserts that the staging name is absent from the public list.
- Focused `autosave engine guards (#913)` coverage passed during this review (3 examples), but that suite covers failure feedback and the player-intent lock rather than scheduler/listing composition. Tracker searches for the staging name, autosave staging, and save-browser exposure found no issue owning this gap.

**Handoff context:**

- **Current behavior:** Between publication and successful rotation, `autosave-incoming` can be the newest public save and therefore the Continue target. If rotation fails or a process stops in that window, the internal name remains a normal loadable browser row until a later rotation repairs it; an interrupted rotation can similarly expose `autosave-retired` after restart.
- **Expected behavior:** Continue and player-facing save lists expose only manual saves and the numbered `autosave-N` family, while rotation can still inspect and validate its internal staging generations for safe retry.
- **Scope and constraints:** Surfaced in PR #1029 / issue #913. Preserve publish-before-rotate crash safety, durable autosave classification, manual-name collision refusal, and resumable incoming/retired recovery; this is a public-list boundary defect, not a request to delete staging data.
- **Remaining uncertainty:** The listing path and persistent-failure case are settled statically. The ordinary successful window is normally brief, and this review did not drive a live menu transition into that interval.

## 2. Transfer session validity

### [#1415] PRR-2. The reusable transfer-session entry point trusts an already-validated source

> **Captured note:** Revalidate the source at `transfer_session.create`, not only while constructing today's context menu. PR #1030 made `create` the reusable boundary for future UI surfaces, but an existing source ID bypasses the player-commandable and self-transfer rules enforced by `resolveSource`.

**Verification:** Verified statically — the shared creation function checks only source existence, then validates only the destination endpoint. Its own comment says it revalidates both sides, while the engine contract has a live source-eligibility rule that the function never queries.

**Evidence:**

- `scripts/transfer_session.lua:147` — `resolveSource` defines a valid source as exactly one selected, player-commandable unit distinct from the destination; `:157-164` implements all three checks.
- `scripts/init_context_menu.lua:41` and `:168` — the two current menus call `resolveSource` before installing their callbacks, so today's gesture normally supplies a prevalidated ID.
- `scripts/transfer_session.lua:167` — the independently callable `create` boundary claims to revalidate both sides against fresh state, but `:179-183` only checks `unit.exists(sourceUid)` and `:185-196` queries eligibility only for the destination.
- `src/Unit/Transfer.hs:423` — the authoritative role-independent endpoint rule requires a unit to remain player-commandable; `:481-487` separately rejects self-transfer, an ineligible source, an ineligible destination, cross-page endpoints, and out-of-range endpoints.
- `src/Engine/Scripting/Lua/API/Units/Transfer.hs:744` — `unit.transferEndpointInfo` already exposes the same live endpoint eligibility for either role, independently of adjacency or item selection.
- `scripts/transfer_session.lua:230` — the session assumes same-page validity without checking it and records the destination's page as `sourceLocation.page`; a direct cross-page call therefore creates internally false source metadata rather than refusing the pair.
- `test-headless/Test/Headless/UI/TransferContextMenu.hs:203` — the requirement-8 test proves `create` is independently callable, but supplies one assumed-valid source and asserts only the resulting identities. No direct-entry test presents an existing hostile/uncommandable source, a self-transfer pair, or cross-page endpoints. The full focused describe passed during this review (22 examples), confirming the coverage gap.
- Tracker searches found future issue #1254 for endpoints becoming invalid after a Mode A session exists and #1239 for nearest-of-many menu selection, but neither owns invalid creation through the reusable boundary.

**Handoff context:**

- **Current behavior:** A later drag-and-drop or other direct caller can create a session for any existing unit, including one that is no longer player-commandable or is also the destination. A source that changes after menu construction is trusted for the same reason, and a cross-page pair is recorded with the destination page copied onto both locations.
- **Expected behavior:** Every caller gets the same fresh source-validity result from the reusable creation boundary; invalid source/destination pairs produce no active session regardless of whether a context menu pre-screened them.
- **Scope and constraints:** Surfaced in PR #1030 / issue #1014 and remains after PR #1085's endpoint generalization. Preserve transient session identity, player-visible failure reporting, no inventory mutation during creation, and the future nearest-of-many selection work; the concern is validation after a source has been selected, not which source the menu selects.
- **Remaining uncertainty:** No current production caller bypasses the two context menus. The defect is immediately reachable through the registered reusable module API and becomes player-facing when another promised surface reuses it.

## 3. Preview filesystem discovery

### PRR-3. Building preview accepts directories as animation frame files

> **Captured note:** Make building animation discovery require regular texture files. PR #1028's helper named `isPlainFile` only rejects symlinks, so child directories whose names end in `.png` satisfy the numbered-frame convention and are returned as frame paths.

**Verification:** Verified with a deterministic synthetic filesystem reproduction — an `anim/` directory containing two child directories named `frame_001.png/` and `frame_002.png/` was classified as one animated preview entry whose frame paths point to those directories.

**Evidence:**

- `src/Engine/Preview/Building.hs:256` — the outer walk correctly distinguishes directories from files before deciding whether to classify or descend.
- `src/Engine/Preview/Building.hs:275` — inside a candidate animation directory, discovery filters names by supported texture extension and `isPlainFile` without repeating that type check.
- `src/Engine/Preview/Building.hs:296` — despite its name, `isPlainFile` is only `not <$> pathIsSymbolicLink`; it never calls `doesFileExist`, which the module already imports.
- `src/Engine/Preview/Building.hs:281` — accepted names become frame paths without another validation, and `:292-293` promotes any all-numbered set to an animation.
- A review-time REPL reproduction against `discoverBuildingEntries Map.empty` returned `PreviewBuildingEntry {pbeLabel = "anim", pbeAnimated = True, ... pbeFrames = [".../anim/frame_001.png",".../anim/frame_002.png"]}` when both frame paths were directories.
- `test-headless/Test/Headless/Preview/Building.hs:230` — the sole synthetic discovery case covers real unpadded frame files and a mixed static directory, but not a non-file carrying a supported extension. The focused `Preview.Building` describe passed during this review (24 examples), including all shipped trees; no checked-in asset currently triggers the defect.
- Tracker searches for building-preview directory/file discovery and plain-file validation found only the closed implementation issue #888 and parent epic #427, not a live owner for this edge.

**Handoff context:**

- **Current behavior:** A malformed or externally supplied building asset tree can advertise a directory as a static texture or numbered animation frame. The pre-boot browse result is therefore successful even though the selected path is not a loadable image.
- **Expected behavior:** Only contained, non-symlinked regular texture files become static entries or animation frames; directories remain traversal/classification containers regardless of their suffix.
- **Scope and constraints:** Surfaced in PR #1028 / issue #888. Preserve filesystem-authoritative discovery, numeric frame ordering, YAML-less animation recognition, recursive static discovery, and the rule that symlinked content is skipped.
- **Remaining uncertainty:** Discovery misclassification is reproduced. This review did not boot the GPU preview to characterize the downstream texture-loader error, and the tracked asset tree contains no `.png` directories today.
