# Project Review Findings: PRs #1165–#1128

These entries record focused evidence from the senior review of merged PRs #1165 through #1128 for later one-at-a-time disposition.

`[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. Stale held-item references report the wrong refusal
- [ ] PRR-2. Cross-manager rollback silently assumes the source survives
- [ ] PRR-3. Component codecs permit malformed historical version tables
- [ ] PRR-4. Bound-form review left consonant-scoping documentation inverted
- [ ] PRR-5. Architecture guide still names the deleted World.ZoomMap facade

## 1. Transfer identity and atomicity

### PRR-1. Stale held-item references report the wrong refusal

> **Captured note:** Validate `defName` before classifying a held instance as non-transferable. A stale `(instanceId, defName)` pair that points at equipped gear currently returns `item_not_transferable`, although the same stale pair in loose inventory correctly returns `instance_missing`.

**Verification:** Verified — the loose-item branch checks both fields, but the held-item fallback uses `itemMatches`, whose positive-ID path deliberately ignores `defName`.

**Evidence:**

- `src/Item/Types.hs:315` — `itemMatches` compares only `iiInstanceId` whenever the requested ID is positive; the supplied definition name is ignored.
- `src/Unit/Transfer.hs:536` — a loose-list ID match is followed by an explicit `iiDefName` guard and a mismatch becomes `ReasonInstanceMissing`.
- `src/Unit/Transfer.hs:541` — the held-list fallback applies the ID-only predicate directly and classifies any match as `ReasonItemNotTransferable` without the equivalent definition-name guard.
- `test-headless/Test/Headless/Unit/Transfer.hs:395` — the stale-definition regression covers only a loose item, while the held-item cases at `:401` and `:407` always supply the correct definition name.

**Handoff context:**

- **Current behavior:** Requesting an equipped instance ID with a stale or incorrect definition name reports that the requested item exists but is non-transferable.
- **Expected behavior:** The `(instanceId, defName)` identity contract is validated consistently before location-specific refusal reasons are chosen; a mismatched pair is missing/stale, not merely worn.
- **Scope and constraints:** Surfaced while reviewing PR #1129 / issue #1085. Preserve the useful distinction for a correctly named equipped or accessory item and the positive-ID exact-instance semantics shared with older inventory actions.
- **Remaining uncertainty:** None at capture time; the incorrect branch is deterministic, though no current production caller is known to construct stale names intentionally.

### PRR-2. Cross-manager rollback silently assumes the source survives

> **Captured note:** Define and enforce the cross-manager rollback result when the source disappears between the pop and a failed destination push. Both restore callbacks silently no-op for a missing source, while `commitCross` reports only the destination-side stale failure.

**Verification:** Partially verified — the loss path and real concurrent deletion writers are present, but source destruction currently discards that endpoint's remaining inventory too, so the intended transfer guarantee under deliberate teardown is not explicit.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/Transfer.hs:537` — `commitCross` performs the source pop and destination push as separate manager transactions, then invokes restoration after a failed push.
- `src/Engine/Scripting/Lua/API/Units/Transfer.hs:552` — the restoration result cannot affect the returned outcome because `RestoreStep` returns `IO ()`; the function always reports only the push's stale reason.
- `src/Engine/Scripting/Lua/API/Units/Transfer.hs:582` — unit restoration returns the manager unchanged when the source unit is absent, leaving the popped item nowhere.
- `src/Engine/Scripting/Lua/API/Units/Transfer.hs:619` — building restoration has the same silent missing-source branch.
- `src/Unit/Thread/Command/Lifecycle.hs:25` and `src/Building/Thread/Command.hs:116` — separate worker-thread commands can delete the source from either manager during this cross-manager window.
- `test-headless/Test/Headless/Unit/TransferApi.hs:706` — rollback tests acknowledge that the branch exists for genuine concurrency, but their injected push failure leaves the source present throughout restoration.

**Handoff context:**

- **Current behavior:** If the source is deleted after a successful pop and the destination then rejects the push, restoration silently fails and the API reports only that the transfer became stale for the destination reason.
- **Expected behavior:** The advertised no-loss/rollback contract either restores the item or exposes a defined teardown outcome that does not silently claim the rollback path completed.
- **Scope and constraints:** Surfaced while reviewing PR #1129 / issue #1085. Same-manager unit→unit and building→building transfers remain single-`IORef` atomic operations and are not implicated.
- **Remaining uncertainty:** Destroying a unit or building already discards its held items, so the processor should decide whether concurrent teardown is an explicit exception to the transfer invariant or whether manager coordination must preserve/report the popped item.

## 2. Save codec invariants

### PRR-3. Component codecs permit malformed historical version tables

> **Captured note:** Reject duplicate, current, and future version entries in `csOlderVersions`. `componentCodec` currently builds an ambiguous dispatch table from them, while `save_compat_audit.py` converts the same declaration through a set and hides the malformed entries.

**Verification:** Verified — the shared constructor and compatibility parser enforce neither uniqueness nor “strictly older than current”; a synthetic declaration with current-version duplicates and a future version parses successfully and normalizes to an apparently valid accepted-version set.

**Evidence:**

- `src/World/Save/Component/Types.hs:328` — the field contract calls every `csOlderVersions` entry older, but its type is an unrestricted list of `ComponentVersion` values.
- `src/World/Save/Component/Types.hs:347` — `componentCodec` sorts the current decoder together with that list without validating uniqueness or ordering relative to `csVersion`.
- `src/World/Save/Component/Types.hs:355` — decode uses association-list `lookup`, so a duplicate version silently makes one decoder unreachable while `ccInputVers` and diagnostics still retain duplicate entries.
- `tools/save_compat_audit.py:369` — the audit extracts every `atVersion` literal but immediately constructs `sorted({current} | set(older))`, erasing the duplicate evidence and accepting versions greater than current.
- `tools/test_save_compat_audit.py:1004` — the parser regression covers a valid current-v3 / older-v2,v1 table but has no malformed duplicate, current-as-older, or future-as-older case.

**Handoff context:**

- **Current behavior:** A future codec edit can declare two decoders for one version or call a version newer than `csVersion` “older”; runtime dispatch silently chooses the first matching decoder and the compatibility audit reports a deduplicated set.
- **Expected behavior:** Every accepted historical version is unique and strictly less than the current version, and malformed declarations fail the same mandatory checks that consume this table.
- **Scope and constraints:** Surfaced while reviewing PR #1143 / issue #1093. All current production `ComponentSpec` call sites inspected in this review are well formed; this is a latent invariant hole in the shared abstraction and its enforcement.
- **Remaining uncertainty:** None about acceptance of malformed tables; the processor should determine whether construction-time validation or the source audit is the authoritative enforcement boundary.

## 3. Generated-language contracts

### PRR-4. Bound-form review left consonant-scoping documentation inverted

> **Captured note:** Correct `Language.Generated.Onset.consonantOnly`'s canonical-contract comment after the dual-role-`y` review. It still says bound-form legality shares the consonant-only scope and skips `y`, while the reviewed implementation and tests deliberately require the wider consonant-capable scope.

**Verification:** Verified — the exported helper's documentation states the exact opposite of the final bound-form rule and names `Bound` as a consumer even though that module no longer calls it.

**Evidence:**

- `src/Language/Generated/Onset.hs:149` — the comment calls `consonantOnly` the one shared cluster definition for both boundary repair and bound-form legality.
- `src/Language/Generated/Onset.hs:152` — the same comment says a pair involving dual-role `y` is deliberately not treated as a cluster by both consumers.
- `src/Language/Generated/Bound.hs:206` — the bound-form contract explicitly says its scope is deliberately wider than boundary repair and includes every consonant-capable pair.
- `src/Language/Generated/Bound.hs:228` — the implementation calls `consonantCapable` for both characters and does not call `consonantOnly`.
- `test-headless/Test/Headless/Language/Generated.hs:1263` — the post-review regression requires dual-role-`y` pairs to be validated and proves a consonant-only scope would miss rejections.

**Handoff context:**

- **Current behavior:** A maintainer following the supposed single source-of-truth comment is told to preserve a scope that would reintroduce the bug fixed during PR review.
- **Expected behavior:** `consonantOnly` documents boundary repair's narrower rewrite scope, while bound-form legality documents and retains its distinct consonant-capable filter scope.
- **Scope and constraints:** Surfaced while reviewing PR #1142 / issue #1096. This is a source-contract defect, not a current generated-output defect; the final implementation and focused tests agree with the issue.
- **Remaining uncertainty:** None at capture time.

## 4. Repository architecture documentation

### PRR-5. Architecture guide still names the deleted World.ZoomMap facade

> **Captured note:** Update the world-generation architecture map after removing the `World.ZoomMap` facade. The guide still presents that exact module beside `World.Render.Zoom.*`, although only the more specific `World.ZoomMap.Cache.*`, texture, and palette modules remain.

**Verification:** Partially verified — the exact `World.ZoomMap` module is gone and the notation reads like a module reference, although a reader could interpret it informally as the surviving namespace prefix.

**Evidence:**

- `CLAUDE.md:226` — the architecture bullet still names `World.ZoomMap` without a wildcard or surviving child-module qualifier.
- `synarchy.cabal:871` — the library inventory starts at `World.ZoomMap.Cache` and lists only child modules; there is no exposed `World.ZoomMap` facade.
- `src/World/Render.hs:24` — the production renderer now imports `World.Render.Zoom.Quads` directly, the dependency direction PR #1164 intended to make explicit.
- `src/World/Thread/Command/Init.hs:54` — zoom-cache construction likewise imports the surviving `World.ZoomMap.Cache` facade rather than an exact `World.ZoomMap` module.

**Handoff context:**

- **Current behavior:** The primary architecture guide directs readers toward the name of the removed facade instead of distinguishing rendering (`World.Render.Zoom.*`) from cache/atlas construction (`World.ZoomMap.Cache.*` and siblings).
- **Expected behavior:** Architecture documentation names only live modules or clearly marks namespace prefixes, preserving the dependency boundary established by the facade removal.
- **Scope and constraints:** Surfaced while reviewing PR #1164 / issue #1133. No runtime or build defect is implied; the surviving `World.ZoomMap.*` child namespace is intentional.
- **Remaining uncertainty:** The reference may have been intended as an informal namespace label, but its asymmetric notation beside `World.Render.Zoom.*` and its match to the deleted facade name make that reading unclear.
