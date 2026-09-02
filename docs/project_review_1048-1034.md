# Project Review Findings: PRs #1048–#1034

These entries record focused evidence from the senior review of merged PRs #1048 through #1034, plus the two direct first-parent documentation commits in the same merge window, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [x] PRR-1. Shutdown comments still claim every worker stops after Vulkan teardown — [#1408]
- [x] PRR-2. `logEntryWith` documents a logging skip list that no longer exists — [#1410]
- [x] PRR-3. Shared YAML loading collapses every asset loader's source attribution — [#2167]

## 1. Shutdown lifecycle documentation

### [#1408] PRR-1. Shutdown comments still claim every worker stops after Vulkan teardown

> **Captured note:** Make the Vulkan-safety comment describe the two-phase worker shutdown that now surrounds teardown. PR #1045 moved combat and simulation shutdown ahead of Vulkan while leaving an inherited statement that Vulkan teardown runs before the worker threads stop.

**Verification:** Verified — the current function stops two workers before reaching a comment that says teardown precedes worker shutdown, then stops the remaining four workers afterward.

**Evidence:**

- `src/Engine/Loop/Shutdown.hs:41` — the function-level contract accurately states the new sequence: pre-render workers, Vulkan/GLFW, post-render workers, then the logger.
- `src/Engine/Loop/Shutdown.hs:53` — `shutdownEngine` stops `preRenderWorkers` before it reads graphics state or begins teardown.
- `src/Engine/Loop/Shutdown.hs:58` — the inherited local comment still says “Vulkan teardown below runs BEFORE the worker threads stop,” contradicting both the preceding call and the function-level contract.
- `src/Engine/Loop/Shutdown.hs:117` — only `postRenderWorkers` are stopped after GLFW teardown.
- `src/Engine/Core/Workers.hs:51` — the canonical phase inventory puts combat and simulation before render teardown; `:62-67` puts unit, world, input, and Lua after it.
- The merged diff for PR #1045 inserted the pre-render stop immediately above the old comment without updating its wording. Tracker searches for this contradiction found only the closed parent issue #1036 and adjacent shutdown work, not an issue that owns the stale contract.

**Handoff context:**

- **Current behavior:** Runtime shutdown follows the intended two-phase order, but the nearest Vulkan-safety explanation describes the old all-workers-after-teardown sequence. A maintainer reading locally can incorrectly infer that combat and simulation remain live during teardown or that moving their stop was accidental.
- **Expected behavior:** The local safety comment says that Vulkan teardown precedes only the remaining post-render workers and acknowledges that combat and simulation have already stopped; it remains consistent with the canonical phase inventory and function-level contract.
- **Scope and constraints:** Surfaced in PR #1045 / issue #1036. Preserve the established combat → simulation → Vulkan/GLFW → unit → world → input → Lua ordering and the rule that no worker calls Vulkan.
- **Remaining uncertainty:** No runtime-order defect was found; this is a documentation-contract defect whose impact is future shutdown maintenance.

## 2. Logging contracts

### [#1410] PRR-2. `logEntryWith` documents a logging skip list that no longer exists

> **Captured note:** Remove the `logEntryWith` Haddock's claim that `extractCallSite` has an unchanged skip list. PR #1042 deleted that mechanism immediately before PR #1043 introduced the shared helper and its stale explanation.

**Verification:** Verified — the comment directly contradicts the adjacent implementation and its authoritative source-attribution contract at HEAD.

**Evidence:**

- `src/Engine/Core/Log.hs:111` — `extractCallSite` is documented as taking the outermost frame from one unbroken `HasCallStack` chain.
- `src/Engine/Core/Log.hs:120` — the same contract explicitly says this design replaces the old hand-maintained internal function-name list from issue #945.
- `src/Engine/Core/Log.hs:133` — the implementation reverses `getCallStack` and returns the outermost location; it contains no skip-list names or filtering.
- `src/Engine/Core/Log.hs:138` — PR #1043's new `logEntryWith` comment nevertheless says computing `srcLoc` in the caller keeps `extractCallSite`'s skip list unchanged.
- `src/Engine/Core/Log.hs:171` — the real invariant is that the public logging entry points retain `HasCallStack` and compute the source before calling the unconstrained helper.
- `test-headless/Test/Headless/Core/LogParity.hs:67` and `test-headless/Test/Headless/Core/LogMonad.hs:39` — focused tests correctly enforce external attribution under the new outermost-frame design, but comments are not checked. Both focused describes passed during this review.
- Tracker searches for `logEntryWith` plus skip-list wording found only closed issue #945, which removed the mechanism; no live issue owns PR #1043's contradictory follow-up comment.

**Handoff context:**

- **Current behavior:** The logger behaves correctly, but the helper's own documentation tells future changes to preserve a nonexistent, deliberately retired mechanism.
- **Expected behavior:** `logEntryWith` documents the actual invariant: callers compute attribution before invoking the unconstrained assembly helper, so the helper contributes no call-stack frame and public entry points must retain `HasCallStack`.
- **Scope and constraints:** Surfaced at the boundary between PR #1042 / issue #945 and PR #1043 / issue #944. Preserve normal/thread entry-construction parity and the outermost-frame attribution tests.
- **Remaining uncertainty:** None at capture time; this is a source-documentation correction, not a logging behavior change.

## 3. Asset-loader logging

### [#2167] PRR-3. Shared YAML loading collapses every asset loader's source attribution

> **Captured note:** Preserve each owning asset loader as the reported source location when sharing the eleven YAML list-loader bodies. The extracted helper has no `HasCallStack` constraint, so every warning and debug entry now starts its call stack inside `Engine.Asset.YamlList`.

**Verification:** Verified statically — the logger's documented call-stack rule terminates at the first wrapper without `HasCallStack`, and both logging calls now live behind such a wrapper. The focused tests pass because they assert level, category, message, and return value but never inspect `leSrcLoc`.

**Evidence:**

- `src/Engine/Asset/YamlList.hs:25` — `loadYamlList` has no `HasCallStack` constraint.
- `src/Engine/Asset/YamlList.hs:37` — every parse-failure warning is emitted at the shared helper site; `:42` does the same for every success debug entry.
- `src/Engine/Asset/YamlItems.hs:230` — the domain loader is now a constraint-free wrapper around `loadYamlList`; the same shape appears in the other ten migrated list loaders, including `src/Engine/Asset/YamlUnits.hs:439` and `src/Engine/Asset/YamlBuildings.hs:94`.
- `src/Engine/Core/Log.hs:113` — `getCallStack` attribution is explicitly documented to follow an unbroken chain only as far as the first caller without `HasCallStack`; `:127-132` calls that constraint the standing requirement for any public logging wrapper family.
- `src/Engine/Core/Log/Format.hs:71` — normal formatted logs expose `leSrcLoc` as the module and line, making the shift from each domain loader to `YamlList:37` or `YamlList:42` externally visible.
- `test-headless/Test/Headless/Asset/YamlList.hs:34` — the success case asserts the level, category, and message but not `leSrcLoc`; `:49-62` has the same omission on the warning path. The entire focused describe passed during this review and therefore demonstrates the acceptance gap rather than refuting the attribution regression.
- Immediately before PR #1034, each logging call lived directly in its owning loader (for example `loadItemYaml`), so the constraint-free boundary and resulting outermost source location were domain-specific. PR #1034 moved those calls into the shared module while issue #1008 required observable behavior and log lines to remain unchanged.
- Tracker searches across open and closed issues for `YamlList`, YAML-loader source location, and log attribution found no issue that owns this regression.

**Handoff context:**

- **Current behavior:** All eleven asset YAML list loaders retain their messages and return values, but their normal log entries attribute success and failure to one generic helper line. Diagnostics no longer identify which owning loader emitted an entry without parsing the message text.
- **Expected behavior:** Shared loader logging preserves the domain loader as its externally reported call site, and focused coverage pins representative success and failure `leSrcLoc` values so another constraint-free wrapper cannot collapse attribution again.
- **Scope and constraints:** Surfaced in PR #1034 / issue #1008 and made especially visible by PR #1042's explicit outermost-call-stack contract. Preserve strict decoding, exact existing messages, `CatAsset` levels, failure-as-empty-list behavior, and the one shared implementation body.
- **Remaining uncertainty:** The call-stack transition is settled by the current static contract, but an explicit callback assertion through one real domain loader would provide a compact executable reproduction.
