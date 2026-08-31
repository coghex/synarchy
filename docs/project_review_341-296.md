# Project Review Findings: PRs #341–#296

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #341, #339, #320, #310, #318, #322, #314, #311, #293, #294, #295, and #296 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #341's executable Lua serializer has since been replaced by the finite, data-only save codec; #339's chemical-erosion clamp remains centralized at both consumers; #320's collapse/crawl recovery retains its two-threshold hysteresis; #318's altitude lapse is still shared by worldgen ice and live ambient queries; #314's failed portal spawns remain rate-limited without consuming the roster; #311's lunge reads the canonical `max_stamina` accessor; #293's all-world snapshot was superseded by the transactional multi-world save system; #294's editor still covers every current rebindable action; #295's designation layer is now exercised by the construction pipeline; and later #806 supplied #296's originally omitted calorie threshold effects. Focused checks passed for the camera clamp (7/7), the glacier-rim exposure guard (2 passed, one full-tier case pending by design), input bindings (21/21), and the offline collapse-hysteresis harness (9 scenarios plus 50 jitter ticks). The previous adjacent batch also completed the current arena multi-world save/restart/load probe with zero failed checks. No full headless suite, full worldgen tier, graphical session, world check, baseline capture, or `make ci` was run. Two non-duplicate concerns remain; the second is deliberately retained as a lower-confidence serialization-contract question for the processor to verify before drafting an issue.

## Status

- [x] PRR-1. The glacier-rim camera workaround outlived the loader bug it fenced — [#1953]
- [x] PRR-2. Debug-console JSON silently collapses distinct Lua table keys — [#1955]

## 1. Camera teleport after the rim-loader repair

### [#1953] PRR-1. The glacier-rim camera workaround outlived the loader bug it fenced

> **Captured note:** Retire `camera.goToTile`'s hazard-specific six-chunk fence and width-8 zoom ban now that PR #363 fixed the shared glacier-rim loader. Keep only the camera/world bounds that are still product policy, and prove safety by loading the repaired rim rather than by preventing the teleport from reaching it.

**Verification:** Verified structurally and in a live width-8 world. The original heap-overflow mechanism is repaired in the shared chunk generator, and a manually requested tile zoom at the exact position `goToTile` considers unsafe now remains alive. The precise replacement fence is a design choice for the processor, but the current restrictions and their stated safety rationale are obsolete.

**Evidence:**

- Issue #297 / PR #310 introduced a deliberately temporary guard around a then-unfixed root cause: `camera.goToTile` used a six-chunk glacier buffer, and the minimum width-8 world stayed zoomed out because any tile-level load was believed to reach a heap-overflowing rim chunk. The issue explicitly left the loader mechanism to #298.
- Issue #298 / PR #363 subsequently found and fixed that mechanism. A mixed glacier-diagonal chunk exposed a real column down to a neighbouring beyond-rim column's `minBound` sentinel, producing a near-`2^63` vector allocation. The fix guards the sentinel inside `generateLoadedChunk`, so both the initialization-region and camera-visible loaders use it.
- `test-headless/Test/Headless/WorldGen/Exposure.hs:116-159` now queues every mixed width-8 rim chunk and waits for them without a heap overflow. The focused `Column exposure invariant` run passed its width-64 and width-8 cases during this review; only the explicitly full-tier volcano case remained pending.
- `src/Engine/Loop/Camera.hs:34-44` still says rim loading heap-overflows and that there is no closer safe location. `:116-135` retains `cameraGotoBufferChunks = chunkLoadRadius + 4`, while ordinary pan/drag uses only the two-chunk fence at `:54-58,99-107`.
- `src/Engine/Loop/Camera.hs:141-157` still concludes that a width-8 world has no safe zoomed-in region and returns `False` from `gotoTileZoomSafe 8`, entirely from the pre-#363 loader model.
- `src/Engine/Scripting/Lua/API/Camera.hs:195-234` applies the six-chunk fence, forces an unsafe-by-definition world to map zoom (`zoomFadeEnd + 0.5`), and disables z-tracking. On width 8, the effective six-chunk buffer consumes the whole half-world, so every teleport target is pinned to the centre.
- `test-headless/Test/Headless/Camera/GotoClamp.hs:54-107` actively pins the workaround: teleports must be more conservative than pan/drag, the width-8 target must land at the centre, and `gotoTileZoomSafe 8` must be false. All seven focused examples passed, confirming that current tests preserve the stale restriction rather than re-evaluate it against the repaired loader.
- A live review reproduction initialized `world.init("camera-review", 7, 8, 3)`, then called `camera.goToTile(500,500)`. It reported position `(0,0)`, zoom approximately `2.1`, and `getZTracking() == false`. Calling `camera.setZoom(0.5)` immediately afterward, waiting three seconds, and querying the active world succeeded; the formerly forbidden camera-visible load no longer killed the world thread.
- Tracker and pending-report searches found closed #297/#298 but no open owner for retiring their superseded safety workaround.

**Handoff context:**

- **Current behavior:** Every `goToTile` on a width-8 world remains at the centre in map view with z-tracking off, even for an interior requested tile. Larger worlds jump as much as six chunks inside the glacier edge, four chunks farther inward than ordinary camera movement. Event-log and popup jumps near the edge therefore land materially away from the event they are meant to show.
- **Expected behavior:** A teleport reaches the nearest location allowed by the current camera/world-visibility policy, enters normal tile zoom where that is the verb's contract, and tracks the surface at the position where it lands. Glacier-rim loading safety comes from #363's repaired column generation and a regression that actually services the relevant chunks.
- **Scope and constraints:** Surfaced from PR #310 / issue #297 after its explicit dependency #298 was resolved by PR #363. Preserve the cylindrical wrap axis, real world bounds, all four facings, velocity/drag reset, z-slice sampling at the final coordinate, no out-of-bounds generation, and the mixed-rim exposure regression. Remove comments and pure tests that still describe the fixed heap-overflow mechanism. Do not assume the pan path's two-chunk aesthetic fence is necessarily the final product policy without checking glacier visibility expectations.
- **Remaining uncertainty:** The repository still deliberately stops ordinary panning two chunks inside the glacier edge, apparently as a visibility/aesthetic boundary independent of heap safety. The processor should decide whether `goToTile` should use that same fence, a viewport-aware variant, or a true tile bound; only the extra four hazard chunks and the width-8 tile-zoom prohibition are directly shown to have lost their justification.

## 2. Debug-console table-key identity

### [#1955] PRR-2. Debug-console JSON silently collapses distinct Lua table keys

> **Captured note:** Give non-array Lua tables an explicit key contract at the debug-console JSON boundary. Distinct Lua keys must not be emitted as duplicate JSON member names that ordinary consumers silently overwrite; either encode their types, reject unsupported/colliding keys with a clear diagnostic, or define another lossless representation.

**Verification:** Verified end to end through the real headless TCP console and Python's documented project workflow, with product severity uncertain. The serializer emits syntactically parseable JSON, but mixed numeric/string keys and multiple non-string key types collide after its key-to-text conversion. `json.loads` then silently discards entries.

**Evidence:**

- Issue #319 / PR #322 made debug-console output valid JSON for non-finite numbers and every C0 control character because repository probes pipe this output directly into `json.loads`. Its current value and string fixes remain present at `src/Engine/Scripting/Lua/API/Shell.hs:171-199,269-282`.
- `src/Engine/Scripting/Lua/API/Shell.hs:215-239` treats a table as an array only when every key parses as exactly the consecutive integers `1..n`; every mixed-key table becomes a JSON object.
- `src/Engine/Scripting/Lua/API/Shell.hs:243-266` converts numeric keys with Lua `tostring`, copies string keys verbatim, and converts every other key type to the same literal `"<key>"`. It never detects duplicate converted names before joining the members into an object.
- Numeric key `1` and string key `"1"` are distinct in Lua but both become JSON member name `"1"`. Boolean keys `true` and `false` are also distinct but both become `"<key>"`; table/function/userdata keys collapse onto that same name too.
- A live review command returned `{[1]="numeric", ["1"]="string", [true]="bool", [false]="false-bool"}` through the real console. The raw result was `{"<key>":"false-bool","1":"numeric","<key>":"bool","1":"string"}`. Python `json.loads` produced only `{'<key>': 'bool', '1': 'string'}`: two of four values were silently lost. Lua table traversal order is not a durable contract, so which colliding value survives should not be relied upon.
- `src/Engine/Scripting/Lua/Thread/Console.hs:145-170` documents that tables are automatically serialized to JSON, and `tools/probelib.py:102-109` parses returned objects through `json.loads`. Neither boundary warns that only string-keyed maps with collision-free textual names are representable.
- Existing save-codec tests reject unsupported and duplicate keys, but that stricter data codec is separate from the debug serializer. Searches across the tracker and pending findings reports found no owner for the console's key-identity collapse.

**Handoff context:**

- **Current behavior:** A debug query or probe can receive valid JSON and continue with an incomplete dictionary. Numeric/string aliases overwrite each other, and all boolean/object-like keys share one placeholder. This is harder to diagnose than invalid JSON because the consumer sees a successful parse and may make assertions against whichever values happened to survive.
- **Expected behavior:** The console boundary never silently maps two distinct Lua entries onto one JSON object name. String-only maps and ordinary consecutive arrays retain their convenient current shapes; unrepresentable maps either use a typed pair representation or fail/report explicitly before a probe mistakes partial data for complete data.
- **Scope and constraints:** Surfaced while reviewing PR #322 / issue #319. Preserve the quoted `inf`/`-inf`/`nan` diagnostics, complete JSON string escaping, finite-number formatting, consecutive-array ordering, recursion-depth guard, multiple return values, and the existing simple string-keyed object shape used throughout the probes. Add mixed `1`/`"1"`, two booleans, and at least one unsupported object-like key case at the actual `executeDebugLua` boundary.
- **Remaining uncertainty:** The debug console may be intended as a best-effort human renderer rather than a lossless general Lua codec, and JSON objects cannot natively retain arbitrary Lua key types. The processor should decide whether explicit rejection is sufficient and whether any shipped probe returns such a map today. The demonstrated silent data loss is real; its tracker priority depends on that intended contract and current caller reachability.
