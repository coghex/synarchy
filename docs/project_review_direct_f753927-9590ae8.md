# Project Review Findings: direct commits `f753927`–`9590ae8`

This report reviews twelve first-parent commits from January 26–27, 2026,
starting at `f753927e5af535379050dfc30150b3a3ef91fc62` and continuing through
`9590ae8094dbc614295a4f4ed044381bf3bbbded`.

The historical patches were checked against the current implementation at
`4c2a26d2e707e05355ca637a5b3717b2d7ddc0f4`. Findings describe current
behavior, not merely mistakes that existed in the historical revision.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Review cursor

Reviewed newest-first:

1. `f753927e` — multiline output
2. `aa9ecbaf` — up/down history browsing
3. `1e224851` — shell cursor movement and other functionality
4. `f280c96e` — ghosted tab suggestions
5. `022f82f5` — shell cursor
6. `15e01443` — refined script handling
7. `a81f5e72` — shell improvements
8. `6409f012` — set position and set size
9. `e0be6cd1` — color support
10. `1496918e` — Escape focus handling and minor filename fix
11. `c732742c` — shell execution
12. `9590ae80` — setText/getText

The next direct commit to review is
`36d97497561c27779b262d8b92f0354997da4a77`.

## Status

- [x] PRR-10. Debug console retains and remeasures unbounded scrollback — [#1956]
- [x] PRR-11. Console autocomplete suggests globals excluded from its execution sandbox — [#1958]
- [x] PRR-12. Debug console remains wider than supported framebuffers — [#1959]
- [x] PRR-13. Scene-text cache records nonexistent objects and survives object destruction — [#1961]

## Findings

### [#1956] PRR-10. Debug console retains and remeasures unbounded scrollback

> **Captured note:** Console output is retained without a limit even though the
> UI exposes only the newest viewportful of entries. Layout work still walks
> the entire retained history.

**Verification:** Partially verified. Unbounded retention and approximately
linear full-history measurement are verified. The exact point at which a real
interactive session develops a noticeable delay was not measured.

**Evidence:**

- `scripts/shell.lua` maintains the displayed `history` table separately from
  the persisted command-oriented `arrowHistory`.
- `arrowHistory` is capped at 1,000 entries, but `shell.addHistory` always
  appends to displayed history without removing old entries.
- `shell.calculateBoxHeight` walks every retained entry and calls
  `countLinesForEntry`, which performs character wrapping and measurement.
- `rebuildHistoryDisplay` renders newest-first and stops once the visible
  viewport is full. There is no scroll offset or control that exposes the
  older retained display entries.
- A focused headless timing probe measured approximately 0.011 seconds for
  1,000 entries and 0.055 seconds for 5,000 entries, a 4.93× increase.
- `c732742c` introduced the retained history, while `f753927e` made multiline
  layout measure every entry.

**Handoff context:**

- **Current behavior:** Arbitrary command results accumulate for the life of
  the console, including entries the user cannot revisit. Reopening or
  recalculating the console layout performs work proportional to that total.
- **Expected behavior:** Retention and layout cost should be bounded, or older
  output should be deliberately accessible without requiring every entry to
  be remeasured on routine rebuilds.
- **Constraints:** Preserve the distinction between clearing visible output
  and clearing persistent command history, the 1,000-entry command-history
  contract, and Unicode-safe wrapping.
- **Uncertainty:** The practical slowdown threshold depends on result size and
  session behavior. This is presently strongest as a latent performance and
  memory-growth defect.
- **Deduplication:** No matching open issue or existing findings-report entry
  was found.

### [#1958] PRR-11. Console autocomplete suggests globals excluded from its execution sandbox

> **Captured note:** Completion is generated from the host Lua global table,
> but console expressions execute inside a deliberately restricted sandbox.

**Verification:** Verified with a live headless reproduction.

**Evidence:**

- `scripts/shell.lua:getTableCompletions` resolves `_G[tableName]` before the
  corresponding value in `shellSandbox`.
- Top-level completion likewise enumerates both `_G` and the sandbox.
- `Engine.Scripting.Lua.API.Shell` constructs a restricted execution
  environment containing selected basic functions and engine APIs. It
  intentionally excludes globals such as `io`, `debug`, `package`, and
  `require`.
- In a headless session, `getCompletions("io.")` returned fourteen candidates,
  including `io.close` and `io.write`.
- Executing `io.open` through `engine.shellExecute` failed because `io` is
  absent from the execution sandbox. As a control, a suggested engine API
  function resolved successfully.
- The divergent completion lookup originated in `f280c96e`.

**Handoff context:**

- **Current behavior:** The console advertises functions that cannot be used
  from that same console.
- **Expected behavior:** Completion candidates should describe the actual
  execution environment, or unavailable host globals should be clearly
  distinguished rather than presented as executable suggestions.
- **Constraints:** Preserve sorted and deduplicated completions, engine API
  discovery, keyword and history suggestions, ghost hints, and the later
  Unicode completion fixes.
- **Deduplication:** No matching issue or findings-report entry was found.

### [#1959] PRR-12. Debug console remains wider than supported framebuffers

> **Captured note:** The console width is derived from a fixed 1,200-pixel
> center section and UI scale, without fitting the result to the framebuffer.

**Verification:** Verified from current source and headless geometry
inspection. A graphical screenshot was not taken.

**Evidence:**

- `scripts/shell.lua` defines a base tile size of 64 pixels and a fixed
  `middleWidth` of 1,200 pixels.
- Its horizontal layout places a margin, left edge tile, middle section, and
  right edge tile without using the queried framebuffer width to constrain
  them.
- At 1× scale, the console's right edge is 1,368 pixels:
  `40 + 64 + 1200 + 64`.
- The input-width calculation and scale-refresh path are likewise based on
  `middleWidth` and global UI scale rather than a local fit.
- A headless geometry query measured the same 1,368-pixel right edge.
- `scripts/ui/responsive.lua` defines 800×600 at 1× UI scale as a supported
  combination, leaving 568 pixels of the console outside that framebuffer.
- Issue #750 included the shell among the responsive surfaces, but its present
  regression coverage checks resize delivery and lifecycle behavior rather
  than shell bounds.
- The fixed 1,200-pixel center width originated in `e0be6cd1`.

**Handoff context:**

- **Current behavior:** The console cannot remain fully in-frame at smaller
  supported framebuffer sizes.
- **Expected behavior:** Console history, input, and controls should remain
  visible and reachable throughout the documented responsive envelope, while
  out-of-envelope sizes continue to degrade safely.
- **Constraints:** Preserve debug-layer pass-through behavior, state and focus
  across rebuilds, single resize delivery, history contents, and the 0×0
  minimization guard.
- **Deduplication:** Closed issue #750 intended to cover this surface, but the
  current width defect persists and has no open tracker owner.

### [#1961] PRR-13. Scene-text cache records nonexistent objects and survives object destruction

> **Captured note:** `engine.setText` updates the scene-text cache before
> determining whether the addressed scene node exists, while destruction never
> removes the cached entry.

**Verification:** Verified for nonexistent IDs through a live headless
reproduction. The destruction behavior is verified directly from the current
message handlers.

**Evidence:**

- `Engine.Scripting.Lua.Message.Scene.handleSetText` unconditionally inserts
  the supplied ID and text into `uicTextBuffersRef`.
- It then calls `modifySceneNode`, whose Boolean result is ignored. The
  documented missing-node behavior is a no-op, but the cache has already been
  changed.
- The scene destruction handler calls `deleteSceneNode` without deleting the
  corresponding map entry.
- Repository-wide inspection found the map's insertions and reads but no
  deletion or lifecycle reset after initialization.
- A headless session called `engine.setText(424242, "orphan")`; a later command
  retrieved `"orphan"` for that arbitrary ID despite there being no matching
  scene node.
- `9590ae80` introduced the text-buffer cache and set/get API. The later
  generalized destruction work in `a81f5e72` did not add cache cleanup.

**Handoff context:**

- **Current behavior:** Missing-object writes create observable cache entries,
  and destroyed nodes leave their last text retained. Reuse of an ID can
  therefore expose stale text, while repeated invalid or destroyed IDs can
  grow the cache.
- **Expected behavior:** A missing-node `setText` should be a genuine no-op,
  and cached text should share the scene node's lifetime.
- **Constraints:** Preserve the cross-thread Lua API contract and the later
  malformed-UTF-8 handling added for scene text.
- **Uncertainty:** Current production Lua does not appear to use the legacy
  scene-text API, so the normal-game impact may presently be low. The public
  API and diagnostic probes remain reachable.
- **Deduplication:** Issue #618 covers malformed UTF-8 in `setText`, and #897
  covers capability migration of the cache. Neither covers missing-node
  insertion or destruction cleanup.

## Historical observations not filed again

- `15e01443` contributed to the disconnected Lua module-identity problem
  already recorded as LUA-1 in `docs/lua_script_findings.md`.
- Invalid Lua tick intervals are already PRR-9 in
  `docs/project_review_direct_ea2c03d-16daead.md`.
- `f753927e` temporarily duplicated `calculateBoxHeight`; the next historical
  commit, `16daead6`, removed the duplicate.
- Byte-based cursor movement, editing, completion, and display wrapping were
  subsequently repaired by issues #1187 and #1159.
- The original script scheduler's `utctDayTime` clock was later replaced with
  POSIX time.
- The misleading half-alpha `"opaque"` color alias was addressed by #378 and
  retained only as a deprecated compatibility alias.
- The console quit command, module loading error handling, and stack cleanup
  were hardened by later work.

## Validation performed

- Inspected all twelve historical patches and their current descendants.
- Searched current issues and findings reports for duplicates.
- Ran focused ephemeral headless sessions for:
  - completion-versus-sandbox behavior;
  - nonexistent scene-text IDs and shell geometry;
  - scrollback measurement scaling.
- Did not run the full Hspec suite, the complete probe sweep, world checks,
  offscreen graphical verification, or `make ci`.
- No implementation or tracker changes were made.
