# Verified bug findings

This report records correctness and operational defects verified in the current
repository during a broad bug audit. It is an evidence handoff, not an issue
backlog or implementation plan.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The audit inspected the current Haskell and Lua implementation, focused tests,
CLI probes, and relevant line history. It concentrated on character/byte
boundaries, UI display transforms, command-line validation, headless control
surfaces, and developer-tool warnings. Focused verification included:

- the real `scripts.shell` module with a UTF-8-validating text-width stub;
- the real `unit_info_v2_panel_engine` abbreviation helper with the same kind
  of validating stub;
- a safe headless boot on invalid port `-1`, terminated after it continued
  running without a debug listener;
- a size-16 dump with `--seed not-a-number`, which exited successfully while
  reporting and using seed 42;
- `python3 -W error::SyntaxWarning -m compileall -q tools` on Python 3.14.6;
- `cabal test synarchy-test-headless --test-options='--match
  Lua.TextWrapping'`, whose seven wrapping tests passed.

No graphical or preview window was launched. The full headless suite, full
world checks, probes unrelated to these concerns, and `make ci` were not run.
No GitHub duplicate search was performed; that belongs to `process-report`.

## Status

- [x] BUG-1. Debug-console editing corrupts non-ASCII input — [#1187]
- [x] BUG-2. Remaining UI truncators split UTF-8 code points — [#1189]
- [x] BUG-3. Headless boot survives without its only control listener — [#1190]
- [x] BUG-4. Dump CLI silently substitutes defaults for malformed arguments — [#1191]
- [x] BUG-5. Action-outcome audit fails strict Python warning compilation — [#1192]
- [x] BUG-6. Adding a concept id can change an existing concept's generated root — [#1868]
- [ ] BUG-7. Escape stops dismissing dropdowns after their IDs exceed 100
- [ ] BUG-8. Replacing list items leaves scrollbar position and visibility stale
- [ ] BUG-9. Resizing the save browser discards its scroll position
- [ ] BUG-10. Crawling restores hydration without a water source
- [ ] BUG-11. Organ-failure stamina drain uses the wrong body-size floor
- [ ] BUG-12. Float rounding can prevent the starvation death threshold from firing
- [ ] BUG-13. Autonomous drinking credits water that was not drained
- [ ] BUG-14. Injury recovery interrupts source drinking and leaves its action locked
- [ ] BUG-15. Small resource changes are discarded forever instead of accumulated
- [ ] BUG-16. Electrical networks disconnect at the cylindrical seam
- [ ] BUG-17. Focus cleanup discards typed dropdown choices before submission
- [ ] BUG-18. Routine body-regrowth ticks lose muscle growth and desynchronize total mass
- [ ] BUG-19. Treatment aliases separate wounds inflicted at the same game time
- [ ] BUG-20. Healing does not restore blood needed for revival
- [ ] BUG-21. Ground-repair return retries never end after the item leaves its worker
- [ ] BUG-22. Dead and collapsed builders continue producing building progress
- [ ] BUG-23. Dead and collapsed medics retain claims that block replacement treatment
- [ ] BUG-24. An out-of-range preferred medic prevents nearby medics from treating
- [ ] BUG-25. Autonomous infection treatment ignores antibiotic kits without bandages

---

## Text handling

### [#1187] BUG-1. Debug-console editing corrupts non-ASCII input

The debug console treats `cursorPos` and `inputScrollOffset` as byte offsets
when slicing Lua strings, but character insertion advances the cursor by
exactly one regardless of the inserted character's encoded byte length.
Left/Right, Backspace, Delete, scrolling, and visible-prefix trimming also move
or cut one byte at a time. As soon as the buffer contains a multibyte code
point, these operations can position the caret inside it and create malformed
UTF-8.

**Evidence:**

- `scripts/shell.lua:293` — `onChar` slices at `cursorPos` and then increments
  it by one, although the delivered `char` can occupy multiple UTF-8 bytes.
- `scripts/shell.lua:304` — Backspace removes the byte at the byte-oriented
  cursor and decrements it by one; it does not remove one code point.
- `scripts/shell.lua:562` — cursor placement measures a byte-sliced prefix.
- `scripts/shell.lua:597` — horizontal scrolling advances
  `inputScrollOffset` one byte at a time and repeatedly measures those slices.
- `scripts/shell.lua:623` — visible-input trimming tests every byte prefix, so
  it passes incomplete code points to `engine.getTextWidth`.
- `scripts/shell.lua:917` — arrow navigation and Delete likewise step or remove
  one byte at a time.
- `test-headless/Test/Headless/Lua/TextWrapping.hs:107` — the existing shell
  source audit covers the output-wrapping loop only; it does not exercise
  console input editing.

The focused real-module reproduction called `onChar("é")`, Backspace, and
then `getVisibleInput`; the validating width function rejected the surviving
lone byte with `invalid UTF-8 at byte 1`.

**Handoff context:**

- **Current behavior:** Typing, navigating through, deleting, or horizontally
  scrolling non-ASCII console input can render mojibake, misplace the caret, or
  leave an invalid command buffer.
- **Expected direction:** Every console caret, selection, edit, and scroll
  boundary should use one consistent code-point coordinate system and should
  only pass complete UTF-8 sequences to display or execution paths.
- **Scope and constraints:** Cover insertion, Backspace/Delete, Left/Right,
  Home/End, history restoration, completion insertion, cursor measurement, and
  horizontal scrolling. Preserve byte-identical ASCII behavior and add focused
  real-module tests rather than relying on the wrapping-only audit.
- **Remaining uncertainty:** The exact visible failure under the production
  font backend is backend-dependent—it may reject or replace malformed bytes—
  but the byte corruption itself is deterministic.

### [#1189] BUG-2. Remaining UI truncators split UTF-8 code points

Several display paths still implement a character cap by applying Lua's byte
length operator and decrementing or slicing byte indices. This violates the
repository's display-text contract and can create an invalid label or pass an
invalid candidate to text measurement. The recent shared wrapping change does
not cover these independent fixed-width truncators.

**Evidence:**

- `scripts/unit_info_v2_panel_engine.lua:61` — `abbreviateToWidth` starts at
  `#text`, decrements one byte per iteration, and measures `text:sub(1, n)`;
  the real helper reproduced an `invalid UTF-8 at byte 5` failure while
  abbreviating `ééé`.
- `scripts/combat_log.lua:186` — combat-tab truncation treats `#text` as a
  character count and cuts with `string.sub`; tab text incorporates persistent
  personal unit names at `scripts/combat_log.lua:447`.
- `scripts/injury_log_panel.lua:152` — injury-tab truncation repeats the same
  byte-count/byte-slice implementation; its tab names also incorporate
  personal unit names at `scripts/injury_log_panel.lua:367`.
- `scripts/crafting_panel.lua:247` — the crafting panel has a third byte-based
  truncator used for recipe names/summaries and claimant names at
  `scripts/crafting_panel.lua:459`, `scripts/crafting_panel.lua:469`, and
  `scripts/crafting_panel.lua:679`.
- `test-headless/Test/Headless/Lua/TextWrapping.hs:92` — current call-site
  coverage asserts that three log panels delegate word wrapping, but it does
  not inspect or run their separate tab-name truncators.

**Handoff context:**

- **Current behavior:** Extended-Latin, CJK, or emoji text can be split inside a
  code point when these labels need abbreviation, producing mojibake or a text
  measurement/rendering failure.
- **Expected direction:** All fixed-character and pixel-width abbreviation
  paths should cut only at complete code-point boundaries while retaining the
  existing ellipsis and layout behavior.
- **Scope and constraints:** Audit the four named implementations together,
  reuse the established UTF-8/display helpers where their malformed-input
  policy fits, and test accented, three-byte, and four-byte characters. Keep
  pure-ASCII output unchanged.
- **Remaining uncertainty:** The checked-in recipe and personal-name data
  inspected during this audit are ASCII; the deterministic failure currently
  requires extended/custom data or another Unicode-producing caller. The
  helpers themselves accept arbitrary strings and fail on that supported
  input class.

## Boot and CLI reliability

### [#1190] BUG-3. Headless boot survives without its only control listener

When the debug server cannot bind, the Lua-thread startup path logs a warning,
substitutes an inert queue, and lets the engine continue. That tolerance may be
reasonable for a graphical game, but a headless or offscreen process has no
window and relies on the debug listener for readiness, commands, and graceful
shutdown. Port zero is an additional trap: it is globally interpreted as dump
mode's intentional no-listener sentinel even when the user explicitly selects
headless mode.

**Evidence:**

- `app/Main.hs:56` — `--port` uses the generic parser, and a malformed value
  becomes `Nothing`; headless then substitutes port 8008 at `app/Main.hs:145`.
- `src/Engine/Scripting/Lua/DebugServer.hs:45` — port 0 unconditionally emits a
  ready marker and returns an inert queue without opening a listener; the
  function has no boot-mode context to distinguish dump from headless.
- `src/Engine/Scripting/Lua/DebugServer.hs:54` — invalid, unavailable, or
  already-bound listener ports return `Left` after synchronous setup fails.
- `src/Engine/Scripting/Lua/Thread.hs:113` — every such `Left` is downgraded to
  a warning and replaced with another inert queue.
- `app/App/Headless.hs:1` — the headless profile explicitly has no window or
  GPU and advertises the debug console as its configurable control surface.

A focused boot with `--headless --port -1` logged the debug-server failure,
started the world, unit, simulation, and combat threads, entered the headless
loop, and remained alive until manually interrupted. It never produced a
usable listener.

**Handoff context:**

- **Current behavior:** A typo, out-of-range port, port collision, or explicit
  port 0 can leave a resource-consuming headless process that never becomes
  controllable and cannot receive `engine.quit()`.
- **Expected direction:** Modes whose only interactive control surface is the
  debug listener should validate their port and fail startup cleanly when no
  listener exists; dump's internal no-listener sentinel should remain confined
  to dump mode.
- **Scope and constraints:** Distinguish absent, malformed, out-of-range, zero,
  and bind-failed ports. Preserve any intentional graphical-mode tolerance,
  and ensure a failure after Lua initialization tears down already-created
  resources instead of leaking workers.
- **Remaining uncertainty:** Whether graphical mode should also fail on a
  listener bind error is a product decision; the headless failure mode is not
  ambiguous.

### [#1191] BUG-4. Dump CLI silently substitutes defaults for malformed arguments

The dump CLI conflates an absent numeric option with a present-but-malformed
one. `parseArg` drops a malformed occurrence and returns `Nothing`, after which
`Main` supplies a default. Region parsing similarly returns its default tuple
on invalid syntax, and dump-layer parsing ignores every unknown token. An
automation typo therefore succeeds while generating or serializing a different
world than requested.

**Evidence:**

- `app/App/Cli.hs:52` — `--dump=<layers>` builds booleans only for recognized
  tokens and never rejects an empty or unknown layer list.
- `app/App/Cli.hs:69` — a present numeric flag whose value fails `reads` is
  treated like absence rather than reported as an error.
- `app/App/Cli.hs:88` — malformed `--region` text silently becomes
  `(-8,-8,8,8)`.
- `app/Main.hs:56` — seed, world size, and plate count all use that lossy
  parser; defaults are applied at `app/Main.hs:66`, `app/Main.hs:70`, and
  `app/Main.hs:94`.
- `tools/preview_cli_probe.py:266` — current no-boot CLI coverage verifies that
  flags are rejected in incompatible modes, but has no cases for malformed
  values in a mode that honors them.

The focused command `--dump=terrain --seed not-a-number --worldSize 16
--plates 1 --region 0,0,0,0` exited 0 and reported `dump: seed=42`, proving the
malformed requested seed was replaced with the default.

**Handoff context:**

- **Current behavior:** Malformed seed/world-size/plate/region input and
  misspelled layer selectors can return successful, plausible output for the
  wrong request.
- **Expected direction:** Omitted options may keep documented defaults, while
  any present malformed or unknown value should produce a specific pre-boot
  error and nonzero exit.
- **Scope and constraints:** Preserve intentional normalization of valid
  numeric world sizes and plate counts. Add pure parser coverage plus no-boot
  CLI checks so validation does not require world generation.
- **Remaining uncertainty:** `--size` uses the same absent-or-invalid fallback
  policy for offscreen mode; whether it joins this finding or receives a
  separate compatibility decision should be settled during processing.

## Developer tooling

### [#1192] BUG-5. Action-outcome audit fails strict Python warning compilation

One ordinary docstring contains regex notation with unescaped `\s` and `\S`.
Python 3.14 reports this as a `SyntaxWarning` whenever the script is compiled
or run, and promoting syntax warnings to errors prevents the audit module from
compiling at all. The actual regex expression below is already a raw string;
the defect is limited to its explanatory docstring.

**Evidence:**

- `tools/action_outcome_coverage.py:295` — the `_portal_accepted_body`
  docstring contains the non-raw text ``[\s\S]`` at line 299.
- `tools/action_outcome_coverage.py:303` — the executable regex beside it is a
  raw string and does not have the same escape problem.

On Python 3.14.6, `python3 tools/action_outcome_coverage.py --help` printed the
warning before normal output. `python3 -W error::SyntaxWarning -m compileall
-q tools` failed on this file and no other tool module.

**Handoff context:**

- **Current behavior:** The action-outcome audit emits warning noise in normal
  use and fails any warnings-as-errors Python compile gate.
- **Expected direction:** The explanatory text should compile warning-clean
  without changing the regex or its audit behavior.
- **Scope and constraints:** Keep the regex documentation readable and run the
  tool's own checks plus strict-warning compilation after the change.
- **Remaining uncertainty:** None at draft time.

## Generated languages and naming

### [#1868] BUG-6. Adding a concept id can change an existing concept's generated root

> **Captured note:** Adding a new concept id to data/language/concepts.yaml can change an EXISTING concept's generated native root, because Language.Generated.Root.assignRoots folds over `sort ids` and resolves collisions against roots already placed. A newly added id that sorts earlier can claim a root an existing concept would have taken, forcing that existing concept to reroll to attempt+1. Reproduced via `cabal repl lib:synarchy`: on language seed 1337 (currentGeneratorVersion 5, 151 shipped concepts), adding one probe id changed GATE's root from "jyk" to "lhirav", and another changed ENVY's from "vra" to "jha" — 2 of 60 single-id probe additions; 0 of 60 on seeds 42, 7, 99 and 2718. Consequence: a previously persisted EtymologySource naming that concept no longer rebuilds to the stored name, so Language.Etymology's surface check (`rebuilt ≢ storedName`) reports EtySurfaceMismatch and the etymology reads as unavailable. Names themselves are unaffected — they are write-once (#1101). This contradicts src/Language/Semantic/Types.hs:8-12, which says ids "may be added, never renamed or reused" — i.e. ADDITION is presented as the safe operation, and it is not root-stable. Not ready to be an issue: the fix needs a design decision between (1) making assignment order-independent, which changes roots for existing seeds and needs a currentGeneratorVersion bump 5→6; (2) detecting rather than fixing, via a check that recomputes roots across a fixed seed panel before/after a catalogue change and fails when an existing concept's root moves; or (3) accepting and documenting additions as best-effort for etymology, which contradicts the stated contract. Related: #710 (root derivation), #713 (add-only rule), #1104 (persisted EtymologySource), #1717 (inventory ratchet — explicitly out of scope there), #1383/#1385 (pinned name vectors, a partial backstop covering only 6 of 151 concepts on one provenance).

**Verification:** Verified — root assignment is order-dependent, and a single
added concept id demonstrably re-roots an existing concept on at least one
language seed.

**Evidence:**

- `src/Language/Generated/Root.hs:38` — `assignRoots` is
  `foldl' place M.empty (sort ids)`, so each concept is placed in sorted-id
  order against the roots already assigned.
- `src/Language/Generated/Root.hs:41` — `usedLower` is built from `M.elems acc`,
  the roots placed *so far*, which is what makes the outcome depend on which
  ids preceded this one.
- `src/Language/Generated/Root.hs:45-48` — on a collision `resolve` recurses
  with `attempt + 1`, and `Hash.hs`'s seed folds that attempt in, so a rerolled
  concept gets an entirely different root rather than a nearby variant.
- `src/Language/Generated/Hash.hs:53-57` — `conceptSeed` derives from the
  generator version, language seed, `conceptIdText`, and the attempt counter.
  Nothing else. An id inserted earlier in sort order can therefore only affect
  another concept through the collision path above.
- `src/Language/Etymology.hs:373` — `explain` recomputes
  `assignLanguageRoots prof (conceptIds cat)` from the *current* catalogue, not
  from anything stored, so a re-rooted concept is re-derived at read time.
- `src/Language/Etymology.hs:361-362` — the rebuilt native surface is compared
  against the authoritative stored name; a mismatch yields
  `EtyUnavailable (EtySurfaceMismatch storedName rebuilt)`.
- `src/Language/Semantic/Types.hs:9-12` — the stated contract: concept ids are
  "LOAD-BEARING", and "Ids may be added, never renamed or reused." Addition is
  the operation this defect makes unsafe.
- `src/Language/Generated/Types.hs:97` — `currentGeneratorVersion = 5`, the
  version any change to assignment would have to advance.
- `test-headless/Test/Headless/Location/Naming.hs:155` — the pinned name vector
  `["Leraj-yroeb", "Jdyebto-efbne", "Fyąyn-fkofbe"]`, whose comment names the
  shipped `data/language/concepts.yaml` as a cause to investigate before
  re-blessing. It is a partial backstop: it covers only the six concepts those
  three names use, on one provenance, out of 151.

Reproduced against the real library in `cabal repl lib:synarchy`, comparing
`lrFree (assignLanguageRoots prof ids)` with
`lrFree (assignLanguageRoots prof (probe : ids))` for 60 synthetic probe ids per
seed, over the shipped 151-entry catalogue at `currentGeneratorVersion`. Seed
1337 produced two re-rootings — `GATE` from `jyk` to `lhirav`, and `ENVY` from
`vra` to `jha`. Seeds 42, 7, 99 and 2718 produced none. No engine, world, or
window was involved.

**Handoff context:**

- **Current behavior:** Expanding the concept catalogue — the one change the
  contract calls safe — can silently change an existing concept's native root
  in some languages. Any name persisted from a world using that concept then
  fails the etymology surface check and reads as unavailable. The stored name
  text is unaffected, being write-once (#1101), so the world still reads
  correctly and only the etymology degrades.
- **Expected behavior:** Adding a concept id leaves every previously assigned
  root unchanged, so a persisted `EtymologySource` keeps decomposing — or, if
  that guarantee is deliberately not offered, the contract says so and the
  limitation is detectable rather than silent.
- **Scope and constraints:** Preserve deterministic, seed-reproducible
  generation and the existing collision-freedom property (roots stay unique
  case-insensitively within a language). Any change to assignment changes roots
  for existing seeds, so it needs a `currentGeneratorVersion` bump and must
  leave already-persisted names decodable under their own recorded version.
  Bound forms are not implicated: `Root.hs`'s `assignLanguageRoots` layers
  `assignBoundForms` strictly on top of the finished free-root map and cannot
  reroll it.
- **Remaining uncertainty:** Which of three directions to take is an open
  design decision, not an implementation detail — (1) make assignment
  order-independent, (2) detect drift with a fixed seed panel rather than fix
  it, or (3) accept and document additions as best-effort for etymology. The
  measured rate (2 of 60 probe additions on one of five seeds) is a sample from
  synthetic ids, not a prediction for a real catalogue expansion.

## UI interaction and state: September 7, 2026 audit

BUG-7 through BUG-9 were verified against local `master` commit
`78e73df070070058585c8279652bca43386d2873`. Source paths and line numbers
below refer to that revision, not the older `docs-wip` code. All cited source
and fixture files remained unchanged when `master` advanced during the audit
to `ca55a6fbcd1ac5517a5145582daa7ec1881b8e54`. Searches of the
Markdown reports in both worktrees found no existing equivalent findings.
No GitHub search, issue creation, implementation change, or publication was
performed.

Verification used `cabal repl test:synarchy-test-headless` and
`Test.Headless.UI.ResponsiveMenus.Fixture`: each reproduction ran in a fresh
`withMenusEngine` / `newBareLuaBackend` through `evalOk`. This fixture isolates
configuration and saves before engine initialization. The real Lua modules
and Haskell UI backend ran with synthetic texture/font handles; texture
loading temporarily returned handle `1` while `dropdown.init()` or
`list.init()` initialized the widget family. No graphical window was opened.
The results establish input decisions and UI state, not rendered appearance.

### BUG-7. Escape stops dismissing dropdowns after their IDs exceed 100

**Verification:** Verified. The same Escape handler closes dropdown ID 1 but
skips an otherwise identical open dropdown with ID 101 and invokes the
settings menu's Back action instead.

The unfocused-dropdown branch in `uiManager.onUIEscape` searches the literal
range `1..100`. Dropdown IDs increase on every creation, and individual
destruction does not reset that counter. Settings rebuilds destroy their own
widgets individually and create replacements, so this limit counts historical
creations rather than simultaneously open controls. Repeated resizing or
menu reconstruction can therefore change Escape behavior during one session.

**Evidence:**

- `scripts/ui_manager_input.lua:123` — the dismissal loop is
  `for id = 1, 100 do`; missing that loop reaches `showMenu("back")` at
  line 141 when the current menu is settings or create-world.
- `scripts/ui/dropdown.lua:183` — `new` allocates and increments `nextId`.
  `destroy` at line 353 removes one entry without resetting the counter;
  the separate `destroyAll` operation is what resets it.
- `scripts/settings_menu.lua:260` — owned dropdowns are destroyed with
  individual `dropdown.destroy(id)` calls. The resize path at line 1078
  rebuilds the UI. `scripts/settings/graphics_tab.lua:224` is a live creation
  site for the resolution dropdown, alongside three other graphics controls.
- `scripts/ui/dropdown.lua:556` — opening the option list does not give its
  display box text focus, so opening via the arrow can reach this bounded
  search rather than the earlier focused-text-input branch.

**Reproduction:** In a fresh widget registry, create a one-option dropdown,
open it, and call the real `uiManager.onUIEscape`; confirm it closes. Destroy
it, then create/destroy IDs 2 through 100. Create ID 101, open its list without
text focus, and call the handler again. The test loaded the real
`scripts.ui_manager_input` onto a manager with `currentMenu = "settings"`,
replacing only `showMenu` with a counter and keybind capture with an inactive
stub. Observed output:

```json
{"firstClosed":true,"lastId":101,"lastStillOpen":true,"menuBackCalls":1}
```

**Handoff context:** Enumerate live dropdowns through their owning module
instead of guessing an ID range. Preserve focused-input Escape behavior and
the menu fallback when no widget handles the key. A regression should create
and destroy more than 100 controls before dismissing the surviving dropdown;
testing only freshly initialized IDs cannot expose this defect. The probe
intercepted navigation to observe it safely; it did not render a complete
settings-menu transition.

### BUG-8. Replacing list items leaves scrollbar position and visibility stale

**Verification:** Verified. Replacing the data of a scrolled list resets its
row offset to zero while its scrollbar retains offset 10; scrolling down
once jumps the rows to offset 11. Filtering to one item and restoring the
full list leaves all four queried scrollbar controls hidden.

`list.setItems` updates the rows and scrollbar through different state paths.
It resets `ls.scrollOffset`, but `scrollbar.setContentSize` only clamps the
scrollbar's existing offset and updates its geometry. The latter neither
resets its offset nor notifies the list. Separately, shrinking the list hides
the scrollbar, but expanding it again never re-shows it.

**Evidence:**

- `scripts/ui/list.lua:798` — `setItems` sets the list offset to zero, calls
  `scrollbar.setContentSize` when scrolling is needed, and calls
  `scrollbar.setVisible(..., false)` otherwise. There is no corresponding
  reset of the scrollbar offset or visibility restoration.
- `scripts/ui/scrollbar.lua:403` — `setContentSize` preserves an offset
  that remains in range and does not invoke `onScroll` or change visibility.
  `scrollDown` at line 317 increments this independent stored offset.
- `scripts/plant_panel.lua:246` — `refreshList` calls `list.setItems` for
  search and category filters; the sort button at line 375 also calls it.
  This is a live caller that can narrow and re-expand an existing list.

**Reproduction:** Initialize the real list/scrollbar modules, create a visible
list with 20 items and `maxVisible = 4`, and run these calls. `sbid` is the
list's scrollbar ID (1 in the fresh fixture):

```lua
list.setScrollOffset(id, 10)
list.setItems(id, items)
-- list.getScrollOffset(id) == 0; scrollbar.getScrollOffset(sbid) == 10
scrollbar.scrollDown(sbid)
-- list.getScrollOffset(id) == 11
list.setItems(id, {items[1]})
list.setItems(id, items)
-- UI.getElementInfo(h).visible is false for every handle returned by
-- scrollbar.getElementHandles(sbid): up, down, thumb, middle track.
```

Observed output:

```json
{"before":{"list":10,"scrollbar":10},"replaced":{"list":0,"scrollbar":10},"afterWheel":11,"scrollbarVisibleAfterFilterClear":[false,false,false,false]}
```

**Handoff context:** Update the list's row offset, scrollbar offset, content
size, and visibility consistently whenever items change. Cover a scrolled
sort/replacement and a scrollable → short → scrollable filter cycle. Respect
the containing widget's visibility when restoring scrollbar controls. The
reproduction intentionally uses a larger synthetic catalogue; the shipped
crop catalogue has only two entries, so the planting-panel manifestation
requires a viewport small enough to scroll those entries or additional crop
data. Loss of the scrollbar does not itself prove loss of wheel scrolling.

### BUG-9. Resizing the save browser discards its scroll position

**Verification:** Verified. With 30 saves at 1280×720 and the list scrolled to
offset 10, resizing to 1200×720 resets the offset to zero. Both dimensions are
inside the supported responsive envelope at UI scale 1.

The browser snapshots its selected value and keyboard control focus before a
resize, then destroys and recreates the list. It never snapshots or restores
the list's scroll offset. Browsing farther down a long save list is therefore
undone by an ordinary geometry change, contrary to the responsive lifecycle's
explicit scroll-preservation rule.

**Evidence:**

- `scripts/save_browser.lua:434` — `onFramebufferResize` preserves selection
  and focus around `createUI`, but makes no `getScrollOffset` or
  `setScrollOffset` call.
- `scripts/save_browser.lua:140` — `destroyOwned` destroys the previous
  list; `createUI` calls it before constructing a fresh list.
- `scripts/ui/list.lua:323` — new lists start at offset zero. The existing
  `getScrollOffset` and `setScrollOffset` APIs at lines 898 and 908 support
  restoration, including scrollbar clamping when geometry changes.
- `docs/engine_contracts.md:1409` — geometry rebuilds must preserve scroll
  offsets without re-firing selection callbacks.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:422` — existing coverage
  preserves selection and counts callbacks using a three-save fixture; it
  does not set or verify a nonzero scroll offset.

**Reproduction:** Initialize the real list widget family, then run:

```lua
local m = require("scripts.save_browser")
local list = require("scripts.ui.list")
local saves = {}
for i = 1, 30 do saves[i] = {name="save-"..i, timestamp="t"} end
local callbacks = 0
m.init(1, 2, 3, 1280, 720)
m.show(saves, function() callbacks = callbacks + 1 end, function() end)
list.setScrollOffset(m.listId, 10)
local before = list.getScrollOffset(m.listId)
m.onFramebufferResize(1200, 720)
return {before=before, after=list.getScrollOffset(m.listId), callbacks=callbacks}
```

Observed output: `{"before":10,"after":0,"callbacks":0}`.

**Handoff context:** Snapshot the offset before destroying the list and
restore it through the existing setter after reconstruction, clamping only
when the new viewport requires it. Preserve the existing selection and focus
restoration and zero extra load callbacks. Add a long-list resize case with
an explicitly nonzero offset; no save-file writing or actual load is needed.

## Survival mechanics: extended September 7, 2026 audit

These findings were verified at `ca55a6fbcd1ac5517a5145582daa7ec1881b8e54`.
Each reproduction used the real Lua module and shipped configuration through
the isolated `withMenusEngine` / `newBareLuaBackend` fixture. A minimal acolyte
was installed with `Test.Headless.Unit.TransferApi.mkUnit` and `minimalDef`;
stat reads and writes used the registered Haskell API and its real Float
storage. No unit simulation worker ran in this fixture. Where command
execution or an interleaving was simulated, that boundary is stated below.
Existing local reports were checked for equivalent concerns. No issues were
created, and no engine code or authored content was changed.

### BUG-10. Crawling restores hydration without a water source

**Verification:** Verified at the production resource-tick boundary.
Starting with hydration 10, maximum hydration 40, and endurance 1, one
0.1-second crawling tick raised hydration to `10.499047279358` while
`world.getFluidAt(0, 0)` returned no water. No container was consumed.

The hydration resource treats the Crawling pose itself as a water supply.
That pose is also used by incapacitated units and the sleep transition chain,
so the resource code credits drinking for unrelated behavior.

**Evidence:**

- `scripts/unit_resource_config.lua:71` — acolyte hydration declares
  `regen_factor_crawling = 5.0`, with the assumption that crawling means
  being at a water source.
- `scripts/unit_resource_tick.lua:70` — pose selection applies that factor
  to any crawling unit. No source, drinking action, fluid kind, location,
  or inventory check occurs before the hydration write.
- `scripts/unit_resource_injury.lua:129` — a conscious unit that cannot
  walk remains crawling because of its injuries.
- `scripts/unit_ai_sleep.lua:215` — falling asleep and waking pass through
  Crawling for reasons unrelated to drinking.
- `scripts/unit_resources.lua:130` — the per-resource loop passes the
  unit's current pose into this same tick.

**Reproduction:** Set the stats above on a fixture unit, then call
`tick.tickResource(1, "acolyte", "hydration", config.acolyte.hydration,
"idle", "crawling", 0.1)` using the real `unit_resource_tick` and
`unit_resource_config` modules. The fixture has no generated water or held
items. Read back hydration through `unit.getStat`.

**Handoff context:** Make hydration gain depend on an actual eligible
drinking operation, with Crawling serving only as its pose requirement if
needed. Cover injured crawling and both sleep transitions on dry land, plus
a positive source-drinking control. This probe establishes the unconditional
gain, not how much an entire sleep animation would award in a live session.

### BUG-11. Organ-failure stamina drain uses the wrong body-size floor

**Verification:** Verified with real stat storage. The stamina path both
starts organ failure above the intended fat floor for a smaller frame and
fails to start it at that floor for a larger frame.

Body initialization and starvation use `0.02 * frame_mass`, where frame mass
includes the unit's rolled bulk. The stamina resource still tests the older
`0.44 * height * height` formula, which only matches when bulk is exactly 1.
Shipped acolytes roll bulk around 1 with range 1, so this is not restricted to
custom species data.

**Evidence:**

- `src/Unit/Thread/Command/Body.hs:95` — initialization computes frame mass
  as `22 * height * height * bulk`, seeds a frame-proportional minimum fat,
  and retains `frame_mass` as the stable viability reference.
- `scripts/unit_resource_energy.lua:168` — starvation uses
  `0.02 * frame_mass`, falling back to the height-only formula only for
  older units without that stat.
- `scripts/unit_resource_tick.lua:58` — organ failure ignores `frame_mass`
  and always compares fat against `0.44 * height * height` plus tolerance.
- `data/units/acolyte.yaml:41` — authored bulk is variable.

**Reproduction:** With height 2, endurance 1, stamina 5, caffeine 0, and
the shipped acolyte stamina configuration, run a 0.1-second idle tick:

| Frame mass | Fat mass | Correct floor | Observed stamina | Consequence |
| --- | --- | --- | --- | --- |
| 44 | 1.2 | 0.88 | 4.9499998 | Organ-failure drain starts despite remaining reserves |
| 132 | 2.64 | 2.64 | 5.0500002 | Ordinary recovery continues at the exhausted-reserve floor |

The height-only threshold is 1.76 in both cases. Positive calories were set
to isolate this decision from catabolism.

**Handoff context:** Share the current frame-based floor and legacy fallback
between both death paths. Preserve the Float-rounding tolerance already
provided for fat. Cover bulk below and above 1, exact exhaustion, and legacy
units without `frame_mass`. This does not imply that a large-frame unit can
never die; dehydration, injury, or lean-tissue loss remain other death paths.

### BUG-12. Float rounding can prevent the starvation death threshold from firing

**Verification:** Verified through real Float stat writes. At frame mass
100.1, 100 consecutive starvation ticks of 10 seconds each left lean mass at
its clamped floor without ever requesting death. An exactly representable
floor control requested death immediately.

Starvation clamps lean mass to a Lua-computed minimum, stores it as a Haskell
Float, then compares the widened stored value against the unrounded Lua
minimum on the next tick. If the store rounds upward, `lean <= minLean` is
permanently false. Further catabolism repeatedly writes the same rounded
floor, so time does not resolve the discrepancy.

**Evidence:**

- `scripts/unit_resource_energy.lua:170` — `minLean = 0.20 * frame_mass`.
  The respiratory-failure guard at line 174 uses a strict numeric floor
  comparison without tolerance.
- `scripts/unit_resource_energy.lua:200` — catabolism clamps lean to that
  minimum and writes it back through `unit.setStat`.
- `src/Engine/Scripting/Lua/API/Units/Stats.hs:352` — stat assignment
  converts Lua's number to the Float stored in `uiStats`.
- `scripts/unit_resource_energy.lua:45` — the same module already explains
  this exact Float32/Float64 hazard for the fat floor and supplies
  `FAT_FLOOR_TOL`; the lean death guard has no corresponding protection.

**Reproduction:** Set frame mass to 100.1, read it back, and set lean to
`0.2 * unit.getStat(1, "frame_mass")`. Set calories to zero, metabolism rate
to 1, fat to its frame-based minimum, height to 2, and body mass to 40.
Replace only `unit.kill` with a request counter, then call the real
`energy.tickStarvation(1, 10)` 100 times. Observed:

```json
{"floor":20.019999694824,"lean":20.020000457764,"killCalls":0}
```

Setting frame mass 70 and lean mass 14, then ticking once, increased the same
kill counter to 1. This demonstrates the missing threshold decision; the
fixture intentionally did not execute queued death commands.

**Handoff context:** Make the lean death decision stable across storage
rounding, using a justified tolerance or comparison in one numeric domain.
Test upward- and downward-rounded floors with real stored values, an exact
floor, and a clearly above-floor living control. Other resource death paths
may mask this fault in a full simulation; they do not repair it.

### BUG-13. Autonomous drinking credits water that was not drained

**Verification:** Verified with the real autonomous action and real hydration
storage, injecting the inventory/drain boundary. A nil, zero, or short drain
all received the same full hydration credit as a successful 0.5-litre sip.

The canteen action calculates its sip from an inventory snapshot, calls the
exact-instance drain, and ignores that call's authoritative result. It then
adds hydration for the requested amount and queues the drink animation. The
player coffee mechanism was repaired for this class of error, but the
independent autonomous water action retains it.

**Evidence:**

- `scripts/unit_ai_needs.lua:121` — `drinkExecute` selects a canteen from
  `unit.getInventory` and computes the requested sip from that snapshot.
- `scripts/unit_ai_needs.lua:153` — `unit.modifyItemFillById` is called
  without retaining its result, followed by the full hydration write and
  `unit.drink`.
- `src/Engine/Scripting/Lua/API/Units/Equipment.hs:64` — the drain returns
  the signed amount actually removed, or nil for a vanished endpoint.
  `adjustFillById` at line 305 clamps against current fill atomically.
- `scripts/consumable.lua:161` — the coffee mechanism already bases effects
  on the actual drain and refuses nil/zero results.
- `tools/canteen_instance_probe.py:105` — the existing water-drinking case
  checks which of two instances was drained; it does not change the selected
  instance between snapshot and mutation or inject a refused/short result.

**Reproduction:** Use the shipped acolyte tunables, starting hydration 10 and
maximum 40, and an inventory snapshot containing one full canteen. Intercept
only inventory reading, the drain's returned value, and animation requests;
run `needs.drinkExecute(1, {}, params)` once per case:

| Drain result | Water supplied | Expected hydration | Observed hydration | Drink requests |
| --- | --- | --- | --- | --- |
| nil | none | 10 | 15.5 | 1 |
| 0 | none | 10 | 15.5 | 1 |
| -0.1 | 0.1 L | 11.1 | 15.5 | 1 |
| -0.5 | 0.5 L | 15.5 | 15.5 | 1 |

**Handoff context:** Credit the positive magnitude of the actual negative
drain, refusing missing/zero drains before effects or animation. Preserve
exact-instance targeting, sip limits, and deficit clamping. This is distinct
from the already-recorded coffee finding in `project_review_1642-1631.md` and
its fix: that code path is now correct. The injected cases establish response
handling, not a measured frequency of live inventory races.

### BUG-14. Injury recovery interrupts source drinking and leaves its action locked

**Verification:** Verified by composing the real source-drinking and injury
modules under a controlled command-execution schedule. A healthy unit reaches
the `drinking` phase, injury recovery stands it up, and 50 further action
executions leave it standing in `drinking` with infinite utility.

The injury recovery code treats any healthy crawling unit as ready to stand.
It makes an explicit exception for the sleep transition chain but omits the
equally deliberate source-drinking chain. Once a drinker's pose becomes
standing, its hydration regeneration stops; its `drinking` phase only checks
for a nearly full hydration meter and never repairs the pose or abandons the
phase. The action therefore remains locked while thirst increases.

**Evidence:**

- `scripts/unit_ai_water.lua:208` — observing Crawling in the descending
  phase advances to `sourcePhase = "drinking"`.
- `scripts/unit_resource_injury.lua:144` — healthy Crawling invokes
  `unit.revive` unless `s.sleepPhase` exists; `s.sourcePhase` is ignored.
- `src/Unit/Thread/Command/Pose.hs:159` — the revive handler really snaps
  a crawling unit to Standing.
- `scripts/unit_ai_water.lua:224` — the drinking branch waits for hydration
  to reach 95%, without checking that the unit remains in its drinking pose.
- `scripts/unit_ai_water.lua:187` — any non-nil `sourcePhase` earns
  `math.huge` utility, preventing ordinary action pre-emption.

**Reproduction:** Give a healthy fixture unit hydration 10/40 and AI state
`{sourcePhase="descending", knownWaterSources={{x=1,y=0}}}`. Start its observed
pose at Crawling. Execute `drinkFromSourceExecute`, then `tickInjuries`, then
the drinking action 50 times. The fixture replaces `unit.revive` with the
handler's immediate Standing result and counts requests; no real unit worker
or water-finding traversal runs. Result:

```json
{"phaseBefore":"drinking","phaseAfter":"drinking","revives":1,"locked":true,"pose":"standing"}
```

As a control, giving that same healthy crawler a `sleepPhase` caused injury
recovery to issue zero revive requests and leave it crawling.

**Handoff context:** Reconcile deliberate pose ownership across injury
recovery, drinking, and sleep. Also make a source-drinking action robust to
losing its pose after admission. Cover the composed injury/resource/AI path,
including losing water or being interrupted, rather than only testing each
transition helper alone. The exact live symptom depends on scheduling:
injury recovery may also interrupt the descent before the AI observes it.

### BUG-15. Small resource changes are discarded forever instead of accumulated

**Verification:** Verified with the real resource tick, shipped squirrel
sleep-pressure configuration, and real Float storage. Equal simulated time
produced almost total depletion with one-second steps and no depletion at all
with the routine 0.1-second step.

The resource tick skips any write whose absolute change is at most `1e-4`.
It retains no fractional remainder, so that is a minimum per-tick rate, not
a batching optimization. Small but valid drains can therefore disappear
forever. A sufficiently small configured or overridden sleep-pressure pool
exposes this at ordinary cadence.

**Evidence:**

- `scripts/unit_resource_tick.lua:153` — only writes when
  `math.abs(next - current) > 1e-4`; skipped changes have no accumulator.
- `scripts/init_loader.lua:107` — registers the resource script at a
  0.1-second interval.
- `scripts/unit_stats.lua:106` — maximum sleep pressure is endurance × 10.
  `scripts/unit_resource_config.lua` gives the squirrel a drain fraction of
  `1 / 3600` per second.
- `data/units/red_squirrel.yaml:21` and `src/Unit/Stats.hs:145` — ordinary
  squirrel rolls clamp endurance to 0.45–0.75, so they do NOT reach this
  reproduction's 0.3 on spawn. It is a valid positive stat override; current
  default species rolls at the default tick rate were not shown to freeze.
- `tools/circadian_probe.py:151` — current drain coverage spawns one acolyte;
  it does not pin a low maximum or compare equal-duration step schedules.

**Reproduction:** Set endurance 0.3 and sleep pressure 3 on the fixture unit.
Call `tickResource` with `config.red_squirrel.sleep_pressure`, idle activity,
and standing pose 36,000 times at `dt = 0.1`. Reset pressure to 3 and call it
3,600 times at `dt = 1`. Both schedules represent 3,600 seconds:

```json
{"fineSteps":3.0,"coarseSteps":0.000002843664105967,"gameSeconds":3600}
```

**Handoff context:** Preserve accumulated changes independently of update
partitioning, or remove the lossy threshold with appropriate storage handling.
Test small real resource pools and equal-duration schedules, including
threshold/death decisions based on values actually committed. This is a
separate defect from the recorded stamina read/modify/write race: it occurs
with one writer and no interleaving at all. Treat its present gameplay impact
as conditional on a smaller configured/overridden pool or a faster resource
tick, not as a demonstrated defect in default squirrel spawning.


## Connectivity and input dispatch: extended September 7, 2026 audit

These findings were checked against `ca55a6fbcd1ac5517a5145582daa7ec1881b8e54`
and rechecked against `167e3e88f093eaa2b9919d482cf2339b2b60b9b7`; their
implementation files did not change between those revisions. Reproductions
use the existing headless test component's REPL and real production helpers.
No implementation or test files were edited.

### BUG-16. Electrical networks disconnect at the cylindrical seam

**Verification:** A two-tile wire run crossing the cylindrical seam becomes
two electrical networks, leaving its attached battery uncharged. A comparable
run away from the seam charges the battery. All 46 existing
`Test.Headless.Power.Network` examples passed in the same REPL.

Wire placement/autotiling recognizes canonical neighbors across the seam,
but power connectivity searches ordinary `(x ± 1, y)` / `(x, y ± 1)` keys.
Across the seam, adjacent tiles have distant canonical coordinates. Both wire
flood-fill and attachment of power nodes/consumers consequently miss these
neighbors. The electrical topology disagrees with the placement topology.

**Evidence:**

- `src/Power/Network.hs:116` — `neighborsOf` uses plain cardinal offsets;
  `wireComponents` has no world-size argument and performs no canonicalization.
- `src/Power/Network.hs:145` — `touchedComponents` uses the same neighbor
  helper for attaching nodes and consumers.
- `src/Power/Network.hs:316` and `:337` — snapshots and charging feed the
  wire set directly into that topology. Their world-size parameter is used
  downstream for solar phasing, not seam-aware connectivity.
- `src/World/Generate/Coordinates.hs:87` — canonical tile coordinates and
  `localizeTileToAnchor` establish that the reproduction's two distant keys
  really are cardinal neighbors.
- `src/Engine/Scripting/Lua/API/StructureArt.hs:466` and
  `scripts/wire.lua:61` — the placement-neighbor API is explicitly seam-aware.
- `test-headless/Test/Headless/Power/Network.hs` — tests solar phasing for
  seam aliases, but does not place adjacent wire tiles across the seam.

**Reproduction:** In a world of size 64, define:

```haskell
wa = canonicalTile 64 511 0  -- (511,0)
wb = canonicalTile 64 512 0  -- (0,512)
```

`localizeTileToAnchor 64 wa wb` returns `(512,0)`, one tile east of `wa`,
but `length (wireComponents (HS.fromList [wa,wb]))` returns `2`. Attach a
100 W source at `canonicalTile 64 510 0` and an empty 100 Wh battery at
`canonicalTile 64 513 0`. Advance `tickPowerNodes` by 3,600 game seconds with
global sun angle 0, no drains, and those two wires. Stored charge remains
`0.0 Wh`. The control with wires `(0,0),(1,0)`, source `(-1,0)`, battery
`(2,0)`, and global sun angle 0.5 stores approximately `100 Wh`.
These are pure topology/charging calls; rendered wire pixels were not tested.

**Handoff context:** Apply the shared cylindrical coordinate rules to both
wire adjacency and node/consumer attachment. Cover each of those boundaries,
including a single wire touching a node across the seam, independently of the
already passing solar-phase and chunk-residency tests.

### BUG-17. Focus cleanup discards typed dropdown choices before submission

**Verification:** The real dropdown, button, and UI-manager Lua handlers
produce different results for a direct outside-click callback and the actual
focus-loss-first dispatch order. The latter silently restores the old value;
an Apply-style button's callback likewise receives the old selection.

Editable dropdowns implement commit-on-outside-click in `onClickOutside`.
However, the input router queues focus loss before an outside mouse event,
and UI-manager focus cleanup calls `dropdown.unfocusAll`. Unfocusing replaces
the raw edit with the previously selected text. The later outside-click
handler then sees no focused dropdown to submit. Clicking another UI control
has the same problem through `handleNonTextBoxClick`.

**Evidence:**

- `scripts/ui/dropdown.lua:420` — `unfocus` resets the input text from
  `selectedIndex`, discarding the in-progress edit.
- `scripts/ui/dropdown.lua:512` and `:1082` — submission matches the raw
  edit and selects its value; clicking outside is meant to invoke submission.
- `src/Engine/Input/Thread/Mouse.hs:300` — a left-click miss queues
  `LuaUIFocusLost` before `LuaMouseDownEvent`.
- `scripts/ui_manager_input.lua:152` — the focus-lost handler unfocuses
  dropdowns immediately.
- `scripts/ui_manager_widgets.lua:27` and `:157` — other-control cleanup
  runs before a button's callback and discards the dropdown edit.
- `scripts/settings/graphics_tab.lua:224` — the editable resolution
  dropdown updates pending width/height only through `onChange`; typing a
  matching resolution and clicking Apply therefore does not commit it.

**Reproduction:** Create a real dropdown with two options, `1280x720` and
`1920x1080`, initially select the former, focus it, and set its raw edit to
the latter. Count `onChange` calls. In separate reset runs:

```json
{
  "directOutside": {"changed": 1, "value": "new"},
  "routedOutside": {"changed": 0, "value": "old"},
  "apply": {"changed": 0, "value": "old"}
}
```

`directOutside` calls `dropdown.onClickOutside(500,500)`;
`routedOutside` calls `manager.onUIFocusLost()` first, matching the queued
order; `apply` calls `manager.onButtonClick` with a real button handle whose
callback records the selected value. Each run uses the real UI text-input
storage in the isolated headless fixture. Texture loading returns a synthetic
handle; no rendered or GLFW pointer-event test was performed.

**Handoff context:** Distinguish committing a valid edit on blur from
cancelling it on Escape, and ensure commit happens before a dependent button
callback reads pending values. Cover routed empty-space clicks, other widget
clicks, Apply/Save, and explicit Escape cancellation. A test that calls
`onClickOutside` directly cannot detect this event-ordering defect.


## Body composition: extended September 7, 2026 audit

### BUG-18. Routine body-regrowth ticks lose muscle growth and desynchronize total mass

**Verification:** With ordinary acolyte-sized body stats, the real regrowth
helper at its routine 0.1-second cadence spends calories but produces no idle
muscle growth and no total-weight gain. Walking regrowth adds muscle while
its intended fat burn and total-mass update both disappear. A one-second
step control produces different body composition over the same elapsed time.

Each tick reads the Float32 body stats into Lua, adds tiny increments, and
immediately stores each result back to Float32. At normal human body sizes,
the intended idle lean increment and total-mass increment are below half a
storage step. They round back to the old value on every tick; there is no
retained remainder. Fat, lean, total mass, and the calorie bill round
independently, so the stored changes also fail to conserve composition.

**Evidence:**

- `scripts/unit_resource_energy.lua:75` — `applyRegrowth` computes separate
  fat/lean/body increments and writes all three every tick. The idle split
  is 90% fat / 10% lean from 0.1 kcal per second; walking also burns fat.
- `scripts/unit_resource_tick.lua:205` — a live calorie store above 75%
  invokes regrowth with the resource tick's `dt`.
- `scripts/init_loader.lua:107` — that routine interval is 0.1 seconds.
- `src/Unit/Types/Instance.hs:67` and
  `src/Engine/Scripting/Lua/API/Units/Stats.hs:343` — these stats are stored
  as `Float`, and the public setter writes through that representation.
- `src/Unit/Thread/Command/Body.hs:159` — recomputation derives strength,
  pool maxima, and carrying capacity from the stored composition; it does
  not reconcile total mass with independently rounded fat/lean deltas.
- `tools/physiology_probe.py:395` — its calorie-drain measurement explicitly
  moves below the surplus-regrowth band. It does not assert regrowth
  accumulation at the production cadence.

**Reproduction:** Seed height 1.8 m, body/frame mass 71.28 kg, lean mass
28.512 kg, fat mass 14.256 kg, strength base 1, and calories 1,400. Call the
real `applyRegrowth` 10,000 times at 0.1 seconds. Reset, then repeat 1,000
times at one second. Repeat the fine-step case with walking activity:

| 1,000 seconds of regrowth | Body mass (kg) | Lean mass (kg) | Fat mass (kg) | Calories |
|---|---:|---:|---:|---:|
| Initial stored values | 71.279999 | 28.511999 | 14.256000 | 1400.000000 |
| Idle, 0.1 s steps | 71.279999 | 28.511999 | 14.265536 | 1299.902344 |
| Idle, 1 s steps | 71.295258 | 28.517721 | 14.267444 | 1300.024414 |
| Walking, 0.1 s steps | 71.279999 | 28.550146 | 14.256000 | 1299.902344 |

Without storage loss, idle growth over that duration adds approximately
0.011688 kg fat and 0.005556 kg lean, with their sum added to total mass.
The isolated fixture uses real stat setters and real body recomputation;
metabolism, digestion, and other physiology updates are not interleaved.
The duration therefore measures the regrowth mechanism, not an unattended
1,000-second whole-game trajectory.

**Handoff context:** Preserve sub-Float increments and a coherent accounting
of tissue/total mass and energy. Cover ordinary default-scale bodies at the
actual tick cadence, with tolerances that cannot accept completely missing
growth or exercise burn. This is distinct from BUG-15: these writes are
unconditional, so removing `tickResource`'s explicit `1e-4` guard would not
repair this defect.


## Medical treatment and recovery: September 7, 2026 continuation

Verified against `289d454b12c0ec2e0db1e72aa4073b0462743a59`, using the
existing headless component's REPL. A final source comparison against
`53e448ac9af8f63723787cf379a3d013abecfc92` found no intervening changes
to implementation, test, tool or data files. The live API
fixtures reuse `Test.Headless.Unit.MedicalKitInstance` inside the isolated
`withMenusEngine` harness. No unit/combat simulation worker or graphical
window runs; treatment APIs, item consumption, wound storage, and Lua
revival decisions are real. Wound progression is called explicitly through
its production pure helper.

**Coverage and existing reports:** The wound group passed 16 examples and
medical-kit instance coverage passed 11 examples. `MedicalReach` passed 29
of 30 examples in the REPL: its concurrent moving-patient test failed only
because all 300 attempts were refused, leaving the required successful-case
sample empty. The earlier state/transaction invariants passed. The same
example passed the canonical compiled targeted Cabal run and five further
compiled runs. This is a qualified observation about race-test sampling,
not a confirmed treatment transaction failure. No production fix is proposed
for that observation without a controlled reproduction.

`docs/expedition_survival_calibration.md` SURV-10 already records missing
end-to-end stabilization observations. The two findings below establish
specific current implementation defects beyond that broader coverage gap;
SURV-9's owner-approved above-collapse locomotion policy is unaffected.

### BUG-19. Treatment aliases separate wounds inflicted at the same game time

**Verification:** One real antibiotic treatment changed two wounds after
consuming one dose. Worse, the less-infected wound increased from 0.1 to 0.4
because treatment assigned it the result computed for the more-infected
wound. A control changing only the second wound's timestamp treated just the
intended first wound.

The treatment code selects one worst wound, but identifies it for mutation
by `(part, kind, woundAt)`. Those fields do not uniquely identify a wound:
combat stamps wounds with the current shared game time and appends them to
the existing list. Separate attacks resolved between game-clock updates can
inflict the same kind of wound on the same part with the same timestamp.
The treatment mutators update EVERY matching entry, not just the selected
wound. Both dressing/cleaning and infection treatment share this key.

**Evidence:**

- `src/Combat/Resolution.hs:420` — `mkWound` stamps `woundAt = gt`;
  the following commit appends the new wounds without assigning a unique
  treatment identity or merging existing same-key wounds.
- `src/Combat/Resolution.hs:177` and `src/Combat/Thread.hs:156` — attacks
  read the shared game clock while a queue-draining loop can resolve several
  attacks before that clock changes. The timestamp is not a sequence number.
- `src/Engine/Scripting/Lua/API/Units/Medical.hs:111` and `:533` — each
  treatment selects one wound and constructs the three-field key.
- The same file's `setWoundDressing` at line 340, `setWoundClean` at
  line 399, and `setWoundInfection` at line 427 map over ALL matching wounds.
- Infection treatment at line 549 computes a single absolute `newInf` from
  the selected wound, then copies it to all matches after spending one dose.
- `test-headless/Test/Headless/Unit/MedicalKitInstance.hs:185` — its clinical
  fixture has one wound, so passing supply-instance tests do not cover
  collisions in the patient's wound identity.

**Reproduction:** Reuse the medical-kit fixture's medic, stocked second kit,
and adjacent patient. Give the patient two torso slash wounds at timestamp
0, with infection 0.9 and 0.1 and inflicted severity 0.5 and 0.2. Set the
medic's intelligence to 0.5; it already knows infection control at 100.
Call the real `unit.treatInfection(1,2)`:

| Case | First infection after | Second infection after | Antibiotic doses spent |
|---|---:|---:|---:|
| Both wound timestamps 0 | 0.399999976 | 0.399999976 | 1 |
| Second timestamp 0.01 | 0.399999976 | 0.100000001 | 1 |

The same-key case marks both wounds clean. The distinct-time control leaves
the second wound dirty. The stocked antibiotic item's fill fell from 5 to 4.
These are real treatment commits against seeded wound lists; an actual
multi-attacker fight was not simulated. Production collision reachability
is established by the combat stamping and append path above.

**Handoff context:** Give a selected wound unambiguous mutation identity,
including across intervening healing/list changes. Do not fix this merely by
adding severity to the key: severity and the other mutable clinical fields
are neither identities nor guaranteed unique. Cover same-time same-part
same-kind wounds for both treatment verbs, including different infection
types and a less-infected non-target whose infection must never increase.
If the chosen solution changes persisted wound records, follow the save
schema/migration contract.

### BUG-20. Healing does not restore blood needed for revival

**Verification:** After the real wound tick removed a patient's final healed
wound, 10,000 ticks left blood at 1 L of a 5.25 L maximum. The real revival
helper requested zero revives in 100 calls despite no wounds and no active
bleeding. Changing only blood to 4.9 L made all 100 calls request revival.

The revival gate requires at least 50% of maximum blood after a hemorrhagic
collapse. Its comment says wound closure refills blood indirectly and that
healing or first aid allows recovery. The actual implementation never
replenishes blood: wound progression subtracts loss, wound-free progression
returns without touching blood, and treatment alters dressings/infection
without restoring volume. Thus successful medical stabilization cannot
complete the promised recovery. This is a missing recovery mechanism, not a
request to remove the anti-flapping blood gate.

**Evidence:**

- `scripts/unit_resource_tick.lua:258` — documents the bleeding-collapse
  hysteresis and asserts wound closure will permit recovery; the gate at
  line 271 refuses revival below 50% of maximum blood.
- `src/Combat/Wounds/Tick.hs:174` — a wound-free unit only decays immunity;
  it leaves `uiBlood` untouched.
- `src/Combat/Wounds/Tick.hs:401` and `:474` — the wounded path computes
  `newBlood = uiBlood inst - totalDrain` and stores its nonnegative clamp.
  Healing/removing the last wound does not add blood.
- `src/Engine/Scripting/Lua/API/Units/Medical.hs:167` and `:550` — bleeding
  and infection treatment change supplies, wound state, knowledge and immune
  response, with no volume-recovery effect.
- `src/Unit/Types/Wound.hs:134` — maximum blood is body mass × 0.075.
  Repository-wide `uiBlood` writer inspection found spawning and save
  restoration, but no live healing, feeding, drinking, or transfusion writer
  that replenishes the field.

**Reproduction:** Seed a 70 kg collapsed patient with blood 1 L and one
clean, fully clotted, dressed torso slash: severity 0.5, heal 0.999,
infection 0. Call `Combat.Wounds.tickOneUnit` 10,000 times with `dt = 0.1`;
the first call removes the healed wound. The final state has zero wounds
and blood still 1 L. Install that result in the medical fixture and call
`unit_resource_tick.checkRevive(2,{})` 100 times, counting `unit.revive`
requests. The empty resource configuration intentionally isolates the blood
gate from other resource thresholds:

```json
{"revives":0,"blood":{"max":5.25,"current":1.0,"bleedRate":0.0},"wounds":0}
```

Changing only blood to 4.9 L produces 100 revive requests. The fixture does
not execute those requests, and it omits other physiology: a full simulation
can deteriorate further through hypoxia or other survival conditions instead
of leaving the patient indefinitely collapsed.

**Handoff context:** Define and implement the intended post-stabilization
blood-volume recovery, including its nutrition/rest/treatment prerequisites
and time scale, while retaining collapse/revival hysteresis. Test the whole
bleed → stabilize → wound closure → blood recovery → revival sequence, plus
continued bleeding and a true exsanguination control. A healing test that
asserts only wound removal misses the blocked recovery step.


## Repair ownership: September 7, 2026 continuation

### BUG-21. Ground-repair return retries never end after the item leaves its worker

**Verification:** After a real pickup and a real transfer of the target to
another unit, aborting its repair entered `returning`. One hundred further
executions retained that phase, utility 6, and the original worker's claim,
although the worker no longer held the item and the other unit did.

Ground-sourced repair jobs must drop their borrowed target when they finish
or abort. The new retry path correctly preserves a still-held item when its
page temporarily cannot accept a drop. It treats EVERY failed drop as that
same retryable condition, however. If the item was transferred, dropped by
another action, or otherwise removed from this inventory, retrying cannot
succeed. The job never checks ownership again, retains its repair utility,
and refreshes its claim on every execution.

**Evidence:**

- `scripts/unit_ai_repair.lua:139` — abort of a fetched ground target calls
  `returnGroundTarget`; any false result preserves the job in `returning`.
- `scripts/unit_ai_repair_target.lua:231` — the return helper only checks
  whether `unit.dropItemById` returned true; it cannot distinguish a missing
  item from a temporarily unavailable destination.
- `scripts/unit_ai_repair.lua:181` — an existing repair job receives the
  configured lock utility before candidate eligibility is considered.
- `scripts/unit_ai_repair.lua:302` — every execution refreshes the claim;
  the `returning` branch at line 308 retries the drop and otherwise returns
  without checking whether the worker still owns the item.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:181` — an exact-instance
  unit transfer really removes the item from this worker's inventory.
  The repair obligation is separate Lua state, not part of that commit.
- `test-headless/Test/Headless/Lua/UnitAiRepairGround.hs:780` — current
  failure coverage removes and restores the page while the worker continues
  holding the target. It does not remove or transfer the target itself.

**Reproduction:** Reuse that fixture with one worker holding lignite, a
broken ground axe (instance 735) one tile away, and a second live same-page
unit. Drive the real repair scoring/claim and pickup, reaching
`fetch_consumable`. Transfer the axe through
`unit.transferItemToUnit(1,2,"axe_steel",735)`; it returns true. Call
`RP.abort(1,s,unit.getInfo(1))`, then `RP.execute(1,s,PARAMS)` 100 times:

```json
{"moved":true,"phase":"returning","utility":6.0,"claimant":1}
```

Real inventory reads show only lignite on the original worker and axe 735
on the second unit. The reproduction reuses the existing fixture's station
and pace stubs, but no pickup, transfer, inventory-read, or drop result is
stubbed. No actual repair operation is needed; it exercises abort cleanup. The
existing ground-repair group passed all 24 examples in the same REPL.

**Handoff context:** Reconcile the return obligation against current item
ownership. Keep retrying while the worker still holds the item and the drop
is temporarily impossible; release stale work when the item has legitimately
left. Preserve the exact-instance and own-page return policies. Cover player
transfer/drop during repair, missing targets, and temporary page loss. The
stale job is not an infinite-utility lock: higher-priority survival/combat can
still preempt it, but ordinary work stays displaced whenever repair resumes.


## Building work eligibility: September 7, 2026 continuation

### BUG-22. Dead and collapsed builders continue producing building progress

**Verification:** The real construction update and real building-progress
API added `10.000001907349` worker-seconds in each of three cases: the sole
cached builder was Dead, Collapsed, or Standing. All three were counted as
one adjacent worker after the real AI suspension helper had run.

Worker-built buildings gain progress from a census of adjacent units whose
cached AI action is `build_nearby`. That census does not check pose or
activity. The AI stops executing dead/collapsed units but preserves their
cached action and building target. Consequently a corpse or unconscious
worker still contributes normal construction work; several such workers
also receive the construction rate's coordination multiplier.

**Evidence:**

- `scripts/unit_ai_core.lua:313` — `countAdjacentBuilders` checks cached
  action, target and geometric adjacency, without checking whether the unit
  is alive, standing or otherwise able to work.
- `src/Engine/Scripting/Lua/API/Units/List.hs:46` — active-unit enumeration
  filters by page, not by living pose, so retained corpses remain enumerable.
- `scripts/unit_ai.lua:280` — Dead/Collapsed takes the `suspendOrders`
  early return without action reselection or an outgoing action callback.
- `scripts/unit_ai_stall.lua:230` — suspension resets order/work clocks and
  craft/structure-construction phases, but does not clear `currentAction`
  or `buildTarget` used by this different construction system.
- `scripts/building_spawn.lua:626` and `:639` — the worker census feeds
  `workerRate(n) * dt` into building progress; one worker yields 1×, two 4×,
  and three 9×.
- `src/Engine/Scripting/Lua/API/Buildings/Progress.hs:172` — the progress
  commit receives only building id and delta, so there is no later worker
  eligibility check that can correct the false census.
- `src/Unit/Thread/Command/Pose.hs:107` — killing a unit stamps Dead while
  retaining its instance; this is not the separate destroy/remove operation.

**Reproduction:** Install the existing medical fixture's unit at `(10,10)`
and a same-page 1×1 building at `(11,10)` with required work 100, progress 0,
and no material demands. The building uses `Building.Placement`'s ordinary
fixture definition with its work requirement changed. Set AI state to
`{currentAction="build_nearby", buildTarget=1}` and call
`core.suspendOrders(1)`. For each pose, reset the fixture and call the real
`building_spawn.update(0.1)` 100 times:

| Unit pose | Counted workers | Committed building progress |
|---|---:|---:|
| Dead | 1 | 10.000001907349 |
| Collapsed | 1 | 10.000001907349 |
| Standing | 1 | 10.000001907349 |

Unit queries, building activity/material checks, and progress writes are
real Haskell APIs. The pause facade returns false to permit explicit
updates; no simulation worker moves or revives the fixture unit. This
verifies stored progress, not construction rendering. The existing five
building-spawn sentinel examples also passed; those cover portal rejection
and roster handling, not worker eligibility.

**Handoff context:** Count only currently eligible workers and use the same
eligibility for saturation/recruitment (`countBuildersAt` in
`unit_ai_logistics.lua`) so an incapacitated reservation cannot falsely fill
a site either. Cover death and collapse after work begins, recovery, mixed
healthy/incapacitated teams, and unrelated action preemption. Keep this
worker-built-building path distinct from `constructJob`'s structure-placement
clock, whose suspension cleanup already exists.


## Medic selection and supplies: further September 7, 2026 audit

Verified against `53e448ac9af8f63723787cf379a3d013abecfc92`. These cases
use the real `unit_ai_medic` module and live unit, wound, inventory,
knowledge, faction and page projections. The headless medical-kit fixture
supplies the world/item definitions and pinned treatment generator. Each
case replaces its unit roster with the explicitly described three units.
The fixture's movement-speed facade returns 1; simulation workers are not
running. Scoring and explicit execution are driven from the component REPL.
No movement completion, rendered scene or whole-game survival outcome is
inferred from these fixtures.

**Validation:** The canonical targeted run
`cabal test synarchy-test-headless --test-options='--match "AI page pairing"'`
passed all 30 examples. Its medic cases test page boundaries, providing
positive controls for the shared discovery code while leaving the specific
same-page conditions below uncovered. Source freshness was checked again at
`cfd30002dde1901f91ad7db251a07f1e01889330`; the only intervening source
change was an unrelated worker-shutdown comment. No full suite was run.

### BUG-23. Dead and collapsed medics retain claims that block replacement treatment

**Verification:** A dead or collapsed medic's retained claim made a nearby
healthy medic return utility `-inf` for a still-wounded ally. Calling the
real AI suspension helper preserved the blocking claim. Clearing only that
claim changed the healthy medic's utility to 8 and nominated the patient.

The claim filter promises a LIVE, AVAILABLE claimant but only checks its
continued existence, matching page and lack of a combat action. Death
retains the unit instance, and collapse/death suspend the AI without clearing
`treatClaim`. Consequently the unavailable owner never reaches the executor
that would release its reservation, while other medics refuse to intervene.
This differs from BUG-22's builder census: it is a treatment reservation
that suppresses another unit's work, requiring its own eligibility check.

**Evidence:**

- `scripts/unit_ai_medic.lua:174` — `patientClaimed` accepts an existing
  same-page claimant without calling `canActAsMedic` or checking pose.
- `scripts/unit_ai_medic.lua:50` and `:133` — the new-medic ranking DOES
  exclude dead/collapsed units. That protection comes too late for an
  existing claim: `findPatient` rejects the patient at line 201 first.
- `scripts/unit_ai.lua:280` — a dead/collapsed actor returns through
  `core.suspendOrders`, bypassing treatment execution.
- `scripts/unit_ai_stall.lua:230` — suspension preserves `treatClaim`.
- `src/Unit/Thread/Command/Pose.hs:107` — death retains the instance;
  `unit.getInfo` can still resolve the former medic.
- `test-headless/Test/Headless/Lua/UnitAiPageTargets.hs:1026` — existing
  claim coverage proves an off-page claimant does not block treatment,
  but its same-page control does not test claimant incapacitation.

**Reproduction:** Patient 2 at `(11,10)` has the fixture's bleeding/infected
slash. Healthy medic 1 at `(10,10)` has bleed-control knowledge 20 and a
stocked kit. Medic 3 at `(10,11)` has knowledge 100, pose `dead` or
`collapsed`, and cached state
`{currentAction="treat_ally", treatClaim={patient=2}}`. Call
`core.suspendOrders(3)`, then score medic 1. Both poses return:

```json
{"blocked":"-inf","retained":true,"control":"8.0","pending":2}
```

`control` and `pending` are measured after deleting only medic 3's claim.
The claim was seeded as a legitimate in-flight reservation; this fixture
does not simulate the injury that incapacitates its owner.

**Handoff context:** A retained reservation must not exclude replacement
care when its owner cannot act. Cover death, collapse, recovery and an
unavailable medic returning after someone else finishes treatment. Preserve
claim exclusivity for a living, available owner and the existing page/combat
exceptions. Avoid relying solely on new-candidate ranking, which this path
never reaches.

### BUG-24. An out-of-range preferred medic prevents nearby medics from treating

**Verification:** With a patient one tile from a novice medic and 100 tiles
from an expert, BOTH medics returned treatment utility `-inf`. Moving only
the expert to 59 tiles from the patient made the expert nominate and score
the patient at 8. The shipped scan range is 60 tiles.

Patient discovery rejects targets outside scan range, but the squad ranking
considers every same-page available medic at any distance. Its distance
penalty stops at 50%, so a sufficiently capable remote medic remains the
preferred choice even though its own discovery cannot select this patient.
The nearby medic stands down for a helper that will never take the job in
that stationary configuration.

**Evidence:**

- `scripts/unit_ai_medic.lua:130` and `:146` — `bestMedicFor` uses scan
  range only to scale a capped distance discount. There is no `d <= range`
  eligibility check before considering a medic.
- `scripts/unit_ai_medic.lua:197` and `:207` — `findPatient` applies a
  hard scan-range cutoff, so the remote winning medic cannot discover the
  patient that made it win the ranking.
- `scripts/unit_ai_medic.lua:236` — every other medic returns `-inf`
  when the ranking prefers someone else.
- `scripts/unit_ai_tunables.lua:486` — acolytes use range 60, with entry
  and lock utility both 8.
- `test-headless/Test/Headless/Lua/UnitAiPageTargets.hs:993` — ranking
  coverage distinguishes same-page versus off-page medics, but does not
  cross the same-page discovery-radius boundary.

**Reproduction:** Use the same wounded patient at `(11,10)` and medic 1 at
`(10,10)` with bleed-control knowledge 20. Give medic 3 knowledge 100 and put
it at `(111,10)`. Both medics are standing, allied, on the patient's page,
have supplies, and have no claims or combat actions. Fixture intelligence
resolves to 1 for each. The expert scores 50 in the ranking while the nearby
medic scores approximately 19.83, but the expert's own scan finds no patient:

```json
{"near":"-inf","far":"-inf"}
```

Move only medic 3 to `(70,10)`, 59 tiles from the patient:

```json
{"near":"-inf","far":"8.0","farTarget":2}
```

These are actual production utility results with real positions, not a
reimplementation of the ranking formula. The test does not assert the
patient stays untreated after unrelated movement changes the configuration.

**Handoff context:** Align the set of ranked medics with those eligible to
accept that patient, or explicitly dispatch the selected remote medic.
Test just inside, exactly on and outside the discovery boundary, differing
capabilities, and a nearby fallback. Retain the intended preference for a
better medic who can actually take the job.

### BUG-25. Autonomous infection treatment ignores antibiotic kits without bandages

**Verification:** A medic with infection-control knowledge repeatedly failed
to treat infection 0.6 despite a same-page supplier one tile away holding
five antibiotic doses. After 100 executions it had fetched nothing, retained
its treatment claim and utility 8, and left infection unchanged. Adding one
bandage to that same kit caused one real transfer, one antibiotic dose to be
spent, infection to reach zero and the claim to clear. The bandage was not
consumed: this patient's bleeding was already controlled.

Autonomous treatment recognizes infection as a reason to claim a patient,
but its supply phase only discovers kits containing bandages. An antibiotic
kit whose last bandage has been used is invisible to that phase. The medic
falls through to treatment without the medicine, reports the missing dose,
and holds the claim. The shared supply module already has a separate
antibiotic-kit finder; this executor never uses it.

**Evidence:**

- `scripts/unit_ai_medic.lua:60` and `:77` — infected wounds independently
  qualify for treatment even if no bleeding needs a dressing.
- `scripts/unit_ai_medic.lua:252` and `:263` — `ownKit` aliases only
  `supply.bandageKit`, and `findKitHolder` calls that alias for every holder.
- `scripts/unit_ai_medic.lua:318` — fetch decisions use that bandage-only
  discovery before either treatment verb is selected.
- `scripts/unit_ai_medic.lua:375` — the infection phase calls
  `unit.treatInfection` using the medic's own supplies. Its failure logs a
  warning without clearing or changing the supply plan or claim.
- `scripts/medical_supply.lua:60` — `antibioticsKit` exists and can find
  the same exact container by antibiotic fill without requiring bandages.
- `test-headless/Test/Headless/Unit/MedicalKitInstance.hs:133` — the
  stocked-kit fixture combines antibiotics and bandages, so exact-instance
  fetch coverage does not establish antibiotic-only acquisition.

**Reproduction:** Patient 2 at `(11,10)` has infection 0.6, wound clot 1
and bandage seep 0. Medic 1 at `(10,10)` knows bleed/infection control at 100
and starts with empty inventory. Supplier 3 at `(10,11)` has no medical
knowledge and holds kit 400. Its sole content is antibiotic instance 407,
fill 5. Register medic 1's real AI state, score it, and run up to 100
score/execute iterations. Compare with the identical fixture plus one
bandage in kit 400:

| Supplier's kit | Transfers | Final infection | Claim retained | Final utility |
|---|---:|---:|---|---|
| Antibiotics only | 0 | 0.60000002384186 | Yes | 8 |
| Antibiotics and one bandage | 1 | 0 | No | `-inf` |

Real inventory contents show the first kit still on the supplier with fill
5; the control kit moves to the medic with fill 4 and its bandage intact.
No inventory query, transfer or treatment result is stubbed. The 100 calls
isolate AI behavior; wound/infection progression is not running concurrently.

**Handoff context:** Discover and fetch supplies for the patient's actual
unmet treatment needs, including antibiotic-only kits and a medic carrying
bandages while antibiotics remain on another unit. Define a bounded fallback
when required medicine is unavailable so a futile cure loop does not hold
ordinary work indefinitely. Preserve the exact-instance and page/reach
policies already enforced by the shared discovery and treatment APIs.
