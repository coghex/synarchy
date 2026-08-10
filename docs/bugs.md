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
