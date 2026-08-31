"""Engine connection for the playtest harness (#647).

Owns the launched game instance — windowed (the original substrate,
steals focus) or offscreen (#650: GPU on, window off, unattended /
parallel-safe) — and every debug-console interaction the lockstep
loop needs: pause control, F1 screenshots, F2 input injection, and the
oracle reads (F3 widgets, event-log progress, menu state) that are
recorded in the trace but NEVER shown to the player agent. The oracle
is split into `oracle_context()` (widgets/menu/pause/seed — cheap,
non-destructive, safe to call once per turn) and `oracle_events()`
(event-log progress + F4 action-outcome drain — cursor/destructive reads
the runner calls once after settle and again after a sim step, so a
turn's action gets credit for both what it produced synchronously and
what the step itself produced, #775). Both render modes serve the
identical render + input pipeline, so everything below the launch
flags is mode-blind.

Getting an instance TO that point is `launch.py`'s job (#1539): the
build, the process/console startup and the wait for a first
player-ready frame are three separately-budgeted setup phases, and the
boundary between them and play is what every player-session budget
starts from.

Also owns the action->input.* translation: the player acts in
screenshot pixel space (F1's framebuffer pixels), which is exactly the
space the input.* verbs accept, so no coordinate conversion happens
here — only clamping to the frame.
"""
from __future__ import annotations

import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from probelib import quit_engine, send, send_json  # noqa: E402


class EngineCrash(Exception):
    """The engine process died or stopped answering mid-session."""


class ActionError(ValueError):
    """The agent's action doesn't fit the vocabulary."""


# The action vocabulary the harness accepts from agents. Mirrors F2's
# verbs; documented for the player in the agent prompt and in README.md.
ACTION_KINDS = ("click", "drag", "scroll", "key", "hold", "type", "wait", "done")

# The wheel contract for the `scroll` action (#1980). `dy` is measured in
# WHEEL NOTCHES, not pixels, and its sign is the CAMERA's rather than the
# gesture's: Engine.Loop.Camera's `scrollZoomImpulse zoom dy =
# zoomScrollScale * zoom * dy` has a positive scale and camZoom is the
# viewport half-height, so a NEGATIVE dy shrinks the half-height and moves
# the camera toward the ground. The player prompt states that in those
# terms, and run.py --selftest re-derives it from the checked-in Haskell so
# the two cannot drift.
#
# The bound exists because the impulse multiplies by the CURRENT zoom, so
# one delta is far more violent from the whole-world view than from near
# the ground and no single magnitude is universally sensible. Ten notches
# is one short multi-notch correction — enough to cross the calibrated
# range in a turn without the unbounded gestures (dy=600) that reached the
# engine as accepted no-ops before this contract existed.
SCROLL_DY_NOTCH = 1.0
SCROLL_DY_MIN = -10.0
SCROLL_DY_MAX = 10.0

# Default per-call console budgets. Named so the setup launcher can
# shrink them to whatever is left of its own deadline (#1539) instead of
# letting one stalled read run 15-20 s past a smaller --setup-timeout.
CONSOLE_READ_TIMEOUT = 15.0
SCREENSHOT_TIMEOUT = 20.0

# How the instance renders: "windowed" opens (and focuses) a real game
# window; "offscreen" (#650) runs the same full render pipeline into
# offscreen images — no window, no focus steal, parallel-safe.
RENDER_MODES = ("windowed", "offscreen")


class OracleContractError(RuntimeError):
    """The engine answered an oracle read in a shape the oracle cannot
    trust. Raised, never swallowed: silently degrading here is exactly
    the failure mode #1714 removed."""


def _row_sequence(event) -> int:
    """The store's mutation sequence for one `engine.getEventLog()` row.

    Loud on anything else (#1714). A row without a usable sequence used
    to be handled by falling back to matching rows by VALUE, which is
    ambiguous the moment two rows are byte-identical and is what let the
    trace lose rows without saying so. If the engine ever stops
    publishing the field, the harness must fail visibly rather than
    quietly restore that behaviour.
    """
    if not isinstance(event, dict) or "sequence" not in event:
        raise OracleContractError(
            "engine.getEventLog() row has no `sequence` field "
            f"({event!r}); the oracle cannot track event-log progress "
            "without it and will not fall back to value matching")
    sequence = event["sequence"]
    # bool is an int subclass; a boolean sequence is a contract break,
    # not a 0/1 row number.
    if isinstance(sequence, bool) or not isinstance(sequence, int):
        raise OracleContractError(
            "engine.getEventLog() row `sequence` must be an integer, got "
            f"{sequence!r} ({type(sequence).__name__})")
    if sequence < 1:
        raise OracleContractError(
            f"engine.getEventLog() row `sequence` must be positive, got {sequence}")
    return sequence


def _event_log_high_water(value) -> int:
    """`engine.getEventLogProgress()`'s `highest`: how far the store has
    committed, whatever it still holds (#1714).

    Loud on anything else, for the same reason `_row_sequence` is: this
    is the ONLY thing that can tell an emptied ring apart from a store
    where nothing happened, so a harness that silently accepted a
    missing or unusable value would go back to reporting a load
    publication's discarded mutations as "no change".
    """
    if isinstance(value, bool) or not isinstance(value, int):
        raise OracleContractError(
            "engine.getEventLogProgress().highest must be an integer, got "
            f"{value!r} ({type(value).__name__})")
    if value < 0:
        raise OracleContractError(
            "engine.getEventLogProgress().highest must not be negative, got "
            f"{value}")
    return value


def _event_log_progress(cursor: int | None, current: list,
                        high_water: int) -> tuple[list, list, int]:
    """Advance the event-log cursor over one snapshot (#1714).

    Returns ``(new_rows, gaps, next_cursor)``.

    Rows carry a store-assigned `sequence`: a positive integer taken
    consecutively from 1, in mutation commit order, for the lifetime of
    one engine process. Both halves of that -- consecutive AND
    commit-ordered -- are load-bearing here. Progress is therefore
    arithmetic on those numbers, never a comparison of row values, so
    byte-identical rows are no longer indistinguishable.

    `high_water` is the store's own count of committed mutations, taken
    from the SAME engine-side snapshot as the rows. It is not derivable from them:
    a load publish empties the ring without resetting the counter, so
    the rows can say "nothing here" while every mutation since the last
    read has in fact been committed and discarded. Taking the ceiling
    from the rows instead would report that as no change at all --
    permanently, if no later row happens to arrive.

    `cursor` is the highest sequence any previous observation has seen.
    `None` means this is the BASELINE observation: it reports the whole
    current log and no gap, because nothing before it was claimed to be
    observed. It still adopts `high_water` as its cursor, so mutations
    that predate the baseline are treated as never-claimed rather than
    as a loss the next read must report.

    `gaps` names every committed sequence newer than `cursor` that is
    NOT in this snapshot, grouped into maximal intervals -- up to
    `high_water`, so an interval whose entire tail is missing is
    reported rather than truncated at the last surviving row. The store
    is bounded, so such sequences genuinely exist and genuinely cannot
    be recovered: front eviction drops them, a coalesced replacement
    retires the sequence it superseded, and a load publish discards
    whatever it held. A gap object asserts ABSENCE only and never
    attributes a cause -- from the snapshot the three are
    indistinguishable.

    The cursor is monotonic and never moves backwards.
    """
    sequences = [_row_sequence(event) for event in current]
    ceiling = max([high_water] + sequences)

    if cursor is None:
        return current[:], [], ceiling

    new_rows = [event for event, sequence in zip(current, sequences)
                if sequence > cursor]
    # Walk the sequences PRESENT rather than the range between them: an
    # arbitrarily long interval can be missing (a burst far larger than
    # the ring, or a whole ring discarded by a load), and enumerating
    # every absent number to group it would allocate proportionally to
    # the loss instead of to the snapshot.
    gaps = []
    expected = cursor + 1
    for sequence in sorted(set(sequences)):
        if sequence <= cursor:
            continue
        if sequence > expected:
            gaps.append({"first_sequence": expected,
                         "last_sequence": sequence - 1,
                         "missing_count": sequence - expected})
        expected = max(expected, sequence + 1)
    # The tail: everything committed after the newest surviving row.
    # This is the load-publish case in its pure form -- an empty ring
    # with a high-water mark ahead of the cursor is ALL tail.
    if ceiling >= expected:
        gaps.append({"first_sequence": expected,
                     "last_sequence": ceiling,
                     "missing_count": ceiling - expected + 1})
    return new_rows, gaps, max(cursor, ceiling)


# Both halves of an event-log observation in one engine call (#1714):
# the surviving rows, and how far the store has committed. It is ONE
# verb rather than two composed in Lua because the two halves have to
# come from a single read of the store -- an emitter committing between
# them would produce a high-water mark naming a row the snapshot does
# not show, and the oracle would report a still-retained row as lost and
# then suppress it forever.
EVENT_LOG_PROGRESS_LUA = "return engine.getEventLogProgress()"


def _event_log_reply(reply) -> tuple[list, int]:
    """Unpack one `engine.getEventLogProgress()` reply into
    `(rows, high_water)` (#1714).

    Strict on every shape, with no tolerant branch. `self.lua()` already
    raises `EngineCrash` when the console is unreachable, so a reply
    that comes BACK and is not a well-formed progress table means the
    API is missing or broken -- `send_json` hands back `None` for a Lua
    `nil` and a string for a Lua error, and a partial table such as
    `{"highest": 2}` is just as broken. Reading any of those as "no
    events and no gaps this turn" would leave the cursor untouched and
    silently erase the turn's event evidence, which is the exact failure
    this change exists to remove.

    Raising ends the session the way any unhandled harness fault does:
    `run.py`'s driver still writes a finished trace (its `finally`
    always runs `trace.finish`), so the partial evidence and the engine
    log survive for diagnosis -- what does not survive is a run that
    looks clean while reporting nothing.
    """
    if not isinstance(reply, dict):
        raise OracleContractError(
            "engine.getEventLogProgress() must return a table of "
            f"{{rows, highest}}, got {reply!r} ({type(reply).__name__})")
    missing = [key for key in ("rows", "highest") if key not in reply]
    if missing:
        raise OracleContractError(
            "engine.getEventLogProgress() reply is missing "
            f"{', '.join(missing)}: {reply!r}")
    return _lua_array(reply["rows"]), _event_log_high_water(reply["highest"])


def _lua_array(value) -> list:
    """A Lua sequence as a Python list.

    Lua has one table type, so an EMPTY array serializes as `{}` -- a
    JSON object. That is exactly the shape a load publication produces,
    so it is a case to handle, never one to reject.
    """
    if isinstance(value, list):
        return value
    if isinstance(value, dict) and not value:
        return []
    raise OracleContractError(
        "engine.getEventLogProgress().rows must be an array of rows, got "
        f"{value!r}")


def _lua_str(text: str) -> str:
    """Quote a string for a single-line debug-console Lua call."""
    return '"' + text.replace("\\", "\\\\").replace('"', '\\"') \
                     .replace("\n", " ").replace("\r", " ") + '"'


def _mods_lua(mods) -> str:
    return "{" + ", ".join(_lua_str(str(m)) for m in mods) + "}"


def _clamp(v, lo, hi) -> float:
    return max(lo, min(hi, float(v)))


def bound_scroll_dy(dy):
    """Apply the published wheel contract to one requested vertical delta.

    Returns ``(effective_dy, note)``: `note` is None when the request was
    already inside the contract, and otherwise says which of the two
    outcomes happened, in the words the turn record keeps (#1980 req 4).

    The two outcomes are deliberately different, because the inputs are:

    * A finite delta outside the range is a real gesture asking for too
      much, so it is CLAMPED to the nearest bound. The turn still spends
      its one action on a wheel movement the camera actually performs,
      and the note carries both the requested and the effective value.
    * A non-finite delta (NaN, +/-inf) is not a magnitude at all — there
      is no nearest bound to clamp it to — so it is REJECTED and no
      scroll call is generated for the turn.

    This is enforced here, at the translation boundary, rather than in the
    structured schema alone: a scripted agent and a lenient provider
    fallback both reach `translate_action` without any schema having
    validated them.
    """
    try:
        value = float(dy)
    except (TypeError, ValueError):
        raise ActionError(
            f"action 'scroll' rejected: dy must be a number in "
            f"[{SCROLL_DY_MIN}, {SCROLL_DY_MAX}], got {dy!r}") from None
    if not math.isfinite(value):
        raise ActionError(
            f"action 'scroll' rejected: dy must be a finite number in "
            f"[{SCROLL_DY_MIN}, {SCROLL_DY_MAX}], got {value!r}; "
            f"no scroll was sent")
    if SCROLL_DY_MIN <= value <= SCROLL_DY_MAX:
        return value, None
    effective = _clamp(value, SCROLL_DY_MIN, SCROLL_DY_MAX)
    return effective, (
        f"scroll dy {value:g} is outside the contract range "
        f"[{SCROLL_DY_MIN:g}, {SCROLL_DY_MAX:g}] and was clamped to "
        f"{effective:g}; one wheel notch is {SCROLL_DY_NOTCH:g} and "
        f"negative dy zooms in toward the ground")


def translate_action(action: dict, fb_size: tuple[int, int], notes=None):
    """Agent action -> (main_calls, post_calls) of input.* Lua lines.

    main_calls are injected before the sim step; post_calls after it
    (only `hold` uses post: keyDown rides through the unpaused dt and
    the matching keyUp lands once the step finishes). Coordinates are
    clamped into the framebuffer so a wild guess still lands on-screen
    (a misclick is wanted signal; an out-of-range coordinate is not an
    interesting one).

    `notes` is an optional list the caller passes to collect the harness's
    own remarks about what it did with a request it could not honour
    verbatim — today only the clamped-wheel-delta note (#1980). The
    requested action itself is never rewritten: the trace keeps what the
    player asked for, and the note says what was actually injected. An
    outright refusal still raises `ActionError`, which the runner already
    records the same way while injecting nothing.
    """
    w, h = fb_size

    def add_note(text):
        if notes is not None:
            notes.append(text)

    kind = action.get("do")
    if kind not in ACTION_KINDS:
        raise ActionError(f"unknown action {kind!r} (expected one of {ACTION_KINDS})")

    def xy(xk="x", yk="y"):
        if action.get(xk) is None or action.get(yk) is None:
            raise ActionError(f"action {kind!r} needs numeric {xk}/{yk}")
        return _clamp(action[xk], 0, w - 1), _clamp(action[yk], 0, h - 1)

    def btn_mods_args():
        args = ""
        button = action.get("button")
        mods = action.get("mods")
        if button or mods:
            args += ", " + _lua_str(str(button or "left"))
        if mods:
            args += ", " + _mods_lua(mods)
        return args

    if kind == "click":
        x, y = xy()
        return [f"return input.click({x:.1f}, {y:.1f}{btn_mods_args()})"], []
    if kind == "drag":
        x1, y1 = xy("x1", "y1")
        x2, y2 = xy("x2", "y2")
        button = _lua_str(str(action.get("button") or "left"))
        return [
            f"return input.mouseDown({x1:.1f}, {y1:.1f}, {button})",
            f"return input.moveMouse({(x1 + x2) / 2:.1f}, {(y1 + y2) / 2:.1f})",
            f"return input.moveMouse({x2:.1f}, {y2:.1f})",
            f"return input.mouseUp({x2:.1f}, {y2:.1f}, {button})",
        ], []
    if kind == "scroll":
        # The bound is checked BEFORE the optional cursor pre-move is
        # generated, so a rejected delta leaves the whole turn without any
        # injected call rather than moving the pointer and then refusing.
        # dx keeps its historical verbatim forwarding: the camera premise
        # and the player-facing notch vocabulary are about dy alone.
        dy, dy_note = bound_scroll_dy(action.get("dy") or 0)
        if dy_note:
            add_note(dy_note)
        calls = []
        if action.get("x") is not None and action.get("y") is not None:
            x, y = xy()
            calls.append(f"return input.moveMouse({x:.1f}, {y:.1f})")
        dx = float(action.get("dx") or 0)
        # dy carries more precision than dx because the contract now
        # advertises fractional notches for trackpad-style input, and at
        # one decimal a small real gesture rounds to a literal 0.0 — the
        # accepted no-op this contract exists to stop. dx keeps its
        # historical formatting along with its historical forwarding.
        calls.append(f"return input.scroll({dx:.1f}, {dy:.4f})")
        return calls, []
    if kind == "key":
        name = action.get("name")
        if not name:
            raise ActionError("action 'key' needs a key name")
        args = _lua_str(str(name))
        if action.get("mods"):
            args += ", " + _mods_lua(action["mods"])
        return [f"return input.key({args})"], []
    if kind == "hold":
        name = action.get("name")
        if not name:
            raise ActionError("action 'hold' needs a key name")
        return ([f"return input.keyDown({_lua_str(str(name))})"],
                [f"return input.keyUp({_lua_str(str(name))})"])
    if kind == "type":
        text = action.get("text")
        if not isinstance(text, str) or text == "":
            raise ActionError("action 'type' needs non-empty text")
        return [f"return input.type({_lua_str(text)})"], []
    # wait / done inject nothing
    return [], []


class PlaytestEngine:
    """A launched game instance driven over the debug console."""

    def __init__(self, port: int, log_path: str,
                 render_mode: str = "windowed"):
        if render_mode not in RENDER_MODES:
            raise ValueError(f"render_mode {render_mode!r} "
                             f"(expected one of {RENDER_MODES})")
        self.port = port
        self.log_path = log_path
        self.render_mode = render_mode
        self.proc = None
        self.fb_size: tuple[int, int] | None = None
        # Highest event-log sequence any observation has seen
        # (#1714). `None` until the baseline read.
        self._event_log_cursor: int | None = None

    # -- lifecycle ---------------------------------------------------

    def boot_mode(self) -> tuple[str, ...]:
        """The boot-profile flag(s) this render mode launches with —
        `launch.start_engine` passes them straight to the executable."""
        return () if self.render_mode == "windowed" else ("--offscreen",)

    # Launching is NOT here: `launch.py` owns the cold-boot sequence and
    # the player-ready boundary (#1539). It builds, spawns and waits in
    # three separately-classified phases under their own budget, and
    # records the process on `self.proc` exactly as this used to — so
    # `quit`/`alive`/`log_tail` below are unchanged. probelib.boot's
    # single 180 s READY deadline (which counted compilation against
    # itself) is deliberately no longer on the playtest path, while its
    # contract for the ~85 behavior probes that DO call it is untouched.

    def quit(self) -> None:
        if self.proc is not None and self.proc.poll() is None:
            try:
                quit_engine(self.port, self.proc)
            except Exception:
                if self.proc.poll() is None:
                    self.proc.kill()
        self.proc = None

    def alive(self) -> bool:
        return self.proc is not None and self.proc.poll() is None

    def log_tail(self, lines: int = 60) -> str:
        try:
            with open(self.log_path, errors="replace") as f:
                return "".join(f.readlines()[-lines:])
        except OSError:
            return ""

    # -- console I/O with crash detection -----------------------------

    def lua(self, code: str, timeout: float = CONSOLE_READ_TIMEOUT):
        """Run one console line, JSON-decoding the reply. Raises
        EngineCrash when the process is gone / unreachable — a crash
        mid-session is a finding, and the caller ends gracefully."""
        try:
            return send_json(self.port, code, timeout=timeout)
        except OSError as e:
            raise EngineCrash(f"console unreachable ({e}); "
                              f"process {'dead' if not self.alive() else 'alive'}") from e

    def lua_fire(self, code: str) -> None:
        try:
            send(self.port, code, expect_result=False)
        except OSError as e:
            raise EngineCrash(f"console unreachable ({e})") from e

    # -- lockstep primitives ------------------------------------------

    def set_paused(self, paused: bool) -> None:
        # Route through scripts/pause so the world clock and engine
        # pause flag stay in sync (same path Space uses).
        flag = "true" if paused else "false"
        self.lua_fire(f'require("scripts.pause").set({flag})')

    def screenshot(self, path: str,
                   timeout: float = SCREENSHOT_TIMEOUT) -> tuple[int, int]:
        reply = self.lua(f"return debug.captureScreenshot({_lua_str(path)})",
                         timeout=timeout)
        if not isinstance(reply, dict) or "width" not in reply:
            err = reply.get("error") if isinstance(reply, dict) else repr(reply)
            raise EngineCrash(f"screenshot failed: {err}")
        self.fb_size = (int(reply["width"]), int(reply["height"]))
        return self.fb_size

    def inject(self, calls: list[str]) -> list:
        """Inject input.* calls; each returns an ack table. A non-ok
        ack is recorded, not fatal — the player pressing on a dead
        surface is signal for the critic."""
        acks = []
        for call in calls:
            acks.append(self.lua(call))
        return acks

    # -- oracle (recorded, never shown to the player) ------------------
    #
    # Split in two (#775) so a turn's record can correctly attribute
    # BOTH the synchronous consequences of injecting an action (while
    # still paused, during `settle`) and whatever the unpaused `dt` sim
    # interval itself produces to that SAME turn, instead of losing the
    # latter to whichever turn happens to query it next:
    #   oracle_context() — the affordance state the player interacted
    #     with (widgets/menu/pause/seed). Read-only, non-destructive,
    #     safe to call once per turn regardless of whether a sim step
    #     follows.
    #   oracle_events() — the event-log progress read (#1714:
    #     `event_log_new` plus `event_log_gaps`) + the F4
    #     action-outcome drain. Both advance state, so calling this
    #     TWICE in one turn (once after settle, once after the step)
    #     yields two disjoint slices that the caller concatenates —
    #     never a double-count, never a drop.
    #   oracle_routing() — #1750's widget read, taken BEFORE the first
    #     input call of the turn is injected, so click correlation is
    #     joined against the state the click was actually routed
    #     against rather than whatever a callback left behind. Also
    #     read-only and non-destructive.

    def oracle_routing(self) -> dict:
        """The #1750 ROUTING context: one ui.dumpWidgets() read taken
        BEFORE this turn's first input call is injected, so the records
        the offline click join correlates against are the ones the real
        pointer router actually resolved the click against.

        `oracle_context` below stays where it is (after inject+settle) —
        its menu/pause/seed fields are consumed with post-injection
        meaning by `_promote_seed` and the session digest, and #775's
        pre-STEP contract is unchanged. This is a separate, narrower
        read taken at a strictly earlier moment: a callback that opens,
        closes, or replaces a modal, or that creates/destroys elements,
        rewrites `widgets` but can no longer rewrite the routing record
        set. Read-only and non-destructive, exactly like
        `oracle_context`."""
        return {"widgets": self._dump_widgets()}

    def _dump_widgets(self):
        widgets = self.lua('return require("scripts.ui.registry").dumpWidgets()')
        return widgets if isinstance(widgets, list) else {"error": str(widgets)}

    def oracle_context(self) -> dict:
        snap: dict = {"player_invisible": True}
        snap["widgets"] = self._dump_widgets()
        menu = self.lua('return require("scripts.ui_manager").currentMenu')
        snap["current_menu"] = menu if isinstance(menu, str) else None
        snap["paused"] = self.lua("return engine.isPaused()") is True
        # The active world's generation seed (world.getSeed, added for
        # this harness) — nil until the player has created a world.
        # The runner promotes the first non-null value into meta so a
        # randomized-seed session is still reproducible/diagnosable.
        seed = self.lua("return world.getSeed()")
        snap["world_seed"] = seed if isinstance(seed, int) else None
        return snap

    def oracle_events(self) -> dict:
        snap: dict = {}
        # Rows and high-water mark from ONE engine-side snapshot
        # (#1714): sampled separately they could straddle a mutation,
        # and the whole point of the pair is that they disagree only in
        # the direction a load publish creates.
        # Every failure here raises rather than degrading: a malformed
        # or missing reply read as "nothing happened" is the silent loss
        # this whole change removes.
        rows, high_water = _event_log_reply(self.lua(EVENT_LOG_PROGRESS_LUA))
        new_rows, gaps, cursor = _event_log_progress(
            self._event_log_cursor, rows, high_water)
        snap["event_log_new"] = new_rows
        snap["event_log_gaps"] = gaps
        self._event_log_cursor = cursor
        # F4 (#646): drains the action-outcome ring — a destructive read,
        # like combat.drainEvents/injury.drainEvents, so no "_seen" index
        # is needed the way event_log_new above needs one; whatever
        # accumulated since the last drain comes back once and the
        # engine-side buffer is empty again immediately after.
        outcomes = self.lua("return debug.drainActionOutcomes()")
        snap["action_outcomes"] = outcomes if isinstance(outcomes, list) else []
        return snap


class FakeEngine(PlaytestEngine):
    """Offline stand-in for --selftest: exercises the loop, trace
    write, stuck detection, and replay with no window, no GPU, and no
    engine build. Screenshot writes a constant 1x1 PNG."""

    _PNG = bytes.fromhex(
        "89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c489"
        "0000000d49444154789c63606060f80f00010401005fe5c34b0000000049454e44ae426082")

    def __init__(self):
        super().__init__(port=0, log_path=os.devnull)
        self.injected: list[str] = []
        self.fired: list[str] = []
        self.paused = True
        self.unpauses = 0  # sim steps taken — selftest counts them (#698)

    def alive(self) -> bool:
        return True

    def quit(self) -> None:
        pass

    def set_paused(self, paused: bool) -> None:
        if not paused:
            self.unpauses += 1
        self.paused = paused

    def screenshot(self, path: str,
                   timeout: float = SCREENSHOT_TIMEOUT) -> tuple[int, int]:
        with open(path, "wb") as f:
            f.write(self._PNG)
        self.fb_size = (1280, 720)
        return self.fb_size

    def inject(self, calls: list[str]) -> list:
        self.injected.extend(calls)
        return [{"ok": True} for _ in calls]

    def oracle_routing(self) -> dict:
        return {"widgets": []}

    def oracle_context(self) -> dict:
        return {"player_invisible": True, "widgets": [], "current_menu": "main",
                "paused": self.paused, "world_seed": 4242}

    def oracle_events(self) -> dict:
        return {"event_log_new": [], "event_log_gaps": [],
                "action_outcomes": []}

    def lua(self, code: str, timeout: float = 0):
        return {"ok": True}

    def lua_fire(self, code: str) -> None:
        self.fired.append(code)
