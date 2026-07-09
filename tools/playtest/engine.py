"""Engine connection for the playtest harness (#647).

Owns the windowed instance lifecycle and every debug-console
interaction the lockstep loop needs: pause control, F1 screenshots,
F2 input injection, and the oracle snapshot (F3 widgets, event-log
delta, menu state) that is recorded in the trace but NEVER shown to
the player agent.

Also owns the action->input.* translation: the player acts in
screenshot pixel space (F1's framebuffer pixels), which is exactly the
space the input.* verbs accept, so no coordinate conversion happens
here — only clamping to the frame.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from probelib import boot, quit_engine, send, send_json  # noqa: E402


class EngineCrash(Exception):
    """The engine process died or stopped answering mid-session."""


class ActionError(ValueError):
    """The agent's action doesn't fit the vocabulary."""


# The action vocabulary the harness accepts from agents. Mirrors F2's
# verbs; documented for the player in the agent prompt and in README.md.
ACTION_KINDS = ("click", "drag", "scroll", "key", "hold", "type", "wait", "done")


def _lua_str(text: str) -> str:
    """Quote a string for a single-line debug-console Lua call."""
    return '"' + text.replace("\\", "\\\\").replace('"', '\\"') \
                     .replace("\n", " ").replace("\r", " ") + '"'


def _mods_lua(mods) -> str:
    return "{" + ", ".join(_lua_str(str(m)) for m in mods) + "}"


def _clamp(v, lo, hi) -> float:
    return max(lo, min(hi, float(v)))


def translate_action(action: dict, fb_size: tuple[int, int]):
    """Agent action -> (main_calls, post_calls) of input.* Lua lines.

    main_calls are injected before the sim step; post_calls after it
    (only `hold` uses post: keyDown rides through the unpaused dt and
    the matching keyUp lands once the step finishes). Coordinates are
    clamped into the framebuffer so a wild guess still lands on-screen
    (a misclick is wanted signal; an out-of-range coordinate is not an
    interesting one).
    """
    w, h = fb_size
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
        calls = []
        if action.get("x") is not None and action.get("y") is not None:
            x, y = xy()
            calls.append(f"return input.moveMouse({x:.1f}, {y:.1f})")
        dx = float(action.get("dx") or 0)
        dy = float(action.get("dy") or 0)
        calls.append(f"return input.scroll({dx:.1f}, {dy:.1f})")
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
    """A launched windowed instance driven over the debug console."""

    def __init__(self, port: int, log_path: str):
        self.port = port
        self.log_path = log_path
        self.proc = None
        self.fb_size: tuple[int, int] | None = None
        self._event_log_seen = 0

    # -- lifecycle ---------------------------------------------------

    def launch(self, ready_timeout: float = 180.0) -> None:
        """Boot a WINDOWED instance (mode=()) — F1/F2 need a real
        render + input pipeline; this deliberately opens (and focuses)
        a game window. probelib.boot blocks until READY."""
        self.proc = boot(self.port, log=self.log_path, mode=(),
                         ready_timeout=ready_timeout, label="playtest engine")

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

    def lua(self, code: str, timeout: float = 15.0):
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

    def screenshot(self, path: str) -> tuple[int, int]:
        reply = self.lua(f"return debug.captureScreenshot({_lua_str(path)})",
                         timeout=20.0)
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

    def oracle_snapshot(self) -> dict:
        snap: dict = {"player_invisible": True}
        widgets = self.lua('return require("scripts.ui.registry").dumpWidgets()')
        snap["widgets"] = widgets if isinstance(widgets, list) else {"error": str(widgets)}
        menu = self.lua('return require("scripts.ui_manager").currentMenu')
        snap["current_menu"] = menu if isinstance(menu, str) else None
        snap["paused"] = self.lua("return engine.isPaused()") is True
        log = self.lua("return engine.getEventLog()")
        if isinstance(log, list):
            snap["event_log_new"] = log[self._event_log_seen:]
            self._event_log_seen = len(log)
        else:
            snap["event_log_new"] = []
        # The active world's generation seed (world.getSeed, added for
        # this harness) — nil until the player has created a world.
        # The runner promotes the first non-null value into meta so a
        # randomized-seed session is still reproducible/diagnosable.
        seed = self.lua("return world.getSeed()")
        snap["world_seed"] = seed if isinstance(seed, int) else None
        # F4 (#646, action-outcome tap) hasn't landed; when it does,
        # its per-turn outcomes belong here alongside the widget dump.
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

    def launch(self, ready_timeout: float = 0) -> None:
        self.proc = None

    def alive(self) -> bool:
        return True

    def quit(self) -> None:
        pass

    def set_paused(self, paused: bool) -> None:
        self.paused = paused

    def screenshot(self, path: str) -> tuple[int, int]:
        with open(path, "wb") as f:
            f.write(self._PNG)
        self.fb_size = (1280, 720)
        return self.fb_size

    def inject(self, calls: list[str]) -> list:
        self.injected.extend(calls)
        return [{"ok": True} for _ in calls]

    def oracle_snapshot(self) -> dict:
        return {"player_invisible": True, "widgets": [], "current_menu": "main",
                "paused": self.paused, "event_log_new": [], "world_seed": 4242}

    def lua(self, code: str, timeout: float = 0):
        return {"ok": True}

    def lua_fire(self, code: str) -> None:
        self.fired.append(code)
