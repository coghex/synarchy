#!/usr/bin/env python3
"""Shared harness for the headless behavior probes (#529).

Every ``tools/*_probe.py`` used to reimplement engine boot, the READY
wait, the single-line debug-console send/recv (with its short-idle-timeout
read), and ``engine.quit`` teardown — plus, in most cases, the acolyte
spawn + ``find_water`` goal-clear bootstrap. This module owns those
primitives once, so a fix (or a gotcha) lives in one place instead of ~40
copies.

Import it from a probe (probes run as ``python3 tools/<name>_probe.py``,
so ``tools/`` is on ``sys.path``)::

    from probelib import send, boot, quit_engine, spawn_acolyte, poll_until

Design notes / gotchas handled here so probes don't each rediscover them:

* **Debug-console idle read.** The console keeps the TCP connection open
  after replying, so we can't read to EOF. ``send`` reads until a short
  idle gap and returns the last non-empty ``"> "`` result line.
* **Never bind 8008 by default.** 8008 is the user's GUI port. Probes must
  pass their own (9xxx) port; ``boot`` refuses 8008 to avoid killing a
  graphical instance.
* **Acolyte ``find_water`` default goal.** A freshly spawned acolyte
  carries a standing ``find_water`` goal whose search utility can edge out
  the behavior under test (and can walk it off a cliff). ``spawn_acolyte``
  clears it by default.
"""
from __future__ import annotations

import json
import socket
import subprocess
import sys
import time
from collections.abc import Callable

import probe_engine

# The user's graphical instance lives on 8008; probes must never touch it.
GUI_PORT = 8008

DEFAULT_READY_TIMEOUT = 180.0
DEFAULT_IDLE = 0.3


# --------------------------------------------------------------------------
# Debug-console send / recv
# --------------------------------------------------------------------------
def _result_lines(chunks: list[bytes]) -> list[str]:
    """Non-empty ``"> <value>"`` reply lines (banner + trailing prompt are
    empty ``"> "`` lines, which drop out)."""
    out = b"".join(chunks).decode(errors="replace")
    return [ln[2:].strip() for ln in out.splitlines()
            if ln.startswith("> ") and ln[2:].strip()]


def send(port: int, lua: str, timeout: float = 10.0,
         expect_result: bool = True, idle: float = DEFAULT_IDLE) -> str:
    """Run one line of Lua in the debug console; return the result text.

    The console keeps the connection open after replying, so we can't read
    to EOF. The console echoes a banner, then ``"> <result>"`` lines, then a
    trailing empty ``"> "`` prompt; we return the last NON-EMPTY result line.

    ``expect_result=True`` (a ``return ...`` command) reads until a real
    result line appears, waiting up to ``timeout`` across idle gaps — this
    survives server-side BLOCKING calls like ``world.waitForInit`` that emit
    nothing until they unblock, then returns as soon as the value arrives.
    ``expect_result=False`` is for fire-and-forget commands (no ``return``):
    it drains the reply burst and stops at the first ``idle`` gap so it
    doesn't sit out the whole ``timeout``.
    """
    deadline = time.monotonic() + timeout
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(idle)
        while time.monotonic() < deadline:
            try:
                b = s.recv(4096)
            except socket.timeout:
                # Idle gap: settle here (this ~idle wait is deliberate — it
                # paces successive commands the way the original probes' read
                # did, which time-dependent probes rely on for async work like
                # inventory materialization or AI ticks). Only keep waiting if
                # this is a `return` command whose (possibly blocking) result
                # hasn't arrived yet.
                if _result_lines(chunks) or not expect_result:
                    break
                continue
            if not b:
                break
            chunks.append(b)
    # Strip surrounding quotes so a Lua string result ("ok") compares as
    # the bare value (ok) — matches the base probes' send and their string
    # equality checks. A no-op for numbers/booleans/JSON arrays+objects
    # (they don't start/end with a quote), so json.loads still works.
    results = _result_lines(chunks)
    if results:
        return results[-1].strip('"')
    return b"".join(chunks).decode(errors="replace").strip().strip('"')


def send_json(port: int, lua: str, timeout: float = 10.0, idle: float = DEFAULT_IDLE):
    """``send`` + ``json.loads`` the result.

    An EMPTY result (no reply, or a Lua empty string, which ``send``
    returns unquoted) is ``None``. Text that is not valid JSON is
    returned AS TEXT, not ``None`` — a Lua ``nil`` arrives as the JSON
    literal ``null`` and so decodes to ``None`` through the normal path,
    which is why callers can test ``is None`` for "the engine said
    nothing was there". A caller that must distinguish an empty STRING
    from an absent value has to call ``send`` directly.
    """
    raw = send(port, lua, timeout=timeout, idle=idle)
    if not raw:
        return None
    try:
        return json.loads(raw)
    except (ValueError, TypeError):
        return raw


# --------------------------------------------------------------------------
# Inline fixtures (#1342)
# --------------------------------------------------------------------------
class FixtureNotRegistered(RuntimeError):
    """A probe's own inline fixture was rejected by the engine loader.

    This is a SETUP failure, not a behavioural one. Nothing downstream of
    a fixture that never registered can pass, so the affected scenario
    must STOP here rather than report the consequences.

    Probes let it propagate out of ``main``: every ``finally:
    quit_engine(...)`` on the way out still runs, so the engine process
    and the temporary files are released exactly as on any other exit.
    The ``__main__`` entry point then prints it and exits nonzero without
    a traceback.
    """


def load_fixture_yaml(port: int, loader: str, path: str,
                      timeout: float = 10.0) -> float:
    """Load a probe-authored fixture, requiring its loader to accept it.

    Every ``engine.load*Yaml`` verb returns how much it registered and
    reports a rejected file as a plain ``0`` (``Engine.Asset.YamlList``
    hands back an empty list after a parse failure). The idiom this
    replaces — ``send(port, f"{loader}('{path}'); return 'ok'")`` —
    discards that: ``'ok'`` is the statement's value, so the call reads as
    success whether the fixture registered five definitions or none. A
    fixture that rots against a schema change then surfaces only as
    whatever downstream assertions happen to fail, arbitrarily far from
    the cause; #1341 spent a day reading as nine transfer-system
    regressions.

    WHAT the count counts is each loader's own business — definitions for
    the item/building/location loaders, textures queued for
    ``engine.loadFloraYaml`` (``registerFloraSpecies`` counts at least the
    base texture for every species it registers), 1-or-0 for
    ``engine.loadLootTableYaml``. Only the REJECTION signal is shared:
    every one of them returns zero, and nothing else, when it registered
    nothing. So this asserts a positive count and leaves the exact value
    to callers that care about it.

    For a probe's OWN inline fixtures only — YAML the probe authored or
    generated during this run. Directory bulk-loads of the game's shipped
    ``data/**`` are deliberately left alone (#1342): they are covered by
    every other gate, and a per-file count assertion there is brittle
    noise.

    Returns the count. Raises `FixtureNotRegistered` on zero or on a
    non-numeric reply.
    """
    raw = send(port, f"return {loader}('{path}')", timeout=timeout)
    try:
        count = float(raw)
    except (TypeError, ValueError):
        raise FixtureNotRegistered(
            f"SETUP FAILURE: {loader} did not answer with a count for the "
            f"probe fixture {path} (got {raw!r}). The probe cannot show "
            f"that its own fixture registered, so it stops here.") from None
    if count <= 0:
        raise FixtureNotRegistered(
            f"SETUP FAILURE: {loader} registered nothing from the probe "
            f"fixture {path} (it returned {count:g}). That fixture is "
            f"invalid for the current schema — the engine log names the "
            f"parse error. Nothing downstream of it can pass, so the probe "
            f"stops here instead of reporting the consequences as "
            f"behavioural failures.")
    return count


# --------------------------------------------------------------------------
# Engine boot / teardown
# --------------------------------------------------------------------------
def _log_path(port: int, log: str | None) -> str:
    return log if log else f"/tmp/synarchy_probe_{port}.log"


def _dispose_unowned(proc: subprocess.Popen) -> None:
    """Kill a child nobody has taken ownership of yet, quietly.

    Used only on the failure path inside ``boot`` before ``on_launch``
    has completed. It never talks to the port: an engine that has not
    printed READY is not known to be the listener there, and on a busy
    port it definitely is not.
    """
    try:
        if proc.poll() is None:
            proc.kill()
        proc.wait(timeout=10)
    except (OSError, subprocess.TimeoutExpired):
        pass


def boot(port: int, log: str | None = None, args: list[str] | None = None,
         ready_timeout: float = DEFAULT_READY_TIMEOUT,
         label: str = "engine",
         mode: tuple[str, ...] = ("--headless",),
         on_launch: Callable[[subprocess.Popen], None] | None = None,
         prepare_timeout: float = probe_engine.DEFAULT_PREPARE_TIMEOUT,
         ) -> subprocess.Popen:
    """Launch an engine on ``port`` and block until it prints READY.

    Exits the probe (non-zero) if the engine dies before READY or never
    prints it. ``args`` are extra CLI args appended after ``--port``.
    ``log`` is the stdout/stderr capture path (defaults to a per-port temp).
    ``mode`` is the boot-profile flag(s) (default ``--headless``); pass e.g.
    ``("--preview", "units/acolyte")`` for a preview-mode boot (#632) — the
    debug console (and its READY print) starts the same way regardless of
    boot profile, so only the launch flags differ.

    ``on_launch``, when given, is handed the process the instant it
    exists — before the READY wait, and therefore before either of this
    function's own failure exits — so a caller's teardown guard can own
    the engine from as early as Python allows instead of only once this
    returns, which on a hung boot is ``ready_timeout`` (three minutes by
    default) later. That gap is what lets an interrupt strand a live
    engine holding the port with nothing left holding its handle
    (#1682). A caller that registers this way must dispose of the handle
    DIRECTLY rather than through ``quit_engine``: this function's own
    failure paths mean the port may belong to somebody else's instance,
    which is precisely why the boot failed.

    ``prepare_timeout`` is a SEPARATE allowance covering the direct
    path's build (#1913), and it is not a second readiness budget: run
    by hand with nobody having supplied an executable, this function
    builds ``exe:synarchy`` and resolves its absolute path BEFORE it
    launches anything, so ``ready_timeout`` below measures an engine
    starting rather than a compiler running. Under the aggregate runner
    there is nothing to prepare and the step is a validated read of the
    path the runner already resolved. A preparation that fails, or
    overruns its own allowance, exits naming PREPARATION and the log the
    build output went to — never "never printed READY" against a log the
    engine never reached.

    Handing ownership over is itself guarded: anything raised between
    the child existing and that hand-off completing — a ``Ctrl-C``
    delivered in the middle of it, or a callback that itself fails —
    kills the child here rather than leave it holding the port with
    nobody downstream aware of it. The one instant that cannot be
    covered from Python is the store of ``Popen``'s own return value:
    an object has to be bound to a name by an instruction, and no
    handler can name what has not been bound yet.
    """
    if port == GUI_PORT:
        sys.exit(f"refusing to boot on port {GUI_PORT} (the GUI port); pass a 9xxx port")
    logpath = _log_path(port, log)
    # The runner resolves ONE executable before any probe starts and
    # hands it over through the environment (#1570). With none supplied
    # this BUILDS one — still no prior build step for a probe run by
    # hand, but the build happens here, outside the READY deadline
    # established below, holding `cabal-build` exclusively and owning
    # its own process group (#1913). Nothing about it is charged to the
    # engine: the failure it raises names preparation, and the engine
    # log is not even opened until it has succeeded, so an empty one
    # can no longer be the only artifact of a build that overran.
    try:
        cmd = probe_engine.prepare_command(
            [*mode, "--port", str(port), *(args or [])],
            log_path=f"{logpath}.prepare", timeout=prepare_timeout,
            announce=lambda note: print(f"[{label}] {note}",
                                        file=sys.stderr, flush=True))
    except probe_engine.EngineExecutableError as error:
        # The error already names preparation; the label says which
        # engine's preparation it was.
        sys.exit(f"{label}: {error}")
    logf = open(logpath, "w")
    proc = None
    try:
        proc = subprocess.Popen(cmd, stdout=logf, stderr=subprocess.STDOUT)
        if on_launch is not None:
            on_launch(proc)
    except BaseException:
        # Ownership never completed, so nothing downstream knows this
        # child exists — including the caller's own teardown guard. It
        # is disposed of HERE, and by killing rather than by asking:
        # there is no engine to ask yet, and the port may be somebody
        # else's.
        if proc is not None:
            _dispose_unowned(proc)
        raise
    proc._probe_log = logpath  # type: ignore[attr-defined]
    deadline = time.time() + ready_timeout
    while time.time() < deadline:
        try:
            if "READY" in open(logpath).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"{label} exited before READY; see {logpath}")
        time.sleep(0.4)
    proc.kill()
    sys.exit(f"{label} never printed READY; see {logpath}")


def quit_engine(port: int, proc: subprocess.Popen | None = None,
                timeout: float = 15.0, wait_port: bool = True) -> None:
    """Ask the engine to quit, then ensure the process is gone and (by
    default) the listener port is released so the next boot on it can bind.

    Safe to call in a ``finally``: ``engine.quit()`` is a fire-and-forget
    command (``expect_result=False`` so it doesn't sit out the timeout), a
    dead-socket error on the send is swallowed, it waits up to ``timeout``
    for a clean exit and hard-kills if needed, then waits briefly for the
    port to free (which is what restart probes need between boots).
    """
    try:
        send(port, "engine.quit()", timeout=3.0, expect_result=False)
    except OSError:
        pass
    if proc is not None:
        deadline = time.time() + timeout
        while time.time() < deadline:
            if proc.poll() is not None:
                break
            time.sleep(0.2)
        if proc.poll() is None:
            proc.kill()
            try:
                proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                pass
    if wait_port:
        for _ in range(50):  # up to ~5 s for the listener socket to release
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                if s.connect_ex(("localhost", port)) != 0:
                    return
            time.sleep(0.1)


# --------------------------------------------------------------------------
# Polling
# --------------------------------------------------------------------------
def poll_until(seconds: float, fn, interval: float = 0.3):
    """Poll ``fn()`` every ``interval`` s until truthy or the budget runs out.

    Returns the truthy value, or ``None`` on timeout.
    """
    deadline = time.time() + seconds
    while time.time() < deadline:
        v = fn()
        if v:
            return v
        time.sleep(interval)
    return None


def wait_load_published(port: int, seconds: float = 180.0, interval: float = 0.2,
                         request_id: int | None = None):
    """Poll ``engine.getLoadStatus()`` (issue #763, save-overhaul C2)
    until a whole-session load transaction reaches a terminal phase.

    ``engine.loadSave`` only ACCEPTS the request synchronously — staging
    (chunk gen, etc.) and publication run asynchronously on the world
    thread, and nothing the save contains exists live (nor does
    ``world.getActiveWorldId()``/``world.waitForInit`` resolve
    meaningfully) until publication actually completes. Every probe that
    drives a real ``engine.loadSave`` call must wait for this before
    touching the loaded state.

    ``request_id``, if given, requires the observed status's own ``id``
    to match — otherwise (the default, preserving every pre-existing
    caller's behavior unchanged) the first terminal phase observed is
    accepted regardless of which load produced it. Pass the id captured
    from ``engine.getLoadStatus()`` immediately after ``engine.loadSave``
    accepts the request (round-5 review: without this, a stale terminal
    status left behind by an EARLIER load could satisfy the wait before
    the load this call is actually waiting for ever reaches one).

    Returns ``(published: bool, status: dict | None)`` — ``status`` is
    the last observed ``engine.getLoadStatus()`` table matching
    ``request_id`` when given (``None`` only if the debug console never
    returned a matching one at all). ``True`` means a fully reconciled
    ``LoadPublished``; both failure dispositions (issue #1204) return
    ``False`` with the status that names which one.
    """
    deadline = time.time() + seconds
    last = None
    while time.time() < deadline:
        status = send_json(port, "return engine.getLoadStatus()")
        if isinstance(status, dict) and (request_id is None
                                          or status.get("id") == request_id):
            last = status
            phase = status.get("phase")
            if phase == "LoadPublished":
                return True, status
            if phase == "LoadFailed":
                return False, status
            # Issue #1204: the session published, but a Lua
            # ``onSaveLoaded`` reconciliation callback raised, so it is
            # only partially reconciled. Terminal (so this never spins
            # to the deadline) and UNSUCCESSFUL (so no probe builds
            # assertions on a session whose Lua-side state was never
            # finished reconciling); ``status`` carries the per-module
            # ``reconciliationFailures`` breakdown for the caller's
            # failure message.
            if phase == "LoadReconciliationFailed":
                return False, status
        time.sleep(interval)
    return False, last


def capture_request_id(port: int, status_lua: str, seconds: float = 5.0,
                        interval: float = 0.1):
    """Poll a ``getSaveStatus``/``getLoadStatus``-shaped debug-console
    call (``status_lua``, e.g. ``"return engine.getLoadStatus()"``) until
    it returns a status TABLE with an ``id`` field, tolerating transient
    non-table responses.

    A load transaction REPLACES the whole live session (#763) -- a
    ``getLoadStatus()`` query queued right as that replacement lands can
    come back as the literal string ``"REJECTED: a load transaction
    replaced the session while this command was queued"`` instead of a
    status table (observed live: a single immediate query right after
    ``engine.loadSave`` returns ``true`` is NOT reliable for a fast,
    e.g. tiny-world, load). Retrying past that transient rejection is
    what makes request-id capture reliable for both saves and loads.

    Returns the captured ``id`` (an int), or ``None`` on timeout.
    """
    deadline = time.time() + seconds
    while time.time() < deadline:
        status = send_json(port, status_lua)
        if isinstance(status, dict) and status.get("id") is not None:
            return status.get("id")
        time.sleep(interval)
    return None


def wait_save_complete(port: int, request_id: int, seconds: float = 60.0,
                        interval: float = 0.2):
    """Poll ``engine.getSaveStatus()`` until the save identified by
    ``request_id`` reaches a terminal phase.

    ``engine.saveWorld`` only ACCEPTS the request synchronously (issue
    #758's ``SaveEncoding`` window runs the real encode + disk write
    AFTER the capture barrier already released, so other state owners
    can resume before the file is actually durable) -- the appearance of
    the save file on disk is a proxy for completion, not the authoritative
    signal engine.getSaveStatus() itself is. ``SaveCaptureComplete`` is
    the terminal phase 'finishSave'/'failSave' set once encoding AND disk
    I/O both finish (see Engine.Save.Barrier), so waiting for it (or the
    terminal 'SaveFailed') ties inspection to a completed, request-
    specific save boundary rather than a same-named-but-possibly-stale
    status left behind by an EARLIER save.

    Returns ``(succeeded: bool, status: dict | None)`` -- ``status`` is
    the last observed ``engine.getSaveStatus()`` table for THIS
    ``request_id`` (``None`` if the console never returned a status for
    it at all before the deadline).
    """
    deadline = time.time() + seconds
    last = None
    while time.time() < deadline:
        status = send_json(port, "return engine.getSaveStatus()")
        if isinstance(status, dict) and status.get("id") == request_id:
            last = status
            phase = status.get("phase")
            if phase == "SaveCaptureComplete":
                return True, status
            if phase == "SaveFailed":
                return False, status
        time.sleep(interval)
    return False, last


# --------------------------------------------------------------------------
# Common bootstrap: AI scripts, worlds, acolytes
# --------------------------------------------------------------------------
#: The unit AI/stat script stack the loading screen would load in the GUI.
AI_STACK = (
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
)


def load_ai_stack(port: int, scripts=AI_STACK) -> None:
    """Load the unit stat/resource/AI Lua stack (headless has no loading screen)."""
    for path, z in scripts:
        send(port, f"engine.loadScript('{path}', {z}); return 'ok'")


def clear_find_water(port: int, uid: int, seconds: float = 10.0) -> bool:
    """Retire a freshly spawned acolyte's standing ``find_water`` goal.

    Without this, the goal's search utility can out-compete the behavior
    under test (and walk the unit off a cliff on a waterless arena).
    """
    return poll_until(seconds, lambda: send(
        port,
        f"local ai = require('scripts.unit_ai'); "
        f"local s = ai.getState({uid}); "
        f"if not s then return false end; "
        f"ai.markGoalAccomplished(s, 'find_water'); return true") == "true") is not None


def spawn_acolyte(port: int, x: float, y: float, unit: str = "acolyte",
                  clear_water: bool = True) -> int:
    """Spawn a unit and (by default) clear its ``find_water`` goal; return its uid."""
    raw = send(port, f"return unit.spawn('{unit}', {x}, {y})")
    try:
        uid = int(float(raw))
    except (ValueError, TypeError):
        sys.exit(f"unit.spawn failed: {raw!r}")
    if clear_water and not clear_find_water(port, uid):
        sys.exit(f"unit {uid} never got AI state (find_water clear failed)")
    return uid


def init_world(port: int, name: str = "probe", seed: int = 42, size: int = 64,
               plates: int = 3, show: bool = True, timeout: float = 190.0) -> None:
    """``world.init`` + block on ``world.waitForInit`` (+ ``world.show``)."""
    send(port, f"world.init('{name}', {seed}, {size}, {plates})", expect_result=False)
    send(port, f"return world.waitForInit({int(timeout)})", timeout=timeout + 10)
    if show:
        send(port, f"world.show('{name}')", expect_result=False)


def init_arena(port: int, name: str = "arena", show: bool = True,
               timeout: float = 60.0) -> None:
    """``world.initArena`` + block on ``world.waitForInit`` (+ ``world.show``)."""
    send(port, f"world.initArena('{name}')", expect_result=False)
    send(port, f"return world.waitForInit({int(timeout)})", timeout=timeout + 10)
    if show:
        send(port, f"world.show('{name}')", expect_result=False)


# --------------------------------------------------------------------------
# On-screen targeting (#1286)
# --------------------------------------------------------------------------
# THREE screen spaces, and an interaction probe crosses all three on every
# scenario. They are equal under ``--offscreen`` (Engine.Loop.Frame writes
# the same offscreen extent into both refs), so a run cannot demonstrate a
# difference -- the conversions below exist so the code is correct by
# construction rather than by coincidence:
#
#   (a) ``world.pickTile`` normalizes the pixel by the WINDOW size but takes
#       its aspect from the FRAMEBUFFER (World.Render.HitTest.pickWorldTile,
#       called from Engine.Scripting.Lua.API.WorldQuery.Pick) -> pass it
#       WINDOW pixels.
#   (b) ``building.hitTestAt`` / ``unit.hitTestAt`` / ``unit.hitTestInRect``
#       use the WINDOW size for both pixel and aspect (Building.HitTest,
#       Unit.HitTest) -> scan in WINDOW space. Their docstrings say
#       "framebuffer"; the code says window, and the code is what runs.
#   (c) ``input.click`` / ``input.moveMouse`` take FRAMEBUFFER pixels and
#       convert to window space themselves
#       (Engine.Scripting.Lua.API.InputInject.withWindowCoords) -> convert
#       a located WINDOW pixel back to FRAMEBUFFER space before clicking.


def viewport(port: int, fallback: tuple[int, int] | None = None) -> dict:
    """Read BOTH screen spaces off the live engine.

    Never derive them from a ``--size`` CLI string: that is the requested
    offscreen extent, not what the engine ended up with. ``fallback`` is
    used only if the console cannot answer at all.
    """
    got = send_json(port, "local ww, wh = engine.getWindowSize();"
                          " local fw, fh = engine.getFramebufferSize();"
                          " return {ww=ww, wh=wh, fw=fw, fh=fh}")
    fw0, fh0 = fallback if fallback else (0, 0)

    def _one(key: str, default: int) -> int:
        v = got.get(key) if isinstance(got, dict) else None
        try:
            iv = int(float(v))
        except (TypeError, ValueError):
            return default
        return iv if iv > 0 else default

    win_w, win_h = _one("ww", fw0), _one("wh", fh0)
    return {"win_w": win_w, "win_h": win_h,
            "fb_w": _one("fw", win_w or fw0), "fb_h": _one("fh", win_h or fh0)}


def win_to_fb(vp: dict, x: float, y: float) -> tuple[int, int]:
    """WINDOW-space pixel -> the FRAMEBUFFER-space pixel ``input.*`` wants."""
    win_w, win_h = vp.get("win_w") or 0, vp.get("win_h") or 0
    if win_w <= 0 or win_h <= 0:
        return int(round(x)), int(round(y))
    return (int(round(x * (vp.get("fb_w") or win_w) / win_w)),
            int(round(y * (vp.get("fb_h") or win_h) / win_h)))


def camera_state(port: int) -> dict:
    """``camera`` position/zoom/z-slice/z-tracking in one round trip."""
    got = send_json(port, "local x, y = camera.getPosition();"
                          " return {x=x, y=y, zoom=camera.getZoom(),"
                          " zSlice=camera.getZSlice(),"
                          " zTracking=camera.getZTracking()}")
    return got if isinstance(got, dict) else {}


def pin_camera_to_tile(port: int, gx: int, gy: int, z: int) -> bool:
    """Point the camera at ``(gx, gy)`` and pin the z-slice to ``z``.

    ``camera.goToTile`` alone is NOT enough to put a tile on screen, and
    this is the whole of issue #1286. It sets ``camZTracking = True``
    (Engine.Scripting.Lua.API.Camera), and while tracking is on the render
    loop rewrites ``camZSlice`` to ``surfaceElev + surfaceHeadroom`` on
    EVERY frame (World.Render; ``surfaceHeadroom`` is 25). Both the render
    and every hit test then offset a tile at height ``z`` by
    ``(z - zSlice) * tileSideHeight``, so with a 25-level headroom the
    surface is drawn ~0.625 world units below the camera centre -- more
    than the 0.5 half-height ``goToTile``'s own zoom of 0.5 gives, i.e.
    the tile the camera "went to" is pushed clean off the bottom of the
    viewport.

    Pinning the slice to the TARGET's own z makes that offset exactly
    zero, so the target lands at the screen centre. Order matters:
    ``goToTile`` re-enables tracking, so the two pins must follow it,
    every time it is called. (This is what
    ``tools/item_list_widget_probe.py`` was already reaching for with a
    bare ``camera.setZSlice`` -- z-tracking overwrote it on the next
    frame.)

    Verified and retried rather than fired once: the render thread reads
    ``camZTracking`` at the top of a frame and writes ``camZSlice`` at the
    bottom, so a pin landing inside that window is discarded by a frame
    that had already decided to track. Once tracking really is off the
    slice is stable -- at tile zoom the tile layer is fully opaque, so
    the fade-band clause that can re-arm tracking never fires.
    Returns whether the pin held.
    """
    for _ in range(3):
        send(port, f"camera.goToTile({gx}, {gy});"
                   f" camera.setZTracking(false); camera.setZSlice({int(z)});"
                   " return 'ok'")
        cam = camera_state(port)
        if cam.get("zTracking") is False and cam.get("zSlice") == int(z):
            return True
        time.sleep(0.2)
    return False


def set_paused(port: int, on: bool, settle: float = 0.3) -> None:
    """``engine.setPaused`` plus a short settle.

    NB ``unit.setFrozen`` is not an alternative: it only makes
    ``publishToRender`` skip the sim-derived update, so a "frozen" unit
    keeps walking while ``unit.getInfo`` reports where it was when the
    flag went up.
    """
    send(port, f"engine.setPaused({'true' if on else 'false'}); return 'ok'")
    time.sleep(settle)


def focus_and_locate(port: int, gx: int, gy: int, z: int, vp: dict, locate):
    """Pin the camera on ``(gx, gy)`` at height ``z``, then locate the
    target with ``locate()`` — a callable returning a WINDOW-space pixel
    or ``None``. Returns that pixel (or ``None``).

    Deliberately NOT an iterative ``world.pickTile`` convergence, which is
    what the probes used to do. Once the slice is pinned to the target's
    own z the placement is EXACT by construction: ``goToTile`` puts the
    camera on the tile's flat world position, and at ``relativeZ == 0``
    that position is also where the tile and anything standing on it are
    drawn. Correcting on top of that made targeting worse, twice over:

      * ``world.pickTile`` answers about TERRAIN, and it reports the
        first solid column walking DOWN from the slice. On a coastal
        ridge the centre pixel resolves the right tile at the WRONG
        height (measured: tile ``(48, -119)`` at ``z = -3`` while the
        target stood on it at ``z = 24``), and "correcting" toward that
        reading walked the camera far enough that the target left the
        viewport entirely;
      * even when it converged cleanly it converged to the TILE centre,
        which is not the sprite: it moved a 1x1 building's clickable box
        from straddling the centre pixel to ending 2 px above it.

    The simulation is expected to be PAUSED. The one thing still owed to
    the world thread is CHUNK LOADING, which a just-teleported camera can
    need, so a miss retries once with the simulation running.
    """
    pin_camera_to_tile(port, gx, gy, z)
    pixel = locate()
    if pixel is None:
        set_paused(port, False)
        time.sleep(1.0)
        pin_camera_to_tile(port, gx, gy, z)
        pixel = locate()
        set_paused(port, True)
    return pixel


def centred_within(vp: dict, pixel, fraction: float = 0.25) -> bool:
    """Is ``pixel`` within ``fraction`` of the smaller screen dimension of
    the screen centre?

    This is how a probe asks "did the camera actually centre on my
    target", and it is a better question than the tile pick it replaces:
    it is measured on the PRODUCTION hit test's own answer, so it is
    terrain-independent and cannot be satisfied by the camera looking at
    some other place that happens to sit over the same column. The
    tolerance is loose on purpose — a sprite is anchored to its tile's
    diamond bottom and extends upward, so its clickable box is offset
    from the tile centre by design.
    """
    if not pixel:
        return False
    cx, cy = vp["win_w"] / 2, vp["win_h"] / 2
    tol = min(vp["win_w"], vp["win_h"]) * fraction
    return abs(pixel[0] - cx) <= tol and abs(pixel[1] - cy) <= tol


def locate_building_pixel(port: int, bid: int, vp: dict, step: int = 4,
                           centre: tuple[int, int] | None = None):
    """WINDOW-space pixel where ``building.hitTestAt`` really reports
    ``bid``, searched outward from the screen centre; ``None`` if the
    building is nowhere on screen.

    Asks the SAME single-target hit test the right-click router uses
    (``scripts/init_context_menu.lua``'s ``tryBuildingMenu``), so it is
    the only question that predicts where the menu will actually go. A
    computed tile-centre pixel is not good enough -- a building's sprite
    is anchored to the diamond bottom, so the quad sits ABOVE the tile
    centre (measured: a 1x1 96x96 fixture's clickable box ends 2 px above
    the centre pixel it stands on).

    Runs engine-side in ONE debug-console statement: a per-pixel round
    trip over a window is thousands of them.
    """
    cx, cy = centre if centre else (vp["win_w"] // 2, vp["win_h"] // 2)
    sw, sh = vp["win_w"], vp["win_h"]
    # Walks each square ring's PERIMETER, never a filtered full square:
    # the filtered form re-tests the whole interior on every ring and
    # turns an O(pixels) scan into an O(pixels x radius) one.
    raw = send(port,
               f"local b, cx, cy, sw, sh, st = {bid}, {cx}, {cy}, {sw}, {sh}, {step};"
               " local function hit(x, y) return x >= 0 and y >= 0 and x <= sw"
               "  and y <= sh and building.hitTestAt(x, y) == b end;"
               " if hit(cx, cy) then return cx .. ',' .. cy end;"
               " local maxr = math.max(cx, sw - cx, cy, sh - cy);"
               " for r = st, maxr + st, st do"
               "  for dx = -r, r, st do"
               "   if hit(cx + dx, cy - r) then return (cx + dx) .. ',' .. (cy - r) end;"
               "   if hit(cx + dx, cy + r) then return (cx + dx) .. ',' .. (cy + r) end"
               "  end;"
               "  for dy = -r + st, r - st, st do"
               "   if hit(cx - r, cy + dy) then return (cx - r) .. ',' .. (cy + dy) end;"
               "   if hit(cx + r, cy + dy) then return (cx + r) .. ',' .. (cy + dy) end"
               "  end"
               " end; return 'none'", timeout=120.0).strip().strip('"')
    if "," not in raw:
        return None
    sx, sy = raw.split(",")
    return int(sx), int(sy)


def targeting_report(port: int, vp: dict, kind: str, tid: int,
                     site: tuple[int, int] | None = None,
                     extra: dict | None = None) -> str:
    """Everything needed to diagnose -- and REPRODUCE -- a localization
    failure (#1286 requirement 6).

    The target's own identity, grid position, grid elevation, activity and
    page; the camera's settled position, zoom, z-slice and z-tracking
    state; both screen spaces; what the tile resolution and the hit tests
    actually answered at the centre pixel; and the world's seed plus the
    fixture site this run happened to choose -- without those last two,
    nothing about a failing run can be reproduced, because both probes
    generate a world through the UI and then search outward for their own
    anchor sites.
    """
    info = send_json(port, f"return {kind}.getInfo({tid})")
    info = info if isinstance(info, dict) else {}
    cam = camera_state(port)
    cx, cy = vp["win_w"] // 2, vp["win_h"] // 2
    pick = send_json(port, f"return {{world.pickTile({cx}, {cy})}}")
    bhit = send(port, f"return building.hitTestAt({cx}, {cy})").strip()
    uhit = send(port, f"return unit.hitTestAt({cx}, {cy})").strip()
    seed = send(port, "return world.getSeed()").strip()
    defaults = send_json(port, "return world.getGenDefaults()")
    size = (defaults or {}).get("world_size") if isinstance(defaults, dict) else None
    # ACTIVITY IS ITS OWN VERB, not a `getInfo` field. `building.getInfo`
    # reports identity/position/gridZ/page and no activity at all, so
    # reading one off it emits a bare `None` and silently drops what
    # requirement 6 asks for. `building.getActivity` / `unit.getActivity`
    # are both registered, so the same call shape serves either kind.
    activity = send(port, f"return {kind}.getActivity({tid})").strip().strip('"')
    # `unit.getInfo` has no page either; units are implicitly scoped to
    # the active page (both hit tests filter on it), so report that.
    page = info.get("page") or send(port, "return world.getActiveWorldId()").strip()
    lines = [
        f"  [diag] target: {kind} #{tid} {info.get('displayName')!r}"
        f" at grid ({info.get('gridX')}, {info.get('gridY')})"
        f" gridZ={info.get('gridZ')} activity={activity!r}"
        f" page={page!r}",
        f"  [diag] world: seed={seed} gen-default size={size} chunks"
        + (f", fixture site {site}" if site else ""),
        f"  [diag] camera: pos=({cam.get('x')}, {cam.get('y')})"
        f" zoom={cam.get('zoom')} zSlice={cam.get('zSlice')}"
        f" zTracking={cam.get('zTracking')}",
        f"  [diag] window={vp['win_w']}x{vp['win_h']}"
        f" framebuffer={vp['fb_w']}x{vp['fb_h']}",
        f"  [diag] at window-space centre pixel ({cx}, {cy}):"
        f" world.pickTile={pick!r} building.hitTestAt={bhit!r}"
        f" unit.hitTestAt={uhit!r}",
    ]
    for k, v in (extra or {}).items():
        lines.append(f"  [diag] {k}: {v}")
    return "\n".join(lines)
