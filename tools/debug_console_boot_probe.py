#!/usr/bin/env python3
"""Required-debug-console boot contract for --headless/--offscreen (#1190).

`--headless` and `--offscreen` have no window: the debug TCP console is
their ONLY interactive control surface. Before #1190 neither mode failed
when that listener never came up — a `Left` from `startDebugServer`
became a warning plus an inert `TQueue` nothing ever fed, and port 0
(issue #46's "no TCP listener at all" sentinel for `--dump`) was honoured
for any caller, because `startDebugServer` sees a port number and no boot
mode. The result was a live process holding every worker thread it went
on to start, with no `READY` line for CLAUDE.md's documented boot wait to
ever match and no reachable `engine.quit()`.

This is the PROCESS half of the gate; the pure policy half (the per-mode
classification, the port-0 dispatch, the diagnostic's wording) is hspec
`--match "debug-console listener policy"`. Nothing in-process can assert
that another process exited, which is why both halves exist.

Since #1365 this is ALSO the blocking CI gate for `scripts/ui/*`: check 8
makes a normal headless boot's widget-module loading observable, which is
what lets `tools/ci_probes.py` select this one probe for a widget-kit
change instead of the whole CI-eligible set.

No boot here generates a world and none initializes Vulkan (offscreen's
`initializeVulkanOffscreen` runs inside the engine action, which a
listener failure never reaches): every check is a fast failure, a
`--dump` of a 16-tile region, or a normal headless boot that quits as
soon as it has answered. That is what makes it CI-eligible.

Checks:
  1. --headless --port 0: exits non-zero, no READY on stdout, and stderr
     names the mode, the effective port, and port 0's own reason (never
     a bind error it never attempted).
  2. --offscreen --port 0: the same, and additionally proves the partial
     worker set was torn down (the input thread starts BEFORE Lua in
     offscreen) and that offscreen Vulkan was never initialized.
  3. --headless --port -1: a real `Left` from the listener (an invalid
     service name), reported as a bind failure carrying the socket
     layer's own error text.
  4. --headless / --offscreen on an OCCUPIED port: the other real `Left`
     — an address already in use, which is the failure an operator
     actually hits. The probe binds and listens on the port itself, so
     this is a genuine EADDRINUSE, not a simulated one.
  5. Cleanup evidence, on every failing case above: the OS reclaims
     threads and descriptors at process exit, so "the process is gone"
     can never show that the pre-thread Lua state was closed or that an
     already-started input worker was stopped. Each cleanup step emits
     its own stderr line AS it runs, and the worker count is asserted
     exactly — 0 for headless (Lua is its first worker), 1 for offscreen
     (the input thread) — so a teardown that silently stopped doing the
     work fails here rather than passing on a vague substring.
  6. --dump is unchanged (#46): exit 0, valid JSON on stdout, the
     `READY port=0` marker still on stderr, and no failure diagnostic.
     Dump is the mode the port-0 sentinel belongs to.
  7. A successful bind in --headless is unchanged: READY on stdout,
     engine.quit() reachable over the console, clean exit 0. Its first
     post-READY command stays `engine.quit()` (issue #1283) — check 8
     therefore uses its OWN boots rather than querying this one first.
  8. Normal-boot widget-module loading is observable and FAILURE-SENSITIVE
     (#1365): a successful boot alone is not evidence that the widget kit
     loaded, because `Engine.Scripting.Lua.Script.callModuleFunction` and
     `engine.loadScript` both log and discard a Lua error, after which the
     boot still binds, prints READY and exits 0. Three real boots:
       8a. the repository's own resource root: every module in
           COVERED_UI_MODULES is present in `package.loaded`, and the boot
           logged no Lua load/init failure at all.
       8b. an alternate resource root whose `scripts/ui/focus_indicator.lua`
           raises after it has already self-registered into
           `package.loaded`: the gate must FAIL and must NAME that module.
           Its own `package.loaded` entry stays a live table, so this is the
           case the presence half alone cannot see.
       8c. an alternate resource root where a covered module is no longer
           required anywhere in the boot: the gate must FAIL naming it,
           with NO failure logged at all — the case the log half alone
           cannot see.
     8b and 8c are what make each half of check 8a's signal load-bearing.

Usage:
  python3 tools/debug_console_boot_probe.py

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import atexit
import json
import os
import re
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time

import probe_engine

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# The user's graphical instance. Never bind it, never kill anything on
# it — same rule probelib.GUI_PORT enforces for every other probe.
GUI_PORT = 8008

# Clear of the GUI port 8008 and of run_probes.py's parallel range base.
DEFAULT_PORT = 9457

# Every failing boot must die on its own well inside this; a regression
# that leaves the process alive (the exact bug #1190 fixed) must FAIL
# fast here rather than hang the probe.
FAIL_TIMEOUT = 60.0

# Check 7's shutdown budget. `hold` keeps the quit connection open while
# the engine acts on the command (see send_quit for why closing early
# loses it); the wait is per ATTEMPT, and there are two.
QUIT_HOLD = 20.0
QUIT_TIMEOUT = 45.0

# What the console replies to an accepted engine.quit() (the built-in in
# Engine.Scripting.Lua.Thread.Console). Its presence is what separates
# "accepted and then discarded" (#1283) from "never read at all".
QUIT_ACK = "shutting down"

# How often check 7 looks for READY. Deliberately far below the startup
# window it is trying to land inside (see the poll loop's own comment).
READY_POLL = 0.02

# ---------------------------------------------------------------------------
# Check 8 (#1365): normal-boot widget-module loading.
# ---------------------------------------------------------------------------

# The `scripts.ui.*` modules a NORMAL (non-preview) headless boot actually
# loads, established by OBSERVATION rather than by listing the directory:
# `scripts/init_loader.lua` requires `scripts.ui.registry` on its
# non-preview branch, the registry pulls in the 13 widget modules directly,
# and the rest arrive through the boot's other loadScript'd modules.
#
# Four of the 32 tracked files under `scripts/ui/` are deliberately absent:
# `asset_browser`, `building_asset_view` and `unit_animation_view` are
# reached only through `scripts/preview_manager.lua`, and `view_teardown` is
# required inside function bodies rather than at a chunk's top level. Only a
# preview-only or lazily-required module may leave this list; anything else
# disappearing from it is the coverage regression check 8a exists to catch.
COVERED_UI_MODULES = (
    "bar", "box_textures", "broken_overlay", "button", "checkbox",
    "context_menu", "dropdown", "focus_indicator", "item_list", "label",
    "list", "panel", "quality_tier", "randbox", "random", "registry",
    "repair_status", "reserved_regions", "responsive", "scale", "scrollbar",
    "slider", "sprite", "tabbar", "text_wrap", "textbox", "toggle",
    "utf8_safe",
)

UI_MODULE_PREFIX = "scripts.ui."

# One line asking the console for every `scripts.ui.*` key the BOOT left in
# `package.loaded`. The console evaluates in the same `lua_State` the boot
# scripts ran in, and `game.init` completes before the listener is even
# attempted (`Engine.Scripting.Lua.Thread.luaStartup`), so this observes
# state the boot itself produced — it never loads anything on demand, which
# would report success for a module the boot never reached.
UI_MODULE_QUERY = (
    "local t={} "
    "for k in pairs(package.loaded) do "
    "if string.sub(k,1,11)=='scripts.ui.' then t[#t+1]=k end "
    "end "
    "table.sort(t) return table.concat(t,',')"
)

# What a Lua load/init failure looks like in the engine's own log, which
# `defaultLogConfig` writes to STDOUT at LevelInfo (`Engine.Core.Log.Types`).
# `loadModuleRef` failing anywhere in a require chain surfaces as the first;
# a `pcall`ed callback raising surfaces as the second
# (`Engine.Scripting.Lua.Script.callModuleFunctionReportingError`,
# `Engine.Scripting.Lua.API.Core`'s `engine.loadScript`).
#
# Scanned BROADLY rather than filtered down to `scripts/ui/` paths: a clean
# boot logs neither line, and an error raised inside a widget module does not
# always report a `scripts/ui/` file (the chunk that failed may be a caller
# deeper in the chain). Over-reporting here is the same fail-safe direction
# `tools/ci_probes.py` takes with an unclassified path.
LUA_LOAD_FAILURE_MARKS = ("Failed to load Lua script:", "Lua error in ")

# The four resource families `App.ResourceRoot` validates. `scripts` is the
# tree check 8's negatives mutate, so it is COPIED; `config` is copied too
# because a boot WRITES into it (`Engine.Core.Init.migrateLegacyConfig`
# materializes `*.local.yaml`, or #1937's `*.legacy-neutral.local.yaml` record),
# and symlinking it would mutate the developer's
# own gitignored runtime state. The two read-only families are symlinked —
# `doesDirectoryExist` follows links, and `assets/` is far too large to copy
# three times per run.
ALT_ROOT_COPIED = ("scripts", "config")
ALT_ROOT_LINKED = ("assets", "data")

# Check 8's console budget. A boot that has printed READY answers instantly;
# this only bounds a console that has stopped answering at all.
QUERY_TIMEOUT = 20.0

FAILURE_MARK = "requires a working debug console"
PORT_ZERO_REASON = "no TCP listener at all"
BIND_REASON = "failed to start"
LUA_CLEANUP = "boot cleanup: closed the Lua state"
INPUT_CLEANUP = "boot cleanup: stopped the input worker"
# Offscreen logs this to STDOUT the moment it enters its engine action,
# immediately before initializeVulkanOffscreen — so its absence is proof
# the boot never reached any GPU work.
OFFSCREEN_ENGINE_MARK = "Starting engine (offscreen)"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f"  ({detail})" if detail else ""))
    return ok


def run_cli(*extra_args: str, timeout: float = FAIL_TIMEOUT):
    """Run the engine to completion with SEPARATE stdout/stderr.

    The separation is load-bearing: #1190 requires the diagnostic on
    stderr and NO READY marker on stdout, and probelib.boot merges the
    two streams into one file. That separation is unaffected by which
    launcher `probe_engine.engine_command` picks (#1570).
    """
    cmd = probe_engine.engine_command(extra_args)
    return subprocess.run(cmd, capture_output=True, text=True,
                          timeout=timeout, cwd=REPO_ROOT)


def check_failed_boot(label: str, args: list[str], mode: str, port: int,
                      reason: str, expected_workers: int) -> bool:
    """One console-required boot that must fail cleanly.

    ``expected_workers`` is how many worker threads the mode had ALREADY
    started when it reached the listener — 0 for headless (Lua is its
    first worker), 1 for offscreen (the input thread) — asserted exactly,
    so a teardown that stops nothing cannot pass.
    """
    try:
        r = run_cli(*args)
    except subprocess.TimeoutExpired:
        return check(label, False,
                     f"process did not exit within {FAIL_TIMEOUT}s — the "
                     "listener failure left it running (the #1190 bug)")
    stderr, stdout = r.stderr, r.stdout
    problems = []
    if r.returncode == 0:
        problems.append(f"exit status {r.returncode} (want non-zero)")
    if "READY" in stdout:
        problems.append("a READY marker reached stdout")
    if FAILURE_MARK not in stderr or f"{mode} mode" not in stderr:
        problems.append("stderr does not name the selected mode")
    if f"port {port}" not in stderr and f"port is {port}" not in stderr:
        problems.append(f"stderr does not name the effective port {port}")
    if reason not in stderr:
        problems.append(f"stderr does not carry the {reason!r} cause")
    if LUA_CLEANUP not in stderr:
        problems.append("no evidence the pre-thread Lua state was closed")
    if f"boot cleanup: {expected_workers} worker thread(s) stopped" not in stderr:
        problems.append(f"did not report exactly {expected_workers} worker(s) stopped")
    if (INPUT_CLEANUP in stderr) != (expected_workers > 0):
        problems.append("input-worker teardown line disagrees with the "
                        "worker set this mode had actually started")
    if mode == "offscreen" and OFFSCREEN_ENGINE_MARK in stdout:
        problems.append("reached the engine action — offscreen Vulkan was initialized")
    return check(label, not problems,
                 f"rc={r.returncode} " + ("; ".join(problems) if problems
                                          else stderr.strip().splitlines()[0][:90]))


def check_port_zero() -> bool:
    print("1-2. port 0 in a console-required mode: refused before any socket")
    return all([
        check_failed_boot("headless --port 0", ["--headless", "--port", "0"],
                          "headless", 0, PORT_ZERO_REASON, 0),
        check_failed_boot("offscreen --port 0", ["--offscreen", "--port", "0"],
                          "offscreen", 0, PORT_ZERO_REASON, 1),
    ])


def check_invalid_service() -> bool:
    print("3. an unbindable port (-1): a real Left from the listener")
    return check_failed_boot("headless --port -1", ["--headless", "--port", "-1"],
                             "headless", -1, BIND_REASON, 0)


def check_occupied_port(port: int) -> bool:
    print(f"4. an OCCUPIED port ({port}): a real EADDRINUSE Left, both modes")
    holder = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # Deliberately no SO_REUSEADDR/SO_REUSEPORT here: the engine sets
    # ReuseAddr on its own socket, and a listening holder without
    # SO_REUSEPORT is what makes its bind genuinely fail.
    try:
        holder.bind(("127.0.0.1", port))
        holder.listen(4)
    except OSError as e:
        holder.close()
        return check(f"occupied port {port}", False, f"probe could not hold the port: {e}")
    try:
        return all([
            check_failed_boot(f"headless --port {port} (in use)",
                              ["--headless", "--port", str(port)],
                              "headless", port, BIND_REASON, 0),
            check_failed_boot(f"offscreen --port {port} (in use)",
                              ["--offscreen", "--port", str(port)],
                              "offscreen", port, BIND_REASON, 1),
        ])
    finally:
        holder.close()


def check_dump_unchanged() -> bool:
    print("6. --dump keeps issue #46's port-0 no-listener contract")
    try:
        r = run_cli("--dump", "--seed", "1", "--worldSize", "16",
                    "--region", "0,0,0,0", timeout=180.0)
    except subprocess.TimeoutExpired:
        return check("dump unchanged", False, "dump did not finish within 180s")
    problems = []
    if r.returncode != 0:
        problems.append(f"exit status {r.returncode} (want 0)")
    try:
        json.loads(r.stdout)
    except (json.JSONDecodeError, ValueError) as e:
        problems.append(f"stdout is not valid JSON: {e}")
    if "READY port=0" not in r.stderr:
        problems.append("the READY port=0 marker is gone from stderr")
    if FAILURE_MARK in r.stderr:
        problems.append("dump was refused its own port-0 sentinel")
    return check("dump unchanged", not problems,
                 f"rc={r.returncode} " + "; ".join(problems))


def send_quit(port: int, hold: float) -> str:
    """Send `engine.quit()`, hold the connection open for up to `hold` s,
    and return everything the server wrote back.

    The RETURN VALUE is the point: the console answers an accepted quit
    with `shutting down` before the engine acts on it (the built-in runs
    on this connection's own client thread), so it is the caller's proof
    that the command was accepted rather than lost. Issue #1283 was
    exactly an accepted-then-discarded quit, and only the reply
    distinguishes that from a command the engine never read.

    Draining before closing matters for a reason that is TCP, not
    timing: the server greets every client with a banner, so a client
    that closes without reading it still has unread data queued — and
    closing THEN makes the kernel send RST rather than FIN. The engine's
    console handler has no handler for that (`DebugServer.hs`
    `handleClient` re-raises through `finally`), so it dies with an
    uncaught `recv: ECONNRESET` on stderr. Reading first means this side
    always closes cleanly.
    """
    got = b""
    with socket.create_connection(("127.0.0.1", port), timeout=10) as s:
        s.sendall(b"engine.quit()\n")
        # Read until the peer goes away (engine exiting closes it). Any
        # banner/echo the server wrote is consumed here, so this side
        # closes cleanly with an empty queue and never emits an RST.
        s.settimeout(hold)
        try:
            while True:
                chunk = s.recv(4096)
                if not chunk:
                    break
                got += chunk
        except (TimeoutError, socket.timeout, OSError):
            pass
    return got.decode("utf-8", "replace")


# Every process group this probe started, so an INTERRUPTED run cleans
# up too. `start_new_session=True` is what makes the group killable as a
# unit, but it also detaches the child from signals aimed at this
# process — so a `timeout`-ed or Ctrl-C'd probe would otherwise leave the
# engine running and the port held, which is the very failure this
# cleanup exists to prevent. Caught that way while testing this change.
_LAUNCHED: list[subprocess.Popen] = []


def _cleanup_launched(signum=None, frame=None) -> None:
    for proc in list(_LAUNCHED):
        kill_process_tree(proc)
    _LAUNCHED.clear()
    if signum is not None:
        sys.exit(128 + signum)


def install_cleanup_handlers() -> None:
    atexit.register(_cleanup_launched)
    for sig in (signal.SIGINT, signal.SIGTERM):
        try:
            signal.signal(sig, _cleanup_launched)
        except (OSError, ValueError):
            pass


def kill_process_tree(proc: subprocess.Popen) -> None:
    """Kill the launched process AND its children.

    Run by hand, `proc` is `cabal`, not the engine it spawns, so
    `proc.kill()` alone reparents a live engine to init and leaves it
    holding the listener — which then fails EVERY later boot on that port
    with a bind error that looks nothing like the original fault.
    Observed exactly that way (a `synarchy --headless` with PPID 1) while
    diagnosing this check. Under the aggregate runner `proc` IS the
    engine (#1570), and the group kill below is correct for both: it
    reaches whatever the launch actually produced without naming it.

    The kill is by PROCESS GROUP, which is why the launch passes
    `start_new_session=True`: it reaches the engine child without
    naming it, needs no external tool (the CI image ships no `lsof`),
    and — unlike asking who holds the port — can only ever touch
    processes this probe itself started.
    """
    if proc.poll() is not None:
        return
    try:
        os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
    except (OSError, ProcessLookupError):
        # Group already gone, or never created; fall back to the direct
        # child so cleanup still does something.
        try:
            proc.kill()
        except OSError:
            return
    try:
        proc.wait(timeout=10)
    except subprocess.TimeoutExpired:
        pass


def check_successful_bind(port: int) -> bool:
    print(f"7. a successful headless bind on {port} behaves exactly as before")
    log_out = f"/tmp/debug_console_boot_probe_{port}.out"
    log_err = f"/tmp/debug_console_boot_probe_{port}.err"
    with open(log_out, "w") as fo, open(log_err, "w") as fe:
        proc = subprocess.Popen(
            probe_engine.engine_command(["--headless", "--port", str(port)]),
            stdout=fo, stderr=fe, cwd=REPO_ROOT,
            # Own process group, so cleanup can reach the engine child
            # without naming it and without touching anything else.
            start_new_session=True)
    _LAUNCHED.append(proc)
    try:
        # Poll TIGHTLY. This check is the regression gate for issue
        # #1283, whose window opens at the READY print and closes when
        # the main loop's startup handshake promotes the lifecycle — so
        # the gate only bites if the quit lands inside it. A 0.3 s poll
        # is longer than the window's own settle and would mostly send
        # the quit AFTER the handshake, quietly testing nothing.
        deadline = time.time() + 180.0
        ready = False
        while time.time() < deadline:
            if f"READY port={port}" in open(log_out).read():
                ready = True
                break
            if proc.poll() is not None:
                break
            time.sleep(READY_POLL)
        if not ready:
            return check("successful bind", False,
                         f"no READY on stdout; see {log_out} / {log_err}")
        # The console is the surface this whole issue is about: prove it
        # actually answers, not merely that a marker was printed.
        #
        # The FIRST accepted quit has to be the one that works.
        #
        # Issue #1283: a quit accepted during the startup window was
        # acked `shutting down` and then silently discarded, leaving an
        # engine that could no longer be stopped through its only
        # control surface. A second quit always worked (by then the
        # lifecycle had advanced), so retrying is precisely what hid the
        # bug — this check must never pass on the strength of one.
        #
        # So: assert the ack, then require a clean exit from that one
        # send. The second send below runs ONLY after this check has
        # already failed, purely to say whether a retry would have
        # worked — a diagnosis in the failure text, never a pass.
        ack = send_quit(port, hold=QUIT_HOLD)
        if QUIT_ACK not in ack:
            return check("successful bind", False,
                         f"console did not acknowledge the quit with "
                         f"{QUIT_ACK!r}; got {ack!r}")
        try:
            rc = proc.wait(timeout=QUIT_TIMEOUT)
        except subprocess.TimeoutExpired:
            send_quit(port, hold=QUIT_HOLD)
            try:
                proc.wait(timeout=QUIT_TIMEOUT)
                retry_note = ("a SECOND quit did shut it down — the first was "
                              "accepted and then discarded (issue #1283)")
            except subprocess.TimeoutExpired:
                retry_note = "a second quit did not shut it down either"
            return check(
                "successful bind", False,
                f"still running {QUIT_TIMEOUT:.0f}s after a quit the console "
                f"acknowledged with {QUIT_ACK!r}; {retry_note}; "
                f"see {log_out} / {log_err}")
        err = open(log_err).read()
        problems = []
        if rc != 0:
            problems.append(f"exit status {rc} after engine.quit() (want 0)")
        if FAILURE_MARK in err:
            problems.append("a successful bind still reported a listener failure")
        return check("successful bind", not problems, "; ".join(problems))
    except OSError as e:
        return check("successful bind", False, f"{type(e).__name__}: {e}")
    finally:
        # Run by hand `proc` is cabal and the engine is its child, which
        # outlives it; under the runner `proc` is the engine itself. The
        # group kill covers both.
        kill_process_tree(proc)
        if proc in _LAUNCHED:
            _LAUNCHED.remove(proc)


# ---------------------------------------------------------------------------
# Check 8 (#1365): the normal boot's widget-module loading, made observable.
# ---------------------------------------------------------------------------


class BootObservation:
    """What one normal headless boot revealed about its widget modules.

    ``error`` is set only when the boot never got far enough to observe
    anything (no READY, or a console that never answered); ``loaded`` and
    ``failures`` are meaningless then, and every caller checks it first.
    """

    def __init__(self, loaded: set, failures: list, error: str = ""):
        self.loaded = loaded
        self.failures = failures
        self.error = error

    def missing(self) -> list:
        return [m for m in COVERED_UI_MODULES
                if UI_MODULE_PREFIX + m not in self.loaded]

    def problems(self) -> list:
        """Both halves of the signal, as one list of human problems.

        The presence half alone cannot see a module that self-registered
        into `package.loaded` before raising; the log half alone cannot see
        a module the boot simply stopped requiring. Checks 8b and 8c each
        prove one of those, so both halves stay here.
        """
        if self.error:
            return [self.error]
        found = []
        gone = self.missing()
        if gone:
            found.append("covered widget module(s) absent from package.loaded: "
                         + ", ".join(gone))
        for line in self.failures:
            found.append("the boot logged a Lua load/init failure: " + line)
        return found


def console_command(port: int, command: str, read_for: float) -> str:
    """Send one console line and return everything the server wrote back.

    Drains before closing for the same TCP reason `send_quit` documents: an
    unread banner turns this side's close into an RST, which the engine's
    `handleClient` re-raises. Stops as soon as the second prompt arrives so
    a healthy boot costs no wall clock at all.
    """
    got = b""
    deadline = time.time() + read_for
    with socket.create_connection(("127.0.0.1", port), timeout=10) as s:
        s.sendall(command.encode("utf-8") + b"\n")
        s.settimeout(1.0)
        while time.time() < deadline and got.count(b"> ") < 2:
            try:
                chunk = s.recv(4096)
            except (TimeoutError, socket.timeout):
                continue
            except OSError:
                break
            if not chunk:
                break
            got += chunk
    return got.decode("utf-8", "replace")


def make_alt_resource_root(parent: str, mutate) -> str:
    """A resource root whose `scripts/` tree `mutate` may rewrite.

    `App.ResourceRoot` validates only that the four families exist as
    directories, so a copied `scripts/`+`config/` beside symlinked
    `assets/`+`data/` is a fully valid root — see ALT_ROOT_COPIED for why
    those two are the copied ones. `mutate` receives the copied scripts
    directory and must raise if its own anchor has moved, so a negative
    regression can never silently degrade into booting an unmodified tree.
    """
    root = tempfile.mkdtemp(prefix="widget_load_root_", dir=parent)
    for family in ALT_ROOT_COPIED:
        shutil.copytree(os.path.join(REPO_ROOT, family),
                        os.path.join(root, family), symlinks=True)
    for family in ALT_ROOT_LINKED:
        os.symlink(os.path.join(REPO_ROOT, family), os.path.join(root, family))
    mutate(os.path.join(root, "scripts"))
    return root


def break_focus_indicator(scripts_dir: str) -> None:
    """Make a covered module raise AFTER it self-registered (check 8b).

    `scripts/ui/focus_indicator.lua` is the one module under `scripts/ui/`
    that assigns itself into `package.loaded` before its body finishes, so a
    chunk error past that point leaves a live table behind and the presence
    half of check 8's signal reports it as loaded. That is exactly why the
    negative regression breaks THIS module.
    """
    path = os.path.join(scripts_dir, "ui", "focus_indicator.lua")
    with open(path) as f:
        source = f.read()
    anchor = 'package.loaded["scripts.ui.focus_indicator"] = M\n'
    if anchor not in source:
        raise RuntimeError(f"{path}: self-registration anchor is gone; the "
                           "partial-load regression would boot an unmodified "
                           "module and prove nothing")
    injected = anchor + 'error("probe: injected widget-module load failure")\n'
    with open(path, "w") as f:
        f.write(source.replace(anchor, injected, 1))


def drop_from_boot(module: str):
    """Stop the boot requiring one covered module, WITHOUT any error (8c).

    Every `require("scripts.ui.<module>")` in the copied tree becomes an
    empty table literal, so the boot runs exactly as before except that the
    module is never loaded and nothing at all is logged. That is the silent
    coverage drift the log half of the signal cannot see.
    """
    if module not in COVERED_UI_MODULES:
        raise RuntimeError(f"{module!r} is not a covered widget module, so "
                           "dropping it would prove nothing about check 8a")
    needle = f'require("{UI_MODULE_PREFIX}{module}")'

    def mutate(scripts_dir: str) -> None:
        patched = 0
        for base, _dirs, files in os.walk(scripts_dir):
            for name in files:
                if not name.endswith(".lua"):
                    continue
                path = os.path.join(base, name)
                with open(path) as f:
                    source = f.read()
                if needle not in source:
                    continue
                patched += source.count(needle)
                with open(path, "w") as f:
                    f.write(source.replace(needle, "{}"))
        if not patched:
            raise RuntimeError(f"no {needle} call site exists any more; the "
                               "silent-drift regression would boot an "
                               "unmodified tree and prove nothing")

    return mutate


def observe_widget_modules(port: int, resource_root: str = "",
                           label: str = "boot") -> BootObservation:
    """Boot headless, read back its widget-module state, and shut it down.

    One boot, one console command, one quit. The command runs AFTER READY,
    which is after `game.init` has returned (`luaStartup` calls it before it
    ever attempts the listener), so every boot-time require has already been
    attempted by the time this looks.
    """
    tag = re.sub(r"[^A-Za-z0-9]+", "_", label).strip("_")
    log_out = f"/tmp/debug_console_boot_probe_ui_{tag}_{port}.out"
    log_err = f"/tmp/debug_console_boot_probe_ui_{tag}_{port}.err"
    args = ["--headless", "--port", str(port)]
    if resource_root:
        args += ["--resource-root", resource_root]
    with open(log_out, "w") as fo, open(log_err, "w") as fe:
        proc = subprocess.Popen(
            probe_engine.engine_command(args),
            stdout=fo, stderr=fe, cwd=REPO_ROOT, start_new_session=True)
    _LAUNCHED.append(proc)
    try:
        deadline = time.time() + FAIL_TIMEOUT
        ready = False
        while time.time() < deadline:
            with open(log_out) as f:
                if f"READY port={port}" in f.read():
                    ready = True
                    break
            if proc.poll() is not None:
                break
            time.sleep(READY_POLL)
        if not ready:
            return BootObservation(set(), [], f"no READY on stdout; see "
                                              f"{log_out} / {log_err}")
        reply = console_command(port, UI_MODULE_QUERY, QUERY_TIMEOUT)
        loaded = set(re.findall(r"scripts\.ui\.[A-Za-z0-9_]+", reply))
        if not loaded:
            return BootObservation(set(), [], f"the console answered the "
                                              f"package.loaded query with no "
                                              f"module at all: {reply!r}")
        send_quit(port, hold=QUIT_HOLD)
        try:
            proc.wait(timeout=QUIT_TIMEOUT)
        except subprocess.TimeoutExpired:
            # Check 7 owns the "a quit must be honoured" contract; here the
            # only thing that matters is that the process is GONE before the
            # log is read and before the next boot claims this same port.
            kill_process_tree(proc)
        # Read the log only once the process is gone, so a failure logged
        # late in the boot cannot be missed by racing it.
        text = ""
        for path in (log_out, log_err):
            with open(path) as f:
                text += f.read()
        failures = [ln.strip() for ln in text.splitlines()
                    if any(m in ln for m in LUA_LOAD_FAILURE_MARKS)]
        return BootObservation(loaded, failures)
    except OSError as e:
        return BootObservation(set(), [], f"{type(e).__name__}: {e}")
    finally:
        kill_process_tree(proc)
        if proc in _LAUNCHED:
            _LAUNCHED.remove(proc)


def check_widget_module_load(port: int) -> bool:
    """Check 8: the boot really loads the widget kit, and the gate can tell.

    Reuses check 7's port rather than claiming new ones: the three boots are
    strictly sequential and each is fully reaped before the next, and every
    port this probe binds must be covered by `main()`'s GUI-port guard.
    """
    print(f"8. normal-boot widget-module loading is observable on {port} "
          f"and fails when it breaks")
    good = observe_widget_modules(port, label="clean")
    problems = good.problems()
    ok_a = check(f"8a. all {len(COVERED_UI_MODULES)} covered scripts.ui.* "
                 f"modules loaded, no Lua failure logged",
                 not problems, "; ".join(problems))

    with tempfile.TemporaryDirectory(prefix="widget_load_probe_") as parent:
        try:
            partial_root = make_alt_resource_root(parent, break_focus_indicator)
            drift_root = make_alt_resource_root(parent, drop_from_boot("quality_tier"))
        except (OSError, RuntimeError) as e:
            check("8b/8c. negative regressions", False,
                  f"could not build an alternate resource root: {e}")
            return False

        broke = observe_widget_modules(port, partial_root, "partial")
        detail = []
        if broke.error:
            detail.append(broke.error)
        else:
            if not broke.problems():
                detail.append("the gate reported a clean boot even though a "
                              "covered widget module raised while loading")
            if not any("focus_indicator" in p for p in broke.problems()):
                detail.append("the gate did not NAME focus_indicator — only the "
                              "log half can, and this is the case the presence "
                              "half cannot see")
            if UI_MODULE_PREFIX + "focus_indicator" not in broke.loaded:
                detail.append("focus_indicator left no package.loaded entry, so "
                              "this boot no longer exercises the partial-load "
                              "trap it exists to cover")
        ok_b = check("8b. a covered module raising after it self-registered is "
                     "detected", not detail, "; ".join(detail))

        drift = observe_widget_modules(port, drift_root, "drift")
        detail = []
        if drift.error:
            detail.append(drift.error)
        else:
            if "quality_tier" not in drift.missing():
                detail.append("quality_tier still loaded despite every require "
                              "of it being removed")
            if not any("quality_tier" in p for p in drift.problems()):
                detail.append("the gate did not report the absent module")
            if drift.failures:
                detail.append("this boot logged a Lua failure, so it no longer "
                              "isolates the presence half: "
                              + "; ".join(drift.failures))
        ok_c = check("8c. a covered module silently dropped from the boot is "
                     "detected", not detail, "; ".join(detail))

    return all([ok_a, ok_b, ok_c])


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=DEFAULT_PORT,
                    help="base port for the checks that need a real one")
    args = ap.parse_args()
    install_cleanup_handlers()
    # `--port` is a BASE: check 4 binds it, and checks 7 and 8 bind
    # port + 1 (check 8 SEQUENTIALLY reuses check 7's, so #1365 widened
    # the probe's coverage without widening the ports it claims). This
    # probe therefore RESERVES two ports, base and base + 1, which is why
    # `probe_runner_registry.PROBE_PORT_SPANS` declares 2 for it — stride-1 parallel
    # allocation is NOT valid for it and handed the next probe in the
    # batch this one's second port (#1571).
    # Guarding only the base let `--port 8007` put check 7 on the GUI
    # port, where it would boot against the user's own running game and
    # then kill that process tree on the way out.
    used = (args.port, args.port + 1)
    if GUI_PORT in used:
        sys.exit(f"refusing to use port {GUI_PORT} (the GUI port): --port "
                 f"{args.port} binds {used[0]} and {used[1]}; pass a 9xxx port")

    results = [
        check_port_zero(),
        check_invalid_service(),
        check_occupied_port(args.port),
        check_dump_unchanged(),
        check_successful_bind(args.port + 1),
        check_widget_module_load(args.port + 1),
    ]
    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: required-debug-console boot contract"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
