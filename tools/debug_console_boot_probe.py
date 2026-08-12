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

Every boot here is a fast failure or `--dump`: no world is generated, no
Vulkan is ever initialized (offscreen's `initializeVulkanOffscreen` runs
inside the engine action, which a listener failure never reaches), and
the whole probe is a handful of seconds. That is what makes it
CI-eligible.

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
     engine.quit() reachable over the console, clean exit 0.

Usage:
  python3 tools/debug_console_boot_probe.py

Exit 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import atexit
import json
import os
import signal
import socket
import subprocess
import sys
import time

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
    two streams into one file.
    """
    cmd = ["cabal", "run", "-v0", "exe:synarchy", "--", *extra_args]
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


def send_quit(port: int, hold: float) -> None:
    """Send `engine.quit()` and hold the connection open for up to
    `hold` s, draining whatever the server writes back.

    Draining matters for a reason that is TCP, not timing: the server
    greets every client with a banner, so a client that closes without
    reading it still has unread data queued — and closing THEN makes the
    kernel send RST rather than FIN. The engine's console handler has no
    handler for that (`DebugServer.hs` `handleClient` re-raises through
    `finally`), so it dies with an uncaught `recv: ECONNRESET` on stderr.
    Reading first means this side always closes cleanly.

    That is NOT, however, what makes this check reliable — measured, not
    assumed. Under the CPU load that reproduces the CI failure, holding
    the connection alone still hung 3/3; it is the caller's RETRY that
    recovers it (5/5). Why the engine ignores the first `engine.quit()`
    for tens of seconds under contention, and then acts on a second one
    promptly, is NOT explained by this probe — see the tracking issue.
    """
    with socket.create_connection(("127.0.0.1", port), timeout=10) as s:
        s.sendall(b"engine.quit()\n")
        # Read until the peer goes away (engine exiting closes it). Any
        # banner/echo the server wrote is consumed here, so this side
        # closes cleanly with an empty queue and never emits an RST.
        s.settimeout(hold)
        try:
            while s.recv(4096):
                pass
        except (TimeoutError, socket.timeout, OSError):
            pass


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

    `proc` is `cabal`, not the engine it spawns, so `proc.kill()` alone
    reparents a live engine to init and leaves it holding the listener —
    which then fails EVERY later boot on that port with a bind error
    that looks nothing like the original fault. Observed exactly that
    way (a `synarchy --headless` with PPID 1) while diagnosing this
    check.

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
            ["cabal", "run", "-v0", "exe:synarchy", "--", "--headless",
             "--port", str(port)],
            stdout=fo, stderr=fe, cwd=REPO_ROOT,
            # Own process group, so cleanup can reach the engine child
            # without naming it and without touching anything else.
            start_new_session=True)
    _LAUNCHED.append(proc)
    try:
        deadline = time.time() + 180.0
        ready = False
        while time.time() < deadline:
            if f"READY port={port}" in open(log_out).read():
                ready = True
                break
            if proc.poll() is not None:
                break
            time.sleep(0.3)
        if not ready:
            return check("successful bind", False,
                         f"no READY on stdout; see {log_out} / {log_err}")
        # The console is the surface this whole issue is about: prove it
        # actually answers, not merely that a marker was printed.
        #
        # Two attempts. This is the part that actually removes the
        # flake: under load the engine ignores the first quit for tens of
        # seconds (hung 3/3 with the connection held; 5/5 pass with this
        # retry), and from out here a dropped command and a starved one
        # are indistinguishable. The retry costs nothing when the first
        # send worked, and a SECOND failure is reported rather than
        # retried away — that would be a genuine console regression.
        rc = None
        for attempt in (1, 2):
            send_quit(port, hold=QUIT_HOLD)
            try:
                rc = proc.wait(timeout=QUIT_TIMEOUT)
                break
            except subprocess.TimeoutExpired:
                if attempt == 2:
                    return check(
                        "successful bind", False,
                        f"still running {2 * QUIT_TIMEOUT:.0f}s after two "
                        f"engine.quit() sends; see {log_out} / {log_err}")
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
        # `proc` is cabal; the engine is its child and outlives it.
        kill_process_tree(proc)
        if proc in _LAUNCHED:
            _LAUNCHED.remove(proc)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=DEFAULT_PORT,
                    help="base port for the two checks that need a real one")
    args = ap.parse_args()
    install_cleanup_handlers()
    # `--port` is a BASE: check 4 binds it and check 7 binds port + 1.
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
    ]
    passed = all(results)
    print(f"\n  {'PASS' if passed else 'FAIL'}: required-debug-console boot contract"
          + ("" if passed else " — see failures above"))
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
