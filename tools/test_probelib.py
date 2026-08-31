#!/usr/bin/env python3
"""Unit tests for probelib's shared primitives.

Two contracts live here:

* `probelib.send_json` and the no-local-copies rule (issue #1160), below.
* Where a probe's ENGINE comes from (issue #1570): under the aggregate
  runner it execs the one executable the runner already resolved, and run
  by hand it keeps the `cabal run` invocation probes have always used.
  That is what stops a `--jobs N` sweep putting N concurrent Cabal
  processes on one `dist-newstyle`, and requirement 3 of #1570 is the
  fallback staying intact.

Twenty-two probes each defined their own `jget` JSON wrapper over
`probelib.send` instead of calling `probelib.send_json`, which already
existed. The copies differed from the shared helper in four ways, none
of them documented, so a probe's result decoding depended on which
spelling its author happened to reach for. #1160 deleted every copy.

Two things keep that consolidated:

  * `send_json`'s three result cases are pinned here against a REAL
    socket speaking the debug console's reply protocol, so the transport
    (`send`) is exercised too -- no engine, no world, no GPU, and no
    reimplementation of the thing under test. An empty result being
    `None` is the one difference the old copies were observably wrong
    about (they returned `""`), so it is asserted directly rather than
    inferred.
  * A source guard fails if any `tools/*.py` grows a local JSON-decoding
    console wrapper again, and if any probe calls `send_json` without
    importing it from `probelib`.

Usage:
  python3 tools/test_probelib.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import _thread
import ast
import contextlib
import inspect
import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import time
import uuid
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import probe_engine  # type: ignore  # noqa: E402
import probe_resource_lock  # type: ignore  # noqa: E402
import probelib  # type: ignore  # noqa: E402
import run_probes  # type: ignore  # noqa: E402
from probelib import send, send_json  # type: ignore  # noqa: E402

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


# ---------------------------------------------------------------------
# A one-shot stand-in for the debug console: banner, one `"> <reply>"`
# line, then the trailing empty prompt -- the exact shape probelib.send
# parses.
# ---------------------------------------------------------------------
class FakeConsole:
    def __init__(self, reply: str):
        self.reply = reply
        self._srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self._srv.bind(("localhost", 0))
        self._srv.listen(1)
        self.port = self._srv.getsockname()[1]
        self._thread = threading.Thread(target=self._serve, daemon=True)

    def __enter__(self) -> "FakeConsole":
        self._thread.start()
        return self

    def __exit__(self, *_exc) -> None:
        self._srv.close()
        self._thread.join(timeout=5.0)

    def _serve(self) -> None:
        try:
            conn, _ = self._srv.accept()
        except OSError:
            return
        with conn:
            conn.settimeout(5.0)
            conn.sendall(b"synarchy debug console\n")
            try:
                conn.recv(4096)
            except (OSError, socket.timeout):
                return
            conn.sendall(f"> {self.reply}\n> ".encode())
            # Hold the connection open the way the real console does, so
            # `send` settles on an idle gap rather than on EOF.
            try:
                conn.recv(4096)
            except (OSError, socket.timeout):
                pass


def decode(reply: str, **kwargs):
    with FakeConsole(reply) as console:
        return send_json(console.port, "return probe()", timeout=5.0, **kwargs)


def raw(reply: str):
    with FakeConsole(reply) as console:
        return send(console.port, "return probe()", timeout=5.0)


# ---------------------------------------------------------------------
# send_json's documented result contract
# ---------------------------------------------------------------------
def test_valid_json_decodes() -> None:
    print("\n-- a JSON reply decodes to the Python value")
    expect(decode('{"gx":3,"gy":4}') == {"gx": 3, "gy": 4},
           "a JSON object should decode to a dict")
    expect(decode("[1,2,3]") == [1, 2, 3],
           "a JSON array should decode to a list")
    expect(decode("7") == 7, "a JSON number should decode to a number")
    expect(decode("true") is True, "a JSON true should decode to True")


def test_lua_nil_is_none() -> None:
    print("\n-- a Lua nil arrives as JSON null and decodes to None")
    expect(decode("null") is None,
           "`null` should decode to None, which is what the probes' "
           "`is None` checks read as \"the engine said nothing was there\"")


def test_empty_result_is_none_not_empty_string() -> None:
    print("\n-- an EMPTY result is None (the deleted jget returned \"\")")
    # A Lua empty string: the console prints `> ""` and `send` strips the
    # surrounding quotes, leaving nothing. This is exactly the reply
    # repair_ai_probe's `rs.suffix` check sees.
    expect(raw('""') == "",
           "`send` should return the empty string for a Lua empty string")
    expect(decode('""') is None,
           "`send_json` should map an empty result to None; a caller that "
           "must see \"\" has to call `send` directly")


def test_invalid_json_is_returned_as_text() -> None:
    print("\n-- text that is not JSON comes back AS TEXT, not as None")
    expect(decode('"ERR:no such recipe"') == "ERR:no such recipe",
           "a quoted Lua string should come back unquoted, not None")
    expect(decode("ERR:bare") == "ERR:bare",
           "unquoted non-JSON text should come back verbatim, not None")


def test_idle_is_reachable() -> None:
    print("\n-- callers can tune the console idle gap (the knob the local "
          "jget copies hid)")
    # `jget` called `send` positionally with three arguments and never
    # named `idle`, so no caller of a copy could reach probelib's one
    # console-read knob. Both halves are asserted: that the parameter
    # exists to be passed, and that passing it still decodes normally.
    expect("idle" in inspect.signature(send_json).parameters,
           "send_json should expose an `idle` parameter")
    expect(decode('{"ok":true}', idle=0.05) == {"ok": True},
           "send_json should accept an explicit idle and still decode")


# ---------------------------------------------------------------------
# Source guards: the copies must not come back
# ---------------------------------------------------------------------
def local_json_console_wrappers(root: Path = TOOLS) -> list[str]:
    """`tools/` functions that reimplement `probelib.send_json`.

    Structural, and name-agnostic on purpose -- `jget` was the spelling
    that proliferated, but a `getj`/`query`/`jsend` copy is the same
    defect. A function qualifies when it `json.loads` a `send` result
    AND the Lua it hands `send` -- argument 2, or keyword `lua`, and
    only that position -- is one of its own PARAMETERS: that pair is
    what makes it a general-purpose console decoder rather than a query
    helper. Deliberately not keyed on `return`, so a copy that guards
    the decode (`json.loads(raw) if raw else None`) or buries it in a
    branch counts the same.

    The parameter clause is what keeps this inside #1160's scope. A
    helper that decodes ONE fixed query it builds itself (`snap`,
    `measure`, `msummary`, `get_identity`) is a different, deliberately
    out-of-scope duplication -- see the issue's "Out of scope" section
    and docs/code_health_findings.md CH-129.
    """
    def send_calls(fn: ast.AST) -> list[ast.Call]:
        return [c for c in ast.walk(fn)
                if isinstance(c, ast.Call) and isinstance(c.func, ast.Name)
                and c.func.id == "send"]

    def is_json_loads(call) -> bool:
        return (isinstance(call, ast.Call)
                and isinstance(call.func, ast.Attribute)
                and call.func.attr == "loads"
                and isinstance(call.func.value, ast.Name)
                and call.func.value.id == "json")

    def param_names(fn) -> set[str]:
        a = fn.args
        return {p.arg for p in
                (*a.posonlyargs, *a.args, *a.kwonlyargs)}

    def lua_arg(call):
        """The Lua `send` was handed: argument 2, or keyword `lua`.

        Only that one position counts. Scanning every argument would
        catch a fixed-query helper that merely forwards its own
        `timeout` parameter, which is exactly the out-of-scope shape
        below.
        """
        if len(call.args) > 1:
            return call.args[1]
        for kw in call.keywords:
            if kw.arg == "lua":
                return kw.value
        return None

    def is_wrapper(fn) -> bool:
        params = param_names(fn)
        # `send` is handed one of this function's parameters as its Lua.
        parameterised = any(
            isinstance(lua, ast.Name) and lua.id in params
            for lua in (lua_arg(call) for call in send_calls(fn)))
        if not parameterised:
            return False
        # ...and that reply is what gets JSON-decoded.
        from_send = {t.id for node in ast.walk(fn)
                     if isinstance(node, ast.Assign)
                     and isinstance(node.value, ast.Call)
                     and isinstance(node.value.func, ast.Name)
                     and node.value.func.id == "send"
                     for t in node.targets if isinstance(t, ast.Name)}
        for node in ast.walk(fn):
            if not is_json_loads(node):
                continue
            arg = node.args[0] if node.args else None
            if isinstance(arg, ast.Name) and arg.id in from_send:
                return True
            if (isinstance(arg, ast.Call) and isinstance(arg.func, ast.Name)
                    and arg.func.id == "send"):
                return True
        return False

    offenders = []
    for path in sorted(root.glob("*.py")):
        if path.name == "probelib.py":
            continue
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in ast.walk(tree):
            if not isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                continue
            if is_wrapper(node):
                offenders.append(f"{path.name}:{node.lineno} {node.name}")
    return offenders


def test_no_local_json_console_wrapper() -> None:
    print("\n-- no tools/ module defines its own send+json.loads wrapper")
    offenders = local_json_console_wrappers()
    expect(offenders == [],
           f"these should call probelib.send_json instead: {offenders}")


# ---------------------------------------------------------------------
# Mutation tests: the guard has to CATCH a reintroduced copy, not just
# agree that today's tree is clean.
# ---------------------------------------------------------------------
# The deleted body verbatim, except that the name is spliced in: #1160's
# own acceptance check greps `tools/` for a local definition of that
# name and must keep finding none, and a fixture carrying the literal
# would answer it forever.
REINTRODUCED_JGET = """
import json
from probelib import send


def {name}(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')
""".format(name="j" + "get")

RENAMED_COPY = """
import json
from probelib import send


def fetch_json(port, command):
    return json.loads(send(port, command))
"""

FIXED_QUERY_HELPER = """
import json
from probelib import send

SNAP = "return debug.snapshot()"


def snap(port):
    raw = send(port, SNAP)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {"_raw": raw}
"""


GUARDED_COPY = """
import json
from probelib import send


def query(port, lua):
    raw = send(port, lua)
    return json.loads(raw) if raw else None
"""


KEYWORD_LUA_COPY = """
import json
from probelib import send


def jsend(port, command):
    raw = send(port, lua=command)
    return json.loads(raw)
"""


TIMEOUT_FIXED_QUERY = """
import json
from probelib import send

SNAP = "return debug.snapshot()"


def snap(port, timeout=10.0):
    raw = send(port, SNAP, timeout)
    return json.loads(raw)
"""


def in_temp_tree(name: str, source: str):
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        (root / name).write_text(source, encoding="utf-8")
        return local_json_console_wrappers(root=root)


def test_a_reintroduced_jget_is_caught() -> None:
    print("\n-- the exact helper #1160 deleted is caught if it comes back")
    found = in_temp_tree("regressed_probe.py", REINTRODUCED_JGET)
    expect(len(found) == 1 and found[0].endswith("jget"),
           f"expected the reintroduced jget to be flagged, got {found!r}")


def test_a_renamed_copy_is_caught() -> None:
    print("\n-- a copy under a different name is caught too")
    found = in_temp_tree("renamed_probe.py", RENAMED_COPY)
    expect(len(found) == 1 and found[0].endswith("fetch_json"),
           f"expected the renamed copy to be flagged, got {found!r}")


def test_a_guarded_copy_is_caught() -> None:
    print("\n-- a copy that guards the decode instead of returning it "
          "directly is caught")
    found = in_temp_tree("guarded_probe.py", GUARDED_COPY)
    expect(len(found) == 1 and found[0].endswith("query"),
           f"expected the guarded copy to be flagged, got {found!r}")


def test_a_keyword_lua_copy_is_caught() -> None:
    print("\n-- a copy that names `send`'s Lua by keyword is caught")
    found = in_temp_tree("keyword_probe.py", KEYWORD_LUA_COPY)
    expect(len(found) == 1 and found[0].endswith("jsend"),
           f"expected the keyword-lua copy to be flagged, got {found!r}")


def test_a_fixed_query_helper_is_not_caught() -> None:
    print("\n-- a helper decoding ONE fixed query is left alone "
          "(out of #1160's scope)")
    found = in_temp_tree("fixed_query_probe.py", FIXED_QUERY_HELPER)
    expect(found == [],
           f"a fixed-query helper should not be flagged, got {found!r}")


def test_a_timeout_forwarding_fixed_query_helper_is_not_caught() -> None:
    print("\n-- a fixed-query helper that forwards its own timeout is left "
          "alone (only `send`'s Lua position counts)")
    found = in_temp_tree("timeout_query_probe.py", TIMEOUT_FIXED_QUERY)
    expect(found == [],
           f"forwarding a `timeout` parameter is not passing the Lua, so "
           f"this should not be flagged, got {found!r}")


def test_send_json_callers_import_it() -> None:
    print("\n-- every tools/ module calling send_json imports it from "
          "probelib")
    offenders = []
    for path in sorted(TOOLS.glob("*.py")):
        if path.name == "probelib.py":
            continue
        source = path.read_text(encoding="utf-8")
        tree = ast.parse(source)
        calls = any(isinstance(n, ast.Call) and isinstance(n.func, ast.Name)
                    and n.func.id == "send_json" for n in ast.walk(tree))
        if not calls:
            continue
        imported = any(
            isinstance(n, ast.ImportFrom) and n.module == "probelib"
            and any(a.name == "send_json" for a in n.names)
            for n in ast.walk(tree))
        if not imported:
            offenders.append(path.name)
    expect(offenders == [],
           f"these call send_json without importing it: {offenders}")


def test_no_unused_send_json_import() -> None:
    print("\n-- no tools/ module imports send_json without calling it")
    offenders = []
    for path in sorted(TOOLS.glob("*.py")):
        if path.name in ("probelib.py", Path(__file__).name):
            continue
        tree = ast.parse(path.read_text(encoding="utf-8"))
        imported = any(
            isinstance(n, ast.ImportFrom) and n.module == "probelib"
            and any(a.name == "send_json" for a in n.names)
            for n in ast.walk(tree))
        if not imported:
            continue
        used = any(isinstance(n, ast.Name) and n.id == "send_json"
                   and not isinstance(n.ctx, ast.Store)
                   for n in ast.walk(tree))
        if not used:
            offenders.append(path.name)
    expect(offenders == [],
           f"these import send_json but never use it: {offenders}")


# ---------------------------------------------------------------------
# Where the engine comes from (#1570)
# ---------------------------------------------------------------------
FAKE_ENGINE = """\
#!/usr/bin/env python3
# Stands in for exe:synarchy: records its own argv, then prints the READY
# marker probelib.boot polls the log for, and stays up until killed.
import sys, time
with open(sys.argv[sys.argv.index("--argv-log") + 1], "w") as fh:
    fh.write("\\n".join(sys.argv))
print("READY port=%s" % sys.argv[sys.argv.index("--port") + 1], flush=True)
time.sleep(600)
"""

# The same stand-in, but slow to become READY, so "the caller already
# holds the handle while boot is still waiting" is a deterministic
# observation rather than a race against the child's first write.
SLOW_FAKE_ENGINE = """\
#!/usr/bin/env python3
import sys, time
with open(sys.argv[sys.argv.index("--argv-log") + 1], "w") as fh:
    fh.write("\\n".join(sys.argv))
time.sleep(2)
print("READY port=%s" % sys.argv[sys.argv.index("--port") + 1], flush=True)
time.sleep(600)
"""


class engine_env:
    """Set (or clear) the runner->probe executable variable for one case."""

    def __init__(self, value: str | None) -> None:
        self.value = value

    def __enter__(self):
        self._saved = os.environ.get(probe_engine.ENV_ENGINE_EXE)
        if self.value is None:
            os.environ.pop(probe_engine.ENV_ENGINE_EXE, None)
        else:
            os.environ[probe_engine.ENV_ENGINE_EXE] = self.value
        return self

    def __exit__(self, *exc):
        if self._saved is None:
            os.environ.pop(probe_engine.ENV_ENGINE_EXE, None)
        else:
            os.environ[probe_engine.ENV_ENGINE_EXE] = self._saved
        return False


def test_direct_mode_keeps_the_cabal_fallback() -> None:
    print("\n-- a probe run by hand still launches through `cabal run`")
    with engine_env(None):
        cmd = probe_engine.engine_command(["--headless", "--port", "9008"])
    expect(cmd == ["cabal", "run", "-v0", "exe:synarchy", "--",
                   "--headless", "--port", "9008"],
           f"the historical invocation is unchanged, prefix and order "
           f"(got {cmd})")
    with engine_env(""):
        empty = probe_engine.engine_command(["--dump"])
    expect(empty[:5] == ["cabal", "run", "-v0", "exe:synarchy", "--"],
           f"an EMPTY variable is 'nobody supplied one', not a launcher "
           f"(got {empty})")


def test_runner_mode_execs_the_resolved_binary() -> None:
    print("\n-- and under the runner it execs that binary, with no Cabal")
    with tempfile.TemporaryDirectory() as tmp:
        exe = Path(tmp) / "synarchy"
        exe.write_text("#!/bin/sh\nexit 0\n")
        exe.chmod(0o755)
        with engine_env(str(exe)):
            cmd = probe_engine.engine_command(
                ["--headless", "--port", "9008", "--arena"])
        expect(cmd[0] == str(exe), f"argv[0] is the resolved binary (got {cmd})")
        expect("cabal" not in cmd, f"and Cabal appears nowhere (got {cmd})")
        expect(cmd[1:] == ["--headless", "--port", "9008", "--arena"],
               f"the engine's own arguments and their ORDER are identical "
               f"to the fallback's (got {cmd})")


def test_an_unusable_supplied_executable_is_refused() -> None:
    print("\n-- an unusable supplied executable raises, never falls back")
    with tempfile.TemporaryDirectory() as tmp:
        cases = [
            ("relative", "dist-newstyle/synarchy"),
            ("missing", str(Path(tmp) / "never-built")),
        ]
        plain = Path(tmp) / "not-executable"
        plain.write_text("")
        plain.chmod(0o644)
        cases.append(("non-executable", str(plain)))
        for why, value in cases:
            with engine_env(value):
                try:
                    probe_engine.engine_command(["--headless"])
                except probe_engine.EngineExecutableError as error:
                    expect(value in str(error),
                           f"a {why} path is refused, naming it "
                           f"(got {error})")
                else:
                    expect(False, f"a {why} path was accepted, or silently "
                                  f"fell back to `cabal run`")


def test_boot_launches_the_supplied_executable() -> None:
    print("\n-- probelib.boot really launches it, with logging unchanged")
    with tempfile.TemporaryDirectory() as tmp:
        exe = Path(tmp) / "fake-synarchy"
        exe.write_text(FAKE_ENGINE)
        exe.chmod(0o755)
        argv_log = Path(tmp) / "argv.txt"
        log = Path(tmp) / "engine.log"
        port = 9457
        proc = None
        with engine_env(str(exe)):
            try:
                proc = probelib.boot(port, log=str(log),
                                      args=["--argv-log", str(argv_log)],
                                      ready_timeout=30.0)
            except SystemExit as leaving:
                expect(False, f"boot did not reach READY: {leaving}")
        try:
            expect(proc is not None and proc.poll() is None,
                   "the engine is up")
            expect(getattr(proc, "_probe_log", None) == str(log),
                   "and boot still records the log path it was given")
            expect("READY" in log.read_text(),
                   "which really holds the engine's merged output")
            argv = argv_log.read_text().splitlines() if argv_log.exists() else []
            expect(argv[:1] == [str(exe)],
                   f"the process launched IS the supplied binary (got {argv[:1]})")
            expect(argv[1:] == ["--headless", "--port", str(port),
                                "--argv-log", str(argv_log)],
                   f"with mode, --port and the caller's extra args in the "
                   f"same order as before (got {argv[1:]})")
        finally:
            if proc is not None:
                proc.kill()
                proc.wait(timeout=10)


def test_boot_registers_the_process_as_it_launches() -> None:
    print("\n-- probelib.boot hands the handle over before it waits for READY")
    # `boot` waits up to `ready_timeout` -- three minutes by default --
    # for READY, so a caller that only learns about the process from the
    # return value owns nothing for that whole span. An interrupt taken
    # in it used to strand a live engine holding the port with nothing
    # left holding its handle (#1682). `on_launch` closes that span.
    with tempfile.TemporaryDirectory() as tmp:
        exe = Path(tmp) / "slow-fake-synarchy"
        exe.write_text(SLOW_FAKE_ENGINE)
        exe.chmod(0o755)
        argv_log = Path(tmp) / "argv.txt"
        log = Path(tmp) / "engine.log"
        port = 9458
        seen: list = []
        proc = None

        def register(handle):
            # What the caller's teardown guard sees, and when: the
            # process must already be running, and READY must not be a
            # precondition for learning about it.
            seen.append((handle, handle.poll(),
                         "READY" in (log.read_text() if log.exists() else "")))

        with engine_env(str(exe)):
            try:
                proc = probelib.boot(port, log=str(log), ready_timeout=30.0,
                                     args=["--argv-log", str(argv_log)],
                                     on_launch=register)
            except SystemExit as leaving:
                expect(False, f"boot did not reach READY: {leaving}")
        try:
            expect(len(seen) == 1,
                   f"the callback fires exactly once (got {len(seen)})")
            if seen:
                handle, alive, ready = seen[0]
                expect(handle is proc,
                       "with the very handle boot goes on to return")
                expect(alive is None,
                       f"while the process is already running (poll {alive})")
                expect(ready is False,
                       "and before READY, which is the span that used to be "
                       "uncovered")
            expect(getattr(proc, "_probe_log", None) == str(log),
                   "and boot still records the log path it was given")
        finally:
            if proc is not None:
                proc.kill()
                proc.wait(timeout=10)


def test_an_interrupt_during_the_hand_off_kills_the_child() -> None:
    print("\n-- an interrupt mid-hand-off leaves no engine holding the port")
    # The child exists the moment `Popen` returns, but nothing
    # downstream knows about it until `on_launch` has completed. A
    # `KeyboardInterrupt` delivered in between used to escape `boot`
    # with a live engine holding the port and no handle anywhere — the
    # caller's teardown guard has nothing to dispose of, and the probe
    # then deletes the tree the engine is still writing into (#1682).
    #
    # The interrupt is injected AT the hand-off, which is where a
    # pending signal is actually delivered: the callback is the first
    # thing to run after the child exists.
    for label, blow_up in (("a Ctrl-C", KeyboardInterrupt),
                           ("a callback that itself fails",
                            lambda: RuntimeError("registration failed"))):
        with tempfile.TemporaryDirectory() as tmp:
            exe = Path(tmp) / "slow-fake-synarchy"
            exe.write_text(SLOW_FAKE_ENGINE)
            exe.chmod(0o755)
            argv_log = Path(tmp) / "argv.txt"
            log = Path(tmp) / "engine.log"
            seen: list = []

            def interrupt(handle):
                seen.append(handle)
                raise blow_up()

            raised: BaseException | None = None
            with engine_env(str(exe)):
                try:
                    probelib.boot(9459, log=str(log), ready_timeout=30.0,
                                  args=["--argv-log", str(argv_log)],
                                  on_launch=interrupt)
                except BaseException as exc:  # noqa: BLE001 - the point
                    raised = exc
            expect(len(seen) == 1,
                   f"[{label}] the hand-off really was reached, so this is "
                   f"not vacuous (got {seen})")
            expect(raised is not None and not isinstance(raised, SystemExit),
                   f"[{label}] the interrupt still ends the run rather than "
                   f"being swallowed (got {raised!r})")
            if seen:
                expect(seen[0].poll() is not None,
                       f"[{label}] and the child boot had already launched "
                       f"is dead, not left holding the port "
                       f"(poll {seen[0].poll()})")


def test_boot_without_a_callback_is_unchanged() -> None:
    print("\n-- probelib.boot's existing callers are untouched")
    signature = inspect.signature(probelib.boot)
    parameter = signature.parameters.get("on_launch")
    expect(parameter is not None and parameter.default is None,
           "on_launch is optional, so every probe that does not pass one "
           "behaves exactly as before")
    expect(list(signature.parameters)[:6]
           == ["port", "log", "args", "ready_timeout", "label", "mode"],
           f"and it was APPENDED, so no positional caller shifted "
           f"(got {list(signature.parameters)})")


def test_resolve_executable_builds_then_locates() -> None:
    print("\n-- resolve_executable builds first, then asks where it landed")
    with tempfile.TemporaryDirectory() as tmp:
        exe = Path(tmp) / "synarchy"
        exe.write_text("#!/bin/sh\nexit 0\n")
        exe.chmod(0o755)
        calls: list[tuple[str, ...]] = []

        def double(argv, cwd=None, capture_output=False, text=False):
            calls.append(tuple(argv))
            out = "" if "build" in argv else f"Warning: noise\n{exe}\n"
            return subprocess.CompletedProcess(tuple(argv), 0, out, "")

        resolved = probe_engine.resolve_executable(tmp, run=double)
        expect(calls == [("cabal", "build", "exe:synarchy"),
                         ("cabal", "list-bin", "exe:synarchy")],
               f"one unconditional freshness build, then one read-only "
               f"query (got {calls})")
        expect(resolved == str(exe),
               f"and the path is the LAST line, so a warning above it is "
               f"not mistaken for one (got {resolved})")

        def failing(argv, cwd=None, capture_output=False, text=False):
            return subprocess.CompletedProcess(tuple(argv), 1, "",
                                                "could not resolve deps")
        try:
            probe_engine.resolve_executable(tmp, run=failing)
        except probe_engine.EngineExecutableError as error:
            expect("could not resolve deps" in str(error),
                   f"a failed build raises, carrying Cabal's own reason "
                   f"(got {error})")
        else:
            expect(False, "a failed build did not raise")


# ---------------------------------------------------------------------
# Preparing the engine BEFORE the READY deadline (#1913)
#
# The direct path's `cabal run` fallback is a BUILD wearing an engine's
# argv, and `boot` used to start the READY clock the moment that child
# existed: a cold compile expired at 180 s reported as "engine never
# printed READY", against a `-v0` log with nothing in it, having killed
# only the `cabal` process and left its GHC descendants compiling.
#
# Everything below drives REAL processes — a stand-in `cabal` on PATH, a
# stand-in engine, real flocks in an isolated sticky scratch root — so
# process-group disposal is observed rather than asserted about a mock.
# ---------------------------------------------------------------------
#: A separate process that takes a `cabal-build` interest and holds it
#: until told to let go. Run as `python3 -c`, so it shares nothing with
#: this process but the module it imports — which is the point: a hold
#: taken in-process would be the same open file description and the
#: kernel would grant it to us again.
HOLDER_SRC = """\
import json, sys, time
from pathlib import Path
sys.path.insert(0, sys.argv[1])
import probe_resource_lock as lock
root, namespace, ready, release = sys.argv[2:6]
plan = json.loads(sys.argv[6])
hold = lock.acquire(exclusive=plan.get("exclusive", []),
                    shared=plan.get("shared", []),
                    namespace=namespace, root=Path(root),
                    purpose="test_probelib holder")
Path(ready).write_text("held")
deadline = time.time() + 120
while not Path(release).exists() and time.time() < deadline:
    time.sleep(0.02)
hold.release()
"""


FAKE_CABAL = r"""#!/usr/bin/env python3
# Stands in for `cabal`. Its whole behaviour is the JSON beside it, so a
# case picks a build that succeeds, fails, hangs, or leaves a descendant
# of its own behind, without a second copy of this script.
import json, os, subprocess, sys, time
from pathlib import Path

config = json.loads(Path(__file__).with_name("cabal.json").read_text())
with open(config["calls"], "a") as fh:
    fh.write(" ".join(sys.argv[1:]) + "\n")
step = sys.argv[1] if len(sys.argv) > 1 else ""
if step == "build":
    print(config.get("build_output", "compiling"), flush=True)
    if config.get("descendant"):
        # A GHC stand-in: a child of this process, in this process
        # group, that outlives it. Nothing but a group kill removes it.
        subprocess.Popen([
            sys.executable, "-c",
            "import os,sys,time\n"
            "open(sys.argv[1],'w').write(str(os.getpid()))\n"
            "time.sleep(600)\n",
            config["descendant"]])
        while not os.path.exists(config["descendant"]):
            time.sleep(0.02)
    if config.get("build_hangs"):
        time.sleep(600)
    sys.exit(config.get("build_status", 0))
if step == "list-bin":
    if config.get("locate_noise"):
        print(config["locate_noise"])
    print(config["engine"])
    sys.exit(0)
sys.exit(9)
"""

SILENT_ENGINE = """\
#!/usr/bin/env python3
# An engine that starts and stays up but never becomes READY: the other
# genuine boot failure, and the one an explicit `ready_timeout` bounds.
import time
time.sleep(600)
"""

DYING_ENGINE = """\
#!/usr/bin/env python3
# An engine that really starts and then fails: READY never arrives, but
# preparation had already succeeded, so this must NOT be diagnosed as a
# preparation problem.
import sys
print("boot failed: no Vulkan device", flush=True)
sys.exit(3)
"""


def proc_stat_state(raw: str) -> str | None:
    """The state letter in a Linux `/proc/<pid>/stat` line.

    Split out and pinned by a case below because macOS never reaches it:
    the `/proc` branch is the one CI actually runs, so a parsing mistake
    there would be invisible to every local run. The command field is
    parenthesised and may itself contain spaces AND parentheses, so the
    state is the first token after the LAST `)` rather than the third
    whitespace field.
    """
    fields = raw.rpartition(")")[2].split()
    return fields[0] if fields else None


def process_state(pid: int) -> str | None:
    """`pid`'s state letter, or None when there is no such entry.

    Read rather than probed. The descendant these cases watch is killed
    while it is an ORPHAN — its `cabal` parent is already dead and
    reaped — so it is reparented to whatever init the host provides, and
    it stays in the process table as a zombie until that init reaps it.
    macOS's launchd does so at once; a CI container's PID 1 need not,
    and on this repository's Linux image it does not.
    """
    stat_path = Path(f"/proc/{pid}/stat")
    if stat_path.exists():
        try:
            return proc_stat_state(stat_path.read_text())
        except OSError:
            return None
    done = subprocess.run(["ps", "-o", "state=", "-p", str(pid)],
                          capture_output=True, text=True)
    state = (done.stdout or "").strip()
    return state[:1] if state else None


def process_running(pid: int) -> bool:
    """True while `pid` is a live process rather than a corpse.

    `os.kill(pid, 0)` cannot answer this: it succeeds against a zombie,
    because the entry survives until somebody reaps it. A killed
    descendant awaiting its reaper is GONE for these cases — it holds
    nothing and compiles nothing — so the state decides, not the signal.
    """
    state = process_state(pid)
    return state is not None and state not in ("Z", "X", "x")


class PreparedScratch:
    """A stand-in `cabal` on PATH, a stand-in engine, an isolated lock root.

    The lock root is a throwaway directory with `/tmp`'s own mode --
    sticky and owned by us, which is what `probe_resource_lock`'s safety
    check insists on. Redirecting it is what keeps these cases away from
    the repository's live `cabal-build` lock: a case here must neither
    wait on a real sweep nor block one.
    """

    def __init__(self, *, engine: str = FAKE_ENGINE, **config) -> None:
        self.dir = Path(tempfile.mkdtemp(prefix="test_probelib_prepare_"))
        self.lock_root = Path(tempfile.mkdtemp(prefix="test_probelib_locks_"))
        os.chmod(self.lock_root, probe_resource_lock.SHARED_DIR_MODE)
        self.namespace = f"selftest{uuid.uuid4().hex[:12]}"
        self.engine = self.dir / "fake-synarchy"
        self.engine.write_text(engine)
        self.engine.chmod(0o755)
        self.calls = self.dir / "cabal-calls.txt"
        self.descendant_pid = self.dir / "descendant.pid"
        self.prepare_log = self.dir / "prepare.log"
        self.engine_log = self.dir / "engine.log"
        # Where `boot` puts preparation output: named after the engine
        # log it was given, so the operator finds the two together.
        self.boot_prepare_log = Path(f"{self.engine_log}.prepare")
        self.argv_log = self.dir / "argv.txt"
        document = {"calls": str(self.calls), "engine": str(self.engine)}
        document.update(config)
        (self.dir / "cabal.json").write_text(json.dumps(document))
        cabal = self.dir / "cabal"
        cabal.write_text(FAKE_CABAL)
        cabal.chmod(0o755)
        self._saved_path = os.environ.get("PATH", "")
        self._saved_root = probe_engine.PREPARE_LOCK_ROOT
        os.environ["PATH"] = f"{self.dir}{os.pathsep}{self._saved_path}"
        probe_engine.PREPARE_LOCK_ROOT = self.lock_root

    def cabal_calls(self) -> list[str]:
        if not self.calls.exists():
            return []
        return [line for line in self.calls.read_text().splitlines() if line]

    def describe_descendant(self) -> str:
        """The descendant's pid and observed state, for a failure message.

        A survivor in `S` was never signalled — a real disposal defect —
        while a `Z` would mean this check had regressed to reading a
        corpse as a live process. Naming the state is what tells those
        two apart from the log alone.
        """
        pid = self.descendant()
        if pid is None:
            return "no descendant was recorded"
        return f"pid {pid}, state {process_state(pid)!r}"

    def descendant(self) -> int | None:
        if not self.descendant_pid.exists():
            return None
        try:
            return int(self.descendant_pid.read_text().strip())
        except ValueError:
            return None

    def descendant_gone(self, seconds: float = 10.0) -> bool:
        """True once the marked descendant is no longer running."""
        pid = self.descendant()
        if pid is None:
            return False
        deadline = time.time() + seconds
        while time.time() < deadline:
            if not process_running(pid):
                return True
            time.sleep(0.1)
        return False

    def cleanup(self) -> None:
        os.environ["PATH"] = self._saved_path
        probe_engine.PREPARE_LOCK_ROOT = self._saved_root
        pid = self.descendant()
        if pid is not None:
            with contextlib.suppress(OSError):
                os.kill(pid, signal.SIGKILL)
        shutil.rmtree(self.dir, ignore_errors=True)
        shutil.rmtree(self.lock_root, ignore_errors=True)


def prepare(scratch: PreparedScratch, **kwargs):
    """`prepare_executable` against the scratch, with its own namespace."""
    kwargs.setdefault("timeout", 60.0)
    kwargs.setdefault("log_path", str(scratch.prepare_log))
    return probe_engine.prepare_executable(
        str(scratch.dir), namespace=scratch.namespace,
        lock_root=scratch.lock_root, **kwargs)


def test_a_killed_descendant_reads_as_gone_before_it_is_reaped() -> None:
    print("\n-- the disposal check reads process STATE, not a bare signal")
    # An orphan killed by a group signal stays in the process table
    # until its init reaps it; macOS does so at once, a CI container's
    # PID 1 need not. `os.kill(pid, 0)` succeeds against that corpse, so
    # a check built on it would call a killed descendant "still
    # compiling" on exactly the platform CI runs.
    child = subprocess.Popen([sys.executable, "-c", "import time\n"
                                                    "time.sleep(600)\n"])
    try:
        expect(process_running(child.pid),
               "a live process reads as running, so this is not vacuous")
        child.kill()
        deadline = time.time() + 10
        while process_running(child.pid) and time.time() < deadline:
            time.sleep(0.05)
        expect(not process_running(child.pid),
               "and a killed one reads as gone while still unreaped")
        signal_says_alive = True
        try:
            os.kill(child.pid, 0)
        except (ProcessLookupError, PermissionError):
            signal_says_alive = False
        expect(signal_says_alive or sys.platform == "darwin",
               "which is the whole point: the bare signal would still "
               "report it alive here")
    finally:
        child.wait(timeout=10)
    # The Linux branch, against the real file format. macOS takes the
    # `ps` branch, so nothing else here would catch a mistake in it.
    for raw, expected, why in (
            ("42 (cabal) Z 1 42 0 0 -1 4194560 0", "Z", "an ordinary name"),
            ("42 (ghc (stage 2)) S 1 42", "S",
             "a name containing spaces AND parentheses"),
            ("42 (x) R 1", "R", "a running process"),
            ("", None, "an empty read")):
        expect(proc_stat_state(raw) == expected,
               f"/proc stat parsing handles {why} "
               f"(got {proc_stat_state(raw)!r}, wanted {expected!r})")


def test_preparation_precedes_the_engine_launch() -> None:
    print("\n-- the executable is BUILT before the READY deadline exists")
    scratch = PreparedScratch(locate_noise="Warning: resolving dependencies")
    try:
        proc = None
        with engine_env(None):
            try:
                proc = probelib.boot(9461, log=str(scratch.engine_log),
                                     args=["--argv-log", str(scratch.argv_log)],
                                     ready_timeout=30.0)
            except SystemExit as leaving:
                expect(False, f"boot did not reach READY: {leaving}")
        try:
            expect(scratch.cabal_calls() == ["build exe:synarchy",
                                             "list-bin exe:synarchy"],
                   f"one freshness build then one read-only query, before "
                   f"any engine (got {scratch.cabal_calls()})")
            argv = (scratch.argv_log.read_text().splitlines()
                    if scratch.argv_log.exists() else [])
            expect(argv[:1] == [str(scratch.engine)],
                   f"and what boot LAUNCHED is the built binary, not "
                   f"`cabal run` (got {argv[:1]})")
            expect("cabal" not in argv,
                   f"so no Cabal process is inside the READY window at all "
                   f"(got {argv})")
            expect(scratch.boot_prepare_log.exists()
                   and "compiling" in scratch.boot_prepare_log.read_text(),
                   f"the build output reached a preparation log beside the "
                   f"engine log ({scratch.boot_prepare_log})")
            engine_log = scratch.engine_log.read_text()
            expect("READY" in engine_log and "compiling" not in engine_log,
                   f"while the engine log holds the ENGINE's output alone "
                   f"(got {engine_log!r})")
        finally:
            if proc is not None:
                proc.kill()
                proc.wait(timeout=10)
    finally:
        scratch.cleanup()


def test_a_failed_preparation_is_not_reported_as_readiness() -> None:
    print("\n-- a failed build is reported as PREPARATION, with its output")
    scratch = PreparedScratch(build_status=1,
                              build_output="Missing dependency: vulkan-1.2")
    try:
        with engine_env(None):
            try:
                probelib.boot(9462, log=str(scratch.engine_log),
                              ready_timeout=30.0, prepare_timeout=60.0)
            except SystemExit as leaving:
                message = str(leaving)
            else:
                message = ""
                expect(False, "a failed preparation did not end the probe")
        expect("could not be prepared" in message,
               f"the diagnostic names preparation (got {message!r})")
        expect("never printed READY" not in message
               and "exited before READY" not in message,
               f"and never blames engine readiness for a build that never "
               f"produced an engine (got {message!r})")
        expect(str(scratch.boot_prepare_log) in message,
               f"it names the log the build output really went to "
               f"(got {message!r})")
        expect("Missing dependency: vulkan-1.2" in message,
               f"and carries that output, rather than pointing at an empty "
               f"file (got {message!r})")
        expect(not scratch.engine_log.exists(),
               "no empty engine log is left behind to be misread as a boot "
               "that was attempted")
    finally:
        scratch.cleanup()


def test_a_failed_preparation_disposes_of_its_whole_process_tree() -> None:
    print("\n-- and takes its `setup`/GHC descendants with it")
    for label, config in (
            ("a nonzero build", {"build_status": 1}),
            ("a build that overruns", {"build_hangs": True})):
        scratch = PreparedScratch(descendant=None, **config)
        # The descendant path is written by the scratch itself, so the
        # stand-in knows where to record the pid it leaves behind.
        document = json.loads((scratch.dir / "cabal.json").read_text())
        document["descendant"] = str(scratch.descendant_pid)
        (scratch.dir / "cabal.json").write_text(json.dumps(document))
        try:
            try:
                prepare(scratch, timeout=(60.0 if "build_status" in config
                                          else 3.0))
            except probe_engine.EnginePreparationError as error:
                expect("could not be prepared" in str(error),
                       f"[{label}] preparation refuses (got {error})")
            else:
                expect(False, f"[{label}] preparation did not fail")
            expect(scratch.descendant() is not None,
                   f"[{label}] the stand-in really did leave a descendant, "
                   f"so this case is not vacuous")
            expect(scratch.descendant_gone(),
                   f"[{label}] and the marked descendant is gone, not left "
                   f"compiling into the build directory "
                   f"({scratch.describe_descendant()})")
        finally:
            scratch.cleanup()


def test_the_preparation_allowance_is_its_own() -> None:
    print("\n-- preparation has a finite allowance unrelated to readiness")
    expect(probe_engine.DEFAULT_PREPARE_TIMEOUT == 1800.0,
           f"the default matches the repository's full-cold-build watchdog "
           f"(got {probe_engine.DEFAULT_PREPARE_TIMEOUT})")
    expect(probe_engine.DEFAULT_PREPARE_TIMEOUT
           != probelib.DEFAULT_READY_TIMEOUT,
           "and is not the readiness allowance wearing another name")
    signature = inspect.signature(probelib.boot)
    parameter = signature.parameters.get("prepare_timeout")
    expect(parameter is not None
           and parameter.default == probe_engine.DEFAULT_PREPARE_TIMEOUT,
           f"boot takes it separately from ready_timeout (got {parameter})")
    expect(list(signature.parameters)[:7]
           == ["port", "log", "args", "ready_timeout", "label", "mode",
               "on_launch"],
           f"and it was APPENDED, so no positional caller shifted "
           f"(got {list(signature.parameters)})")
    scratch = PreparedScratch(build_hangs=True)
    try:
        started = time.monotonic()
        try:
            # A readiness allowance an order of magnitude larger: if the
            # build were being timed by it, this would not return here.
            with engine_env(None):
                probelib.boot(9463, log=str(scratch.engine_log),
                              ready_timeout=600.0, prepare_timeout=3.0)
        except SystemExit as leaving:
            message = str(leaving)
        else:
            message = ""
            expect(False, "an overrunning build did not end the probe")
        elapsed = time.monotonic() - started
        expect(elapsed < 60.0,
               f"the build is bounded by ITS allowance, not readiness "
               f"(took {elapsed:.1f} s)")
        expect("could not be prepared" in message and "3 s" in message,
               f"and the diagnostic names the preparation allowance "
               f"(got {message!r})")
    finally:
        scratch.cleanup()


def test_preparation_will_not_build_beside_another_writer() -> None:
    print("\n-- preparation takes `cabal-build` exclusively, or builds nothing")
    scratch = PreparedScratch()
    holder = None
    try:
        # An ordinary SHARED interest, which is what every probe and
        # every `/deflake` measurement holds while a run is in flight.
        holder = subprocess.Popen(
            [sys.executable, "-c", HOLDER_SRC, str(TOOLS),
             str(scratch.lock_root), scratch.namespace,
             str(scratch.dir / "held.json"), str(scratch.dir / "release"),
             json.dumps({"shared": [probe_engine.BUILD_RESOURCE]})])
        deadline = time.time() + 20
        while not (scratch.dir / "held.json").exists() and time.time() < deadline:
            time.sleep(0.05)
        expect((scratch.dir / "held.json").exists(),
               "the foreign holder really took the resource")
        try:
            prepare(scratch, timeout=3.0)
        except probe_engine.EnginePreparationError as error:
            expect(probe_engine.BUILD_RESOURCE in str(error),
                   f"preparation refuses, naming the resource (got {error})")
        else:
            expect(False, "preparation built beside a live holder")
        expect(scratch.cabal_calls() == [],
               f"and NOTHING was built: waiting was not degraded into an "
               f"unlocked build (got {scratch.cabal_calls()})")
        (scratch.dir / "release").write_text("go")
        holder.wait(timeout=20)
        holder = None
        expect(prepare(scratch, timeout=60.0) == str(scratch.engine),
               "and once the resource is free the same call prepares")
        expect(scratch.cabal_calls() == ["build exe:synarchy",
                                         "list-bin exe:synarchy"],
               f"building exactly then (got {scratch.cabal_calls()})")
    finally:
        if holder is not None:
            holder.kill()
            holder.wait(timeout=10)
        scratch.cleanup()


def test_an_engine_that_fails_after_preparation_is_still_a_boot_failure() -> None:
    print("\n-- an engine that dies after its executable exists is diagnosed "
          "as one")
    scratch = PreparedScratch(engine=DYING_ENGINE)
    try:
        with engine_env(None):
            try:
                probelib.boot(9464, log=str(scratch.engine_log),
                              ready_timeout=30.0)
            except SystemExit as leaving:
                message = str(leaving)
            else:
                message = ""
                expect(False, "a dying engine did not end the probe")
        expect("exited before READY" in message,
               f"the engine's own failure keeps its own diagnostic "
               f"(got {message!r})")
        expect("could not be prepared" not in message,
               f"and is not relabelled as a preparation problem "
               f"(got {message!r})")
        expect("no Vulkan device" in scratch.engine_log.read_text(),
               "with the engine log holding what the engine actually said")
    finally:
        scratch.cleanup()


def test_an_interrupt_during_preparation_leaves_no_build_behind() -> None:
    print("\n-- a Ctrl-C during the build takes the whole build tree with it")
    scratch = PreparedScratch(build_hangs=True)
    document = json.loads((scratch.dir / "cabal.json").read_text())
    document["descendant"] = str(scratch.descendant_pid)
    (scratch.dir / "cabal.json").write_text(json.dumps(document))
    timer = threading.Timer(2.0, _thread.interrupt_main)
    try:
        timer.start()
        raised: BaseException | None = None
        try:
            prepare(scratch, timeout=120.0)
        except BaseException as exc:  # noqa: BLE001 - the point
            raised = exc
        expect(isinstance(raised, KeyboardInterrupt),
               f"the interrupt still ends the run rather than being "
               f"swallowed (got {raised!r})")
        expect(scratch.descendant() is not None,
               "the stand-in really did leave a descendant, so this case is "
               "not vacuous")
        expect(scratch.descendant_gone(),
               f"and it is gone, not left compiling after the probe was "
               f"interrupted ({scratch.describe_descendant()})")
    finally:
        timer.cancel()
        scratch.cleanup()


def test_an_unlaunchable_cabal_is_a_preparation_failure() -> None:
    print("\n-- and a `cabal` that cannot be launched at all is reported "
          "as one")
    scratch = PreparedScratch()
    try:
        # The stand-in goes, and so does every other directory on PATH:
        # leaving the REAL `cabal` reachable would test what it says
        # about a temporary directory, not what preparation says about a
        # `cabal` it cannot launch at all.
        (scratch.dir / "cabal").unlink()
        os.environ["PATH"] = str(scratch.dir)
        try:
            prepare(scratch, timeout=30.0)
        except probe_engine.EnginePreparationError as error:
            expect("not found on PATH" in str(error),
                   f"the diagnostic names the missing tool (got {error})")
        else:
            expect(False, "a missing `cabal` was not reported")
        expect(not scratch.prepare_log.exists()
               or scratch.prepare_log.read_text() == "",
               "and nothing was built")
    finally:
        scratch.cleanup()


def test_an_explicit_ready_timeout_still_bounds_readiness_alone() -> None:
    print("\n-- an explicit ready_timeout keeps meaning engine readiness")
    scratch = PreparedScratch(engine=SILENT_ENGINE)
    try:
        started = time.monotonic()
        with engine_env(None):
            try:
                probelib.boot(9465, log=str(scratch.engine_log),
                              ready_timeout=4.0)
            except SystemExit as leaving:
                message = str(leaving)
            else:
                message = ""
                expect(False, "a silent engine did not end the probe")
        elapsed = time.monotonic() - started
        expect("never printed READY" in message,
               f"the allowance the caller asked for still governs READY "
               f"(got {message!r})")
        expect("could not be prepared" not in message,
               f"and is not spent on, or blamed on, preparation "
               f"(got {message!r})")
        expect(4.0 <= elapsed < 60.0,
               f"it really waited its own 4 s and no longer "
               f"(took {elapsed:.1f} s)")
        expect(scratch.cabal_calls() == ["build exe:synarchy",
                                         "list-bin exe:synarchy"],
               f"with the build already finished before that wait began "
               f"(got {scratch.cabal_calls()})")
    finally:
        scratch.cleanup()


def test_the_runner_path_prepares_nothing() -> None:
    print("\n-- under the runner there is no build, no lock and no log")
    scratch = PreparedScratch()
    try:
        with engine_env(str(scratch.engine)):
            resolved = probe_engine.prepare_executable(
                str(scratch.dir), log_path=str(scratch.prepare_log),
                namespace=scratch.namespace, lock_root=scratch.lock_root)
            expect(resolved == str(scratch.engine),
                   f"the runner's executable is returned as-is "
                   f"(got {resolved})")
            expect(scratch.cabal_calls() == [],
                   f"with no Cabal contact of its own "
                   f"(got {scratch.cabal_calls()})")
            expect(not scratch.prepare_log.exists(),
                   "and no preparation log, because nothing was prepared")
            expect(probe_engine.prepare_command(["--headless", "--port", "9008"],
                                                repo_root=str(scratch.dir))
                   == probe_engine.engine_command(["--headless", "--port",
                                                   "9008"]),
                   "so the argv is exactly what engine_command already "
                   "produced")
        with engine_env(str(scratch.dir / "never-built")):
            try:
                prepare(scratch)
            except probe_engine.EngineExecutableError as error:
                expect("never-built" in str(error),
                       f"an unusable supplied path is still refused rather "
                       f"than quietly rebuilt (got {error})")
            else:
                expect(False, "an unusable supplied path was accepted")
            expect(scratch.cabal_calls() == [],
                   f"and no build was started to paper over it "
                   f"(got {scratch.cabal_calls()})")
    finally:
        scratch.cleanup()


def test_the_build_resource_is_named_once() -> None:
    print("\n-- the direct path and the runner name the same resource")
    expect(run_probes.BUILD_RESOURCE is probe_engine.BUILD_RESOURCE,
           f"run_probes reads the name from probe_engine, so the two "
           f"cannot drift (got {run_probes.BUILD_RESOURCE!r} / "
           f"{probe_engine.BUILD_RESOURCE!r})")
    expect(probe_engine.BUILD_RESOURCE
           in run_probes.IMPLICIT_SHARED_RESOURCES,
           "and it is the same resource every probe already declares")


def main() -> int:
    test_valid_json_decodes()
    test_lua_nil_is_none()
    test_empty_result_is_none_not_empty_string()
    test_invalid_json_is_returned_as_text()
    test_idle_is_reachable()
    test_no_local_json_console_wrapper()
    test_a_reintroduced_jget_is_caught()
    test_a_renamed_copy_is_caught()
    test_a_guarded_copy_is_caught()
    test_a_keyword_lua_copy_is_caught()
    test_a_fixed_query_helper_is_not_caught()
    test_a_timeout_forwarding_fixed_query_helper_is_not_caught()
    test_send_json_callers_import_it()
    test_no_unused_send_json_import()
    test_direct_mode_keeps_the_cabal_fallback()
    test_runner_mode_execs_the_resolved_binary()
    test_an_unusable_supplied_executable_is_refused()
    test_boot_launches_the_supplied_executable()
    test_boot_registers_the_process_as_it_launches()
    test_an_interrupt_during_the_hand_off_kills_the_child()
    test_boot_without_a_callback_is_unchanged()
    test_resolve_executable_builds_then_locates()
    test_a_killed_descendant_reads_as_gone_before_it_is_reaped()
    test_preparation_precedes_the_engine_launch()
    test_a_failed_preparation_is_not_reported_as_readiness()
    test_a_failed_preparation_disposes_of_its_whole_process_tree()
    test_the_preparation_allowance_is_its_own()
    test_preparation_will_not_build_beside_another_writer()
    test_an_interrupt_during_preparation_leaves_no_build_behind()
    test_an_unlaunchable_cabal_is_a_preparation_failure()
    test_an_explicit_ready_timeout_still_bounds_readiness_alone()
    test_an_engine_that_fails_after_preparation_is_still_a_boot_failure()
    test_the_runner_path_prepares_nothing()
    test_the_build_resource_is_named_once()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll probelib tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
