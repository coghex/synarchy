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

import ast
import inspect
import os
import socket
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import probe_engine  # type: ignore  # noqa: E402
import probelib  # type: ignore  # noqa: E402
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
    test_resolve_executable_builds_then_locates()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll probelib tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
