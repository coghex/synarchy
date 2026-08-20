#!/usr/bin/env python3
"""Unit tests for probelib.send_json and the no-local-copies rule
(issue #1160).

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
import socket
import sys
import tempfile
import threading
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
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
    expect(decode('{"ok":true}', idle=0.05) == {"ok": True},
           "send_json should accept and honour an explicit idle")


# ---------------------------------------------------------------------
# Source guards: the copies must not come back
# ---------------------------------------------------------------------
def local_json_console_wrappers(root: Path = TOOLS) -> list[str]:
    """`tools/` functions that reimplement `probelib.send_json`.

    Structural, and name-agnostic on purpose -- `jget` was the spelling
    that proliferated, but a `getj`/`query`/`jsend` copy is the same
    defect. A function qualifies when it `json.loads` a `send` result
    AND the Lua it hands `send` is one of its own PARAMETERS: that pair
    is what makes it a general-purpose console decoder rather than a
    query helper. Deliberately not keyed on `return`, so a copy that
    guards the decode (`json.loads(raw) if raw else None`) or buries it
    in a branch counts the same.

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

    def is_wrapper(fn) -> bool:
        params = param_names(fn)
        # `send` is handed one of this function's parameters as its Lua.
        parameterised = any(
            any(isinstance(arg, ast.Name) and arg.id in params
                for arg in call.args[1:])
            or any(isinstance(kw.value, ast.Name) and kw.value.id in params
                   for kw in call.keywords)
            for call in send_calls(fn))
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


def test_a_fixed_query_helper_is_not_caught() -> None:
    print("\n-- a helper decoding ONE fixed query is left alone "
          "(out of #1160's scope)")
    found = in_temp_tree("fixed_query_probe.py", FIXED_QUERY_HELPER)
    expect(found == [],
           f"a fixed-query helper should not be flagged, got {found!r}")


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
    test_a_fixed_query_helper_is_not_caught()
    test_send_json_callers_import_it()
    test_no_unused_send_json_import()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll probelib send_json tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
