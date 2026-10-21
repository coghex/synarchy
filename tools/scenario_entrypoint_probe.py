#!/usr/bin/env python3
"""Real-process probe of the scenario ENTRY POINTS (issue #2715, SCN-15 of
epic #2698): ``engine.loadScenario`` / ``engine.saveScenario`` /
``engine.getScenarioStatus`` over the socket console, and a direct
``--scenario <path>`` boot.

The hspec groups ``Scenario.Paths``, ``Lua scenario API`` and ``App.Cli``
own the path policy, the strict argument handling, the status vocabulary
and the pure CLI parsing. They cannot cross a process boundary, start a
listener, or watch an engine exit. This probe does exactly that, against
the REAL headless engine (every owner thread running):

  1. CLI BOOT, LAUNCH-RELATIVE. ``--scenario`` names a file RELATIVE to the
     directory the process was launched from — a different directory from
     the resource root — whose name carries spaces, both quote marks and
     non-ASCII text. The scenario folder (``<resource root>/scenarios/``)
     holds a file of the SAME name with different content, so only
     launch-relative resolution can produce the loaded result. The boot
     selects the arena profile without ``--arena``; the console ``READY``
     line precedes the scenario's own completion; the scenario is awaited
     by its OPERATION IDENTITY (kind and id), and ends ready and PAUSED
     with no warnings.
  2. LUA, SAME PROCESS. A bare name resolves inside ``scenarios/`` under the
     resource root (the same-named file there, not the launch one); the
     name with its extension resolves to the same file; an absolute path
     works; every refusal (traversal, a missing name, a foreign extension,
     a name built to be Lua code) is an immediate rejection that allocates
     nothing, changes no status and runs nothing.
  3. ADMISSION AND COMPLETION. In one console chunk a second request made
     while the first is in flight is rejected ``busy``; the active status
     is unchanged; the SAME operation completes; an explicit retry is
     admitted under a new id. A failing load into an existing session
     (invalid YAML) is recoverable: the session stands, a later valid load
     succeeds.
  4. CAPTURE. ``engine.saveScenario`` refuses to overwrite by default and
     replaces on explicit intent; the captured file reloads; the original
     source's bytes never change.
  5. SAME BASELINE FROM EVERY ENTRY POINT. A SECOND process booted without
     ``--scenario`` loads the same file through Lua and captures it: the
     document is byte-identical to the CLI boot's capture (the supported
     values, and the warning semantics, are one thing, not two). That
     bare boot also keeps its old behaviour: no startup request, no
     status, no latch.
  6. PARTIAL CONTENT. ``--arena --scenario`` of a file with one unknown
     definition boots, stays up, and is ready with exactly one structured
     warning and exactly the retained content.
  7. FATAL STARTUP. Each of these exits NONZERO within a bounded wait after
     ``READY``: invalid YAML syntax, a document that is not a mapping, an
     unsupported version, an unreadable file, a construction failure (the
     unit-readiness hook raises), an incomplete reconciliation (a session
     callback raises), and a definition family that will not load. Each
     asserts the cause on the output, the worker-stop and log-flush trace
     the fatal tail prints, and — for construction — that the recovered
     FLAT ARENA does not make the startup pass. The two failures that need
     a seam use an isolated resource root whose ``scripts/`` is a copy with
     one fixture line; nothing races the startup.

``--offscreen`` (manual, needs a GPU, not registered for CI) runs the same
boot, partial-content and fatal checks against ``--offscreen`` instead.

Every engine runs on an isolated writable resource root (real assets, data
and config linked in, its own ``scenarios/`` and ``saves/``), on a non-8008
port, with bounded waits and its log retained (``--retain DIR`` copies them).

Run:  python3 tools/scenario_entrypoint_probe.py [--port 9247] [--retain DIR]
      python3 tools/scenario_entrypoint_probe.py --offscreen   (manual)
"""
from __future__ import annotations

import argparse
import hashlib
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

import probe_engine
from probelib import GUI_PORT, lua_string, quit_engine, send, send_json

REPO = Path(__file__).resolve().parent.parent

# the launch-relative file: spaces, both quote marks and non-ASCII text
FANCY = "ünï \"q\" 's' x"
FANCY_FILE = FANCY + ".yaml"

ONE_UNIT = """\
version: 3
map: {width: 40, height: 40}
camera: {x: 3, y: 4}
units:
  - id: medic
    definition: acolyte
    x: 1
    y: 1
    stats: {hydration: 4.0}
"""
TWO_UNITS = """\
version: 3
map: {width: 40, height: 40}
units:
  - {id: first, definition: acolyte, x: 1, y: 1}
  - {id: second, definition: acolyte, x: 3, y: 3}
"""
PARTIAL = """\
version: 3
map: {width: 40, height: 40}
units:
  - {id: ghost, definition: no_such_unit, x: 1, y: 1}
  - {id: loner, definition: acolyte, x: 2, y: 2}
"""
SYNTAX_ERROR = "version: 3\nunits: [unclosed\n"
NOT_A_MAPPING = "- 1\n- 2\n"
FUTURE_VERSION = "version: 99\n"

FAILED: list[str] = []


def ok(cond: bool, what: str) -> bool:
    print(("  PASS " if cond else "  FAIL ") + what)
    if not cond:
        FAILED.append(what)
    return bool(cond)


# ---------------------------------------------------------------------------
# Rigs: isolated resource roots
# ---------------------------------------------------------------------------
def make_root(base: str, name: str, *, patch_scripts=None, patch_data=None) -> str:
    """An isolated resource root. ``scripts``, ``assets``, ``data`` and
    ``config`` are linked to the repository's, except where a seam patch
    asks for a private COPY of ``scripts`` or ``data``."""
    root = os.path.join(base, name)
    os.makedirs(root)
    for family in ("scripts", "assets", "data", "config"):
        dest = os.path.join(root, family)
        if family == "scripts" and patch_scripts is not None:
            shutil.copytree(REPO / "scripts", dest)
            patch_scripts(dest)
        elif family == "data" and patch_data is not None:
            os.makedirs(dest)
            for entry in os.listdir(REPO / "data"):
                os.symlink(REPO / "data" / entry, os.path.join(dest, entry))
            patch_data(dest)
        else:
            os.symlink(REPO / family, dest)
    os.makedirs(os.path.join(root, "saves"))
    os.makedirs(os.path.join(root, "scenarios"))
    return root


def patch_construction_failure(scripts: str) -> None:
    """The units' paused-readiness hook raises: the scenario's CONTENT
    cannot be installed after the replacement session was published."""
    path = os.path.join(scripts, "unit_resources.lua")
    text = Path(path).read_text()
    marker = "\nreturn unitResources"
    assert text.rstrip().endswith("return unitResources"), "unit_resources.lua changed shape"
    head = text.rstrip()[: -len("return unitResources")]
    Path(path).write_text(
        head + '\nunitResources.settleScenarioUnit = function()\n'
        '    error("probe fixture: settleScenarioUnit refused")\nend\n\nreturn unitResources\n')


def patch_reconciliation_failure(scripts: str) -> None:
    """A loaded script's ``onSaveLoaded`` raises: the published session is
    incompletely reconciled."""
    Path(os.path.join(scripts, "zz_probe_reconcile.lua")).write_text(
        "local m = {}\n"
        "function m.init(id) end\n"
        "function m.update(dt) end\n"
        'function m.onSaveLoaded() error("probe fixture: reconciliation refused") end\n'
        "return m\n")
    path = os.path.join(scripts, "init_loader.lua")
    text = Path(path).read_text()
    anchor = 'scenarioStartupScriptId = engine.loadScript("scripts/scenario_startup.lua", 0.05)\n'
    assert anchor in text, "init_loader.lua no longer loads the startup driver"
    Path(path).write_text(text.replace(
        anchor, anchor + '    engine.loadScript("scripts/zz_probe_reconcile.lua", 1.0)\n', 1))


def patch_broken_materials(data: str) -> None:
    """A definition family that will not load: the arena profile's startup
    loader fails terminally."""
    real = os.path.join(data, "materials")
    os.unlink(real)
    shutil.copytree(REPO / "data" / "materials", real)
    Path(os.path.join(real, "zz_probe_broken.yaml")).write_text("materials: [unclosed\n")


# ---------------------------------------------------------------------------
# Processes
# ---------------------------------------------------------------------------
_EXE: list[str] = []


def engine_exe(logdir: str) -> str:
    """The engine executable, prepared ONCE per probe run (the build, when a
    probe is run by hand, is not repeated for every boot)."""
    if not _EXE:
        _EXE.append(probe_engine.prepare_executable(
            log_path=os.path.join(logdir, "prepare.log"),
            timeout=probe_engine.DEFAULT_PREPARE_TIMEOUT))
    return _EXE[0]


class Engine:
    """One engine process: its own cwd, its own log, bounded waits."""

    def __init__(self, label: str, port: int, root: str, launch: str, logdir: str,
                 args: list[str], mode: str = "--headless"):
        self.label, self.port, self.root, self.launch = label, port, root, launch
        self.log = os.path.join(logdir, f"{label}.log")
        cmd = [engine_exe(logdir), mode, "--port", str(port), "--resource-root", root, *args]
        self.logf = open(self.log, "w")
        self.proc = subprocess.Popen(cmd, cwd=launch, stdout=self.logf,
                                     stderr=subprocess.STDOUT)

    def text(self) -> str:
        try:
            return Path(self.log).read_text(errors="replace")
        except FileNotFoundError:
            return ""

    def wait_ready(self, seconds: float = 120.0) -> bool:
        deadline = time.time() + seconds
        while time.time() < deadline:
            if "READY" in self.text():
                return True
            if self.proc.poll() is not None:
                return False
            time.sleep(0.2)
        return False

    def wait_exit(self, seconds: float) -> int | None:
        try:
            return self.proc.wait(timeout=seconds)
        except subprocess.TimeoutExpired:
            return None

    def stop(self) -> int | None:
        """Quit through the console (never a name-wide kill) and reap."""
        if self.proc.poll() is None:
            quit_engine(self.port, self.proc)
        if self.proc.poll() is None:
            self.proc.kill()
            self.proc.wait(timeout=10)
        self.logf.close()
        return self.proc.returncode


def status(port: int) -> dict | None:
    r = send_json(port, "return engine.getScenarioStatus()")
    return r if isinstance(r, dict) else None


def await_operation(port: int, kind: str, op_id: int, seconds: float = 120.0) -> dict | None:
    """Poll until THE operation (kind and id) is terminal. A status naming a
    different operation, or none, is not this one's result."""
    deadline = time.time() + seconds
    last = None
    while time.time() < deadline:
        last = status(port)
        if last and last.get("kind") == kind and last.get("id") == op_id and last.get("done"):
            return last
        time.sleep(0.25)
    return last


def accepted(reply: str) -> int | None:
    try:
        return int(float(reply.strip()))
    except ValueError:
        return None


def lua_load(port: int, name: str) -> tuple[int | None, str]:
    r = send(port, f"return engine.loadScenario({lua_string(name)})")
    return accepted(r), r


def lua_save(port: int, name: str, overwrite: bool = False) -> tuple[int | None, str]:
    opts = "{overwrite = true}" if overwrite else "{}"
    r = send(port, f"return engine.saveScenario({lua_string(name)}, {opts})")
    return accepted(r), r


def sha(path: str) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def unit_count(port: int) -> int:
    return int(float(send(port, "return #unit.getAllIds()").strip() or -1))


def arena_definitions(port: int) -> bool:
    """A bare boot loads the arena profile's definitions the way the startup
    driver does for headless: the shared startup loader."""
    r = send_json(port, "local sl = require('scripts.startup_loader') "
                        "sl.build('arena') sl.runAll() return sl.isDone() == true",
                  timeout=120)
    return r is True


# ---------------------------------------------------------------------------
# The checks
# ---------------------------------------------------------------------------
def check_cli_boot(ctx) -> str | None:
    """1-4: the CLI boot, then the Lua entry points on the same process.
    Returns the capture of the CLI-loaded file (for check 5)."""
    print("\n--- 1. CLI boot: launch-relative, fancy name, distinct roots ---")
    root, launch = ctx["root"], ctx["launch"]
    cli_file = os.path.join(launch, FANCY_FILE)
    Path(cli_file).write_text(ONE_UNIT)
    Path(os.path.join(root, "scenarios", FANCY_FILE)).write_text(TWO_UNITS)
    cli_sha = sha(cli_file)
    eng = Engine("cli-boot", ctx["port"], root, launch, ctx["logs"], ["--scenario", FANCY_FILE])
    capture_path = os.path.join(ctx["work"], "cli-capture.yaml")
    try:
        if not ok(eng.wait_ready(), "the console printed READY"):
            return None
        port = ctx["port"]
        ok(send(port, "return engine.getBootProfile()").strip() == "arena",
           "--scenario selected the arena profile without --arena")
        req = send_json(port, "return engine.getScenarioStartup()")
        ok(isinstance(req, dict) and req.get("path") == cli_file and req.get("loadsDefinitions") is True,
           f"the engine carries the RESOLVED file as data ({req})")
        # console availability is not scenario readiness: follow the operation
        done = await_operation(port, "load", 1)
        ok(bool(done) and done.get("outcome") == "ready" and done.get("done") is True,
           f"operation load #1 reached its ready result ({done and done.get('outcome')})")
        ok(bool(done) and done.get("source") == cli_file,
           "its source is the launch-relative file, resolved against the LAUNCH directory")
        ok(bool(done) and done.get("warnings") == [] and done.get("published") is True
           and done.get("recovery") == "none", "no warnings; published; nothing recovered")
        ok(send(port, "return engine.isPaused()").strip() == "true", "the scenario stands PAUSED")
        ok(unit_count(port) == 1, "the launch file's single unit stands, not the folder file's two")
        text = eng.text()
        ready_at = text.find("READY")
        done_at = text.find("Scenario load #1 ready")
        ok(0 <= ready_at < done_at, "console READY precedes the scenario's own completion")
        ok(text.count("Scenario startup load #") == 2 and "submitted" in text,
           "exactly one startup load was submitted")

        print("\n--- 2. Lua names, absolute paths and rejections (same process) ---")
        # a bare name resolves INSIDE scenarios/ under the resource root
        lid, _ = lua_load(port, FANCY)
        done = lid and await_operation(port, "load", lid)
        ok(bool(done) and done.get("source") == os.path.join(root, "scenarios", FANCY_FILE)
           and done.get("outcome") == "ready", "a bare name resolves in the scenario folder")
        ok(unit_count(port) == 2, "…to the folder's file (two units), not the launch file")
        lid2, _ = lua_load(port, FANCY_FILE)
        done2 = lid2 and await_operation(port, "load", lid2)
        ok(bool(done2) and done2.get("source") == os.path.join(root, "scenarios", FANCY_FILE),
           "the name with its extension is the same file")
        before = status(port)
        for label, arg in [("a traversal", "../" + FANCY), ("a missing name", "no-such-2715"),
                           ("a foreign extension", "x.yml"), ("an empty name", ""),
                           ("a name built to be code", "x']]; os.execute('touch " + ctx["pwned"] + "') --")]:
            r = send_json(port, f"local id, why = engine.loadScenario({lua_string(arg)}) "
                                "return {id = id or false, reason = why and why.reason}")
            ok(isinstance(r, dict) and r.get("id") is False
               and r.get("reason") in ("invalid_path", "not_found"),
               f"{label} is rejected at once ({r})")
        ok(status(port) == before, "no rejection allocated an identity or changed the status")
        ok(not os.path.exists(ctx["pwned"]), "nothing a path contained was ever run as code")
        # absolute path through the same verb
        aid, _ = lua_load(port, cli_file)
        absdone = aid and await_operation(port, "load", aid)
        ok(bool(absdone) and absdone.get("source") == cli_file and absdone.get("outcome") == "ready",
           "an absolute path works through Lua")

        print("\n--- 3. admission versus completion ---")
        chunk = (f"local a = engine.loadScenario({lua_string(cli_file)}) "
                 f"local b, why = engine.loadScenario({lua_string(cli_file)}) "
                 f"local c, why2 = engine.saveScenario({lua_string(capture_path)}, {{}}) "
                 "local s = engine.getScenarioStatus() "
                 "return {first = a, second = b or false, reason = why and why.reason, "
                 "save = c or false, savereason = why2 and why2.reason, "
                 "active = s.id, stage = s.stage, paused = engine.isPaused()}")
        r = send_json(port, chunk)
        ok(isinstance(r, dict) and r.get("second") is False and r.get("reason") == "busy"
           and r.get("save") is False and r.get("savereason") == "busy",
           f"a second load and a save made while one is in flight are rejected busy ({r})")
        first = int(r["first"])
        ok(r.get("active") == first and r.get("paused") is True,
           "the active operation's status and the pause are unchanged by them")
        done = await_operation(port, "load", first)
        ok(bool(done) and done.get("outcome") == "ready", "the SAME operation completes")
        retry, _ = lua_load(port, cli_file)
        ok(retry == first + 1, f"an explicit retry is admitted under a new id ({retry})")
        await_operation(port, "load", retry)
        units_before = unit_count(port)
        bad = os.path.join(ctx["work"], "bad.yaml")
        Path(bad).write_text(SYNTAX_ERROR)
        bid, _ = lua_load(port, bad)
        failed = bid and await_operation(port, "load", bid)
        ok(bool(failed) and failed.get("outcome") == "failed"
           and failed["failure"].get("source") == "syntax_error"
           and failed.get("recovery") == "preserved" and failed.get("published") is False,
           "a failing load into an existing session is a recoverable failure")
        ok(unit_count(port) == units_before and send(port, "return engine.isPaused()").strip() == "true",
           "…the session stands, paused")
        again, _ = lua_load(port, cli_file)
        ok(bool(again) and (await_operation(port, "load", again) or {}).get("outcome") == "ready",
           "a later valid load succeeds")

        print("\n--- 4. capture: explicit overwrite, reload, source untouched ---")
        sid, _ = lua_save(port, capture_path)
        saved = sid and await_operation(port, "save", sid)
        ok(bool(saved) and saved.get("outcome") == "saved" and saved.get("destination") == capture_path,
           f"a new destination is captured ({saved and saved.get('outcome')})")
        first_bytes = sha(capture_path)
        sid2, _ = lua_save(port, capture_path)
        refused = sid2 and await_operation(port, "save", sid2)
        ok(bool(refused) and refused.get("outcome") == "failed"
           and refused["failure"].get("kind") == "overwrite_refused"
           and refused.get("published") is False, "overwrite is refused by default")
        ok(sha(capture_path) == first_bytes, "…and changes no byte")
        sid3, _ = lua_save(port, capture_path, overwrite=True)
        replaced = sid3 and await_operation(port, "save", sid3)
        ok(bool(replaced) and replaced.get("outcome") == "saved" and replaced.get("replaced") is True,
           "explicit overwrite replaces it")
        named, _ = lua_save(port, "captured-by-name")
        namedst = named and await_operation(port, "save", named)
        ok(bool(namedst) and namedst.get("destination") == os.path.join(root, "scenarios", "captured-by-name.yaml"),
           "a bare capture name lands in the scenario folder")
        rid, _ = lua_load(port, "captured-by-name")
        ok(bool(rid) and (await_operation(port, "load", rid) or {}).get("outcome") == "ready",
           "the captured file reloads")
        ok(sha(cli_file) == cli_sha, "the original source's bytes never changed")
        ok(send(port, "return engine.isPaused()").strip() == "true", "everything ended paused")
        return capture_path if os.path.exists(capture_path) else None
    finally:
        rc = eng.stop()
        ok(rc == 0, f"the CLI-booted engine quit cleanly (exit {rc})")


def check_bare_boot(ctx, cli_capture: str | None) -> None:
    print("\n--- 5. a boot WITHOUT --scenario: unchanged, and the same baseline via Lua ---")
    root, launch = ctx["root"], ctx["launch"]
    eng = Engine("bare-boot", ctx["port"], root, launch, ctx["logs"], ["--arena"])
    try:
        if not ok(eng.wait_ready(), "the bare boot printed READY"):
            return
        port = ctx["port"]
        ok(send(port, "return tostring(engine.getScenarioStartup())").strip() == "nil",
           "no startup request without the flag")
        ok(send(port, "return tostring(engine.getScenarioStatus())").strip() == "nil",
           "no scenario status without the flag")
        r = send_json(port, "local id, why = engine.startScenarioStartup() return {id = id or false, reason = why and why.reason}")
        ok(isinstance(r, dict) and r.get("id") is False, "the startup verb refuses without the flag")
        time.sleep(3.0)
        ok(eng.proc.poll() is None, "the bare boot keeps running")
        ok(arena_definitions(port), "the arena definitions loaded through the shared startup loader")
        cli_file = os.path.join(launch, FANCY_FILE)
        lid, reply = lua_load(port, cli_file)
        done = lid and await_operation(port, "load", lid)
        ok(bool(done) and done.get("outcome") == "ready" and done.get("warnings") == [],
           f"the same file loads through Lua ({done and done.get('outcome')})")
        out = os.path.join(ctx["work"], "lua-capture.yaml")
        sid, _ = lua_save(port, out)
        saved = sid and await_operation(port, "save", sid)
        ok(bool(saved) and saved.get("outcome") == "saved", "…and captures")
        if cli_capture and os.path.exists(out):
            ok(sha(out) == sha(cli_capture),
               "the capture is BYTE-IDENTICAL to the CLI boot's: one baseline, not two")
        ok(send(port, "return engine.isPaused()").strip() == "true", "paused")
    finally:
        eng.stop()


def check_partial(ctx) -> None:
    print("\n--- 6. partial content: stays available, paused, with its warnings ---")
    root, launch = ctx["root"], ctx["launch"]
    path = os.path.join(launch, "partial.yaml")
    Path(path).write_text(PARTIAL)
    eng = Engine("partial", ctx["port"], root, launch, ctx["logs"], ["--arena", "--scenario", "partial.yaml"])
    try:
        if not ok(eng.wait_ready(), "the console printed READY"):
            return
        port = ctx["port"]
        done = await_operation(port, "load", 1)
        ok(bool(done) and done.get("outcome") == "ready" and len(done.get("warnings", [])) == 1,
           f"ready with exactly one warning ({done and done.get('warnings')})")
        w = (done or {}).get("warnings", [{}])[0]
        ok(w.get("path") == "units[0].definition" and w.get("code") == "unknown_definition"
           and w.get("detail") == "no_such_unit" and w.get("effect") == "entry_rejected",
           f"the warning is structured ({w})")
        ok(unit_count(port) == 1, "exactly the retained content stands (the valid unit)")
        ok(send(port, "return engine.isPaused()").strip() == "true", "paused")
        time.sleep(3.0)
        ok(eng.proc.poll() is None, "the process stays up (a warning is not a startup failure)")
    finally:
        rc = eng.stop()
        ok(rc == 0, f"clean quit (exit {rc})")


def fatal_case(ctx, label: str, scenario_text: str | None, *, root: str | None = None,
               mode: str = "--headless", expect: list[str], flat_arena: bool = False,
               prepare=None, wait: float = 90.0) -> None:
    """One fatal-startup boot: the process must exit NONZERO within a bounded
    wait, with the cause, the worker-stop and the log-flush trace."""
    print(f"\n  fatal startup: {label}")
    root = root or ctx["root"]
    launch = ctx["launch"]
    name = "fatal-" + label.replace(" ", "-") + ".yaml"
    path = os.path.join(launch, name)
    if scenario_text is not None:
        Path(path).write_text(scenario_text)
    if prepare:
        prepare(path)
    eng = Engine("fatal-" + label.replace(" ", "-"), ctx["port"], root, launch, ctx["logs"],
                 ["--scenario", name] + (["--size", "320x240"] if mode == "--offscreen" else []),
                 mode=mode)
    try:
        saw_ready = eng.wait_ready()
        rc = eng.wait_exit(wait)
        text = eng.text()
        ok(rc is not None and rc != 0, f"exits NONZERO within {wait:.0f}s (exit {rc})")
        ok(saw_ready or "READY" in text, "the console was up first (READY printed)")
        for needle in expect:
            ok(needle in text, f"the cause is on the output: {needle!r}")
        ok("boot cleanup: stopped the " in text and "worker thread(s) stopped" in text,
           "orderly worker shutdown is traced")
        ok(text.rstrip().endswith("boot cleanup: log flushed") or "boot cleanup: log flushed" in text,
           "the log was flushed before exit")
        ok("Scenario load #1 ready" not in text, "the scenario was never reported ready")
        if flat_arena:
            ok("a default flat arena stands, NOT the requested scenario" in text,
               "the recovered flat arena is reported as recovery, never as the scenario")
    finally:
        if eng.proc.poll() is None:
            eng.proc.kill()
        eng.stop()


def check_fatal(ctx, mode: str) -> None:
    print("\n--- 7. fatal startup exits nonzero after an orderly shutdown ---")
    fatal_case(ctx, "invalid yaml", SYNTAX_ERROR, mode=mode, expect=["ScenarioSyntaxError"])
    fatal_case(ctx, "not a mapping", NOT_A_MAPPING, mode=mode, expect=["ScenarioNotAMapping"])
    fatal_case(ctx, "unsupported version", FUTURE_VERSION, mode=mode,
               expect=["ScenarioVersionUnsupported 99"])
    if os.geteuid() != 0:
        def unreadable(path):
            os.chmod(path, 0)
        try:
            fatal_case(ctx, "unreadable", ONE_UNIT, mode=mode, expect=["ScenarioUnreadable"],
                       prepare=unreadable)
        finally:
            for f in Path(ctx["launch"]).glob("fatal-unreadable*"):
                os.chmod(f, 0o644)
    else:
        print("  SKIP unreadable file (running as root: file modes do not bind)")
    construct_root = make_root(ctx["base"], "root-construct", patch_scripts=patch_construction_failure)
    fatal_case(ctx, "construction failure", ONE_UNIT, root=construct_root, mode=mode,
               expect=["probe fixture: settleScenarioUnit refused", "scenario startup failed"],
               flat_arena=True)
    reconcile_root = make_root(ctx["base"], "root-reconcile", patch_scripts=patch_reconciliation_failure)
    fatal_case(ctx, "incomplete reconciliation", ONE_UNIT, root=reconcile_root, mode=mode,
               expect=["probe fixture: reconciliation refused",
                       "the replacement session is incompletely initialized"])
    if mode == "--headless":
        # headless drives the definitions itself, so a family that will not
        # load is the driver's to report
        broken_root = make_root(ctx["base"], "root-broken-data", patch_data=patch_broken_materials)
        fatal_case(ctx, "definition family failure", ONE_UNIT, root=broken_root, mode=mode,
                   expect=["Startup failed:", "materials"])


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9247)
    ap.add_argument("--retain", help="copy the engine logs and captured documents here")
    ap.add_argument("--offscreen", action="store_true",
                    help="MANUAL, needs a GPU: run the boot, partial and fatal checks with --offscreen")
    args = ap.parse_args()
    if args.port == GUI_PORT:
        sys.exit(f"refusing to use port {GUI_PORT} (the GUI port)")

    base = tempfile.mkdtemp(prefix="scenario_entrypoint_probe_")
    ctx = {"port": args.port, "base": base, "logs": os.path.join(base, "logs"),
           "work": os.path.join(base, "work"), "launch": os.path.join(base, "launch dir"),
           "pwned": os.path.join(base, "PWNED")}
    for key in ("logs", "work", "launch"):
        os.makedirs(ctx[key])
    ctx["root"] = make_root(base, "resource root")
    try:
        if args.offscreen:
            run_offscreen(ctx)
        else:
            capture = check_cli_boot(ctx)
            check_bare_boot(ctx, capture)
            check_partial(ctx)
            check_fatal(ctx, "--headless")
    finally:
        if args.retain:
            os.makedirs(args.retain, exist_ok=True)
            for src_dir in ("logs", "work"):
                shutil.copytree(ctx[src_dir], os.path.join(args.retain, src_dir), dirs_exist_ok=True)
        if FAILED:
            print(f"\nlogs retained at {ctx['logs']}")
        else:
            shutil.rmtree(base, ignore_errors=True)
    print()
    if FAILED:
        print(f"FAIL: {len(FAILED)} check(s) failed")
        for f in FAILED:
            print(f"  - {f}")
        return 1
    print("PASS: every check passed")
    return 0


def run_offscreen(ctx) -> None:
    """The windowless GPU mode: boot paused, and fail fatally the same way."""
    print("\n--- offscreen: --scenario boots paused ---")
    root, launch = ctx["root"], ctx["launch"]
    path = os.path.join(launch, "offscreen.yaml")
    Path(path).write_text(ONE_UNIT)
    eng = Engine("offscreen-boot", ctx["port"], root, launch, ctx["logs"],
                 ["--size", "320x240", "--scenario", "offscreen.yaml"], mode="--offscreen")
    try:
        if ok(eng.wait_ready(180.0), "the console printed READY"):
            done = await_operation(ctx["port"], "load", 1, 180.0)
            ok(bool(done) and done.get("outcome") == "ready", f"ready ({done and done.get('outcome')})")
            ok(send(ctx["port"], "return engine.isPaused()").strip() == "true", "paused")
            ok(unit_count(ctx["port"]) == 1, "the scenario's unit stands (no intermediate default arena)")
    finally:
        rc = eng.stop()
        ok(rc == 0, f"clean quit (exit {rc})")
    check_fatal(ctx, "--offscreen")


if __name__ == "__main__":
    sys.exit(main())
