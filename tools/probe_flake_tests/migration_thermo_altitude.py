#!/usr/bin/env python3
"""thermo_altitude's `probe-result/v1` migration contract (#2087).

Drives the real `tools/thermo_altitude_probe.py` through a faked console so the
probe's standalone behaviour, its structured-result parity and its
failure attribution are pinned without booting an engine.
"""
from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

from . import support
from .support import probe_protocol
from .support import expect

PROBE = "thermo_altitude"

def _thermo_console(sweep: str):
    """A canned debug console for `thermo_altitude_probe._run`.

    Engine-free on purpose: the migration's sequencing and its MISSING
    outcome are decided entirely by the probe's own control flow, so
    they can be driven without generating a 128-tile world.
    """
    def fake_send(_port, lua, **_kw):
        if "world.init(" in lua:
            return "ok"
        if "getInitProgress" in lua:
            return "3"
        if "world.initArena(" in lua:
            return "ok"
        if "world.show(" in lua:
            return "shown"
        if "world.hide(" in lua:
            return "hidden"
        if "string.format" in lua:
            return f'"{sweep}"'
        if "getActiveWorldId" in lua:
            return '"t308"'
        if "getClimateAt(0,0)" in lua:
            return "10.00"
        if "getAmbientAt(0,0)" in lua:
            return "10.00"
        if "getAmbientAt(" in lua:
            return "-4.00"
        raise AssertionError(f"unexpected console command: {lua[:80]!r}")
    return fake_send


def _drive_thermo(rep, sweep: str, dump_returncode: int = 0,
                  dump_stdout: str = "[]", seed: int = 42, size: int = 128):
    """Run the real `_run` against fake launches; return `(rc, launches)`.

    `seed`/`size` are the requested generation inputs, so a caller can
    drive a size the engine NORMALIZES (#1757).
    """
    import types
    import thermo_altitude_probe as thermo  # type: ignore

    launches: dict = {}
    console_lua: list[str] = []

    def fake_boot(port, log=None, args=None, **_kw):
        launches["console"] = {"port": port, "log": log, "args": list(args or [])}
        return object()

    def fake_run(cmd, stdout=None, stderr=None, text=None, **_kw):
        launches["dump"] = {"cmd": list(cmd), "stderr": getattr(stderr, "name", None),
                            "stdout_piped": stdout is subprocess.PIPE}
        return types.SimpleNamespace(returncode=dump_returncode,
                                     stdout=dump_stdout)

    console = _thermo_console(sweep)

    def recording_send(port, lua, **kw):
        # The console half of the world-parameter comparison: `world.init`
        # is a formatted Lua string, so the only way to see what the FIRST
        # launch generated is to capture what was sent (#1757).
        console_lua.append(lua)
        return console(port, lua, **kw)

    saved = (thermo.boot, thermo.quit_engine, thermo.send, thermo.time,
             thermo.subprocess)
    thermo.boot = fake_boot
    thermo.quit_engine = lambda *a, **k: None
    thermo.send = recording_send
    thermo.time = types.SimpleNamespace(sleep=lambda _s: None)
    thermo.subprocess = types.SimpleNamespace(run=fake_run,
                                              PIPE=subprocess.PIPE)
    try:
        args = argparse.Namespace(port=9171, seed=seed, size=size,
                                  describe=False)
        rc = thermo._run(args, args.port, rep)
    finally:
        (thermo.boot, thermo.quit_engine, thermo.send, thermo.time,
         thermo.subprocess) = saved
    launches["console_lua"] = console_lua
    return rc, launches


def _thermo_init_params(console_lua):
    """`(seed, size, plates)` the console launch's `world.init` asked for."""
    for lua in console_lua:
        match = re.search(r"world\.init\(\s*\"[^\"]*\"\s*,\s*"
                          r"(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*\)", lua)
        if match:
            return tuple(int(group) for group in match.groups())
    return None


def _thermo_reported_line(printed):
    """The one standalone line naming the world-generation parameters."""
    lines = [line for line in printed.splitlines()
             if "seed" in line and "plates" in line]
    return lines


def _thermo_spoken(line, labels=("seed", "world size", "plates")):
    """The integers a standalone parameter line names, by label."""
    return tuple(int(found.group(1)) if found else None
                 for found in (re.search(rf"{label}\s+(-?\d+)", line)
                               for label in labels))


def _thermo_dump_params(cmd):
    """`(seed, size, plates)` the dump launch's argv asked for.

    Reads the values positionally out of the real argv rather than
    trusting a formatted string, so a flag renamed or dropped shows up as
    a missing value instead of a silent default (#1757).
    """
    values = {}
    for flag, key in (("--seed", "seed"), ("--worldSize", "size"),
                      ("--plates", "plates")):
        if flag in cmd:
            index = cmd.index(flag)
            if index + 1 < len(cmd):
                try:
                    values[key] = int(cmd[index + 1])
                except ValueError:
                    return None
    if set(values) != {"seed", "size", "plates"}:
        return None
    return values["seed"], values["size"], values["plates"]


def test_thermo_altitude_standalone() -> None:
    print("\n-- thermo_altitude probe migration --")
    done = subprocess.run(
        [sys.executable, "tools/thermo_altitude_probe.py", "--describe"],
        cwd=support.REPO_ROOT, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "thermo_altitude --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="thermo_altitude")
    except probe_protocol.ProtocolError as error:
        expect(False, f"thermo_altitude's descriptor is valid "
                      f"probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 5,
           f"thermo_altitude declares its five checks (got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "thermo_altitude's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "thermo_altitude's identifiers are all stable, word-like identifiers")
    # The labels these replaced led with their ordinal (`1 safety`,
    # `4 ice agreement`) and interpolated observed temperatures.
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "thermo_altitude's identifiers carry no runtime values")
    # Ice agreement is the ONE check allowed to end up MISSING, and
    # `Reporter.skip` does not advance the declared sequence, so it must
    # be declared last or a following check would be a harness error.
    expect(descriptor.ids[-1] == "ice_agreement",
           f"ice agreement is the last declared check, so its skip cannot "
           f"strand a successor (got {descriptor.ids})")

    # Standalone mode still prints the bracketed human markers, and
    # protocol mode never does.
    import thermo_altitude_probe as thermo  # type: ignore
    import io
    stream = io.StringIO()
    rep = probe_protocol.Reporter(thermo.DESCRIPTOR, stream=stream)
    rep.check("safety", True, "getAmbientAt never exceeds the regional mean")
    rep.abort("world never finished generating")
    expect("[PASS] getAmbientAt never exceeds the regional mean"
           in stream.getvalue(),
           "standalone thermo_altitude still prints its bracketed [PASS] line")
    expect("[FAIL] world never finished generating" in stream.getvalue(),
           "standalone thermo_altitude still prints a setup abort as [FAIL]")
    expect(rep.engine_args() == [],
           "standalone thermo_altitude passes no RTS override")
    expect(rep.engine_log_path(thermo.CONSOLE_LOG_NAME, thermo.CONSOLE_LOG)
           == thermo.CONSOLE_LOG,
           "standalone thermo_altitude keeps its own console engine-log path")
    expect(rep.engine_log_path(thermo.DUMP_LOG_NAME, thermo.DUMP_LOG)
           == thermo.DUMP_LOG,
           "standalone thermo_altitude keeps its own dump engine-log path")

    # The whole run, engine-free: both launches wired through the
    # reporter, four checks reported, and the fifth left MISSING.
    sweep = "0|-300,-300,-12.00|100,100,25.00,24.00|200,240,5.00,-2.00"
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_rep = probe_protocol.Reporter(
            thermo.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        before = stream.getvalue()
        rc, launches = _drive_thermo(protocol_rep, sweep)
        protocol_rep.close()
        expect(stream.getvalue() == before,
               "protocol mode prints nothing to stdout")
        expect(rc == 0,
               f"a run whose only unreported check was skipped still exits 0 "
               f"(got {rc})")

        # BOTH engine launches, not just the console one: the dump is
        # the specific way this migration goes wrong.
        console = launches.get("console", {})
        dump = launches.get("dump", {})
        expect(console.get("args") == ["+RTS", "-N4", "-RTS"],
               f"the console engine gets the harness's RTS capabilities "
               f"(got {console.get('args')})")
        expect(dump.get("cmd", [])[-3:] == ["+RTS", "-N4", "-RTS"],
               f"the ice-dump engine gets them too "
               f"(got {dump.get('cmd', [])[-3:]})")
        expect(console.get("log") == os.path.join(tmp, thermo.CONSOLE_LOG_NAME),
               f"the console engine logs into the harness's run directory "
               f"(got {console.get('log')})")
        expect(dump.get("stderr") == os.path.join(tmp, thermo.DUMP_LOG_NAME),
               f"the ice-dump engine's stderr does too "
               f"(got {dump.get('stderr')})")
        expect(console.get("log") != dump.get("stderr"),
               "the two launches use DISTINCT reporter-selected engine logs, "
               "so neither overwrites the other")
        expect(dump.get("stdout_piped") is True,
               "the dump's stdout stays a pipe, since its JSON is the payload")

        # ONE WORLD, TWO LAUNCHES (#1757). `ice_agreement` reads ice
        # coordinates out of the dump and samples ambient in the console
        # world, so the two engines must be given the same seed, world
        # size AND plate count. The dump used to be handed no plate count
        # at all and resolved the engine's `defaultPlatesFor` (9 at size
        # 128) against the console's literal 5.
        init_params = _thermo_init_params(launches.get("console_lua", []))
        dump_params = _thermo_dump_params(dump.get("cmd", []))
        expect(init_params is not None,
               f"the console launch's world.init names seed, world size and "
               f"plate count (got {launches.get('console_lua', [])[:1]})")
        expect(dump_params is not None,
               f"the dump launch's argv names --seed, --worldSize AND "
               f"--plates (got {dump.get('cmd', [])})")
        expect(init_params == dump_params,
               f"both engine launches generate the SAME world: console "
               f"{init_params} vs dump {dump_params} (seed, size, plates)")
        expect(init_params == (42, 128, thermo.PLATE_COUNT),
               f"both launches use the probe's single plate-count source "
               f"(got {init_params}, PLATE_COUNT={thermo.PLATE_COUNT})")
        expect("--plates" in dump.get("cmd", [])
               and "--ages" not in dump.get("cmd", []),
               "the dump uses the canonical --plates flag, not the legacy "
               "--ages alias")

        text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            text, thermo.DESCRIPTOR)
        expect(all(outcomes[cid] == "PASS" for cid in
                   ("safety", "bug_fix", "monotone", "arena_safety")),
               f"every check preceding ice agreement is reported (got {outcomes})")
        expect(outcomes["ice_agreement"] == "MISSING",
               f"an unsampleable ice region leaves ice agreement MISSING "
               f"rather than vacuously passing (got {outcomes['ice_agreement']})")
        expect('"level": "SKIP"' in text,
               "the empty ice sample is reported as a SKIP diagnostic")
        expect(probe_protocol.forbidden_marker_lines(text) == [],
               "the event stream itself holds no bracketed marker lines")

        # The parameter report reaches the STRUCTURED channel, and does
        # so on this very run — the one whose ice_agreement is MISSING,
        # which is where a reader most needs to know which world was
        # measured (#1757).
        reported = [event for event in _events
                    if isinstance(event, probe_protocol.DiagnosticEvent)
                    and {"seed", "world_size", "plates"} <= set(event.detail)]
        expect(len(reported) == 1,
               f"exactly one diagnostic event carries the world-generation "
               f"parameters (got {len(reported)})")
        if reported:
            detail = reported[0].detail
            expect((detail["seed"], detail["world_size"], detail["plates"])
                   == init_params,
                   f"the reported parameters are the ones both launches "
                   f"actually used (reported {detail}, launched {init_params})")

    # A FAILED second launch is a setup failure, never a MISSING check:
    # nonzero exit, undecodable stdout, and a non-list payload each abort.
    for label, rc_in, stdout_in in (
            ("a nonzero dump exit", 1, "[]"),
            ("undecodable dump stdout", 0, "cabal: error\n"),
            ("a non-list dump payload", 0, '{"tiles": []}')):
        with tempfile.TemporaryDirectory() as tmp:
            events = Path(tmp) / "events.jsonl"
            failing_rep = probe_protocol.Reporter(
                thermo.DESCRIPTOR, events_path=str(events),
                engine_log_dir=tmp, rts_caps=4, stream=stream)
            rc, _launches = _drive_thermo(failing_rep, sweep,
                                          dump_returncode=rc_in,
                                          dump_stdout=stdout_in)
            failing_rep.close()
            text = events.read_text(encoding="utf-8")
            _events, outcomes = probe_protocol.parse_event_stream(
                text, thermo.DESCRIPTOR)
            expect(rc == 1, f"{label} exits the probe nonzero (got {rc})")
            expect(outcomes["ice_agreement"] == "MISSING",
                   f"{label} leaves ice agreement MISSING")
            expect('"level": "SKIP"' not in text,
                   f"{label} is never reported as a legitimate skip")
            expect('"level": "WARN"' in text,
                   f"{label} is reported as a setup abort")

    # ...and the SAME parameters reach standalone output, where
    # `Reporter._diagnostic` prints only the human text and drops the
    # detail dict entirely (#1757).
    standalone = io.StringIO()
    standalone_rep = probe_protocol.Reporter(thermo.DESCRIPTOR,
                                             stream=standalone)
    rc, standalone_launches = _drive_thermo(standalone_rep, sweep)
    printed = standalone.getvalue()
    standalone_init = _thermo_init_params(
        standalone_launches.get("console_lua", []))
    expect(rc == 0, f"the standalone drive still exits 0 (got {rc})")
    expect(standalone_init == (42, 128, thermo.PLATE_COUNT),
           f"the standalone drive generates the same single-sourced world "
           f"(got {standalone_init})")
    parameter_lines = _thermo_reported_line(printed)
    expect(len(parameter_lines) == 1,
           f"standalone output carries exactly one world-parameter line "
           f"(got {parameter_lines})")
    line = parameter_lines[0] if parameter_lines else ""
    spoken = _thermo_spoken(line)
    expect(spoken == standalone_init,
           f"the standalone parameter line names seed, world size and plate "
           f"count by value (read {spoken} from {line!r}, launched "
           f"{standalone_init})")
    expect("requested" not in line,
           f"a size the engine does not normalize is reported plainly, with "
           f"no request/effective split (got {line!r})")

    # A size the engine NORMALIZES: `normalizeWorldSize` rounds 129 up to
    # 136, so reporting the REQUEST would name a world that was never
    # generated. Both launches still receive the same raw request and
    # normalize it identically, so they still generate ONE world (#1757).
    expect((thermo.normalize_world_size(129),
            thermo.normalize_world_size(128),
            thermo.normalize_world_size(1),
            thermo.normalize_world_size(thermo.MINIMUM_WORLD_SIZE))
           == (136, 128, thermo.MINIMUM_WORLD_SIZE, thermo.MINIMUM_WORLD_SIZE),
           "the probe mirrors normalizeWorldSize: round up to a multiple of "
           "the minimum, and clamp below it")
    expect(thermo.normalize_plate_count(thermo.PLATE_COUNT)
           == thermo.PLATE_COUNT,
           f"the probe's own plate count is already normal "
           f"(got {thermo.normalize_plate_count(thermo.PLATE_COUNT)})")

    normalizing = io.StringIO()
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        normalizing_rep = probe_protocol.Reporter(
            thermo.DESCRIPTOR, events_path=str(events), engine_log_dir=tmp,
            stream=normalizing)
        rc, odd_launches = _drive_thermo(normalizing_rep, sweep, size=129)
        normalizing_rep.close()
        odd_events, _outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), thermo.DESCRIPTOR)
        expect(rc == 0, f"the normalizing drive still exits 0 (got {rc})")
        odd_init = _thermo_init_params(odd_launches.get("console_lua", []))
        odd_dump = _thermo_dump_params(odd_launches.get("dump", {}).get("cmd", []))
        expect(odd_init == odd_dump == (42, 129, thermo.PLATE_COUNT),
               f"both launches still request the identical world, normalizing "
               f"or not (console {odd_init}, dump {odd_dump})")
        odd_reported = [event.detail for event in odd_events
                        if isinstance(event, probe_protocol.DiagnosticEvent)
                        and {"seed", "world_size", "plates"} <= set(event.detail)]
        expect(len(odd_reported) == 1,
               f"the normalizing run reports its parameters exactly once "
               f"(got {len(odd_reported)})")
        if odd_reported:
            expect(odd_reported[0].get("world_size") == 136
                   and odd_reported[0].get("requested_world_size") == 129,
                   f"the structured report names the GENERATED size 136 and "
                   f"keeps the requested 129 (got {odd_reported[0]})")

    odd_lines = _thermo_reported_line(normalizing.getvalue())
    expect(len(odd_lines) == 0,
           f"protocol mode still prints no parameter line (got {odd_lines})")

    plain = io.StringIO()
    rc, _ = _drive_thermo(
        probe_protocol.Reporter(thermo.DESCRIPTOR, stream=plain),
        sweep, size=129)
    odd_lines = _thermo_reported_line(plain.getvalue())
    expect(len(odd_lines) == 1,
           f"the normalizing standalone run prints one parameter line "
           f"(got {odd_lines})")
    odd_line = odd_lines[0] if odd_lines else ""
    expect(_thermo_spoken(odd_line) == (42, 136, thermo.PLATE_COUNT),
           f"standalone names the GENERATED world size, not the request "
           f"(read {_thermo_spoken(odd_line)} from {odd_line!r})")
    expect(_thermo_spoken(odd_line, ("requested",)) == (129,),
           f"and still names the request that produced it "
           f"(got {odd_line!r})")

    # The non-empty sampling path, also engine-free: a warm ice tile fails.
    stream = io.StringIO()
    rep = probe_protocol.Reporter(thermo.DESCRIPTOR, stream=stream)
    tiles = [{"x": 10, "y": 20, "iceSurf": 4},
             {"x": 11, "y": 20, "iceSurf": 4, "glacierZone": True},
             {"x": 12, "y": 20, "iceSurf": None}]
    expect(len(thermo.interior_ice(tiles)) == 1,
           "polar glacier bands and ice-free tiles are excluded from the sample")
    expect(thermo.report_ice_agreement(rep, tiles, (0, 0, 1, 1),
                                       lambda _x, _y: -3.0) is True,
           "ice tiles at/below freezing pass ice agreement")
    expect(thermo.report_ice_agreement(rep, tiles, (0, 0, 1, 1),
                                       lambda _x, _y: 9.0) is False,
           "an ice tile reading above freezing fails ice agreement")
    expect(thermo.report_ice_agreement(rep, tiles, (0, 0, 1, 1),
                                       lambda _x, _y: None) is False,
           "an unreadable ambient on an ice tile fails ice agreement")


TESTS = (test_thermo_altitude_standalone,)
