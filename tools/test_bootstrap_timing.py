#!/usr/bin/env python3
"""Engine-free self-test for ``tools/bootstrap_timing.py`` (#2948).

Covers every part of the timing tool that needs no engine: the median, the
parsing of the loader's aggregate lines and of every console reply, the
refusal to treat a failed or incomplete load as a sample, the unloaded-baseline
check, the aggregation and the signed difference, port 8008 and repeat-count
refusal, the READY wait against a stand-in process, and two source guards that
keep the tool honest about what it measures (path A is still
``cargo_capacity_probe.bootstrap()``'s loop; the family inventory is still the
loader's arena queue; ``send`` is never given a custom idle).

Usage:
  python3 tools/test_bootstrap_timing.py [-v]
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import ast
import contextlib
import io
import json
import re
import sys
import tempfile
import time
from pathlib import Path

import selftestlib
from selftestlib import FAILURES, expect

import bootstrap_timing as bt
import probelib

TOOLS = Path(__file__).resolve().parent
REPO = TOOLS.parent


def raises(fn, *args, kind=bt.MeasurementError) -> str | None:
    """The message ``fn`` raised as ``kind``, or ``None`` if it did not."""
    try:
        fn(*args)
    except kind as error:
        return str(error)
    return None


# --------------------------------------------------------------------------
def aggregate_log(skip=(), dup=(), extra=()) -> str:
    lines = ["2026-11-07 21:36:35 [INFO] [Lua] [startup_loader.lua:530] "
             "Startup loader queued 104 items for profile arena"]
    totals = {"material": (219, 13), "vegetation": (77, 5), "substance": (16, 4),
              "infection": (8, 1), "recipe": (19, 6), "item": (67, 39),
              "equipment": (1, 1), "building": (8, 8), "faction": (8, 1),
              "physiology": (11, 1), "unit": (8, 8), "loot_table": (1, 1),
              "loot_profile": (1, 1), "location": (1, 1)}
    for fam in bt.FAMILY_IDS:
        if fam in skip:
            continue
        total, files = totals[fam]
        row = (f"2026-11-07 21:36:35 [INFO] [Lua] [startup_loader.lua:147] "
               f"Startup assets: {fam} loaded {total} from {files} file(s)")
        lines.append(row)
        if fam in dup:
            lines.append(row)
    for fam in extra:
        lines.append(f"[INFO] Startup assets: {fam} loaded 1 from 1 file(s)")
    return "\n".join(lines) + "\n"


def test_median_and_stat_block() -> None:
    print("\n-- median")
    expect(bt.median([3]) == 3.0, "a single value is its own median")
    expect(bt.median([5, 1, 3]) == 3.0, "odd count takes the middle value")
    expect(bt.median([4, 1, 3, 2]) == 2.5, "even count averages the middle pair")
    expect(raises(bt.median, []) is not None, "no values is refused")
    expect(raises(bt.median, [1.0, float("nan")]) is not None, "nan is refused")
    expect(raises(bt.median, [1.0, float("inf")]) is not None, "inf is refused")
    expect(raises(bt.median, [1.0, "2"]) is not None, "a string is refused")
    block = bt.stat_block([2, 4, 3])
    expect(block == {"values": [2.0, 4.0, 3.0], "median": 3.0},
           "a stat block keeps each value in order and the median")


def test_return_counts() -> None:
    print("\n-- loader return counts")
    for raw, want in (("39", 39), ("39.0", 39), ("0", 0), ("0.0", 0), (" 7 ", 7)):
        expect(bt.parse_return_count(raw) == want, f"{raw!r} reads as {want}")
    for raw in ("", "ok", "nil", "error: x", "-1", "2.5", "nan", "inf",
                "table: 0x1", "1e999", None):
        expect(raises(bt.parse_return_count, raw) is not None,
               f"{raw!r} is a malformed reply, never a zero")


def test_aggregates() -> None:
    print("\n-- loader aggregate lines")
    parsed = bt.parse_aggregates(aggregate_log())
    expect(len(parsed) == 14 and parsed[0] == ("material", 219, 13),
           "all fourteen aggregates parse in log order")
    inventory = bt.aggregate_inventory(parsed)
    expect(list(inventory) == bt.FAMILY_IDS, "the inventory is in queue order")
    expect(inventory["item"] == {"files": 39, "returned_total": 67},
           "files and the returned total are kept apart")
    expect(bt.aggregate_inventory(
        [(f, 0, 1) for f in bt.FAMILY_IDS])["unit"]["returned_total"] == 0,
        "a legitimate zero total is valid")
    expect(bt.aggregate_inventory(
        [(f, 0, 0) for f in bt.FAMILY_IDS])["unit"]["files"] == 0,
        "a legitimate zero-file family parses (the loader fails it itself)")
    missing = raises(bt.aggregate_inventory,
                     bt.parse_aggregates(aggregate_log(skip=("unit",))))
    expect(missing is not None and "unit" in missing,
           f"a missing aggregate is refused ({missing})")
    dupe = raises(bt.aggregate_inventory,
                  bt.parse_aggregates(aggregate_log(dup=("item",))))
    expect(dupe is not None and "duplicate" in dupe,
           f"a duplicate aggregate is refused ({dupe})")
    extra = raises(bt.aggregate_inventory,
                   bt.parse_aggregates(aggregate_log(extra=("flora",))))
    expect(extra is not None and "flora" in extra,
           f"an unexpected family is refused ({extra})")
    bad = raises(bt.parse_aggregates,
                 "[INFO] Startup assets: item loaded many from 39 file(s)\n")
    expect(bad is not None and "malformed" in bad,
           "a malformed aggregate line is refused, not skipped")
    expect(bt.parse_aggregates("[INFO] nothing here\n") == [],
           "a log with no aggregate lines parses to none")
    expect(raises(bt.aggregate_inventory, []) is not None,
           "an empty inventory is refused rather than read as zero totals")


def test_path_b_status() -> None:
    print("\n-- path B status")
    expect(raises(bt.parse_status_b, "done=true;failed=false;msg=") is None,
           "done and not failed is accepted")
    for raw, why in (
            ("done=false;failed=false;msg=", "not done"),
            ("done=false;failed=true;msg=Startup failed: x could not parse y",
             "failed"),
            ("done=true;failed=true;msg=boom", "done but failed"),
            ("", "an empty reply"),
            (None, "no reply"),
            ("ok", "a stray reply"),
            ("done=maybe;failed=false;msg=", "a garbled flag")):
        expect(raises(bt.parse_status_b, raw) is not None, f"{why} is refused")
    msg = raises(bt.parse_status_b, "done=false;failed=true;msg=Startup failed: a")
    expect(msg is not None and "Startup failed: a" in msg,
           "a loader failure message is carried into the error")


def registry_reply(**over) -> str:
    counts = {k: 0 for k in bt.REGISTRY_QUERIES}
    counts.update(over)
    return ";".join(f"{k}={v}" for k, v in counts.items())


def test_registry_and_verbs() -> None:
    print("\n-- registry and call-stat replies")
    counts = bt.parse_registry_counts(registry_reply(item=67))
    expect(counts["item"] == 67 and counts["unit"] == 0, "counts parse by name")
    for raw, why in ((registry_reply(item=-1), "a query that raised"),
                     (registry_reply(item=-2), "a non-table answer"),
                     ("", "an empty reply"),
                     ("item=1", "a reply missing registries"),
                     (registry_reply() + ";extra=1", "an unknown registry"),
                     ("item=x", "a garbled value")):
        expect(raises(bt.parse_registry_counts, raw) is not None, f"{why} is refused")
    verbs = bt.parse_load_verbs(
        "load_verbs|engine.loadItemYaml=39:4000;engine.loadUnitYaml=8:1812000000")
    expect(verbs["engine.loadUnitYaml"] == {"count": 8, "total_ns": 1812000000},
           "call-stat rows parse")
    expect(bt.parse_load_verbs("load_verbs|") == {}, "no loader verbs is an empty map")
    for raw in ("", "engine.loadItemYaml=1:2", "load_verbs|engine.loadItemYaml=x"):
        expect(raises(bt.parse_load_verbs, raw) is not None,
               f"{raw!r} is refused")
    expect("engine.loadYaml" not in re.findall(
        r"'\^engine%\.load%a%a\*Yaml\$'", bt.LOAD_VERBS_LUA) and
        "%a%a*Yaml" in bt.LOAD_VERBS_LUA,
        "the verb filter needs a family name, so the generic loadYaml is out")


def test_baseline() -> None:
    print("\n-- unloaded baseline")
    clean_verbs = {}
    clean_registry = {k: 0 for k in bt.REGISTRY_QUERIES}
    expect(bt.check_baseline(clean_verbs, clean_registry, "READY port=1\n") == [],
           "no calls, empty registries and a quiet log is a baseline")
    expect(bt.check_baseline(
        {"engine.loadItemYaml": {"count": 0, "total_ns": 0}},
        clean_registry, "") == [], "a verb present with count 0 is still clean")
    for verbs, registry, log, needle in (
            ({"engine.loadItemYaml": {"count": 2, "total_ns": 9}}, clean_registry,
             "", "loadItemYaml was already called"),
            ({}, {**clean_registry, "unit": 3}, "", "unit registry"),
            ({}, clean_registry, "Startup assets: item loaded 1 from 1 file(s)",
             "aggregates"),
            ({}, clean_registry, "Startup loader queued 104 items", "already ran")):
        problems = bt.check_baseline(verbs, registry, log)
        expect(any(needle in p for p in problems),
               f"a warm engine is caught ({needle})")


def test_engine_side_delta() -> None:
    print("\n-- engine-side call delta")
    before = {"engine.loadItemYaml": {"count": 0, "total_ns": 0}}
    after = {"engine.loadItemYaml": {"count": 39, "total_ns": 4_000_000},
             "engine.loadUnitYaml": {"count": 8, "total_ns": 1_800_000_000}}
    d = bt.engine_side_delta(before, after)
    expect(d["load_calls"] == 47, "calls are summed across verbs")
    expect(abs(d["load_seconds"] - 1.804) < 1e-9, "inclusive time is summed")
    expect(d["by_verb"]["engine.loadUnitYaml"]["calls"] == 8, "rows are per verb")
    expect(raises(bt.engine_side_delta, after, before) is not None,
           "a call window that went backwards is refused")


def load_reply(n="3", parsed="true", refused=""):
    return f"n={n};parsed={parsed};refused={refused}"


def test_load_replies_and_paths() -> None:
    print("\n-- path A replies and the measurement paths")
    expect(bt.parse_load_reply(load_reply("39.0")) == 39, "a counted file parses")
    expect(bt.parse_load_reply(load_reply("0")) == 0,
           "a successfully parsed file that registered nothing is a valid zero")
    for raw, why in ((load_reply("0", "false"), "a file that failed to parse"),
                     (load_reply("0", "nil"), "a reply with no parse outcome"),
                     (load_reply("3", "true", "dup_name"), "a refused file"),
                     (load_reply("ok"), "a non-numeric count"),
                     (load_reply("-1"), "a negative count"),
                     ("", "an empty reply"), ("ok", "a stray reply")):
        expect(raises(bt.parse_load_reply, raw) is not None, f"{why} is refused")

    # Drive the real path A against the real file lists with a fake console.
    sent = []

    def console(port, lua, **kw):
        sent.append(lua)
        return load_reply("2")

    result = bt.run_path_a(9473, REPO, send_fn=console)
    files = {fam: 0 for fam in bt.PATH_A_FAMILIES}
    from glob import glob
    for pattern, _ in bt.PATH_A_LOADERS:
        fam = bt.FAMILY_BY_DIR[pattern.split("/*")[0]]
        files[fam] += len(glob(pattern, root_dir=REPO))
    expect(result["round_trips"] == len(sent) == sum(files.values()) > 0,
           "path A makes exactly one console call per file")
    expect(all(result["families"][f]["files"] == files[f] for f in files),
           "per-family file counts follow the globs")
    expect(all(result["families"][f]["returned_total"] == 2 * files[f]
               for f in files), "returned totals are summed per family")
    expect(sent[0].count("true") >= 1 and ", true)" in sent[0]
           and "engine.loadSubstanceYaml" in sent[0],
           "the first call is the first family's loader, opted in to the outcome")
    first = [s for s in sent if "loadItemYaml" in s][0]
    expect("data/items/" in first and first.count(";") >= 2,
           "each call names one file")

    zero = bt.run_path_a(9473, REPO, send_fn=lambda *a, **k: load_reply("0"))
    expect(all(r["returned_total"] == 0 for r in zero["families"].values()),
           "successful zero counts pass through path A as zero totals")

    for bad, why in ((load_reply("0", "false"), "a parse failure"),
                     (load_reply("5", "true", "dup"), "a refusal"),
                     ("garbled", "a malformed reply")):
        msg = raises(bt.run_path_a, 9473, REPO, lambda *a, **k: bad)
        expect(msg is not None and "data/" in msg,
               f"path A fails the run on {why}, naming the file")

    # Path B against a temp log with the loader's real aggregate spelling.
    with tempfile.TemporaryDirectory() as tmp:
        log = Path(tmp) / "engine.log"
        log.write_text(aggregate_log())
        ok = bt.run_path_b(9473, str(log),
                           send_fn=lambda *a, **k: "done=true;failed=false;msg=")
        expect(ok["round_trips"] == 1 and ok["loader_status"]["failed"] is False
               and list(ok["families"]) == bt.FAMILY_IDS,
               "path B succeeds on a done, not-failed loader with all aggregates")
        for reply in ("done=false;failed=true;msg=Startup failed: x",
                      "done=true;failed=true;msg=x", "", None):
            expect(raises(bt.run_path_b, 9473, str(log),
                          lambda *a, _r=reply, **k: _r) is not None,
                   f"path B fails the run on {reply!r}")
        log.write_text(aggregate_log(skip=("unit",)))
        t = time.monotonic()
        short = raises(bt.run_path_b, 9473, str(log),
                       lambda *a, **k: "done=true;failed=false;msg=")
        expect(short is not None and "unit" in short and time.monotonic() - t < 30,
               "path B fails the run when an aggregate is missing")

    # Post-load registry validation.
    fams = {"item": {"files": 1, "returned_total": 2},
            "equipment": {"files": 1, "returned_total": 0}}
    registry = {k: 0 for k in bt.REGISTRY_QUERIES}
    registry["item"] = 2
    expect(raises(bt.require_registered, "A", registry, fams) is None,
           "an empty registry beside a zero total is a legitimate empty family")
    registry["item"] = 0
    msg = raises(bt.require_registered, "A", registry, fams)
    expect(msg is not None and "item" in msg,
           "a positive total with an empty registry is refused")
    expect(raises(bt.require_registered, "B",
                  registry, {"vegetation": {"files": 1, "returned_total": 5}})
           is None, "a family with no list verb is not judged by a registry")


# --------------------------------------------------------------------------
def sample(path, index, wall, boot=3.0, trips=None, rt=0.31, families=None,
           side=1.9):
    fams = families if families is not None else (
        {"unit": {"files": 8, "returned_total": 8}} if path == "A"
        else {"unit": {"files": 8, "returned_total": 8},
              "recipe": {"files": 6, "returned_total": 19}})
    row = {"path": path, "index": index, "engine_boot_s": boot,
           "load_wall_s": wall,
           "round_trips": trips if trips is not None else (75 if path == "A" else 1),
           "families": fams, "engine_side": {"load_seconds": side, "load_calls": 1,
                                             "by_verb": {}},
           "registry_after": {k: 1 for k in bt.REGISTRY_QUERIES}}
    if path == "A":
        row["median_round_trip_s"] = rt
        row["round_trip_s"] = [rt] * 75
    return row


def test_summary() -> None:
    print("\n-- aggregation and the signed difference")
    samples = []
    for i, (a, b) in enumerate(((24.0, 2.2), (25.0, 2.1), (23.0, 2.3)), 1):
        samples += [sample("A", i, a, boot=3.0 + i / 10),
                    sample("B", i, b, boot=3.1)]
    s = bt.summarise(samples)
    expect(s["path_a"]["load_wall_s"]["median"] == 24.0, "A median is the median")
    expect(s["path_b"]["load_wall_s"]["median"] == 2.2, "B median is the median")
    expect(abs(s["difference_s"]["value"] - 21.8) < 1e-9,
           "the difference is median(A) minus median(B)")
    est = s["estimate_against_historical_probe_engine_cost"]
    expect(est["denominator_s"] == 34.0 and "estimate" in est["denominator_source"],
           "the 34 s denominator is labelled an estimate")
    expect(abs(est["path_a_fraction"] - 24.0 / 34.0) < 1e-12, "A fraction of 34 s")
    expect(abs(est["signed_saving_fraction"] - 21.8 / 34.0) < 1e-12,
           "the saving is signed")
    expect(s["path_a"]["round_trips"] == 75 and s["path_b"]["round_trips"] == 1,
           "round trips are reported per path")
    expect(abs(s["path_a"]["idle_floor_s"] - 75 * bt.CONSOLE_IDLE_S) < 1e-12,
           "the idle floor is trips times the console idle")
    expect(len(s["engine_boot_s"]["all"]["values"]) == 6,
           "boot has one value per engine")
    slow_b = [sample("A", i, 2.0) for i in (1, 2, 3)] + \
             [sample("B", i, 5.0) for i in (1, 2, 3)]
    expect(bt.summarise(slow_b)["difference_s"]["value"] == -3.0,
           "a slowdown is reported as a negative difference, not hidden")
    short = [x for x in samples if not (x["path"] == "B" and x["index"] == 3)]
    expect(raises(bt.summarise, short) is not None,
           "fewer than three engines per path is refused")
    drift = [dict(x) for x in samples]
    drift[2] = sample("A", 2, 25.0, families={"unit": {"files": 8,
                                                       "returned_total": 7}})
    expect(raises(bt.summarise, drift) is not None,
           "repeats that registered different totals are refused")
    trips = [dict(x) for x in samples]
    trips[2] = sample("A", 2, 25.0, trips=74)
    expect(raises(bt.summarise, trips) is not None,
           "repeats with different round-trip counts are refused")


def test_report_and_table() -> None:
    print("\n-- report document and table")
    samples = []
    for i in (1, 2, 3):
        samples += [sample("A", i, 24.0 + i), sample("B", i, 2.0 + i / 10)]
    report = bt.build_report(samples, {"port": 9473, "repo_head": "abc"})
    expect(report["schema"] == "bootstrap-timing/v1", "schema is v1")
    expect(json.loads(json.dumps(report)) == report, "the report is plain JSON")
    expect([r["family"] for r in report["families"]] == bt.FAMILY_IDS,
           "every arena family has a row")
    unit = next(r for r in report["families"] if r["family"] == "unit")
    expect(unit["returned_count_unit"] == "definitions" and unit["path_a"]["files"] == 8
           and unit["path_b"]["files"] == 8, "a family row carries unit and counts")
    vegetation = next(r for r in report["families"] if r["family"] == "vegetation")
    expect(vegetation["path_a"] is None and vegetation["returned_count_unit"]
           == "textures", "a B-only family has no path A row and says textures")
    table = bt.render_table(report)
    expect("median(A) - median(B)" in table and "estimate" in table
           and "vegetation" in table and "idle" in table,
           "the table states the signed difference, the estimate label and families")
    expect(not table.endswith("\n"), "the table is plain text")


# --------------------------------------------------------------------------
def test_ports_and_arguments() -> None:
    print("\n-- port and repeat refusal")
    expect(raises(bt.refuse_gui_port, 8008) is not None, "8008 is refused")
    expect(raises(bt.refuse_gui_port, 9473) is None, "another port is accepted")
    out = TOOLS / "does_not_exist_report.json"
    for argv in (["--output", str(out), "--port", "8008"],
                 ["--output", str(out), "--repeats", "2"],
                 ["--output", str(out), "--port", "80"],
                 []):
        try:
            with contextlib.redirect_stderr(io.StringIO()):
                bt.parse_args(argv)
            expect(False, f"{argv} is refused")
        except SystemExit as stop:
            expect(stop.code == 2, f"{argv} is refused at the command line")
    args = bt.parse_args(["--output", str(out)])
    expect(args.port == bt.DEFAULT_PORT and args.repeats == 3,
           "the defaults are a non-GUI port and three fresh engines")
    expect(bt.DEFAULT_PORT != 8008, "the default port is not the GUI port")
    expect(not out.exists(), "refusals write nothing")


def test_launch_and_ready() -> None:
    print("\n-- READY wait against a stand-in process")
    with tempfile.TemporaryDirectory() as tmp:
        log = str(Path(tmp) / "engine.log")
        code = ("import sys,time; time.sleep(0.4); print('READY port=9999', "
                "flush=True); time.sleep(30)")
        proc, seconds = bt.launch([sys.executable, "-c", code], log, 9999,
                                  poll=0.02)
        try:
            expect(0.35 <= seconds < 5.0,
                   f"the boot interval spans launch to READY ({seconds:.2f} s)")
        finally:
            proc.kill()
            proc.wait()
        code = "import sys; print('READY port=1234', flush=True); sys.stdout.flush(); import time; time.sleep(30)"
        err = raises(bt.launch, [sys.executable, "-c", code], log, 9999, None, 0.6, 0.02)
        expect(err is not None and "never printed READY" in err,
               "another process's READY (a different port) does not count")
        err = raises(bt.launch, [sys.executable, "-c", "print('hi')"], log, 9999,
                     None, 5.0, 0.02)
        expect(err is not None and "exited before READY" in err,
               "an engine that dies before READY is a failed measurement")
        code = "import time; print('NOTREADY port=9999', flush=True); time.sleep(30)"
        err = raises(bt.launch, [sys.executable, "-c", code], log, 9999, None, 0.5, 0.02)
        expect(err is not None, "READY must be the whole marker, not a substring")
    env = bt.engine_env({"ENGINE_DEBUG": "Asset", "PATH": "/bin"})
    expect("ENGINE_DEBUG" not in env and env["PATH"] == "/bin",
           "ENGINE_DEBUG is dropped so per-file DEBUG lines cannot perturb the load")
    expect(bt.READY_POLL_S <= 0.1, "the READY poll is finer than probelib's 0.4 s")


# --------------------------------------------------------------------------
def test_source_guards() -> None:
    print("\n-- source guards")
    # Path A is the per-file loop cargo_capacity_probe.bootstrap() ran until
    # #2952 moved it to probelib.load_definitions. bootstrap_timing keeps its
    # own frozen copy as the baseline; it must stay the helper's STANDARD
    # family set, in order, so "before" and "after" load the same files.
    standard = [(probelib.DEFINITION_FAMILIES[name].pattern,
                 probelib.DEFINITION_FAMILIES[name].verb)
                for name in probelib.STANDARD_FAMILIES]
    expect(standard == bt.PATH_A_LOADERS,
           "PATH_A_LOADERS is probelib.STANDARD_FAMILIES' patterns and verbs, in order")
    expect(len(standard) == 8, "the standard loop has eight families")
    tree = ast.parse((TOOLS / "cargo_capacity_probe.py").read_text())
    func = next(n for n in ast.walk(tree)
                if isinstance(n, ast.FunctionDef) and n.name == "bootstrap")
    calls = [n for n in ast.walk(func) if isinstance(n, ast.Call)
             and getattr(n.func, "id", "") == "load_definitions"]
    expect(len(calls) == 1 and len(calls[0].args) == 1 and not calls[0].keywords,
           "cargo_capacity_probe.bootstrap() loads the STANDARD set in one "
           "load_definitions call")
    expect(not any(isinstance(n, ast.Call) and getattr(n.func, "attr", "") == "glob"
                   for n in ast.walk(func)),
           "and no longer carries its own per-file loop")

    # The inventory is the loader's arena queue, in order, and the unit table
    # covers every family.
    lua = (REPO / "scripts" / "startup_loader.lua").read_text()
    arena = lua[lua.index("local function queueArenaProfile"):
                lua.index("function startupLoader.build")]
    queued = re.findall(r'addYaml(?:Dir|Tree)\("(data/[a-z_]+)"[^,]*,\s*"[^"]*",?\s*'
                        r'(engine\.\w+)', arena)
    expect([d for d, _ in queued] == [f[1] for f in bt.FAMILIES],
           "FAMILIES is the arena queue's directories, in queue order")
    expect([v for _, v in queued] == [f[2] for f in bt.FAMILIES],
           "FAMILIES carries each family's loader verb")
    ids = dict(re.findall(r'\["(data/[a-z_]+)"\]\s*=\s*"(\w+)"', lua))
    expect(all(ids[f[1]] == f[0] for f in bt.FAMILIES),
           "each family id is the loader's own aggregate identifier")
    expect(set(bt.FAMILY_UNIT) == set(bt.FAMILY_IDS)
           and set(bt.LOAD_VERB_BY_FAMILY) == set(bt.FAMILY_IDS),
           "every family has a unit and a verb")
    expect(set(bt.PATH_A_FAMILIES) <= set(bt.FAMILY_IDS)
           and len(bt.PATH_A_FAMILIES) == 8, "path A's families are a subset of B's")
    expect(arena.count("addYaml") == 14 and "queueNormal" not in arena,
           "the arena profile still queues fourteen families")
    queued_verbs = {v for _, v in queued}
    expect(all(v in queued_verbs for _, v in bt.PATH_A_LOADERS),
           "every path A verb is also queued by path B")

    # Both paths use send()'s default idle: no call passes idle=.
    source = (TOOLS / "bootstrap_timing.py").read_text()
    tree = ast.parse(source)
    sends = [n for n in ast.walk(tree) if isinstance(n, ast.Call)
             and getattr(n.func, "id", "") in ("send", "send_fn")]
    expect(len(sends) >= 4 and not any(k.arg == "idle" for n in sends
                                       for k in n.keywords),
           "send() is never given a custom idle")
    expect("def " + "send" not in source and "_thread" not in source,
           "the tool uses probelib.send, not a private console client")
    expect("--headless" in source and "--arena" in source and "8008" not in
           [a for a in re.findall(r'"(\d+)"', source)],
           "the engine is booted --headless --arena")
    expect("export const" not in source, "no stray script text")


def main() -> int:
    selftestlib.parse_verbose()
    for test in (test_median_and_stat_block, test_return_counts, test_aggregates,
                 test_path_b_status, test_registry_and_verbs, test_baseline,
                 test_engine_side_delta, test_load_replies_and_paths, test_summary, test_report_and_table,
                 test_ports_and_arguments, test_launch_and_ready,
                 test_source_guards):
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} FAILED:")
        for failure in FAILURES:
            print(f"  - {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "bootstrap_timing self-test: all tests passed")


if __name__ == "__main__":
    sys.exit(main())
