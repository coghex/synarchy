#!/usr/bin/env python3
"""Time the probes' per-file definition loading against the startup loader's
one-call arena profile (#2948, report finding PRB-15).

This is a measurement for ONE question, not a probe: seventy-one files under
``tools/`` load the game definitions into each engine they boot with one
debug-console round trip per YAML file; ``scripts/startup_loader.lua`` has an
``arena`` profile that drains the whole queue in one console call. How much of
a probe engine's cost is the first, and what would the second save? Nothing
here changes a probe, ``probelib`` or the loader, and nothing here is a
rollout.

Each measurement boots a FRESH ``--headless --arena`` engine on an isolated
resource root and a non-8008 port, proves the authored registries are empty,
and times exactly one path on it:

* **path A** -- ``tools/cargo_capacity_probe.py`` ``bootstrap()``'s eight-family
  loop and nothing after it (family order, sorted flat globs, one console call
  per file). The only difference is that the call returns the loader's count
  instead of the constant ``'ok'``, so the count is captured WITHOUT adding a
  round trip.
* **path B** -- ``require('scripts.startup_loader').build('arena')`` then
  ``runAll()`` as ONE console call that also answers the loader's own
  ``isDone()`` / ``isFailed()``, so a missing result cannot become a timeout
  inside the measurement.

Both paths use ``probelib.send`` with its default idle wait, because that wait
is part of what a probe pays per round trip. Engine preparation (a build, the
``cabal-build`` lock), resource-root staging, the baseline check, result
aggregation and teardown all fall OUTSIDE every timed interval. The engine
boot interval is process launch to this process observing that process's own
``READY port=<port>`` line, on a monotonic clock, polled every
``READY_POLL_S``.

An unsuccessful or incomplete load is a FAILED measurement, never a fast
sample: the run exits non-zero and writes no report.

Usage:
  python3 tools/bootstrap_timing.py --output /path/to/report.json [--port 9473]
                                    [--repeats 3]
"""
from __future__ import annotations

import argparse
import json
import math
import os
import platform
import re
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path

import probe_engine
import probelib
from probelib import quit_engine, send

SCHEMA = "bootstrap-timing/v1"
REPO = Path(__file__).resolve().parent.parent

DEFAULT_PORT = 9473
MIN_REPEATS = 3
#: ``probelib.boot`` polls the log every 0.4 s; this tool polls faster so the
#: boot interval is not rounded up by a probe-sized granularity. Reported.
READY_POLL_S = 0.05
READY_TIMEOUT_S = 180.0
#: The console's per-round-trip idle wait, read from ``probelib`` so the report
#: states what ``send`` really does rather than a copy of it.
CONSOLE_IDLE_S = probelib.DEFAULT_IDLE
#: The roughly-34-second per-engine cost of the multi-engine probes. It is an
#: INFERENCE recorded in ``docs/probe_value_findings.md`` PRB-15 (a probe total
#: divided by its engine count), not something this tool measures.
PROBE_ENGINE_COST_ESTIMATE_S = 34.0
PROBE_ENGINE_COST_SOURCE = (
    "docs/probe_value_findings.md PRB-15: an estimate (a probe total divided "
    "by its engine count), not a newly measured end-to-end probe total")

#: Path A, exactly as ``tools/cargo_capacity_probe.py`` ``bootstrap()`` loops
#: it. ``tools/test_bootstrap_timing.py`` compares this with that source.
PATH_A_LOADERS: list[tuple[str, str]] = [
    ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
    ("data/items/*.yaml",      "engine.loadItemYaml"),
    ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
    ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
    ("data/factions/*.yaml",   "engine.loadFactionYaml"),
    ("data/physiology/*.yaml", "engine.loadPhysiologyYaml"),
    ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
]

#: The arena profile's families in ``queueArenaProfile`` order: aggregate id,
#: data directory, loader verb, and WHAT the loader's returned count counts.
#: The unit differs by family (materials and vegetation return TEXTURE totals,
#: factions declarations, physiology systems, loot tables and profiles 0 or 1
#: per file), so a total is only comparable within one family.
#: ``tools/test_bootstrap_timing.py`` compares this with the loader source.
FAMILIES: list[tuple[str, str, str, str]] = [
    ("material",     "data/materials",     "engine.loadMaterialYaml",     "textures"),
    ("vegetation",   "data/vegetation",    "engine.loadVegetationYaml",   "textures"),
    ("substance",    "data/substances",    "engine.loadSubstanceYaml",    "definitions"),
    ("infection",    "data/infections",    "engine.loadInfectionYaml",    "definitions"),
    ("recipe",       "data/recipes",       "engine.loadRecipeYaml",       "definitions"),
    ("item",         "data/items",         "engine.loadItemYaml",         "definitions"),
    ("equipment",    "data/equipment",     "engine.loadEquipmentYaml",    "classes"),
    ("building",     "data/buildings",     "engine.loadBuildingYaml",     "definitions"),
    ("faction",      "data/factions",      "engine.loadFactionYaml",      "declarations"),
    ("physiology",   "data/physiology",    "engine.loadPhysiologyYaml",   "systems"),
    ("unit",         "data/units",         "engine.loadUnitYaml",         "definitions"),
    ("loot_table",   "data/loot_tables",   "engine.loadLootTableYaml",    "tables (0 or 1 per file)"),
    ("loot_profile", "data/loot_profiles", "engine.loadLootProfileYaml",  "profiles (0 or 1 per file)"),
    ("location",     "data/locations",     "engine.loadLocationYaml",     "definitions"),
]
FAMILY_IDS = [f[0] for f in FAMILIES]
FAMILY_BY_DIR = {f[1]: f[0] for f in FAMILIES}
FAMILY_UNIT = {f[0]: f[3] for f in FAMILIES}
LOAD_VERB_BY_FAMILY = {f[0]: f[2] for f in FAMILIES}

#: Existing registry queries, one expression each. A family with no list verb
#: (faction, vegetation, loot_table) is evidenced by the call-stat verb count
#: and the absence of its loader aggregate instead. ``physiology`` counts the
#: registry's top-level entries and reads ``nil`` as empty; ``material`` counts
#: materials, not the textures the loader's total counts.
REGISTRY_QUERIES: dict[str, str] = {
    "substance":    "substance.getNames()",
    "item":         "item.listDefs()",
    "equipment":    "equipment.getClassNames()",
    "material":     "world.listMaterials()",
    "building":     "building.listDefs()",
    "unit":         "unit.listDefs()",
    "physiology":   "engine.getPhysiologyRegistry()",
    "infection":    "infection.getNames()",
    "recipe":       "craft.getNames()",
    "location":     "engine.listLocationDefs()",
    "loot_profile": "loot.listProfiles()",
}
PATH_A_FAMILIES = [FAMILY_BY_DIR[p.split("/*")[0]] for p, _ in PATH_A_LOADERS]

REGISTRY_LUA = (
    "local function n(f) local ok, t = pcall(f); "
    "if not ok then return -1 end; if t == nil then return 0 end; "
    "if type(t) ~= 'table' then return -2 end; "
    "local c = 0; for _ in pairs(t) do c = c + 1 end; return c end; "
    "return string.format('"
    + ";".join(f"{k}=%d" for k in REGISTRY_QUERIES)
    + "', "
    + ", ".join(f"n(function() return {e} end)" for e in REGISTRY_QUERIES.values())
    + ")")

#: The per-family loader verbs' call counts and inclusive engine-side time, read
#: from the unreset ``debug.getLuaCallStats`` window. ``engine.loadYaml`` (the
#: generic loader) and ``loadFont``/``loadScript``/``loadTexture`` do not match.
LOAD_VERBS_LUA = (
    "local out = {}; for _, v in ipairs(debug.getLuaCallStats().verbs) do "
    "if v.id:match('^engine%.load%a%a*Yaml$') then "
    "out[#out + 1] = string.format('%s=%d:%d', v.id, v.count, v.totalDurationNs) "
    "end end; return 'load_verbs|' .. table.concat(out, ';')")

#: Path B as ONE console call, answering its own completion and failure.
PATH_B_LUA = (
    "local sl = require('scripts.startup_loader'); sl.build('arena'); sl.runAll(); "
    "local f = sl.getFailure(); "
    "return string.format('done=%s;failed=%s;msg=%s', tostring(sl.isDone()), "
    "tostring(sl.isFailed()), f and (tostring(f.message):gsub('%s+', ' ')) or '')")


class MeasurementError(RuntimeError):
    """A sample that must not be reported: the load was unsuccessful or
    incomplete, the baseline was not empty, or a reply was malformed."""


# --------------------------------------------------------------------------
# Pure parsing and aggregation (no engine; covered by test_bootstrap_timing)
# --------------------------------------------------------------------------
def median(values) -> float:
    vals = list(values)
    if not vals:
        raise MeasurementError("median of no values")
    if any(not isinstance(v, (int, float)) or not math.isfinite(v) for v in vals):
        raise MeasurementError(f"median of non-finite or non-numeric values: {vals!r}")
    return float(statistics.median(vals))


def parse_return_count(raw: str) -> int:
    """A loader's reply as a whole non-negative count, or ``MeasurementError``.

    The console prints a Lua number, so ``39`` may arrive as ``39.0``. Anything
    else -- an empty reply, ``ok``, ``nil``, an error text, a fraction, a
    negative or non-finite number -- is a malformed reply, never a zero.
    """
    try:
        value = float(raw)
    except (TypeError, ValueError):
        raise MeasurementError(f"loader reply is not a number: {raw!r}") from None
    if not math.isfinite(value) or value < 0 or value != math.floor(value):
        raise MeasurementError(f"loader reply is not a whole count: {raw!r}")
    return int(value)


AGGREGATE_RE = re.compile(
    r"Startup assets: (\w+) loaded (\d+) from (\d+) file\(s\)\s*$")


def parse_aggregates(log_text: str) -> list[tuple[str, int, int]]:
    """Every loader aggregate line, in log order, as (family, total, files).

    A line that says ``Startup assets:`` but does not parse is malformed and
    refused rather than skipped.
    """
    found = []
    for line in log_text.splitlines():
        if "Startup assets:" not in line:
            continue
        m = AGGREGATE_RE.search(line)
        if not m:
            raise MeasurementError(f"malformed loader aggregate line: {line!r}")
        found.append((m.group(1), int(m.group(2)), int(m.group(3))))
    return found


def aggregate_inventory(parsed, expected_ids=None) -> dict[str, dict[str, int]]:
    """The aggregates as ``{family: {files, returned_total}}``, requiring
    exactly the expected families, each once. Zero totals are legitimate."""
    expected = FAMILY_IDS if expected_ids is None else list(expected_ids)
    seen: dict[str, dict[str, int]] = {}
    for family, total, files in parsed:
        if family in seen:
            raise MeasurementError(f"duplicate loader aggregate for {family!r}")
        seen[family] = {"files": files, "returned_total": total}
    missing = [f for f in expected if f not in seen]
    extra = [f for f in seen if f not in expected]
    if missing or extra:
        raise MeasurementError(
            f"loader aggregate inventory is wrong: missing {missing}, "
            f"unexpected {extra}")
    return {f: seen[f] for f in expected}


STATUS_B_RE = re.compile(r"^done=(true|false);failed=(true|false);msg=(.*)$")


def parse_status_b(raw: str) -> None:
    """Require path B's reply to say ``done=true`` and ``failed=false``."""
    m = STATUS_B_RE.match(raw or "")
    if not m:
        raise MeasurementError(f"path B reply is malformed or missing: {raw!r}")
    done, failed, msg = m.group(1) == "true", m.group(2) == "true", m.group(3)
    if failed:
        raise MeasurementError(f"the startup loader reported a failure: {msg!r}")
    if not done:
        raise MeasurementError("the startup loader did not report done")


REGISTRY_RE = re.compile(r"^(\w+=-?\d+)(;\w+=-?\d+)*$")


def parse_registry_counts(raw: str) -> dict[str, int]:
    if not REGISTRY_RE.match(raw or ""):
        raise MeasurementError(f"registry reply is malformed: {raw!r}")
    counts = {}
    for part in raw.split(";"):
        key, value = part.split("=")
        counts[key] = int(value)
    if set(counts) != set(REGISTRY_QUERIES):
        raise MeasurementError(
            f"registry reply names {sorted(counts)}, expected "
            f"{sorted(REGISTRY_QUERIES)}")
    bad = {k: v for k, v in counts.items() if v < 0}
    if bad:
        raise MeasurementError(
            f"registry queries failed or answered a non-table: {bad}")
    return counts


def parse_load_verbs(raw: str) -> dict[str, dict[str, int]]:
    """``load_verbs|engine.loadItemYaml=39:4123;...`` as
    ``{verb: {count, total_ns}}``."""
    if not raw or not raw.startswith("load_verbs|"):
        raise MeasurementError(f"call-stat reply is malformed: {raw!r}")
    body = raw[len("load_verbs|"):]
    verbs: dict[str, dict[str, int]] = {}
    if not body:
        return verbs
    for part in body.split(";"):
        m = re.fullmatch(r"(engine\.load\w+Yaml)=(\d+):(\d+)", part)
        if not m:
            raise MeasurementError(f"call-stat row is malformed: {part!r}")
        verbs[m.group(1)] = {"count": int(m.group(2)), "total_ns": int(m.group(3))}
    return verbs


def check_baseline(verbs, registry, log_text) -> list[str]:
    """Reasons this engine is NOT an unloaded baseline; empty when it is.

    Three independent witnesses, scoped to the families the two paths load: no
    per-family loader verb has been called (the call window is never reset),
    every queryable registry is empty, and the log holds no loader aggregate
    and no queue line.
    """
    problems = []
    for verb, row in sorted(verbs.items()):
        if row["count"] > 0:
            problems.append(f"{verb} was already called {row['count']} time(s)")
    for name, count in sorted(registry.items()):
        if count != 0:
            problems.append(f"the {name} registry already holds {count} entries")
    if "Startup assets:" in log_text:
        problems.append("the log already holds startup-loader aggregates")
    if "Startup loader queued" in log_text:
        problems.append("the log shows the startup loader already ran")
    return problems


def engine_side_delta(before, after) -> dict:
    """Per-family-loader calls and inclusive engine-side time between two call
    snapshots (after minus before); nested calls overlap, as the stat says."""
    by_verb = {}
    for verb, row in sorted(after.items()):
        base = before.get(verb, {"count": 0, "total_ns": 0})
        count = row["count"] - base["count"]
        ns = row["total_ns"] - base["total_ns"]
        if count < 0 or ns < 0:
            raise MeasurementError(f"call window went backwards for {verb}")
        if count:
            by_verb[verb] = {"calls": count, "seconds": ns / 1e9}
    return {
        "load_calls": sum(v["calls"] for v in by_verb.values()),
        "load_seconds": sum(v["seconds"] for v in by_verb.values()),
        "by_verb": by_verb,
    }


def stat_block(values) -> dict:
    vals = [float(v) for v in values]
    return {"values": vals, "median": median(vals)}


def summarise(samples: list[dict]) -> dict:
    """Aggregate raw samples into the report's ``summary``.

    Requires at least ``MIN_REPEATS`` samples for each path and identical
    per-family inventories across the repeats of a path, so one noisy or
    broken run cannot decide.
    """
    by_path = {"A": [], "B": []}
    for s in samples:
        by_path[s["path"]].append(s)
    for path, rows in by_path.items():
        if len(rows) < MIN_REPEATS:
            raise MeasurementError(
                f"path {path} has {len(rows)} sample(s); at least "
                f"{MIN_REPEATS} fresh engines are required")
        first = rows[0]
        for other in rows[1:]:
            if other["families"] != first["families"]:
                raise MeasurementError(
                    f"path {path} registered different per-family totals "
                    f"across repeats")
            if other["round_trips"] != first["round_trips"]:
                raise MeasurementError(
                    f"path {path} made different numbers of round trips "
                    f"across repeats")
    a, b = by_path["A"], by_path["B"]
    wall_a = stat_block(s["load_wall_s"] for s in a)
    wall_b = stat_block(s["load_wall_s"] for s in b)
    difference = wall_a["median"] - wall_b["median"]
    est = PROBE_ENGINE_COST_ESTIMATE_S
    return {
        "engine_boot_s": {
            "A": stat_block(s["engine_boot_s"] for s in a),
            "B": stat_block(s["engine_boot_s"] for s in b),
            "all": stat_block(s["engine_boot_s"] for s in samples),
        },
        "path_a": {
            "load_wall_s": wall_a,
            "round_trips": a[0]["round_trips"],
            "median_round_trip_s": stat_block(
                s["median_round_trip_s"] for s in a),
            "idle_floor_s": a[0]["round_trips"] * CONSOLE_IDLE_S,
            "engine_side_load_s": stat_block(
                s["engine_side"]["load_seconds"] for s in a),
        },
        "path_b": {
            "load_wall_s": wall_b,
            "round_trips": b[0]["round_trips"],
            "idle_floor_s": b[0]["round_trips"] * CONSOLE_IDLE_S,
            "engine_side_load_s": stat_block(
                s["engine_side"]["load_seconds"] for s in b),
        },
        "difference_s": {
            "definition": "median(A load wall) minus median(B load wall); "
                          "negative means B was slower",
            "value": difference,
        },
        "estimate_against_historical_probe_engine_cost": {
            "denominator_s": est,
            "denominator_source": PROBE_ENGINE_COST_SOURCE,
            "path_a_fraction": wall_a["median"] / est,
            "path_b_fraction": wall_b["median"] / est,
            "signed_saving_fraction": difference / est,
        },
        "scope": "this headless --arena bootstrap comparison only: B also "
                 "loads vegetation, infections, recipes, loot tables, loot "
                 "profiles and locations, and both paths include the "
                 "console's per-round-trip idle wait",
    }


def build_report(samples: list[dict], meta: dict) -> dict:
    """The ``bootstrap-timing/v1`` document."""
    summary = summarise(samples)
    a = next(s for s in samples if s["path"] == "A")
    b = next(s for s in samples if s["path"] == "B")
    families = []
    for fam in FAMILY_IDS:
        families.append({
            "family": fam,
            "returned_count_unit": FAMILY_UNIT[fam],
            "path_a": a["families"].get(fam),
            "path_b": b["families"].get(fam),
        })
    return {
        "schema": SCHEMA,
        **meta,
        "console_idle_s": CONSOLE_IDLE_S,
        "ready_poll_s": READY_POLL_S,
        "summary": summary,
        "families": families,
        "registry_entries_after": {"A": a["registry_after"],
                                   "B": b["registry_after"]},
        "samples": samples,
    }


def render_table(report: dict) -> str:
    s = report["summary"]
    pa, pb = s["path_a"], s["path_b"]

    def vals(block):
        return " ".join(f"{v:.3f}" for v in block["values"])

    est = s["estimate_against_historical_probe_engine_cost"]
    lines = [
        f"{report['schema']}  headless --arena, {len(pa['load_wall_s']['values'])} "
        f"fresh engines per path, console idle {report['console_idle_s']} s "
        f"per round trip",
        f"engine boot (launch -> READY, polled every {report['ready_poll_s']} s): "
        f"A {vals(s['engine_boot_s']['A'])} | B {vals(s['engine_boot_s']['B'])} "
        f"| median of all {s['engine_boot_s']['all']['median']:.3f} s",
        "",
        f"{'':26}{'path A (per-file loop)':>26}{'path B (one call)':>22}",
        f"{'console round trips':26}{pa['round_trips']:>26}{pb['round_trips']:>22}",
        f"{'load wall s (each)':26}{vals(pa['load_wall_s']):>26}{vals(pb['load_wall_s']):>22}",
        f"{'load wall s (median)':26}{pa['load_wall_s']['median']:>26.3f}"
        f"{pb['load_wall_s']['median']:>22.3f}",
        f"{'median s / round trip':26}{pa['median_round_trip_s']['median']:>26.3f}"
        f"{pb['load_wall_s']['median']:>22.3f}",
        f"{'idle floor s (trips*idle)':26}{pa['idle_floor_s']:>26.3f}"
        f"{pb['idle_floor_s']:>22.3f}",
        f"{'engine-side load verbs s':26}{pa['engine_side_load_s']['median']:>26.3f}"
        f"{pb['engine_side_load_s']['median']:>22.3f}",
        "",
        f"median(A) - median(B) = {s['difference_s']['value']:+.3f} s "
        f"(negative would mean B slower)",
        f"against the ~{est['denominator_s']:.0f} s per-engine figure "
        f"(an estimate from PRB-15, not measured here): A is "
        f"{est['path_a_fraction']:.1%}, B is {est['path_b_fraction']:.1%}, "
        f"signed saving {est['signed_saving_fraction']:+.1%}",
        f"scope: {s['scope']}",
        "",
        f"{'family':13}{'returned count counts':28}{'A files':>8}{'A total':>9}"
        f"{'B files':>9}{'B total':>9}",
    ]
    for row in report["families"]:
        pa_row, pb_row = row["path_a"], row["path_b"]
        lines.append(
            f"{row['family']:13}{row['returned_count_unit']:28}"
            f"{(pa_row['files'] if pa_row else '-'):>8}"
            f"{(pa_row['returned_total'] if pa_row else '-'):>9}"
            f"{(pb_row['files'] if pb_row else '-'):>9}"
            f"{(pb_row['returned_total'] if pb_row else '-'):>9}")
    lines += [
        "",
        "registry entries after the path (counts of names; materials count "
        "materials, not textures; faction, vegetation and loot_table have no "
        "list verb):",
        f"{'registry':14}{'A':>6}{'B':>6}",
    ]
    for name in REGISTRY_QUERIES:
        lines.append(f"{name:14}{report['registry_entries_after']['A'][name]:>6}"
                     f"{report['registry_entries_after']['B'][name]:>6}")
    return "\n".join(lines)


# --------------------------------------------------------------------------
# Engine handling
# --------------------------------------------------------------------------
def refuse_gui_port(port: int) -> None:
    if port == probelib.GUI_PORT:
        raise MeasurementError(
            f"refusing port {probelib.GUI_PORT}: it belongs to the GUI instance")


def ready_regex(port: int):
    return re.compile(rf"^READY port={port}\s*$", re.MULTILINE)


def launch(cmd: list[str], log_path: str, port: int, env: dict | None = None,
           ready_timeout: float = READY_TIMEOUT_S,
           poll: float = READY_POLL_S):
    """Start ``cmd`` and return ``(process, seconds_to_READY)``.

    The clock starts immediately before the process is created and stops when
    this process reads that process's own ``READY port=<port>`` line from its
    log, so preparation and staging are never inside it. The caller disposes of
    the returned process; on failure it is killed here.
    """
    marker = ready_regex(port)
    with open(log_path, "w") as log:
        t0 = time.monotonic()
        proc = subprocess.Popen(cmd, stdout=log, stderr=subprocess.STDOUT,
                                stdin=subprocess.DEVNULL, env=env)
    deadline = t0 + ready_timeout
    try:
        while True:
            try:
                text = Path(log_path).read_text(errors="replace")
            except FileNotFoundError:
                text = ""
            if marker.search(text):
                return proc, time.monotonic() - t0
            if proc.poll() is not None:
                raise MeasurementError(
                    f"the engine exited before READY (status {proc.returncode}); "
                    f"see {log_path}")
            if time.monotonic() >= deadline:
                raise MeasurementError(
                    f"the engine never printed READY within {ready_timeout:g} s; "
                    f"see {log_path}")
            time.sleep(poll)
    except BaseException:
        if proc.poll() is None:
            proc.kill()
        proc.wait(timeout=10)
        raise


def engine_env(environ=None) -> dict:
    """The environment for the engine: ``ENGINE_DEBUG`` dropped, because it
    replaces the log categories and adds per-file DEBUG lines that would
    perturb the load being timed."""
    env = dict(os.environ if environ is None else environ)
    env.pop("ENGINE_DEBUG", None)
    return env


def stage_root(parent: Path) -> Path:
    """A resource root of its own: the repository's read-only content
    symlinked in, ``config/`` copied (the engine may write there) and an empty
    ``saves/``. Built before the engine exists, so outside every interval."""
    root = parent / "root"
    root.mkdir(parents=True)
    for family in ("scripts", "assets", "data"):
        (root / family).symlink_to(REPO / family)
    shutil.copytree(REPO / "config", root / "config")
    (root / "saves").mkdir()
    return root


def read_log(path: str) -> str:
    return Path(path).read_text(errors="replace")


def query_state(port: int) -> tuple[dict, dict]:
    verbs = parse_load_verbs(send(port, LOAD_VERBS_LUA))
    registry = parse_registry_counts(send(port, REGISTRY_LUA))
    return verbs, registry


def run_path_a(port: int, root: Path) -> dict:
    """Time ``bootstrap()``'s eight-family loop. The globbing happens inside
    the interval, as it does in the probe."""
    from glob import glob
    trips = []
    t0 = time.monotonic()
    for pattern, verb in PATH_A_LOADERS:
        family = FAMILY_BY_DIR[pattern.split("/*")[0]]
        for path in sorted(glob(pattern, root_dir=root)):
            s = time.monotonic()
            raw = send(port, f"local n = {verb}('{path}'); return n")
            trips.append((family, path, time.monotonic() - s, raw))
    wall = time.monotonic() - t0
    families: dict[str, dict[str, int]] = {}
    for family, path, _, raw in trips:
        count = parse_return_count(raw)
        if count == 0:
            raise MeasurementError(
                f"{path}: the loader returned 0, which is how it reports a "
                f"rejected file")
        row = families.setdefault(family, {"files": 0, "returned_total": 0})
        row["files"] += 1
        row["returned_total"] += count
    if not trips:
        raise MeasurementError("path A found no YAML files to load")
    return {
        "load_wall_s": wall,
        "round_trips": len(trips),
        "round_trip_s": [t[2] for t in trips],
        "median_round_trip_s": median(t[2] for t in trips),
        "families": families,
    }


def run_path_b(port: int, log_path: str) -> dict:
    """Time ``build('arena')`` + ``runAll()`` as one console call."""
    t0 = time.monotonic()
    raw = send(port, PATH_B_LUA, timeout=300.0)
    wall = time.monotonic() - t0
    parse_status_b(raw)
    deadline = time.monotonic() + 10.0
    while True:
        parsed = parse_aggregates(read_log(log_path))
        if len(parsed) >= len(FAMILY_IDS) or time.monotonic() >= deadline:
            break
        time.sleep(0.1)
    return {
        "load_wall_s": wall,
        "round_trips": 1,
        "families": aggregate_inventory(parsed),
        "loader_status": {"done": True, "failed": False},
    }


def require_registered(path_id: str, registry: dict) -> None:
    families = PATH_A_FAMILIES if path_id == "A" else FAMILY_IDS
    empty = [f for f in families if f in registry and registry[f] == 0]
    if empty:
        raise MeasurementError(
            f"path {path_id} left the {empty} registries empty")


def measure_one(path_id: str, index: int, exe: str, port: int,
                workdir: Path) -> dict:
    """One fresh engine, one path."""
    refuse_gui_port(port)
    sample_dir = workdir / f"{path_id}{index}"
    sample_dir.mkdir()
    root = stage_root(sample_dir)
    log_path = str(sample_dir / "engine.log")
    cmd = [exe, "--headless", "--arena", "--port", str(port),
           "--resource-root", str(root)]
    proc, boot_s = launch(cmd, log_path, port, env=engine_env())
    try:
        verbs_before, registry_before = query_state(port)
        problems = check_baseline(verbs_before, registry_before,
                                  read_log(log_path))
        if problems:
            raise MeasurementError(
                "the engine is not an unloaded baseline: " + "; ".join(problems))
        result = run_path_a(port, root) if path_id == "A" \
            else run_path_b(port, log_path)
        verbs_after, registry_after = query_state(port)
        require_registered(path_id, registry_after)
    finally:
        quit_engine(port, proc)
    return {
        "path": path_id,
        "index": index,
        "engine_boot_s": boot_s,
        "baseline": {"load_verb_calls": {v: r["count"]
                                         for v, r in sorted(verbs_before.items())},
                     "registry_entries": registry_before,
                     "log_has_loader_lines": False},
        **result,
        "engine_side": engine_side_delta(verbs_before, verbs_after),
        "registry_after": registry_after,
    }


def describe_environment(exe: str, port: int) -> dict:
    def git(*args):
        try:
            return subprocess.run(["git", "-C", str(REPO), *args], check=True,
                                  capture_output=True, text=True).stdout.strip()
        except (OSError, subprocess.CalledProcessError):
            return None
    st = os.stat(exe)
    return {
        "engine_executable": exe,
        "engine_executable_bytes": st.st_size,
        "engine_executable_mtime": time.strftime(
            "%Y-%m-%dT%H:%M:%S%z", time.localtime(st.st_mtime)),
        "repo_head": git("rev-parse", "HEAD"),
        "repo_dirty": bool(git("status", "--porcelain", "--untracked-files=no")),
        "platform": platform.platform(),
        "python": platform.python_version(),
        "port": port,
    }


def parse_args(argv=None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--output", required=True,
                    help="path of the bootstrap-timing/v1 JSON document")
    ap.add_argument("--port", type=int, default=DEFAULT_PORT,
                    help=f"engine console port (default {DEFAULT_PORT}; "
                         f"{probelib.GUI_PORT} is refused)")
    ap.add_argument("--repeats", type=int, default=MIN_REPEATS,
                    help=f"fresh engines per path (at least {MIN_REPEATS})")
    args = ap.parse_args(argv)
    if args.port == probelib.GUI_PORT:
        ap.error(f"port {probelib.GUI_PORT} belongs to the GUI instance")
    if not 1024 <= args.port <= 65535:
        ap.error("--port must be an unprivileged TCP port")
    if args.repeats < MIN_REPEATS:
        ap.error(f"--repeats must be at least {MIN_REPEATS}")
    return args


def main(argv=None) -> int:
    args = parse_args(argv)
    try:
        exe = probe_engine.prepare_executable(
            announce=lambda note: print(note, file=sys.stderr, flush=True))
    except probe_engine.EngineExecutableError as error:
        print(f"bootstrap_timing: {error}", file=sys.stderr)
        return 1
    workdir = Path(tempfile.mkdtemp(prefix="bootstrap_timing_"))
    samples = []
    try:
        for index in range(1, args.repeats + 1):
            for path_id in ("A", "B"):
                samples.append(measure_one(path_id, index, exe, args.port, workdir))
        report = build_report(samples, describe_environment(exe, args.port))
    except MeasurementError as error:
        print(f"bootstrap_timing: FAILED MEASUREMENT: {error}\n"
              f"evidence kept in {workdir}", file=sys.stderr)
        return 1
    Path(args.output).write_text(json.dumps(report, indent=2) + "\n")
    print(render_table(report))
    shutil.rmtree(workdir, ignore_errors=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
