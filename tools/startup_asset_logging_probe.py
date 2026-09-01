#!/usr/bin/env python3
"""Startup asset logging ownership, end to end (#1930).

The contract has two owners and they must not swap:

  * each ``engine.load*Yaml`` binding owns per-file success DETAIL, at
    ``CatAsset`` Debug, carrying the file's full path and the
    authoritative count it returned to Lua -- and emits nothing at Info;
  * ``scripts/startup_loader.lua`` owns exactly ONE Info aggregate per
    registry family, summing what those calls actually returned.

The hspec group ``Startup asset logging`` proves both halves against
doubles and a private engine. This probe is the INTEGRATION half: it
boots the real thing twice and reads the real log, so the two halves are
proved to line up in a session nobody staged.

It has to be ``--offscreen``. A ``--headless`` boot never runs
``scripts/loading_screen.lua`` or ``scripts/ui_manager_boot.lua`` --
``uiManager.checkReady`` gates on ``fontsReady``, which needs a GPU font
atlas -- so the startup asset queue never executes there at all and every
check below would pass vacuously against a log with no aggregates and no
per-file lines in it. Hence ``needs-gpu``, manual-only.

The two profiles differ in how they finish, so both are waited on
through the loader's OWN ``isDone()`` rather than a profile-specific
marker: normal drains across frames under ``loadingScreen.update``,
while arena runs ``startupLoader.runAll()`` synchronously inside
``uiManager.checkReady``.

``ENGINE_DEBUG=Asset`` is what makes the per-file Debug detail reachable.
Note that it REPLACES the default debug-category map rather than adding
to it, and that Debug entries bypass the global minimum level entirely
(``Engine.Core.Log.isEnabled``), so this one token is both necessary and
sufficient. The aggregates need no flag at all: they are ordinary Info
output, and they land under ``CatLua`` rather than ``CatAsset``, because
the loader emits them through ``engine.logInfo``.

Every check locates its lines by the STABLE family identifier or the
binding's own name, never by a category-wide line count -- one check
below exists purely to prove that discrimination, by requiring the
unrelated lifecycle Info lines to be present and uncounted.

Usage: python3 tools/startup_asset_logging_probe.py [--port 9232]
"""
from __future__ import annotations

import argparse
import os
import re
import sys

from probelib import boot, quit_engine, send_json

NORMAL_LOG = "/tmp/startup_asset_logging_normal.log"
ARENA_LOG = "/tmp/startup_asset_logging_arena.log"

#: One scoped family: its stable aggregate identifier, its binding, the
#: data directory every one of its files must live under, and the regex
#: that reads ONE per-file Debug line's authoritative count and path.
#:
#: The count phrase is spelled per family on purpose. It is not the same
#: QUANTITY across the twelve -- materials, vegetation and flora return a
#: TEXTURE total, loot tables 0 or 1 per file, the rest a definition
#: count -- and a single generic pattern would quietly accept a line that
#: had started reporting a different number.
FAMILIES: list[tuple[str, str, str, str]] = [
    ("material", "loadMaterialYaml", "data/materials",
     r"loadMaterialYaml: loaded (\d+) textures from (\S+)"),
    ("vegetation", "loadVegetationYaml", "data/vegetation",
     r"loadVegetationYaml: loaded (\d+) textures from (\S+)"),
    ("flora", "loadFloraYaml", "data/flora",
     r"loadFloraYaml: loaded \d+ species \((\d+) textures\) from (\S+)"),
    ("substance", "loadSubstanceYaml", "data/substances",
     r"loadSubstanceYaml: loaded (\d+) substances from (\S+)"),
    ("infection", "loadInfectionYaml", "data/infections",
     r"loadInfectionYaml: loaded (\d+) infections from (\S+)"),
    ("recipe", "loadRecipeYaml", "data/recipes",
     r"loadRecipeYaml: loaded (\d+) recipes from (\S+)"),
    ("item", "loadItemYaml", "data/items",
     r"loadItemYaml: loaded (\d+) item definitions from (\S+)"),
    ("equipment", "loadEquipmentYaml", "data/equipment",
     r"loadEquipmentYaml: loaded (\d+) equipment classes from (\S+)"),
    ("building", "loadBuildingYaml", "data/buildings",
     r"loadBuildingYaml: loaded (\d+) building definitions from (\S+)"),
    ("unit", "loadUnitYaml", "data/units",
     r"loadUnitYaml: loaded (\d+) unit definitions from (\S+)"),
    ("loot_table", "loadLootTableYaml", "data/loot_tables",
     r"loadLootTableYaml: loaded (\d+) loot tables?(?: '[^']*')? from (\S+)"),
    ("location", "loadLocationYaml", "data/locations",
     r"loadLocationYaml: loaded (\d+) locations from (\S+)"),
]

#: Normal startup's inventory, in queue order; arena's is the same set
#: without flora (scripts/startup_loader.lua's queueArenaProfile).
NORMAL_IDS = [fam[0] for fam in FAMILIES]
ARENA_IDS = [fam[0] for fam in FAMILIES if fam[0] != "flora"]

AGGREGATE_RE = re.compile(
    r"Startup assets: (\w+) loaded (\d+) from (\d+) file\(s\)")

#: Info lines the startup path emits that are NOT aggregates. Their
#: presence is asserted, so "the aggregate matcher counted 12" cannot be
#: satisfied by a log that simply has nothing else in it.
UNRELATED_INFO_MARKERS = (
    "Startup loader queued ",
    "Notification registry loaded",
)


def read_log(path: str) -> list[str]:
    with open(path, encoding="utf-8", errors="replace") as handle:
        return handle.read().splitlines()


def at_level(lines: list[str], level: str) -> list[str]:
    mark = f"[{level}]"
    return [line for line in lines if mark in line]


def wait_for_startup_loader(port: int, timeout: float = 300.0) -> bool:
    """Block until the real startup asset queue has drained.

    The loader's own `isDone()`, because the two profiles finish through
    different code: normal ends under `loadingScreen.update` with its
    "Startup loader complete" line, arena ends inside
    `uiManager.checkReady` with no such line at all.
    """
    import time
    deadline = time.time() + timeout
    while time.time() < deadline:
        done = send_json(
            port,
            "local sl = package.loaded['scripts.startup_loader']; "
            "return (sl ~= nil and sl.isDone() == true)",
            timeout=15)
        if done is True:
            return True
        time.sleep(0.5)
    return False


def capture(port: int, log: str, arena: bool, size: str) -> list[str] | None:
    """Boot one profile with CatAsset Debug on, drain it, return its log."""
    label = "arena" if arena else "normal"
    args = ["--size", size]
    if arena:
        args.append("--arena")
    print(f"\n== {label} profile: offscreen boot on port {port} ==")
    # Inherited by the engine subprocess (probelib.boot does not override
    # the environment). `Asset` is CatAsset's ENGINE_DEBUG spelling --
    # derived from the constructor name, lowercased, matched
    # case-insensitively (Engine.Core.Log.Types.categoryEnvName).
    os.environ["ENGINE_DEBUG"] = "Asset"
    proc = boot(port, log=log, args=args, mode=("--offscreen",),
                ready_timeout=240, label=f"{label} engine")
    try:
        if not wait_for_startup_loader(port):
            print(f"[FAIL] {label}: the startup asset queue never drained; "
                  f"see {log}")
            return None
    finally:
        quit_engine(port, proc)
    return read_log(log)


def check_profile(label: str, lines: list[str],
                  expected_ids: list[str]) -> list[str]:
    """Every check for one profile; returns the failures it found."""
    failures: list[str] = []

    def ok(message: str) -> None:
        print(f"[PASS] {label}: {message}")

    def bad(message: str) -> None:
        print(f"[FAIL] {label}: {message}")
        failures.append(f"{label}: {message}")

    info_lines = at_level(lines, "INFO")
    debug_lines = at_level(lines, "DEBUG")

    # --- 1. the aggregate inventory, located by family identifier -----
    found = [(m.group(1), int(m.group(2)), int(m.group(3)))
             for line in info_lines
             for m in [AGGREGATE_RE.search(line)] if m]
    found_ids = [fam for fam, _, _ in found]
    if found_ids == expected_ids:
        ok(f"emits exactly the {len(expected_ids)} expected aggregates, "
           f"in queue order")
    else:
        bad(f"aggregate inventory is {found_ids}, expected {expected_ids}")

    if label == "arena":
        if any(fam == "flora" for fam, _, _ in found):
            bad("emits a flora aggregate, which queueArenaProfile does "
                "not load")
        elif any("loadFloraYaml" in line for line in lines):
            bad("reached loadFloraYaml at all")
        else:
            ok("emits no flora aggregate and never calls loadFloraYaml")

    totals = {fam: (total, files) for fam, total, files in found}

    # --- 2/3. per-file detail: Info silent, Debug complete ------------
    for fam_id, verb, data_dir, pattern in FAMILIES:
        if fam_id not in expected_ids:
            continue
        stray = [line for line in info_lines if f"{verb}: loaded" in line]
        if stray:
            bad(f"{verb} still emits a per-file success line at Info: "
                f"{stray[0].strip()}")
        else:
            ok(f"{verb} emits no per-file success line at Info")

        per_file = [(int(m.group(1)), m.group(2))
                    for line in debug_lines
                    for m in [re.search(pattern, line)] if m]
        if not per_file:
            bad(f"{verb} logged no per-file Debug detail at all")
            continue
        offsite = [path for _, path in per_file
                   if not path.startswith(data_dir + "/")]
        if offsite:
            bad(f"{verb} logged a path outside {data_dir}/: {offsite[0]}")
        else:
            ok(f"{verb} logged {len(per_file)} per-file Debug line(s), each "
               f"with its full {data_dir}/ path and returned count")

        # --- 4. the aggregate IS that family's own sum ----------------
        if fam_id not in totals:
            continue
        total, files = totals[fam_id]
        observed = sum(count for count, _ in per_file)
        if total != observed:
            bad(f"{fam_id} aggregate reports {total} but its per-file Debug "
                f"counts sum to {observed}")
        elif files != len(per_file):
            bad(f"{fam_id} aggregate reports {files} file(s) but "
                f"{len(per_file)} per-file Debug line(s) were logged")
        else:
            ok(f"{fam_id} aggregate {total} over {files} file(s) equals its "
               f"observed per-file sum")

    # --- 5. unrelated lifecycle output is present and NOT counted -----
    unrelated = [line for line in info_lines
                 if any(mark in line for mark in UNRELATED_INFO_MARKERS)]
    if not unrelated:
        bad("none of the unrelated startup Info lines were emitted, so "
            "'only the aggregates were counted' proves nothing here")
    elif any(AGGREGATE_RE.search(line) for line in unrelated):
        bad("an unrelated lifecycle Info line matched the aggregate "
            "pattern")
    else:
        ok(f"{len(info_lines)} Info lines in all, {len(found)} of them "
           f"aggregates — {len(unrelated)} unrelated lifecycle line(s) "
           f"present and uncounted")

    return failures


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9232)
    ap.add_argument("--size", default="1280x720")
    args = ap.parse_args()

    failures: list[str] = []

    normal = capture(args.port, NORMAL_LOG, arena=False, size=args.size)
    if normal is None:
        return 1
    failures += check_profile("normal", normal, NORMAL_IDS)

    arena = capture(args.port + 1, ARENA_LOG, arena=True, size=args.size)
    if arena is None:
        return 1
    failures += check_profile("arena", arena, ARENA_IDS)

    print()
    if failures:
        print(f"SOME CHECKS FAILED ({len(failures)}):")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
