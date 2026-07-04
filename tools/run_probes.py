#!/usr/bin/env python3
"""Opt-in aggregate runner for the headless behavior probes (#370).

Each `tools/*_probe.py` script is a self-contained regression harness: it
boots its own headless engine (`--headless --port NNNN`), drives a scenario
over the debug-console TCP protocol, asserts on the result, and exits 0/1.
They're normally run one at a time, by hand, whichever is relevant to a
change. This script runs a selection of them back-to-back and prints a
single PASS/FAIL summary.

Probes are run SEQUENTIALLY, one full engine boot+teardown at a time — this
script does not attempt to make probes share a running engine (they're
independent scripts, each owning its own boot/teardown, not built around an
injectable engine handle). A full run is therefore slow: expect low tens of
minutes. This is NOT part of any default test tier (see CLAUDE.md Testing
Tiers) — run it deliberately, and prefer `--only` to scope it to what your
change touches.

Usage:
  python3 tools/run_probes.py                  # run everything
  python3 tools/run_probes.py --only combat,movement
  python3 tools/run_probes.py --list
  python3 tools/run_probes.py --port 9500       # override --port where supported
  python3 tools/run_probes.py --timeout 300

Exit 0 = all selected probes passed. 1 = at least one failed. 2 = bad
invocation (e.g. --only matched nothing).
"""
from __future__ import annotations
import argparse
import os
import signal
import subprocess
import sys
import time

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# (key, script filename, supports --port, one-line purpose for --list)
PROBES = [
    ("cargo_capacity", "cargo_capacity_probe.py", False,
     "depositToCargo weighs the actual ItemInstance, not the def base weight (#189)"),
    ("chop", "chop_probe.py", True,
     "chop-designation layer + chop AI + wood_log yield (#97)"),
    ("collapse_crawl", "collapse_crawl_probe.py", True,
     "collapse<->crawl pose hysteresis in tickInjuries (#304)"),
    ("combat_anim", "combat_anim_probe.py", True,
     "real fight headless; verifies swing/death animations play"),
    ("concussion_revive", "concussion_revive_probe.py", True,
     "checkRevive concussion-band hysteresis (#304)"),
    ("construction", "construction_probe.py", True,
     "construct_job AI end-to-end: claim/source/progress/place/stake/release (#96)"),
    ("craft", "craft_probe.py", True,
     "craft.* API: catalogue, execute, stations, quality, smelting (#325/#326/#343/#327)"),
    ("craft_bill", "craft_bill_probe.py", True,
     "craft-bill backend + craft_job AI: queue/claim/progress, source (ground+cargo) -> work -> produce loop (#329)"),
    ("disarm", "disarm_probe.py", False,
     "disabled-hand auto-drop must re-fire (#193)"),
    ("flora_growth", "flora_growth_probe.py", True,
     "derived flora growth/age/phase under the advancing calendar (#332)"),
    ("follow_command_priority", "follow_command_priority_probe.py", True,
     "follow-command priority against other AI goals (#306)"),
    ("foraging", "foraging_probe.py", True,
     "foraging AI + harvestable-flora gating (#94)"),
    ("infection", "infection_probe.py", True,
     "infection growth/prevention/cure/sepsis loop end-to-end"),
    ("injury_log", "injury_log_probe.py", True,
     "injury-log stream roundtrip: emit/drain, unit.injure, emitEventForUnit"),
    ("item_instance", "item_instance_probe.py", True,
     "per-instance item identity (#67)"),
    ("item_temp", "item_temp_probe.py", True,
     "item temperature model (#344)"),
    ("location_content", "location_content_probe.py", True,
     "location content spawning + ruin probe (#90, #91)"),
    ("location_overlay", "location_overlay_probe.py", True,
     "world-gen location-overlay placement (#89)"),
    ("lua_orphan_prune", "lua_orphan_prune_probe.py", True,
     "Lua per-id AI state pruned (not inherited) after a save load (#195)"),
    ("medic_coord", "medic_coord_probe.py", True,
     "bestMedicFor/medicAvailable distance-discounted selection fix"),
    ("movement", "movement_probe.py", True,
     "obstacle-course movement: pathing/climbs/falls/ramps"),
    ("multiworld_save", "multiworld_save_probe.py", True,
     "multi-world save -> quit -> restart -> load; cross-page survival (#214, #219)"),
    ("physiology", "physiology_probe.py", True,
     "thermoregulation/circulation sanity across controlled environments"),
    ("repair_item", "repair_item_probe.py", True,
     "unit.repairItem primitive (#300)"),
    ("repair", "repair_probe.py", True,
     "repair policy layer, station-gated (#301)"),
    ("role", "role_probe.py", True,
     "derived unit-role hysteresis/demotion/work-XP growth (#265)"),
    ("save_pause", "save_pause_probe.py", True,
     "save/load pause-semantics regression (#42)"),
    ("thermo_altitude", "thermo_altitude_probe.py", True,
     "altitude-lapse thermal effect (#308)"),
]

DEFAULT_TIMEOUT = 600.0


def select(only: str | None) -> list[tuple[str, str, bool, str]]:
    if not only:
        return list(PROBES)
    needles = [n.strip() for n in only.split(",") if n.strip()]
    selected = [p for p in PROBES if any(n in p[0] or n in p[1] for n in needles)]
    return selected


def run_one(script: str, supports_port: bool, port: int | None, timeout: float):
    cmd = ["python3", os.path.join("tools", script)]
    if port is not None and supports_port:
        cmd += ["--port", str(port)]
    start = time.time()
    proc = subprocess.Popen(
        cmd, cwd=REPO_ROOT,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        text=True, start_new_session=True,
    )
    timed_out = False
    try:
        out, _ = proc.communicate(timeout=timeout)
        rc = proc.returncode
    except subprocess.TimeoutExpired:
        timed_out = True
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            out, _ = proc.communicate(timeout=10)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
            except ProcessLookupError:
                pass
            out, _ = proc.communicate()
        rc = -1
    elapsed = time.time() - start
    ok = (rc == 0) and not timed_out
    return ok, timed_out, elapsed, out or ""


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--only", default=None,
                     help="comma-separated substrings matched against probe key/filename")
    ap.add_argument("--list", action="store_true", help="list known probes and exit")
    ap.add_argument("--port", type=int, default=None,
                     help="override --port on probes that support it (default: avoids 8008; "
                          "each probe keeps its own default if unset)")
    ap.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT,
                     help=f"per-probe wall-clock timeout in seconds (default {DEFAULT_TIMEOUT:.0f})")
    ap.add_argument("--tail", type=int, default=25,
                     help="lines of captured output to print for a failing probe")
    args = ap.parse_args()

    if args.port == 8008:
        sys.exit("refusing --port 8008: that's the user's GUI port, see CLAUDE.md")

    chosen = select(args.only)
    if not chosen:
        print(f"--only {args.only!r} matched no probes; see --list", file=sys.stderr)
        return 2

    if args.list:
        for key, script, supports_port, purpose in chosen:
            flag = "" if supports_port else "  (no --port flag)"
            print(f"{key:28s} {script:32s} {purpose}{flag}")
        return 0

    print(f"Running {len(chosen)} probe(s) sequentially "
          f"(timeout {args.timeout:.0f}s each)...\n")

    results = []
    for i, (key, script, supports_port, purpose) in enumerate(chosen, 1):
        print(f"[{i}/{len(chosen)}] {script} ... ", end="", flush=True)
        ok, timed_out, elapsed, out = run_one(script, supports_port, args.port, args.timeout)
        status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
        print(f"{status} ({elapsed:.1f}s)")
        if not ok and args.tail > 0:
            tail_lines = out.splitlines()[-args.tail:]
            for ln in tail_lines:
                print(f"    {ln}")
        results.append((key, script, status, elapsed))

    passed = sum(1 for _, _, status, _ in results if status == "PASS")
    total_time = sum(elapsed for _, _, _, elapsed in results)
    print(f"\n{passed}/{len(results)} passed, total {total_time:.1f}s")
    if passed != len(results):
        print("FAILED:")
        for key, script, status, _ in results:
            if status != "PASS":
                print(f"  {status:8s} {script}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
