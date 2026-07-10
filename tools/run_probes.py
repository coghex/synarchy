#!/usr/bin/env python3
"""Opt-in aggregate runner for the headless behavior probes (#370).

Each `tools/*_probe.py` script is a self-contained regression harness: it
boots its own headless engine (`--headless --port NNNN`), drives a scenario
over the debug-console TCP protocol, asserts on the result, and exits 0/1.
They're normally run one at a time, by hand, whichever is relevant to a
change. This script runs a selection of them back-to-back and prints a
single PASS/FAIL summary.

Probes each own their engine (boot + teardown). By default they run one at
a time; `--jobs N` runs up to N CONCURRENTLY, each an independent engine on
a unique port (#531). Probes canNOT share a single engine — 8 neutralise
the global `unit_ai.update`, 37 load defs engine-wide, many reuse the same
world/page names, and 16 restart the engine, so there is no clean per-
scenario isolation on one long-lived engine; running independent engines in
parallel gets the speed without the isolation problem. A full sequential
run is low tens of minutes; `--jobs` cuts wall-time to ~total/N (bounded by
the slowest single probe). This is NOT part of any default test tier (see
CLAUDE.md Testing Tiers) — run it deliberately, and prefer `--only`.

Usage:
  python3 tools/run_probes.py                  # run everything, sequentially
  python3 tools/run_probes.py --jobs 4         # up to 4 probes at once
  python3 tools/run_probes.py --only combat,movement
  python3 tools/run_probes.py --list
  python3 tools/run_probes.py --port 9500       # override --port where supported
  python3 tools/run_probes.py --timeout 300

Exit 0 = all selected probes passed. 1 = at least one failed. 2 = bad
invocation (e.g. --only matched nothing).
"""
from __future__ import annotations
import argparse
import concurrent.futures
import os
import signal
import subprocess
import sys
import time

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Base for the unique per-probe ports handed out in parallel mode (--jobs).
# Chosen above the fixed port of the one probe that doesn't take --port
# (cargo_capacity 9009) and clear of the GUI port 8008, so a concurrent
# batch never double-binds a port.
PARALLEL_PORT_BASE = 9400

# (key, script filename, supports --port, one-line purpose for --list)
PROBES = [
    ("action_outcome", "action_outcome_probe.py", True,
     "F4 action-outcome oracle: recordOutcome/drainActionOutcomes public "
     "contract, destructive drain, till/chop mixed-sweep partial paths (#646)"),
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
    ("config_state", "config_state_probe.py", True,
     "local runtime config (video/keybinds/notifications) vs versioned "
     "_default.yaml templates never dirties git (#638)"),
    ("construction", "construction_probe.py", True,
     "construct_job AI end-to-end: claim/source/progress/place/stake/release (#96)"),
    ("consumable_effects", "consumable_effects_probe.py", True,
     "drink effects scaled by item quality/temperature: hydration/caffeine/mood/warmth (#347)"),
    ("cooking", "cooking_probe.py", True,
     "kitchen workshop + cooking skill/basic_cuisine + basic_food/coffee content (#346)"),
    ("craft", "craft_probe.py", True,
     "craft.* API: catalogue, execute, stations, quality, smelting (#325/#326/#343/#327)"),
    ("craft_bill", "craft_bill_probe.py", True,
     "craft-bill backend + craft_job AI: queue/claim/progress, source (ground+cargo) -> work -> produce loop (#329)"),
    ("disarm", "disarm_probe.py", True,
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
    ("location_stamp_idempotent", "location_stamp_idempotent_probe.py", True,
     "geometry-stamp idempotency survives a cleared anchor floor + save/reload (#424)"),
    ("lua_orphan_prune", "lua_orphan_prune_probe.py", True,
     "Lua per-id AI state pruned (not inherited) after a save load (#195)"),
    ("lua_strict_msg", "lua_strict_msg_probe.py", True,
     "a Haskell exception embedded in a LuaToEngineMsg/LuaMsg field must not escape and crash the engine (#622)"),
    ("machine_shop", "machine_shop_probe.py", True,
     "electric furnace smelt recipe + machine_shop wiring/motor recipes, real content (#591)"),
    ("medic_coord", "medic_coord_probe.py", True,
     "bestMedicFor/medicAvailable distance-discounted selection fix"),
    ("mental_state", "mental_state_probe.py", True,
     "mental-state ladder: stressed hysteresis, break episodes + wander/flee AI, euphoria (#352)"),
    ("movement", "movement_probe.py", True,
     "obstacle-course movement: pathing/climbs/falls/ramps"),
    ("multiworld_save", "multiworld_save_probe.py", True,
     "multi-world save -> quit -> restart -> load; cross-page survival (#214, #219)"),
    ("offscreen", "offscreen_probe.py", True,
     "--offscreen render mode: windowless Vulkan boot, UI flow, screenshots, input injection, parallel instances (#650; needs a GPU)"),
    ("physiology", "physiology_probe.py", True,
     "thermoregulation/circulation sanity across controlled environments"),
    ("power_workshop", "power_workshop_probe.py", True,
     "requires_power workshop consumer: unpowered refusal, wired-uncharged gate, AI stall/resume, day/night balance (#361)"),
    ("preview", "preview_probe.py", True,
     "--preview boot skeleton: CLI dispatch (grouped-no-item/unrecognized exit before boot) + boot profile/target over the debug console (#632)"),
    ("repair_item", "repair_item_probe.py", True,
     "unit.repairItem primitive (#300)"),
    ("repair", "repair_probe.py", True,
     "repair policy layer, station-gated (#301)"),
    ("repair_ai", "repair_ai_probe.py", True,
     "repair AI: claim/fetch/walk/repair/return + role weighting (#302)"),
    ("resource_root", "resource_root_probe.py", True,
     "resource-root launch contract: built exe runs --dump/--headless from outside the repo (#636)"),
    ("role", "role_probe.py", True,
     "derived unit-role hysteresis/demotion/work-XP growth (#265)"),
    ("save_pause", "save_pause_probe.py", True,
     "save/load pause-semantics regression (#42)"),
    ("sleep", "sleep_probe.py", False,
     "Sleeping pose + go_to_sleep AI goal: lie-down/wake chain, sleep_pressure regen, wake conditions (#612)"),
    ("text_encoding", "text_encoding_probe.py", True,
     "TE.decodeUtf8Lenient in the Lua text API: malformed UTF-8 through engine.setText no longer errors, and round-trips through engine.getText (#618)"),
    ("thermo_altitude", "thermo_altitude_probe.py", True,
     "altitude-lapse thermal effect (#308)"),
]

DEFAULT_TIMEOUT = 600.0


def select(only: str | None, exact: bool = False) -> list[tuple[str, str, bool, str]]:
    if not only:
        return list(PROBES)
    needles = [n.strip() for n in only.split(",") if n.strip()]
    if exact:
        # Match probe KEYS exactly. The CI gate (#530) needs this: a
        # substring "craft" would otherwise also pull in "craft_bill".
        wanted = set(needles)
        return [p for p in PROBES if p[0] in wanted]
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


def run_with_retry(script, supports_port, port, timeout, retries, announce=None):
    """Run a probe, re-running SOLO up to `retries` times on failure.

    Returns (status, elapsed, out, attempts). `announce(kind, attempt,
    retries)` is an optional callback for live progress before each retry.
    """
    attempt = 0
    while True:
        ok, timed_out, elapsed, out = run_one(script, supports_port, port, timeout)
        attempt += 1
        if ok or attempt > retries:
            break
        if announce:
            announce("TIMEOUT" if timed_out else "FAIL", attempt, retries)
    status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
    return status, elapsed, out, attempt


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--only", default=None,
                     help="comma-separated substrings matched against probe key/filename")
    ap.add_argument("--exact", action="store_true",
                     help="treat --only as exact probe KEYS, not substrings (the CI gate "
                          "uses this so e.g. 'craft' can't also select 'craft_bill')")
    ap.add_argument("--list", action="store_true", help="list known probes and exit")
    ap.add_argument("--port", type=int, default=None,
                     help="override --port on probes that support it (default: avoids 8008; "
                          "each probe keeps its own default if unset)")
    ap.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT,
                     help=f"per-probe wall-clock timeout in seconds (default {DEFAULT_TIMEOUT:.0f})")
    ap.add_argument("--tail", type=int, default=25,
                     help="lines of captured output to print for a failing probe")
    ap.add_argument("--retries", type=int, default=0,
                     help="on failure, re-run a probe SOLO up to N more times before "
                          "marking it failed — absorbs the sequential-engine contention "
                          "flakes seen in a back-to-back run (#530); a probe that passes "
                          "on any attempt counts as PASS")
    ap.add_argument("--jobs", type=int, default=1, metavar="N",
                     help="run up to N probes CONCURRENTLY, each its own engine on a "
                          "unique port (#531). Cuts wall-time to ~total/N. Default 1 = the "
                          "sequential behavior CI relies on. Since concurrency raises "
                          "contention, --retries re-runs failures SOLO after the parallel "
                          "batch. Cap N to (cores - 1) or so — each probe is a full engine.")
    args = ap.parse_args()

    if args.port == 8008:
        sys.exit("refusing --port 8008: that's the user's GUI port, see CLAUDE.md")

    chosen = select(args.only, exact=args.exact)
    if not chosen:
        print(f"--only {args.only!r} matched no probes; see --list", file=sys.stderr)
        return 2

    if args.list:
        for key, script, supports_port, purpose in chosen:
            flag = "" if supports_port else "  (no --port flag)"
            print(f"{key:28s} {script:32s} {purpose}{flag}")
        return 0

    n = len(chosen)
    results: dict[str, tuple[str, str, float, str]] = {}  # key -> (script, status, elapsed, out)
    wall_start = time.time()

    if args.jobs <= 1:
        # Sequential — the mode CI relies on: live, ordered, inline retry.
        print(f"Running {n} probe(s) sequentially (timeout {args.timeout:.0f}s each)...\n")

        def announce(kind, attempt, retries):
            print(f"{kind}, retrying solo ({attempt}/{retries}) ... ", end="", flush=True)

        for i, (key, script, supports_port, purpose) in enumerate(chosen, 1):
            print(f"[{i}/{n}] {script} ... ", end="", flush=True)
            status, elapsed, out, attempts = run_with_retry(
                script, supports_port, args.port, args.timeout, args.retries, announce)
            note = f"  [passed on retry {attempts}]" if status == "PASS" and attempts > 1 else ""
            print(f"{status} ({elapsed:.1f}s){note}")
            if status != "PASS" and args.tail > 0:
                for ln in out.splitlines()[-args.tail:]:
                    print(f"    {ln}")
            results[key] = (script, status, elapsed, out)
    else:
        # Parallel (#531) — one independent engine per probe, up to --jobs at
        # once, each on a unique port. No isolation issue: separate processes,
        # separate ports, unique save names. Retries run SOLO afterward,
        # since parallel contention is exactly what a retry needs to escape.
        jobs = args.jobs
        print(f"Running {n} probe(s), up to {jobs} concurrently "
              f"(timeout {args.timeout:.0f}s each)...\n")

        def work(idx, probe):
            key, script, supports_port, _ = probe
            ok, timed_out, elapsed, out = run_one(
                script, supports_port, PARALLEL_PORT_BASE + idx, args.timeout)
            status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
            return key, script, status, elapsed, out

        with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as ex:
            futs = [ex.submit(work, i, p) for i, p in enumerate(chosen)]
            for done, fut in enumerate(concurrent.futures.as_completed(futs), 1):
                key, script, status, elapsed, out = fut.result()
                print(f"[{done}/{n}] {script} ... {status} ({elapsed:.1f}s)")
                results[key] = (script, status, elapsed, out)

        failed = [p for p in chosen if results[p[0]][1] != "PASS"]
        if failed and args.retries > 0:
            # The parallel batch was already the FIRST attempt, so a probe
            # gets exactly `--retries` more solo attempts here — total
            # attempts (1 + retries) match the sequential path, no bonus try.
            print(f"\nRe-running {len(failed)} failed probe(s) SOLO "
                  f"(up to {args.retries} more attempt(s) each; the parallel "
                  f"batch was the first)...")
            for key, script, supports_port, _ in failed:
                for r in range(1, args.retries + 1):
                    ok, timed_out, elapsed, out = run_one(
                        script, supports_port, PARALLEL_PORT_BASE, args.timeout)
                    status = "TIMEOUT" if timed_out else ("PASS" if ok else "FAIL")
                    print(f"  {script} solo retry {r}/{args.retries} ... "
                          f"{status} ({elapsed:.1f}s)")
                    results[key] = (script, status, elapsed, out)
                    if ok:
                        break

        if args.tail > 0:
            for key, script, _, _ in chosen:
                r = results[key]
                if r[1] != "PASS":
                    print(f"\n--- {r[0]} ({r[1]}) ---")
                    for ln in r[3].splitlines()[-args.tail:]:
                        print(f"    {ln}")

    wall = time.time() - wall_start
    ordered = [(key, results[key][0], results[key][1], results[key][2]) for key, *_ in chosen]
    passed = sum(1 for _, _, status, _ in ordered if status == "PASS")
    probe_time = sum(elapsed for _, _, _, elapsed in ordered)
    extra = f" (wall {wall:.1f}s, {probe_time / wall:.1f}x)" if args.jobs > 1 and wall > 0 else ""
    print(f"\n{passed}/{n} passed, total probe-time {probe_time:.1f}s{extra}")
    if passed != n:
        print("FAILED:")
        for key, script, status, _ in ordered:
            if status != "PASS":
                print(f"  {status:8s} {script}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
