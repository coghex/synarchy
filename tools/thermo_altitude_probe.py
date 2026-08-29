#!/usr/bin/env python3
"""Thermo altitude-lapse probe (issue #308).

Verifies that the unit thermo sim's ambient temperature is ELEVATION-CORRECTED:
high ground is colder than the regional climate mean, matching where worldgen
forms ice. Before the fix, `scripts/thermo.lua` sampled `world.getClimateAt().temp`
(the regional mean, no altitude term) so a unit on an ice-capped peak felt the
same warmth as the valley floor.

This drives the engine's `world.getAmbientAt(gx,gy)` (the centralized
elevation-corrected ambient used by thermo.lua) on a real generated world and
asserts:

  1. SAFETY: getAmbientAt is never WARMER than the regional mean anywhere
     (the lapse rate only cools).
  2. THE BUG: there is high ground where the regional mean is ABOVE freezing
     but the elevation-corrected ambient is BELOW freezing — i.e. a unit there
     now gets cold where before it stayed temperate.
  3. MONOTONE: the coldest-by-altitude tile reads strictly colder than a
     lowland tile in the same area.
  4. ARENA SAFETY: the flat no-geology arena has empty plates; getAmbientAt
     must NOT crash (elevationAtGlobal would error "no plates") — it falls
     back to the regional mean. thermo.tick calls this every tick, so a throw
     here would kill the whole unit resource update (regression #308).
  5. ICE AGREEMENT: tiles that worldgen freezes (the ice system, which applies
     the SAME lapse rate) read at/below freezing — ambient can't disagree with
     where ice visibly forms.

Ice agreement is declared and emitted LAST because it is the one check allowed
to end up MISSING (`rep.skip`), and `probe-result/v1` rejects a check that
arrives after a missing predecessor. Arena safety therefore runs before it, and
the arena is hidden again afterwards to hand the generated page back.

Ice agreement spans this probe's TWO engine launches — the ice coordinates come
from a `--dump` process and the ambient readings from the debug-console world —
so both launches take seed, world size and plate count from the same place
(`PLATE_COUNT` below) and the run reports all three up front. Sampling one
world's ice positions in a differently generated world is the bug #1757 fixed.

This probe implements the shared `probe-result/v1` protocol (#1425/#1474):
`--describe` prints its ordered, stable check declaration without booting
anything, and when a harness supplies an event-stream path it reports through
structured events instead of the bracketed stdout markers below. Run by hand
it still prints one human-readable line per check and exits 0/1.

Generated-world, deterministic for the pinned seed. Runtime ~1 min.

Usage: python3 tools/thermo_altitude_probe.py [--port 9171] [--seed 42] [--size 128]
       python3 tools/thermo_altitude_probe.py --describe   # no engine
Exit 0 = all checks passed.
"""
from __future__ import annotations
import argparse, json, subprocess, sys, time

import probe_engine
import probe_protocol
from probelib import quit_engine, boot, send

SPROOT = "/tmp"

PROBE_KEY = "thermo_altitude"

# This probe launches TWO engines, and each needs its own log: the
# harness hands out one directory per run, so two launches sharing one
# name would overwrite each other's capture.
CONSOLE_LOG_NAME = "thermo_altitude_probe_engine.log"
DUMP_LOG_NAME = "thermo_altitude_probe_dump.log"
CONSOLE_LOG = f"{SPROOT}/{CONSOLE_LOG_NAME}"
DUMP_LOG = f"{SPROOT}/{DUMP_LOG_NAME}"

WORLD_PAGE = "t308"
ARENA_PAGE = "arena"

# The tectonic plate count BOTH engine launches generate with, and the
# probe's ONE source for it. `ice_agreement` samples the console
# engine's `world.getAmbientAt` at coordinates taken from the separate
# `--dump` engine's ice payload, and `World.Weather.Ambient`'s whole
# altitude correction is plate-derived (`elevationAtGlobal seed plates
# worldSize`), so the two engines must agree on this or the check
# compares two different worlds. It used to be a literal in the console
# path only: with `--plates` absent the dump fell back to the engine's
# `defaultPlatesFor worldSize` (9 at the default --size 128) while the
# console asked for 5, and the same seed's first five plates being a
# prefix of its nine is exactly why that passed on partial coincidence
# instead of failing loudly (#1757).
PLATE_COUNT = 5

# Ice tiles are sampled a handful at a time; the whole dumped region
# would be a few thousand console round trips for no extra signal.
ICE_SAMPLE_LIMIT = 8
# An "ice tile" that reads warmer than this disagrees with worldgen.
ICE_WARM_C = 0.5
# The monotone check wants a real gap, not float noise.
MONOTONE_MARGIN_C = 1.0
# The arena falls back to the regional mean exactly, so this is a
# float-equality tolerance rather than a physical one.
ARENA_TOLERANCE_C = 0.001

# The ordered `probe-result/v1` check sequence. Identifiers are STABLE
# and carry no runtime value: the ordinals the old labels led with
# (`PASS 1 safety`, `PASS 4 ice agreement`) live in declaration order
# and nowhere else, and every observed temperature and coordinate that
# used to be interpolated into a label now rides in the event's
# `detail` instead.
CHECKS = [
    ("safety", "getAmbientAt is never warmer than the regional mean"),
    ("bug_fix", "a temperate region's high ground reads below freezing"),
    ("monotone",
     "the coldest-by-altitude tile reads colder than a lowland reference"),
    ("arena_safety",
     "getAmbientAt on the flat arena returns the regional mean without crashing"),
    ("ice_agreement", "worldgen ice tiles read at or below freezing"),
]

DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


class DumpFailure(Exception):
    """The ice dump — this probe's SECOND engine launch — did not produce a
    usable payload.

    Distinct from "this region has no interior ice" on purpose: only the
    latter may reach `rep.skip`, because a failed engine launch reported
    as MISSING would look exactly like a legitimately unsampleable
    region.
    """


def fnum(port, lua):
    try:
        return float(send(port, lua))
    except (ValueError, TypeError):
        return None


def boot_console(port, rep):
    """This probe's FIRST engine launch: the debug-console engine.

    Both launches go through the reporter, so an RTS cohort is honest.
    """
    return boot(port, rep.engine_log_path(CONSOLE_LOG_NAME, CONSOLE_LOG),
                args=rep.engine_args())


def dump_command(seed, size, cx, cy, engine_args):
    """Argv for the SECOND engine launch: the terrain+ice region dump.

    `engine_args` is `rep.engine_args()`. A `+RTS ... -RTS` block is
    consumed by the GHC RTS before `getArgs`, so appending it after the
    engine's own flags is safe.

    The launcher itself is `probe_engine.engine_command` (#1570): the
    aggregate runner's already-resolved executable when there is one, and
    otherwise the same `cabal run` this always used. The engine's own
    arguments, and their order, are identical either way.

    `--plates` is passed EXPLICITLY, from the same `PLATE_COUNT` the
    console launch initializes with, so this engine cannot silently
    resolve a different plate count through `defaultPlatesFor` (#1757).
    `--plates` is the canonical spelling; `--ages` is only a legacy
    alias.
    """
    return probe_engine.engine_command(
        ["--dump=terrain,ice",
         "--seed", str(seed), "--worldSize", str(size),
         "--plates", str(PLATE_COUNT),
         "--region", f"{cx - 3},{cy - 3},{cx + 3},{cy + 3}",
         *engine_args])


def run_ice_dump(rep, seed, size, cx, cy):
    """Run the ice dump and return `(tiles, log_path)`.

    stdout stays a pipe because the JSON payload is the point; stderr
    goes to its own reporter-selected engine log rather than the shared
    `/tmp` constant. A nonzero exit, undecodable stdout, or a payload
    that is not a list of tile objects raises `DumpFailure` — never an
    empty tile list, which the caller would be entitled to read as a
    legitimate MISSING.
    """
    cmd = dump_command(seed, size, cx, cy, rep.engine_args())
    log = rep.engine_log_path(DUMP_LOG_NAME, DUMP_LOG)
    with open(log, "w", encoding="utf-8") as errf:
        done = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=errf,
                              text=True)
    if done.returncode != 0:
        raise DumpFailure(
            f"the ice dump engine exited {done.returncode}; see {log}")
    try:
        tiles = json.loads(done.stdout)
    except (TypeError, ValueError) as error:
        raise DumpFailure(
            f"the ice dump engine printed no valid JSON ({error}); "
            f"see {log}") from None
    if not isinstance(tiles, list) or not all(isinstance(t, dict) for t in tiles):
        raise DumpFailure(
            f"the ice dump payload is not a list of tiles "
            f"(got {type(tiles).__name__}); see {log}")
    return tiles, log


def interior_ice(tiles):
    """Dumped tiles worldgen froze, excluding the polar glacier bands."""
    return [t for t in tiles if t.get("iceSurf") is not None
            and not t.get("glacierZone") and not t.get("beyondGlacier")]


def report_ice_agreement(rep, tiles, region, sample):
    """Report the final `ice_agreement` check from a DECODED dump payload.

    `sample(gx, gy)` returns the engine's ambient there, or None. A
    dump that failed never reaches here (see `run_ice_dump`), so an
    empty interior-ice set is a genuine "nothing to sample" and is
    reported through `rep.skip`: the check stays MISSING rather than
    passing vacuously. Returns True when the check did not fail.
    """
    ice = interior_ice(tiles)
    if not ice:
        rep.skip("no interior ice tiles in the dumped region to sample",
                 {"region": list(region), "tiles": len(tiles),
                  "interior_ice": 0})
        return True
    warm_ice = []
    for t in ice[:ICE_SAMPLE_LIMIT]:
        ambient = sample(t["x"], t["y"])
        if ambient is None or ambient > ICE_WARM_C:
            warm_ice.append([t["x"], t["y"], ambient])
    sampled = min(ICE_SAMPLE_LIMIT, len(ice))
    return rep.check(
        "ice_agreement", not warm_ice,
        (f"all {sampled} sampled ice tiles read at/below freezing"
         if not warm_ice else
         f"ice tiles reading above freezing: {warm_ice}"),
        {"region": list(region), "interior_ice": len(ice),
         "sampled": sampled, "warm_ice": warm_ice,
         "threshold_c": ICE_WARM_C})


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9171)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=128)
    ap.add_argument("--describe", action="store_true",
                    help="print this probe's probe-result/v1 check "
                         "declaration and exit; boots no engine (#1425)")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args, args.port, rep)
    finally:
        rep.close()


def _run(args, port, rep):
    passed = True
    # Reported BEFORE anything can fail, and as a diagnostic of its own
    # rather than a field of a check's detail: standalone mode prints a
    # diagnostic's human text and discards its detail dict, and
    # `ice_agreement` — the check these parameters matter most to — is
    # the one allowed to end up MISSING. Both spellings therefore carry
    # all three values, so whichever channel a reader has, the world
    # that was measured is on it (#1757).
    rep.info(
        f"world generation parameters: seed {args.seed}, "
        f"world size {args.size}, plates {PLATE_COUNT} "
        f"(both engine launches)",
        {"seed": args.seed, "world_size": args.size, "plates": PLATE_COUNT})
    proc = boot_console(port, rep)
    try:
        send(port, f'world.init("{WORLD_PAGE}",{args.seed},{args.size},'
                   f'{PLATE_COUNT}); '
                   f'return "ok"')
        for _ in range(180):
            if send(port, 'local p=world.getInitProgress(); return p').strip() == "3":
                break
            time.sleep(1)
        else:
            rep.abort("world never finished generating")
            return 1
        send(port, f'world.show("{WORLD_PAGE}"); return "shown"')

        half = args.size * 16 // 2  # tile half-extent (chunkSize 16)
        lo, hi, step = -half + 40, half - 40, 40

        # One in-engine sweep: track never-warmer violations, the coldest tile,
        # the warmest-mean tile, and the best "warm region / freezing peak" hit.
        # The debug console is SINGLE-LINE only, so this is one statement-stream.
        lua = (
            "local viol=0;"
            "local cax,cay,camb=0,0,1e9;"
            "local wmx,wmy,wmean,wamb=0,0,-1e9,0;"
            "local bx,by,bm,ba,bc=0,0,0,0,-1;"
            f"for gx={lo},{hi},{step} do for gy={lo},{hi},{step} do "
            "local c=world.getClimateAt(gx,gy); local a=world.getAmbientAt(gx,gy);"
            "if c and a then "
            "if a > c.temp + 0.01 then viol=viol+1 end "
            "if a < camb then camb=a; cax=gx; cay=gy end "
            "if c.temp > wmean then wmean=c.temp; wmx=gx; wmy=gy; wamb=a end "
            "if c.temp > 0 and a < 0 then local gap=c.temp-a; if gap>bc then bc=gap; bx=gx; by=gy; bm=c.temp; ba=a end end "
            "end end end;"
            "return string.format('%d|%d,%d,%.2f|%d,%d,%.2f,%.2f|%d,%d,%.2f,%.2f',"
            "viol, cax,cay,camb, wmx,wmy,wmean,wamb, bx,by,bm,ba)"
        )
        raw = send(port, lua, idle=20.0, timeout=180)
        rep.info("altitude sweep completed", {"sweep": raw.strip(),
                                              "lo": lo, "hi": hi, "step": step})
        raw = raw.strip().strip('"')   # console wraps string returns in quotes
        try:
            parts = raw.split("|")
            viol = int(parts[0])
            cax, cay, camb = parts[1].split(",")
            cax, cay, camb = int(cax), int(cay), float(camb)
            wmx, wmy, wmean, wamb = parts[2].split(",")
            wmx, wmy = int(wmx), int(wmy)
            wmean, wamb = float(wmean), float(wamb)
            bx, by, bm, ba = parts[3].split(",")
            bx, by = int(bx), int(by)
            bm, ba = float(bm), float(ba)
        except (IndexError, ValueError):
            rep.abort("the altitude sweep returned no parseable result",
                      {"sweep": raw})
            return 1

        # 1. SAFETY: never warmer than the regional mean.
        passed &= rep.check(
            "safety", viol == 0,
            (f"getAmbientAt never exceeds the regional mean ({viol} violations)"
             if viol == 0 else
             f"{viol} tiles read WARMER than the regional mean"),
            {"violations": viol})

        # 2. THE BUG: warm region, freezing peak.
        passed &= rep.check(
            "bug_fix", bm > 0 and ba < 0,
            (f"({bx},{by}) regional mean {bm:.2f}°C -> ambient {ba:.2f}°C "
             f"(altitude pushes a temperate region below freezing)"
             if bm > 0 and ba < 0 else
             "found no tile where mean>0 but elevation-corrected ambient<0"),
            {"gx": bx, "gy": by, "regional_mean_c": bm, "ambient_c": ba})

        # 3. MONOTONE: coldest-by-altitude tile colder than a lowland reference.
        passed &= rep.check(
            "monotone", camb < wamb - MONOTONE_MARGIN_C,
            (f"coldest tile ({cax},{cay}) ambient {camb:.2f}°C < lowland ref "
             f"({wmx},{wmy}) ambient {wamb:.2f}°C"
             if camb < wamb - MONOTONE_MARGIN_C else
             f"coldest ambient {camb:.2f} not below lowland ref {wamb:.2f}"),
            {"coldest_gx": cax, "coldest_gy": cay, "coldest_ambient_c": camb,
             "lowland_gx": wmx, "lowland_gy": wmy, "lowland_ambient_c": wamb,
             "lowland_mean_c": wmean, "margin_c": MONOTONE_MARGIN_C})

        # 4. ARENA SAFETY: the flat no-geology arena has empty plates;
        #    getAmbientAt must NOT crash (elevationAtGlobal would error "no
        #    plates") — it falls back to the regional mean. thermo.tick calls
        #    this every tick, so a throw here would kill the whole unit
        #    resource update (regression #308). It switches the active page,
        #    so the generated page is restored below before the ice sampling.
        send(port, f'world.initArena("{ARENA_PAGE}"); return "ok"')
        time.sleep(2)
        send(port, f'world.show("{ARENA_PAGE}"); return "shown"')
        a_arena = fnum(port, 'return world.getAmbientAt(0,0)')
        c_arena = fnum(port, 'local c=world.getClimateAt(0,0); return c and c.temp')
        arena_ok = (a_arena is not None and c_arena is not None
                    and abs(a_arena - c_arena) < ARENA_TOLERANCE_C)
        passed &= rep.check(
            "arena_safety", arena_ok,
            (f"getAmbientAt returns the regional mean ({a_arena:.2f}°C), "
             f"no crash on empty plates" if arena_ok else
             f"getAmbientAt={a_arena} (expected regional mean {c_arena}, "
             f"non-nil, no crash)"),
            {"arena_ambient_c": a_arena, "arena_regional_mean_c": c_arena,
             "tolerance_c": ARENA_TOLERANCE_C})

        # 5. ICE AGREEMENT: sample worldgen ice tiles via a local dump and
        #    check getAmbientAt reads at/below freezing on them. Declared and
        #    emitted last: it is the one check that may end up MISSING.
        # `world.show` on an ALREADY-visible page is a no-op (it does not
        # re-head the visible list, and the active page is that list's
        # HEAD), so the arena is HIDDEN to hand `t308` back rather than
        # re-shown. The world thread applies it off the queue, hence the
        # poll.
        send(port, f'world.hide("{ARENA_PAGE}"); return "hidden"')
        active = ""
        for _ in range(20):
            active = send(port,
                          'return world.getActiveWorldId()').strip().strip('"')
            if active == WORLD_PAGE:
                break
            time.sleep(0.5)
        if active != WORLD_PAGE:
            rep.abort(f"the generated page could not be re-activated after the "
                      f"arena check (active page {active!r}, expected "
                      f"{WORLD_PAGE!r})",
                      {"active": active, "expected": WORLD_PAGE})
            return 1
        cx, cy = bx // 16, by // 16
        try:
            tiles, dump_log = run_ice_dump(rep, args.seed, args.size, cx, cy)
        except (DumpFailure, OSError) as error:
            rep.abort(f"ice dump failed: {error}")
            return 1
        rep.info("ice dump completed", {"tiles": len(tiles), "log": dump_log})
        passed &= report_ice_agreement(
            rep, tiles, (cx - 3, cy - 3, cx + 3, cy + 3),
            lambda gx, gy: fnum(port, f'return world.getAmbientAt({gx},{gy})'))

        rep.note("\n" + ("ALL CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
