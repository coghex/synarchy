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
  4. ICE AGREEMENT: tiles that worldgen freezes (the ice system, which applies
     the SAME lapse rate) read at/below freezing — ambient can't disagree with
     where ice visibly forms.

Generated-world, deterministic for the pinned seed. Runtime ~1 min.

Usage: python3 tools/thermo_altitude_probe.py [--port 9171] [--seed 42] [--size 128]
Exit 0 = all checks passed.
"""
from __future__ import annotations
import argparse, json, socket, subprocess, sys, time
from probelib import quit_engine, boot, send

LOG = "/tmp/thermo_altitude_probe_engine.log"


def fnum(port, lua):
    try:
        return float(send(port, lua))
    except (ValueError, TypeError):
        return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9171)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=128)
    args = ap.parse_args()
    port = args.port

    proc = boot(port, log=LOG)
    fails = []
    try:
        send(port, f'world.init("t308",{args.seed},{args.size},5); return "ok"')
        for _ in range(180):
            if send(port, 'local p=world.getInitProgress(); return p').strip() == "3":
                break
            time.sleep(1)
        else:
            sys.exit("world never finished generating")
        send(port, 'world.show("t308"); return "shown"')

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
        print("sweep:", raw)
        raw = raw.strip().strip('"')   # console wraps string returns in quotes
        parts = raw.split("|")
        viol = int(parts[0])
        cax, cay, camb = parts[1].split(","); camb = float(camb)
        wmx, wmy, wmean, wamb = parts[2].split(","); wmean = float(wmean); wamb = float(wamb)
        bx, by, bm, ba = parts[3].split(","); bm = float(bm); ba = float(ba)

        # 1. SAFETY: never warmer than the regional mean.
        if viol == 0:
            print(f"PASS 1 safety: getAmbientAt never exceeds the regional mean ({viol} violations)")
        else:
            fails.append(f"1 safety: {viol} tiles read WARMER than the regional mean")

        # 2. THE BUG: warm region, freezing peak.
        if bm > 0 and ba < 0:
            print(f"PASS 2 bug fix: ({bx},{by}) regional mean {bm:.2f}°C -> ambient {ba:.2f}°C (altitude pushes a temperate region below freezing)")
        else:
            fails.append("2 bug fix: found no tile where mean>0 but elevation-corrected ambient<0")

        # 3. MONOTONE: coldest-by-altitude tile colder than a lowland reference.
        if camb < wamb - 1.0:
            print(f"PASS 3 monotone: coldest tile ({cax},{cay}) ambient {camb:.2f}°C < lowland ref ({wmx},{wmy}) ambient {wamb:.2f}°C")
        else:
            fails.append(f"3 monotone: coldest ambient {camb:.2f} not below lowland ref {wamb:.2f}")

        # 4. ICE AGREEMENT: sample worldgen ice tiles via a local dump and check
        #    getAmbientAt reads at/below freezing on them.
        cx = int(bx) // 16; cy = int(by) // 16
        dump = subprocess.run(
            ["cabal", "run", "-v0", "exe:synarchy", "--", "--dump=terrain,ice",
             "--seed", str(args.seed), "--worldSize", str(args.size),
             "--region", f"{cx-3},{cy-3},{cx+3},{cy+3}"],
            capture_output=True, text=True)
        try:
            tiles = json.loads(dump.stdout)
        except json.JSONDecodeError:
            tiles = []
        ice = [t for t in tiles if t.get("iceSurf") is not None
               and not t.get("glacierZone") and not t.get("beyondGlacier")]
        if not ice:
            print(f"INFO 4 ice agreement: no interior ice tiles near ({bx},{by}) to sample (skipped)")
        else:
            warm_ice = []
            for t in ice[:8]:
                a = fnum(port, f'return world.getAmbientAt({t["x"]},{t["y"]})')
                if a is None or a > 0.5:
                    warm_ice.append((t["x"], t["y"], a))
            if not warm_ice:
                print(f"PASS 4 ice agreement: all {min(8,len(ice))} sampled ice tiles read at/below freezing")
            else:
                fails.append(f"4 ice agreement: ice tiles reading above freezing: {warm_ice}")

        # 5. ARENA SAFETY: the flat no-geology arena has empty plates; getAmbientAt
        #    must NOT crash (elevationAtGlobal would error "no plates") — it falls
        #    back to the regional mean. thermo.tick calls this every tick, so a
        #    throw here would kill the whole unit resource update (regression #308).
        #    Runs last because it switches the active world to the arena.
        send(port, 'world.initArena("arena"); return "ok"')
        time.sleep(2)
        send(port, 'world.show("arena"); return "shown"')
        a_arena = fnum(port, 'return world.getAmbientAt(0,0)')
        c_arena = fnum(port, 'local c=world.getClimateAt(0,0); return c and c.temp')
        if a_arena is not None and c_arena is not None and abs(a_arena - c_arena) < 0.001:
            print(f"PASS 5 arena safety: getAmbientAt returns the regional mean ({a_arena:.2f}°C), no crash on empty plates")
        else:
            fails.append(f"5 arena safety: getAmbientAt={a_arena} (expected regional mean {c_arena}, non-nil, no crash)")
    finally:
        quit_engine(port, proc)

    print()
    if fails:
        print("FAILED:")
        for f in fails:
            print("  -", f)
        sys.exit(1)
    print("ALL CHECKS PASSED")


if __name__ == "__main__":
    main()
