#!/usr/bin/env python3
"""Headless river identity + naming probe (#1102, epic #708).

The pure hspec groups ("River naming" / "River identity") cover the
naming machine and the event↔feature pairing against hand-built inputs.
This probe covers what only a real engine can: the actual Lua table
`world.getRivers()` builds, the production save codec, and a query made
after a real load in a FRESH process.

  1. Identity reaches Lua. Every river a named world surfaces carries an
     integer `id`, ids are unique, and calling `world.getRivers()` a
     SECOND time returns the identical id→geometry association — same
     ids, same order, same source/mouth/flow/segment counts. Reading the
     table twice is the point: a per-call id would pass any single read.
  2. The names are in the world's own language. Every river has a
     non-empty `name` and `gloss`, no name repeats, every gloss is two
     words, and at least one HEAD word recurs across rivers — the
     repetition #1102 exists to produce.
  3. A world with NO language leaves every river unnamed. The same
     terrain seed, initialized with a custom name and no language seed,
     still gives every river its id, and the `name`/`gloss` keys are
     ABSENT (nil in Lua) rather than empty strings — so check 2 is a
     real outcome, not a field that is always populated.
  4. Names and ids survive save -> quit -> FRESH PROCESS -> load. Every
     id, name, gloss AND the geometry attached to each id come back
     byte-identical through the real `engine.saveWorld` /
     `engine.loadSave` transaction.
  5. Regeneration reproduces them. A third fresh process re-generating
     the identical seed + language from scratch produces the identical
     ids, names, and glosses — determinism from stable inputs, not from
     anything carried in the save.

Rivers need a world with enough land to grow several; the default
seed/size pair below produces nine, which is what makes check 2's
head-recurrence assertion meaningful (a one-river world could never show
a repeat). A 16-chunk world generates in seconds, so the whole probe
stays cheap.

Usage:
  python3 tools/river_naming_probe.py
  python3 tools/river_naming_probe.py --seed 42 --size 16 --port 9192

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import sys
from probelib import (boot, quit_engine, send, send_json, capture_request_id,
                      wait_save_complete, wait_load_published)

LOG = "/tmp/river_naming_engine.log"

#: The language seed the named world is named from. Deliberately above
#: 2^63-1, so the decimal-string carrier world.init uses is exercised
#: over a range a Lua number could not hold losslessly.
LANG_SEED = "12345678901234567890"

SLOT = "river_naming_probe"

#: The fields a river's geometry is compared on across calls, across a
#: save/load, and across a regeneration. Deliberately NOT just the id
#: set: #1102's contract is that each id stays attached to the SAME
#: river, which an id-set comparison cannot see.
GEOMETRY_LUA = """
local rs = world.getRivers()
if rs == nil then return 'nil' end
local out = {}
for i, r in ipairs(rs) do
  out[i] = { id = r.id, name = r.name, gloss = r.gloss,
             sx = r.source.x, sy = r.source.y,
             mx = r.mouth.x, my = r.mouth.y,
             flow = r.flowRate, segs = r.segmentCount }
end
return out
"""


def rivers(port: int) -> list[dict]:
    """`world.getRivers()` as a list of comparable dicts, in the order
    the engine returned them."""
    data = send_json(port, GEOMETRY_LUA.strip().replace("\n", " "),
                     timeout=30.0)
    if not isinstance(data, (list, dict)):
        return []
    if isinstance(data, dict):
        # A Lua array serializes as an object when it is sparse; it never
        # is here, but ordering by numeric key keeps that safe.
        return [data[k] for k in sorted(data, key=lambda k: int(k))]
    return data


def gen_world(port: int, page: str, seed: int, size: int,
              named: bool) -> None:
    if named:
        init = (f"world.init('{page}', {seed}, {size}, 3, 'Vashenkoro', "
                f"'Ashen Land', '{LANG_SEED}')")
    else:
        # A CUSTOM name: display text with no language provenance
        # (#1092 requirement 2), which is what leaves rivers unnamed.
        init = f"world.init('{page}', {seed}, {size}, 3, \"Player's Own Name\")"
    send(port, f"{init}; return 'ok'")
    send(port, "return world.waitForInit(240)", timeout=250)
    send(port, f"world.show('{page}'); return 'ok'")


def check_identity(rs: list[dict], failures: list[str], where: str) -> None:
    ids = [r.get("id") for r in rs]
    if any(i is None for i in ids):
        failures.append(f"{where}: a river reached Lua with no id: {ids}")
        return
    if len(set(ids)) != len(ids):
        failures.append(f"{where}: river ids are not unique: {ids}")


def check_named(rs: list[dict], failures: list[str], where: str) -> None:
    names = [r.get("name") for r in rs]
    glosses = [r.get("gloss") for r in rs]
    if any(not n for n in names):
        failures.append(f"{where}: expected every river named, got {names}")
        return
    if any(not g for g in glosses):
        failures.append(f"{where}: expected a gloss on every river, "
                        f"got {glosses}")
        return
    if len(set(names)) != len(names):
        failures.append(f"{where}: two rivers share a name: {names}")
    heads = []
    for g in glosses:
        words = str(g).split()
        if len(words) != 2:
            failures.append(f"{where}: expected a two-word gloss, got {g!r}")
            return
        heads.append(words[1])
    if len(set(heads)) == len(heads):
        failures.append(
            f"{where}: no head recurs across {len(heads)} rivers ({heads}) -- "
            f"#1102's whole point is that a head morpheme repeats")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=16)
    ap.add_argument("--port", type=int, default=9192)
    args = ap.parse_args()

    failures: list[str] = []
    port = args.port

    # ---- Phase 1-3: identity, naming, and the no-language fallback ---
    proc = boot(port, LOG)
    try:
        gen_world(port, "named", args.seed, args.size, named=True)
        first = rivers(port)
        if not first:
            return fail_out([
                f"no rivers on seed {args.seed} size {args.size} -- this "
                f"probe needs a world that grows several; pick another "
                f"--seed/--size pair"])
        print(f"phase 1: {len(first)} rivers on the named world")
        check_identity(first, failures, "phase 1")

        second = rivers(port)
        if second != first:
            failures.append(
                "phase 1: a second world.getRivers() returned a DIFFERENT "
                "table -- ids must stay attached to the same geometry "
                f"call over call\n  first:  {json.dumps(first)}\n"
                f"  second: {json.dumps(second)}")

        check_named(first, failures, "phase 2")

        gen_world(port, "unnamed", args.seed, args.size, named=False)
        plain = rivers(port)
        check_identity(plain, failures, "phase 3")
        if [r["id"] for r in plain] != [r["id"] for r in first]:
            failures.append(
                "phase 3: the same terrain seed produced different river "
                "ids with and without a language -- naming must not touch "
                "worldgen")
        if any("name" in r or "gloss" in r for r in plain):
            failures.append(
                "phase 3: a world with NO language provenance must leave "
                "the name/gloss keys ABSENT, got "
                f"{json.dumps([{k: v for k, v in r.items() if k in ('name', 'gloss')} for r in plain])}")

        send(port, "world.show('named'); return 'ok'")
        send(port, f"engine.saveWorld('named', '{SLOT}'); return 'ok'",
             timeout=30.0)
        rid = capture_request_id(port, "return engine.getSaveStatus()",
                                 seconds=15.0)
        if rid is None:
            failures.append("phase 4: engine.saveWorld never reported a "
                            "request id")
        else:
            ok, status = wait_save_complete(port, rid, seconds=120.0)
            if not ok:
                failures.append(f"phase 4: the save did not complete: {status}")
    finally:
        quit_engine(port, proc)

    if failures:
        return fail_out(failures)

    # ---- Phase 4: a FRESH process loads the save --------------------
    proc = boot(port, LOG)
    try:
        send(port, f"engine.loadSave('{SLOT}'); return 'ok'", timeout=30.0)
        if not wait_load_published(port, seconds=240.0):
            return fail_out(["phase 4: the load never reached LoadPublished"])
        active = send(port, "return world.getActiveWorldId()").strip().strip('"')
        send(port, f"world.show('{active}'); return 'ok'")
        loaded = rivers(port)
        if loaded != first:
            failures.append(
                "phase 4: rivers changed across save -> fresh process -> "
                f"load\n  before: {json.dumps(first)}\n"
                f"  after:  {json.dumps(loaded)}")
        else:
            print(f"phase 4: {len(loaded)} rivers reloaded identically "
                  f"(ids, names, glosses, geometry)")
    finally:
        quit_engine(port, proc)

    # ---- Phase 5: a FRESH process regenerates the same world ---------
    proc = boot(port, LOG)
    try:
        gen_world(port, "named", args.seed, args.size, named=True)
        regen = rivers(port)
        if regen != first:
            failures.append(
                "phase 5: regenerating the identical seed + language in a "
                f"fresh process produced different rivers\n"
                f"  original: {json.dumps(first)}\n"
                f"  regen:    {json.dumps(regen)}")
        else:
            print("phase 5: regeneration reproduced every id, name and gloss")
    finally:
        quit_engine(port, proc)

    if failures:
        return fail_out(failures)
    print("river naming probe: PASS")
    return 0


def fail_out(failures: list[str]) -> int:
    print("river naming probe: FAIL")
    for f in failures:
        print(f"  - {f}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
