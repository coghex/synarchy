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

Every page this probe generates is set up through `wait_page_setup`
(#2653): `world.waitForInit` polls only the ACTIVE world, so on the
second page it would be satisfied by the already-finished first one and
the river queries could answer from the wrong page. The wait is tied to
the requested page itself, and a stall is an apparatus failure that
names the page and the boundary, not a river-check failure.

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
import probe_protocol
from probelib import (boot, quit_engine, send, send_json, capture_request_id,
                      poll_until, wait_save_complete, wait_load_published)

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


def _river_list(data) -> list[dict]:
    """A `world.getRivers()` payload as a list of comparable dicts."""
    if not isinstance(data, (list, dict)):
        return []
    if isinstance(data, dict):
        # A Lua array serializes as an object when it is sparse; it never
        # is here, but ordering by numeric key keeps that safe.
        return [data[k] for k in sorted(data, key=lambda k: int(k))]
    return data


def rivers(port: int) -> list[dict]:
    """`world.getRivers()` as a list of comparable dicts, in the order
    the engine returned them."""
    data = send_json(port, GEOMETRY_LUA.strip().replace("\n", " "),
                     timeout=30.0)
    return _river_list(data)


#: The same rows, with the active page id read in the same console
#: chunk, so the id names the page `world.getRivers()` answered from.
OBSERVED_LUA = """
local active = world.getActiveWorldId()
local rs = world.getRivers()
local out = {}
if rs ~= nil then
  for i, r in ipairs(rs) do
    out[i] = { id = r.id, name = r.name, gloss = r.gloss,
               sx = r.source.x, sy = r.source.y,
               mx = r.mouth.x, my = r.mouth.y,
               flow = r.flowRate, segs = r.segmentCount }
  end
end
return { active = active, rivers = out }
"""


def observe_rivers(port: int) -> tuple[str | None, list[dict]]:
    """`(active page id, rivers)` from ONE console chunk (#2653)."""
    data = send_json(port, OBSERVED_LUA.strip().replace("\n", " "),
                     timeout=30.0)
    if not isinstance(data, dict):
        return None, []
    active = data.get("active")
    return (active if isinstance(active, str) else None,
            _river_list(data.get("rivers")))


#: Setup budgets, in seconds. A page is REGISTERED as soon as the world
#: thread runs its init command; ACTIVE once the `show` queued behind its
#: generation lands; DONE once the load phase reaches `LoadDone`. The
#: last two are the generation budget, which `waitForInit(240)` used to
#: be.
PAGE_READY_SECS = 30.0
PAGE_ACTIVE_SECS = 240.0
PAGE_DONE_SECS = 240.0

#: `world.getInitProgress()`'s first value once the ACTIVE page is done.
LOAD_DONE_PHASE = 3


class SetupStalled(Exception):
    """A page-setup wait ran out: an apparatus failure, not a result.

    `boundary` is the one that stalled -- `registered`, `active` or
    `done` -- so the report says whether the world thread never ran the
    init, the generation queued ahead of the show never finished, or the
    page became active but never reached `LoadDone`."""

    def __init__(self, page: str, boundary: str, message: str) -> None:
        super().__init__(message)
        self.page = page
        self.boundary = boundary


def page_registered(port: int, page: str) -> bool:
    """True once `page` is registered in the world manager.

    `world.getDate(pageId)` returns nil until then, which makes it the
    one per-page observable that exists BEFORE the page is active (the
    same one tools/multiworld_save_probe.py uses, #1447)."""
    return send(port, f"return world.getDate('{page}')").strip() not in (
        "nil", "null", "")


def active_page(port: int) -> str:
    return send(port, "return world.getActiveWorldId()").strip().strip('"')


def load_phase(port: int) -> int | None:
    """The ACTIVE page's load phase (0 idle .. 3 done), or None if the
    console did not answer with one."""
    raw = send(port, "local p = world.getInitProgress(); return p").strip()
    try:
        return int(raw)
    except ValueError:
        return None


def wait_page_setup(port: int, page: str,
                    ready_secs: float = PAGE_READY_SECS,
                    active_secs: float = PAGE_ACTIVE_SECS,
                    done_secs: float = PAGE_DONE_SECS) -> None:
    """Block until the REQUESTED page is registered, active and done.

    Every wait is tied to `page` itself. `world.waitForInit` is not: it
    polls the ACTIVE world's load phase, so an already-finished other
    page satisfies it at once -- and it also returns when its own
    timeout expires, so its return proves nothing. The load phase is
    therefore read directly, and only after `page` is the active world,
    which is what makes `LoadDone` a statement about `page`.

    Raises `SetupStalled` naming the page and the boundary."""
    if not poll_until(ready_secs, lambda: page_registered(port, page)):
        raise SetupStalled(
            page, "registered",
            f"setup for page '{page}' stalled at REGISTERED: "
            f"world.getDate('{page}') stayed nil within {ready_secs:.0f}s "
            f"of world.init -- the world thread never ran the init "
            f"command for this page")
    if not poll_until(active_secs, lambda: active_page(port) == page):
        raise SetupStalled(
            page, "active",
            f"setup for page '{page}' stalled at ACTIVE: it registered "
            f"but never became the active world within {active_secs:.0f}s "
            f"of world.show (active is {active_page(port)!r}) -- the "
            f"generation queued ahead of that show did not finish")
    if not poll_until(done_secs,
                      lambda: load_phase(port) == LOAD_DONE_PHASE):
        raise SetupStalled(
            page, "done",
            f"setup for page '{page}' stalled at DONE: it is the active "
            f"world but its load phase is {load_phase(port)!r}, not "
            f"{LOAD_DONE_PHASE} (LoadDone), after {done_secs:.0f}s")


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
    send(port, f"world.show('{page}'); return 'ok'")
    wait_page_setup(port, page)


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


PROBE_CHECKS = [
    ('rivers_present', 'generated world has rivers to inspect'),
    ('named_identity', 'named river ids are present and unique'),
    ('stable_geometry', 'repeated queries preserve river identity and geometry'),
    ('river_names', 'river names and glosses have the expected structure and recurring heads'),
    ('unnamed_identity', 'unnamed river ids are present and unique'),
    ('unnamed_same_ids', 'language provenance does not change river ids'),
    ('unnamed_absent_names', 'unnamed rivers omit name and gloss fields'),
    ('save_request', 'saving reports a request id'),
    ('save_completed', 'saving reaches its terminal completion'),
    ('load_published', 'fresh-process load reaches reconciled publication'),
    ('restored_rivers', 'fresh-process load restores identical rivers'),
    ('regenerated_rivers', 'fresh-process regeneration reproduces identical rivers'),
]
DESCRIPTOR = probe_protocol.build_descriptor('river_naming', PROBE_CHECKS)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=16)
    ap.add_argument("--port", type=int, default=9192)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 descriptor without booting")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args, rep)
    except SetupStalled as exc:
        # An apparatus failure: the checks it prevented stay MISSING
        # (Reporter.abort emits no check event), and `_run`'s finally
        # blocks have already quit the engine.
        rep.abort(str(exc), {"page": exc.page, "boundary": exc.boundary})
        return 1
    except Exception as exc:
        rep.abort(str(exc))
        raise
    finally:
        rep.close()


def _run(args, rep):
    failures: list[str] = []
    port = args.port

    # ---- Phase 1-3: identity, naming, and the no-language fallback ---
    proc = boot(port, rep.engine_log_path("river_generate.log", LOG),
                args=rep.engine_args())
    try:
        gen_world(port, "named", args.seed, args.size, named=True)
        first = rivers(port)
        if not rep.check('rivers_present', bool(first), DESCRIPTOR.label('rivers_present')):
            return fail_out([
                f"no rivers on seed {args.seed} size {args.size} -- this "
                f"probe needs a world that grows several; pick another "
                f"--seed/--size pair"])
        rep.note(f'phase 1: {len(first)} rivers on the named world')
        before_errors = len(failures)
        check_identity(first, failures, "phase 1")
        rep.check('named_identity', len(failures) == before_errors, DESCRIPTOR.label('named_identity'),
                  {"failures": failures[before_errors:]})

        second = rivers(port)
        if not rep.check('stable_geometry', second == first, DESCRIPTOR.label('stable_geometry')):
            failures.append(
                "phase 1: a second world.getRivers() returned a DIFFERENT "
                "table -- ids must stay attached to the same geometry "
                f"call over call\n  first:  {json.dumps(first)}\n"
                f"  second: {json.dumps(second)}")

        before_errors = len(failures)
        check_named(first, failures, "phase 2")
        rep.check('river_names', len(failures) == before_errors, DESCRIPTOR.label('river_names'),
                  {"failures": failures[before_errors:]})

        gen_world(port, "unnamed", args.seed, args.size, named=False)
        grade_unnamed_page(rep, port, "unnamed", first, failures)

        send(port, "world.show('named'); return 'ok'")
        send(port, f"engine.saveWorld('named', '{SLOT}'); return 'ok'",
             timeout=30.0)
        rid = capture_request_id(port, "return engine.getSaveStatus()",
                                 seconds=15.0)
        if not rep.check('save_request', rid is not None, DESCRIPTOR.label('save_request')):
            failures.append("phase 4: engine.saveWorld never reported a "
                            "request id")
        else:
            ok, status = wait_save_complete(port, rid, seconds=120.0)
            if not rep.check('save_completed', bool(ok), DESCRIPTOR.label('save_completed')):
                failures.append(f"phase 4: the save did not complete: {status}")
    finally:
        quit_engine(port, proc)

    if failures:
        return fail_out(failures)

    # ---- Phase 4: a FRESH process loads the save --------------------
    proc = boot(port, rep.engine_log_path("river_load.log", LOG),
                args=rep.engine_args())
    try:
        send(port, f"engine.loadSave('{SLOT}'); return 'ok'", timeout=30.0)
        # Unpack: wait_load_published returns a (published, status)
        # TUPLE, which is truthy even when published is False -- so
        # `if not wait_load_published(...)` never fired for ANY
        # unsuccessful terminal, and this phase went on to query a
        # session that had not published. Both failure dispositions stop
        # it now: LoadFailed, and #1204's LoadReconciliationFailed
        # (published, but a Lua onSaveLoaded callback raised, so the
        # river names read below would come off half-reconciled state).
        published, load_status = wait_load_published(port, seconds=240.0)
        if not rep.check('load_published', bool(published), DESCRIPTOR.label('load_published')):
            return fail_out(["phase 4: the load did not reach a fully "
                             f"reconciled LoadPublished ({load_status})"])
        active = send(port, "return world.getActiveWorldId()").strip().strip('"')
        send(port, f"world.show('{active}'); return 'ok'")
        loaded = rivers(port)
        if not rep.check('restored_rivers', loaded == first, DESCRIPTOR.label('restored_rivers')):
            failures.append(
                "phase 4: rivers changed across save -> fresh process -> "
                f"load\n  before: {json.dumps(first)}\n"
                f"  after:  {json.dumps(loaded)}")
        else:
            rep.note(f'phase 4: {len(loaded)} rivers reloaded identically (ids, names, glosses, geometry)')
    finally:
        quit_engine(port, proc)

    # ---- Phase 5: a FRESH process regenerates the same world ---------
    proc = boot(port, rep.engine_log_path("river_regenerate.log", LOG),
                args=rep.engine_args())
    try:
        gen_world(port, "named", args.seed, args.size, named=True)
        regen = rivers(port)
        if not rep.check('regenerated_rivers', regen == first, DESCRIPTOR.label('regenerated_rivers')):
            failures.append(
                "phase 5: regenerating the identical seed + language in a "
                f"fresh process produced different rivers\n"
                f"  original: {json.dumps(first)}\n"
                f"  regen:    {json.dumps(regen)}")
        else:
            rep.note('phase 5: regeneration reproduced every id, name and gloss')
    finally:
        quit_engine(port, proc)

    if failures:
        return fail_out(failures)
    rep.note('river naming probe: PASS')
    return 0


def grade_unnamed_page(rep, port: int, page: str, first: list[dict],
                       failures: list[str]) -> None:
    """Phase 3: query the no-language page and grade it.

    The active page id is read in the same console chunk as the rivers
    and recorded with the payload in every check's detail and failure
    text (#2653), so a failure says WHICH page answered rather than
    leaving that to be reconstructed afterwards."""
    observed, plain = observe_rivers(port)
    where = f"phase 3 (active page {observed!r}, graded page {page!r})"
    seen = {"active_page": observed, "graded_page": page, "rivers": plain}
    before_errors = len(failures)
    check_identity(plain, failures, where)
    rep.check('unnamed_identity', len(failures) == before_errors,
              DESCRIPTOR.label('unnamed_identity'),
              {"failures": failures[before_errors:], **seen})
    if not rep.check(
        'unnamed_same_ids',
        [r['id'] for r in plain] == [r['id'] for r in first],
        DESCRIPTOR.label('unnamed_same_ids'), seen
    ):
        failures.append(
            f"{where}: the same terrain seed produced different river "
            f"ids with and without a language -- naming must not touch "
            f"worldgen")
    if not rep.check(
        'unnamed_absent_names',
        not (any(('name' in r or 'gloss' in r for r in plain))),
        DESCRIPTOR.label('unnamed_absent_names'), seen
    ):
        failures.append(
            f"{where}: a world with NO language provenance must leave "
            "the name/gloss keys ABSENT, got "
            f"{json.dumps([{k: v for k, v in r.items() if k in ('name', 'gloss')} for r in plain])}")


def fail_out(failures: list[str]) -> int:
    print("river naming probe: FAIL")
    for f in failures:
        print(f"  - {f}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
