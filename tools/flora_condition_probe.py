#!/usr/bin/env python3
"""Flora condition seam probe (#2555, EFM-7).

Boots a headless ARENA engine (no worldgen, no GPU) and drives the one
authoritative seam that sets and clears a persistent flora condition,
end to end through the real Lua verbs and the real world thread:

  1. CO-TENANTS. Three plants of one fixture species share ONE tile.
     `world.setFloraCondition(gx, gy, instanceId, 'fire')` kills exactly
     one: the victim reports `dead` / `condition=dead` / `cause=fire` with
     its frozen phase/stage and a snapshotted `retention`, while its two
     siblings stay alive and HARVESTABLE (a harvest of a sibling is
     accepted, of the victim refused).
  2. DEAD TREE. A wood-tagged tree is killed and a tagged `wood` harvest
     (the chop path's verb) is still ACCEPTED: the authored ungated tag
     keeps a standing-dead tree fellable.
  3. ROW CROP AND GROUNDCOVER PLOT. A planted row crop (identity-addressed)
     and a groundcover plot (tile-addressed, `instanceId` nil) are killed;
     both report the cultivated context and the cultivated successor
     (`await_replanting`), and a bare harvest of the dead plot is refused.
  4. FALLBACK TRACE. `world.getFloraVisualAt` names the texture and the
     winning candidate: a species with only generic dead art resolves to
     the legacy dead phase's art, a base-only species to its base art.
  5. REFUSALS. An unknown occurrence, a repeat, an unrecognized cause and
     an unplanted tile each record a `rejected` `flora.condition.set`
     action outcome naming the reason; a malformed request returns false.
  6. CLEAR. Clearing returns exactly the addressed occurrence to its
     clock-derived behaviour; a repeat is a recorded no-op.
  7. PERSISTENCE. Every remaining condition survives a SAVE and LOAD
     through the real transaction: the probe waits for the save to
     complete and the load to publish before it reads anything back.

The fixtures are placed by an arena SCENARIO, so every plant's tile and
count is authored and the probe can FAIL when a fixture is unavailable
rather than skip. The run owns one directory (an isolated resource root,
its saves, fixtures and log) and removes it on every handled exit; the
developer's `saves/` is never read or written.

Usage: python3 tools/flora_condition_probe.py [--port 9211]
       [--keep-artifacts]
"""
import argparse, copy, glob, os, sys, tempfile, time, uuid
import yaml
from probelib import (FixtureNotRegistered, boot, init_arena,
                      load_fixture_yaml, quit_engine, send, send_json)
from flora_growth_probe import (RunArtifacts, abandon_engine,
                                release_artifacts, save_and_reload, set_date)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

HERB = "cond_herb"      # three co-tenants, leaves harvest, generic dead art
OAK = "cond_oak"        # wood tree, ungated wood tag
PLAIN = "cond_plain"    # no phases, no cycle: base art only
ROW = "tomato_plant"    # shipped row crop (cultivated, identity-addressed)
PLOT = "wheat"          # shipped groundcover crop (a tile-keyed plot)

HERB_TILE = (2, 2)
OAK_TILE = (5, 2)
PLAIN_TILE = (7, 2)
ROW_TILE = (9, 2)
PLOT_TILE = (11, 2)
EMPTY_TILE = (13, 2)

SCENARIO = "flora_condition"


def _load_shipped(path, name):
    with open(os.path.join(REPO, path)) as f:
        entries = yaml.safe_load(f)["flora"]
    return copy.deepcopy(next(e for e in entries if e["name"] == name))


def fixture_species():
    """Three authored fixtures, derived from shipped content so their
    harvest blocks and art are the real shapes: a clover-shaped herb, a
    white-oak-shaped tree, and a species reduced to its base art."""
    herb = _load_shipped("data/flora/temperate_wildflowers.yaml", "white_clover")
    herb["name"] = HERB
    # Only the legacy dead PHASE remains, so its generic dead art is the
    # one dead declaration the trace can resolve to.
    herb.pop("cycleOverrides", None)
    oak = _load_shipped("data/flora/temperate_deciduous.yaml", "white_oak")
    oak["name"] = OAK
    plain = _load_shipped("data/flora/temperate_wildflowers.yaml", "white_clover")
    plain["name"] = PLAIN
    plain["lifecycle"] = "evergreen"
    for key in ("phases", "annualCycle", "cycleOverrides", "harvestable",
                "corpsePolicy"):
        plain.pop(key, None)
    return [herb, oak, plain]


def scenario_text():
    """An arena scenario: a flat granite pad carrying every fixture plant.
    Three herbs share HERB_TILE; ages are total growth in game days."""
    lines = [
        "version: 3",
        "terrain:",
        "- material: granite",
        "  rect: {x0: -2, y0: -2, x1: 16, y1: 6}",
        "  surface_z: 0",
        "flora:",
    ]
    for _ in range(3):
        lines.append(f"- {{species: {HERB}, x: {HERB_TILE[0]}, "
                     f"y: {HERB_TILE[1]}, age: 400}}")
    lines.append(f"- {{species: {OAK}, x: {OAK_TILE[0]}, y: {OAK_TILE[1]}, "
                 f"age: 3000}}")
    lines.append(f"- {{species: {PLAIN}, x: {PLAIN_TILE[0]}, "
                 f"y: {PLAIN_TILE[1]}}}")
    lines.append(f"- {{species: {ROW}, x: {ROW_TILE[0]}, y: {ROW_TILE[1]}, "
                 f"age: 60}}")
    lines.append(f"- {{species: {PLOT}, x: {PLOT_TILE[0]}, y: {PLOT_TILE[1]}, "
                 f"age: 60}}")
    return "\n".join(lines) + "\n"


class Report:
    def __init__(self):
        self.failed = []

    def check(self, ok, what):
        print(f"  [{'PASS' if ok else 'FAIL'}] {what}", flush=True)
        if not ok:
            self.failed.append(what)
        return ok


#: Everything a query reports about an occurrence's effective state.
EFFECTIVE_KEYS = ("dead", "condition", "cause", "context", "phase", "stage",
                  "phaseAtDeath", "stageAtDeath", "age", "generation",
                  "harvestable", "retention")


def growth(port, tile):
    got = send_json(port, f"return world.getFloraGrowthAt({tile[0]},{tile[1]})")
    return got if isinstance(got, list) else []


def wait_for(fn, seconds=15.0, interval=0.25):
    """Poll `fn` until it returns a truthy value; the world thread applies
    a queued command asynchronously, so no fixed sleep can stand in."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        value = fn()
        if value:
            return value
        time.sleep(interval)
    return fn()


def outcomes(port):
    got = send_json(port, "return debug.drainActionOutcomes()")
    return got if isinstance(got, list) else []


def set_cond(port, tile, iid, cause):
    arg = "nil" if iid is None else str(iid)
    got = send(port, f"return tostring(world.setFloraCondition("
                     f"{tile[0]},{tile[1]},{arg},'{cause}'))")
    return got.strip().strip('"') == "true"


def clear_cond(port, tile, iid):
    arg = "nil" if iid is None else str(iid)
    got = send(port, f"return tostring(world.clearFloraCondition("
                     f"{tile[0]},{tile[1]},{arg}))")
    return got.strip().strip('"') == "true"


def is_dead(entry):
    return bool(entry) and entry.get("dead") is True \
        and entry.get("condition") == "dead"


def entry_of(port, tile, iid):
    for e in growth(port, tile):
        if e.get("instanceId") == iid:
            return e
    return None


def plot_of(port, tile):
    got = send_json(port, f"return world.getCropPlotAt({tile[0]},{tile[1]})")
    return got if isinstance(got, dict) else None


def visual(port, tile, iid):
    arg = "nil" if iid is None else str(iid)
    return send_json(port, f"return world.getFloraVisualAt("
                           f"{tile[0]},{tile[1]},{arg})")


def bootstrap(port, art):
    """Substances, items, materials and the shipped flora (for the row
    crop and the groundcover plot), then this probe's three fixtures. The
    shipped flora is loaded with its result read: a file that stopped
    registering is exactly the regression this probe must not miss."""
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    for path in sorted(glob.glob("data/flora/*.yaml")):
        raw = send(port, f"return engine.loadFloraYaml('{path}')")
        try:
            queued = float(raw)
        except (TypeError, ValueError):
            queued = 0.0
        if queued <= 0:
            sys.exit(f"SETUP FAILURE: shipped flora {path} rejected ({raw!r})")
    path = art.fixture("condition_species")
    with open(path, "w") as f:
        yaml.safe_dump({"flora": fixture_species()}, f)
    load_fixture_yaml(port, "engine.loadFloraYaml", path)


def load_scenario(port, art):
    """Install the scenario into a fresh arena and wait for ITS operation."""
    target = os.path.join(art.root, "scenarios", f"{SCENARIO}.yaml")
    with open(target, "w") as f:
        f.write(scenario_text())
    reply = send(port, f"return engine.loadScenario('{SCENARIO}')")
    try:
        op = int(float(reply.strip()))
    except ValueError:
        sys.exit(f"SETUP FAILURE: engine.loadScenario was not accepted: "
                 f"{reply!r}")
    deadline = time.time() + 120.0
    status = None
    while time.time() < deadline:
        status = send_json(port, "return engine.getScenarioStatus()")
        if isinstance(status, dict) and status.get("kind") == "load" \
                and status.get("id") == op and status.get("done"):
            break
        time.sleep(0.25)
    if not (isinstance(status, dict) and status.get("done")):
        sys.exit(f"SETUP FAILURE: scenario load never finished: {status!r}")
    return status


def find_fixture_iids(port):
    """The authored co-tenants by identity. FAILS (never skips) when a
    fixture plant did not materialize."""
    herbs = [e for e in growth(port, HERB_TILE) if e.get("id") == HERB]
    oaks = [e for e in growth(port, OAK_TILE) if e.get("id") == OAK]
    plains = [e for e in growth(port, PLAIN_TILE) if e.get("id") == PLAIN]
    rows = [e for e in growth(port, ROW_TILE) if e.get("id") == ROW]
    return herbs, oaks, plains, rows


def run_checks(port, art, rep, slot):
    # The scenario replaced the session, so the arena it built has its OWN
    # page id; every page-addressed verb below uses that one.
    page = send(port, "return world.getActiveWorldId()").strip().strip('"')
    rep.check(bool(page) and page != "nil", f"a scenario page is active: {page}")
    set_date(port, page, 2, 7, 21)   # day-of-year 201: mid-year, mature
    herbs, oaks, plains, rows = find_fixture_iids(port)
    if not (rep.check(len(herbs) == 3, f"three {HERB} co-tenants share "
                      f"{HERB_TILE}: {len(herbs)}")
            and rep.check(len(oaks) == 1, f"one {OAK} stands at {OAK_TILE}")
            and rep.check(len(plains) == 1, f"one {PLAIN} stands at "
                          f"{PLAIN_TILE}")
            and rep.check(len(rows) == 1, f"one {ROW} row crop stands at "
                          f"{ROW_TILE}")):
        return
    plot = plot_of(port, PLOT_TILE)
    if not rep.check(plot is not None and plot.get("id") == PLOT,
                     f"a {PLOT} plot stands at {PLOT_TILE}: {plot}"):
        return
    ids = [e["instanceId"] for e in herbs]
    rep.check(len(set(ids)) == 3, f"the three co-tenants have distinct "
                                  f"identities: {ids}")
    # A scenario plant is a planted occurrence, and context is fixed at
    # creation (#2547): planted flora is cultivated. The wild context is
    # exercised by the hspec group, where chunks are generated.
    rep.check(all(e.get("dead") is False and e.get("condition") == "alive"
                  and e.get("context") == "cultivated" for e in herbs),
              "every co-tenant starts alive in the planted (cultivated) context")
    outcomes(port)   # drop anything recorded before the checks

    # --- 1. kill one of three co-tenants ---
    victim, sib_a, sib_b = ids
    rep.check(set_cond(port, HERB_TILE, victim, "fire"),
              "setFloraCondition is ADMITTED (true = queued, not applied)")
    dead = wait_for(lambda: is_dead(entry_of(port, HERB_TILE, victim))
                    and entry_of(port, HERB_TILE, victim))
    rep.check(bool(dead), "the world thread applied the condition")
    dead = dead or {}
    rep.check(dead.get("cause") == "fire"
              and dead.get("context") == "cultivated",
              f"the victim reports its cause and context: {dead.get('cause')}"
              f"/{dead.get('context')}")
    rep.check(dead.get("phaseAtDeath") is not None
              and dead.get("phase") == dead.get("phaseAtDeath"),
              f"the victim's reported phase is its frozen phase: "
              f"{dead.get('phase')} == {dead.get('phaseAtDeath')}")
    ret = dead.get("retention") or {}
    rep.check(ret.get("visibility") in ("transient", "persistent"),
              f"the record snapshots its retention outcome: {ret}")
    rep.check(dead.get("harvestable") is False,
              "the dead victim is not harvestable")
    sibs = [entry_of(port, HERB_TILE, i) for i in (sib_a, sib_b)]
    rep.check(all(s and s.get("dead") is False and s.get("condition") == "alive"
                  and s.get("harvestable") is True for s in sibs),
              "both siblings are still alive and harvestable")
    got_victim = send_json(port, f"return world.harvestFloraInstance("
                                 f"{HERB_TILE[0]},{HERB_TILE[1]},{victim})")
    rep.check(got_victim is None, "a harvest of the dead victim is refused")
    got_sib = send_json(port, f"return world.harvestFloraInstance("
                              f"{HERB_TILE[0]},{HERB_TILE[1]},{sib_a})")
    rep.check(isinstance(got_sib, list),
              "a harvest of a living sibling is accepted")
    flora_at = send_json(port, f"return world.getFloraAt({HERB_TILE[0]},"
                               f"{HERB_TILE[1]})")
    rep.check(isinstance(flora_at, dict) and "dead" in flora_at
              and "condition" in flora_at,
              f"getFloraAt reports the additive condition fields: "
              f"{flora_at and sorted(flora_at)}")

    # --- 4. fallback trace: only generic dead art ---
    trace = visual(port, HERB_TILE, victim)
    rep.check(isinstance(trace, dict)
              and trace.get("winner") == "variant:condition=dead"
              and isinstance(trace.get("tried"), list)
              and trace["tried"][-1] == trace.get("winner")
              and "dead" in str(trace.get("texture")),
              f"a species with only generic dead art resolves to it: {trace}")

    # --- 2. dead tree stays fellable ---
    oak = oaks[0]["instanceId"]
    rep.check(set_cond(port, OAK_TILE, oak, "drought"), "kill the tree")
    rep.check(bool(wait_for(lambda: is_dead(entry_of(port, OAK_TILE, oak)))),
              "the tree reports dead")
    felled = send_json(port, f"return world.harvestFloraInstance("
                             f"{OAK_TILE[0]},{OAK_TILE[1]},{oak},'wood')")
    rep.check(felled is not None,
              "a wood harvest of the standing-dead tree is ACCEPTED "
              f"(the ungated tag keeps it fellable): {felled}")

    # --- 4. fallback trace: base art only ---
    plain = plains[0]["instanceId"]
    rep.check(set_cond(port, PLAIN_TILE, plain, "unknown"),
              "'unknown' is a valid cause")
    rep.check(bool(wait_for(lambda: is_dead(entry_of(port, PLAIN_TILE, plain)))),
              "the base-only species reports dead")
    trace = visual(port, PLAIN_TILE, plain)
    rep.check(isinstance(trace, dict) and trace.get("winner") == "base"
              and trace.get("texture") == f"flora_base_{PLAIN}",
              f"a base-only species falls back to its base art: {trace}")

    # --- 3. row crop (identity) and plot (tile) ---
    row = rows[0]["instanceId"]
    rep.check(set_cond(port, ROW_TILE, row, "frost"), "kill the row crop")
    row_dead = wait_for(lambda: is_dead(entry_of(port, ROW_TILE, row))
                        and entry_of(port, ROW_TILE, row))
    rep.check(bool(row_dead) and row_dead.get("context") == "cultivated",
              f"the row crop reports the cultivated context: {row_dead}")
    row_ret = (row_dead or {}).get("retention") or {}
    rep.check(row_ret.get("visibility") == "persistent"
              or row_ret.get("successor") == "await_replanting",
              f"a cultivated transient corpse awaits replanting: {row_ret}")
    rep.check(set_cond(port, PLOT_TILE, None, "drought"),
              "kill the groundcover plot by tile alone")
    plot_dead = wait_for(lambda: (plot_of(port, PLOT_TILE) or {}).get("dead")
                         and plot_of(port, PLOT_TILE))
    rep.check(bool(plot_dead) and plot_dead.get("context") == "cultivated"
              and plot_dead.get("cause") == "drought",
              f"the plot reports dead, cultivated, drought: {plot_dead}")
    plot_ret = (plot_dead or {}).get("retention") or {}
    rep.check(plot_ret.get("visibility") == "persistent"
              or plot_ret.get("successor") == "await_replanting",
              f"the plot's retention is the cultivated outcome: {plot_ret}")
    refused = send_json(port, f"return world.harvestFlora({PLOT_TILE[0]},"
                              f"{PLOT_TILE[1]})")
    rep.check(refused is None and plot_of(port, PLOT_TILE) is not None,
              "a bare harvest of the dead plot is refused and keeps the plot")
    plot_trace = visual(port, PLOT_TILE, None)
    rep.check(isinstance(plot_trace, dict) and plot_trace.get("winner"),
              f"the plot has a fallback trace: {plot_trace}")

    # --- 5. refusals record a named reason; malformed returns false ---
    outcomes(port)
    absent = 3_000_000_000_000
    cases = [
        (lambda: set_cond(port, HERB_TILE, victim, "frost"),
         "already has a condition"),
        (lambda: set_cond(port, HERB_TILE, absent, "fire"),
         "no such flora"),
        (lambda: set_cond(port, HERB_TILE, sib_b, "arson"),
         "unknown cause 'arson'"),
        (lambda: set_cond(port, EMPTY_TILE, None, "fire"),
         "no such flora"),
    ]
    for call, fragment in cases:
        outcomes_buf.seen.clear()
        outcomes(port)
        admitted = call()
        got = wait_for(lambda: [o for o in outcomes_buf(port)
                                if o.get("kind") == "flora.condition.set"])
        reasons = [o.get("reason", "") for o in got]
        rep.check(admitted and bool(got) and got[0].get("outcome") == "rejected"
                  and fragment in reasons[0],
                  f"a refused set records '{fragment}': {reasons}")
    for bad in ("world.setFloraCondition('a',1,nil,'fire')",
                "world.setFloraCondition(1,1,0,'fire')",
                "world.setFloraCondition(1,1,'5','fire')",
                "world.setFloraCondition(1,1,nil,7)",
                "world.clearFloraCondition(1,1,-4)"):
        got = send(port, f"return tostring({bad})").strip().strip('"')
        rep.check(got == "false", f"malformed request returns false: {bad}")

    # --- 6. clear returns exactly one occurrence to its clock ---
    outcomes(port)
    outcomes_buf.seen.clear()
    rep.check(clear_cond(port, HERB_TILE, victim), "clear is admitted")
    alive_again = wait_for(lambda: (entry_of(port, HERB_TILE, victim) or {})
                           .get("dead") is False)
    rep.check(bool(alive_again), "the cleared occurrence is alive again")
    rep.check(is_dead(entry_of(port, OAK_TILE, oak)),
              "clearing one occurrence leaves the others' conditions alone")
    rep.check(clear_cond(port, HERB_TILE, victim), "a repeat clear is admitted")
    noop = wait_for(lambda: [o for o in outcomes_buf(port)
                             if o.get("kind") == "flora.condition.clear"
                             and o.get("outcome") == "noop"])
    rep.check(bool(noop), "a repeat clear is a recorded no-op")

    # --- 7. frozen state survives a stage change, the stored expiry and
    #        a real save/load ---
    rep.check(set_cond(port, HERB_TILE, sib_b, "disease"),
              "re-kill one co-tenant before saving")
    rep.check(bool(wait_for(lambda: is_dead(entry_of(port, HERB_TILE, sib_b)))),
              "the re-kill applied")

    subjects = {
        "herb": (HERB_TILE, sib_b),
        "oak": (OAK_TILE, oak),
        "plain": (PLAIN_TILE, plain),
        "row": (ROW_TILE, row),
        "plot": (PLOT_TILE, None),
    }

    def effective():
        """What each condition currently MEANS: the effective growth state
        (phase, stage, age, generation, harvestability), the stored
        metadata, and what is drawn (the trace's winner and texture)."""
        out = {}
        for name, (tile, iid) in subjects.items():
            e = plot_of(port, tile) if iid is None else \
                entry_of(port, tile, iid)
            e = e or {}
            keep = {k: e.get(k) for k in EFFECTIVE_KEYS}
            trace = visual(port, tile, iid)
            if isinstance(trace, dict):
                keep["winner"] = trace.get("winner")
                keep["texture"] = trace.get("texture")
            out[name] = keep
        return out

    def absolute_day():
        d = send_json(port, f"return world.getDate('{page}')")
        return d.get("absoluteDay") if isinstance(d, dict) else None

    snapshot = effective()
    control_before = entry_of(port, HERB_TILE, sib_a)
    day_before = absolute_day()
    expiries = {n: ((v.get("retention") or {}).get("expiryDay"))
                for n, v in snapshot.items()}
    transient = {n: x for n, x in expiries.items() if x is not None}
    rep.check(len(transient) >= 3 and all(snapshot[n].get("dead")
                                          for n in snapshot),
              f"transient corpses are in play (stored expiry days): "
              f"{transient}")

    # Advance the calendar across a stage boundary AND past every stored
    # expiry. A living control on the same clock must visibly move, which
    # is what proves the clock really crossed something.
    set_date(port, page, 4, 2, 5)
    day_after = absolute_day()
    rep.check(isinstance(day_after, int) and isinstance(day_before, int)
              and day_after > max(transient.values()) > day_before,
              f"the calendar moved past every stored expiry: day "
              f"{day_before} -> {day_after} (expiries {transient})")
    control_after = entry_of(port, HERB_TILE, sib_a)
    rep.check(control_before is not None and control_after is not None
              and (control_after.get("stage"), control_after.get("age"))
              != (control_before.get("stage"), control_before.get("age")),
              f"a living control moved with the clock: "
              f"{control_before and (control_before.get('stage'), control_before.get('age'))}"
              f" -> {control_after and (control_after.get('stage'), control_after.get('age'))}")
    moved = effective()
    for name in subjects:
        rep.check(moved[name] == snapshot[name],
                  f"{name}: effective growth and appearance stay frozen "
                  f"across the stage change and past its expiry: "
                  f"{moved[name]}")

    failure = save_and_reload(port, page, slot)
    if failure:
        rep.check(False, failure)
        return
    send(port, f"world.show('{page}'); return 'ok'")

    after = wait_for(lambda: effective() if all(
        v.get("dead") is True for v in effective().values()) else None)
    after = after or effective()
    rep.check(absolute_day() == day_after,
              f"the calendar survived the load: {absolute_day()}")
    for name in subjects:
        rep.check(after[name] == snapshot[name],
                  f"{name}: the frozen state survives save/load unchanged: "
                  f"{after[name]}")
    victim_after = entry_of(port, HERB_TILE, victim)
    rep.check(bool(victim_after) and victim_after.get("dead") is False,
              "the occurrence cleared before the save is still alive")


def outcomes_buf(port):
    """Accumulate drained outcomes so a poll that drains does not lose the
    record it is waiting for."""
    outcomes_buf.seen.extend(outcomes(port))
    return outcomes_buf.seen
outcomes_buf.seen = []


def run_probe(args, art) -> bool:
    port = args.port
    root = art.build()
    os.makedirs(os.path.join(root, "scenarios"), exist_ok=True)
    slot = f"flora_condition_check_{uuid.uuid4().hex[:8]}"
    print(f"isolated resource root: {root}", flush=True)
    print(f"save slot: {slot}", flush=True)
    rep = Report()
    launched: list = []
    proc = None
    try:
        proc = boot(port, art.engine_log, args=["--resource-root", root],
                    on_launch=launched.append)
        bootstrap(port, art)
        init_arena(port, "probe")
        load_scenario(port, art)
        run_checks(port, art, rep, slot)
        return not rep.failed
    finally:
        if proc is not None:
            try:
                quit_engine(port, proc)
            finally:
                abandon_engine(proc)
        elif launched:
            abandon_engine(launched[0])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9211)
    ap.add_argument("--keep-artifacts", action="store_true",
                    help="keep this run's artifact directory (fixtures, "
                         "engine log, isolated resource root and saves) "
                         "instead of deleting it, and name it")
    args = ap.parse_args()
    art = RunArtifacts(tempfile.mkdtemp(prefix="synarchy_flora_condition_"))
    passed = False
    try:
        passed = run_probe(args, art)
        print("\n" + ("ALL FLORA CONDITION CHECKS PASSED" if passed
                      else "SOME FAILED"))
    except SystemExit as exc:
        print(f"\n  [FAIL] the run aborted before finishing: {exc}")
    finally:
        released = release_artifacts(art, args.keep_artifacts)
        if not passed and not args.keep_artifacts:
            print("  (this run's engine log, fixtures and save slot went with "
                  "its artifact directory — re-run with --keep-artifacts)")
    return 0 if passed and released else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        sys.exit(1)
