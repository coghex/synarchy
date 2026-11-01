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
  7. RETENTION. A whole-day expiry leaves transient reseed, absent, row,
     and plot outcomes on their owning page calendar; persistent remains
     stay dead. Rows and plots can be replanted after expiry.
  8. FRESH-PROCESS PERSISTENCE. Reseed anchors, absence tombstones,
     persistent corpses, cultivated removal edits, and deliberate
     replanting survive a completed save, process exit, and fresh load.

Synthetic flora fixtures are placed by an arena SCENARIO; row and plot
fixtures are tilled and planted through their production commands on
authored loam tiles. Every fixture is required, so the probe fails when one
is unavailable rather than skipping. The run owns one directory (an
isolated resource root, its saves, fixtures and log) and removes it on
every handled exit; the developer's `saves/` is never read or written.

Usage: python3 tools/flora_condition_probe.py [--port 9211]
       [--keep-artifacts]
"""
import argparse, copy, glob, os, sys, tempfile, time, uuid
import yaml
from probelib import (FixtureNotRegistered, boot, capture_request_id,
                      init_arena, load_fixture_yaml, quit_engine, send,
                      send_json, wait_load_published, wait_save_complete)
from flora_growth_probe import (RunArtifacts, abandon_engine,
                                release_artifacts, set_date)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

HERB = "cond_herb"      # three co-tenants, leaves harvest, generic dead art
OAK = "cond_oak"        # wood tree, ungated wood tag
PLAIN = "cond_plain"    # no phases, no cycle: base art only
ABSENT = "cond_absent"  # transient corpse whose wild successor is absence
ROW = "tomato_plant"    # shipped row crop (cultivated, identity-addressed)
PLOT = "wheat"          # shipped groundcover crop (a tile-keyed plot)

HERB_TILE = (2, 2)
OAK_TILE = (5, 2)
PLAIN_TILE = (7, 2)
ROW_TILE = (9, 2)
PLOT_TILE = (11, 2)
EMPTY_TILE = (13, 2)
ABSENT_TILE = (15, 2)

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
    plain["corpsePolicy"] = {"visibility": "persistent"}
    absent = copy.deepcopy(herb)
    absent["name"] = ABSENT
    absent["corpsePolicy"] = {"visibility": "transient",
                              "durationDays": 60,
                              "successor": "absent"}
    # Give the generated-page half of this probe reliable habitat coverage;
    # these are temporary fixtures, not repository species definitions.
    for entry in (herb, absent, plain):
        wg = entry["worldGen"]
        wg.update({"minTemp": -100, "maxTemp": 100, "idealTemp": 0,
                   "minPrecip": 0, "maxPrecip": 1, "idealPrecip": 0.5,
                   "minAlt": -100000, "maxAlt": 100000, "idealAlt": 0,
                   "minHumidity": 0, "maxHumidity": 1,
                   "idealHumidity": 0.5, "maxSlope": 255,
                   "density": 1.0})
    return [herb, oak, plain, absent]


def scenario_text():
    """An arena scenario: a flat loam pad carrying every fixture plant.
    Three herbs share HERB_TILE; ages are total growth in game days."""
    lines = [
        "version: 3",
        "terrain:",
        "- material: loam",
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
    lines.append(f"- {{species: {ABSENT}, x: {ABSENT_TILE[0]}, "
                 f"y: {ABSENT_TILE[1]}, age: 400}}")
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


def find_live_occurrence(port, species):
    return send_json(port, f"(function() for gx=-64,64 do "
        f"for gy=-64,64 do local a=world.getFloraGrowthAt(gx,gy); "
        f"if a then for _,e in ipairs(a) do if e.id=='{species}' and "
        f"not e.dead then return {{x=gx,y=gy,instanceId=e.instanceId,"
        f"generation=e.generation}} end end end end end return nil end)()",
        timeout=60.0)


def run_wild_retention(port, rep):
    """Use a generated world page for genuine wild-context occurrences.
    Scenario-authored flora are intentionally cultivated by their owner."""
    page = "flora_condition_wild"
    admitted = send(port, f"world.init('{page}',42,64,3); return 'ok'")
    rep.check("ok" in admitted, f"the wild fixture world is admitted: {admitted}")
    send(port, "return world.waitForInit(300)", timeout=310)
    send(port, f"world.show('{page}'); return 'ok'")
    active = wait_for(lambda: send(port, "return world.getActiveWorldId()")
                      .strip().strip('"') == page)
    rep.check(bool(active), f"the wild fixture world is active: {page}")
    send(port, "return world.loadChunksInRegion(-4,-4,4,4)", timeout=30)
    send(port, "return world.waitForChunks(120)", timeout=125)

    occurrences = {name: find_live_occurrence(port, name)
                   for name in (HERB, ABSENT, PLAIN)}
    for name, found in occurrences.items():
        rep.check(bool(found), f"a live wild {name} fixture exists: {found}")
    if not all(occurrences.values()):
        return None

    def condition(name, cause):
        found = occurrences[name]
        tile = (found["x"], found["y"])
        admitted = set_cond(port, tile, found["instanceId"], cause)
        rep.check(admitted, f"wild {name} condition is admitted")
        entry = wait_for(lambda: entry_of(port, tile, found["instanceId"]))
        rep.check(bool(entry) and entry.get("dead") is True
                  and entry.get("context") == "wild",
                  f"wild {name} occurrence reaches its frozen corpse: {entry}")
        return found, tile, entry or {}

    reseed_found, reseed_tile, reseed_dead = condition(HERB, "disease")
    absent_found, absent_tile, absent_dead = condition(ABSENT, "fire")
    persistent_found, persistent_tile, persistent_dead = condition(PLAIN, "frost")
    reseed_expiry = (reseed_dead.get("retention") or {}).get("expiryDay")
    absent_expiry = (absent_dead.get("retention") or {}).get("expiryDay")
    rep.check(isinstance(reseed_expiry, int),
              f"wild reseed has a whole-day expiry: {reseed_dead}")
    rep.check(isinstance(absent_expiry, int),
              f"wild absence has a whole-day expiry: {absent_dead}")
    if not isinstance(reseed_expiry, int) or not isinstance(absent_expiry, int):
        return None
    rep.check((reseed_dead.get("retention") or {}).get("successor") == "reseed",
              "wild reseed policy is snapshotted")
    rep.check((absent_dead.get("retention") or {}).get("successor") == "absent",
              "wild absent policy is snapshotted")

    target = max(reseed_expiry, absent_expiry)
    set_absolute_day(port, page, min(reseed_expiry, absent_expiry) - 1)
    rep.check(is_dead(entry_of(port, reseed_tile, reseed_found["instanceId"]))
              and is_dead(entry_of(port, absent_tile, absent_found["instanceId"])),
              "wild reseed and absence corpses persist before expiry")
    set_absolute_day(port, page, target)
    reseeded = entry_of(port, reseed_tile, reseed_found["instanceId"])
    absent_query = growth(port, absent_tile)
    persistent = entry_of(port, persistent_tile,
                          persistent_found["instanceId"])
    rep.check(bool(reseeded) and reseeded.get("dead") is False
              and reseeded.get("generation", 0) >
                  reseed_found.get("generation", 0)
              and reseeded.get("age") is not None,
              f"wild reseed advances from its expiry-day anchor: {reseeded}")
    rep.check(not any(e.get("instanceId") == absent_found["instanceId"]
                      for e in absent_query),
              f"wild absent tombstone excludes the occurrence: {absent_query}")
    rep.check(bool(persistent) and persistent.get("dead") is True,
              f"wild persistent remains survive the same calendar jump: "
              f"{persistent}")
    return {
        "page": page,
        "reseed": {"tile": reseed_tile,
                   "instanceId": reseed_found["instanceId"],
                   "generation": (reseeded or {}).get("generation"),
                   "age": (reseeded or {}).get("age")},
        "absent": {"tile": absent_tile,
                   "instanceId": absent_found["instanceId"]},
        "persistent": {"tile": persistent_tile,
                       "instanceId": persistent_found["instanceId"]},
    }


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


def set_absolute_day(port, page, target):
    year = target // 360 + 1
    month = (target % 360) // 30 + 1
    day = target % 30 + 1
    return set_date(port, page, year, month, day)


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
    absent = [e for e in growth(port, ABSENT_TILE) if e.get("id") == ABSENT]
    rows = [e for e in growth(port, ROW_TILE) if e.get("id") == ROW]
    return herbs, oaks, plains, absent, rows


def prepare_plantable_tile(port, page, tile):
    # Match crop_probe's proven read: getSurfaceAt returns multiple Lua
    # values, so bind only its first result as the surface z before the
    # queued vegetation edit and poll the same gate planting uses.
    surface = send_json(port, f"local sz=world.getSurfaceAt({tile[0]},"
                              f"{tile[1]}); return sz")
    if surface is None:
        return False
    send(port, f"world.setVegAt('{page}', {tile[0]}, {tile[1]}, {surface}, 77); "
               f"return 'ok'")
    return wait_for(lambda: send_json(
        port, f"return world.isPlantable({tile[0]},{tile[1]})"
    ) is True)


def run_checks(port, art, rep, slot, process, launched):
    # The scenario replaced the session, so the arena it built has its OWN
    # page id; every page-addressed verb below uses that one.
    page = send(port, "return world.getActiveWorldId()").strip().strip('"')
    rep.check(bool(page) and page != "nil", f"a scenario page is active: {page}")
    set_date(port, page, 2, 7, 21)   # day-of-year 201: mid-year, mature
    herbs, oaks, plains, absents, rows = find_fixture_iids(port)
    row_prepared = prepare_plantable_tile(port, page, ROW_TILE)
    rep.check(bool(row_prepared), f"the row fixture tile is plantable: {ROW_TILE}")
    row_admitted = send(port, f"return tostring(world.plantRowCropAt('{page}',"
                    f"{ROW_TILE[0]},{ROW_TILE[1]},'{ROW}'))")
    rep.check(row_admitted.strip().strip('"') == "true",
              f"the row fixture is admitted through world.plantRowCropAt: "
              f"{row_admitted}")
    rows = wait_for(lambda: [e for e in growth(port, ROW_TILE)
                             if e.get("id") == ROW]) or []
    plot_prepared = prepare_plantable_tile(port, page, PLOT_TILE)
    rep.check(bool(plot_prepared), f"the plot fixture tile is plantable: {PLOT_TILE}")
    plot_admitted = send_json(port, f"return world.plantCropAt("
                    f"{PLOT_TILE[0]},{PLOT_TILE[1]},'{PLOT}')")
    rep.check(plot_admitted is True,
              f"the plot fixture is admitted through world.plantCropAt: "
              f"{plot_admitted}")
    plot = wait_for(lambda: plot_of(port, PLOT_TILE))
    row_raw = send_json(port, f"return world.getFloraGrowthAt({ROW_TILE[0]},"
                              f"{ROW_TILE[1]})")
    if not (rep.check(len(herbs) == 3, f"three {HERB} co-tenants share "
                      f"{HERB_TILE}: {len(herbs)}")
            and rep.check(len(oaks) == 1, f"one {OAK} stands at {OAK_TILE}")
            and rep.check(len(plains) == 1, f"one {PLAIN} stands at "
                          f"{PLAIN_TILE}")
            and rep.check(len(absents) == 1, f"one {ABSENT} stands at "
                          f"{ABSENT_TILE}")
            and rep.check(len(rows) == 1, f"one {ROW} row crop stands at "
                          f"{ROW_TILE}: entries={row_raw}")):
        return process[0]
    if not rep.check(plot is not None and plot.get("id") == PLOT,
                     f"a {PLOT} plot stands at {PLOT_TILE}: {plot}"):
        return process[0]
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

    absent_id = absents[0]["instanceId"]
    rep.check(set_cond(port, ABSENT_TILE, absent_id, "fire"),
              "kill the wild absent-successor fixture")
    absent_dead = wait_for(lambda: is_dead(entry_of(port, ABSENT_TILE,
                                                    absent_id))
                           and entry_of(port, ABSENT_TILE, absent_id))
    rep.check(bool(absent_dead)
              and absent_dead.get("context") == "cultivated"
              and (absent_dead.get("retention") or {}).get("successor")
              == "await_replanting",
              f"the scenario corpse snapshots its cultivated successor: "
              f"{absent_dead}")

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

    # Hold every transient corpse at the day before expiry, then cross its
    # stored boundary. Day numbers are 360-day years with twelve 30-day
    # months, matching the owning page's canonical calendar.
    expiry = max(transient.values())
    set_absolute_day(port, page, expiry - 1)
    pre_expiry = effective()
    rep.check(all(pre_expiry[n].get("dead") for n in
                  ("herb", "row", "plot")),
              f"wild, row, and plot corpses remain dead before expiry: "
              f"{pre_expiry}")
    set_absolute_day(port, page, expiry)
    day_after = absolute_day()
    rep.check(isinstance(day_after, int) and day_after >= expiry,
              f"the calendar reached stored expiry {expiry}: {day_after}")
    control_after = entry_of(port, HERB_TILE, sib_a)
    rep.check(control_before is not None and control_after is not None
              and (control_after.get("stage"), control_after.get("age"))
              != (control_before.get("stage"), control_before.get("age")),
              f"a living control moved with the clock: "
              f"{control_before and (control_before.get('stage'), control_before.get('age'))}"
              f" -> {control_after and (control_after.get('stage'), control_after.get('age'))}")
    moved = effective()
    absent_query = growth(port, ABSENT_TILE)
    row_after = entry_of(port, ROW_TILE, row)
    plot_after = plot_of(port, PLOT_TILE)
    rep.check(entry_of(port, HERB_TILE, sib_b) is None,
              "a scenario-planted transient flora occurrence uses the "
              "cultivated await-replanting outcome")
    rep.check(not any(e.get("instanceId") == absent_id
                      for e in absent_query),
              "the expired scenario crop identity is absent from queries")
    rep.check(row_after is None,
              f"the expired cultivated row is durably removed: {row_after}")
    rep.check(plot_after is None,
              f"the expired cultivated plot is removed: {plot_after}")
    rep.check(bool(moved["oak"].get("dead"))
              and moved["oak"].get("retention") == snapshot["oak"].get("retention"),
              f"persistent remains keep their snapshotted retention: "
              f"{moved['oak']}")

    row_replantable = prepare_plantable_tile(port, page, ROW_TILE)
    plot_replantable = prepare_plantable_tile(port, page, PLOT_TILE)
    rep.check(bool(row_replantable and plot_replantable),
              "expired cultivated tiles can be prepared for replanting")
    row_replant = send_json(port, f"return world.plantRowCropAt('{page}',"
                       f"{ROW_TILE[0]},{ROW_TILE[1]},'{ROW}')")
    plot_replant = send_json(port, f"return world.plantCropAt("
                       f"{PLOT_TILE[0]},{PLOT_TILE[1]},'{PLOT}')")
    rep.check(row_replant is True,
              f"the emptied row tile accepts deliberate replanting: "
              f"{row_replant}")
    rep.check(plot_replant is True,
              f"the emptied plot tile accepts deliberate replanting: "
              f"{plot_replant}")
    row_new = wait_for(lambda: next((e for e in growth(port, ROW_TILE)
                                    if e.get("id") == ROW), None))
    plot_new = wait_for(lambda: plot_of(port, PLOT_TILE))
    rep.check(bool(row_new) and row_new.get("instanceId") != row,
              f"replanting creates a fresh row identity: {row_new}")
    rep.check(bool(plot_new) and plot_new.get("id") == PLOT,
              f"replanting creates a living plot: {plot_new}")

    wild_snapshot = run_wild_retention(port, rep)
    # Save with the wild-retention page as the requested primary page so the
    # load transaction restores it as active. Hide the scenario page first:
    # world.show is idempotent for a page that is already visible and does not
    # promote a secondary visible page to the active head.
    save_page = wild_snapshot["page"] if wild_snapshot is not None else page
    send(port, f"world.hide('{page}'); return 'ok'")
    wait_for(lambda: send(port, "return world.getActiveWorldId()")
             .strip().strip('"') == save_page)

    # Capture all successor forms, then restart the actual engine process
    # before loading the production save. This checks disk-backed state,
    # not a same-process snapshot cache.
    saved = send(port, f"return engine.saveWorld('{save_page}', '{slot}')")
    rep.check(saved.strip() == "true", f"save is accepted: {saved}")
    save_id = capture_request_id(port, "return engine.getSaveStatus()")
    if not save_id:
        rep.check(False, "save request exposes its completion id")
        return process[0]
    save_ok, save_status = wait_save_complete(port, save_id)
    rep.check(save_ok, f"save request {save_id} completes: {save_status}")
    quit_engine(port, process[0])
    from flora_growth_probe import abandon_engine
    abandon_engine(process[0])
    process[0] = boot(port, art.engine_log,
                      args=["--resource-root", art.root],
                      on_launch=launched.append)
    bootstrap(port, art)
    loaded = send(port, f"return engine.loadSave('{slot}')")
    rep.check(loaded.strip() == "true", f"fresh process accepts load: {loaded}")
    load_id = capture_request_id(port, "return engine.getLoadStatus()")
    if not load_id:
        rep.check(False, "fresh-process load exposes its publication id")
        return process[0]
    published, load_status = wait_load_published(port, 200,
                                                  request_id=load_id)
    rep.check(published, f"fresh-process load {load_id} publishes: "
              f"{load_status}")
    if wild_snapshot is not None:
        wild_page = wild_snapshot["page"]
        wild_active = wait_for(lambda: send(
            port, "return world.getActiveWorldId()"
        ).strip().strip('"') == wild_page)
        rep.check(bool(wild_active),
                  f"the restored wild page becomes active: {wild_page}")
        send(port, f"return world.loadChunksInRegion(-4,-4,4,4,'{wild_page}')",
             timeout=30)
        send(port, f"return world.waitForChunks(120,'{wild_page}')",
             timeout=125)
        reseed_saved = wild_snapshot["reseed"]
        absent_saved = wild_snapshot["absent"]
        persistent_saved = wild_snapshot["persistent"]
        reseed_entry = wait_for(lambda: entry_of(
            port, tuple(reseed_saved["tile"]), reseed_saved["instanceId"]))
        rep.check(bool(reseed_entry) and reseed_entry.get("dead") is False
                  and reseed_entry.get("generation") ==
                      reseed_saved["generation"]
                  and reseed_entry.get("age") == reseed_saved["age"],
                  f"wild reseed anchor survives fresh-process load: "
                  f"{reseed_entry}")
        rep.check(not any(e.get("instanceId") == absent_saved["instanceId"]
                          for e in growth(port, tuple(absent_saved["tile"]))),
                  "wild absence tombstone survives fresh-process load")
        persistent_entry = entry_of(port, tuple(persistent_saved["tile"]),
                                    persistent_saved["instanceId"])
        rep.check(bool(persistent_entry) and persistent_entry.get("dead") is True,
                  f"wild persistent corpse survives fresh-process load: "
                  f"{persistent_entry}")
    send(port, f"world.show('{page}'); return 'ok'")
    scenario_exists = send_json(port, f"return world.getDate('{page}')")
    scenario_active = wait_for(lambda: send(
        port, "return world.getActiveWorldId()"
    ).strip().strip('"') == page)
    rep.check(bool(scenario_exists),
              f"the saved scenario page is registered after load: {page}")
    rep.check(bool(scenario_active),
              f"the saved scenario page becomes active after load: {page}")
    send(port, f"return world.loadChunksInRegion(-4,-4,4,4,'{page}')",
         timeout=30)
    send(port, f"return world.waitForChunks(120,'{page}')", timeout=125)
    after = entry_of(port, HERB_TILE, sib_b)
    rep.check(absolute_day() == day_after,
              f"the calendar and successor day survive process restart: "
              f"{absolute_day()}")
    rep.check(bool(scenario_exists) and after is None,
              "the cultivated await-replanting successor stays removed after load")
    rep.check(not any(e.get("instanceId") == absent_id
                      for e in growth(port, ABSENT_TILE)),
              "absence tombstone survives fresh-process load")
    rep.check(bool(entry_of(port, OAK_TILE, oak))
              and is_dead(entry_of(port, OAK_TILE, oak)),
              "persistent corpse survives fresh-process load")
    rep.check(not any(e.get("instanceId") == row
                      for e in growth(port, ROW_TILE)),
              "durable cultivated removal survives fresh-process load")
    rep.check(bool(scenario_exists and row_new)
              and any(e.get("instanceId") == row_new.get("instanceId")
                      for e in growth(port, ROW_TILE)),
              "deliberately replanted row survives fresh-process load")
    rep.check(bool(scenario_exists) and plot_of(port, PLOT_TILE) is not None,
              "deliberately replanted plot survives fresh-process load")
    victim_after = entry_of(port, HERB_TILE, victim)
    rep.check(bool(victim_after) and victim_after.get("dead") is False,
              "the occurrence cleared before the save is still alive")
    return process[0]


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
        process = [proc]
        proc = run_checks(port, art, rep, slot, process, launched)
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
