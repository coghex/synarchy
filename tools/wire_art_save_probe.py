#!/usr/bin/env python3
"""Wire art save-compatibility probe (#2516 requirement 6).

Proves a save written BEFORE the Wire art change keeps working after it:
existing Wire, including a piece in an EVICTED chunk and a paid
in-progress construction site, draws with the new per-appearance
facemaps and keeps its gameplay state, with no manual replacement.

Two phases, run by different binaries:

  make     Build a fixture save with whatever engine this checkout builds.
           For the committed fixture it was run from a binary built at
           master 047832b6b (before the Wire art change; old wire.yaml
           and the shared floorface.png facemap on every Wire piece):

             SYNARCHY_PROBE_ENGINE_EXE=<pre-change synarchy> \\
               python3 tools/wire_art_save_probe.py make \\
               --out <fixture dir> --port 9516

           The world is real (seed 42, worldSize 64) so chunk eviction is
           real: a solar panel and a battery joined by a five-tile Wire
           run inside chunk (2,-1); a paid wire site at 50 % progress in
           the same chunk; the camera is then driven through fresh ground
           until that chunk is genuinely evicted, a second two-tile run is
           laid where the camera stopped, and the page is saved.

  verify   In a FRESH process load the fixture, then check:
             * the saved Wire still names its legacy facemap (proving the
               fixture really is pre-change), while the facemap the
               renderer draws it with (structure.drawnFacemap, the render
               pass's own resolver) is that appearance's dedicated map,
               for the resident run AND for the run in the evicted chunk
               once the camera brings it back;
             * network membership and battery charge are unchanged;
             * the in-progress site is still paid, at the same progress,
               with its receipt.
           then RESAVE, restart and run the same checks again, which is
           the "persists across another round trip" half.

             python3 tools/wire_art_save_probe.py verify \\
               --fixture <fixture dir> --port 9517

Needs no GPU. Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import sys
import tempfile
import time
import uuid
from pathlib import Path

from probelib import boot, quit_engine, send, send_json, wait_load_published

REPO = Path(__file__).resolve().parent.parent

PAGE = "wire_art_fixture"
SEED, SIZE, PLATES = 42, 64, 4
CHUNK = (2, -1)
PANEL_TILE = (36, -8)
BATT_TILE = (40, -8)
WIRE_RUN = [(36, -7), (37, -7), (38, -7), (39, -7), (40, -7)]
SITE_TILE = (44, -7)
HOME_TILE = (38, -8)
FILL_REGION = (-7, 10, 7, 24)
CAMERA_STOPS = [(cx * 16 + 8, -8) for cx in (10, 13, 16, 19, 22, 25)]
LEGACY_FACEMAP = "assets/textures/facemap/floorface.png"

RESULTS: list[bool] = []


def check(ok: bool, label: str, detail="") -> bool:
    RESULTS.append(bool(ok))
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}"
          + (f": {detail}" if detail != "" else ""))
    return bool(ok)


def as_int(s):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def make_isolated_root(base: str) -> str:
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap_defs(port: int) -> None:
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml", "engine.loadItemYaml"),
        ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml", "engine.loadMaterialYaml"),
        ("data/factions/*.yaml", "engine.loadFactionYaml"),
        ("data/physiology/*.yaml", "engine.loadPhysiologyYaml"),
        ("data/units/*.yaml", "engine.loadUnitYaml"),
        ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
    ]:
        for path in sorted(glob.glob(os.path.join(REPO, pattern))):
            rel = os.path.relpath(path, REPO)
            send(port, f"{fn}('{rel}'); return 'ok'")


def wait_active(port: int, page: str, secs: float = 20.0) -> bool:
    deadline = time.time() + secs
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == page:
            return True
        time.sleep(0.2)
    return False


def chunk_loaded(port: int, coord) -> bool | None:
    info = send_json(port, f"return world.getChunkInfo({coord[0]}, {coord[1]})")
    return bool(info.get("loaded")) if isinstance(info, dict) else None


def force_eviction(port: int, coord) -> int | None:
    for idx, (gx, gy) in enumerate(CAMERA_STOPS):
        send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
        deadline = time.time() + 25.0
        while time.time() < deadline:
            if chunk_loaded(port, coord) is False:
                return idx
            time.sleep(0.25)
    return None


def go_home(port: int, coord, tile, secs: float = 40.0) -> bool:
    send(port, f"camera.goToTile({tile[0]}, {tile[1]}); return 'ok'")
    deadline = time.time() + secs
    while time.time() < deadline:
        if chunk_loaded(port, coord) is True:
            return True
        time.sleep(0.25)
    return False


def wire_at(port: int, x: int, y: int):
    return send_json(port, f"return structure.getAt({x}, {y}, 'wire')")


def shape_of(piece) -> str | None:
    if not isinstance(piece, dict) or "tex" not in piece:
        return None
    return piece["tex"].rsplit("/", 1)[-1].removesuffix(".png")


def design_at(port: int, x: int, y: int):
    return send_json(port, "return construction.getDesignationAt("
                           f"world.getActiveWorldId(), {x}, {y})")


def battery_wh(port: int, bid: int):
    node = send_json(port, f"return power.getNodeForBuilding({bid})")
    return float(node["storedWh"]) if isinstance(node, dict) else None


def net_members(port: int, nid: int):
    net = send_json(port, f"return power.getNetworkForNode({nid})")
    return sorted(net.get("nodeIds", [])) if isinstance(net, dict) else None


# -- make --------------------------------------------------------------

def cmd_make(args) -> int:
    tmp = tempfile.mkdtemp(prefix="wire_art_make_")
    proc = None
    try:
        root = make_isolated_root(tmp)
        port = args.port
        proc = boot(port, log="/tmp/wire_art_save_make.log",
                    label="make engine", args=["--resource-root", root])
        bootstrap_defs(port)
        send(port, f"world.init('{PAGE}', {SEED}, {SIZE}, {PLATES}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, f"world.show('{PAGE}'); return 'ok'")
        if not wait_active(port, PAGE):
            sys.exit(f"FAIL: {PAGE} never became active")
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
        send(port, f"world.setTime('{PAGE}', 12, 0); return 'ok'")
        send(port, "engine.setPaused(false); return 'ok'")

        mule = as_int(send(port, f"return unit.spawn('technomule', "
                                 f"{PANEL_TILE[0]}, {PANEL_TILE[1] - 1})"))
        if mule is None or mule < 0:
            sys.exit("FAIL: technomule spawn rejected")
        for _ in range(60):
            n = send(port, f"local c=0; for _,it in ipairs(unit.getInventory({mule}) or {{}}) "
                           "do if it.defName=='solar_panel' then c=c+1 end end; return c")
            if as_int(n) and as_int(n) >= 1:
                break
            time.sleep(0.1)
        send(port, f"unit.select({mule}); return 'ok'")
        panel = as_int(send(port, "return require('scripts.build_tool').commitPlacement("
                                  f"'solar_panel', {PANEL_TILE[0]}, {PANEL_TILE[1]})"))
        batt = as_int(send(port, "return require('scripts.build_tool').commitPlacement("
                                 f"'high_voltage_battery', {BATT_TILE[0]}, {BATT_TILE[1]})"))
        if panel is None or batt is None:
            sys.exit(f"FAIL: node placement rejected ({panel}, {batt})")
        for gx, gy in WIRE_RUN:
            send(port, f"require('scripts.wire').place({gx}, {gy}); return 'ok'")
        runA = {f"{x},{y}": shape_of(wire_at(port, x, y)) for x, y in WIRE_RUN}
        check(all(runA.values()), "five-tile Wire run placed", runA)

        # A paid wire site at 50 %: designate, pay from the mule's wiring,
        # add progress, and take the builder away so nothing finishes it.
        w = send(port, "return world.getActiveWorldId()").strip('"')
        sx, sy = SITE_TILE
        send(port, f"construction.designate('{w}', {sx}, {sy}, {sx}, {sy}, "
                   "'structure', 'wire', 'wire'); return 'ok'")
        time.sleep(0.5)
        job = design_at(port, sx, sy)
        check(isinstance(job, dict) and "attempt" in job,
              "wire site designated", job)
        attempt = int(job["attempt"])
        paid = send(port, f"return tostring(construction.payMaterials('{w}', "
                          f"{sx}, {sy}, {attempt}, {mule}))").strip('"')
        check(paid == "true", "wire site paid from the mule's wiring", paid)
        send(port, f"construction.addJobProgress('{w}', {sx}, {sy}, 0.5, "
                   f"{attempt}); return 'ok'")
        send(port, f"unit.destroy({mule}); return 'ok'")
        time.sleep(1.0)
        site = design_at(port, sx, sy)
        check(isinstance(site, dict) and site.get("paid") is True
              and abs(float(site.get("progress", 0)) - 0.5) < 1e-6,
              "site is paid at progress 0.5 and still unbuilt", site)

        panel_node = send_json(port, f"return power.getNodeForBuilding({panel})")
        batt_node = send_json(port, f"return power.getNodeForBuilding({batt})")
        pid, bid = as_int(panel_node["id"]), as_int(batt_node["id"])
        members = net_members(port, pid)
        check(members == sorted([pid, bid]),
              "panel and battery share one network", members)

        send(port, f"world.setTimeScale('{PAGE}', 120); return 'ok'")
        time.sleep(2.0)
        send(port, f"world.setTimeScale('{PAGE}', 0); return 'ok'")
        stored = battery_wh(port, batt)
        check(stored is not None and stored > 0,
              "battery holds charge before the save", stored)

        send(port, f"return world.loadChunksInRegion({FILL_REGION[0]}, "
                   f"{FILL_REGION[1]}, {FILL_REGION[2]}, {FILL_REGION[3]})",
             timeout=30)
        send(port, "return world.waitForChunks(300)", timeout=310)
        stop = force_eviction(port, CHUNK)
        check(stop is not None and chunk_loaded(port, CHUNK) is False,
              "the wire chunk is genuinely evicted", chunk_loaded(port, CHUNK))
        if stop is None:
            sys.exit("FAIL: never evicted the wire chunk")

        # A resident run where the camera stopped.
        cx, cy = CAMERA_STOPS[stop]
        resident = [(cx, cy), (cx + 1, cy)]
        for gx, gy in resident:
            send(port, f"require('scripts.wire').place({gx}, {gy}); return 'ok'")
        runB = {f"{x},{y}": shape_of(wire_at(port, x, y)) for x, y in resident}
        check(all(runB.values()), "two-tile resident Wire run placed", runB)

        name = f"wire_art_pre_{uuid.uuid4().hex[:8]}"
        saved = send(port, f"return engine.saveWorld('{PAGE}', '{name}')")
        check(saved.strip() == "true", "engine.saveWorld accepted", saved)
        sfile = os.path.join(root, "saves", name, "world.synworld")
        for _ in range(300):
            if os.path.exists(sfile):
                break
            time.sleep(0.1)
        check(os.path.exists(sfile), "save file written", sfile)
        time.sleep(3.0)

        out = Path(args.out)
        if out.exists():
            shutil.rmtree(out)
        out.mkdir(parents=True)
        shutil.copytree(os.path.join(root, "saves", name), out / "save")
        meta = {
            "saveName": name, "page": PAGE, "seed": SEED, "size": SIZE,
            "plates": PLATES, "chunk": list(CHUNK),
            "panelBuilding": panel, "batteryBuilding": batt,
            "panelNode": pid, "batteryNode": bid,
            "networkMembers": members, "batteryWh": stored,
            "evictedRun": [list(t) for t in WIRE_RUN],
            "evictedShapes": runA,
            "residentRun": [list(t) for t in resident],
            "residentShapes": runB,
            "site": {"tile": list(SITE_TILE), "progress": 0.5,
                     "attempt": attempt},
            "homeTile": list(HOME_TILE),
        }
        (out / "fixture.json").write_text(json.dumps(meta, indent=2) + "\n")
        print(f"fixture written to {out}")
        return 0 if all(RESULTS) else 1
    finally:
        if proc is not None:
            quit_engine(args.port, proc)
        shutil.rmtree(tmp, ignore_errors=True)


# -- verify ------------------------------------------------------------

def drawn_facemap(port: int, x: int, y: int):
    raw = send(port, f"return structure.drawnFacemap({x}, {y}, 'wire')").strip()
    return raw.strip('"') if raw not in ("nil", "null", "") else None


def expected_face(shape: str) -> str:
    return f"assets/textures/facemap/wire/{shape}.png"


def verify_once(port: int, root: str, fx: dict, label: str,
                expect_legacy_stored: bool) -> dict:
    """Boot a fresh engine on `root`, load the fixture's save, run every
    check, and return facts the next round compares against."""
    print(f"\n== {label} ==")
    proc = boot(port, log=f"/tmp/wire_art_save_{label.replace(' ', '_')}.log",
                label=label, args=["--resource-root", root])
    facts: dict = {}
    try:
        bootstrap_defs(port)
        loaded = send(port, f"return engine.loadSave('{fx['_name']}')")
        check(loaded.strip() == "true", "engine.loadSave accepted", loaded)
        published, status = wait_load_published(port, 300)
        check(published, "load transaction published", status)
        send(port, f"world.show('{fx['page']}'); return 'ok'")
        check(wait_active(port, fx["page"]), "page active after the load")
        # The clock is frozen for the whole check: a running clock would
        # charge the battery between the save and the comparison.
        send(port, f"world.setTimeScale('{fx['page']}', 0); return 'ok'")

        chunk = tuple(fx["chunk"])
        if label.endswith("A"):
            # Only the first load is from a save taken with the chunk
            # evicted; the resave below is taken at home, with it resident.
            check(chunk_loaded(port, chunk) is False,
                  "the Wire chunk is NOT loaded in the fresh process",
                  chunk_loaded(port, chunk))

        # The resident run first: its chunk is whatever the camera held.
        rx, ry = fx["residentRun"][0]
        send(port, f"camera.goToTile({rx}, {ry}); return 'ok'")
        time.sleep(2.0)
        for (x, y) in fx["residentRun"]:
            piece = wire_at(port, x, y)
            shape = shape_of(piece)
            check(shape == fx["residentShapes"][f"{x},{y}"],
                  f"resident wire ({x},{y}) keeps shape {shape}", piece)
            if expect_legacy_stored:
                check(isinstance(piece, dict)
                      and piece.get("face") == LEGACY_FACEMAP,
                      f"resident wire ({x},{y}) still STORES the legacy facemap",
                      piece.get("face") if isinstance(piece, dict) else piece)
            drawn = drawn_facemap(port, x, y)
            check(drawn == expected_face(shape),
                  f"resident wire ({x},{y}) is DRAWN with {expected_face(shape)}",
                  drawn)

        # Bring the evicted chunk back: pieces regenerate from the edit log.
        hx, hy = fx["homeTile"]
        check(go_home(port, chunk, (hx, hy)), "the evicted Wire chunk reloads")
        for (x, y) in fx["evictedRun"]:
            piece = wire_at(port, x, y)
            shape = shape_of(piece)
            check(shape == fx["evictedShapes"][f"{x},{y}"],
                  f"evicted-chunk wire ({x},{y}) keeps shape {shape}", piece)
            if expect_legacy_stored:
                check(isinstance(piece, dict)
                      and piece.get("face") == LEGACY_FACEMAP,
                      f"evicted-chunk wire ({x},{y}) still STORES the legacy "
                      "facemap",
                      piece.get("face") if isinstance(piece, dict) else piece)
            drawn = drawn_facemap(port, x, y)
            check(drawn == expected_face(shape),
                  f"evicted-chunk wire ({x},{y}) is DRAWN with "
                  f"{expected_face(shape)}", drawn)

        pid, bid = fx["panelNode"], fx["batteryNode"]
        members = net_members(port, pid)
        check(members == fx["networkMembers"],
              "network membership unchanged", members)
        stored = battery_wh(port, fx["batteryBuilding"])
        check(stored is not None and abs(stored - fx["batteryWh"]) < 1e-3,
              "battery charge unchanged", f"{fx['batteryWh']} -> {stored}")
        sx, sy = fx["site"]["tile"]
        site = design_at(port, sx, sy)
        check(isinstance(site, dict) and site.get("paid") is True
              and abs(float(site.get("progress", -1)) - fx["site"]["progress"]) < 1e-6,
              "in-progress site is still paid at the same progress", site)
        check(isinstance(site, dict) and bool(site.get("receipt")),
              "the site keeps its material receipt",
              site.get("receipt") if isinstance(site, dict) else None)
        facts["site"] = site

        if label.endswith("A"):
            name2 = f"wire_art_resave_{uuid.uuid4().hex[:8]}"
            saved = send(port, f"return engine.saveWorld('{fx['page']}', '{name2}')")
            check(saved.strip() == "true", "resave accepted", saved)
            sfile = os.path.join(root, "saves", name2, "world.synworld")
            for _ in range(300):
                if os.path.exists(sfile):
                    break
                time.sleep(0.1)
            check(os.path.exists(sfile), "resave written")
            time.sleep(3.0)
            facts["resave"] = name2
        return facts
    finally:
        quit_engine(port, proc)


def cmd_verify(args) -> int:
    fixture = Path(args.fixture)
    fx = json.loads((fixture / "fixture.json").read_text())
    fx["_name"] = fx["saveName"]
    tmp = tempfile.mkdtemp(prefix="wire_art_verify_")
    try:
        root = make_isolated_root(tmp)
        shutil.copytree(fixture / "save", os.path.join(root, "saves", fx["saveName"]))
        a = verify_once(args.port, root, fx, "engine A", expect_legacy_stored=True)
        resave = a.get("resave")
        if resave:
            fx2 = dict(fx, _name=resave)
            # The resaved world keeps every piece's stored ids, so it
            # still names the legacy facemap; what must persist is the
            # DRAWN map, the gameplay state and the receipt.
            verify_once(args.port, root, fx2, "engine B",
                        expect_legacy_stored=True)
        print("\n" + ("ALL WIRE ART SAVE CHECKS PASSED" if all(RESULTS)
                      else "SOME FAILED"))
        return 0 if all(RESULTS) else 1
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    mk = sub.add_parser("make")
    mk.add_argument("--out", required=True)
    mk.add_argument("--port", type=int, default=9516)
    vf = sub.add_parser("verify")
    vf.add_argument("--fixture", required=True)
    vf.add_argument("--port", type=int, default=9517)
    args = ap.parse_args()
    return cmd_make(args) if args.cmd == "make" else cmd_verify(args)


if __name__ == "__main__":
    sys.exit(main())
