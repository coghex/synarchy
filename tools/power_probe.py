#!/usr/bin/env python3
"""Power-node placement probe (#358) — the build-tool-driven path, not
just the raw Lua API.

#358 ships the power-node registry (Power.Types), the power.* Lua verbs
(isPlaceable / placeNode / getNode / getNodeForBuilding / listNodes), and
the ACTUAL player-facing placement path: scripts/build_tool.lua's
buildTool.commitPlacement routes a solar_panel / high_voltage_battery
placement through power.placeNode against the currently-selected unit
(consuming the item), while every other building def keeps using the
free building.spawn path unchanged. This probe exercises that routed
path end-to-end, not just the underlying Haskell API, then proves the
result through a save -> quit -> fresh-restart -> load round-trip (the
gold-standard save check, mirroring multiworld_save_probe.py) since a
power node is only "network-attachment-ready" if it actually reconnects
to its building after a reload.

What it does:
  1. Boots a headless engine, loads defs, builds a flat arena, spawns a
     technomule (its starting kit carries the new items — #358).
  2. power.isPlaceable: true for the two power items, false for an
     ordinary building (furnace).
  3. buildTool.commitPlacement with NO unit selected refuses a power-item
     placement (no building appears, inventory untouched) but still
     places an ordinary building for free.
  4. With the technomule selected, commitPlacement places a solar panel
     and a battery — each consumes exactly one matching item and
     registers a node reporting the right role + parameters
     (power.getNode / getNodeForBuilding). A furnace placed the same way
     gets NO power node (the registry doesn't leak to ordinary
     buildings).
  5. Exhausting the mule's remaining solar panels makes the next
     commitPlacement refuse (inventory unaffected).
  6. Save -> quit -> fresh restart -> reload defs -> load: every placed
     building AND its power node survive, reconnected by BuildingId.

Usage: python3 tools/power_probe.py [--port 9358]
Exit 0 = every check passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import shutil
import socket
import subprocess
import sys
import time
import uuid

SAVE_PREFIX = "power_probe_"  # save dirs this probe owns (cleanup scoped to it)


def _results(raw: bytes) -> list[str]:
    out = raw.decode(errors="replace")
    return [ln[2:].strip() for ln in out.splitlines()
            if ln.startswith("> ") and ln[2:].strip()]


def send(port: int, lua: str, timeout: float = 10.0) -> str:
    """Run one Lua line over the debug console and return its result
    (mirrors multiworld_save_probe.py's send — waits the full timeout so
    blocking builtins like world.waitForChunks/waitForInit work)."""
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(timeout)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
                if _results(b"".join(chunks)):
                    break
        except socket.timeout:
            pass
    vals = _results(b"".join(chunks))
    return (vals[-1] if vals else "").strip('"')


def jget(port: int, lua: str, timeout: float = 10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def as_int(s) -> int | None:
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def boot(port: int, logpath: str, tag: str) -> subprocess.Popen:
    log = open(logpath, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT,
    )
    deadline = time.time() + 180
    while time.time() < deadline:
        try:
            if "READY" in open(logpath).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"{tag}: engine exited before READY; see {logpath}")
        time.sleep(0.4)
    proc.kill()
    sys.exit(f"{tag}: engine never printed READY; see {logpath}")


def shutdown(proc: subprocess.Popen, port: int) -> None:
    try:
        send(port, "engine.quit()", timeout=2)
    except OSError:
        pass
    for _ in range(50):
        if proc.poll() is not None:
            break
        time.sleep(0.1)
    if proc.poll() is None:
        proc.kill()
        proc.wait(timeout=5)
    for _ in range(50):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            if s.connect_ex(("localhost", port)) != 0:
                return
        time.sleep(0.1)


def bootstrap_defs(port: int) -> None:
    """Load the defs needed to spawn AND to re-resolve saved buildings on
    load (fromBuildingSnapshot drops any entity whose def isn't
    registered — the load side needs these too)."""
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def wait_active(port: int, page: str, secs: float = 10.0) -> bool:
    deadline = time.time() + secs
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == page:
            return True
        time.sleep(0.2)
    return False


def count_item(port: int, uid: int, name: str) -> int:
    return as_int(send(port,
        f"local c=0; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
        f"if it.defName=='{name}' then c=c+1 end end; return c")) or 0


def check(passed: bool, ok: bool, label: str, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {label}" + (f": {detail}" if detail else ""))
    return passed and ok


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9358)
    args = ap.parse_args()
    port = args.port
    passed = True

    save_name = f"{SAVE_PREFIX}{uuid.uuid4().hex[:12]}"
    save_dir = os.path.join("saves", save_name)
    if os.path.exists(save_dir):
        sys.exit(f"refusing to run: {save_dir} already exists")

    logA = "/tmp/power_probe_A.log"
    logB = "/tmp/power_probe_B.log"
    procA = procB = None
    try:
        # ── Engine A: place nodes through the build tool, save ──────────
        procA = boot(port, logA, "engine A")
        bootstrap_defs(port)
        send(port, "world.initArena('power_probe'); return 'ok'")
        send(port, "world.show('power_probe'); return 'ok'")
        if not wait_active(port, "power_probe"):
            sys.exit("FAIL: power_probe arena never became active")

        uid = as_int(send(port, "return unit.spawn('technomule', 5, 5)"))
        if uid is None:
            sys.exit("FAIL: technomule spawn rejected")
        # starting_inventory materializes asynchronously on the unit
        # thread — wait for it rather than racing the first query.
        for _ in range(50):
            if count_item(port, uid, "solar_panel") >= 2:
                break
            time.sleep(0.1)

        passed = check(passed, count_item(port, uid, "solar_panel") == 2,
                        "mule starting kit carries 2 solar_panel")
        passed = check(passed, count_item(port, uid, "high_voltage_battery") == 2,
                        "mule starting kit carries 2 high_voltage_battery")

        # --- 2. power.isPlaceable ---
        passed = check(passed,
            send(port, "return power.isPlaceable('solar_panel')") == "true",
            "isPlaceable(solar_panel)")
        passed = check(passed,
            send(port, "return power.isPlaceable('high_voltage_battery')") == "true",
            "isPlaceable(high_voltage_battery)")
        passed = check(passed,
            send(port, "return power.isPlaceable('furnace')") == "false",
            "isPlaceable(furnace) is false (ordinary building)")

        # --- 3. commitPlacement with NO unit selected ---
        send(port, "unit.deselectAll(); return 'ok'")
        r = jget(port,
            "local id,err = require('scripts.build_tool').commitPlacement("
            "'solar_panel', 7, 5); return {ok = (id ~= nil), err = err}")
        passed = check(passed, isinstance(r, dict) and r.get("ok") is False,
                       "commitPlacement(solar_panel) refused with no selection", r)
        passed = check(passed, count_item(port, uid, "solar_panel") == 2,
                       "refused placement left the mule's inventory untouched")
        passed = check(passed,
            send(port, "return building.getInfo(1) and 'yes' or 'no'") == "no",
            "no building appeared at (7,5) after the refusal")
        # An ordinary building still places for free with nothing selected.
        fid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'furnace', 20, 20)"))
        passed = check(passed, fid is not None,
                       "commitPlacement(furnace) still places for free "
                       "with no unit selected", fid)

        # --- 4. Select the mule; place a source + a storage node ---
        send(port, f"unit.select({uid}); return 'ok'")
        panel_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'solar_panel', 7, 5)"))
        passed = check(passed, panel_bid is not None,
                       "commitPlacement(solar_panel) with mule selected", panel_bid)
        passed = check(passed, count_item(port, uid, "solar_panel") == 1,
                       "one solar_panel consumed from the mule")
        node = jget(port, f"return power.getNodeForBuilding({panel_bid})")
        passed = check(passed,
            isinstance(node, dict) and node.get("role") == "source"
            and node.get("peakWatts") == 400 and node.get("capacityWh") == 0,
            "solar panel node reports role=source, 400 W, 0 Wh", node)

        batt_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'high_voltage_battery', 8, 5)"))
        passed = check(passed, batt_bid is not None,
                       "commitPlacement(high_voltage_battery) with mule selected",
                       batt_bid)
        passed = check(passed, count_item(port, uid, "high_voltage_battery") == 1,
                       "one high_voltage_battery consumed from the mule")
        node = jget(port, f"return power.getNodeForBuilding({batt_bid})")
        passed = check(passed,
            isinstance(node, dict) and node.get("role") == "storage"
            and node.get("capacityWh") == 5000 and node.get("peakWatts") == 0,
            "battery node reports role=storage, 5000 Wh, 0 W", node)

        # A furnace placed WITH the mule selected still doesn't register a
        # power node — the registry must not leak to ordinary buildings.
        furnace2_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'furnace', 21, 21)"))
        no_node = send(port,
            f"return power.getNodeForBuilding({furnace2_bid}) == nil")
        passed = check(passed, furnace2_bid is not None and no_node == "true",
                       "furnace placed with a unit selected still gets no power node")

        # --- 5. Exhausting inventory refuses further placement ---
        second_panel_bid = as_int(send(port,
            "return require('scripts.build_tool').commitPlacement("
            "'solar_panel', 10, 5)"))
        passed = check(passed, second_panel_bid is not None,
                       "second solar_panel placement (last one in the mule)",
                       second_panel_bid)
        passed = check(passed, count_item(port, uid, "solar_panel") == 0,
                       "mule now carries 0 solar_panel")
        r = jget(port,
            "local id,err = require('scripts.build_tool').commitPlacement("
            "'solar_panel', 11, 5); return {ok = (id ~= nil), err = err}")
        passed = check(passed, isinstance(r, dict) and r.get("ok") is False,
                       "third solar_panel placement refused (mule is empty)", r)

        expected_nodes = 3  # 2 solar panels + 1 battery
        node_count = as_int(send(port, "local ns=power.listNodes(); return #ns"))
        passed = check(passed, node_count == expected_nodes,
                       f"listNodes reports {expected_nodes} nodes", node_count)

        # --- 6. Save -> quit -> fresh restart -> load ---
        saved = send(port, f"return engine.saveWorld('power_probe', '{save_name}')")
        passed = check(passed, saved.strip() == "true",
                       "engine.saveWorld returned true", saved)
        save_file = os.path.join(save_dir, "world.synworld")
        for _ in range(100):
            if os.path.exists(save_file):
                break
            time.sleep(0.1)
        passed = check(passed, os.path.exists(save_file),
                       f"save file appeared at {save_file}")

        shutdown(procA, port)
        procA = None

        procB = boot(port, logB, "engine B")
        bootstrap_defs(port)
        pre = send(port, "return world.getActiveWorldId()")
        loaded = send(port, f"return engine.loadSave('{save_name}')")
        passed = check(passed, loaded.strip() == "true",
                       "engine.loadSave returned true (pre-load active: "
                       f"{pre})", loaded)
        send(port, "return world.waitForInit(180)", timeout=190)
        time.sleep(2)
        send(port, "world.show('main_world'); return 'ok'")
        wait_active(port, "main_world")

        for bid, want_def, want_role, want_peak, want_cap in [
            (panel_bid, "solar_panel", "source", 400, 0),
            (batt_bid, "high_voltage_battery", "storage", 0, 5000),
            (second_panel_bid, "solar_panel", "source", 400, 0),
        ]:
            info = jget(port, f"return building.getInfo({bid})")
            passed = check(passed,
                isinstance(info, dict) and info.get("defName") == want_def,
                f"building #{bid} ({want_def}) survived the reload", info)
            node = jget(port, f"return power.getNodeForBuilding({bid})")
            passed = check(passed,
                isinstance(node, dict) and node.get("role") == want_role
                and node.get("peakWatts") == want_peak
                and node.get("capacityWh") == want_cap,
                f"building #{bid}'s power node survived with role/params intact",
                node)

        furnace_info = jget(port, f"return building.getInfo({fid})")
        passed = check(passed,
            isinstance(furnace_info, dict) and furnace_info.get("defName") == "furnace",
            "furnace building survived the reload too", furnace_info)
        no_node_after_load = send(port,
            f"return power.getNodeForBuilding({fid}) == nil")
        passed = check(passed, no_node_after_load == "true",
                       "furnace still has no power node after reload")

        node_count_after = as_int(send(port, "local ns=power.listNodes(); return #ns"))
        passed = check(passed, node_count_after == expected_nodes,
                       f"listNodes still reports {expected_nodes} nodes after reload",
                       node_count_after)

        print("\n" + ("ALL POWER CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        if procA is not None:
            shutdown(procA, port)
        if procB is not None:
            shutdown(procB, port)
        if os.path.exists(save_dir):
            shutil.rmtree(save_dir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
