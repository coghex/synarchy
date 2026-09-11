#!/usr/bin/env python3
"""Fresh-process durability probe for the lava-water solidification
product (#2485, FR-2 of epic #2480).

The hspec group ``--match "solidification"`` proves the commit in
process: an admitted reaction result appends a ``WeAddTile`` of the
chosen stone, and replaying that log over the freshly generated chunk
reproduces the height and the material. What it structurally CANNOT
prove is that the stone survives leaving the process -- the whole point
of routing the product through the edit log rather than a fluid
writeback, which is replaced in memory and appended to nothing.

So this drives the REAL reaction end to end and then terminates the
engine:

  1. Engine A: boot headless, load the material definitions, ``world.init``
     a small (worldSize 8) REAL GENERATED page and show it. Not an arena:
     ``docs/headless_console.md`` NB #365 records that loading a save
     containing an arena page hangs the world thread, so a durability
     probe that saved an arena could never complete step 3.
  2. Find a flat, dry, adjacent pair of loaded land tiles, put lava on
     one and ocean on the other with ``world.setFluidTile``, and wait for
     the lava column's TERRAIN top to rise -- which only the world
     thread's reaction commit can do, since neither edit raises terrain
     and a fluid writeback cannot. Record the height and the product
     material.
  3. Save, quit, boot a fresh engine, load, and assert the same height
     and the same material at the same tile.

Registered CI-eligible in ``tools/ci_probes.py``.

Usage:
  python3 tools/fluid_reaction_probe.py [--port 9281]
"""
from __future__ import annotations

import argparse
import glob
import os
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, quit_engine, send, capture_request_id,
                      wait_load_published, wait_save_complete)

REPO = Path(__file__).resolve().parent.parent

SEED = 771
WORLD_SIZE = 8
PLATE_COUNT = 3
PAGE = "fluid_reaction_page"
SLOT = "fluid_reaction_probe"
# Products #2485 can choose between. Both are authored in
# data/materials/igneous_extrusive.yaml; this slice adds neither.
PRODUCTS = ("basalt", "obsidian")
# How long the reaction has to reach the world thread: two live edits,
# the sim's own activation and settle ticks at 10 Hz, one contact, and
# the commit that follows it.
REACTION_TIMEOUT = 60.0


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real read-only content symlinked in,
    plus its OWN empty saves/ -- this probe never touches the
    developer's real saves/ (the pattern tools/save_storage_probe.py and
    tools/persistence_contract_probe.py both use)."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap_materials(port: int) -> None:
    """The product material is resolved through the REGISTRY, so a boot
    that never loaded the material YAML would drop the stone loudly and
    this probe would (correctly) fail for the wrong reason."""
    for path in sorted(glob.glob("data/materials/*.yaml")):
        send(port, f"engine.loadMaterialYaml('{path}'); return 'ok'")


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


def terrain_at(port: int, gx: int, gy: int):
    """The column's TERRAIN top (not the rendered surface, which folds
    in fluid). ``nil`` for an unloaded chunk."""
    return as_int(send(
        port,
        f"local s, t = world.getTerrainAt({gx}, {gy}, '{PAGE}'); return t"))


def surface_at(port: int, gx: int, gy: int):
    return as_int(send(
        port,
        f"local s, t = world.getTerrainAt({gx}, {gy}, '{PAGE}'); return s"))


def material_at(port: int, gx: int, gy: int) -> str:
    """The material name at the column's terrain top."""
    raw = send(port,
               f"local i, n = world.getMaterialAt({gx}, {gy}, '{PAGE}'); "
               f"return n")
    return raw.strip().strip('"')


def alias_step(port: int):
    """How far a u-alias of a tile coordinate sits from its canonical
    image, in tiles.

    Derived from the engine's OWN reported wrap width rather than
    recomputed here, so a change to that convention fails this probe
    instead of quietly leaving it testing a coordinate that is not an
    alias at all. One whole world in u is `worldWidthTiles`, and the
    alias preserves v = gx + gy, so each axis moves by half of it.
    """
    width = as_int(send(port, f"return world.getWrapWidth('{PAGE}')"))
    if width is None or width <= 0 or width % 2 != 0:
        return None
    return width // 2


def find_contact_pair(port: int):
    """A dry, above-sea-level pair of horizontally adjacent land tiles,
    with the one-z STEP between them BUILT rather than searched for.

    The step is what makes the contact happen at all rather than happen
    to. ``world.setFluidTile`` gives every cell the same single level of
    depth, so a FLAT pair leaves both surfaces equal and the lateral
    phase — which only moves fluid down a surface gradient — plans
    nothing; the two sit side by side indefinitely unless the surrounding
    terrain happens to drain one of them. With the lava side one z higher
    its surface stands above the water's and gravity carries it down into
    it, which is the contact.

    A generated page is not guaranteed to contain a suitable natural
    step anywhere near the camera, so one ordinary ``world.addTile`` —
    the same player edit the debug terrain tool makes — supplies it. That
    edit lands BEFORE any capture, so nothing it changes is attributed to
    the reaction.

    Dry, so each fluid edit genuinely places the fluid it names rather
    than replacing a cell the page already had; above sea level, so the
    page's own ocean is not what is reacting.
    """
    for gy in range(-12, 13):
        for gx in range(-12, 12):
            ta = terrain_at(port, gx, gy)
            tb = terrain_at(port, gx + 1, gy)
            if ta is None or tb is None or ta != tb or ta <= 0:
                continue
            if surface_at(port, gx, gy) != ta:
                continue
            if surface_at(port, gx + 1, gy) != tb:
                continue
            return (gx, gy), (gx + 1, gy), ta
    return None, None, None


def raise_lava_side(chk: Checks, port: int, tile, baseline: int):
    """One ordinary add-tile, so the lava sits a z above the water."""
    send(port, f"world.addTile('{PAGE}', {tile[0]}, {tile[1]}, 'loam'); "
               f"return 'ok'")
    deadline = time.time() + 20.0
    top = baseline
    while time.time() < deadline:
        top = terrain_at(port, *tile)
        if top is not None and top > baseline:
            break
        time.sleep(0.2)
    chk.ok(top == baseline + 1,
           f"built the one-z step the contact needs: {tile} rose from "
           f"{baseline} to {top}")
    return top


def wait_for_stone(port: int, tile, baseline: int, seconds: float):
    """Poll the lava column's terrain top until it RISES.

    Only a committed ``WeAddTile`` can move it: ``world.setFluidTile``
    edits fluid alone, and a fluid writeback replaces a chunk's
    sim-owned fields without ever touching the column's materials.
    """
    gx, gy = tile
    deadline = time.time() + seconds
    while time.time() < deadline:
        top = terrain_at(port, gx, gy)
        if top is not None and top > baseline:
            return top
        time.sleep(0.25)
    return None


def build_and_react(chk: Checks, port: int):
    """Engine A's whole scenario. Returns (tile, height, material)."""
    bootstrap_materials(port)
    send(port,
         f"world.init('{PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
         f"'Fluid Reaction Probe', 'the lava-water reaction probe world'); "
         f"return 'ok'")
    send(port, f"world.show('{PAGE}'); return 'ok'")
    send(port, "return world.waitForInit(120)", timeout=130)
    active = send(port, "return world.getActiveWorldId()").strip().strip('"')
    chk.ok(active == PAGE, f"'{PAGE}' is the active world (got {active!r})")

    lava_tile, water_tile, baseline = find_contact_pair(port)
    chk.ok(lava_tile is not None,
           f"found a flat dry land pair to react on (lava {lava_tile}, "
           f"water {water_tile}, terrain top {baseline})")
    if lava_tile is None:
        return None, None, None

    baseline = raise_lava_side(chk, port, lava_tile, baseline)
    before_mat = material_at(port, *lava_tile)
    chk.ok(before_mat not in PRODUCTS,
           f"the lava column is not ALREADY made of a reaction product "
           f"(got {before_mat!r}) -- otherwise 'the stone appeared' would "
           f"be unfalsifiable")

    # Ocean on the water side, so D-5's first clause decides the product
    # without depending on where generation put sea level under this
    # particular seed.
    send(port, f"world.setFluidTile('{PAGE}', {water_tile[0]}, "
               f"{water_tile[1]}, 'ocean'); return 'ok'")
    send(port, f"world.setFluidTile('{PAGE}', {lava_tile[0]}, "
               f"{lava_tile[1]}, 'lava'); return 'ok'")

    height = wait_for_stone(port, lava_tile, baseline, REACTION_TIMEOUT)
    chk.ok(height is not None,
           f"the contact solidified: the lava column's terrain top rose "
           f"from {baseline} to {height}")
    if height is None:
        return lava_tile, None, None
    chk.ok(height == baseline + 1,
           f"the stone raised the column by exactly one z "
           f"(got {height}, expected {baseline + 1})")

    material = material_at(port, *lava_tile)
    chk.ok(material in PRODUCTS,
           f"the new top is one of the authored reaction products "
           f"(got {material!r})")
    chk.ok(material == "basalt",
           f"an OCEAN contact chose basalt, as D-5's first clause says "
           f"(got {material!r})")

    # The query is a POINT query, so it accepts a seam ALIAS and answers
    # about the tile the page actually stores (CLAUDE.md
    # SSTile coordinates). Without canonicalization an alias resolves to
    # a chunk key nothing is stored under and reports nil about a tile
    # that is right there.
    step = alias_step(port)
    chk.ok(step is not None,
           f"the page reports a usable u-wrap width (alias step {step})")
    if step is not None:
        aliased = material_at(port, lava_tile[0] + step, lava_tile[1] - step)
        chk.ok(aliased == material,
               f"a u-ALIAS of the stone's coordinate answers with the same "
               f"material (got {aliased!r}, expected {material!r})")
        # Only the material query is asserted through the alias:
        # `world.getTerrainAt` beside it has never canonicalized, and
        # changing that is not this slice's to make.
    return lava_tile, height, material


def save_and_wait(chk: Checks, port: int) -> None:
    saved = send(port, f"return engine.saveWorld('{PAGE}', '{SLOT}')")
    chk.ok(saved.strip() == "true",
           f"engine.saveWorld('{SLOT}') accepted (got {saved!r})")
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    chk.ok(request_id is not None,
           "engine.getSaveStatus() reports a request id right after saveWorld")
    ok, final_status = wait_save_complete(port, request_id)
    chk.ok(ok, f"the save reached SaveCaptureComplete (got {final_status!r})")


def reload_and_verify(chk: Checks, port: int, tile, height, material) -> None:
    """Engine B: a genuinely fresh process, which is the whole point."""
    bootstrap_materials(port)
    accepted = send(port, f"return engine.loadSave('{SLOT}')")
    chk.ok(accepted.strip() == "true",
           f"engine.loadSave('{SLOT}') accepted (got {accepted!r})")
    request_id = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, request_id=request_id)
    chk.ok(published, f"the load reached LoadPublished (got {status!r})")
    if not published:
        return
    send(port, f"world.show('{PAGE}'); return 'ok'")
    send(port, "return world.waitForInit(120)", timeout=130)

    reloaded_height = terrain_at(port, *tile)
    chk.ok(reloaded_height == height,
           f"the stone's column height survived the fresh-process load "
           f"(got {reloaded_height}, expected {height})")
    reloaded_material = material_at(port, *tile)
    chk.ok(reloaded_material == material,
           f"the stone's product material survived the fresh-process load "
           f"(got {reloaded_material!r}, expected {material!r})")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9281)
    args = ap.parse_args()
    port = args.port

    chk = Checks()
    with tempfile.TemporaryDirectory(prefix="fluid_reaction_probe_") as base:
        root = make_isolated_root(base)
        log_a = os.path.join(base, "engine_a.log")
        log_b = os.path.join(base, "engine_b.log")

        print("== engine A: generate, react, record, save ==")
        proc = boot(port, log=log_a, args=["--resource-root", root],
                    ready_timeout=180)
        try:
            tile, height, material = build_and_react(chk, port)
            if tile is not None and height is not None:
                save_and_wait(chk, port)
        finally:
            quit_engine(port, proc)

        if tile is None or height is None:
            print("\nFAILED: the reaction never produced stone; "
                  "nothing to reload.")
            return 1

        print("== engine B: fresh process, load, verify ==")
        proc = boot(port, log=log_b, args=["--resource-root", root],
                    ready_timeout=180)
        try:
            reload_and_verify(chk, port, tile, height, material)
        finally:
            quit_engine(port, proc)

    print()
    if chk.failed:
        print(f"FAILED: {chk.failed} check(s)")
        return 1
    print("PASSED: the reaction product is durable across a fresh process")
    return 0


if __name__ == "__main__":
    sys.exit(main())
