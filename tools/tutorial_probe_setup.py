#!/usr/bin/env python3
"""Content, script and world setup — plus the shared save/load barrier —
for `tools/tutorial_probe.py` (#2145).

Everything a booted engine needs before a scenario can start: the YAML
loader declarations, the AI + tutorial script stack at the z-orders
scripts/init_loader.lua uses, pause control, and the generated-world
initialization every leg with a world performs.

It also owns the two save/load barrier helpers. They are session
operations rather than content, but both stage owners save and both
reload, and a copy of the request-id/barrier dance in each stage module
is exactly the duplication requirement 20 forbids — so it is
single-sourced here, one layer below both of them.

This module BOOTS NOTHING. `probelib.boot` and `probelib.quit_engine`
are called only by the facade; everything here runs against a port an
engine is already listening on.
"""
from __future__ import annotations

import glob
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (send, capture_request_id, wait_load_published,
                      wait_save_complete)
from tutorial_probe_contracts import (PAGE, REPO_ROOT, Checks, ProbeError)

YAML_LOADERS = [
    ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
    ("data/infections/*.yaml", "engine.loadInfectionYaml"),
    ("data/items/*.yaml", "engine.loadItemYaml"),
    ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
    ("data/materials/*.yaml", "engine.loadMaterialYaml"),
    ("data/flora/*.yaml", "engine.loadFloraYaml"),
    ("data/units/*.yaml", "engine.loadUnitYaml"),
    ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
]

#: The AI stack plus the tutorial runtime. scripts/init_loader.lua loads
#: the tutorial trio at these same z-orders in a real session; headless
#: has no loading screen, so the probe reproduces it.
SCRIPTS = [
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
    ("scripts/tutorial_progress.lua", 1.0),
    ("scripts/tutorial_eval.lua", 1.0),
    ("scripts/tutorial_hud.lua", 0.2),
]


# --------------------------------------------------------------------------
# Engine bootstrap
# --------------------------------------------------------------------------
def load_yaml_dir(port: int, pattern: str, fn: str) -> None:
    for path in sorted(glob.glob(os.path.join(REPO_ROOT, pattern))):
        rel = os.path.relpath(path, REPO_ROOT)
        send(port, f"{fn}('{rel}'); return 'ok'", timeout=20.0)


def load_content(port: int) -> None:
    for pattern, fn in YAML_LOADERS:
        load_yaml_dir(port, pattern, fn)
    got = send(port, "return engine.loadTutorialDir('data/tutorials')", timeout=20.0)
    if got.strip() in ("", "nil", "false"):
        raise ProbeError(f"engine.loadTutorialDir failed: {got!r}")


def load_scripts(port: int) -> None:
    for path, z in SCRIPTS:
        send(port, f"engine.loadScript('{path}', {z}); return 'ok'", timeout=20.0)
    # The tree is fetched lazily on first use; resolve it now so a
    # content failure reports here instead of as a mystery empty panel.
    tree = send(port,
                "local t = require('scripts.tutorial_progress').ensureTree(); "
                "return t and t.id or 'nil'", timeout=15.0)
    if tree != "first_session":
        raise ProbeError(f"expected the first_session tree, got {tree!r}")


def set_paused(port: int, on: bool) -> None:
    send(port, f"engine.setPaused({'true' if on else 'false'}); "
               f"return tostring(engine.isPaused())", timeout=10.0)


def generate_world(port: int, seed: int, size: int) -> None:
    send(port, f"world.init('{PAGE}', {seed}, {size}, 3); return 'ok'",
         expect_result=False)
    send(port, "return world.waitForInit(300)", timeout=310.0)
    send(port, f"world.show('{PAGE}'); return 'ok'", expect_result=False)
    send(port, "return world.loadChunksInRegion(-3,-3,3,3)", timeout=30.0)
    send(port, "return world.waitForChunks(120)", timeout=130.0)


def prepare_generated_session(port: int, seed: int, size: int) -> None:
    """A fresh engine carried to "paused, with a generated world and the
    tutorial runtime live" — what both world-generating legs open with."""
    load_content(port)
    generate_world(port, seed, size)
    load_scripts(port)
    set_paused(port, True)


def prepare_reload_session(port: int) -> None:
    """A fresh engine carried to "ready to load a save" — content and
    scripts, no world of its own. Both reload legs open with this."""
    load_content(port)
    load_scripts(port)


# --------------------------------------------------------------------------
# The real save/load barrier, shared by both round trips
# --------------------------------------------------------------------------
def save_through_barrier(port: int, checks: Checks, slot: str,
                         label: str) -> None:
    """Take a real save of `PAGE` into `slot` and wait for the write
    barrier to report it complete."""
    accepted = send(port, f"return engine.saveWorld('{PAGE}', '{slot}')",
                    timeout=30.0)
    if accepted != "true":
        raise ProbeError(f"engine.saveWorld was not accepted: {accepted!r}")
    req = capture_request_id(port, "return engine.getSaveStatus()")
    ok, status = wait_save_complete(port, req) if req is not None else (False, None)
    checks.check(label, ok, str(status))


def load_through_barrier(port: int, checks: Checks, slot: str,
                         label: str) -> bool:
    """Load `slot` and wait for the world to publish. Returns whether the
    load published — a caller must not assert on a session that never
    arrived."""
    accepted = send(port, f"return engine.loadSave('{slot}')", timeout=30.0)
    if accepted != "true":
        raise ProbeError(f"engine.loadSave was not accepted: {accepted!r}")
    req = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, request_id=req)
    return checks.check(label, published, str(status))
