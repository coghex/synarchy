"""Shared fixtures of the two fresh-process persistence probes (issue #2943).

``tools/persistence_contract_probe.py`` (the CI-eligible core: the generic
engines A-D plus the S11a mind-episode engines) and
``tools/persistence_system_cycles_probe.py`` (the manual-only focused
system cycles: engagement, exhaustion/sleep, cardio, blood and relation
causes) boot real engines the same way. What both need lives here, once:
the isolated and the PRE-S11b legacy resource roots, the definition
bootstrap, ``boot_probe``, ``engagement_boot`` / ``engagement_load`` (the
engine boot with the unit AI paused, #2865, and the paused load), the
``Checks`` counter, ``save_and_wait`` and the small polling helpers.

This module never deletes a scratch tree: each entrypoint retires its own
through ``probe_evidence`` (so a failing run keeps its saves and logs), and
``tools/test_probe_evidence.py`` pins that for both.
"""
from __future__ import annotations

import os
import re
import time
from pathlib import Path

from probelib import (boot, load_definitions, pause_ai, quit_engine, send,
                      wait_load_published, wait_save_complete,
                      capture_request_id)

REPO = Path(__file__).resolve().parent.parent

SEED = 424242
WORLD_SIZE = 8
PLATE_COUNT = 3


class Checks:
    def __init__(self) -> None:
        self.failed = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory (mirrors tools/save_storage_probe.py's helper) --
    this probe never touches the developer's real saves/."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def make_legacy_root(base: str) -> str:
    """A second throwaway resource root whose physiology registry is the
    PRE-S11b content (#2840): the same files as the shipped ones, except that
    ``systems.yaml`` puts the interim all-Simulated mind back into the
    ``reduced``, ``simple`` and ``exempt`` profiles. An engine booted on it
    spawns a bear, squirrel and nomad the way S11a did, so the save it writes
    is a real old-default save (a Simulated mind and its episode) produced by
    the engine itself, never by a raw write of a Nominal-owned key. Nothing
    under the repository is edited: the modified registry is a copy in the
    probe's own temporary directory (everything else is symlinked)."""
    root = os.path.join(base, "legacy_root")
    os.makedirs(os.path.join(root, "data", "physiology"), exist_ok=True)
    for family in ("scripts", "assets", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    for entry in sorted(os.listdir(os.path.join(REPO, "data"))):
        if entry == "physiology":
            continue
        target = os.path.join(root, "data", entry)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, "data", entry), target)
    for entry in sorted(os.listdir(os.path.join(REPO, "data", "physiology"))):
        src = os.path.join(REPO, "data", "physiology", entry)
        dst = os.path.join(root, "data", "physiology", entry)
        if entry != "systems.yaml":
            if not os.path.exists(dst):
                os.symlink(src, dst)
            continue
        text = Path(src).read_text()
        head, sep, profiles = text.partition("\nprofiles:\n")
        assert sep, "systems.yaml has no profiles: block"
        # a profile's last line is `    mind: <mode>`; put the S11a value back
        # in every non-rich profile
        out, count = [], 0
        current = None
        for line in profiles.splitlines():
            m = re.match(r"^  (\w+):\s*$", line)
            if m:
                current = m.group(1)
            if current in ("reduced", "simple", "exempt") and line.strip() == "mind: nominal":
                line = line.replace("nominal", "simulated")
                count += 1
            out.append(line)
        assert count == 3, f"expected to restore 3 profile mind rows, restored {count}"
        Path(dst).write_text(head + sep + "\n".join(out) + "\n")
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap_defs(port: int) -> None:
    load_definitions(port)


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=180)


def wait_for_file(path: str, seconds: float = 30.0) -> bool:
    deadline = time.time() + seconds
    while time.time() < deadline:
        if os.path.isfile(path):
            return True
        time.sleep(0.1)
    return False


def save_and_wait(chk: Checks, port: int, page: str, slot: str) -> str:
    """engine.saveWorld, then tie completion to THIS save's own request id
    via engine.getSaveStatus() reaching SaveCaptureComplete (round-4
    review: the appearance of world.synworld on disk is a proxy, not the
    authoritative completion signal -- see wait_save_complete). Asserts
    acceptance, request-id capture, and terminal success as three
    separate checks so a failure names exactly which step broke."""
    saved = send(port, f"return engine.saveWorld('{page}', '{slot}')")
    chk.ok(saved.strip() == "true", f"engine.saveWorld('{slot}') accepted (got {saved!r})")
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    chk.ok(request_id is not None,
           f"engine.getSaveStatus() reports a request id right after saveWorld('{slot}')")
    ok, final_status = wait_save_complete(port, request_id)
    chk.ok(ok, f"save '{slot}' (request {request_id}) reached SaveCaptureComplete "
                f"(got {final_status!r})")
    return final_status


def as_int(s: str):
    try:
        return int(float(s))
    except (TypeError, ValueError):
        return None


# The focused scenarios (the mind-episode engines of the core probe and the
# engagement, exhaustion/sleep, cardio, blood and relation-cause engines of
# the system-cycles probe) need no AI: units must never act on their own. A headless boot's
# scripts/init_loader.lua has already loaded unit_resources AND unit_ai, so
# engagement_boot PAUSES that unit_ai (probelib.pause_ai, #2865) -- left
# running, a fixture acolyte's survival or medic errand can walk it off this
# seed's summit to its death mid-fixture. These loads add unit_stats; the
# unit_resources one dedups to init_loader's instance and its 0.1 s interval.
ENG_SCRIPTS = (
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
)


def lua_number(port: int, expr: str):
    """A Lua expression as a float, or None for nil / anything else."""
    r = send(port, f"return tostring({expr})").strip().strip('"')
    try:
        return float(r)
    except ValueError:
        return None


def lua_text(port: int, expr: str) -> str:
    return send(port, f"return tostring({expr})").strip().strip('"')


def wait_for(port: int, predicate, seconds: float = 12.0) -> bool:
    deadline = time.time() + seconds
    while time.time() < deadline:
        if predicate():
            return True
        time.sleep(0.2)
    return predicate()


def ensure_chunks(port: int) -> None:
    """Load the chunks around the origin before anything is spawned (a second
    or less): a spawn with no explicit height looks the surface up in the
    LOADED chunks and falls back to z = 0 when its chunk is not there yet, which
    drops the unit off the terrain on a slow runner (#2837's CI evidence)."""
    send(port, "return world.loadChunksInRegion(-1, -1, 1, 1)")
    send(port, "return world.waitForChunks(60)", timeout=65)


def engagement_boot(root: str, tmpdir: str, port: int, letter: str):
    """Boot one focused-scenario engine with the unit AI paused before anything spawns.

    The boot owns the process until it returns it: a setup step that fails
    (pause_ai raises ProbeSetupError) stops this engine before propagating,
    because the callers' cleanup only starts once they hold the process."""
    proc = boot_probe(root, port, os.path.join(tmpdir, f"engine{letter}.log"))
    try:
        bootstrap_defs(port)
        for path, delay in ENG_SCRIPTS:
            send(port, f"engine.loadScript('{path}', {delay}); return 'ok'")
        pause_ai(port)
    except BaseException:
        quit_engine(port, proc)
        raise
    return proc


def engagement_load(chk: Checks, port: int, slot: str) -> None:
    loaded = send(port, f"return engine.loadSave('{slot}')")
    chk.ok(loaded.strip() == "true", f"engine.loadSave('{slot}') accepted (got {loaded!r})")
    load_id = capture_request_id(port, "return engine.getLoadStatus()")
    published, status = wait_load_published(port, request_id=load_id)
    chk.ok(published, f"load transaction {load_id} published (status={status})")
    chk.ok(send(port, "return engine.isPaused()").strip() == "true",
           f"'{slot}' loads paused")
