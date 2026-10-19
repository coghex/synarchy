#!/usr/bin/env python3
"""Compact, CI-eligible fresh-process persistence contract probe (issue
#767, save-overhaul D1).

The pure hspec "persistence contract" gate
(``Test.Headless.World.Save.Contract``) already proves the in-process
encode/decode round trip is lossless for a representative multi-page
session. What that gate structurally CANNOT prove is requirement 5's own
literal ask: "terminate the engine. Start a fresh headless engine." This
probe is the compact, deterministic smoke version of that -- three real
fresh-process boots after the initial save, each loading the previous
one's save and
immediately re-saving, so requirement 9's "at least three fresh-process
save -> load -> save cycles" is exercised even at smoke scale.

Flow (isolated resource root -- see ``tools/save_storage_probe.py``'s
identical pattern -- never the developer's real ``saves/``):

  1. Engine A: boot, load defs + the AI script stack, ``world.init`` a
     small (worldSize 8) REAL generated world with a meaningful seed +
     player-facing identity, spawn an ``acolyte_portal`` (real,
     non-vacuous ``lua.building_spawn`` state once its roster starts
     spawning) and two units with one issued a real ``unitAi.
     commandAttack`` against the other (real, non-vacuous ``lua.unit_ai``
     state), a non-default map mode, save to slot "gen1".
  2. Quit A. Boot engine B (fresh process). Load "gen1"; assert the
     reset policy (paused, default tool, empty selections) and a short
     paused dwell where a live sample (date, unit position, the real
     unit_ai attack-target reference, the building_spawn roster
     countdown) is identical before and after. Re-save (no mutation in
     between) to "gen2".
  3. Quit B. Boot engines C and D the same way, each a fresh process
     loading the previous generation and re-saving: "gen2" -> "gen3",
     then "gen3" -> "gen4". B, C and D are the three complete
     fresh-process cycles requirement 9 asks for; engine A's save is
     the initial one, not itself a cycle.
  4. Quit D. ``persistence_snapshot.compare_session_files`` decodes all
     four generations through the REAL production codec
     (``World.Save.Envelope.decodeSessionEnvelope``, reached since #2274
     by exec'ing the compiled ``exe:synarchy-save-codec`` rather than a
     ``cabal repl`` of the test suite) and asserts every
     one is structurally IDENTICAL -- both the Haskell
     ``SessionSnapshot`` (via derived ``Eq``) and every ``lua.<module>``
     canonical component payload (via byte equality) -- the canonical
     persistence-state inspection/comparison surface (requirement 1),
     tied to each save/load boundary by waiting for
     ``engine.getSaveStatus()``/``engine.getLoadStatus()`` to reach a
     terminal phase before ever touching the resulting file
     (requirement 2). The non-default map mode is additionally checked
     directly via ``save_compat_audit.dump_canonical_summary`` (there is
     no live ``world.getMapMode`` query to assert it through the debug
     console instead).

Registered CI-eligible in ``tools/ci_probes.py`` -- ``src/World/Save/*``/
``src/World/Load/*``/``src/Engine/Save/*``/``src/Engine/Load/*`` are
already in ``CORE_GLOBS``, so any persistence-related source change
already selects the full CI-eligible set, this probe included.

S8a (#2834) adds three FOCUSED fresh-process cycles for the SD-20 combat
engagement (``engagement_cycles`` below), because the generic cycles above
never create the state: a nomad whose engagement was ADMITTED at a paused
save boundary (marker 1, still Nominal, no stamina), a nomad whose engagement
was APPLIED (marker 2, a pinned Simulated override, a partial pool), and
what each becomes across fresh processes -- the pending one transitions
exactly once with a full seed, the applied one keeps its pool and pin
exactly and is never reseeded.

S8b (#2835) adds three FOCUSED fresh-process cycles for the exhaustion and
sleep modes (``exhaustion_sleep_cycles`` below), because the generic cycles
never hold a partial pool of either: an acolyte with exact partial exhaustion
and sleep-pressure pools, a squirrel PINNED to a Simulated sleep with its own
partial pool (its shipped default is Nominal), a plain squirrel and a mule
(both Nominal, holding neither key), and a facade switch of one system to
Nominal that must scrub only its own key and survive another reload.

S9 (#2836) adds three FOCUSED fresh-process cycles for cardio
(``cardio_circulation_cycles`` below): an acolyte with exact partial heart rate
and blood oxygen, a nomad whose pin equals its species default, a squirrel
pinned Nominal (neither key stored, 70 bpm / 1.0 read), a bodyless deer (every
cardio key Unsupported), the facade switch of one unit to Nominal that must scrub
only its own two keys, and circulation DERIVED on demand and stored in no
process: a raw write of it is refused and no engine ever holds the key.

S10 (#2837) adds three FOCUSED fresh-process cycles for blood
(``blood_cycles`` below): a Simulated acolyte bled to an exact partial volume, a
nomad pinned Nominal (a full volume and a pin that survive), a bodyless deer
(Unsupported, reading nothing), a severe cut on the reloaded Nominal unit (it
survives with a full volume and a live bleed rate while the Simulated one keeps
draining), and the facade switch back to Simulated that seeds the maximum into
the dedicated uiBlood field. No shadow ``blood`` stat exists in any process, and
a raw write of the volume is refused.

The BROADER manual sweep (all of requirement 4's representative-scenario
checklist, the assembled failure contract, and every maintained
migration baseline) is ``tools/persistence_contract_sweep.py``.

Usage:
  python3 tools/persistence_contract_probe.py [--port 9277]
"""
from __future__ import annotations

import argparse
import glob
import os
import re
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import (boot, pause_ai, quit_engine, send, send_json, wait_load_published,
                       wait_save_complete, capture_request_id)
from persistence_snapshot import compare_session_files, prepare_decoder
from save_compat_audit import dump_canonical_summary

REPO = Path(__file__).resolve().parent.parent

SEED = 424242
WORLD_SIZE = 8
PLATE_COUNT = 3
PAGE = "contract_page"
# A page that only ever exists in a fresh, PRE-load live session -- never
# part of any save this suite writes -- used once (round-5 review) to
# prove a load replaces the whole session rather than merging into it.
GHOST_PAGE = "contract_preload_ghost"

# The AI/stats script stack the generic scenario (engines A-D) drives. A
# headless boot already runs scripts/init_loader.lua, which loads
# unit_resources, unit_ai and building_spawn itself; engine.loadScript
# dedups by path and keeps the first load's interval, so these calls return
# init_loader's instances (unit_stats is the one they actually load) rather
# than starting second ones. Mirrors probelib.AI_STACK exactly.
AI_SCRIPTS = (
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
    ("scripts/building_spawn.lua", 0.2),
)


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
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/factions/*.yaml", "engine.loadFactionYaml"),
        ("data/physiology/*.yaml", "engine.loadPhysiologyYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def load_ai_stack(port: int) -> None:
    for path, delay in AI_SCRIPTS:
        send(port, f"engine.loadScript('{path}', {delay}); return 'ok'")


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


def page_exists(port: int, page: str) -> bool:
    """A page's registration oracle (mirrors transactional_load_probe.py's
    identical helper): world.getDate returns nil for a page that isn't
    (or is no longer) registered."""
    r = send(port, f"return world.getDate('{page}')")
    return r not in ("nil", "null", "")


def build_scenario(chk: Checks, port: int) -> tuple[int, int, int]:
    """Engine A: a small real generated world with a meaningful seed +
    identity, a built acolyte_portal with a real roster countdown
    (non-vacuous lua.building_spawn state), two units with a real
    unitAi.commandAttack between them (non-vacuous lua.unit_ai state),
    and a non-default map mode. Returns (portal_bid, attacker_uid,
    target_uid)."""
    bootstrap_defs(port)
    load_ai_stack(port)
    inited = send(
        port,
        f"world.init('{PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
        f"'Contract Test World', 'the persistence contract test world'); "
        f"return 'ok'")
    chk.ok("ok" in inited, f"world.init accepted (got {inited!r})")
    # world.show (not just world.init) puts the page in wmVisible --
    # without it, building.spawn/canPlaceAt's visible-page terrain
    # read can reject a spawn (mirrors save_compat_audit.py's identical
    # note in generate_current_format_session).
    send(port, f"world.show('{PAGE}'); return 'ok'")
    deadline = time.time() + 30.0
    while time.time() < deadline:
        if send(port, "return world.getActiveWorldId()").strip('"') == PAGE:
            break
        time.sleep(0.2)
    else:
        chk.ok(False, f"'{PAGE}' never became the active world")
    send(port, "return world.waitForInit(30)", timeout=35)
    ensure_chunks(port)

    # A real, non-vacuous lua.building_spawn state: acolyte_portal is the
    # ONE building_spawn.lua-configured def (an ordinary building like
    # furnace/cargo_hold_S creates no per-building sequencer state at all
    # -- round-1 review finding). building.setSpawnRemaining seeds the
    # roster countdown directly rather than reverse-engineering the
    # build-tool "stake" placement flow.
    portal_bid = as_int(send(port, f"return building.spawn('acolyte_portal', 0, 0)"))
    chk.ok(portal_bid is not None and portal_bid >= 0,
           f"building.spawn('acolyte_portal') succeeded (got {portal_bid!r})")
    send(port, f"building.setSpawnRemaining({portal_bid}, 6); return 'ok'")
    remaining_before = as_int(send(port, f"return building.getSpawnRemaining({portal_bid})"))
    chk.ok(remaining_before == 6,
           f"acolyte_portal roster seeded to 6 (got {remaining_before!r})")
    # Give building_spawn.lua's sequencer a moment to fire its first
    # roster spawn -- proves the Lua sequencer (not just the engine-side
    # countdown) actually ran, so its per-building state entry is real.
    deadline = time.time() + 10.0
    remaining_after = remaining_before
    while time.time() < deadline:
        remaining_after = as_int(send(port, f"return building.getSpawnRemaining({portal_bid})"))
        if remaining_after is not None and remaining_after < remaining_before:
            break
        time.sleep(0.3)
    chk.ok(remaining_after is not None and remaining_after < remaining_before,
           f"acolyte_portal's roster sequencer spawned at least one unit "
           f"(remaining {remaining_before} -> {remaining_after})")

    # A real, non-vacuous lua.unit_ai state: a real commandAttack sets
    # attackTargetUid, verifiable live via unitAi.getState (round-1 review
    # finding -- a freshly spawned, idle unit's AI state is otherwise
    # near-empty).
    atk = as_int(send(port, "return unit.spawn('acolyte', 2, 2, 0, 'player')"))
    chk.ok(atk is not None and atk >= 0, f"attacker unit.spawn succeeded (got {atk!r})")
    # #2515: the target is a legacy-HOSTILE acolyte. Before the faction
    # profile a 'wildlife' acolyte was hostile to the player; under D-26
    # it is now an uncontrolled acolyte the legacy adapter reads as
    # neutral, so 'hostile' is what keeps this an attack on an enemy.
    tgt = as_int(send(port, "return unit.spawn('acolyte', 5, 5, 0, 'hostile')"))
    chk.ok(tgt is not None and tgt >= 0, f"target unit.spawn succeeded (got {tgt!r})")
    send(port, f"require('scripts.unit_ai').commandAttack({atk}, {tgt}); return 'ok'")
    time.sleep(0.5)
    attack_target = get_attack_target(port, atk)
    chk.ok(attack_target == tgt,
           f"commandAttack set a real, live attackTargetUid ({attack_target} "
           f"vs expected {tgt})")

    # #2515: the session holds every legacy faction's profile, so the
    # fresh-process cycles below compare each one's controller,
    # memberships with provenance, and capabilities. The bear's wildlife
    # tag is its definition default; the attacker carries the local
    # controller; the target legacy_hostile.
    FACTION_ROSTER.clear()
    FACTION_ROSTER.update({atk: "player", tgt: "hostile"})
    for spawn, tag in (("unit.spawn('bear_brown', 8, 2)", "wildlife"),
                       ("unit.spawn('acolyte', 2, 8, 0, 'neutral')", "neutral"),
                       ("unit.spawn('acolyte', 8, 8, 0, 'debug')", "debug")):
        uid = as_int(send(port, f"return {spawn}"))
        chk.ok(uid is not None and uid >= 0,
               f"{tag} unit {spawn} succeeded (got {uid!r})")
        if uid is not None:
            FACTION_ROSTER[uid] = tag
    time.sleep(0.5)  # unit.spawn only queues; let the unit thread commit
    assert_faction_roster(chk, port, "before the initial save")

    # world.setMapMode(pageId, mode) -- a bare-mode call (missing pageId)
    # is a silent no-op (round-1 review finding); there is no live
    # world.getMapMode query, so the non-default value is verified via
    # dump_canonical_summary once the save file exists (see main()).
    send(port, f"world.setMapMode('{PAGE}', 'map_pressure'); return 'ok'")
    # setMapMode is queued (worldQueue, async) -- give the world thread a
    # moment to apply it before the save captures state.
    time.sleep(0.5)

    # Round-2/3 review: the reset-policy check only proves something if
    # REAL non-default selections/tool exist before the save -- seed all
    # three selection classes requirement 6 names (unit, building, tile)
    # plus the tool mode, so "cleared after load" is a meaningful
    # assertion, not a vacuous one (nothing was ever selected/armed to
    # begin with). Each selection is verified IMMEDIATELY after it's set,
    # not batched up and checked only at the end: with a real acolyte_ai
    # combat loop and a roster sequencer both actively ticking in the
    # background (this scenario deliberately keeps both non-vacuous, see
    # above), umSelected/bmSelected are live, frequently-read engine state
    # that a slow batch of several round-tripped debug-console calls can
    # observe mid-flux -- checking right after each individual set keeps
    # the window between "set" and "verify" as small as a single
    # request/response round trip, matching how a real player's click and
    # its own immediate visual feedback would never race a save several
    # seconds later.
    selected = send(port, f"return unit.select({atk})").strip()
    chk.ok(selected == "true", f"unit.select({atk}) succeeded before saving (got {selected!r})")
    sel_before = send(port, "return unit.getSelected()")
    chk.ok(atk_in_selection(sel_before, atk),
           f"unit {atk} is genuinely selected before saving (got {sel_before!r})")

    # building.select(bid) returns no Lua value (0 results) -- verify via
    # building.getSelected() instead, which reports a bare bid (or nil),
    # NOT an array like unit.getSelected().
    send(port, f"building.select({portal_bid}); return 'ok'")
    bsel_before = as_int(send(port, "return building.getSelected()"))
    chk.ok(bsel_before == portal_bid,
           f"building {portal_bid} is genuinely selected before saving (got {bsel_before!r})")

    send(port, f"world.selectTile('{PAGE}', 3, 4, 2); return 'ok'")
    tile_before = send(port, f"return world.getSelectedTile('{PAGE}')")
    chk.ok(tile_before.strip() not in ("nil", "null", ""),
           f"a tile is genuinely selected before saving (got {tile_before!r})")

    send(port, f"world.setToolMode('{PAGE}', 'tool_mine'); return 'ok'")
    tool_before = send(port, "return world.getToolMode()").strip().strip('"')
    chk.ok(tool_before == "mine",
           f"tool mode is genuinely non-default before saving (got {tool_before!r})")

    return portal_bid, atk, tgt


# uid -> the legacy faction tag unit.getFaction must report for it,
# filled by build_scenario (#2515).
FACTION_ROSTER: dict[int, str] = {}


def assert_faction_roster(chk: Checks, port: int, when: str) -> None:
    """Every roster unit reads its legacy tag through the adapter."""
    for uid, tag in sorted(FACTION_ROSTER.items()):
        got = send(port, f"return unit.getFaction({uid})").strip().strip('"')
        chk.ok(got == tag,
               f"unit {uid} reads as '{tag}' {when} (got {got!r})")


def atk_in_selection(raw: str, uid: int) -> bool:
    try:
        import json
        parsed = json.loads(raw)
        return isinstance(parsed, list) and float(uid) in [float(x) for x in parsed]
    except (TypeError, ValueError):
        return False


def get_attack_target(port: int, uid: int):
    raw = send(port, f"local s = require('scripts.unit_ai').getState({uid}); "
                      f"return s and s.attackTargetUid or nil")
    return as_int(raw)


def sample_live_state(port: int, portal_bid: int, atk: int) -> dict:
    """A small, cheap live sample used for the paused-stability dwell
    check (requirement 7) -- repeating this during a paused dwell must
    yield identical values. This scenario never puts a player-faction
    unit within SIGHT of an undiscovered location, so it never
    hits the ONE documented, deliberate exception to that rule:
    World.Thread.Discovery.tickLocationDiscovery (#780) runs independent
    of the pause flag and can promote a location's lifecycle on the very
    first post-load tick if a unit can already see it (since #911 that
    is the instance's lifecycle, not a chunk set; since #1230 the
    trigger is sight against its bounds, not a discovery margin)."""
    return {
        "date": send(port, f"return world.getDate('{PAGE}')"),
        "activePage": send(port, "return world.getActiveWorldId()"),
        "paused": send(port, "return engine.isPaused()"),
        "toolMode": send(port, "return world.getToolMode()"),
        "spawnRemaining": send(port, f"return building.getSpawnRemaining({portal_bid})"),
        "attackTargetUid": get_attack_target(port, atk),
    }


def assert_reset_policy(chk: Checks, port: int, when: str) -> None:
    chk.ok(send(port, "return engine.isPaused()").strip() == "true",
           f"{when}: session begins paused")
    tool_mode = send(port, "return world.getToolMode()").strip().strip('"')
    chk.ok(tool_mode == "default",
           f"{when}: tool resets to the default tool (#103), got {tool_mode!r}")
    sel = send(port, "return unit.getSelected()")
    chk.ok(sel.strip() in ("nil", "null", "[]", "{}", ""),
           f"{when}: unit selection is empty (got {sel!r})")
    # Round-3 review: requirement 6 names building and tile selections
    # too, not just unit selection.
    bsel = send(port, "return building.getSelected()")
    chk.ok(bsel.strip() in ("nil", "null", ""),
           f"{when}: building selection is empty (got {bsel!r})")
    tile = send(port, f"return world.getSelectedTile('{PAGE}')")
    chk.ok(tile.strip() in ("nil", "null", ""),
           f"{when}: tile selection is empty (got {tile!r})")


def assert_nondefault_map_mode(chk: Checks, tmpdir: str, save_path: str, page: str) -> None:
    """world.setMapMode has no live query counterpart, so the non-default
    value is verified through the same dump_canonical_summary decode
    tools/save_compat_migration_probe.py already uses -- its per-page
    dump already includes "mapMode" (app-save-codec/Main.hs's
    `canonicalSummary`, which since #2273 is a compiled helper rather
    than a GHCi program)."""
    import json
    out_path = os.path.join(tmpdir, "map_mode_check.json")
    ok, tail = dump_canonical_summary(Path(save_path), Path(out_path))
    if not ok:
        chk.ok(False, f"dump_canonical_summary failed for map-mode check: {tail}")
        return
    summary = json.loads(Path(out_path).read_text(encoding="utf-8"))
    page_entry = next((p for p in summary.get("pages", []) if p.get("pageId") == page), None)
    chk.ok(bool(page_entry) and page_entry.get("mapMode") == "ZMPressure",
           f"world.setMapMode('{page}', 'map_pressure') actually took effect "
           f"(decoded pgsMapMode = {page_entry.get('mapMode') if page_entry else None!r}, "
           f"expected 'ZMPressure')")


# ── S8a (#2834): the SD-20 combat engagement across fresh processes ─────────

ENG_PAGE = "engagement_page"
# The engagement, exhaustion/sleep, cardio and blood scenarios (engines E-P)
# need no AI: units must never act on their own. A headless boot's
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


def engagement_state(port: int, uid: int) -> dict:
    """What a unit stores about the engagement: the marker, the pinned stamina
    override, the resolved mode, the stored pool and the maximum."""
    return {
        "marker": lua_number(port, f"unit.getStat({uid}, 'combat_engaged')"),
        "override": lua_text(port, f"unit.getPhysioOverride({uid}, 'stamina')"),
        "mode": lua_text(port, f"unit.getPhysioMode({uid}, 'stamina')"),
        "pool": lua_number(port, f"unit.getStat({uid}, 'stamina')"),
        "max": lua_number(port, f"unit.getMaxStamina({uid})"),
    }


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
    """Boot one of engines E-P with the unit AI paused before anything spawns.

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


def engagement_cycles(chk: Checks, root: str, tmpdir: str, port: int) -> None:
    """Three fresh processes: save a PENDING and an APPLIED engagement,
    load them and let the pending one transition, save again, load again."""
    print("=== engagement engine E: pending (paused save boundary) + applied, save 'eng1' ===")
    proc = engagement_boot(root, tmpdir, port, "E")
    try:
        send(port, f"world.init('{ENG_PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
                   f"'Engagement Test World', 'the SD-20 engagement'); return 'ok'")
        send(port, f"world.show('{ENG_PAGE}'); return 'ok'")
        wait_for(port, lambda: send(port, "return world.getActiveWorldId()")
                 .strip('"') == ENG_PAGE, 30.0)
        send(port, "return world.waitForInit(30)", timeout=35)
        ensure_chunks(port)
        # The STANDARD spawn profile (#2756): every stat at its definition's
        # mean, no draw from the stat RNG, so an endurance-1 nomad's maximum is
        # exactly 15 and its reach is the definition's, not a roll.
        applied = as_int(send(port, "return unit.spawn('nomad_primitive', 4, 4, 0, "
                                    "'player', nil, nil, 'standard')"))
        pending = as_int(send(port, "return unit.spawn('nomad_primitive', 4, 4, 0, "
                                    "'player', nil, nil, 'standard')"))
        target = as_int(send(port, "return unit.spawn('acolyte', 4, 4, 0, "
                                   "'hostile', nil, nil, 'standard')"))
        chk.ok(None not in (applied, pending, target) and min(applied, pending, target) >= 0,
               f"nomads and a target spawned (got {applied}, {pending}, {target})")
        time.sleep(0.6)   # unit.spawn only queues; let the unit thread commit

        # Before any combat: both nomads are Nominal with no stamina and no marker.
        for uid in (applied, pending):
            st = engagement_state(port, uid)
            chk.ok(st["marker"] is None and st["pool"] is None
                   and st["mode"] == "nominal" and st["override"] == "nil"
                   and st["max"] == 15.0,
                   f"nomad {uid} starts Nominal at maximum 15, no marker, no pool (got {st})")

        # APPLIED: a real admitted attack while the engine runs; the next
        # physiology pass seeds the full pool and pins Simulated. The units
        # are put back on one tile first (collision pushes them apart), so
        # the strike is inside both reaches.
        def engaged() -> bool:
            for _ in range(8):
                for uid in (applied, target):
                    send(port, f"unit.setPos({uid}, 4, 4); return 'ok'")
                send(port, f"combat.attack({applied}, {target}, 'quick'); return 'ok'")
                if wait_for(port, lambda: lua_number(
                        port, f"unit.getStat({applied}, 'combat_engaged')") == 2.0, 2.0):
                    return True
            return False
        chk.ok(engaged(),
               "an admitted attack engages the nomad and the next pass applies it (marker 2)")
        st = engagement_state(port, applied)
        chk.ok(st["override"] == "simulated" and st["mode"] == "simulated"
               and st["pool"] is not None and st["pool"] > 0,
               f"the applied nomad pinned Simulated with a seeded pool (got {st})")

        # Pause, then give the applied nomad a PARTIAL pool no reseed would keep.
        send(port, "require('scripts.pause').set(true); return 'ok'")
        send(port, f"unit.setStat({applied}, 'stamina', 4); return 'ok'")
        chk.ok(lua_number(port, f"unit.getStat({applied}, 'stamina')") == 4.0,
               "the applied nomad holds an exact partial pool of 4")

        # PENDING: an attack queued while paused is drained ONLY at the save
        # boundary (Combat.Thread), so the marker is set by the save itself.
        # (Paused: nothing moves, so the units stay on one tile inside both reaches.)
        for uid in (pending, target):
            send(port, f"unit.setPos({uid}, 4, 4); return 'ok'")
        time.sleep(0.3)   # let the unit thread apply the placement
        send(port, f"combat.attack({pending}, {target}, 'quick'); return 'ok'")
        save_and_wait(chk, port, ENG_PAGE, "eng1")
        st = engagement_state(port, pending)
        chk.ok(st["marker"] == 1.0 and st["pool"] is None and st["mode"] == "nominal"
               and st["override"] == "nil",
               f"a combat admission at a paused save boundary leaves the nomad PENDING (got {st})")
        quit_engine(port, proc)
        proc = None

        print("=== engagement engine F: load 'eng1', pending transitions once, save 'eng2' ===")
        proc = engagement_boot(root, tmpdir, port, "F")
        engagement_load(chk, port, "eng1")
        st = engagement_state(port, pending)
        chk.ok(st["marker"] == 1.0 and st["pool"] is None and st["mode"] == "nominal"
               and st["override"] == "nil" and st["max"] == 15.0,
               f"a pending save reloads pending, Nominal, no stamina, maximum 15 (got {st})")
        st = engagement_state(port, applied)
        chk.ok(st["marker"] == 2.0 and st["override"] == "simulated"
               and st["mode"] == "simulated" and st["pool"] == 4.0 and st["max"] == 15.0,
               f"an applied save reloads applied, pinned Simulated, the exact partial pool (got {st})")
        send(port, "require('scripts.pause').set(false); return 'ok'")
        chk.ok(wait_for(port, lambda: lua_number(
                   port, f"unit.getStat({pending}, 'combat_engaged')") == 2.0),
               "the first eligible pass after the load applies the pending engagement")
        send(port, "require('scripts.pause').set(true); return 'ok'")
        st = engagement_state(port, pending)
        chk.ok(st["override"] == "simulated" and st["mode"] == "simulated"
               and st["pool"] is not None and 14.0 <= st["pool"] <= 15.0001,
               f"the pending nomad transitioned once with a full seed (got {st})")
        st = engagement_state(port, applied)
        chk.ok(st["marker"] == 2.0 and st["pool"] is not None and 3.9 <= st["pool"] < 9.0,
               f"the applied nomad was never reseeded: its partial pool kept ticking (got {st})")
        save_and_wait(chk, port, ENG_PAGE, "eng2")
        saved = {uid: engagement_state(port, uid) for uid in (pending, applied)}
        quit_engine(port, proc)
        proc = None

        print("=== engagement engine G: load 'eng2', both applied exactly ===")
        proc = engagement_boot(root, tmpdir, port, "G")
        engagement_load(chk, port, "eng2")
        for uid in (pending, applied):
            got = engagement_state(port, uid)
            want = saved[uid]
            chk.ok(got["marker"] == 2.0 and got["override"] == "simulated"
                   and got["mode"] == "simulated" and got["max"] == 15.0
                   and got["pool"] is not None and want["pool"] is not None
                   and abs(got["pool"] - want["pool"]) < 1e-4,
                   f"nomad {uid}: applied marker, pinned override and pool persist "
                   f"exactly across a fresh process (got {got}, saved {want})")
        quit_engine(port, proc)
        proc = None
    finally:
        if proc is not None:
            quit_engine(port, proc)


# ── S8b (#2835): exhaustion and sleep across fresh processes ────────────────

ES_PAGE = "exhaustion_sleep_page"


def pool_state(port: int, uid: int) -> dict:
    """What a unit stores and resolves for the two S8b systems."""
    return {
        "ex": lua_number(port, f"unit.getStatBase({uid}, 'exhaustion')"),
        "sp": lua_number(port, f"unit.getStatBase({uid}, 'sleep_pressure')"),
        "ex_mode": lua_text(port, f"unit.getPhysioMode({uid}, 'exhaustion')"),
        "sp_mode": lua_text(port, f"unit.getPhysioMode({uid}, 'sleep')"),
        "sp_pin": lua_text(port, f"unit.getPhysioOverride({uid}, 'sleep')"),
        "max_sp": lua_number(port, f"require('scripts.unit_stats').get({uid}, "
                                   f"'max_sleep_pressure')"),
    }


def exhaustion_sleep_cycles(chk: Checks, root: str, tmpdir: str, port: int) -> None:
    """Three fresh processes: save partial pools, a pinned Simulated squirrel
    and two Nominal units; load, switch one system through the facade, save
    again; load again."""
    print("=== exhaustion/sleep engine H: partial pools + pinned squirrel, save 'es1' ===")
    proc = engagement_boot(root, tmpdir, port, "H")
    try:
        send(port, f"world.init('{ES_PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
                   f"'Exhaustion Sleep World', 'the S8b modes'); return 'ok'")
        send(port, f"world.show('{ES_PAGE}'); return 'ok'")
        wait_for(port, lambda: send(port, "return world.getActiveWorldId()")
                 .strip('"') == ES_PAGE, 30.0)
        send(port, "return world.waitForInit(30)", timeout=35)
        ensure_chunks(port)
        # The STANDARD spawn profile (#2756): every stat at its definition's
        # mean, so each maximum is a known number, not a roll. The height is
        # OMITTED (nil), so each unit spawns ON the resolved surface: an
        # explicit Z of 0 sits off the generated terrain, the unit FALLS, and
        # under a slow runner it dies of the fall before the engine can be
        # paused with every unit standing (#2837's CI evidence).
        acolyte = as_int(send(port, "return unit.spawn('acolyte', 4, 4, nil, "
                                    "'player', nil, nil, 'standard')"))
        pinned = as_int(send(port, "return unit.spawn('red_squirrel', 5, 5, nil, "
                                   "'player', nil, nil, 'standard')"))
        plain = as_int(send(port, "return unit.spawn('red_squirrel', 6, 6, nil, "
                                  "'player', nil, nil, 'standard')"))
        mule = as_int(send(port, "return unit.spawn('technomule', 7, 7, nil, "
                                 "'player', nil, nil, 'standard')"))
        uids = (acolyte, pinned, plain, mule)
        chk.ok(None not in uids and min(uids) >= 0, f"the four units spawned (got {uids})")
        time.sleep(0.6)   # unit.spawn only queues; let the unit thread commit

        # The arena's cliffs make a spawned unit fall and, some seconds later,
        # die of it, and a unit saved MID-CLIMB or mid-fall restores in a pose
        # it cannot resume. So pause the instant every unit is standing, check
        # it still is, and do everything else with the engine paused.
        def all_standing() -> bool:
            return all(lua_text(port, f"unit.getPose({u})") == "standing" for u in uids)

        def paused_standing() -> bool:
            if not all_standing():
                return False
            send(port, "require('scripts.pause').set(true); return 'ok'")
            if all_standing():
                return True
            send(port, "require('scripts.pause').set(false); return 'ok'")
            return False
        chk.ok(wait_for(port, paused_standing, 40.0),
               "every unit has settled into a standing pose and the engine is paused")

        # The shipped table, through real spawns: acolyte S/S; the squirrel
        # exhaustion Simulated and sleep Nominal; the mule N/N.
        want = {acolyte: ("simulated", "simulated"), pinned: ("simulated", "nominal"),
                plain: ("simulated", "nominal"), mule: ("nominal", "nominal")}
        for uid, (ex_mode, sp_mode) in want.items():
            st = pool_state(port, uid)
            chk.ok(st["ex_mode"] == ex_mode and st["sp_mode"] == sp_mode,
                   f"unit {uid} resolves exhaustion {ex_mode} / sleep {sp_mode} (got {st})")
        for uid in (plain, mule):
            st = pool_state(port, uid)
            chk.ok(st["sp"] is None and (uid != mule or st["ex"] is None),
                   f"unit {uid} holds no sleep_pressure"
                   f"{' and no exhaustion' if uid == mule else ''} (got {st})")

        # Pin the squirrel's sleep Simulated through the facade (it seeds the
        # full pool), then give three units EXACT partial pools no reseed keeps.
        pinned_ok = send(port, f"local ok, why = require('scripts.physiology')"
                               f".setMode({pinned}, 'sleep', 'simulated'); "
                               f"return tostring(ok) .. '|' .. tostring(why)").strip('"')
        chk.ok(pinned_ok == "true|true",
               f"the squirrel's sleep is pinned Simulated through the facade "
               f"(got {pinned_ok!r})")
        st = pool_state(port, pinned)
        chk.ok(st["sp"] is not None and st["max_sp"] is not None
               and abs(st["sp"] - st["max_sp"]) < 1e-4 and st["sp_pin"] == "simulated",
               f"the pin seeded the full pool (got {st})")
        send(port, f"unit.setStat({acolyte}, 'exhaustion', 3.5); "
                   f"unit.setStat({acolyte}, 'sleep_pressure', 2.25); "
                   f"unit.setStat({pinned}, 'sleep_pressure', 4.5); return 'ok'")
        chk.ok(lua_number(port, f"unit.getStat({acolyte}, 'exhaustion')") == 3.5
               and lua_number(port, f"unit.getStat({acolyte}, 'sleep_pressure')") == 2.25
               and lua_number(port, f"unit.getStat({pinned}, 'sleep_pressure')") == 4.5,
               "the partial pools are exact")
        save_and_wait(chk, port, ES_PAGE, "es1")
        quit_engine(port, proc)
        proc = None

        print("=== exhaustion/sleep engine I: load 'es1', switch sleep, save 'es2' ===")
        proc = engagement_boot(root, tmpdir, port, "I")
        engagement_load(chk, port, "es1")
        st = pool_state(port, acolyte)
        chk.ok(st["ex"] == 3.5 and st["sp"] == 2.25 and st["ex_mode"] == "simulated"
               and st["sp_mode"] == "simulated",
               f"the acolyte's partial pools reload exactly (got {st})")
        st = pool_state(port, pinned)
        chk.ok(st["sp"] == 4.5 and st["sp_mode"] == "simulated" and st["sp_pin"] == "simulated",
               f"the pinned squirrel keeps its pin and exact pool (got {st})")
        for uid in (plain, mule):
            st = pool_state(port, uid)
            chk.ok(st["sp"] is None and st["sp_mode"] == "nominal",
                   f"unit {uid} reloads Nominal holding no sleep_pressure (got {st})")
        # unpause just long enough for the pinned squirrel's pool to drain
        # (drain-only parameters); a longer window risks a cliff fall
        send(port, "require('scripts.pause').set(false); return 'ok'")
        chk.ok(wait_for(port, lambda: (lua_number(
                   port, f"unit.getStat({pinned}, 'sleep_pressure')") or 99) < 4.5, 6.0),
               "the pinned squirrel's reloaded pool keeps draining")
        send(port, "require('scripts.pause').set(true); return 'ok'")
        chk.ok(lua_number(port, f"unit.getStat({plain}, 'sleep_pressure')") is None,
               "the plain squirrel still holds no sleep_pressure after ticking")
        # a facade switch scrubs only its own key
        switched = send(port, f"local ok, why = require('scripts.physiology')"
                              f".setMode({acolyte}, 'sleep', 'nominal'); "
                              f"return tostring(ok) .. '|' .. tostring(why)").strip('"')
        chk.ok(switched == "true|true",
               f"the acolyte's sleep is switched to Nominal through the facade "
               f"(got {switched!r})")
        st = pool_state(port, acolyte)
        chk.ok(st["sp"] is None and st["ex"] is not None and st["sp_mode"] == "nominal",
               f"only the sleep pool was scrubbed (got {st})")
        ex_saved = st["ex"]
        pin_pool = lua_number(port, f"unit.getStatBase({pinned}, 'sleep_pressure')")
        save_and_wait(chk, port, ES_PAGE, "es2")
        quit_engine(port, proc)
        proc = None

        print("=== exhaustion/sleep engine J: load 'es2' ===")
        proc = engagement_boot(root, tmpdir, port, "J")
        engagement_load(chk, port, "es2")
        st = pool_state(port, acolyte)
        chk.ok(st["sp"] is None and st["sp_mode"] == "nominal" and st["sp_pin"] == "nominal"
               and st["ex"] is not None and ex_saved is not None
               and abs(st["ex"] - ex_saved) < 1e-4,
               f"the switched acolyte reloads Nominal, the pin and exhaustion exact (got {st})")
        st = pool_state(port, pinned)
        chk.ok(st["sp"] is not None and pin_pool is not None
               and abs(st["sp"] - pin_pool) < 1e-4 and st["sp_pin"] == "simulated",
               f"the pinned squirrel's pool persists exactly again (got {st})")
        quit_engine(port, proc)
        proc = None
    finally:
        if proc is not None:
            quit_engine(port, proc)


# ── S9 (#2836): cardio and the derived circulation across fresh processes ───

CC_PAGE = "cardio_circulation_page"


def cardio_state(port: int, uid: int) -> dict:
    """What a unit stores, resolves and derives for the S9 cardio system."""
    return {
        "hr": lua_number(port, f"unit.getStatBase({uid}, 'heart_rate')"),
        "o2": lua_number(port, f"unit.getStatBase({uid}, 'blood_oxygen')"),
        "stored_circ": lua_number(port, f"unit.getStatBase({uid}, 'circulation')"),
        "mode": lua_text(port, f"unit.getPhysioMode({uid}, 'cardio')"),
        "pin": lua_text(port, f"unit.getPhysioOverride({uid}, 'cardio')"),
        "read_hr": lua_number(port, f"unit.readPhysio({uid}, 'heart_rate')"),
        "read_o2": lua_number(port, f"unit.readPhysio({uid}, 'blood_oxygen')"),
        "circ": lua_number(port, f"unit.readPhysio({uid}, 'circulation')"),
    }


def cardio_circulation_cycles(chk: Checks, root: str, tmpdir: str, port: int) -> None:
    """Three fresh processes: save partial heart rate / oxygen, a pin equal to
    the species default, a Nominal pin and a bodyless unit; load, switch one
    unit through the facade, save again; load again. Circulation is derived on
    demand and never stored, in any process."""
    print("=== cardio engine K: partial integrators + pins, save 'cc1' ===")
    proc = engagement_boot(root, tmpdir, port, "K")
    try:
        send(port, f"world.init('{CC_PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
                   f"'Cardio Circulation World', 'the S9 modes'); return 'ok'")
        send(port, f"world.show('{CC_PAGE}'); return 'ok'")
        wait_for(port, lambda: send(port, "return world.getActiveWorldId()")
                 .strip('"') == CC_PAGE, 30.0)
        send(port, "return world.waitForInit(30)", timeout=35)
        ensure_chunks(port)
        acolyte = as_int(send(port, "return unit.spawn('acolyte', 4, 4, nil, "
                                    "'player', nil, nil, 'standard')"))
        nomad = as_int(send(port, "return unit.spawn('nomad_primitive', 5, 5, nil, "
                                  "'player', nil, nil, 'standard')"))
        squirrel = as_int(send(port, "return unit.spawn('red_squirrel', 6, 6, nil, "
                                     "'player', nil, nil, 'standard')"))
        deer = as_int(send(port, "return unit.spawn('white_tailed_deer', 7, 7, nil, "
                                 "'player', nil, nil, 'standard')"))
        uids = (acolyte, nomad, squirrel, deer)
        chk.ok(None not in uids and min(uids) >= 0, f"the four units spawned (got {uids})")
        time.sleep(0.6)   # unit.spawn only queues; let the unit thread commit

        # The arena's cliffs: pause the instant every unit is standing (see
        # exhaustion_sleep_cycles), and do everything else paused.
        def all_standing() -> bool:
            return all(lua_text(port, f"unit.getPose({u})") == "standing" for u in uids)

        def paused_standing() -> bool:
            if not all_standing():
                return False
            send(port, "require('scripts.pause').set(true); return 'ok'")
            if all_standing():
                return True
            send(port, "require('scripts.pause').set(false); return 'ok'")
            return False
        chk.ok(wait_for(port, paused_standing, 40.0),
               "every unit has settled into a standing pose and the engine is paused")

        # The shipped default through real spawns: cardio is Simulated for every
        # bodied species (the nomad included) and Unsupported for the bodyless.
        for uid, mode in ((acolyte, "simulated"), (nomad, "simulated"),
                          (squirrel, "simulated"), (deer, "nil")):
            st = cardio_state(port, uid)
            chk.ok(st["mode"] == mode,
                   f"unit {uid} resolves cardio {mode} (got {st})")
            chk.ok(st["stored_circ"] is None,
                   f"unit {uid} stores no circulation (got {st})")
        for uid in (acolyte, nomad, squirrel):
            st = cardio_state(port, uid)
            chk.ok(st["hr"] is not None and st["o2"] is not None
                   and st["circ"] is not None and 0.05 <= st["circ"] <= 1.0,
                   f"unit {uid} holds heart rate and oxygen and derives circulation "
                   f"(got {st})")
        st = cardio_state(port, deer)
        chk.ok(st["hr"] is None and st["o2"] is None and st["circ"] is None
               and st["read_hr"] is None and st["read_o2"] is None,
               f"the bodyless unit holds and reads none of the three (got {st})")

        # A raw circulation write is refused in every mode and creates nothing.
        refused = send(port, f"return tostring(unit.setStat({acolyte}, "
                             f"'circulation', 0.3))").strip('"')
        chk.ok(refused == "false" and cardio_state(port, acolyte)["stored_circ"] is None,
               f"a raw circulation write is refused and stores nothing (got {refused!r})")

        # Exact partial integrators no reseed keeps; a pin EQUAL to the default;
        # a Nominal pin through the facade (scrubs both keys).
        send(port, f"unit.setStat({acolyte}, 'heart_rate', 88.5); "
                   f"unit.setStat({acolyte}, 'blood_oxygen', 0.42); "
                   f"unit.setStat({nomad}, 'heart_rate', 61); return 'ok'")
        pinned = send(port, f"local ok, ch = require('scripts.physiology')"
                            f".setMode({nomad}, 'cardio', 'simulated'); "
                            f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
        chk.ok(pinned == "true|false",
               f"the nomad's pin equal to the default is recorded (got {pinned!r})")
        nominal = send(port, f"local ok, ch = require('scripts.physiology')"
                             f".setMode({squirrel}, 'cardio', 'nominal'); "
                             f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
        chk.ok(nominal == "true|true",
               f"the squirrel's cardio is pinned Nominal through the facade (got {nominal!r})")
        st = cardio_state(port, squirrel)
        chk.ok(st["hr"] is None and st["o2"] is None and st["read_hr"] == 70.0
               and st["read_o2"] == 1.0 and st["mode"] == "nominal",
               f"the Nominal squirrel holds neither key and reads 70 / 1.0 (got {st})")
        save_and_wait(chk, port, CC_PAGE, "cc1")
        quit_engine(port, proc)
        proc = None

        print("=== cardio engine L: load 'cc1', switch cardio, save 'cc2' ===")
        proc = engagement_boot(root, tmpdir, port, "L")
        engagement_load(chk, port, "cc1")
        st = cardio_state(port, acolyte)
        chk.ok(st["hr"] == 88.5 and st["o2"] is not None and abs(st["o2"] - 0.42) < 1e-6
               and st["mode"] == "simulated" and st["stored_circ"] is None
               and st["circ"] is not None,
               f"the acolyte's partial integrators reload exactly, circulation derived "
               f"before the first tick (got {st})")
        st = cardio_state(port, nomad)
        chk.ok(st["hr"] == 61.0 and st["pin"] == "simulated" and st["mode"] == "simulated",
               f"the nomad keeps its exact heart rate and its pin equal to the default (got {st})")
        st = cardio_state(port, squirrel)
        chk.ok(st["hr"] is None and st["o2"] is None and st["pin"] == "nominal"
               and st["mode"] == "nominal" and st["read_hr"] == 70.0,
               f"the Nominal squirrel reloads Nominal holding neither key (got {st})")
        st = cardio_state(port, deer)
        chk.ok(st["hr"] is None and st["o2"] is None and st["mode"] == "nil"
               and st["stored_circ"] is None,
               f"the bodyless unit reloads holding nothing (got {st})")
        # unpause just long enough for the acolyte's reloaded integrators to move:
        # its saved oxygen of 0.42 is far under the 0.9 compensation threshold, so
        # the heart rate is pulled UP from 88.5 toward 70 + 80 * (0.9 - 0.42); a
        # longer window risks a cliff fall
        send(port, "require('scripts.pause').set(false); return 'ok'")
        chk.ok(wait_for(port, lambda: abs((lua_number(
                   port, f"unit.getStat({acolyte}, 'heart_rate')") or 88.5) - 88.5) > 0.05, 6.0),
               "the acolyte's reloaded heart rate keeps integrating")
        send(port, "require('scripts.pause').set(true); return 'ok'")
        switched = send(port, f"local ok, why = require('scripts.physiology')"
                              f".setMode({acolyte}, 'cardio', 'nominal'); "
                              f"return tostring(ok) .. '|' .. tostring(why)").strip('"')
        chk.ok(switched == "true|true",
               f"the acolyte's cardio is switched to Nominal through the facade "
               f"(got {switched!r})")
        st = cardio_state(port, acolyte)
        chk.ok(st["hr"] is None and st["o2"] is None and st["read_hr"] == 70.0
               and st["circ"] is not None and st["stored_circ"] is None,
               f"both integrators were scrubbed and circulation is still derived (got {st})")
        nomad_hr = lua_number(port, f"unit.getStatBase({nomad}, 'heart_rate')")
        save_and_wait(chk, port, CC_PAGE, "cc2")
        quit_engine(port, proc)
        proc = None

        print("=== cardio engine M: load 'cc2' ===")
        proc = engagement_boot(root, tmpdir, port, "M")
        engagement_load(chk, port, "cc2")
        st = cardio_state(port, acolyte)
        chk.ok(st["hr"] is None and st["o2"] is None and st["pin"] == "nominal"
               and st["mode"] == "nominal" and st["read_hr"] == 70.0
               and st["stored_circ"] is None and st["circ"] is not None,
               f"the switched acolyte reloads Nominal with the pin (got {st})")
        st = cardio_state(port, nomad)
        chk.ok(st["hr"] is not None and nomad_hr is not None
               and abs(st["hr"] - nomad_hr) < 1e-4 and st["pin"] == "simulated",
               f"the nomad's integrator and pin persist exactly again (got {st})")
        quit_engine(port, proc)
        proc = None
    finally:
        if proc is not None:
            quit_engine(port, proc)


# ── S10 (#2837): blood across fresh processes ───────────────────────────────

BL_PAGE = "blood_modes_page"


def blood_state(port: int, uid: int) -> dict:
    """What a unit stores, resolves and reads for the S10 blood system."""
    cur = lua_number(port, f"unit.getBlood({uid}).current")
    mx = lua_number(port, f"unit.getBlood({uid}).max")
    return {
        "cur": cur, "max": mx,
        "mode": lua_text(port, f"unit.getPhysioMode({uid}, 'blood')"),
        "pin": lua_text(port, f"unit.getPhysioOverride({uid}, 'blood')"),
        "read": lua_number(port, f"unit.readPhysio({uid}, 'blood')"),
        "frac": lua_number(port, f"unit.readPhysio({uid}, 'blood_fraction')"),
        "shadow": lua_number(port, f"unit.getStatBase({uid}, 'blood')"),
        "pose": lua_text(port, f"unit.getPose({uid})"),
    }


def blood_cycles(chk: Checks, root: str, tmpdir: str, port: int) -> None:
    """Three fresh processes: a bled Simulated unit (a partial volume), a unit
    pinned Nominal, a bodyless unit; load, bleed a Nominal unit severely and
    switch it back, save again; load again. The volume lives in the dedicated
    uiBlood field in every process: a shadow `blood` stat never appears."""
    print("=== blood engine N: partial volume + Nominal pin, save 'bl1' ===")
    # Engine N runs on the PRE-S11b registry (#2840): its save is the real
    # old-default save the changed-default load repair is proven against.
    legacy = make_legacy_root(tmpdir)
    proc = engagement_boot(legacy, tmpdir, port, "N")
    try:
        send(port, f"world.init('{BL_PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
                   f"'Blood Modes World', 'the S10 modes'); return 'ok'")
        send(port, f"world.show('{BL_PAGE}'); return 'ok'")
        wait_for(port, lambda: send(port, "return world.getActiveWorldId()")
                 .strip('"') == BL_PAGE, 30.0)
        send(port, "return world.waitForInit(30)", timeout=35)
        ensure_chunks(port)
        bled = as_int(send(port, "return unit.spawn('acolyte', 4, 4, nil, "
                                 "'player', nil, nil, 'standard')"))
        nomad = as_int(send(port, "return unit.spawn('nomad_primitive', 5, 5, nil, "
                                  "'player', nil, nil, 'standard')"))
        squirrel = as_int(send(port, "return unit.spawn('red_squirrel', 6, 6, nil, "
                                     "'player', nil, nil, 'standard')"))
        deer = as_int(send(port, "return unit.spawn('white_tailed_deer', 7, 7, nil, "
                                 "'player', nil, nil, 'standard')"))
        # the S11a brain/mind rides the same fresh processes (no extra boots):
        # a second acolyte whose consciousness is pinned Nominal
        cons = as_int(send(port, "return unit.spawn('acolyte', 8, 8, nil, "
                                 "'player', nil, nil, 'standard')"))
        bear = as_int(send(port, "return unit.spawn('bear_brown', 9, 9, nil, "
                                 "'player', nil, nil, 'standard')"))
        uids = (bled, nomad, squirrel, deer, cons, bear)
        chk.ok(None not in uids and min(uids) >= 0, f"the six units spawned (got {uids})")
        time.sleep(0.6)

        def all_standing() -> bool:
            return all(lua_text(port, f"unit.getPose({u})") == "standing" for u in uids)

        def paused_standing() -> bool:
            if not all_standing():
                return False
            send(port, "require('scripts.pause').set(true); return 'ok'")
            if all_standing():
                return True
            send(port, "require('scripts.pause').set(false); return 'ok'")
            return False
        chk.ok(wait_for(port, paused_standing, 40.0),
               "every unit has settled into a standing pose and the engine is paused")

        # The shipped default through real spawns: blood is Simulated for every
        # bodied species and Unsupported for the bodyless one; the volume is
        # full and no shadow stat exists.
        for uid, mode in ((bled, "simulated"), (nomad, "simulated"),
                          (squirrel, "simulated"), (deer, "nil")):
            st = blood_state(port, uid)
            chk.ok(st["mode"] == mode and st["shadow"] is None,
                   f"unit {uid} resolves blood {mode} with no shadow stat (got {st})")
        # A raw write of the volume is refused in every mode.
        refused = send(port, f"return tostring(unit.setStat({bled}, 'blood', 1.0))").strip('"')
        chk.ok(refused == "false" and blood_state(port, bled)["shadow"] is None,
               f"a raw blood write is refused and stores nothing (got {refused!r})")

        # Bleed one Simulated unit to a genuine partial volume, then pause.
        send(port, f"unit.injure({bled}, 'torso', 'slash', 0.1, 1.0); return 'ok'")
        send(port, "require('scripts.pause').set(false); return 'ok'")

        def bled_partially() -> bool:
            # pause the instant the volume has dropped: on a slow runner a
            # poll interval is long enough for a deep cut to run the unit to
            # zero and kill it (#2837 CI evidence), which is no partial volume
            st = blood_state(port, bled)
            if st["cur"] is None or st["cur"] >= (st["max"] or 0) - 0.05:
                return False
            send(port, "require('scripts.pause').set(true); return 'ok'")
            return True
        chk.ok(wait_for(port, bled_partially, 30.0),
               "the Simulated unit bled to a partial volume")
        send(port, "require('scripts.pause').set(true); return 'ok'")
        partial = blood_state(port, bled)
        chk.ok(partial["cur"] is not None and 0.0 < partial["cur"] < partial["max"],
               f"the bled unit holds a partial, non-zero volume (got {partial})")
        # Pin the nomad Nominal through the facade: the field is written canonical.
        pinned = send(port, f"local ok, ch = require('scripts.physiology')"
                            f".setMode({nomad}, 'blood', 'nominal'); "
                            f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
        chk.ok(pinned == "true|true",
               f"the nomad's blood is pinned Nominal through the facade (got {pinned!r})")
        st = blood_state(port, nomad)
        chk.ok(st["mode"] == "nominal" and st["cur"] == st["max"] and st["frac"] == 1.0
               and st["shadow"] is None,
               f"the Nominal nomad reads a full volume, fraction 1.0, no shadow (got {st})")
        mind_saved = mind_setup(chk, port, bled, nomad, squirrel, bear, cons, deer)
        save_and_wait(chk, port, BL_PAGE, "bl1")
        quit_engine(port, proc)
        proc = None
        # hand the old-default save to the shipped-registry engines
        shutil.copytree(os.path.join(legacy, "saves", "bl1"),
                        os.path.join(root, "saves", "bl1"))

        print("=== blood engine O: load 'bl1', bleed the Nominal unit, switch, save 'bl2' ===")
        proc = engagement_boot(root, tmpdir, port, "O")
        engagement_load(chk, port, "bl1")
        mind_loaded(chk, port, tmpdir, "O", bled, nomad, squirrel, bear, cons, deer,
                    mind_saved)
        mind_defaults(chk, port)
        st = blood_state(port, bled)
        chk.ok(partial["cur"] is not None and st["cur"] is not None
               and abs(st["cur"] - partial["cur"]) < 1e-3 and st["mode"] == "simulated"
               and st["shadow"] is None,
               f"the bled unit's partial volume reloads exactly, Simulated (got {st}, saved {partial})")
        st = blood_state(port, nomad)
        chk.ok(st["mode"] == "nominal" and st["pin"] == "nominal" and st["cur"] == st["max"]
               and st["frac"] == 1.0,
               f"the Nominal nomad reloads Nominal and full with its pin (got {st})")
        st = blood_state(port, deer)
        chk.ok(st["mode"] == "nil" and st["read"] is None and st["shadow"] is None,
               f"the bodyless unit reloads Unsupported, reading nothing (got {st})")
        # A severe cut on the Nominal unit: it must survive with a full volume
        # while the Simulated unit keeps bleeding.
        send(port, f"unit.injure({nomad}, 'torso', 'slash', 0.6, 1.0); return 'ok'")
        # a fresh cut on the Simulated unit too: its saved wound may have clotted
        send(port, f"unit.injure({bled}, 'torso', 'slash', 0.1, 1.0); return 'ok'")
        send(port, "require('scripts.pause').set(false); return 'ok'")
        chk.ok(wait_for(port, lambda: (blood_state(port, bled)["cur"] or 99) < partial["cur"] - 0.02, 30.0),
               "the Simulated unit drains after the reload and a fresh cut")
        st = blood_state(port, nomad)
        chk.ok(st["cur"] == st["max"] and st["pose"] != "dead"
               and (lua_number(port, f"unit.getBlood({nomad}).bleedRate") or 0) > 0,
               f"the severely cut Nominal unit holds a full volume, alive, with a live bleed rate (got {st})")
        send(port, "require('scripts.pause').set(true); return 'ok'")
        switched = send(port, f"local ok, ch = require('scripts.physiology')"
                              f".setMode({nomad}, 'blood', 'simulated'); "
                              f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
        chk.ok(switched == "true|true",
               f"the Nominal unit is switched back to Simulated (got {switched!r})")
        st = blood_state(port, nomad)
        chk.ok(st["mode"] == "simulated" and st["cur"] == st["max"] and st["shadow"] is None,
               f"the switch seeded the full maximum into the field (got {st})")
        saved = blood_state(port, nomad)
        mind_switched = mind_switch(chk, port, nomad)
        save_and_wait(chk, port, BL_PAGE, "bl2")
        quit_engine(port, proc)
        proc = None

        print("=== blood engine P: load 'bl2' ===")
        proc = engagement_boot(root, tmpdir, port, "P")
        engagement_load(chk, port, "bl2")
        st = blood_state(port, nomad)
        chk.ok(st["mode"] == "simulated" and st["pin"] == "simulated" and st["shadow"] is None
               and saved["cur"] is not None and st["cur"] is not None
               and abs(st["cur"] - saved["cur"]) < 1e-3,
               f"the switched unit reloads Simulated, pinned, volume exact (got {st})")
        mind_second(chk, port, tmpdir, "P", bled, nomad, squirrel, bear, mind_switched,
                    mind_saved)
        quit_engine(port, proc)
        proc = None
    finally:
        if proc is not None:
            quit_engine(port, proc)


MIND_KEYS = ("mood", "emotional_pain", "caffeine", "mental_state", "mental_since",
             "mental_until", "mental_next_roll_at", "mental_cooldown_until",
             "mental_high_since", "mental_break_behavior", "thought_next_at")
DERIVED_BRAIN_KEYS = ("consciousness", "concentration", "state_of_mind")


def mind_state(port: int, uid: int) -> dict:
    """What a unit stores, resolves and reads for the S11a brain and mind, in
    ONE console round trip (a nil value is a missing key, read back as None)."""
    keys = ", ".join(f"'{k}'" for k in MIND_KEYS)
    derived = ", ".join(f"'{k}'" for k in DERIVED_BRAIN_KEYS)
    raw = send_json(port,
        f"local u = {uid}; local o = {{ stored = {{}}, derived_stored = {{}}, read = {{}} }}; "
        f"o.mind = tostring(unit.getPhysioMode(u, 'mind')); "
        f"o.mind_pin = tostring(unit.getPhysioOverride(u, 'mind')); "
        f"o.cons_mode = tostring(unit.getPhysioMode(u, 'consciousness')); "
        f"o.cons_pin = tostring(unit.getPhysioOverride(u, 'consciousness')); "
        f"for _, k in ipairs({{ {keys} }}) do o.stored[k] = unit.getStatBase(u, k) end; "
        f"for _, k in ipairs({{ {derived} }}) do o.derived_stored[k] = unit.getStatBase(u, k) end; "
        f"for _, k in ipairs({{ 'mood', 'mental_state', {derived} }}) do "
        f"o.read[k] = unit.readPhysio(u, k) end; return o")
    out = {"mind": raw.get("mind"), "mind_pin": raw.get("mind_pin"),
           "cons_mode": raw.get("cons_mode"), "cons_pin": raw.get("cons_pin")}
    out["stored"] = {k: (raw.get("stored") or {}).get(k) for k in MIND_KEYS}
    out["derived_stored"] = {k: (raw.get("derived_stored") or {}).get(k)
                             for k in DERIVED_BRAIN_KEYS}
    out["read"] = {k: (raw.get("read") or {}).get(k)
                   for k in ("mood", "mental_state") + DERIVED_BRAIN_KEYS}
    return out


NORMALIZATION_LOG = "normalized physiology of unit"


def normalization_lines(tmpdir: str, letter: str) -> list:
    """The load's logged physiology repairs (one line per repaired unit) from an
    engine's own log."""
    try:
        text = Path(os.path.join(tmpdir, f"engine{letter}.log")).read_text(errors="replace")
    except OSError:
        return []
    return [ln for ln in text.splitlines() if NORMALIZATION_LOG in ln]


def commit_episode(port: int, uid: int, behavior: int, state: int = 2) -> str:
    return send(port, f"local now = engine.gameTime(); "
                      f"local ok, why = unit.commitPhysio({uid}, {{ "
                      f"mood = 0.31, emotional_pain = 0.2, caffeine = 0.4, "
                      f"mental_state = {state}, mental_since = now - 10, "
                      f"mental_until = now + 1000, mental_next_roll_at = now - 2, "
                      f"mental_break_behavior = {behavior}, thought_next_at = now + 40 }}); "
                      f"return tostring(ok) .. '|' .. tostring(why)").strip('"')


def mind_setup(chk: Checks, port: int, episode: int, nomad: int, squirrel: int,
               bear: int, conspin: int, deer: int) -> dict:
    """Engine N (the PRE-S11b registry; paused, every unit standing): the old
    all-Simulated mind through real spawns, a mid-episode mind committed as one
    atomic batch on each of an acolyte, a nomad, a squirrel and a bear whose
    mind is PINNED Simulated, and an acolyte whose mind and consciousness are
    pinned Nominal. Returns each episode unit's state."""
    for uid, want in ((episode, "simulated"), (nomad, "simulated"),
                      (squirrel, "simulated"), (bear, "simulated"),
                      (conspin, "simulated"), (deer, "nil")):
        st = mind_state(port, uid)
        chk.ok(st["mind"] == want and st["cons_mode"] == want,
               f"unit {uid} resolves the old mind and consciousness {want} (got "
               f"{st['mind']}, {st['cons_mode']})")
        chk.ok(all(v is None for v in st["derived_stored"].values()),
               f"unit {uid} stores none of the derived brain values (got {st['derived_stored']})")
        seeded = all(v is not None for v in st["stored"].values())
        chk.ok(seeded == (want == "simulated"),
               f"unit {uid} holds every mind key exactly when its mind is Simulated "
               f"(got {st['stored']})")
    refused = send(port, f"return tostring(unit.setStat({episode}, 'concentration', 0.5))").strip('"')
    chk.ok(refused == "false" and mind_state(port, episode)["derived_stored"]["concentration"] is None,
           f"a raw concentration write is refused and stores nothing (got {refused!r})")
    # the bear's mind is PINNED Simulated: intent equal to the old default
    pinned = send(port, f"local ok, ch = require('scripts.physiology')"
                        f".setMode({bear}, 'mind', 'simulated'); "
                        f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
    chk.ok(pinned.startswith("true|"), f"the bear's mind is pinned Simulated (got {pinned!r})")
    chk.ok(mind_state(port, bear)["mind_pin"] == "simulated",
           "the bear's Simulated pin is stored as an override")
    for uid, behavior in ((episode, 3), (nomad, 1), (squirrel, 0), (bear, 3)):
        committed = commit_episode(port, uid, behavior)
        chk.ok(committed == "true|nil",
               f"unit {uid}'s break episode commits as one batch (got {committed!r})")
    saved = {uid: mind_state(port, uid) for uid in (episode, nomad, squirrel, bear)}
    pinned = send(port, f"local ok, ch = require('scripts.physiology')"
                        f".setMode({conspin}, 'mind', 'nominal'); "
                        f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
    chk.ok(pinned == "true|true", f"unit {conspin}'s mind is pinned Nominal (got {pinned!r})")
    pinned = send(port, f"local ok, ch = require('scripts.physiology')"
                        f".setMode({conspin}, 'consciousness', 'nominal'); "
                        f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
    chk.ok(pinned == "true|true", f"unit {conspin}'s consciousness is pinned Nominal (got {pinned!r})")
    st = mind_state(port, conspin)
    chk.ok(st["mind"] == "nominal" and all(v is None for v in st["stored"].values())
           and st["read"]["mood"] == 1.0 and st["read"]["mental_state"] == 0.0
           and st["cons_mode"] == "nominal" and st["read"]["consciousness"] == 1.0,
           f"the Nominal-pinned acolyte holds none of its mind keys and reads lucid (got {st})")
    return saved


def mind_loaded(chk: Checks, port: int, tmpdir: str, letter: str, episode: int,
                nomad: int, squirrel: int, bear: int, conspin: int, deer: int,
                saved: dict) -> None:
    """Engine O (the SHIPPED registry), right after loading the old-default save
    (paused, NO tick has run): the unpinned nomad and squirrel are normalized
    to the new Nominal default (keys and episode gone, ONE logged repair each),
    the pinned-Simulated bear and the acolyte keep every key exactly, the
    pinned-Nominal acolyte and the bodyless unit are as saved."""
    chk.ok(wait_for(port, lambda: len(normalization_lines(tmpdir, letter)) >= 2, 20.0),
           "the load logged the changed-default repairs")
    lines = normalization_lines(tmpdir, letter)
    chk.ok(len(lines) == 2 and all("mind" in ln for ln in lines),
           f"exactly two units were repaired, each logged once and naming the mind system "
           f"(got {len(lines)}: {lines})")
    for uid, label in ((nomad, "nomad"), (squirrel, "squirrel")):
        st = mind_state(port, uid)
        chk.ok(st["mind"] == "nominal" and st["mind_pin"] == "nil"
               and all(v is None for v in st["stored"].values())
               and st["read"]["mood"] == 1.0 and st["read"]["mental_state"] == 0.0
               and st["cons_mode"] == "simulated",
               f"the unpinned {label} loads Nominal, unpinned, holding none of its mind keys, "
               f"reading mood 1 and STABLE, consciousness still Simulated (got {st})")
        chk.ok(st["read"]["state_of_mind"] is not None and st["read"]["consciousness"] is not None
               and abs(st["read"]["state_of_mind"] - st["read"]["consciousness"]) < 1e-3
               and all(v is None for v in st["derived_stored"].values()),
               f"the paused {label} reads state_of_mind = consciousness, nothing derived stored "
               f"(got {st['read']})")
    for uid, label in ((episode, "acolyte"), (bear, "pinned bear")):
        st = mind_state(port, uid)
        same = all(st["stored"][k] is not None and saved[uid]["stored"][k] is not None
                   and abs(st["stored"][k] - saved[uid]["stored"][k]) < 1e-3
                   for k in MIND_KEYS)
        chk.ok(same and st["mind"] == "simulated",
               f"the mid-episode {label} reloads every mind key exactly, Simulated "
               f"(got {st['stored']}, saved {saved[uid]['stored']})")
        chk.ok(all(v is None for v in st["derived_stored"].values()),
               f"no derived value is stored for the {label} after the load")
    st = mind_state(port, bear)
    chk.ok(st["mind_pin"] == "simulated",
           f"the bear keeps its Simulated pin across the default change (got {st['mind_pin']})")
    st = mind_state(port, episode)
    wellbeing = 0.31 - 0.5 * 0.2
    chk.ok(st["read"]["state_of_mind"] is not None
           and abs(st["read"]["state_of_mind"] - min(st["read"]["consciousness"] or 1.0, wellbeing)) < 1e-3,
           f"the paused acolyte reads the canonical derived state of mind from its loaded inputs "
           f"(got {st['read']})")
    st = mind_state(port, conspin)
    chk.ok(st["mind"] == "nominal" and st["mind_pin"] == "nominal"
           and st["cons_mode"] == "nominal" and st["cons_pin"] == "nominal"
           and all(v is None for v in st["stored"].values())
           and st["read"]["consciousness"] == 1.0,
           f"the Nominal-pinned acolyte reloads Nominal with both pins, reading 1.0 (got {st})")
    st = mind_state(port, deer)
    chk.ok(st["mind"] == "nil" and st["read"]["consciousness"] is None
           and all(v is None for v in st["stored"].values()),
           f"the bodyless unit reloads Unsupported, reading nothing (got {st})")


def mind_switch(chk: Checks, port: int, nomad: int) -> dict:
    """Engine O, paused, before the second save: the default-Nominal nomad pinned
    Simulated seeds every key in one transition (the thought clock re-rolled
    within 30 s) -- a genuine Nominal-to-Simulated pin on a shipped species."""
    switched = send(port, f"local ok, ch = require('scripts.physiology')"
                          f".setMode({nomad}, 'mind', 'simulated'); "
                          f"return tostring(ok) .. '|' .. tostring(ch)").strip('"')
    chk.ok(switched == "true|true", f"the Nominal nomad's mind is pinned Simulated (got {switched!r})")
    st = mind_state(port, nomad)
    now = lua_number(port, "engine.gameTime()")
    chk.ok(st["mind"] == "simulated" and st["mind_pin"] == "simulated"
           and st["stored"]["mental_state"] == 0.0 and st["stored"]["mental_since"] is not None
           and now is not None and abs(st["stored"]["mental_since"] - now) < 1e-2
           and st["stored"]["mental_break_behavior"] == 0.0
           and st["stored"]["thought_next_at"] is not None
           and now <= st["stored"]["thought_next_at"] <= now + 30.001,
           f"the pin seeded every key (got {st['stored']}, now={now})")
    return mind_state(port, nomad)


def mind_defaults(chk: Checks, port: int) -> None:
    """Engine O: the SD-23 defaults through FRESH spawns of the shipped
    species (no override anywhere): only the acolyte holds mind keys; the
    bear, squirrel, technomule and nomad hold none and resolve mind Nominal,
    consciousness Simulated."""
    spots = (("acolyte", 20, 20, "simulated"), ("bear_brown", 21, 21, "nominal"),
             ("red_squirrel", 22, 22, "nominal"), ("technomule", 23, 23, "nominal"),
             ("nomad_primitive", 24, 24, "nominal"))
    spawned = []
    for name, x, y, want in spots:
        uid = as_int(send(port, f"return unit.spawn('{name}', {x}, {y}, nil, "
                                f"'player', nil, nil, 'standard')"))
        chk.ok(uid is not None and uid >= 0, f"a fresh {name} spawned (got {uid})")
        spawned.append((name, uid, want))
    for name, uid, want in spawned:
        if uid is None:
            continue
        # the engine stays paused: wait for the spawn command to land
        chk.ok(wait_for(port, lambda u=uid: send(
                   port, f"return tostring(unit.getInfo({u}) ~= nil)").strip('"') == "true", 10.0),
               f"the fresh {name} exists")
        st = mind_state(port, uid)
        has_keys = all(v is not None for v in st["stored"].values())
        none_keys = all(v is None for v in st["stored"].values())
        chk.ok(st["mind"] == want and st["mind_pin"] == "nil" and st["cons_mode"] == "simulated"
               and (has_keys if want == "simulated" else none_keys)
               and all(v is None for v in st["derived_stored"].values()),
               f"a fresh {name} resolves mind {want}, consciousness simulated, unpinned, "
               f"holding the mind keys exactly when Simulated (got {st['mind']}, "
               f"{st['cons_mode']}, {st['stored']})")


def mind_second(chk: Checks, port: int, tmpdir: str, letter: str, episode: int,
                nomad: int, squirrel: int, bear: int, switched_saved: dict,
                saved: dict) -> None:
    """Engine P: a second generation repairs NOTHING, the pinned nomad's seeds
    and the episodes survive exactly, and the normalized squirrel stays
    Nominal."""
    chk.ok(len(normalization_lines(tmpdir, letter)) == 0,
           f"re-saving and reloading logs zero further repairs (got "
           f"{normalization_lines(tmpdir, letter)})")
    st = mind_state(port, nomad)
    same = all(st["stored"][k] is not None and switched_saved["stored"][k] is not None
               and abs(st["stored"][k] - switched_saved["stored"][k]) < 1e-3
               for k in MIND_KEYS)
    chk.ok(same and st["mind"] == "simulated" and st["mind_pin"] == "simulated",
           f"the pinned nomad reloads Simulated, pinned, every seeded key exact "
           f"(got {st['stored']}, saved {switched_saved['stored']})")
    st = mind_state(port, squirrel)
    chk.ok(st["mind"] == "nominal" and all(v is None for v in st["stored"].values()),
           f"the normalized squirrel stays Nominal holding none of its keys (got {st})")
    for uid, label in ((episode, "acolyte"), (bear, "pinned bear")):
        st = mind_state(port, uid)
        # the unpaused stretch in engine O drifts mood and the clocks; the
        # EPISODE (state, end, behavior) is what must survive exactly
        episode_keys = ("mental_state", "mental_until", "mental_break_behavior")
        same = all(st["stored"][k] is not None and saved[uid]["stored"][k] is not None
                   and abs(st["stored"][k] - saved[uid]["stored"][k]) < 1e-3
                   for k in episode_keys)
        chk.ok(same and st["mind"] == "simulated" and st["stored"]["mental_state"] == 2.0,
               f"the {label}'s episode survives a second generation exactly (got {st['stored']})")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9277)
    args = ap.parse_args()
    port = args.port
    chk = Checks()

    tmpdir = tempfile.mkdtemp(prefix="persistence_contract_probe_")
    proc = None
    try:
        root = make_isolated_root(tmpdir)

        # Every decode below -- the map-mode check and the
        # four-generation comparison -- execs the compiled save codec
        # (#2274). Resolved HERE, before the first engine boot: under
        # tools/run_probes.py it is already exported and this costs
        # nothing, and run by hand it keeps the build out of the window
        # that times engine work (#1913's argument, applied to the
        # second binary).
        prepare_decoder()

        # ── Engine A: build the scenario, save ───────────────────────────
        print("=== engine A: build scenario + save 'gen1' ===")
        proc = boot_probe(root, port, os.path.join(tmpdir, "engineA.log"))
        portal_bid, atk, tgt = build_scenario(chk, port)
        save_and_wait(chk, port, PAGE, "gen1")
        gen1_path = os.path.join(root, "saves", "gen1", "world.synworld")
        chk.ok(wait_for_file(gen1_path, seconds=5.0),
               f"save file appeared at {gen1_path}")
        # #767 requirement 5: exclude slot name/timestamp/path from the
        # structural comparison, but smTimestamp is still a real, well-
        # formed field on the wire -- check it exists and looks like a
        # timestamp (contract SS12's metadata coverage-map row).
        listing = send_json(port, "return engine.listSaves()")
        gen1_entry = next((s for s in listing if s.get("name") == "gen1"), None) \
            if isinstance(listing, list) else None
        chk.ok(bool(gen1_entry and gen1_entry.get("timestamp")),
               f"engine.listSaves() reports a well-formed timestamp for 'gen1' "
               f"(got {gen1_entry!r})")
        assert_nondefault_map_mode(chk, tmpdir, gen1_path, PAGE)
        quit_engine(port, proc)
        proc = None

        # ── Requirement 9: at least THREE fresh-process save->load->save
        # cycles. Engine A's save above is the INITIAL save, not itself a
        # cycle -- each of the three engines below is one complete fresh-
        # process "load prior generation -> save the next one" cycle
        # (round-2 review: two engines only complete two cycles, not
        # three) -----------------------------------------------------────
        gen_paths = [gen1_path]
        prev_slot = "gen1"
        for i, letter in enumerate("BCD", start=2):
            next_slot = f"gen{i}"
            print(f"=== engine {letter}: fresh process, load '{prev_slot}', "
                  f"save '{next_slot}' ===")
            proc = boot_probe(root, port, os.path.join(tmpdir, f"engine{letter}.log"))
            bootstrap_defs(port)
            load_ai_stack(port)

            if letter == "B":
                # Round-5 review: prove a load REPLACES the whole session
                # rather than merging into it -- a page that exists only
                # in this fresh, pre-load session (never part of the
                # save being loaded) must NOT survive publication (#763,
                # mirrors transactional_load_probe.py's "complete
                # replacement (not a merge)" scenario, but exercised here
                # inside THIS suite's own isolated resource root/scenario
                # rather than relying solely on that excluded-by-default
                # cross-probe). A no-generator arena is enough -- its
                # own content is irrelevant, only its registration is.
                send(port, f"world.initArena('{GHOST_PAGE}'); return 'ok'")
                chk.ok(page_exists(port, GHOST_PAGE),
                       f"setup: pre-load-only page '{GHOST_PAGE}' is live "
                       f"before the load")

            loaded = send(port, f"return engine.loadSave('{prev_slot}')")
            chk.ok(loaded.strip() == "true",
                   f"engine.loadSave('{prev_slot}') accepted (got {loaded!r})")
            load_id = capture_request_id(port, "return engine.getLoadStatus()")
            chk.ok(load_id is not None,
                   f"engine.getLoadStatus() reports a request id right after "
                   f"loadSave('{prev_slot}')")
            published, status = wait_load_published(port, request_id=load_id)
            chk.ok(published, f"load transaction {load_id} published (status={status})")

            if letter == "B":
                chk.ok(not page_exists(port, GHOST_PAGE),
                       f"pre-load-only page '{GHOST_PAGE}' (never part of "
                       f"'{prev_slot}') did NOT survive the load -- a load "
                       f"replaces the whole session, it does not merge")

            assert_reset_policy(chk, port, f"after loading {prev_slot}")
            assert_faction_roster(chk, port, f"after loading {prev_slot}")
            attack_target = get_attack_target(port, atk)
            chk.ok(attack_target == tgt,
                   f"lua.unit_ai's attackTargetUid survived the fresh-process "
                   f"load of '{prev_slot}' ({attack_target} vs expected {tgt})")

            if letter == "B":
                # requirement 7's paused-stability dwell -- only needs
                # proving once, right after the first fresh-process load.
                before = sample_live_state(port, portal_bid, atk)
                time.sleep(2.0)
                after = sample_live_state(port, portal_bid, atk)
                chk.ok(before == after,
                       f"requirement 7: repeating the canonical live "
                       f"inspection during a 2s paused dwell produces the "
                       f"SAME persistent state ({before} -> {after})")

            save_and_wait(chk, port, PAGE, next_slot)
            next_path = os.path.join(root, "saves", next_slot, "world.synworld")
            chk.ok(wait_for_file(next_path, seconds=5.0),
                   f"save file appeared at {next_path}")
            gen_paths.append(next_path)

            if letter == "D":
                # #2515: a player spawned AFTER a fresh-process load (and
                # after this generation's save, so the compared files are
                # untouched) reads as the player through the adapter,
                # exactly as the restored attacker does -- the adapter's
                # 'player' answer requires the ONE local controller.
                fresh = as_int(send(port, "return unit.spawn('acolyte', 3, 3, 0, 'player')"))
                chk.ok(fresh is not None and fresh >= 0,
                       f"post-load player unit.spawn succeeded (got {fresh!r})")
                time.sleep(0.5)
                got = send(port, f"return unit.getFaction({fresh})").strip().strip('"')
                chk.ok(got == "player" and FACTION_ROSTER.get(atk) == "player",
                       f"a player spawned after the load shares the restored "
                       f"player's local controller (got {got!r})")

                # Unpause ONLY to confirm the default speed (requirement 8)
                # -- never comparing any subsequent random gameplay outcome.
                send(port, "require('scripts.pause').set(false); return 'ok'",
                     expect_result=False)
                time.sleep(0.5)
                ts = send(port, f"return world.getTimeScale('{PAGE}')")
                chk.ok(ts.strip() in ("1", "1.0"),
                       f"unpausing resumes at the default simulation speed "
                       f"(got {ts})")

            quit_engine(port, proc)
            proc = None
            prev_slot = next_slot

        # ── Requirement 1/5/9: canonical persistence-state comparison ───
        print(f"=== comparing {len(gen_paths)} generations through the real "
              f"production codec ===")
        ok, detail = compare_session_files([Path(p) for p in gen_paths])
        chk.ok(ok, f"all {len(gen_paths)} generations are structurally "
                   f"IDENTICAL (SessionSnapshot Eq + lua.* payload "
                   f"byte-equality) across three real fresh-process "
                   f"save->load->save cycles"
               + (f" -- {detail}" if not ok else ""))

        # ── S8a (#2834): the SD-20 engagement, focused ────────────────────
        engagement_cycles(chk, root, tmpdir, port)

        # ── S8b (#2835): exhaustion and sleep, focused ───────────────────
        exhaustion_sleep_cycles(chk, root, tmpdir, port)

        # ── S9 (#2836): cardio and the derived circulation, focused ──────
        cardio_circulation_cycles(chk, root, tmpdir, port)

        # ── S10 (#2837): blood in the dedicated field, focused ───────────
        blood_cycles(chk, root, tmpdir, port)

    finally:
        if proc is not None:
            quit_engine(port, proc)
        shutil.rmtree(tmpdir, ignore_errors=True)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: {chk.failed} check(s) failed")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
