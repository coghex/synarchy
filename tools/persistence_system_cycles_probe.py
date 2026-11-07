#!/usr/bin/env python3
"""Manual-only focused persistence system cycles (issue #2943).

These are the focused fresh-process cycles that used to run inside
``tools/persistence_contract_probe.py`` after its generic engines A-D. They
moved here, unchanged in what they assert, so the CI save gate (the core
probe: engines A-D plus the S11a mind-episode engines) stays cheap. This probe
is NOT in any CI gate; run it when you change combat engagement,
exhaustion/sleep, cardio, blood or relation-cause persistence, or their save
components:

  python3 tools/run_probes.py --only persistence_system_cycles --exact

Shared fixtures (isolated and legacy resource roots, ``engagement_boot``,
``engagement_load``, ``Checks``, ``save_and_wait`` ...) live in
``tools/persistence_probe_common.py``, which both probes import.

S8a (#2834) adds three FOCUSED fresh-process cycles for the SD-20 combat
engagement (``engagement_cycles`` below), because the generic cycles never
create the state: a nomad whose engagement was ADMITTED at a paused
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
a raw write of the volume is refused. (The S11a mind-episode checks that used
to ride on these engines run on their own engines in the CI core probe.)

#2548 adds three FOCUSED fresh-process cycles for the live directed relation
causes (``relation_cause_cycles`` below), because no production code adds a
cause yet and the generic cycles therefore always carry an EMPTY store.
A save is written by a real engine, one ``wildlife -> acolyte`` ally cause is
written into its bytes through the real session codec (``synarchy-save-codec
seed-tags``, the compiled-codec seeding the compatibility corpus uses), and
three fresh engines each load the previous generation, ask the PRODUCTION verbs
what a wildlife unit and a player acolyte now think of each other (ally in
one direction only, where the shipped table says hostile both ways) and
re-save. Every generation must still carry that one cause, and the four must
compare structurally identical.

Usage:
  python3 tools/persistence_system_cycles_probe.py [--port 9279]
"""
from __future__ import annotations

import argparse
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

import probe_evidence
from probelib import quit_engine, send
from persistence_probe_common import (
    PLATE_COUNT, SEED, WORLD_SIZE, Checks, as_int, engagement_boot,
    engagement_load, ensure_chunks, lua_number, lua_text, make_isolated_root,
    make_legacy_root, save_and_wait, wait_for, wait_for_file)
from persistence_snapshot import compare_session_files, prepare_decoder
from save_compat_audit import dump_canonical_summary
from save_compat_audit_codec import seed_fixture_tags

# ── S8a (#2834): the SD-20 combat engagement across fresh processes ─────────

ENG_PAGE = "engagement_page"


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


# ── #2548: live directed relation causes across fresh processes ────────────

RC_PAGE = "relation_cause_page"
#: The one cause the seeded save carries: wildlife regards acolyte as an ALLY.
#: The shipped base table makes that pair hostile both ways, and a live cause
#: outranks the base table (Unit.Faction.Profile, tier 1), so a restored store
#: shows as exactly one flipped direction: wildlife -> acolyte.
RC_CAUSE = {"owner": "diplomacy:probe_truce", "source": "wildlife",
            "target": "acolyte", "relation": "ally"}
RC_SEED = ('{"relationCauses":[{"owner":"diplomacy:probe_truce",'
           '"source":"wildlife","target":"acolyte","relation":"ally"}]}')


def relation_answers(port: int, a: int, b: int) -> str:
    """`relation,canAttack` of a -> b through the PRODUCTION unit verbs."""
    return lua_text(port, f"unit.relationFromTo({a}, {b}) .. ',' .. "
                           f"tostring(unit.canAttack({a}, {b}))")


def relation_causes_of(tmpdir: str, save_path: str, tag: str):
    """The live causes a saved file's canonical summary reads, or None."""
    out = Path(tmpdir) / f"relation_causes_{tag}.json"
    ok, tail = dump_canonical_summary(Path(save_path), out)
    if not ok:
        return None, tail
    import json
    return json.loads(out.read_text()).get("relationCauses", []), ""


def relation_cause_cycles(chk: Checks, root: str, tmpdir: str, port: int) -> None:
    """Three fresh processes carry one live relation cause through
    load -> re-save -> load, and the restored authority answers exactly."""
    print("=== relation-cause engine Q: baseline answers, save 'rcause1' ===")
    proc = engagement_boot(root, tmpdir, port, "Q")
    try:
        send(port, f"world.init('{RC_PAGE}', {SEED}, {WORLD_SIZE}, {PLATE_COUNT}, "
                   f"'Relation Cause World', 'the relation causes'); return 'ok'")
        send(port, f"world.show('{RC_PAGE}'); return 'ok'")
        wait_for(port, lambda: send(port, "return world.getActiveWorldId()")
                 .strip('"') == RC_PAGE, 30.0)
        send(port, "return world.waitForInit(30)", timeout=35)
        ensure_chunks(port)
        player = as_int(send(port, "return unit.spawn('acolyte', 4, 4, 0, 'player', "
                                   "nil, nil, 'standard')"))
        # A squirrel: its definition's own default tag is `wildlife`. An acolyte
        # spawned as 'wildlife' would keep the definition's `acolyte` tag
        # (D-26) and so be the player's ally by the shared tag.
        wild = as_int(send(port, "return unit.spawn('red_squirrel', 5, 4, 0, 'wildlife', "
                                 "nil, nil, 'standard')"))
        chk.ok(None not in (player, wild) and min(player, wild) >= 0,
               f"a player acolyte and a wildlife squirrel spawned (got {player}, {wild})")
        time.sleep(0.6)   # unit.spawn only queues; let the unit thread commit

        # Non-vacuity: with an EMPTY store the shipped table says hostile both
        # ways, so the ally answer below can only come from the restored cause.
        chk.ok(relation_answers(port, wild, player) == "hostile,true"
               and relation_answers(port, player, wild) == "hostile,true",
               f"with no cause the pair is hostile both ways (got "
               f"{relation_answers(port, wild, player)!r} / "
               f"{relation_answers(port, player, wild)!r})")
        send(port, "require('scripts.pause').set(true); return 'ok'")
        save_and_wait(chk, port, RC_PAGE, "rcause1")
        quit_engine(port, proc)
        proc = None

        # No production code adds a cause, so the store is seeded into the
        # saved bytes through the real session decoder and encoder.
        slot1 = os.path.join(root, "saves", "rcause1", "world.synworld")
        chk.ok(wait_for_file(slot1, seconds=5.0), f"save file appeared at {slot1}")
        spec = os.path.join(tmpdir, "relation_cause_seed.json")
        with open(spec, "w") as fh:
            fh.write(RC_SEED)
        seeded, tail = seed_fixture_tags(Path(slot1), Path(spec), Path(slot1))
        chk.ok(seeded, f"the cause was seeded into 'rcause1' through the real codec ({tail})")

        paths = [slot1]
        prev, expected_answers = "rcause1", ("ally,false", "hostile,true")
        for letter, nxt in (("R", "rcause2"), ("S", "rcause3"), ("T", "rcause4")):
            print(f"=== relation-cause engine {letter}: load '{prev}', save '{nxt}' ===")
            proc = engagement_boot(root, tmpdir, port, letter)
            engagement_load(chk, port, prev)
            got = (relation_answers(port, wild, player),
                   relation_answers(port, player, wild))
            chk.ok(got == expected_answers,
                   f"after loading '{prev}' the wildlife squirrel regards the acolyte as an ally "
                   f"(and may not attack it) while the acolyte still regards wildlife "
                   f"as hostile -- the restored store is the live authority "
                   f"(got {got}, wanted {expected_answers})")
            save_and_wait(chk, port, RC_PAGE, nxt)
            path = os.path.join(root, "saves", nxt, "world.synworld")
            chk.ok(wait_for_file(path, seconds=5.0), f"save file appeared at {path}")
            paths.append(path)
            quit_engine(port, proc)
            proc = None
            prev = nxt

        # Non-empty before AND after: every generation, including the three
        # written by real engines, still carries exactly the seeded cause.
        for i, path in enumerate(paths, start=1):
            causes, why = relation_causes_of(tmpdir, path, str(i))
            chk.ok(causes == [RC_CAUSE],
                   f"generation rcause{i} carries exactly the one seeded cause "
                   f"(got {causes}{' -- ' + why if why else ''})")
        ok, detail = compare_session_files([Path(p) for p in paths])
        chk.ok(ok, f"all {len(paths)} relation-cause generations are structurally "
                   f"IDENTICAL across three real fresh-process cycles"
               + (f" -- {detail}" if not ok else ""))
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
        # a second acolyte and a bear, kept from the fixture as it was (the
        # S11a mind checks that used them now run on their own engines, #2943)
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
        save_and_wait(chk, port, BL_PAGE, "bl1")
        quit_engine(port, proc)
        proc = None
        # hand the old-default save to the shipped-registry engines
        shutil.copytree(os.path.join(legacy, "saves", "bl1"),
                        os.path.join(root, "saves", "bl1"))

        print("=== blood engine O: load 'bl1', bleed the Nominal unit, switch, save 'bl2' ===")
        proc = engagement_boot(root, tmpdir, port, "O")
        engagement_load(chk, port, "bl1")
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
        quit_engine(port, proc)
        proc = None
    finally:
        if proc is not None:
            quit_engine(port, proc)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9279)
    args = ap.parse_args()
    port = args.port
    chk = Checks()

    tmpdir = tempfile.mkdtemp(prefix="persistence_system_cycles_probe_")
    try:
        root = make_isolated_root(tmpdir)

        # The relation-cause cycle decodes saves through the compiled save
        # codec (#2274). Resolved HERE, before the first engine boot, so the
        # build stays out of the window that times engine work (#1913's
        # argument, applied to the second binary): under tools/run_probes.py
        # it is already exported and this costs nothing.
        prepare_decoder()

        # ── S8a (#2834): the SD-20 engagement, focused ────────────────────
        engagement_cycles(chk, root, tmpdir, port)

        # ── S8b (#2835): exhaustion and sleep, focused ───────────────────
        exhaustion_sleep_cycles(chk, root, tmpdir, port)

        # ── S9 (#2836): cardio and the derived circulation, focused ──────
        cardio_circulation_cycles(chk, root, tmpdir, port)

        # ── S10 (#2837): blood in the dedicated field, focused ───────────
        blood_cycles(chk, root, tmpdir, port)

        # ── #2548: live relation causes, seeded and restored ─────────────
        relation_cause_cycles(chk, root, tmpdir, port)

    finally:
        probe_evidence.discard_scratch(tmpdir)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: {chk.failed} check(s) failed")
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
