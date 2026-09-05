#!/usr/bin/env python3
"""The ``first-aid`` scenario of the manual gameplay runner (#925, #2151).

The same real starting roster on a repeatable movement arena. The
mule's pre-stocked first-aid kit is issued to the selected expedition
acolyte through the capacity-gated transfer (#1212), who then takes the
SHALLOWEST real fall off a ridge; the injury, the kit state and the
final unit state are reported. Roster setup, the kit issue and the
pre-fall baseline all run with the simulation STOPPED (#1218), so
ambient AI cannot move or injure the scout between the kit issue and
the fall; the fall itself and everything after it stay live and
observational. Treatment is then FOLLOWED to a named terminal condition
(#1221): the runner administers none of it, the real medic AI
claims/fetches/dresses on its own, and every sampling interval records
the patient's blood, aggregate bleed rate, dressing state, remaining
bandages and each treatment result until bleeding is controlled, the
bandages run out, the patient collapses or dies, or a bounded budget
expires. A kit refused for want of room, an untreated patient, a
collapse, a death and the timeout are all OBSERVATIONS, never failures.

This module owns the arena and ridge constants, the treatment policy
constants, kit inspection, the stopped pre-fall baseline, the real-fall
observation, the medic-treatment observer and sampler, terminal
evaluation, trajectory sampling, the first-aid report and
``run_first_aid``. It consumes ``gameplay_scenarios_support`` only —
never the expedition owner and never the façade — boots exactly one
engine and shuts it down through ``quit_engine`` in a ``finally``.
Selected and dispatched by ``python3 tools/gameplay_scenarios.py --test
first-aid``; not a probe, not a CI gate, and the exit status is never a
gameplay verdict (see the façade's docstring).
"""
from __future__ import annotations

import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from probelib import boot, quit_engine, send, send_json  # noqa: E402
from gameplay_scenarios_support import (  # noqa: E402
    ARRIVAL_TILES, LOG, ScenarioError, _as_list, _lua_uid_list, _num,
    bootstrap, checkpoint, command_move, fmt_inventory, fmt_wounds,
    is_paused, print_checkpoint, provisioning_load, release_simulation,
    snapshot, spawn_roster, transfer)

# --- first-aid arena --------------------------------------------------
# A ridge the scout walks off. It spans far more than the local A*
# search radius (16 tiles) in y, so there is no way around it inside the
# planner's horizon and the only route east is over the edge.
#
# The height is the SHALLOWEST real fall: `pcFallTriggerDrop` is 2, so a
# 2-z step is the first descent the mover turns into a Falling
# transition at all. Since the #998 correction that is a bruising but
# survivable landing, not the old pathological one: the checked contract
# in `test-headless/Test/Headless/Unit/Fall.hs` requires an average
# acolyte's 2-z fall to be all-blunt, non-vital, FEWER THAN 15 wounds,
# and to leave MORE THAN 30 SECONDS before a naive (untreated,
# unclotted) exsanguination. That margin is what makes treatment
# throughput observable at all (#1221) — there is time for the real
# medic AI to claim, close and dress. Taller drops are still avoided:
# they spend the margin before a medic can arrive, leaving nothing to
# treat. Whether the scout survives is still NOT asserted; that balance
# question belongs to #919.
FA_RIDGE_X1, FA_RIDGE_X2 = -4, 0
FA_RIDGE_Y1, FA_RIDGE_Y2 = -20, 20
FA_RIDGE_H = 2
FA_LOAM = 56                  # movement_arena's loam material id
FA_SCOUT_TILE = (-2, 0)       # on top of the ridge
FA_MEDIC_TILE = (2, 0)        # a bystander at the foot, beside the
                              # landing — staging only; the AI picks
                              # who actually treats (#1221)
FA_CAMP_X = 4                 # low ground east of the ridge
FA_LANDING = (1, 0)           # the tile the fall lands on

# --- first-aid treatment throughput (#1221) ---------------------------
# How long the AI-driven treatment is followed and how often the
# trajectory is sampled. The budget is a BOUND, never an expectation:
# reaching it is the `timeout` terminal condition, reported exactly like
# any other outcome.
FA_TREAT_BUDGET = 120.0       # seconds of AI treatment followed
FA_TREAT_INTERVAL = 2.0       # seconds between trajectory samples
# "Still bleeding" is the medic AI's OWN policy, not a cutoff invented
# here, so `controlled` means "the medic itself would stop treating".
# `treat_min_seep` is exported (scripts/unit_ai_tunables.lua) and is read
# LIVE off the running engine; these two are module-locals in
# scripts/unit_ai_medic.lua with no accessor, so they are restated here
# with their source named — change them there and here together.
FA_CLOT_ENOUGH = 0.85         # unit_ai_medic.lua CLOT_ENOUGH
FA_TREAT_SKIP_KINDS = ("concussion", "fracture", "internal")


def kit_state(port: int, uids: list[int]) -> str:
    """Where the first-aid kit is now and what is left in it."""
    raw = send_json(port,
                    "local o = {}; "
                    "for _, u in ipairs(" + _lua_uid_list(uids) + ") do "
                    "for _, it in ipairs(unit.getInventory(u) or {}) do "
                    "if it.defName == 'first_aid_kit' then "
                    "o[#o+1] = {holder = u, "
                    "contents = unit.getItemContents(u, 'first_aid_kit', "
                    "it.instanceId)} end end end; return o", timeout=20.0)
    entries = _as_list(raw)
    if not entries:
        return "no first-aid kit is held by any roster unit"
    out = []
    for entry in entries:
        if not isinstance(entry, dict):
            continue
        rows = _as_list(entry.get("contents"))
        items = ", ".join(
            f"{r.get('defName', '?')}x{int(_num(r.get('count'), 0) or 0)}"
            for r in rows if isinstance(r, dict)) or "(empty)"
        out.append(f"held by unit {entry.get('holder')}: {items}")
    return "; ".join(out)


def baseline_scout(port: int, scout: int, expect_kit: bool = True) -> dict:
    """Read the scout ONCE with the simulation stopped and prove the
    pre-fall preconditions against that very read (#1218).

    `expect_kit=False` says the kit was legitimately refused for want of
    carrying room (#1212) — a gameplay outcome the caller has already
    recorded as an observation — so the run continues into the fall
    rather than aborting, and the report says the precondition was
    dropped instead of claiming a kit the scout does not hold. The fall
    is still TREATED in that case: `unit.treatBleeding` improvises a
    makeshift tourniquet when the kit owner has no supplies
    (`src/Engine/Scripting/Lua/API/Units/Medical.hs`), which is itself
    worth observing. Every other precondition still holds.

    The whole point of the hold is that this snapshot is coherent, so the
    stopped state is re-checked on BOTH sides of the read: a value
    sampled while the sim was running again would describe a unit that
    has since moved. A violated precondition is a `ScenarioError` naming
    what drifted, because continuing into the fall from an invalid
    baseline is exactly what makes the before/after treatment
    observations ambiguous.

    Returns the validated snapshot for the caller to report as-is."""
    if not is_paused(port):
        raise ScenarioError(
            "the simulation was running when the pre-fall baseline was read — "
            "the checkpoint would not be a stopped-simulation snapshot")
    snap = snapshot(port, scout)
    if snap.get("gone"):
        raise ScenarioError(
            f"the scout (unit {scout}) no longer exists at the pre-fall "
            f"baseline")
    sx, sy = _num(snap.get("gridX")), _num(snap.get("gridY"))
    if sx is None or sy is None:
        raise ScenarioError(
            f"the scout (unit {scout}) reported no usable position at the "
            f"pre-fall baseline: gridX={snap.get('gridX')!r}, "
            f"gridY={snap.get('gridY')!r}")
    tx, ty = FA_SCOUT_TILE
    drift = ((sx - tx) ** 2 + (sy - ty) ** 2) ** 0.5
    if drift > ARRIVAL_TILES:
        raise ScenarioError(
            f"the scout (unit {scout}) left its staging tile before the "
            f"baseline: at ({sx:.2f}, {sy:.2f}), {drift:.2f} tiles from "
            f"{FA_SCOUT_TILE} (limit {ARRIVAL_TILES})")
    wounds = _as_list(snap.get("wounds"))
    if wounds:
        raise ScenarioError(
            f"the scout (unit {scout}) was already injured before the fall: "
            f"{len(wounds)} wound(s) — {fmt_wounds(snap)}")
    if expect_kit and not any(
            isinstance(item, dict) and item.get("defName") == "first_aid_kit"
            for item in _as_list(snap.get("inventory"))):
        raise ScenarioError(
            f"the scout (unit {scout}) is not carrying the issued "
            f"first_aid_kit at the pre-fall baseline (inventory: "
            f"{fmt_inventory(snap)})")
    if not is_paused(port):
        raise ScenarioError(
            "the simulation restarted while the pre-fall baseline was being "
            "read — the snapshot is not coherent")
    return snap


def descend_and_fall(port: int, scout: int, tx: float, ty: float,
                     budget: float, observations: list[str]) -> tuple:
    """Send the scout off the ridge and watch tightly for the landing.

    The descent ORDER is issued under the caller's simulation hold and
    the sim is released immediately afterwards (#1218), so no ambient-AI
    window ever separates the verified baseline from the intended
    command. Everything from that release on is live and observational.

    The landing is polled as fast as the console allows rather than on
    the leisurely walk_leg cadence, so the wound set the throughput
    observation (#1221) starts from is the fall's OWN output rather than
    one already partly dressed or partly self-clotted. Post-#998 a 2-z
    fall leaves well over a minute before a naive exsanguination
    (`test-headless/Test/Headless/Unit/Fall.hs`), so the tight poll buys
    a clean starting snapshot — it is no longer a race against a
    corpse."""
    command_move(port, [scout], tx, ty)
    release_simulation(port)
    deadline = time.time() + budget
    pose, nwounds, where = "unknown", 0, "unknown"
    while time.time() < deadline:
        raw = send(port,
                   "engine.setPaused(false); "
                   f"local i = unit.getInfo({scout}); "
                   "if not i then return '0|gone|unknown' end; "
                   f"local w = unit.getWounds({scout}); "
                   "return (w and #w or 0) .. '|' .. "
                   f"tostring(unit.getPose({scout})) .. '|' .. "
                   "string.format('(%.2f, %.2f, z %.0f)', i.gridX, i.gridY, "
                   "i.gridZ)")
        bits = raw.split("|")
        if len(bits) == 3:
            nwounds, pose, where = int(_num(bits[0], 0) or 0), bits[1], bits[2]
        if nwounds > 0 or pose in ("dead", "gone"):
            break
        time.sleep(0.2)
    if pose == "gone":
        observations.append("the scout no longer exists after the descent")
    elif nwounds == 0:
        observations.append(
            "the descent produced no wound at all within "
            f"{budget:.0f}s — the fall either did not happen or was survived "
            f"unscathed (scout at {where}, pose {pose})")
    return pose, nwounds, where


def dressed(port: int, uid: int) -> bool:
    """True once any wound carries a dressing (bandage < 1 = seep cut)."""
    raw = send(port,
               f"local ws = unit.getWounds({uid}); "
               "if type(ws) ~= 'table' then return 'no' end; "
               "for _, w in ipairs(ws) do "
               "if (w.bandage or 1) < 1 or (w.dressing or '') ~= '' "
               "then return 'yes' end end; return 'no'")
    return raw == "yes"


# --- treatment throughput (#1221) ------------------------------------
def install_treatment_observer(port: int) -> None:
    """Install the two scenario-local Lua pieces the throughput
    observation needs, both purely additive.

    1. A TRANSPARENT wrapper around `unit.treatBleeding`. It forwards the
       call to the original engine function with the caller's exact
       argument list (`table.pack`/`table.unpack` preserve arity, so a
       two-argument AI call still defaults its kit owner to the medic),
       appends the arguments and the returned table to a log, and returns
       the original results unchanged. No engine surface and no AI script
       is touched: the wrapper only WATCHES the calls the medic AI
       already makes, which is what lets every treatment counted below be
       the AI's own.
    2. `_SCEN_treatObs(patient, roster)`, one console round trip per
       trajectory sample. It reads the patient, classifies the wounds by
       the medic's own policy, counts the bandages wherever the kit ended
       up, finds who is actually claiming the patient, and DRAINS the
       log so each sample carries exactly the treatments made since the
       previous one.

    Installed once, before the fall, so no treatment can escape the log.
    Both guards are idempotent: re-installing must never double-wrap (a
    second wrapper would log every call twice and report a treatment
    throughput the AI never achieved)."""
    wrapper = (
        "if not _SCEN_treatLog then "
        "_SCEN_treatLog = {}; "
        "local orig = unit.treatBleeding; "
        "_SCEN_treatOrig = orig; "
        "unit.treatBleeding = function(...) "
        "local a = table.pack(...); "
        "local r = table.pack(orig(table.unpack(a, 1, a.n))); "
        "local res = r[1]; "
        "_SCEN_treatLog[#_SCEN_treatLog + 1] = {medic = a[1], "
        "patient = a[2], kitOwner = a[3], returned = (res ~= nil), "
        "ok = res and res.ok, method = res and res.method, "
        "part = res and res.part, kind = res and res.kind, "
        "bandagesUsed = res and res.bandagesUsed, "
        "attempts = res and res.attempts, seep = res and res.seep, "
        "message = res and res.message}; "
        "return table.unpack(r, 1, r.n) end end; "
        "return 'ok'")
    if send(port, wrapper) != "ok":
        raise ScenarioError(
            "could not install the unit.treatBleeding observation wrapper — "
            "without it no AI treatment could be attributed")

    skip = ", ".join(f"{kind} = true" for kind in FA_TREAT_SKIP_KINDS)
    sampler = (
        "function _SCEN_treatObs(patient, roster) "
        "local out = {}; "
        "local okCfg, cfgMod = pcall(require, 'scripts.unit_ai_tunables'); "
        "local cfg = okCfg and cfgMod and cfgMod.acolyte or nil; "
        "local minSeep = (cfg and cfg.treat_min_seep) or 0.6; "
        f"local clotEnough = {FA_CLOT_ENOUGH}; "
        f"local skip = {{{skip}}}; "
        "out.minSeep = minSeep; out.clotEnough = clotEnough; "
        "out.seepLive = (cfg ~= nil); "
        "local i = unit.getInfo(patient); "
        "if not i then out.gone = true else "
        "out.gone = false; out.pose = unit.getPose(patient); "
        "out.knockedDown = i.knockedDown and true or false; "
        "out.blood = unit.getBlood(patient); "
        "local total, dressed, external, bleeding = 0, 0, 0, 0; "
        "for _, w in ipairs(unit.getWounds(patient) or {}) do "
        "total = total + 1; local band = w.bandage or 1; "
        "if band < 1 or (w.dressing or '') ~= '' then "
        "dressed = dressed + 1 end; "
        "if not skip[w.kind] then external = external + 1; "
        "if band > minSeep and (w.clot or 0) < clotEnough then "
        "bleeding = bleeding + 1 end end end; "
        "out.wounds = {total = total, dressed = dressed, "
        "undressed = total - dressed, external = external, "
        "bleeding = bleeding} end; "
        "local bandages, holders = 0, {}; "
        "for _, u in ipairs(roster) do "
        "for _, it in ipairs(unit.getInventory(u) or {}) do "
        "if it.kind == 'container' then "
        "for _, r in ipairs(unit.getItemContents(u, it.defName, "
        "it.instanceId) or {}) do "
        "if r.defName == 'bandage' and (r.count or 0) > 0 then "
        "bandages = bandages + r.count; "
        "holders[#holders + 1] = {holder = u, kit = it.defName, "
        "count = r.count} end end end end end; "
        "out.bandages = bandages; out.bandageHolders = holders; "
        "local ai = package.loaded['scripts.unit_ai']; "
        "local claimers = {}; "
        "if ai and ai.getState then for _, u in ipairs(roster) do "
        "local s = ai.getState(u); "
        "if s and s.treatClaim and s.treatClaim.patient == patient then "
        "claimers[#claimers + 1] = u end end end; "
        "out.claimers = claimers; "
        "out.treatments = _SCEN_treatLog or {}; _SCEN_treatLog = {}; "
        "return out end; return 'ok'")
    if send(port, sampler) != "ok":
        raise ScenarioError(
            "could not install the treatment-throughput sampler")


def _treatment_terminal(sample: dict) -> str | None:
    """Name the terminal condition this sample reaches, or None.

    Precedence, in this order: the patient is gone, dead, in a SURVIVAL
    collapse, bleeding-controlled, out of bandages. Patient-state
    outcomes come first because they end the treatment situation
    outright; every sample is printed either way, so a co-occurrence
    stays visible in the trajectory rather than being hidden by the
    label.

    The collapse test is `pose == "collapsed" AND knockedDown false`.
    Every real fall lands in `Collapsed` with a self-timed get-up
    pending (`src/Unit/Thread/Movement/Timers.hs`), and `knockedDown`
    (`src/Engine/Scripting/Lua/API/Units/List.hs`) is exactly the flag
    that distinguishes that ordinary knockdown from a survival collapse
    — without the flag test this would call every single run "collapsed"
    on its first sample."""
    if sample.get("gone"):
        return "gone"
    pose = str(sample.get("pose"))
    if pose == "dead":
        return "died"
    if pose == "collapsed" and not sample.get("knockedDown"):
        return "collapsed"
    wounds = sample.get("wounds") or {}
    if int(_num(wounds.get("bleeding"), 0) or 0) == 0:
        return "controlled"
    if int(_num(sample.get("bandages"), 0) or 0) == 0:
        return "supplies exhausted"
    return None


def follow_treatment(port: int, patient: int, roster: list[int], t0: float,
                     observations: list[str]) -> dict:
    """Follow the REAL medic AI treating `patient` to a terminal
    condition (#1221).

    Nothing here treats, commands or nudges the squad: the claim, the
    kit fetch and every `unit.treatBleeding` call are the AI's own, on
    the real `treat_ally` path with the real kit contents, the live
    blood tick and real clotting. The runner only keeps the simulation
    running and reads — which is why the section this returns is an
    OBSERVATION of throughput and not a staged demonstration of it.

    Which acolyte does the treating is DISCOVERED, never assumed:
    `bestMedicFor` ranks every available allied medic and each acolyte
    rolls its own `bleed_control` knowledge at spawn, so the claimant is
    read back out of the AI's own state and the treating unit out of the
    wrapper's log.

    Sampling is one console round trip per interval, and each sample
    drains the wrapper's log, so treatments made between two samples are
    attributed to the later row. Every sample becomes a row and the loop
    always breaks immediately after appending one, so the terminal row
    is printed even when no treatment call ever happened.

    Raises `ScenarioError` only when the console read itself breaks —
    a terminal condition is never a failure, whichever one it is."""
    rows: list[dict] = []
    deadline = time.time() + FA_TREAT_BUDGET
    terminal = None
    while True:
        # The unpause rides along on the sample's own round trip: a
        # `unit_warning` notification can auto-pause the whole sim
        # (config/notifications), which would silently freeze the very
        # AI this is measuring. Same defence poll_positions uses.
        sample = send_json(
            port,
            "engine.setPaused(false); "
            f"return _SCEN_treatObs({patient}, {_lua_uid_list(roster)})",
            timeout=20.0)
        if not isinstance(sample, dict):
            raise ScenarioError(
                f"the treatment-throughput read for unit {patient} failed: "
                f"{sample!r}")
        sample["elapsed"] = time.time() - t0
        sample["treatments"] = [
            rec for rec in _as_list(sample.get("treatments"))
            if isinstance(rec, dict)]
        rows.append(sample)
        terminal = _treatment_terminal(sample)
        if terminal is not None:
            break
        if time.time() >= deadline:
            terminal = "timeout"
            break
        time.sleep(FA_TREAT_INTERVAL)

    treatments = [rec for row in rows for rec in row["treatments"]]
    treating = sorted({int(rec["medic"]) for rec in treatments
                       if _num(rec.get("medic")) is not None})
    claimers = sorted({int(uid) for row in rows
                       for uid in _as_list(row.get("claimers"))
                       if _num(uid) is not None})
    start = rows[0]
    result = {
        "rows": rows,
        "terminal": terminal,
        "treatments": treatments,
        "treating": treating,
        "claimers": claimers,
        "start": start,
        "budget": FA_TREAT_BUDGET,
    }

    observations.append(
        "the AI-driven treatment ended in the terminal condition "
        f"'{terminal}' after {rows[-1]['elapsed'] - start['elapsed']:.1f}s of "
        f"following (budget {FA_TREAT_BUDGET:.0f}s), across "
        f"{len(rows)} sample(s) and {len(treatments)} AI treatment call(s)")
    observations.append(
        f"the treating unit(s) the AI actually used: "
        f"{treating or 'none — no unit.treatBleeding call was made'}; "
        f"the unit(s) seen holding a treat claim on the scout: "
        f"{claimers or 'none'}")
    if not claimers and not treating:
        observations.append(
            "no acolyte ever claimed or treated the scout — with every "
            "acolyte rolling its own bleed_control knowledge, a squad in "
            "which nobody qualifies is a real (and reportable) outcome")
    return result


def _as_dict(value) -> dict:
    """A console sub-table that may legitimately be absent (a dead
    patient reports no blood or wound block at all)."""
    return value if isinstance(value, dict) else {}


def _count(value) -> int:
    """A console count, absent-or-unreadable reading as 0."""
    return int(_num(value, 0) or 0)


def _fmt_num(value, places: int = 2) -> str:
    number = _num(value)
    return "?" if number is None else f"{number:.{places}f}"


def _fmt_treatment(rec: dict) -> str:
    medic, pat = rec.get("medic"), rec.get("patient")
    if not rec.get("returned"):
        return (f"medic {medic} -> patient {pat}: the call returned nil "
                f"(missing id arguments)")
    owner = rec.get("kitOwner")
    owner_s = str(owner) if owner is not None else "the medic (defaulted)"
    return (f"medic {medic} -> patient {pat} (kit owner {owner_s}): "
            f"ok={rec.get('ok')} method={rec.get('method')!r} "
            f"part={rec.get('part')!r} kind={rec.get('kind')!r} "
            f"bandagesUsed={rec.get('bandagesUsed')} "
            f"attempts={rec.get('attempts')} "
            f"seep={_fmt_num(rec.get('seep'), 3)} "
            f"message={rec.get('message')!r}")


def print_treatment_row(row: dict) -> None:
    """One trajectory sample: patient state, supply state, who is on it,
    and every treatment observed since the previous sample."""
    stamp = f"    t={row['elapsed']:7.1f}s  "
    if row.get("gone"):
        print(stamp + "the patient no longer exists")
    else:
        blood = _as_dict(row.get("blood"))
        wounds = _as_dict(row.get("wounds"))
        knocked = " (knocked down)" if row.get("knockedDown") else ""
        print(f"{stamp}pose {row.get('pose')}{knocked}  "
              f"blood {_fmt_num(blood.get('current'))}/"
              f"{_fmt_num(blood.get('max'))} L  "
              f"bleed {_fmt_num(blood.get('bleedRate'), 4)} L/s")
        pad = " " * len(stamp)
        print(f"{pad}wounds {_count(wounds.get('total'))} "
              f"(dressed {_count(wounds.get('dressed'))}, "
              f"undressed {_count(wounds.get('undressed'))}, "
              f"external {_count(wounds.get('external'))}, "
              f"still bleeding {_count(wounds.get('bleeding'))})")
    holders = ", ".join(
        f"{_count(h.get('count'))} in {h.get('kit')} on unit {h.get('holder')}"
        for h in _as_list(row.get("bandageHolders")) if isinstance(h, dict))
    claim = _as_list(row.get("claimers"))
    pad = " " * len(stamp)
    print(f"{pad}bandages {_count(row.get('bandages'))} "
          f"({holders or 'nowhere on the roster'})  "
          f"treat claim: {claim or 'none'}")
    for rec in row["treatments"]:
        print(f"{pad}  * {_fmt_treatment(rec)}")


def print_throughput(result: dict) -> None:
    start = result["start"]
    rows = result["rows"]
    wounds = _as_dict(start.get("wounds"))
    print("\n  -- treatment throughput (#1221): the real medic AI, followed "
          "to a terminal condition --")
    print("    every treatment below came through the AI's own treat_ally "
          "path (real claim, real kit,")
    print("    live blood tick, real clotting); the runner administered "
          "none of it and only read.")
    seep_src = ("live from scripts/unit_ai_tunables.lua"
                if start.get("seepLive") else "FALLBACK — tunables unreadable")
    print(f"    policy         a wound still counts as BLEEDING when its "
          f"kind is external (not "
          f"{'/'.join(FA_TREAT_SKIP_KINDS)}),")
    print(f"                   its seep multiplier is above treat_min_seep "
          f"({_fmt_num(start.get('minSeep'))}, {seep_src}) and its clot is "
          f"below {_fmt_num(start.get('clotEnough'))}")
    print("                   — scripts/unit_ai_medic.lua's own "
          "needsTreatment policy, so 'controlled' means the medic itself "
          "would stop")
    print(f"    at the landing {_count(wounds.get('total'))} wound(s) from "
          f"the fall, {_count(wounds.get('external'))} external, "
          f"{_count(wounds.get('bleeding'))} of them bleeding above the "
          f"clot cutoff")
    print(f"    treating unit  "
          f"{result['treating'] or 'none — no AI treatment call was made'}"
          f"  (claimed by {result['claimers'] or 'nobody'})")
    print(f"    calls          {len(result['treatments'])} AI "
          f"unit.treatBleeding call(s) over {len(rows)} sample(s), "
          f"{FA_TREAT_INTERVAL:.0f}s apart")
    print(f"    TERMINAL       {result['terminal']}  "
          f"(after {rows[-1]['elapsed'] - start['elapsed']:.1f}s of a "
          f"{result['budget']:.0f}s budget)")
    print("    -- trajectory --")
    for row in rows:
        print_treatment_row(row)


def run_first_aid(port: int) -> int:
    proc = boot(port, log=LOG, label="first-aid engine")
    t0 = time.time()
    observations: list[str] = []
    checkpoints: list[dict] = []
    try:
        bootstrap(port)
        # Before ANY unit exists, so no treatment can predate the log
        # (#1221). Purely additive: it wraps unit.treatBleeding to watch
        # the AI's calls and installs the trajectory sampler.
        install_treatment_observer(port)
        print("building the arena ridge ...")
        send(port, "require('scripts.movement_arena'); return 'ok'")
        send(port, "return require('scripts.movement_arena')"
                   ".buildCourse('flat')", timeout=90.0)
        page = send(port, "return require('scripts.movement_arena').page")
        send(port,
             f"require('scripts.movement_arena').plateau({FA_RIDGE_X1}, "
             f"{FA_RIDGE_Y1}, {FA_RIDGE_X2}, {FA_RIDGE_Y2}, {FA_RIDGE_H}, "
             f"{FA_LOAM}); return 'built'", timeout=180.0)
        # The tile edits are queued to the world thread; wait for the
        # ridge to actually stand before spawning onto it.
        top = None
        for _ in range(60):
            time.sleep(0.5)
            raw = send(port, f"local _s, t = world.getTerrainAt"
                             f"({FA_SCOUT_TILE[0]}, {FA_SCOUT_TILE[1]}); "
                             f"return t")
            top = _num(raw)
            if top is not None and top >= FA_RIDGE_H:
                break
        if top is None or top < FA_RIDGE_H:
            raise ScenarioError(
                f"the arena ridge never reached z {FA_RIDGE_H} "
                f"(got {top}) — cannot stage a real fall")

        camp_tiles = [FA_SCOUT_TILE,                        # the scout
                      FA_MEDIC_TILE,                        # a bystander
                      (FA_CAMP_X, 1), (FA_CAMP_X, -1),
                      (FA_CAMP_X + 1, 0),                   # 3 more acolytes
                      (FA_CAMP_X + 1, 1)]                   # technomule
        # From here to the descent order the simulation is STOPPED
        # (#1218): the roster materializes, the kit is issued and the
        # baseline is read with no ambient AI running, so nothing can
        # walk the scout off the ridge or injure it before the fall the
        # scenario means to stage. spawn_roster returns still holding.
        uids = spawn_roster(port, page, camp_tiles, hold=True)
        acolytes, mule = uids[:5], uids[5]
        # `bystander` is only the acolyte STAGED beside the landing — it
        # is not "the medic". Which acolyte treats is the AI's call:
        # every acolyte rolls its own bleed_control knowledge at spawn
        # and `bestMedicFor` ranks the whole squad, so the treating unit
        # is discovered from the AI's state and the wrapper's log rather
        # than asserted here (#1221).
        scout, bystander = acolytes[0], acolytes[1]

        # The kit issue runs through the same capacity-gated helper the
        # expedition provisioning uses (#1212). A refusal for want of
        # room is a GAMEPLAY outcome and stays an observation with exit
        # 0; only a broken source or a broken engine verb is a setup
        # failure worth aborting on.
        res = transfer(port, mule, scout, "first_aid_kit", 1)
        kit_issued = res.moved == 1
        if not kit_issued:
            if res.observational:
                observations.append(
                    f"the mule's stocked first-aid kit was not issued to "
                    f"the scout ({scout}): {res.detail}")
            else:
                raise ScenarioError(
                    "the mule's stocked first-aid kit could not be moved "
                    "onto the expedition acolyte via "
                    f"unit.transferItemToUnit — {res.detail}")
        # Read the post-issue load HERE, under the hold, rather than at
        # report time: by then the treatment has drawn bandages out of
        # the kit and the number would no longer describe the issue.
        kit_issue_load = provisioning_load(port, scout)
        kit_before = kit_state(port, uids)
        scout_baseline = baseline_scout(port, scout, expect_kit=kit_issued)
        pre_fall = checkpoint(port, "kit issued, before the fall",
                              [scout, bystander, mule], t0,
                              snaps={scout: scout_baseline},
                              paused=is_paused(port))
        if not pre_fall["paused"]:
            raise ScenarioError(
                "the simulation restarted while the pre-fall checkpoint was "
                "being recorded — the baseline is not a stopped-simulation "
                "snapshot")
        checkpoints.append(pre_fall)

        # descend_and_fall issues the order under the hold, then releases.
        print("  sending the scout off the ridge ...")
        pose, nwounds, landing = descend_and_fall(
            port, scout, FA_CAMP_X, 0, budget=90.0, observations=observations)

        # NOTHING is administered here (#1221). The runner used to fire
        # unit.treatBleeding itself the moment the injury landed, which
        # made the AI's own throughput unmeasurable — every dressing the
        # report showed was the runner's. Post-#998 a 2-z fall leaves
        # well over a minute before a naive exsanguination, so the real
        # treat_ally path can simply be followed instead.
        checkpoints.append(checkpoint(port, "the landing, before any "
                                      "treatment", [scout, bystander], t0))
        if pose == "dead":
            observations.append(
                "the scout was already dead when the injury was first "
                "observed, so the treatment observation starts from a corpse")

        print("  following the medic AI's treatment ...")
        throughput = follow_treatment(port, scout, uids, t0, observations)
        if dressed(port, scout):
            observations.append("the scout ends the run with a dressed wound")
        observations.append(
            f"the scout's final pose is "
            f"{send(port, f'return unit.getPose({scout})')}")
        checkpoints.append(checkpoint(port, "final state",
                                      [scout, bystander, mule], t0))

        print("\n" + "=" * 72)
        print("FIRST-AID SCENARIO REPORT")
        print("=" * 72)
        print(f"world          movement_arena page '{page}' with a "
              f"{FA_RIDGE_H}-z ridge at x {FA_RIDGE_X1}..{FA_RIDGE_X2}, "
              f"y {FA_RIDGE_Y1}..{FA_RIDGE_Y2}")
        print(f"roster         {len(acolytes)} acolytes + 1 technomule "
              f"(uids {uids}), player faction")
        print(f"scout          acolyte {scout} (fell); acolyte {bystander} "
              f"staged beside the landing")
        print( "               (which acolyte treats is NOT staged — see "
               "the treating unit in the throughput section)")
        print(f"supply point   technomule {mule}, stationary at camp")
        print("kit issue      " + (
            "1x first_aid_kit moved mule -> scout via "
            "unit.transferItemToUnit,\n               gated on that "
            f"instance's own weight — scout at {kit_issue_load} once "
            "issued"
            if kit_issued else
            f"REFUSED — {res.detail}\n               (a capacity refusal "
            "is an observation, not a setup failure)"))
        print("baseline       spawn, provisioning and the pre-fall "
              "checkpoint all ran with the simulation STOPPED "
              "(engine.isPaused() == true, re-read either side of the "
              "snapshot);")
        print(f"               the scout was verified within "
              f"{ARRIVAL_TILES} tiles of {FA_SCOUT_TILE} and unwounded"
              + (", and holding the kit," if kit_issued else
                 " (the kit precondition was dropped with the refused "
                 "issue),")
              + "\n               before the descent order released it")
        print(f"kit before     {kit_before}")
        print(f"kit after      {kit_state(port, uids)}")
        print(f"landing        {landing}, pose {pose}, {nwounds} wound(s); "
              f"expected near {FA_LANDING}")
        print(f"treatment      administered entirely by the medic AI — "
              f"terminal condition '{throughput['terminal']}' after "
              f"{len(throughput['treatments'])} AI call(s)")
        for cp in checkpoints:
            print_checkpoint(cp)
        print_throughput(throughput)
        print("\n  -- observations --")
        for line in observations or ["nothing out of the ordinary"]:
            print(f"    * {line}")
        print("\n  NOTE: this report is an observation, not a verdict. "
              "Whether the\n  treatment succeeded or the scout survived is "
              "NOT asserted here.")
        return 0
    finally:
        quit_engine(port, proc)
