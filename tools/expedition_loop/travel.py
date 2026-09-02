#!/usr/bin/env python3
"""[travel] and [control] — one shared leg, and what it proves (#2092).

The two stages are one owner because they are one measurement. `travel`
walks both travellers over an identical leg and captures the paired
departure and arrival observations; `control` scores those same
retained observations, in engine A, before the fresh process ever boots.
Splitting them would put the sampling and the judgement in different
modules with the samples travelling between them for no gain — and
requirement 4 groups "travel, sight-based discovery, paired sampling,
and the survival control" as one owner for exactly that reason.

Nothing here re-reads a metric at scoring time: `control` consumes
`st.depart` / `st.arrive` and their ration counts exactly as `travel`
captured them, inside the paused windows, which is what makes the
comparison a single coherent instant rather than two round trips with a
running simulation in between.

The departure calibration this stage opens with — the muster and the
shared hunger deficit — belongs to `prepare`, whose documented public
interface it is; see that module.
"""
from __future__ import annotations

import time

from probelib import poll_until, send

from .constants import (MAX_START_SEPARATION, MAX_START_SPREAD,
                        MIN_STOMACH_DELTA, PAGE)
from .harness import Checks, ExpeditionState, StageAbort, assert_real_travel
from .prepare import muster_travellers, seed_departure_deficit
from .readers import (arrival_box, carried, current_action, dist, event_log,
                      fmt_vitals, in_arrival_box, instance_by_id, inventory,
                      known_locations, num, paired_positions, unit_pos, vitals)


def run(chk: Checks, st: ExpeditionState) -> None:
    """Both travellers walk the same route; the ruin is discovered by
    sight."""
    port = st.port
    prepared, control = st.prepared, st.control
    ruin, ruin_xy, ruin_id = st.ruin, st.ruin_xy, st.ruin_id
    deposit_spot, stay_home = st.deposit_spot, st.stay_home
    target = st.target

    chk.enter("travel", "both travellers walk the same route; "
                        "the ruin is discovered by sight")
    # Muster both travellers at a common departure point first.
    # A shared DESTINATION is not a shared journey: hunger drains
    # with time on the road, so two travellers setting out from
    # different distances would be running different-length legs
    # (36.4 vs 31.5 tiles, in an earlier run of this probe) and
    # the difference would land in the measured delta alongside
    # supplies.
    # Leaves the session PAUSED on success, holding the verified
    # departure positions still: everything from here to the
    # paired orders happens inside that one window, so nothing
    # moves between the check and the departure.
    staged, sep, spread, bearing = muster_travellers(
        port, (prepared, control), deposit_spot, ruin_xy)
    at_start = staged or {u: unit_pos(port, u)
                          for u in (prepared, control)}
    if not chk.ok(staged is not None,
                  f"both travellers depart from the SAME PLACE, not just "
                  f"the same distance — so they walk one route, and "
                  f"route shape is time on the road, which is hunger "
                  f"(prepared at {at_start[prepared]}, control at "
                  f"{at_start[control]}; {sep:.2f} tiles apart, bar "
                  f"{MAX_START_SEPARATION}; "
                  f"{dist(at_start[prepared], ruin_xy):.1f} vs "
                  f"{dist(at_start[control], ruin_xy):.1f} tiles out, "
                  f"spread {spread:.2f}, bar {MAX_START_SPREAD}; "
                  f"bearings to the ruin {bearing:.1f} deg apart; "
                  f"verified with the simulation stopped)"):
        send(port, "engine.setPaused(false); return 'ok'")
        raise StageAbort("the travellers never mustered at a shared origin")

    # Already paused by the muster. Seed the shared deficit and
    # issue both orders inside that same window, so the two
    # travellers genuinely leave from the same place, in the same
    # state, under the same command.
    seeded = seed_departure_deficit(port, (prepared, control))
    depart = {u: vitals(port, u) for u in (prepared, control)}
    # Rations in the pack at departure. Consumption is the
    # DURABLE record that a meal happened: `unit.feed` removes a
    # discrete ration outright, so a count that has gone down
    # cannot be missed by a sampling loop the way the
    # `eat_from_inventory` action itself can.
    depart_food = {u: carried(port, u)[1] for u in (prepared, control)}
    print(f"  departure  prepared {prepared}: "
          f"{fmt_vitals(depart[prepared])}, "
          f"{dist(at_start[prepared], ruin_xy):.1f} tiles to go",
          flush=True)
    print(f"  departure  control  {control}: "
          f"{fmt_vitals(depart[control])}, "
          f"{dist(at_start[control], ruin_xy):.1f} tiles to go",
          flush=True)
    if not chk.ok(seeded and all(
            depart[u]["pose"] not in ("collapsed", "dead")
            for u in (prepared, control)),
            f"precondition: both travellers set out on their feet from "
            f"the same seeded hunger (seeded={seeded}; prepared "
            f"{depart[prepared]['pose']!r}, control "
            f"{depart[control]['pose']!r})"):
        raise StageAbort("a traveller did not set out on its feet")

    already = {it["instanceId"] for it in inventory(port, prepared)
               if it.get("defName") == target["defName"]}
    # ONE identical leg for both: the same verb, to the same
    # tile, issued in the same paused window.
    #
    # The verb has to match, not just the destination.
    # `commandMove` walks at `movement_speed.ordered` while
    # `pickup_ground` walks at `comfort`, and ordered is
    # comfort * 1.15 — so ordering one traveller to fetch and
    # the other to walk would put a 15% speed difference inside
    # the comparison. The prepared traveller's retrieval order
    # is therefore issued LATER, in the extract stage, once the
    # control measurement has already been taken.
    #
    # The control is given no retrieval target at all: handing
    # it the ruin's other loot roll would put the loot TABLE
    # inside the experiment, since a ruin can roll food and a
    # control that eats what it finds destroys the measurement.
    tx, ty = int(ruin["gx"]), int(ruin["gy"])
    for uid in (prepared, control):
        send(port, f"require('scripts.unit_ai').commandMove({uid},"
                   f"{tx},{ty}); return 'ok'")
    tasks = {}
    for uid in (prepared, control):
        tasks[uid] = send(
            port, f"local s=require('scripts.unit_ai').getState({uid}); "
                  f"local t=s and s.commandedTask; "
                  f"return t and (math.floor(t.x)..','..math.floor(t.y)) "
                  f"or 'none'").strip().strip('"')
    chk.ok(all(tasks[u] == f"{tx},{ty}" for u in (prepared, control)),
           f"both travellers leave under the IDENTICAL pending move "
           f"order to the same tile ({tx},{ty}) — prepared "
           f"{tasks[prepared]!r}, control {tasks[control]!r}")
    send(port, "engine.setPaused(false); return 'ok'")

    box = arrival_box(ruin)
    p_samples: list = []
    c_samples: list = []
    arrived_at: dict[int, float] = {}
    # Watch each traveller's CHOSEN ACTION, so the control-stage
    # delta can be attributed to a mechanism that was observed
    # running rather than inferred from the number it left
    # behind.
    ate: dict[int, bool] = {prepared: False, control: False}
    # Any stay-at-home colonist seen at the ruin during the
    # leg has been there, and is therefore not evidence about
    # units that have NOT been there.
    visited_ruin: set[int] = set()
    start = time.time()
    deadline = start + 480.0
    together = None
    arrive = None
    arrive_food = None
    while time.time() < deadline:
        # ONE round trip for both, so the simultaneity test below
        # is over coordinates from a single read.
        live = paired_positions(port, prepared, control)
        for uid, samples in ((prepared, p_samples), (control, c_samples)):
            p = live[uid]
            if p:
                samples.append(p)
                if uid not in arrived_at and in_arrival_box(p, box):
                    arrived_at[uid] = time.time() - start
            # Corroboration only — see `depart_food`. A meal is
            # over in a tick or two and this poll runs about once
            # a second, so catching the action is luck; catching
            # the missing ration is not.
            if (current_action(port, uid) == "eat_from_inventory"
                    or send(port, f"return unit.getActivity({uid})")
                    == "eating"):
                ate[uid] = True
        for uid in stay_home:
            q = unit_pos(port, uid)
            if q and in_arrival_box(q, box):
                visited_ruin.add(uid)
        # The shared observation point is both travellers inside
        # the ruin IN THE SAME SAMPLE — not "each has been there
        # at some point". The two arrive at different times, and
        # although an arrived unit holds its destination (#1216)
        # its own physiology can still carry it back out while
        # the other is still walking; latching first-entry would
        # score that as a shared observation point.
        if all(live[u] and in_arrival_box(live[u], box)
               for u in (prepared, control)):
            # A candidate. STOP the simulation and revalidate,
            # so the snapshot the control is scored from is
            # coherent rather than merely closely-spaced: the
            # metrics below are several console round trips, and
            # a running sim would let the pair drift apart
            # between them.
            send(port, "engine.setPaused(true); return 'ok'")
            held = paired_positions(port, prepared, control)
            if all(held[u] and in_arrival_box(held[u], box)
                   for u in (prepared, control)):
                together = held
                arrive = {u: vitals(port, u)
                          for u in (prepared, control)}
                arrive_food = {u: carried(port, u)[1]
                               for u in (prepared, control)}
                send(port, "engine.setPaused(false); return 'ok'")
                break
            send(port, "engine.setPaused(false); return 'ok'")
        time.sleep(1.0)

    chk.ok(together is not None,
           f"BOTH travellers are at the ruin {box} "
           f"in ONE COHERENT SNAPSHOT — a single paired read, "
           f"revalidated with the simulation STOPPED, and the control's "
           f"metrics taken from that same stopped window — so it is "
           f"measured where the prepared one is, not part-way behind it "
           f"and not from two positions sampled moments apart "
           f"(snapshot {together}; first entered after "
           f"{arrived_at.get(prepared, -1):.0f}s / "
           f"{arrived_at.get(control, -1):.0f}s)")
    if arrive is None:
        arrive = {u: vitals(port, u) for u in (prepared, control)}
    if arrive_food is None:
        arrive_food = {u: carried(port, u)[1]
                       for u in (prepared, control)}
    for uid, samples, label in ((prepared, p_samples, "prepared"),
                                (control, c_samples, "control")):
        assert_real_travel(chk, samples, ruin_xy,
                           f"the {label} traveller's outbound leg",
                           min_samples=10, min_closed=10.0)
    if together:
        print(f"  observation point (one shared sample): prepared "
              f"{dist(together[prepared], ruin_xy):.1f} tiles from the "
              f"ruin anchor, control "
              f"{dist(together[control], ruin_xy):.1f} tiles", flush=True)

    # -- the control observation point: one identical leg from a
    #    common departure point, both at the ruin in the same
    #    sample, only the packs differ. `arrive` was captured at
    #    that sample and is deliberately NOT re-read here.
    print(f"  arrival    prepared {prepared}: "
          f"{fmt_vitals(arrive[prepared])}", flush=True)
    print(f"  arrival    control  {control}: "
          f"{fmt_vitals(arrive[control])}", flush=True)
    # #999: both travellers arriving collapsed used to be silently
    # tolerated here — the pose was recorded above but only ever
    # printed, so the location/travel checks below could still
    # pass around it. A traveller should reach the ruin on
    # its feet; a real ordinary leg collapsing is exactly the
    # run/faint/run bug this gate now has to catch.
    chk.ok(all(arrive[u]["pose"] not in ("collapsed", "dead")
               for u in (prepared, control)),
           f"both travellers are standing at the arrival snapshot, "
           f"not collapsed/dead from the ordinary leg (prepared "
           f"{arrive[prepared]['pose']!r}, control "
           f"{arrive[control]['pose']!r})")

    # -- discovery: lifecycle, player event, per-unit knowledge
    #
    # Since #917 the expected state here is `discovered`, NOT
    # `cleared`. This ruin's zero-nomad encounter half has been
    # satisfied since placement, so before #917 first sight took
    # it straight to `cleared`; now its guaranteed significant
    # item is still outstanding, so the compound predicate is
    # unsatisfied and sight can only reveal it. `cleared` is
    # still accepted, because a colonist's own `store_materials`
    # AI may already have recovered the item on the way in —
    # which clears it legitimately, just not by the player's
    # gesture. `active` remains accepted for the occupied ruins
    # this scenario does not select.
    inst = poll_until(60.0, lambda: (
        lambda i: i if isinstance(i, dict)
        and i.get("lifecycle") in ("discovered", "active", "cleared")
        else None)(
            instance_by_id(port, PAGE, ruin_id)), interval=1.0)
    chk.ok(inst is not None,
           f"approaching the ruin reveals it — lifecycle "
           f"'discovered' (its guaranteed item is still outstanding), "
           f"or 'active'/'cleared' "
           f"({(instance_by_id(port, PAGE, ruin_id) or {}).get('lifecycle')!r})")
    # The WHOLE log, deliberately not a slice from a mark taken
    # before departure. `Engine.PlayerEvent.Emit.pushBounded`
    # keeps a bounded ring buffer and drops the oldest rows past
    # `eventStoreCap`, so an index captured earlier does not
    # survive a busy session: once the buffer saturates, the
    # slice silently skips real entries, and if the mark is past
    # the new length it yields nothing at all (observed — a run
    # whose lifecycle had demonstrably reached its visible encounter state
    # reported no event). Scanning the whole log is also exactly
    # as strict: the promotion is one-way, so an instance can
    # emit its discovery event only once per session.
    label = (ruin.get("name") or "").strip()
    hits = [e for e in event_log(port)
            if e.get("category") == "location_discovery"
            and label and label in (e.get("text") or "")]
    chk.ok(len(hits) == 1,
           f"discovery emits exactly one player-facing event naming the "
           f"location ({[h.get('text') for h in hits]})")
    key = f"{PAGE}#{ruin_id}"
    knew = poll_until(30.0, lambda: key in known_locations(port, prepared),
                      interval=1.0)
    chk.ok(bool(knew),
           f"the traveller that walked there personally KNOWS the location "
           f"({key} in {sorted(known_locations(port, prepared))})")
    # The premise is asserted, not assumed: each held colonist
    # must genuinely still be away from the ruin, and its position
    # and memory are both reported so a failure says which.
    home_state = []
    never_went = []
    for u in stay_home:
        p = unit_pos(port, u)
        known = known_locations(port, u)
        inside = bool(p) and in_arrival_box(p, box)
        been = u in visited_ruin or inside
        home_state.append(
            f"uid {u} at {p} "
            f"({'HAS BEEN at' if been else 'never reached'} the ruin) "
            f"knows {sorted(known) or 'nothing'}")
        if not been:
            never_went.append((u, known))
    chk.ok(bool(never_went),
           f"precondition: at least one colonist never entered the "
           f"ruin, so there is something to test the knowledge "
           f"layer against ({'; '.join(home_state) or 'none'})")
    chk.ok(bool(never_went) and all(key not in k for _u, k in never_went),
           f"the colonists who never went learned nothing — per-unit "
           f"knowledge is experiential, not broadcast "
           f"({'; '.join(home_state)})")

    # The paired observations `control` is scored from, handed on
    # exactly as they were captured. Assigned once, here, after every
    # fallback above has settled them.
    st.box = box
    st.already = already
    st.depart, st.depart_food = depart, depart_food
    st.arrive, st.arrive_food = arrive, arrive_food
    st.ate = ate
    st.visited_ruin = visited_ruin


def measure_control(chk: Checks, st: ExpeditionState) -> None:
    """The unprepared party is measurably worse off.

    Scored in engine A, after `save`, from the observations `run`
    retained — never re-read here. `chk.enter` before the fresh process
    boots is deliberate: the presentation order in the aggregate summary
    puts `control` last, while its measurement happens before `load`.

    Named `measure_control` rather than `control` so the traveller uid
    bound below keeps the name the pre-split body gave it.
    """
    port = st.port
    prepared, control = st.prepared, st.control
    depart, arrive = st.depart, st.arrive
    depart_food, arrive_food, ate = st.depart_food, st.arrive_food, st.ate

    chk.enter("control", "the unprepared party is measurably worse off")
    dp, dc = depart[prepared], depart[control]
    ap_, ac = arrive[prepared], arrive[control]
    chk.ok(all(v is not None for v in (dp["stomach"], dc["stomach"],
                                       ap_["stomach"], ac["stomach"])),
           "both travellers reported a stomach fraction at both "
           "observation points")
    chk.ok(abs(num(dp["stomach"]) - num(dc["stomach"])) < 0.02,
           f"the two travellers set out from the SAME deficit "
           f"(stomach {num(dp['stomach']):.3f} vs "
           f"{num(dc['stomach']):.3f}) — the packs are the only "
           f"difference between them")
    s_delta = num(ap_["stomach"]) - num(ac["stomach"])
    # The MECHANISM, evidenced by what it CONSUMED — not inferred
    # from the numbers below, which two differently-massed
    # acolytes could in principle reach by other means.
    #
    # Rations eaten, rather than the eat_from_inventory action
    # being caught in the act: `eatExecute` finishes a whole meal
    # inside one AI tick, so at a ~1s poll the action is a
    # coin-flip to observe (a run where the stomach demonstrably
    # went 0.20 -> 0.82 recorded ate=False). `unit.feed` removes
    # a discrete ration outright, so the pack is a durable
    # record. The action sighting is still reported, as
    # corroboration.
    eaten = depart_food[prepared] - arrive_food[prepared]
    chk.ok(eaten > 0
           and depart_food[control] == 0 and arrive_food[control] == 0,
           f"the delta comes from EATING: the provisioned traveller "
           f"consumed {eaten} ration(s) en route "
           f"({depart_food[prepared]} -> {arrive_food[prepared]}) while "
           f"the control had none to consume and consumed nothing "
           f"({depart_food[control]} -> {arrive_food[control]}); "
           f"eat_from_inventory/eating also seen live: "
           f"prepared={ate[prepared]}, control={ate[control]}")
    chk.ok(num(ap_["stomach"]) > num(dp["stomach"])
           and num(ac["stomach"]) <= num(dc["stomach"]),
           f"only the traveller carrying food ate: prepared "
           f"{num(dp['stomach']):.3f} -> {num(ap_['stomach']):.3f}, "
           f"control {num(dc['stomach']):.3f} -> "
           f"{num(ac['stomach']):.3f}")
    chk.ok(s_delta >= MIN_STOMACH_DELTA,
           f"at the same point in the same journey the unprepared party is "
           f"{s_delta * 100:.0f} points of its own stomach worse off "
           f"(bar {MIN_STOMACH_DELTA * 100:.0f})")
    # Reported, not gated — see the facade's module docstring on why
    # water is evidence here rather than a threshold.
    p_water, p_rations = carried(port, prepared)
    c_water, c_rations = carried(port, control)
    print(f"  water/food evidence (not gated): prepared carries "
          f"{p_water:.2f} L + {p_rations} rations at hydration "
          f"{num(ap_['hydration']):.3f}; control carries "
          f"{c_water:.2f} L + {c_rations} rations at hydration "
          f"{num(ac['hydration']):.3f}", flush=True)
    print(f"  control stomach delta {s_delta:.3f} "
          f"(measurement, not part of the fingerprint)", flush=True)
