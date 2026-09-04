#!/usr/bin/env python3
"""Session (c): the fresh process that proves the save survived (#2164).

A genuinely new engine, reading only `SAVE_LOCAL` — the half of
save -> quit -> restart -> load no single-process check can stand in
for. It re-derives both ruins by anchor, pins the location count and
each ruin's discovery state, and compares the restored icons against
the SAME per-ruin baselines session (a) photographed before any portal
existed.

It runs the three-state icon sweep last, and only here: promoting the
control ruin through discovered/cleared/depleted is one-way, so it must
come after every check that needs the control still unknown.
"""
from __future__ import annotations

import os
import time

from probelib import poll_until, send
from location_content_probe import load_defs
from offscreen_probe import (center_on, png_differs, png_stats, screenshot,
                             set_zoom, zoom_fade_end)
from portal_ghost_probe import in_world_view
from .constants import SAVE_LOCAL
from .invocation import SessionContext, check
from .support import centre_luma, list_locations_sorted, match_by_anchor


def session_reload_check(ctx: SessionContext) -> None:
    port, shots = ctx.port, ctx.shots
    w, h = ctx.w, ctx.h
    target, control = ctx.target, ctx.control
    expected_total = ctx.expected_total
    load_defs(port)
    send(port, f"require('scripts.main_menu').loadAndShowSave('{SAVE_LOCAL}'); return 'ok'")
    reached = poll_until(90.0, lambda: in_world_view(port))
    if not check("session c: reached world_view after loading the saved session",
                 bool(reached)):
        return

    locs = list_locations_sorted(port)
    check("no locations were duplicated by the save -> quit -> restart -> "
          "load round-trip", len(locs) == expected_total,
          f"expected {expected_total}, got {len(locs)}")
    t = match_by_anchor(locs, target["gx"], target["gy"])
    c = match_by_anchor(locs, control["gx"], control["gy"])
    check("target ruin still known after reload", bool(t))
    check("control ruin still known after reload", bool(c))
    if t:
        check("target ruin remains discovered after save -> quit -> restart -> load",
              bool(t.get("discovered")))
    if c:
        check("control ruin remains undiscovered after save -> quit -> "
              "restart -> load", not c.get("discovered"))

    # -- step 21 checkpoint: restored icons after save/load, checked
    # against the SAME terrain-stable per-ruin baselines session (a)
    # captured pre-portal (identical camera centre/zoom, same tempdir) —
    # not just against each other. --
    fade_end = zoom_fade_end(port)
    full_zoom = fade_end * 1.5
    shot_hidden_target = os.path.join(shots, "icon_unknown_target.png")
    shot_hidden_control = os.path.join(shots, "icon_unknown_control.png")
    center_on(port, target["gx"], target["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_t = os.path.join(shots, "icon_reloaded_target.png")
    if check("reloaded target-icon screenshot answers", screenshot(port, shot_t)):
        st = png_stats(shot_t)
        check("reloaded target-icon frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
        check("reloaded target ruin's icon still differs from its own "
              "pre-portal unknown-marker baseline (discovered state survived "
              "save -> quit -> restart -> load)",
              png_differs(shot_hidden_target, shot_t, min_fraction=0.0002))
    center_on(port, control["gx"], control["gy"])
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    shot_c = os.path.join(shots, "icon_reloaded_control.png")
    if check("reloaded control-icon screenshot answers", screenshot(port, shot_c)):
        check("the reloaded type icon and unknown marker render differently",
              png_differs(shot_t, shot_c, min_fraction=0.0002))
        check("reloaded control ruin's icon still matches its own pre-portal "
              "unknown-marker baseline (unknown state survived "
              "save -> quit -> restart -> load)",
              not png_differs(shot_hidden_control, shot_c, min_fraction=0.0002))

    # -- steps 22a-22d (#1230): the three icon appearances, on the GPU,
    # at ONE camera centre and ONE zoom, so the only thing varying
    # between the frames is the location's own lifecycle.
    #
    # This runs LAST, in the reload session, for two reasons. It drives
    # the control ruin forward through discovered/cleared/depleted and
    # lifecycle promotion is one-way, so it must come after every check
    # that requires the control to still be unknown — which is all of
    # session (b) and everything above. And it reuses the camera/zoom
    # sequence this session has just proven works (the reloaded-icon
    # shots above), rather than the post-save tail of session (b),
    # where the same sequence photographed a static frame.
    #
    # What this can and cannot prove: data/locations/ruin_small.yaml is
    # the only shipped location definition, so both ruins are instances
    # of it and "every definition shares ONE unknown marker" is not
    # demonstrable from a real world — that claim belongs to the pure
    # "Location map icons" Hspec group, which registers synthetic
    # definitions. What IS demonstrable here is that the unknown marker
    # and the ruin type icon render DIFFERENTLY at one camera centre and
    # zoom, and that the spent states render that type icon strictly
    # DARKER.
    inst_id = (c or {}).get("instance_id")
    if not check("reloaded control ruin exposes its instance id for the "
                 "icon-state sweep", inst_id is not None):
        return

    def shoot_state(tag: str) -> str | None:
        center_on(port, control["gx"], control["gy"])
        set_zoom(port, full_zoom)
        time.sleep(0.4)
        path = os.path.join(shots, f"icon_state_{tag}.png")
        return path if screenshot(port, path) else None

    def force_lifecycle(state: str) -> bool:
        got = send(port,
                   f"return tostring(world.setLocationLifecycle("
                   f"{int(inst_id)}, '{state}'))")
        return "true" in (got or "").lower()

    # `shot_c` above is this same camera centre and zoom, taken while
    # the control was still unknown — reuse it rather than re-shooting,
    # so the unknown frame is provably the one the persistence check
    # just validated.
    shot_unknown = shot_c
    lit_path = None
    if check("control ruin promotes to discovered", force_lifecycle("discovered")):
        lit_path = shoot_state("discovered")
        if check("discovered-state screenshot answers", bool(lit_path)):
            check("the unknown marker and the ruin type icon render "
                  "DIFFERENTLY at the same camera centre and zoom — the "
                  "map does not show the ruin's type until a unit has "
                  "seen it",
                  png_differs(shot_unknown, lit_path, min_fraction=0.0001))

    lit = centre_luma(lit_path, w, h) if lit_path else None
    spent_shots: dict[str, str] = {}
    for spent in ("cleared", "depleted"):
        if not check(f"control ruin promotes to {spent}", force_lifecycle(spent)):
            continue
        shot = shoot_state(spent)
        if not check(f"{spent}-state screenshot answers", bool(shot)):
            continue
        spent_shots[spent] = shot
        dark = centre_luma(shot, w, h)
        if check(f"{spent}-state luminance measurable and comparable",
                 lit is not None and dark is not None,
                 f"discovered={lit} {spent}={dark}"):
            check(f"the {spent} icon renders strictly DARKER than the "
                  f"discovered one at the same camera centre and zoom",
                  dark < lit,
                  f"discovered luma={lit:.3f} {spent} luma={dark:.3f}")
    # Both spent states share one bitmap and one tint, so they must be
    # indistinguishable from each other — the thing that would break if
    # either grew its own texture.
    if len(spent_shots) == 2:
        check("cleared and depleted render identically — one type icon, "
              "one tint, no second authored bitmap",
              not png_differs(spent_shots["cleared"], spent_shots["depleted"],
                              min_fraction=0.0001))
