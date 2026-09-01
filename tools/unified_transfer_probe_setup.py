#!/usr/bin/env python3
"""The setup stage for `tools/unified_transfer_probe.py` (#2048).

A fixed-seed world through the REAL create-world screen, the probe's own
throwaway defs, seven fixtures sited on ONE LEVEL SHELF, and the camera
settled into the tile zoom band before any session exists. Every
environmental choice here was a live failure first; `allocate_flat_anchors`
and the camera pin in particular are what keep the walks below measuring
the transfer system rather than terrain.

Every engine-A stage shares the world and the fixture graph this stage
returns, so it runs first and nothing else may re-create either.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (camera_state, pin_camera_to_tile, send_json,
                      set_paused, viewport)
from unified_transfer_probe_support import (Checks, DEF_HOLD, DEF_PARTIAL,
                                            DEF_STALE, DEF_STALE_HOLD,
                                            DEF_UNSEEN)
from unified_transfer_probe_world import (adjacent_dry_tiles,
                                          allocate_flat_anchors,
                                          create_world, ep_info,
                                          load_fixtures, reach_main_menu,
                                          remembered_ids, spawn_hold,
                                          spawn_unit, stage_into_hold)


def stage_setup(chk: Checks, port: int, fp: dict, base: str, args):
    """A fixed-seed world, the probe's own defs, and the three endpoint
    classes stocked. Returns the fixture ids, or None."""
    if not reach_main_menu(chk, port):
        return None, None
    if not create_world(chk, port, args.seed, args.world_size, args.plates):
        return None, None
    vp = viewport(port, fallback=tuple(int(v) for v in args.size.split("x")))
    chk.ok(vp["win_w"] > 0 and vp["win_h"] > 0 and vp["fb_w"] > 0
           and vp["fb_h"] > 0,
           "the engine reports a usable window and framebuffer extent",
           f"got {vp!r}")
    print(f"  (window {vp['win_w']}x{vp['win_h']}, "
          f"framebuffer {vp['fb_w']}x{vp['fb_h']})", flush=True)
    if not load_fixtures(chk, port, base):
        return None, vp

    print("  (scanning terrain outward from the origin for dry anchor sites)",
          flush=True)
    sites = allocate_flat_anchors(port, 7)
    if not chk.ok(sites is not None,
                  "seven separated dry sites are found for the fixtures"):
        return None, vp
    (hold_xy, partial_xy, stale_xy, unseen_xy,
     acolyte_xy, mule_xy, wild_xy) = sites
    print(f"  (fixture sites: hold={hold_xy} partial={partial_xy} "
          f"stale={stale_xy} unseen={unseen_xy} acolyte={acolyte_xy} "
          f"technomule={mule_xy} wildlife={wild_xy})", flush=True)

    ids = {"sites": list(sites)}
    ids["hold"] = spawn_hold(chk, port, DEF_HOLD, *hold_xy, "storage hold")
    ids["partial"] = spawn_hold(chk, port, DEF_PARTIAL, *partial_xy,
                                "small hold (room for exactly eight)")
    ids["stale"] = spawn_hold(chk, port, DEF_STALE_HOLD, *stale_xy,
                              "knowledge hold")
    ids["unseen"] = spawn_hold(chk, port, DEF_UNSEEN, *unseen_xy,
                               "never-inspected hold", want_built=False)
    ids["acolyte"] = spawn_unit(chk, port, "acolyte", *acolyte_xy, "acolyte")
    ids["technomule"] = spawn_unit(chk, port, "technomule", *mule_xy,
                                   "technomule")
    # `unit.spawn` defaults to the WILDLIFE faction when no tag is given.
    ids["wildlife"] = spawn_unit(chk, port, "red_squirrel", *wild_xy,
                                 "wildlife", faction=None, quiet=True)
    if any(v is None for v in ids.values()):
        chk.ok(False, "every fixture spawned", f"got {ids!r}")
        return None, vp

    # The knowledge stage's two extra units, both spawned in place rather
    # than walked there: what it measures is the reveal rule, not
    # pathfinding, and a unit already standing beside the container makes
    # both the proximity case and the Mode A arrival immediate.
    beside = adjacent_dry_tiles(port, *stale_xy)
    if not chk.ok(len(beside) >= 2,
                  "two dry tiles beside the knowledge hold, so neither of its "
                  "two units has to stand on the building itself",
                  f"got {beside!r}"):
        return None, vp
    ids["watcher"] = spawn_unit(chk, port, "acolyte", beside[0][0], beside[0][1],
                                "knowledge watcher")
    ids["reveal"] = spawn_unit(chk, port, "acolyte", beside[1][0], beside[1][1],
                               "knowledge escort")
    if ids["watcher"] is None or ids["reveal"] is None:
        return None, vp

    # Settle the camera into the TILE zoom band NOW, while nothing is
    # open. A Mode A session's arrival snap calls `camera.goToTile`, and
    # crossing a zoom band is one of `view_teardown`'s triggers — it
    # clears the session outright — so a session created while the
    # camera is still on the post-worldgen zoomed-out view tears itself
    # down the instant it opens. Pinning here once makes every later
    # snap a within-band move.
    hz = send_json(port, f"return building.getInfo({ids['hold']})")
    pin_camera_to_tile(port, hold_xy[0], hold_xy[1],
                       int((hz or {}).get("gridZ", 0)))
    cam = camera_state(port)
    chk.ok(isinstance(cam.get("zoom"), (int, float)) and cam["zoom"] < 1.2,
           "the camera is settled in the zoomed-in tile band before any "
           "session exists, so no arrival snap crosses a band and tears one "
           "down", f"got {cam!r}")

    ids["homes"] = {ids["acolyte"]: acolyte_xy, ids["technomule"]: mule_xy}

    for key, ep in (("acolyte", ("unit", ids["acolyte"])),
                    ("technomule", ("unit", ids["technomule"])),
                    ("storage", ("building", ids["hold"]))):
        info = ep_info(port, ep)
        chk.ok(info.get("eligible") is True,
               f"the {key} endpoint is eligible to the transfer contract",
               f"got {info!r}")
    chk.ok(ep_info(port, ("unit", ids["wildlife"])).get("eligible") is False,
           "and the wildlife unit is NOT — it is not player-commandable, which "
           "is what makes its withdrawal below a non-revealing one")

    # Two instances into the knowledge hold, through the lax AI verb, so
    # it starts KNOWN with a real observation time.
    for _ in range(2):
        stage_into_hold(port, ids["reveal"], ids["stale"], DEF_STALE)
    chk.ok(len(remembered_ids(port, ids["stale"], DEF_STALE)) == 2,
           "the knowledge hold is stocked and remembered")

    set_paused(port, True)
    fp["endpoints"] = {k: v for k, v in sorted(ids.items())
                       if isinstance(v, int)}
    return ids, vp
