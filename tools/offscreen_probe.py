#!/usr/bin/env python3
"""Offscreen render mode probe — the #650 gate.

Boots the engine with --offscreen (full Vulkan render, NO window, no
GLFW, no swapchain) and asserts the mode end to end:

1. Boot + READY, and the real UI flow runs (the loading screen
   completes and the main menu's widgets appear) — which GPU-less
   --headless never does.
2. debug.captureScreenshot returns a valid, NON-BLANK PNG at the
   requested --size.
3. F2 input injection drives the UI windowless: clicking the main
   menu's "Create World" button — located via the F3 ui.dumpWidgets
   oracle, not hardcoded coordinates — lands on the Create World
   screen, and the next screenshot differs from the menu.
4. Parallel instances: a second offscreen engine boots on another port
   WHILE the first is still running; both answer console queries and
   capture screenshots without interfering.
5. (unless --skip-worldgen) End-to-end to the in-game HUD: click
   "Generate World", wait for generation to finish, and assert a
   third, again-different screenshot plus a working world query.
6. (unless --skip-worldgen) Remote portal warning (#779), against the
   SAME generated world from phase 5, located dynamically via
   building.canPlaceAt/building.remoteCheck (never hardcoded world
   coordinates — the world is PINNED but the POSITIONS are still the
   engine's own verdict, the same "oracle, not guessed coordinates"
   rule the issue asks the UI half of this probe to follow too): a
   valid remote position renders a normal (white, not red) ghost;
   clicking it presents the remote-settlement modal
   (located via ui.dumpWidgets, not hardcoded coordinates) without
   spawning anything; a second click while it's open cannot stack a
   second modal; "Choose Another Site" closes it, spawns nothing, and
   leaves placement armed; clicking a nearby valid position (close to
   a real placed location) commits instantly with no modal; re-opening
   and clicking "Establish Here" revalidates and spawns exactly one
   portal, exiting placement.
7. (unless --skip-worldgen) Location lifecycle-state map icons
   (#781, #1230), against the same generated world, located via
   world.listPlacedLocations() (the world is pinned; the ruins it
   grades are still whatever that oracle reports, never a hardcoded
   coordinate): a placed ruin shows the SHARED unknown marker before
   any player unit has seen it; loading its chunks (structure
   physically visible) alone
   does not change the icon; spawning a player-faction unit at the ruin
   flips ONLY that ruin's icon to its ruin TYPE icon (a second, unseen
   ruin keeps the unknown marker) — measured over a BOUNDED region
   around the icon, with the reveal's own shipped notification card
   dismissed first so it neither covers the icon nor supplies the
   changed pixels (#1765); the icon stays legible at a second map
   zoom level; rotating the camera keeps the pipeline rendering (the
   icon's screen-upright invariant across all 4 facings is proven
   exactly, at the math level, by the pure Hspec group "Location map
   icons" — this phase only proves the GPU path renders SOMETHING that
   updates on rotation); and the discovered state survives a real
   save -> quit -> fresh restart -> load.

Needs a GPU (Vulkan device) — manual-only, never CI-gated.

Every engine this probe launches writes its OWN log (#1763) — including
the third, which restarts on the first engine's port and used to
overwrite that long session's capture. Each path is printed when it is
allocated and again in a closing summary naming the launch that wrote
it.

The pinned fixture world (#2166)
--------------------------------
Phases 6 and 7 both need a world that actually CONTAINS what they
grade — a buildable position beyond the remote threshold, a buildable
one next to a placed location, and at least two placed locations. The
create-world screen rolls a fresh random hex seed on every visit
(scripts/create_world/settings_tab.lua's ``randbox.newHexSeed()`` when
``pending.seed`` is empty), so those preconditions used to be a
per-run coin flip: one retained run missed the remote search and
silently skipped the whole portal path, another passed it.

So the generation parameters are PINNED here, through the create-world
screen's own controls (the seed randbox, the world-size dropdown, and
the advanced tab's plate-count textbox — never a bypass around the
screen, and never a change to the screen itself), ``world.getSeed``
confirms the world that came out is the pinned one, ``--seed``
overrides the default, and every run prints a one-line ``FIXTURE``
identity so two runs can be compared.

A pinned fixture that stops fitting must be NOTICED, not hidden, so a
search that finds nothing is reported as a FIXTURE/SETUP failure —
printed ``[SETUP]``, counted separately in the closing summary, and
still exiting non-zero — rather than as a product assertion about the
remote-portal feature. Every candidate either search rejects prints
its own line (resolved tile, canPlaceAt verdict AND reason, and the
full remoteCheck classification), so the next miss names its cause
without a rerun.

Usage: python3 tools/offscreen_probe.py [--port 9418] [--size 1280x720]
       [--seed 0000002A] [--skip-worldgen]
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_runner_diagnostics import FailureEmitter   # durable failure records (#1982)
from probelib import boot, poll_until, quit_engine, send, send_json, wait_load_published

# --------------------------------------------------------------------------
# The pinned fixture world (#2166)
# --------------------------------------------------------------------------
# Written the way the create-world seed field spells one — 8 uppercase
# hex digits, which scripts/create_world/generation.lua reads with
# tonumber(seed, 16).
#
# This exact triple is what phases 6 and 7 are verified against: it
# carries a buildable position beyond the remote threshold, a buildable
# non-remote position next to a placed location, and the two placed
# locations the icon phase needs. Change one of the three and all of
# that has to be re-verified — which is what the FIXTURE line every run
# prints exists to make checkable.
DEFAULT_SEED_HEX = "5DEA7E52"
DEFAULT_WORLD_SIZE = "128"
DEFAULT_PLATE_COUNT = "10"

# Reported for a tile that no search ever chose. NONE is a search that
# ran and found nothing (or never ran because the world was wrong);
# SKIPPED is --skip-worldgen, where there is no world to search at all.
TILE_NONE = "NONE"
TILE_SKIPPED = "SKIPPED"

# Both kinds are kept as the failure TEXTS, not just counts, so the run
# can hand them to `FailureEmitter` below. `run_probes` drains this
# process's stdout only at exit, so a failure printed inline can fall off
# the retained tail of a long run; a durable record cannot (#1982).
failures: list[str] = []
setup_failures: list[str] = []
FAILURE = FailureEmitter("offscreen_probe")


def check(name: str, ok: bool, detail: str = "") -> bool:
    """One GRADED product assertion about the engine's behavior."""
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    if not ok:
        failures.append(name + (f" — {detail}" if detail else ""))
    return ok


def setup_check(name: str, ok: bool, cause: str = "") -> bool:
    """One FIXTURE precondition — the pinned world either carries what a
    phase needs or it does not.

    Deliberately NOT `check`: a fixture that stopped fitting (a re-pinned
    seed, a worldgen-output change, a changed placement rule) is not a
    regression in the remote-portal feature, and reporting it as one
    sends the next reader hunting the wrong code. It is still a failure
    — printed distinctly, counted separately, and non-zero at exit —
    because a fixture that silently stopped covering the feature is
    exactly what #2166 was filed about.
    """
    print(f"  [{'OK   ' if ok else 'SETUP'}] {name}"
          + (f" — {cause}" if cause and not ok else ""))
    if not ok:
        setup_failures.append(name + (f" — {cause}" if cause else ""))
    return ok


# The one-line fixture identity (#2166 requirement 4). Populated as the
# run establishes each field and printed exactly once, from `main`'s
# `finally`, so even a run that dies mid-phase still says which world it
# was grading.
FIXTURE = {
    "seed": DEFAULT_SEED_HEX,
    "worldSize": DEFAULT_WORLD_SIZE,
    "plateCount": DEFAULT_PLATE_COUNT,
    "remote": TILE_NONE,
    "nearby": TILE_NONE,
}
_fixture_printed = False


def print_fixture() -> None:
    """Print the fixture identity, once per run."""
    global _fixture_printed
    if _fixture_printed:
        return
    _fixture_printed = True
    print(f"FIXTURE seed=0x{FIXTURE['seed']} worldSize={FIXTURE['worldSize']} "
          f"plateCount={FIXTURE['plateCount']} remote={FIXTURE['remote']} "
          f"nearby={FIXTURE['nearby']}")


# --------------------------------------------------------------------------
# PNG helpers (PIL)
# --------------------------------------------------------------------------
def png_stats(path: str):
    """(width, height, #distinct colors) or None if unreadable."""
    try:
        from PIL import Image
        with Image.open(path) as im:
            im = im.convert("RGBA")
            colors = im.getcolors(maxcolors=1 << 20)
            return im.width, im.height, (len(colors) if colors else (1 << 20))
    except Exception:
        return None


def png_differs(path_a: str, path_b: str, min_fraction: float = 0.001) -> bool:
    """True when at least min_fraction of pixels differ. (Deliberately
    no getbbox() short-circuit: on RGBA, Pillow 10+ defaults it to
    alpha_only=True, and two fully-opaque frames always bbox to None.)"""
    from PIL import Image, ImageChops
    with Image.open(path_a) as a, Image.open(path_b) as b:
        if a.size != b.size:
            return True
        diff = ImageChops.difference(a.convert("RGBA"), b.convert("RGBA"))
        changed = sum(diff.convert("L").histogram()[1:])
        return changed >= min_fraction * a.width * a.height


def png_region_changed_pixels(path_a: str, path_b: str, box):
    """Changed-pixel count inside box=(x, y, w, h) — the BOUNDED
    counterpart of png_differs, which counts the whole frame. Returns
    None when the two captures aren't comparable (missing, or different
    sizes), so a caller can tell "nothing changed here" apart from "these
    two frames could not be compared at all"."""
    from PIL import Image, ImageChops
    x, y, w, h = box
    try:
        with Image.open(path_a) as a, Image.open(path_b) as b:
            if a.size != b.size:
                return None
            ra = a.convert("RGBA").crop((x, y, x + w, y + h))
            rb = b.convert("RGBA").crop((x, y, x + w, y + h))
            diff = ImageChops.difference(ra, rb)
            return sum(diff.convert("L").histogram()[1:])
    except Exception:
        return None


# --------------------------------------------------------------------------
# UI helpers (F2 inject + F3 widget oracle)
# --------------------------------------------------------------------------
def widgets(port: int):
    got = send_json(port, "return ui.dumpWidgets()", timeout=10.0)
    return got if isinstance(got, list) else []


def find_widget(port: int, label: str):
    for w in widgets(port):
        if (w.get("label") or "").strip().lower() == label.lower():
            return w
    return None


def click_widget(port: int, label: str) -> bool:
    w = find_widget(port, label)
    if not w:
        return False
    b = w.get("bounds") or {}
    x = int(b.get("x", 0) + b.get("w", 0) / 2)
    y = int(b.get("y", 0) + b.get("h", 0) / 2)
    send(port, f"return input.click({x}, {y})", timeout=10.0)
    return True


def screenshot(port: int, path: str) -> bool:
    got = send_json(port, f"return debug.captureScreenshot('{path}')",
                    timeout=30.0)
    return isinstance(got, dict) and got.get("path") == path


# --------------------------------------------------------------------------
# Notification-popup helpers (#1765)
# --------------------------------------------------------------------------
# A popup notification is a real, screen-CENTRED, pointer-blocking card
# (scripts/popup.lua) that stays up until dismissed. Any phase here that
# compares screenshots has to get it out of the frame first, or it both
# supplies the changed pixels the comparison is looking for and covers
# the thing being compared.
def notification_popup_enabled(port: int, category: str) -> bool:
    """Whether `category` currently raises a popup, read from the
    EFFECTIVE merged configuration (engine.getNotificationCfg — the same
    view the settings tab edits), never from the tracked
    data/notification_categories.yaml default. A developer's own
    config/notifications.local.yaml override is merged in by
    Engine.Asset.YamlNotifications, so the shipped `popup: true` is not a
    guarantee on every machine — which is exactly why the callers below
    only ever WAIT for a card when this says one is coming, while still
    proving the frame is card-free either way."""
    cfg = send_json(port, "return engine.getNotificationCfg()", timeout=10.0)
    for entry in cfg if isinstance(cfg, list) else []:
        if isinstance(entry, dict) and entry.get("id") == category:
            return bool(entry.get("popup"))
    return False


def popup_counts(port: int) -> tuple[int, int]:
    """(active cards, queued entries) from scripts/popup.lua's own
    counters. (0, 0) when the module isn't loaded — a menu-only or
    pre-bootstrap state, where there is nothing on screen to dismiss."""
    got = send_json(
        port,
        "local ok, p = pcall(require, 'scripts.popup'); "
        "if not ok or type(p) ~= 'table' then return {active = 0, queued = 0} end; "
        "return {active = p.activeCount(), queued = p.queueLength()}",
        timeout=10.0)
    if not isinstance(got, dict):
        return (0, 0)
    try:
        return (int(got.get("active", 0)), int(got.get("queued", 0)))
    except (TypeError, ValueError):
        return (0, 0)


def popup_ok_buttons(port: int) -> list[dict]:
    """Every live popup card's own OK control, from the F3 widget oracle
    — located by the name scripts/popup.lua gives it ("popup_ok_<id>"),
    so an unrelated dialog that happens to label a button "OK" is never
    mistaken for a notification card."""
    return [w for w in widgets(port)
            if str(w.get("name") or "").startswith("popup_ok_")]


def dismiss_popups(port: int, attempts: int = 6) -> tuple[int, int]:
    """Clear every notification card, and return the resulting
    (active, queued) counts so the caller can assert the frame is
    card-free.

    Clicking each card's own OK control at the oracle-reported bounds is
    the primary path: it is exactly what a player does, and it exercises
    the real dismissal wiring rather than reaching around it. The
    scripts/popup.lua API fallback exists only so that "the popup is gone
    before the capture" is a guarantee and not a best effort — a card
    whose control the oracle cannot see would otherwise leave the
    comparison below silently measuring the wrong thing."""
    for _ in range(attempts):
        if popup_counts(port) == (0, 0):
            return (0, 0)
        buttons = popup_ok_buttons(port)
        if not buttons:
            break
        b = buttons[0].get("bounds") or {}
        x = int(b.get("x", 0) + b.get("w", 0) / 2)
        y = int(b.get("y", 0) + b.get("h", 0) / 2)
        send(port, f"return input.click({x}, {y})", timeout=10.0)
        time.sleep(0.2)
    if popup_counts(port) != (0, 0):
        send(port,
             "local ok, p = pcall(require, 'scripts.popup'); "
             "if ok and type(p) == 'table' then p.dismissAll() end; return 'ok'",
             timeout=10.0)
        time.sleep(0.2)
    return popup_counts(port)


def check_no_popup(port: int, what: str) -> bool:
    """Clear every notification card and assert the frame really is
    card-free, naming what is about to be captured and the counts that
    were still standing if it is not."""
    active, queued = dismiss_popups(port)
    return check(f"no notification card covers the {what}",
                 (active, queued) == (0, 0),
                 f"active={active}, queued={queued}")


# --------------------------------------------------------------------------
# Remote portal warning (#779) helpers
# --------------------------------------------------------------------------
PORTAL = "acolyte_portal"


def can_place_at(port: int, def_name: str, gx: int, gy: int) -> tuple[bool, str | None]:
    r = send(port,
              f"local v,r = building.canPlaceAt('{def_name}', {gx}, {gy}); "
              f"return tostring(v) .. '|' .. tostring(r)").strip('"')
    valid_s, _, reason_s = r.partition("|")
    return valid_s == "true", (None if reason_s in ("nil", "") else reason_s)


def remote_check(port: int, def_name: str, gx: int, gy: int):
    """(remote: bool, distance: int|None, thresholdTiles: int)."""
    r = send(port,
              f"local rem,d,t = building.remoteCheck('{def_name}', {gx}, {gy}); "
              f"return tostring(rem) .. '|' .. tostring(d) .. '|' .. tostring(t)"
              ).strip('"')
    rem_s, _, rest = r.partition("|")
    dist_s, _, thr_s = rest.partition("|")
    distance = None if dist_s == "nil" else int(float(dist_s))
    threshold = int(float(thr_s)) if thr_s not in ("", "nil") else None
    return rem_s == "true", distance, threshold


def load_region_around(port: int, gx: int, gy: int, radius_chunks: int = 2) -> None:
    cx, cy = gx // 16, gy // 16
    send(port, f"return world.loadChunksInRegion("
               f"{cx - radius_chunks},{cy - radius_chunks},"
               f"{cx + radius_chunks},{cy + radius_chunks})")
    send(port, "return world.waitForChunks(30)", timeout=35)


def goto_and_resolve(port: int, gx: int, gy: int, screen_x: int, screen_y: int):
    """Points the camera at (gx, gy) and returns whatever tile ACTUALLY
    resolves under (screen_x, screen_y) via world.pickTile. The
    isometric projection plus that tile's own terrain height mean the
    screen-centre tile is essentially never exactly (gx, gy) (verified
    empirically in tools/portal_ghost_probe.py's center_on_tile) — worse,
    iteratively CORRECTING toward an exact match assumes a roughly
    linear pixel-to-tile shift, which rough/varied terrain around an
    arbitrary guessed coordinate can violate badly enough to diverge.
    So every caller here works with the RESOLVED tile instead of
    assuming convergence onto a specific guess. Navigating to the SAME
    (gx, gy) again is deterministic (fixed terrain + fixed camera
    target), so re-issuing this exact call later reliably reproduces
    the same resolved tile — that's what re-positions the camera before
    each click below, rather than a fragile iterative correction."""
    send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
    time.sleep(0.4)
    picked = send_json(port, f"return {{world.pickTile({screen_x}, {screen_y})}}")
    return (picked[0], picked[1]) if picked else None


def classify_candidate(port: int, def_name: str, gx: int, gy: int,
                       screen_x: int, screen_y: int) -> dict:
    """Resolve one seed coordinate and classify the tile it lands on.

    Every field a rejection could turn on is captured here (#2166
    requirement 2), including `canPlaceAt`'s own REASON and the full
    `remoteCheck` classification — the latter even when placement was
    refused, because `building.remoteCheck` is an independent, safe
    classification of any resolved tile (it asks about the nearest
    placed location, not about buildability), and a candidate rejected
    for being too close is a different fixture problem from one
    rejected for standing in water.

    A candidate whose tile never resolved has no placement or
    remoteness verdict at all; those fields stay None and are reported
    as unavailable rather than guessed."""
    record = {
        "seed": (gx, gy), "resolved": None,
        "valid": None, "reason": None,
        "remote": None, "distance": None, "threshold": None,
    }
    resolved = goto_and_resolve(port, gx, gy, screen_x, screen_y)
    if not resolved:
        return record
    rgx, rgy = resolved
    record["resolved"] = (rgx, rgy)
    load_region_around(port, rgx, rgy)
    record["valid"], record["reason"] = can_place_at(port, def_name, rgx, rgy)
    (record["remote"], record["distance"],
     record["threshold"]) = remote_check(port, def_name, rgx, rgy)
    return record


def describe_candidate(record: dict, want_remote: bool,
                       force_miss: bool = False) -> str:
    """One line naming this candidate's fate and the cause of it."""
    sx, sy = record["seed"]
    if record["resolved"] is None:
        return (f"seed ({sx},{sy}) -> REJECT: the camera resolved no tile "
                f"under the screen centre; canPlaceAt and remoteCheck are "
                f"unavailable for it")
    rgx, rgy = record["resolved"]
    reason = record["reason"] if record["reason"] is not None else "none given"
    remoteness = (f"remote={record['remote']} "
                  f"nearestDistance={record['distance']} "
                  f"thresholdTiles={record['threshold']}")
    if not record["valid"]:
        verdict = f"REJECT: canPlaceAt=false ({reason})"
    elif record["remote"] != want_remote:
        verdict = (f"REJECT: canPlaceAt=true but remote={record['remote']}, "
                   f"wanted remote={want_remote}")
    elif force_miss:
        verdict = ("ACCEPT, IGNORED: canPlaceAt=true and the remoteness "
                   "matches, but --force-search-miss is forcing this search "
                   "to reject everything")
    else:
        verdict = "ACCEPT: canPlaceAt=true and the remoteness matches"
    return f"seed ({sx},{sy}) -> tile ({rgx},{rgy}) {verdict}; {remoteness}"


def find_buildable(port: int, def_name: str, seed_targets, want_remote: bool,
                    screen_x: int, screen_y: int, force_miss: bool = False):
    """For each seed (gx, gy), resolve the tile ACTUALLY under the
    screen centre (goto_and_resolve) and classify THAT tile with the
    engine's own oracle (building.canPlaceAt / building.remoteCheck)
    rather than the seed coordinates themselves — an oracle-driven
    search, matching how ruin/location probes (e.g.
    tools/portal_ghost_probe.py) locate real worldgen features, robust
    to the seed itself landing on unbuildable or off-target terrain.

    Returns (hit, records): `hit` is (seed_gx, seed_gy, resolved_gx,
    resolved_gy, distance, thresholdTiles) or None, and `records` is
    every candidate classified before the search stopped — the caller
    prints them so a miss names its own cause without a rerun (#2166
    requirement 2). Re-navigating with the SEED coordinates
    (goto_and_resolve) later reliably reproduces the resolved tile."""
    records = []
    for gx, gy in seed_targets:
        record = classify_candidate(port, def_name, gx, gy, screen_x, screen_y)
        records.append(record)
        if force_miss:
            # --force-search-miss: classify EVERY candidate, accept none.
            # Deliberately not an emptied candidate list — the point of
            # exercising this path is to see the per-candidate report the
            # next real miss will print, and an empty list prints none.
            continue
        if record["valid"] and record["remote"] == want_remote:
            rgx, rgy = record["resolved"]
            return (gx, gy, rgx, rgy, record["distance"],
                    record["threshold"]), records
    return None, records


def report_candidates(label: str, records, want_remote: bool,
                      force_miss: bool = False) -> None:
    """Print every classified candidate under a named search."""
    print(f"  {label}: {len(records)} candidate"
          f"{'' if len(records) == 1 else 's'} classified"
          + (" [--force-search-miss]" if force_miss else ""))
    for record in records:
        print(f"    {describe_candidate(record, want_remote, force_miss)}")


def arm_portal_placement(port: int) -> None:
    send(port,
         "require('scripts.build_tool').enterPlacement("
         "{kind='building', def='" + PORTAL + "', isStarting=true}); "
         "return 'ok'")


def placement_mode(port: int) -> str:
    return send(port,
                "return require('scripts.build_tool').state.mode").strip('"')


def building_count(port: int, def_name: str) -> int:
    r = send(port, "return building.list()")
    return r.count(def_name)


def click_at_seed(port: int, seed_gx: int, seed_gy: int, resolved_gx: int,
                   resolved_gy: int, screen_x: int, screen_y: int) -> bool:
    """Re-navigates to the seed tile (deterministically reproducing the
    same resolved tile — see goto_and_resolve) and clicks screen centre.
    Returns whether the re-resolved tile still matches the one the
    caller already classified; the click always fires regardless (a
    mismatch is a probe-environment anomaly worth surfacing via the
    caller's own check, not a reason to skip exercising the feature)."""
    resolved = goto_and_resolve(port, seed_gx, seed_gy, screen_x, screen_y)
    send(port, f"return input.moveMouse({screen_x}, {screen_y})")
    send(port, f"return input.click({screen_x}, {screen_y})")
    return resolved == (resolved_gx, resolved_gy)


def remote_warning_phase(port: int, cx0: int, cy0: int, shots: str,
                          force_search_miss: bool = False) -> None:
    """The #779 gate, graded against the pinned fixture world.

    Both bounded searches below are FIXTURE preconditions, not product
    assertions (#2166 requirement 3): the remote-portal feature is not
    what fails when the pinned world stops carrying a position to build
    on. A miss is reported through `setup_check`, every candidate it
    rejected is printed with its cause, and the phase returns before
    grading anything — the run still exits non-zero, so a fixture that
    quietly stopped covering this feature cannot pass unnoticed."""
    print("== remote portal warning (#779) ==")

    located = send_json(port, "return world.listPlacedLocations()", timeout=10.0)
    located = located if isinstance(located, list) else []

    # -- find a valid, remote position: seed points scattered across the
    # world, resolved + oracle-classified rather than assumed (see
    # find_buildable/goto_and_resolve).
    remote_seeds = [
        (400, 400), (900, 300), (300, 900), (1400, 1400), (1700, 500),
        (500, 1700), (100, 1200), (1200, 100), (1900, 900), (900, 1900),
    ]
    remote_hit, remote_records = find_buildable(
        port, PORTAL, remote_seeds, want_remote=True,
        screen_x=cx0, screen_y=cy0, force_miss=force_search_miss)
    report_candidates("remote search", remote_records, want_remote=True,
                      force_miss=force_search_miss)
    if not setup_check(
            "the fixture world carries a valid remote buildable position",
            bool(remote_hit),
            f"none of the {len(remote_records)} candidate(s) above is both "
            f"buildable and beyond the remote threshold on this world "
            f"(seed 0x{FIXTURE['seed']}, worldSize {FIXTURE['worldSize']}, "
            f"plateCount {FIXTURE['plateCount']}). Nothing about the "
            f"remote-portal warning was graded — re-pin DEFAULT_SEED_HEX to a "
            f"world that carries one rather than reading this as a #779 "
            f"regression"):
        return
    rseed_gx, rseed_gy, rgx, rgy, rdist, rthr = remote_hit
    FIXTURE["remote"] = f"{rgx},{rgy}"
    print(f"  remote position ({rgx},{rgy}) distance={rdist} threshold={rthr}")

    # -- find a valid, NON-remote position near an actual placed
    # location (needs at least one placed location to be meaningful).
    # Both halves of that — the world having a placed location to derive
    # candidates from, and one of the derived candidates being usable —
    # are fixture preconditions of the same search.
    if not setup_check(
            "the fixture world has at least one placed location to derive "
            "nearby candidates from", bool(located),
            f"world.listPlacedLocations() reports no placed location on this "
            f"world (seed 0x{FIXTURE['seed']}), so the nearby search has no "
            f"anchor and the instant-commit half of #779 cannot be graded at "
            f"all"):
        return
    loc = located[0]
    lgx, lgy = loc.get("gx", 0), loc.get("gy", 0)
    nearby_seeds = [
        (lgx + dx, lgy + dy)
        for dx, dy in ((40, 0), (-40, 0), (0, 40), (0, -40),
                       (60, 20), (-60, -20), (20, 60), (-20, -60),
                       (90, 0), (-90, 0), (0, 90), (0, -90))
    ]
    nearby_hit, nearby_records = find_buildable(
        port, PORTAL, nearby_seeds, want_remote=False,
        screen_x=cx0, screen_y=cy0, force_miss=force_search_miss)
    report_candidates(f"nearby search (around the placed location at "
                      f"{lgx},{lgy})", nearby_records, want_remote=False,
                      force_miss=force_search_miss)
    if not setup_check(
            "the fixture world carries a valid nearby (non-remote) buildable "
            "position", bool(nearby_hit),
            f"none of the {len(nearby_records)} candidate(s) above is both "
            f"buildable and inside the remote threshold of the placed "
            f"location at ({lgx},{lgy}) on this world (seed "
            f"0x{FIXTURE['seed']}). Nothing about the remote-portal warning "
            f"was graded — re-pin DEFAULT_SEED_HEX rather than reading this "
            f"as a #779 regression"):
        return
    nseed_gx, nseed_gy, ngx, ngy, ndist, nthr = nearby_hit
    FIXTURE["nearby"] = f"{ngx},{ngy}"
    print(f"  nearby position ({ngx},{ngy}) distance={ndist} threshold={nthr}")

    before = building_count(port, PORTAL)

    # -- valid remote position renders a normal (white) ghost: hover it
    # while armed and confirm canPlaceAt (which drives the ghost tint)
    # still reports valid — remote is never coloured invalid/red.
    arm_portal_placement(port)
    hover_resolved = goto_and_resolve(port, rseed_gx, rseed_gy, cx0, cy0)
    check("camera resolves the remote position", hover_resolved == (rgx, rgy),
          f"got {hover_resolved}")
    send(port, f"return input.moveMouse({cx0}, {cy0})")
    time.sleep(0.3)
    valid_now, _ = can_place_at(port, PORTAL, rgx, rgy)
    check("remote position still reports canPlaceAt=true (white ghost, "
          "never red)", valid_now)
    shot_ghost = os.path.join(shots, "remote_ghost.png")
    check("remote-ghost screenshot answers", screenshot(port, shot_ghost))

    # -- clicking it presents the modal without spawning anything.
    check("click lands on the remote position",
          click_at_seed(port, rseed_gx, rseed_gy, rgx, rgy, cx0, cy0))
    time.sleep(0.3)
    modal_up = poll_until(5.0, lambda: find_widget(port, "Establish Here"))
    check("clicking the remote position presents the modal", bool(modal_up))
    check("no portal spawned while the modal is open",
          building_count(port, PORTAL) == before)
    shot_modal = os.path.join(shots, "remote_modal.png")
    check("modal screenshot answers", screenshot(port, shot_modal))

    # -- a second click while it's open cannot stack a second modal
    # (world clicks don't even pass through the modal boundary, so this
    # click is expected to land on the modal itself, not the world).
    send(port, f"return input.click({cx0}, {cy0})")
    time.sleep(0.3)
    establish_widgets = [w for w in widgets(port)
                         if (w.get("label") or "").strip().lower()
                         == "establish here"]
    check("repeated clicks do not stack a second modal",
          len(establish_widgets) == 1, f"found {len(establish_widgets)}")

    # -- "Choose Another Site" closes it, spawns nothing, placement
    # stays armed.
    check("click 'Choose Another Site'",
          click_widget(port, "Choose Another Site"))
    time.sleep(0.3)
    check("modal closed after Choose Another Site",
          not find_widget(port, "Establish Here"))
    check("still no portal spawned after cancel",
          building_count(port, PORTAL) == before)
    check("placement remains armed after cancel",
          placement_mode(port) == "placement")
    shot_cancelled = os.path.join(shots, "remote_cancelled.png")
    if check("post-cancel screenshot answers",
             screenshot(port, shot_cancelled)):
        check("post-cancel frame differs from the open-modal frame "
              "(modal gone, placement context unchanged underneath)",
              png_differs(shot_modal, shot_cancelled))

    # -- re-open, then "Establish Here" revalidates and spawns exactly
    # one portal, exiting placement.
    click_at_seed(port, rseed_gx, rseed_gy, rgx, rgy, cx0, cy0)
    modal_up2 = poll_until(5.0, lambda: find_widget(port, "Establish Here"))
    check("re-clicking the remote position re-presents the modal",
          bool(modal_up2))
    check("click 'Establish Here'", click_widget(port, "Establish Here"))
    time.sleep(0.5)
    check("exactly one portal placed after confirming",
          building_count(port, PORTAL) == before + 1,
          f"before={before} after={building_count(port, PORTAL)}")
    check("modal closed after confirming", not find_widget(port, "Establish Here"))
    check("placement mode exited after a confirmed remote placement",
          placement_mode(port) == "off")

    # -- a nearby valid position commits instantly, no modal. Reached
    # unconditionally now: the nearby search is a fixture precondition
    # above, so a run that gets here HAS one (#2166 requirement 3).
    before2 = building_count(port, PORTAL)
    arm_portal_placement(port)
    click_at_seed(port, nseed_gx, nseed_gy, ngx, ngy, cx0, cy0)
    time.sleep(0.5)
    check("clicking a nearby valid position never presents the modal",
          not find_widget(port, "Establish Here"))
    check("clicking a nearby valid position commits instantly "
          "(single click, no confirmation)",
          building_count(port, PORTAL) == before2 + 1,
          f"before={before2} after={building_count(port, PORTAL)}")


# --------------------------------------------------------------------------
# Location discovery-state map icons (#781) helpers
# --------------------------------------------------------------------------
def list_locations(port: int, page: str = "main_world") -> list[dict]:
    got = send_json(port, f"return world.listPlacedLocations('{page}')", timeout=10.0)
    return got if isinstance(got, list) else []


# The location icon's own on-screen size, in LOGICAL (window, not
# framebuffer) pixels: src/World/Render/Zoom/Icons.hs's
# locationIconTargetPixels, which iconWorldSize holds constant across
# every zoom level and DPI scale.
ICON_LOGICAL_PX = 32

# Side of the bounded region the type-icon comparison measures, as a
# multiple of the icon itself. The icon quad is centred on its anchor
# tile's world position (makeLocationIconQuads draws it at
# gridToWorld(anchor) minus half its size) and camera.goToTile puts the
# camera on that same world position, so the icon sits at the frame
# centre; three icons wide leaves a full icon of slack on either side
# for the projection's own sub-pixel rounding while still excluding
# everything else on screen.
ICON_ROI_SCALE = 3

# How much of the icon's own area must change for the marker to count as
# having resolved into the type icon. The two shipped bitmaps
# (assets/textures/icons/location/location_unknown.png and .../ruin.png)
# differ in 593 of their 1024 texels, so an eighth of the icon's area is
# a ~4.6x margin below the real signal while staying far above anything
# terrain or antialiasing contributes to a box this small.
ICON_MIN_CHANGED_FRACTION = 0.125


def icon_roi(png_w: int, png_h: int, logical_h: int):
    """The centred (x, y, w, h) box, in the capture's OWN pixels, that
    contains the location icon.

    Offscreen boots seed windowSize and framebufferSize from the same
    --size (app/App/Offscreen.hs), so the scale below is 1 today; it is
    derived from the capture rather than assumed so the box stays right
    if a capture ever comes back at a different scale."""
    scale = (png_h / logical_h) if logical_h > 0 else 1.0
    side = max(8, int(round(ICON_LOGICAL_PX * ICON_ROI_SCALE * scale)))
    side = min(side, png_w, png_h)
    x = (png_w - side) // 2
    y = (png_h - side) // 2
    return (x, y, side, side), scale


def zoom_fade_end(port: int) -> float:
    r = send(port, "return camera.getZoomFadeEnd()")
    return float(r)


def set_zoom(port: int, zoom: float) -> None:
    send(port, f"camera.setZoom({zoom}); return 'ok'")


def center_on(port: int, gx: int, gy: int) -> None:
    send(port, f"camera.goToTile({gx}, {gy}); return 'ok'")
    time.sleep(0.3)


def spawn_player_unit(port: int, gx: int, gy: int, page: str = "main_world") -> int:
    r = send(port,
              f"return unit.spawn('acolyte', {gx}, {gy}, nil, 'player', '{page}')")
    try:
        return int(float(r.strip('"')))
    except ValueError:
        return -1


def location_map_icons_phase(port: int, w: int, h: int, shots: str):
    """The #781/#1230 gate: lifecycle-state zoom-map icons, verified
    through screenshots + the world.listPlacedLocations() oracle against
    THIS run's real worldgen — a PINNED world (#2166: DEFAULT_SEED_HEX
    at the pinned size and plate count, confirmed by world.getSeed
    before this phase is reached), but never a hardcoded map coordinate
    or click position: which ruins exist, and where, is still whatever
    that oracle reports on the world that was generated.

    Since #1230 the pre-reveal marker is the ONE shared
    location_unknown.png rather than a per-definition undiscovered
    icon, and reveal is driven by SIGHT rather than by entering a
    discovery halo. What this phase can prove on screen is that the
    frame changes when a location is revealed and that an unseen
    control keeps its marker; that every definition draws the same
    unknown marker is proven where it can be — the pure Hspec group
    'Location map icons', which can register synthetic definitions,
    whereas this world has only ruin_small placed. The precise
    wrap/seam/duplicate-icon geometry is exhaustively covered there
    too — this phase proves the full GPU render pipeline actually
    surfaces that same behaviour on screen, not a second derivation of
    the wrap math."""
    print("== location map icons (#781/#1230) ==")
    cx0, cy0 = w // 2, h // 2

    locations = list_locations(port)
    if not check("world has at least two placed locations "
                 "(need one to reveal, one to leave unseen)",
                 len(locations) >= 2, f"found {len(locations)}"):
        return None
    target, control = locations[0], locations[1]
    tgx, tgy = target["gx"], target["gy"]
    ccx, ccy = control["gx"], control["gy"]

    fade_end = zoom_fade_end(port)
    full_zoom = fade_end * 1.5

    # -- full map visibility, centred on the target ruin, BEFORE any
    # player unit has seen it: the shared unknown marker. camera.goToTile
    # itself resets zoom (its "zoomSafe" branch, Engine.Scripting.Lua.
    # API.Camera.cameraGotoTileFn), so it must run BEFORE setZoom, never
    # after, or the map-visibility zoom gets clobbered back to 0.5.
    center_on(port, tgx, tgy)
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    check("target ruin starts unknown per world.listPlacedLocations()",
          not list_locations(port)[0].get("discovered")
          if list_locations(port) else False)
    shot_unknown = os.path.join(shots, "icon_unknown.png")
    check("unknown-marker screenshot answers",
          screenshot(port, shot_unknown))

    # -- the terrain/structure being physically visible (chunks around
    # the ruin loaded) must not, by itself, change the icon: load the
    # region and re-shoot before any unit is near it. This is checked at
    # the STATE level (world.listPlacedLocations' discovered flag), not
    # by pixel-diffing against 'shot_unknown' — loading a 2-chunk
    # radius legitimately repaints most of the frame with newly-visible
    # terrain, which would swamp any icon-sized pixel delta and make a
    # full-frame diff meaningless here. 'shot_loaded' below instead
    # becomes the TERRAIN-STABLE baseline the discovery comparison uses.
    load_region_around(port, tgx, tgy, radius_chunks=2)
    time.sleep(0.3)
    still_unknown = next(
        (loc for loc in list_locations(port)
         if loc.get("gx") == tgx and loc.get("gy") == tgy), None)
    check("loading the ruin's chunks (structure visible) does not "
          "discover it", bool(still_unknown)
          and not still_unknown.get("discovered"))
    shot_loaded = os.path.join(shots, "icon_loaded_not_discovered.png")
    check("post-load screenshot answers", screenshot(port, shot_loaded))

    # -- a player-faction unit that SEES the target ruin (#1230; spawned
    # on its anchor, and a unit's own tile is always in its visible set)
    # flips ONLY that ruin to discovered.
    player_uid = spawn_player_unit(port, tgx, tgy)
    check("player unit spawned at the target ruin", player_uid >= 0)
    target_discovered = poll_until(10.0, lambda: next(
        (loc.get("discovered") for loc in list_locations(port)
         if loc.get("gx") == tgx and loc.get("gy") == tgy), False))
    check("seeing the target ruin flips it to discovered",
          bool(target_discovered))
    control_still_hidden = next(
        (loc for loc in list_locations(port)
         if loc.get("gx") == ccx and loc.get("gy") == ccy), None)
    check("the unseen control ruin keeps the shared unknown marker",
          bool(control_still_hidden)
          and not control_still_hidden.get("discovered"))

    # -- #1765: `location_discovery` ships as a POPUP category
    # (data/notification_categories.yaml), so the reveal above normally
    # raises a screen-centred notification card. Left up it defeats the
    # comparison below twice over: it changes far more pixels than the
    # icon does, and it covers the very icon being compared. Wait for it
    # (the event is delivered asynchronously, so "not up yet" is not the
    # same as "not coming"), dismiss it through its own OK control, and
    # prove the frame is card-free BEFORE capturing. A machine whose
    # config/notifications.local.yaml has turned the category's popup
    # off is tolerated — there is simply nothing to wait for — but the
    # card-free assertion still has to hold.
    if notification_popup_enabled(port, "location_discovery"):
        arrived = poll_until(10.0, lambda: popup_counts(port)[0] > 0)
        check("the discovery notification card is raised (the shipped "
              "location_discovery popup)", bool(arrived))
    check_no_popup(port, "type-icon capture")

    time.sleep(0.3)
    shot_discovered = os.path.join(shots, "icon_type.png")
    if check("type-icon screenshot answers",
             screenshot(port, shot_discovered)):
        # Compared against 'shot_loaded' (same loaded terrain, taken
        # right before this unit spawned) rather than 'shot_unknown'
        # — see the note above on why a terrain-stable baseline is the
        # only pixel-diff pair that isolates the discovery-driven change.
        #
        # #1765: and compared over a BOUNDED box around the icon rather
        # than the whole frame. The reasoning the terrain note above
        # gives for the previous comparison applies here unchanged: a
        # whole-frame diff passes on any visible change anywhere, so it
        # cannot distinguish "the marker resolved into the type icon"
        # from "something else on screen moved". Both captures come from
        # the same camera position and zoom, so the identical box in each
        # frames the same icon.
        st = png_stats(shot_discovered)
        if check("type-icon capture is readable", bool(st)):
            box, scale = icon_roi(st[0], st[1], h)
            min_changed = max(
                1, int(round(ICON_MIN_CHANGED_FRACTION
                             * (ICON_LOGICAL_PX * scale) ** 2)))
            changed = png_region_changed_pixels(shot_loaded, shot_discovered,
                                                box)
            detail = (f"region (x={box[0]}, y={box[1]}, w={box[2]}, "
                      f"h={box[3]}): "
                      + ("frames not comparable" if changed is None
                         else f"{changed} of {box[2] * box[3]} px changed, "
                              f"needed >= {min_changed}"))
            print(f"    icon region diff — {detail}")
            check("the icon itself visibly changes once the ruin is seen — "
                  "the shared unknown marker resolves into the ruin type "
                  "icon", changed is not None and changed >= min_changed,
                  detail)

    # -- readable at a second, different map zoom level.
    set_zoom(port, full_zoom * 1.6)
    time.sleep(0.3)
    # #1765: every downstream capture in this phase is taken under the
    # same conditions, so each one clears any card that arrived since the
    # last capture rather than inheriting it.
    check_no_popup(port, "second-zoom capture")
    shot_zoom2 = os.path.join(shots, "icon_zoom2.png")
    if check("second-zoom-level screenshot answers",
             screenshot(port, shot_zoom2)):
        st = png_stats(shot_zoom2)
        check("second-zoom-level frame is not blank",
              bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")
    # -- rotating the map moves the icon with its location (the frame
    # changes) while the render pipeline keeps working; upright-ness
    # itself is proven exactly by the Hspec group's axis-aligned-square
    # assertion across all four facings, not re-derived from pixels here.
    # (goToTile-before-setZoom again — see the note above.)
    center_on(port, tgx, tgy)
    set_zoom(port, full_zoom)
    time.sleep(0.3)
    check_no_popup(port, "pre-rotation capture")
    shot_pre_rotate = os.path.join(shots, "icon_pre_rotate.png")
    screenshot(port, shot_pre_rotate)
    send(port, "camera.rotateCW(); return 'ok'")
    time.sleep(0.5)
    # The rotation comparison stays a deliberate WHOLE-frame diff (it is
    # a generic "the GPU path still renders and updates" check, not an
    # icon assertion), which is exactly why it too must be free of a card
    # that would satisfy it on its own.
    check_no_popup(port, "post-rotation capture")
    shot_rotated = os.path.join(shots, "icon_rotated.png")
    if check("post-rotation screenshot answers", screenshot(port, shot_rotated)):
        check("the frame changes after rotating the camera",
              png_differs(shot_pre_rotate, shot_rotated))
    send(port, "camera.rotateCCW(); return 'ok'")
    time.sleep(0.3)

    # -- save (quit -> fresh restart -> load happens back in main(), the
    # same shape every other save-persistence check in this probe suite
    # uses — see 'location_map_icons_reload_check').
    check("save the world", "true" in send(
        port, "engine.saveWorld('main_world', 'offscreen_icon_test'); "
              "return 'true'").lower())
    time.sleep(0.5)
    return (tgx, tgy)


def location_map_icons_reload_check(port: int, tgx: int, tgy: int) -> None:
    """Continuation of 'location_map_icons_phase' after a fresh restart +
    load (run against a NEW engine instance, mirroring how the rest of
    this probe's own quit/restart pattern works elsewhere)."""
    # The debug console answering (what boot() itself waits for) is a
    # much earlier milestone than the real UI flow's unit/building/
    # item/location content defs actually being loaded -- those load
    # asynchronously via scripts/*.lua's own startup_loader once the
    # loading screen reaches the main menu (see the identical wait at
    # this file's own "1. real UI flow" step). Calling engine.loadSave
    # before that finishes raced an essentially empty content registry
    # against issue #760/#763's own pre-publication content-reference
    # validation, which correctly rejects a load referencing defs that
    # were never actually loaded yet -- not a defect in that
    # validation, a missing readiness wait here.
    menu_up = poll_until(60.0, lambda: find_widget(port, "Create World"))
    if menu_up is None:
        print("  WARNING: main menu never became ready before the reload "
              "attempt -- proceeding anyway, load will likely fail")
    send(port, "engine.loadSave('offscreen_icon_test'); return 'queued'")
    # Issue #763: engine.loadSave only ACCEPTS synchronously -- the saved
    # page doesn't exist live (and world.waitForInit/getInitProgress
    # resolve nothing) until the whole-session transaction publishes.
    published, load_status = wait_load_published(port, 120)
    check("load transaction published", published)
    if not published:
        print(f"  (getLoadStatus: {load_status})")
    send(port, "return world.waitForInit(120)", timeout=125.0)
    send(port, "world.show('main_world'); return 'ok'")
    time.sleep(0.5)
    reloaded = next(
        (loc for loc in list_locations(port)
         if loc.get("gx") == tgx and loc.get("gy") == tgy), None)
    check("the discovered icon state survives save -> quit -> restart -> load",
          bool(reloaded) and bool(reloaded.get("discovered")))


def debug_console_load_rebind_phase(port: int) -> None:
    """Round 11 review, issue #763: a direct debug-console engine.loadSave
    while gameplay is already open bypasses main_menu.lua's
    loadingScreen-driven rebind (worldManager.currentWorld, hud.worldId,
    resent textures) entirely -- that path only runs via
    mainMenu.loadAndShowSave's own loadingScreen.setOnLoadReady callback.
    scripts/ui_manager_menu.lua's onSaveLoaded (broadcast on EVERY load,
    any trigger) must do this rebind itself. Requires the caller to
    already be in-game (worldManager.currentWorld bound to the world
    generated by an earlier phase in this same process)."""
    current = send(port, "return require('scripts.world_manager').currentWorld")
    check("precondition: gameplay is open on a real page before the "
          "debug-console load", current.strip().strip("'\"") not in ("", "nil"))
    started_on = current.strip().strip("'\"")

    # A second, distinct, small page -- saved under its OWN slot so it
    # keeps its own id on load (#763: no remap to main_world).
    send(port, "world.init('debug_rebind_b', 777, 8, 3); return 'ok'")
    # world.waitForInit returns the same (phase, current, total, stage)
    # tuple as world.getInitProgress(), not a boolean -- phase 3 is done.
    init_result = send(port, "return world.waitForInit(60)", timeout=65.0)
    check(f"debug_rebind_b init completes (waitForInit={init_result!r})",
          "done" in init_result.lower())
    send(port, "world.show('debug_rebind_b'); return 'ok'")

    # engine.saveWorld rejects outright while another save/load
    # transaction is still in flight (the SAME mutual-exclusion contract
    # tools/transactional_load_probe.py exercises for loads) -- the
    # location-map-icons phase just above issued its own save without
    # itself waiting for the barrier to fully settle, so wait here
    # rather than race it.
    settled = False
    for _ in range(100):
        status = send_json(port, "return engine.getSaveStatus()")
        if not isinstance(status, dict) or status.get("outcome") is not None:
            settled = True
            break
        time.sleep(0.1)
    check("the prior save's transaction settled before this one starts",
          settled)

    # world.waitForInit/getInitProgress track a single shared progress
    # ref that world.init resets only once the world thread actually
    # starts processing the queued command -- calling waitForInit
    # immediately after a second world.init can observe the PREVIOUS
    # page's stale "done" before the new page's own init has even
    # started, well before its gen params exist (pre-existing race,
    # unrelated to this issue -- worked around here with a bounded
    # retry rather than guessing a fixed extra delay).
    save_accepted = "false"
    for _ in range(20):
        save_accepted = send(
            port, "return engine.saveWorld('debug_rebind_b', 'debug_rebind_test')")
        if save_accepted.strip().lower() == "true":
            break
        time.sleep(0.5)
    check(f"save the second page under its own slot (got {save_accepted!r})",
          save_accepted.strip().lower() == "true")
    # The world thread writes the save file asynchronously after the API
    # call returns -- poll for it rather than guessing a fixed delay
    # (mirrors tools/multiworld_save_probe.py's own wait).
    save_file = os.path.join("saves", "debug_rebind_test", "world.synworld")
    for _ in range(100):
        if os.path.exists(save_file):
            break
        time.sleep(0.1)
    check("save file for debug_rebind_test appeared on disk",
          os.path.exists(save_file))

    # Back to the ORIGINAL page -- gameplay "is open" on it, exactly the
    # precondition the reviewed bug describes.
    send(port, f"world.show('{started_on}'); return 'ok'")
    time.sleep(0.3)
    rebound_before = send(
        port, "return require('scripts.world_manager').currentWorld")
    check("still bound to the original page just before the debug load",
          rebound_before.strip().strip("'\"") == started_on)

    # THE bypass: a raw debug-console engine.loadSave, never routed
    # through main_menu.lua's loadAndShowSave/loadingScreen at all.
    check("debug-console engine.loadSave accepted", "true" in send(
        port, "return engine.loadSave('debug_rebind_test')").lower())
    published, load_status = wait_load_published(port, 60)
    check("debug-console load transaction published", published)
    if not published:
        print(f"  (getLoadStatus: {load_status})")
    send(port, "return world.waitForInit(60)", timeout=65.0)
    time.sleep(0.5)

    active_id = send(port, "return world.getActiveWorldId()").strip().strip("'\"")
    check("the loaded page keeps its own id (debug_rebind_b, never "
          "remapped)", active_id == "debug_rebind_b")

    wm_current = send(
        port, "return require('scripts.world_manager').currentWorld"
    ).strip().strip("'\"")
    check("worldManager.currentWorld rebinds to the newly-loaded page, "
          "not left on the pre-load page", wm_current == "debug_rebind_b")

    hud_world_id = send(
        port, "return require('scripts.hud').worldId"
    ).strip().strip("'\"")
    check("hud.worldId rebinds to the newly-loaded page, not left on "
          "the pre-load page", hud_world_id == "debug_rebind_b")


# --------------------------------------------------------------------------
# Pinning the fixture world through the create-world screen (#2166)
# --------------------------------------------------------------------------
def pin_generation_params(port: int, seed_hex: str, world_size: str,
                          plate_count: str) -> dict:
    """Pin seed, world size and plate count through the create-world
    screen's OWN controls, and read back what the screen now holds.

    Each value goes through the same mutation a player's keystroke or
    click ends at — ``randbox.setValue`` on the seed control,
    ``dropdown.selectOption`` on the world-size dropdown, and
    ``textbox.setText`` on the advanced tab's plate-count box — so the
    screen, its defaults, and how ``create_world/generation.lua``
    resolves them are left exactly as shipped (#2166 out-of-scope).

    Going through the plate-count TEXTBOX specifically, rather than
    writing ``pending.plateCount``, is load-bearing: every tab is built
    eagerly (``create_world_menu.lua``'s ``advancedTab.create``), and
    ``generation.start`` re-reads ``advancedTab.getWidgetValues()`` over
    ``pending`` on its first lines — a direct ``pending`` write would be
    silently overwritten by the widget at the moment it matters.

    Returns the screen's own read-back of all three."""
    got = send_json(
        port,
        "local m = require('scripts.create_world_menu'); "
        "local st = require('scripts.create_world.settings_tab'); "
        "local adv = require('scripts.create_world.advanced_tab'); "
        "local rb = require('scripts.ui.randbox'); "
        "local dd = require('scripts.ui.dropdown'); "
        "local tb = require('scripts.ui.textbox'); "
        "if st.seedRandBoxId then "
        f"rb.setValue(st.seedRandBoxId, '{seed_hex}') end; "
        "if st.sizeDropdownId then "
        "  for i, opt in ipairs(st.worldSizeOptions) do "
        f"    if opt.value == '{world_size}' then "
        "      dd.selectOption(st.sizeDropdownId, i) end end end; "
        "if adv.plateCountTextBoxId then "
        f"tb.setText(adv.plateCountTextBoxId, '{plate_count}') end; "
        "return {seed = tostring(m.pending.seed), "
        "        seedControl = st.seedRandBoxId "
        "            and tostring(rb.getValue(st.seedRandBoxId)) or 'NO-CONTROL', "
        "        worldSize = tostring(m.pending.worldSize), "
        "        plateCount = adv.plateCountTextBoxId "
        "            and tostring(tb.getValue(adv.plateCountTextBoxId)) "
        "            or tostring(m.pending.plateCount)}",
        timeout=10.0)
    return got if isinstance(got, dict) else {}


def resolved_gen_params(port: int) -> dict:
    """(worldSize, plateCount) as the create-world menu FINALLY resolved
    them — read after ``generation.start``, which writes every tab's
    widget values back into ``pending``, so this is the pair worldgen
    was actually handed."""
    got = send_json(
        port,
        "local m = require('scripts.create_world_menu'); "
        "return {worldSize = tostring(m.pending.worldSize), "
        "        plateCount = tostring(m.pending.plateCount)}",
        timeout=10.0)
    return got if isinstance(got, dict) else {}


def live_world_seed(port: int) -> int | None:
    """The generated world's OWN seed, or None when it cannot be read.

    This is the check that matters: it proves the pinned hex survived
    ``generation.lua``'s ``tonumber(seed, 16)`` and reached worldgen,
    not merely that a control read back the right text."""
    raw = send(port, "return world.getSeed()", timeout=10.0).strip()
    try:
        return int(float(raw))
    except (TypeError, ValueError):
        return None


class Engines:
    """The offscreen engines booted and not yet stopped (#1323).

    This probe runs THREE engines: the main one, a briefly-parallel
    second, and — only when the icon phase produced a target — a third
    that restarts on the FIRST one's port after it was stopped. So the
    teardown that has to survive an unexpected exception cannot be a flat
    "quit all three at the end": it would miss the not-yet-created one,
    re-quit the deliberately stopped ones, and send a quit for a stale
    handle to whatever now holds a reused port. Registering each handle
    the moment it boots — before the fallible work that follows it — and
    dropping it again on a deliberate stop keeps the set exact.

    Each launch also gets its OWN log (#1763). `probelib.boot`'s default
    path is derived from the port and opened truncating, so the third
    engine — which deliberately restarts on the FIRST one's port — used
    to overwrite the capture of the long session that preceded it: the
    loading screen, worldgen, gameplay and icon discovery, replaced by
    the brief load-and-check that follows. Allocation happens here, in
    the one place every launch passes through, so the count follows the
    launches actually made rather than the three call sites (the third
    is conditional, and `--skip-worldgen` makes two).
    """

    LOG_DIR = "/tmp"
    LOG_PREFIX = "offscreen_probe_engine"

    def __init__(self) -> None:
        self._live: dict[int, object] = {}
        self._logs: list[tuple[str, int, str]] = []

    def _allocate_log(self, port: int, phase: str) -> str:
        """Reserve (and announce) this launch's own log path.

        Announced immediately rather than only in the closing summary:
        `boot` calls `sys.exit` when an engine dies before READY, and a
        failed boot is precisely the one whose log a reader wants.
        """
        ordinal = len(self._logs) + 1
        slug = re.sub(r"[^a-z0-9]+", "-", phase.lower()).strip("-") or "boot"
        path = os.path.join(self.LOG_DIR,
                            f"{self.LOG_PREFIX}_{ordinal:02d}_{slug}.log")
        self._logs.append((phase, port, path))
        print(f"  engine log [{ordinal:02d}] {phase} (port {port}): {path}")
        return path

    def start(self, port: int, phase: str, **kw) -> None:
        self._live[port] = boot(port, log=self._allocate_log(port, phase), **kw)

    def stop(self, port: int) -> None:
        """Deliberate shutdown; a later boot may reuse this port."""
        proc = self._live.pop(port, None)
        if proc is not None:
            quit_engine(port, proc)

    def stop_all(self) -> None:
        for port in reversed(list(self._live)):
            self.stop(port)

    def report_failure_context(self, emitter: FailureEmitter) -> None:
        """Name every engine log beside the failure records (#1982).

        A fixture failure and a product failure look identical in a check
        name and different in the last few lines the engine wrote, so the
        durable block carries a bounded tail of each launch's own log.
        """
        for ordinal, (phase, port, path) in enumerate(self._logs, start=1):
            emitter.context_log(path,
                                label=f"engine log {ordinal:02d} ({phase}, "
                                      f"port {port})")

    def report_logs(self) -> None:
        """Name every log this run wrote, against the launch that wrote it."""
        if not self._logs:
            print("\nno engine was booted, so this run wrote no engine logs")
            return
        print(f"\nengine logs from this run ({len(self._logs)} boot"
              f"{'' if len(self._logs) == 1 else 's'}):")
        for ordinal, (phase, port, path) in enumerate(self._logs, start=1):
            print(f"  {ordinal:02d}. {phase} (port {port}): {path}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9418)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--skip-worldgen", action="store_true",
                    help="skip the Generate World -> HUD phase (~1 min)")
    ap.add_argument("--seed", default=DEFAULT_SEED_HEX,
                    help="hex world seed to pin the fixture world to "
                         f"(default {DEFAULT_SEED_HEX})")
    ap.add_argument("--force-search-miss", action="store_true",
                    help="classify every candidate in both bounded searches "
                         "but accept none, to exercise the fixture/setup "
                         "failure path end to end without editing this file")
    args = ap.parse_args()

    seed_hex = args.seed.strip().upper()
    try:
        int(seed_hex, 16)
    except ValueError:
        ap.error(f"--seed must be hexadecimal (got {args.seed!r})")
    FIXTURE["seed"] = seed_hex
    if args.skip_worldgen:
        FIXTURE["remote"] = TILE_SKIPPED
        FIXTURE["nearby"] = TILE_SKIPPED

    w, h = (int(v) for v in args.size.lower().split("x"))
    shots = tempfile.mkdtemp(prefix="offscreen_probe_")

    engines = Engines()
    try:
        return _run(args, seed_hex, w, h, shots, engines)
    finally:
        engines.stop_all()
        # After stop_all, and in a `finally`, so the map survives the runs
        # that do not reach the summary at the end of `_run` — including a
        # `boot` that exits before its engine ever printed READY.
        engines.report_logs()
        # Durable failure records, in the runner's own shared vocabulary
        # (#1982): `setup` for a fixture that stopped fitting, `check` for
        # a product assertion. Emitted here rather than at the end of
        # `_run` so a run that died mid-phase still names what it had
        # already failed.
        if failures or setup_failures:
            FAILURE.report(failures, setup_failures)
            engines.report_failure_context(FAILURE)
        # Same reason (#2166 requirement 4): EVERY run says which world it
        # was grading, including one that died before reaching a verdict.
        print_fixture()


def _run(args, seed_hex: str, w: int, h: int, shots: str,
         engines: Engines) -> int:
    print(f"== offscreen boot (port {args.port}, {args.size}) ==")
    engines.start(args.port, phase="main session (menu, worldgen, gameplay)",
                  mode=("--offscreen",),
                  args=["--size", args.size], label="offscreen engine")

    # -- 1. real UI flow: the loading screen finishes and the menu's
    # widgets exist. GPU-less --headless never gets here, so this is
    # the "full UI stack actually booted" assertion.
    menu_up = poll_until(60.0, lambda: find_widget(args.port, "Create World"))
    check("loading screen -> main menu (Create World widget visible)",
          bool(menu_up))

    # -- 2. non-blank screenshot at the requested size.
    shot_menu = os.path.join(shots, "menu.png")
    check("captureScreenshot answers", screenshot(args.port, shot_menu))
    st = png_stats(shot_menu)
    check(f"menu PNG valid at {w}x{h}", bool(st) and st[0] == w and st[1] == h,
          f"got {st}")
    check("menu PNG is not blank (>= 3 distinct colors)",
          bool(st) and st[2] >= 3, f"distinct colors: {st and st[2]}")

    # -- 3. windowless input injection: click Create World via the F3
    # oracle's bounds and require both the widget change and a visibly
    # different frame.
    check("click 'Create World' (bounds from ui.dumpWidgets)",
          click_widget(args.port, "Create World"))
    create_up = poll_until(20.0, lambda: find_widget(args.port, "Generate World"))
    check("create-world screen reached (Generate World widget visible)",
          bool(create_up))
    shot_create = os.path.join(shots, "create.png")
    if check("second screenshot answers", screenshot(args.port, shot_create)):
        check("create-world frame differs from menu frame",
              png_differs(shot_menu, shot_create))

    # -- pin the fixture world (#2166) BEFORE anything is generated.
    # Left alone this screen rolls a fresh random seed on every visit,
    # which made phases 6 and 7 a per-run coin flip. Done here, after
    # the frame comparison just above, so pinning cannot perturb what
    # that comparison measures.
    pinned = pin_generation_params(args.port, seed_hex, DEFAULT_WORLD_SIZE,
                                   DEFAULT_PLATE_COUNT)
    for field, want in (("seed", seed_hex),
                        ("seedControl", seed_hex),
                        ("worldSize", DEFAULT_WORLD_SIZE),
                        ("plateCount", DEFAULT_PLATE_COUNT)):
        setup_check(
            f"create-world screen pinned: {field} = {want}",
            pinned.get(field) == want,
            f"the create-world screen reads {field}={pinned.get(field)!r}, "
            f"not the pinned {want!r}. Without every one of these the "
            f"generated world is not the fixture and nothing graded against "
            f"it means what it says")
    FIXTURE["worldSize"] = pinned.get("worldSize", DEFAULT_WORLD_SIZE)
    FIXTURE["plateCount"] = pinned.get("plateCount", DEFAULT_PLATE_COUNT)

    # -- 4. parallel instances: a second engine while the first runs.
    # Both engines are up at once, so `--port` is a BASE reserving two
    # ports — which is why `probe_runner_registry.PROBE_PORT_SPANS` declares 2 for
    # this probe (#1571).
    port2 = args.port + 1
    print(f"== parallel second instance (port {port2}) ==")
    engines.start(port2, phase="parallel second instance",
                  mode=("--offscreen",),
                  args=["--size", args.size], label="second offscreen engine")
    menu2 = poll_until(60.0, lambda: find_widget(port2, "Create World"))
    check("second instance reaches its own menu", bool(menu2))
    shot2 = os.path.join(shots, "second.png")
    check("second instance captures its own screenshot",
          screenshot(port2, shot2) and bool(png_stats(shot2)))
    check("first instance still answering alongside the second",
          bool(find_widget(args.port, "Generate World")))
    engines.stop(port2)

    # -- 5. through real worldgen to the in-game HUD.
    if not args.skip_worldgen:
        print("== Generate World -> in-game HUD (takes ~1 min) ==")
        check("click 'Generate World'", click_widget(args.port, "Generate World"))

        def world_done():
            got = send(args.port, "local p = world.getInitProgress(); return p",
                       timeout=5.0)
            return got.strip() == "3"  # phase 3 = done

        check("worldgen completes (phase 3)", bool(poll_until(300.0, world_done,
                                                              interval=2.0)))
        # Generation done -> the screen offers Regenerate/Continue;
        # Continue is the click that actually enters the game.
        cont = poll_until(60.0, lambda: find_widget(args.port, "Continue"))
        check("post-generation Continue button appears", bool(cont))
        check("click 'Continue'", click_widget(args.port, "Continue"))
        hud_up = poll_until(60.0, lambda: not find_widget(args.port, "Continue"))
        check("create-world screen dismissed (in-game view)", bool(hud_up))
        time.sleep(3.0)  # let the first world frames render
        shot_hud = os.path.join(shots, "hud.png")
        if check("in-game screenshot answers",
                 screenshot(args.port, shot_hud)):
            check("in-game frame differs from create-world frame",
                  png_differs(shot_create, shot_hud))
        got = send_json(args.port, "return world.getChunkInfo(0, 0)", timeout=10.0)
        check("world query answers in-game", isinstance(got, dict))

        # -- the world that came out is the world that was pinned. Read
        # from the live page rather than from the menu, so this proves
        # the hex survived generation.lua's tonumber(seed, 16) and
        # reached worldgen. The size/plate pair is re-read here too:
        # generation.start rewrites `pending` from every tab's widgets on
        # its first lines, so the values it actually generated with are
        # only knowable afterwards.
        resolved = resolved_gen_params(args.port)
        FIXTURE["worldSize"] = resolved.get("worldSize", FIXTURE["worldSize"])
        FIXTURE["plateCount"] = resolved.get("plateCount", FIXTURE["plateCount"])
        want_seed = int(seed_hex, 16)
        live_seed = live_world_seed(args.port)
        seed_pinned = setup_check(
            f"the generated world carries the pinned seed {want_seed} "
            f"(0x{seed_hex})",
            live_seed == want_seed,
            f"world.getSeed() reports "
            f"{'nothing readable' if live_seed is None else live_seed}, not "
            f"the pinned {want_seed}. The world in front of the portal and "
            f"icon phases is not the fixture they were verified against, so "
            f"grading them here would report on the wrong world")
        if not seed_pinned:
            # The identity line has to name the world that was actually
            # generated, not the one that was asked for — an identity
            # that reports the request would make two runs on two
            # different worlds compare as the same fixture.
            actual = "UNREADABLE" if live_seed is None else f"{live_seed:08X}"
            FIXTURE["seed"] = f"{actual}(NOT-THE-PINNED-{seed_hex})"
        size_pinned = setup_check(
            f"the generated world used the pinned worldSize "
            f"{DEFAULT_WORLD_SIZE} / plateCount {DEFAULT_PLATE_COUNT}",
            resolved.get("worldSize") == DEFAULT_WORLD_SIZE
            and resolved.get("plateCount") == DEFAULT_PLATE_COUNT,
            f"generation resolved worldSize={resolved.get('worldSize')!r} "
            f"plateCount={resolved.get('plateCount')!r}, not the pinned pair. "
            f"The same seed at a different size or plate count is a "
            f"different world")
        world_is_pinned = seed_pinned and size_pinned

        # -- 6. remote portal warning (#779), against this same world.
        # -- 7. location discovery-state map icons (#781), against this
        # same world.
        # Both are skipped outright when the world is not the pinned one:
        # a verdict from either would be a verdict about some other
        # world. The run still exits non-zero on the setup failure above.
        icon_target = None
        if world_is_pinned:
            remote_warning_phase(args.port, w // 2, h // 2, shots,
                                 force_search_miss=args.force_search_miss)
            icon_target = location_map_icons_phase(args.port, w, h, shots)
        else:
            print("== portal + icon phases SKIPPED (the generated world is "
                  "not the pinned fixture) ==")

        # -- 8. debug-console engine.loadSave while gameplay is already
        # open rebinds worldManager/hud, not just world.getActiveWorldId()
        # (round 11 review, issue #763). Runs LAST on this instance since
        # it switches the active page away from 'main_world' permanently.
        print("== debug-console load while gameplay is open (#763 round 11) ==")
        debug_console_load_rebind_phase(args.port)
    else:
        icon_target = None

    engines.stop(args.port)

    # -- 7 (cont'd): fresh restart -> load, proving the discovered icon
    # state (not the one-off in-process view) actually round-trips.
    if icon_target:
        print("== fresh restart -> load (icon persistence, #781) ==")
        engines.start(args.port, phase="icon-reload restart",
                      mode=("--offscreen",),
                      args=["--size", args.size],
                      label="offscreen engine (icon reload)")
        location_map_icons_reload_check(args.port, *icon_target)
        engines.stop(args.port)

    print(f"\nscreenshots kept in {shots}")
    print_fixture()
    # Both totals, always, and separately (#2166 requirement 3): a
    # fixture that stopped fitting and a product regression are
    # different problems with different repairs, and a reader who sees
    # only "N checks FAILED" cannot tell which one this run found.
    print(f"offscreen_probe: {len(failures)} product assertion(s) failed, "
          f"{len(setup_failures)} fixture/setup precondition(s) failed")
    if setup_failures:
        print("offscreen_probe: FIXTURE/SETUP FAILURE — the pinned world no "
              "longer carries what a phase needs; re-pin DEFAULT_SEED_HEX "
              "(or pass --seed) rather than reading this as a product "
              "regression")
    if failures or setup_failures:
        return 1
    print("offscreen_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
