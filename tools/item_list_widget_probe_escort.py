#!/usr/bin/env python3
"""The two escort-session scenarios of
`tools/item_list_widget_probe.py` (#2046), with their movement staging,
AI-action observation and two-sided hold helpers.

  * `escort_session_scenario` — #1250's Mode A pair over a BUILDING
    destination: two flanking panes as one non-modal stack level, the
    directional row menus, a committing Store all, a real resize, and
    one dismissal closing both.
  * `unit_escort_session_scenario` — #1251's unit-to-unit session, whose
    SETUP half this is: three terminal `setup:` preconditions (#1911),
    each of which ends the scenario rather than grading a measurement
    against a fixture the probe has already reported invalid.
  * `escort_hold_measurement` — everything downstream of those three,
    kept in its own function so it CANNOT run against a rejected pair.

The staging helpers below them — `spawn_pair_apart`, `order_target_away`,
`accepts_movement`, `stage_escort_separation`, `end_escort_setup`,
`EscortSeparation` and the `unit_pos`/`tile_gap`/`chebyshev`/`ai_action`/
`retire_medic_drive` observations — have no consumer outside this module,
so the whole precondition and staging sequence stays together: #1911's
open repair of the non-terminal reach precondition remains one focused
edit in one file.

Spawning the escort pair, loading their chunks and walking them are this
scenario's own fixture staging and stay here. What no scenario module
does is boot an engine, generate a world, open a port or tear an engine
down — those belong to the facade alone.
"""
from __future__ import annotations

import math
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check
from item_list_widget_probe_oracle import (click_widget_center, close_menu,
                                           find_widget, item_rows,
                                           level_list_id, open_window_on,
                                           right_click_widget_center,
                                           stack_dump, widgets)
from item_list_widget_probe_terrain import CHUNK_TILES, tile_surface
from probelib import (clear_find_water, poll_until, send, send_json,
                      set_paused, viewport)


# --------------------------------------------------------------------------
# #1250: the Mode A escort session's rendered pair
# --------------------------------------------------------------------------
def pane_list_id(pane_key: str) -> str:
    """A debug-console expression naming one escort pane's widget
    instance — the same shape `level_list_id` gives a single-pane
    level, addressed through the manager's own pane accessor rather
    than by indexing `panes` positionally."""
    return ("(function() local c = require('scripts.cargo_inventory_panel');"
            f" local p = c.getPane(c.getLevel(1), '{pane_key}');"
            " return p and p.listId end)()")


def escort_session_scenario(port: int, bid: int, bax: int, bay: int,
                            vp: dict) -> None:
    """#1250: the rendered escort pair.

    Its own acolyte, spawned ON the cargo's anchor tile so the contract's
    footprint rule already reports it in reach — the WALK is the headless
    gate's business (and the AI's), while what needs a GPU here is the
    pair of real panels and the real row menus on them. Everything below
    is located through the widget oracle and the manager's dump; not one
    screen coordinate is written down.

    Deliberately last of the transfer scenarios: it opens a window at the
    base level, which replaces whatever else was open, and it leaves the
    stack empty again on the way out."""
    print("== #1250 escort session (the rendered flanking pair) ==")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")
    esc_raw = send(port, f"return unit.spawn('acolyte', {bax}, {bay},"
                         " nil, 'player')")
    if not check("escort acolyte spawned on the cargo's own tile",
                 esc_raw.strip() not in ("", "nil", "null"), f"got {esc_raw!r}"):
        return
    esc_uid = int(float(esc_raw))
    # A freshly spawned acolyte carries the standing `find_water` goal,
    # whose search utility competes with the escort hold for this very
    # unit. Retiring it is what makes the arrival below a measurement of
    # the ESCORT rather than a race against thirst on whatever terrain
    # this run's world happened to generate.
    check("the escort's standing find_water goal is retired",
          clear_find_water(port, esc_uid))
    # ...and so is the one drive that outranks the hold outright. This
    # world reliably contains a bleeding ally by the time the transfer
    # scenarios run, `treat_ally` scans 60 tiles for one, and it scores
    # 8.0 against the escort's 7.5 — so whether this escort arrives at
    # all came down to which acolyte the medic squad ranked best that
    # run. Observed failing here on a run where the identical check had
    # passed twice; see `retire_medic_drive`, which #1251's own scenario
    # needs for exactly the same reason.
    check("the escort is out of the medic squad, so the arrival below "
          "measures the hold and not the medic ranking",
          retire_medic_drive(port, esc_uid))
    # Three MORE rations on top of the spawn loadout's own, so the row
    # is unambiguously merged and "Store all" has more than one instance
    # to name.
    send(port, f"for i = 1, 3 do unit.addItem({esc_uid}, 'rations') end;"
               " return 'ok'", timeout=20.0)

    # Pan the camera well away from the pair FIRST. The probe's earlier
    # scenarios already centred it on this very cargo, so a snap onto the
    # pair would otherwise be a no-op and unobservable — this is what
    # makes the D-4 check a real one rather than a tautology.
    send(port, f"camera.goToTile({bax + 60}, {bay + 60}); return 'ok'")
    time.sleep(0.5)
    cam_before = send(port, "local x, y = camera.getPosition();"
                            " return string.format('%.3f,%.3f', x or 0, y or 0)")
    made = send(port, "return tostring(require('scripts.transfer_session')"
                      f".create({esc_uid}, 'building', {bid}) ~= nil)")
    if not check("a Mode A session is created on the cargo endpoint",
                 made.strip().strip('"') == "true", f"got {made!r}"):
        return

    opened = poll_until(45.0, lambda: send(
        port, "local s = require('scripts.transfer_session').get();"
              " return s and s.phase or 'none'").strip('"') == "open",
        interval=0.5)
    if not check("the REAL unit AI holds the escort and opens the pair on "
                 "arrival", bool(opened),
                 # WHICH action owns the escort, and whether the session
                 # is even still there, are the two things that tell a
                 # timeout apart from a hold that lost to a higher band
                 # (this check has been seen to lose to `treat_ally`) or
                 # from panels that refused to open. Reported rather than
                 # left to a second run to guess at.
                 "escort running "
                 f"{ai_action(port, esc_uid)!r}, session phase "
                 + send(port,
                        "local s = require('scripts.transfer_session').get();"
                        " return s and s.phase or 'none'")):
        send(port, "require('scripts.transfer_session').clear(); return 'ok'")
        return
    time.sleep(0.6)

    d = stack_dump(port)
    lv = (d.get("levels") or [{}])[0]
    check("the pair is ONE stack level of the escort kind (D-9's stated "
          "exception), non-modal at the base",
          d.get("depth") == 1 and lv.get("kind") == "escort"
          and lv.get("paneCount") == 2 and lv.get("modal") is False,
          f"got {d!r}")
    panes = lv.get("panes") or []
    if check("both panes report rendered geometry", len(panes) == 2
             and all(isinstance(p.get("width"), (int, float)) for p in panes),
             f"got {panes!r}"):
        a, b = panes[0], panes[1]
        fb_w, fb_h = vp["fb_w"], vp["fb_h"]

        def in_frame(p):
            return (p["x"] >= 0 and p["y"] >= 0
                    and p["x"] + p["width"] <= fb_w
                    and p["y"] + p["height"] <= fb_h)

        check("both panels are clamped inside the framebuffer",
              in_frame(a) and in_frame(b),
              f"framebuffer {fb_w}x{fb_h}, panes {a!r} {b!r}")
        overlap = not (a["x"] + a["width"] <= b["x"]
                       or b["x"] + b["width"] <= a["x"]
                       or a["y"] + a["height"] <= b["y"]
                       or b["y"] + b["height"] <= a["y"])
        check("the two panels flank rather than overlap", not overlap,
              f"panes {a!r} {b!r}")
        check("the source unit is on the left and the destination on the "
              "right, reading from -> to",
              a.get("paneKey") == "source"
              and b.get("paneKey") == "destination" and a["x"] < b["x"],
              f"panes {a!r} {b!r}")

    cam_after = send(port, "local x, y = camera.getPosition();"
                           " return string.format('%.3f,%.3f', x or 0, y or 0)")
    check("opening the session snapped the camera onto the pair (D-4 — "
          "Mode A is the ONE gesture that moves it)",
          cam_after != cam_before,
          f"before={cam_before!r} after={cam_after!r}")

    # -- the real rendered row menus, one per pane, in both directions.
    src_rows = item_rows(port, pane_list_id("source"))
    dst_rows = item_rows(port, pane_list_id("destination"))
    check("both panes render their endpoint's rows",
          len(src_rows) > 0 and len(dst_rows) > 0,
          f"source={len(src_rows)} destination={len(dst_rows)}")

    def row_named(rows, def_name):
        for w in rows:
            if (w.get("defName") or "") == def_name:
                return w
        return None

    ration = row_named(src_rows, "rations")
    if check("the merged rations row is located on the source pane",
             bool(ration), f"rows {[w.get('defName') for w in src_rows]!r}"):
        right_click_widget_center(port, ration)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("a source-pane row offers Store 1 and Store all",
              "Store 1" in labels and "Store all" in labels,
              f"menu labels: {labels!r}")
        check("a source-pane row offers no Retrieve — direction comes from "
              "WHICH pane was clicked",
              not any((l or "").startswith("Retrieve") for l in labels),
              f"menu labels: {labels!r}")

        before_panes = {p.get("paneKey"): (p.get("subtitle"), p.get("rowCount"))
                        for p in panes}
        entry = find_widget(port, "Store all")
        if check("the Store all entry is clickable", bool(entry)):
            click_widget_center(port, entry)
            time.sleep(0.8)
            d2 = stack_dump(port)
            lv2 = (d2.get("levels") or [{}])[0]
            after_panes = {p.get("paneKey"): (p.get("subtitle"), p.get("rowCount"))
                           for p in (lv2.get("panes") or [])}
            check("committing refreshes BOTH panes in the same gesture — "
                  "each header's own stored weight moved",
                  after_panes.get("source") != before_panes.get("source")
                  and after_panes.get("destination")
                      != before_panes.get("destination"),
                  f"before={before_panes!r} after={after_panes!r}")
            check("the session stays open and repeatable after a commit",
                  d2.get("depth") == 1 and lv2.get("kind") == "escort"
                  and send(port,
                           "local s = require('scripts.transfer_session').get();"
                           " return s and s.phase or 'none'").strip('"')
                      == "open",
                  f"got {d2!r}")
            check("the rations really left the acolyte's own pane",
                  not any((w.get("defName") or "") == "rations"
                          for w in item_rows(port, pane_list_id("source"))),
                  "the source pane still lists a rations row")
        else:
            close_menu(port)

    # -- a real resize, rendered (#1250 review round 1).
    #
    #    Two 440-wide panels need 904 px at 1x, so on a framebuffer
    #    narrower than that the PAIR has to be fitted to width — each
    #    panel clamped on its own would land them on top of each other.
    #    What is asserted here is the invariant that holds at EVERY
    #    width: the pair's combined width fits the framebuffer it is
    #    drawn on, and the two still flank rather than stack.
    #
    #    Deliberately NOT asserted as "narrower than 440": `--offscreen`
    #    renders to a fixed-size target, so `engine.setWindowSize` moves
    #    the window without necessarily moving the FRAMEBUFFER this
    #    geometry is measured against, and demanding a shrink would then
    #    be demanding one that was never required. The envelope's formal
    #    minimum (800x600 @ 1x, where the fit genuinely fires and each
    #    pane really does come out under 440) is pinned deterministically
    #    by hspec instead — Test.Headless.UI.TransferSession drives the
    #    real framebuffer ref down to it.
    send(port, "return engine.setWindowSize(800, 600)")
    time.sleep(1.5)
    tight = stack_dump(port)
    tlv = (tight.get("levels") or [{}])[0]
    tpanes = tlv.get("panes") or []
    if check("the session survives a real resize",
             tight.get("depth") == 1 and tlv.get("kind") == "escort"
             and len(tpanes) == 2, f"got {tight!r}"):
        tvp = viewport(port, fallback=(800, 600))
        a, b = tpanes[0], tpanes[1]

        def fits(p):
            return (p["x"] >= 0 and p["y"] >= 0
                    and p["x"] + p["width"] <= tvp["fb_w"]
                    and p["y"] + p["height"] <= tvp["fb_h"])

        overlap = not (a["x"] + a["width"] <= b["x"]
                       or b["x"] + b["width"] <= a["x"]
                       or a["y"] + a["height"] <= b["y"]
                       or b["y"] + b["height"] <= a["y"])
        check("after the resize both panels still fit the framebuffer and "
              "still flank, source left",
              fits(a) and fits(b) and not overlap and a["x"] < b["x"]
              and a.get("paneKey") == "source",
              f"framebuffer {tvp['fb_w']}x{tvp['fb_h']}, panes {a!r} {b!r}")
        check("the PAIR is sized to the framebuffer it is drawn on, not "
              "each panel independently",
              a["width"] + b["width"] <= tvp["fb_w"],
              f"framebuffer width {tvp['fb_w']}, "
              f"panes {a['width']} + {b['width']}")

    dst_rows = item_rows(port, pane_list_id("destination"))
    target = dst_rows[0] if dst_rows else None
    if check("a destination-pane row is located for the reverse direction",
             bool(target)):
        right_click_widget_center(port, target)
        time.sleep(0.4)
        labels = [w.get("label") for w in widgets(port) if w.get("label")]
        check("a destination-pane row offers Retrieve, never Store",
              any((l or "").startswith("Retrieve") for l in labels)
              and not any((l or "").startswith("Store") for l in labels),
              f"menu labels: {labels!r}")
        close_menu(port)

    # -- coupled close: one dismissal takes both panels AND the session.
    send(port, "require('scripts.cargo_inventory_panel').popLevel(); return 'ok'")
    time.sleep(0.5)
    closed = stack_dump(port)
    check("closing the window closes BOTH panels and ends the session "
          "(requirement 7)",
          closed.get("depth") == 0
          and send(port, "return tostring(require('scripts.transfer_session')"
                         ".get())").strip('"') == "nil",
          f"got {closed!r}")

    # -- #1254 (UIT-5B): an ABNORMAL close leaves a usable window stack.
    #
    # The one thing this needs a GPU for. Every failure TRIGGER, and the
    # release of every hold, is pinned deterministically by hspec
    # (Test.Headless.UI.TransferSession, `session failure handling`);
    # what no fixture that renders nothing can state is that the real
    # panels' ELEMENTS went with the session, and that the next window to
    # open on the same page draws and hit-tests normally rather than over
    # two orphaned panels.
    #
    # A FRESH escort, spawned on a surface-checked tile just OUTSIDE the
    # cargo's footprint — adjacent, so the contract's Chebyshev-1
    # rect-to-rect rule already reports it in reach and the session opens
    # on the first AI tick with no walk at all — and destroyed again at
    # the end.
    #
    # Both halves of that are deliberate. Fresh, because this probe's
    # world reliably contains bleeding acolytes (see
    # `retire_medic_drive`) and one that has been standing in it for a
    # few minutes can be at a third of its blood; since #1254 an
    # UNCONSCIOUS held unit correctly ends its own session, so a second
    # session on a worn-down unit would measure the world's attrition
    # instead of the teardown. And OUTSIDE the footprint, because
    # spawning on the anchor puts a unit inside the building, where the
    # drop to the ground can wound it — the same hazard, arriving faster.
    #
    # `unit.destroy` rather than `unit.kill`: a death is a world EVENT (a
    # corpse, witnesses, the mood and combat reactions they provoke) and
    # the very next scenario measures AI priority on a pair nearby, so a
    # kill here would make that measurement depend on this one. WHICH
    # detector fires is hspec's business — `session failure handling`
    # covers death, unconsciousness, faction loss and demolition
    # separately; what needs a GPU is only that the panels really left the
    # screen and that the stack still works.
    ab_uid, ab_raw = None, ""
    for dx, dy in ((-1, 0), (2, 0), (0, -1), (0, 2), (-1, -1), (2, 2)):
        tx, ty = bax + dx, bay + dy
        surface = tile_surface(port, tx, ty)
        if surface is None or not surface[1]:
            continue
        ab_raw = send(port, f"return unit.spawn('acolyte', {tx}, {ty},"
                            " nil, 'player')")
        if ab_raw.strip() not in ("", "nil", "null"):
            ab_uid = int(float(ab_raw))
            break
    if not check("a fresh acolyte is spawned beside the cargo for the "
                 "abnormal-close phase",
                 ab_uid is not None, f"got {ab_raw!r}"):
        return
    check("its standing find_water goal is retired", clear_find_water(port, ab_uid))
    check("it is out of the medic squad, so nothing outranks the hold",
          retire_medic_drive(port, ab_uid))
    reopened = send(port, "return tostring(require('scripts.transfer_session')"
                          f".create({ab_uid}, 'building', {bid}) ~= nil)")
    if check("a second Mode A session opens on the same endpoint",
             reopened.strip().strip('"') == "true", f"got {reopened!r}"):
        again = poll_until(45.0, lambda: send(
            port, "local s = require('scripts.transfer_session').get();"
                  " return s and s.phase or 'none'").strip('"') == "open",
            interval=0.5)
        if check("it reaches the open state with both panes drawn",
                 again and (stack_dump(port).get("levels")
                            or [{}])[0].get("paneCount") == 2,
                 "session "
                 + send(port,
                        "local s = require('scripts.transfer_session').get();"
                        " return (s and s.phase or 'gone') .. ' pose='"
                        f" .. tostring(unit.getPose({ab_uid}))")):
            send(port, f"return unit.destroy({ab_uid})")
            time.sleep(1.5)
            abnormal = stack_dump(port)
            alive = send(port, f"return tostring(unit.exists({ab_uid}))")
            check("losing the held escort ends the session and takes both "
                  "panels with it (#1254 requirements 1 and 5)",
                  alive.strip().strip('"') == "false"
                  and abnormal.get("depth") == 0
                  and send(port, "return tostring(require('scripts"
                                 ".transfer_session').get())").strip('"')
                      == "nil",
                  f"exists {alive!r}, stack {abnormal!r}")
            # Nothing orphaned: the panels' own widgets are gone from the
            # live UI, not merely forgotten by the manager.
            panes_left = [w for w in widgets(port)
                          if (w.get("name") or "").startswith("cargo_inv")]
            check("no escort panel widget is left on screen",
                  not panes_left, f"still present: {panes_left!r}")
            # ...and the stack accepts the next open, rendering rows.
            if check("the next container window opens normally after the "
                     "abnormal close", open_window_on(port, bid)):
                rows = item_rows(port, level_list_id(1))
                nxt = stack_dump(port)
                check("that window is an ordinary endpoint level with real "
                      "rows",
                      nxt.get("depth") == 1
                      and (nxt.get("levels") or [{}])[0].get("kind")
                          == "endpoint"
                      and len(rows) > 0,
                      f"stack {nxt!r}, {len(rows)} row(s)")
            send(port, "require('scripts.cargo_inventory_panel')"
                       ".closeIfOpen(); return 'ok'")
    send(port, f"return unit.destroy({ab_uid})")


# --------------------------------------------------------------------------
# #1251: the unit-to-unit escort's two-sided hold
# --------------------------------------------------------------------------

def unit_pos(port: int, uid: int):
    """`(gridX, gridY)` for `uid`, or None if it stopped resolving."""
    info = send_json(port, f"return unit.getInfo({uid})")
    if not isinstance(info, dict):
        return None
    x, y = info.get("gridX"), info.get("gridY")
    if not isinstance(x, (int, float)) or not isinstance(y, (int, float)):
        return None
    return (float(x), float(y))


def tile_gap(a, b) -> float:
    """Euclidean tile distance, or -1 when either end is unknown — which
    never satisfies a `> n` motion test, so an unresolvable unit reads as
    "did not move" rather than as movement."""
    if a is None or b is None:
        return -1.0
    return math.hypot(b[0] - a[0], b[1] - a[1])


def ai_action(port: int, uid: int) -> str:
    """`aiState[uid].currentAction` — which action last WON for this
    unit, which is how a preemption is observed rather than inferred."""
    return send(port, "local s = require('scripts.unit_ai').getState("
                      f"{uid}); return s and s.currentAction or 'nil'"
                ).strip('"')


def retire_medic_drive(port: int, uid: int) -> bool:
    """Take `uid` out of the medic squad for the duration of this
    measurement, and answer whether it took.

    `treat_ally` scores 8.0 — deliberately ABOVE the 7.5 escort hold,
    because #1250 records treatment as one of the bands that still
    preempts it. Its patient scan reaches 60 tiles, so ONE ally bleeding
    anywhere in this probe's world makes an acolyte's own AI walk away
    from the hold, correctly, and the scenario below would then be
    measuring that documented exception instead of the hold. (This is
    not hypothetical: the first run of this gate failed with the target
    sitting in `treat_ally`.)

    So the competing drive is retired first, exactly as the escort
    scenario retires `find_water` for the same reason. `medicCapability`
    gates on the `bleed_control` knowledge, and an already-locked claim
    outranks even that, so both go — in ONE console statement, which
    runs on the Lua thread and therefore cannot be interleaved with a
    tick that re-claims. What is left competing with the hold is the
    routine-work band it is supposed to beat."""
    send(port, f"unit.setKnowledge({uid}, 'bleed_control', 0);"
               f" local s = require('scripts.unit_ai').getState({uid});"
               " if s then s.treatClaim = nil; s.treatPending = nil end;"
               " return 'ok'")
    try:
        return float(send(port,
                          f"return unit.getKnowledge({uid}, 'bleed_control')"
                          ).strip().strip('"')) == 0.0
    except (TypeError, ValueError):
        return False


def chebyshev(a, b) -> float:
    """The contract's OWN distance measure between two 1x1 endpoints —
    `withinReach` is Chebyshev <= 1 — or -1 when either end is unknown.
    Used here instead of a Euclidean gap because "the escort still has
    somewhere to walk" is exactly "the pair is NOT yet in reach", and
    only this measure answers that question."""
    if a is None or b is None:
        return -1.0
    return max(abs(b[0] - a[0]), abs(b[1] - a[1]))


class EscortSeparation:
    """Everything the escort staging loop below observed, retained per
    ATTEMPT rather than only for the last one (#1911).

    A single pair of variables used to hold the destination and the
    paused snapshot, overwritten on every retry — so a run that
    exhausted all four attempts reported one bearing and one gap and
    lost the three tries before it. That is exactly the evidence needed
    to tell "this world never had room" from "one bearing kept pointing
    back at the escort", and it was unavailable without rerunning a
    fifteen-minute GPU probe. The retained coordinated run of
    2026-08-26 is the case in point: it recorded a maximum-axis gap of
    exactly `1.0` and nothing about the three attempts that preceded it.

    Recorded per attempt: where the target was actually sent, both
    units' positions at the paused instant whenever a snapshot was taken
    at all, and the Chebyshev gap those imply — the contract's OWN
    measure, so `separated` answers the same question `withinReach`
    does."""

    def __init__(self) -> None:
        self.attempts: list[dict] = []

    def record(self, sent_to, src, dst) -> None:
        """One attempt. `sent_to` is None when the target's AI never took
        a move order up, in which case no snapshot was taken either."""
        self.attempts.append({"sent_to": sent_to, "src": src, "dst": dst,
                              "gap": chebyshev(src, dst)})

    @property
    def last(self) -> dict:
        return self.attempts[-1] if self.attempts else {}

    @property
    def ordered(self) -> bool:
        """Did the target end up under a real player move order?"""
        return self.last.get("sent_to") is not None

    @property
    def separated(self) -> bool:
        """Is the pair OUTSIDE the transfer contract's own reach rule —
        Chebyshev > 1 — at the paused instant a session would be created?"""
        return self.ordered and self.last.get("gap", -1.0) > 1.0

    @property
    def sent_to(self):
        return self.last.get("sent_to")

    @property
    def at_create(self):
        return self.last.get("dst")

    def detail(self) -> str:
        """Every attempt, in order, so a setup failure is attributable
        from the run's own output instead of from a rerun."""
        if not self.attempts:
            return "no attempt was made"
        return "; ".join(
            f"#{n} sent to {a['sent_to']!r}, escort at {a['src']!r}, "
            f"target at {a['dst']!r}, Chebyshev {a['gap']:.3f}"
            for n, a in enumerate(self.attempts, 1))


def stage_escort_separation(port: int, src_uid: int, dst_uid: int,
                            attempts: int = 4,
                            settle: float = 1.5) -> EscortSeparation:
    """Order the target away until the pair is outside the transfer
    contract's reach with the simulation stopped, and answer everything
    observed on the way.

    Both acolytes keep ticking, so a separation observed a moment ago
    can be gone a couple of console round trips later — which is how two
    earlier versions of this check failed (once at 0.84 tiles apart,
    once at 0.58, each having been clear moments before).
    `engine.setPaused` is the only thing that really holds a unit still
    (`unit.setFrozen` is a render pin, CLAUDE.md) and positions must be
    re-read AFTER pausing; the order points away, so a gap that has not
    opened yet opens by waiting rather than by trying something else.
    Nothing new is tried when it does not open: a third staging
    heuristic is exactly what the two documented failures above warn
    against, so an exhausted loop is reported rather than worked around.

    Hands back with the simulation stopped at the instant of its last
    snapshot, whether or not that snapshot separated: the caller either
    creates the session right there, or gives the pause to
    `end_escort_setup` along with everything else it restores. Only the
    world BETWEEN attempts is resumed, and only because the next attempt
    needs somewhere to walk."""
    staging = EscortSeparation()
    for _ in range(attempts):
        if staging.attempts:
            set_paused(port, False)
        sent_to = order_target_away(port, dst_uid, src_uid)
        if sent_to is None:
            staging.record(None, None, None)
            break
        time.sleep(settle)
        set_paused(port, True)
        at_create = unit_pos(port, dst_uid)
        src_at_create = unit_pos(port, src_uid)
        staging.record(sent_to, src_at_create, at_create)
        if staging.separated:
            break
    return staging


def end_escort_setup(port: int) -> None:
    """Leave the engine as a completed run leaves it: running, with no
    session behind it.

    Every early return from the escort scenario goes through this one
    helper. The setup-failure path needs it as much as the later ones
    do — the staging loop pauses in order to measure, so a scenario
    returning from there without it would hand whatever runs next a
    stopped simulation."""
    set_paused(port, False)
    send(port, "require('scripts.transfer_session').clear(); return 'ok'")


def spawn_pair_apart(port: int, ax: int, ay: int):
    """Put the escort a few tiles from its target, and answer
    `(src_uid, dst_uid)`.

    Reachability is established by WALKING, never assumed. The escort is
    spawned on the anchor and sent to a candidate tile; only a tile it
    actually reaches is used, and the TARGET is then spawned on the
    anchor the escort has just left — so the escort's own approach is
    the path it has already walked, in reverse.

    Both weaker versions of this failed on real terrain. Walking the
    TARGET clear put the measurement behind a second pathfinding
    problem: once it could not get two tiles clear in any of twelve
    directions, once it could not leave its spawn tile at all while the
    escort beside it wandered four tiles. Spawning it on a
    surface-checked tile instead removed that, but a tile with a surface
    is not necessarily a tile the escort can REACH, and the pair then
    never opened inside a minute across three tiles. Walking the leg
    that the scenario depends on is the only version that proves it.

    Chunks around the anchor are loaded first: the camera has been
    elsewhere for this whole probe, and pathing across terrain that was
    never generated is its own way to produce a unit that does not
    move."""
    ai = "require('scripts.unit_ai')"
    ccx, ccy = ax // CHUNK_TILES, ay // CHUNK_TILES
    send(port, f"return world.loadChunksInRegion({ccx - 1}, {ccy - 1},"
               f" {ccx + 1}, {ccy + 1})")
    send(port, "return world.waitForChunks(60)", timeout=65.0)
    src_raw = send(port, f"return unit.spawn('acolyte', {ax}, {ay},"
                         " nil, 'player')")
    if src_raw.strip() in ("", "nil", "null"):
        return None, None
    src_uid = int(float(src_raw))
    # Its own standing goals must not fight the scouting walk below, for
    # the same reason they must not fight the measurement afterwards.
    clear_find_water(port, src_uid)
    retire_medic_drive(port, src_uid)
    for dx, dy in ((2, 0), (0, 2), (-2, 0), (0, -2), (2, 2), (-2, -2),
                   (3, 0), (0, 3), (-3, 0), (0, -3)):
        tx, ty = ax + dx, ay + dy
        surface = tile_surface(port, tx, ty)
        if surface is None or not surface[1]:
            continue
        send(port, f"unit.stop({src_uid});"
                   f" {ai}.commandMove({src_uid}, {tx}, {ty}); return 'ok'")
        if poll_until(20.0,
                      lambda: tile_gap(unit_pos(port, src_uid),
                                       (float(tx), float(ty))) < 0.9,
                      interval=0.5):
            dst_raw = send(port, f"return unit.spawn('acolyte', {ax}, {ay},"
                                 " nil, 'player')")
            if dst_raw.strip() in ("", "nil", "null"):
                return src_uid, None
            # Leave the escort idle where it stands, so nothing it was
            # told to do earlier is still in flight when the session is
            # created.
            send(port, f"unit.stop({src_uid}); return 'ok'")
            return src_uid, int(float(dst_raw))
    return src_uid, None


def order_target_away(port: int, dst_uid: int, src_uid: int):
    """Put the target under a real player move order pointing AWAY from
    the escort; answer where it was sent, or None if the AI never took
    one up.

    A commanded move rather than the ambient wander: `follow_command`
    sits at 7.0, above every routine-work lock, so the escort hold
    preempting it is strictly harder than preempting the wander tick,
    and its destination is chosen here rather than rolled, so "it never
    got there" is a fact the caller can check.

    AWAY is load-bearing. An earlier version aimed at fixed offsets from
    the anchor, and since the escort had walked out to a tile of its
    own, the first of those pointed straight at it: the target dutifully
    closed the gap to 0.58 tiles and the pair was in reach before the
    session was ever created. Perpendicular fallbacks are offered next
    because they at least do not close it.

    Taking the order UP is what this waits for — `follow_command`
    becoming the unit's current action, which is the AI selecting it and
    issuing the walk. How far that walk then gets is generated terrain's
    business, and it is not what the hold is measured against: the
    caller checks separately that the target does not move once held,
    and that it never reached where it was sent."""
    ai = "require('scripts.unit_ai')"
    here = unit_pos(port, dst_uid)
    src = unit_pos(port, src_uid)
    if here is None or src is None:
        return None
    vx, vy = here[0] - src[0], here[1] - src[1]
    norm = math.hypot(vx, vy)
    if norm < 0.01:
        vx, vy, norm = 1.0, 0.0, 1.0
    ux, uy = vx / norm, vy / norm
    bearings = ((ux, uy), (-uy, ux), (uy, -ux))
    for reach in (6, 4, 3):
        for bx, by in bearings:
            tx = int(round(here[0] + bx * reach))
            ty = int(round(here[1] + by * reach))
            surface = tile_surface(port, tx, ty)
            if surface is None or not surface[1]:
                continue
            send(port, f"unit.stop({dst_uid});"
                       f" {ai}.commandMove({dst_uid}, {tx}, {ty});"
                       " return 'ok'")
            if poll_until(8.0,
                          lambda: ai_action(port, dst_uid) == "follow_command",
                          interval=0.4):
                return (float(tx), float(ty))
    return None


def accepts_movement(port: int, uid: int, ax: int, ay: int,
                     window: float = 8.0) -> bool:
    """Does `uid` take an ordinary move order again? Answered by
    watching the real AI either SELECT the order or act on it.

    Selecting counts, and that is the point rather than a concession.
    The hold is a utility, so "released" means neither side of it wins
    any more and the ordinary ladder decides again — and `follow_command`
    becoming this unit's current action IS the ladder taking the
    player's order. Whether the unit then covers ground is generated
    terrain's business: a unit that cannot path anywhere from a ledge
    does not move under ANY order, which looks identical to one still
    pinned and is not. Displacement is still accepted first, because
    when it happens it says the same thing more loudly.

    `unit.stop` FIRST. A unit released from the hold still carries the
    move order the hold preempted, and `follow_command` resumes it — so
    the action does not SWITCH when a new order arrives, and unit_ai
    re-executes a running action only on a switch or when the unit is
    idle. The new destination would otherwise sit unapplied behind a
    walk to the old one — the same reason `transfer_session.close` stops
    rather than merely releasing.

    Several destinations, for the reason `order_target_away` offers
    several: one reachable tile is the whole answer."""
    ai = "require('scripts.unit_ai')"
    for dx, dy in ((0, 0), (2, 0), (0, 2), (-2, 0), (0, -2),
                   (4, 0), (0, 4), (-4, 0), (0, -4)):
        tx, ty = ax + dx, ay + dy
        surface = tile_surface(port, tx, ty)
        if surface is None or not surface[1]:
            continue
        before = unit_pos(port, uid)
        if before is None:
            return False
        send(port, f"unit.stop({uid}); {ai}.commandMove({uid}, {tx}, {ty});"
                   " return 'ok'")
        if poll_until(window,
                      lambda: tile_gap(before, unit_pos(port, uid)) > 0.5
                              or ai_action(port, uid) == "follow_command",
                      interval=0.4):
            return True
    return False


def unit_escort_session_scenario(port: int, aax: int, aay: int) -> None:
    """#1251 (UIT-4): a session whose DESTINATION is a unit holds both
    ends, and both are released together.

    The behavioural half needs a real engine, which is why it lives in a
    probe rather than beside the deterministic hspec cases: the target
    has to be in observable motion BEFORE the session exists, since an
    initially idle unit standing still afterwards proves nothing. So it
    is put under a real move order first, and what is checked is that
    the hold takes that order over on its next tick, that the target's
    position then does not change for the WHOLE of the source's
    approach, and that both units can be steered again once the window
    closes.

    The rendered half is the pair itself: two live panes over two unit
    endpoints, a real row menu committing in each direction, and one
    dismissal closing both.

    Deliberately after the building escort, whose stack this takes over
    and leaves empty again.

    This function is the SETUP half only, and each of its three `setup:`
    preconditions is terminal: a fixture it cannot establish ends the
    scenario here (#1911). `escort_hold_measurement` — everything that
    grades the hold, the approach and the rendered pair — is reached
    only once all three hold, so no check downstream of them can be
    graded against a pair the probe has already reported invalid."""
    print("== #1251 unit-to-unit escort (the two-sided hold) ==")
    send(port, "require('scripts.cargo_inventory_panel').closeIfOpen();"
               " return 'ok'")

    src_uid, dst_uid = spawn_pair_apart(port, aax, aay)
    if not check("setup: the escort stands a few tiles from its target, over "
                 "ground it has WALKED — so its approach below is a leg this "
                 "world is known to admit",
                 src_uid is not None and dst_uid is not None,
                 f"escort={src_uid!r} target={dst_uid!r}"):
        end_escort_setup(port)
        return
    for uid in (src_uid, dst_uid):
        check(f"unit {uid}'s standing find_water goal is retired",
              clear_find_water(port, uid))
        # After clear_find_water, which is what waits for the AI state to
        # exist at all.
        check(f"unit {uid} is out of the medic squad for this measurement "
              "— treat_ally (8.0) is a band that legitimately preempts "
              "the 7.5 hold, so it must not be what decides",
              retire_medic_drive(port, uid))
    # One stack per side, so each pane has a row and each direction has
    # something of its own to move.
    send(port, f"for i = 1, 3 do unit.addItem({src_uid}, 'rations') end;"
               f" for i = 1, 2 do unit.addItem({dst_uid}, 'bandage') end;"
               " return 'ok'", timeout=20.0)

    # Order the target away, let it open the gap, and COMMIT to the
    # measurement with the simulation stopped.
    staging = stage_escort_separation(port, src_uid, dst_uid)
    if not check("setup: the target is under a real player move order "
                 "before the session exists — its own AI has SELECTED "
                 "follow_command (7.0, above every routine-work lock), so "
                 "an idle unit standing still afterwards is not what gets "
                 "measured",
                 staging.ordered,
                 f"running {ai_action(port, dst_uid)!r} at "
                 f"{unit_pos(port, dst_uid)!r}; {staging.detail()}"):
        end_escort_setup(port)
        return

    # WHICH action is pending matters as much as that one is: the claim
    # below is that the hold preempts a player move order, so the thing
    # preempted has to be named, not assumed from the fact that something
    # moved. (The first run of this gate found the target walking under
    # `treat_ally` instead — see `retire_medic_drive`.) Read off the AI
    # state, so pausing does not disturb it.
    walking_under = ai_action(port, dst_uid)
    check("the pending action really is the move order, not some other "
          "action that happens to walk",
          walking_under == "follow_command", f"got {walking_under!r}")

    # TERMINAL, because every check past it is a claim about an approach
    # (#1911). A pair already within Chebyshev 1 makes "the pair opens"
    # pass with no approach at all and "the target did not move for the
    # whole of the approach" measure a walk that never happened — so a
    # staging loop that ran out of attempts fails the scenario at SETUP
    # rather than grading five checks against a fixture the probe itself
    # has just reported invalid.
    if not check("setup: the escort has a real approach to make — the pair "
                 "is outside the contract's own reach rule at the moment "
                 "the session is created, so 'held still through the "
                 "approach' is a claim about an approach that happened",
                 staging.separated, staging.detail()):
        end_escort_setup(port)
        return
    escort_hold_measurement(port, src_uid, dst_uid, staging, aax, aay)


def escort_hold_measurement(port: int, src_uid: int, dst_uid: int,
                            staging: EscortSeparation,
                            aax: int, aay: int) -> None:
    """The half that only means anything once the pair really is staged
    apart, in its own function so that it CANNOT run against a fixture
    the gate above rejected (#1911 requirement 4).

    Enters with the simulation stopped at the instant `staging`
    measured, and hands back with it running."""
    sent_to, at_create = staging.sent_to, staging.at_create
    # Created while STOPPED, so nothing can drift between the gate above
    # and the session it is a precondition for. The target's move order
    # is still pending and un-preempted at this instant; the very next
    # tick after the sim resumes is what has to take it over.
    made = send(port, "return tostring(require('scripts.transfer_session')"
                      f".create({src_uid}, 'unit', {dst_uid}) ~= nil)")
    set_paused(port, False)
    if not check("a unit-to-unit Mode A session is created",
                 made.strip().strip('"') == "true", f"got {made!r}"):
        end_escort_setup(port)
        return
    # `escort_hold`, not `escort_transfer`: the TARGET side is its own
    # registered action so that every commandable species has it, while
    # the source side stays the per-species capability the source gate
    # asks about (#1251 review round 1).
    took = poll_until(20.0,
                      lambda: ai_action(port, dst_uid) == "escort_hold",
                      interval=0.3)
    if not check("session creation PREEMPTS the target's move order — the "
                 "hold wins on its very next tick, mid-walk", bool(took),
                 # A hold that never wins and a session that quietly
                 # ENDED look identical from currentAction alone: with no
                 # session, `roleOf` answers nil, the hold scores -inf
                 # and the target goes straight back to its move order.
                 # The escort retires a session whose destination stops
                 # resolving, so the phase and both roles are what tell
                 # those apart.
                 f"currentAction={ai_action(port, dst_uid)!r}, session "
                 + send(port,
                        "local s = require('scripts.transfer_session');"
                        " local a = s.get();"
                        " return (a and a.phase or 'gone') .. ' src='"
                        f" .. tostring(s.roleOf({src_uid})) .. ' dst='"
                        f" .. tostring(s.roleOf({dst_uid}))")):
        end_escort_setup(port)
        return
    held = unit_pos(port, dst_uid)
    # Between the create call and the tick that acts on it the target is
    # still walking, so a tile of drift is the hold arriving on the next
    # tick rather than the hold failing. What must NOT have happened is
    # it carrying on to where it was sent.
    check("the target stopped where it stood, short of where it was sent",
          tile_gap(at_create, held) < 2.0 and tile_gap(held, sent_to) > 1.0,
          f"created at {at_create!r}, held at {held!r}, sent to {sent_to!r}")

    opened = poll_until(60.0, lambda: send(
        port, "local s = require('scripts.transfer_session').get();"
              " return s and s.phase or 'none'").strip('"') == "open",
        interval=0.5)
    if not check("the source walks over to the held target and the pair "
                 "opens", bool(opened)):
        end_escort_setup(port)
        return
    settled = unit_pos(port, dst_uid)
    check("the target's position did not move for the WHOLE of the "
          "source's approach — the walk had a fixed destination",
          tile_gap(held, settled) < 0.01,
          f"held at {held!r}, at arrival {settled!r}")
    roles = send_json(port, "local s = require('scripts.transfer_session');"
                            f" return {{ src = s.roleOf({src_uid}),"
                            f" dst = s.roleOf({dst_uid}) }}")
    check("both units are held, on their two sides of the one session",
          isinstance(roles, dict) and roles.get("src") == "source"
          and roles.get("dst") == "target", f"got {roles!r}")
    time.sleep(0.6)

    d = stack_dump(port)
    lv = (d.get("levels") or [{}])[0]
    check("the unit-to-unit pair is ONE escort level with two panes",
          d.get("depth") == 1 and lv.get("kind") == "escort"
          and lv.get("paneCount") == 2, f"got {d!r}")
    src_rows = item_rows(port, pane_list_id("source"))
    dst_rows = item_rows(port, pane_list_id("destination"))
    check("both panes render their own UNIT endpoint's rows",
          any((w.get("defName") or "") == "rations" for w in src_rows)
          and any((w.get("defName") or "") == "bandage" for w in dst_rows),
          f"source={[w.get('defName') for w in src_rows]!r} "
          f"destination={[w.get('defName') for w in dst_rows]!r}")

    def row_named(rows, def_name):
        for w in rows:
            if (w.get("defName") or "") == def_name:
                return w
        return None

    def carries(uid: int, def_name: str) -> bool:
        inv = send_json(port, f"return unit.getInventory({uid})")
        return isinstance(inv, list) and any(
            isinstance(it, dict) and it.get("defName") == def_name
            for it in inv)

    # -- both directions, through the real rendered row menus. Which pane
    #    was clicked is what picks the direction, so this is the same
    #    builder answering twice rather than two gestures.
    ration = row_named(src_rows, "rations")
    if check("the escort's own rations row is located on the source pane",
             bool(ration)):
        right_click_widget_center(port, ration)
        time.sleep(0.4)
        entry = find_widget(port, "Store all")
        if check("a source-pane row offers Store all against a UNIT "
                 "destination", bool(entry)):
            click_widget_center(port, entry)
            time.sleep(0.8)
            check("Store committed into the held TARGET unit",
                  carries(dst_uid, "rations") and not carries(src_uid, "rations"),
                  "the rations did not move between the two units")
        else:
            close_menu(port)

    bandage = row_named(item_rows(port, pane_list_id("destination")), "bandage")
    if check("the target's own bandage row is located on the destination "
             "pane", bool(bandage)):
        right_click_widget_center(port, bandage)
        time.sleep(0.4)
        entry = find_widget(port, "Retrieve all")
        if check("a destination-pane row offers Retrieve all off a UNIT "
                 "endpoint", bool(entry)):
            click_widget_center(port, entry)
            time.sleep(0.8)
            check("Retrieve committed back into the escort",
                  carries(src_uid, "bandage") and not carries(dst_uid, "bandage"),
                  "the bandages did not move back")
        else:
            close_menu(port)

    # -- coupled close: one dismissal takes both panels, the session AND
    #    both holds (requirement 2).
    send(port, "require('scripts.cargo_inventory_panel').popLevel();"
               " return 'ok'")
    time.sleep(0.5)
    closed = stack_dump(port)
    released = send_json(port, "local s = require('scripts.transfer_session');"
                               " return { session = tostring(s.get()),"
                               f" src = s.holdsUnit({src_uid}),"
                               f" dst = s.holdsUnit({dst_uid}) }}")
    check("one dismissal closes both panels, ends the session and releases "
          "BOTH units",
          closed.get("depth") == 0 and isinstance(released, dict)
          and released.get("session") == "nil"
          and released.get("src") is False and released.get("dst") is False,
          f"stack={closed!r} session={released!r}")
    # Release means the real AI is deciding again — a cleared Lua table
    # would not prove that, since the hold is a utility rather than a
    # flag. Two observations per unit: the hold has stopped winning, and
    # an ordinary move order is taken up.
    for role, uid in (("escort", src_uid), ("target", dst_uid)):
        loose = poll_until(
            10.0,
            lambda: ai_action(port, uid) not in ("escort_transfer",
                                                 "escort_hold"),
            interval=0.4)
        check(f"the released {role} is no longer being decided by the hold "
              "— the ordinary action ladder has taken it back",
              bool(loose), f"still running {ai_action(port, uid)!r}")
        check(f"the released {role} accepts ordinary AI movement again",
              accepts_movement(port, uid, aax, aay),
              f"at {unit_pos(port, uid)!r}, running "
              f"{ai_action(port, uid)!r}, under none of the move orders "
              "offered")
