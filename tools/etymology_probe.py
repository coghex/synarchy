#!/usr/bin/env python3
"""Name-etymology probe (#1104, epic #708) — the GPU-backed, windowless gate.

The pure hspec groups cover the decomposition itself ("Language
etymology") and the panel's lifecycle against a bare Lua backend
("Etymology panel"). This probe covers what only a REAL boot can: the
in-game panel driven through the actual UI, against a real generated
world, with the real engine query behind it.

It boots with ``--offscreen`` — full Vulkan, NO window — so the genuine
UI flow runs (loading screen, menus, HUD) and ``input.*`` injection drives
it. It never creates a visible window.

Every control is located through the widget/dump ORACLES
(``previewManager``-style ``dump()`` tables, ``ui.dumpWidgets``), never a
hardcoded screen coordinate, and every entity is located through
``world.listPlacedLocations`` / ``world.getRivers`` rather than a guessed
tile.

Phases:

  1. Boot, let the boot asset queue drain — ``data/locations`` is the
     LAST data family it registers, and a world generated before that
     places no locations at all — then generate a world named through
     the real generated-language path and reach the in-game HUD.
  2. The WORLD entry point opens the panel, and its content is genuinely
     populated: the stored name, the whole gloss, at least one morpheme
     row carrying a concept/role/realized spelling/canonical free
     spelling/English lemma, and surface tokens that concatenate back to
     the stored name exactly.
  3. A DISCOVERED location opens the SAME panel — same module, same
     viewport handle lineage — retargeted rather than duplicated.
  4. Selecting a visible named RIVER segment opens it again, resolved
     through #1102's stable identity by ``world.getRiverAt``.
  5. Bound/free explanation and recurrence rows are visibly populated
     when the language produces them, and the free/bound relationship is
     reported as ONE morpheme rather than two.
  6. A long decomposition scrolls through the REAL input routing — the
     scrollbar's own arrow buttons and a wheel event over the panel's
     capturing box — which the bare-Lua hspec group cannot drive, since a
     scrollbar there owns no sprite handles.
  7. The honest UNAVAILABLE state renders for a name with no recoverable
     derivation, still showing the stored name.
  8. A resize keeps the panel valid, reachable, and pointed at the same
     entity; close and teardown remove it cleanly with no stale handles.

Phases 3 and 4 each need the generated world to actually SUPPLY an
entity — a placed location, and a named river with segments to select.
The default fixture (seed 42, 64 chunks, three plates) supplies both.
When one is absent the probe FAILS (#1604) rather than printing ``SKIP``
and exiting 0, because a required phase that never ran is not a phase
that passed. Such a failure is reported as a ``FIXTURE`` line naming
those generation parameters, kept distinct from an ordinary ``FAIL`` so
a reader can tell "the world came up short" from "the UI behaved
wrongly".

Phase 6 needs a precondition of a different kind, and fails the same way
(#1608). Its overflow is not a property of the generated language: the
phase MANUFACTURES it, by forcing a reduced framebuffer and a high UI
scale before inspecting the world's own name and up to eight rivers. So
"nothing overflowed" says the manufactured configuration stopped
working — row population, panel sizing, scrollbar creation, the
responsive envelope, UI-scale handling — and the six arrow and wheel
routing checks below it never ran, which is a setup failure rather than
a language that happened to come out short. Only phase 5's bound-form
and recurrence cases are genuinely data-dependent, and only they still
skip.

Needs a GPU (Vulkan device) — manual-only, never CI-gated. ``--self-test``
is the exception: it drives the fixture classification with synthetic
readings and boots nothing at all.

Usage:
  python3 tools/etymology_probe.py
  python3 tools/etymology_probe.py --port 9422 --seed 42 --size 64
  python3 tools/etymology_probe.py --self-test     # no engine at all

Exit code 0 = all checks passed AND the fixture supplied every entity
the required phases need.
"""
from __future__ import annotations

import argparse
import io
import json
import os
import sys
import time
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, quit_engine, send, send_json

LOG = "/tmp/etymology_engine.log"

#: The world page the production chain creates — scripts/world_view.lua
#: hardcodes it, and scripts/hud.lua points at the same id, which is what
#: the name plate reads through.
PAGE = "main_world"

#: The tectonic plate count the fixture generates with. Named alongside
#: the seed and world size by every FIXTURE failure, so the world that
#: came up short can be reproduced exactly.
PLATE_COUNT = 3


def q(v) -> str:
    """A Lua string literal, or `nil` for an absent value."""
    if v is None:
        return "nil"
    return "'" + str(v).replace("\\", "\\\\").replace("'", "\\'") + "'"


#: The language seed the world is named from. Deliberately above 2^63-1,
#: so the decimal-string carrier world.init/world.getEtymology use is
#: exercised over a range a Lua number could not hold losslessly.
LANG_SEED = "12345678901234567890"

failures = 0
fixture_failures = 0


def check(ok: bool, label: str, detail: str = "") -> bool:
    global failures
    if ok:
        print(f"  PASS  {label}")
    else:
        failures += 1
        print(f"  FAIL  {label}" + (f"\n        {detail}" if detail else ""))
    return ok


def fixture_failure(label: str, detail: str) -> None:
    """A required phase could not run: the world never supplied the
    entity it needs, or the precondition for reading that entity never
    held.

    Counted and printed SEPARATELY from ``check``'s FAIL because the two
    say different things (#1604 requirement 4): a FAIL says the UI
    behaved wrongly, a FIXTURE says the UI was never asked. Both exit
    non-zero — the whole point is that a phase which never ran can no
    longer report as one that passed.
    """
    global fixture_failures
    fixture_failures += 1
    print(f"  FIXTURE  {label}\n           {detail}")


# --------------------------------------------------------------------
# Fixture classification (#1604) — pure decisions over readings, so
# `--self-test` can drive both missing-entity branches with no engine.
# --------------------------------------------------------------------


def fixture_params(args) -> str:
    """The generation parameters every FIXTURE failure names."""
    return (f"seed {args.seed}, world size {args.size}, "
            f"plateCount {PLATE_COUNT}")


def residency_cause(active, remaining, params: str) -> str | None:
    """Why phase 3's precondition does not hold, or ``None``.

    Chunk loading is NOT what populates ``world.listPlacedLocations``:
    ``src/World/Thread/Command/Init.hs`` builds the instances and
    publishes them into the page's generation parameters at
    ``world.init`` time, and
    ``src/Engine/Scripting/Lua/API/WorldQuery/Location.hs`` reads that
    precomputed overlay straight back out. What a completed load supplies
    is a SYNCHRONIZATION point against the world thread's command queue
    for the ACTIVE page, which is why both readings are graded here:

    * ``world.waitForInit`` follows only the active world, and
      ``world.getRivers`` answers only for it, so a run whose active page
      is not ``main_world`` would be reading a different world entirely;
    * ``world.waitForChunks`` returns the number of chunks still
      OUTSTANDING and a timeout returns a nonzero remainder
      (``.../WorldQuery/Chunk.hs``), so only a zero remainder proves the
      queue drained rather than the wait expiring.

    Either way this is a fixture/setup failure, not a verdict on whether
    the world placed locations — it is what makes an empty list afterwards
    mean "this world placed none" instead of "not synchronized yet".
    """
    if not isinstance(active, str) or active != PAGE:
        return (f"'{PAGE}' is not the active page "
                f"(world.getActiveWorldId() = {active!r}), so every query "
                f"below would answer for a different world; the fixture "
                f"({params}) never came up")
    if isinstance(remaining, bool) or not isinstance(remaining, int):
        return (f"world.waitForChunks reported no remaining chunk count "
                f"(got {remaining!r}), so the load around the camera cannot "
                f"be known to have drained; the fixture ({params}) never "
                f"synchronized")
    if remaining != 0:
        return (f"world.waitForChunks timed out with {remaining} chunk(s) "
                f"still outstanding, so the load around the camera never "
                f"drained; the fixture ({params}) never synchronized")
    return None


def is_empty_table(value) -> bool:
    """Lua cannot tell an empty array from an empty map, so the console's
    serializer renders BOTH as ``{}`` — which decodes to an empty dict,
    not an empty list. Every "the engine returned nothing" reading here
    has to accept that shape, or an absent entity would be misreported as
    a malformed query result."""
    return value == [] or value == {}


def location_fixture_cause(locs, params: str) -> str | None:
    """Why phase 3 has no location to drive, or ``None``. Read only once
    ``residency_cause`` has passed, so an empty list is an answer about
    the WORLD rather than about timing.
    """
    if is_empty_table(locs):
        return (f"this world placed no locations, so the fixture ({params}) "
                f"supplied no discovered-location entry point for phase 3 "
                f"to open")
    if not isinstance(locs, list):
        return (f"world.listPlacedLocations('{PAGE}') returned {locs!r} "
                f"rather than a list, so the fixture ({params}) supplied no "
                f"discovered-location entry point for phase 3 to open")
    return None


def named_rivers(rivers) -> list:
    """The rivers phase 4 can actually drive: a stable #1102 identity, a
    stored name, and at least one segment whose tile can be selected."""
    if not isinstance(rivers, list):
        return []
    return [r for r in rivers
            if isinstance(r, dict) and r.get("id") is not None
            and r.get("name") and r.get("segments")]


def river_fixture_cause(rivers, params: str) -> str | None:
    """Why phase 4 has no named river to drive, or ``None``."""
    if is_empty_table(rivers):
        return (f"this world generated no rivers at all, so the fixture "
                f"({params}) supplied no named river entry point for phase 4 "
                f"to select")
    if not isinstance(rivers, list):
        return (f"world.getRivers() returned {rivers!r} rather than a list, "
                f"so the fixture ({params}) supplied no named river entry "
                f"point for phase 4 to select")
    if not named_rivers(rivers):
        return (f"none of the {len(rivers)} river(s) this world generated "
                f"carries the id, name and segments phase 4 selects through, "
                f"so the fixture ({params}) supplied no named river entry "
                f"point")
    return None


#: Phase 6's forced configuration: the framebuffer it rebuilds the HUD
#: at, and the UI scale it applies. Named once so the diagnostic that
#: reports the configuration failing cannot drift from the values
#: actually applied.
SCROLL_FORCE_SIZE = (800, 600)
SCROLL_FORCE_SCALE = 4.0

#: What a setup diagnostic prints for a field the panel's dump did not
#: carry. A malformed or absent dump carries none of the three phase 6
#: judges, and the diagnostic still has to name each one rather than
#: silently dropping it.
UNAVAILABLE = "<unavailable>"


def panel_overflows(d) -> bool:
    """Whether a panel dump shows more rows than its window can hold —
    the condition phase 6's forced configuration exists to manufacture,
    and the one the real scroll controls only appear under.

    Deliberately the SAME reading phase 6 has always used, absent-field
    ``or 0`` included, so #1608 changes only what happens when nothing
    overflows and never what the six arrow and wheel checks are entered
    on (requirement 3).
    """
    return (isinstance(d, dict)
            and (d.get("rowCount") or 0) > (d.get("visibleRows") or 0))


def scroll_reading(d):
    """The compact projection of a panel dump phase 6's diagnostic
    quotes: the judged fields plus their open/available context.

    Only keys the dump ACTUALLY carried appear, so the projection agrees
    with the ``UNAVAILABLE`` labels beside it instead of contradicting
    them: a field the dump omitted is absent here too, while one it
    carried as an explicit null still reads as ``None``. A dump that is
    not a table at all is quoted RAW, because then the shape itself is
    the evidence.
    """
    if not isinstance(d, dict):
        return d
    return {k: d[k] for k in ("open", "rowCount", "visibleRows",
                              "scrollbar", "available") if k in d}


def scroll_fixture_cause(last, inspected: int) -> str:
    """Why phase 6 has no overflowing panel to scroll (#1608).

    Always reached with a reading that did NOT overflow, so this states
    the cause rather than deciding whether there is one. Each of the
    three judged fields is named explicitly even when the reading
    carried none of them — an absent, errored or non-table dump prints
    ``UNAVAILABLE`` for all three and keeps the raw response beside
    them — so the diagnostic is legible whatever shape came back.
    """
    fields = " ".join(
        f"{key}={last[key]!r}" if isinstance(last, dict) and key in last
        else f"{key}={UNAVAILABLE}"
        for key in ("rowCount", "visibleRows", "scrollbar"))
    w, h = SCROLL_FORCE_SIZE
    return (f"{fields}; none of the {inspected} inspected name(s) overflowed "
            f"the panel's window under phase 6's forced {w}x{h} / UI scale "
            f"{SCROLL_FORCE_SCALE} configuration, so the scrollbar handles "
            f"the six arrow and wheel checks drive never appeared (last "
            f"dump: {scroll_reading(last)!r})")


def exit_code(failed_checks: int, failed_fixtures: int) -> int:
    """The run's exit status. A fixture failure is as non-zero as a
    behavioural one (#1604 requirements 2 and 3) — a required phase that
    never ran must not report as a pass."""
    return 1 if (failed_checks or failed_fixtures) else 0


def panel_dump(port: int):
    """The panel's own read-only introspection table."""
    return send_json(
        port,
        "local ep = package.loaded['scripts.etymology_panel']; "
        "if not ep then return {missing = true} end; return ep.dump()",
        timeout=15,
    )


def plate_dump(port: int):
    return send_json(
        port,
        "local np = package.loaded['scripts.name_plate']; "
        "if not np then return {missing = true} end; return np.dump()",
        timeout=15,
    )


def open_via_plate(port: int, kind: str) -> bool:
    """Click the name plate's row for ``kind`` at its REAL interactive
    bounds, located through the plate's own dump rather than a guessed
    coordinate."""
    d = plate_dump(port)
    if not isinstance(d, dict):
        return False
    for row in d.get("rows") or []:
        if row.get("kind") == kind and row.get("x") is not None:
            cx = int(row["x"]) + int(row.get("width") or 0) // 2
            cy = int(row["y"]) + int(row.get("height") or 0) // 2
            send(port, f"return input.click({cx}, {cy})", timeout=15)
            # Injected input is queued and routed on the engine's own
            # frames (press and release are separate events, and #745's
            # activation only fires on the release), so the panel does
            # not exist yet when this returns.
            time.sleep(0.5)
            return True
    return False


def tokens_reproduce_name(d) -> bool:
    """#1104 requirement 3, checked by the probe ITSELF rather than
    trusted from the engine's own claim: concatenating the reported
    surface tokens must reproduce the stored name exactly."""
    toks = d.get("tokens") or []
    if not toks:
        return False
    return "".join(t.get("text") or "" for t in toks) == (d.get("name") or "")


def morpheme_fields_populated(d) -> tuple[bool, str]:
    ms = d.get("morphemes") or []
    if not ms:
        return False, "no morpheme rows"
    for m in ms:
        for key in ("concept", "role", "surface", "free", "lemma", "id"):
            if not m.get(key):
                return False, f"morpheme {m!r} missing {key}"
    return True, ""


def wait_for_startup_loader(port: int, timeout: float = 300.0) -> bool:
    """Block until the boot loading screen's asset queue has drained.

    Phase 3's precondition starts HERE, before the world exists, and it
    is the half no later wait can repair. ``scripts/startup_loader.lua``
    queues ``data/locations`` LAST, after every other data family, and
    ``src/World/Thread/Command/Init.hs`` builds a page's placed-location
    instances from the location REGISTRY at ``world.init`` time — so a
    world generated while that registry is still empty places no
    locations at all, permanently, however long anything waits
    afterwards. The real game cannot reach Create World before this
    drains (``scripts/ui_manager_boot.lua`` only pushes the main menu
    once the loader is done); a probe that stages the gameplay view as
    soon as the debug console answers can, and did.
    """
    deadline = time.time() + timeout
    while time.time() < deadline:
        done = send_json(
            port,
            "local sl = package.loaded['scripts.startup_loader']; "
            "return (sl ~= nil and sl.isDone() == true)",
            timeout=15)
        if done is True:
            return True
        time.sleep(0.5)
    return False


def phase1_boot(args) -> tuple[object, str]:
    print("\n[1] boot offscreen, generate a language-named world, reach the HUD")
    proc = boot(args.port, log=LOG, args=["--size", args.window],
                ready_timeout=240, mode=("--offscreen",))
    if not wait_for_startup_loader(args.port):
        fixture_failure(
            "the boot loading screen never drained, so the world below "
            "would be generated against half-empty data registries",
            f"scripts/startup_loader.lua never reported isDone(); the "
            f"fixture ({fixture_params(args)}) never came up, and a world "
            f"generated before data/locations registers places no locations "
            f"at all")
        return proc, ""
    check(True, "the boot asset queue drained before the world is generated")
    # The name/gloss/expression triple comes from world.suggestName
    # itself, so the stored name really was rendered from the expression
    # stored beside it — a hand-written pair would satisfy the surface
    # check only by accident.
    sug = send_json(
        args.port, f"return world.suggestName({args.seed}, 0)", timeout=30)
    if not isinstance(sug, dict) or not sug.get("expr"):
        check(False, "world.suggestName returned a usable suggestion",
              f"got {sug!r}")
        return proc, ""
    name, gloss, expr = sug["name"], sug["gloss"], sug["expr"]
    lang = sug.get("language") or {}
    seed, version = lang.get("seed"), lang.get("version")
    # Drive the PRODUCTION path, not world.init directly: entering the
    # gameplay view re-creates the page from scripts/world_view.lua's own
    # worldParams through worldManager.createWorld, so anything the Create
    # World screen fails to forward is missing here too. Staging those
    # params and then taking the real "world_view" transition is what
    # makes this probe cover the whole chain rather than the engine call
    # at the end of it — and is exactly how it caught the expression not
    # being forwarded at all.
    send(args.port,
        "local wv = require('scripts.world_view'); "
        "wv.worldParams = { seed = %d, worldSize = %d, plateCount = %d, "
        "worldName = %s, worldGloss = %s, languageSeed = %s, "
        "languageVersion = %s, nameExpr = %s }; return 'ok'"
        % (args.seed, args.size, PLATE_COUNT, q(name), q(gloss), q(seed),
           version, q(expr)),
        timeout=20)
    send(args.port,
         "local ui = require('scripts.ui_manager'); ui.showMenu('world_view')",
         timeout=60)
    send(args.port, "return world.waitForInit(600)", timeout=620)
    ready = send_json(
        args.port,
        "local hud = package.loaded['scripts.hud']; "
        "local np = package.loaded['scripts.name_plate']; "
        "return {hud = (hud and hud.uiCreated) == true, "
        "        plate = (np and np.hud ~= nil) == true, "
        "        page = world.getActiveWorldId(), "
        "        named = (world.getIdentity(world.getActiveWorldId()) or {}).name}",
        timeout=30)
    check(isinstance(ready, dict) and ready.get("hud") is True
          and ready.get("plate") is True,
          "the real gameplay view built the HUD and hosted the name plate",
          f"got {ready!r}")
    check(isinstance(ready, dict) and ready.get("named") == name,
          "the production Create World chain carried the generated name "
          "through to the live page",
          f"got {ready!r}")
    check(True, f"generated world '{name}' ({gloss}) from {expr}")
    return proc, name


def phase2_world(port: int, stored_name: str) -> None:
    print("\n[2] the WORLD entry point opens a populated panel")
    send(port, "local np = package.loaded['scripts.name_plate']; "
               "if np then np.refresh() end", timeout=15)
    if not check(open_via_plate(port, "world"),
                 "the name plate offers a world row, clicked at its real bounds"):
        # Fall back to the panel's own entry point so later phases still
        # have something to assert against; the plate failure is recorded.
        send(port, "local ep = package.loaded['scripts.etymology_panel']; "
                   "if ep then ep.openFor('world') end", timeout=15)
    d = panel_dump(port)
    if not isinstance(d, dict):
        check(False, "panel dump readable", f"got {d!r}")
        return
    check(d.get("open") is True, "the panel is open")
    check(d.get("kind") == "world", "it is inspecting the world")
    check(d.get("name") == stored_name,
          "it shows the AUTHORITATIVE stored name unchanged",
          f"panel {d.get('name')!r} vs stored {stored_name!r}")
    check(bool(d.get("gloss")), "the whole-name gloss is shown")
    ok, why = morpheme_fields_populated(d)
    check(ok, "every morpheme row carries concept/role/surface/free/lemma/id", why)
    check(tokens_reproduce_name(d),
          "the reported surface tokens concatenate back to the stored name",
          f"tokens={d.get('tokens')!r} name={d.get('name')!r}")
    check((d.get("rowCount") or 0) > 0, "visible rows were actually rendered")


def location_precondition(port: int, params: str) -> str | None:
    """Synchronize against the world thread, then grade phase 3's
    precondition. ``None`` means it holds.

    The region is the one around the camera, which the gameplay view
    leaves at the world origin — the same
    ``loadChunksInRegion`` -> ``waitForChunks`` order
    ``tools/location_content_probe.py`` establishes before it queries a
    page. The active page is only VERIFIED, never forced: phase 1's
    production Create World chain is what activates it, so switching
    pages here would paper over exactly the setup failure this is
    grading. See ``residency_cause`` for what each reading proves.
    """
    send(port, "return world.loadChunksInRegion(-2, -2, 2, 2)", timeout=60)
    remaining = send_json(port, "return world.waitForChunks(120)", timeout=130)
    active = send(port, "return world.getActiveWorldId()", timeout=20)
    return residency_cause(active, remaining, params)


def phase3_location(args) -> None:
    port = args.port
    print("\n[3] a DISCOVERED location opens the SAME panel")
    params = fixture_params(args)
    cause = location_precondition(port, params)
    if cause:
        fixture_failure(
            "phase 3's precondition never held, so no location entry point "
            "was exercised", cause)
        return
    locs = send_json(port, f"return world.listPlacedLocations('{PAGE}')", timeout=30)
    cause = location_fixture_cause(locs, params)
    if cause:
        fixture_failure("phase 3 had no placed location to exercise", cause)
        return
    # Prefer the ruin nearest the origin, for the same reason phase 4
    # prefers the nearest river segment: the camera starts there, so that
    # column is the likeliest to be selectable through the REAL route
    # (requirement 6's primary path) rather than through the fallback.
    target = min(locs, key=lambda loc: abs(loc.get("gx") or 0)
                 + abs(loc.get("gy") or 0))
    iid = target.get("instance_id")
    # Discovery is a lifecycle promotion the game drives; drive it the way
    # the game does rather than asserting our own write, then select the
    # ruin's own anchor tile so the plate resolves it.
    send(port, f"world.setLocationLifecycle({iid}, 'discovered', '{PAGE}')",
         timeout=15)
    if select_tile(port, target["gx"], target["gy"]):
        send(port, "local np = package.loaded['scripts.name_plate']; "
                   "if np then np.refresh() end", timeout=15)
        check(open_via_plate(port, "location"),
              "the plate offers a row for the DISCOVERED location, clicked "
              "at its real bounds")
    else:
        print(f"  SKIP  the ruin's tile ({target['gx']}, {target['gy']}) "
              f"could not be made resident to select")
        send(port, "local ep = package.loaded['scripts.etymology_panel']; "
                   f"if ep then ep.openFor('location', {iid}) end", timeout=15)
    d = panel_dump(port)
    if not isinstance(d, dict):
        check(False, "panel dump readable after the location route")
        return
    check(d.get("kind") == "location" and d.get("targetId") == iid,
          "the same panel retargeted onto the location",
          f"kind={d.get('kind')!r} target={d.get('targetId')!r}")
    if d.get("available"):
        check(tokens_reproduce_name(d),
              "the location's tokens reproduce ITS stored name")


def select_tile(port: int, gx: int, gy: int) -> bool:
    """Make (gx, gy) the player's selected tile, the way the Info tool
    does. Requires the column to be resident, so its chunk region is
    loaded first; returns whether the selection actually took."""
    cx, cy = gx // 32, gy // 32
    # Bring the camera along, which is what a player does and what keeps
    # the region resident: eviction is camera-relative
    # ('World.Tile.Types.evictDistantChunksWithReport' keeps a radius
    # around the camera chunk and drops the furthest of the rest once the
    # page is over its chunk cap), so loading a region eight chunks away
    # and leaving the camera at the origin can see it generated and then
    # evicted again before the selection lands. Each caller moves to its
    # OWN target, so no phase strands another phase's tile.
    send(port, f"camera.goToTile({gx}, {gy})", timeout=15)
    send(port, f"return world.loadChunksInRegion({cx - 2}, {cy - 2}, "
               f"{cx + 2}, {cy + 2})", timeout=60)
    send(port, "return world.waitForChunks(120)", timeout=130)
    send(port, f"world.selectTile('{PAGE}', {gx}, {gy})", timeout=15)
    time.sleep(0.5)
    sel = send_json(port, f"return world.getSelectedTile('{PAGE}')", timeout=15)
    return (isinstance(sel, dict) and sel.get("gx") == gx
            and sel.get("gy") == gy)


def phase4_river(args) -> None:
    port = args.port
    print("\n[4] selecting a visible named RIVER opens it again")
    params = fixture_params(args)
    # Rivers need no residency wait of their own: world.getRivers reads
    # the ACTIVE page's settled fluid identification, which world.init
    # finishes before world.waitForInit returns — unlike the chunk-queue
    # synchronization phase 3 establishes, nothing here is queued behind
    # a chunk load.
    rivers = send_json(port, "return world.getRivers()", timeout=45)
    cause = river_fixture_cause(rivers, params)
    if cause:
        fixture_failure("phase 4 had no named, segmented river to exercise",
                        cause)
        return
    named = named_rivers(rivers)
    # Prefer a river whose first segment sits nearest the origin: the
    # camera starts there, so its chunks are the likeliest to become
    # resident, and a tile can only be SELECTED once its column is.
    def dist(r):
        seg = r["segments"][0]
        return abs(seg.get("sx", 0)) + abs(seg.get("sy", 0))
    river = min(named, key=dist)
    seg = river["segments"][0]
    gx, gy = seg["sx"], seg["sy"]

    # The engine's own tile -> stable identity resolution, which is the
    # selection path #1104 adds. Asserted unconditionally: it is a pure
    # geometry query and needs no resident chunk.
    at = send_json(port, f"return world.getRiverAt({gx}, {gy})", timeout=20)
    check(isinstance(at, dict) and at.get("id") == river["id"],
          "world.getRiverAt resolves the selected segment to its own river id",
          f"getRiverAt={at!r} expected id={river.get('id')}")
    check(isinstance(at, dict) and at.get("name") == river["name"],
          "and reports that river's own stored name",
          f"getRiverAt={at!r}")

    if select_tile(port, gx, gy):
        send(port, "local np = package.loaded['scripts.name_plate']; "
                   "if np then np.refresh() end", timeout=15)
        check(open_via_plate(port, "river"),
              "the plate offers a row for the selected river, clicked at "
              "its real bounds")
    else:
        # A river far from the camera can have no resident column to
        # select; the plate's own row-building is gated by the hspec
        # "Etymology panel" group, so skip rather than fail on residency.
        print(f"  SKIP  the river's tile ({gx}, {gy}) could not be made "
              f"resident to select")
        send(port, "local ep = package.loaded['scripts.etymology_panel']; "
                   f"if ep then ep.openFor('river', {river['id']}) end",
             timeout=15)

    d = panel_dump(port)
    if not isinstance(d, dict):
        check(False, "panel dump readable after the river route")
        return
    check(d.get("kind") == "river" and d.get("targetId") == river["id"],
          "the same panel retargeted onto the river")
    check(d.get("name") == river["name"],
          "it shows the river's own stored name",
          f"panel {d.get('name')!r} vs query {river['name']!r}")
    if d.get("available"):
        check(tokens_reproduce_name(d),
              "the river's tokens reproduce ITS stored name")


def phase5_bound_and_recurrence(port: int) -> None:
    print("\n[5] bound/free explanation and recurrence rows")
    # Look across every eligible entity for a bound form and a recurrence
    # link, rather than assuming this seed's language produces either at a
    # particular one: both are real language properties, not guarantees.
    found_bound = False
    found_recurrence = False
    targets = [("world", "nil")]
    locs = send_json(port, f"return world.listPlacedLocations('{PAGE}')", timeout=30)
    for loc in (locs or [])[:4]:
        targets.append(("location", str(loc.get("instance_id"))))
    rivers = send_json(port, "return world.getRivers()", timeout=45)
    for r in (rivers or [])[:6]:
        if r.get("id") is not None:
            targets.append(("river", str(r["id"])))

    for kind, ident in targets:
        send(port, "local ep = package.loaded['scripts.etymology_panel']; "
                   f"if ep then ep.openFor('{kind}', {ident}) end", timeout=15)
        d = panel_dump(port)
        if not isinstance(d, dict) or not d.get("available"):
            continue
        for m in d.get("morphemes") or []:
            if m.get("bound"):
                found_bound = True
                # ONE morpheme, two spellings: the realized bound form and
                # the canonical free root are both reported, and differ.
                check(bool(m.get("free")) and
                      m["free"].lower() != (m.get("surface") or "").lower(),
                      "a bound form reports its canonical free spelling too",
                      f"morpheme={m!r}")
        for link in d.get("recurrence") or []:
            for entry in link.get("entries") or []:
                found_recurrence = True
                check(entry.get("kind") in ("world", "location", "river")
                      and bool(entry.get("name")),
                      "a recurrence entry exposes only an entity kind and a name",
                      f"entry={entry!r}")
                check(set(entry.keys()) <= {"kind", "name"},
                      "a recurrence entry leaks nothing else",
                      f"entry keys={sorted(entry.keys())}")
    if not found_bound:
        print("  SKIP  this language formed no bound form on any inspected name")
    if not found_recurrence:
        print("  SKIP  no morpheme recurred across this world's eligible names")


def phase7_unavailable(args) -> None:
    print("\n[7] the honest UNAVAILABLE state")
    port = args.port
    # A CUSTOM-named page: a player-entered name has no language and no
    # expression, which is requirement 7's first case. Deliberately TINY
    # regardless of --size — this needs an identity, not terrain, and a
    # second full-size generation would only cost minutes.
    send(port, f"world.init('custom', {args.seed}, 16, 3, 'Player Name')",
         timeout=20)
    send(port, "return world.waitForInit(600)", timeout=620)
    send(port, "world.show('custom')", timeout=20)
    # Assert the switch actually happened before reading anything through
    # it: openFor('world') resolves the ACTIVE page, so a page that never
    # became active would silently re-answer for the previous world and
    # every check below would be measuring the wrong thing.
    active = send(port, "return world.getActiveWorldId()", timeout=20)
    if "custom" not in (active or ""):
        check(False, "the custom-named page became the active world",
              f"active={active!r}")
        return
    send(port, "local ep = package.loaded['scripts.etymology_panel']; "
               "if ep then ep.openFor('world') end", timeout=15)
    d = panel_dump(port)
    if not isinstance(d, dict):
        check(False, "panel dump readable for the custom-named world")
        return
    check(d.get("available") is False,
          "a custom name reports its etymology as unavailable",
          f"available={d.get('available')!r}")
    check(d.get("reason") == "custom",
          "and names the reason honestly as 'custom'",
          f"reason={d.get('reason')!r}")
    check(bool(d.get("reasonText")),
          "with a non-empty player-facing explanation")
    check(d.get("name") == "Player Name",
          "while STILL showing the stored name",
          f"name={d.get('name')!r}")
    check(not (d.get("morphemes") or []),
          "and inventing no morpheme rows")
    check((d.get("rowCount") or 0) > 0,
          "the unavailable state still renders something visible")
    send(port, f"world.show('{PAGE}')", timeout=20)
    send(port, "local ep = package.loaded['scripts.etymology_panel']; "
               "if ep then ep.closeIfOpen() end", timeout=15)


def phase6_scrolling(port: int) -> None:
    """A long decomposition must actually SCROLL through the real input
    routing. The hspec group can only assert the wiring — a scrollbar's
    arrows are UI sprites, and the bare Lua backend has no textures, so
    it owns no clickable handles there. Here it does."""
    print("\n[6] the panel scrolls through the real input routing")
    # Restoring the scale is UNCONDITIONAL cleanup, not a tail step: the
    # forced-scale body below has several exits (the fixture failure, the
    # missing-handles return, an exception out of any send), and every
    # one of them must leave phase 7's resize checks running against the
    # ordinary envelope rather than this phase's deliberate extreme. The
    # 4.0 apply is inside the guard too, so a failure partway through it
    # is covered as well; restoring a scale that never changed is a
    # no-op.
    try:
        _phase6_forced_scale(port)
    finally:
        send(port, "engine.setUIScale(1.0)", timeout=15)


def _phase6_forced_scale(port: int) -> None:
    """Phase 6's body, everything it does at UI scale 4.0. Its caller
    owns restoring the scale on every exit from it."""
    # A generated name has only a couple of morphemes, so its
    # decomposition fits comfortably at a normal size. Shrink the HUD to
    # 800x600 — the responsive envelope's formal MINIMUM FRAMEBUFFER
    # (scripts/ui/responsive.lua) — so the panel bounds its visible rows
    # by a much smaller window, and force UI scale 4.0 so each row costs
    # far more of it. The PAIR is deliberately out of envelope: at that
    # height only scales 0.5-1.0 are fully supported, and 800x600@4x is
    # the headless suite's own out-of-envelope exemplar
    # (test-headless/Test/Headless/UI/ResponsiveMenus.hs). This is a
    # manufactured extreme that makes short content overflow so the real
    # scroll controls appear — not a configuration a supported player
    # setup reaches, and not something a name at normal scale is
    # expected to do.
    w, h = SCROLL_FORCE_SIZE
    send(port, f"engine.setUIScale({SCROLL_FORCE_SCALE}); "
               f"local hud = require('scripts.hud'); "
               f"hud.init(hud.texWorldSelect or 1, hud.boxTexSet or 2, "
               f"{w}, {h}); "
               f"hud.createUI()", timeout=30)
    targets = [("world", "nil")]
    rivers = send_json(port, "return world.getRivers()", timeout=45)
    for r in (rivers or [])[:8]:
        if r.get("id") is not None:
            targets.append(("river", str(r["id"])))
    scrollable = None
    last = None
    inspected = 0
    for kind, ident in targets:
        send(port, "local ep = package.loaded['scripts.etymology_panel']; "
                   f"if ep then ep.openFor('{kind}', {ident}) end", timeout=15)
        d = panel_dump(port)
        last = d
        inspected += 1
        if panel_overflows(d):
            scrollable = d
            break
    if not scrollable:
        # NOT a skip (#1608): the overflow is this phase's own doing, so
        # its absence means the setup stopped working and the six checks
        # below never ran. Counted as a FIXTURE failure, which exits
        # non-zero exactly like a behavioural FAIL while still reading
        # as "the phase was never asked" rather than "the UI misbehaved".
        fixture_failure(
            "phase 6's forced configuration produced no overflowing panel, "
            "so its arrow and wheel routing checks never ran",
            scroll_fixture_cause(last, inspected))
        return

    handles = scrollable.get("scrollHandles") or []
    check(bool(handles),
          "the panel's scrollbar owns real, clickable element handles",
          f"scrollHandles={handles!r}")
    if not handles:
        return
    # Drive the ARROWS through the real router, at the scrollbar's own
    # handles rather than guessed coordinates.
    moved = send_json(
        port,
        "local ep = package.loaded['scripts.etymology_panel']; "
        "local ui = require('scripts.ui_manager'); "
        "local before = ep.state.scrollOffset; "
        "local downOk = false; "
        "for _, h in ipairs(ep.dump().scrollHandles or {}) do "
        "  if ui.onScrollDown(h) then downOk = true; break end end; "
        "local afterDown = ep.state.scrollOffset; "
        "local upOk = false; "
        "for _, h in ipairs(ep.dump().scrollHandles or {}) do "
        "  if ui.onScrollUp(h) then upOk = true; break end end; "
        "return {before = before, afterDown = afterDown, "
        "        afterUp = ep.state.scrollOffset, "
        "        downOk = downOk, upOk = upOk}",
        timeout=20)
    if isinstance(moved, dict):
        check(moved.get("downOk") is True and moved.get("upOk") is True,
              "uiManager's arrow routes reach the panel at real handles",
              f"got {moved!r}")
        check((moved.get("afterDown") or 0) > (moved.get("before") or 0),
              "the down arrow really advanced the view",
              f"got {moved!r}")
        check(moved.get("afterUp") == moved.get("before"),
              "and the up arrow brought it back",
              f"got {moved!r}")

    # And the WHEEL, over the panel's own capturing box.
    wheeled = send_json(
        port,
        "local ep = package.loaded['scripts.etymology_panel']; "
        "local ui = require('scripts.ui_manager'); "
        "local box = ep.dump().box; "
        "local before = ep.state.scrollOffset; "
        "ui.onUIScroll(box, 0, -1, false); "
        "local after = ep.state.scrollOffset; "
        "local info = box and UI.getElementInfo(box); "
        "return {before = before, after = after, "
        "        captures = (info and info.scrollCapturing) == true}",
        timeout=20)
    if isinstance(wheeled, dict):
        check(wheeled.get("captures") is True,
              "the panel box is a real scroll-CAPTURING surface, so the "
              "wheel routes to it instead of zooming the world")
        check((wheeled.get("after") or 0) > (wheeled.get("before") or 0),
              "and a wheel event over it advanced the view",
              f"got {wheeled!r}")


def phase8_lifecycle(port: int) -> None:
    print("\n[8] resize keeps it valid and reachable; close/teardown are clean")
    send(port, "local ep = package.loaded['scripts.etymology_panel']; "
               "if ep then ep.openFor('world') end", timeout=15)
    before = panel_dump(port)
    send(port, "local ui = require('scripts.ui_manager'); "
               "ui.onFramebufferResize(1024, 768)", timeout=20)
    send(port, "local hud = require('scripts.hud'); "
               "hud.init(hud.texWorldSelect or 1, hud.boxTexSet or 2, 1024, 768); "
               "hud.createUI()", timeout=30)
    after = panel_dump(port)
    if isinstance(before, dict) and isinstance(after, dict):
        check(after.get("open") is True, "the panel survives a resize")
        check(after.get("kind") == before.get("kind")
              and after.get("targetId") == before.get("targetId"),
              "pointed at the SAME entity as before the resize",
              f"before={before.get('kind')!r}/{before.get('targetId')!r} "
              f"after={after.get('kind')!r}/{after.get('targetId')!r}")
        check((after.get("rowCount") or 0) > 0,
              "with valid, non-degenerate content after the rebuild")
        rows = after.get("rows") or []
        check(all((r.get("width") or 0) >= 0 and (r.get("height") or 0) >= 0
                  for r in rows),
              "every rendered row has non-degenerate bounds")
    send(port, "local ep = package.loaded['scripts.etymology_panel']; "
               "if ep then ep.closeIfOpen() end", timeout=15)
    closed = panel_dump(port)
    if isinstance(closed, dict):
        check(closed.get("open") is False, "close removes it")
        check((closed.get("rowCount") or 0) == 0, "leaving no rows behind")
        check(closed.get("viewport") is None,
              "and no stale viewport handle",
              f"viewport={closed.get('viewport')!r}")


# --------------------------------------------------------------------
# Negative-path coverage (#1604) — no engine, no worldgen, no GPU
# --------------------------------------------------------------------


class _FixtureArgs:
    """The default fixture's identifying parameters, for the self-test's
    message assertions."""

    def __init__(self, seed: int = 42, size: int = 64) -> None:
        self.seed = seed
        self.size = size


def self_test() -> int:
    """Drive the fixture classification with synthetic readings.

    A live run can only show that the DEFAULT fixture supplies both
    entities; it cannot show what happens when a fixture does not. One
    ``--size 8`` is a live negative fixture for the RIVER branch (seed
    42 generates no rivers at all that small), but the LOCATION branch
    has no cheap live fixture at any size: #997 places a guaranteed
    location whenever the world has land, so only a landless world
    reaches it. Both branches are therefore covered here, alike and
    deterministically, against the same decision functions and the same
    exit accounting a live run uses.

    Phase 6's overflow precondition (#1608) is covered the same way, and
    for the same reason: forcing a live run not to overflow means
    breaking the configuration the phase depends on, so its readings are
    driven here instead — including the malformed and absent dumps whose
    diagnostic still has to name all three judged fields.
    """
    global failures, fixture_failures
    args = _FixtureArgs()
    params = fixture_params(args)
    problems: list[str] = []

    def expect_none(label: str, cause) -> None:
        if cause is not None:
            problems.append(f"{label}: expected no cause, got {cause!r}")

    def expect_cause(label: str, cause) -> None:
        if cause is None:
            problems.append(f"{label}: expected a cause, got none")
            return
        # Requirements 2 and 3: the fixture that came up short is named.
        for needle in ("seed 42", "world size 64", f"plateCount {PLATE_COUNT}"):
            if needle not in cause:
                problems.append(
                    f"{label}: cause does not name {needle!r}: {cause!r}")

    # --- the fixture parameter line itself ---------------------------
    expect_cause("fixture_params carries the whole triple",
                 f"placeholder ({params})")

    # --- phase 3's precondition --------------------------------------
    expect_none("synchronized on the right page",
                residency_cause(PAGE, 0, params))
    expect_cause("a different active page",
                 residency_cause("custom", 0, params))
    expect_cause("no active page at all", residency_cause(None, 0, params))
    expect_cause("a chunk wait that timed out",
                 residency_cause(PAGE, 3, params))
    expect_cause("a chunk wait that reported nothing",
                 residency_cause(PAGE, None, params))
    expect_cause("a chunk wait that answered with text",
                 residency_cause(PAGE, "120", params))
    # A bool is an int in Python; `True` is not a remaining count.
    expect_cause("a chunk wait that answered with a boolean",
                 residency_cause(PAGE, True, params))

    # --- phase 3's entity --------------------------------------------
    expect_none("one placed location is enough",
                location_fixture_cause([{"instance_id": 1, "gx": 0, "gy": 0}],
                                       params))
    expect_cause("a world that placed no locations",
                 location_fixture_cause([], params))
    # The console renders an empty Lua table as `{}`, which decodes to a
    # dict; a live run really does read that shape, so it must classify
    # as "placed none" and not as a malformed query result.
    expect_cause("an empty location table serialized as {}",
                 location_fixture_cause({}, params))
    expect_cause("a location query that returned nothing",
                 location_fixture_cause(None, params))
    expect_cause("a location query that errored into text",
                 location_fixture_cause("attempt to index a nil value", params))

    # --- phase 4's entity (no live fixture exists for this branch) ----
    river = {"id": 7, "name": "Ashen", "segments": [{"sx": 1, "sy": 2}]}
    expect_none("one named, segmented river is enough",
                river_fixture_cause([river], params))
    expect_cause("a world that generated no rivers",
                 river_fixture_cause([], params))
    expect_cause("an empty river table serialized as {}",
                 river_fixture_cause({}, params))
    expect_cause("rivers that carry no name",
                 river_fixture_cause(
                     [{"id": 7, "segments": [{"sx": 1, "sy": 2}]}], params))
    expect_cause("a named river with no segment to select",
                 river_fixture_cause([{"id": 7, "name": "Ashen"}], params))
    expect_cause("a named river with no stable identity",
                 river_fixture_cause(
                     [{"name": "Ashen", "segments": [{"sx": 1}]}], params))
    expect_cause("a river query that returned nothing",
                 river_fixture_cause(None, params))
    if named_rivers([river, {"id": 8}]) != [river]:
        problems.append("named_rivers kept a river phase 4 cannot drive")
    for shape in ([], {}):
        if not is_empty_table(shape):
            problems.append(f"{shape!r} was not read as an empty table")
    for shape in ([river], {"missing": True}, None, "boom"):
        if is_empty_table(shape):
            problems.append(f"{shape!r} was misread as an empty table")

    # --- phase 6's manufactured overflow (#1608) ----------------------
    overflowing = {"open": True, "rowCount": 12, "visibleRows": 5,
                   "scrollbar": {"handle": 3}, "available": True}
    if not panel_overflows(overflowing):
        problems.append("an overflowing panel was not read as overflowing")
    for shape in ({"rowCount": 5, "visibleRows": 5},
                  {"rowCount": 2, "visibleRows": 5},
                  {"visibleRows": 5},
                  {}, [], None, "attempt to index a nil value"):
        if panel_overflows(shape):
            problems.append(f"{shape!r} was misread as an overflowing panel")
    # An absent field still reads as 0, exactly as it did before #1608:
    # the entry condition for the six arrow and wheel checks is
    # unchanged, and only the no-overflow path is new.
    if not panel_overflows({"rowCount": 12}):
        problems.append("the absent-field reading of a panel dump changed")

    def expect_scroll_cause(label: str, last, inspected: int,
                            expected: dict) -> None:
        cause = scroll_fixture_cause(last, inspected)
        # Requirement 2: all three judged fields are named, whatever
        # shape the reading came back as.
        for key, want in expected.items():
            if f"{key}={want}" not in cause:
                problems.append(
                    f"{label}: cause does not name {key}={want}: {cause!r}")
        if repr(scroll_reading(last)) not in cause:
            problems.append(
                f"{label}: cause drops the reading itself: {cause!r}")
        if f"{inspected} inspected name(s)" not in cause:
            problems.append(
                f"{label}: cause does not say how many names were "
                f"inspected: {cause!r}")

    expect_scroll_cause(
        "a panel that simply did not overflow",
        {"open": True, "rowCount": 4, "visibleRows": 9, "scrollbar": None,
         "available": True},
        9, {"rowCount": "4", "visibleRows": "9", "scrollbar": "None"})
    expect_scroll_cause(
        "a dump missing the scrollbar field entirely",
        {"open": True, "rowCount": 4, "visibleRows": 9},
        3, {"rowCount": "4", "visibleRows": "9",
            "scrollbar": UNAVAILABLE})
    # A dump that is not a table at all carries none of the three, and
    # the diagnostic still names each one rather than dropping them.
    for label, last in (("an absent dump", None),
                        ("a dump that errored into text",
                         "attempt to index a nil value"),
                        ("an empty table serialized as {}", {})):
        expect_scroll_cause(label, last, 1,
                            {"rowCount": UNAVAILABLE,
                             "visibleRows": UNAVAILABLE,
                             "scrollbar": UNAVAILABLE})
    # The projection keeps a readable dump compact and quotes an
    # unreadable one raw, so the raw response survives as context.
    if scroll_reading(overflowing) != {
            "open": True, "rowCount": 12, "visibleRows": 5,
            "scrollbar": {"handle": 3}, "available": True}:
        problems.append("the panel reading dropped a judged field")
    if scroll_reading("boom") != "boom":
        problems.append("an unreadable dump was not quoted raw")
    # The projection must not invent a field the dump omitted, or it
    # would print `scrollbar: None` beside a `scrollbar=<unavailable>`
    # label and contradict it. An explicit null still reads as one.
    if scroll_reading({"rowCount": 4, "visibleRows": 9}) != {
            "rowCount": 4, "visibleRows": 9}:
        problems.append("the panel reading invented a field the dump omitted")
    if scroll_reading({"rowCount": 4, "scrollbar": None}) != {
            "rowCount": 4, "scrollbar": None}:
        problems.append("the panel reading dropped an explicit null")

    # --- requirement 4: the two failures stay distinguishable ---------
    before = (failures, fixture_failures)
    captured = io.StringIO()
    with redirect_stdout(captured):
        check(False, "synthetic behavioural failure", "detail")
        fixture_failure("synthetic fixture failure", params)
    counted = (failures, fixture_failures)
    failures, fixture_failures = before
    if counted != (before[0] + 1, before[1] + 1):
        problems.append(
            f"the two failure kinds do not count independently: "
            f"{before!r} -> {counted!r}")
    printed = captured.getvalue()
    if "  FAIL  synthetic behavioural failure" not in printed:
        problems.append(f"an ordinary failure lost its FAIL marker: {printed!r}")
    if "  FIXTURE  synthetic fixture failure" not in printed:
        problems.append(
            f"a fixture failure lost its FIXTURE marker: {printed!r}")
    if "FAIL" in printed.split("FIXTURE", 1)[-1]:
        problems.append(
            f"a fixture failure is reported as an ordinary FAIL: {printed!r}")

    # --- and both kinds really do exit non-zero ----------------------
    for label, seen, want in (
            ("a clean run", (0, 0), 0),
            ("a behavioural failure alone", (1, 0), 1),
            ("a fixture failure alone", (0, 1), 1),
            ("both kinds together", (2, 3), 1)):
        got = exit_code(*seen)
        if got != want:
            problems.append(f"{label} exits {got}, expected {want}")

    for line in problems:
        print(f"FAIL: {line}", file=sys.stderr)
    print(f"--- self-test ---\n  fixture classification cases: "
          f"{'OK' if not problems else f'{len(problems)} FAILED'}")
    return 0 if not problems else 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9422)
    ap.add_argument("--seed", type=int, default=42)
    # 64 chunks, matching tools/location_content_probe.py. Phase 3's
    # entity is not what pins it: #997's guaranteed placement puts a
    # location on even a 4-chunk world. Phase 4's is — measured on this
    # fixture, seed 42 generates NO rivers at all at 4 or 8 chunks and
    # rivers by 16, and since #1604 an absent river FAILS rather than
    # skipping. 64 also buys the recurrence coverage phase 5 looks for:
    # two discovered ruins of one definition are the likeliest place a
    # modifier recurs.
    ap.add_argument("--size", type=int, default=64,
                    help="world size in chunks")
    ap.add_argument("--window", default="1280x720",
                    help="offscreen render target size")
    ap.add_argument("--self-test", action="store_true",
                    help="grade the fixture classification with synthetic "
                         "readings; boots no engine")
    args = ap.parse_args()

    if args.self_test:
        return self_test()

    proc = None
    try:
        proc, stored_name = phase1_boot(args)
        if stored_name:
            phase2_world(args.port, stored_name)
            phase3_location(args)
            phase4_river(args)
            phase5_bound_and_recurrence(args.port)
            # Scrolling runs BEFORE the unavailable phase: that phase
            # switches the active page to a custom-named one, and every
            # query here resolves the ACTIVE page.
            phase6_scrolling(args.port)
            phase7_unavailable(args)
            phase8_lifecycle(args.port)
    finally:
        quit_engine(args.port, proc)

    print()
    if fixture_failures:
        print(f"etymology_probe: {fixture_failures} FIXTURE failure(s) — a "
              f"required phase's precondition never held (the generated "
              f"world supplied no entity it needs, or the configuration it "
              f"manufactures stopped producing one), so that phase never ran")
    if failures:
        print(f"etymology_probe: {failures} check(s) FAILED")
    if exit_code(failures, fixture_failures):
        return exit_code(failures, fixture_failures)
    print("etymology_probe: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
