"""The `buildings` family: the buildings viewer (#888, #2492) in its
normal built-state, no-built-state, and YAML-free forms.

Three scenarios, one fresh hidden boot each:

  * `check_buildings_mode` — phase `5.`, `--preview buildings/acolyte_portal`.
  * `check_buildings_without_built` — phase `6.`, `--preview buildings/cargo_hold_S`.
  * `check_buildings_without_yaml` — phase `7.`, `--preview buildings/dungeon_1`.

This module owns the building expectations: the numbered-frame
convention, the `data/buildings/<name>.yaml` scanner, the
filesystem+YAML entry list, the built-state default label, and — since
#2492 — the declared lifecycle/facing matrix layered over that browser.

Row lookups go through `row_by_identity` / `filesystem_rows`, never by
matching a dumped row's LABEL against a raw entry: the combined list
holds declared rows beside raw ones and the two may legitimately draw
the same text, so a label match could click the wrong row. The raw
`entries` / `defaultEntry` / `selected` / trimmed-loading checks below
are deliberately unchanged — they gate exactly what they gated before,
which is the point of keeping those dump fields meaning what they meant.

A library, not a probe: registered nowhere, runnable only through the
facade's inventory (`python3 tools/preview_probe.py --only buildings`).
"""
from __future__ import annotations

import base64
import os
import shutil
import time
from probelib import quit_engine, send, poll_until

from .harness import (boot_preview, check, check_forced_replay,
                      check_no_gameplay_scripts_loaded, check_trimmed_loading,
                      click_element, dump, hold_preview_key, poll_state,
                      press_preview_key, window_size)

def is_frame_name(f: str) -> bool:
    """The checked-in numbered-frame convention, mirroring
    Engine.Preview.Building.isFrameFileName: frame_000.png, frame1.png,
    frame-3.png — but never floor.png or wall_ne.png."""
    stem = os.path.splitext(f)[0].lower()
    if not stem.startswith("frame"):
        return False
    rest = stem[len("frame"):].lstrip("-_")
    return bool(rest) and rest.isascii() and rest.isdigit()


def building_yaml(name: str) -> dict:
    """{'sprite': str|None, 'built': str|None,
        'anims': {name: {'fps': float, 'loop': bool, 'frames': [paths]}}}
    from data/buildings/<name>.yaml, or all-empty when the file is
    absent (dungeon_1 has none).

    Parsed with a deliberately dumb indent scanner rather than PyYAML
    (not a probe dependency), the same way expected_yaml_meta above
    reads a unit file. The per-animation defaults restated here (fps 8,
    loop FALSE) are BuildingYamlAnim's own — note loop differs from the
    units schema's default of true."""
    out: dict = {"sprite": None, "built": None, "anims": {}, "roles": {}}
    path = os.path.join("data", "buildings", name + ".yaml")
    if not os.path.exists(path):
        return out
    section = None            # None | "state_animations" | "animations"
    section_indent = -1
    anim_indent = None
    cur = None
    with open(path) as fh:
        for raw in fh:
            stripped = raw.strip()
            if not stripped or stripped.startswith("#"):
                continue
            indent = len(raw) - len(raw.lstrip())
            if section is not None and indent <= section_indent:
                section, anim_indent, cur = None, None, None
            if section is None:
                if stripped.startswith("sprite:"):
                    out["sprite"] = stripped.split(":", 1)[1].strip().strip('"')
                elif stripped == "state_animations:":
                    section, section_indent = "state_animations", indent
                elif stripped == "animations:":
                    section, section_indent = "animations", indent
                continue
            if section == "state_animations":
                # Every declared role, not just `built` (#2492): the
                # lifecycle rows the viewer exposes are exactly these,
                # in Building.Schema's fixed order.
                key, _, value = stripped.partition(":")
                key, value = key.strip(), value.strip().strip('"')
                if key and value:
                    out["roles"][key] = value
                    if key == "built":
                        out["built"] = value
                continue
            # section == "animations": a key at the block's own child
            # indent starts a new animation; anything deeper belongs to it.
            if stripped.endswith(":") and (anim_indent is None
                                           or indent == anim_indent):
                anim_indent = indent
                cur = stripped[:-1].strip()
                out["anims"][cur] = {"fps": 8.0, "loop": False, "frames": []}
            elif cur is None:
                continue
            elif stripped.startswith("fps:"):
                out["anims"][cur]["fps"] = float(stripped.split(":", 1)[1].strip())
            elif stripped.startswith("loop:"):
                out["anims"][cur]["loop"] = (
                    stripped.split(":", 1)[1].strip() == "true")
            elif stripped.startswith("- "):
                out["anims"][cur]["frames"].append(stripped[2:].strip().strip('"'))
    return out


def expected_building_entries(name: str) -> list[tuple[str, bool]]:
    """Independent, filesystem+YAML-derived expectation for a building's
    entry list — mirrors Engine.Preview.Building.discoverBuildingEntries
    without importing any Haskell/Lua code: a directory whose textures a
    YAML animation actually DECLARES, or whose textures all follow the
    numbered-frame convention, is ONE animation entry labeled by its
    item-relative path; every other directory is descended into, so its
    textures surface as ordinary statics. Returns (label, animated)
    pairs in the engine's own label order."""
    root = os.path.join("assets", "textures", "buildings", name)
    meta = building_yaml(name)
    anim_dirs = {
        os.path.dirname(p).replace(os.sep, "/")
        for a in meta["anims"].values() for p in a["frames"]
    }
    out: list[tuple[str, bool]] = []

    def walk(d: str) -> None:
        for entry in sorted(os.listdir(d)):
            full = os.path.join(d, entry)
            if os.path.islink(full):
                continue
            label = os.path.relpath(full, root).replace(os.sep, "/")
            if os.path.isdir(full):
                pngs = [f for f in os.listdir(full)
                        if f.lower().endswith(".png")
                        and not os.path.islink(os.path.join(full, f))]
                if pngs and (full.replace(os.sep, "/") in anim_dirs
                             or all(is_frame_name(f) for f in pngs)):
                    out.append((label, True))
                else:
                    walk(full)
            elif entry.lower().endswith(".png"):
                out.append((label, False))

    walk(root)
    return sorted(out)


def dumped_building_entries(d: dict) -> list[tuple[str, bool]]:
    return [(e.get("label"), e.get("animated") is True)
            for e in (d.get("entries") or [])]


def row_by_identity(d: dict, identity: str) -> dict | None:
    """The visible combined row with this stable identity, or None when
    it is scrolled out of view. Identity, never label: `built -> idle`
    and the raw `idle` directory are two different rows."""
    return next((r for r in (d.get("rows") or [])
                 if r.get("identity") == identity), None)


def filesystem_rows(d: dict) -> list[dict]:
    """Only the RAW rows of the combined list — the ones whose label is
    a filesystem entry label and whose behavior #888 fixed."""
    return [r for r in (d.get("rows") or []) if r.get("kind") == "filesystem"]


def declared_identities(name: str) -> list[str]:
    """The row identities the building's YAML must produce, in the fixed
    order Building.Schema.BuildingRole enumerates, then the sprite —
    derived from the file, so the dump is checked against the
    declaration rather than against itself."""
    meta = building_yaml(name)
    order = ["construction", "appearance", "built", "destruction"]
    out = [f"lifecycle:{r}" for r in order if r in meta["roles"]]
    if meta["sprite"] is not None:
        out.append("sprite")
    return out


def built_default_label(name: str) -> str | None:
    """The label the state_animations.built animation must resolve to:
    the DIRECTORY its declared frames live in — deliberately computed
    from the frame paths, never from the animation's own YAML name,
    because acolyte_portal's differ (portal-idle vs idle/)."""
    meta = building_yaml(name)
    built = meta["built"]
    frames = meta["anims"].get(built, {}).get("frames") if built else None
    if not frames:
        return None
    return os.path.basename(os.path.dirname(frames[0]))


def check_buildings_mode(port: int) -> bool:
    print("5. buildings viewer (--preview buildings/acolyte_portal)")
    name = "acolyte_portal"
    proc = boot_preview(port, f"5. buildings/{name}", f"buildings/{name}",
                        "preview engine (buildings)")
    try:
        meta = building_yaml(name)
        expected = expected_building_entries(name)
        want_default = built_default_label(name)
        d = poll_state(port, "ready")

        ok_mode = check("mode == building (no placeholder remains)",
                        d.get("mode") == "building", d.get("mode"))

        # Requirement 1: the FULL ordered entry list — animation
        # subdirectories AND loose statics together — cross-checked
        # against the filesystem+YAML.
        ok_entries = check("entry list (labels + static/animation identity) "
                           "matches the filesystem-derived expectation "
                           "exactly, in order",
                           dumped_building_entries(d) == expected,
                           f"dumped={dumped_building_entries(d)} "
                           f"expected={expected}")

        ok_default = check("default selection is the state_animations.built "
                           "animation's DIRECTORY, not its YAML name",
                           want_default is not None
                           and d.get("defaultEntry") == want_default
                           and (d.get("selected") or {}).get("label") == want_default,
                           f"defaultEntry={d.get('defaultEntry')} "
                           f"selected={d.get('selected')} want={want_default} "
                           f"(yaml animation name={meta['built']!r})")

        # Requirement 1: playback metadata comes from that YAML entry.
        pb = d.get("playback") or {}
        want_meta = meta["anims"].get(meta["built"] or "", None)
        ok_meta = check("effective fps/loop match data/buildings/"
                        f"{name}.yaml",
                        want_meta is not None
                        and abs((pb.get("fps") or 0) - want_meta["fps"]) < 1e-6
                        and pb.get("loop") == want_meta["loop"],
                        f"dump=({pb.get('fps')}, {pb.get('loop')}) yaml={want_meta}")

        # The frame index advances over WALL time. NB the 1-tuple:
        # poll_until returns on a TRUTHY value and frame 0 is falsy.
        before = ((dump(port).get("playback") or {}).get("frameIndex"))
        after = poll_until(6.0, lambda: (
            (lambda i: (i,) if i != before else None)(
                (dump(port).get("playback") or {}).get("frameIndex"))))
        ok_advance = check("frame index advances over wall time",
                           after is not None and after[0] != before,
                           f"before={before} after={after}")

        # Resize: the selected entry and scroll offset both survive.
        pre = dump(port)
        pre_bounds = pre.get("panelBounds") or {}
        pre_selected = (pre.get("selected") or {}).get("label")
        pre_scroll = pre.get("scrollOffset")
        win_w, win_h = window_size(port)
        send(port, f"return engine.setResolution({win_w + 200}, {win_h + 150})",
             timeout=10.0)
        post = poll_until(10.0, lambda: (
            (lambda s: s if (s.get("panelBounds") or {}) != pre_bounds else None)(
                dump(port)))) or dump(port)
        ok_resize = check("panel bounds reflow while the selection and scroll "
                          "offset survive a resize",
                          (post.get("panelBounds") or {}) != pre_bounds
                          and (post.get("selected") or {}).get("label") == pre_selected
                          and post.get("scrollOffset") == pre_scroll,
                          f"before=({pre_selected}, {pre_scroll}) "
                          f"after=({(post.get('selected') or {}).get('label')}, "
                          f"{post.get('scrollOffset')})")

        # Requirement 1: selecting a STATIC row (located from the dump,
        # never a hardcoded coordinate) switches to it — and a static
        # selection exposes NO playback at all, which is exactly what
        # distinguishes it from an animation entry.
        statics = {label for label, animated in expected if not animated}
        row = next((r for r in filesystem_rows(post)
                    if r.get("label") in statics), None)
        if row is None:
            ok_static = check("clicking a static row selects it with no playback",
                              False, "no visible static row to click")
        else:
            click_element(port, row.get("bounds") or {})
            after_click = poll_until(10.0, lambda: (
                (lambda s: s if (s.get("selected") or {}).get("label")
                    == row["label"] and s.get("state") == "ready" else None)(
                        dump(port)))) or dump(port)
            ok_static = check("clicking a static row (via row bounds, not "
                              "hardcoded coords) selects it, resolves, and "
                              "exposes no playback",
                              (after_click.get("selected") or {}).get("label")
                              == row["label"]
                              and after_click.get("state") == "ready"
                              and after_click.get("playback") is None,
                              f"selected={after_click.get('selected')} "
                              f"state={after_click.get('state')} "
                              f"playback={after_click.get('playback')}")

        # #1833: forced continuous replay, on the buildings half. The
        # portal's `appear` is 16 frames at 8 fps — a 2.0 s cycle, the
        # widest tracked `loop: false` margin — and buildings DEFAULT to
        # loop=false, so this is the half where holding the end bit
        # hardest. Placed after the behavioural checks above and reading
        # its rows from a FRESH dump, because those may have scrolled or
        # reselected — and because it deliberately ends on an animation
        # rather than the static selection ok_static needs.
        cur = dump(port)
        anim_loops = {e.get("label"): e.get("loop")
                      for e in (cur.get("entries") or [])
                      if e.get("animated") is True}
        nonloop = next((r for r in filesystem_rows(cur)
                        if anim_loops.get(r.get("label")) is False), None)
        if nonloop is None:
            ok_replay = check("a loop:false animation entry replays "
                              "continuously", False,
                              "no visible loop:false animation row to click")
        else:
            click_element(port, nonloop.get("bounds") or {})
            got = poll_until(10.0, lambda: (
                (lambda s: s if (s.get("playback") or {}).get("entry")
                    == nonloop["label"] and s.get("state") == "ready" else None)(
                        dump(port))))
            selected_at = time.monotonic()
            pb2 = (got or dump(port)).get("playback") or {}
            # Requirement 6: the dump still reports the AUTHORED value —
            # without this the replay check would also pass on an
            # implementation that forced every entry to loop=true.
            ok_truthful = check("the replay fixture still reports its authored "
                                "loop=false (or this check proves nothing)",
                                pb2.get("entry") == nonloop["label"]
                                and pb2.get("loop") is False,
                                f"entry={pb2.get('entry')} loop={pb2.get('loop')}")
            ok_replay = check_forced_replay(
                port, f"buildings/{name} {nonloop['label']} (authored loop=false)",
                selected_at, pb2.get("frameCount"), pb2.get("fps")) \
                and ok_truthful

        ok_matrix = check_declared_matrix(port, name)

        # Requirement 1: only THIS building's textures (plus list chrome).
        root_prefix = os.path.join("assets", "textures", "buildings", name) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_mode, ok_entries, ok_default, ok_meta, ok_advance,
                    ok_resize, ok_replay, ok_static, ok_matrix, ok_trimmed,
                    ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_buildings_without_built(port: int) -> bool:
    print("6. building with no state_animations.built "
          "(--preview buildings/cargo_hold_S): sprite fallback + "
          "convention-recognized animation")
    name = "cargo_hold_S"
    proc = boot_preview(port, f"6. buildings/{name}", f"buildings/{name}",
                        "preview engine (buildings, no built state)")
    try:
        meta = building_yaml(name)
        expected = expected_building_entries(name)
        ok_fixture = check("the fixture really declares no built state (or "
                           "this check proves nothing)",
                           meta["built"] is None and meta["sprite"] is not None,
                           f"built={meta['built']} sprite={meta['sprite']}")
        d = poll_state(port, "ready")
        ok_entries = check("entry list matches the filesystem-derived "
                           "expectation exactly, in order",
                           dumped_building_entries(d) == expected,
                           f"dumped={dumped_building_entries(d)} "
                           f"expected={expected}")
        ok_default = check("default selection falls back to the YAML's own "
                           "sprite",
                           d.get("defaultEntry")
                           == os.path.basename(meta["sprite"] or ""),
                           f"defaultEntry={d.get('defaultEntry')} "
                           f"sprite={meta['sprite']}")

        # demolish/ is a real numbered-frame folder the YAML never
        # mentions: recognized by convention, with the documented
        # building defaults (fps 8, loop FALSE — not the units viewer's
        # loop=true).
        by_label = {e.get("label"): e for e in (d.get("entries") or [])}
        demolish = by_label.get("demolish")
        ok_convention = check("a YAML-less numbered-frame directory is still "
                              "an animation, with fps=8 / loop=false",
                              demolish is not None
                              and demolish.get("animated") is True
                              and abs((demolish.get("fps") or 0) - 8.0) < 1e-6
                              and demolish.get("loop") is False,
                              demolish)

        # #2492: with no `built` role declared, the initial combined-list
        # selection falls to the declared SPRITE row — and `defaultEntry`
        # and `selected` still name the raw entry they always did.
        got_ids = [r.get("identity") for r in (d.get("rows") or [])]
        ok_matrix = check("declares construction + sprite only, and the "
                          "sprite row is the initial selection",
                          got_ids[:len(declared_identities(name))]
                          == declared_identities(name)
                          and (d.get("selection") or {}).get("identity")
                              == "sprite"
                          and d.get("defaultSelection") == "sprite",
                          f"rows={got_ids} "
                          f"selection={d.get('selection')} "
                          f"defaultSelection={d.get('defaultSelection')}")

        # demolish/ is browsable but no YAML mentions it: it must be
        # classified UNDECLARED and still be a first-class raw row.
        fs = {e.get("label"): e for e in (d.get("filesystemEntries") or [])}
        ok_undeclared = check("a YAML-less directory is classified "
                              "undeclared without being filtered out",
                              "demolish" in fs
                              and fs["demolish"].get("undeclared") is True
                              and "construct" in fs
                              and fs["construct"].get("declared")
                                  == ["lifecycle:construction"],
                              fs)

        root_prefix = os.path.join("assets", "textures", "buildings", name) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        return all([ok_fixture, ok_entries, ok_default, ok_convention,
                    ok_matrix, ok_undeclared, ok_trimmed])
    finally:
        quit_engine(port, proc)


def check_buildings_without_yaml(port: int) -> bool:
    print("7. building with NO data/buildings YAML (--preview buildings/dungeon_1): "
          "first-entry default, nested statics")
    name = "dungeon_1"
    proc = boot_preview(port, f"7. buildings/{name}", f"buildings/{name}",
                        "preview engine (buildings, no yaml)")
    try:
        ok_fixture = check("the fixture really has no building YAML (or this "
                           "check proves nothing)",
                           not os.path.exists(os.path.join("data", "buildings",
                                                           name + ".yaml")))
        expected = expected_building_entries(name)
        d = poll_state(port, "ready")
        ok_entries = check("entry list matches the filesystem-derived "
                           "expectation exactly, in order",
                           dumped_building_entries(d) == expected,
                           f"dumped={dumped_building_entries(d)} "
                           f"expected={expected}")
        # damaged/ holds piece sprites, not frames: it must surface as
        # item-relative statics, never as one animation entry.
        labels = [label for label, _ in dumped_building_entries(d)]
        ok_nested = check("the damaged/ subtree surfaces as ordinary "
                          "item-relative statics, not one animation",
                          "damaged" not in labels
                          and any(l.startswith("damaged/") for l in labels)
                          and all(not animated
                                  for _, animated in dumped_building_entries(d)),
                          labels)
        ok_default = check("default falls all the way through to the first entry",
                           d.get("defaultEntry") == (expected[0][0] if expected else "")
                           and (d.get("selected") or {}).get("label")
                               == (expected[0][0] if expected else None),
                           f"defaultEntry={d.get('defaultEntry')} "
                           f"selected={d.get('selected')}")
        ok_no_playback = check("a static selection exposes no playback",
                               d.get("playback") is None, d.get("playback"))
        # #2492 requirement 4: no YAML means no declared rows at all, and
        # the raw browser behaves exactly as it did before the matrix
        # existed — including having no facing model to navigate.
        ok_no_matrix = check("a YAML-less building exposes no declared rows "
                             "and keeps every raw row selectable",
                             not (d.get("lifecycle") or [])
                             and d.get("staticSprite") is None
                             and d.get("declaration") is None
                             and not (d.get("facingRow") or [])
                             and d.get("selectedFacing") is None
                             and len(filesystem_rows(d)) == len(d.get("rows") or [])
                             and (d.get("totals") or {})
                                 .get("undeclaredFilesystemEntries")
                                 == len(d.get("entries") or []),
                             f"lifecycle={d.get('lifecycle')} "
                             f"staticSprite={d.get('staticSprite')} "
                             f"totals={d.get('totals')}")

        root_prefix = os.path.join("assets", "textures", "buildings", name) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        return all([ok_fixture, ok_entries, ok_nested, ok_default,
                    ok_no_playback, ok_no_matrix, ok_trimmed])
    finally:
        quit_engine(port, proc)


def check_declared_matrix(port: int, name: str) -> bool:
    """#2492: the declared lifecycle/facing matrix, driven through the
    live viewer.

    Everything here is located from the dump — row identities and
    facing-cell bounds — never from a hardcoded coordinate or a label
    match, so a geometry or ordering regression fails rather than being
    clicked past.
    """
    meta = building_yaml(name)
    want_ids = declared_identities(name)
    d = dump(port)

    # The combined list: declared rows first, in the fixed role order,
    # then every raw row in its existing relative order.
    got_ids = [r.get("identity") for r in (d.get("rows") or [])]
    raw_ids = [f"filesystem:{label}"
               for label, _ in dumped_building_entries(d)]
    ok_order = check("combined rows are the declared rows in role order, "
                     "then every raw row in its existing order",
                     got_ids == want_ids + raw_ids,
                     f"got={got_ids} want={want_ids + raw_ids}")

    ok_distinct = check("every combined row identity is distinct",
                        len(set(got_ids)) == len(got_ids), got_ids)

    # An UNDECLARED role is absent, never reported as missing.
    declared_roles = [e.get("role") for e in (d.get("lifecycle") or [])]
    ok_absent = check("an undeclared lifecycle role produces no row at all",
                      sorted(r for r in declared_roles if r)
                      == sorted(meta["roles"].keys()),
                      f"dump={declared_roles} yaml={sorted(meta['roles'])}")

    # Every shipped building declares its art legacy today, so every
    # cell must be flagged legacy and repeat one list four times.
    lifecycle = d.get("lifecycle") or []
    ok_legacy = check("each declared entry reports its OWN provenance, and "
                      "a legacy entry flags all four repeated cells",
                      bool(lifecycle)
                      and all(e.get("declaration") in ("legacy", "canonical")
                              for e in lifecycle)
                      and all(all(c.get("legacy") is True
                                  for c in (e.get("cells") or []))
                              for e in lifecycle
                              if e.get("declaration") == "legacy"),
                      [(e.get("identity"), e.get("declaration"),
                        [c.get("legacy") for c in (e.get("cells") or [])])
                       for e in lifecycle])

    ok_cells = check("every declared entry has exactly four cells in camera "
                     "order south, west, north, east",
                     all([c.get("facing") for c in (e.get("cells") or [])]
                         == ["south", "west", "north", "east"]
                         for e in lifecycle + ([d["staticSprite"]]
                                               if d.get("staticSprite") else [])),
                     [[c.get("facing") for c in (e.get("cells") or [])]
                      for e in lifecycle])

    # The shipped art really is on disk, so nothing here is diagnostic —
    # which is what makes a future missing-art regression visible.
    totals = d.get("totals") or {}
    ok_totals = check("a fully-authored building reports no missing cells "
                      "and no unresolved lifecycle rows",
                      totals.get("missingCells") == 0
                      and totals.get("unresolvedLifecycleRows") == 0,
                      totals)

    # Selecting a declared row, then each of its facings, through
    # dump-reported bounds only.
    built = row_by_identity(d, "lifecycle:built")
    if built is None:
        return all([ok_order, ok_distinct, ok_absent, ok_legacy, ok_cells,
                    ok_totals,
                    check("the declared built row is visible to click",
                          False, got_ids)])

    click_element(port, built.get("bounds") or {})
    after = poll_until(10.0, lambda: (
        (lambda s: s if (s.get("selection") or {}).get("identity")
            == "lifecycle:built" and s.get("state") == "ready" else None)(
                dump(port)))) or dump(port)
    ok_select = check("clicking the declared built row selects it, reports "
                      "its provenance, and still projects `selected` onto "
                      "the RAW entry",
                      (after.get("selection") or {}).get("identity")
                      == "lifecycle:built"
                      and after.get("declaration") in ("legacy", "canonical")
                      and (after.get("selected") or {}).get("label")
                          == built_default_label(name),
                      f"selection={after.get('selection')} "
                      f"declaration={after.get('declaration')} "
                      f"selected={after.get('selected')}")

    facings = [c.get("facing") for c in (after.get("facingRow") or [])]
    ok_strip = check("the facing strip shows four cells in camera order",
                     facings == ["south", "west", "north", "east"], facings)

    ok_click = True
    for want in ["west", "north", "east", "south"]:
        cell = next((c for c in (dump(port).get("facingRow") or [])
                     if c.get("facing") == want), None)
        if cell is None or not cell.get("bounds"):
            ok_click = check(f"facing cell {want} is clickable", False, cell)
            break
        click_element(port, cell["bounds"])
        got = poll_until(10.0, lambda: (
            (lambda s: s if s.get("selectedFacing") == want else None)(
                dump(port)))) or dump(port)
        if got.get("selectedFacing") != want:
            ok_click = check(f"clicking the {want} cell enlarges it", False,
                             f"selectedFacing={got.get('selectedFacing')}")
            break
        # A facing change must not reselect the row.
        if (got.get("selection") or {}).get("identity") != "lifecycle:built":
            ok_click = check("a facing change does not reselect the row",
                             False, got.get("selection"))
            break
    else:
        ok_click = check("each facing cell enlarges its own view when "
                         "clicked through its dump-reported bounds, without "
                         "reselecting the row", True)

    # Keyboard wraparound, through a REAL key tap on the same row: the
    # last click above left the strip on south, so Left must wrap.
    wrapped, _ = press_preview_key(
        port, "Left", lambda st: st.get("selectedFacing") == "east")
    ok_wrap = check("Left from south wraps round to east",
                    wrapped.get("selectedFacing") == "east",
                    wrapped.get("selectedFacing"))

    # Held repeat reaches the strip too, and stops on release.
    #
    # The hold is proved by COUNTING moves, and the release by the facing
    # then holding still — not by comparing the observation that ended
    # the hold against the state after it. Release is enqueued after that
    # observation, so one more repeat can legitimately land in between;
    # asserting those two are equal would be asserting a race, not the
    # contract. Facings WRAP, so a hold never terminates itself.
    seen: list[str] = []

    def moved(st: dict) -> bool:
        facing = st.get("selectedFacing")
        if facing and (not seen or seen[-1] != facing):
            seen.append(facing)
        return len(seen) >= 3

    hold_preview_key(port, "Right", moved)
    settled = dump(port).get("selectedFacing")
    still = poll_until(1.5, lambda: (
        (lambda s: (s,) if s.get("selectedFacing") != settled else None)(
            dump(port))))
    ok_hold = check("a held Right repeats through the facing strip and stops "
                    "moving once released",
                    len(seen) >= 3 and still is None,
                    f"visited={seen} settled={settled} "
                    f"moved-after-release={still}")

    # And a RAW row still has no facing model at all.
    raw = next((r for r in filesystem_rows(dump(port))), None)
    ok_raw = True
    if raw is not None:
        click_element(port, raw.get("bounds") or {})
        back = poll_until(10.0, lambda: (
            (lambda s: s if (s.get("selection") or {}).get("identity")
                == raw.get("identity") else None)(dump(port)))) or dump(port)
        ok_raw = check("a raw filesystem row exposes no facing strip and no "
                       "declaration",
                       not (back.get("facingRow") or [])
                       and back.get("selectedFacing") is None
                       and back.get("declaration") is None,
                       f"facingRow={back.get('facingRow')} "
                       f"selectedFacing={back.get('selectedFacing')} "
                       f"declaration={back.get('declaration')}")

    return all([ok_order, ok_distinct, ok_absent, ok_legacy, ok_cells,
                ok_totals, ok_select, ok_strip, ok_click, ok_wrap, ok_hold,
                ok_raw])


# --- #2492's live matrix fixture ------------------------------------
#
# No shipped building declares a canonical `sprites`/`frames` block, a
# `destruction` role, an unresolved animation reference, or art that is
# not on disk — so the acceptance's "verifies missing, unresolved,
# legacy, and provenance states" is unreachable from the eight checked-in
# definitions. This phase generates one building that declares all of
# them, exercises it through the real engine, and removes it again.
#
# It lives at the canonical paths rather than a temporary root because
# that is where the viewer resolves a building: `resolveItemDir` takes a
# single directory name under `assets/textures/buildings`, and
# `buildingDataPath` reads `data/buildings/<name>.yaml`. Both paths are
# gitignored, and the teardown runs in a `finally`.

FIXTURE = "probe_matrix_fixture"
FIXTURE_ROOT = os.path.join("assets", "textures", "buildings", FIXTURE)
FIXTURE_YAML = os.path.join("data", "buildings", FIXTURE + ".yaml")

# A 1x1 PNG. The viewer only needs `engine.getTextureSize` to answer, so
# the pixels are immaterial — what matters is that a declared-and-present
# path really loads while a declared-and-invalid one really cannot.
_PNG_1X1 = base64.b64decode(
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQ"
    "DwAEhQGAhKmMIQAAAABJRU5ErkJggg==")


def _png(path: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "wb") as fh:
        fh.write(_PNG_1X1)


def write_matrix_fixture() -> None:
    """One building declaring every state the acceptance names.

    Roles: a CANONICAL `construction` whose four facings are all on disk;
    a CANONICAL `appearance` whose four facings are the four invalid
    kinds; a LEGACY multi-frame `built` on disk; and a `destruction`
    naming an animation the definition never declares. Plus a LEGACY
    sprite — so one definition answers `canonical` and `legacy` for
    different entries, which is exactly what entry-specific provenance
    means.

    Raw classes present beside them: two animation directories, a
    top-level loose static, and a nested static.
    """
    remove_matrix_fixture()
    for name in ("default.png", "loose.png"):
        _png(os.path.join(FIXTURE_ROOT, name))
    _png(os.path.join(FIXTURE_ROOT, "sub", "nested.png"))
    for facing in ("s", "w", "n", "e"):
        _png(os.path.join(FIXTURE_ROOT, "build", f"{facing}.png"))
    for i in range(2):
        _png(os.path.join(FIXTURE_ROOT, "idle", f"frame_{i:03d}.png"))

    # The four invalid kinds, one per facing of `appearance`.
    os.makedirs(os.path.join(FIXTURE_ROOT, "broken_dir.png"), exist_ok=True)
    os.symlink("default.png", os.path.join(FIXTURE_ROOT, "broken_link.png"))
    with open(os.path.join(FIXTURE_ROOT, "broken.txt"), "w") as fh:
        fh.write("")
    os.mkfifo(os.path.join(FIXTURE_ROOT, "broken_fifo.png"))

    def p(*parts: str) -> str:
        return "/".join((FIXTURE_ROOT.replace(os.sep, "/"),) + parts)

    os.makedirs(os.path.dirname(FIXTURE_YAML), exist_ok=True)
    with open(FIXTURE_YAML, "w") as fh:
        fh.write(f"""buildings:
  - name: "{FIXTURE}"
    display_name: "Probe Matrix Fixture"
    category: "Test"
    visual_class: "gateway"
    tile_size: {{ x: 1, y: 1 }}
    build_work: 12.0
    sprite: "{p('default.png')}"
    state_animations:
      construction: build-anim
      appearance:   broken-anim
      built:        idle-anim
      destruction:  no-such-anim
    animations:
      build-anim:
        fps: 8
        loop: false
        frames:
          south: ["{p('build', 's.png')}"]
          west:  ["{p('build', 'w.png')}"]
          north: ["{p('build', 'n.png')}"]
          east:  ["{p('build', 'e.png')}"]
      broken-anim:
        fps: 8
        loop: false
        frames:
          south: ["{p('broken_dir.png')}"]
          west:  ["{p('broken_link.png')}"]
          north: ["{p('broken.txt')}"]
          east:  ["{p('broken_fifo.png')}"]
      idle-anim:
        fps: 8
        loop: false
        frames:
          default:
            - "{p('idle', 'frame_000.png')}"
            - "{p('idle', 'frame_001.png')}"
""")


def remove_matrix_fixture() -> None:
    """Teardown. Never a variable/wildcard delete: both paths are the
    module constants above and nothing else."""
    fifo = os.path.join(FIXTURE_ROOT, "broken_fifo.png")
    if os.path.exists(fifo) or os.path.islink(fifo):
        os.remove(fifo)
    shutil.rmtree(FIXTURE_ROOT, ignore_errors=True)
    if os.path.exists(FIXTURE_YAML):
        os.remove(FIXTURE_YAML)


def select_row(port: int, identity: str) -> dict:
    """Select the combined row with this identity by CLICKING its
    dump-reported bounds, scrolling it into view with the keyboard first
    when the list is taller than the panel.

    Bounds, never a hardcoded coordinate — and identity, never a label:
    a lifecycle row and the raw row backing the same files draw
    different text but either could move.
    """
    for _ in range(40):
        row = row_by_identity(dump(port), identity)
        if row is not None:
            break
        press_preview_key(port, "Down", lambda _s: True)
    else:
        return dump(port)
    click_element(port, row.get("bounds") or {})
    return poll_until(10.0, lambda: (
        (lambda s: s if (s.get("selection") or {}).get("identity") == identity
            and s.get("state") == "ready" else None)(dump(port)))) or dump(port)


def check_buildings_matrix(port: int) -> bool:
    """8. Every lifecycle role, the static row, every raw row class and
    every facing — selected through dump-reported bounds against a live
    engine — plus the missing, unresolved, legacy and provenance states
    the shipped definitions cannot express (#2492).
    """
    print("8. declared lifecycle/facing matrix "
          f"(--preview buildings/{FIXTURE}): every role, class and facing")
    write_matrix_fixture()
    proc = None
    try:
        proc = boot_preview(port, f"8. buildings/{FIXTURE}",
                            f"buildings/{FIXTURE}",
                            "preview engine (buildings matrix)")
        d = poll_state(port, "ready")

        want_rows = [
            "lifecycle:construction", "lifecycle:appearance",
            "lifecycle:built", "lifecycle:destruction", "sprite",
            "filesystem:build", "filesystem:default.png",
            "filesystem:idle", "filesystem:loose.png",
            "filesystem:sub/nested.png",
        ]
        got_rows = [r.get("identity") for r in (d.get("rows") or [])]
        ok_rows = check("the combined list is every declared row in role "
                        "order, then every raw row in label order",
                        got_rows == want_rows,
                        f"got={got_rows} want={want_rows}")

        # The raw browser is untouched by any of it: the invalid kinds
        # are not entries, and the animation directories still are.
        ok_raw = check("the raw browser still holds exactly its own "
                       "entries — no invalid kind became one",
                       dumped_building_entries(d) == [
                           ("build", True), ("default.png", False),
                           ("idle", True), ("loose.png", False),
                           ("sub/nested.png", False)],
                       dumped_building_entries(d))

        ok_default = check("the declared built row is the initial "
                           "selection, while defaultEntry stays raw",
                           d.get("defaultSelection") == "lifecycle:built"
                           and (d.get("selection") or {}).get("identity")
                               == "lifecycle:built"
                           and d.get("defaultEntry") == "idle"
                           and (d.get("selected") or {}).get("label") == "idle",
                           f"defaultSelection={d.get('defaultSelection')} "
                           f"defaultEntry={d.get('defaultEntry')} "
                           f"selected={d.get('selected')}")

        ok_totals = check("the totals report the declared diagnostics",
                          (d.get("totals") or {}).get("missingCells") == 8
                          and (d.get("totals") or {})
                              .get("unresolvedLifecycleRows") == 1
                          and (d.get("totals") or {})
                              .get("undeclaredFilesystemEntries") == 2,
                          d.get("totals"))

        # Every DECLARED row, selected through its own bounds, with its
        # own expected provenance and diagnostic state.
        expected = {
            "lifecycle:construction": ("canonical", False, True, {}),
            "lifecycle:appearance": ("canonical", False, True, {
                "south": "directory", "west": "symlink",
                "north": "unsupported_extension", "east": "special"}),
            "lifecycle:built": ("legacy", True, True, {}),
            "lifecycle:destruction": ("canonical", False, False, {
                "south": "unresolved", "west": "unresolved",
                "north": "unresolved", "east": "unresolved"}),
            "sprite": ("legacy", True, True, {}),
        }
        ok_declared = True
        for identity, (source, legacy, resolved, bad) in expected.items():
            state = select_row(port, identity)
            sel = state.get("selection") or {}
            facings = [c.get("facing") for c in (state.get("facingRow") or [])]
            reasons = {c.get("facing"): c.get("missingReason")
                       for c in (state.get("facingRow") or [])
                       if c.get("missing")}
            requested = [c.get("facing") for c in (state.get("facingRow") or [])
                         if c.get("missing") and c.get("handle") is not None]
            ok_declared = check(
                f"{identity}: selected by bounds, four facings, "
                f"declaration={source}, legacy={legacy}, "
                f"resolved={resolved}, diagnostics={sorted(bad.items())}",
                sel.get("identity") == identity
                and state.get("state") == "ready"
                and facings == ["south", "west", "north", "east"]
                and state.get("declaration") == source
                and sel.get("legacy") is legacy
                and sel.get("resolved") is resolved
                and reasons == bad
                and not requested,
                f"selection={sel} declaration={state.get('declaration')} "
                f"facings={facings} reasons={reasons} "
                f"state={state.get('state')} "
                f"requested-invalid={requested}") and ok_declared

            # Every facing of every declared row, clicked through its
            # own dump-reported bounds.
            for facing in ["west", "north", "east", "south"]:
                cell = next((c for c in (dump(port).get("facingRow") or [])
                             if c.get("facing") == facing), None)
                if cell is None or not cell.get("bounds"):
                    ok_declared = check(f"{identity}: {facing} cell is "
                                        "clickable", False, cell) and False
                    break
                click_element(port, cell["bounds"])
                got = poll_until(10.0, lambda: (
                    (lambda s: s if s.get("selectedFacing") == facing
                        and s.get("state") == "ready" else None)(
                            dump(port)))) or dump(port)
                # Enlarging a diagnostic facing must terminate, not hang.
                if (got.get("selectedFacing") != facing
                        or got.get("state") != "ready"
                        or (got.get("selection") or {}).get("identity")
                            != identity):
                    ok_declared = check(
                        f"{identity}: enlarging {facing} terminates without "
                        "reselecting the row", False,
                        f"facing={got.get('selectedFacing')} "
                        f"state={got.get('state')} "
                        f"selection={got.get('selection')}") and False
                    break
                want_missing = facing in bad
                if bool(got.get("path")) is want_missing:
                    ok_declared = check(
                        f"{identity}: {facing} resolves a path only when it "
                        "is not diagnostic", False,
                        f"missing={want_missing} path={got.get('path')}"
                    ) and False
                    break
            else:
                ok_declared = check(f"{identity}: every facing enlarges "
                                    "through its own bounds, and a "
                                    "diagnostic one resolves no path",
                                    True) and ok_declared

        # Every RAW row class: an animation directory, a top-level loose
        # static, and a nested static. None has a facing model.
        ok_rawrows = True
        for identity in ("filesystem:build", "filesystem:loose.png",
                         "filesystem:sub/nested.png"):
            state = select_row(port, identity)
            sel = state.get("selection") or {}
            ok_rawrows = check(
                f"{identity}: selectable, with no facing strip and no "
                "declaration",
                sel.get("identity") == identity
                and sel.get("kind") == "filesystem"
                and state.get("state") == "ready"
                and not (state.get("facingRow") or [])
                and state.get("selectedFacing") is None
                and state.get("declaration") is None
                and (state.get("selected") or {}).get("label")
                    == identity.split(":", 1)[1],
                f"selection={sel} facingRow={state.get('facingRow')} "
                f"declaration={state.get('declaration')} "
                f"selected={state.get('selected')}") and ok_rawrows

        # Trimmed loading, with the sharper claim this fixture allows:
        # not one of the invalid declared paths was ever requested.
        root_prefix = os.path.join("assets", "textures", "buildings",
                                   FIXTURE) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix,
                                           allow_chrome=True)
        loaded = dump(port).get("loadedPaths") or []
        invalid = [p for p in loaded
                   if os.path.basename(p) in ("broken_dir.png",
                                              "broken_link.png",
                                              "broken.txt",
                                              "broken_fifo.png")]
        ok_never = check("no invalid declared path was ever requested",
                         not invalid, invalid)

        return all([ok_rows, ok_raw, ok_default, ok_totals, ok_declared,
                    ok_rawrows, ok_trimmed, ok_never])
    finally:
        if proc is not None:
            quit_engine(port, proc)
        remove_matrix_fixture()
