"""The `units` family: the unit animation viewer (#887), the promoted
declaration (#1257/#1261), and the rest of the shipped roster (#1261),
every one atlas-backed since #1260.

Three scenarios; the third boots once per remaining unit in
`data/units/`, read from disk rather than written here:

  * `check_units_mode` — phase `3.`, `--preview units/acolyte`.
  * `check_units_promoted` — phase `4.`, `--preview units/tiller`.
  * `check_units_roster` — phase `4b.`, `--preview units/<name>` for
    every declared unit the two above did not cover.

This module owns the unit-asset expectations — the filesystem-derived
animation list, the compiled atlas index read straight off disk, the
YAML fps/loop scanner and the roster — which is why
`tools/ci_expensive_gates.py` lists it under `UNIT_ASSET_GLOBS`.

A library, not a probe: registered nowhere, runnable only through the
facade's inventory (`python3 tools/preview_probe.py --only units`).
"""
from __future__ import annotations

import json
import os
import time
from probelib import quit_engine, send, send_json, poll_until

from .harness import (boot_preview, check, check_forced_replay,
                      check_no_gameplay_scripts_loaded, check_trimmed_loading,
                      click_element, dump, press_preview_key, window_size)

GAME_DIRECTION_ORDER = ["south", "south-west", "west", "north-west",
                        "north", "north-east", "east", "south-east"]

# The five stored directions a bilaterally-symmetric animation ships;
# the other three are mirrored at draw time.
MIRROR_SOURCE = {"south-west": "south-east", "west": "east",
                 "north-west": "north-east"}


# Every spelling Engine.Preview.Unit.parseDirectionDirName accepts, so
# this expectation stays a faithful independent implementation of the
# documented rule rather than a stricter one that would false-fail on a
# short-form direction folder.
DIRECTION_SPELLINGS = frozenset(
    [d.lower() for d in GAME_DIRECTION_ORDER]
    + ["s", "sw", "w", "nw", "n", "ne", "e", "se"])


def expected_unit_animations(unit: str) -> list[str]:
    """Independent, FILESYSTEM-derived expectation for the animation
    list: direct children of animations/ that hold at least one
    recognized, non-symlinked direction folder with at least one
    non-symlinked .png, case-sensitive lexicographic — computed without
    importing any Haskell/Lua code.

    #1261 moved the viewer's own authority to the unit YAML and its
    compiled index, which makes this a genuinely independent oracle
    rather than a restatement: the two must agree exactly, because
    `tools/pack_atlas.py --validate-only --strict` fails on any
    animation PNG no declaration owns, and the engine refuses a unit
    whose index does not cover its declarations. A folder that appears
    here and NOT in the dump is an undeclared one — which is the
    excluded case #1261 specifies, and a real finding for a tracked
    tree."""
    root = os.path.join("assets", "textures", "units", unit, "animations")
    out = []
    for name in os.listdir(root):
        animdir = os.path.join(root, name)
        if not os.path.isdir(animdir) or os.path.islink(animdir):
            continue
        has_frames = False
        for d in os.listdir(animdir):
            if d.lower() not in DIRECTION_SPELLINGS:
                continue
            ddir = os.path.join(animdir, d)
            if not os.path.isdir(ddir) or os.path.islink(ddir):
                continue
            if any(f.lower().endswith(".png")
                   and not os.path.islink(os.path.join(ddir, f))
                   for f in os.listdir(ddir)):
                has_frames = True
                break
        if has_frames:
            out.append(name)
    return sorted(out)


def compiled_index(unit: str):
    """The unit's generated atlas index, or None when it ships none.

    Read straight off disk with the stdlib json module: the probe's whole
    job here (#1260, D-9) is to prove the RUNNING viewer is sampling the
    artifacts that are actually checked in, so its expectation has to
    come from those artifacts rather than from anything the engine
    reports about itself."""
    path = os.path.join("assets", "textures", "units", unit, "atlas", "index.json")
    if not os.path.exists(path):
        return None
    with open(path) as fh:
        return json.load(fh)


def check_atlas_backed(port: int, unit: str, d: dict) -> bool:
    """#1260: the units viewer renders a MIGRATED unit through the
    production atlas metadata, not a preview-only per-frame decoder.

    Everything below is cross-checked against the compiled index on
    disk. A viewer that quietly fell back to the source frames beside a
    broken atlas would still pass every #887 check — it would list the
    same animations, play at the same rate, and mirror the same cells —
    so the storage mode has to be observed directly."""
    index = compiled_index(unit)
    if index is None:
        return check(f"{unit} ships a compiled atlas index", False,
                     "no assets/textures/units/%s/atlas/index.json" % unit)
    by_name = {a["name"]: a for a in index.get("animations", [])}

    # 1. EVERY animation selected the atlas — not just the one playing.
    entries = d.get("entries") or []
    legacy = [e.get("label") for e in entries if e.get("storage") != "atlas"]
    wrong_path = [e.get("label") for e in entries
                  if e.get("atlas") != (by_name.get(e.get("label")) or {}).get("atlas_path")]
    ok_all = check("every animation reports atlas storage, each naming its "
                   "own compiled atlas from the index",
                   entries and not legacy and not wrong_path,
                   f"count={len(entries)} legacy={legacy[:5]} "
                   f"mismatched={wrong_path[:5]}")
    ok_count = check("the viewer lists every animation the index compiled",
                     len(entries) == len(by_name),
                     f"dumped={len(entries)} index={len(by_name)}")

    # 2. The PLAYING clip samples that atlas, with the index's own cell
    #    geometry — a whole-image sample would report no cell at all.
    pb = d.get("playback") or {}
    want = by_name.get(pb.get("animation")) or {}
    cell = pb.get("cell") or {}
    ok_playing = check("the playing clip samples its compiled atlas with the "
                       "index's own cell size",
                       pb.get("storage") == "atlas"
                       and pb.get("atlas") == want.get("atlas_path")
                       and pb.get("texturePath") == want.get("atlas_path")
                       and cell.get("width") == want.get("cell_width")
                       and cell.get("height") == want.get("cell_height"),
                       f"playback=({pb.get('storage')}, {pb.get('texturePath')}, "
                       f"{cell}) index=({want.get('atlas_path')}, "
                       f"{want.get('cell_width')}x{want.get('cell_height')})")

    # 3. Each visible direction cell samples a SUB-RECT of that one
    #    image, and reports the index's REAL frame count for its own
    #    direction — never the padded column count (D-5).
    rows = {r["direction"]: r for r in want.get("directions", [])}
    dirs = pb.get("directions") or []
    bad = []
    for c in dirs:
        uv = c.get("uv") or {}
        src = rows.get(c.get("source"))
        if (c.get("texturePath") != want.get("atlas_path")
                or not uv
                or not (0.0 <= uv.get("u0", -1) < uv.get("u1", -1) <= 1.0)
                or not (0.0 <= uv.get("v0", -1) < uv.get("v1", -1) <= 1.0)
                or src is None
                or c.get("frameCount") != src.get("frame_count")):
            bad.append(c.get("direction"))
    ok_cells = check("every direction cell samples a sub-rect of that one "
                     "atlas, with the index's REAL per-direction frame count",
                     dirs and not bad,
                     f"cells={len(dirs)} bad={bad}")

    # 4. And nothing opened a source frame. This is the reduction the
    #    pilot claims, measured against the engine's own load record.
    loaded = send_json(port, "return engine.getLoadedTexturePaths()")
    loaded = loaded if isinstance(loaded, list) else []
    src_prefix = os.path.join("assets", "textures", "units", unit,
                              "animations") + os.sep
    from_source = [p for p in loaded if p.startswith(src_prefix)]
    atlas_prefix = os.path.join("assets", "textures", "units", unit,
                                "atlas") + os.sep
    from_atlas = [p for p in loaded if p.startswith(atlas_prefix)]
    ok_no_source = check("no source animation frame is loaded at all — only "
                         "compiled atlases",
                         not from_source and from_atlas,
                         f"source={from_source[:3]}({len(from_source)}) "
                         f"atlas={len(from_atlas)}")

    return all([ok_all, ok_count, ok_playing, ok_cells, ok_no_source])


def expected_yaml_meta(unit: str, animation: str):
    """(fps, loop) as declared in data/units/<unit>.yaml, or None when
    the file or the entry is absent. Parsed with a deliberately dumb
    line scanner rather than PyYAML (not a probe dependency) — the unit
    files are uniformly two-space-indented, so the animation's own
    fps:/loop: lines are the first ones after its key at a deeper
    indent."""
    path = os.path.join("data", "units", unit + ".yaml")
    if not os.path.exists(path):
        return None
    key = animation + ":"
    fps = loop = None
    indent = None
    with open(path) as fh:
        for line in fh:
            stripped = line.strip()
            cur = len(line) - len(line.lstrip())
            if indent is None:
                if stripped == key:
                    indent = cur
                continue
            if stripped and cur <= indent:
                break
            if stripped.startswith("fps:"):
                fps = float(stripped.split(":", 1)[1].strip())
            elif stripped.startswith("loop:"):
                loop = stripped.split(":", 1)[1].strip() == "true"
    if indent is None:
        return None
    # The YAML's own per-field defaults when the entry omits them.
    return (8.0 if fps is None else fps, True if loop is None else loop)


def poll_unit_ready(port: int, seconds: float = 15.0) -> dict:
    """Poll until the unit viewer has a playing animation (its textures
    upload asynchronously, so the first dumps carry no playback yet)."""
    got = poll_until(seconds, lambda: (
        (lambda d: d if (d.get("playback") or {}).get("ready") else None)(dump(port))))
    return got or dump(port)


def check_units_mode(port: int) -> bool:
    print("3. unit animation viewer (--preview units/acolyte)")
    unit = "acolyte"
    proc = boot_preview(port, f"3. units/{unit}", f"units/{unit}",
                        "preview engine (units)")
    try:
        expected = expected_unit_animations(unit)
        d = poll_unit_ready(port)

        ok_mode = check("mode == unit", d.get("mode") == "unit", d.get("mode"))
        ok_filter = check("texture filter forced to nearest",
                          send(port, "return select(10, engine.getVideoConfig())")
                          == "nearest")

        # Requirement 1: the FULL ordered animation list, cross-checked
        # against the filesystem — a count-only check can't catch an
        # omission or substitution past the visible rows.
        listed = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check("animation list matches the filesystem-derived "
                           "expectation exactly, in order",
                           listed == expected,
                           f"dumped={listed[:5]}...({len(listed)}) "
                           f"expected={expected[:5]}...({len(expected)})"
                           if listed != expected else "")

        # Requirement 2: idle (or the first animation), direction south.
        pb = d.get("playback") or {}
        ok_default = check("default selection is idle / south",
                           d.get("defaultAnim") == "idle"
                           and pb.get("animation") == "idle"
                           and pb.get("direction") == "south",
                           f"defaultAnim={d.get('defaultAnim')} playback={pb}")

        # Requirement 5: the clip's effective fps/loop come from the
        # unit's own YAML when it declares them.
        want_meta = expected_yaml_meta(unit, "idle")
        ok_meta = check("effective fps/loop match data/units/acolyte.yaml",
                        want_meta is not None
                        and abs((pb.get("fps") or 0) - want_meta[0]) < 1e-6
                        and pb.get("loop") == want_meta[1],
                        f"dump=({pb.get('fps')}, {pb.get('loop')}) yaml={want_meta}")

        # Requirement 3/4: every direction cell present, in game order,
        # with the three western ones reporting their real mirror source.
        dirs = pb.get("directions") or []
        ok_dirs = check("all eight direction cells, in the game's own order",
                        [c.get("direction") for c in dirs] == GAME_DIRECTION_ORDER,
                        [c.get("direction") for c in dirs])
        mirrored = {c["direction"]: c.get("source")
                    for c in dirs if c.get("mirrored")}
        ok_mirror = check("mirrored cells report their real source direction",
                          mirrored == MIRROR_SOURCE, mirrored)

        # #2026: use actual engine input pairs for both navigation axes.
        # Animation navigation goes through the browser's ordinary select
        # callback (including async texture readiness); direction navigation
        # walks the view's rendered cells, so mirrored/wrapped entries are
        # covered without reselecting or restarting the animation.
        idle_index = listed.index("idle") if "idle" in listed else -1
        if idle_index < 0 or len(listed) < 2:
            ok_key_animation = check("Up/Down animation navigation has a fixture",
                                     False, f"animations={listed}")
        else:
            step = 1 if idle_index + 1 < len(listed) else -1
            key = "Down" if step == 1 else "Up"
            restore_key = "Up" if step == 1 else "Down"
            adjacent = listed[idle_index + step]
            held, released = press_preview_key(
                port, key,
                lambda state: (state.get("selected") or {}).get("label")
                    == adjacent
                    and (state.get("playback") or {}).get("animation")
                    == adjacent
                    and (state.get("playback") or {}).get("ready") is True,
                seconds=15.0)
            _restore_held, restored = press_preview_key(
                port, restore_key,
                lambda state: (state.get("selected") or {}).get("label") == "idle"
                    and (state.get("playback") or {}).get("animation") == "idle"
                    and (state.get("playback") or {}).get("ready") is True,
                seconds=15.0)
            ok_key_animation = check(
                "Up/Down key pairs select adjacent animations exactly once",
                (held.get("selected") or {}).get("label") == adjacent
                and (released.get("selected") or {}).get("label") == adjacent
                and (restored.get("selected") or {}).get("label") == "idle",
                f"held={held.get('selected')} released={released.get('selected')} "
                f"restored={restored.get('selected')}")

        restored = poll_unit_ready(port)
        restored_pb = restored.get("playback") or {}
        restored_dirs = restored_pb.get("directions") or []
        initial_zoom = (restored.get("zoom") or {}).get("multiplier")
        if not restored_dirs or restored_pb.get("direction") != "south":
            ok_key_direction = check("Left/Right direction navigation has a fixture",
                                     False, restored_pb)
        else:
            left_want = restored_dirs[-1]
            left_held, left_released = press_preview_key(
                port, "Left",
                lambda state: (state.get("playback") or {}).get("direction")
                    == left_want.get("direction"))
            _right_held, right_released = press_preview_key(
                port, "Right",
                lambda state: (state.get("playback") or {}).get("direction")
                    == "south")
            left_pb = left_held.get("playback") or {}
            left_up_pb = left_released.get("playback") or {}
            right_pb = right_released.get("playback") or {}
            ok_key_direction = check(
                "Left/Right key pairs wrap rendered directions without changing animation or zoom",
                left_pb.get("direction") == left_want.get("direction")
                and left_pb.get("sourceDirection") == left_want.get("source")
                and left_up_pb.get("direction") == left_want.get("direction")
                and right_pb.get("direction") == "south"
                and left_pb.get("animation") == "idle"
                and right_pb.get("animation") == "idle"
                and (left_held.get("zoom") or {}).get("multiplier") == initial_zoom
                and (right_released.get("zoom") or {}).get("multiplier") == initial_zoom,
                f"left=({left_pb.get('direction')}, {left_pb.get('sourceDirection')}) "
                f"right={right_pb.get('direction')} zoom={initial_zoom}")

        # Requirement 9: the frame index advances over WALL time. idle
        # runs at the YAML's fps, so a second is many frames — poll for
        # any change rather than asserting a specific index.
        # NB the 1-tuple: poll_until returns on a TRUTHY value, and a
        # frame index of 0 is falsy — returning the bare index would
        # silently poll past a real advance back to frame zero.
        before = ((dump(port).get("playback") or {}).get("frameIndex"))
        after = poll_until(6.0, lambda: (
            (lambda i: (i,) if i != before else None)(
                (dump(port).get("playback") or {}).get("frameIndex"))))
        ok_advance = check("frame index advances over wall time",
                           after is not None and after[0] != before,
                           f"before={before} after={after}")

        # Requirement 3: clicking a direction cell enlarges it — pick a
        # MIRRORED one so source-direction reporting is exercised through
        # a real click, not just the initial dump. Done BEFORE the
        # animation switch below, deliberately: `idle` is a flip:true
        # clip, whereas the first visible row (attack_heavy_RH_dagger)
        # declares flip:false and so has no mirrored cell at all.
        target = next((c for c in dirs if c.get("mirrored")), None)
        if target is None:
            ok_cell = check("clicking a direction cell enlarges it", False,
                            "no mirrored direction cell to click")
        else:
            click_element(port, target.get("bounds") or {})
            after_click = poll_until(6.0, lambda: (
                (lambda p: p if p.get("direction") == target["direction"] else None)(
                    (dump(port).get("playback") or {}))))
            after_click = after_click or (dump(port).get("playback") or {})
            ok_cell = check("clicking a mirrored direction cell enlarges it and "
                            "reports its source direction",
                            after_click.get("direction") == target["direction"]
                            and after_click.get("mirrored") is True
                            and after_click.get("sourceDirection")
                                == MIRROR_SOURCE.get(target["direction"]),
                            after_click)

        # Resize: animation, direction, and scroll offset all survive
        # (the #887 amendment's reflow contract). Run right after the
        # direction click so the direction under test is the MIRRORED
        # one just selected, not merely the south default.
        pre = dump(port)
        pre_pb = pre.get("playback") or {}
        pre_bounds = pre.get("panelBounds") or {}
        pre_scroll = pre.get("scrollOffset")
        # Window units, not panel bounds — see window_size()'s docstring.
        win_w, win_h = window_size(port)
        send(port, f"return engine.setResolution({win_w + 200}, {win_h + 150})",
             timeout=10.0)
        post = poll_until(10.0, lambda: (
            (lambda s: s if (s.get("panelBounds") or {}) != pre_bounds else None)(
                dump(port)))) or dump(port)
        post_pb = post.get("playback") or {}
        ok_resize = check("animation, direction, and scroll offset all survive a resize",
                          post_pb.get("animation") == pre_pb.get("animation")
                          and post_pb.get("direction") == pre_pb.get("direction")
                          and post.get("scrollOffset") == pre_scroll,
                          f"before=({pre_pb.get('animation')}, "
                          f"{pre_pb.get('direction')}, {pre_scroll}) "
                          f"after=({post_pb.get('animation')}, "
                          f"{post_pb.get('direction')}, {post.get('scrollOffset')})")

        # Requirement 3: selecting a different animation (row located
        # from the dump, never a hardcoded coordinate) switches the clip
        # — and the NEW clip's effective fps/loop come from its own YAML
        # entry, a second, independent metadata data point.
        rows = post.get("rows") or []
        other = next((r for r in rows
                      if r.get("label") not in (None, post_pb.get("animation"))), None)
        if other is None:
            ok_select = check("selecting another animation changes the clip",
                              False, "no second visible row to click")
        else:
            click_element(port, other.get("bounds") or {})
            d2 = poll_unit_ready(port)
            d2_pb = d2.get("playback") or {}
            want2 = expected_yaml_meta(unit, other["label"])
            ok_select = check("selecting another animation (via row bounds, not "
                              "hardcoded coords) switches the clip and its metadata",
                              (d2.get("selected") or {}).get("label") == other["label"]
                              and d2_pb.get("animation") == other["label"]
                              and want2 is not None
                              and abs((d2_pb.get("fps") or 0) - want2[0]) < 1e-6
                              and d2_pb.get("loop") == want2[1],
                              f"clicked={other['label']} "
                              f"selected={(d2.get('selected') or {}).get('label')} "
                              f"playback=({d2_pb.get('animation')}, "
                              f"{d2_pb.get('fps')}, {d2_pb.get('loop')}) yaml={want2}")

        # #1833: forced continuous replay. Pick a row whose SOURCE
        # `loop` is false (acolyte has 26 of them) — under the
        # pre-#1833 hold-at-end policy such a clip freezes on its last
        # frame within half a second of selection, which is exactly the
        # usability gap this viewer existed to hit.
        d3 = poll_unit_ready(port)
        loops = {e.get("label"): e.get("loop") for e in (d3.get("entries") or [])}
        nonloop = next((r for r in (d3.get("rows") or [])
                        if loops.get(r.get("label")) is False), None)
        if nonloop is None:
            ok_replay = check("a source loop:false animation replays "
                              "continuously", False,
                              "no visible loop:false row to click")
        else:
            click_element(port, nonloop.get("bounds") or {})
            got = poll_until(10.0, lambda: (
                (lambda s: s if (s.get("playback") or {}).get("animation")
                    == nonloop["label"]
                    and (s.get("playback") or {}).get("ready") else None)(
                        dump(port))))
            selected_at = time.monotonic()
            pb3 = (got or dump(port)).get("playback") or {}
            # Requirement 6: the dump still reports the AUTHORED value.
            # Without this the replay check below would also pass on an
            # implementation that simply forced every clip to loop=true.
            ok_truthful = check("the replay fixture still reports its authored "
                                "loop=false (or this check proves nothing)",
                                pb3.get("animation") == nonloop["label"]
                                and pb3.get("loop") is False,
                                f"animation={pb3.get('animation')} "
                                f"loop={pb3.get('loop')}")
            ok_replay = check_forced_replay(
                port, f"units/{unit} {nonloop['label']} (authored loop=false)",
                selected_at, pb3.get("frameCount"), pb3.get("fps")) \
                and ok_truthful

        # #1260: the atlas contract, checked against the compiled index
        # on disk. Deliberately LAST, after the animation switch and the
        # resize above, so it observes a viewer that has already
        # reselected and rebuilt rather than only its first frame.
        ok_atlas = check_atlas_backed(port, unit, poll_unit_ready(port))

        # Requirement 6: only THIS unit's textures (plus list chrome).
        root_prefix = os.path.join("assets", "textures", "units", unit) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_mode, ok_filter, ok_entries, ok_default, ok_meta,
                    ok_dirs, ok_mirror, ok_key_animation, ok_key_direction,
                    ok_advance, ok_select, ok_cell,
                    ok_resize, ok_replay, ok_atlas, ok_trimmed,
                    ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def check_units_promoted(port: int) -> bool:
    """A tree #1261 (TEX-6) promoted out of the inventory-only form.

    `tiller` used to be this probe's "no data/units YAML at all"
    fixture. #1257 gave it a declaration under the top-level
    `asset_units:` key; #1261 promoted that to a real `units:` entry,
    because with per-frame unit-animation loading retired an animation
    renders only through the compiled atlas its declaration drives.

    The visible behaviour asserted here has not changed across either
    step — the declaration states `flip: true` over the canonical five,
    which is exactly what the pre-#1257 inference produced from that
    layout — so both were a change of SOURCE, not of result.
    """
    print("4. promoted declaration (--preview units/tiller): declared metadata")
    unit = "tiller"
    proc = boot_preview(port, f"4. units/{unit}", f"units/{unit}",
                        "preview engine (units, promoted)")
    try:
        yaml_path = os.path.join("data", "units", unit + ".yaml")
        declaration = ""
        if os.path.exists(yaml_path):
            with open(yaml_path) as fh:
                declaration = fh.read()
        # Pin the fixture's premise: the file exists AND declares the
        # gameplay key rather than the inventory-only one. Without both,
        # the metadata assertions below could pass against an
        # `asset_units:` entry and prove nothing about the promotion.
        ok_declared = check(
            "the fixture really is declared under units: (or this "
            "check proves nothing)",
            any(line.rstrip() == "units:"
                for line in declaration.splitlines())
            and not any(line.rstrip() == "asset_units:"
                        for line in declaration.splitlines()),
            yaml_path)

        d = poll_unit_ready(port)
        expected = expected_unit_animations(unit)
        listed = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check("animation list matches the filesystem-derived "
                           "expectation exactly, in order",
                           listed == expected, f"dumped={listed} expected={expected}")

        ok_meta = check("every animation reports the DECLARED fps=8 / "
                        "loop=true / flip=true",
                        all(abs((e.get("fps") or 0) - 8.0) < 1e-6
                            and e.get("loop") is True
                            and e.get("flip") is True
                            for e in (d.get("entries") or [])),
                        [(e.get("label"), e.get("fps"), e.get("loop"),
                          e.get("flip")) for e in (d.get("entries") or [])])

        # The declared five-direction mirroring populates all eight
        # cells, exactly as the pre-#1257 inference did.
        pb = d.get("playback") or {}
        dirs = [c.get("direction") for c in (pb.get("directions") or [])]
        ok_dirs = check("declared mirroring populates all eight cells",
                        dirs == GAME_DIRECTION_ORDER, dirs)
        # `source`, not `sourceDirection`: a per-direction CELL reports
        # its own authored source under `source`, while `sourceDirection`
        # is the top-level playback field naming the ENLARGED cell's.
        # This check read the top-level name off each cell and so
        # compared three nils to three direction names — it could only
        # ever fail (found by #1260, which first ran the probe against a
        # tree where the units phase above passes).
        ok_mirrored = check("W/SW/NW are the mirrored cells, sourced from "
                            "their eastern counterparts",
                            [c.get("source")
                             for c in (pb.get("directions") or [])
                             if c.get("mirrored")]
                            == ["south-east", "east", "north-east"],
                            [(c.get("direction"), c.get("mirrored"),
                              c.get("source"))
                             for c in (pb.get("directions") or [])])

        # #1261: a promoted tree is atlas-backed like any other. Before
        # it, tiller was the probe's canonical LEGACY unit.
        ok_atlas = check_atlas_backed(port, unit, d)

        root_prefix = os.path.join("assets", "textures", "units", unit) + os.sep
        ok_trimmed = check_trimmed_loading(port, root_prefix, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)

        return all([ok_declared, ok_entries, ok_meta, ok_dirs, ok_mirrored,
                    ok_atlas, ok_trimmed, ok_no_gameplay])
    finally:
        quit_engine(port, proc)


def shipped_units() -> list[str]:
    """Every declared unit, from data/units/ rather than from a list
    written here — a tree added without this probe noticing is the
    failure mode a hardcoded roster would hide."""
    return sorted(f[:-len(".yaml")]
                  for f in os.listdir(os.path.join("data", "units"))
                  if f.endswith(".yaml"))


def check_units_roster(port: int) -> bool:
    """#1261 requirement 2: EVERY shipped unit tree browses, and does so
    through the production atlas/index path.

    Phases 3 and 4 cover acolyte and tiller in depth. This one is
    breadth: one boot per remaining tree, asserting the three things
    that can only be observed against real compiled artifacts — the
    animation list still equals what is on disk, every animation
    reports atlas storage naming its own compiled atlas, and nothing
    outside this unit's own directory was loaded.
    """
    covered = {"acolyte", "tiller"}
    units = [u for u in shipped_units() if u not in covered]
    print(f"4b. the rest of the shipped roster ({', '.join(units)}): "
          f"atlas-backed browsing")
    ok = True
    for unit in units:
        proc = boot_preview(port, f"4b. units/{unit}", f"units/{unit}",
                            f"preview engine (units/{unit})")
        try:
            d = poll_unit_ready(port)
            expected = expected_unit_animations(unit)
            listed = [e.get("label") for e in (d.get("entries") or [])]
            ok_entries = check(f"{unit}: animation list matches the "
                               f"filesystem-derived expectation, in order",
                               listed == expected,
                               f"dumped={listed} expected={expected}")
            ok_atlas = check_atlas_backed(port, unit, d)
            root_prefix = os.path.join(
                "assets", "textures", "units", unit) + os.sep
            ok_trimmed = check_trimmed_loading(port, root_prefix,
                                               allow_chrome=True)
            ok = all([ok, ok_entries, ok_atlas, ok_trimmed])
        finally:
            quit_engine(port, proc)
    return ok
