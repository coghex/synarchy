"""The `dispatch` family: grouped flora items routed into the shared
simple browser, structure PACKS routed into their own viewer (#2495),
and the canonical category sweep (#888, epic #427 acceptance).

Two scenarios; both boot more than once:

  * `check_flat_grouped_dispatch` — phase `8.`, one boot for
    `flora/<first item>` and one per shipped structure pack
    (`structures/dungeon_1`, `structures/wire`).
  * `check_canonical_dispatch_sweep` — phase `9.`, one boot per
    canonical category target.

A library, not a probe: registered nowhere, runnable only through the
facade's inventory (`python3 tools/preview_probe.py --only dispatch`).
"""
from __future__ import annotations

import os
import time

import yaml
from probelib import quit_engine, poll_until, send_json

from .harness import (boot_preview, check, check_no_gameplay_scripts_loaded,
                      check_trimmed_loading, check_trimmed_loading_paths,
                      click_element, dump, expected_entries_at, first_item,
                      poll_state, press_preview_key)

def check_flat_grouped_item(port: int, category: str, item: str) -> bool:
    """#888 Requirement 2: flora and pack-less structures item folders are flat
    sets of static PNGs, so they are ROUTED into #886's simple-category
    browser rooted at the item's own folder rather than given viewers of
    their own. This is a dispatch-level check by design — the browsing
    behavior itself is already gated by check 1."""
    proc = boot_preview(port, f"8. {category}/{item}", f"{category}/{item}",
                        f"preview engine ({category}/{item})")
    try:
        root = os.path.join("assets", "textures", category, item)
        expected = expected_entries_at(root)
        d = poll_state(port, "ready")
        ok_mode = check(f"{category}/{item}: mode == list (the shared simple "
                        "browser, rooted at the item folder)",
                        d.get("mode") == "list", d.get("mode"))
        listed = [e.get("label") for e in (d.get("entries") or [])]
        ok_entries = check(f"{category}/{item}: the item folder's own textures, "
                           "in order",
                           listed == expected,
                           f"dumped={listed} expected={expected}")
        ok_first = check(f"{category}/{item}: first entry auto-selected and "
                         "resolved",
                         (d.get("selected") or {}).get("label")
                         == (expected[0] if expected else None)
                         and d.get("state") == "ready",
                         f"selected={d.get('selected')} state={d.get('state')}")
        ok_trimmed = check_trimmed_loading(port, root + os.sep, allow_chrome=True)
        ok_no_gameplay = check_no_gameplay_scripts_loaded(port)
        return all([ok_mode, ok_entries, ok_first, ok_trimmed, ok_no_gameplay])
    finally:
        quit_engine(port, proc)


WALL_CAPS = ("00", "10", "01", "11")


def expected_pack(name: str) -> tuple[list[dict], set[str]]:
    """An independent, YAML-derived expectation for one shipped pack
    (#2495): the appearances in the viewer's grouped order — each piece
    kind's default then its variant overrides, each wall edge likewise,
    then Wire's connections, all in DOCUMENT order (PyYAML keeps it) —
    each with its resolved texture and facemaps, plus every path the
    pack names at all. Read with PyYAML rather than any engine code, so
    it cross-checks Engine.Preview.StructurePack instead of restating
    it."""
    with open(os.path.join("data", "structure_packs", f"{name}.yaml"),
              encoding="utf-8") as f:
        doc = yaml.safe_load(f)
    pieces = doc.get("pieces") or {}
    walls = doc.get("walls") or {}
    variants = doc.get("variants") or {}
    conns = doc.get("connections") or {}
    apps: list[dict] = []
    named: set[str] = set()

    def lifecycle_paths(decl: dict) -> list[str]:
        out = list(decl.get("construction") or [])
        out += list((decl.get("destruction") or {}).get("frames") or [])
        return out

    for kind, base in pieces.items():
        named.update([base["texture"], base["facemap"], *lifecycle_paths(base)])
        apps.append({"identity": kind, "texture": base["texture"],
                     "facemaps": {None: base["facemap"]}})
        for vname, v in variants.items():
            over = (v.get("pieces") or {}).get(kind)
            if over is None:
                continue
            named.update(p for p in [over.get("texture"), over.get("facemap"),
                                     *lifecycle_paths(over)] if p)
            apps.append({"identity": f"{kind}@{vname}",
                         "texture": over.get("texture", base["texture"]),
                         "facemaps": {None: over.get("facemap", base["facemap"])}})
    for edge, base in walls.items():
        faces = base.get("facemaps") or {}
        named.update([base["texture"], *faces.values(), *lifecycle_paths(base)])
        apps.append({"identity": f"wall:{edge}",
                     "texture": base["texture"],
                     "facemaps": {c: faces.get(c) for c in WALL_CAPS}})
        for vname, v in variants.items():
            over = (v.get("walls") or {}).get(edge)
            if over is None:
                continue
            own = over.get("facemaps") or {}
            named.update(p for p in [over.get("texture"), *own.values(),
                                     *lifecycle_paths(over)] if p)
            apps.append({"identity": f"wall:{edge}@{vname}",
                         "texture": over.get("texture", base["texture"]),
                         "facemaps": {c: own.get(c, faces.get(c))
                                      for c in WALL_CAPS}})
    for cname, entry in conns.items():
        tex = entry if isinstance(entry, str) else entry["texture"]
        named.update([tex, doc["facemap"]])
        if isinstance(entry, dict):
            named.update(lifecycle_paths(entry))
        apps.append({"identity": f"wire:{cname}", "texture": tex,
                     "facemaps": {None: doc["facemap"]}})
    return apps, named


def _element(port: int, handle) -> dict:
    got = send_json(port, f"return UI.getElementInfo({int(handle)})")
    return got if isinstance(got, dict) else {}


def check_structure_pack(port: int, name: str) -> bool:
    """#2495: `structures/<name>` with a pack manifest browses the PACK.
    Every appearance the YAML declares is listed in grouped declaration
    order with its resolved texture; the default is the first declared
    piece kind's default at `static`; every lifecycle cell and (for a
    wall edge) every cap cell is selected through its own dump-reported
    bounds; Left/Right walk the lifecycle row; and only textures the
    pack names (plus list chrome) are ever loaded."""
    target = f"structures/{name}"
    expected, named = expected_pack(name)
    proc = boot_preview(port, f"8. {target}", target,
                        f"preview engine ({target})")
    try:
        d = poll_state(port, "ready", seconds=20.0)
        results = [check(f"{target}: mode == structure (the pack viewer)",
                         d.get("mode") == "structure" and d.get("pack") == name,
                         f"mode={d.get('mode')} pack={d.get('pack')}")]
        got = [a.get("identity") for a in d.get("appearances") or []]
        want = [a["identity"] for a in expected]
        results.append(check(f"{target}: every declared appearance, grouped, "
                             "in declaration order", got == want,
                             f"dumped={got} expected={want}"))
        by_id = {a.get("identity"): a for a in d.get("appearances") or []}
        textures_ok = all(by_id.get(a["identity"], {}).get("texture")
                          == a["texture"] for a in expected)
        results.append(check(f"{target}: each appearance's resolved texture "
                             "matches the YAML (variant inheritance included)",
                             textures_ok))
        results.append(check(f"{target}: default is the first appearance at "
                             "static, showing its texture",
                             d.get("selectedAppearance") == want[0]
                             and d.get("selectedLifecycle") == "static"
                             and d.get("path") == expected[0]["texture"]
                             and d.get("alphaPolicy") == "facemap-alpha",
                             f"selected={d.get('selectedAppearance')} "
                             f"lifecycle={d.get('selectedLifecycle')} "
                             f"path={d.get('path')}"))

        # Every lifecycle cell, clicked through its own bounds.
        for cell in list(d.get("lifecycleRow") or []):
            lname = cell.get("lifecycle")
            click_element(port, cell.get("bounds") or {})
            after = poll_until(10.0, lambda: (
                (lambda s: s if s.get("selectedLifecycle") == lname else None)(
                    dump(port)))) or dump(port)
            ok = after.get("selectedLifecycle") == lname
            if ok and after.get("undeclared"):
                marker = _element(port, after.get("missingElement"))
                sprite = _element(port, after.get("spriteElement"))
                ok = (marker.get("visible") is True
                      and marker.get("text") == "undeclared"
                      and sprite.get("visible") is False
                      and after.get("state") == "ready")
            results.append(check(f"{target}: lifecycle cell `{lname}` selects "
                                 "it (undeclared shows its marker, no sprite)",
                                 ok, f"lifecycle={after.get('selectedLifecycle')} "
                                 f"undeclared={after.get('undeclared')}"))

        # Left/Right walk the row: Right from the last cell wraps to static.
        _, after = press_preview_key(port, "Right", lambda s: (
            s.get("selectedLifecycle") == "static"))
        results.append(check(f"{target}: Right wraps the lifecycle row",
                             after.get("selectedLifecycle") == "static",
                             f"lifecycle={after.get('selectedLifecycle')}"))

        # A wall edge's caps, when the pack has walls.
        wall = next((a for a in expected if a["identity"].startswith("wall:")),
                    None)
        if wall is not None:
            for _ in range(len(want)):
                if dump(port).get("selectedAppearance") == wall["identity"]:
                    break
                press_preview_key(port, "Down", lambda s: True)
            s = dump(port)
            results.append(check(f"{target}: Down reaches {wall['identity']}",
                                 s.get("selectedAppearance") == wall["identity"],
                                 f"selected={s.get('selectedAppearance')}"))
            for cell in list(s.get("capRow") or []):
                cap = cell.get("cap")
                click_element(port, cell.get("bounds") or {})
                c = poll_until(10.0, lambda: (
                    (lambda x: x if x.get("selectedCap") == cap else None)(
                        dump(port)))) or dump(port)
                results.append(check(
                    f"{target}: cap {cap} reports its own facemap, same texture",
                    c.get("selectedCap") == cap
                    and c.get("facemap") == wall["facemaps"].get(cap)
                    and c.get("path") == wall["texture"],
                    f"cap={c.get('selectedCap')} facemap={c.get('facemap')}"))
            results.append(check(f"{target}: a wall edge offers all four caps",
                                 len(s.get("capRow") or []) == 4,
                                 f"caps={len(s.get('capRow') or [])}"))

        time.sleep(0.2)
        results.append(check_trimmed_loading_paths(port, named, target))
        results.append(check_no_gameplay_scripts_loaded(port))
        return all(results)
    finally:
        quit_engine(port, proc)


def check_flat_grouped_dispatch(port: int) -> bool:
    print("8. flora items reuse the simple-category browser; structure "
          "packs browse the pack (#2495)")
    return all([check_flat_grouped_item(port, "flora", first_item("flora")),
                check_structure_pack(port, "dungeon_1"),
                check_structure_pack(port, "wire")])


def check_canonical_dispatch_sweep(port: int) -> bool:
    """The epic (#427) acceptance sweep: EVERY canonical category
    dispatches to its documented behavior, and the Phase 1 (#632)
    placeholder mode no longer exists anywhere."""
    print("9. canonical dispatch sweep: every category, no placeholder left")
    targets = [
        ("icons", "list"), ("items", "list"), ("ui", "list"), ("world", "list"),
        ("units/acolyte", "unit"),
        (f"flora/{first_item('flora')}", "list"),
        ("buildings/workbench", "building"),
        ("structures/wire", "structure"),
        ("structures/dungeon_1", "structure"),
        ("audio", "audio"),
    ]
    results = []
    for target, want_mode in targets:
        proc = boot_preview(port, f"9. sweep {target}", target,
                            f"preview engine (sweep {target})")
        try:
            d = poll_until(20.0, lambda: (
                (lambda s: s if s.get("mode") else None)(dump(port)))) or dump(port)
            mode = d.get("mode")
            results.append(check(f"--preview {target} dispatches to mode="
                                 f"{want_mode}",
                                 mode == want_mode and mode != "placeholder",
                                 f"mode={mode}"))
        finally:
            quit_engine(port, proc)
    return all(results)
