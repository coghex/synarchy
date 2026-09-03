"""The `dispatch` family: grouped flora/structure items routed into the
shared simple browser, and the canonical category sweep (#888, epic
#427 acceptance).

Two scenarios; both boot more than once:

  * `check_flat_grouped_dispatch` — phase `8.`, one boot each for
    `flora/<first item>` and `structures/wire`.
  * `check_canonical_dispatch_sweep` — phase `9.`, one boot per
    canonical category target.

A library, not a probe: registered nowhere, runnable only through the
facade's inventory (`python3 tools/preview_probe.py --only dispatch`).
"""
from __future__ import annotations

import os
from probelib import quit_engine, poll_until

from .harness import (boot_preview, check, check_no_gameplay_scripts_loaded,
                      check_trimmed_loading, dump, expected_entries_at,
                      first_item, poll_state)

def check_flat_grouped_item(port: int, category: str, item: str) -> bool:
    """#888 Requirement 2: flora and structures item folders are flat
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


def check_flat_grouped_dispatch(port: int) -> bool:
    print("8. flora/structures items reuse the simple-category browser")
    return all([check_flat_grouped_item(port, "flora", first_item("flora")),
                check_flat_grouped_item(port, "structures", "wire")])


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
        ("structures/wire", "list"),
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
