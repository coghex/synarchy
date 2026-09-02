#!/usr/bin/env python3
"""Offscreen scene-primitive pixel probe (#2192).

GPU-backed screenshot verification that the public Lua scene primitives
actually reach the frame: `engine.spawnText` on a UI layer and
`engine.spawnSprite` on a UI layer are each drawn at their declared
layer, and every mutation verb is reflected in the next frame.

Before #2192 `updateSceneForRender` built text batches nobody read and
the frame kept only scene sprites BELOW `uiLayerThreshold`, so both
calls reported success and drew nothing. The GPU-free half of the fix —
the exact per-layer merge/order command recording consumes — is the
hspec group `frame layer assembly`; this probe is the half only a real
Vulkan frame can prove: pixels.

Boots `--offscreen` (GPU on, window off) to the real main menu, then:

  1. Two baseline captures of the menu, asserting the three probe
     regions are STATIC between them (fixture stability — a region that
     changes on its own can neither attribute nor restore anything).
  2. Spawns a text in region A (bottom-left) and a white-texture sprite
     in region B (bottom-right), on a UI layer, and attributes each
     primitive INDEPENDENTLY: A changed, B changed, the untouched region
     C did not.
  3. Mutations, each captured on its own frame and attributed to its own
     region: `setText` changes A; `setVisible(false)` restores A to the
     baseline while B stays drawn; `setVisible(true)` redraws A; `setPos`
     moves the sprite out of B (B restores) into C; `setSize` and
     `setColor` each change C; `destroy` restores C, then A.

The regions are bounded so a single changed frame containing both nodes
cannot pass when only one renders, and the hide/show/destroy captures
prove the route is rebuilt every frame rather than drawn once.

Needs a GPU (Vulkan device) — manual-only, never CI-gated, same as
tools/offscreen_probe.py.

Usage:
  python3 tools/scene_primitives_probe.py
  python3 tools/scene_primitives_probe.py --port 9431 --win-size 800x600
"""
from __future__ import annotations

import argparse
import os
import tempfile
import time

from probelib import boot, poll_until, quit_engine, send, send_json
from offscreen_probe import (find_widget, screenshot, png_stats,
                             png_region_changed_pixels)
from probe_runner_diagnostics import FailureEmitter   # durable failure records (#1982)

LOG = "/tmp/scene_primitives_engine.log"
#: #1982 — this run's durable failure records.
FAILURE = FailureEmitter("scene_primitives_probe")

#: Just above the UI threshold (World.Grid.uiLayerThreshold = 10): the
#: routing under test is the threshold split itself, so the layer sits
#: as close to it as a UI layer can. Every UI page sits far higher
#: (UI.Types.uiLayerBand starts at 0 for the HUD and 10000 for menus),
#: which is fine — the probe regions are corners no page paints.
LAYER = 12
#: How many changed pixels a drawn primitive must produce in its region
#: (a 32px text string or a 64x48 sprite paints hundreds).
MIN_DRAWN = 40
#: Frames to settle after a console verb before capturing: the Lua
#: thread queues the request, the render thread applies it on its next
#: tick and draws it the frame after.
SETTLE = 0.4

failures: list[str] = []


def as_id(value) -> int | None:
    """A handle or object id the console returned, or None. `loadTexture`
    pushes a Lua NUMBER (so it arrives as `1199.0`) while the spawn verbs
    push integers; both are whole numbers and both are accepted."""
    if isinstance(value, bool):
        return None
    if isinstance(value, int):
        return value
    if isinstance(value, float) and value.is_integer():
        return int(value)
    return None


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    if not ok:
        failures.append(name if not detail else f"{name} — {detail}")
    return ok


def regions(w: int, h: int) -> dict[str, tuple[int, int, int, int]]:
    """Three disjoint (x, y, w, h) boxes in framebuffer pixels, clear of
    the centred menu panel and its title: A = bottom-left (text),
    B = bottom-right (sprite), C = right, above B (sprite after setPos)."""
    return {
        "A": (10, h - 130, 380, 120),
        "B": (w - 160, h - 100, 150, 90),
        "C": (w - 160, h // 2 - 45, 150, 90),
    }


def capture(port: int, shots: str, name: str) -> str | None:
    time.sleep(SETTLE)
    path = os.path.join(shots, f"{name}.png")
    if not check(f"capture '{name}' answers", screenshot(port, path)):
        return None
    if not check(f"capture '{name}' is a readable PNG", png_stats(path) is not None):
        return None
    return path


def changed(a: str | None, b: str | None, box) -> int | None:
    if a is None or b is None:
        return None
    return png_region_changed_pixels(a, b, box)


def region_drawn(name: str, before: str | None, after: str | None, box) -> None:
    n = changed(before, after, box)
    check(name, n is not None and n >= MIN_DRAWN,
          "captures not comparable" if n is None else f"{n} changed pixels")


def region_untouched(name: str, before: str | None, after: str | None, box) -> None:
    n = changed(before, after, box)
    check(name, n == 0,
          "captures not comparable" if n is None else f"{n} changed pixels")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9431)
    ap.add_argument("--win-size", default="800x600")
    args = ap.parse_args()
    shots = tempfile.mkdtemp(prefix="scene_primitives_probe_")
    return run(args, shots)


def run(args, shots: str) -> int:
    w, h = (int(v) for v in args.win_size.lower().split("x"))
    box = regions(w, h)
    port = args.port
    # Text pen origin inside A; sprite CENTRE inside B, then C.
    text_x, text_y = box["A"][0] + 30, box["A"][1] + 50
    b_cx = box["B"][0] + box["B"][2] // 2
    b_cy = box["B"][1] + box["B"][3] // 2
    c_cx = box["C"][0] + box["C"][2] // 2
    c_cy = box["C"][1] + box["C"][3] // 2

    print(f"== offscreen scene-primitive pixel check (port {port}, {w}x{h}) ==")
    proc = boot(port, mode=("--offscreen",), args=["--size", args.win_size],
                log=LOG, label="offscreen engine")
    try:
        reached = poll_until(90.0, lambda: find_widget(port, "Create World") is not None)
        if not check("main menu reached (Create World widget present)", bool(reached)):
            return report(shots)

        # The menu's own font is already cached, so this returns the
        # same handle ui_manager_boot holds; the texture load is a real
        # (async) upload, so it settles before anything draws with it.
        font = as_id(send_json(port,
            'return engine.loadFont("assets/fonts/arcade.ttf", 24)'))
        tex = as_id(send_json(port,
            'return engine.loadTexture("assets/textures/utility/white.png", "ui")'))
        if not check("font and texture handles obtained",
                     font is not None and tex is not None,
                     f"font={font!r} tex={tex!r}"):
            return report(shots)
        time.sleep(1.0)

        # -- 1. baseline, twice: the regions must be static on their own.
        base = capture(port, shots, "baseline")
        base2 = capture(port, shots, "baseline_again")
        for name, b in box.items():
            region_untouched(f"region {name} is static between two idle captures",
                             base, base2, b)
        if failures:
            return report(shots)

        # -- 2. spawn both, attribute each to its own region.
        text_id = as_id(send_json(port,
            f'return engine.spawnText({text_x}, {text_y}, {font}, "SCENE TEXT", '
            f'"white", {LAYER}, 32)'))
        sprite_id = as_id(send_json(port,
            f'return engine.spawnSprite({b_cx}, {b_cy}, 64, 48, {tex}, {LAYER})'))
        if not check("spawnText and spawnSprite returned object ids",
                     text_id is not None and sprite_id is not None,
                     f"text={text_id!r} sprite={sprite_id!r}"):
            return report(shots)
        both = capture(port, shots, "spawned")
        region_drawn("UI-layer scene text draws in region A", base, both, box["A"])
        region_drawn("UI-layer scene sprite draws in region B", base, both, box["B"])
        region_untouched("region C is untouched by either spawn", base, both, box["C"])

        # -- 3. mutations, one frame each.
        send(port, f'engine.setText({text_id}, "CHANGED"); return "ok"')
        retext = capture(port, shots, "set_text")
        region_drawn("setText changes what region A shows", both, retext, box["A"])
        region_untouched("setText leaves the sprite's region B alone", both, retext, box["B"])

        send(port, f'engine.setVisible({text_id}, false); return "ok"')
        hidden = capture(port, shots, "text_hidden")
        region_untouched("setVisible(false) on the text restores region A to the baseline",
                         base, hidden, box["A"])
        region_drawn("the sprite is still drawn in B while the text is hidden",
                     base, hidden, box["B"])

        send(port, f'engine.setVisible({text_id}, true); return "ok"')
        reshown = capture(port, shots, "text_reshown")
        region_drawn("setVisible(true) redraws the text in region A", base, reshown, box["A"])

        send(port, f'engine.setPos({sprite_id}, {c_cx}, {c_cy}); return "ok"')
        moved = capture(port, shots, "sprite_moved")
        region_untouched("setPos moves the sprite out of region B (B restores)",
                         base, moved, box["B"])
        region_drawn("setPos draws the sprite in region C", base, moved, box["C"])

        send(port, f'engine.setSize({sprite_id}, 120, 80); return "ok"')
        resized = capture(port, shots, "sprite_resized")
        region_drawn("setSize changes what region C shows", moved, resized, box["C"])

        send(port, f'engine.setColor({sprite_id}, "red"); return "ok"')
        recolored = capture(port, shots, "sprite_recolored")
        region_drawn("setColor changes what region C shows", resized, recolored, box["C"])

        send(port, f'engine.destroy({sprite_id}); return "ok"')
        no_sprite = capture(port, shots, "sprite_destroyed")
        region_untouched("destroy on the sprite restores region C to the baseline",
                         base, no_sprite, box["C"])
        region_drawn("the text is still drawn in A after the sprite is destroyed",
                     base, no_sprite, box["A"])

        send(port, f'engine.destroy({text_id}); return "ok"')
        none_left = capture(port, shots, "text_destroyed")
        region_untouched("destroy on the text restores region A to the baseline",
                         base, none_left, box["A"])
    finally:
        quit_engine(port, proc)

    return report(shots)


def report(shots: str) -> int:
    print(f"\nscreenshots kept in {shots}")
    print("-" * 56)
    if failures:
        # Durable records rather than an unflushed stderr print (#1982):
        # run_probes.py keeps only the tail of the merged output.
        FAILURE.report(failures)
        FAILURE.context_log(LOG, label="offscreen engine log")
        FAILURE.context("screenshots", shots)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
